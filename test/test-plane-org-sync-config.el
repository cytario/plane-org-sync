;;; test-plane-org-sync-config.el --- Tests for plane-org-sync-config  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Martin

;; Author: Martin
;; Maintainer: Martin
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the configuration module of plane-org-sync.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'plane-org-sync-config)

;;;; HTTPS Validation Tests

(ert-deftest plane-org-sync-config-test-https-url-accepted ()
  "HTTPS URLs should pass validation without error."
  (let ((plane-org-sync-instance-url "https://api.plane.so"))
    (should (eq nil (plane-org-sync-config--validate-instance-url)))))

(ert-deftest plane-org-sync-config-test-http-url-rejected ()
  "HTTP URLs should signal a `user-error'."
  (let ((plane-org-sync-instance-url "http://api.plane.so"))
    (should-error (plane-org-sync-config--validate-instance-url)
                  :type 'user-error)))

(ert-deftest plane-org-sync-config-test-https-custom-host ()
  "HTTPS URLs with custom hosts should pass validation."
  (let ((plane-org-sync-instance-url "https://plane.example.com"))
    (should (eq nil (plane-org-sync-config--validate-instance-url)))))

;;;; API Key Retrieval -- Direct String

(ert-deftest plane-org-sync-config-test-direct-api-key ()
  "A non-empty string `plane-org-sync-api-key' is returned directly."
  (let ((plane-org-sync-instance-url "https://api.plane.so")
        (plane-org-sync-api-key "my-secret-key"))
    (should (equal "my-secret-key"
                   (plane-org-sync-config--get-api-key)))))

;;;; API Key Retrieval -- Empty String Falls Through

(ert-deftest plane-org-sync-config-test-empty-string-api-key ()
  "An empty string `plane-org-sync-api-key' falls through to auth-source."
  (let ((plane-org-sync-instance-url "https://api.plane.so")
        (plane-org-sync-api-key ""))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args)
                 (list (list :secret "auth-source-key")))))
      (should (equal "auth-source-key"
                     (plane-org-sync-config--get-api-key))))))

;;;; API Key Retrieval -- Auth-Source Lookup

(ert-deftest plane-org-sync-config-test-auth-source-lookup ()
  "When `plane-org-sync-api-key' is nil, auth-source is queried."
  (let ((plane-org-sync-instance-url "https://plane.example.com")
        (plane-org-sync-api-key nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest args)
                 ;; Verify that the host is extracted correctly.
                 (if (equal (plist-get args :host) "plane.example.com")
                     (list (list :secret "found-key"))
                   nil))))
      (should (equal "found-key"
                     (plane-org-sync-config--get-api-key))))))

;;;; API Key Retrieval -- Functional :secret

(ert-deftest plane-org-sync-config-test-functional-secret ()
  "A functional :secret from auth-source is funcall'd."
  (let ((plane-org-sync-instance-url "https://api.plane.so")
        (plane-org-sync-api-key nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args)
                 (list (list :secret (lambda () "deferred-key"))))))
      (should (equal "deferred-key"
                     (plane-org-sync-config--get-api-key))))))

;;;; API Key Retrieval -- Missing Key

(ert-deftest plane-org-sync-config-test-missing-key-signals-error ()
  "When no key is configured and auth-source finds nothing, signal `user-error'."
  (let ((plane-org-sync-instance-url "https://api.plane.so")
        (plane-org-sync-api-key nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args) nil)))
      (should-error (plane-org-sync-config--get-api-key)
                    :type 'user-error))))

;;;; API Key Retrieval -- HTTP Rejected Before Auth-Source

(ert-deftest plane-org-sync-config-test-http-rejected-in-get-api-key ()
  "Getting the API key with an HTTP URL signals `user-error' before auth lookup."
  (let ((plane-org-sync-instance-url "http://insecure.example.com")
        (plane-org-sync-api-key nil)
        (auth-source-called nil))
    (cl-letf (((symbol-function 'auth-source-search)
               (lambda (&rest _args)
                 (setq auth-source-called t)
                 nil)))
      (should-error (plane-org-sync-config--get-api-key)
                    :type 'user-error)
      (should-not auth-source-called))))

;;;; Setup Request -- HTTP Response Handling

(defun plane-org-sync-config-test--make-http-buffer (status-code body-string)
  "Create a buffer simulating a url-retrieve-synchronously response.
STATUS-CODE is the HTTP status.  BODY-STRING is the response body."
  (let ((buf (generate-new-buffer " *test-http-response*")))
    (with-current-buffer buf
      (insert (format "HTTP/1.1 %d OK\r\n" status-code))
      (insert "Content-Type: application/json\r\n")
      (insert "\r\n")
      (let ((header-end (point)))
        (insert body-string)
        (setq-local url-http-end-of-headers header-end)
        (setq-local url-http-response-status status-code)))
    buf))

(defun plane-org-sync-config-test--make-http-buffer-no-headers
    (body-string)
  "Create a buffer simulating an incomplete HTTP response.
BODY-STRING is inserted but `url-http-end-of-headers' is left nil."
  (let ((buf (generate-new-buffer " *test-http-response*")))
    (with-current-buffer buf
      (insert body-string)
      (setq-local url-http-end-of-headers nil)
      (setq-local url-http-response-status nil))
    buf))

(ert-deftest plane-org-sync-config-test-setup-request-json-success ()
  "Setup request should return parsed JSON on 2xx with valid JSON."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (plane-org-sync-config-test--make-http-buffer
                200 "{\"id\": \"user-1\", \"email\": \"test@example.com\"}"))))
    (let ((result (plane-org-sync-config--setup-request
                   "https://plane.example.com" "test-key"
                   "/api/v1/users/me/")))
      (should (equal "user-1" (plist-get result :id)))
      (should (equal "test@example.com" (plist-get result :email))))))

(ert-deftest plane-org-sync-config-test-setup-request-nil-buffer ()
  "Setup request should signal error when url-retrieve returns nil."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args) nil)))
    (should-error (plane-org-sync-config--setup-request
                   "https://plane.example.com" "test-key"
                   "/api/v1/users/me/")
                  :type 'error)))

(ert-deftest plane-org-sync-config-test-setup-request-nil-buffer-message ()
  "Setup request nil buffer error should mention the host."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args) nil)))
    (condition-case err
        (plane-org-sync-config--setup-request
         "https://plane.example.com" "test-key" "/api/v1/users/me/")
      (error
       (should (string-match-p "plane\\.example\\.com"
                               (error-message-string err)))
       (should (string-match-p "url-retrieve returned nil"
                               (error-message-string err)))))))

(ert-deftest plane-org-sync-config-test-setup-request-nil-headers ()
  "Setup request should signal error when url-http-end-of-headers is nil."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (plane-org-sync-config-test--make-http-buffer-no-headers
                "incomplete response"))))
    (should-error (plane-org-sync-config--setup-request
                   "https://plane.example.com" "test-key"
                   "/api/v1/users/me/")
                  :type 'error)))

(ert-deftest plane-org-sync-config-test-setup-request-invalid-json ()
  "Setup request should signal descriptive error on non-JSON 200 response."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (plane-org-sync-config-test--make-http-buffer
                200 "<html><body>Welcome to Nginx</body></html>"))))
    (condition-case err
        (plane-org-sync-config--setup-request
         "https://plane.example.com" "test-key" "/api/v1/users/me/")
      (error
       (let ((msg (error-message-string err)))
         (should (string-match-p "not valid JSON" msg))
         (should (string-match-p "proxy, CDN, or misconfigured URL" msg))
         (should (string-match-p "plane\\.example\\.com" msg))
         (should (string-match-p "Nginx" msg)))))))

(ert-deftest plane-org-sync-config-test-setup-request-non-2xx ()
  "Setup request should include host and body preview in non-2xx errors."
  (cl-letf (((symbol-function 'url-retrieve-synchronously)
             (lambda (_url &rest _args)
               (plane-org-sync-config-test--make-http-buffer
                403 "{\"detail\": \"Authentication failed\"}"))))
    (condition-case err
        (plane-org-sync-config--setup-request
         "https://plane.example.com" "test-key" "/api/v1/users/me/")
      (error
       (let ((msg (error-message-string err)))
         (should (string-match-p "403" msg))
         (should (string-match-p "plane\\.example\\.com" msg))
         (should (string-match-p "Authentication failed" msg)))))))

(provide 'test-plane-org-sync-config)
;;; test-plane-org-sync-config.el ends here
