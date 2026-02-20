;;; test-plane-org-sync-api.el --- Tests for plane-org-sync-api  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Martin

;; Author: Martin
;; Maintainer: Martin
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the API client module of plane-org-sync.
;; Uses cl-letf to mock HTTP functions and verify behavior without
;; network access.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'plane-org-sync-api)

;;;; Test Helpers

(defmacro plane-org-sync-test-with-config (&rest body)
  "Execute BODY with standard test configuration bindings."
  (declare (indent 0) (debug body))
  `(let ((plane-org-sync-instance-url "https://plane.example.com")
         (plane-org-sync-api-key "test-api-key-123")
         (plane-org-sync-workspace "test-workspace"))
     ,@body))

(defmacro plane-org-sync-test-with-mock-api (request-fn &rest body)
  "Execute BODY with API request functions overridden by REQUEST-FN.
REQUEST-FN receives (METHOD PATH BODY CALLBACK) and should call CALLBACK
appropriately.  Both `--request' and `--request-with-retry' are mocked
so resource functions work regardless of which they call.
Test configuration is also bound."
  (declare (indent 1) (debug (form body)))
  `(plane-org-sync-test-with-config
     (cl-letf (((symbol-function 'plane-org-sync-api--request)
                ,request-fn)
               ((symbol-function 'plane-org-sync-api--request-with-retry)
                ,request-fn))
       ,@body)))

;;;; Feature Provided

(ert-deftest plane-org-sync-api-test-feature-provided ()
  "The `plane-org-sync-api' feature should be available."
  (should (featurep 'plane-org-sync-api)))

;;;; URL Construction

(ert-deftest plane-org-sync-api-test-base-url ()
  "Base URL should strip trailing slash and append /api/v1."
  (let ((plane-org-sync-instance-url "https://plane.example.com"))
    (should (equal "https://plane.example.com/api/v1"
                   (plane-org-sync-api--base-url))))
  ;; With trailing slash
  (let ((plane-org-sync-instance-url "https://plane.example.com/"))
    (should (equal "https://plane.example.com/api/v1"
                   (plane-org-sync-api--base-url)))))

(ert-deftest plane-org-sync-api-test-workspace-url ()
  "Workspace URL should include the workspace slug."
  (plane-org-sync-test-with-config
    (should (equal "https://plane.example.com/api/v1/workspaces/test-workspace"
                   (plane-org-sync-api--workspace-url)))))

(ert-deftest plane-org-sync-api-test-project-url ()
  "Project URL should include workspace and project ID."
  (plane-org-sync-test-with-config
    (should (equal (concat "https://plane.example.com/api/v1"
                           "/workspaces/test-workspace"
                           "/projects/proj-uuid-123")
                   (plane-org-sync-api--project-url "proj-uuid-123")))))

;;;; Logging

(ert-deftest plane-org-sync-api-test-log-creates-buffer ()
  "Logging should create the log buffer and insert timestamped entries."
  (let ((buf-name plane-org-sync-api--log-buffer-name))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (plane-org-sync-api--log 'info "Test message %d" 42)
    (let ((buf (get-buffer buf-name)))
      (should buf)
      (with-current-buffer buf
        (should (string-match-p "\\[info\\] Test message 42"
                                (buffer-string))))
      (kill-buffer buf))))

(ert-deftest plane-org-sync-api-test-log-multiple-entries ()
  "Multiple log entries should appear in order."
  (let ((buf-name plane-org-sync-api--log-buffer-name))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (plane-org-sync-api--log 'info "First")
    (plane-org-sync-api--log 'warn "Second")
    (plane-org-sync-api--log 'error "Third")
    (let ((buf (get-buffer buf-name)))
      (should buf)
      (with-current-buffer buf
        (let ((content (buffer-string)))
          (should (string-match-p "\\[info\\] First" content))
          (should (string-match-p "\\[warn\\] Second" content))
          (should (string-match-p "\\[error\\] Third" content))
          ;; Verify ordering
          (should (< (string-match "First" content)
                     (string-match "Second" content)))
          (should (< (string-match "Second" content)
                     (string-match "Third" content)))))
      (kill-buffer buf))))

(ert-deftest plane-org-sync-api-test-log-timestamp-format ()
  "Log entries should have ISO 8601 timestamps."
  (let ((buf-name plane-org-sync-api--log-buffer-name))
    (when (get-buffer buf-name)
      (kill-buffer buf-name))
    (plane-org-sync-api--log 'info "Timestamped")
    (with-current-buffer (get-buffer buf-name)
      (should (string-match-p
               "\\[[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}T[0-9]\\{2\\}:[0-9]\\{2\\}:[0-9]\\{2\\}\\]"
               (buffer-string))))
    (kill-buffer buf-name)))

;;;; Synchronous Request -- Mock Tests

(defun plane-org-sync-test--make-http-buffer (status-code body-string)
  "Create a buffer simulating a url-retrieve-synchronously response.
STATUS-CODE is the HTTP status.  BODY-STRING is the response body."
  (let ((buf (generate-new-buffer " *test-http-response*")))
    (with-current-buffer buf
      (insert (format "HTTP/1.1 %d OK\r\n" status-code))
      (insert "Content-Type: application/json\r\n")
      (insert "\r\n")
      (let ((header-end (point)))
        (insert body-string)
        ;; Set url.el variables
        (setq-local url-http-end-of-headers header-end)
        (setq-local url-http-response-status status-code)))
    buf))

(ert-deftest plane-org-sync-api-test-request-sync-success ()
  "Synchronous request should parse and return JSON on 2xx."
  (plane-org-sync-test-with-config
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _args)
                 (plane-org-sync-test--make-http-buffer
                  200 "{\"id\": \"abc\", \"name\": \"Test\"}"))))
      (let ((result (plane-org-sync-api--request-sync
                     "GET" "/users/me/")))
        (should (equal "abc" (plist-get result :id)))
        (should (equal "Test" (plist-get result :name)))))))

(ert-deftest plane-org-sync-api-test-request-sync-201 ()
  "Synchronous request should accept any 2xx status."
  (plane-org-sync-test-with-config
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _args)
                 (plane-org-sync-test--make-http-buffer
                  201 "{\"id\": \"new-item\"}"))))
      (let ((result (plane-org-sync-api--request-sync
                     "POST" "/workspaces/ws/projects/p/work-items/"
                     '(:name "New Item"))))
        (should (equal "new-item" (plist-get result :id)))))))

(ert-deftest plane-org-sync-api-test-request-sync-429-retry ()
  "Synchronous request should retry once on 429 after 2s backoff."
  (plane-org-sync-test-with-config
    (let ((call-count 0))
      (cl-letf (((symbol-function 'url-retrieve-synchronously)
                 (lambda (_url &rest _args)
                   (cl-incf call-count)
                   (if (= call-count 1)
                       (plane-org-sync-test--make-http-buffer
                        429 "{\"detail\": \"rate limited\"}")
                     (plane-org-sync-test--make-http-buffer
                      200 "{\"id\": \"ok\"}"))))
                ((symbol-function 'sleep-for)
                 (lambda (_seconds) nil)))  ; skip actual sleep in tests
        (let ((result (plane-org-sync-api--request-sync
                       "GET" "/users/me/")))
          (should (equal 2 call-count))
          (should (equal "ok" (plist-get result :id))))))))

(ert-deftest plane-org-sync-api-test-request-sync-429-still-429 ()
  "Synchronous request should signal error if retry also returns 429."
  (plane-org-sync-test-with-config
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _args)
                 (plane-org-sync-test--make-http-buffer
                  429 "{\"detail\": \"rate limited\"}")))
              ((symbol-function 'sleep-for)
               (lambda (_seconds) nil)))
      (should-error (plane-org-sync-api--request-sync "GET" "/users/me/")
                    :type 'error))))

(ert-deftest plane-org-sync-api-test-request-sync-network-error ()
  "Synchronous request should signal error on network failure."
  (plane-org-sync-test-with-config
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _args)
                 (signal 'file-error '("connection refused")))))
      (should-error (plane-org-sync-api--request-sync "GET" "/users/me/")
                    :type 'error))))

(ert-deftest plane-org-sync-api-test-request-sync-malformed-json ()
  "Synchronous request should signal error on malformed JSON."
  (plane-org-sync-test-with-config
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _args)
                 (plane-org-sync-test--make-http-buffer
                  200 "<html>Not JSON</html>"))))
      (should-error (plane-org-sync-api--request-sync "GET" "/users/me/")
                    :type 'error))))

(ert-deftest plane-org-sync-api-test-request-sync-server-error ()
  "Synchronous request should signal error on 5xx."
  (plane-org-sync-test-with-config
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _args)
                 (plane-org-sync-test--make-http-buffer
                  500 "{\"detail\": \"internal error\"}"))))
      (should-error (plane-org-sync-api--request-sync "GET" "/users/me/")
                    :type 'error))))

(ert-deftest plane-org-sync-api-test-request-sync-404 ()
  "Synchronous request should signal error on 404."
  (plane-org-sync-test-with-config
    (cl-letf (((symbol-function 'url-retrieve-synchronously)
               (lambda (_url &rest _args)
                 (plane-org-sync-test--make-http-buffer
                  404 "{\"detail\": \"not found\"}"))))
      (should-error (plane-org-sync-api--request-sync "GET" "/bad/path/")
                    :type 'error))))

;;;; Async Request -- api-me

(ert-deftest plane-org-sync-api-test-me-callback ()
  "api-me should invoke callback with status-code and parsed body."
  (let ((received-status nil)
        (received-body nil))
    (plane-org-sync-test-with-mock-api
      (lambda (method path _body callback)
        (should (equal "GET" method))
        (should (string-suffix-p "/users/me/" path))
        (funcall callback 200 '(:id "user-uuid" :display_name "Test")))
      (plane-org-sync-api-me
       (lambda (status body)
         (setq received-status status
               received-body body)))
      (should (equal 200 received-status))
      (should (equal "user-uuid" (plist-get received-body :id))))))

(ert-deftest plane-org-sync-api-test-me-error ()
  "api-me should pass errors through to callback."
  (let ((received-status nil)
        (received-error nil))
    (plane-org-sync-test-with-mock-api
      (lambda (_method _path _body callback)
        (funcall callback nil "Connection refused"))
      (plane-org-sync-api-me
       (lambda (status body)
         (setq received-status status
               received-error body)))
      (should-not received-status)
      (should (equal "Connection refused" received-error)))))

;;;; List Work Items -- Draft/Archived Filtering

(ert-deftest plane-org-sync-api-test-list-work-items-filters-drafts ()
  "list-work-items should remove draft items."
  (let ((received nil))
    (plane-org-sync-test-with-mock-api
      (lambda (_method _path _body callback)
        (funcall callback 200
                 '(:results [(:id "1" :name "Active" :is_draft nil :archived_at nil)
                             (:id "2" :name "Draft" :is_draft t :archived_at nil)
                             (:id "3" :name "Normal" :is_draft nil :archived_at nil)]
                   :next_page_results nil)))
      ;; Override --paginate to use our mock --request
      (cl-letf (((symbol-function 'plane-org-sync-api--paginate)
                 (lambda (path params callback)
                   (let ((query (mapconcat
                                 (lambda (p)
                                   (concat (car p) "=" (cdr p)))
                                 params "&")))
                     (plane-org-sync-api--request
                      "GET"
                      (if (string-empty-p query) path
                        (concat path "?" query))
                      nil
                      (lambda (status-code body)
                        (if (null status-code)
                            (funcall callback nil body)
                          (let* ((results (plist-get body :results))
                                 (items (if (vectorp results)
                                            (append results nil)
                                          results)))
                            (funcall callback items)))))))))
        (plane-org-sync-api-list-work-items
         "proj-id" nil
         (lambda (items &optional _err)
           (setq received items)))
        (should (equal 2 (length received)))
        (should (equal "1" (plist-get (car received) :id)))
        (should (equal "3" (plist-get (cadr received) :id)))))))

(ert-deftest plane-org-sync-api-test-list-work-items-filters-archived ()
  "list-work-items should remove archived items."
  (let ((received nil))
    (plane-org-sync-test-with-mock-api
      (lambda (_method _path _body callback)
        (funcall callback 200
                 '(:results [(:id "1" :name "Active" :is_draft nil :archived_at nil)
                             (:id "2" :name "Archived" :is_draft nil :archived_at "2026-01-01")]
                   :next_page_results nil)))
      (cl-letf (((symbol-function 'plane-org-sync-api--paginate)
                 (lambda (_path _params callback)
                   (plane-org-sync-api--request
                    "GET" "/dummy" nil
                    (lambda (status-code body)
                      (if (null status-code)
                          (funcall callback nil body)
                        (let* ((results (plist-get body :results))
                               (items (if (vectorp results)
                                          (append results nil)
                                        results)))
                          (funcall callback items))))))))
        (plane-org-sync-api-list-work-items
         "proj-id" nil
         (lambda (items &optional _err)
           (setq received items)))
        (should (equal 1 (length received)))
        (should (equal "1" (plist-get (car received) :id)))))))

(ert-deftest plane-org-sync-api-test-list-work-items-empty ()
  "list-work-items should handle empty results."
  (let ((received nil)
        (called nil))
    (plane-org-sync-test-with-mock-api
      (lambda (_method _path _body callback)
        (funcall callback 200
                 '(:results [] :next_page_results nil)))
      (cl-letf (((symbol-function 'plane-org-sync-api--paginate)
                 (lambda (_path _params callback)
                   (plane-org-sync-api--request
                    "GET" "/dummy" nil
                    (lambda (status-code body)
                      (if (null status-code)
                          (funcall callback nil body)
                        (let* ((results (plist-get body :results))
                               (items (if (vectorp results)
                                          (append results nil)
                                        results)))
                          (funcall callback items))))))))
        (plane-org-sync-api-list-work-items
         "proj-id" nil
         (lambda (items &optional _err)
           (setq received items
                 called t)))
        (should called)
        (should (equal 0 (length received)))))))

;;;; List States

(ert-deftest plane-org-sync-api-test-list-states ()
  "list-states should invoke callback with states."
  (let ((received-status nil)
        (received-body nil))
    (plane-org-sync-test-with-mock-api
      (lambda (method path _body callback)
        (should (equal "GET" method))
        (should (string-match-p "/states/" path))
        (funcall callback 200
                 '(:results [(:id "s1" :name "Backlog" :group "backlog")
                             (:id "s2" :name "In Progress" :group "started")])))
      (plane-org-sync-api-list-states
       "proj-id"
       (lambda (status body)
         (setq received-status status
               received-body body)))
      (should (equal 200 received-status))
      (should (equal 2 (length received-body)))
      (should (equal "s1" (plist-get (car received-body) :id))))))

;;;; List Projects

(ert-deftest plane-org-sync-api-test-list-projects ()
  "list-projects should paginate and return project list."
  (let ((received nil))
    (plane-org-sync-test-with-config
      (cl-letf (((symbol-function 'plane-org-sync-api--paginate)
                 (lambda (path _params callback)
                   (should (string-match-p "/projects/" path))
                   (funcall callback
                            '((:id "p1" :name "Project One")
                              (:id "p2" :name "Project Two"))))))
        (plane-org-sync-api-list-projects
         (lambda (items)
           (setq received items)))
        (should (equal 2 (length received)))))))

;;;; Update Work Item

(ert-deftest plane-org-sync-api-test-update-work-item ()
  "update-work-item should send PATCH with data."
  (let ((received-method nil)
        (received-body nil)
        (received-status nil))
    (plane-org-sync-test-with-mock-api
      (lambda (method path body callback)
        (setq received-method method
              received-body body)
        (should (string-match-p "/work-items/item-1/" path))
        (funcall callback 200 '(:id "item-1" :state "new-state")))
      (plane-org-sync-api-update-work-item
       "proj-id" "item-1" '(:state "new-state-uuid")
       (lambda (status _body)
         (setq received-status status)))
      (should (equal "PATCH" received-method))
      (should (equal '(:state "new-state-uuid") received-body))
      (should (equal 200 received-status)))))

;;;; Create Work Item

(ert-deftest plane-org-sync-api-test-create-work-item ()
  "create-work-item should send POST with data."
  (let ((received-method nil)
        (received-status nil))
    (plane-org-sync-test-with-mock-api
      (lambda (method _path _body callback)
        (setq received-method method)
        (funcall callback 201 '(:id "new-id" :name "New Item")))
      (plane-org-sync-api-create-work-item
       "proj-id" '(:name "New Item" :state "state-uuid")
       (lambda (status _body)
         (setq received-status status)))
      (should (equal "POST" received-method))
      (should (equal 201 received-status)))))

;;;; Get Work Item

(ert-deftest plane-org-sync-api-test-get-work-item ()
  "get-work-item should fetch a single item by ID."
  (let ((received-status nil)
        (received-body nil))
    (plane-org-sync-test-with-mock-api
      (lambda (method path _body callback)
        (should (equal "GET" method))
        (should (string-match-p "/work-items/item-99/" path))
        (funcall callback 200 '(:id "item-99" :name "Single Item")))
      (plane-org-sync-api-get-work-item
       "proj-id" "item-99"
       (lambda (status body)
         (setq received-status status
               received-body body)))
      (should (equal 200 received-status))
      (should (equal "item-99" (plist-get received-body :id))))))

;;;; List Labels

(ert-deftest plane-org-sync-api-test-list-labels ()
  "list-labels should paginate and return label list."
  (let ((received nil))
    (plane-org-sync-test-with-config
      (cl-letf (((symbol-function 'plane-org-sync-api--paginate)
                 (lambda (path _params callback)
                   (should (string-match-p "/labels/" path))
                   (funcall callback
                            '((:id "l1" :name "Bug" :color "#ff0000")
                              (:id "l2" :name "Feature" :color "#00ff00"))))))
        (plane-org-sync-api-list-labels
         "proj-id"
         (lambda (items)
           (setq received items)))
        (should (equal 2 (length received)))
        (should (equal "Bug" (plist-get (car received) :name)))))))

;;;; Pagination

(ert-deftest plane-org-sync-api-test-paginate-single-page ()
  "Paginate should handle single-page responses."
  (let ((received nil))
    (plane-org-sync-test-with-config
      (cl-letf (((symbol-function 'plane-org-sync-api--request-with-retry)
                 (lambda (_method _path _body callback &optional _retry)
                   (funcall callback 200
                            '(:results [(:id "a") (:id "b")]
                              :next_page_results nil
                              :next_cursor nil)))))
        (plane-org-sync-api--paginate
         "/test/path/" nil
         (lambda (results)
           (setq received results)))
        (should (equal 2 (length received)))
        (should (equal "a" (plist-get (car received) :id)))))))

(ert-deftest plane-org-sync-api-test-paginate-multi-page ()
  "Paginate should accumulate results across multiple pages."
  (let ((received nil)
        (call-count 0))
    (plane-org-sync-test-with-config
      (cl-letf (((symbol-function 'plane-org-sync-api--request-with-retry)
                 (lambda (_method path _body callback &optional _retry)
                   (cl-incf call-count)
                   (if (= call-count 1)
                       ;; First page
                       (funcall callback 200
                                '(:results [(:id "a") (:id "b")]
                                  :next_page_results t
                                  :next_cursor "cursor-page2"))
                     ;; Second page (last)
                     (progn
                       (should (string-match-p "cursor=cursor-page2" path))
                       (funcall callback 200
                                '(:results [(:id "c")]
                                  :next_page_results nil
                                  :next_cursor nil)))))))
        (plane-org-sync-api--paginate
         "/test/path/" nil
         (lambda (results)
           (setq received results)))
        (should (equal 3 (length received)))
        (should (equal "a" (plist-get (nth 0 received) :id)))
        (should (equal "b" (plist-get (nth 1 received) :id)))
        (should (equal "c" (plist-get (nth 2 received) :id)))))))

(ert-deftest plane-org-sync-api-test-paginate-error ()
  "Paginate should propagate errors from --request."
  (let ((received-err nil))
    (plane-org-sync-test-with-config
      (cl-letf (((symbol-function 'plane-org-sync-api--request-with-retry)
                 (lambda (_method _path _body callback &optional _retry)
                   (funcall callback nil "Connection refused"))))
        (plane-org-sync-api--paginate
         "/test/path/" nil
         (lambda (results &optional err)
           (when (null results)
             (setq received-err err))))
        (should (equal "Connection refused" received-err))))))

;;;; HTTPS Validation in Requests

(ert-deftest plane-org-sync-api-test-request-rejects-http ()
  "Async request should signal `user-error' for HTTP URLs."
  (let ((plane-org-sync-instance-url "http://insecure.example.com")
        (plane-org-sync-api-key "key")
        (plane-org-sync-workspace "ws"))
    (should-error (plane-org-sync-api--request
                   "GET" "/users/me/" nil #'ignore)
                  :type 'user-error)))

(ert-deftest plane-org-sync-api-test-request-sync-rejects-http ()
  "Synchronous request should reject HTTP URLs."
  (let ((plane-org-sync-instance-url "http://insecure.example.com")
        (plane-org-sync-api-key "key"))
    (should-error (plane-org-sync-api--request-sync "GET" "/users/me/")
                  :type 'user-error)))

;;;; Paginate with Query Params

(ert-deftest plane-org-sync-api-test-paginate-with-params ()
  "Paginate should include query parameters in the request."
  (let ((captured-path nil))
    (plane-org-sync-test-with-config
      (cl-letf (((symbol-function 'plane-org-sync-api--request-with-retry)
                 (lambda (_method path _body callback &optional _retry)
                   (setq captured-path path)
                   (funcall callback 200
                            '(:results [] :next_page_results nil)))))
        (plane-org-sync-api--paginate
         "/test/path/"
         '(("expand" . "state,labels") ("assignees" . "user-id"))
         (lambda (_results) nil))
        (should (string-match-p "expand=state%2Clabels" captured-path))
        (should (string-match-p "assignees=user-id" captured-path))))))

(provide 'test-plane-org-sync-api)
;;; test-plane-org-sync-api.el ends here
