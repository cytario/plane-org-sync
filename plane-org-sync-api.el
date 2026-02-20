;;; plane-org-sync-api.el --- Plane API client for plane-org-sync  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Martin

;; Author: Martin
;; Maintainer: Martin
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; HTTP client for the Plane.so REST API.  Provides async request/paginate
;; for pull operations and synchronous request for push operations.
;; Handles authentication, pagination, rate limiting, and error recovery.
;;
;; Callback conventions:
;;
;; Single-resource functions (api-me, api-list-states, api-get-work-item,
;; api-update-work-item, api-create-work-item):
;;   CALLBACK receives (STATUS-CODE PARSED-BODY) on success, or
;;   (nil ERROR-MESSAGE) on error.
;;
;; Paginated list functions (api-list-work-items, api-list-projects,
;; api-list-labels):
;;   CALLBACK receives (RESULTS-LIST) on success -- a single argument
;;   containing the accumulated list of items.  On error, CALLBACK
;;   receives (nil ERROR-MESSAGE) -- two arguments where the first is
;;   nil and the second is the error string.

;;; Code:

(require 'url)
(require 'url-http)
(require 'seq)
(require 'plane-org-sync-config)

;; NOTE: Do NOT (require 'json) -- json-parse-buffer and json-serialize
;; are C builtins available since Emacs 27.

;; Declare dynamic variables from url-http to silence byte-compiler.
(defvar url-http-end-of-headers)
(defvar url-http-response-status)

;;;; Logging

(defconst plane-org-sync-api--log-buffer-name "*plane-org-sync-log*"
  "Name of the log buffer for API operations.")

(defun plane-org-sync-api--log (level format-string &rest args)
  "Log a timestamped message to the plane-org-sync log buffer.
LEVEL is a symbol such as `info', `warn', or `error'.
FORMAT-STRING and ARGS are passed to `format'."
  (let ((buf (get-buffer-create plane-org-sync-api--log-buffer-name))
        (msg (apply #'format format-string args))
        (timestamp (format-time-string "%Y-%m-%dT%H:%M:%S")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "[%s] [%s] %s\n" timestamp level msg)))))

;;;; URL Construction

(defun plane-org-sync-api--base-url ()
  "Return the base API URL for the configured Plane instance.
Strips any trailing slash from `plane-org-sync-instance-url' and
appends /api/v1."
  (concat (string-remove-suffix "/" plane-org-sync-instance-url)
          "/api/v1"))

(defun plane-org-sync-api--workspace-url ()
  "Return the workspace-scoped API URL.
Appends /workspaces/{slug} to the base URL."
  (concat (plane-org-sync-api--base-url)
          "/workspaces/" plane-org-sync-workspace))

(defun plane-org-sync-api--project-url (project-id)
  "Return the project-scoped API URL for PROJECT-ID.
Appends /projects/{project-id} to the workspace URL."
  (concat (plane-org-sync-api--workspace-url)
          "/projects/" project-id))

;;;; Core HTTP (async)

(defun plane-org-sync-api--request (method path &optional body callback)
  "Perform an async HTTP request to the Plane API.
METHOD is an HTTP method string (\"GET\", \"POST\", \"PATCH\", etc.).
PATH is appended to the base API URL.  BODY, when non-nil, is a plist
serialized to JSON.  CALLBACK is called with (STATUS-CODE PARSED-BODY)
on success, or (nil ERROR-MESSAGE) on error."
  (plane-org-sync-config--validate-instance-url)
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("X-API-Key" . ,(plane-org-sync-config--get-api-key))
            ("Content-Type" . "application/json")
            ("Accept" . "application/json")))
         (url-request-data (when body (json-serialize body)))
         (full-url (concat (plane-org-sync-api--base-url) path))
         (cb (or callback #'ignore)))
    (plane-org-sync-api--log 'info "%s %s" method full-url)
    (condition-case err
        (url-retrieve
         full-url
         (lambda (status)
           (let ((response-buf (current-buffer)))
             (unwind-protect
                 (condition-case err
                     (if (plist-get status :error)
                         (let ((msg (format "HTTP error: %s"
                                            (plist-get status :error))))
                           (plane-org-sync-api--log 'error "%s" msg)
                           (funcall cb nil msg))
                       (goto-char url-http-end-of-headers)
                       (let ((status-code url-http-response-status)
                             (parsed (json-parse-buffer
                                      :object-type 'plist
                                      :null-object nil
                                      :false-object nil)))
                         (plane-org-sync-api--log
                          'info "Response: %d" status-code)
                         (funcall cb status-code parsed)))
                   (json-parse-error
                    (let ((msg (format "JSON parse error: %s"
                                       (error-message-string err))))
                      (plane-org-sync-api--log 'error "%s" msg)
                      (funcall cb nil msg)))
                   (error
                    (let ((msg (format "Response processing error: %s"
                                       (error-message-string err))))
                      (plane-org-sync-api--log 'error "%s" msg)
                      (funcall cb nil msg))))
               (when (buffer-live-p response-buf)
                 (kill-buffer response-buf)))))
         nil t)  ; CBARGS=nil, SILENT=t
      (file-error
       (let ((msg (format "Network error: %s"
                          (error-message-string err))))
         (plane-org-sync-api--log 'error "%s" msg)
         (funcall cb nil msg)))
      (error
       (let ((msg (format "Request error: %s"
                          (error-message-string err))))
         (plane-org-sync-api--log 'error "%s" msg)
         (funcall cb nil msg))))))

;;;; Core HTTP (synchronous)

(defun plane-org-sync-api--request-sync (method path &optional body)
  "Perform a synchronous HTTP request to the Plane API.
METHOD is an HTTP method string.  PATH is appended to the base API URL.
BODY, when non-nil, is a plist serialized to JSON.  Returns parsed JSON
plist on success.  Signals error on failure.  Used for push operations
where blocking is acceptable.

Retries once on HTTP 429 after a 2-second backoff.  Total worst case
blocking time: 5s request + 2s backoff + 5s retry = 12s."
  (plane-org-sync-config--validate-instance-url)
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("X-API-Key" . ,(plane-org-sync-config--get-api-key))
            ("Content-Type" . "application/json")
            ("Accept" . "application/json")))
         (url-request-data (when body (json-serialize body)))
         (full-url (concat (plane-org-sync-api--base-url) path))
         (buffer (condition-case err
                     (url-retrieve-synchronously full-url nil nil 5)
                   (file-error
                    (plane-org-sync-api--log 'error "Network error: %s" err)
                    (signal 'error (list (format "Network error: %s"
                                                 (error-message-string err)))))
                   (error
                    (plane-org-sync-api--log 'error "Request error: %s" err)
                    (signal 'error (list (format "Request error: %s"
                                                 (error-message-string err))))))))
    (plane-org-sync-api--log 'info "SYNC %s %s" method full-url)
    (unless buffer
      (error "No response from %s (url-retrieve returned nil)"
             (url-host (url-generic-parse-url
                        plane-org-sync-instance-url))))
    (unwind-protect
        (with-current-buffer buffer
          (unless url-http-end-of-headers
            (error "Incomplete HTTP response from %s"
                   (url-host (url-generic-parse-url
                              plane-org-sync-instance-url))))
          (goto-char url-http-end-of-headers)
          (let ((status url-http-response-status)
                (body-start (point)))
            (cond
             ((<= 200 status 299)
              (condition-case _err
                  (json-parse-buffer :object-type 'plist
                                     :null-object nil
                                     :false-object nil)
                (json-parse-error
                 (let ((preview (buffer-substring
                                 body-start
                                 (min (+ body-start 300) (point-max)))))
                   (error "Plane API returned HTTP %d but the response is not valid JSON.\n\
This usually means a proxy, CDN, or misconfigured URL is returning an HTML page.\n\
Instance URL: %s\nResponse preview: %s"
                          status plane-org-sync-instance-url preview)))))
             ((= status 429)
              ;; Single retry after 2s backoff
              (kill-buffer buffer)
              (setq buffer nil)
              (plane-org-sync-api--log 'warn "429 rate limited, retrying in 2s")
              (sleep-for 2)
              (let ((retry-buf
                     (condition-case err
                         (url-retrieve-synchronously full-url nil nil 5)
                       (error
                        (signal 'error
                                (list (format "Retry failed: %s"
                                              (error-message-string err))))))))
                (unless retry-buf
                  (error "No response from %s on retry (url-retrieve returned nil)"
                         (url-host (url-generic-parse-url
                                    plane-org-sync-instance-url))))
                (unwind-protect
                    (with-current-buffer retry-buf
                      (unless url-http-end-of-headers
                        (error "Incomplete HTTP response from %s (retry)"
                               (url-host (url-generic-parse-url
                                          plane-org-sync-instance-url))))
                      (goto-char url-http-end-of-headers)
                      (let ((retry-status url-http-response-status)
                            (retry-body-start (point)))
                        (if (<= 200 retry-status 299)
                            (condition-case _err
                                (json-parse-buffer :object-type 'plist
                                                   :null-object nil
                                                   :false-object nil)
                              (json-parse-error
                               (let ((preview (buffer-substring
                                               retry-body-start
                                               (min (+ retry-body-start 300)
                                                    (point-max)))))
                                 (error "Plane API returned HTTP %d but the response is not valid JSON.\n\
This usually means a proxy, CDN, or misconfigured URL is returning an HTML page.\n\
Instance URL: %s\nResponse preview: %s"
                                        retry-status
                                        plane-org-sync-instance-url
                                        preview))))
                          (let ((preview (buffer-substring
                                          retry-body-start
                                          (min (+ retry-body-start 200)
                                               (point-max)))))
                            (error "Plane API error: HTTP %d from %s (after retry)\n\
Response preview: %s"
                                   retry-status
                                   (url-host (url-generic-parse-url
                                              plane-org-sync-instance-url))
                                   preview)))))
                  (when (buffer-live-p retry-buf)
                    (kill-buffer retry-buf)))))
             (t
              (let ((preview (buffer-substring
                              body-start
                              (min (+ body-start 200) (point-max)))))
                (error "Plane API error: HTTP %d from %s\nResponse preview: %s"
                       status
                       (url-host (url-generic-parse-url
                                  plane-org-sync-instance-url))
                       preview))))))
      (when (and buffer (buffer-live-p buffer))
        (kill-buffer buffer)))))

;;;; Rate Limit Handling (async)

(defun plane-org-sync-api--handle-429 (method path body callback retry-count)
  "Retry an async request after exponential backoff for HTTP 429.
METHOD, PATH, BODY, and CALLBACK are passed to `plane-org-sync-api--request'.
RETRY-COUNT tracks the current retry attempt (0-indexed).  Maximum 3 retries
with delays of 2s, 4s, 8s (2^(RETRY-COUNT+1) seconds)."
  (if (>= retry-count 3)
      (progn
        (plane-org-sync-api--log 'error
                                 "Rate limit: max retries exhausted for %s"
                                 path)
        (when callback
          (funcall callback nil "Rate limit exceeded after 3 retries")))
    (let ((delay (expt 2 (1+ retry-count))))
      (plane-org-sync-api--log 'warn
                               "429 rate limited on %s, retry %d in %ds"
                               path (1+ retry-count) delay)
      (run-at-time delay nil
                   (lambda ()
                     (plane-org-sync-api--request-with-retry
                      method path body callback (1+ retry-count)))))))

(defun plane-org-sync-api--request-with-retry
    (method path &optional body callback retry-count)
  "Perform an async HTTP METHOD request to PATH with 429 retry handling.
BODY is an optional plist serialized to JSON.  CALLBACK receives
\(STATUS-CODE PARSED-BODY) or (nil ERROR-MESSAGE).  RETRY-COUNT
tracks retries; 429 responses trigger exponential backoff."
  (let ((retries (or retry-count 0)))
    (plane-org-sync-config--validate-instance-url)
    (let* ((url-request-method method)
           (url-request-extra-headers
            `(("X-API-Key" . ,(plane-org-sync-config--get-api-key))
              ("Content-Type" . "application/json")
              ("Accept" . "application/json")))
           (url-request-data (when body (json-serialize body)))
           (full-url (concat (plane-org-sync-api--base-url) path))
           (cb (or callback #'ignore)))
      (plane-org-sync-api--log 'info "%s %s (retry=%d)" method full-url retries)
      (condition-case err
          (url-retrieve
           full-url
           (lambda (status)
             (let ((response-buf (current-buffer)))
               (unwind-protect
                   (condition-case err
                       (if (plist-get status :error)
                           (let ((msg (format "HTTP error: %s"
                                              (plist-get status :error))))
                             (plane-org-sync-api--log 'error "%s" msg)
                             (funcall cb nil msg))
                         (goto-char url-http-end-of-headers)
                         (let ((status-code url-http-response-status))
                           (if (= status-code 429)
                               (plane-org-sync-api--handle-429
                                method path body cb retries)
                             (let ((parsed
                                    (json-parse-buffer
                                     :object-type 'plist
                                     :null-object nil
                                     :false-object nil)))
                               (plane-org-sync-api--log
                                'info "Response: %d" status-code)
                               (funcall cb status-code parsed)))))
                     (json-parse-error
                      (let ((msg (format "JSON parse error: %s"
                                         (error-message-string err))))
                        (plane-org-sync-api--log 'error "%s" msg)
                        (funcall cb nil msg)))
                     (error
                      (let ((msg (format "Response processing error: %s"
                                         (error-message-string err))))
                        (plane-org-sync-api--log 'error "%s" msg)
                        (funcall cb nil msg))))
                 (when (buffer-live-p response-buf)
                   (kill-buffer response-buf)))))
           nil t)
        (file-error
         (let ((msg (format "Network error: %s"
                            (error-message-string err))))
           (plane-org-sync-api--log 'error "%s" msg)
           (funcall cb nil msg)))
        (error
         (let ((msg (format "Request error: %s"
                            (error-message-string err))))
           (plane-org-sync-api--log 'error "%s" msg)
           (funcall cb nil msg)))))))

;;;; Pagination

(defun plane-org-sync-api--paginate (path &optional params callback)
  "Fetch all pages from a paginated Plane API GET endpoint.
PATH is the API path.  PARAMS is an optional alist of query parameters.
CALLBACK is called with the full accumulated results list when all
pages have been collected, or with (nil ERROR-MESSAGE) on error.

Assumes Plane pagination response shape:
  {:results [...] :next_cursor \"...\" :next_page_results t/nil}"
  (let ((query-string
         (when params
           (mapconcat (lambda (pair)
                        (concat (url-hexify-string (car pair))
                                "="
                                (url-hexify-string (cdr pair))))
                      params "&"))))
    (plane-org-sync-api--paginate-accumulate
     path query-string nil callback)))

(defun plane-org-sync-api--paginate-accumulate
    (path query-string accumulated callback)
  "Accumulate paginated results across pages.
PATH is the API path.  QUERY-STRING is the current query string.
ACCUMULATED is the list of results collected so far.  CALLBACK is
called with the full results list when done."
  (let* ((separator (if (string-match-p "\\?" path) "&" "?"))
         (full-path (if query-string
                        (concat path separator query-string)
                      path)))
    (plane-org-sync-api--request-with-retry
     "GET" full-path nil
     (lambda (status-code body)
       (if (null status-code)
           ;; Error case: body is the error message
           (funcall callback nil body)
         (let* ((results (plist-get body :results))
                (page-items (if (vectorp results)
                                (append results nil)
                              results))
                (all-items (append accumulated page-items))
                (next-cursor (plist-get body :next_cursor))
                (has-next (plist-get body :next_page_results)))
           (if (and has-next next-cursor)
               ;; More pages: recurse with cursor
               (let ((cursor-qs (concat "cursor="
                                        (url-hexify-string next-cursor))))
                 (plane-org-sync-api--paginate-accumulate
                  path
                  (if query-string
                      (concat query-string "&" cursor-qs)
                    cursor-qs)
                  all-items callback))
             ;; Last page: deliver all results
             (funcall callback all-items))))))))

;;;; Resource Functions

(defun plane-org-sync-api-me (callback)
  "Fetch the authenticated user's profile.
CALLBACK is called with (STATUS-CODE USER-PLIST) on success, or
\(nil ERROR-MESSAGE) on error."
  (plane-org-sync-api--request-with-retry "GET" "/users/me/" nil callback))

(defun plane-org-sync-api-list-states (project-id callback)
  "Fetch workflow states for PROJECT-ID.
CALLBACK is called with the list of state plists on success, or
\(nil ERROR-MESSAGE) on error."
  (let ((path (concat (plane-org-sync-api--workspace-url)
                      "/projects/" project-id "/states/")))
    ;; States are not paginated (typically <20 per project),
    ;; but extract :results if wrapped.
    (plane-org-sync-api--request-with-retry
     "GET"
     (string-remove-prefix (plane-org-sync-api--base-url) path)
     nil
     (lambda (status-code body)
       (if (null status-code)
           (funcall callback nil body)
         (let ((results (plist-get body :results)))
           (funcall callback status-code
                    (if results
                        (if (vectorp results)
                            (append results nil)
                          results)
                      ;; If no :results wrapper, body might be a vector
                      (if (vectorp body)
                          (append body nil)
                        body)))))))))

(defun plane-org-sync-api-list-work-items (project-id &optional params callback)
  "Fetch work items for PROJECT-ID, filtering drafts and archived items.
PARAMS is an optional alist of extra query parameters.  The
?expand=state,labels,assignees parameter is always added.
CALLBACK is called with the filtered results list on success, or
\(nil ERROR-MESSAGE) on error."
  (let* ((base-path (concat "/workspaces/" plane-org-sync-workspace
                            "/projects/" project-id "/work-items/"))
         (expand-params (cons (cons "expand" "state,labels,assignees")
                              (or params nil))))
    (plane-org-sync-api--paginate
     base-path expand-params
     (lambda (results &optional error-msg)
       (if (and (null results) error-msg)
           (funcall callback nil error-msg)
         (let ((filtered
                (seq-remove
                 (lambda (item)
                   (or (eq t (plist-get item :is_draft))
                       (plist-get item :archived_at)))
                 (or results '()))))
           (funcall callback filtered)))))))

(defun plane-org-sync-api-get-work-item (project-id item-id callback)
  "Fetch a single work item ITEM-ID from PROJECT-ID.
CALLBACK is called with (STATUS-CODE ITEM-PLIST) on success, or
\(nil ERROR-MESSAGE) on error."
  (let ((path (concat "/workspaces/" plane-org-sync-workspace
                      "/projects/" project-id
                      "/work-items/" item-id "/")))
    (plane-org-sync-api--request-with-retry "GET" path nil callback)))

(defun plane-org-sync-api-update-work-item (project-id item-id data callback)
  "Update work item ITEM-ID in PROJECT-ID with DATA.
DATA is a plist of fields to update (e.g., (:state \"uuid\")).
CALLBACK is called with (STATUS-CODE RESPONSE-PLIST) on success, or
\(nil ERROR-MESSAGE) on error."
  (let ((path (concat "/workspaces/" plane-org-sync-workspace
                      "/projects/" project-id
                      "/work-items/" item-id "/")))
    (plane-org-sync-api--request-with-retry "PATCH" path data callback)))

(defun plane-org-sync-api-list-projects (callback)
  "Fetch projects in the configured workspace.
CALLBACK is called with the list of project plists on success, or
\(nil ERROR-MESSAGE) on error."
  (let ((path (concat "/workspaces/" plane-org-sync-workspace
                      "/projects/")))
    (plane-org-sync-api--paginate path nil callback)))

(defun plane-org-sync-api-list-labels (project-id callback)
  "Fetch labels for PROJECT-ID.
CALLBACK is called with the list of label plists on success, or
\(nil ERROR-MESSAGE) on error."
  (let ((path (concat "/workspaces/" plane-org-sync-workspace
                      "/projects/" project-id "/labels/")))
    (plane-org-sync-api--paginate path nil callback)))

(defun plane-org-sync-api-create-work-item (project-id data callback)
  "Create a new work item in PROJECT-ID with DATA.
DATA is a plist with fields like :name, :state, :priority, :labels,
:assignees.  CALLBACK is called with (STATUS-CODE RESPONSE-PLIST) on
success, or (nil ERROR-MESSAGE) on error."
  (let ((path (concat "/workspaces/" plane-org-sync-workspace
                      "/projects/" project-id "/work-items/")))
    (plane-org-sync-api--request-with-retry "POST" path data callback)))

(provide 'plane-org-sync-api)
;;; plane-org-sync-api.el ends here
