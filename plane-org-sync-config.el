;;; plane-org-sync-config.el --- Configuration for plane-org-sync  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Martin

;; Author: Martin
;; Maintainer: Martin
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration variables and helpers for the plane-org-sync package.
;; Provides defcustom declarations for all user-facing settings, HTTPS
;; validation, and auth-source integration for API key retrieval.

;;; Code:

(require 'auth-source)
(require 'url-parse)

;;;; Customization Group

(defgroup plane-org-sync nil
  "Synchronize Plane.so work items with Org-mode."
  :group 'comm
  :group 'convenience
  :prefix "plane-org-sync-")

;;;; Connection Settings

(defcustom plane-org-sync-instance-url "https://app.plane.so"
  "Base URL of the Plane instance.
Must use HTTPS.  Do not include a trailing slash."
  :type 'string
  :group 'plane-org-sync
  :safe #'stringp)

(defcustom plane-org-sync-api-key nil
  "Plane API key.
If nil, the key is retrieved from `auth-source' using the instance URL host.
If a non-empty string, used directly as the API key."
  :type '(choice (const :tag "Use auth-source" nil)
                 (string :tag "API key string"))
  :group 'plane-org-sync)

(defcustom plane-org-sync-workspace nil
  "Plane workspace slug.
The slug portion of the Plane URL, e.g. for
`https://app.plane.so/my-team/', the slug is \"my-team\"."
  :type '(choice (const :tag "Not configured" nil)
                 (string :tag "Workspace slug"))
  :group 'plane-org-sync
  :safe #'stringp)

;;;; Project Settings

(defcustom plane-org-sync-projects nil
  "List of Plane project UUIDs to synchronize."
  :type '(repeat string)
  :group 'plane-org-sync)

(defcustom plane-org-sync-file "~/org/plane.org"
  "Path to the Org file used for synced Plane work items.
The file is created if it does not exist."
  :type 'file
  :group 'plane-org-sync
  :safe #'stringp)

(defcustom plane-org-sync-filter-assignee t
  "When non-nil, sync only work items assigned to the authenticated user."
  :type 'boolean
  :group 'plane-org-sync
  :safe #'booleanp)

(defcustom plane-org-sync-auto-interval nil
  "Interval in seconds for automatic sync, or nil to disable.
When non-nil, `plane-org-sync-mode' sets up a repeating timer."
  :type '(choice (const :tag "Disabled" nil)
                 (integer :tag "Seconds"))
  :group 'plane-org-sync
  :safe (lambda (v) (or (null v) (and (integerp v) (> v 0)))))

;;;; State Mapping

(defcustom plane-org-sync-group-keyword-mapping
  '((backlog   . "BACKLOG")
    (unstarted . "TODO")
    (started   . "STARTED")
    (completed . "DONE")
    (cancelled . "CANCELLED"))
  "Alist mapping Plane state groups to Org TODO keywords.
Each entry is (GROUP-SYMBOL . KEYWORD-STRING).  The five Plane state
groups are: backlog, unstarted, started, completed, cancelled."
  :type '(alist :key-type symbol :value-type string)
  :group 'plane-org-sync)

(defcustom plane-org-sync-state-mapping nil
  "Alist of Plane state name overrides.
Each entry is (STATE-NAME-STRING . KEYWORD-STRING).  States listed
here override the group-based default from
`plane-org-sync-group-keyword-mapping'.  Example:

  \\='((\"In Review\" . \"REVIEW\")
    (\"QA\"        . \"TESTING\"))"
  :type '(alist :key-type string :value-type string)
  :group 'plane-org-sync)

;;;; Conflict Resolution

(defcustom plane-org-sync-conflict-function #'y-or-n-p
  "Function called to resolve sync conflicts.
Called with a prompt string.  Should return non-nil to proceed
with the push, nil to cancel.  The default uses `y-or-n-p'."
  :type 'function
  :group 'plane-org-sync
  :risky t)

;;;; Internal Variables

(defvar plane-org-sync--inhibit-push nil
  "When non-nil, suppress the push hook.
Bound to t during pull operations to prevent push-back loops.")

(defvar plane-org-sync--last-sync nil
  "Timestamp of the last successful sync, or nil.")

(defvar plane-org-sync--last-sync-result nil
  "Plist describing the result of the last sync operation, or nil.
Example: (:created 5 :updated 2 :unchanged 10).")

(defvar plane-org-sync--user-id nil
  "Plane user ID of the authenticated user, or nil.
Populated by `plane-org-sync-api-me' during sync.")

(defvar plane-org-sync--state-cache nil
  "Alist of (PROJECT-ID . STATES-LIST) for cached Plane states.
Each STATES-LIST element is a plist with :id, :name, :group keys.
Populated at the start of each pull; invalidated on reset or
project configuration change.")

;;;; HTTPS Validation

(defun plane-org-sync-config--validate-instance-url ()
  "Signal an error if `plane-org-sync-instance-url' is not HTTPS."
  (let ((url (url-generic-parse-url plane-org-sync-instance-url)))
    (unless (string= (url-type url) "https")
      (user-error "Plane-org-sync requires HTTPS.  Got: %s"
                  plane-org-sync-instance-url))))

;;;; Auth-Source Integration

(defun plane-org-sync-config--get-api-key ()
  "Return the Plane API key.
Checks `plane-org-sync-api-key' first.  If nil, queries auth-source
for an entry matching the host portion of `plane-org-sync-instance-url'."
  (plane-org-sync-config--validate-instance-url)
  (or (and (stringp plane-org-sync-api-key)
           (not (string-empty-p plane-org-sync-api-key))
           plane-org-sync-api-key)
      (let* ((url (url-generic-parse-url plane-org-sync-instance-url))
             (host (url-host url))
             (found (car (auth-source-search :host host
                                             :max 1
                                             :require '(:secret)))))
        (when found
          (let ((secret (plist-get found :secret)))
            (if (functionp secret)
                (funcall secret)
              secret))))
      (user-error
       "No API key configured.  Set `plane-org-sync-api-key' or add an auth-source entry for %s"
       (url-host (url-generic-parse-url plane-org-sync-instance-url)))))

;;;; Setup Wizard

;; Declare dynamic variables from url-http used in setup validation.
(defvar url-http-end-of-headers)
(defvar url-http-response-status)

(defun plane-org-sync-config--setup-request (url api-key path)
  "Make a synchronous GET request to PATH during setup.
URL is the instance base URL.  API-KEY is the resolved key string.
PATH is an API path like \"/api/v1/users/me/\".
Returns the parsed JSON plist on 2xx, signals error otherwise."
  (let* ((full-url (concat (string-remove-suffix "/" url) path))
         (url-request-method "GET")
         (url-request-extra-headers
          `(("X-API-Key" . ,api-key)
            ("Content-Type" . "application/json")))
         (buffer (url-retrieve-synchronously full-url nil nil 10)))
    (unwind-protect
        (with-current-buffer buffer
          (goto-char url-http-end-of-headers)
          (let ((status url-http-response-status))
            (unless (<= 200 status 299)
              (error "HTTP %d" status))
            (json-parse-buffer :object-type 'plist
                               :null-object nil
                               :false-object nil)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;;;###autoload
(defun plane-org-sync-setup ()
  "Interactively configure plane-org-sync connection settings.
Walks through instance URL, workspace slug, API key, sync file path,
project selection, and state mapping confirmation.  Validates the
connection by calling the Plane API, then persists settings via
`customize-save-variable'."
  (interactive)
  ;; 1. Instance URL.
  (let ((url (read-string "Plane instance URL: "
                          plane-org-sync-instance-url)))
    (setq url (string-trim-right url "/"))
    (unless (string-prefix-p "https://" url)
      (user-error "Instance URL must use HTTPS.  Got: %s" url))
    ;; 2. Workspace slug.
    (let ((workspace (read-string "Workspace slug: "
                                  plane-org-sync-workspace)))
      (when (string-empty-p workspace)
        (user-error "Workspace slug cannot be empty"))
      ;; 3. API key.
      (let* ((key-method (completing-read
                          "API key source: "
                          '("Enter directly" "Use auth-source")
                          nil t nil nil "Enter directly"))
             (api-key (if (equal key-method "Enter directly")
                          (let ((key (read-passwd "API key: ")))
                            (when (string-empty-p key)
                              (user-error "API key cannot be empty"))
                            key)
                        nil)))
        ;; 4. Sync file path.
        (let ((sync-file (read-file-name "Sync file: "
                                         nil plane-org-sync-file nil
                                         plane-org-sync-file)))
          ;; Apply settings temporarily for validation.
          (let ((plane-org-sync-instance-url url)
                (plane-org-sync-workspace workspace)
                (plane-org-sync-api-key api-key))
            ;; 5. Validate connection synchronously.
            (message "Validating connection...")
            (let ((resolved-key (plane-org-sync-config--get-api-key)))
              (condition-case err
                  (let ((me-body (plane-org-sync-config--setup-request
                                  url resolved-key "/api/v1/users/me/")))
                    (message "Connected as: %s"
                             (or (plist-get me-body :display_name)
                                 (plist-get me-body :email)
                                 "unknown user")))
                (error
                 (user-error "Connection failed: %s"
                             (error-message-string err))))
              ;; 6. Fetch and select projects.
              (message "Fetching projects...")
              (let* ((projects-body
                      (condition-case err
                          (plane-org-sync-config--setup-request
                           url resolved-key
                           (format "/api/v1/workspaces/%s/projects/" workspace))
                        (error
                         (user-error "Failed to fetch projects: %s"
                                     (error-message-string err)))))
                     (results-raw (plist-get projects-body :results))
                     (results (if (vectorp results-raw)
                                  (append results-raw nil)
                                results-raw))
                     ;; If no :results wrapper, body itself may be a vector/list.
                     (project-list (or results
                                       (if (vectorp projects-body)
                                           (append projects-body nil)
                                         projects-body)))
                     (candidates
                      (mapcar (lambda (p)
                                (cons (format "%s (%s)"
                                              (plist-get p :identifier)
                                              (plist-get p :name))
                                      (plist-get p :id)))
                              project-list)))
                (unless candidates
                  (user-error "No projects found in workspace \"%s\"" workspace))
                (let* ((selected (completing-read-multiple
                                  "Select projects (comma-separated): "
                                  candidates nil t))
                       (selected-ids (mapcar (lambda (s)
                                               (cdr (assoc s candidates)))
                                             selected)))
                  (unless selected-ids
                    (user-error "At least one project must be selected"))
                  ;; 7. Fetch states and display mapping.
                  (message "Fetching workflow states...")
                  (let ((all-states nil))
                    (dolist (proj-id selected-ids)
                      (condition-case nil
                          (let* ((states-body
                                  (plane-org-sync-config--setup-request
                                   url resolved-key
                                   (format "/api/v1/workspaces/%s/projects/%s/states/"
                                           workspace proj-id)))
                                 (raw (plist-get states-body :results))
                                 (states (if (vectorp raw)
                                             (append raw nil)
                                           (or raw
                                               (if (vectorp states-body)
                                                   (append states-body nil)
                                                 states-body)))))
                            (setq all-states (append all-states states)))
                        (error nil)))
                    (when all-states
                      (let ((mapping-lines nil))
                        (dolist (s all-states)
                          (let* ((name (plist-get s :name))
                                 (group (plist-get s :group))
                                 (kw (cdr (assoc (intern (or group ""))
                                                 plane-org-sync-group-keyword-mapping)))
                                 (override (cdr (assoc name plane-org-sync-state-mapping))))
                            (push (format "  %s (%s) -> %s"
                                          name group (or override kw "TODO"))
                                  mapping-lines)))
                        (message "State mapping:\n%s"
                                 (mapconcat #'identity (nreverse mapping-lines) "\n"))
                        (unless (y-or-n-p "Accept state mapping? ")
                          (message "Customize `plane-org-sync-group-keyword-mapping' and `plane-org-sync-state-mapping' manually.")))))
                  ;; 8. Persist settings.
                  (customize-save-variable 'plane-org-sync-instance-url url)
                  (customize-save-variable 'plane-org-sync-workspace workspace)
                  (when api-key
                    (customize-save-variable 'plane-org-sync-api-key api-key))
                  (customize-save-variable 'plane-org-sync-file sync-file)
                  (customize-save-variable 'plane-org-sync-projects selected-ids)
                  (message "Setup complete!  Run M-x plane-org-sync-pull to sync."))))))))))

(provide 'plane-org-sync-config)
;;; plane-org-sync-config.el ends here
