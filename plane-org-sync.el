;;; plane-org-sync.el --- Synchronize Plane.so work items with Org-mode  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Martin

;; Author: Martin
;; Maintainer: Martin
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, comm
;; URL: https://github.com/martin/plane-org-sync
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; plane-org-sync synchronizes Plane.so work items with an Org-mode file.
;; It provides commands to pull work items from Plane into Org headings
;; and push TODO state changes back to Plane.
;;
;; Configuration:
;;   (setq plane-org-sync-instance-url "https://plane.example.com")
;;   (setq plane-org-sync-workspace "my-workspace")
;;   (setq plane-org-sync-projects '("project-uuid-1"))
;;
;; Usage:
;;   M-x plane-org-sync-pull   -- fetch work items from Plane
;;   M-x plane-org-sync-mode   -- enable automatic sync
;;   M-x plane-org-sync-browse -- open Plane URL for heading at point
;;   M-x plane-org-sync-status -- display sync status
;;   M-x plane-org-sync-reset  -- clear caches and state

;;; Code:

(require 'plane-org-sync-config)
(require 'plane-org-sync-api)
(require 'plane-org-sync-org)
(require 'plane-org-sync-engine)

(eval-when-compile (require 'cl-lib))

;; Declare dynamic variables set by org-mode before hook invocation.
(defvar org-state)
(defvar org-last-state)

;;;; Async Utilities

;; NOTE: `plane-org-sync--chain' is not used in the current pull flow
;; (which uses `--collect' for fan-out).  It is retained as a utility
;; for future multi-step sequential async flows (e.g., chained updates).

(defun plane-org-sync--chain (steps &optional error-callback)
  "Chain async functions sequentially, threading results through callbacks.
Each element of STEPS is a function receiving (NEXT &rest PREV-RESULTS).
NEXT is a callback the step must call to continue the chain.
Calling (funcall NEXT \\='error MSG) aborts remaining steps and
invokes ERROR-CALLBACK with MSG.  If ERROR-CALLBACK is nil,
errors are logged to the plane-org-sync log buffer."
  (if (null steps)
      nil
    (let ((step (car steps))
          (rest (cdr steps)))
      (funcall step
               (lambda (&rest results)
                 (if (and results (eq (car results) 'error))
                     (let ((msg (or (cadr results) "Unknown error")))
                       (plane-org-sync-api--log 'error "chain aborted: %s" msg)
                       (when error-callback
                         (funcall error-callback msg)))
                   (if rest
                       (let ((next-step (car rest))
                             (remaining (cdr rest)))
                         (apply next-step
                                (lambda (&rest next-results)
                                  (if (and next-results
                                           (eq (car next-results) 'error))
                                      (let ((msg (or (cadr next-results)
                                                     "Unknown error")))
                                        (plane-org-sync-api--log
                                         'error "chain aborted: %s" msg)
                                        (when error-callback
                                          (funcall error-callback msg)))
                                    (when remaining
                                      (plane-org-sync--chain
                                       (cons (lambda (next &rest _prev)
                                               (apply next next-results))
                                             remaining)
                                       error-callback))))
                                results))
                     nil)))))))

(defun plane-org-sync--collect (fns callback)
  "Call each function in FNS with a per-slot callback.
Each fn receives one argument: a callback to call with its result.
When all fns have completed, CALLBACK is called with the list of
results in the same order as FNS.  If any fn signals an error, the
corresponding result slot is (:error . message)."
  (if (null fns)
      (funcall callback nil)
    (let* ((n (length fns))
           (results (make-vector n nil))
           (remaining n))
      (cl-loop for fn in fns
               for i from 0
               do (let ((idx i))
                    (funcall fn
                             (lambda (result)
                               (aset results idx result)
                               (cl-decf remaining)
                               (when (zerop remaining)
                                 (funcall callback
                                          (append results nil))))))))))

;;;; Timer Management

;; Forward-declare the mode variable; defined later by `define-minor-mode'.
(defvar plane-org-sync-mode)

(defvar plane-org-sync--timer nil
  "Idle timer for automatic sync.")

(defun plane-org-sync--schedule-auto-sync ()
  "Schedule the next auto sync after idle interval."
  (when (and plane-org-sync-mode
             plane-org-sync-auto-interval
             (not plane-org-sync--timer))
    (setq plane-org-sync--timer
          (run-with-idle-timer
           plane-org-sync-auto-interval nil
           (lambda ()
             (setq plane-org-sync--timer nil)
             (plane-org-sync-pull
              (lambda (&rest _)
                (plane-org-sync--schedule-auto-sync))))))))

;;;; Pull Command

;;;###autoload
(defun plane-org-sync-pull (&optional callback)
  "Fetch work items from Plane and synchronize to the Org sync file.
CALLBACK, if provided, is called with a plist (:created N :updated M)
when the sync completes.  When called interactively, results are
displayed in the minibuffer."
  (interactive)
  (unless plane-org-sync-projects
    (user-error "No projects configured.  Set `plane-org-sync-projects'"))
  ;; Check for unsaved modifications BEFORE any mutations.
  (let ((buf (find-buffer-visiting plane-org-sync-file)))
    (when (and buf (buffer-modified-p buf))
      (user-error
       "Sync file has unsaved modifications.  Save the file first, then re-run sync")))
  ;; Step 1: Get user ID.
  (plane-org-sync-api-me
   (lambda (status-code body)
     (if (null status-code)
         (progn
           (message "Plane: failed to get user info: %s" body)
           (when callback (funcall callback nil)))
       (let ((user-id (plist-get body :id)))
         (setq plane-org-sync--user-id user-id)
         ;; Step 2: Fan-out across projects.
         (let ((project-fns
                (mapcar
                 (lambda (project-id)
                   (lambda (slot-cb)
                     ;; Fetch states first, then work items.
                     (plane-org-sync-api-list-states
                      project-id
                      (lambda (st-status states)
                        (if (null st-status)
                            ;; States fetch failed.
                            (funcall slot-cb
                                     (list :error states
                                           :project-id project-id))
                          ;; Cache states.
                          (let ((state-plists
                                 (seq-map
                                  (lambda (s)
                                    (list :id (plist-get s :id)
                                          :name (plist-get s :name)
                                          :group (plist-get s :group)
                                          :sequence
                                          (or (plist-get s :sequence) 0)))
                                  states)))
                            ;; Update state cache for this project.
                            (setf (alist-get project-id
                                             plane-org-sync--state-cache
                                             nil nil #'equal)
                                  state-plists)
                            ;; Fetch work items.
                            (let ((params
                                   (when (and plane-org-sync-filter-assignee
                                             user-id)
                                     (list (cons "assignees" user-id)))))
                              (plane-org-sync-api-list-work-items
                               project-id params
                               (lambda (items &optional error-msg)
                                 (if (and (null items) error-msg)
                                     (funcall slot-cb
                                              (list :error error-msg
                                                    :project-id project-id))
                                   (funcall
                                    slot-cb
                                    (list :items items
                                          :states state-plists
                                          :project-id
                                          project-id))))))))))))
                 plane-org-sync-projects)))
           ;; Collect all project results.
           (plane-org-sync--collect
            project-fns
            (lambda (project-results)
              (plane-org-sync--pull-apply-results
               project-results callback)))))))))

(defun plane-org-sync--pull-apply-results (project-results callback)
  "Apply PROJECT-RESULTS to the Org sync file.
PROJECT-RESULTS is a list of plists, one per project, each with
:items, :states, :project-id (or :error).  CALLBACK is called
with the sync result plist."
  (let ((org-buf (plane-org-sync-org--get-buffer plane-org-sync-file))
        (headings nil)
        (total-created 0)
        (total-updated 0)
        (total-unchanged 0)
        (errors nil))
    (unwind-protect
        (progn
          (setq headings (plane-org-sync-org--read-headings
                          plane-org-sync-file))
          ;; Collect all states across projects for TODO line.
          (let ((all-states nil))
            (dolist (pr project-results)
              (if (plist-get pr :error)
                  (push (format "Project %s: %s"
                                (plist-get pr :project-id)
                                (plist-get pr :error))
                        errors)
                (let ((states (plist-get pr :states)))
                  (setq all-states (append all-states states)))))
            ;; Ensure TODO line from all collected states.
            (when all-states
              (plane-org-sync-org--ensure-todo-line org-buf all-states))
            ;; Process each successful project.
            (dolist (pr project-results)
              (unless (plist-get pr :error)
                (let* ((items (plist-get pr :items))
                       (states (plist-get pr :states))
                       (project-id (plist-get pr :project-id))
                       ;; Filter headings for this project.
                       (proj-headings
                        (seq-filter
                         (lambda (h)
                           (equal (plist-get h :plane-project-id) project-id))
                         headings))
                       ;; Resolve state for each work item.
                       (resolved-items
                        (mapcar
                         (lambda (item)
                           (let* ((state-detail (plist-get item :state_detail))
                                  (state-name
                                   (or (plist-get state-detail :name)
                                       (plist-get item :state_name)
                                       ;; Fallback: look up in states list.
                                       (let ((st (seq-find
                                                  (lambda (s)
                                                    (equal (plist-get s :id)
                                                           (plist-get item :state)))
                                                  states)))
                                         (when st (plist-get st :name)))))
                                  (state-group
                                   (or (plist-get state-detail :group)
                                       (plist-get item :state_group)
                                       (let ((st (seq-find
                                                  (lambda (s)
                                                    (equal (plist-get s :id)
                                                           (plist-get item :state)))
                                                  states)))
                                         (when st (plist-get st :group)))))
                                  (state-id
                                   (or (plist-get state-detail :id)
                                       (plist-get item :state)))
                                  (kw (plane-org-sync-engine--state-to-keyword
                                       state-name state-group)))
                             ;; Enrich work item with resolved state info.
                             ;; plist-put may return a new list; always capture.
                             (setq item (plist-put item :state_name state-name))
                             (setq item (plist-put item :state_group state-group))
                             (setq item (plist-put item :state_id state-id))
                             (cons kw item)))
                         items))
                       ;; Compute diff.
                       (diff (plane-org-sync-engine--diff
                              (mapcar #'cdr resolved-items)
                              proj-headings))
                       (to-create (plist-get diff :create))
                       (to-update (plist-get diff :update)))
                  ;; Apply creates with push suppression.
                  (let ((plane-org-sync--inhibit-push t))
                    (dolist (item to-create)
                      (let* ((pair (seq-find
                                    (lambda (ri)
                                      (equal (plist-get (cdr ri) :id)
                                             (plist-get item :id)))
                                    resolved-items))
                             (kw (car pair)))
                        (plane-org-sync-org--insert-heading
                         org-buf item kw)))
                    ;; Apply updates in reverse buffer order.
                    (let ((sorted-updates
                           (sort (copy-sequence to-update)
                                 (lambda (a b)
                                   (> (marker-position
                                       (plist-get (car a) :marker))
                                      (marker-position
                                       (plist-get (car b) :marker)))))))
                      (dolist (pair sorted-updates)
                        (let* ((heading-rec (car pair))
                               (item (cdr pair))
                               (ri (seq-find
                                    (lambda (ri)
                                      (equal (plist-get (cdr ri) :id)
                                             (plist-get item :id)))
                                    resolved-items))
                               (kw (car ri)))
                          (plane-org-sync-org--update-heading
                           org-buf (plist-get heading-rec :marker)
                           item kw)))))
                  (cl-incf total-created (length to-create))
                  (cl-incf total-updated (length to-update))
                  (cl-incf total-unchanged
                           (length (plist-get diff :unchanged)))))))
          ;; Save atomically.
          (plane-org-sync-org--save-atomic org-buf plane-org-sync-file)
          ;; Report results.
          (let ((total (+ total-created total-updated total-unchanged))
                (result (list :created total-created
                              :updated total-updated
                              :unchanged total-unchanged)))
            (setq plane-org-sync--last-sync (current-time))
            (setq plane-org-sync--last-sync-result result)
            (if errors
                (message "Plane: synced %d items (%d new, %d updated); errors: %s"
                         total total-created total-updated
                         (string-join errors "; "))
              (message "Plane: synced %d items (%d new, %d updated)"
                       total total-created total-updated))
            (when callback (funcall callback result))))
      ;; Cleanup markers.
      (when headings
        (mapc (lambda (h)
                (when-let* ((m (plist-get h :marker)))
                  (set-marker m nil)))
              headings)))))

;;;; State Push Hook

(defun plane-org-sync--on-todo-state-change ()
  "Push a state change to Plane when a synced heading's TODO keyword changed.
Added to `org-after-todo-state-change-hook' by `plane-org-sync-mode'.
Uses synchronous HTTP to ensure the push completes before the user
continues editing."
  (when (and (not plane-org-sync--inhibit-push)
             (org-entry-get nil "PLANE_ID"))
    (let* ((plane-id (org-entry-get nil "PLANE_ID"))
           (project-id (org-entry-get nil "PLANE_PROJECT_ID"))
           (new-keyword org-state)
           (old-keyword org-last-state)
           (state-uuid (plane-org-sync-engine--keyword-to-state
                        new-keyword project-id
                        (org-entry-get nil "PLANE_STATE_ID"))))
      (when state-uuid
        (condition-case err
            (let* ((path (format "/workspaces/%s/projects/%s/work-items/%s/"
                                 plane-org-sync-workspace
                                 project-id plane-id))
                   (remote (plane-org-sync-api--request-sync "GET" path))
                   (remote-ts (plist-get remote :updated_at))
                   (local-ts (org-entry-get nil "PLANE_UPDATED_AT")))
              ;; Conflict detection.
              (when (and (plane-org-sync-engine--detect-conflict
                          local-ts remote-ts)
                         (not (funcall plane-org-sync-conflict-function
                                       (concat
                                        "Remote item modified since last sync."
                                        "  Push anyway? "))))
                (let ((plane-org-sync--inhibit-push t))
                  (org-todo old-keyword))
                (user-error "Push cancelled"))
              ;; Push the state change.
              (let ((result (plane-org-sync-api--request-sync
                             "PATCH" path (list :state state-uuid))))
                (org-entry-put nil "PLANE_UPDATED_AT"
                               (plist-get result :updated_at))
                (org-entry-put nil "PLANE_STATE_ID" state-uuid)
                (message "Plane: state updated to %s" new-keyword)))
          (user-error (signal (car err) (cdr err)))
          (error
           (let ((plane-org-sync--inhibit-push t))
             (org-todo old-keyword))
           (message "Plane push failed: %s"
                    (error-message-string err))))))))

;;;; Minor Mode

;;;###autoload
(define-minor-mode plane-org-sync-mode
  "Toggle automatic Plane.so synchronization.
When enabled, adds a hook to push TODO state changes to Plane,
schedules automatic sync if `plane-org-sync-auto-interval' is set,
and adds `plane-org-sync-file' to `org-agenda-files'."
  :global t
  :lighter " PlaneSync"
  :group 'plane-org-sync
  (if plane-org-sync-mode
      (progn
        (add-hook 'org-after-todo-state-change-hook
                  #'plane-org-sync--on-todo-state-change)
        (plane-org-sync--schedule-auto-sync)
        (when (and plane-org-sync-file
                   (not (member plane-org-sync-file org-agenda-files)))
          (push plane-org-sync-file org-agenda-files)))
    (remove-hook 'org-after-todo-state-change-hook
                 #'plane-org-sync--on-todo-state-change)
    (when plane-org-sync--timer
      (cancel-timer plane-org-sync--timer)
      (setq plane-org-sync--timer nil))
    (when plane-org-sync-file
      (setq org-agenda-files
            (delete plane-org-sync-file org-agenda-files)))))

;;;; Browse Command

;;;###autoload
(defun plane-org-sync-browse ()
  "Open the Plane URL for the heading at point in a web browser."
  (interactive)
  (let ((url (org-entry-get nil "PLANE_URL")))
    (unless url
      (user-error "No PLANE_URL property at point.  Not a synced heading"))
    (browse-url url)))

;;;; Status Command

;;;###autoload
(defun plane-org-sync-status ()
  "Display plane-org-sync status in the minibuffer."
  (interactive)
  (let* ((last-sync (if plane-org-sync--last-sync
                        (format-time-string "%Y-%m-%d %H:%M:%S"
                                            plane-org-sync--last-sync)
                      "Never synced"))
         (result (if plane-org-sync--last-sync-result
                     (format "created=%d updated=%d unchanged=%d"
                             (or (plist-get plane-org-sync--last-sync-result
                                           :created) 0)
                             (or (plist-get plane-org-sync--last-sync-result
                                           :updated) 0)
                             (or (plist-get plane-org-sync--last-sync-result
                                           :unchanged) 0))
                   "No results"))
         (cached-projects (length plane-org-sync--state-cache))
         (user-id (or plane-org-sync--user-id "Unknown"))
         (auto-sync (cond
                     ((not plane-org-sync-mode) "mode off")
                     (plane-org-sync-auto-interval
                      (format "every %ds" plane-org-sync-auto-interval))
                     (t "manual only"))))
    (message "Plane: last sync=%s  result=(%s)  cached-projects=%d  user=%s  auto-sync=%s"
             last-sync result cached-projects user-id auto-sync)))

;;;; Reset Command

;;;###autoload
(defun plane-org-sync-reset ()
  "Clear sync metadata and perform a fresh pull from Plane.
Prompts for confirmation, then removes all PLANE_* properties from
headings in the sync file, clears in-memory caches, and triggers a
full re-sync.

Note: this removes sync metadata (PLANE_* properties) from headings
but preserves heading content.  After the re-pull, headings are
re-matched by PLANE_ID and updated."
  (interactive)
  (unless (yes-or-no-p "Reset all sync state and re-pull from Plane? ")
    (user-error "Reset cancelled"))
  ;; Strip PLANE_* properties from all headings in the sync file.
  (when (and plane-org-sync-file (file-exists-p plane-org-sync-file))
    (let ((buf (find-file-noselect plane-org-sync-file)))
      (with-current-buffer buf
        (org-with-wide-buffer
         (org-map-entries
          (lambda ()
            (let ((props '("PLANE_ID" "PLANE_URL" "PLANE_UPDATED_AT"
                           "PLANE_STATE" "PLANE_STATE_ID"
                           "PLANE_PROJECT_ID" "PLANE_PROJECT"
                           "PLANE_PRIORITY" "PLANE_ASSIGNEES")))
              (dolist (prop props)
                (org-entry-delete nil prop))))
          "LEVEL=1"))
        (save-buffer))))
  ;; Clear in-memory caches.
  (setq plane-org-sync--state-cache nil)
  (setq plane-org-sync--user-id nil)
  (setq plane-org-sync--last-sync nil)
  (setq plane-org-sync--last-sync-result nil)
  (message "plane-org-sync: state cleared, starting fresh pull...")
  ;; Trigger a fresh pull.
  (plane-org-sync-pull))

;;;; Capture Template

(defvar plane-org-sync--capture-project-id nil
  "Project ID selected during capture, used by the before-finalize hook.")

(defvar plane-org-sync--capture-item-name nil
  "Work item name entered during capture, used by the before-finalize hook.")

;;;###autoload
(defun plane-org-sync-capture-template (&optional keys)
  "Return an Org capture template entry for creating Plane work items.
KEYS is the keybinding string for the template (default \"P\").
The template creates a new work item in Plane and sets PLANE_*
properties on the captured heading.

Add to `org-capture-templates' like:
  (push (plane-org-sync-capture-template) org-capture-templates)"
  (require 'org-capture)
  (let ((key (or keys "P")))
    (list key "Plane work item"
          'entry
          (list 'file plane-org-sync-file)
          "* TODO %^{Title}\n:PROPERTIES:\n:END:\n"
          :before-finalize #'plane-org-sync--capture-before-finalize)))

(defun plane-org-sync--capture-fetch-labels (project-id)
  "Fetch labels for PROJECT-ID synchronously for capture prompts.
Returns a list of label plists, or nil on failure."
  (condition-case nil
      (let* ((path (format "/workspaces/%s/projects/%s/labels/"
                           plane-org-sync-workspace project-id))
             (response (plane-org-sync-api--request-sync "GET" path))
             (raw (plist-get response :results)))
        (if (vectorp raw) (append raw nil)
          (or raw
              (if (vectorp response) (append response nil)
                response))))
    (error nil)))

(defun plane-org-sync--capture-before-finalize ()
  "Create a Plane work item for the heading being captured.
Called from the `:before-finalize' hook of the capture template.
Prompts for project, priority, and labels, then posts the work
item synchronously and sets PLANE_* properties on the heading."
  (require 'org-capture)
  (when (and (bound-and-true-p org-capture-mode)
             plane-org-sync-workspace
             plane-org-sync-projects)
    (let* ((project-id (if (= (length plane-org-sync-projects) 1)
                           (car plane-org-sync-projects)
                         (completing-read "Project: " plane-org-sync-projects
                                          nil t)))
           (title (save-excursion
                    (org-back-to-heading t)
                    (org-get-heading t t t t)))
           ;; Prompt for priority.
           (priority (completing-read "Priority: "
                                      '("none" "low" "medium" "high" "urgent")
                                      nil t nil nil "none"))
           ;; Prompt for labels.
           (available-labels (plane-org-sync--capture-fetch-labels project-id))
           (label-candidates (mapcar (lambda (l)
                                       (cons (plist-get l :name)
                                             (plist-get l :id)))
                                     available-labels))
           (selected-label-names (when label-candidates
                                   (completing-read-multiple
                                    "Labels (comma-separated, or empty): "
                                    label-candidates nil t)))
           (selected-label-ids (mapcar (lambda (name)
                                         (cdr (assoc name label-candidates)))
                                       selected-label-names)))
      (when (and project-id (not (string-empty-p title)))
        (condition-case err
            (let* ((path (format "/workspaces/%s/projects/%s/work-items/"
                                 plane-org-sync-workspace project-id))
                   (body (list :name title :priority priority))
                   (_body (when selected-label-ids
                            (setq body (plist-put body :labels
                                                  (vconcat selected-label-ids)))))
                   (result (plane-org-sync-api--request-sync
                            "POST" path body)))
              (when result
                (let ((item-id (plist-get result :id))
                      (updated-at (or (plist-get result :updated_at) ""))
                      (sequence-id (plist-get result :sequence_id))
                      (project-identifier
                       (or (plist-get result :project_identifier) ""))
                      (state-name "")
                      (state-id (or (plist-get result :state) "")))
                  ;; Resolve state name from cache or response.
                  (when-let* ((state-detail (plist-get result :state_detail)))
                    (setq state-name (or (plist-get state-detail :name) ""))
                    (setq state-id (or (plist-get state-detail :id) state-id)))
                  (org-entry-put nil "PLANE_ID" item-id)
                  (org-entry-put nil "PLANE_URL"
                                 (plane-org-sync-org--build-plane-url
                                  plane-org-sync-instance-url
                                  plane-org-sync-workspace
                                  project-id
                                  sequence-id))
                  (org-entry-put nil "PLANE_UPDATED_AT" updated-at)
                  (org-entry-put nil "PLANE_STATE" state-name)
                  (org-entry-put nil "PLANE_STATE_ID" state-id)
                  (org-entry-put nil "PLANE_PROJECT_ID" project-id)
                  (org-entry-put nil "PLANE_PROJECT" project-identifier)
                  (org-entry-put nil "PLANE_PRIORITY" priority)
                  (org-entry-put nil "PLANE_ASSIGNEES" "")
                  (message "Plane: created work item %s-%s"
                           project-identifier sequence-id))))
          (error
           (message "Plane: failed to create work item: %s"
                    (error-message-string err))))))))

(provide 'plane-org-sync)
;;; plane-org-sync.el ends here
