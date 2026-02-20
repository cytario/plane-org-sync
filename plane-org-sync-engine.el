;;; plane-org-sync-engine.el --- Sync engine for plane-org-sync  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Martin

;; Author: Martin
;; Maintainer: Martin
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Orchestrates the synchronization between Plane and Org.  Compares
;; remote Plane state with local Org state, computes a diff, and
;; produces a list of operations (create heading, update heading,
;; push state change).
;;
;; All functions in this module are pure (no I/O) except for the
;; on-demand state cache fallback in `plane-org-sync-engine--keyword-to-state',
;; which fetches states synchronously when the cache is empty.

;;; Code:

(require 'plane-org-sync-config)

(declare-function plane-org-sync-api--request-sync "plane-org-sync-api"
                  (method path &optional body))
(declare-function plane-org-sync-api--workspace-url "plane-org-sync-api" ())

;;;; State Mapping

(defun plane-org-sync-engine--state-to-keyword (state-name state-group)
  "Resolve a Plane state to an Org TODO keyword.
STATE-NAME is the human-readable state name (a string or nil).
STATE-GROUP is the state group from the API (a string or nil).
Returns a keyword string.

Resolution order:
1. Explicit override in `plane-org-sync-state-mapping' matching STATE-NAME.
2. Group default from `plane-org-sync-group-keyword-mapping' for STATE-GROUP.
3. Fallback to \"TODO\"."
  (or (and state-name
           (cdr (assoc state-name plane-org-sync-state-mapping)))
      (and state-group
           (cdr (assoc (intern state-group)
                       plane-org-sync-group-keyword-mapping)))
      "TODO"))

(defun plane-org-sync-engine--keyword-to-state (keyword project-id current-state-id)
  "Resolve an Org TODO KEYWORD to a Plane state UUID.
PROJECT-ID is the Plane project UUID.  CURRENT-STATE-ID is the
heading's current PLANE_STATE_ID (a UUID string or nil).

Returns the target state UUID, or nil if no change is needed or
no matching state is found.

Resolution order:
1. Explicit override in `plane-org-sync-state-mapping' matching KEYWORD.
2. Group default via inverse of `plane-org-sync-group-keyword-mapping'.
   Returns nil without warning if the current state is already in the
   target group (no change needed).
3. Warn and return nil."
  (let ((project-states (plane-org-sync-engine--get-project-states project-id)))
    (or
     ;; 1. Check explicit state-mapping for an entry whose cdr = KEYWORD.
     (plane-org-sync-engine--keyword-to-state-by-name
      keyword project-states)
     ;; 2. Inverse group-keyword-mapping lookup.
     ;; Returns a UUID, 'no-change for already-in-group, or nil for no match.
     (let ((group-result (plane-org-sync-engine--keyword-to-state-by-group
                          keyword project-states current-state-id)))
       (cond
        ((stringp group-result) group-result)
        ((eq group-result 'no-change) nil)
        (t
         ;; 3. No match found.
         (display-warning 'plane-org-sync
                          (format "No Plane state found for keyword \"%s\"" keyword)
                          :warning)
         nil))))))

(defun plane-org-sync-engine--keyword-to-state-by-name (keyword project-states)
  "Find a Plane state UUID by matching KEYWORD in `plane-org-sync-state-mapping'.
PROJECT-STATES is the list of state plists for the project.
Returns a UUID string or nil."
  (when-let* ((entry (seq-find (lambda (e)
                                 (string-equal (cdr e) keyword))
                               plane-org-sync-state-mapping))
              (state-name (car entry))
              (state (seq-find (lambda (s)
                                 (string-equal (plist-get s :name) state-name))
                               project-states)))
    (plist-get state :id)))

(defun plane-org-sync-engine--keyword-to-state-by-group (keyword project-states current-state-id)
  "Find a Plane state UUID by mapping KEYWORD to a group.
Uses the inverse of `plane-org-sync-group-keyword-mapping'.
PROJECT-STATES is the list of state plists for the project.
CURRENT-STATE-ID is the heading's current state UUID or nil.
Returns a UUID string, or nil if already in the target group or
no group matches KEYWORD."
  (when-let* ((group-entry (seq-find (lambda (e)
                                       (string-equal (cdr e) keyword))
                                     plane-org-sync-group-keyword-mapping))
              (target-group (symbol-name (car group-entry))))
    ;; If current-state-id already belongs to the target group, return
    ;; the symbol `no-change' to distinguish from "no match found" (nil).
    (if (and current-state-id
             (seq-find (lambda (s)
                         (and (string-equal (plist-get s :id) current-state-id)
                              (string-equal (plist-get s :group) target-group)))
                       project-states))
        'no-change
      ;; Return the first state in the target group, sorted by :sequence.
      (let ((group-states (seq-filter
                           (lambda (s)
                             (string-equal (plist-get s :group) target-group))
                           project-states)))
        (when group-states
          (plist-get (car (seq-sort-by (lambda (s) (plist-get s :sequence))
                                       #'< group-states))
                     :id))))))

(defun plane-org-sync-engine--get-project-states (project-id)
  "Return the list of state plists for PROJECT-ID.
Reads from `plane-org-sync--state-cache'.  If the cache is empty
for this project, fetches states synchronously via the API and
populates the cache."
  (or (cdr (assoc project-id plane-org-sync--state-cache))
      (plane-org-sync-engine--fetch-and-cache-states project-id)))

(defun plane-org-sync-engine--fetch-and-cache-states (project-id)
  "Fetch states for PROJECT-ID synchronously and cache them.
Returns the list of state plists."
  (require 'plane-org-sync-api)
  (let* ((path (format "%s/projects/%s/states/"
                       (plane-org-sync-api--workspace-url)
                       project-id))
         (response (plane-org-sync-api--request-sync "GET" path))
         (results (plist-get response :results))
         (states (seq-map (lambda (s)
                            (list :id (plist-get s :id)
                                  :name (plist-get s :name)
                                  :group (plist-get s :group)
                                  :sequence (or (plist-get s :sequence) 0)))
                          results)))
    (push (cons project-id states) plane-org-sync--state-cache)
    states))

;;;; Keyword Collection

(defun plane-org-sync-engine--collect-keywords (project-states)
  "Collect active and done Org keywords from PROJECT-STATES.
PROJECT-STATES is a list of state plists, each with :name, :group,
and :id keys.

Returns a cons cell (ACTIVE-KEYWORDS . DONE-KEYWORDS) where
ACTIVE-KEYWORDS are keywords for backlog, unstarted, and started
groups, and DONE-KEYWORDS are keywords for completed and cancelled
groups.  Both lists are deduplicated."
  (let ((active-groups '("backlog" "unstarted" "started"))
        (done-groups '("completed" "cancelled"))
        (active-keywords nil)
        (done-keywords nil))
    (dolist (state project-states)
      (let* ((name (plist-get state :name))
             (group (plist-get state :group))
             (kw (plane-org-sync-engine--state-to-keyword name group)))
        (cond
         ((member group active-groups)
          (push kw active-keywords))
         ((member group done-groups)
          (push kw done-keywords)))))
    (cons (delete-dups (nreverse active-keywords))
          (delete-dups (nreverse done-keywords)))))

;;;; Diff Calculation

(defun plane-org-sync-engine--heading-needs-repair-p (heading)
  "Return non-nil if HEADING has missing or empty critical metadata.
This detects headings created by older code versions that lack
PLANE_PROJECT_ID, PLANE_STATE_ID, or PLANE_STATE properties.
Such headings should be updated even when timestamps match to
repair the missing data."
  (or (let ((v (plist-get heading :plane-project-id)))
        (or (null v) (string-empty-p v)))
      (let ((v (plist-get heading :plane-state-id)))
        (or (null v) (string-empty-p v)))
      (let ((v (plist-get heading :plane-state)))
        (or (null v) (string-empty-p v)))))

(defun plane-org-sync-engine--diff (remote-items local-headings)
  "Compute a sync diff between REMOTE-ITEMS and LOCAL-HEADINGS.
REMOTE-ITEMS is a list of work item plists from the API, each
with an :id key.  LOCAL-HEADINGS is a list of heading record
plists from the Org reader, each with :plane-id and
:plane-updated-at keys.

Returns a plist with keys:
  :create    - remote items not found locally
  :update    - pairs of (heading-record . work-item) where
               timestamps differ or heading metadata needs repair
  :unchanged - pairs of (heading-record . work-item) where
               timestamps match and metadata is complete
  :orphaned  - local headings not found in remote items"
  (let ((local-by-id (make-hash-table :test #'equal))
        (seen-local-ids (make-hash-table :test #'equal))
        (create nil)
        (update nil)
        (unchanged nil)
        (orphaned nil))
    ;; Index local headings by plane-id.
    (dolist (heading local-headings)
      (when-let* ((pid (plist-get heading :plane-id)))
        (puthash pid heading local-by-id)))
    ;; Classify each remote item.
    (dolist (item remote-items)
      (let* ((item-id (plist-get item :id))
             (heading (gethash item-id local-by-id)))
        (if heading
            (progn
              (puthash item-id t seen-local-ids)
              (if (or (not (equal (plist-get item :updated_at)
                                  (plist-get heading :plane-updated-at)))
                      (plane-org-sync-engine--heading-needs-repair-p
                       heading))
                  (push (cons heading item) update)
                (push (cons heading item) unchanged)))
          (push item create))))
    ;; Find orphaned local headings (not in remote set).
    (dolist (heading local-headings)
      (when-let* ((pid (plist-get heading :plane-id)))
        (unless (gethash pid seen-local-ids)
          (push heading orphaned))))
    (list :create (nreverse create)
          :update (nreverse update)
          :unchanged (nreverse unchanged)
          :orphaned (nreverse orphaned))))

;;;; Conflict Detection

(defun plane-org-sync-engine--detect-conflict (local-updated-at remote-updated-at)
  "Return non-nil if REMOTE-UPDATED-AT is strictly newer than LOCAL-UPDATED-AT.
Both are ISO 8601 timestamp strings.  ISO 8601 strings are
lexicographically orderable, so `string>' suffices.

Returns nil if LOCAL-UPDATED-AT is nil (first sync, no conflict)."
  (and local-updated-at
       remote-updated-at
       (string> remote-updated-at local-updated-at)))

(provide 'plane-org-sync-engine)
;;; plane-org-sync-engine.el ends here
