;;; test-plane-org-sync-engine.el --- Tests for plane-org-sync-engine  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Martin

;; Author: Martin
;; Maintainer: Martin
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the sync engine module of plane-org-sync.
;; All engine functions are pure (operate on data structures), so no
;; mocks are needed except for the keyword-to-state cache fallback.

;;; Code:

(require 'ert)
(require 'plane-org-sync-engine)

;;;; Test Fixtures

(defvar test-plane-org-sync-engine--sample-states
  (list (list :id "state-backlog-1" :name "Backlog" :group "backlog" :sequence 1000)
        (list :id "state-unstarted-1" :name "Todo" :group "unstarted" :sequence 2000)
        (list :id "state-started-1" :name "In Progress" :group "started" :sequence 3000)
        (list :id "state-started-2" :name "In Review" :group "started" :sequence 4000)
        (list :id "state-completed-1" :name "Done" :group "completed" :sequence 5000)
        (list :id "state-cancelled-1" :name "Cancelled" :group "cancelled" :sequence 6000))
  "Sample project states for testing.")

(defmacro test-plane-org-sync-engine--with-default-config (&rest body)
  "Execute BODY with default plane-org-sync configuration bindings."
  (declare (indent 0))
  `(let ((plane-org-sync-group-keyword-mapping
          '((backlog   . "BACKLOG")
            (unstarted . "TODO")
            (started   . "STARTED")
            (completed . "DONE")
            (cancelled . "CANCELLED")))
         (plane-org-sync-state-mapping nil))
     ,@body))

(defmacro test-plane-org-sync-engine--with-custom-config (&rest body)
  "Execute BODY with custom state-mapping overrides."
  (declare (indent 0))
  `(let ((plane-org-sync-group-keyword-mapping
          '((backlog   . "BACKLOG")
            (unstarted . "TODO")
            (started   . "STARTED")
            (completed . "DONE")
            (cancelled . "CANCELLED")))
         (plane-org-sync-state-mapping
          '(("In Review" . "REVIEW")
            ("QA"        . "TESTING"))))
     ,@body))

;;;; Feature Loaded

(ert-deftest plane-org-sync-engine-test-feature-provided ()
  "The `plane-org-sync-engine' feature should be available."
  (should (featurep 'plane-org-sync-engine)))

;;;; state-to-keyword Tests

(ert-deftest plane-org-sync-engine-test-state-to-keyword-group-default ()
  "Group-based default maps state group to keyword."
  (test-plane-org-sync-engine--with-default-config
    (should (equal (plane-org-sync-engine--state-to-keyword "In Progress" "started")
                   "STARTED"))
    (should (equal (plane-org-sync-engine--state-to-keyword "Backlog" "backlog")
                   "BACKLOG"))
    (should (equal (plane-org-sync-engine--state-to-keyword "Done" "completed")
                   "DONE"))
    (should (equal (plane-org-sync-engine--state-to-keyword "Cancelled" "cancelled")
                   "CANCELLED"))
    (should (equal (plane-org-sync-engine--state-to-keyword "Todo" "unstarted")
                   "TODO"))))

(ert-deftest plane-org-sync-engine-test-state-to-keyword-explicit-override ()
  "Explicit state-mapping overrides group-based default."
  (test-plane-org-sync-engine--with-custom-config
    (should (equal (plane-org-sync-engine--state-to-keyword "In Review" "started")
                   "REVIEW"))
    ;; Non-overridden state still uses group default.
    (should (equal (plane-org-sync-engine--state-to-keyword "In Progress" "started")
                   "STARTED"))))

(ert-deftest plane-org-sync-engine-test-state-to-keyword-unknown ()
  "Unknown state name and group falls back to TODO."
  (test-plane-org-sync-engine--with-default-config
    (should (equal (plane-org-sync-engine--state-to-keyword "Nonexistent" "unknown_group")
                   "TODO"))))

(ert-deftest plane-org-sync-engine-test-state-to-keyword-nil-both ()
  "Nil state-name and nil state-group returns TODO."
  (test-plane-org-sync-engine--with-default-config
    (should (equal (plane-org-sync-engine--state-to-keyword nil nil)
                   "TODO"))))

(ert-deftest plane-org-sync-engine-test-state-to-keyword-nil-name-valid-group ()
  "Nil state-name with valid group uses group default."
  (test-plane-org-sync-engine--with-default-config
    (should (equal (plane-org-sync-engine--state-to-keyword nil "started")
                   "STARTED"))))

(ert-deftest plane-org-sync-engine-test-state-to-keyword-nil-group-valid-name ()
  "Valid state-name with nil group uses override if present."
  (test-plane-org-sync-engine--with-custom-config
    (should (equal (plane-org-sync-engine--state-to-keyword "In Review" nil)
                   "REVIEW"))))

(ert-deftest plane-org-sync-engine-test-state-to-keyword-nil-group-unmatched-name ()
  "Unmatched state-name with nil group falls back to TODO."
  (test-plane-org-sync-engine--with-default-config
    (should (equal (plane-org-sync-engine--state-to-keyword "SomeState" nil)
                   "TODO"))))

;;;; keyword-to-state Tests

(ert-deftest plane-org-sync-engine-test-keyword-to-state-override-match ()
  "Override match finds state UUID by name from state-mapping."
  (test-plane-org-sync-engine--with-custom-config
    (let ((plane-org-sync--state-cache
           (list (cons "proj-1" test-plane-org-sync-engine--sample-states))))
      (should (equal (plane-org-sync-engine--keyword-to-state
                      "REVIEW" "proj-1" nil)
                     "state-started-2")))))

(ert-deftest plane-org-sync-engine-test-keyword-to-state-group-match ()
  "Group-based match returns the first state in group by sequence."
  (test-plane-org-sync-engine--with-default-config
    (let ((plane-org-sync--state-cache
           (list (cons "proj-1" test-plane-org-sync-engine--sample-states))))
      ;; STARTED maps to group "started"; first by sequence is "In Progress"
      (should (equal (plane-org-sync-engine--keyword-to-state
                      "STARTED" "proj-1" nil)
                     "state-started-1")))))

(ert-deftest plane-org-sync-engine-test-keyword-to-state-already-in-group ()
  "Returns nil when current state already belongs to the target group."
  (test-plane-org-sync-engine--with-default-config
    (let ((plane-org-sync--state-cache
           (list (cons "proj-1" test-plane-org-sync-engine--sample-states))))
      ;; Current state is "In Progress" (started group), keyword is STARTED
      ;; (also maps to started group) -> no change needed.
      (should-not (plane-org-sync-engine--keyword-to-state
                   "STARTED" "proj-1" "state-started-1")))))

(ert-deftest plane-org-sync-engine-test-keyword-to-state-already-in-group-different-state ()
  "Returns nil when current state is different but in the same target group."
  (test-plane-org-sync-engine--with-default-config
    (let ((plane-org-sync--state-cache
           (list (cons "proj-1" test-plane-org-sync-engine--sample-states))))
      ;; Current state is "In Review" (started), keyword is STARTED (started).
      (should-not (plane-org-sync-engine--keyword-to-state
                   "STARTED" "proj-1" "state-started-2")))))

(ert-deftest plane-org-sync-engine-test-keyword-to-state-unknown-keyword ()
  "Unknown keyword warns and returns nil."
  (test-plane-org-sync-engine--with-default-config
    (let ((plane-org-sync--state-cache
           (list (cons "proj-1" test-plane-org-sync-engine--sample-states))))
      (should-not (plane-org-sync-engine--keyword-to-state
                   "WEIRD_KEYWORD" "proj-1" nil)))))

(ert-deftest plane-org-sync-engine-test-keyword-to-state-group-sorted-by-sequence ()
  "Group match returns the state with the lowest sequence number."
  (test-plane-org-sync-engine--with-default-config
    (let* ((states (list (list :id "s2" :name "Review" :group "started" :sequence 5000)
                         (list :id "s1" :name "Working" :group "started" :sequence 1000)
                         (list :id "s3" :name "Testing" :group "started" :sequence 9000)))
           (plane-org-sync--state-cache (list (cons "proj-1" states))))
      ;; Should pick "Working" (sequence 1000), not "Review" (sequence 5000).
      (should (equal (plane-org-sync-engine--keyword-to-state
                      "STARTED" "proj-1" nil)
                     "s1")))))

;;;; collect-keywords Tests

(ert-deftest plane-org-sync-engine-test-collect-keywords-standard ()
  "Standard 5-group set produces correct active and done keywords."
  (test-plane-org-sync-engine--with-default-config
    (let ((result (plane-org-sync-engine--collect-keywords
                   test-plane-org-sync-engine--sample-states)))
      ;; Active: BACKLOG (backlog), TODO (unstarted), STARTED (started x2)
      (should (equal (car result) '("BACKLOG" "TODO" "STARTED")))
      ;; Done: DONE (completed), CANCELLED (cancelled)
      (should (equal (cdr result) '("DONE" "CANCELLED"))))))

(ert-deftest plane-org-sync-engine-test-collect-keywords-with-overrides ()
  "Custom state-mapping overrides appear in keyword lists."
  (test-plane-org-sync-engine--with-custom-config
    (let ((result (plane-org-sync-engine--collect-keywords
                   test-plane-org-sync-engine--sample-states)))
      ;; "In Review" maps to "REVIEW" instead of "STARTED".
      (should (member "REVIEW" (car result)))
      (should (member "STARTED" (car result))))))

(ert-deftest plane-org-sync-engine-test-collect-keywords-deduplication ()
  "Duplicate keywords from multiple states in the same group are deduplicated."
  (test-plane-org-sync-engine--with-default-config
    (let* ((states (list (list :id "s1" :name "State A" :group "started" :sequence 1000)
                         (list :id "s2" :name "State B" :group "started" :sequence 2000)
                         (list :id "s3" :name "State C" :group "started" :sequence 3000)))
           (result (plane-org-sync-engine--collect-keywords states)))
      ;; All three map to STARTED, but should appear only once.
      (should (equal (car result) '("STARTED")))
      (should (null (cdr result))))))

(ert-deftest plane-org-sync-engine-test-collect-keywords-empty ()
  "Empty states list produces empty keyword lists."
  (test-plane-org-sync-engine--with-default-config
    (let ((result (plane-org-sync-engine--collect-keywords nil)))
      (should (null (car result)))
      (should (null (cdr result))))))

;;;; diff Tests

(ert-deftest plane-org-sync-engine-test-diff-all-create ()
  "All remote items are new when local is empty."
  (let* ((remote (list (list :id "r1" :updated_at "2026-01-01T00:00:00Z")
                       (list :id "r2" :updated_at "2026-01-02T00:00:00Z")))
         (result (plane-org-sync-engine--diff remote nil)))
    (should (= (length (plist-get result :create)) 2))
    (should (null (plist-get result :update)))
    (should (null (plist-get result :unchanged)))
    (should (null (plist-get result :orphaned)))))

(ert-deftest plane-org-sync-engine-test-diff-all-update ()
  "All items need update when timestamps differ."
  (let* ((remote (list (list :id "r1" :updated_at "2026-01-02T00:00:00Z")
                       (list :id "r2" :updated_at "2026-01-03T00:00:00Z")))
         (local (list (list :plane-id "r1" :plane-updated-at "2026-01-01T00:00:00Z")
                      (list :plane-id "r2" :plane-updated-at "2026-01-01T00:00:00Z")))
         (result (plane-org-sync-engine--diff remote local)))
    (should (null (plist-get result :create)))
    (should (= (length (plist-get result :update)) 2))
    (should (null (plist-get result :unchanged)))
    (should (null (plist-get result :orphaned)))
    ;; Each update entry is (heading . work-item).
    (let ((first-update (car (plist-get result :update))))
      (should (equal (plist-get (car first-update) :plane-id) "r1"))
      (should (equal (plist-get (cdr first-update) :id) "r1")))))

(ert-deftest plane-org-sync-engine-test-diff-all-unchanged ()
  "All items unchanged when timestamps match."
  (let* ((remote (list (list :id "r1" :updated_at "2026-01-01T00:00:00Z")
                       (list :id "r2" :updated_at "2026-01-02T00:00:00Z")))
         (local (list (list :plane-id "r1" :plane-updated-at "2026-01-01T00:00:00Z")
                      (list :plane-id "r2" :plane-updated-at "2026-01-02T00:00:00Z")))
         (result (plane-org-sync-engine--diff remote local)))
    (should (null (plist-get result :create)))
    (should (null (plist-get result :update)))
    (should (= (length (plist-get result :unchanged)) 2))
    (should (null (plist-get result :orphaned)))))

(ert-deftest plane-org-sync-engine-test-diff-mixed ()
  "Mixed scenario: some create, some update, some unchanged."
  (let* ((remote (list (list :id "r1" :updated_at "2026-01-01T00:00:00Z")   ; unchanged
                       (list :id "r2" :updated_at "2026-01-03T00:00:00Z")   ; update
                       (list :id "r3" :updated_at "2026-01-01T00:00:00Z"))) ; create
         (local (list (list :plane-id "r1" :plane-updated-at "2026-01-01T00:00:00Z")
                      (list :plane-id "r2" :plane-updated-at "2026-01-02T00:00:00Z")))
         (result (plane-org-sync-engine--diff remote local)))
    (should (= (length (plist-get result :create)) 1))
    (should (equal (plist-get (car (plist-get result :create)) :id) "r3"))
    (should (= (length (plist-get result :update)) 1))
    (should (equal (plist-get (cdr (car (plist-get result :update))) :id) "r2"))
    (should (= (length (plist-get result :unchanged)) 1))
    (should (null (plist-get result :orphaned)))))

(ert-deftest plane-org-sync-engine-test-diff-orphaned ()
  "Local headings not in remote are orphaned."
  (let* ((remote (list (list :id "r1" :updated_at "2026-01-01T00:00:00Z")))
         (local (list (list :plane-id "r1" :plane-updated-at "2026-01-01T00:00:00Z")
                      (list :plane-id "r2" :plane-updated-at "2026-01-01T00:00:00Z")
                      (list :plane-id "r3" :plane-updated-at "2026-01-01T00:00:00Z")))
         (result (plane-org-sync-engine--diff remote local)))
    (should (= (length (plist-get result :unchanged)) 1))
    (should (= (length (plist-get result :orphaned)) 2))
    (let ((orphan-ids (mapcar (lambda (h) (plist-get h :plane-id))
                              (plist-get result :orphaned))))
      (should (member "r2" orphan-ids))
      (should (member "r3" orphan-ids)))))

(ert-deftest plane-org-sync-engine-test-diff-empty-remote ()
  "Empty remote list makes all local headings orphaned."
  (let* ((local (list (list :plane-id "r1" :plane-updated-at "2026-01-01T00:00:00Z")
                      (list :plane-id "r2" :plane-updated-at "2026-01-02T00:00:00Z")))
         (result (plane-org-sync-engine--diff nil local)))
    (should (null (plist-get result :create)))
    (should (null (plist-get result :update)))
    (should (null (plist-get result :unchanged)))
    (should (= (length (plist-get result :orphaned)) 2))))

(ert-deftest plane-org-sync-engine-test-diff-empty-both ()
  "Empty remote and local produces empty diff."
  (let ((result (plane-org-sync-engine--diff nil nil)))
    (should (null (plist-get result :create)))
    (should (null (plist-get result :update)))
    (should (null (plist-get result :unchanged)))
    (should (null (plist-get result :orphaned)))))

;;;; detect-conflict Tests

(ert-deftest plane-org-sync-engine-test-detect-conflict-remote-newer ()
  "Remote newer than local is a conflict."
  (should (plane-org-sync-engine--detect-conflict
           "2026-01-01T00:00:00Z"
           "2026-01-02T00:00:00Z")))

(ert-deftest plane-org-sync-engine-test-detect-conflict-same ()
  "Same timestamps are not a conflict."
  (should-not (plane-org-sync-engine--detect-conflict
               "2026-01-01T00:00:00Z"
               "2026-01-01T00:00:00Z")))

(ert-deftest plane-org-sync-engine-test-detect-conflict-remote-older ()
  "Remote older than local is not a conflict."
  (should-not (plane-org-sync-engine--detect-conflict
               "2026-01-02T00:00:00Z"
               "2026-01-01T00:00:00Z")))

(ert-deftest plane-org-sync-engine-test-detect-conflict-nil-local ()
  "Nil local timestamp is not a conflict (first sync)."
  (should-not (plane-org-sync-engine--detect-conflict
               nil
               "2026-01-02T00:00:00Z")))

(ert-deftest plane-org-sync-engine-test-detect-conflict-nil-remote ()
  "Nil remote timestamp is not a conflict."
  (should-not (plane-org-sync-engine--detect-conflict
               "2026-01-01T00:00:00Z"
               nil)))

(ert-deftest plane-org-sync-engine-test-detect-conflict-nil-both ()
  "Nil both timestamps is not a conflict."
  (should-not (plane-org-sync-engine--detect-conflict nil nil)))

(provide 'test-plane-org-sync-engine)
;;; test-plane-org-sync-engine.el ends here
