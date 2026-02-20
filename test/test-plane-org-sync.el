;;; test-plane-org-sync.el --- Tests for plane-org-sync  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Martin

;; Author: Martin
;; Maintainer: Martin
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the main plane-org-sync entry point: async utilities,
;; pull/push commands, minor mode, and auxiliary commands.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'plane-org-sync)

;; Ensure org-state and org-last-state are defined as special variables.
;; Org-mode only sets them dynamically during hook execution; we need
;; them bound for tests that simulate the hook context.
(defvar org-state nil "Current TODO state (set by org during hook).")
(defvar org-last-state nil "Previous TODO state (set by org during hook).")

;;;; Test Helpers

(defvar test-plane-org-sync--api-calls nil
  "List of (METHOD PATH BODY) plists recording API calls during tests.")

(defvar test-plane-org-sync--api-responses nil
  "Alist of (PATH . RESPONSE) for mock API responses.
RESPONSE is either a plist (returned as parsed JSON) or a cons
cell (STATUS-CODE . BODY) for async responses.")

(defun test-plane-org-sync--mock-request (_method path &optional _body callback)
  "Mock async API request.  Records the call and responds from lookup table."
  (push (list :method _method :path path :body _body) test-plane-org-sync--api-calls)
  (let* ((response (cdr (assoc path test-plane-org-sync--api-responses
                                #'string-match-p)))
         (status-code (if (consp response) (car response) 200))
         (body (if (consp response) (cdr response) response)))
    (when callback
      (funcall callback status-code body))))

(defun test-plane-org-sync--mock-request-sync (_method path &optional _body)
  "Mock synchronous API request.  Records the call and returns from lookup table.
Responses in the table are returned as-is (plists).  To simulate an
error, use a cons cell (error . MESSAGE)."
  (push (list :method _method :path path :body _body) test-plane-org-sync--api-calls)
  (let ((response (cdr (assoc path test-plane-org-sync--api-responses
                               #'string-match-p))))
    (if (and (consp response) (eq (car response) 'error))
        (error "%s" (cdr response))
      response)))

(defun test-plane-org-sync--mock-paginate (path &optional _params callback)
  "Mock paginate.  Returns results from lookup table."
  (let* ((response (cdr (assoc path test-plane-org-sync--api-responses
                                #'string-match-p)))
         (items (if (listp response) response nil)))
    (when callback
      (funcall callback items))))

(defun test-plane-org-sync--mock-list-states (project-id callback)
  "Mock list-states for PROJECT-ID."
  (push (list :method "GET" :path (format "states/%s" project-id)
              :body nil)
        test-plane-org-sync--api-calls)
  (let ((states (cdr (assoc (format "states/%s" project-id)
                            test-plane-org-sync--api-responses
                            #'string-match-p))))
    (funcall callback 200 (or states '()))))

(defun test-plane-org-sync--mock-list-work-items
    (project-id &optional params callback)
  "Mock list-work-items for PROJECT-ID with PARAMS."
  (push (list :method "GET"
              :path (format "work-items/%s" project-id)
              :body params)
        test-plane-org-sync--api-calls)
  (let ((items (cdr (assoc (format "work-items/%s" project-id)
                           test-plane-org-sync--api-responses
                           #'string-match-p))))
    (funcall callback (or items '()))))

(defun test-plane-org-sync--mock-api-me (callback)
  "Mock api-me."
  (push (list :method "GET" :path "/users/me/" :body nil)
        test-plane-org-sync--api-calls)
  (funcall callback 200 '(:id "user-123" :display_name "Test User")))

(defvar test-plane-org-sync--browse-url-called nil
  "URL passed to the mock `browse-url'.")

(defun test-plane-org-sync--make-work-item (id &optional overrides)
  "Create a test work item plist with ID and optional OVERRIDES."
  (let ((item (list :id id
                    :name (format "Test Item %s" id)
                    :project "proj-1"
                    :project_identifier "TEST"
                    :sequence_id 42
                    :state "state-started"
                    :state_detail (list :id "state-started"
                                        :name "In Progress"
                                        :group "started")
                    :priority "medium"
                    :updated_at "2026-01-15T10:00:00Z"
                    :description_html "<p>Test description</p>"
                    :labels []
                    :assignee_details []
                    :start_date nil
                    :target_date nil
                    :is_draft nil
                    :archived_at nil)))
    (while overrides
      (setq item (plist-put item (car overrides) (cadr overrides)))
      (setq overrides (cddr overrides)))
    item))

(defun test-plane-org-sync--make-states ()
  "Return a standard list of test state plists."
  (list (list :id "state-backlog"   :name "Backlog"     :group "backlog"   :sequence 0)
        (list :id "state-todo"      :name "Todo"        :group "unstarted" :sequence 1)
        (list :id "state-started"   :name "In Progress" :group "started"   :sequence 2)
        (list :id "state-done"      :name "Done"        :group "completed" :sequence 3)
        (list :id "state-cancelled" :name "Cancelled"   :group "cancelled" :sequence 4)))

(defmacro test-plane-org-sync--with-temp-org (&rest body)
  "Execute BODY with a temporary org file as `plane-org-sync-file'.
Cleans up the temp file and any visiting buffer afterward."
  (declare (indent 0) (debug t))
  `(let* ((temp-file (make-temp-file "plane-org-sync-test-" nil ".org"))
          (plane-org-sync-file temp-file)
          (plane-org-sync-instance-url "https://plane.example.com")
          (plane-org-sync-workspace "test-ws")
          (plane-org-sync-projects '("proj-1"))
          (plane-org-sync-filter-assignee t)
          (plane-org-sync--state-cache nil)
          (plane-org-sync--user-id nil)
          (plane-org-sync--last-sync nil)
          (plane-org-sync--last-sync-result nil)
          (plane-org-sync--inhibit-push nil)
          (plane-org-sync-group-keyword-mapping
           '((backlog . "BACKLOG")
             (unstarted . "TODO")
             (started . "STARTED")
             (completed . "DONE")
             (cancelled . "CANCELLED")))
          (plane-org-sync-state-mapping nil)
          (test-plane-org-sync--api-calls nil)
          (test-plane-org-sync--api-responses nil))
     (unwind-protect
         (progn ,@body)
       (let ((buf (find-buffer-visiting temp-file)))
         (when buf
           (with-current-buffer buf
             (set-buffer-modified-p nil))
           (kill-buffer buf)))
       (when (file-exists-p temp-file)
         (delete-file temp-file)))))

;;;; Tests: Feature Provided

(ert-deftest plane-org-sync-test-feature-provided ()
  "The `plane-org-sync' feature should be available."
  (should (featurep 'plane-org-sync)))

;;;; Tests: --chain

(ert-deftest plane-org-sync-test-chain-sequential ()
  "Chain executes steps in order, passing results forward."
  (let ((trace nil))
    (plane-org-sync--chain
     (list
      (lambda (next)
        (push 'step-1 trace)
        (funcall next 'result-1))
      (lambda (next &rest prev)
        (push (cons 'step-2 prev) trace)
        (funcall next 'result-2))))
    (should (equal (car trace) '(step-2 result-1)))
    (should (equal (cadr trace) 'step-1))))

(ert-deftest plane-org-sync-test-chain-error-aborts ()
  "Chain stops on error and calls error-callback."
  (let ((trace nil)
        (error-msg nil))
    (plane-org-sync--chain
     (list
      (lambda (next)
        (push 'step-1 trace)
        (funcall next 'error "boom"))
      (lambda (next &rest _prev)
        (push 'step-2-should-not-run trace)
        (funcall next)))
     (lambda (msg) (setq error-msg msg)))
    (should (equal trace '(step-1)))
    (should (equal error-msg "boom"))))

(ert-deftest plane-org-sync-test-chain-empty ()
  "Chain with no steps does nothing."
  (should-not (plane-org-sync--chain nil)))

;;;; Tests: --collect

(ert-deftest plane-org-sync-test-collect-fan-out ()
  "Collect gathers results from multiple async operations."
  (let ((result nil))
    (plane-org-sync--collect
     (list
      (lambda (cb) (funcall cb 'a))
      (lambda (cb) (funcall cb 'b))
      (lambda (cb) (funcall cb 'c)))
     (lambda (results) (setq result results)))
    (should (equal result '(a b c)))))

(ert-deftest plane-org-sync-test-collect-empty ()
  "Collect with empty fn list calls callback with nil."
  (let ((result 'unset))
    (plane-org-sync--collect nil (lambda (r) (setq result r)))
    (should (null result))))

(ert-deftest plane-org-sync-test-collect-preserves-order ()
  "Collect preserves order even if callbacks fire out of sequence."
  (let ((result nil)
        (callbacks (make-vector 3 nil)))
    (plane-org-sync--collect
     (list
      (lambda (cb) (aset callbacks 0 cb))
      (lambda (cb) (aset callbacks 1 cb))
      (lambda (cb) (aset callbacks 2 cb)))
     (lambda (results) (setq result results)))
    ;; Fire in reverse order.
    (funcall (aref callbacks 2) 'third)
    (funcall (aref callbacks 0) 'first)
    (funcall (aref callbacks 1) 'second)
    (should (equal result '(first second third)))))

;;;; Tests: Full Pull Cycle

(ert-deftest plane-org-sync-test-pull-full-cycle ()
  "Full pull creates headings from API work items."
  (test-plane-org-sync--with-temp-org
    (let ((states (test-plane-org-sync--make-states))
          (items (list (test-plane-org-sync--make-work-item "item-1")
                       (test-plane-org-sync--make-work-item
                        "item-2"
                        (list :name "Second Item"
                              :state "state-done"
                              :state_detail (list :id "state-done"
                                                   :name "Done"
                                                   :group "completed")
                              :sequence_id 43))))
          (sync-done nil))
      (setq test-plane-org-sync--api-responses
            `(("states/proj-1" . ,states)
              ("work-items/proj-1" . ,items)))
      (cl-letf (((symbol-function 'plane-org-sync-api-me)
                 #'test-plane-org-sync--mock-api-me)
                ((symbol-function 'plane-org-sync-api-list-states)
                 #'test-plane-org-sync--mock-list-states)
                ((symbol-function 'plane-org-sync-api-list-work-items)
                 #'test-plane-org-sync--mock-list-work-items))
        (plane-org-sync-pull (lambda (result) (setq sync-done result))))
      ;; Verify results.
      (should sync-done)
      (should (= (plist-get sync-done :created) 2))
      (should (= (plist-get sync-done :updated) 0))
      ;; Verify org file content.
      (let ((content (with-temp-buffer
                       (insert-file-contents plane-org-sync-file)
                       (buffer-string))))
        (should (string-match-p "Test Item item-1" content))
        (should (string-match-p "Second Item" content))
        (should (string-match-p "PLANE_ID.*item-1" content))
        (should (string-match-p "PLANE_ID.*item-2" content))
        (should (string-match-p "STARTED" content))
        (should (string-match-p "DONE" content)))
      ;; Verify state was recorded.
      (should plane-org-sync--last-sync)
      (should plane-org-sync--last-sync-result)
      (should (equal plane-org-sync--user-id "user-123")))))

;;;; Tests: Idempotent Pull

(ert-deftest plane-org-sync-test-pull-idempotent ()
  "Pulling twice with same data produces identical file content."
  (test-plane-org-sync--with-temp-org
    (let ((states (test-plane-org-sync--make-states))
          (items (list (test-plane-org-sync--make-work-item "item-1")))
          (result-1 nil)
          (result-2 nil))
      (setq test-plane-org-sync--api-responses
            `(("states/proj-1" . ,states)
              ("work-items/proj-1" . ,items)))
      (cl-letf (((symbol-function 'plane-org-sync-api-me)
                 #'test-plane-org-sync--mock-api-me)
                ((symbol-function 'plane-org-sync-api-list-states)
                 #'test-plane-org-sync--mock-list-states)
                ((symbol-function 'plane-org-sync-api-list-work-items)
                 #'test-plane-org-sync--mock-list-work-items))
        (plane-org-sync-pull (lambda (r) (setq result-1 r)))
        (let ((content-1 (with-temp-buffer
                           (insert-file-contents plane-org-sync-file)
                           (buffer-string))))
          ;; Pull again with same data.
          (plane-org-sync-pull (lambda (r) (setq result-2 r)))
          (let ((content-2 (with-temp-buffer
                             (insert-file-contents plane-org-sync-file)
                             (buffer-string))))
            ;; File content should be identical.
            (should (equal content-1 content-2))
            ;; Second pull should show 0 created, 0 updated.
            (should (= (plist-get result-2 :created) 0))
            (should (= (plist-get result-2 :updated) 0))
            (should (= (plist-get result-2 :unchanged) 1))))))))

;;;; Tests: State Push Cycle

(ert-deftest plane-org-sync-test-push-state-change ()
  "Changing TODO keyword pushes state change to Plane."
  (test-plane-org-sync--with-temp-org
    (let ((states (test-plane-org-sync--make-states))
          (items (list (test-plane-org-sync--make-work-item "item-1"))))
      ;; First pull to create heading.
      (setq test-plane-org-sync--api-responses
            `(("states/proj-1" . ,states)
              ("work-items/proj-1" . ,items)))
      (cl-letf (((symbol-function 'plane-org-sync-api-me)
                 #'test-plane-org-sync--mock-api-me)
                ((symbol-function 'plane-org-sync-api-list-states)
                 #'test-plane-org-sync--mock-list-states)
                ((symbol-function 'plane-org-sync-api-list-work-items)
                 #'test-plane-org-sync--mock-list-work-items))
        (plane-org-sync-pull #'ignore))
      ;; Now mock sync API for push.
      (setq test-plane-org-sync--api-calls nil)
      (setq test-plane-org-sync--api-responses
            `(("/workspaces/test-ws/projects/proj-1/work-items/item-1/"
               . (:id "item-1"
                  :updated_at "2026-01-15T10:00:00Z"
                  :state "state-started"))))
      (cl-letf (((symbol-function 'plane-org-sync-api--request-sync)
                 #'test-plane-org-sync--mock-request-sync))
        ;; Open the org file and change the TODO state.
        (with-current-buffer (find-file-noselect plane-org-sync-file)
          (org-with-wide-buffer
           (goto-char (point-min))
           (org-next-visible-heading 1)
           ;; Simulate the hook environment.
           (let ((org-state "DONE")
                 (org-last-state "STARTED")
                 (plane-org-sync--inhibit-push nil))
             (plane-org-sync--on-todo-state-change))))
        ;; Verify PATCH was called.
        (let ((patch-call (seq-find
                           (lambda (c)
                             (equal (plist-get c :method) "PATCH"))
                           test-plane-org-sync--api-calls)))
          (should patch-call)
          (should (equal (plist-get (plist-get patch-call :body) :state)
                         "state-done")))))))

;;;; Tests: Push Suppression During Pull

(ert-deftest plane-org-sync-test-push-suppressed-during-pull ()
  "The push hook does not fire when --inhibit-push is t."
  (test-plane-org-sync--with-temp-org
    (let ((push-called nil))
      (cl-letf (((symbol-function 'plane-org-sync-api--request-sync)
                 (lambda (&rest _args)
                   (setq push-called t)
                   '(:id "x" :updated_at "2026-01-01T00:00:00Z"))))
        ;; Simulate hook context with inhibit-push.
        (with-temp-buffer
          (org-mode)
          (insert "* STARTED Test\n:PROPERTIES:\n:PLANE_ID: item-1\n")
          (insert ":PLANE_PROJECT_ID: proj-1\n:PLANE_STATE_ID: state-started\n")
          (insert ":END:\n")
          (goto-char (point-min))
          (let ((org-state "DONE")
                (org-last-state "STARTED")
                (plane-org-sync--inhibit-push t))
            (plane-org-sync--on-todo-state-change))))
      (should-not push-called))))

;;;; Tests: Push with Conflict

(ert-deftest plane-org-sync-test-push-conflict-declined ()
  "When remote is newer and user declines, keyword is reverted."
  (test-plane-org-sync--with-temp-org
    ;; Populate state cache.
    (setq plane-org-sync--state-cache
          `(("proj-1" . ,(test-plane-org-sync--make-states))))
    (setq test-plane-org-sync--api-responses
          `(("/workspaces/test-ws/projects/proj-1/work-items/item-1/"
             . (:id "item-1"
                :updated_at "2026-02-01T00:00:00Z"
                :state "state-started"))))
    (let ((reverted nil))
      (cl-letf (((symbol-function 'plane-org-sync-api--request-sync)
                 #'test-plane-org-sync--mock-request-sync)
                ((symbol-function 'org-todo)
                 (lambda (kw) (setq reverted kw))))
        (with-temp-buffer
          (org-mode)
          (insert "* STARTED Test\n:PROPERTIES:\n:PLANE_ID: item-1\n")
          (insert ":PLANE_PROJECT_ID: proj-1\n")
          (insert ":PLANE_STATE_ID: state-started\n")
          (insert ":PLANE_UPDATED_AT: 2026-01-15T10:00:00Z\n")
          (insert ":END:\n")
          (goto-char (point-min))
          (let ((org-state "DONE")
                (org-last-state "STARTED")
                (plane-org-sync--inhibit-push nil)
                (plane-org-sync-conflict-function (lambda (_prompt) nil)))
            (should-error
             (plane-org-sync--on-todo-state-change)
             :type 'user-error))
          (should (equal reverted "STARTED")))))))

;;;; Tests: Push with Error

(ert-deftest plane-org-sync-test-push-error-reverts ()
  "When API call fails, keyword is reverted."
  (test-plane-org-sync--with-temp-org
    ;; Populate state cache.
    (setq plane-org-sync--state-cache
          `(("proj-1" . ,(test-plane-org-sync--make-states))))
    (setq test-plane-org-sync--api-responses
          `(("/workspaces/test-ws/projects/proj-1/work-items/item-1/"
             . (error . "Network timeout"))))
    (let ((reverted nil))
      (cl-letf (((symbol-function 'plane-org-sync-api--request-sync)
                 #'test-plane-org-sync--mock-request-sync)
                ((symbol-function 'org-todo)
                 (lambda (kw) (setq reverted kw))))
        (with-temp-buffer
          (org-mode)
          (insert "* STARTED Test\n:PROPERTIES:\n:PLANE_ID: item-1\n")
          (insert ":PLANE_PROJECT_ID: proj-1\n")
          (insert ":PLANE_STATE_ID: state-started\n")
          (insert ":PLANE_UPDATED_AT: 2026-01-15T10:00:00Z\n")
          (insert ":END:\n")
          (goto-char (point-min))
          (let ((org-state "DONE")
                (org-last-state "STARTED")
                (plane-org-sync--inhibit-push nil))
            (plane-org-sync--on-todo-state-change))
          (should (equal reverted "STARTED")))))))

;;;; Tests: Minor Mode

(ert-deftest plane-org-sync-test-mode-enable-disable ()
  "Enabling mode adds hook, disabling removes it."
  (test-plane-org-sync--with-temp-org
    (unwind-protect
        (progn
          (plane-org-sync-mode 1)
          (should (memq #'plane-org-sync--on-todo-state-change
                        org-after-todo-state-change-hook))
          (should (member plane-org-sync-file org-agenda-files))
          (plane-org-sync-mode -1)
          (should-not (memq #'plane-org-sync--on-todo-state-change
                            org-after-todo-state-change-hook))
          (should-not (member plane-org-sync-file org-agenda-files)))
      ;; Ensure mode is off after test.
      (plane-org-sync-mode -1))))

;;;; Tests: Browse Command

(ert-deftest plane-org-sync-test-browse ()
  "Browse opens the correct URL."
  (require 'browse-url)
  (let ((browsed-url nil))
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url &rest _) (setq browsed-url url))))
      (with-temp-buffer
        (org-mode)
        (insert "* TODO Test\n:PROPERTIES:\n")
        (insert ":PLANE_URL: https://plane.example.com/ws/projects/p/work-items/42\n")
        (insert ":END:\n")
        (goto-char (point-min))
        (org-back-to-heading t)
        (plane-org-sync-browse)
        (should (equal browsed-url
                       "https://plane.example.com/ws/projects/p/work-items/42"))))))

(ert-deftest plane-org-sync-test-browse-no-url ()
  "Browse errors when not on a synced heading."
  (with-temp-buffer
    (org-mode)
    (insert "* TODO Regular heading\n")
    (goto-char (point-min))
    (should-error (plane-org-sync-browse) :type 'user-error)))

;;;; Tests: Status Command

(ert-deftest plane-org-sync-test-status ()
  "Status command reports last sync info."
  (let ((plane-org-sync--last-sync (encode-time 0 30 14 15 1 2026))
        (plane-org-sync--last-sync-result '(:created 3 :updated 1 :unchanged 5))
        (plane-org-sync--state-cache '(("proj-1" . ((:id "s1")))))
        (plane-org-sync--user-id "user-abc")
        (msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (plane-org-sync-status)
      (should (string-match-p "2026-01-15" msg))
      (should (string-match-p "created=3" msg))
      (should (string-match-p "updated=1" msg))
      (should (string-match-p "user-abc" msg)))))

(ert-deftest plane-org-sync-test-status-never-synced ()
  "Status reports never synced when no sync has occurred."
  (let ((plane-org-sync--last-sync nil)
        (plane-org-sync--last-sync-result nil)
        (plane-org-sync--state-cache nil)
        (plane-org-sync--user-id nil)
        (msg nil))
    (cl-letf (((symbol-function 'message)
               (lambda (fmt &rest args) (setq msg (apply #'format fmt args)))))
      (plane-org-sync-status)
      (should (string-match-p "Never synced" msg))
      (should (string-match-p "Unknown" msg)))))

;;;; Tests: Reset Command

(ert-deftest plane-org-sync-test-reset ()
  "Reset clears all caches, strips properties, and triggers a pull."
  (test-plane-org-sync--with-temp-org
    ;; Pre-populate cache.
    (setq plane-org-sync--state-cache '(("proj-1" . ((:id "s1")))))
    (setq plane-org-sync--user-id "user-1")
    (setq plane-org-sync--last-sync (current-time))
    (setq plane-org-sync--last-sync-result '(:created 1))
    ;; Write a heading with PLANE_* properties.
    (with-temp-buffer
      (insert "* TODO Test\n:PROPERTIES:\n:PLANE_ID: item-1\n")
      (insert ":PLANE_UPDATED_AT: 2026-01-01\n:END:\n")
      (write-region (point-min) (point-max) plane-org-sync-file nil 'quiet))
    ;; Mock yes-or-no-p and pull.
    (let ((pull-called nil))
      (cl-letf (((symbol-function 'yes-or-no-p) (lambda (_prompt) t))
                ((symbol-function 'plane-org-sync-pull)
                 (lambda (&optional _cb) (setq pull-called t))))
        (plane-org-sync-reset)
        ;; Caches cleared.
        (should-not plane-org-sync--state-cache)
        (should-not plane-org-sync--user-id)
        (should-not plane-org-sync--last-sync)
        (should-not plane-org-sync--last-sync-result)
        ;; Pull was triggered.
        (should pull-called)
        ;; PLANE_* properties should be stripped from the file.
        (let ((content (with-temp-buffer
                         (insert-file-contents plane-org-sync-file)
                         (buffer-string))))
          (should-not (string-match-p ":PLANE_ID:" content))
          (should-not (string-match-p ":PLANE_UPDATED_AT:" content)))))))

;;;; Tests: Pull Rejects Modified Buffer

(ert-deftest plane-org-sync-test-pull-rejects-modified-buffer ()
  "Pull aborts if the sync file has unsaved modifications."
  (test-plane-org-sync--with-temp-org
    ;; Create the file and mark the buffer as modified.
    (let ((buf (plane-org-sync-org--get-buffer plane-org-sync-file)))
      (with-current-buffer buf
        (insert "unsaved change")
        (set-buffer-modified-p t))
      (should-error (plane-org-sync-pull) :type 'user-error))))

;;;; Tests: Pull with No Projects

(ert-deftest plane-org-sync-test-pull-no-projects ()
  "Pull errors when no projects are configured."
  (let ((plane-org-sync-projects nil))
    (should-error (plane-org-sync-pull) :type 'user-error)))

;;;; Tests: Expanded Field Normalizers

(ert-deftest plane-org-sync-test-normalize-state-expanded ()
  "Normalizer extracts state info from an expanded state plist."
  (let* ((item (list :id "item-1"
                     :state (list :id "state-1" :name "In Progress"
                                  :group "started" :color "#F59E0B")))
         (result (plane-org-sync--normalize-state item)))
    (should (equal (plist-get result :state) "state-1"))
    (should (equal (plist-get result :state_id) "state-1"))
    (should (equal (plist-get result :state_name) "In Progress"))
    (should (equal (plist-get result :state_group) "started"))))

(ert-deftest plane-org-sync-test-normalize-state-plain-uuid ()
  "Normalizer handles plain UUID state field."
  (let* ((item (list :id "item-1" :state "state-uuid-123"))
         (result (plane-org-sync--normalize-state item)))
    (should (equal (plist-get result :state) "state-uuid-123"))
    (should (equal (plist-get result :state_id) "state-uuid-123"))))

(ert-deftest plane-org-sync-test-normalize-state-with-detail ()
  "Normalizer extracts from state_detail when state is a UUID."
  (let* ((item (list :id "item-1"
                     :state "state-uuid-123"
                     :state_detail (list :id "state-uuid-123"
                                         :name "Done" :group "completed")))
         (result (plane-org-sync--normalize-state item)))
    (should (equal (plist-get result :state_name) "Done"))
    (should (equal (plist-get result :state_group) "completed"))
    (should (equal (plist-get result :state_id) "state-uuid-123"))))

(ert-deftest plane-org-sync-test-normalize-labels-expanded ()
  "Normalizer handles expanded label objects."
  (let* ((item (list :id "item-1"
                     :labels (vector (list :id "lbl-1" :name "Backend")
                                     (list :id "lbl-2" :name "Frontend"))))
         (result (plane-org-sync--normalize-labels item))
         (labels (plist-get result :labels)))
    (should (listp labels))
    (should (= 2 (length labels)))
    (should (equal "Backend" (plist-get (car labels) :name)))))

(ert-deftest plane-org-sync-test-normalize-labels-uuid-strings ()
  "Normalizer converts UUID string labels to plists."
  (let* ((item (list :id "item-1"
                     :labels (vector "uuid-1" "uuid-2")))
         (result (plane-org-sync--normalize-labels item))
         (labels (plist-get result :labels)))
    (should (listp labels))
    (should (= 2 (length labels)))
    (should (equal "uuid-1" (plist-get (car labels) :name)))))

(ert-deftest plane-org-sync-test-normalize-assignees-expanded ()
  "Normalizer populates assignee_details from expanded :assignees."
  (let* ((item (list :id "item-1"
                     :assignees (vector (list :id "u-1"
                                              :display_name "Alice")
                                        (list :id "u-2"
                                              :display_name "Bob"))
                     :assignee_details []))
         (result (plane-org-sync--normalize-assignees item))
         (details (plist-get result :assignee_details)))
    (should (listp details))
    (should (= 2 (length details)))
    (should (equal "Alice" (plist-get (car details) :display_name)))))

(ert-deftest plane-org-sync-test-normalize-assignees-keeps-existing-details ()
  "Normalizer keeps existing assignee_details when populated."
  (let* ((details (vector (list :display_name "Existing")))
         (item (list :id "item-1"
                     :assignees []
                     :assignee_details details))
         (result (plane-org-sync--normalize-assignees item)))
    (should (equal "Existing"
                   (plist-get (car (plist-get result :assignee_details))
                              :display_name)))))

;;;; Tests: Full Pull with Expanded API Fields

(ert-deftest plane-org-sync-test-pull-expanded-state ()
  "Pull handles expanded state objects instead of UUID strings."
  (test-plane-org-sync--with-temp-org
    (let* ((states (test-plane-org-sync--make-states))
           ;; Simulate expanded API response: :state is a plist, not a UUID.
           (items (list (list :id "item-exp-1"
                              :name "Expanded State Item"
                              :project "proj-1"
                              :project_identifier "TEST"
                              :sequence_id 99
                              :state (list :id "state-started"
                                           :name "In Progress"
                                           :group "started"
                                           :color "#F59E0B")
                              :priority "medium"
                              :updated_at "2026-01-20T10:00:00Z"
                              :description_html nil
                              :labels (vector (list :id "lbl-1"
                                                    :name "Backend"))
                              :assignees (vector (list :id "u-1"
                                                       :display_name "Alice"))
                              :assignee_details []
                              :start_date nil
                              :target_date nil
                              :is_draft nil
                              :archived_at nil)))
           (sync-done nil))
      (setq test-plane-org-sync--api-responses
            `(("states/proj-1" . ,states)
              ("work-items/proj-1" . ,items)))
      (cl-letf (((symbol-function 'plane-org-sync-api-me)
                 #'test-plane-org-sync--mock-api-me)
                ((symbol-function 'plane-org-sync-api-list-states)
                 #'test-plane-org-sync--mock-list-states)
                ((symbol-function 'plane-org-sync-api-list-work-items)
                 #'test-plane-org-sync--mock-list-work-items))
        (plane-org-sync-pull (lambda (result) (setq sync-done result))))
      ;; Should succeed without error.
      (should sync-done)
      (should (= (plist-get sync-done :created) 1))
      ;; Verify org file content.
      (let ((content (with-temp-buffer
                       (insert-file-contents plane-org-sync-file)
                       (buffer-string))))
        ;; Headline should have STARTED keyword (from state group "started").
        (should (string-match-p "STARTED" content))
        (should (string-match-p "Expanded State Item" content))
        ;; State name should be written to property.
        (should (string-match-p ":PLANE_STATE: In Progress" content))
        (should (string-match-p ":PLANE_STATE_ID: state-started" content))
        ;; Label should appear as tag.
        (should (string-match-p ":backend:" content))
        ;; Assignee should appear in property.
        (should (string-match-p ":PLANE_ASSIGNEES: Alice" content))))))

;;;; Tests: Legacy Heading Repair (Bug Fixes)

(ert-deftest plane-org-sync-test-pull-repairs-legacy-headings ()
  "Pull repairs headings with missing PLANE_PROJECT_ID, state, and wrong URL.
Simulates upgrading from an older version that did not set these properties."
  (test-plane-org-sync--with-temp-org
    (let ((states (test-plane-org-sync--make-states))
          (items (list (test-plane-org-sync--make-work-item "item-1")))
          (result nil))
      (setq test-plane-org-sync--api-responses
            `(("states/proj-1" . ,states)
              ("work-items/proj-1" . ,items)))
      ;; Pre-populate with a legacy heading: no PLANE_PROJECT_ID,
      ;; empty state, and an api.plane.so URL.
      (with-temp-buffer
        (insert "#+TODO: BACKLOG TODO STARTED | DONE CANCELLED  # plane-org-sync-managed\n")
        (insert "* STARTED Test Item item-1\n")
        (insert ":PROPERTIES:\n")
        (insert ":PLANE_ID: item-1\n")
        (insert ":PLANE_URL: https://api.plane.so/test-ws/projects/proj-1/work-items/42\n")
        (insert ":PLANE_UPDATED_AT: 2026-01-15T10:00:00Z\n")
        (insert ":PLANE_STATE: \n")
        (insert ":PLANE_STATE_ID: \n")
        (insert ":END:\n")
        (insert "# plane-org-sync-description-begin\n")
        (insert "# plane-org-sync-description-end\n")
        (write-region (point-min) (point-max) plane-org-sync-file nil 'quiet))
      (cl-letf (((symbol-function 'plane-org-sync-api-me)
                 #'test-plane-org-sync--mock-api-me)
                ((symbol-function 'plane-org-sync-api-list-states)
                 #'test-plane-org-sync--mock-list-states)
                ((symbol-function 'plane-org-sync-api-list-work-items)
                 #'test-plane-org-sync--mock-list-work-items))
        (plane-org-sync-pull (lambda (r) (setq result r))))
      ;; Should be updated (repaired), not created as duplicate.
      (should (= (plist-get result :created) 0))
      (should (= (plist-get result :updated) 1))
      (let ((content (with-temp-buffer
                       (insert-file-contents plane-org-sync-file)
                       (buffer-string))))
        ;; Only one PLANE_ID occurrence (no duplicate).
        (let ((count 0) (start 0))
          (while (string-match ":PLANE_ID: item-1" content start)
            (setq count (1+ count))
            (setq start (match-end 0)))
          (should (= count 1)))
        ;; State should now be populated.
        (should (string-match-p ":PLANE_STATE: In Progress" content))
        (should (string-match-p ":PLANE_STATE_ID: state-started" content))
        ;; Project ID should now be populated.
        (should (string-match-p ":PLANE_PROJECT_ID: proj-1" content))
        ;; URL should use the instance URL (not api.plane.so).
        ;; The test uses plane.example.com via the test macro.
        (should (string-match-p "plane\\.example\\.com" content))
        (should-not (string-match-p "api\\.plane\\.so" content))))))

(ert-deftest plane-org-sync-test-pull-no-duplicate-missing-project-id ()
  "Pull does not create duplicates when PLANE_PROJECT_ID is missing.
This is the exact scenario from the user bug report."
  (test-plane-org-sync--with-temp-org
    (let ((states (test-plane-org-sync--make-states))
          (items (list (test-plane-org-sync--make-work-item "item-1")))
          (result-1 nil)
          (result-2 nil)
          (result-3 nil))
      (setq test-plane-org-sync--api-responses
            `(("states/proj-1" . ,states)
              ("work-items/proj-1" . ,items)))
      ;; Pre-populate with heading that has NO PLANE_PROJECT_ID.
      (with-temp-buffer
        (insert "#+TODO: BACKLOG TODO STARTED | DONE CANCELLED  # plane-org-sync-managed\n")
        (insert "* TODO Test Item item-1\n")
        (insert ":PROPERTIES:\n")
        (insert ":PLANE_ID: item-1\n")
        (insert ":PLANE_URL: https://api.plane.so/test-ws/projects/proj-1/work-items/42\n")
        (insert ":PLANE_UPDATED_AT: 2026-01-15T10:00:00Z\n")
        (insert ":PLANE_STATE: \n")
        (insert ":PLANE_STATE_ID: \n")
        (insert ":END:\n")
        (write-region (point-min) (point-max) plane-org-sync-file nil 'quiet))
      (cl-letf (((symbol-function 'plane-org-sync-api-me)
                 #'test-plane-org-sync--mock-api-me)
                ((symbol-function 'plane-org-sync-api-list-states)
                 #'test-plane-org-sync--mock-list-states)
                ((symbol-function 'plane-org-sync-api-list-work-items)
                 #'test-plane-org-sync--mock-list-work-items))
        ;; First pull: should repair, not duplicate.
        (plane-org-sync-pull (lambda (r) (setq result-1 r)))
        (should (= (plist-get result-1 :created) 0))
        ;; Second pull: should be unchanged now.
        (plane-org-sync-pull (lambda (r) (setq result-2 r)))
        (should (= (plist-get result-2 :created) 0))
        (should (= (plist-get result-2 :unchanged) 1))
        ;; Third pull: still unchanged.
        (plane-org-sync-pull (lambda (r) (setq result-3 r)))
        (should (= (plist-get result-3 :created) 0))
        (should (= (plist-get result-3 :unchanged) 1)))
      ;; Verify exactly one PLANE_ID in final file.
      (let ((content (with-temp-buffer
                       (insert-file-contents plane-org-sync-file)
                       (buffer-string))))
        (let ((count 0) (start 0))
          (while (string-match ":PLANE_ID: item-1" content start)
            (setq count (1+ count))
            (setq start (match-end 0)))
          (should (= count 1)))))))

(provide 'test-plane-org-sync)
;;; test-plane-org-sync.el ends here
