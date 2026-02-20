;;; test-plane-org-sync-org.el --- Tests for plane-org-sync-org  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Martin

;; Author: Martin
;; Maintainer: Martin
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; ERT tests for the Org interface module of plane-org-sync.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)
(require 'plane-org-sync-org)

;;;; Test Helpers

(defmacro plane-org-sync-test-with-org-buffer (content &rest body)
  "Execute BODY in a temp buffer with CONTENT in Org mode.
The Org element cache is disabled for deterministic behavior."
  (declare (indent 1))
  `(let ((org-element-use-cache nil))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (insert ,content)
       (goto-char (point-min))
       ,@body)))

(defun plane-org-sync-test--make-work-item (&rest overrides)
  "Create a test work item plist with default values.
OVERRIDES is a plist of keys to override."
  (let ((item (list :id "test-uuid-1234"
                    :name "Test work item"
                    :priority "medium"
                    :start_date nil
                    :target_date nil
                    :sequence_id 42
                    :project "proj-uuid-5678"
                    :project_identifier "PROJ"
                    :state_name "In Progress"
                    :state_id "state-uuid-9999"
                    :updated_at "2026-02-19T10:30:00Z"
                    :description_html nil
                    :labels []
                    :assignee_details [])))
    (while overrides
      (setq item (plist-put item (car overrides) (cadr overrides)))
      (setq overrides (cddr overrides)))
    item))

;;;; Feature Provided

(ert-deftest plane-org-sync-org-test-feature-provided ()
  "The `plane-org-sync-org' feature should be available."
  (should (featurep 'plane-org-sync-org)))

;;;; Priority Mapping: plane-to-org

(ert-deftest plane-org-sync-org-test-priority-to-org-urgent ()
  "Urgent priority maps to ?A."
  (should (equal ?A (plane-org-sync-org--priority-to-org "urgent"))))

(ert-deftest plane-org-sync-org-test-priority-to-org-high ()
  "High priority maps to ?A."
  (should (equal ?A (plane-org-sync-org--priority-to-org "high"))))

(ert-deftest plane-org-sync-org-test-priority-to-org-medium ()
  "Medium priority maps to ?B."
  (should (equal ?B (plane-org-sync-org--priority-to-org "medium"))))

(ert-deftest plane-org-sync-org-test-priority-to-org-low ()
  "Low priority maps to ?C."
  (should (equal ?C (plane-org-sync-org--priority-to-org "low"))))

(ert-deftest plane-org-sync-org-test-priority-to-org-none ()
  "None priority maps to nil."
  (should-not (plane-org-sync-org--priority-to-org "none")))

(ert-deftest plane-org-sync-org-test-priority-to-org-nil ()
  "Nil input maps to nil."
  (should-not (plane-org-sync-org--priority-to-org nil)))

;;;; Priority Mapping: org-to-plane

(ert-deftest plane-org-sync-org-test-priority-from-org-a ()
  "Org priority ?A maps to high."
  (should (equal "high" (plane-org-sync-org--priority-from-org ?A))))

(ert-deftest plane-org-sync-org-test-priority-from-org-b ()
  "Org priority ?B maps to medium."
  (should (equal "medium" (plane-org-sync-org--priority-from-org ?B))))

(ert-deftest plane-org-sync-org-test-priority-from-org-c ()
  "Org priority ?C maps to low."
  (should (equal "low" (plane-org-sync-org--priority-from-org ?C))))

(ert-deftest plane-org-sync-org-test-priority-from-org-nil ()
  "Nil Org priority maps to none."
  (should (equal "none" (plane-org-sync-org--priority-from-org nil))))

;;;; Tag Sanitization

(ert-deftest plane-org-sync-org-test-sanitize-tag-simple ()
  "Simple alphanumeric label passes through downcased."
  (should (equal "backend" (plane-org-sync-org--sanitize-tag "Backend"))))

(ert-deftest plane-org-sync-org-test-sanitize-tag-spaces ()
  "Spaces are replaced with underscores."
  (should (equal "front_end" (plane-org-sync-org--sanitize-tag "Front End"))))

(ert-deftest plane-org-sync-org-test-sanitize-tag-special-chars ()
  "Special characters are replaced with underscores."
  (should (equal "bug_fix" (plane-org-sync-org--sanitize-tag "bug/fix"))))

(ert-deftest plane-org-sync-org-test-sanitize-tag-leading-trailing ()
  "Leading and trailing underscores are removed."
  (should (equal "test" (plane-org-sync-org--sanitize-tag " test "))))

(ert-deftest plane-org-sync-org-test-sanitize-tag-preserves-allowed ()
  "Characters @, #, % are preserved."
  (should (equal "v@1#2%3" (plane-org-sync-org--sanitize-tag "V@1#2%3"))))

(ert-deftest plane-org-sync-org-test-sanitize-tag-empty-string ()
  "Empty string returns empty string."
  (should (equal "" (plane-org-sync-org--sanitize-tag ""))))

;;;; Build Plane URL

(ert-deftest plane-org-sync-org-test-build-plane-url ()
  "Build a valid Plane URL from components."
  (should (equal "https://app.plane.so/my-ws/projects/proj-123/work-items/42"
                 (plane-org-sync-org--build-plane-url
                  "https://app.plane.so" "my-ws" "proj-123" 42))))

(ert-deftest plane-org-sync-org-test-build-plane-url-strips-trailing-slash ()
  "Trailing slash on instance URL is stripped."
  (should (equal "https://plane.example.com/ws/projects/p1/work-items/7"
                 (plane-org-sync-org--build-plane-url
                  "https://plane.example.com/" "ws" "p1" 7))))

;;;; HTML-to-Org Converter

(ert-deftest plane-org-sync-org-test-html-to-org-nil ()
  "Nil input returns nil."
  (should-not (plane-org-sync-org--html-to-org nil)))

(ert-deftest plane-org-sync-org-test-html-to-org-empty ()
  "Empty string returns nil."
  (should-not (plane-org-sync-org--html-to-org "")))

(ert-deftest plane-org-sync-org-test-html-to-org-paragraphs ()
  "Paragraphs are converted to double newlines."
  (let ((result (plane-org-sync-org--html-to-org "<p>First</p><p>Second</p>")))
    (should (string-match-p "First" result))
    (should (string-match-p "Second" result))))

(ert-deftest plane-org-sync-org-test-html-to-org-bold ()
  "Bold tags are converted to Org bold markup."
  (should (string-match-p "\\*important\\*"
                          (plane-org-sync-org--html-to-org
                           "<strong>important</strong>"))))

(ert-deftest plane-org-sync-org-test-html-to-org-italic ()
  "Italic tags are converted to Org italic markup."
  (should (string-match-p "/emphasis/"
                          (plane-org-sync-org--html-to-org
                           "<em>emphasis</em>"))))

(ert-deftest plane-org-sync-org-test-html-to-org-code ()
  "Inline code tags are converted to Org verbatim markup."
  (should (string-match-p "~snippet~"
                          (plane-org-sync-org--html-to-org
                           "<code>snippet</code>"))))

(ert-deftest plane-org-sync-org-test-html-to-org-link ()
  "Anchor tags are converted to Org links."
  (should (string-match-p "\\[\\[https://example.com\\]\\[click here\\]\\]"
                          (plane-org-sync-org--html-to-org
                           "<a href=\"https://example.com\">click here</a>"))))

(ert-deftest plane-org-sync-org-test-html-to-org-image ()
  "Image tags are converted to Org links."
  (should (string-match-p "\\[\\[https://img.example.com/pic.png\\]\\]"
                          (plane-org-sync-org--html-to-org
                           "<img src=\"https://img.example.com/pic.png\" />"))))

(ert-deftest plane-org-sync-org-test-html-to-org-list ()
  "List items are converted to Org list items."
  (let ((result (plane-org-sync-org--html-to-org
                 "<ul><li>Alpha</li><li>Beta</li></ul>")))
    (should (string-match-p "^- Alpha" result))
    (should (string-match-p "^- Beta" result))))

(ert-deftest plane-org-sync-org-test-html-to-org-pre ()
  "Pre blocks are converted to SRC blocks."
  (let ((result (plane-org-sync-org--html-to-org
                 "<pre><code>int x = 1;</code></pre>")))
    (should (string-match-p "#\\+BEGIN_SRC" result))
    (should (string-match-p "int x = 1;" result))
    (should (string-match-p "#\\+END_SRC" result))))

(ert-deftest plane-org-sync-org-test-html-to-org-blockquote ()
  "Blockquotes are converted to Org QUOTE blocks."
  (let ((result (plane-org-sync-org--html-to-org
                 "<blockquote>wisdom here</blockquote>")))
    (should (string-match-p "#\\+BEGIN_QUOTE" result))
    (should (string-match-p "wisdom here" result))
    (should (string-match-p "#\\+END_QUOTE" result))))

(ert-deftest plane-org-sync-org-test-html-to-org-heading ()
  "HTML headings are converted to bold text."
  (should (string-match-p "\\*Section Title\\*"
                          (plane-org-sync-org--html-to-org
                           "<h2>Section Title</h2>"))))

(ert-deftest plane-org-sync-org-test-html-to-org-entity-order ()
  "HTML entities are decoded AFTER tag stripping.
This ensures &lt; does not create false tags."
  (let ((result (plane-org-sync-org--html-to-org "<p>x &lt; y &amp; z</p>")))
    (should (string-match-p "x < y & z" result))))

(ert-deftest plane-org-sync-org-test-html-to-org-pre-entities ()
  "Entities inside pre blocks are decoded."
  (let ((result (plane-org-sync-org--html-to-org
                 "<pre><code>a &lt; b &amp;&amp; c</code></pre>")))
    (should (string-match-p "a < b && c" result))))

(ert-deftest plane-org-sync-org-test-html-to-org-strips-unknown-tags ()
  "Unknown tags are stripped but text content is preserved."
  (should (equal "hello world"
                 (plane-org-sync-org--html-to-org
                  "<div><span>hello</span> <span>world</span></div>"))))

;;;; Read Headings

(ert-deftest plane-org-sync-org-test-read-headings-empty-file ()
  "Reading headings from an empty buffer returns nil."
  (plane-org-sync-test-with-org-buffer ""
    (cl-letf (((symbol-function 'plane-org-sync-org--get-buffer)
               (lambda (_file) (current-buffer))))
      (let ((headings (plane-org-sync-org--read-headings "/tmp/fake.org")))
        (should (null headings))))))

(ert-deftest plane-org-sync-org-test-read-headings-single ()
  "A single heading with PLANE_ID is read."
  (plane-org-sync-test-with-org-buffer
      (concat "* TODO Test heading\n"
              ":PROPERTIES:\n"
              ":PLANE_ID: uuid-123\n"
              ":PLANE_UPDATED_AT: 2026-02-19T10:00:00Z\n"
              ":PLANE_STATE_ID: state-1\n"
              ":PLANE_STATE: In Progress\n"
              ":PLANE_PROJECT_ID: proj-1\n"
              ":END:\n")
    (cl-letf (((symbol-function 'plane-org-sync-org--get-buffer)
               (lambda (_file) (current-buffer))))
      (let ((headings (plane-org-sync-org--read-headings "/tmp/fake.org")))
        (unwind-protect
            (progn
              (should (= 1 (length headings)))
              (should (equal "uuid-123" (plist-get (car headings) :plane-id)))
              (should (equal "TODO" (plist-get (car headings) :todo-keyword)))
              (should (equal "In Progress" (plist-get (car headings) :plane-state)))
              (should (markerp (plist-get (car headings) :marker))))
          (mapc (lambda (h) (set-marker (plist-get h :marker) nil)) headings))))))

(ert-deftest plane-org-sync-org-test-read-headings-skips-no-plane-id ()
  "Headings without PLANE_ID are skipped."
  (plane-org-sync-test-with-org-buffer
      (concat "* TODO Regular heading\n"
              ":PROPERTIES:\n"
              ":CUSTOM_ID: foo\n"
              ":END:\n"
              "* TODO Synced heading\n"
              ":PROPERTIES:\n"
              ":PLANE_ID: uuid-456\n"
              ":PLANE_UPDATED_AT: 2026-01-01T00:00:00Z\n"
              ":END:\n")
    (cl-letf (((symbol-function 'plane-org-sync-org--get-buffer)
               (lambda (_file) (current-buffer))))
      (let ((headings (plane-org-sync-org--read-headings "/tmp/fake.org")))
        (unwind-protect
            (progn
              (should (= 1 (length headings)))
              (should (equal "uuid-456" (plist-get (car headings) :plane-id))))
          (mapc (lambda (h) (set-marker (plist-get h :marker) nil)) headings))))))

(ert-deftest plane-org-sync-org-test-read-headings-narrowed-buffer ()
  "Read headings works despite buffer narrowing (via `org-with-wide-buffer')."
  (plane-org-sync-test-with-org-buffer
      (concat "* TODO First\n"
              ":PROPERTIES:\n"
              ":PLANE_ID: uuid-a\n"
              ":END:\n"
              "* TODO Second\n"
              ":PROPERTIES:\n"
              ":PLANE_ID: uuid-b\n"
              ":END:\n")
    ;; Narrow to first heading only
    (narrow-to-region (point-min) (+ (point-min) 40))
    (cl-letf (((symbol-function 'plane-org-sync-org--get-buffer)
               (lambda (_file) (current-buffer))))
      (let ((headings (plane-org-sync-org--read-headings "/tmp/fake.org")))
        (unwind-protect
            ;; Should see both headings despite narrowing
            (should (= 2 (length headings)))
          (mapc (lambda (h) (set-marker (plist-get h :marker) nil)) headings))))))

;;;; Ensure TODO Line

(ert-deftest plane-org-sync-org-test-ensure-todo-line-creates ()
  "Creates a new managed #+TODO: line when none exists."
  (plane-org-sync-test-with-org-buffer ""
    (let ((plane-org-sync-group-keyword-mapping
           '((backlog . "BACKLOG")
             (unstarted . "TODO")
             (started . "STARTED")
             (completed . "DONE")
             (cancelled . "CANCELLED")))
          (plane-org-sync-state-mapping nil))
      (plane-org-sync-org--ensure-todo-line (current-buffer) nil)
      (goto-char (point-min))
      (should (looking-at "^#\\+TODO:"))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (should (string-match-p "BACKLOG" line))
        (should (string-match-p "TODO" line))
        (should (string-match-p "STARTED" line))
        (should (string-match-p "| DONE" line))
        (should (string-match-p "CANCELLED" line))
        (should (string-match-p "plane-org-sync-managed" line))))))

(ert-deftest plane-org-sync-org-test-ensure-todo-line-updates ()
  "Updates an existing managed #+TODO: line."
  (plane-org-sync-test-with-org-buffer
      "#+TODO: OLD_TODO | OLD_DONE  # plane-org-sync-managed\n* A heading\n"
    (let ((plane-org-sync-group-keyword-mapping
           '((backlog . "BACKLOG")
             (unstarted . "TODO")
             (started . "STARTED")
             (completed . "DONE")
             (cancelled . "CANCELLED")))
          (plane-org-sync-state-mapping nil))
      (plane-org-sync-org--ensure-todo-line (current-buffer) nil)
      (goto-char (point-min))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        ;; Old keywords should be gone
        (should-not (string-match-p "OLD_TODO" line))
        ;; New keywords should be present
        (should (string-match-p "BACKLOG" line))
        ;; Heading should still exist
        (should (string-match-p "A heading"
                                (buffer-substring-no-properties
                                 (point-min) (point-max))))))))

(ert-deftest plane-org-sync-org-test-ensure-todo-line-with-overrides ()
  "State mapping overrides add extra keywords."
  (plane-org-sync-test-with-org-buffer ""
    (let ((plane-org-sync-group-keyword-mapping
           '((backlog . "BACKLOG")
             (unstarted . "TODO")
             (started . "STARTED")
             (completed . "DONE")
             (cancelled . "CANCELLED")))
          (plane-org-sync-state-mapping
           '(("In Review" . "REVIEW")))
          (all-states (list (list :name "In Review" :group "started"))))
      (plane-org-sync-org--ensure-todo-line (current-buffer) all-states)
      (goto-char (point-min))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (should (string-match-p "REVIEW" line))))))

(ert-deftest plane-org-sync-org-test-ensure-todo-line-calls-regexps ()
  "After updating the line, org-set-regexps-and-options is called."
  (plane-org-sync-test-with-org-buffer ""
    (let ((plane-org-sync-group-keyword-mapping
           '((backlog . "BACKLOG")
             (unstarted . "TODO")
             (started . "STARTED")
             (completed . "DONE")
             (cancelled . "CANCELLED")))
          (plane-org-sync-state-mapping nil)
          (regexps-called nil))
      (cl-letf (((symbol-function 'org-set-regexps-and-options)
                 (lambda (&rest _) (setq regexps-called t))))
        (plane-org-sync-org--ensure-todo-line (current-buffer) nil)
        (should regexps-called)))))

;;;; Format Headline

(ert-deftest plane-org-sync-org-test-format-headline-full ()
  "Full headline with keyword, priority, and tags."
  (let* ((item (plane-org-sync-test--make-work-item
                :name "Fix the bug"
                :priority "high"
                :labels (vector (list :name "Backend")
                                (list :name "P1 Bug"))))
         (result (plane-org-sync-org--format-headline item "TODO")))
    (should (string-match-p "^\\* TODO \\[#A\\] Fix the bug" result))
    (should (string-match-p ":backend:p1_bug:$" result))))

(ert-deftest plane-org-sync-org-test-format-headline-no-priority ()
  "Headline without priority (none)."
  (let* ((item (plane-org-sync-test--make-work-item
                :name "A task"
                :priority "none"
                :labels []))
         (result (plane-org-sync-org--format-headline item "TODO")))
    (should (equal "* TODO A task" result))))

(ert-deftest plane-org-sync-org-test-format-headline-no-tags ()
  "Headline without tags."
  (let* ((item (plane-org-sync-test--make-work-item
                :name "Simple"
                :priority "medium"
                :labels []))
         (result (plane-org-sync-org--format-headline item "STARTED")))
    (should (equal "* STARTED [#B] Simple" result))))

;;;; Insert Heading

(ert-deftest plane-org-sync-org-test-insert-heading-full ()
  "Insert a full heading with all fields."
  (plane-org-sync-test-with-org-buffer ""
    (let ((plane-org-sync-instance-url "https://app.plane.so")
          (plane-org-sync-workspace "my-ws")
          (item (plane-org-sync-test--make-work-item
                 :name "Auth module"
                 :priority "high"
                 :start_date "2026-02-20"
                 :target_date "2026-02-28"
                 :description_html "<p>Build the auth module.</p>"
                 :labels (vector (list :name "Backend"))
                 :assignee_details (vector (list :display_name "Alice")))))
      (plane-org-sync-org--insert-heading (current-buffer) item "TODO")
      (goto-char (point-min))
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        ;; Headline
        (should (string-match-p "^\\* TODO \\[#A\\] Auth module.*:backend:" content))
        ;; Dates
        (should (string-match-p "SCHEDULED: <2026-02-20>" content))
        (should (string-match-p "DEADLINE: <2026-02-28>" content))
        ;; Properties
        (should (string-match-p ":PLANE_ID: test-uuid-1234" content))
        (should (string-match-p ":PLANE_URL:" content))
        (should (string-match-p ":PLANE_STATE: In Progress" content))
        (should (string-match-p ":PLANE_PROJECT: PROJ" content))
        (should (string-match-p ":PLANE_PRIORITY: high" content))
        (should (string-match-p ":PLANE_ASSIGNEES: Alice" content))
        (should (string-match-p ":CATEGORY: PROJ" content))
        ;; Link
        (should (string-match-p "\\[\\[.*\\]\\[PROJ-42\\]\\]" content))
        ;; Description sentinels and content
        (should (string-match-p "plane-org-sync-description-begin" content))
        (should (string-match-p "Build the auth module" content))
        (should (string-match-p "plane-org-sync-description-end" content))))))

(ert-deftest plane-org-sync-org-test-insert-heading-minimal ()
  "Insert a minimal heading with no dates, labels, or description."
  (plane-org-sync-test-with-org-buffer ""
    (let ((plane-org-sync-instance-url "https://app.plane.so")
          (plane-org-sync-workspace "ws")
          (item (plane-org-sync-test--make-work-item
                 :name "Bare item"
                 :priority "none")))
      (plane-org-sync-org--insert-heading (current-buffer) item "TODO")
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        ;; Headline (no priority cookie, no tags)
        (should (string-match-p "^\\* TODO Bare item$" content))
        ;; No SCHEDULED or DEADLINE
        (should-not (string-match-p "SCHEDULED:" content))
        (should-not (string-match-p "DEADLINE:" content))
        ;; Properties still present
        (should (string-match-p ":PLANE_ID:" content))))))

(ert-deftest plane-org-sync-org-test-insert-heading-nil-description-sentinels ()
  "Sentinels are emitted even when description is nil."
  (plane-org-sync-test-with-org-buffer ""
    (let ((plane-org-sync-instance-url "https://app.plane.so")
          (plane-org-sync-workspace "ws")
          (item (plane-org-sync-test--make-work-item
                 :description_html nil)))
      (plane-org-sync-org--insert-heading (current-buffer) item "TODO")
      (let ((content (buffer-substring-no-properties (point-min) (point-max))))
        (should (string-match-p "plane-org-sync-description-begin" content))
        (should (string-match-p "plane-org-sync-description-end" content))
        ;; Begin and end should be adjacent (only newline between)
        (should (string-match-p
                 "description-begin\n# plane-org-sync-description-end"
                 content))))))

;;;; Update Heading

(defun plane-org-sync-test--insert-and-get-marker (buffer item keyword)
  "Insert ITEM into BUFFER and return a marker at the heading.
Uses KEYWORD as the TODO keyword."
  (with-current-buffer buffer
    (plane-org-sync-org--insert-heading buffer item keyword)
    (goto-char (point-min))
    (org-back-to-heading t)
    (point-marker)))

(ert-deftest plane-org-sync-org-test-update-heading-title-change ()
  "Updating a heading changes the title."
  (plane-org-sync-test-with-org-buffer ""
    (let* ((plane-org-sync-instance-url "https://app.plane.so")
           (plane-org-sync-workspace "ws")
           (plane-org-sync--inhibit-push t)
           (item (plane-org-sync-test--make-work-item :name "Old title"))
           (marker (plane-org-sync-test--insert-and-get-marker
                    (current-buffer) item "TODO"))
           (updated-item (plane-org-sync-test--make-work-item :name "New title")))
      (unwind-protect
          (progn
            (plane-org-sync-org--update-heading
             (current-buffer) marker updated-item "TODO")
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "New title" content))
              (should-not (string-match-p "Old title" content))))
        (set-marker marker nil)))))

(ert-deftest plane-org-sync-org-test-update-heading-description-change ()
  "Updating a heading replaces the description between sentinels."
  (plane-org-sync-test-with-org-buffer ""
    (let* ((plane-org-sync-instance-url "https://app.plane.so")
           (plane-org-sync-workspace "ws")
           (plane-org-sync--inhibit-push t)
           (item (plane-org-sync-test--make-work-item
                  :description_html "<p>Old desc</p>"))
           (marker (plane-org-sync-test--insert-and-get-marker
                    (current-buffer) item "TODO"))
           (updated-item (plane-org-sync-test--make-work-item
                          :description_html "<p>New desc</p>")))
      (unwind-protect
          (progn
            (plane-org-sync-org--update-heading
             (current-buffer) marker updated-item "TODO")
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (should (string-match-p "New desc" content))
              (should-not (string-match-p "Old desc" content))
              ;; Sentinels still present
              (should (string-match-p "description-begin" content))
              (should (string-match-p "description-end" content))))
        (set-marker marker nil)))))

(ert-deftest plane-org-sync-org-test-update-heading-description-removal ()
  "Setting description to nil removes content but keeps sentinels."
  (plane-org-sync-test-with-org-buffer ""
    (let* ((plane-org-sync-instance-url "https://app.plane.so")
           (plane-org-sync-workspace "ws")
           (plane-org-sync--inhibit-push t)
           (item (plane-org-sync-test--make-work-item
                  :description_html "<p>To be removed</p>"))
           (marker (plane-org-sync-test--insert-and-get-marker
                    (current-buffer) item "TODO"))
           (updated-item (plane-org-sync-test--make-work-item
                          :description_html nil)))
      (unwind-protect
          (progn
            (plane-org-sync-org--update-heading
             (current-buffer) marker updated-item "TODO")
            (let ((content (buffer-substring-no-properties (point-min) (point-max))))
              (should-not (string-match-p "To be removed" content))
              ;; Both sentinels still present
              (should (string-match-p "description-begin" content))
              (should (string-match-p "description-end" content))
              ;; Sentinels should be adjacent
              (should (string-match-p
                       "description-begin\n# plane-org-sync-description-end"
                       content))))
        (set-marker marker nil)))))

(ert-deftest plane-org-sync-org-test-update-heading-preserves-sub-heading ()
  "Content after description-end sentinel is preserved."
  (plane-org-sync-test-with-org-buffer ""
    (let* ((plane-org-sync-instance-url "https://app.plane.so")
           (plane-org-sync-workspace "ws")
           (plane-org-sync--inhibit-push t)
           (item (plane-org-sync-test--make-work-item
                  :description_html "<p>Initial desc</p>")))
      ;; Insert the heading
      (plane-org-sync-org--insert-heading (current-buffer) item "TODO")
      ;; Manually append a sub-heading after description-end
      (goto-char (point-max))
      (insert "** My local notes\nThese should survive.\n")
      ;; Get marker
      (goto-char (point-min))
      (org-back-to-heading t)
      (let ((marker (point-marker))
            (updated-item (plane-org-sync-test--make-work-item
                           :description_html "<p>Updated desc</p>")))
        (unwind-protect
            (progn
              (plane-org-sync-org--update-heading
               (current-buffer) marker updated-item "TODO")
              (let ((content (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-match-p "Updated desc" content))
                (should (string-match-p "My local notes" content))
                (should (string-match-p "These should survive" content))))
          (set-marker marker nil))))))

(ert-deftest plane-org-sync-org-test-update-heading-preserves-content-after-sentinel ()
  "Non-heading content after description-end sentinel is preserved."
  (plane-org-sync-test-with-org-buffer ""
    (let* ((plane-org-sync-instance-url "https://app.plane.so")
           (plane-org-sync-workspace "ws")
           (plane-org-sync--inhibit-push t)
           (item (plane-org-sync-test--make-work-item
                  :description_html "<p>Desc</p>")))
      (plane-org-sync-org--insert-heading (current-buffer) item "TODO")
      ;; Add content after sentinel but before end of subtree
      (goto-char (point-max))
      (insert "My personal annotation\n")
      (goto-char (point-min))
      (org-back-to-heading t)
      (let ((marker (point-marker))
            (updated-item (plane-org-sync-test--make-work-item
                           :description_html "<p>New desc</p>")))
        (unwind-protect
            (progn
              (plane-org-sync-org--update-heading
               (current-buffer) marker updated-item "TODO")
              (let ((content (buffer-substring-no-properties (point-min) (point-max))))
                (should (string-match-p "New desc" content))
                (should (string-match-p "My personal annotation" content))))
          (set-marker marker nil))))))

;;;; Save Atomic

(ert-deftest plane-org-sync-org-test-save-atomic-success ()
  "Atomic save writes buffer to file."
  (let ((test-file (make-temp-file "plane-org-sync-test-")))
    (unwind-protect
        (with-temp-buffer
          (insert "* Test heading\n")
          (set-visited-file-name test-file t)
          (plane-org-sync-org--save-atomic (current-buffer) test-file)
          ;; File should have content
          (should (equal "* Test heading\n"
                         (with-temp-buffer
                           (insert-file-contents test-file)
                           (buffer-string))))
          ;; Buffer should not be modified
          (should-not (buffer-modified-p)))
      (when (file-exists-p test-file)
        (delete-file test-file)))))

(ert-deftest plane-org-sync-org-test-save-atomic-temp-cleanup-on-error ()
  "Temporary file is cleaned up when rename fails."
  (let ((test-file "/tmp/plane-org-sync-test-no-such-dir/file.org"))
    ;; Ensure the directory does not exist so rename will fail
    ;; We mock rename-file to simulate failure
    (with-temp-buffer
      (insert "content")
      (let ((write-called nil)
            (temp-deleted nil))
        (cl-letf (((symbol-function 'write-region)
                   (lambda (&rest _args) (setq write-called t)))
                  ((symbol-function 'rename-file)
                   (lambda (&rest _args) (error "Simulated rename failure")))
                  ((symbol-function 'file-exists-p)
                   (lambda (f) (if (string-suffix-p ".tmp~" f) t nil)))
                  ((symbol-function 'delete-file)
                   (lambda (_f) (setq temp-deleted t))))
          (should-error
           (plane-org-sync-org--save-atomic (current-buffer) test-file))
          (should write-called)
          (should temp-deleted))))))

(provide 'test-plane-org-sync-org)
;;; test-plane-org-sync-org.el ends here
