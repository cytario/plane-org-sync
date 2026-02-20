;;; plane-org-sync-org.el --- Org-mode interface for plane-org-sync  -*- lexical-binding: t; -*-

;; Copyright (C) 2026 Martin

;; Author: Martin
;; Maintainer: Martin
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Reads and writes Org headings in the sync file format.  Handles
;; HTML-to-Org description conversion, priority mapping, tag
;; sanitization, and atomic file saving.

;;; Code:

(require 'org)
(require 'plane-org-sync-config)

;;;; Description Sentinel Constants

(defconst plane-org-sync-org--description-begin
  "# plane-org-sync-description-begin"
  "Sentinel comment marking the start of the synced description region.")

(defconst plane-org-sync-org--description-end
  "# plane-org-sync-description-end"
  "Sentinel comment marking the end of the synced description region.")

;;;; Pure Mapping Functions

(defun plane-org-sync-org--priority-to-org (priority-string)
  "Convert Plane PRIORITY-STRING to an Org priority character or nil.
Mapping: urgent/high -> ?A, medium -> ?B, low -> ?C, none/nil -> nil."
  (pcase priority-string
    ("urgent" ?A)
    ("high"   ?A)
    ("medium" ?B)
    ("low"    ?C)
    ("none"   nil)
    (_        nil)))

(defun plane-org-sync-org--priority-from-org (char)
  "Convert Org priority CHAR to a Plane priority string.
Mapping: ?A -> \"high\", ?B -> \"medium\", ?C -> \"low\", nil -> \"none\"."
  (pcase char
    (?A  "high")
    (?B  "medium")
    (?C  "low")
    (_   "none")))

(defun plane-org-sync-org--sanitize-tag (label-name)
  "Sanitize LABEL-NAME for use as an Org tag.
Replace non-alphanumeric characters (except `_', `@', `#', `%')
with underscores, downcase, and strip leading/trailing underscores."
  (let ((tag (downcase (replace-regexp-in-string
                        "[^[:alnum:]_@#%]" "_" label-name))))
    (setq tag (replace-regexp-in-string "\\`_+" "" tag))
    (setq tag (replace-regexp-in-string "_+\\'" "" tag))
    tag))

(defun plane-org-sync-org--build-plane-url (instance workspace project-id sequence-id)
  "Build a Plane work item browser URL.
INSTANCE is the base URL, WORKSPACE the slug, PROJECT-ID the UUID,
and SEQUENCE-ID the integer work item number.
For Plane Cloud, INSTANCE may be the API URL (`api.plane.so');
this function rewrites it to the frontend URL (`app.plane.so')
so that browser links work correctly.
Returns a URL like \"https://app.plane.so/ws/projects/pid/work-items/42\"."
  (let ((browser-url (replace-regexp-in-string
                      "\\bapi\\.plane\\.so\\b" "app.plane.so"
                      (string-trim-right instance "/"))))
    (format "%s/%s/projects/%s/work-items/%s"
            browser-url
            workspace
            project-id
            sequence-id)))

;;;; HTML-to-Org Converter

(defun plane-org-sync-org--html-to-org (html-string)
  "Convert HTML-STRING to Org markup.
Returns nil for nil or empty input.  Pipeline order:
1. Extract and stash <pre> blocks with unique placeholders
2. Block elements (blockquote, lists, headings, paragraphs)
3. Inline elements (bold, italic, code, links, images)
4. Strip remaining HTML tags
5. Decode HTML entities (LAST to avoid creating false tags)
6. Re-insert <pre> blocks as #+BEGIN_SRC...#+END_SRC
7. Normalize whitespace"
  (when (and html-string (not (string-empty-p html-string)))
    (let ((s html-string)
          (pre-blocks nil)
          (counter 0))
      ;; 1. Extract <pre> blocks
      (while (string-match "<pre[^>]*>\\(\\(?:.\\|\n\\)*?\\)</pre>" s)
        (let ((placeholder (format "\n@@PRE-BLOCK-%d@@\n" counter))
              (content (match-string 1 s)))
          ;; Strip inner <code> wrapper if present
          (when (string-match "\\`<code[^>]*>\\(\\(?:.\\|\n\\)*\\)</code>\\'" content)
            (setq content (match-string 1 content)))
          (push (cons counter content) pre-blocks)
          (setq s (replace-match placeholder t t s))
          (setq counter (1+ counter))))

      ;; 2. Block elements
      ;; Blockquotes
      (setq s (replace-regexp-in-string
               "<blockquote[^>]*>\\(\\(?:.\\|\n\\)*?\\)</blockquote>"
               "\n#+BEGIN_QUOTE\n\\1\n#+END_QUOTE\n" s))
      ;; Unordered lists
      (setq s (replace-regexp-in-string "<ul[^>]*>" "\n" s))
      (setq s (replace-regexp-in-string "</ul>" "\n" s))
      ;; Ordered lists
      (setq s (replace-regexp-in-string "<ol[^>]*>" "\n" s))
      (setq s (replace-regexp-in-string "</ol>" "\n" s))
      ;; List items in ordered lists - use "1." placeholder
      ;; We handle ol/ul items uniformly as "- " since distinguishing
      ;; requires stateful parsing.  Use "- " for all list items.
      (setq s (replace-regexp-in-string
               "<li[^>]*>\\(\\(?:.\\|\n\\)*?\\)</li>"
               "\n- \\1" s))
      ;; Headings h1-h6 -> bold text on own line
      (setq s (replace-regexp-in-string
               "<h[1-6][^>]*>\\(\\(?:.\\|\n\\)*?\\)</h[1-6]>"
               "\n*\\1*\n" s))
      ;; Paragraphs
      (setq s (replace-regexp-in-string "<p[^>]*>" "\n\n" s))
      (setq s (replace-regexp-in-string "</p>" "\n\n" s))
      ;; Line breaks
      (setq s (replace-regexp-in-string "<br\\s-*/?>" "\n" s))

      ;; 3. Inline elements
      ;; Bold
      (setq s (replace-regexp-in-string
               "<\\(?:strong\\|b\\)\\(?:\\s-[^>]*\\)?>\\(\\(?:.\\|\n\\)*?\\)</\\(?:strong\\|b\\)>"
               "*\\1*" s))
      ;; Italic
      (setq s (replace-regexp-in-string
               "<\\(?:em\\|i\\)\\(?:\\s-[^>]*\\)?>\\(\\(?:.\\|\n\\)*?\\)</\\(?:em\\|i\\)>"
               "/\\1/" s))
      ;; Code (inline)
      (setq s (replace-regexp-in-string
               "<code[^>]*>\\(\\(?:.\\|\n\\)*?\\)</code>"
               "~\\1~" s))
      ;; Links
      (setq s (replace-regexp-in-string
               "<a\\s-[^>]*href=\"\\([^\"]*\\)\"[^>]*>\\(\\(?:.\\|\n\\)*?\\)</a>"
               "[[\\1][\\2]]" s))
      ;; Images
      (setq s (replace-regexp-in-string
               "<img\\s-[^>]*src=\"\\([^\"]*\\)\"[^>]*/?>\\(</img>\\)?"
               "[[\\1]]" s))

      ;; 4. Strip remaining HTML tags
      (setq s (replace-regexp-in-string "<[^>]+>" "" s))

      ;; 5. Decode HTML entities LAST
      (setq s (replace-regexp-in-string "&amp;" "&" s t t))
      (setq s (replace-regexp-in-string "&lt;" "<" s t t))
      (setq s (replace-regexp-in-string "&gt;" ">" s t t))
      (setq s (replace-regexp-in-string "&quot;" "\"" s t t))
      (setq s (replace-regexp-in-string "&#39;" "'" s t t))
      (setq s (replace-regexp-in-string "&nbsp;" " " s t t))

      ;; 6. Re-insert pre blocks as SRC blocks
      (dolist (pair (nreverse pre-blocks))
        (let* ((idx (car pair))
               (content (cdr pair))
               (placeholder (format "@@PRE-BLOCK-%d@@" idx)))
          ;; Decode entities inside pre blocks too
          (setq content (replace-regexp-in-string "&amp;" "&" content t t))
          (setq content (replace-regexp-in-string "&lt;" "<" content t t))
          (setq content (replace-regexp-in-string "&gt;" ">" content t t))
          (setq content (replace-regexp-in-string "&quot;" "\"" content t t))
          (setq content (replace-regexp-in-string "&#39;" "'" content t t))
          (setq content (replace-regexp-in-string "&nbsp;" " " content t t))
          ;; Strip remaining tags inside pre (e.g. spans for syntax highlighting)
          (setq content (replace-regexp-in-string "<[^>]+>" "" content))
          (setq s (replace-regexp-in-string
                   (regexp-quote placeholder)
                   (concat "#+BEGIN_SRC\n" content "\n#+END_SRC")
                   s t t))))

      ;; 7. Normalize whitespace
      (setq s (replace-regexp-in-string "\n\\{3,\\}" "\n\n" s))
      (setq s (string-trim s))
      (if (string-empty-p s) nil s))))

;;;; Buffer Access

(defun plane-org-sync-org--get-buffer (file)
  "Return buffer visiting FILE, opening it if necessary.
Creates the file if it does not exist."
  (unless (file-exists-p file)
    (make-empty-file file t))
  (or (find-buffer-visiting file)
      (find-file-noselect file)))

;;;; Org Reader

(defun plane-org-sync-org--read-headings (file)
  "Read synced heading records from FILE.
Returns a list of plists, one per heading with a PLANE_ID property.
Each plist has keys: :plane-id, :plane-updated-at, :plane-state-id,
:plane-state, :plane-project-id, :todo-keyword, :marker.
Callers must clean up markers in `unwind-protect'."
  (with-current-buffer (plane-org-sync-org--get-buffer file)
    (org-with-wide-buffer
     (delq nil
           (org-map-entries
            (lambda ()
              (when-let ((plane-id (org-entry-get nil "PLANE_ID")))
                (list :plane-id plane-id
                      :plane-updated-at (org-entry-get nil "PLANE_UPDATED_AT")
                      :plane-state-id (org-entry-get nil "PLANE_STATE_ID")
                      :plane-state (org-entry-get nil "PLANE_STATE")
                      :plane-project-id (org-entry-get nil "PLANE_PROJECT_ID")
                      :todo-keyword (org-get-todo-state)
                      :marker (point-marker))))
            "LEVEL=1")))))

;;;; TODO Keyword Line Management

(defun plane-org-sync-org--ensure-todo-line (buffer all-project-states)
  "Ensure BUFFER has a managed #+TODO: line for Plane states.
ALL-PROJECT-STATES is a list of state plists (each with :name, :group)
from all synced projects.  Generates the #+TODO: line from
`plane-org-sync-group-keyword-mapping' and `plane-org-sync-state-mapping'.
Creates or updates the managed line at the top of BUFFER.
Calls `org-set-regexps-and-options' after modification."
  (with-current-buffer buffer
    (let* ((active-groups '(backlog unstarted started))
           (done-groups '(completed cancelled))
           (active-keywords nil)
           (done-keywords nil))
      ;; Collect keywords from group mapping
      (dolist (entry plane-org-sync-group-keyword-mapping)
        (let ((group (car entry))
              (kw (cdr entry)))
          (if (memq group done-groups)
              (unless (member kw done-keywords)
                (push kw done-keywords))
            (when (memq group active-groups)
              (unless (member kw active-keywords)
                (push kw active-keywords))))))
      ;; Add override keywords from state-mapping
      (dolist (entry plane-org-sync-state-mapping)
        (let* ((state-name (car entry))
               (kw (cdr entry))
               ;; Find which group this state belongs to
               (state-plist (seq-find (lambda (s)
                                        (equal (plist-get s :name) state-name))
                                      all-project-states))
               (group (when state-plist
                        (intern (plist-get state-plist :group)))))
          (if (and group (memq group done-groups))
              (unless (member kw done-keywords)
                (push kw done-keywords))
            (unless (member kw active-keywords)
              (push kw active-keywords)))))
      ;; Build the #+TODO: line
      (let ((todo-line (concat "#+TODO: "
                               (mapconcat #'identity (nreverse active-keywords) " ")
                               " | "
                               (mapconcat #'identity (nreverse done-keywords) " ")
                               "  # plane-org-sync-managed")))
        (save-excursion
          (org-with-wide-buffer
           (goto-char (point-min))
           (if (re-search-forward "^#\\+TODO:.*# plane-org-sync-managed" nil t)
               ;; Update existing managed line
               (progn
                 (beginning-of-line)
                 (delete-region (line-beginning-position) (line-end-position))
                 (insert todo-line))
             ;; Insert new managed line at top
             (goto-char (point-min))
             (insert todo-line "\n"))))
        (org-set-regexps-and-options)))))

;;;; Heading Formatter

(defun plane-org-sync-org--format-headline (work-item keyword)
  "Format an Org headline string for WORK-ITEM with TODO KEYWORD.
WORK-ITEM is a plist with :name, :priority, :labels (vector of plists
with :name).  Returns a string like \"* KEYWORD [#P] Title  :tag1:tag2:\"."
  (let* ((name (plist-get work-item :name))
         (priority-char (plane-org-sync-org--priority-to-org
                         (plist-get work-item :priority)))
         (labels-raw (plist-get work-item :labels))
         (labels (when labels-raw
                   (if (vectorp labels-raw) (append labels-raw nil) labels-raw)))
         (tags (when (and labels (> (length labels) 0))
                 (mapconcat (lambda (l)
                              (plane-org-sync-org--sanitize-tag
                               (if (stringp l) l
                                 (or (plist-get l :name) ""))))
                            labels ":")))
         (parts (list "*")))
    (when keyword
      (push keyword parts))
    (when priority-char
      (push (format "[#%c]" priority-char) parts))
    (push name parts)
    (let ((headline (mapconcat #'identity (nreverse parts) " ")))
      (if (and tags (not (string-empty-p tags)))
          (concat headline "  :" tags ":")
        headline))))

;;;; Heading Inserter

(defun plane-org-sync-org--insert-heading (buffer work-item keyword)
  "Append a new Org heading for WORK-ITEM to BUFFER with TODO KEYWORD.
Inserts headline, SCHEDULED/DEADLINE, PROPERTIES drawer, Plane link,
and description sentinels.  Description sentinels are always emitted
even when description is nil."
  (with-current-buffer buffer
    (save-excursion
      (org-with-wide-buffer
       (goto-char (point-max))
       ;; Ensure we start on a fresh line
       (unless (bolp) (insert "\n"))
       ;; Headline
       (insert (plane-org-sync-org--format-headline work-item keyword) "\n")
       ;; SCHEDULED / DEADLINE
       (let ((start-date (plist-get work-item :start_date))
             (target-date (plist-get work-item :target_date)))
         (when (or start-date target-date)
           (when start-date
             (insert "SCHEDULED: <" start-date "> "))
           (when target-date
             (insert "DEADLINE: <" target-date ">"))
           (insert "\n")))
       ;; PROPERTIES drawer
       (let* ((plane-id (plist-get work-item :id))
              (project-id (plist-get work-item :project))
              (project-identifier (or (plist-get work-item :project_identifier) ""))
              (sequence-id (plist-get work-item :sequence_id))
              (url (plane-org-sync-org--build-plane-url
                    plane-org-sync-instance-url
                    plane-org-sync-workspace
                    project-id
                    sequence-id))
              (state-name (or (plist-get work-item :state_name) ""))
              (state-id (or (plist-get work-item :state_id) ""))
              (priority (or (plist-get work-item :priority) "none"))
              (assignees-raw (or (plist-get work-item :assignee_details)
                                 (plist-get work-item :assignees)))
              (assignees (when assignees-raw
                           (if (vectorp assignees-raw)
                               (append assignees-raw nil)
                             assignees-raw)))
              (assignee-str (if (and assignees (> (length assignees) 0))
                                (mapconcat
                                 (lambda (a)
                                   (if (stringp a) a
                                     (or (plist-get a :display_name)
                                         (plist-get a :first_name)
                                         "")))
                                 assignees ", ")
                              ""))
              (updated-at (or (plist-get work-item :updated_at) "")))
         (insert ":PROPERTIES:\n")
         (insert ":PLANE_ID: " plane-id "\n")
         (insert ":PLANE_URL: " url "\n")
         (insert ":PLANE_UPDATED_AT: " updated-at "\n")
         (insert ":PLANE_STATE: " state-name "\n")
         (insert ":PLANE_STATE_ID: " state-id "\n")
         (insert ":PLANE_PROJECT_ID: " project-id "\n")
         (insert ":PLANE_PROJECT: " project-identifier "\n")
         (insert ":PLANE_PRIORITY: " priority "\n")
         (insert ":PLANE_ASSIGNEES: " assignee-str "\n")
         (insert ":CATEGORY: " project-identifier "\n")
         (insert ":END:\n")
         ;; Link line
         (insert (format "[[%s][%s-%s]]\n" url project-identifier sequence-id))
         ;; Description sentinels (always emitted)
         (insert plane-org-sync-org--description-begin "\n")
         (let ((desc (plane-org-sync-org--html-to-org
                      (plist-get work-item :description_html))))
           (when desc
             (insert desc "\n")))
         (insert plane-org-sync-org--description-end "\n"))))))

;;;; Heading Updater

(defun plane-org-sync-org--update-heading (buffer marker work-item keyword)
  "Update an existing Org heading in BUFFER at MARKER for WORK-ITEM.
KEYWORD is the new TODO keyword.  Updates headline text, SCHEDULED/DEADLINE,
all PLANE_* properties, link line, and description region.
Sub-headings and content after the description-end sentinel are preserved."
  (with-current-buffer buffer
    (save-excursion
      (org-with-wide-buffer
       (goto-char marker)
       ;; a) Replace headline text (from bol to eol)
       (beginning-of-line)
       (let ((beg (point)))
         (end-of-line)
         (delete-region beg (point)))
       (insert (plane-org-sync-org--format-headline work-item keyword))

       ;; Move to after headline for remaining updates
       (forward-line 1)

       ;; b) SCHEDULED/DEADLINE via org-schedule/org-deadline
       ;; First go back to the heading
       (org-back-to-heading t)
       (let ((start-date (plist-get work-item :start_date))
             (target-date (plist-get work-item :target_date))
             (plane-org-sync--inhibit-push t))
         ;; Remove existing SCHEDULED/DEADLINE before re-adding
         (if start-date
             (org-schedule nil (concat "<" start-date ">"))
           (org-schedule '(4)))  ; prefix arg removes
         (if target-date
             (org-deadline nil (concat "<" target-date ">"))
           (org-deadline '(4))))  ; prefix arg removes

       ;; c) Update all PLANE_* properties
       (let* ((project-id (plist-get work-item :project))
              (project-identifier (or (plist-get work-item :project_identifier) ""))
              (sequence-id (plist-get work-item :sequence_id))
              (url (plane-org-sync-org--build-plane-url
                    plane-org-sync-instance-url
                    plane-org-sync-workspace
                    project-id
                    sequence-id))
              (state-name (or (plist-get work-item :state_name) ""))
              (state-id (or (plist-get work-item :state_id) ""))
              (priority (or (plist-get work-item :priority) "none"))
              (assignees-raw (or (plist-get work-item :assignee_details)
                                 (plist-get work-item :assignees)))
              (assignees (when assignees-raw
                           (if (vectorp assignees-raw)
                               (append assignees-raw nil)
                             assignees-raw)))
              (assignee-str (if (and assignees (> (length assignees) 0))
                                (mapconcat
                                 (lambda (a)
                                   (if (stringp a) a
                                     (or (plist-get a :display_name)
                                         (plist-get a :first_name)
                                         "")))
                                 assignees ", ")
                              ""))
              (updated-at (or (plist-get work-item :updated_at) "")))
         (org-entry-put nil "PLANE_ID" (plist-get work-item :id))
         (org-entry-put nil "PLANE_URL" url)
         (org-entry-put nil "PLANE_UPDATED_AT" updated-at)
         (org-entry-put nil "PLANE_STATE" state-name)
         (org-entry-put nil "PLANE_STATE_ID" state-id)
         (org-entry-put nil "PLANE_PROJECT_ID" project-id)
         (org-entry-put nil "PLANE_PROJECT" project-identifier)
         (org-entry-put nil "PLANE_PRIORITY" priority)
         (org-entry-put nil "PLANE_ASSIGNEES" assignee-str)

         ;; d) CATEGORY property
         (org-entry-put nil "CATEGORY" project-identifier)

         ;; Navigate to body (after properties drawer)
         (org-back-to-heading t)
         (let ((heading-end (save-excursion (org-end-of-subtree t t) (point))))
           ;; e) Find and replace link line
           (org-end-of-meta-data t)
           (let ((body-start (point)))
             ;; Look for existing link line
             (when (re-search-forward "^\\[\\[.*\\]\\[.*\\]\\]$" heading-end t)
               (beginning-of-line)
               (delete-region (point) (progn (forward-line 1) (point))))
             ;; Insert new link line at body start
             (goto-char body-start)
             (insert (format "[[%s][%s-%s]]\n" url project-identifier sequence-id))
             ;; Recalculate heading-end after link insertion
             (org-back-to-heading t)
             (let ((new-heading-end (save-excursion (org-end-of-subtree t t) (point))))
               ;; f) Replace description between sentinels
               (goto-char body-start)
               (let ((desc-begin-re (regexp-quote plane-org-sync-org--description-begin))
                     (desc-end-re (regexp-quote plane-org-sync-org--description-end))
                     (new-desc (plane-org-sync-org--html-to-org
                                (plist-get work-item :description_html))))
                 (if (re-search-forward (concat "^" desc-begin-re "$") new-heading-end t)
                     ;; Found begin sentinel - replace content up to end sentinel
                     (let ((begin-line-end (line-end-position)))
                       (if (re-search-forward (concat "^" desc-end-re "$") new-heading-end t)
                           (let ((end-line-begin (line-beginning-position)))
                             ;; Delete everything between sentinels
                             (delete-region (1+ begin-line-end) end-line-begin)
                             ;; Insert new description
                             (goto-char (1+ begin-line-end))
                             (when new-desc
                               (insert new-desc "\n")))
                         ;; Begin sentinel found but no end sentinel -- insert end
                         (goto-char (1+ begin-line-end))
                         (when new-desc
                           (insert new-desc "\n"))
                         (insert plane-org-sync-org--description-end "\n")))
                   ;; No sentinels found - insert them
                   ;; Position after link line
                   (goto-char body-start)
                   (forward-line 1)  ; skip the link line we just inserted
                   (insert plane-org-sync-org--description-begin "\n")
                   (when new-desc
                     (insert new-desc "\n"))
                   (insert plane-org-sync-org--description-end "\n")))))))))))

;;;; Atomic File Save

(defun plane-org-sync-org--save-atomic (buffer file)
  "Write BUFFER contents to FILE atomically.
Writes to a temporary file first, then renames to target.
Does not call `revert-buffer' or `set-visited-file-name'."
  (let ((temp-file (concat file ".tmp~")))
    (with-current-buffer buffer
      (condition-case err
          (progn
            (write-region (point-min) (point-max) temp-file nil 'quiet)
            (rename-file temp-file file t)
            (set-buffer-modified-p nil)
            (set-visited-file-modtime))
        (error
         (when (file-exists-p temp-file)
           (delete-file temp-file))
         (signal (car err) (cdr err)))))))

(provide 'plane-org-sync-org)
;;; plane-org-sync-org.el ends here
