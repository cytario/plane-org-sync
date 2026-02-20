# Emacs Lisp Engineering Review

**Reviewer**: Principal Emacs Package Maintainer
**Documents reviewed**: SDS v1.0 Draft, IMPLEMENTATION-PLAN v1.0 Draft, SRS v1.0 Draft
**Date**: 2026-02-19

---

## Verdict

The design is well-structured for a first package and the 5-file decomposition
is appropriate.  However, there are several concrete Elisp problems that will
cause bugs, test failures, or MELPA rejection if not addressed before
implementation.  This review is ordered by severity.

---

## 1. The `--chain` Async Utility is Under-specified and Misplaced

### Problem

`plane-org-sync--chain` is described in the SDS (Section 6.1) and listed in
the API file (Section 7), but the implementation plan puts it in
`plane-org-sync-api.el` with a `plane-org-sync--` prefix (no `-api` infix).
This creates a naming violation: a symbol defined in `plane-org-sync-api.el`
must use the `plane-org-sync-api--` prefix.  More importantly, the utility is
used by the commands layer (`plane-org-sync.el`), not only by the API layer.

The sketched signature `(plane-org-sync--chain &rest fns)` uses `&rest` for a
macro-like calling convention but the SDS shows it used as a regular function
call.  If it is a function receiving a `&rest` list of lambdas, the caller
must dynamically construct closures -- this works, but the error handling
story is absent.  What happens when step 2 of 5 errors?  Does the chain
abort?  Is there cleanup?  The plan says "all org buffer mutations happen in a
single synchronous block AFTER all async calls complete" -- but `--chain` as
described executes steps sequentially, not in parallel.  For multi-project
sync you need to fan out across projects, collect results, then converge.
`--chain` does not support fan-out/fan-in.

### Recommendation

1. Move `--chain` to `plane-org-sync.el` or a new `plane-org-sync-util.el`
   so the prefix matches the file.  Alternatively, define it in
   `plane-org-sync-api.el` with the `plane-org-sync-api--chain` name.
2. Specify the error propagation model explicitly.  At minimum: each step
   receives `(next &rest prev-results)` and can call `(funcall next 'error
   msg)` or signal an error, and the chain aborts with a logged message.
3. For multi-project fan-out, implement a separate
   `plane-org-sync--collect (fns callback)` that fires N async operations and
   calls `callback` with a list of results once all N complete (a simple
   countdown latch pattern).  This is ~15 lines of Elisp and avoids pulling in
   `promise.el` or `deferred.el`.
4. Do not use `promise.el` or `deferred.el`.  They are unnecessary for this
   use case and add external dependencies.  The combination of `--chain`
   (sequential) and `--collect` (parallel) covers every async pattern in this
   package.

---

## 2. State Push Hook Cannot Be Async

### Problem

The implementation plan (Section 8.7, Item 3) describes the push flow as
using `plane-org-sync-api-get-work-item` (async) for conflict check, then
`plane-org-sync-api-update-work-item` (async) for the PATCH.  But
`org-after-todo-state-change-hook` runs synchronously.  If you fire an async
request and return, the user continues editing while the request is in flight.
If the request fails or detects a conflict, you need to revert the TODO
keyword -- but the user may have moved point, narrowed the buffer, or even
killed it.

The plan acknowledges this in Section 8, Item 3 ("Should resolve during
implementation") and recommends synchronous HTTP for push.  But the API
module is designed entirely around async `url-retrieve`.  There is no
synchronous path.

### Recommendation

Add `plane-org-sync-api--request-sync` that wraps `url-retrieve-synchronously`.
Use it exclusively in the push hook.  The push involves exactly 2 HTTP calls
(GET for conflict check + PATCH for update), each typically <500ms.  A ~1s
block is acceptable for a single interactive state change -- the user just
pressed a key and expects feedback.

The synchronous variant also simplifies revert-on-failure enormously: the hook
body becomes a linear `condition-case` with no callback nesting.

```elisp
(defun plane-org-sync--on-todo-state-change ()
  "Push state change to Plane when a synced heading's TODO keyword changes."
  (when (and (not plane-org-sync--inhibit-push)
             (org-entry-get nil "PLANE_ID"))
    (let* ((plane-id (org-entry-get nil "PLANE_ID"))
           (project-id (org-entry-get nil "PLANE_PROJECT_ID"))
           (new-keyword org-state)      ; dynamic var from hook context
           (old-keyword org-last-state) ; dynamic var from hook context
           (state-uuid (plane-org-sync-engine--keyword-to-state
                        new-keyword project-id
                        (org-entry-get nil "PLANE_STATE_ID"))))
      (when state-uuid
        (condition-case err
            (let* ((remote (plane-org-sync-api--request-sync
                            "GET" (format ".../%s" plane-id)))
                   (remote-ts (plist-get remote :updated_at))
                   (local-ts (org-entry-get nil "PLANE_UPDATED_AT")))
              (when (and (plane-org-sync-engine--detect-conflict local-ts remote-ts)
                         (not (funcall plane-org-sync-conflict-resolver
                                       "Remote item modified since last sync. Push anyway? ")))
                (let ((plane-org-sync--inhibit-push t))
                  (org-todo old-keyword))
                (user-error "Push cancelled"))
              (let ((result (plane-org-sync-api--request-sync
                             "PATCH" (format ".../%s" plane-id)
                             `(:state ,state-uuid))))
                (org-entry-put nil "PLANE_UPDATED_AT" (plist-get result :updated_at))
                (org-entry-put nil "PLANE_STATE_ID" state-uuid)
                (message "Plane: state updated to %s" new-keyword)))
          (error
           (let ((plane-org-sync--inhibit-push t))
             (org-todo old-keyword))
           (message "Plane push failed: %s" (error-message-string err))))))))
```

This is dramatically simpler than the async version described in the plan.

---

## 3. `org-map-entries` Pitfalls in `--read-headings`

### Problem

The plan uses `org-map-entries` with `"LEVEL=1"` to extract heading records.
This is fine in principle but has several gotchas:

1. **Buffer narrowing**: `org-map-entries` respects buffer narrowing by
   default.  If the user has narrowed the sync file buffer (e.g., to a
   subtree), `--read-headings` will silently return a subset.  You must
   `widen` first.
2. **Match string quoting**: `"LEVEL=1"` is correct for the match parameter.
   However, if someone passes `nil` for the match, `org-map-entries` iterates
   ALL headings, which in a large file with sub-headings is wasteful.
3. **Point markers**: The plan correctly uses `(point-marker)` instead of
   `(point)`.  Good.  But markers pin a buffer in memory -- if you store
   markers in a data structure and the sync errors out before clearing them,
   those markers will never be garbage-collected until the buffer is killed.
   This is a minor memory leak for long-running Emacs sessions.
4. **`org-map-entries` return value**: The function returns a list of whatever
   the body form returns.  Make sure the body form returns a plist, not `nil`
   for non-synced headings (headings without `PLANE_ID`).  Returning `nil` for
   skipped entries means the result list will contain `nil` values that must be
   filtered out.

### Recommendation

```elisp
(defun plane-org-sync-org--read-headings (file)
  "Read synced heading records from FILE.
Returns a list of plists, one per heading with a PLANE_ID property."
  (with-current-buffer (plane-org-sync-org--get-buffer file)
    (org-with-wide-buffer
     (delq nil
           (org-map-entries
            (lambda ()
              (when-let ((plane-id (org-entry-get nil "PLANE_ID")))
                (list :plane-id plane-id
                      :plane-updated-at (org-entry-get nil "PLANE_UPDATED_AT")
                      :plane-state-id (org-entry-get nil "PLANE_STATE_ID")
                      :plane-project-id (org-entry-get nil "PLANE_PROJECT_ID")
                      :todo-keyword (org-get-todo-state)
                      :marker (point-marker))))
            "LEVEL=1")))))
```

Use `org-with-wide-buffer` (available since Emacs 26) to ensure narrowing is
temporarily removed.  Use `delq nil` to filter out non-synced headings.

For marker cleanup, wrap the sync operation in `unwind-protect` and call
`(mapc (lambda (h) (set-marker (plist-get h :marker) nil)) headings)` in the
cleanup form to release markers.

---

## 4. `--update-heading` Description Region Detection is the Hardest Problem

### Problem

The plan correctly identifies `--update-heading` as the hardest function and
allocates 40% of Org Interface testing to it.  However, the approach described
-- "find region between link line and first sub-heading or end of entry" -- is
under-specified.  Org has no formal concept of a "description paragraph".

Consider this real-world content:

```org
* TODO [#B] Implement auth   :backend:
SCHEDULED: <2026-02-20>
:PROPERTIES:
:PLANE_ID: abc-123
:END:
[[https://plane.so/...][PROJ-42]]

This is the description from Plane.
It has multiple lines.

- And a list item

** My local notes
More content here.
```

Where does the description end?  Before the empty line after "multiple lines"?
Before the `- And a list item`?  Before `** My local notes`?  The plan does
not define this boundary.

### Recommendation

Define a concrete sentinel-based protocol.  Two approaches, in order of
preference:

**Option A: Invisible sentinel comment** (recommended).  Bracket the
description with Org comments:

```org
[[https://plane.so/...][PROJ-42]]
# plane-org-sync-description-begin
Description text here.
# plane-org-sync-description-end

User's own content here (never touched).
```

On update, find the two comment lines and replace everything between them.  On
insert, always emit both comments even if description is nil (emit them with
nothing between).  This is unambiguous, survives arbitrary user edits outside
the sentinels, and the comments are invisible in most export backends.

**Option B: Replace everything between link and first sub-heading**.  Simpler
but fragile -- any user content added after the description but before a
sub-heading gets overwritten.  The plan seems to intend this, but it
contradicts the promise of preserving "content after the description
paragraph".

Go with Option A.  It adds two comment lines per heading but eliminates the
entire class of "where does the description end?" bugs.

---

## 5. `--save-atomic` Has a Race Condition With Visiting Buffers

### Problem

The SDS describes `--save-atomic` as: write to temp file, rename to target,
then `revert-buffer` or `set-visited-file-modtime`.  But the sync already
modifies the visiting buffer in memory (via `--insert-heading` and
`--update-heading`).  The buffer IS the canonical state.  Writing the buffer
to a temp file and renaming is correct for crash safety, but calling
`revert-buffer` afterward will re-read the file from disk, which is redundant
and triggers hooks (`after-revert-hook`, `find-file-hook`, etc.).  It also
discards any undo history from the sync modifications.

The `buffer-modified-p` check before writing is described as aborting "if
modified outside sync".  But the sync itself modifies the buffer, so
`buffer-modified-p` will always be `t` after `--insert-heading` / `--update-heading`.
The check needs to happen BEFORE the sync modifies the buffer, not at
save time.

### Recommendation

1. Check `buffer-modified-p` at the START of `plane-org-sync-pull`, before any
   buffer mutations.  If the buffer has unsaved changes, abort the entire sync
   with a user-facing error.  Or, if the buffer is visiting the file, call
   `save-buffer` first (with user confirmation).
2. For the atomic write, use this pattern instead of `revert-buffer`:

```elisp
(defun plane-org-sync-org--save-atomic (buffer file)
  "Write BUFFER contents to FILE atomically."
  (let ((temp-file (concat file ".tmp~")))
    (with-current-buffer buffer
      (condition-case err
          (progn
            (write-region (point-min) (point-max) temp-file nil 'quiet)
            (rename-file temp-file file t)
            (set-visited-file-name file t t)  ; update buffer's file association
            (set-buffer-modified-p nil)        ; mark as unmodified
            (set-visited-file-modtime))        ; sync modtime with disk
        (error
         (when (file-exists-p temp-file)
           (delete-file temp-file))
         (signal (car err) (cdr err)))))))
```

Do NOT call `revert-buffer`.  The buffer already has the correct content.  Just
update the bookkeeping.  Note: `set-visited-file-name` with the third argument
`t` means "keep the buffer name"; use this only if the file path might have
changed.  If the file path is always the same, just `set-buffer-modified-p`
and `set-visited-file-modtime` suffice.

---

## 6. Auth-source Integration is Incomplete

### Problem

The plan describes `plane-org-sync-config--get-api-key` as checking
`plane-org-sync-api-key` first, then falling back to `auth-source`.  But the
`defcustom` for `plane-org-sync-api-key` is typed as "string or nil".  If the
user sets it via auth-source, what do they set the defcustom to?  `nil`?  An
auth-source query spec?  This is not defined.

Additionally, the auth-source query needs to know what `host` to search for.
The plan says "host matching `plane-org-sync-instance-url`" -- but auth-source
host entries typically do not include the `https://` scheme prefix.  Searching
for `"https://app.plane.so"` will not match an auth-source entry with host
`"app.plane.so"`.

### Recommendation

Define the auth-source contract precisely:

```elisp
(defcustom plane-org-sync-api-key nil
  "Plane API key.
If nil, the key is retrieved from `auth-source' using the instance URL host.
If a non-empty string, used directly as the API key."
  :type '(choice (const :tag "Use auth-source" nil)
                 (string :tag "API key string"))
  :group 'plane-org-sync)

(defun plane-org-sync-config--get-api-key ()
  "Return the Plane API key.
Checks `plane-org-sync-api-key' first.  If nil, queries auth-source
for an entry matching the host portion of `plane-org-sync-instance-url'."
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
      (user-error "No API key configured.  Set `plane-org-sync-api-key' or add an auth-source entry for %s"
                  (url-host (url-generic-parse-url plane-org-sync-instance-url)))))
```

Key points: (a) strip the URL scheme before querying auth-source, (b) the
`:secret` value from auth-source can be a function (it usually is, for
deferred decryption) -- you must `funcall` it, (c) use `user-error` not
`error` for configuration problems so the debugger does not trigger.

---

## 7. `plane-org-sync-conflict-resolver` Should Be a `defcustom`, Not `defvar`

### Problem

The plan declares `plane-org-sync-conflict-resolver` as a `defvar` defaulting
to `#'y-or-n-p`.  But this is user-facing configuration (the SDS Section 6.4
explicitly mentions users might want to customize it).  A `defvar` does not
appear in the Customize interface and cannot be set via `custom-set-variables`.

Additionally, the variable name ends in `-resolver`, not `-function`.  Per
Elisp conventions, variables that hold a single function should end in
`-function`.

### Recommendation

```elisp
(defcustom plane-org-sync-conflict-function #'y-or-n-p
  "Function called to resolve sync conflicts.
Called with a prompt string.  Should return non-nil to proceed
with the push, nil to cancel.  The default uses `y-or-n-p'."
  :type 'function
  :group 'plane-org-sync
  :risky t)
```

Mark it `:risky t` because it holds a function -- this is a convention for
variables whose values are called.

---

## 8. Missing `(require 'org)` Dependencies

### Problem

`plane-org-sync-org.el` lists `(require 'org)` but several org functions used
throughout the package come from sub-features that are NOT guaranteed to be
loaded by `(require 'org)` alone:

- `org-entry-get`, `org-entry-put` -- these are in `org.el` proper, fine.
- `org-map-entries` -- also `org.el`, fine.
- `org-element-*` -- requires `org-element`, auto-loaded in practice but
  not guaranteed.
- `org-capture` functions -- require `org-capture`.
- `org-agenda-files` manipulation -- requires `org-agenda` or at least
  `org.el` loading the variable.

### Recommendation

In `plane-org-sync-org.el`:

```elisp
(require 'org)
(require 'org-element)  ; if using org-element-* functions
```

In `plane-org-sync.el` (for capture):

```elisp
(require 'org-capture)  ; only needed in Phase 5
```

Alternatively, if capture is truly optional, guard it:

```elisp
(defun plane-org-sync-capture-template (&optional keys)
  "Return a capture template entry for creating Plane work items."
  (require 'org-capture)
  ...)
```

This is fine because the function is only called when the user explicitly sets
up capture.

---

## 9. The HTML-to-Org Regex Approach Has Known Failure Modes

### Problem

The plan describes an ordered sequence of regex substitutions for HTML-to-Org
conversion.  This is pragmatic and I agree with the approach for v1.  However,
two specific issues:

1. **Entity decoding order**.  The plan says "decode HTML entities first" (step
   1), but this must happen LAST, not first.  If you decode `&lt;` to `<`
   before stripping tags, you create false tags.  Decode entities only after
   all tag processing is complete.

2. **Nested `<pre>` content**.  Inside `<pre>` blocks, HTML entities must be
   decoded but other transformations (bold, italic, links) must NOT be
   applied.  The regex approach must extract `<pre>` blocks first, process
   them separately, substitute placeholder tokens, run the rest of the
   pipeline, then re-insert the processed `<pre>` content.

### Recommendation

Reorder the pipeline:

1. Extract and stash `<pre>` blocks (replace with unique placeholders)
2. Block elements: `<blockquote>`, lists, headings, paragraphs
3. Inline elements: bold, italic, code, links, images
4. Strip remaining tags
5. Decode HTML entities (`&amp;` -> `&`, `&lt;` -> `<`, `&gt;` -> `>`,
   `&quot;` -> `"`, `&#39;` -> `'`)
6. Re-insert processed `<pre>` blocks (with entities decoded inside them)
7. Normalize whitespace

---

## 10. Package Header and MELPA Readiness Issues

### Problem

The plan shows only the main file `plane-org-sync.el` with a package header.
For MELPA, ALL `.el` files need proper headers, but only the main file needs
the full `Package-Requires` header.  Several specific issues:

1. **Missing `Maintainer` header**.  MELPA and `package-lint` require it.
2. **`Keywords` value**.  The plan uses `outlines, tools`.  These should be
   from the standard `finder-known-keywords` list.  `outlines` is not a
   standard keyword.  Use `convenience, comm` or `comm, outlines` (with
   `outlines` being acceptable as a secondary).
3. **Sub-file header format**.  Each sub-file needs:
   ```
   ;;; plane-org-sync-api.el --- Plane API client for plane-org-sync  -*- lexical-binding: t; -*-
   ;; Copyright (C) 2026  Martin
   ;; Author: Martin
   ;; SPDX-License-Identifier: GPL-3.0-or-later
   ```
   But NOT a `Package-Requires` header (only the main file has that).
4. **License**.  No license is mentioned anywhere in the plan or SDS.  MELPA
   requires a recognized free software license.  For MELPA (not GNU ELPA),
   GPL-3.0-or-later is standard.  Add an `SPDX-License-Identifier` header
   and a `COPYING` file.

### Recommendation

Add a checklist item to Phase 1 (M0 Skeleton): "All files have correct
headers including Maintainer, license identifier, and `;;; Commentary:`
section."  Run `package-lint` on every file during M0, not just M5.

---

## 11. `--ensure-todo-line` Mutates File-Level Content During Buffer Iteration

### Problem

The plan says `--ensure-todo-line` finds or creates the `#+TODO:` line as the
first line of the sync file, and this runs before heading modifications during
pull sync.  Inserting or modifying lines at the top of the buffer will
invalidate all markers stored by `--read-headings` (markers shift when text is
inserted before them).  Wait -- markers DO adjust when text is inserted before
them.  So this should be fine.

Actually, the real issue is subtler: if `--ensure-todo-line` modifies the
`#+TODO:` line, `org-mode` will need to re-scan the buffer to recognize the
new keyword set.  The function `org-ctrl-c-ctrl-c` on a `#+TODO` line triggers
this, but programmatically you need:

```elisp
(org-set-regexps-and-options)
```

Without this call, `org-todo` will not recognize newly added keywords, and
`org-get-todo-state` may return unexpected results.

### Recommendation

After modifying the `#+TODO:` line, call `(org-set-regexps-and-options)` or
equivalently `(org-refresh-properties "TODO" "TODO")`.  The simplest approach
is `(org-set-regexps-and-options)` which re-parses all in-buffer settings.
Test this explicitly: create a buffer, insert a `#+TODO:` line with a new
keyword, call `org-set-regexps-and-options`, then verify `org-todo-keywords-1`
contains the new keyword.

---

## 12. Minor Mode Should Be Buffer-Local, Not Global

### Problem

The plan describes `plane-org-sync-mode` as a global minor mode.  But the
hook `org-after-todo-state-change-hook` is a global hook.  Adding a function
to it globally means EVERY org buffer's TODO state changes get intercepted,
not just the sync file.  The hook function checks for `PLANE_ID`, so non-synced
headings are no-ops, but this is wasteful and potentially surprising if there
are bugs.

### Counter-argument

A global mode is actually correct here because:
- The user might refile a synced heading to another org file
- The auto-sync timer is global (not per-buffer)
- Adding to `org-agenda-files` is a global operation

### Recommendation

Keep it global but document clearly that the hook fires on ALL org TODO
changes, not just the sync file.  The `PLANE_ID` check at the top of the hook
function is the guard -- make sure it is the very first check and is cheap
(it is: `org-entry-get` on a non-existent property returns `nil` immediately).

Consider adding an additional guard: check if the current buffer's file is the
sync file or if the heading was refiled from it.  This is optional but
reduces false activations to zero:

```elisp
(when (and (not plane-org-sync--inhibit-push)
           (org-entry-get nil "PLANE_ID")
           (buffer-file-name)  ; not a non-file buffer
           ...)
```

---

## 13. Timer Management Details

### Problem

The plan recommends `run-with-idle-timer` with a `repeat` flag.  The SDS does
not specify.  The SRS says "repeating idle timer".

`run-with-idle-timer` with REPEAT set to a non-nil value means the timer
fires after N seconds of idle time, and then fires AGAIN after another N
seconds of idle time (without the user becoming active in between).  This
means if the user goes idle for 5 minutes with a 60-second interval, the
timer fires at 60s, 120s, 180s, 240s, 300s -- five syncs in a row.  Each
sync is async, so they will overlap and potentially corrupt state.

### Recommendation

Use a non-repeating idle timer that re-arms itself after the sync completes:

```elisp
(defvar plane-org-sync--timer nil
  "Idle timer for automatic sync.")

(defun plane-org-sync--schedule-auto-sync ()
  "Schedule the next auto sync after idle interval."
  (when (and plane-org-sync-auto-interval
             (not plane-org-sync--timer))
    (setq plane-org-sync--timer
          (run-with-idle-timer
           plane-org-sync-auto-interval nil
           (lambda ()
             (setq plane-org-sync--timer nil)
             (plane-org-sync-pull
              (lambda (_result)
                (plane-org-sync--schedule-auto-sync))))))))
```

This ensures exactly one sync runs at a time and the next sync is scheduled
only after the previous one completes.  The non-repeating timer avoids the
stacking problem.

Alternatively, use `run-with-timer` (wall-clock) with a guard variable
that prevents concurrent syncs.  But the idle-timer + self-re-arm pattern
is cleaner and only syncs when the user is not actively typing.

---

## 14. `json-parse-buffer` vs `json-read` (Emacs 29 Feature)

### Problem

The plan requires Emacs 29.1+ but does not mention `json-parse-buffer` or
`json-parse-string`, which are the C-level JSON parsers available since Emacs
27.  The SDS mentions `json.el` (the Elisp implementation) as a dependency.

### Recommendation

Use `json-parse-buffer` and `json-serialize` (the C-native functions) instead
of `json-read` and `json-encode`.  They are significantly faster for parsing
200+ work items and are available in all target Emacs versions (29.1+).

```elisp
;; Parsing:
(json-parse-buffer :object-type 'plist :null-object nil :false-object nil)

;; Serializing:
(json-serialize data :null-object nil :false-object :json-false)
```

The `:object-type 'plist` option returns plists directly, which matches the
data contract described in the plan (IP-1).  Using the C parser also means
you do not need `(require 'json)` -- the functions are built into the C core.

---

## 15. Testing Gotchas

### Problem

The mock macro `plane-org-sync-test-with-mock-api` uses `cl-letf` to override
`plane-org-sync-api--request`.  This is the correct pattern.  However:

1. **Async vs sync in tests**.  The mock calls the callback synchronously
   (immediately via `funcall`).  This means async chain logic is never
   actually tested as async -- everything runs on the call stack.  If there
   are bugs that only manifest when callbacks run in a future event loop
   iteration (e.g., buffer state changed between initiating the request and
   the callback firing), tests will miss them.

2. **`org-mode` initialization cost**.  Calling `(org-mode)` in
   `with-temp-buffer` is slow (~50ms per call on a fast machine) because it
   runs all mode hooks, font-lock setup, etc.  With 50+ tests this adds up.
   Consider using `delay-mode-hooks` to skip hooks in tests where you only
   need org parsing, not full mode initialization:

   ```elisp
   (with-temp-buffer
     (delay-mode-hooks (org-mode))
     ...)
   ```

3. **`org-element` cache**.  Since Emacs 29, `org-element` has an aggressive
   cache.  If you mutate buffer text without going through org's API, the
   cache becomes stale.  Use `org-element-cache-reset` after raw text
   insertion in tests, or disable the cache entirely in test setup:

   ```elisp
   (setq org-element-use-cache nil)
   ```

### Recommendation

Accept that the sync mock tests `--chain` synchronously.  This is a known
limitation of this test strategy and is acceptable.  Add a comment in the test
file acknowledging it.

Use `delay-mode-hooks` in test fixtures.  Disable `org-element-use-cache` in
test setup.

---

## 16. Buffer Management: `find-file-noselect` Leaves Buffers Open

### Problem

`plane-org-sync-org--get-buffer` is described as "returns existing visiting
buffer or opens file with `find-file-noselect`".  If the user has never opened
the sync file, `find-file-noselect` opens it and creates a buffer.  This
buffer will persist for the entire Emacs session even if the user never
interacts with it.  This is actually fine and expected for a sync file that
should be in `org-agenda-files`.

However, if `--read-headings` or `--get-buffer` is called with a file that
does not exist, `find-file-noselect` will create a buffer visiting a
non-existent file.  The plan says "creates [the file] if absent" -- how?
Does `find-file-noselect` on a non-existent path create the file?  No, it
creates a buffer but the file does not exist on disk until saved.

### Recommendation

In `--get-buffer`, if the file does not exist, create it first:

```elisp
(defun plane-org-sync-org--get-buffer (file)
  "Return buffer visiting FILE, opening it if necessary.
Creates the file if it does not exist."
  (unless (file-exists-p file)
    (make-empty-file file t))  ; create parent dirs if needed
  (or (find-buffer-visiting file)
      (find-file-noselect file)))
```

Use `find-buffer-visiting` (which resolves symlinks) instead of
`org-find-base-buffer-visiting` -- the latter is for indirect buffers, which
is not relevant here.

---

## 17. The 5-File Split is Correct

The decomposition into config, api, org, engine, commands is the right
granularity.  Some packages over-split (one file per function) or under-split
(everything in one file).  Five files with clear dependency direction is good
for a package of this complexity.

One concern: the engine file (`plane-org-sync-engine.el`) is very small --
just 5 functions, all pure.  It could arguably live in `plane-org-sync.el`.
However, keeping it separate makes testing easier (no need to load the API
client or org interface to test pure diff logic), so the split is justified.

---

## 18. Missing Emacs 29 Features Worth Leveraging

The plan requires Emacs 29.1+ but does not exploit several useful 29+ features:

1. **`sqlite` built-in** -- not relevant here, but worth noting.
2. **`use-package` built-in** -- document `use-package` configuration in README.
3. **`url-retrieve` improvements in Emacs 29** -- connection reuse is better.
   No code changes needed, but worth testing.
4. **`org-fold` (Emacs 29)** -- replaces text-property-based folding.  Should
   not affect this package unless you manipulate visibility, but test that
   `--update-heading` does not break fold state.

No action required -- just be aware of the environment.

---

## Summary of Required Changes Before Implementation

| # | Severity | Issue | Action |
|---|----------|-------|--------|
| 1 | HIGH | `--chain` misplaced and under-specified | Move or rename; add `--collect` for fan-out; specify error handling |
| 2 | HIGH | Push hook cannot be async | Add `plane-org-sync-api--request-sync`; use synchronous HTTP for push |
| 3 | HIGH | `--read-headings` missing `org-with-wide-buffer` | Wrap in `org-with-wide-buffer`; clean up markers in `unwind-protect` |
| 4 | HIGH | Description region boundary undefined | Use sentinel comments to bracket description |
| 5 | HIGH | `--save-atomic` calls `revert-buffer` unnecessarily | Use `set-buffer-modified-p` + `set-visited-file-modtime` instead |
| 6 | MEDIUM | Auth-source query uses full URL including scheme | Strip scheme; handle functional `:secret`; use `user-error` |
| 7 | MEDIUM | `conflict-resolver` is `defvar`, should be `defcustom` | Change to `defcustom`; rename to `-function`; add `:risky t` |
| 8 | MEDIUM | Missing `org-element` require | Add explicit requires for org sub-features |
| 9 | MEDIUM | HTML entity decoding order wrong | Decode entities LAST, not first; stash `<pre>` blocks |
| 10 | MEDIUM | Package headers incomplete for MELPA | Add Maintainer, license, fix Keywords |
| 11 | MEDIUM | `--ensure-todo-line` does not call `org-set-regexps-and-options` | Call it after modifying `#+TODO:` line |
| 12 | LOW | Global minor mode hooks all org buffers | Acceptable; document the `PLANE_ID` guard |
| 13 | LOW | Repeating idle timer causes overlapping syncs | Use non-repeating timer with self-re-arm |
| 14 | LOW | Uses `json.el` instead of C-native JSON parser | Use `json-parse-buffer` with `:object-type 'plist` |
| 15 | LOW | Test org-mode init is slow | Use `delay-mode-hooks`; disable element cache |
| 16 | LOW | `find-file-noselect` on non-existent file | Create file first; use `find-buffer-visiting` |

---

## Change History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | 2026-02-19 | Emacs Package Maintainer (Review Agent) | Initial review |
