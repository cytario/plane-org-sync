# Implementation Plan

**Product**: plane-org-sync (PS)
**Version**: 3.0 (Definitive)
**Date**: 2026-02-19

---

## 1. Implementation Phases

The package has five `.el` files with clear dependency relationships. The build
order follows the dependency graph bottom-up: foundation layers first, then
orchestration, then user-facing entry points.

**Phase 0 is a hard blocker**: API verification must complete before any coding
begins. This is the single largest risk item (see Section 3).

```
Phase 0: API Verification       (BEFORE coding starts -- Day 1)
Phase 1: Config + API Client    (independent, can be built in parallel)
Phase 2: Org Interface           (depends on Config for defcustom values)
Phase 3: Sync Engine             (depends on Config; pure functions, can overlap Phase 2)
Phase 4: Commands & Modes        (depends on all three above)
Phase 5: Integration & Polish    (capture template, setup wizard, timer)
```

### Dependency Graph

```
plane-org-sync.el (Phase 4)
  |-- plane-org-sync-engine.el (Phase 3)
  |     |-- plane-org-sync-api.el (Phase 1b)
  |     |-- plane-org-sync-org.el (Phase 2)
  |     +-- plane-org-sync-config.el (Phase 1a)
  |-- plane-org-sync-org.el (Phase 2)
  |     +-- plane-org-sync-config.el (Phase 1a)
  |-- plane-org-sync-api.el (Phase 1b)
  |     +-- plane-org-sync-config.el (Phase 1a)
  +-- plane-org-sync-config.el (Phase 1a)
```

### Phase 0: API Verification (BLOCKER)

**Goal**: Confirm the Plane API behaves as assumed before writing any code.
Results are recorded in `docs/API-VERIFICATION.md`.

**Duration estimate**: 1 day (Day 1)

**Critical path verifications (must complete before Phase 1b)**:

| ID | Verification | Blocking? |
|----|-------------|-----------|
| V1 | `/work-items/` endpoint exists and returns 200 | Yes |
| V2 | States endpoint returns `id`, `name`, `group` | Yes |
| V7 | `/users/me/` returns `id` usable as assignee filter | Yes |

**Design-informing verifications (should complete before Phase 1b)**:

| ID | Verification | Blocking? |
|----|-------------|-----------|
| V3 | `?expand=state,labels,assignees` on `/work-items/` | No (fallback exists) |
| V4 | Pagination field names (`next_cursor` vs `cursor`, response shape) | No (but affects `--paginate`) |
| V8 | Rate limit header casing and 429 response body | No (but affects `--handle-429`) |

**Can happen during Phase 2-3**:

| ID | Verification | Blocking? |
|----|-------------|-----------|
| V5 | PATCH for state change (required fields, response shape, 400 on workflow violation) | No |
| V6 | POST for creation (minimum body, response fields) | No |
| V9 | Draft/archived item visibility in list response | No |
| V10 | Labels endpoint returns `id`, `name`, `color` | No |

**Verification method**: `curl` or Emacs `url-retrieve-synchronously` against a
real Plane instance with test data. Results recorded in
`docs/API-VERIFICATION.md` with actual JSON responses (sensitive fields redacted).

**If `/work-items/` does not exist yet**: Use `/issues/` with an abstraction
layer that swaps the path segment. Track the migration timeline. Add a
`plane-org-sync-api-endpoint-prefix` defcustom defaulting to `"work-items"`
with `"issues"` as a documented fallback.

### Phase 1: Foundation (Config + API Client)

**Goal**: Establish configuration variables and a working HTTP client that can
authenticate with Plane and retrieve data.

**Duration estimate**: 3-4 days (Days 2-5)

#### Phase 1a: Config (`plane-org-sync-config.el`)

Build first because every other module reads defcustom values.

| Function / Variable | SDS Ref | Priority |
|---------------------|---------|----------|
| `plane-org-sync` customization group | SDS-PS-040001 | P0 |
| `plane-org-sync-instance-url` defcustom | SRS-PS-310102 | P0 |
| `plane-org-sync-api-key` defcustom | SRS-PS-310101 | P0 |
| `plane-org-sync-workspace` defcustom | SRS-PS-310103 | P0 |
| `plane-org-sync-projects` defcustom | SRS-PS-310104 | P0 |
| `plane-org-sync-file` defcustom | SRS-PS-310106 | P0 |
| `plane-org-sync-filter-assignee` defcustom | SRS-PS-310107 | P0 |
| `plane-org-sync-group-keyword-mapping` defcustom | SRS-PS-310105 | P0 |
| `plane-org-sync-state-mapping` defcustom | SRS-PS-310105 | P0 |
| `plane-org-sync-auto-interval` defcustom | SRS-PS-310202 | P1 |
| `plane-org-sync-conflict-function` defcustom | SDS 6.4 | P1 |
| `plane-org-sync--inhibit-push` variable | SDS-PS-050004 | P1 |
| HTTPS-only URL validation | SRS-PS-520002 | P0 |
| API key retrieval from auth-source | SRS-PS-520001 | P0 |

**HTTPS-only validation** [SRS-PS-520002]: The config module must validate that
`plane-org-sync-instance-url` uses the `https://` scheme. Validation fires in
`plane-org-sync-config--validate-instance-url`, called from `--get-api-key`
and `plane-org-sync-pull`. Non-HTTPS URLs produce a `user-error`. Uses
`url-generic-parse-url` to extract the scheme.

```elisp
(defun plane-org-sync-config--validate-instance-url ()
  "Signal an error if `plane-org-sync-instance-url' is not HTTPS."
  (let ((url (url-generic-parse-url plane-org-sync-instance-url)))
    (unless (string= (url-type url) "https")
      (user-error "plane-org-sync requires HTTPS.  Got: %s"
                  plane-org-sync-instance-url))))
```

**Auth-source integration** [SRS-PS-520001]: Strip URL scheme before querying
auth-source (auth-source entries use bare hostnames). Handle functional
`:secret` values (deferred decryption -- `auth-source-search` returns a
function for `:secret` when backed by GPG or Keychain). Use `user-error` for
configuration problems so the debugger does not trigger.

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
      (user-error "No API key configured.  Set `plane-org-sync-api-key' or add an auth-source entry for %s"
                  (url-host (url-generic-parse-url plane-org-sync-instance-url)))))
```

**Conflict resolver** [SDS 6.4]: Declared as a `defcustom` (not `defvar`),
named `-function` per Elisp conventions (variables holding a single function
end in `-function`), marked `:risky t` (convention for variables whose values
are called):

```elisp
(defcustom plane-org-sync-conflict-function #'y-or-n-p
  "Function called to resolve sync conflicts.
Called with a prompt string.  Should return non-nil to proceed
with the push, nil to cancel.  The default uses `y-or-n-p'."
  :type 'function
  :group 'plane-org-sync
  :risky t)
```

**State cache lifecycle**: `plane-org-sync--state-cache` is an alist of
`(project-id . states-list)`:
- **Populated**: At the start of `plane-org-sync-pull`, after fetching states
  for each project. Fetched synchronously before the async chain begins.
- **Read by**: `--keyword-to-state` (during push), `--collect-keywords`
  (during pull), `--state-to-keyword` (during pull).
- **Invalidated**: On `plane-org-sync-reset`, on project config change.
  Re-fetched fresh on each pull (states can change between syncs).
- **Fallback on push**: If cache is empty when the push hook fires (no pull has
  occurred yet in this session), fetch states synchronously via
  `plane-org-sync-api--request-sync` before resolving. This adds one HTTP call
  (~200ms) to the first push but avoids a confusing "run pull first" error.

**Deliverable**: A file that loads cleanly, declares all customization
variables with correct types and defaults, validates HTTPS via
`url-generic-parse-url`, and provides `plane-org-sync-config--get-api-key`
(resolves auth-source with scheme stripping and functional `:secret` handling).

#### Phase 1b: API Client (`plane-org-sync-api.el`)

Build in parallel with or immediately after Config. **Phase 0 (V1, V2, V7)
must be complete before this phase starts.**

| Function | SDS Ref | Priority |
|----------|---------|----------|
| `plane-org-sync-api--log` | SRS-PS-310502 | P0 |
| `plane-org-sync-api--request` (async) | SDS-PS-010101 | P0 |
| `plane-org-sync-api--request-sync` (synchronous) | (new) | P0 |
| `plane-org-sync-api--paginate` | SDS-PS-010103 | P0 |
| Rate limit handling (429 + backoff) | SDS-PS-010102 | P0 |
| Network error handling (`condition-case`) | SRS-PS-310502 | P0 |
| Malformed JSON handling (`condition-case`) | SRS-PS-310502 | P0 |
| `plane-org-sync-api-me` | SDS-PS-010205 | P0 |
| `plane-org-sync-api-list-work-items` | SDS-PS-010201 | P0 |
| `plane-org-sync-api-list-states` | SDS-PS-010204 | P0 |
| `plane-org-sync-api-list-labels` | (implied by SRS-PS-310301) | P1 |
| `plane-org-sync-api-list-projects` | (implied by SDS-PS-040002) | P1 |
| `plane-org-sync-api-get-work-item` | (implied by SDS-PS-020301) | P1 |
| `plane-org-sync-api-update-work-item` | SDS-PS-010202 | P1 |
| `plane-org-sync-api-create-work-item` | SDS-PS-010203 | P2 |
| Draft/archived item filtering | SRS-PS-310201 | P0 |

**`--request` (async HTTP wrapper)**: Wraps `url-retrieve`. The sentinel
callback body is wrapped in `condition-case`:
- `file-error` -- DNS failure, connection refused, TLS handshake failure
- `json-parse-error` -- malformed JSON (e.g., HTML error page from reverse proxy)
- `error` -- catch-all

On any error, invokes the callback with `(funcall callback nil error-message)`.
Never exposes raw Elisp error objects to the caller.

**`--request-sync` (synchronous HTTP wrapper)**: Used exclusively by the push
hook and for state cache fallback. Wraps `url-retrieve-synchronously` with a
5-second timeout (the `TIMEOUT` parameter, available since Emacs 26) and 1
retry max for 429 responses. On timeout or failure, signals an error (caught
by `condition-case` in the push hook). Total worst case blocking time:
5s request + 2s backoff + 5s retry = 12s.

```elisp
(defun plane-org-sync-api--request-sync (method path &optional body)
  "Synchronous HTTP request to Plane API.
Returns parsed JSON plist on success.  Signals error on failure.
Used for push operations where blocking ~1s is acceptable."
  (plane-org-sync-config--validate-instance-url)
  (let* ((url-request-method method)
         (url-request-extra-headers
          `(("X-API-Key" . ,(plane-org-sync-config--get-api-key))
            ("Content-Type" . "application/json")))
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
    (unwind-protect
        (with-current-buffer buffer
          (goto-char url-http-end-of-headers)
          (let ((status url-http-response-status))
            (cond
             ((<= 200 status 299)
              (condition-case nil
                  (json-parse-buffer :object-type 'plist
                                     :null-object nil
                                     :false-object nil)
                (json-parse-error
                 (error "Invalid JSON response from Plane API"))))
             ((= status 429)
              ;; Single retry after 2s backoff
              (kill-buffer buffer)
              (sleep-for 2)
              (let ((retry-buf (condition-case err
                                   (url-retrieve-synchronously full-url nil nil 5)
                                 (error (signal 'error
                                                (list (format "Retry failed: %s"
                                                              (error-message-string err))))))))
                (unwind-protect
                    (with-current-buffer retry-buf
                      (goto-char url-http-end-of-headers)
                      (if (<= 200 url-http-response-status 299)
                          (condition-case nil
                              (json-parse-buffer :object-type 'plist
                                                 :null-object nil
                                                 :false-object nil)
                            (json-parse-error
                             (error "Invalid JSON response from Plane API")))
                        (error "Plane API error: HTTP %d (after retry)"
                               url-http-response-status)))
                  (kill-buffer retry-buf))))
             (t (error "Plane API error: HTTP %d" status)))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))
```

**JSON parsing**: Use `json-parse-buffer` and `json-serialize` (C-native
functions available since Emacs 27) instead of `json-read` / `json-encode`.
This is faster (especially for 200+ work items) and does not require
`(require 'json)` -- the functions are built into the C core.

```elisp
;; Parsing:
(json-parse-buffer :object-type 'plist :null-object nil :false-object nil)

;; Serializing:
(json-serialize data :null-object nil :false-object :json-false)
```

**Draft/archived item filtering** [SRS-PS-310201]: After fetching work items,
remove items where `:is_draft` is `t` or `:archived_at` is non-nil. Use
`seq-remove` (from `seq.el`, built-in since Emacs 25) for clarity:

```elisp
(seq-remove (lambda (item)
              (or (eq t (plist-get item :is_draft))
                  (plist-get item :archived_at)))
            items)
```

With `json-parse-buffer` using `:false-object nil`, `:is_draft` will be `t`
for drafts and `nil` for non-drafts. This filtering happens in
`plane-org-sync-api-list-work-items` so that no caller ever sees draft or
archived items.

**Implementation order within the file**:
1. Log buffer creation and `plane-org-sync-api--log` helper
2. URL construction helpers (`--base-url`, `--workspace-url`, `--project-url`)
3. `plane-org-sync-api--request` (core async HTTP with `condition-case` for
   `file-error`, `json-parse-error`, and generic `error`)
4. `plane-org-sync-api--request-sync` (synchronous HTTP for push, 5s timeout,
   1 retry for 429)
5. `plane-org-sync-api--handle-429` (backoff for async path; 1 retry inline
   for sync path)
6. `plane-org-sync-api--paginate` (wraps `--request` for multi-page GET)
7. `plane-org-sync-api-me` (simplest endpoint, validates auth works)
8. `plane-org-sync-api-list-states` (needed for state mapping)
9. `plane-org-sync-api-list-work-items` (main data fetch, with draft/archived
   filter via `seq-remove`)
10. `plane-org-sync-api-get-work-item` (single item for conflict check)
11. `plane-org-sync-api-update-work-item` (PATCH for state push)
12. `plane-org-sync-api-list-projects` (for setup wizard)
13. `plane-org-sync-api-list-labels` (for tag mapping)
14. `plane-org-sync-api-create-work-item` (POST for capture)

**Deliverable**: A client that can authenticate, fetch the current user, list
work items with pagination and `?expand=`, push state changes synchronously,
and handle errors/rate-limits/network failures/malformed JSON. Tested against a
real Plane instance via manual REPL verification.

### Phase 2: Org Interface (`plane-org-sync-org.el`)

**Goal**: Read and write org headings in the sync file format specified by
SRS-PS-420101.

**Duration estimate**: 3-4 days (Days 5-8)

| Function | SDS Ref | Priority |
|----------|---------|----------|
| `plane-org-sync-org--html-to-org` | SDS-PS-030401 | P0 |
| `plane-org-sync-org--priority-to-org` | SDS-PS-030301 | P0 |
| `plane-org-sync-org--sanitize-tag` | (implied by SRS-PS-420101) | P0 |
| `plane-org-sync-org--read-headings` | SDS-PS-030101 | P0 |
| `plane-org-sync-org--insert-heading` | SDS-PS-030201 | P0 |
| `plane-org-sync-org--update-heading` | SDS-PS-030202 | P0 |
| `plane-org-sync-org--save-atomic` | SDS-PS-030203 | P0 |
| `plane-org-sync-org--ensure-todo-line` | SRS-PS-420105 | P0 |
| `plane-org-sync-org--build-plane-url` | (implied by SRS-PS-310401) | P0 |

**`--read-headings` pattern**: Wrap in `org-with-wide-buffer` to handle
narrowed buffers (if the user has narrowed the sync file, `org-map-entries`
would silently return a subset). Use `delq nil` to filter non-synced headings.
Callers must clean up markers in `unwind-protect`.

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
                      :plane-state (org-entry-get nil "PLANE_STATE")
                      :plane-project-id (org-entry-get nil "PLANE_PROJECT_ID")
                      :todo-keyword (org-get-todo-state)
                      :marker (point-marker))))
            "LEVEL=1")))))
```

Marker cleanup at the call site (in `plane-org-sync-pull`):

```elisp
(let ((headings (plane-org-sync-org--read-headings file)))
  (unwind-protect
      (progn ... use headings ...)
    (mapc (lambda (h) (set-marker (plist-get h :marker) nil)) headings)))
```

**Description region boundaries**: Use sentinel comments to unambiguously
bracket the synced description. This eliminates the "where does the description
end?" class of bugs entirely.

```org
[[https://plane.so/...][PROJ-42]]
# plane-org-sync-description-begin
Description text here.
# plane-org-sync-description-end

User's own content here (never touched by sync).
```

On update, find the two sentinel comment lines and replace everything between
them. On insert, always emit both sentinels even if description is nil (emit
them with nothing between). The sentinels are invisible in most export backends.

Constants for the sentinels:

```elisp
(defconst plane-org-sync-org--description-begin
  "# plane-org-sync-description-begin"
  "Sentinel comment marking the start of the synced description region.")

(defconst plane-org-sync-org--description-end
  "# plane-org-sync-description-end"
  "Sentinel comment marking the end of the synced description region.")
```

**`--save-atomic` pattern**: Do NOT call `revert-buffer`. The buffer already
has the correct content after `--insert-heading` / `--update-heading`. Calling
`revert-buffer` would re-read from disk (redundant), trigger hooks
(`after-revert-hook`, `find-file-hook`), and discard undo history. Just update
bookkeeping: `set-buffer-modified-p` to mark the buffer as unmodified, and
`set-visited-file-modtime` to sync the buffer's recorded modification time so
Emacs does not prompt "file changed on disk".

```elisp
(defun plane-org-sync-org--save-atomic (buffer file)
  "Write BUFFER contents to FILE atomically."
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
```

Note: `set-visited-file-name` is NOT needed here because the file path never
changes. Only `set-buffer-modified-p` and `set-visited-file-modtime` are
required.

Check `buffer-modified-p` at the START of `plane-org-sync-pull` (before any
buffer mutations), not at save time. If the buffer has unsaved user changes,
abort with a user-facing error: "Sync file has unsaved modifications. Save the
file first, then re-run sync."

**`--ensure-todo-line`**: After modifying the `#+TODO:` line, call
`(org-set-regexps-and-options)` to make org-mode recognize the new keyword
set. Without this call, `org-todo` will not recognize newly added keywords,
and `org-get-todo-state` may return unexpected results. This is a critical
correctness requirement.

```elisp
;; After inserting or updating the #+TODO: line:
(org-set-regexps-and-options)
```

Test explicitly: create a buffer, insert a `#+TODO:` line with a new keyword,
call `org-set-regexps-and-options`, then verify `org-todo-keywords-1` contains
the new keyword.

**`--get-buffer` pattern**: Handle non-existent files explicitly. Use
`find-buffer-visiting` (resolves symlinks) instead of
`org-find-base-buffer-visiting` (the latter is for indirect buffers, not
relevant here).

```elisp
(defun plane-org-sync-org--get-buffer (file)
  "Return buffer visiting FILE, opening it if necessary.
Creates the file if it does not exist."
  (unless (file-exists-p file)
    (make-empty-file file t))  ;; t = create parent dirs if needed
  (or (find-buffer-visiting file)
      (find-file-noselect file)))
```

**HTML-to-Org pipeline order** (corrected per Emacs maintainer review):
1. Extract and stash `<pre>` blocks (replace with unique placeholders)
2. Block elements: `<blockquote>` -> `#+BEGIN_QUOTE`, lists, headings -> bold,
   paragraphs
3. Inline elements: `<strong>/<b>` -> `*bold*`, `<em>/<i>` -> `/italic/`,
   `<code>` -> `~code~`, `<a>` -> `[[url][text]]`, `<img>` -> `[[src]]`
4. Strip remaining tags (preserve text content)
5. Decode HTML entities (`&amp;` -> `&`, `&lt;` -> `<`, `&gt;` -> `>`,
   `&quot;` -> `"`, `&#39;` -> `'`) -- LAST, not first
6. Re-insert processed `<pre>` blocks (with entities decoded inside them)
7. Normalize whitespace (collapse multiple blank lines to two)

Entity decoding must happen LAST because decoding `&lt;` to `<` before
stripping tags creates false tags. Inside `<pre>` blocks, entities are decoded
but other transformations (bold, italic, links) are NOT applied -- the stash-
and-restore pattern handles this automatically.

**Properties tracked per heading** (explicit list for implementors):

| Property | Source | Read by `--read-headings`? |
|----------|--------|---------------------------|
| `PLANE_ID` | `id` | Yes |
| `PLANE_URL` | Constructed from instance/workspace/project/sequence_id | No |
| `PLANE_UPDATED_AT` | `updated_at` | Yes |
| `PLANE_STATE` | State name (human-readable) | Yes |
| `PLANE_STATE_ID` | State UUID | Yes |
| `PLANE_PROJECT_ID` | Project UUID | Yes |
| `PLANE_PROJECT` | Project identifier (e.g., "PROJ") | No |
| `PLANE_PRIORITY` | `priority` string | No |
| `PLANE_ASSIGNEES` | Comma-separated display names | No |
| `CATEGORY` | Project identifier (e.g., "PROJ") | No |

**Implementation order within the file**:
1. Dependencies: `(require 'org)`, `(require 'plane-org-sync-config)`
2. Description sentinel constants
3. Pure functions: `--priority-to-org`, `--priority-from-org`, `--sanitize-tag`,
   `--build-plane-url`
4. `--html-to-org` (self-contained, heavily testable)
5. `--get-buffer` (with file creation via `make-empty-file`)
6. `--read-headings` (read-only, `org-with-wide-buffer`, marker cleanup
   contract documented)
7. `--ensure-todo-line` (manages `#+TODO:` header + calls
   `org-set-regexps-and-options`)
8. `--format-headline` (constructs headline string)
9. `--insert-heading` (creates new entries with sentinel comments, sets all
   properties including `PLANE_STATE`, `CATEGORY`)
10. `--update-heading` (modifies existing entries -- the hardest function.
    Sentinel-based description replacement. Binds
    `plane-org-sync--inhibit-push` to `t` around `org-todo` calls.)
11. `--save-atomic` (file safety wrapper, no `revert-buffer`)

**Deliverable**: Given a list of work item plists, can create a valid org file
from scratch, re-read it, and update headings without losing sub-headings or
local notes. Description regions are delimited by sentinel comments. All
testable with ERT using in-memory org buffers.

### Phase 3: Sync Engine (`plane-org-sync-engine.el`)

**Goal**: Given remote data and local data, compute what needs to change and
resolve states bidirectionally.

**Duration estimate**: 1-2 days (Days 7-8, overlapping with Phase 2)

Phase 3 has no I/O dependencies -- these are pure functions that depend only on
Config. Can be built in parallel with Phase 2 to de-risk integration points
IP-1 and IP-2 early.

| Function | SDS Ref | Priority |
|----------|---------|----------|
| `plane-org-sync-engine--state-to-keyword` | SDS-PS-020201 | P0 |
| `plane-org-sync-engine--keyword-to-state` | SDS-PS-020202 | P0 |
| `plane-org-sync-engine--collect-keywords` | (implied by SRS-PS-420105) | P0 |
| `plane-org-sync-engine--diff` | SDS-PS-020101 | P0 |
| `plane-org-sync-engine--detect-conflict` | SDS-PS-020301 | P1 |

**`--keyword-to-state` on-demand cache fetch**: If
`plane-org-sync--state-cache` is nil for the given project when this function
is called (e.g., first push before any pull), fetch states synchronously via
`plane-org-sync-api--request-sync` and populate the cache. This adds ~200ms
to the first push but avoids a "run pull first" error.

**Implementation order**:
1. `--state-to-keyword` (forward resolution: Plane state -> org keyword,
   with nil handling: nil state-name/state-group -> "TODO")
2. `--keyword-to-state` (reverse resolution: org keyword -> Plane state UUID,
   with on-demand state cache fetch)
3. `--collect-keywords` (for `#+TODO:` line generation)
4. `--diff` (compare remote items vs local headings)
5. `--detect-conflict` (timestamp comparison for push)

**Deliverable**: Pure functions that accept data structures and return diff
results. No I/O (except the on-demand state fetch fallback in
`--keyword-to-state`). Fully testable with ERT.

### Phase 4: Commands & Modes (`plane-org-sync.el`)

**Goal**: Wire everything together into user-facing interactive commands.

**Duration estimate**: 3-4 days (Days 9-12)

| Function | SDS Ref | Priority |
|----------|---------|----------|
| `plane-org-sync--chain` async utility | SDS 6.1 | P0 |
| `plane-org-sync--collect` fan-out/fan-in | (new) | P0 |
| `plane-org-sync-pull` command | SRS-PS-310201 | P0 |
| `plane-org-sync-mode` minor mode | SDS-PS-050001 | P0 |
| State change hook function | SDS-PS-050003 | P0 |
| Push suppression (`--inhibit-push`) | SDS-PS-050004 | P0 |
| `plane-org-sync-status` command | SRS-PS-310501 | P1 |
| `plane-org-sync-reset` command | SRS-PS-310205 | P1 |
| `plane-org-sync-browse` command | SRS-PS-310401 | P1 |
| Auto-sync timer management | SRS-PS-310202 | P1 |
| Agenda file management | SDS-PS-050001 | P1 |

**`--chain` utility** (defined in `plane-org-sync.el`, NOT in the API file):
Sequences async callbacks. The `plane-org-sync--` prefix matches the file.
Each step receives `(next &rest prev-results)` and can call
`(funcall next 'error msg)` to abort the chain. On error, logs the message
and does not execute subsequent steps.

**`--collect` utility** (new): Fan-out/fan-in combinator for multi-project
async. Fires N async operations and calls a single callback with a list of
results once all N complete. Uses a countdown latch pattern (~15 lines).
This is the async foundation for multi-project pull.

```elisp
(defun plane-org-sync--collect (fns callback)
  "Call each function in FNS with a per-slot callback.
Each fn receives one argument: a callback to call with its result.
When all fns have completed, CALLBACK is called with the list of results
in the same order as FNS.  If any fn signals an error, the corresponding
result slot is (:error . message)."
  (if (null fns)
      (funcall callback nil)
    (let* ((n (length fns))
           (results (make-vector n nil))
           (remaining n))
      (cl-loop for fn in fns
               for i from 0
               do (let ((idx i))  ;; capture loop variable for closure
                    (funcall fn
                      (lambda (result)
                        (aset results idx result)
                        (cl-decf remaining)
                        (when (zerop remaining)
                          (funcall callback (append results nil))))))))))
```

Note: The `(let ((idx i)) ...)` binding is essential for correct closure
capture in lexical binding. Without it, all closures would share the same
loop variable `i` (which, under `cl-loop`, is actually fine in Emacs Lisp with
lexical binding since `cl-loop` creates fresh bindings -- but the explicit
`let` makes the intent unambiguous and is defensive against future refactoring).

**State push hook** (synchronous HTTP): Uses `plane-org-sync-api--request-sync`
exclusively. The hook body is a linear `condition-case` with no callback
nesting. 5-second timeout, 1 retry max for 429.

```elisp
(defun plane-org-sync--on-todo-state-change ()
  "Push state change to Plane when a synced heading's TODO keyword changes."
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
            (let* ((path (format "/projects/%s/work-items/%s/"
                                 project-id plane-id))
                   (remote (plane-org-sync-api--request-sync "GET" path))
                   (remote-ts (plist-get remote :updated_at))
                   (local-ts (org-entry-get nil "PLANE_UPDATED_AT")))
              (when (and (plane-org-sync-engine--detect-conflict
                          local-ts remote-ts)
                         (not (funcall plane-org-sync-conflict-function
                                       "Remote item modified since last sync.  Push anyway? ")))
                (let ((plane-org-sync--inhibit-push t))
                  (org-todo old-keyword))
                (user-error "Push cancelled"))
              (let ((result (plane-org-sync-api--request-sync
                             "PATCH" path `(:state ,state-uuid))))
                (org-entry-put nil "PLANE_UPDATED_AT"
                               (plist-get result :updated_at))
                (org-entry-put nil "PLANE_STATE_ID" state-uuid)
                (message "Plane: state updated to %s" new-keyword)))
          (error
           (let ((plane-org-sync--inhibit-push t))
             (org-todo old-keyword))
           (message "Plane push failed: %s"
                    (error-message-string err))))))))
```

Key correctness points:
- `org-state` and `org-last-state` are dynamic variables available in the hook
  context (set by org-mode before calling the hook)
- On ANY error (timeout, network, 4xx, 5xx, JSON parse), the `condition-case`
  error branch reverts the keyword via `(org-todo old-keyword)` with
  `plane-org-sync--inhibit-push` bound to `t` to prevent recursive push
- Conflict detection prompts the user; if declined, reverts and signals
  `user-error`

**Timer management**: Use a non-repeating idle timer that re-arms itself after
the sync completes. A repeating idle timer fires every N seconds of continuous
idle time (60s, 120s, 180s, ...), causing overlapping syncs and corrupt state.
The non-repeating + self-re-arm pattern ensures exactly one sync runs at a time.

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

**Implementation order**:
1. Package header, `require` statements, `provide`
2. `plane-org-sync--chain` (async sequential utility)
3. `plane-org-sync--collect` (async fan-out/fan-in utility)
4. `plane-org-sync-pull` -- the core async pull flow
5. `plane-org-sync--on-todo-state-change` (synchronous push hook)
6. `plane-org-sync-mode` (registers hook, manages timer and agenda files)
7. `plane-org-sync-browse` (open PLANE_URL in browser)
8. `plane-org-sync-status`
9. `plane-org-sync-reset`

**Deliverable**: `M-x plane-org-sync-pull` fetches items from Plane and
populates the sync file. Changing a TODO keyword on a synced heading pushes the
state change back synchronously. Timer-based auto-sync runs without overlap.

### Phase 5: Integration & Polish

**Goal**: Capture template, setup wizard, documentation, CI, and distribution
readiness.

**Duration estimate**: 2-3 days (Days 13-15)

| Function | SDS Ref | Priority |
|----------|---------|----------|
| `plane-org-sync-capture-template` | SDS-PS-050002 | P2 |
| `plane-org-sync-setup` wizard | SDS-PS-040002 | P2 |
| README.md | URS-PS-30102 | P2 |
| CI workflow (GitHub Actions) | SRS-PS-530001 | P2 |
| MELPA recipe | SRS-PS-530001 | P2 |
| License file (COPYING, GPL-3.0-or-later) | MELPA requirement | P2 |

For capture, guard `org-capture` dependency:

```elisp
(defun plane-org-sync-capture-template (&optional keys)
  "Return a capture template entry for creating Plane work items."
  (require 'org-capture)
  ...)
```

This is fine because the function is only called when the user explicitly sets
up capture. `org-capture` is not loaded at package load time.

---

## 2. Milestone Definitions

### M0: Skeleton + API Verification (Days 1-2)

- **Phase 0 complete**: V1, V2, V7 verified. Results in `docs/API-VERIFICATION.md`.
- All 5 `.el` files created with correct headers:
  - `Author`, `Maintainer`, `SPDX-License-Identifier: GPL-3.0-or-later`
  - `;;; Commentary:` section in each file
  - `Package-Requires: ((emacs "29.1"))` in main file only
  - `Keywords: convenience, comm` (from `finder-known-keywords`)
- `require` chains and `provide` forms
- `plane-org-sync-config.el` with all defcustom variables declared
- Package loads without errors: `emacs -batch -l plane-org-sync.el` exits 0
- Byte-compiles cleanly with zero warnings
- `package-lint` clean on all files (run early, not just at M5)
- Test infrastructure: `test/` directory, ERT test runner, one trivial passing
  test per file
- COPYING file with GPL-3.0-or-later

### M1: API Client Works (Day 5)

- `plane-org-sync-api-me` returns the authenticated user's ID
- `plane-org-sync-api-list-states` returns states for a project
- `plane-org-sync-api-list-work-items` returns paginated work items with
  `?expand=state,labels,assignees` (or pre-fetch fallback)
- Draft and archived items filtered out (via `seq-remove`)
- `plane-org-sync-api--request-sync` works for single-item GET and PATCH
- Rate limit handling tested (simulate 429 response)
- Network error handling tested (simulate connection failure via
  `condition-case` with `file-error`)
- Malformed JSON handling tested (simulate HTML error page via
  `condition-case` with `json-parse-error`)
- Error logging to `*plane-org-sync-log*` works
- All API functions have ERT tests using mocked HTTP responses

**Acceptance criteria**: From an Emacs REPL with a valid API key, calling
`(plane-org-sync-api-me callback)` logs the user's display name.

### M2: Pull Sync Works (Day 8)

- `plane-org-sync-pull` fetches work items and creates/updates org headings
- State mapping (group-based + overrides) works correctly
- `#+TODO:` header line is auto-generated and maintained
  (with `org-set-regexps-and-options` called after modification)
- HTML descriptions convert to org markup (correct entity decoding order:
  entities LAST, `<pre>` stashed first)
- Description regions delimited by sentinel comments
- Orphaned headings are preserved
- Local sub-headings and notes survive re-sync
- Content after description-end sentinel preserved
- Atomic file writes prevent corruption (no `revert-buffer`)
- `buffer-modified-p` checked at pull start (before mutations)
- Marker cleanup in `unwind-protect`
- `--read-headings` wrapped in `org-with-wide-buffer`

**Acceptance criteria**: Running `M-x plane-org-sync-pull` twice produces the
same file content (idempotent). Adding a sub-heading under a synced item and
re-syncing preserves it. Adding content after the description-end sentinel
and re-syncing preserves it.

### M3: Bidirectional State Sync (Day 10) -- MVP

- Changing a TODO keyword on a synced heading pushes the state to Plane
  via synchronous HTTP (5s timeout, 1 retry max)
- Conflict detection prompts when remote is newer
- Invalid state transitions (HTTP 400) revert the local keyword
- Push is suppressed during pull (no feedback loop)
- `plane-org-sync-mode` registers the hook
- Push with empty state cache triggers on-demand state fetch
- Multi-project pull with partial failure reports per-project results
- Network error on push reverts keyword and displays message
- Timeout on push reverts keyword and displays message

**Acceptance criteria**: Mark a synced heading DONE in org, verify the Plane
web UI shows the work item as Done. Change state in Plane web UI, run pull,
verify the org heading updates.

**This is the MVP.** A user can pull items, see them in their agenda, and push
state changes back. Everything after this is quality-of-life.

### M4: Capture + Setup (Day 12)

- `plane-org-sync-capture-template` creates work items in Plane
- `plane-org-sync-setup` walks through configuration
- `plane-org-sync-status` reports sync state
- `plane-org-sync-reset` rebuilds from scratch (clears state cache)
- `plane-org-sync-browse` opens Plane URL

### M5: Release Ready (Day 15)

- README.md with installation, configuration, usage
  (include `use-package` configuration example)
- GitHub Actions CI: byte-compile, package-lint, checkdoc, ERT tests
- CI tests on **Emacs 29.1** (the floor) and **Emacs 30.1**, Ubuntu and macOS
- Clean `package-lint` and `checkdoc` output
- No Emacs 30-only APIs used (verified by CI on 29.1)
- MELPA recipe prepared
- COPYING file with GPL-3.0-or-later

---

## 3. Risk Areas

### High Risk

| Risk | Impact | Mitigation |
|------|--------|------------|
| **`/work-items/` endpoint behavior differs from `/issues/`** | Core sync broken | **Phase 0 verifies this before any code is written.** Document actual response shapes. Have a fallback to `/issues/` behind a config option if `/work-items/` is broken. |
| **`?expand=` not supported on `/work-items/`** | Must pre-fetch states/labels/assignees separately, burning rate limit | The SDS already accounts for this (Design Note in Section 3.1). Implement the pre-fetch fallback from day 1. Test which expand params work in Phase 0 (V3). |
| **`?assignees=` filter not supported on `/work-items/`** | Must fetch all items and filter client-side, slower for large projects | SRS-PS-310201 already requires client-side fallback. Implement it. |
| **`--update-heading` corrupts local content** | Data loss, trust destruction | Use sentinel comments for description boundaries. Invest 40% of Org Interface testing here. Test all transitions: description added, changed, removed, nil. |
| **Async callback ordering breaks sync** | Partial/corrupt sync results | Use `--chain` (sequential) and `--collect` (fan-out/fan-in) strictly. All org buffer mutations happen in a single synchronous block AFTER all async API calls complete. Never interleave async callbacks with buffer mutations. |
| **Push blocks Emacs too long** | User frustration, perceived hang | 5s timeout on synchronous push. 1 retry max for 429. Total worst case: 12s. If push times out, revert local keyword and report error. |

### Medium Risk

| Risk | Impact | Mitigation |
|------|--------|------------|
| **Rate limit exceeded in multi-project sync** | Partial sync, confusing state | Track `X-RateLimit-Remaining` header. Pause between projects if remaining < 10. Report partial results clearly per SRS-PS-310201. |
| **`updated_at` too sensitive for conflict detection** | False positive conflicts annoy users | Make the conflict prompt informative: show what changed, when, and by whom (if available via expand). Consider a `plane-org-sync-conflict-threshold` (seconds) to ignore near-simultaneous changes. |
| **HTML-to-Org regex breaks on unexpected HTML** | Garbled descriptions | Defensive approach: stash `<pre>` blocks first, decode entities last. Strip all unrecognized tags (preserve text content). Test against real Plane descriptions. The converter must never error -- worst case returns plain text. |
| **Plane workflow constraints reject state transitions** | User confusion when local change is reverted | Clear error message per SRS-PS-310203. Include old and new state names. Suggest checking Plane's workflow settings. |
| **Network unreachable during push** | User's local state diverges from Plane | Revert local keyword on any push failure (timeout, network error, HTTP error). Display clear error message. |
| **State cache empty on first push** | Push fails or produces wrong state | On-demand synchronous state fetch before resolving keyword (adds ~200ms to first push). |

### Low Risk

| Risk | Impact | Mitigation |
|------|--------|------------|
| **auth-source integration complexity** | Users can't authenticate | Strip URL scheme before query via `url-generic-parse-url`. Handle functional `:secret` via `funcall`. Use `user-error`. Provide both auth-source and plain string paths. Document auth-source setup with examples for `.authinfo.gpg` and macOS Keychain. |
| **org-capture `:before-finalize` timing** | Properties not set on captured heading | Using `url-retrieve-synchronously` as specified in SDS-PS-050002. Brief block is acceptable. |
| **Repeating idle timer causes overlapping syncs** | Corrupt state, duplicate requests | Use non-repeating idle timer with self-re-arm pattern. |

---

## 4. Testing Strategy

### Test Infrastructure

- **Framework**: ERT (built-in, no additional dependency)
- **Test files**: One test file per source file, in `test/` directory
- **Runner**: `emacs -batch -l ert -l test/test-*.el -f ert-run-tests-batch-and-exit`
- **CI**: GitHub Actions with **Emacs 29.1** (the floor, per SRS-PS-530002)
  and **30.1**, Ubuntu and macOS
- **Org element cache**: Disable in test setup with
  `(setq org-element-use-cache nil)` to prevent stale cache issues when
  mutating buffer text in tests

### Test File Map

| Source File | Test File | Mock Requirements |
|-------------|-----------|-------------------|
| `plane-org-sync-config.el` | `test/test-plane-org-sync-config.el` | None (pure config) |
| `plane-org-sync-api.el` | `test/test-plane-org-sync-api.el` | HTTP response mocks (see below) |
| `plane-org-sync-org.el` | `test/test-plane-org-sync-org.el` | Temp org buffers/files |
| `plane-org-sync-engine.el` | `test/test-plane-org-sync-engine.el` | None (pure functions) |
| `plane-org-sync.el` | `test/test-plane-org-sync.el` | API mocks + temp org files |

### Mock Strategy

#### API Client Mocks

Override `plane-org-sync-api--request` with a mock that returns predefined
JSON responses. The mock invokes callbacks **synchronously** (immediately via
`funcall`). This means async chain logic is tested as synchronous -- the tests
validate callback wiring, not actual async timing. This is a known and
acceptable limitation: we accept that bugs which only manifest when callbacks
run in a future event loop iteration will not be caught by ERT tests.

```elisp
(defmacro plane-org-sync-test-with-mock-api (responses &rest body)
  "Execute BODY with API calls returning RESPONSES.
RESPONSES is an alist of ((method . path) . (status-code . json-body)).
Callbacks are invoked synchronously."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'plane-org-sync-api--request)
              (lambda (method path _body callback)
                (let ((response (alist-get (cons method path) ,responses
                                           nil nil #'equal)))
                  (if response
                      (funcall callback (car response) (cdr response))
                    (error "Unmocked API call: %s %s" method path))))))
     ,@body))
```

For `--request-sync` mocks, override `plane-org-sync-api--request-sync`
similarly but return the parsed body directly (no callback).

#### Org Buffer Fixtures

Use `with-temp-buffer` + `delay-mode-hooks` + `org-mode` for isolated org
buffer tests. `delay-mode-hooks` skips mode hooks (font-lock setup, etc.),
which cuts ~50ms per test on a fast machine. With 50+ tests this adds up.

```elisp
(defmacro plane-org-sync-test-with-org-buffer (content &rest body)
  "Execute BODY in a temp org buffer initialized with CONTENT."
  (declare (indent 1))
  `(let ((org-element-use-cache nil))
     (with-temp-buffer
       (delay-mode-hooks (org-mode))
       (insert ,content)
       (goto-char (point-min))
       ,@body)))
```

For file-level tests (atomic save, sync file creation), use
`make-temp-file` with cleanup in `unwind-protect`.

### Test Categories

#### Unit Tests (per function)

| Function | Key Test Cases |
|----------|---------------|
| `--html-to-org` | nil input, empty string, `<p>` paragraphs, `<strong>`/`<em>` inline, `<pre>` code blocks (stashed/restored), `<a>` links, `<ul>`/`<ol>` lists, `<blockquote>`, nested elements, unknown tags stripped, `<table>` passthrough, entity decoding happens AFTER tag stripping |
| `--priority-to-org` | Each of: urgent, high, medium, low, none, nil |
| `--sanitize-tag` | Spaces to underscores, special chars removed, downcased, empty string |
| `--state-to-keyword` | Group-based default, explicit override, unknown state, nil state (defaults to "TODO"), nil group (defaults to "TODO") |
| `--keyword-to-state` | Override match, group-based match, already-in-target-group (returns nil), unknown keyword, **empty state cache (triggers on-demand fetch via mocked `--request-sync`)** |
| `--diff` | All create, all update, all unchanged, mixed, orphaned headings, empty remote list, empty local list |
| `--read-headings` | Empty file, single heading, multiple headings, heading without PLANE_ID (ignored), heading with all properties, **narrowed buffer (verify `org-with-wide-buffer` works)** |
| `--insert-heading` | Full work item, minimal work item (no dates, no labels, no description), null description_html, **sentinel comments always present even when description is nil** |
| `--update-heading` | Title change, state change, priority change, date added/removed, description added/removed/changed, **description null (removal: sentinels remain, content between them cleared, sub-headings preserved)**, tags changed, sub-heading preserved, local notes preserved, **content after description-end sentinel preserved** |
| `--save-atomic` | Successful write, write failure (cleanup temp file), **no revert-buffer called**, **`set-buffer-modified-p` and `set-visited-file-modtime` called** |
| `--ensure-todo-line` | New TODO line created, existing TODO line updated, **`org-set-regexps-and-options` called (verify `org-todo-keywords-1` contains new keyword)** |
| `--validate-instance-url` | HTTPS accepted, HTTP rejected, missing scheme rejected |
| `--get-api-key` | Direct string, auth-source lookup (with functional `:secret` via `funcall`), nil key + no auth-source (user-error) |
| `--request-sync` | Success, timeout, 429 (1 retry then success), 429 (1 retry then fail), network error (`file-error`), malformed JSON (`json-parse-error`) |

#### Integration Tests

| Scenario | What It Validates |
|----------|-------------------|
| **Full pull cycle** | API mock -> engine diff -> org write -> file save -> re-read matches expectations |
| **Idempotent pull** | Pull twice with same data produces identical file content |
| **Pull with local notes** | Sub-headings and post-description-end-sentinel content survive update |
| **State push cycle** | Change TODO -> hook fires -> conflict check (no conflict) -> sync PATCH -> metadata updated |
| **State push with conflict** | Change TODO -> conflict detected -> prompt (mocked to reject) -> local state reverted |
| **State push with workflow rejection** | PATCH returns 400 -> local state reverted -> error message |
| **Push suppression during pull** | Pull updates TODO keywords -> hook does NOT fire push |
| **Push with empty state cache** | No prior pull -> push fires -> on-demand state fetch (mocked `--request-sync`) -> push succeeds |
| **Orphaned heading preservation** | Item removed from remote -> local heading unchanged |
| **TODO line management** | Pull generates correct `#+TODO:` header, re-pull updates it if states change, `org-set-regexps-and-options` called |
| **Multi-project partial failure** | Mock 3 projects, second returns HTTP 500. First and third synced, error reported for second. Verify `*plane-org-sync-log*` contains the error for project 2. |
| **State mapping change re-sync** | Pull with default mapping, change `group-keyword-mapping`, pull again. Headings update to new keywords. |
| **Description removal** | Heading with existing description, update with `description_html: nil`, description content between sentinels removed, sentinels remain, sub-headings preserved. |
| **Rate limit recovery mid-sync** | Mock API returns 429 after first project, succeeds on retry. Sync completes. |
| **Empty project list** | `plane-org-sync-projects` is nil -> `user-error` "No projects configured." |
| **Network error on push** | Mock `--request-sync` to signal `file-error` -> keyword reverted, error message displayed. |
| **Push timeout** | Mock `--request-sync` to signal timeout error -> keyword reverted, error message displayed. |

### What NOT to Test in Automated Tests

- Actual Plane API calls (use manual verification during Phase 0 and development)
- Interactive prompts (mock `plane-org-sync-conflict-function`)
- org-capture template integration (too dependent on global state; test the
  `:before-finalize` function in isolation)
- Timer behavior (test the timer setup/teardown, not the timing)
- Actual async timing (mocks invoke callbacks synchronously; this is accepted)

---

## 5. Integration Points

These are the seams where subsystems connect. Each needs at least one
integration test.

### IP-1: API Client -> Sync Engine (data format contract)

The API client returns work item plists. The engine expects specific keys.
**The plist key naming convention must be agreed upon before implementation.**

Recommendation: Use `json-parse-buffer` with `:object-type 'plist`, which
produces keyword-symbol keys matching the JSON field names. Example:

```elisp
(:id "uuid" :name "Title" :state (:id "uuid" :name "In Progress" :group "started")
 :priority "high" :labels [(:id "uuid" :name "backend")] ...)
```

When `?expand=` works, state/labels/assignees are inline objects. When it
doesn't, the API client pre-fetches lookup tables and inlines them before
returning to the caller. The engine never sees raw UUIDs.

**Test**: Verify that `plane-org-sync-api-list-work-items` returns plists that
`plane-org-sync-engine--diff` accepts without error.

### IP-2: Sync Engine -> Org Interface (operation contract)

The engine produces diff results. The Org interface consumes them. The
operation format:

```elisp
;; Create operation: just the work item plist
;; Update operation: (heading-record . work-item-plist)
```

**Test**: Feed engine diff output directly to org insert/update functions.
Verify resulting buffer content.

### IP-3: Org Interface -> Sync Engine (heading record format)

`plane-org-sync-org--read-headings` returns heading records that the engine's
`--diff` function matches against remote items by `PLANE_ID`.

Heading record plist:

```elisp
(:plane-id "uuid" :plane-updated-at "2026-02-19T10:30:00Z"
 :plane-state-id "uuid" :plane-state "In Progress"
 :plane-project-id "uuid" :todo-keyword "STARTED" :marker #<marker>)
```

**Test**: Create an org buffer, insert headings, read them back, feed to
engine diff.

### IP-4: Commands -> API Client (async callback wiring + fan-out)

`plane-org-sync-pull` uses `--chain` for sequential steps (get user ID, then
for-each project...) and `--collect` for fan-out across projects (fetch all
projects' items in parallel, converge results). The critical invariant: **all
buffer mutations happen in a single synchronous block after all API responses
are collected.**

**Test**: Mock API with multiple projects. Verify buffer is modified only
after all project callbacks complete. Verify partial failure is reported.

### IP-5: Hook -> API Client (synchronous push chain)

The `org-after-todo-state-change-hook` function uses `--request-sync` for a
linear flow:
1. Read heading properties (Org Interface)
2. Resolve keyword to state (Engine)
3. Fetch current work item (API Client, sync)
4. Check for conflicts (Engine)
5. Push state (API Client, sync)
6. Update heading metadata (Org Interface)

On any error, revert the keyword with `--inhibit-push` bound to `t`.

**Test**: Mock `--request-sync`, change a TODO keyword programmatically, verify
the full chain executes and metadata is updated.

---

## 6. API Verification Tasks

These must be performed against a real Plane instance **before coding begins**
(Phase 0). Each verification produces a documented result (response JSON
snippet) that informs implementation decisions.

### V1: /work-items/ endpoint availability (BLOCKER)

```
GET /api/v1/workspaces/{slug}/projects/{id}/work-items/
```

- Does it exist and return 200?
- Does the response format match `/issues/`?
- What fields are present? Is `sequence_id` there?
- Does `?expand=state,labels,assignees` work?
- Does `?assignees={user-id}` filter work?
- Does it support pagination (`next_cursor`, `next_page_results`)?

**If `/work-items/` does not exist yet**: Use `/issues/` with an abstraction
layer that swaps the path segment. Track the migration timeline.

### V2: State response structure

```
GET /api/v1/workspaces/{slug}/projects/{id}/states/
```

- Does it return all states for the project?
- Does each state have `id`, `name`, `group`, `sequence`?
- What are the exact `group` string values? (`backlog`, `unstarted`, `started`,
  `completed`, `cancelled` -- confirm casing)

### V3: Expand parameter behavior

```
GET .../work-items/?expand=state,labels,assignees
```

- When expanded, what is the shape of `state`? Object with `id`, `name`,
  `group`? Or just `name`?
- When expanded, what is the shape of each label? Object with `id`, `name`,
  `color`?
- When expanded, what is the shape of each assignee? Object with `id`,
  `display_name`, `email`?

### V4: Pagination mechanics

```
GET .../work-items/?cursor={opaque}
```

- Is the cursor field called `next_cursor` or `cursor`?
- Is there a `results` array wrapper, or are items at the top level?
- What is the response shape? `{ results: [...], next_cursor: "...",
  next_page_results: true, total_results: 42 }`?

### V5: Work item PATCH for state change

```
PATCH .../work-items/{id}/
Body: {"state": "new-state-uuid"}
```

- Does it accept just `{"state": "uuid"}` or require other fields?
- What does the response look like on success?
- What does a workflow constraint violation (400) look like?
- Is `updated_at` in the PATCH response?

### V6: Work item POST for creation

```
POST .../work-items/
Body: {"name": "...", "state": "uuid", "priority": "medium", ...}
```

- What is the minimum required body? (`name` only?)
- Does `sequence_id` come back in the response?
- Does it accept `labels` as an array of UUIDs?
- Does it accept `assignees` as an array of UUIDs?

### V7: Users/me endpoint

```
GET /api/v1/users/me/
```

- What fields are returned? `id`, `display_name`, `email`?
- Is the `id` field the same UUID used in `assignees` arrays?

### V8: Rate limit headers

- Are `X-RateLimit-Remaining` and `X-RateLimit-Reset` present on every
  response?
- What is the exact header casing?
- What does a 429 response body look like?

### V9: Draft and archived items

- Do draft items (`is_draft: true`) appear in the list response?
- Do archived items (`archived_at` not null) appear in the list response?
- Is there a query parameter to exclude them, or must we filter client-side?

### V10: Labels endpoint

```
GET /api/v1/workspaces/{slug}/projects/{id}/labels/
```

- Does it return `id`, `name`, `color` for each label?
- Is it affected by the `/work-items/` migration?

---

## 7. File-by-File Breakdown

### 7.1 `plane-org-sync-config.el`

**Purpose**: All defcustom declarations, auth-source integration, URL
validation, and the setup wizard.

```
;;; plane-org-sync-config.el --- Configuration for plane-org-sync -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Martin
;; Author: Martin
;; Maintainer: Martin
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Configuration variables and auth-source integration for plane-org-sync.

;;; Code:

;; Section 1: Dependencies
;;   (require 'auth-source)
;;   (require 'url-parse)

;; Section 2: Customization group
;;   - (defgroup plane-org-sync nil ...)

;; Section 3: Connection settings
;;   - plane-org-sync-instance-url (defcustom, string, default "https://app.plane.so")
;;   - plane-org-sync-api-key (defcustom, choice: nil or string)
;;   - plane-org-sync-workspace (defcustom, string or nil)

;; Section 4: Project settings
;;   - plane-org-sync-projects (defcustom, list of strings)
;;   - plane-org-sync-file (defcustom, file path)
;;   - plane-org-sync-filter-assignee (defcustom, boolean, default t)
;;   - plane-org-sync-auto-interval (defcustom, integer or nil)

;; Section 5: State mapping
;;   - plane-org-sync-group-keyword-mapping (defcustom, alist)
;;   - plane-org-sync-state-mapping (defcustom, alist)

;; Section 6: Conflict and behavior settings
;;   - plane-org-sync-conflict-function (defcustom, function, default #'y-or-n-p, :risky t)

;; Section 7: Internal variables
;;   - plane-org-sync--inhibit-push (defvar, nil)
;;   - plane-org-sync--last-sync (defvar, nil) -- timestamp of last sync
;;   - plane-org-sync--last-sync-result (defvar, nil) -- result string
;;   - plane-org-sync--user-id (defvar, nil) -- cached current user UUID
;;   - plane-org-sync--state-cache (defvar, nil) -- alist of (project-id . states-list)
;;     Lifecycle: populated on pull (fresh each time), read on push,
;;     invalidated on reset. If empty on push, fetched synchronously.

;; Section 8: URL validation
;;   - plane-org-sync-config--validate-instance-url () -> nil or user-error
;;     Uses url-generic-parse-url to extract scheme.

;; Section 9: Auth-source integration
;;   - plane-org-sync-config--get-api-key () -> string
;;     1. Validates HTTPS first.
;;     2. Checks plane-org-sync-api-key (if non-empty string, use directly).
;;     3. If nil, queries auth-source for host matching URL host (stripped scheme
;;        via url-generic-parse-url + url-host).
;;     4. Handles functional :secret (funcall it).
;;     5. Errors with user-error if no key found.

;; Section 10: Setup wizard (Phase 5)
;;   - plane-org-sync-setup () -- interactive command (autoloaded)

(provide 'plane-org-sync-config)
```

**Functions to implement (in order)**:
1. Dependencies (`auth-source`, `url-parse`)
2. Customization group and all defcustom variables
3. Internal variable declarations (including state cache lifecycle doc)
4. `plane-org-sync-config--validate-instance-url` (uses `url-generic-parse-url`)
5. `plane-org-sync-config--get-api-key` (strip scheme, funcall :secret,
   user-error)
6. `plane-org-sync-setup` (deferred to Phase 5)

### 7.2 `plane-org-sync-api.el`

**Purpose**: All HTTP communication with Plane. Both async (for pull) and
synchronous (for push) paths.

```
;;; plane-org-sync-api.el --- Plane API client for plane-org-sync -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Martin
;; Author: Martin
;; Maintainer: Martin
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; HTTP client for Plane.so REST API.  Provides async request/paginate for
;; pull operations and synchronous request for push operations.

;;; Code:

;; Section 1: Dependencies
;;   (require 'url)
;;   (require 'seq)
;;   (require 'plane-org-sync-config)
;;   ;; NOTE: Do NOT (require 'json) -- using C-native json-parse-buffer/json-serialize

;; Section 2: Logging
;;   - plane-org-sync-api--log (level format-string &rest args)

;; Section 3: URL construction
;;   - plane-org-sync-api--base-url () -> string
;;   - plane-org-sync-api--workspace-url () -> string
;;   - plane-org-sync-api--project-url (project-id) -> string

;; Section 4: Core HTTP (async)
;;   - plane-org-sync-api--request (method path &optional body callback)
;;     Wraps url-retrieve.
;;     condition-case in sentinel callback for:
;;       - file-error (DNS, connection refused, TLS)
;;       - json-parse-error (malformed JSON / HTML error page)
;;       - error (catch-all)
;;     Uses json-parse-buffer with :object-type 'plist.
;;
;;   - plane-org-sync-api--paginate (path &optional params callback)

;; Section 5: Core HTTP (synchronous)
;;   - plane-org-sync-api--request-sync (method path &optional body)
;;     Wraps url-retrieve-synchronously with 5s timeout.
;;     1 retry max for 429 (2s sleep between).
;;     condition-case for file-error, json-parse-error, error.
;;     Signals error on failure.
;;     Uses json-parse-buffer with :object-type 'plist.

;; Section 6: Rate limit handling
;;   - plane-org-sync-api--handle-429 (method path body callback retry-count)
;;     Async: exponential backoff (2s, 4s, 8s), max 3 retries.
;;     Sync: 1 retry max (handled inline in --request-sync).

;; Section 7: Resource functions
;;   - plane-org-sync-api-me (callback)
;;   - plane-org-sync-api-list-projects (callback)
;;   - plane-org-sync-api-list-states (project-id callback)
;;   - plane-org-sync-api-list-labels (project-id callback)
;;   - plane-org-sync-api-list-work-items (project-id &optional params callback)
;;     Uses ?expand=state,labels,assignees. Falls back to pre-fetch if expand
;;     fails. Handles ?assignees= filter with client-side fallback.
;;     Filters out is_draft and archived_at items via seq-remove.
;;   - plane-org-sync-api-get-work-item (project-id item-id callback)
;;   - plane-org-sync-api-update-work-item (project-id item-id data callback)
;;   - plane-org-sync-api-create-work-item (project-id data callback)

(provide 'plane-org-sync-api)
```

**Functions to implement (in order)**:
1. Log helper
2. URL construction helpers
3. `--request` (core async HTTP with `condition-case` for `file-error`,
   `json-parse-error`, `error`)
4. `--request-sync` (synchronous HTTP for push, 5s timeout, 1 retry for 429)
5. `--handle-429`
6. `--paginate`
7. `plane-org-sync-api-me`
8. `plane-org-sync-api-list-states`
9. `plane-org-sync-api-list-work-items` (with draft/archived filter via
   `seq-remove`)
10. `plane-org-sync-api-get-work-item`
11. `plane-org-sync-api-update-work-item`
12. `plane-org-sync-api-list-projects`
13. `plane-org-sync-api-list-labels`
14. `plane-org-sync-api-create-work-item`

### 7.3 `plane-org-sync-org.el`

**Purpose**: All org buffer read/write operations.

```
;;; plane-org-sync-org.el --- Org-mode interface for plane-org-sync -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Martin
;; Author: Martin
;; Maintainer: Martin
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Reading and writing Org headings for synced Plane work items.

;;; Code:

;; Section 1: Dependencies
;;   (require 'org)
;;   (require 'plane-org-sync-config)

;; Section 2: Constants
;;   - plane-org-sync-org--description-begin "# plane-org-sync-description-begin"
;;   - plane-org-sync-org--description-end "# plane-org-sync-description-end"

;; Section 3: Pure mapping functions
;;   - plane-org-sync-org--priority-to-org (priority) -> ?A | ?B | ?C | nil
;;   - plane-org-sync-org--priority-from-org (char) -> string
;;   - plane-org-sync-org--sanitize-tag (label-name) -> string
;;   - plane-org-sync-org--build-plane-url (instance workspace project-id sequence-id) -> string

;; Section 4: HTML-to-Org converter
;;   - plane-org-sync-org--html-to-org (html-string) -> string | nil
;;     Pipeline order:
;;     1. Extract <pre> blocks (replace with placeholders)
;;     2. Block elements: <blockquote>, lists, headings, paragraphs
;;     3. Inline: bold, italic, code, links, images
;;     4. Strip remaining tags
;;     5. Decode HTML entities (LAST, not first)
;;     6. Re-insert processed <pre> blocks (entities decoded inside)
;;     7. Normalize whitespace

;; Section 5: Buffer access
;;   - plane-org-sync-org--get-buffer (file) -> buffer
;;     Creates file if not exists (make-empty-file with t for parent dirs).
;;     Uses find-buffer-visiting (not org-find-base-buffer-visiting).

;; Section 6: Org reader
;;   - plane-org-sync-org--read-headings (file) -> list of heading-record plists
;;     Wrapped in org-with-wide-buffer.
;;     Uses delq nil to filter non-synced headings.
;;     Returns markers -- caller must clean up in unwind-protect.
;;     Reads: PLANE_ID, PLANE_UPDATED_AT, PLANE_STATE_ID, PLANE_STATE,
;;            PLANE_PROJECT_ID, todo-keyword, marker.

;; Section 7: TODO keyword line management
;;   - plane-org-sync-org--ensure-todo-line (buffer states-by-group)
;;     Calls (org-set-regexps-and-options) after modifying #+TODO: line.

;; Section 8: Org writer
;;   - plane-org-sync-org--format-headline (work-item keyword) -> string
;;   - plane-org-sync-org--insert-heading (buffer work-item keyword)
;;     Always emits both description sentinel comments.
;;     Sets all PLANE_* properties including PLANE_STATE, PLANE_PROJECT,
;;     PLANE_PROJECT_ID, CATEGORY.
;;   - plane-org-sync-org--update-heading (buffer marker work-item keyword)
;;     Finds sentinel comments, replaces description between them.
;;     Updates: headline, SCHEDULED/DEADLINE, all PLANE_* properties,
;;     CATEGORY, link line.
;;     CRITICAL: binds (let ((plane-org-sync--inhibit-push t)) ...) for org-todo.

;; Section 9: File I/O
;;   - plane-org-sync-org--save-atomic (buffer file)
;;     write-region to temp -> rename-file to target.
;;     set-buffer-modified-p nil + set-visited-file-modtime.
;;     NO revert-buffer. NO set-visited-file-name.

(provide 'plane-org-sync-org)
```

**Functions to implement (in order)**:
1. Dependencies: `(require 'org)`, `(require 'plane-org-sync-config)`
2. Description sentinel constants
3. `--priority-to-org`, `--priority-from-org`
4. `--sanitize-tag`
5. `--build-plane-url`
6. `--html-to-org` (corrected pipeline order: entities LAST, `<pre>` stashed)
7. `--get-buffer` (with file creation via `make-empty-file`,
   `find-buffer-visiting`)
8. `--read-headings` (with `org-with-wide-buffer`, `delq nil`, marker cleanup
   contract documented in docstring)
9. `--ensure-todo-line` (with `org-set-regexps-and-options` after modification)
10. `--format-headline`
11. `--insert-heading` (with sentinel comments, all properties including
    `PLANE_STATE`, `PLANE_PROJECT`, `PLANE_PROJECT_ID`, `CATEGORY`)
12. `--update-heading` (sentinel-based description replacement, content
    preservation, `--inhibit-push` binding)
13. `--save-atomic` (no `revert-buffer`, no `set-visited-file-name`)

### 7.4 `plane-org-sync-engine.el`

**Purpose**: Pure logic -- diff computation and state resolution.

```
;;; plane-org-sync-engine.el --- Sync engine for plane-org-sync -*- lexical-binding: t; -*-

;; Copyright (C) 2026  Martin
;; Author: Martin
;; Maintainer: Martin
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; Pure functions for diff computation and state resolution.  No I/O
;; (except on-demand state cache fetch in --keyword-to-state fallback).

;;; Code:

;; Section 1: Dependencies
;;   (require 'plane-org-sync-config)

;; Section 2: State resolution
;;   - plane-org-sync-engine--state-to-keyword (state-name state-group) -> string
;;     1. Check plane-org-sync-state-mapping for state-name
;;     2. Fall back to plane-org-sync-group-keyword-mapping for state-group
;;     3. Fall back to "TODO" if group unknown
;;     4. Handle nil state-name/state-group gracefully (return "TODO")
;;
;;   - plane-org-sync-engine--keyword-to-state (keyword project-id current-state-id) -> uuid | nil
;;     Reads plane-org-sync--state-cache for project-id.
;;     If cache is empty for this project, fetches states synchronously
;;     via plane-org-sync-api--request-sync and populates the cache.
;;     1. Check plane-org-sync-state-mapping for entry with value = keyword
;;        -> find state by name in project-states -> return UUID
;;     2. Inverse of group-keyword-mapping -> find group
;;        -> if current-state-id already in that group, return nil
;;        -> else return first state in group (by sequence order)
;;     3. Warn user, return nil
;;
;;   - plane-org-sync-engine--collect-keywords (project-states) -> (active-keywords . done-keywords)

;; Section 3: Diff computation
;;   - plane-org-sync-engine--diff (remote-items local-headings) -> diff-result
;;     Returns (:create items :update items :unchanged items :orphaned headings)

;; Section 4: Conflict detection
;;   - plane-org-sync-engine--detect-conflict (local-updated-at remote-updated-at) -> boolean

(provide 'plane-org-sync-engine)
```

**Functions to implement (in order)**:
1. `--state-to-keyword` (with nil handling for both arguments)
2. `--keyword-to-state` (with on-demand state cache fetch via
   `plane-org-sync-api--request-sync`)
3. `--collect-keywords`
4. `--diff`
5. `--detect-conflict`

### 7.5 `plane-org-sync.el`

**Purpose**: Entry point, minor mode, commands, async utilities, capture template.

```
;;; plane-org-sync.el --- Synchronize Plane.so work items with Org-mode -*- lexical-binding: t; -*-

;; Author: Martin
;; Maintainer: Martin
;; Version: 1.0.0
;; Package-Requires: ((emacs "29.1"))
;; Keywords: convenience, comm
;; URL: https://github.com/.../plane-org-sync
;; SPDX-License-Identifier: GPL-3.0-or-later

;;; Commentary:

;; plane-org-sync synchronizes Plane.so work items with Org-mode files.
;; Pull work items assigned to you, push TODO state changes back,
;; and manage your Plane work from within Emacs.

;;; Code:

;; Section 1: Dependencies
;;   (require 'plane-org-sync-config)
;;   (require 'plane-org-sync-api)
;;   (require 'plane-org-sync-engine)
;;   (require 'plane-org-sync-org)

;; Section 2: Async utilities
;;   - plane-org-sync--chain (&rest fns)
;;     Sequential async pipeline.  On error, logs and aborts remaining steps.
;;     Each step receives (next &rest prev-results).
;;
;;   - plane-org-sync--collect (fns callback)
;;     Fan-out/fan-in.  Fires N async ops, calls callback with list of
;;     results when all complete.  Error slots get (:error . message).
;;     Uses (let ((idx i)) ...) for correct closure capture.

;; Section 3: Pull sync command
;;   ;;;###autoload
;;   - plane-org-sync-pull (&optional callback) -- interactive
;;     1. Check buffer-modified-p BEFORE any mutations (abort if modified)
;;     2. Get user ID (plane-org-sync-api-me), cache it
;;     3. Fan-out across projects via --collect:
;;        a. Fetch states (plane-org-sync-api-list-states), cache them
;;        b. Fetch work items with assignee filter if configured
;;     4. Collect all results, then synchronously:
;;        a. Get org buffer
;;        b. Read existing headings (with org-with-wide-buffer)
;;        c. For each project's items, resolve states to keywords
;;        d. Compute diff
;;        e. Update TODO line (with org-set-regexps-and-options)
;;        f. Apply creates and updates
;;           (with plane-org-sync--inhibit-push bound to t)
;;        g. Save atomically (no revert-buffer)
;;        h. Clean up markers in unwind-protect
;;     5. Report results in minibuffer (including partial failures)
;;     6. Update --last-sync and --last-sync-result
;;     7. Call callback if provided (for timer re-arm)

;; Section 4: State push hook
;;   - plane-org-sync--on-todo-state-change ()
;;     Uses plane-org-sync-api--request-sync (synchronous HTTP).
;;     5s timeout, 1 retry max for 429.
;;     Linear condition-case, no callback nesting.
;;     Reverts keyword on any failure (timeout, network, 4xx, 5xx).
;;     Uses org-state and org-last-state dynamic variables.

;; Section 5: Minor mode
;;   ;;;###autoload
;;   - plane-org-sync-mode (&optional arg) -- global minor mode
;;     On enable:
;;       - Add plane-org-sync--on-todo-state-change to
;;         org-after-todo-state-change-hook
;;       - Start auto-sync timer (non-repeating, self-re-arming)
;;       - Add plane-org-sync-file to org-agenda-files
;;     On disable:
;;       - Remove hook
;;       - Cancel timer, set plane-org-sync--timer to nil
;;       - Remove from org-agenda-files
;;     NOTE: Global mode means hook fires on ALL org TODO changes.
;;     The PLANE_ID guard is the first check and is cheap (org-entry-get
;;     on a non-existent property returns nil immediately).

;; Section 6: Timer management
;;   - plane-org-sync--timer (defvar, nil)
;;   - plane-org-sync--schedule-auto-sync ()
;;     Non-repeating idle timer.  Re-arms after sync completes.
;;     Prevents overlapping syncs.

;; Section 7: Utility commands
;;   ;;;###autoload
;;   - plane-org-sync-status () -- interactive
;;   ;;;###autoload
;;   - plane-org-sync-reset () -- interactive
;;     Clears plane-org-sync--state-cache, --user-id, --last-sync.
;;   ;;;###autoload
;;   - plane-org-sync-browse () -- interactive

;; Section 8: Capture template (Phase 5)
;;   ;;;###autoload
;;   - plane-org-sync-capture-template (&optional keys) -> capture-template-entry
;;     (require 'org-capture) -- guarded, only loaded when called.
;;   - plane-org-sync--capture-before-finalize ()

(provide 'plane-org-sync)
```

**Functions to implement (in order)**:
1. Package header and requires
2. `plane-org-sync--chain` (sequential async utility with error propagation)
3. `plane-org-sync--collect` (fan-out/fan-in with correct closure capture)
4. `plane-org-sync-pull` (core command, uses `--chain` + `--collect`,
   `buffer-modified-p` check first, marker cleanup in `unwind-protect`)
5. `plane-org-sync--on-todo-state-change` (synchronous push hook with
   `condition-case`, keyword revert on any failure)
6. `plane-org-sync--schedule-auto-sync` (non-repeating timer with self-re-arm)
7. `plane-org-sync-mode` (global minor mode with documented PLANE_ID guard)
8. `plane-org-sync-browse`
9. `plane-org-sync-status`
10. `plane-org-sync-reset` (clears all caches)
11. `plane-org-sync-capture-template` and `--capture-before-finalize` (Phase 5,
    guarded `(require 'org-capture)`)

---

## 8. Scope Boundaries

The following are explicitly out of scope for v1.0, per SRS Section 1.5.
Implementors must not add these without requirements revision.

| Exclusion | Reference |
|-----------|-----------|
| Sub-work-item hierarchy (flat list only) | SRS 1.5 |
| Work item types (Bug, Task, Story, Epic) | SRS 1.5 |
| Cycles and Modules | SRS 1.5 |
| Title push (Org -> Plane) | SRS 1.5 |
| Description push (Org -> Plane) | SRS 1.5 |
| Comments sync (read or write) | SRS 1.5 |
| Attachment handling | SRS 1.5 |
| Multi-workspace support | SRS 1.5 |
| Webhook-based real-time sync | SRS 1.5 |
| Bulk operations | SRS 1.5 |
| Offline mode / queue | SRS 1.5 |

---

## 9. Requirements Likely to Need Revision

Based on analysis of the requirements against Plane API reality and org-mode
constraints, these areas will likely need clarification or revision during
implementation.

### Must Resolve Before Implementation

1. **API response format verification** (Section 6, V1-V9). The entire design
   assumes specific JSON response shapes. Phase 0 resolves this before coding.

2. **`?expand=` availability on `/work-items/`**. The SDS Design Note
   acknowledges this might not work. If it doesn't, we need the pre-fetch
   fallback from day 1, and the rate limit budget changes significantly.

### Should Resolve During Implementation

3. **CATEGORY property behavior**. The SDS says to set `:CATEGORY:` to the
   project identifier. `org-agenda` uses CATEGORY for display. Verify this is
   the desired behavior (applies to heading and all sub-headings).

4. **State push when keyword maps to multiple Plane states**. Example: both
   "In Progress" and "In Review" map to `STARTED` via the group default. When
   the user sets `STARTED`, which Plane state is chosen? The SRS says "first
   state by API sequence order" -- document this clearly in the README.

5. **`org-lint` reference in SDS 6.5**. Recommendation: remove it from the SDS.
   `org-lint` is slow, designed for interactive use, not a save-time check.
   The atomic write pattern provides adequate safety.

---

## 10. Implementation Checklist Summary

```
[ ] Phase 0: API Verification
    [ ] V1: /work-items/ endpoint exists and returns 200
    [ ] V2: States endpoint returns id, name, group
    [ ] V7: /users/me/ returns id usable as assignee filter
    [ ] V3: ?expand= behavior documented
    [ ] V4: Pagination field names documented
    [ ] V8: Rate limit headers documented
    [ ] Results recorded in docs/API-VERIFICATION.md

[ ] Phase 1a: plane-org-sync-config.el
    [ ] Customization group
    [ ] All defcustom variables with correct types
    [ ] plane-org-sync-conflict-function (defcustom, :risky t, #'y-or-n-p)
    [ ] Internal variables (state cache lifecycle documented)
    [ ] HTTPS-only URL validation via url-generic-parse-url (SRS-PS-520002)
    [ ] Auth-source key retrieval (strip scheme, funcall :secret, user-error)
    [ ] ERT tests for URL validation and key retrieval

[ ] Phase 1b: plane-org-sync-api.el
    [ ] Log helper
    [ ] URL construction
    [ ] Core --request (condition-case for file-error, json-parse-error, error)
    [ ] --request-sync (5s timeout, 1 retry max for 429)
    [ ] 429 rate limit handling (async: 3 retries; sync: 1 retry)
    [ ] Pagination
    [ ] api-me
    [ ] api-list-states
    [ ] api-list-work-items (expand + assignee filter + draft/archived filter
        via seq-remove)
    [ ] api-get-work-item
    [ ] api-update-work-item
    [ ] api-list-projects
    [ ] api-list-labels
    [ ] api-create-work-item
    [ ] ERT tests with mocked HTTP (including network error, malformed JSON)

[ ] Phase 2: plane-org-sync-org.el
    [ ] Description sentinel constants
    [ ] Priority mapping
    [ ] Tag sanitization
    [ ] URL builder
    [ ] HTML-to-Org converter (correct pipeline: entities LAST, pre stashed)
    [ ] Buffer access (file creation via make-empty-file, find-buffer-visiting)
    [ ] Org reader (org-with-wide-buffer, delq nil, marker cleanup contract)
    [ ] TODO line management (org-set-regexps-and-options after modification)
    [ ] Heading insert (sentinel comments, all properties incl. PLANE_STATE,
        PLANE_PROJECT, PLANE_PROJECT_ID, CATEGORY)
    [ ] Heading update (sentinel-based description replacement, --inhibit-push
        binding, content preservation)
    [ ] Atomic file save (no revert-buffer, no set-visited-file-name,
        set-buffer-modified-p + set-visited-file-modtime)
    [ ] ERT tests with temp buffers (delay-mode-hooks, org-element-use-cache nil)

[ ] Phase 3: plane-org-sync-engine.el
    [ ] State-to-keyword resolution (nil handling for both args)
    [ ] Keyword-to-state resolution (on-demand state cache fetch)
    [ ] Keyword collection for TODO line
    [ ] Diff computation
    [ ] Conflict detection
    [ ] ERT tests (pure functions, mock --request-sync for cache fetch)

[ ] Phase 4: plane-org-sync.el
    [ ] --chain (sequential async, error propagation)
    [ ] --collect (fan-out/fan-in, countdown latch, correct closure capture)
    [ ] Pull command (buffer-modified-p check, --collect for fan-out,
        marker cleanup in unwind-protect)
    [ ] State push hook (synchronous HTTP via --request-sync, 5s timeout,
        condition-case, revert on any failure, org-state/org-last-state)
    [ ] Push suppression (--inhibit-push binding)
    [ ] Non-repeating idle timer with self-re-arm
    [ ] Global minor mode (documented PLANE_ID guard on all org buffers)
    [ ] Browse command
    [ ] Status command
    [ ] Reset command (clears state cache, user-id, last-sync)
    [ ] Integration tests:
        [ ] Full pull cycle
        [ ] Idempotent pull
        [ ] Pull with local notes (sentinel boundary preservation)
        [ ] State push cycle
        [ ] State push with conflict
        [ ] State push with workflow rejection (400)
        [ ] Push suppression during pull
        [ ] Push with empty state cache (on-demand fetch)
        [ ] Orphaned heading preservation
        [ ] TODO line management (org-set-regexps-and-options)
        [ ] Multi-project partial failure (3 projects, 1 fails)
        [ ] State mapping change re-sync
        [ ] Description removal (null description_html, sentinels remain)
        [ ] Rate limit recovery mid-sync
        [ ] Empty project list error
        [ ] Network error on push (keyword reverted)
        [ ] Push timeout (keyword reverted)

[ ] Phase 5: Polish
    [ ] Capture template (guarded require org-capture)
    [ ] Setup wizard
    [ ] README.md (include use-package example)
    [ ] COPYING (GPL-3.0-or-later)
    [ ] GitHub Actions CI:
        [ ] Emacs 29.1 (floor) and 30.1
        [ ] Ubuntu and macOS
        [ ] byte-compile
        [ ] package-lint (run from M0, not just M5)
        [ ] checkdoc
        [ ] ERT tests
    [ ] Clean package-lint and checkdoc output
    [ ] No Emacs 30-only APIs (verified by CI on 29.1)
    [ ] MELPA recipe

[ ] M3 (MVP) verification:
    [ ] Pull items from real Plane instance
    [ ] Items appear in org-agenda
    [ ] Change TODO state -> pushes to Plane (synchronous, <5s typical)
    [ ] Change state in Plane -> pull reflects it
    [ ] Conflict detection works
    [ ] No feedback loop on pull
    [ ] Multi-project partial failure reported
    [ ] Push with empty state cache works
    [ ] Network error on push: keyword reverted, message displayed
```

---

## 11. Time Estimate

| Phase | Effort | Calendar (1 developer) |
|-------|--------|------------------------|
| Phase 0 (API Verification) | 1 day | Day 1 |
| Phase 1 (Config + API) | 3-4 days | Days 2-5 |
| Phase 2 (Org Interface) | 3-4 days | Days 5-8 |
| Phase 3 (Engine) | 1-2 days | Days 7-8 (overlaps Phase 2) |
| Phase 4 (Commands) | 3-4 days | Days 9-12 |
| Phase 5 (Polish) | 2-3 days | Days 13-15 |
| **Total** | **14-18 days** | **~3 weeks** |

The critical path runs through **Phase 0 (V1 verification)** -> **Phase 1b
(API client must work)** -> **Phase 2 (`--update-heading` is the hardest
function)** -> **Phase 4 (`plane-org-sync-pull` async wiring + `--collect`
fan-out)**.

Phase 3 is the smallest and most predictable. It can overlap with Phase 2
since it has no I/O dependencies (except the on-demand state cache fetch
fallback).

Phase 4 is estimated at 3-4 days (increased from 2-3 in v1.0) because
`plane-org-sync-pull` is the most complex function in the package: it chains
async API calls via `--chain` and `--collect`, aggregates results across
projects, handles partial failures, performs synchronous buffer mutations with
marker cleanup in `unwind-protect`, and saves atomically. The push hook is
simpler (synchronous, linear) but has subtle edge cases around conflict
detection and keyword reversion.

---

## 12. Summary of Changes

This section documents all changes made in response to the TPO review
(REVIEW-TPO.md), the Emacs maintainer review (REVIEW-EMACS.md), and the
Emacs maintainer's response to the TPO review (REVIEW-EMACS-RESPONSE.md).

### From TPO Review

| Action | Section Changed |
|--------|----------------|
| Move API verification (V1-V9) to Phase 0, before coding | Section 1 (Phase 0), Section 6, Section 11 |
| Add HTTPS-only URL validation | Phase 1a, Section 7.1 |
| Define state cache lifecycle | Phase 1a, Section 7.1, Section 7.4 |
| Set push timeout (5s) and retry limit (1) | Phase 1b, Phase 4, Section 3 |
| Add draft/archived filtering to Phase 1b | Phase 1b, Section 7.2 |
| Add network error and malformed JSON handling | Phase 1b, Section 3, Section 7.2 |
| Add multi-project partial failure integration test | Section 4 |
| Move `--chain` to `plane-org-sync.el` | Phase 4, Section 7.5 |
| Fix CI Emacs version to 29.1 floor | M0, M5, Section 4 |
| Update time estimate to 14-18 days | Section 11 |
| Add scope boundaries section | Section 8 |
| Add V10 (labels endpoint verification) | Section 6 |
| Add PLANE_STATE to heading record plist | Phase 2, Section 5 (IP-3), Section 7.3 |
| Add CATEGORY to insert/update heading | Phase 2, Section 7.3 |
| Add empty state cache push test | Section 4 |
| Add description removal test | Section 4 |
| Add state mapping change re-sync test | Section 4 |
| Document sync mock limitation for async tests | Section 4 |

### From Emacs Maintainer Review

| Action | Section Changed |
|--------|----------------|
| Add `plane-org-sync-api--request-sync` | Phase 1b, Phase 4, Section 7.2, Section 7.5 |
| Add `--collect` fan-out/fan-in combinator | Phase 4, Section 7.5 |
| Wrap `--read-headings` in `org-with-wide-buffer` with marker cleanup | Phase 2, Section 7.3 |
| Use sentinel comments for description region boundaries | Phase 2, Section 7.3 |
| Fix `--save-atomic` (no `revert-buffer`, use `set-buffer-modified-p` + `set-visited-file-modtime`) | Phase 2, Section 7.3 |
| Fix auth-source (strip URL scheme via `url-generic-parse-url`, funcall `:secret`, `user-error`) | Phase 1a, Section 7.1 |
| Change `conflict-resolver` to `defcustom` `conflict-function` with `:risky t` | Phase 1a, Section 7.1 |
| Fix HTML entity decoding order (LAST, not first; stash `<pre>` blocks) | Phase 2, Section 7.3 |
| Add `org-set-regexps-and-options` after `#+TODO:` modification | Phase 2, Section 7.3 |
| Non-repeating idle timer with self-re-arm | Phase 4, Section 7.5 |
| Use `json-parse-buffer` / `json-serialize` (C-native) | Phase 1b, Section 7.2 |
| Use `delay-mode-hooks` and disable `org-element-use-cache` in tests | Section 4 |
| Use `find-buffer-visiting` + `make-empty-file` in `--get-buffer` | Phase 2, Section 7.3 |
| Add `Maintainer`, `SPDX-License-Identifier`, `;;; Commentary:` to all files | M0, Section 7.1-7.5 |
| Fix `Keywords` to `convenience, comm` | Section 7.5 |
| Guard `(require 'org-capture)` in capture function | Phase 5, Section 7.5 |
| Add COPYING file (GPL-3.0-or-later) | M0, M5 |
| Run `package-lint` from M0, not just M5 | M0 |

### From Emacs Maintainer Response (REVIEW-EMACS-RESPONSE.md)

| Action | Section Changed |
|--------|----------------|
| Use `url-generic-parse-url` for HTTPS validation (explicit) | Phase 1a, Section 7.1 |
| Use `seq-remove` for draft/archived filtering | Phase 1b, Section 7.2 |
| Use `condition-case` with specific error classes (`file-error`, `json-parse-error`, `error`) | Phase 1b, Section 7.2 |
| Clarify `--collect` closure capture correctness (`let ((idx i))`) | Phase 4, Section 7.5 |
| Verify synchronous push path end-to-end (condition-case with keyword revert) | Phase 4, Section 7.5 |
| Verify description sentinels (constants, insert always emits both, update replaces between) | Phase 2, Section 7.3 |
| Verify `--save-atomic` without `revert-buffer` or `set-visited-file-name` | Phase 2, Section 7.3 |
| Verify `org-set-regexps-and-options` + `org-with-wide-buffer` | Phase 2, Section 7.3 |
| Document total worst-case push blocking time (12s) | Phase 4, Section 3 |
| State cache populated synchronously before async chain in pull | Phase 1a |

### Preserved Strengths (validated by both reviews)

| Decision | Validation |
|----------|------------|
| 5-file split (config, api, org, engine, commands) | Both reviews confirmed correct granularity |
| `org-map-entries` over `org-element-map` for read-write | Emacs review confirmed correct approach |
| Point markers over integer positions | Emacs review confirmed correct |
| `cl-letf` mock strategy | Both reviews confirmed sound |
| Pure Elisp, no external dependencies | Both reviews confirmed requirement |
| Global minor mode (with `PLANE_ID` guard) | Emacs review analyzed and confirmed |
| Synchronous HTTP for push | Both reviews agreed with the recommendation |

---

## Change History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 Draft | 2026-02-19 | TPO Agent | Initial implementation plan |
| 2.0 Revised | 2026-02-19 | TPO Agent | Addressed issues from TPO review and Emacs maintainer review |
| 3.0 Definitive | 2026-02-19 | TPO Agent | Incorporated Emacs maintainer response; clarified closure capture, specific error classes, seq-remove, url-generic-parse-url, worst-case push timing, save-atomic simplification. Definitive implementation guide. |
