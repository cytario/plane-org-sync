# Implementation Review

**Product**: plane-org-sync (PS)
**Reviewer**: Technical Product Owner
**Date**: 2026-02-20
**Review Scope**: All source files vs. URS, SRS, SDS, and Implementation Plan v3.0

---

## Summary Verdict: PASS WITH ISSUES

The implementation is solid and covers the core sync workflow end-to-end. The
architecture follows the SDS decomposition faithfully, the async/sync HTTP split
is correct, and the most critical design decisions (sentinel-based description
regions, inhibit-push during pull, atomic file writes, two-tier state mapping)
are all implemented as specified. The code is well-structured, consistently
documented, and follows Emacs Lisp conventions.

However, there are several gaps between the implementation and the requirements,
ranging from missing features specified in the SRS to minor correctness
concerns. None are showstoppers for initial use, but several should be addressed
before a v1.0 release.

---

## Issue Summary

| Severity | Count |
|----------|-------|
| BLOCKER  | 0     |
| MAJOR    | 7     |
| MINOR    | 9     |
| NOTE     | 8     |

---

## MAJOR Issues

### MAJ-01: Setup wizard does not configure projects [SRS-PS-310104, SDS-PS-040002]

**File**: `plane-org-sync-config.el`, lines 180-251

The setup wizard (`plane-org-sync-setup`) prompts for instance URL, workspace,
API key, and sync file -- but does NOT prompt for project selection. The SRS
(310104) specifies: "When not set, `plane-org-sync-setup` fetches available
projects from the configured workspace and presents them for multi-selection."
The SDS (040002) specifies step 4: "Fetch and select projects (at least one must
be selected)."

The user must manually set `plane-org-sync-projects` to a list of UUIDs after
running setup, which defeats the purpose of a guided wizard.

**Recommended fix**: After validation, call `plane-org-sync-api-list-projects`
synchronously, present results via `completing-read-multiple`, resolve
identifiers to UUIDs, and persist via `customize-save-variable`.

### MAJ-02: Setup wizard does not display/confirm state mapping [SDS-PS-040002]

**File**: `plane-org-sync-config.el`, lines 180-251

The SDS (040002) specifies step 5: "Fetch states for selected projects, display
the auto-derived mapping for confirmation." The wizard skips this entirely.

**Recommended fix**: After project selection (once MAJ-01 is fixed), fetch
states for the selected projects, display the derived group-based mapping, and
optionally allow overrides. This is lower priority than MAJ-01 since group
defaults work out of the box.

### MAJ-03: Reset command does not match SRS specification [SRS-PS-310205]

**File**: `plane-org-sync.el`, lines 461-468

The SRS (310205) specifies that `plane-org-sync-reset`:
1. Prompts for confirmation
2. Removes all sync metadata properties from existing headings
3. Performs a full re-sync

The implementation only clears in-memory caches. It does not prompt, does not
touch the org file, and does not trigger a re-sync. The current behavior is a
"clear caches" command, not a "force re-sync" command.

**Recommended fix**: Either implement the full SRS spec (with confirmation
prompt, metadata stripping, and re-sync trigger), or rename/redocument the
command and add a separate `plane-org-sync-force-resync` command. Given v1.0
scope, a pragmatic approach is to document the current behavior accurately and
defer full re-sync to v1.1.

### MAJ-04: Capture template does not prompt for priority or labels [SRS-PS-310301]

**File**: `plane-org-sync.el`, lines 479-547

The SRS (310301) specifies the capture template should prompt for:
1. Project (via completing-read) -- IMPLEMENTED
2. Title (free text) -- IMPLEMENTED (via org-capture %^{Title})
3. Priority (via completing-read) -- MISSING
4. Labels (via completing-read with multiple selection) -- MISSING

The POST body only sends `:name`. Priority is hardcoded to "none", labels are
not sent.

**Recommended fix**: Add priority and label prompts in the `:before-finalize`
hook. Priority is a simple `completing-read` from a fixed list. Labels require
fetching from the API (can use `plane-org-sync-api-list-labels` synchronously).
Alternatively, document this as a v1.0 limitation and track for v1.1.

### MAJ-05: `plane-org-sync-api-list-work-items` callback signature inconsistency

**File**: `plane-org-sync-api.el`, lines 378-399

The function's callback receives `(items &optional error-msg)` on error, where
`items` is nil and `error-msg` is the error string. This differs from the
standard pattern used by all other resource functions:
`(status-code body)` or `(nil error-message)`.

The paginate function delivers results as `(results)` -- a single list argument.
But `list-work-items` wraps paginate and changes the signature to a two-argument
error pattern. The caller in `plane-org-sync-pull` (line 187-193) handles this
correctly, but the inconsistency is fragile.

Additionally, the lambda passed to `--paginate` has the signature
`(results &optional error-msg)`, but `--paginate`'s internal callback calls
`(funcall callback nil body)` on error (two args) and
`(funcall callback all-items)` on success (one arg). This works because the
`&optional error-msg` parameter captures the error string when present, but
it's a subtle contract.

**Recommended fix**: Normalize the callback pattern. Either always use
`(status-code body)` or always use `(results-or-nil &optional error)`.
Document the chosen pattern in a comment at the top of the API module.

### MAJ-06: State push path includes `/workspaces/` but base URL already provides it

**File**: `plane-org-sync.el`, lines 365-366

In `--on-todo-state-change`, the path is constructed as:
```elisp
(format "/workspaces/%s/projects/%s/work-items/%s/"
        plane-org-sync-workspace project-id plane-id)
```

But `plane-org-sync-api--request-sync` prepends `(plane-org-sync-api--base-url)`
which returns `{instance}/api/v1`. The resulting URL is:
`{instance}/api/v1/workspaces/{ws}/projects/{pid}/work-items/{id}/`

This is correct. However, the implementation plan's code snippet (Section Phase
4, line ~669) uses just `/projects/{pid}/work-items/{id}/` without the
`/workspaces/` prefix, suggesting a design intent to use workspace-relative
paths via a helper. The current implementation is technically correct but
inconsistent with how other resource functions construct paths (they use
`plane-org-sync-api--workspace-url()` helper).

**Recommended fix**: Use the existing path construction helpers for consistency:
```elisp
(let* ((path (concat "/workspaces/" plane-org-sync-workspace
                     "/projects/" project-id
                     "/work-items/" plane-id "/")))
```
This is what the code already does. However, consider extracting a helper to
reduce duplication with `plane-org-sync-api-get-work-item` and
`plane-org-sync-api-update-work-item`.

**Revised assessment**: On closer inspection, the path construction is correct
and consistent with the API module's resource functions. Downgrading to NOTE.
The inconsistency is only with the implementation plan's code snippet, not with
the actual codebase.

### MAJ-07: `plist-put` on `copy-sequence` result is discarded

**File**: `plane-org-sync.el`, lines 276-281

```elisp
(plist-put (copy-sequence item) :state_name state-name)
(plist-put item :state_name state-name)
(plist-put item :state_group state-group)
(plist-put item :state_id state-id)
(cons kw item)
```

Line 276 calls `(plist-put (copy-sequence item) :state_name state-name)` but
discards the result -- it is not bound to any variable. Line 277 then calls
`(plist-put item ...)` on the original item, which IS the mutation that takes
effect. The `copy-sequence` call on line 276 is dead code.

More critically, `plist-put` is NOT guaranteed to modify the list in place in
Emacs Lisp. Per the Emacs manual: "If PLIST already contains a given KEY, the
new VALUE replaces the old; otherwise, a new KEY-VALUE pair is added." The
return value must always be used. When adding a new key (like `:state_name` if
it does not already exist), `plist-put` may return a new cons cell that the
caller must capture.

In practice, since `item` comes from `json-parse-buffer` and likely does NOT
have `:state_name`, `:state_group`, or `:state_id` keys, the `plist-put` calls
on lines 277-279 may silently fail to modify `item`. The `(cons kw item)` on
line 281 would then pair `kw` with an item missing its resolved state info.

**Recommended fix**: Capture the return value of each `plist-put`:
```elisp
(setq item (plist-put item :state_name state-name))
(setq item (plist-put item :state_group state-group))
(setq item (plist-put item :state_id state-id))
(cons kw item)
```

Or use `plist-put` as an expression chain. This is a correctness bug that could
cause work items to be inserted/updated without proper state name resolution.

---

## MINOR Issues

### MIN-01: `plane-org-sync-file` default differs from SRS

**File**: `plane-org-sync-config.el`, line 61

The default is `"~/org/plane.org"`. The SRS (310106) specifies the default as
`plane.org` in `org-directory`. These may differ if the user has set
`org-directory` to something other than `~/org/`.

**Recommended fix**: Use `(expand-file-name "plane.org" org-directory)` as the
default, with a `:set` function or `defcustom :initialize` to handle the case
where `org-directory` is not yet loaded. Alternatively, since `org-directory`
defaults to `~/org`, the current value is correct for the common case. Document
the assumption.

### MIN-02: `--request` does not use retry-aware wrapper

**File**: `plane-org-sync-api.el`, lines 68-126

`plane-org-sync-api--request` does not handle 429 responses. The retry-aware
version is `plane-org-sync-api--request-with-retry`. However, the paginate
function (line 318) calls `--request` (no retry), meaning paginated fetches
do not retry on 429.

The resource functions (`api-list-states`, `api-me`, etc.) also use `--request`
directly, missing 429 handling.

**Recommended fix**: Either have `--paginate` and resource functions use
`--request-with-retry` instead of `--request`, or merge the retry logic into
`--request` itself (making it the default behavior).

### MIN-03: Ordered list items rendered as unordered [SRS-PS-420107]

**File**: `plane-org-sync-org.el`, lines 111-116

The SRS (420107) specifies `<ol>/<li>` should map to `1. item`. The
implementation maps ALL list items to `- item` (unordered). The comment on
line 113 acknowledges this: "We handle ol/ul items uniformly as `- ` since
distinguishing requires stateful parsing."

**Recommended fix**: This is acceptable for v1.0 as a documented limitation.
Add a note to the SRS known limitations section.

### MIN-04: `<table>` passthrough not implemented [SRS-PS-420107]

**File**: `plane-org-sync-org.el`

The SRS (420107) specifies `<table>` elements should be "Passed through as-is
(v1 limitation)." The implementation strips all remaining HTML tags (line 150:
`(replace-regexp-in-string "<[^>]+>" "" s)`), which would also strip `<table>`,
`<tr>`, `<td>` etc. Tables in descriptions will lose their structure.

**Recommended fix**: Extract and stash `<table>` blocks (similar to `<pre>`
handling) before the strip-tags step, then re-insert them. Or accept this as a
v1.0 limitation.

### MIN-05: `--update-heading` does not use `org-todo` for keyword changes

**File**: `plane-org-sync-org.el`, lines 370-480

The update function replaces the headline text directly (line 383-384) via
string manipulation. This sets the TODO keyword without going through
`org-todo`, which means:
1. `org-after-todo-state-change-hook` does not fire (which is actually desired
   during pull -- the inhibit-push mechanism handles this)
2. Org logging (state change notes in LOGBOOK) is not triggered

This is arguably correct for pull sync (we do not want logging for remote
state changes), but it differs from the SDS approach which mentions using
`org-todo` with inhibit-push binding. The current approach is simpler and safer.

**Recommended fix**: Document as an intentional design decision. If users want
LOGBOOK entries for remote state changes, this can be added later.

### MIN-06: Auto-sync timer re-arm does not check mode status

**File**: `plane-org-sync.el`, lines 113-124

If `plane-org-sync-mode` is disabled while a pull is in progress, the callback
will still call `--schedule-auto-sync`, which checks `plane-org-sync-auto-interval`
but not whether `plane-org-sync-mode` is still active. The timer could re-arm
after the mode is disabled.

**Recommended fix**: Add `plane-org-sync-mode` to the guard condition in
`--schedule-auto-sync`:
```elisp
(when (and plane-org-sync-mode
           plane-org-sync-auto-interval
           (not plane-org-sync--timer))
  ...)
```

### MIN-07: Status command does not show auto-sync status [SRS-PS-310501]

**File**: `plane-org-sync.el`, lines 437-456

The SRS (310501) specifies the status command should display "Whether auto-sync
is active and its interval." The implementation shows last sync time, result,
cached projects, and user ID -- but not auto-sync status.

**Recommended fix**: Add `plane-org-sync-mode` and `plane-org-sync-auto-interval`
to the status output.

### MIN-08: `plane-org-sync-api-key` stored via `customize-save-variable` in plaintext

**File**: `plane-org-sync-config.el`, line 249

When the user enters the API key directly in setup, it is persisted via
`customize-save-variable`, which writes it to `custom-file` (typically
`~/.emacs.d/custom.el`) in plaintext. The SRS (520001) states the recommended
method is auth-source and plaintext storage should be "supported but discouraged
in documentation."

**Recommended fix**: When the user enters the API key directly, offer to store
it in auth-source instead of in custom.el. At minimum, display a warning that
the key will be stored in plaintext and recommend auth-source.

### MIN-09: Heading format places `*` as a list element in `parts`

**File**: `plane-org-sync-org.el`, lines 278-300

The `--format-headline` function builds the headline by pushing elements onto
a `parts` list and joining with spaces. The initial `parts` is `(list "*")`.
Then keyword and priority are pushed (which prepends), then title is pushed.
After `nreverse`, the order is `* keyword [#P] title`. This works correctly.

However, if `keyword` is nil (e.g., no TODO state), the headline would be
`* title` -- which is correct. But the function always includes `*` in the
joined string, so `--insert-heading` does not need to add its own `*`. The
caller at line 316 directly inserts the result. This is correct but the
convention is slightly unusual (most org code constructs headlines without the
leading `*`).

**Recommended fix**: No fix needed. This is a style observation. The approach
works correctly.

---

## NOTES

### NOTE-01: No `plane-org-sync-api-get-work-item` used in pull path

The `plane-org-sync-api-get-work-item` function exists but is only used in the
push path (via `--request-sync` inline in the hook). The pull path uses
`list-work-items` for bulk fetch. This is correct per the design.

### NOTE-02: `--chain` utility is defined but not used in the codebase

**File**: `plane-org-sync.el`, lines 46-84

The `plane-org-sync--chain` async utility is implemented but never called. The
pull flow uses `--collect` for fan-out and nested callbacks for sequencing. The
`--chain` function is dead code in the current implementation.

**Recommendation**: Remove or add a usage comment explaining it is reserved for
future multi-step flows.

### NOTE-03: `plane-org-sync-api--request-with-retry` duplicates `--request` logic

**File**: `plane-org-sync-api.el`, lines 223-286

The retry-aware request function is a near-complete copy of `--request` with
429 handling added. This is ~60 lines of duplicated logic.

**Recommendation**: Refactor `--request` to accept an optional retry-count
parameter, eliminating the separate function.

### NOTE-04: SRS-PS-310205 (Force Re-sync) scope decision needed

As noted in MAJ-03, the reset command's behavior differs from the SRS. This
should be explicitly addressed in the SRS as either a v1.0 scope reduction or
a tracked gap.

### NOTE-05: `--collect` receives `&rest _prev` but ignores previous results

**File**: `plane-org-sync.el`, line 79

In the `--chain` utility's recursive call, a wrapper lambda is created with
signature `(lambda (next &rest _prev) ...)`. The `_prev` parameter is explicitly
ignored. This is correct for the chain reset pattern but worth noting for
future maintainers.

### NOTE-06: MAJ-06 reclassified

See MAJ-06 above. The path construction in the push hook is actually correct
and consistent with the rest of the codebase. The discrepancy is only with the
implementation plan's code snippet, which used a shorter path format. No fix
needed.

### NOTE-07: Assignee filter sends UUID as string parameter

**File**: `plane-org-sync.el`, lines 181-184

The assignee filter sends `(cons "assignees" user-id)` as a query parameter.
This passes a single UUID string. The Plane API documentation is not explicit
about whether `?assignees=` accepts a single UUID or requires an array format.
The SRS (310201) specifies a client-side fallback if the server-side filter is
not supported, but the client-side fallback is NOT implemented. If the API
ignores the parameter, all items would be synced regardless of assignment.

**Recommendation**: Verify against the actual API. If the filter is not
supported on `/work-items/`, implement client-side filtering by comparing item
assignee UUIDs against `plane-org-sync--user-id`.

### NOTE-08: `PLANE_ASSIGNEES` property uses display names, not UUIDs

The `PLANE_ASSIGNEES` property stores comma-separated display names (from
`assignee_details`). This is correct for human readability but means the
property cannot be used for programmatic assignee comparison. The
`PLANE_ASSIGNEES` property is documented as informational in the SRS.

---

## Requirements Traceability

### SRS Requirements Coverage

| SRS Requirement | Status | Notes |
|----------------|--------|-------|
| SRS-PS-310101 (API Key Config) | COVERED | defcustom + auth-source |
| SRS-PS-310102 (Instance URL) | COVERED | HTTPS validation |
| SRS-PS-310103 (Workspace Config) | COVERED | Setup wizard + defcustom |
| SRS-PS-310104 (Project Config) | PARTIAL | defcustom exists, but wizard does not populate it (MAJ-01) |
| SRS-PS-310105 (State Mapping) | COVERED | Two-tier mapping implemented |
| SRS-PS-310106 (Sync File) | COVERED | Default path differs slightly (MIN-01) |
| SRS-PS-310107 (Assignee Filter) | PARTIAL | Server-side filter sent, no client-side fallback (NOTE-07) |
| SRS-PS-310108 (Reverse State Resolution) | COVERED | Full 4-step algorithm |
| SRS-PS-310201 (Manual Sync) | COVERED | Async pull with fan-out |
| SRS-PS-310202 (Auto Sync Timer) | COVERED | Non-repeating idle timer (MIN-06) |
| SRS-PS-310203 (Push State Change) | COVERED | Synchronous push with revert |
| SRS-PS-310204 (Conflict Detection) | COVERED | Timestamp comparison + prompt |
| SRS-PS-310205 (Force Re-sync) | PARTIAL | Only clears caches (MAJ-03) |
| SRS-PS-310206 (Suppress Push During Pull) | COVERED | inhibit-push binding |
| SRS-PS-310301 (Capture Template) | PARTIAL | Missing priority/label prompts (MAJ-04) |
| SRS-PS-310401 (Plane Link) | COVERED | Link line in heading body |
| SRS-PS-310501 (Sync Status) | PARTIAL | Missing auto-sync info (MIN-07) |
| SRS-PS-310502 (Error Reporting) | COVERED | Log buffer + messages |
| SRS-PS-420101 (Heading Structure) | COVERED | Full property set |
| SRS-PS-420102 (Heading Matching) | COVERED | PLANE_ID matching |
| SRS-PS-420103 (Local Content Preservation) | COVERED | Sentinel-based description |
| SRS-PS-420104 (Atomic File Writes) | COVERED | temp + rename |
| SRS-PS-420105 (TODO Keyword Generation) | COVERED | Managed #+TODO line |
| SRS-PS-420106 (State Identity Properties) | COVERED | PLANE_STATE + PLANE_STATE_ID |
| SRS-PS-420107 (HTML-to-Org) | PARTIAL | ol->unordered (MIN-03), table stripped (MIN-04) |
| SRS-PS-420108 (Orphaned Preservation) | COVERED | Orphaned headings untouched |
| SRS-PS-420201 (Priority Mapping) | COVERED | Correct table |
| SRS-PS-510001 (Sync Response Time) | COVERED | Async design |
| SRS-PS-510002 (Rate Limit) | PARTIAL | Async path has handler but not wired (MIN-02) |
| SRS-PS-520001 (API Key Storage) | COVERED | auth-source integration |
| SRS-PS-520002 (HTTPS Only) | COVERED | url-generic-parse-url validation |
| SRS-PS-530001 (Package Distribution) | COVERED | Correct package headers |
| SRS-PS-530002 (Emacs 29.1+) | COVERED | Package-Requires declaration |

### SDS Design Compliance

| SDS Component | Status | Notes |
|--------------|--------|-------|
| SDS-PS-010101 (Async HTTP) | COVERED | url-retrieve wrapper |
| SDS-PS-010102 (Rate Limit) | PARTIAL | Handler exists but not used by all paths (MIN-02) |
| SDS-PS-010103 (Pagination) | COVERED | Cursor-based accumulation |
| SDS-PS-010201 (Fetch Work Items) | COVERED | With expand and filter |
| SDS-PS-010202 (Update Work Item) | COVERED | PATCH endpoint |
| SDS-PS-010203 (Create Work Item) | COVERED | POST endpoint |
| SDS-PS-010204 (Fetch States) | COVERED | With results unwrapping |
| SDS-PS-010205 (Fetch Current User) | COVERED | /users/me/ |
| SDS-PS-020101 (Compute Sync Diff) | COVERED | Hash-table based diff |
| SDS-PS-020201 (State to Keyword) | COVERED | Two-tier resolution |
| SDS-PS-020202 (Keyword to State) | COVERED | Reverse resolution with group check |
| SDS-PS-020301 (Detect Conflicts) | COVERED | Timestamp comparison |
| SDS-PS-030101 (Extract Headings) | COVERED | org-map-entries with markers |
| SDS-PS-030201 (Create Heading) | COVERED | Full heading with sentinels |
| SDS-PS-030202 (Update Heading) | COVERED | Reverse buffer order updates |
| SDS-PS-030203 (Atomic Save) | COVERED | temp + rename, no revert-buffer |
| SDS-PS-030301 (Priority Mapper) | COVERED | Correct mapping table |
| SDS-PS-030401 (HTML-to-Org) | COVERED | 7-step pipeline |
| SDS-PS-040001 (Customization Group) | COVERED | All defcustoms present |
| SDS-PS-040002 (Setup Wizard) | PARTIAL | Missing project selection and state confirmation |
| SDS-PS-050001 (Minor Mode) | COVERED | Hook, timer, agenda integration |
| SDS-PS-050002 (Capture Template) | PARTIAL | Missing priority/label prompts |
| SDS-PS-050003 (State Change Push) | COVERED | Synchronous with revert |
| SDS-PS-050004 (Push Suppression) | COVERED | Dynamic variable binding |

---

## Priority Fix Order

1. **MAJ-07** (plist-put return value) -- Correctness bug affecting all synced items
2. **MIN-02** (429 retry not wired for async) -- Rate limit compliance
3. **MIN-06** (timer re-arm guard) -- Could cause unwanted background syncs
4. **MAJ-01** (setup wizard projects) -- Major UX gap
5. **MAJ-03** (reset command scope) -- Either implement or document the gap
6. **MAJ-04** (capture priority/labels) -- Feature gap
7. **MAJ-05** (callback signature) -- Code health
8. **MIN-07** (status auto-sync info) -- Minor UX gap
9. **MIN-08** (plaintext API key warning) -- Security best practice
10. Remaining MINOR and NOTE items

---

## Change History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | 2026-02-20 | TPO Agent | Initial implementation review |
