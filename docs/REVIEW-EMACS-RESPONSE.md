# Emacs Maintainer Response to TPO Review

**Author**: Principal Emacs Package Maintainer
**Date**: 2026-02-19
**Input**: REVIEW-TPO.md v1.0, REVIEW-EMACS.md v1.0

---

## 1. Response to TPO Blocking Issues (B1--B4)

### B1: Complete V1 and V2 API verification before Phase 1b

**Agree.** No Elisp-specific notes. This is a pure sequencing/risk issue and the TPO is correct that building `url-retrieve` wrappers against an unverified endpoint is wasted effort.

### B2: Add HTTPS-only URL validation to Phase 1a

**Agree.** Implementation note: use `url-generic-parse-url` to extract the scheme, then reject anything other than `"https"`. This validation belongs in `plane-org-sync-config--get-api-key` or a dedicated `plane-org-sync-config--validate-instance-url` called at the top of both pull and push paths. Use `user-error` (not `error`) so the debugger does not trigger on a configuration mistake.

```elisp
(let ((scheme (url-type (url-generic-parse-url plane-org-sync-instance-url))))
  (unless (equal scheme "https")
    (user-error "plane-org-sync requires HTTPS.  Got: %s" scheme)))
```

### B3: Define state cache lifecycle

**Agree, and this intersects with my review item 2 (push hook cannot be async).** The TPO correctly identifies that push can fire when the state cache is empty. The synchronous push path I recommended (REVIEW-EMACS item 2) makes the fix straightforward: if `plane-org-sync--state-cache` is nil at push time, fetch states synchronously before resolving the keyword. This is one additional HTTP call (~200ms) and only happens on the first push after Emacs starts.

The lifecycle should be:
- **Populated**: at the start of `plane-org-sync-pull`, synchronously, before the async chain begins.
- **Read by**: `plane-org-sync-engine--keyword-to-state` (push path) and `--state-to-keyword` (pull path).
- **Invalidated**: on `plane-org-sync-reset`, on project config change, and at the start of each pull (re-fetched fresh).
- **Fallback (push)**: fetch synchronously via `plane-org-sync-api--request-sync` if nil.

### B4: Decide sync vs. async for push; add timeout and retry limit

**Agree -- this is my review item 2.** The TPO's recommended timeout (5s) and retry limit (1) are correct. I would add: use `url-retrieve-synchronously` with the TIMEOUT parameter (available since Emacs 26). For the retry, a single 429 retry with a 2-second sleep is the maximum acceptable blocking time in a hook function. Total worst case: 5s request + 2s backoff + 5s retry = 12s. Anything longer and Emacs feels frozen.

The TPO asks whether to revert the keyword on timeout. **Yes, revert.** The `condition-case` pattern from REVIEW-EMACS item 2 handles this cleanly -- any error (timeout, network, 4xx, 5xx) triggers revert via `(org-todo old-keyword)` with `plane-org-sync--inhibit-push` bound to `t`.

---

## 2. Elisp Implementation Guidance for TPO High-Priority Gaps (H1--H6)

### H1: Draft/archived filtering in `api-list-work-items`

If the Plane API does not filter server-side (V9 will determine this), add client-side filtering immediately after JSON parsing in `plane-org-sync-api-list-work-items`:

```elisp
(seq-remove (lambda (item)
              (or (eq t (plist-get item :is_draft))
                  (plist-get item :archived_at)))
            items)
```

Use `seq-remove` (from `seq.el`, built-in since Emacs 25) rather than `cl-remove-if`. The `:is_draft` value from `json-parse-buffer` with `:false-object nil` will be `t` for drafts and `nil` for non-drafts.

### H2: Network error and JSON parse error handling in `--request`

Wrap the `url-retrieve` callback body in `condition-case`:

- `file-error` -- DNS failure, connection refused, TLS handshake failure
- `json-parse-error` -- malformed JSON from the API (e.g., HTML error page from a reverse proxy)
- `error` -- catch-all for anything else

For the synchronous push path (`url-retrieve-synchronously`), the `condition-case` goes around the call itself. For the async pull path, it goes inside the sentinel/callback. Both should invoke the error callback with a user-readable message, never expose raw Elisp error objects to the user.

### H3: Multi-project partial failure integration test

Use `cl-letf` on `plane-org-sync-api--request` with a dispatch that returns success for projects 1 and 3, and signals `(error "HTTP 500")` for project 2. Verify that the sync file contains headings from projects 1 and 3, that project 2's existing headings are untouched, and that `*plane-org-sync-log*` contains the error for project 2.

### H4: Empty state cache push test

```elisp
(ert-deftest plane-org-sync-test-push-empty-cache ()
  "Push with empty state cache fetches states on demand."
  (let ((plane-org-sync--state-cache nil))
    ;; mock --request-sync to return states on GET .../states/
    ;; then verify --keyword-to-state resolves correctly
    ...))
```

The test should verify that (a) states are fetched, (b) the cache is populated as a side effect, and (c) the push succeeds.

### H5: Move `--chain` out of the API client file

**Agree -- this matches my review item 1.** Move to `plane-org-sync.el` with the prefix `plane-org-sync--chain`. Additionally, add `plane-org-sync--collect` (fan-out/fan-in, ~15 lines) for multi-project parallel fetching. Both are package-level utilities, not API-specific.

### H6: Add Emacs 29.1 to CI matrix

**Agree, but note**: Emacs 29.1 is not available via `purcell/setup-emacs` on all platforms. The matrix should include `29.1` and `29.4` (latest 29.x). If 29.1 is unavailable, 29.2 is the floor. The `Package-Requires` header should declare `(emacs "29.1")` regardless of CI availability -- `package-lint` validates the header, not the CI matrix.

---

## 3. Conflicts Between TPO and Emacs Maintainer Reviews

### 3.1 No real conflicts -- strong convergence

The two reviews identified many of the same issues independently, which is a good signal. Specific overlaps:

| Topic | TPO | Emacs | Verdict |
|-------|-----|-------|---------|
| `--chain` placement | H5: move out of API file | Item 1: move + add `--collect` | **Emacs review is stricter** -- also requires `--collect` for fan-out |
| Push sync/async | B4: sync, 5s timeout, 1 retry | Item 2: sync, full code sketch | **Aligned.** Emacs review provides implementation |
| State cache lifecycle | B3: define populate/invalidate | Item 2 (partial): sync fetch fallback | **Aligned.** TPO is more comprehensive on lifecycle |
| Mock strategy for async | S6: synchronous invocation | Item 15: accept limitation, document | **Aligned** |
| `org-lint` in SDS 6.5 | S3: recommend removal | Not covered | **No conflict.** I agree with removing it. `org-lint` is interactive-use tooling, not a save-time check |

### 3.2 One nuance: description region boundary

The TPO review (S4) asks for a test of description removal (null `description_html`). My review (item 4) says the description region boundary is undefined and proposes sentinel comments. These are complementary, not conflicting -- but the test cannot be written until the sentinel strategy is decided. **The sentinel comment approach from REVIEW-EMACS item 4 must be accepted before S4 test design.**

### 3.3 One addition the TPO missed

The TPO review does not cover:
- `org-with-wide-buffer` requirement (my item 3)
- `org-set-regexps-and-options` after `#+TODO:` modification (my item 11)
- HTML entity decoding order (my item 9)
- `json-parse-buffer` vs `json-read` (my item 14)
- Timer stacking with repeating idle timers (my item 13)

These are Elisp-specific issues that would not be visible to a product-level review. They must all be addressed in the revised plan.

---

## 4. TOP 5 Verification Items for the Revised Plan

These are the five things I will check first when reviewing the consolidated plan:

1. **`--chain` and `--collect` are defined in `plane-org-sync.el` with correct prefixes, explicit error propagation, and `--collect` supports multi-project fan-out.** This is the async foundation for the entire package. If it is wrong, pull sync is wrong.

2. **Push path uses `plane-org-sync-api--request-sync` (synchronous HTTP) end-to-end, with `condition-case`, 5-second timeout, single retry, keyword revert on any failure, and `plane-org-sync--inhibit-push` guard.** The code sketch from REVIEW-EMACS item 2 should be the reference implementation.

3. **Description region uses sentinel comments (`# plane-org-sync-description-begin` / `# plane-org-sync-description-end`), and `--update-heading` replaces only content between sentinels.** Without this, every description-related test is testing undefined behavior.

4. **`--save-atomic` does NOT call `revert-buffer`.** It uses `set-buffer-modified-p` + `set-visited-file-modtime` after `rename-file`. The `buffer-modified-p` guard check happens at the START of pull, before any mutations.

5. **`--ensure-todo-line` calls `(org-set-regexps-and-options)` after modifying the `#+TODO:` line, and `--read-headings` wraps its body in `org-with-wide-buffer`.** These are the two most likely sources of "works in my buffer but fails in tests" bugs.

---

## Change History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | 2026-02-19 | Emacs Package Maintainer | Initial response to TPO review |
