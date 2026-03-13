# TPO Agent Memory

## Project Structure
- Requirements: `docs/URS.md`, `docs/SRS.md`, `docs/SDS.md`
- Implementation plan: `docs/IMPLEMENTATION-PLAN.md` (v3.0 Definitive, 2026-02-19)
- Reviews: `docs/REVIEW-TPO.md`, `docs/REVIEW-EMACS.md`, `docs/REVIEW-EMACS-RESPONSE.md`, `docs/REVIEW-IMPLEMENTATION.md`
- Source files: `plane-org-sync.el`, `plane-org-sync-api.el`, `plane-org-sync-engine.el`, `plane-org-sync-org.el`, `plane-org-sync-config.el`
- Tests (planned): `test/test-plane-org-sync-*.el`
- README.md created 2026-02-20

## Key Architecture Decisions
- Phase 0 (API verification) is a hard blocker before any coding
- Build order: Config -> API Client -> Org Interface -> Engine -> Commands
- MVP = M3: pull + push + conflict detection (bidirectional state sync)
- Push uses synchronous HTTP via `--request-sync` (5s timeout, 1 retry max)
- API client normalizes responses: engine never sees raw UUIDs
- `--update-heading` is the highest-risk function -- sentinel comments for description
- `--chain` (sequential) and `--collect` (fan-out) live in `plane-org-sync.el`
- `json-parse-buffer` (C-native) not `json-read`
- `org-set-regexps-and-options` after modifying #+TODO line
- `--save-atomic`: no `revert-buffer`, no `set-visited-file-name`
- State cache: populated on pull, on-demand fetch on push if empty
- Timer: non-repeating idle timer with self-re-arm (prevents stacking)

## Implementation Review Findings (2026-02-20)
- Verdict: PASS WITH ISSUES (0 blockers, 7 major, 9 minor, 8 notes)
- Top priority fix: MAJ-07 -- `plist-put` return values not captured in pull-apply-results
- Setup wizard missing: project selection (MAJ-01) and state confirmation (MAJ-02)
- Reset command only clears caches, does not match SRS full re-sync spec (MAJ-03)
- Capture template missing priority/label prompts (MAJ-04)
- Async 429 retry handler exists but not wired into paginate or resource functions (MIN-02)
- `--chain` utility is dead code (NOTE-02)
- Client-side assignee filtering fallback not implemented (NOTE-07)
- See full review: `docs/REVIEW-IMPLEMENTATION.md`

## Critical Elisp Patterns (from Emacs maintainer review)
- `org-with-wide-buffer` in `--read-headings` to handle narrowed buffers
- `url-generic-parse-url` for HTTPS validation (not string matching)
- `funcall :secret` from auth-source (deferred decryption)
- `seq-remove` for draft/archived filtering
- `condition-case` with specific classes: `file-error`, `json-parse-error`
- `delay-mode-hooks` + `org-element-use-cache nil` in test fixtures
- `(let ((idx i)) ...)` for correct closure capture in `--collect`
- ALWAYS capture `plist-put` return value -- not guaranteed to mutate in place

## 429 Rate Limit Storm (2026-03-13) -- RESOLVED
- Root causes: uncached user-id, no sync overlap guard, immediate re-arm after failure, states re-fetched every sync
- All fixes implemented and reviewed: tasks #10-#14 PASS
- Pull refactored into 4 functions: pull (entry) -> pull-fetch-projects -> pull-fetch-work-items -> pull-apply-results
- New defvars in config.el: `--state-cache-timestamp`, `--sync-in-progress`, `--backoff-multiplier`, `--consecutive-failures`
- Steady-state: 1 request/sync/project (work-items only) after first sync
- Note: api-me error path (line 295) uses unwrapped callback (not wrapped-cb) -- correct but subtle

## Open Questions Requiring API Verification
- `/work-items/` endpoint existence and response format (V1)
- `?expand=state,labels,assignees` support on `/work-items/` (V3)
- `?assignees=` filter support on `/work-items/` (V1)
- Pagination field names: `next_cursor` vs `cursor` (V4)
- See full list in docs/IMPLEMENTATION-PLAN.md Section 6
