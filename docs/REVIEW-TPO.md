# TPO Review: Implementation Plan

**Reviewer**: Technical Product Owner Agent
**Date**: 2026-02-19
**Documents Reviewed**: IMPLEMENTATION-PLAN.md v1.0 Draft against URS v1.0, SRS v1.0, SDS v1.0
**Verdict**: Conditionally approved -- address blocking issues before Phase 1b begins.

---

## 1. Requirements Coverage Gaps

### 1.1 Missing SRS Requirements

**[BLOCKER] SRS-PS-520002 (HTTPS Only) -- Not in any phase checklist.**
The SRS requires: "The system must reject HTTP instance URLs." No function in the plan validates the instance URL scheme. This belongs in Phase 1a (`plane-org-sync-config.el`) as a validation in `plane-org-sync-config--get-api-key` or a dedicated `--validate-instance-url` function.

**Action**: Add URL scheme validation to Phase 1a checklist. A `user-error` on non-HTTPS URLs.

**[BLOCKER] SRS-PS-530002 (Emacs 29.1 Compatibility) -- Not in any phase checklist.**
The SRS requires Emacs 29.1+ support. The plan mentions CI testing on Emacs 29.4 and 30.1, but the checklist has no item for verifying that no Emacs 30-only APIs are used. The `Package-Requires` header is mentioned only in the file skeleton (Section 7.5) but not validated.

**Action**: Add explicit compatibility check to M0 and M5 checklists. CI must test on Emacs 29.1 (the floor), not 29.4.

**[MINOR] SRS-PS-310201 -- Draft/Archived Exclusion Logic Missing from Phase Checklist.**
The SRS explicitly states: "The system excludes work items where `is_draft` is true or `archived_at` is not null." The plan mentions V9 (API verification for draft/archived behavior), but the Phase 1b checklist for `api-list-work-items` does not include client-side filtering for these fields. If the API returns drafts and archived items by default (which V9 will determine), the filtering logic must exist somewhere.

**Action**: Add "Filter out `is_draft` and `archived_at` items" to Phase 1b `api-list-work-items` implementation notes. If the API does not filter server-side, add client-side filtering alongside the assignee fallback.

**[MINOR] SRS-PS-420106 (PLANE_STATE and PLANE_STATE_ID properties) -- Partially covered.**
The SRS requires both `PLANE_STATE` (human-readable) and `PLANE_STATE_ID` (UUID) properties. The Phase 2 heading record (IP-3, Section 5) mentions `:plane-state-id` but not `:plane-state`. The `--read-headings` function (Section 7.3, line 771) lists `PLANE_STATE_ID` but not `PLANE_STATE`. The `--update-heading` function description mentions "PLANE_* properties" generically. This is ambiguous enough to cause an implementor to miss `PLANE_STATE`.

**Action**: Explicitly list `PLANE_STATE` in the heading record plist and in the `--update-heading` property update list.

### 1.2 Missing SDS Requirements

**[MINOR] SDS Section 6.5 (Org File Safety) -- `org-lint` check not in checklist.**
The SDS states: "validated (e.g., `org-lint` basic checks)." The plan's `--save-atomic` implementation notes (Section 7.3, line 805-806) check `buffer-modified-p` but do not mention `org-lint`. Either implement the lint check or revise the SDS to remove the reference. My recommendation: remove it from the SDS. `org-lint` is slow, designed for interactive use, and would add latency to every save. The atomic write pattern already provides adequate safety.

**Action**: Decide whether to keep or drop the `org-lint` reference. If dropped, update SDS 6.5.

### 1.3 Covered But Implicit

The following requirements are covered but only through generic references like "all PLANE_* properties" or via the SDS reference column. These should be made explicit in the checklist to prevent omission during implementation:

- `PLANE_ASSIGNEES` property (SRS-PS-420101) -- the heading structure shows it, but no function in the plan explicitly formats or reads it.
- `CATEGORY` property (SDS-PS-050001) -- mentioned only under minor mode, but actually needs to be set during `--insert-heading` and `--update-heading`.

---

## 2. Phase Ordering Risks

### 2.1 Engine Before Org Interface -- Low Risk, But Suboptimal

The plan orders phases as Config -> API -> Org -> Engine -> Commands. The dependency graph is correct: the engine depends on both API and Org. However, the engine (Phase 3) is listed *after* the Org interface (Phase 2), despite the engine being pure functions with no I/O dependencies. The engine could be built in parallel with Phase 2.

More importantly, the engine's `--diff` function defines the **contract** between the API client's output and the Org interface's input. Building it first (or at least designing its input/output types first) would catch data format mismatches before they propagate through two layers.

**Recommendation**: Move `plane-org-sync-engine--diff` type definitions and `--state-to-keyword`/`--keyword-to-state` to Phase 1 (they depend only on Config). This de-risks the integration points IP-1 and IP-2 early.

### 2.2 `--chain` Utility Placement -- Wrong Phase

The `--chain` async utility is listed at the bottom of Phase 1b (item 12 of 14 in the API client). But `plane-org-sync-pull` (Phase 4) is the first consumer. More critically, `--chain` is not an API function -- it is a general-purpose async sequencing utility. It belongs in the main `plane-org-sync.el` or in a shared utilities section, not in the API client.

Placement in the API client file creates a circular dependency concern if future callers from other modules need it. The SDS places it in Section 6.1 (Crosscutting Concepts), suggesting it is package-level, not API-level.

**Action**: Move `--chain` to `plane-org-sync.el` or create it as a standalone utility. It is needed in Phase 4, not Phase 1b.

### 2.3 State Cache Lifecycle -- Underdefined

`plane-org-sync--state-cache` is declared in Phase 1a (config.el) but populated in Phase 4 (the pull command). The engine (Phase 3) needs state data for `--keyword-to-state`. The plan does not specify when the cache is populated, validated, or invalidated.

**Action**: Define the cache lifecycle explicitly:
- **Populated**: At the start of `plane-org-sync-pull`, after fetching states.
- **Read by**: `--keyword-to-state` (during push), `--collect-keywords` (during pull).
- **Invalidated**: On `plane-org-sync-reset`, on project config change, and optionally on each pull (since states can change).
- **Fallback**: If cache is empty during push, fetch states synchronously before resolving.

This is important because push happens outside of pull (via the hook), so the cache may be empty if no pull has occurred yet.

---

## 3. API Verification Timing

### 3.1 V1 Must Complete Before Phase 1b Coding

The plan says API verification happens "before or during implementation" (Section 6) and schedules it for "Day 2 (parallel)" with Phase 1. This is dangerously optimistic.

V1 (does `/work-items/` exist?) is the single largest risk. If it does not exist, the entire URL construction layer and every endpoint function in Phase 1b must change. Building the API client while V1 is unresolved means potentially rewriting it.

**V1 and V2 are hard prerequisites for Phase 1b coding.** V3 (expand behavior) and V4 (pagination shape) are soft prerequisites -- the code can be written with fallbacks, but the fallbacks themselves need to be designed.

**Action**: Restructure the schedule:
- Day 1: Phase 1a (Config) + V1 + V2 + V7 (test auth, confirm endpoint exists, check state structure)
- Day 2: V3 + V4 + V8 (confirm expand, pagination, rate limit headers) -- these inform the API client design
- Day 2-4: Phase 1b (API Client) with verified endpoint behavior
- V5, V6, V9: Can happen during Phase 2-3

### 3.2 Missing Verification: Label Endpoint on `/work-items/`

V1 asks about work items. V2 asks about states. But the label list endpoint (`GET .../labels/`) is not included in V1-V9 for the `/work-items/` context. Plane's label endpoint is project-level, not work-item-level, so it likely is not affected by the migration. But confirm it.

**Action**: Add V10: Verify `GET .../labels/` returns `id`, `name`, `color` for a project.

---

## 4. Testing Gaps

### 4.1 Missing Test Scenarios

**[HIGH] Multi-project pull with mixed success/failure.**
SRS-PS-310201 specifies partial sync reporting ("Synced 2/3 projects. Failed: PROJ-C (HTTP 500)."). No integration test covers this scenario. The plan's integration tests (Section 4) assume all projects succeed.

**Action**: Add integration test: mock 3 projects, second returns HTTP 500. Verify first and third are synced, error reported for second.

**[HIGH] Push when state cache is empty.**
The push hook can fire at any time (user changes a TODO keyword). If no pull has occurred, `plane-org-sync--state-cache` is nil. The plan does not test this path. The push must either fail gracefully or fetch states on-demand.

**Action**: Add unit test for `--keyword-to-state` with empty state cache. Define behavior: fetch-on-demand or error with "Run plane-org-sync-pull first"?

**[MEDIUM] Rate limit recovery mid-sync.**
The plan mentions rate limit handling (429 + backoff) in the API client, but no test validates behavior when the rate limit is hit *between* projects in a multi-project sync. Does the backoff delay correctly, and does the sync resume?

**Action**: Add integration test: mock API to return 429 after first project's items, then succeed on retry. Verify sync completes.

**[MEDIUM] `--update-heading` with no description (null `description_html`).**
The unit test list mentions "description added/removed/changed" but does not explicitly call out the transition from "had description" to "null description_html" (removal). SRS-PS-420103 specifies this case: "If a description previously existed and the remote value is now null, the existing description paragraph is removed on the next sync."

**Action**: Add explicit test case: heading with existing description paragraph, update with `description_html: nil`, verify paragraph is removed but sub-headings are preserved.

**[MEDIUM] Re-sync idempotency with state mapping changes.**
M2 acceptance criteria say "running pull twice produces the same file." But what if `plane-org-sync-group-keyword-mapping` changes between pulls? The `#+TODO:` line updates, keywords change, and headings may end up with stale keywords. No test covers this.

**Action**: Add test: pull with default mapping, change `group-keyword-mapping` (e.g., `started` -> `IN-PROGRESS` instead of `STARTED`), pull again. Verify headings update to new keywords and `#+TODO:` line reflects the change.

**[LOW] Empty project list pull.**
SRS-PS-310104 says nil projects should error: "No projects configured." No test covers this.

**Action**: Add unit test for `plane-org-sync-pull` with `plane-org-sync-projects` set to nil.

### 4.2 Test Infrastructure Concern

The mock macro (Section 4, line 338-349) uses `cl-letf` to override `plane-org-sync-api--request`. This works for synchronous tests, but for async tests (where callbacks fire via `run-at-time` or `url-retrieve`), the `cl-letf` binding may have already unwound by the time the callback executes. The plan does not address this.

**Action**: For async chain integration tests, either:
1. Make the mock invoke callbacks synchronously (immediately call the callback, simulating resolved futures), or
2. Use a different mock strategy that persists across the event loop.

Option 1 is simpler and sufficient because the tests do not need to validate actual async timing -- just callback wiring.

---

## 5. Async vs. Sync Decision for Push

### 5.1 The Plan's Recommendation Is Correct, But Incomplete

The plan recommends synchronous HTTP for push (Section 8, item 3). The reasoning is sound: the hook fires synchronously, the push is a single item (<1s), and capture already uses sync HTTP. I agree with this decision.

However, the plan does not address what happens when the sync HTTP blocks longer than expected (e.g., slow network, rate limit retry). Specifically:

1. **Retry behavior**: The 429 handler uses exponential backoff (2s, 4s, 8s). With sync HTTP, this blocks Emacs for up to 14 seconds in the worst case. That is unacceptable for a hook function.
2. **Timeout**: No timeout is specified for push HTTP calls. `url-retrieve-synchronously` has a `TIMEOUT` parameter. If the server is unreachable, the default timeout can be 30+ seconds.

**Action**:
- Set a timeout of 5 seconds for synchronous push calls.
- Limit retries to 1 (not 3) for synchronous push. If the first retry fails, abort and tell the user.
- Document in the SDS that push uses `url-retrieve-synchronously` with a timeout.
- Consider: if push times out, should the local keyword revert? I say yes -- the user's intent was to change state, and if we cannot confirm it, the local state should reflect reality (unchanged on the server).

### 5.2 SDS Gap

The SDS does not specify sync vs. async for push. The runtime view (Section 4.2) shows the push flow but does not indicate synchronous behavior. The SDS should be updated to match the implementation plan's decision.

**Action**: Add a Design Decision (Section 7.4) to the SDS documenting the sync HTTP choice for push, with justification and the timeout/retry constraints.

---

## 6. Missing Error Paths

### 6.1 Errors Not Covered in the Implementation Checklist

| Error Scenario | SRS Reference | Coverage in Plan |
|---|---|---|
| API key invalid (401/403) | SRS-PS-310101, SRS-PS-310502 | Covered (error logging) |
| Rate limit (429) | SRS-PS-510002 | Covered (backoff) |
| Network unreachable | SRS-PS-310502 | NOT covered |
| Workflow constraint violation (400) | SRS-PS-310203 | Covered (revert keyword) |
| Partial multi-project failure | SRS-PS-310201 | NOT covered (see Section 4.1) |
| Sync file locked by another process | SRS-PS-420104 (implied) | NOT covered |
| Sync file has unsaved modifications | SDS-PS-030203 | Covered (abort with warning) |
| Malformed JSON response | SRS-PS-310502 (implied) | NOT covered |
| Empty/null `state` on a work item | SRS-PS-310105 (implied) | NOT covered |
| Project UUID no longer valid (deleted project) | SRS-PS-310104 (implied) | NOT covered |

**[HIGH] Network unreachable.** `url-retrieve` signals `file-error` on DNS failure, connection refused, etc. The plan's `--request` function must catch this in `condition-case` and invoke the error callback. Currently, the plan only describes handling HTTP status codes (429, 400, etc.), not transport-level failures.

**Action**: Add `condition-case` for `file-error` and `error` around `url-retrieve` in `--request`. Log the error. For pull, report "Network error: {message}". For push, revert the keyword and report.

**[MEDIUM] Malformed JSON.** If the API returns invalid JSON (e.g., HTML error page from a reverse proxy), `json-read` signals `json-readtable-error`. The plan does not mention this.

**Action**: Wrap `json-read` in `condition-case` within `--request`. On parse failure, treat as a transport error and report "Invalid response from Plane API."

**[LOW] Null state on work item.** If `state` is null (theoretically possible for newly created items), `--state-to-keyword` would receive nil arguments. The plan should handle this gracefully (default to "TODO").

---

## 7. Scope Creep Risk

### 7.1 Items in the Plan That Are NOT in the Requirements

The plan is generally disciplined about scope. I found two items that go beyond the requirements:

1. **`plane-org-sync--last-sync` and `plane-org-sync--last-sync-result` variables** (Section 7.1, line 639-640). These are internal state for `plane-org-sync-status`. They are not explicitly required by the SRS, but they are the obvious implementation for SRS-PS-310501 (status command). **Verdict: acceptable**, not scope creep.

2. **`plane-org-sync-org--priority-from-org`** (Section 7.3, line 751: "reverse mapping for future use"). This is explicitly marked as "for future use." Implementing unused functions is mild scope creep. **Verdict: borderline.** Implement it since it is 3 lines and aids symmetry, but do not test it beyond a basic unit test.

### 7.2 Items Missing from Scope Exclusions

The plan does not explicitly exclude these, though the SRS does:

- The SRS scope exclusions (Section 1.5) list "Title push (Org to Plane)" and "Description push (Org to Plane)." The plan correctly does not include these, but should reference the scope exclusions to prevent future implementors from adding them.

**Action**: Add a "Scope Boundaries" section to the plan that references SRS Section 1.5 explicitly.

---

## 8. Critical Path Assessment

### 8.1 The Stated Critical Path Is Mostly Correct

The plan identifies: Phase 1b (API Client) -> Phase 2 (`--update-heading`) -> Phase 4 (wiring).

I agree this is the critical path, but with a refinement:

**The actual critical path is: V1 verification -> Phase 1b -> Phase 2 (`--update-heading`) -> Phase 4 (`plane-org-sync-pull` async chain wiring).**

V1 is on the critical path because Phase 1b cannot begin meaningfully without knowing whether `/work-items/` exists. If V1 fails (endpoint does not exist), the fallback to `/issues/` changes the URL construction layer and every resource function. This is a 0.5-day delay minimum.

### 8.2 Underestimated Risk: Phase 4 Pull Command

The plan estimates Phase 4 at 2-3 days. The `plane-org-sync-pull` function is the most complex function in the entire package -- it chains 3-4 async API calls, aggregates results across projects, handles partial failures, performs synchronous buffer mutations, and saves atomically. This is not a 2-3 day effort if the async chain utility also needs debugging.

The push hook is simpler (synchronous HTTP, single item), but the conflict detection + revert flow has subtle edge cases (what if the heading is narrowed? what if the buffer was killed between hook fire and callback?).

**Recommendation**: Allocate 3-4 days for Phase 4, not 2-3. The overall estimate of 12-16 days should be 14-18 days.

---

## 9. Summary of Required Actions

### Before Phase 1b Begins (Blocking)

| # | Action | Section |
|---|--------|---------|
| B1 | Complete V1 and V2 API verification against real Plane instance | 3.1 |
| B2 | Add HTTPS-only URL validation to Phase 1a checklist | 1.1 |
| B3 | Define state cache lifecycle (populate, read, invalidate, fallback) | 2.3 |
| B4 | Decide sync vs. async for push, add timeout (5s) and retry limit (1) | 5.1 |

### Before Implementation Starts (High Priority)

| # | Action | Section |
|---|--------|---------|
| H1 | Add draft/archived filtering to Phase 1b `api-list-work-items` | 1.1 |
| H2 | Add network error and JSON parse error handling to `--request` | 6.1 |
| H3 | Add multi-project partial failure integration test | 4.1 |
| H4 | Add empty state cache push test | 4.1 |
| H5 | Move `--chain` utility out of the API client file | 2.2 |
| H6 | Add Emacs 29.1 (floor version) to CI matrix | 1.1 |

### During Implementation (Should Fix)

| # | Action | Section |
|---|--------|---------|
| S1 | Explicitly list `PLANE_STATE` in heading record plist | 1.1 |
| S2 | Add `CATEGORY` property to `--insert-heading` and `--update-heading` | 1.3 |
| S3 | Resolve `org-lint` reference in SDS 6.5 (recommend: remove it) | 1.2 |
| S4 | Add description removal test case | 4.1 |
| S5 | Add state mapping change re-sync test | 4.1 |
| S6 | Document mock strategy for async tests (synchronous invocation) | 4.2 |
| S7 | Add V10: label endpoint verification | 3.2 |
| S8 | Add scope boundary reference to plan | 7.2 |
| S9 | Update Phase 4 time estimate to 3-4 days | 8.2 |
| S10 | Update SDS with push sync/async design decision | 5.2 |

---

## 10. What the Plan Gets Right

To be fair, this is a strong implementation plan. Specific strengths:

- **The dependency graph and phase ordering are correct.** Building bottom-up with clear deliverables per phase is the right approach.
- **The risk table is honest.** The `--update-heading` risk, the `?expand=` uncertainty, and the async callback ordering concern are all real risks that other plans would paper over.
- **The mock strategy is practical.** `cl-letf` on `--request` is the right abstraction level for API mocking.
- **The integration points (IP-1 through IP-5) are well-identified.** These are exactly the seams where bugs will live.
- **The "Requirements Likely to Need Revision" section (Section 8) is valuable.** Especially item 3 (async model for push), item 6 (ambiguous group-to-state resolution), and item 7 (idle timer semantics).
- **The MVP definition (M3) is correctly scoped.** Pull + push + conflict detection is the minimum viable product.

---

## Change History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 | 2026-02-19 | TPO Agent | Initial review |
