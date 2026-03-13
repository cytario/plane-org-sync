# plane-org-sync Project Memory

## Project Structure
- Multi-file Emacs package: `plane-org-sync.el` (main), `-config.el`, `-api.el`, `-engine.el`, `-org.el`
- Tests in `test/test-plane-org-sync-*.el` using ERT
- Docs in `docs/` (SRS.md, SDS.md, REVIEW-IMPLEMENTATION.md, etc.)

## Key Patterns
- API functions use `--request-with-retry` (not `--request`) for async calls with 429 handling
- Test macro `plane-org-sync-test-with-mock-api` mocks both `--request` and `--request-with-retry`
- Paginate tests mock `--request-with-retry` with `&optional _retry` param
- `plane-org-sync-mode` variable is forward-declared before `--schedule-auto-sync` to avoid byte-compile warning
- `plist-put` return values must always be captured with `setq` (MAJ-07 lesson)

## Build Commands
- Byte-compile: `emacs -batch -L . --eval "(setq byte-compile-error-on-warn t)" -f batch-byte-compile *.el`
- Tests: `emacs -batch -L . -l ert -l test/test-plane-org-sync-config.el -l test/test-plane-org-sync-api.el -l test/test-plane-org-sync-engine.el -l test/test-plane-org-sync-org.el -l test/test-plane-org-sync.el -f ert-run-tests-batch-and-exit`

## Plane API Expand Behavior
- `plane-org-sync-api-list-work-items` adds `?expand=state,labels,assignees`
- When expanded: `:state` is a plist (not UUID), `:labels` is vector of plists, `:assignees` is vector of plists
- Normalizer functions in `plane-org-sync.el` handle both expanded and plain forms
- Always normalize before passing items to org.el insert/update functions

## Critical Bug Patterns (fixed)
- **proj-headings filter**: Pull filters headings by `:plane-project-id` before diff. Missing PLANE_PROJECT_ID causes exclusion -> duplicates. Fix: include nil/empty project-id headings.
- **Stale metadata**: Diff only compared timestamps. Missing state/project-id not repaired. Fix: `heading-needs-repair-p` forces update when metadata incomplete.
- **Always `rm *.elc` before testing source changes**: Emacs prefers bytecode; stale .elc masks fixes.
