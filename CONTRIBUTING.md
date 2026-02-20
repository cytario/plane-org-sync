# Contributing to plane-org-sync

Thank you for considering a contribution to plane-org-sync. This guide covers
everything you need to get started.

## Development Environment

### Prerequisites

- **Emacs 29.1** or later (29.1 and 30.1 are tested in CI)
- **Git**

### Setup

Clone the repository and verify byte-compilation and tests pass:

```sh
git clone https://github.com/martin/plane-org-sync.git
cd plane-org-sync

# Byte-compile (must produce zero warnings)
emacs -batch -L . \
  --eval "(setq byte-compile-error-on-warn t)" \
  -f batch-byte-compile \
  plane-org-sync-config.el \
  plane-org-sync-api.el \
  plane-org-sync-engine.el \
  plane-org-sync-org.el \
  plane-org-sync.el

# Run tests
emacs -batch -L . \
  -l ert \
  -l test/test-plane-org-sync-config.el \
  -l test/test-plane-org-sync-api.el \
  -l test/test-plane-org-sync-engine.el \
  -l test/test-plane-org-sync-org.el \
  -l test/test-plane-org-sync.el \
  -f ert-run-tests-batch-and-exit
```

No external dependencies are required. The package uses only built-in Emacs
libraries (`url.el`, `json.el`, `auth-source`, `org`).

### Running a Single Test File

```sh
emacs -batch -L . -l ert \
  -l test/test-plane-org-sync-api.el \
  -f ert-run-tests-batch-and-exit
```

### Running a Specific Test

```sh
emacs -batch -L . -l ert \
  -l test/test-plane-org-sync-api.el \
  --eval '(ert-run-tests-batch-and-exit "test-name-pattern")'
```

## Code Style

### General Conventions

- All files must use `lexical-binding: t`.
- Follow the [Emacs Lisp coding conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html).
- All public symbols use the `plane-org-sync-` prefix.
- Internal symbols use a double-dash prefix scoped to their module:
  `plane-org-sync-api--`, `plane-org-sync-org--`, `plane-org-sync-engine--`.
- All `defcustom` variables belong to the `plane-org-sync` customization group.

### Checkdoc

All public functions and variables must have docstrings that pass `checkdoc`.
Verify with:

```sh
emacs -batch -L . \
  --eval "(require 'checkdoc)" \
  --eval "(setq sentence-end-double-space nil)" \
  -f checkdoc-file plane-org-sync.el
```

### Byte-Compilation

Code must byte-compile with zero warnings when `byte-compile-error-on-warn` is
`t`. CI enforces this on every push and pull request.

### Source File Organization

| File | Responsibility |
|------|---------------|
| `plane-org-sync-config.el` | Custom variables, validation, auth-source lookup |
| `plane-org-sync-api.el` | HTTP client, Plane API endpoints, response parsing |
| `plane-org-sync-org.el` | Org buffer manipulation (read/write headings, properties) |
| `plane-org-sync-engine.el` | Sync logic: pull, push, conflict detection, state mapping |
| `plane-org-sync.el` | Entry point: commands, minor mode, capture, async orchestration |

### Tests

| Test File | Covers |
|-----------|--------|
| `test/test-plane-org-sync-config.el` | Configuration validation |
| `test/test-plane-org-sync-api.el` | API client, request/response handling |
| `test/test-plane-org-sync-org.el` | Org buffer read/write operations |
| `test/test-plane-org-sync-engine.el` | Sync logic, state mapping, conflict detection |
| `test/test-plane-org-sync.el` | Commands, minor mode, integration |

Tests use ERT (Emacs Lisp Regression Testing). All API tests mock HTTP calls;
no network access is required.

New features and bug fixes should include tests. Aim to test both the happy
path and relevant error/edge cases.

## Commit Messages

This project uses [Conventional Commits](https://www.conventionalcommits.org/).

### Format

```
type(scope): short description

Optional body explaining the change in more detail.

Optional footer (e.g., Closes #123).
```

### Types

| Type | When to Use |
|------|-------------|
| `feat` | New feature or capability |
| `fix` | Bug fix |
| `docs` | Documentation only |
| `refactor` | Code change that neither fixes a bug nor adds a feature |
| `test` | Adding or updating tests |
| `chore` | Build, CI, tooling, or dependency changes |

### Scope

Use the module name without the `plane-org-sync-` prefix: `api`, `org`,
`engine`, `config`. Omit scope for cross-cutting changes.

### Examples

```
feat(engine): add timestamp-based conflict detection on push
fix(api): handle null description_html in work item response
docs: add auth-source setup instructions to README
test(org): add tests for heading update with missing properties
chore: add Emacs 30.1 to CI matrix
```

## Pull Request Process

1. **Fork and branch.** Create a feature branch from `main`. Use a descriptive
   name: `feat/capture-template`, `fix/null-description`, etc.

2. **Keep changes focused.** One logical change per PR. If you find an
   unrelated issue while working, open a separate PR for it.

3. **Verify locally.** Before pushing, confirm:
   - Byte-compilation passes with zero warnings
   - All tests pass
   - New code has test coverage

4. **Open the PR.** Write a clear title and description. Reference any related
   issues.

5. **CI must pass.** The GitHub Actions workflow runs byte-compilation and tests
   on Emacs 29.1 and 30.1 across Ubuntu and macOS. All checks must be green.

6. **Review.** A maintainer will review your PR. Be responsive to feedback.
   Force-pushing to rebase on `main` is fine during review.

## Bug Reports

Open a [GitHub issue](https://github.com/martin/plane-org-sync/issues) with:

- **Emacs version**: output of `M-x emacs-version`
- **Package version**: git commit hash or tag
- **Plane edition**: Cloud or self-hosted (and version if self-hosted)
- **Steps to reproduce**: minimal configuration and actions to trigger the bug
- **Expected behavior**: what you expected to happen
- **Actual behavior**: what actually happened
- **Log output**: relevant content from `*plane-org-sync-log*` buffer

## Feature Requests

Open a GitHub issue with the `enhancement` label. Describe the use case (what
you are trying to accomplish) rather than just the solution you have in mind.

## License

By contributing, you agree that your contributions will be licensed under the
GNU General Public License v3.0 or later (GPL-3.0-or-later), the same license
as this project. See [COPYING](COPYING) for the full text.
