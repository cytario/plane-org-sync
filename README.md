# plane-org-sync

Synchronize [Plane.so](https://plane.so) work items with Emacs Org-mode.

<!-- Badges placeholder: CI, MELPA, license -->
<!-- [![MELPA](https://melpa.org/packages/plane-org-sync-badge.svg)](https://melpa.org/#/plane-org-sync) -->

plane-org-sync pulls your Plane work items into an Org file, pushes TODO state
changes back to Plane, and integrates with org-agenda and org-capture. Your
Plane tasks become first-class Org headings with scheduling, priorities, tags,
and metadata -- fully compatible with the Org ecosystem.

## Features

- **Pull work items** from Plane into Org headings with full metadata
- **Push state changes** back to Plane when you change a TODO keyword
- **Two-tier state mapping**: automatic group-based defaults with optional
  per-state overrides
- **Conflict detection** on push: warns when the remote item changed since last
  sync
- **Org-agenda integration**: synced items appear in your daily/weekly agenda
- **Org-capture integration**: create new Plane work items without leaving Emacs
- **Auto-sync mode**: periodic background sync via idle timer
- **Async HTTP**: pull operations do not block Emacs
- **Atomic file writes**: your Org file is never left in a corrupted state
- **Auth-source support**: store your API key in `.authinfo.gpg` or system
  keychain
- **Zero external dependencies**: pure Emacs Lisp, uses only built-in libraries
- **Self-hosted support**: works with Plane Cloud and self-hosted instances

## Requirements

- **Emacs 29.1** or later
- **Plane.so** account (Cloud or self-hosted) with an API key
- Network access (HTTPS) to your Plane instance

## Installation

### Manual

Clone the repository and add it to your load path:

```sh
git clone https://github.com/martin/plane-org-sync.git ~/.emacs.d/site-lisp/plane-org-sync
```

```elisp
(add-to-list 'load-path "~/.emacs.d/site-lisp/plane-org-sync")
(require 'plane-org-sync)
```

### use-package

```elisp
(use-package plane-org-sync
  :load-path "~/.emacs.d/site-lisp/plane-org-sync"
  :commands (plane-org-sync-pull
             plane-org-sync-setup
             plane-org-sync-browse
             plane-org-sync-status
             plane-org-sync-mode)
  :custom
  (plane-org-sync-instance-url "https://app.plane.so")
  (plane-org-sync-workspace "my-workspace")
  (plane-org-sync-projects '("project-uuid-here"))
  (plane-org-sync-file "~/org/plane.org")
  :config
  ;; Optional: enable auto-push on state change and periodic sync
  (plane-org-sync-mode 1))
```

### package-vc (Emacs 29+)

```elisp
(package-vc-install "https://github.com/martin/plane-org-sync.git")
```

### MELPA

Not yet available. MELPA submission is planned for a future release.

## Quick Start

### Option 1: Interactive Setup

Run the setup wizard to configure your connection:

```
M-x plane-org-sync-setup
```

The wizard walks you through:
1. Plane instance URL (default: `https://app.plane.so`)
2. Workspace slug (the slug portion of your Plane URL)
3. API key (enter directly or use auth-source)
4. Sync file path

After setup, you need to manually configure which projects to sync:

```elisp
(setq plane-org-sync-projects '("your-project-uuid"))
```

You can find project UUIDs in Plane's URL bar when viewing a project, or via
the Plane API.

Then pull your work items:

```
M-x plane-org-sync-pull
```

### Option 2: Manual Configuration

Add to your Emacs init file:

```elisp
(setq plane-org-sync-instance-url "https://app.plane.so")
(setq plane-org-sync-workspace "my-workspace")
(setq plane-org-sync-projects '("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"))
(setq plane-org-sync-file "~/org/plane.org")

;; API key: either set directly (not recommended) or use auth-source (see below)
;; (setq plane-org-sync-api-key "plane_api_xxxxxxxx")

;; Enable auto-push and optional periodic sync
(plane-org-sync-mode 1)
```

## Configuration

All settings are exposed as `defcustom` variables, configurable via
`M-x customize-group RET plane-org-sync` or in your init file.

### Connection Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `plane-org-sync-instance-url` | string | `"https://app.plane.so"` | Base URL of the Plane instance. Must use HTTPS. |
| `plane-org-sync-api-key` | string or nil | `nil` | API key. When nil, retrieved from auth-source. |
| `plane-org-sync-workspace` | string or nil | `nil` | Workspace slug (e.g., `"my-team"`). |

### Project Settings

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `plane-org-sync-projects` | list of strings | `nil` | List of project UUIDs to synchronize. |
| `plane-org-sync-file` | file path | `"~/org/plane.org"` | Path to the Org file for synced work items. Created if absent. |
| `plane-org-sync-filter-assignee` | boolean | `t` | When non-nil, sync only work items assigned to you. |
| `plane-org-sync-auto-interval` | integer or nil | `nil` | Auto-sync interval in seconds. Nil disables auto-sync. |

### State Mapping

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `plane-org-sync-group-keyword-mapping` | alist | See below | Maps Plane state groups to Org TODO keywords. |
| `plane-org-sync-state-mapping` | alist | `nil` | Optional per-state name overrides. |

### Other

| Variable | Type | Default | Description |
|----------|------|---------|-------------|
| `plane-org-sync-conflict-function` | function | `#'y-or-n-p` | Function called to resolve push conflicts. Receives a prompt string, returns non-nil to proceed. |

## Usage

### Pull Sync

Fetch work items from Plane and create/update Org headings:

```
M-x plane-org-sync-pull
```

The command:
1. Fetches your user ID from the Plane API
2. Fetches work items from all configured projects (in parallel)
3. Fetches workflow states for state mapping
4. Creates new headings for items not yet in the Org file
5. Updates existing headings where the remote `updated_at` timestamp has changed
6. Saves the file atomically
7. Reports results: "Plane: synced 15 items (3 new, 5 updated)"

Draft and archived items are automatically excluded.

### State Push

When `plane-org-sync-mode` is active and you change a TODO keyword on a synced
heading, the new state is pushed to Plane automatically.

For example, changing `TODO` to `DONE` on a synced heading sends a PATCH
request to update the work item's state in Plane.

The push is synchronous (blocks briefly) to ensure the state update completes
before you continue editing. If the push fails, the TODO keyword is
automatically reverted to its previous value.

### Auto Sync

Enable `plane-org-sync-mode` to activate:
- Automatic state push on TODO keyword changes
- Periodic background sync (when `plane-org-sync-auto-interval` is set)
- Sync file added to `org-agenda-files`

```elisp
;; Enable the mode
(plane-org-sync-mode 1)

;; Optional: auto-sync every 5 minutes
(setq plane-org-sync-auto-interval 300)
```

The auto-sync timer uses a non-repeating idle timer that re-arms after each
sync completes, preventing overlapping syncs.

### Capture New Work Items

Add the capture template to `org-capture-templates`:

```elisp
(require 'plane-org-sync)
(push (plane-org-sync-capture-template) org-capture-templates)

;; Or with a custom key binding:
(push (plane-org-sync-capture-template "p") org-capture-templates)
```

Then use `C-c c P` (or your chosen key) to create a new work item. The template
prompts for a title, creates the item in Plane via the API, and sets all
`PLANE_*` properties on the resulting Org heading.

If you have multiple projects configured, you are prompted to select the target
project.

### Browse Work Item in Plane

Open the Plane web UI for the heading at point:

```
M-x plane-org-sync-browse
```

This opens the `PLANE_URL` property in your default web browser. You can also
use `C-c C-o` on the link line in the heading body.

### Status and Reset

Check sync status:

```
M-x plane-org-sync-status
```

Displays last sync time, result counts, cached project count, and user ID.

Clear all caches and internal state:

```
M-x plane-org-sync-reset
```

Useful when you change project configuration or encounter stale state.

## State Mapping

plane-org-sync uses a two-tier system to map Plane workflow states to Org TODO
keywords.

### Tier 1: Group Defaults (automatic)

Plane organizes states into five fixed groups. The default mapping is:

| Plane State Group | Org TODO Keyword | Done State? |
|-------------------|-----------------|-------------|
| `backlog`         | `BACKLOG`       | No          |
| `unstarted`       | `TODO`          | No          |
| `started`         | `STARTED`       | No          |
| `completed`       | `DONE`          | Yes         |
| `cancelled`       | `CANCELLED`     | Yes         |

This works out of the box with no configuration. All Plane states are mapped
through their group. For example, if your project has states "In Progress",
"In Review", and "QA" -- all in the `started` group -- they all map to
`STARTED`.

To change the group defaults:

```elisp
(setq plane-org-sync-group-keyword-mapping
      '((backlog   . "TODO")
        (unstarted . "TODO")
        (started   . "IN-PROGRESS")
        (completed . "DONE")
        (cancelled . "OBSOLETE")))
```

### Tier 2: Per-State Overrides (optional)

If you need different Org keywords for specific Plane states within the same
group, add explicit overrides:

```elisp
(setq plane-org-sync-state-mapping
      '(("In Review" . "REVIEW")
        ("QA"        . "TESTING")))
```

States listed here take priority over the group default. States not listed fall
back to Tier 1.

### Reverse Mapping (Push)

When you change a TODO keyword in Org, plane-org-sync resolves it back to a
Plane state:

1. If the keyword appears in `plane-org-sync-state-mapping`, the corresponding
   Plane state name is looked up and its UUID is used.
2. Otherwise, the keyword is mapped to a state group via the inverse of
   `plane-org-sync-group-keyword-mapping`, and the first state in that group
   (by Plane's sequence order) is selected.
3. If the heading's current state already belongs to the target group, no
   change is pushed (the states are considered compatible).

### Generated #+TODO Line

plane-org-sync automatically generates and maintains a `#+TODO:` line at the top
of the sync file that declares all keywords from the mapping:

```org
#+TODO: BACKLOG TODO STARTED | DONE CANCELLED  # plane-org-sync-managed
```

This line is updated on each pull sync. Other `#+TODO:` lines in the file are
left unchanged.

## Auth-source

The recommended way to store your API key is via Emacs auth-source, which
supports encrypted storage.

### ~/.authinfo.gpg

Create or edit `~/.authinfo.gpg` (GPG-encrypted) with:

```
machine app.plane.so password plane_api_xxxxxxxxxxxxxxxx
```

For self-hosted instances, replace `app.plane.so` with your instance hostname.

Then set `plane-org-sync-api-key` to nil (the default) so the key is retrieved
from auth-source:

```elisp
(setq plane-org-sync-api-key nil)  ; default -- uses auth-source
```

### macOS Keychain

If you use `auth-source-macos-keychain`, add an entry with:
- Account: (any)
- Server: `app.plane.so`
- Password: your API key

### Direct Configuration (not recommended)

You can set the key directly, but it will be stored in plaintext:

```elisp
(setq plane-org-sync-api-key "plane_api_xxxxxxxxxxxxxxxx")
```

## Org File Format

After a pull sync, the sync file contains headings like this:

```org
#+TODO: BACKLOG TODO STARTED | DONE CANCELLED  # plane-org-sync-managed

* TODO [#B] Implement user authentication  :backend:auth:
SCHEDULED: <2026-02-20> DEADLINE: <2026-02-28>
:PROPERTIES:
:PLANE_ID: 01234567-89ab-cdef-0123-456789abcdef
:PLANE_URL: https://app.plane.so/my-team/projects/abc123/work-items/42
:PLANE_UPDATED_AT: 2026-02-19T10:30:00.000000+00:00
:PLANE_STATE: Todo
:PLANE_STATE_ID: abcdef01-2345-6789-abcd-ef0123456789
:PLANE_PROJECT_ID: fedcba98-7654-3210-fedc-ba9876543210
:PLANE_PROJECT: MYPROJ
:PLANE_PRIORITY: medium
:PLANE_ASSIGNEES: Jane Doe
:CATEGORY: MYPROJ
:END:
[[https://app.plane.so/my-team/projects/abc123/work-items/42][MYPROJ-42]]
# plane-org-sync-description-begin
Implement OAuth2 authentication for the user login flow.

- Support Google and GitHub providers
- Add session management
- Write integration tests
# plane-org-sync-description-end

** My local notes
These are preserved across syncs. Anything after the description-end
sentinel or in sub-headings is never touched by the sync engine.
```

Key points:
- Each work item is a top-level (`*`) heading
- `PLANE_*` properties in the `:PROPERTIES:` drawer track sync metadata
- `CATEGORY` is set to the project identifier for agenda display
- The Plane link line provides one-click navigation to the web UI
- Description is bracketed by sentinel comments (`# plane-org-sync-description-begin/end`)
- Content outside the sentinel region (sub-headings, personal notes) is preserved
- Labels from Plane become Org tags (sanitized: lowercase, special characters
  replaced with underscores)
- `SCHEDULED` comes from Plane's `start_date`, `DEADLINE` from `target_date`

### Priority Mapping

| Plane Priority | Org Priority |
|---------------|-------------|
| urgent        | `[#A]`      |
| high          | `[#A]`      |
| medium        | `[#B]`      |
| low           | `[#C]`      |
| none          | (no cookie) |

## Troubleshooting

### "No projects configured"

You need to set `plane-org-sync-projects` to a list of project UUIDs:

```elisp
(setq plane-org-sync-projects '("xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx"))
```

Find project UUIDs in the Plane URL when viewing a project page.

### HTTP errors during sync

Check the log buffer for details:

```
M-x switch-to-buffer RET *plane-org-sync-log*
```

Common causes:
- **401 Unauthorized**: API key is invalid or expired. Regenerate at
  Plane Settings > API Tokens.
- **404 Not Found**: Workspace slug or project UUID is incorrect.
- **429 Too Many Requests**: Rate limit exceeded. The sync retries
  automatically with exponential backoff.
- **Network errors**: Check your internet connection and that the Plane
  instance URL is reachable.

### "Sync file has unsaved modifications"

Save the sync file (`C-x C-s`) before running `plane-org-sync-pull`. The sync
engine refuses to modify a buffer with unsaved changes to prevent data loss.

### State change push fails / keyword reverts

When a TODO state change fails to push to Plane, the keyword is automatically
reverted to its previous value. Check `*plane-org-sync-log*` for the error.

Common causes:
- Network timeout (5-second limit for push operations)
- Invalid state transition (Plane workflow constraints)
- API key lacks write permissions

### "Remote item modified since last sync"

This conflict prompt appears when someone else modified the work item in Plane
after your last sync. You can:
- Press `y` to push your state change anyway (overwrites the remote state)
- Press `n` to cancel (your local keyword reverts to the previous value)

Run `plane-org-sync-pull` to get the latest state before retrying.

### State mapping confusion

If TODO keywords do not match what you expect:
1. Check the generated `#+TODO:` line at the top of the sync file
2. Review `plane-org-sync-group-keyword-mapping` for group defaults
3. Review `plane-org-sync-state-mapping` for per-state overrides
4. Use `M-x plane-org-sync-status` to verify the connection

### Heading not updating on pull

If a heading exists but is not updated, the `updated_at` timestamps may match
(the item has not changed in Plane). Use `M-x plane-org-sync-reset` followed
by `M-x plane-org-sync-pull` to clear the cache and re-fetch.

## Contributing

### Running Tests

```sh
emacs -batch -L . -L test -l ert -l test/test-plane-org-sync-config.el \
  -l test/test-plane-org-sync-api.el \
  -l test/test-plane-org-sync-org.el \
  -l test/test-plane-org-sync-engine.el \
  -f ert-run-tests-batch-and-exit
```

### Byte-Compilation

Verify clean byte-compilation with no warnings:

```sh
emacs -batch -L . --eval '(batch-byte-compile)' \
  plane-org-sync-config.el \
  plane-org-sync-api.el \
  plane-org-sync-org.el \
  plane-org-sync-engine.el \
  plane-org-sync.el
```

### Code Style

- Follow the [Emacs Lisp coding conventions](https://www.gnu.org/software/emacs/manual/html_node/elisp/Coding-Conventions.html)
- Use `lexical-binding: t` in all files
- Public functions use the `plane-org-sync-` prefix
- Internal functions use `plane-org-sync-MODULE--` double-dash prefix
- All `defcustom` variables belong to the `plane-org-sync` group

## License

GPL-3.0-or-later. See [COPYING](COPYING) for the full license text.
