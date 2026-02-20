# Software Design Specification (SDS)

**Product**: plane-org-sync (PS)
**Version**: 1.0 Draft
**System Type**: Non-regulated Software
**Date**: 2026-02-19

---

## 1. Introduction

### 1.1 Purpose

This document describes the software architecture and detailed design of
plane-org-sync, translating the software requirements (SRS) into a concrete
implementation structure.

### 1.2 Scope

plane-org-sync is a single Emacs Lisp package consisting of multiple source
files organized into sub-systems. This document decomposes the package into its
constituent components, describes their interfaces, and documents key design
decisions.

### 1.3 Referenced Documents

| Document | Description |
|----------|-------------|
| SRS-PS v1.0 | Software Requirements Specification for plane-org-sync |
| URS-PS v1.0 | User Requirements Specification for plane-org-sync |
| [Plane API Reference](https://developers.plane.so/api-reference/) | REST API documentation |

### 1.4 Definitions and Abbreviations

See URS and SRS. Additional terms:

| Term | Definition |
|------|------------|
| plist | Emacs Lisp property list, used as lightweight key-value structure |
| alist | Emacs Lisp association list |
| defcustom | Emacs macro for declaring user-customizable variables |
| url.el | Built-in Emacs HTTP client library |

---

## 2. Solution Strategy

| Quality Goal | Scenario | Solution Approach | Reference |
|-------------|----------|-------------------|-----------|
| Non-blocking UI | Sync of 200 items must not freeze Emacs | Async HTTP via `url-retrieve` with callback chains | Section 3.1 |
| Data integrity | Org file must never be corrupted by interrupted sync | Collect all changes in memory, write atomically | Section 3.3 |
| Configurability | Power users customize all behavior | All settings via `defcustom`, hooks for extensibility | Section 3.4 |
| Minimal dependencies | No external binaries, pure elisp | Use built-in `url.el`, `json.el`, `auth-source` | Section 7.1 |
| API forward-compat | Plane is migrating `/issues/` to `/work-items/` | Use `/work-items/` endpoints exclusively | Section 3.1 |

---

## 3. Building Block View

### Level 1: System Decomposition

```
┌─────────────────────────────────────────────────────┐
│                  plane-org-sync                      │
│                                                     │
│  ┌──────────┐  ┌──────────┐  ┌──────────────────┐  │
│  │  API      │  │  Sync    │  │  Org             │  │
│  │  Client   │  │  Engine  │  │  Interface       │  │
│  │  (3.1)    │◄─┤  (3.2)   │─►│  (3.3)           │  │
│  │          │  │          │  │                  │  │
│  └─────┬────┘  └────┬─────┘  └──────────────────┘  │
│        │            │                               │
│        │       ┌────┴─────┐                         │
│        │       │  Config  │                         │
│        └───────┤  (3.4)   │                         │
│                │          │                         │
│                └──────────┘                         │
│                                                     │
│  ┌──────────────────────────────────────────────┐   │
│  │  Commands & Modes (3.5)                      │   │
│  │  (interactive entry points, minor mode,      │   │
│  │   capture template, hooks)                   │   │
│  └──────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────┘
```

### 3.1 Sub-System: API Client (`plane-org-sync-api.el`)

**Purpose / Responsibility**: Handles all HTTP communication with the Plane.so
REST API. Provides an async, callback-based interface for CRUD operations on
work items, states, labels, and projects. Encapsulates authentication, rate
limiting, pagination, and error handling.

**Interface:**

- **Structural IO**: HTTPS/TLS to Plane instance
- **Syntactic IO**: JSON request/response bodies
- **Semantic IO**: Plane work item objects, state objects, label objects
- **Organizational IO**: API key authentication via `X-API-Key` header

**Level 2 Components:**

#### 3.1.1 Component: HTTP Layer (`plane-org-sync-api--request`)

**Purpose**: Low-level async HTTP request function wrapping `url-retrieve`.

**[SDS-PS-010101] Async HTTP Request**
The function `plane-org-sync-api--request` accepts method, path, optional body,
and a callback. It constructs the full URL from `plane-org-sync-instance-url`,
sets authentication and content-type headers, performs the request
asynchronously, parses the JSON response, and calls the callback with
`(status-code parsed-body)` or `(error error-message)`.
Refs: SRS-PS-310201, SRS-PS-520002

**[SDS-PS-010102] Rate Limit Handling**
On HTTP 429 response, the function retries after an exponential backoff delay
(2s, 4s, 8s) up to 3 retries. The retry count is tracked per request, not
globally. After max retries, the error callback is invoked.
Refs: SRS-PS-510002

**[SDS-PS-010103] Pagination Handler**
The function `plane-org-sync-api--paginate` wraps `plane-org-sync-api--request`
to follow pagination. It accumulates `results` arrays across pages and invokes
the final callback with the complete list.
Refs: SRS-PS-510001

#### 3.1.2 Component: Resource Functions

**Purpose**: High-level functions for specific Plane resources.

**[SDS-PS-010201] Fetch Work Items**
`plane-org-sync-api-list-work-items (project-id &optional params callback)`
Fetches work items for a project with optional query params (assignees, state,
expand). Uses pagination. Returns list of work item plists.

The function first attempts server-side filtering via the `?assignees=` query
parameter. If the API returns an error indicating the parameter is not
supported on `/work-items/`, the function retries without the filter and
performs client-side filtering by comparing each item's assignee UUIDs against
the authenticated user's ID.
Refs: SRS-PS-310201

**[SDS-PS-010202] Update Work Item State**
`plane-org-sync-api-update-work-item (project-id item-id state-id callback)`
Sends PATCH to update a work item's state field.
Refs: SRS-PS-310203

**[SDS-PS-010203] Create Work Item**
`plane-org-sync-api-create-work-item (project-id data callback)`
Sends POST to create a new work item. `data` is a plist with name, state,
priority, labels, assignees.
Refs: SRS-PS-310301

**[SDS-PS-010204] Fetch States**
`plane-org-sync-api-list-states (project-id callback)`
Fetches workflow states for a project. Used for state mapping validation and
the setup wizard.
Refs: SRS-PS-310105

**[SDS-PS-010205] Fetch Current User**
`plane-org-sync-api-me (callback)`
Fetches the authenticated user's ID. Used for assignee filtering.
Refs: SRS-PS-310107

**Design Note: `?expand=` Parameter Availability**
The design assumes `?expand=state,labels,assignees` is supported on the
`/work-items/` endpoint to inline related objects and avoid separate lookups.
If `?expand=` is not supported on `/work-items/`, the system pre-fetches and
caches states, labels, and user data per project at the start of each sync
cycle. These lookup tables are cached in memory for the duration of the sync
and discarded afterward.

### 3.2 Sub-System: Sync Engine (`plane-org-sync-engine.el`)

**Purpose / Responsibility**: Orchestrates the sync process. Compares remote
Plane state with local Org state, computes a diff, and produces a list of
operations (create heading, update heading, push state change).

**Interface:**

- **Input**: List of Plane work item plists (from API Client) + list of
  existing Org heading data (from Org Interface)
- **Output**: List of sync operations to apply

**Level 2 Components:**

#### 3.2.1 Component: Diff Calculator

**Purpose**: Compares remote and local state to determine required operations.

**[SDS-PS-020101] Compute Sync Diff**
The function `plane-org-sync-engine--diff` accepts a list of remote work items
and a list of local heading records (extracted from the sync file). For each
remote item, it:
1. Looks for a local heading with matching `PLANE_ID`
2. If not found -> emit `create` operation
3. If found and remote `updated_at` > local `PLANE_UPDATED_AT` -> emit `update` operation
4. If found and timestamps match -> emit `unchanged` (no-op)

After processing all remote items, the function identifies local headings
whose `PLANE_ID` does not appear in the remote result set and emits
`unchanged-orphan` for each. These headings are left completely unmodified
(per SRS-PS-420108).

Returns an alist of `((create . items) (update . items) (unchanged . items) (unchanged-orphan . items))`.
Refs: SRS-PS-420102, SRS-PS-420108

#### 3.2.2 Component: State Resolver

**Purpose**: Translates between Plane state UUIDs/names and Org TODO keywords
using a two-tier resolution: explicit overrides take priority, then group-based
defaults.

**[SDS-PS-020201] Resolve Plane State to Org Keyword**
`plane-org-sync-engine--state-to-keyword (state-name state-group)`
First checks `plane-org-sync-state-mapping` for an explicit override matching
`state-name`. If found, returns the mapped keyword. Otherwise, looks up
`plane-org-sync-group-keyword-mapping` by `state-group` and returns the group's
default keyword.
Refs: SRS-PS-310105

**[SDS-PS-020202] Resolve Org Keyword to Plane State**
`plane-org-sync-engine--keyword-to-state (keyword project-id current-state-id)`
Implements the reverse resolution algorithm from SRS-PS-310108:
1. Check `plane-org-sync-state-mapping` for an entry whose value matches
   `keyword`. If found, look up the Plane state name in the project's state
   list and return its UUID.
2. Otherwise, resolve `keyword` to a state group via the inverse of
   `plane-org-sync-group-keyword-mapping`. Select the first state (by API
   `sequence` order) in that group for the given project.
3. If `current-state-id` already belongs to the target group, return nil
   (no change needed — states are compatible).
4. If no matching state is found, warn the user and return nil.

Accepts the heading's current `PLANE_STATE_ID` to support the "already in
target group" optimization that avoids unnecessary API calls.
Refs: SRS-PS-310108, SRS-PS-310203

#### 3.2.3 Component: Conflict Detector

**Purpose**: Identifies work items modified on both sides since last sync.

**[SDS-PS-020301] Detect Conflicts**
When pushing a local state change, the engine fetches the single work item from
the API and compares `updated_at` with the stored `PLANE_UPDATED_AT`. If remote
is newer, the operation is flagged as a conflict.
Refs: SRS-PS-310204

### 3.3 Sub-System: Org Interface (`plane-org-sync-org.el`)

**Purpose / Responsibility**: Reads from and writes to Org files. Extracts
heading data from existing sync files and applies sync operations (create/update
headings) to the buffer.

**Interface:**

- **Input**: Sync operations from the engine, file path from config
- **Output**: Modified Org buffer contents, saved atomically to disk

**Level 2 Components:**

#### 3.3.1 Component: Org Reader

**Purpose**: Parses the sync file to extract heading metadata.

**[SDS-PS-030101] Extract Heading Records**
`plane-org-sync-org--read-headings (file)`
Opens the sync file (or creates it if absent), iterates over top-level headings
using `org-map-entries` with `"LEVEL=1"` match scope, and extracts for each:
title, TODO keyword, priority, tags, PLANE_ID, PLANE_UPDATED_AT, and a point
marker (via `point-marker`). Returns a list of heading records (plists).

Point markers (not integer positions) are stored so they remain valid during
subsequent buffer modifications in the same sync cycle. `org-map-entries` is
used instead of `org-element-map` because the sync cycle performs mixed
read-write operations on the buffer.
Refs: SRS-PS-420102

#### 3.3.2 Component: Org Writer

**Purpose**: Applies sync operations to the Org buffer.

**[SDS-PS-030201] Create New Heading**
`plane-org-sync-org--insert-heading (work-item)`
Appends a new top-level heading at the end of the sync file buffer with:
- Headline: `{keyword} {priority} {title} {tags}`
- SCHEDULED/DEADLINE from start_date/target_date
- PROPERTIES drawer with all PLANE_* metadata
- Org hyperlink to the Plane work item (e.g., `[[url][PROJ-42]]`)
- Description body (converted from HTML via the HTML-to-Org converter, Section 3.3.4)

Refs: SRS-PS-420101, SRS-PS-310401

**[SDS-PS-030202] Update Existing Heading**
`plane-org-sync-org--update-heading (heading-record work-item)`
Navigates to the heading's buffer position (via point marker) and updates:
- Headline text (title, keyword, priority, tags)
- SCHEDULED/DEADLINE timestamps
- PROPERTIES drawer values
- Plane hyperlink line (first body line, before description)
- First body paragraph after the link (description)

Sub-headings and content after the description paragraph are not modified.

Updates are processed in reverse buffer order (bottom-to-top) to prevent
earlier modifications from invalidating later heading positions. When using
point markers this is a safety measure rather than a strict requirement, but
it avoids subtle issues with marker relocation.
Refs: SRS-PS-420103, SRS-PS-310401

**[SDS-PS-030203] Atomic File Save**
`plane-org-sync-org--save-atomic (buffer file)`

All sync modifications are made in the buffer visiting the sync file (obtained
via `org-find-base-buffer-visiting` or `find-file-noselect`). After all
modifications are complete:

1. Write the buffer contents to a temporary file in the same directory
   (`{file}.tmp~`)
2. Rename the temporary file to the target path (atomic on POSIX)
3. Update the visiting buffer's state: call `revert-buffer` with
   `ignore-auto=t` and `noconfirm=t` to re-sync the buffer with the file on
   disk, or equivalently use `set-visited-file-modtime` to update the buffer's
   recorded modification time so Emacs does not prompt "file changed on disk"

If the buffer has unsaved user modifications outside the sync regions (e.g.,
the user edited a sub-heading while sync was running), the system detects this
via `buffer-modified-p` before writing and aborts with a warning: "Sync file
has unsaved modifications. Save the file first, then re-run sync."

If the rename fails, the original file is untouched, the temporary file is
cleaned up via `condition-case`, and an error is signaled.
Refs: SRS-PS-420104

#### 3.3.3 Component: Priority Mapper

**[SDS-PS-030301] Map Priority Values**
`plane-org-sync-org--priority-to-org (plane-priority)`
Converts Plane priority strings to Org priority characters per the mapping table
in SRS-PS-420201 (urgent/high -> `?A`, medium -> `?B`, low -> `?C`, none -> nil).
Refs: SRS-PS-420201, SRS-PS-420101

#### 3.3.4 Component: HTML-to-Org Converter

**Purpose**: Converts Plane's `description_html` (Tiptap/ProseMirror HTML) to
Org markup for insertion as heading body text.

**[SDS-PS-030401] Convert HTML Description to Org Markup**
`plane-org-sync-org--html-to-org (html-string)`
Accepts an HTML string (or nil) and returns an Org markup string (or nil).

The conversion uses a sequence of regex-based transformations applied in a
defined order (block elements first, then inline elements, then cleanup). The
conversion table from SRS-PS-420107 defines all supported mappings. Nested
structures (e.g., lists inside blockquotes) are handled on a best-effort basis.

When `html-string` is nil or empty, returns nil. The caller (insert/update
heading) is responsible for omitting the description paragraph when nil is
returned and removing any previously existing description paragraph per
SRS-PS-420103.

HTML `<table>` elements are passed through as-is (v1 limitation). All
unrecognized HTML tags are stripped, preserving their text content.
Refs: SRS-PS-420107, SRS-PS-420103

### 3.4 Sub-System: Config (`plane-org-sync-config.el`)

**Purpose / Responsibility**: Declares all user-facing customization variables
and provides the interactive setup wizard.

**[SDS-PS-040001] Customization Group**
All defcustom variables are declared under the `plane-org-sync` customization
group.
Refs: SRS-PS-310101 through SRS-PS-310108, URS-PS-30103

**[SDS-PS-040002] Setup Wizard**
`plane-org-sync-setup` is an interactive command that walks the user through
configuration:
1. Prompt for instance URL (default: api.plane.so, the API base URL)
2. Prompt for API key (with auth-source hint)
3. Prompt for workspace slug (user copies from their Plane browser URL, e.g.,
   `https://app.plane.so/{workspace-slug}/`). The Plane API does not provide
   a workspace enumeration endpoint accessible with a standard API key.

Note: `plane-org-sync-instance-url` is the API base URL (default:
`https://api.plane.so` for Plane Cloud), which is distinct from the browser
URL (`https://app.plane.so`). Self-hosted instances typically use the same
host for both API and browser access.
4. Fetch and select projects (at least one must be selected)
5. Fetch states for selected projects, display the auto-derived mapping for
   confirmation (e.g., "Backlog [backlog] → TODO, In Progress [started] →
   STARTED, Done [completed] → DONE"). If the user accepts, no
   `plane-org-sync-state-mapping` is written (group defaults suffice). If the
   user wants overrides, present each state for keyword assignment.
6. Confirm sync file path
7. Write configuration to `custom-set-variables`

Refs: SRS-PS-310103, SRS-PS-310104, SRS-PS-310105

### 3.5 Sub-System: Commands & Modes (`plane-org-sync.el`)

**Purpose / Responsibility**: Top-level entry point. Defines the minor mode,
interactive commands, org-capture integration, and hooks. Wires together the
other sub-systems.

**[SDS-PS-050001] Minor Mode**
`plane-org-sync-mode` is a global minor mode that:
- Adds `org-after-todo-state-change-hook` for bidirectional state push
- Starts the auto-sync timer (if configured)
- Adds the sync file to `org-agenda-files` (if not already present)
- During pull sync, sets the `:CATEGORY:` property on each synced heading to
  the project identifier (e.g., "PROJ") so that `org-agenda` displays project
  context in its views

Refs: SRS-PS-310202, SRS-PS-310203, SRS-PS-420101

**[SDS-PS-050002] Capture Template**
`plane-org-sync-capture-template` returns a list suitable for adding to
`org-capture-templates`. It uses a custom `:before-finalize` function to
POST the new work item to Plane and populate the heading's PLANE_* properties.

The capture POST uses `url-retrieve-synchronously` because the
`:before-finalize` hook runs synchronously and requires the API response (item
ID, `sequence_id`) to populate `PLANE_*` properties before capture
finalization. This brief block (typically <1s for a single POST) is acceptable
for single-item creation.
Refs: SRS-PS-310301

**[SDS-PS-050003] Hook: State Change Push**
The function added to `org-after-todo-state-change-hook` first checks the
dynamic variable `plane-org-sync--inhibit-push`. If bound to `t`, the hook
returns immediately (suppressed during pull sync). Otherwise, it checks if the
current heading has a `PLANE_ID` property. If yes, it invokes the sync
engine's state push flow (conflict check -> API update -> metadata update). If
no, it returns immediately (no-op).
Refs: SRS-PS-310203, SRS-PS-310204, SRS-PS-310206

**[SDS-PS-050004] Pull Sync Push Suppression**
During pull sync, all `org-todo` calls that update heading keywords are wrapped
in a dynamic binding:

```elisp
(let ((plane-org-sync--inhibit-push t))
  (org-todo keyword))
```

This prevents the `org-after-todo-state-change-hook` from triggering a push
back to Plane for state changes that originated from Plane. The variable
`plane-org-sync--inhibit-push` defaults to nil and is only bound to `t` during
the pull sync's Org buffer modification phase.
Refs: SRS-PS-310206

---

## 4. Runtime View

### 4.1 Pull Sync Scenario

```
User                Commands (3.5)        API Client (3.1)      Engine (3.2)       Org Interface (3.3)
 │                      │                       │                    │                     │
 │  M-x plane-org-      │                       │                    │                     │
 │  sync-pull           │                       │                    │                     │
 │─────────────────────►│                       │                    │                     │
 │                      │  api-me               │                    │                     │
 │                      │──────────────────────►│                    │                     │
 │                      │         user-id       │                    │                     │
 │                      │◄──────────────────────│                    │                     │
 │                      │                       │                    │                     │
 │                      │  For each project:    │                    │                     │
 │                      │  api-list-work-items  │                    │                     │
 │                      │  (assignee=user-id)   │                    │                     │
 │                      │──────────────────────►│                    │                     │
 │                      │       work-items[]    │                    │                     │
 │                      │◄──────────────────────│                    │                     │
 │                      │                       │                    │                     │
 │                      │                       │  read-headings     │                     │
 │                      │                       │                    │────────────────────►│
 │                      │                       │                    │    heading-records  │
 │                      │                       │                    │◄────────────────────│
 │                      │                       │                    │                     │
 │                      │  engine--diff(remote, local)               │                     │
 │                      │───────────────────────────────────────────►│                     │
 │                      │              {create: [], update: []}      │                     │
 │                      │◄───────────────────────────────────────────│                     │
 │                      │                       │                    │                     │
 │                      │  For each create/update:                   │                     │
 │                      │  org--insert or org--update                │                     │
 │                      │────────────────────────────────────────────────────────────────►│
 │                      │                       │                    │                     │
 │                      │  org--save-atomic      │                    │                     │
 │                      │────────────────────────────────────────────────────────────────►│
 │                      │                       │                    │                     │
 │  "Synced: 3 new,     │                       │                    │                     │
 │   5 updated"         │                       │                    │                     │
 │◄─────────────────────│                       │                    │                     │
```

### 4.2 State Push Scenario (Bidirectional)

```
User                    Hook (3.5)          Engine (3.2)        API Client (3.1)
 │                         │                     │                    │
 │  Change TODO→DONE       │                     │                    │
 │  on synced heading      │                     │                    │
 │────────────────────────►│                     │                    │
 │                         │                     │                    │
 │                         │  Has PLANE_ID?      │                    │
 │                         │  Yes                │                    │
 │                         │                     │                    │
 │                         │  keyword-to-state   │                    │
 │                         │  ("DONE")           │                    │
 │                         │────────────────────►│                    │
 │                         │    state-uuid       │                    │
 │                         │◄────────────────────│                    │
 │                         │                     │                    │
 │                         │  Conflict check:    │                    │
 │                         │  api-get-work-item  │                    │
 │                         │─────────────────────────────────────────►│
 │                         │         item.updated_at                  │
 │                         │◄─────────────────────────────────────────│
 │                         │                     │                    │
 │                         │  Compare timestamps │                    │
 │                         │  No conflict        │                    │
 │                         │                     │                    │
 │                         │  api-update-work-item                    │
 │                         │  (state=state-uuid) │                    │
 │                         │─────────────────────────────────────────►│
 │                         │         200 OK      │                    │
 │                         │◄─────────────────────────────────────────│
 │                         │                     │                    │
 │                         │  Update PLANE_UPDATED_AT                 │
 │                         │  on heading         │                    │
 │                         │                     │                    │
 │  "Plane updated: DONE"  │                     │                    │
 │◄────────────────────────│                     │                    │
```

### 4.3 Capture → Create Scenario

```
User                Commands (3.5)          API Client (3.1)     Org Interface (3.3)
 │                      │                        │                     │
 │  C-c c → capture     │                        │                     │
 │  template            │                        │                     │
 │─────────────────────►│                        │                     │
 │                      │                        │                     │
 │  Select project      │                        │                     │
 │◄─────────────────────│                        │                     │
 │  "my-project"        │                        │                     │
 │─────────────────────►│                        │                     │
 │                      │                        │                     │
 │  Enter title,        │                        │                     │
 │  priority, labels    │                        │                     │
 │◄────────────────────►│                        │                     │
 │                      │                        │                     │
 │                      │  :before-finalize      │                     │
 │                      │  api-create-work-item  │                     │
 │                      │───────────────────────►│                     │
 │                      │      {id, url, ...}    │                     │
 │                      │◄───────────────────────│                     │
 │                      │                        │                     │
 │                      │  Set PLANE_* props on  │                     │
 │                      │  captured heading      │                     │
 │                      │────────────────────────────────────────────►│
 │                      │                        │                     │
 │  Heading created     │                        │                     │
 │  with Plane link     │                        │                     │
 │◄─────────────────────│                        │                     │
```

---

## 5. Deployment View

```
┌─────────────────────────────────────────────────────┐
│  User's Machine                                     │
│                                                     │
│  ┌─────────────────────────────────────────────┐    │
│  │  Emacs (>= 29.1)                            │    │
│  │                                             │    │
│  │  ~/.emacs.d/elpa/plane-org-sync/            │    │
│  │    ├── plane-org-sync.el          (3.5)     │    │
│  │    ├── plane-org-sync-api.el      (3.1)     │    │
│  │    ├── plane-org-sync-engine.el   (3.2)     │    │
│  │    ├── plane-org-sync-org.el      (3.3)     │    │
│  │    └── plane-org-sync-config.el   (3.4)     │    │
│  │                                             │    │
│  │  ~/Dropbox/org/plane.org  ← sync file       │    │
│  └─────────────────────────────────────────────┘    │
│           │                                         │
│           │ HTTPS                                   │
└───────────┼─────────────────────────────────────────┘
            │
            ▼
┌───────────────────────┐
│  Plane.so Instance    │
│  (cloud or self-host) │
│  /api/v1/...          │
└───────────────────────┘
```

**Deployable Artifact**: 5 elisp files distributed as a single MELPA package
or via package-vc from a git repository.

---

## 6. Crosscutting Concepts

### 6.1 Async Callback Pattern

All API calls use `url-retrieve` with continuation-passing style callbacks.
To avoid callback hell for multi-step operations (e.g., fetch user → fetch
projects → fetch items), a simple async chain utility
`plane-org-sync--chain` sequences callbacks:

```elisp
(plane-org-sync--chain
  (lambda (next) (plane-org-sync-api-me next))
  (lambda (next user) (plane-org-sync-api-list-work-items project user next))
  (lambda (_next items) (plane-org-sync-engine--apply items)))
```

### 6.2 Labeling

The package version is declared in the main `plane-org-sync.el` file header:
```elisp
;; Version: 1.0.0
```
Users can check it with `M-x package-describe plane-org-sync` or
`(pkg-info-version-info 'plane-org-sync)`.

### 6.3 Error Handling

All API errors are caught in the HTTP layer and:
1. Logged to `*plane-org-sync-log*` buffer with timestamp, endpoint, and response
2. Surfaced to the user via `message` with a one-line summary
3. Never propagated as unhandled signals (which would break idle timers)

### 6.4 Testability

The conflict prompt is implemented via the variable
`plane-org-sync-conflict-resolver` (default `#'y-or-n-p`), allowing tests to
bind it to a stub function (e.g., `(lambda (_) t)` to always accept, or
`(lambda (_) nil)` to always reject). This avoids interactive prompts in ERT
tests.

### 6.5 Org File Safety

The Org interface never modifies the sync file directly on disk. All changes
are made in a temporary buffer, validated (e.g., `org-lint` basic checks),
and written atomically. This prevents data loss from mid-sync crashes or
API timeouts.

---

## 7. Design Decisions

### 7.1 Pure Elisp vs. Python Helper

#### 7.1.1 Issue
Should the package shell out to a Python script for API calls and org file
generation, or implement everything in Emacs Lisp?

#### 7.1.2 Boundary Conditions
- Target users are Emacs users who may not have Python installed or configured
- The package should be installable via MELPA without external setup
- Emacs has built-in HTTP (`url.el`) and JSON (`json.el`) support

#### 7.1.3 Assumptions
- `url-retrieve` async performance is sufficient for Plane's API
- JSON parsing of 200 work items is fast enough in elisp
- No need for complex HTML→Org conversion (simple regex suffices for Plane's
  limited HTML subset)

#### 7.1.4 Considered Alternatives
| Option | Pros | Cons |
|--------|------|------|
| Pure Elisp | Zero deps, MELPA-ready, single install | Async HTTP is verbose, no `requests` equivalent |
| Python script | Easier HTTP/JSON, richer HTML parsing | External dep, PATH issues, two-language maintenance |
| Hybrid (plz.el) | Better HTTP ergonomics than url.el | Adds curl dependency, not built-in |

#### 7.1.5 Decision
**Pure Elisp**. The API surface is simple (5-6 endpoints, JSON payloads),
`url-retrieve` is adequate, and zero external dependencies maximizes
installability. If `url.el` proves too painful, `plz.el` (curl wrapper)
can be adopted later as an optional backend.

### 7.2 Sync File Strategy: Dedicated File vs. Inline in Existing Files

#### 7.2.1 Issue
Should synced items go into a dedicated `plane.org` file, or be inserted into
existing user org files (e.g., `work.org`)?

#### 7.2.2 Boundary Conditions
- Users have their own org file structure they don't want disrupted
- Org-agenda can aggregate across files
- Sync needs to reliably find and update headings by PLANE_ID

#### 7.2.3 Assumptions
- Users accept having a separate file for Plane items
- The file appears in agenda automatically via `org-agenda-files` directory scan

#### 7.2.4 Considered Alternatives
| Option | Pros | Cons |
|--------|------|------|
| Dedicated file | Clean separation, safe to rebuild, no conflicts with user content | Extra file, items not co-located with related org notes |
| Inline in existing | Items live where user wants them | Complex merge logic, risk of corrupting user content, hard to rebuild |
| Heading under existing file | Middle ground: `* Plane` subtree in work.org | Still modifies user file, subtree management is fragile |

#### 7.2.5 Decision
**Dedicated file** (default `plane.org` in `org-directory`). This is the safest
approach — the file is fully managed by the package and can be rebuilt from
scratch without risk. Users who want co-location can use org-agenda filtering
or refile individual items.

### 7.3 Conflict Resolution Strategy

#### 7.3.1 Issue
When a work item is modified in both Org and Plane between syncs, which version
wins?

#### 7.3.2 Boundary Conditions
- Plane is the team's source of truth
- The user may have intentionally changed state locally
- Silent data loss is unacceptable

#### 7.3.3 Assumptions
- Conflicts are rare (most state changes happen on one side)
- The `updated_at` timestamp on Plane items is reliable

#### 7.3.4 Considered Alternatives
| Option | Pros | Cons |
|--------|------|------|
| Remote wins | Simple, Plane stays authoritative | User loses local changes silently |
| Local wins | Preserves user intent | Could overwrite teammate changes |
| Prompt user | User decides per-conflict | Annoying at scale, blocks sync |
| Last-write-wins | Deterministic | Unpredictable behavior |

#### 7.3.5 Decision
**Prompt for state conflicts, remote wins for other fields**. State changes
(the most meaningful user action) prompt for confirmation when conflicting.
Other field updates (title, description, dates) always take the remote value,
since Plane is authoritative and local edits to these fields are not synced
back.

---

## 8. Change History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 Draft | 2026-02-19 | Martin / Claude | Initial draft |
