# Software Requirements Specification (SRS)

**Product**: plane-org-sync (PS)
**Version**: 1.0 Draft
**System Type**: Non-regulated Software
**Date**: 2026-02-19

---

## 1. Introduction

### 1.1 Purpose

This document specifies how the user requirements defined in the URS are
translated into testable software system requirements for plane-org-sync.

### 1.2 Scope

plane-org-sync is an Emacs Lisp package that synchronizes Plane.so work items
with Org-mode files via the Plane REST API. This document describes the
system's behavior at its interface boundaries — user-facing commands and
Plane API interactions — without prescribing internal architecture.

### 1.3 Referenced Documents

| Document | Description |
|----------|-------------|
| URS-PS v1.0 | User Requirements Specification for plane-org-sync |
| [Plane API Reference](https://developers.plane.so/api-reference/) | REST API documentation |
| [Org-mode Manual](https://orgmode.org/manual/) | GNU Emacs Org-mode documentation |

### 1.4 Definitions and Abbreviations

See URS Section 1.4. Additional terms:

| Term | Definition |
|------|------------|
| Sync metadata | Org properties stored on each synced heading (Plane ID, last-modified timestamp, sync hash) used to track sync state |
| State mapping | A bidirectional association between Plane workflow states and Org TODO keywords |
| Sync file | The designated Org file that receives synced Plane work items |

### 1.5 Scope Exclusions

The following capabilities are explicitly out of scope for v1.0:

| Exclusion | Rationale |
|-----------|-----------|
| Sub-work-item hierarchy | v1 uses a flat list only; parent-child relationships are not represented |
| Work item types (Bug, Task, Story, Epic) | Plane's type system is not mapped to Org constructs |
| Cycles and Modules | Sprint/feature group membership is not synced |
| Comments (read or write) | Comment sync requires complex threading; out of scope |
| Attachments | Binary attachment handling adds significant complexity |
| Multi-workspace support | A single workspace is configured per installation |
| Description push (Org to Plane) | Description is one-way: Plane to Org only |
| Title push (Org to Plane) | Title is one-way: Plane to Org only |
| Bulk operations | Each state change is a separate API call |
| Webhook-based real-time sync | Requires a local HTTP server; polling is used instead |
| Offline mode / change queue | Changes require immediate API connectivity |

**Known limitations:**
- Work item titles longer than 255 characters are truncated to 255 characters
  in the Org heading. The full title is preserved in Plane.

---

## 2. Context Diagram

```
┌─────────────────┐          ┌──────────────────────────┐
│                  │          │                          │
│  Emacs User      │◄────────►  [UI-1] Emacs Commands   │
│  (Solo Dev /     │          │         & Org Buffers    │
│   Power User)    │          │                          │
│                  │          │                          │
└─────────────────┘          │    plane-org-sync         │
                             │    ┌──────────────┐      │
                             │    │ (black box)  │      │
┌─────────────────┐          │    └──────────────┘      │
│                  │          │                          │
│  Plane.so        │◄────────►  [SI-1] Plane REST API   │
│  (Cloud or       │          │                          │
│   Self-hosted)   │          │                          │
│                  │          └──────────────────────────┘
└─────────────────┘                      ▲
                                         │
                             ┌───────────┴──────────┐
                             │                      │
                             │  [SI-2] Org Files    │
                             │  (local filesystem)  │
                             │                      │
                             └──────────────────────┘
```

**Interfaces:**
- **UI-1**: Emacs interactive commands, minibuffer prompts, org-capture
  integration, message/notification output
- **SI-1**: Plane.so REST API (HTTPS, JSON, API key authentication)
- **SI-2**: Local Org files (read/write org-formatted text with properties)

---

## 3. User Interfaces

### 3.1 UI-1: Emacs Commands & Org Buffers

#### 3.1.1 Configuration Interface

**[SRS-PS-310101] API Key Configuration**
The system accepts a Plane API key via the customization variable
`plane-org-sync-api-key`. The value may reference an auth-source entry or a
string. The system validates the key by attempting a test API call on first use.
If validation fails, the system displays a descriptive error in the minibuffer.
Refs: URS-PS-10101

**[SRS-PS-310102] Instance URL Configuration**
The system accepts a Plane API base URL via `plane-org-sync-instance-url`.
Default value: `https://api.plane.so`. The URL must use HTTPS. If the URL does
not respond or returns a non-Plane response, the system reports the error.

Note: This URL is the API endpoint, not the browser URL. Plane Cloud uses
`api.plane.so` for API access and `app.plane.so` for browser access. The
`PLANE_URL` property on synced headings uses the browser URL (`app.plane.so`)
for human navigation, while `plane-org-sync-instance-url` is used exclusively
for API calls.
Refs: URS-PS-10101

**[SRS-PS-310103] Workspace Configuration**
The system accepts a workspace slug via `plane-org-sync-workspace`. When not
set, the interactive command `plane-org-sync-setup` prompts the user to enter
their workspace slug (the slug portion of their Plane URL, e.g.,
`https://app.plane.so/{workspace-slug}/`). The Plane API does not provide a
workspace enumeration endpoint accessible with a standard API key.
Refs: URS-PS-10102

**[SRS-PS-310104] Project Configuration**
The system accepts a list of project UUIDs via `plane-org-sync-projects`. The
user may enter project identifiers (e.g., "PROJ") during setup; the system
resolves these to UUIDs via the `GET .../projects/` endpoint and stores UUIDs
in `plane-org-sync-projects`. Display strings and human-readable properties use
the project identifier; API calls use the UUID.

When not set, `plane-org-sync-setup` fetches available projects from the
configured workspace and presents them for multi-selection. The setup wizard
requires at least one project to be selected.

An empty or nil `plane-org-sync-projects` list means no projects are synced.
If `plane-org-sync-projects` is nil when `plane-org-sync-pull` is invoked, the
system reports an error: "No projects configured. Run M-x plane-org-sync-setup."
Refs: URS-PS-10102

**[SRS-PS-310105] State Mapping Configuration**
The system derives a default state mapping from Plane's state group
classification. On first sync (or when project configuration changes), the
system fetches states from `GET .../states/` for each configured project and
maps them by group:

| State Group | Default Org Keyword | Org Done-State |
|-------------|-------------------|----------------|
| `backlog` | `TODO` | No |
| `unstarted` | `TODO` | No |
| `started` | `STARTED` | No |
| `completed` | `DONE` | Yes |
| `cancelled` | `CANCELLED` | Yes |

The default group-to-keyword mapping is exposed as the defcustom
`plane-org-sync-group-keyword-mapping` (alist of state-group string to Org
keyword string). Users may change the group defaults (e.g., map `started` to
`IN-PROGRESS` instead of `STARTED`).

Additionally, the system accepts an optional alist
`plane-org-sync-state-mapping` that overrides the group-based default for
specific Plane state names. Example:

```elisp
;; Only needed if the group defaults are insufficient:
'(("In Review" . "REVIEW")
  ("QA"        . "TESTING"))
```

States not present in the override alist use the group-based default. The
group-based default is always available as a fallback and never requires user
configuration.
Refs: URS-PS-10103

**[SRS-PS-310106] Sync File Configuration**
The system accepts a file path via `plane-org-sync-file`. Default:
`plane.org` in `org-directory`. The file is created if it does not exist.
Refs: URS-PS-10104

**[SRS-PS-310107] Assignee Filter Configuration**
The system accepts `plane-org-sync-filter-assignee` (boolean, default `t`).
When true, only work items assigned to the authenticated user are synced.
Refs: URS-PS-10201

#### 3.1.2 Sync Commands

**[SRS-PS-310201] Manual Sync Command**
The system provides the interactive command `plane-org-sync-pull` which:
1. Fetches work items from Plane matching the configured filters. The system
   excludes work items where `is_draft` is true or `archived_at` is not null.
   The system attempts server-side filtering via the `?assignees=` query
   parameter. If the API does not support this filter on the `/work-items/`
   endpoint, the system fetches all items and filters client-side by comparing
   assignee UUIDs to the authenticated user's ID.
2. Creates or updates Org headings in the sync file
3. Reports the count of created, updated, and unchanged items in the minibuffer

If no work items match the configured filters, the system reports: "Synced: 0
items (no matching work items found)."

If a multi-project sync encounters an error on one project, the system
completes synchronization for all successful projects, reports failures for
the failed project(s), and informs the user of partial results (e.g., "Synced
2/3 projects. Failed: PROJ-C (HTTP 500).").

The command must not block Emacs during API calls (async HTTP).
Refs: URS-PS-10201, URS-PS-10202

**[SRS-PS-310202] Automatic Sync Timer**
The system provides `plane-org-sync-auto-interval` (integer, seconds, default
`nil`/disabled). When set to a positive integer, a repeating idle timer triggers
`plane-org-sync-pull` at the configured interval. The timer is started by
`plane-org-sync-mode` and cancelled when the mode is disabled.
Refs: URS-PS-10203

**[SRS-PS-310203] Push State Change Command**
When the user changes the TODO keyword on a synced Org heading (detected via
`org-after-todo-state-change-hook`), the system:
1. Looks up the corresponding Plane state from the reverse state mapping
2. Sends a PATCH request to update the work item's state in Plane
3. Updates the sync metadata on the heading
4. Reports success or failure in the minibuffer

If the heading has no sync metadata (not a Plane-synced item), the hook is a
no-op.

If the Plane API rejects a state change with HTTP 400 (e.g., due to workflow
constraints), the system reverts the local TODO keyword to its previous value
and displays: "State transition not allowed: {old-state} -> {new-state}". The
API error detail is included if available.

**[SRS-PS-310206] Suppress Push During Pull Sync**
The system must suppress outbound state push when updating TODO keywords during
a pull sync operation. When the sync engine changes a heading's TODO keyword to
reflect the remote state, the `org-after-todo-state-change-hook` must not
trigger a push back to Plane. This prevents feedback loops during pull sync.
Refs: URS-PS-10401, SRS-PS-310201

**[SRS-PS-310204] Conflict Detection on State Push**
Before pushing a state change, the system compares the heading's stored
`PLANE_UPDATED_AT` property with the work item's current `updated_at` from the
API. If the remote timestamp is newer, the system prompts the user:
"Remote item was modified since last sync. Push anyway? (y/n)".
If the user declines, the local heading is reverted to the remote state.
Refs: URS-PS-10403

**[SRS-PS-310205] Force Re-sync Command**
The system provides the interactive command `plane-org-sync-reset` which:
1. Prompts for confirmation ("This will rebuild the sync file from Plane. Continue?")
2. Removes all sync metadata properties from existing headings
3. Performs a full sync, re-matching headings by Plane ID or recreating them

Refs: URS-PS-10603

#### 3.1.3 Org Capture Integration

**[SRS-PS-310301] Capture Template Function**
The system provides the function `plane-org-sync-capture-template` that returns
a valid org-capture template entry. The template:
1. Prompts for project (via `completing-read` against configured projects)
2. Prompts for title (free text)
3. Prompts for priority (via `completing-read`: none, urgent, high, medium, low)
4. Prompts for labels (via `completing-read` with multiple selection, fetched from API)
5. Creates the work item in Plane via POST
6. Inserts an Org heading in the sync file with the title, mapped TODO state,
   priority, labels as tags, and sync metadata including the Plane URL

Refs: URS-PS-10501, URS-PS-10502

#### 3.1.4 Navigation

**[SRS-PS-310401] Plane Link in Synced Headings**
Each synced Org heading includes an org-mode hyperlink to the work item's Plane
URL. The link text is the human-readable work item identifier (e.g., `PROJ-42`).
The link is placed as the first line of the heading body, before the description.
The user navigates to Plane using standard `org-open-at-point` (`C-c C-o`). The
`PLANE_URL` property is retained for programmatic use by the sync engine.
Refs: URS-PS-10303, URS-PS-10302

#### 3.1.5 Status & Diagnostics

**[SRS-PS-310501] Sync Status Command**
The system provides `plane-org-sync-status` which displays in the minibuffer:
- Last sync timestamp (or "Never synced")
- Result of last sync (success, N items synced / error message)
- Whether auto-sync is active and its interval

Refs: URS-PS-10601

**[SRS-PS-310502] Error Reporting**
When an API call fails, the system displays a message with:
- HTTP status code and Plane error message (if available)
- Actionable hint (e.g., "API key may be invalid — check plane-org-sync-api-key")
- Rate limit information if HTTP 429 is received

Errors are also logged to the `*plane-org-sync-log*` buffer.
Refs: URS-PS-10602

---

## 4. System Interfaces

### 4.1 SI-1: Plane REST API

#### 4.1.1 Endpoints

The system uses the following Plane API endpoints. Base URL:
`{instance-url}/api/v1/workspaces/{workspace-slug}/projects/{project-id}/`

| Operation | Method | Path | Purpose |
|-----------|--------|------|---------|
| List work items | GET | `work-items/` | Fetch items (with `assignees` filter, pagination) |
| Get work item | GET | `work-items/{id}/` | Fetch single item (for conflict check) |
| Create work item | POST | `work-items/` | Create new item from capture |
| Update work item | PATCH | `work-items/{id}/` | Push state changes |
| List states | GET | `states/` | Fetch workflow states for mapping |
| List labels | GET | `labels/` | Fetch labels for capture completion |
| List projects | GET | (workspace level) `projects/` | For setup wizard |
| Get current user | GET | `/api/v1/users/me/` | Identify authenticated user for assignee filter |

**Note**: The API is transitioning from `/issues/` to `/work-items/` endpoints.
The system must use `/work-items/` paths (supported from Plane v1.x, mandatory
by 2026-03-31).

#### 4.1.2 Models

**Authentication:**
- Header: `X-API-Key: {api-key}`
- Content-Type: `application/json`

**Rate Limiting:**
- 60 requests per minute per API key
- The system must respect `429 Too Many Requests` responses and implement
  exponential backoff (initial delay 2s, max 3 retries)

**Work Item (relevant fields for sync):**

| Field | Type | Org mapping |
|-------|------|-------------|
| `id` | UUID | `PLANE_ID` property |
| `name` | string | Heading title |
| `state` | UUID → state name | TODO keyword (via state mapping) |
| `priority` | string | `PLANE_PRIORITY` property + `[#A]`/`[#B]`/`[#C]` |
| `assignees` | UUID[] | `PLANE_ASSIGNEES` property |
| `labels` | UUID[] → label names | Org tags |
| `target_date` | date string | `DEADLINE` |
| `start_date` | date string | `SCHEDULED` |
| `sequence_id` | integer | Part of `PLANE_URL` |
| `updated_at` | ISO timestamp | `PLANE_UPDATED_AT` property (conflict detection) |
| `description_html` | HTML string | Body text (converted to org markup) |
| `project` | UUID | `PLANE_PROJECT` property (human-readable identifier, e.g., "PROJ") + `PLANE_PROJECT_ID` property (UUID for API calls) |

**Pagination:**
- The API returns paginated results. The system must follow pagination cursors
  until all matching items are retrieved.

Refs: URS-PS-10201, URS-PS-10401, URS-PS-10501

### 4.2 SI-2: Org File Interface

#### 4.2.1 Sync File Structure

**[SRS-PS-420101] Heading Structure**
Each synced work item is represented as a top-level Org heading in the sync
file with the following structure:

```org
* TODO [#B] Implement user authentication       :backend:auth:
SCHEDULED: <2026-02-20> DEADLINE: <2026-02-28>
:PROPERTIES:
:PLANE_ID: 01234567-89ab-cdef-0123-456789abcdef
:PLANE_URL: https://app.plane.so/workspace/projects/abc/work-items/42
:PLANE_PROJECT: PROJ
:PLANE_PROJECT_ID: fedcba98-7654-3210-fedc-ba9876543210
:PLANE_PRIORITY: high
:PLANE_STATE: In Progress
:PLANE_STATE_ID: abcdef01-2345-6789-abcd-ef0123456789
:PLANE_UPDATED_AT: 2026-02-19T10:30:00Z
:CATEGORY: PROJ
:END:
[[https://app.plane.so/workspace/projects/abc/work-items/42][PROJ-42]]

Description from Plane (converted to org markup).

** My local notes
These are preserved across syncs.
```

Body content starts after the `:PROPERTIES:` drawer. The Plane link is the
first line of body content, followed by the description paragraph.

Plane label names are sanitized for Org tags: non-alphanumeric characters
(except `_`, `@`, `#`, `%`) are replaced with underscores, and the result is
downcased.

Refs: URS-PS-10204, URS-PS-10301, URS-PS-10302, URS-PS-10303

**[SRS-PS-420102] Heading Matching on Sync**
The system matches existing headings to Plane work items by the `PLANE_ID`
property. If a heading with a matching `PLANE_ID` exists, it is updated
in-place. If no match exists, a new heading is appended.
Refs: URS-PS-10201

**[SRS-PS-420103] Local Content Preservation**
When updating a synced heading, the system only modifies:
- The headline text (title, TODO keyword, priority, tags)
- The `SCHEDULED` and `DEADLINE` timestamps
- The `:PROPERTIES:` drawer values
- The Plane link line (first body line, before description)
- The first body paragraph after the link (description from Plane)

When `description_html` is null or empty, no description paragraph is inserted.
If a description previously existed and the remote value is now null, the
existing description paragraph is removed on the next sync.

Description text is synced one-way from Plane to Org. Local edits to the
description paragraph may be overwritten on the next pull sync. Content added
as sub-headings or after the description paragraph is preserved.

Title text is also synced one-way from Plane to Org. Local edits to the
heading title may be overwritten on the next pull sync.

Sub-headings and any content after the description paragraph are preserved.
Refs: URS-PS-10204

**[SRS-PS-420104] Atomic File Writes**
The system writes the sync file atomically: changes are written to a temporary
file first, then renamed to the target path. If any error occurs during
writing, the original file remains unchanged.
Refs: URS-PS-10604

**[SRS-PS-420107] HTML-to-Org Description Conversion**
The system converts Plane's `description_html` (Tiptap/ProseMirror HTML) to
Org markup using the following conversion table:

| HTML Element | Org Markup |
|-------------|-----------|
| `<p>` | Double newline (paragraph separator) |
| `<strong>`, `<b>` | `*bold*` |
| `<em>`, `<i>` | `/italic/` |
| `<code>` | `~code~` |
| `<pre>` | `#+BEGIN_SRC ... #+END_SRC` |
| `<a href="url">text</a>` | `[[url][text]]` |
| `<ul>/<li>` | `- item` |
| `<ol>/<li>` | `1. item` |
| `<blockquote>` | `#+BEGIN_QUOTE ... #+END_QUOTE` |
| `<h1>` through `<h6>` | Bold text (heading level stripped; content is inside an Org heading) |
| `<img src="url">` | `[[url]]` (link only, no inline display) |
| `<table>` | Passed through as-is (v1 limitation) |
| All other tags | Tags stripped, text content preserved |

Refs: SRS-PS-420101, SRS-PS-420103

**[SRS-PS-420108] Orphaned Heading Preservation**
Headings whose `PLANE_ID` does not appear in the current remote result set are
left unchanged in the sync file. Their TODO keyword, properties, and content
are not modified. This covers items that have been unassigned from the user,
deleted, or archived in Plane. The system does not delete headings.
Refs: URS-PS-10204, SRS-PS-420102

#### 4.2.2 Priority Mapping

**[SRS-PS-420201] Priority Mapping Table**

| Plane Priority | Org Priority |
|---------------|-------------|
| urgent | `[#A]` |
| high | `[#A]` |
| medium | `[#B]` |
| low | `[#C]` |
| none | (no priority cookie) |

Refs: URS-PS-10201

#### 4.2.3 State Sync

**[SRS-PS-420105] TODO Keyword Sequence Generation**
The system generates and maintains a `#+TODO:` header line in the sync file
that declares all Org keywords produced by the state mapping (group defaults
plus overrides). Active-state keywords (from `backlog`, `unstarted`, `started`
groups) appear before the `|` separator; done-state keywords (from `completed`,
`cancelled` groups) appear after it.

The managed `#+TODO:` line is identified by a trailing comment
`# plane-org-sync-managed`. Other `#+TODO:` lines in the file are preserved
unchanged. If no managed line exists, one is inserted as the first line of the
sync file. The line is updated before heading modifications during each pull
sync.
Refs: SRS-PS-310105, SRS-PS-420101, URS-PS-10103

**[SRS-PS-420106] Plane State Identity Properties**
Each synced heading stores two additional properties:
- `PLANE_STATE_ID`: The UUID of the work item's current Plane state (used for
  reverse resolution on push)
- `PLANE_STATE`: The human-readable name of the Plane state (informational,
  enables Org property searches like `PLANE_STATE="In Review"`)

These properties are updated on each pull sync alongside `PLANE_UPDATED_AT`.
Refs: SRS-PS-420101, SRS-PS-310108, URS-PS-10401

**[SRS-PS-310108] Reverse State Resolution for Push**
When pushing a TODO state change to Plane, the system resolves the Org keyword
to a Plane state UUID as follows:

1. If `plane-org-sync-state-mapping` contains an entry whose value matches the
   Org keyword, the system looks up the corresponding Plane state name in the
   project's state list and uses its UUID. When resolution succeeds via this
   explicit override, the state is always pushed — the group compatibility
   check in step 3 is skipped.
2. Otherwise, the system resolves the Org keyword to a state group via the
   inverse of `plane-org-sync-group-keyword-mapping`, then selects the first
   state (by API sequence order) in that group for the heading's project.
3. If resolution came via step 2 (group-based) and the heading's current
   `PLANE_STATE_ID` already belongs to the target group, no state change is
   pushed (the states are considered compatible).
4. If no matching state can be resolved, the system warns the user and does not
   push the change.

Refs: URS-PS-10401, SRS-PS-310203

---

## 5. Cross-functional Requirements

### 5.1 Performance Requirements

**[SRS-PS-510001] Sync Response Time**
A manual sync of up to 200 work items must complete within 30 seconds
wall-clock time on a standard broadband connection (>10 Mbps). Of this, local
processing (diff computation, Org buffer manipulation, file write) must not
exceed 2 seconds; the remainder is network time. The system must not block
Emacs UI during sync (network operations are async; Org buffer writes occur in
the async callback).
Refs: URS-PS-10202

**[SRS-PS-510002] Rate Limit Compliance**
The system must not exceed 60 API requests per minute. For workspaces with
multiple projects, requests should be batched where the API supports it.
Refs: URS-PS-10201

### 5.2 Safety & Security Requirements

**[SRS-PS-520001] API Key Storage**
The system must support reading the API key from Emacs `auth-source` (e.g.,
`~/.authinfo.gpg`, macOS Keychain) as the recommended method. Storing the key
as a plaintext string in init.el is supported but discouraged in documentation.
Refs: URS-PS-10101

**[SRS-PS-520002] HTTPS Only**
All API communication must use HTTPS. The system must reject HTTP instance URLs.
Refs: URS-PS-10101

### 5.3 Service Requirements

**[SRS-PS-530001] Package Distribution**
The system must be installable via MELPA, package-vc (Emacs 29+), and manual
load-path addition.
Refs: URS-PS-30101

**[SRS-PS-530002] Emacs Version Compatibility**
The system must support Emacs 29.1 and later. Emacs 30+ features may be used
with appropriate version guards.
Refs: URS-PS-30101

### 5.4 Applicable Standards and Regulations

None (non-regulated software).

---

## 6. Run-time Environment

| Component | Requirement |
|-----------|-------------|
| Emacs | >= 29.1 |
| Org-mode | >= 9.6 (bundled with Emacs 29+) |
| Dependencies | `url.el` (built-in), `json.el` (built-in), `auth-source` (built-in) |
| Network | HTTPS access to Plane instance |
| OS | Any OS supported by Emacs (macOS, Linux, Windows) |

No external binaries (Python, curl, etc.) are required. The package is
pure Emacs Lisp.

---

## 7. Change History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 Draft | 2026-02-19 | Martin / Claude | Initial draft |
