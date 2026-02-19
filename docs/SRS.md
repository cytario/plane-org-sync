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
The system accepts a Plane instance base URL via `plane-org-sync-instance-url`.
Default value: `https://app.plane.so`. The URL must use HTTPS. If the URL does
not respond or returns a non-Plane response, the system reports the error.
Refs: URS-PS-10101

**[SRS-PS-310103] Workspace Configuration**
The system accepts a workspace slug via `plane-org-sync-workspace`. When not
set, the interactive command `plane-org-sync-setup` fetches available workspaces
from the API and presents them for selection via `completing-read`.
Refs: URS-PS-10102

**[SRS-PS-310104] Project Configuration**
The system accepts a list of project identifiers (slugs or UUIDs) via
`plane-org-sync-projects`. When not set, `plane-org-sync-setup` fetches
available projects from the configured workspace and presents them for
multi-selection. An empty list means "all projects in the workspace".
Refs: URS-PS-10102

**[SRS-PS-310105] State Mapping Configuration**
The system accepts an alist via `plane-org-sync-state-mapping` that maps Plane
state names (strings) to Org TODO keywords (strings). Example:

```elisp
'(("Backlog"     . "TODO")
  ("Todo"        . "TODO")
  ("In Progress" . "STARTED")
  ("Done"        . "DONE")
  ("Cancelled"   . "OBSOLETE"))
```

Plane states not present in the mapping are synced as-is (uppercased). Org
keywords not present in the reverse mapping trigger a warning on push.
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
1. Fetches work items from Plane matching the configured filters
2. Creates or updates Org headings in the sync file
3. Reports the count of created, updated, and unchanged items in the minibuffer

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
Refs: URS-PS-10401

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

**[SRS-PS-310401] Open in Plane Browser**
The system provides the interactive command `plane-org-sync-browse` which, when
point is on a synced Org heading, opens the work item's Plane URL in the default
browser via `browse-url`. The URL is stored in the `PLANE_URL` Org property.
If point is not on a synced heading, the system reports "Not a Plane-synced heading".
Refs: URS-PS-10303

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
| `project` | UUID | `PLANE_PROJECT` property |

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
:PLANE_PROJECT: my-project
:PLANE_PRIORITY: high
:PLANE_UPDATED_AT: 2026-02-19T10:30:00Z
:END:

Description from Plane (converted to org markup).

** My local notes
These are preserved across syncs.
```

Refs: URS-PS-10204, URS-PS-10301, URS-PS-10302

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
- The first body paragraph (description from Plane)

Sub-headings and any content after the first body paragraph are preserved.
Refs: URS-PS-10204

**[SRS-PS-420104] Atomic File Writes**
The system writes the sync file atomically: changes are written to a temporary
file first, then renamed to the target path. If any error occurs during
writing, the original file remains unchanged.
Refs: URS-PS-10604

#### 4.2.2 Priority Mapping

| Plane Priority | Org Priority |
|---------------|-------------|
| urgent | `[#A]` |
| high | `[#A]` |
| medium | `[#B]` |
| low | `[#C]` |
| none | (no priority cookie) |

Refs: URS-PS-10201

---

## 5. Cross-functional Requirements

### 5.1 Performance Requirements

**[SRS-PS-510001] Sync Response Time**
A manual sync of up to 200 work items must complete within 30 seconds on a
standard broadband connection (>10 Mbps). The system must not block Emacs UI
during sync.
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
