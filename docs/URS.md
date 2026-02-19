# User Requirements Specification (URS)

**Product**: plane-org-sync (PS)
**Version**: 1.0 Draft
**System Type**: Non-regulated Software
**Date**: 2026-02-19

---

## 1. Introduction

### 1.1 Purpose

This document defines the user requirements for **plane-org-sync**, an Emacs
package that enables users to work with their Plane.so project management tasks
from within Org-mode.

### 1.2 Scope

plane-org-sync bridges Plane.so (a project management platform) and Emacs
Org-mode (a personal productivity system). It enables bidirectional
synchronization of work items between the two systems, allowing users to
leverage Org-mode's agenda, capture, and TODO workflow while keeping Plane.so as
the team's source of truth.

### 1.3 Referenced Documents

| Document | Description |
|----------|-------------|
| [Plane API Reference](https://developers.plane.so/api-reference/) | REST API documentation for Plane.so |
| [Org-mode Manual](https://orgmode.org/manual/) | GNU Emacs Org-mode documentation |

### 1.4 Definitions and Abbreviations

| Term | Definition |
|------|------------|
| PS | plane-org-sync (this product) |
| Plane | Plane.so project management platform |
| Org | Emacs Org-mode |
| Work item | A task/issue in Plane.so (formerly called "issue", being migrated to "work-item" in the API) |
| Agenda | Org-mode's scheduling and task review interface |
| Capture | Org-mode's mechanism for quickly creating new entries |
| Workspace | Top-level organizational unit in Plane (contains projects) |
| Project | A Plane project within a workspace (contains work items) |

---

## 2. System Overview

### 2.1 General Description

Emacs users who participate in teams using Plane.so for project management need
to interact with their assigned tasks without leaving their Emacs workflow.
Currently, they must context-switch to a web browser to review, update, or
create Plane tasks. plane-org-sync eliminates this context switch by
synchronizing Plane work items into Org-mode files, where they become
first-class Org headings with TODO states, scheduling, and metadata — fully
integrated into the Org agenda.

### 2.2 Overall Process Description

#### 2.2.1 Macro-Step 1: Configure Sync

A user connects plane-org-sync to their Plane.so instance (cloud or
self-hosted) by providing API credentials and selecting which workspace and
projects to synchronize. The user also maps Plane workflow states to Org TODO
keywords and chooses where synced tasks should be stored.

- **Input Data**: API key, instance URL, workspace slug, project identifiers,
  state-to-TODO mapping, target org file path
- **Output Data**: Persisted configuration that subsequent sync operations use
- **User Requests**:
  - "I want to set my API key once and not be asked again"
  - "I need to choose which projects to pull from"
  - "My Plane has custom states — they should map to my org TODO keywords"
  - "I want synced tasks in a dedicated org file, not mixed into my work.org"
  - "It should work with both app.plane.so and my self-hosted instance"

#### 2.2.2 Macro-Step 2: Sync Issues from Plane to Org

A user pulls their assigned work items from Plane into an Org file. The sync
creates or updates Org headings with the work item's title, state, priority,
labels, assignee, and dates. Previously synced items that have been completed or
removed in Plane are updated accordingly.

- **Input Data**: Configured Plane connection, optional filters (assignee,
  project, state)
- **Output Data**: Org file with headings representing Plane work items
- **User Requests**:
  - "I only want to see issues assigned to me"
  - "I want to trigger sync manually with a command"
  - "It would be nice if it synced automatically every N minutes"
  - "Priority and labels from Plane should be visible in the org heading"
  - "If an issue was closed in Plane, the org heading should show DONE"
  - "Don't lose my local org notes when re-syncing"

#### 2.2.3 Macro-Step 3: Review Tasks in Org Agenda

A user reviews their Plane tasks alongside other org tasks in the standard Org
agenda. Plane-synced items appear with their scheduled dates, priorities, and
states just like any other org heading.

- **Input Data**: Org agenda query
- **Output Data**: Agenda view containing Plane-synced items interleaved with
  other org entries
- **User Requests**:
  - "Plane tasks should show up in my daily/weekly agenda"
  - "I want to see which project a task belongs to"
  - "I should be able to filter the agenda to show only Plane tasks"
  - "Target dates from Plane should become org DEADLINE or SCHEDULED"

#### 2.2.4 Macro-Step 4: Update Task State (Bidirectional)

A user changes a TODO state on a Plane-synced org heading (e.g., marks it
DONE), and the change propagates back to Plane. Conversely, state changes made
by teammates in Plane appear in org after the next sync.

- **Input Data**: State change on an org heading (local) or state change in
  Plane (remote)
- **Output Data**: Updated state in both systems
- **User Requests**:
  - "When I mark something DONE in org, it should close in Plane"
  - "When a teammate changes state in Plane, I see it after sync"
  - "I don't want accidental state changes — maybe confirm before pushing"
  - "If both sides changed, warn me instead of silently overwriting"

#### 2.2.5 Macro-Step 5: Capture New Issues to Plane

A user creates a new Plane work item directly from Emacs using org-capture. The
captured entry is pushed to Plane as a new work item and simultaneously added to
the local org sync file.

- **Input Data**: Task title, optional description, project, priority, labels
- **Output Data**: New work item in Plane + corresponding org heading locally
- **User Requests**:
  - "I want an org-capture template that creates a Plane issue"
  - "I should be able to pick the target project during capture"
  - "Priority and labels should be settable during capture"
  - "The org heading should link back to the Plane issue URL"

#### 2.2.6 Macro-Step 6: Manage Sync Lifecycle

A user monitors sync status, identifies issues (API errors, conflicts,
rate-limiting), and resolves them. The user can also force a full re-sync if
the local state has drifted.

- **Input Data**: Sync status query, conflict resolution decisions
- **Output Data**: Status information, resolved conflicts, clean sync state
- **User Requests**:
  - "Tell me when the last sync happened and if it succeeded"
  - "If the API key expired, give me a clear error"
  - "If sync fails, don't corrupt my org file"
  - "I should be able to force a full re-sync from scratch"

### 2.3 User Groups

| Group | Description | Characteristics |
|-------|-------------|-----------------|
| **Solo Developer** | Individual using Plane.so for personal or small-team project tracking. Uses org-mode as their primary task management system. | Comfortable with elisp configuration. Typically syncs 1-2 projects. Wants minimal setup friction. |
| **Emacs Power User** | Heavily customizes their Emacs workflow. Expects programmatic control over sync behavior, custom state mappings, and hook functions. | Will write elisp to customize behavior. Expects clean APIs and defcustom variables. May contribute to the package. |

---

## 3. User Requirements

### 3.1 Workflow Requirements

#### 3.1.1 Configure Sync

**[URS-PS-10101] Authenticate with Plane Instance**
A user authenticates plane-org-sync with their Plane.so instance by providing
an API key and instance URL, so that subsequent operations can access the Plane
API on their behalf.

**[URS-PS-10102] Select Workspace and Projects**
A user selects which Plane workspace and projects to synchronize, so that only
relevant work items are pulled into their org workflow.

**[URS-PS-10103] Map Plane States to Org TODO Keywords**
A user defines how Plane workflow states (e.g., "Backlog", "In Progress",
"Done") correspond to their Org TODO keywords (e.g., TODO, STARTED, DONE), so
that state transitions are meaningful in both systems.

**[URS-PS-10104] Designate Sync Target File**
A user specifies which Org file receives synced work items, so that Plane tasks
are stored separately from other org content.

#### 3.1.2 Sync Issues from Plane to Org

**[URS-PS-10201] Pull Assigned Work Items**
A user retrieves their personally assigned work items from configured Plane
projects into the designated Org file.

**[URS-PS-10202] Trigger Sync Manually**
A user initiates a sync on demand via an interactive Emacs command.

**[URS-PS-10203] Schedule Automatic Sync**
A user enables periodic background sync at a configurable interval, so that
their Org file stays current without manual intervention.

**[URS-PS-10204] Preserve Local Org Notes**
A user adds personal notes, properties, or sub-headings under a synced Org
heading, and these additions survive subsequent sync operations.

**[URS-PS-10205] Recognize Completed Remote Items**
A user sees work items that were closed or archived in Plane reflected as DONE
(or equivalent) in their Org file after sync.

#### 3.1.3 Review Tasks in Org Agenda

**[URS-PS-10301] View Plane Tasks in Agenda**
A user reviews Plane-synced work items in the standard Org agenda alongside
their other tasks, using the same navigation and filtering affordances.

**[URS-PS-10302] Identify Task Origin and Project**
A user distinguishes Plane-synced tasks from other Org entries and identifies
which Plane project each task belongs to.

**[URS-PS-10303] Navigate to Plane Issue**
A user opens the Plane web UI for a specific work item directly from the
corresponding Org heading, to access details not synced locally (comments,
attachments, activity).

#### 3.1.4 Update Task State (Bidirectional)

**[URS-PS-10401] Push Local State Changes to Plane**
A user changes a TODO state on a synced Org heading and the new state is
propagated to Plane, so that the team sees the updated status.

**[URS-PS-10402] Receive Remote State Changes**
A user sees state changes made by teammates in Plane reflected in the
corresponding Org headings after the next sync.

**[URS-PS-10403] Detect Conflicting Changes**
A user is notified when a work item was modified in both Org and Plane since the
last sync, so that they can decide which version to keep.

#### 3.1.5 Capture New Issues to Plane

**[URS-PS-10501] Create Plane Issue from Org Capture**
A user creates a new Plane work item using an org-capture template, specifying
title, project, priority, and labels during capture.

**[URS-PS-10502] Link Captured Entry to Plane**
A user sees a link to the newly created Plane issue in the resulting Org
heading, so they can navigate to it in the web UI.

#### 3.1.6 Manage Sync Lifecycle

**[URS-PS-10601] Review Sync Status**
A user checks when the last sync occurred and whether it completed successfully.

**[URS-PS-10602] Diagnose Sync Failures**
A user receives clear, actionable error messages when sync fails (e.g., invalid
API key, network error, rate limit exceeded).

**[URS-PS-10603] Force Full Re-sync**
A user triggers a complete re-sync that rebuilds the local Org file from the
current Plane state, discarding any cached sync metadata.

**[URS-PS-10604] Maintain Org File Integrity**
A user's Org file is never left in a corrupted or partially-written state, even
if sync is interrupted by errors or user action.

### 3.2 Regulatory Requirements

#### 3.2.1 Applicable Standards and Regulations

None. plane-org-sync is non-regulated open-source software.

#### 3.2.7 Not Applicable Regulatory Requirements

All regulatory sub-sections (3.2.2 through 3.2.6) are not applicable.
**Justification**: plane-org-sync is a developer productivity tool with no
regulatory, compliance, or safety implications. It does not process personal
health information, financial data, or other regulated data categories.

### 3.3 Other Requirements

**[URS-PS-30101] Package Installation**
A user installs plane-org-sync via standard Emacs package managers (MELPA,
package-vc, use-package with :vc).

**[URS-PS-30102] Documentation**
A user reads a README with installation instructions, configuration examples,
and usage guide to set up the package without external assistance.

**[URS-PS-30103] Customization via Standard Emacs Mechanisms**
An Emacs power user configures all sync behavior through defcustom variables
and hook functions, following established Emacs Lisp conventions.

---

## 4. Change History

| Version | Date | Author | Description |
|---------|------|--------|-------------|
| 1.0 Draft | 2026-02-19 | Martin / Claude | Initial draft |
