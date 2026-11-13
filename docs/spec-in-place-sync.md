# Spec: in-place sync of Org TODOs across many files

Status: plan approved for design; not implemented.

## Goal

Add a second way of using org-linear. Today it mirrors Linear into one file
(the *mirror* layout). The new *in-place* layout syncs TODO headings wherever
they already live, across many Org files, with Org as the primary copy. Both
layouts must run side by side in one Emacs, each against its own Linear
workspace.

## Non-goals (v1)

- Sub-issues from Org heading nesting (each TODO is a flat issue).
- Linear projects, cycles and estimates in the in-place layout.
- Linear recurring-issue templates (repeaters stay Org's job; see below).
- Syncing habits (`STYLE: habit`).

## Decisions (fixed)

1. The in-place layout talks to its own Linear workspace with its own API key.
   The user's choice is a separate, paid personal workspace, so there is no
   issue cap and no shared visibility.
2. Scope: every open TODO heading in configured directories, minus excluded
   files, excluded tags and habits. Default exclusions are set in the user's
   config, not in the package.
3. Conflicts: when a field changed on both sides since the last sync, the
   in-place layout keeps Org's value and logs Linear's. The mirror layout keeps
   today's rule (Linear wins).
4. Issues created in Linear land as TODOs under a configured heading in a
   configured inbox file. Once refiled anywhere in scope they keep syncing.

## Current state (verified in `org-linear.el`)

- All options are global `defcustom`s and all sync state is global `defvar`s
  (`org-linear--state`, `--busy`, `--meta`, `--timer`, ...), so only one
  configuration can exist.
- `org-linear--index` assumes level 1 = project (`LINEAR_PROJECT`) and
  level 2 = issue (`LINEAR_UUID`). `org-linear--heading-project` reads the
  parent heading.
- The merge (`org-linear--merge`) is a per-field three-way merge against the
  last-sync base, with Linear winning conflicts. This is the right core; the
  winner just needs to be an option.
- The sync works on one buffer: it refuses to apply while the buffer has
  unsaved edits and fails if the file changed on disk.
- `org-linear--body` runs to the next heading of any level.
- Closed issues leave the file (archive or delete). Trashed issues delete
  their heading.
- Async work runs in subprocess callbacks, so dynamic `let` bindings made at
  sync start do not reach them.

## Design

### 1. Profiles

- New option `org-linear-profiles`: an alist from a profile name to a plist of
  option values (`:teams`, `:file` or `:directories`, `:layout`,
  `:api-key-command`, `:state-keywords`, ...). Options not given fall back to
  the global `defcustom`, so the existing single-config setup keeps working as
  an implicit default profile.
- Sync state moves into a per-profile struct (`org-linear--profile`): state,
  meta, busy, pending, timer, stats, rerun, archive, API key cache.
- Every entry point and every async callback runs inside
  `org-linear--with-profile`, which binds the profile's options (via
  `cl-progv`) and its state struct. Callbacks capture the profile explicitly;
  nothing relies on a dynamic binding surviving across a process sentinel.
- Each profile has its own state file and log section.
- `org-linear-auto-sync-mode` starts one timer per profile.

### 2. Layouts

`:layout` is `mirror` (today) or `in-place`. The places that assume the mirror
layout go behind a small set of layout functions:

| Concern | mirror | in-place |
|---|---|---|
| Find issue headings | level-2 headings of one file | open-keyword headings in scope across files |
| New heading → create | level 2 under a project | any in-scope heading without `LINEAR_UUID` |
| Project | parent heading | none |
| Body | to next heading | to next heading of any level (children are not body) |
| New Linear issue | insert under project | insert under inbox heading |
| Closed issue | archive/delete heading | keep heading, set done keyword |
| Trashed issue | delete heading | drop `LINEAR_UUID`, keep heading, log |
| Conflict winner | Linear | Org |

### 3. Scope and the file scan (in-place)

- Options: `:directories`, `:exclude-files` (regexps), `:exclude-tags`,
  `:exclude-keywords` (e.g. `PROJECT`), habits always excluded.
- A heading is in scope if it has a not-done TODO keyword and no excluded tag
  (inherited tags count), or it has `LINEAR_UUID` (so a heading closed in Org
  still sends its close).
- The index is a hash from UUID to (file . marker) across files, plus new
  headings and duplicates (same UUID twice, e.g. after copying a heading:
  refuse to sync that UUID and log, as today).
- Cost control: list candidate files with a fast text search for TODO lines
  or `LINEAR_UUID`, and cache each file's index by modification time. Visit a
  file only to index it or to apply edits to it.
- A file that is not already visited is opened, edited, saved and its buffer
  killed. A file with unsaved edits is skipped for this sync and logged; its
  issues neither pull nor push until it is saved. A file changed on disk since
  its buffer was read is reverted first if the buffer is unmodified (covers
  edits pulled in by git from another device), otherwise skipped.

### 4. Fields

Same field set as today, minus `:project`. Mapping is per profile:

- Keywords ↔ states: `:keyword-states` and `:state-keywords`, as today. Any
  open keyword without a mapping goes to the team's first `unstarted` state.
- Priorities: `:priorities` maps Org cookies to Linear's 0–4.
- Tags: managed tags only (assignee and labels, as today), plus one label per
  file, `file:<basename>`, so Linear can filter by source. Org tags not
  managed stay untouched.
- Notes: log-drawer notes ↔ comments, as today. State-change log lines are not
  notes; this needs a test against real log drawers.

### 5. Repeating TODOs

A TODO with a repeater is one issue. Completing it in Linear makes the sync
run `org-todo` with the done keyword, so Org applies the repeat (keyword
back to open, date moved forward). The next sync sees an open heading with a
new date and reopens the issue with that date. Completing it in Org does the
same in the opposite order.

### 6. Creation and first import

- New in-scope headings get an issue each, reusing the `LINEAR_PENDING_UUID`
  mechanism so a lost reply never creates a duplicate.
- Creation is throttled to stay well inside Linear's documented API rate
  limits (check the current numbers when implementing; do not hard-code a
  guess). A large first import spreads over several syncs and resumes safely.
- `org-linear-preview` (new, read-only) lists what the next sync would
  create, update and skip, per file, without contacting Linear for writes.

### 7. Safety

- In the in-place layout the sync never deletes heading text. The only edits
  it makes are to a heading's keyword, priority, tags it manages, planning
  dates, body, its own properties and notes.
- Undo history is dropped only in buffers the sync edited, as today, and only
  when they had no unsaved edits.
- Linear credentials follow the existing `api-key-command` pattern.

## Implementation order

1. Profiles refactor with no change in behavior. The existing 92 tests pass
   unchanged, plus tests that two profiles keep separate state and that an
   async callback sees its own profile.
2. Layout functions; the mirror layout routes through them (tests unchanged).
3. In-place scanner, multi-file index and file-cache.
4. In-place merge rules: conflict winner, closed and trashed handling, body
   boundary, file label.
5. Creation throttling and `org-linear-preview`.
6. Inbox insertion of issues created in Linear.
7. Repeaters.
8. Manual and README.

## Verification

| Requirement | Check |
|---|---|
| Mirror layout unchanged | existing ERT suite; one live sync of the existing mirror file shows no changes |
| Two profiles independent | ERT with two fake profiles syncing concurrently |
| Scope and exclusions | ERT over a fixture tree; `org-linear-preview` on real files matches expectations |
| Org wins conflicts | ERT: both sides edit the title → Org title stays, Linear updated, log entry |
| Never deletes text | ERT: trash, close and out-of-scope cases leave heading text intact |
| Unsaved and changed-on-disk files | ERT for skip and revert paths |
| Linear-created issues reach the inbox | ERT; live: create one issue in Linear, see it in the inbox file |
| Repeaters | ERT; live: complete a repeating issue in Linear, see the date advance and the issue reopen |
| First import resumable | ERT: interrupt mid-import, rerun, no duplicates |
| Live acceptance | preview → import one small file → import the rest, checking Linear's issue count against the preview |

## Assumptions (nonblocking)

- The property stays `LINEAR_UUID`, as in the mirror layout.
- No Linear projects in the in-place layout; the `file:` label stands in.
- Headings whose keyword is a container (e.g. `PROJECT`) are excluded by
  config, not by the package.

## Deferred

- Sub-issues from heading nesting.
- Mapping Org files to Linear projects instead of labels.
- Linear recurring-issue templates.
