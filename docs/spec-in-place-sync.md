# Spec: in-place sync of Org TODOs across many files

Status: approved; revised after one independent review (findings 1–4 and 6 accepted as defects; finding 5 decided by the user: trashed → cancelled). Not implemented.

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

Author's design choices, approved with the plan:

5. Multiple configurations are profiles in one package, not separate copies:
   the user keeps the existing mirror setup and adds the in-place one.
6. The mirror layout's behavior does not change; it routes through the new
   layout functions, and its test suite passes unchanged.
7. The in-place sync never deletes heading text: personal notes hold more
   than the task, and Org is the primary copy.
8. A closed issue keeps its heading (done keyword set). A trashed issue's
   heading keeps its text, gets the cancelled keyword and loses its
   `LINEAR_UUID`, so nothing re-creates it (user's choice after review
   finding 5).
9. Each Org file maps to a `file:<basename>` label, not a Linear project:
   projects in Linear need team scoping and add a second hierarchy to keep in
   sync; a label is enough to filter by source.
10. The property stays `LINEAR_UUID`, as in the mirror layout, so code and
    users see one convention.
11. A repeating TODO is one issue whose date and state follow Org's repeat:
    Linear's recurring templates cannot be driven from Org reliably.
12. A file with unsaved edits is skipped for that sync; an unmodified buffer
    whose file changed on disk is reverted first: the user edits these files
    directly and on another device (via git).
13. The first import is throttled, resumable and preceded by a read-only
    preview: hundreds of issues are created at once.
14. Exclusions (files, tags, keywords) are user options with no
    user-specific defaults in the package; habits are always excluded,
    since a habit's repeat history has no Linear counterpart.
15. A description edited in Linear whose Org body does not survive an
    Org → Markdown → Org round trip unchanged is merged by a model, not
    pulled as is (user's decision, after a side agent flagged lossy body
    pulls). See §4a.
16. Out of scope for v1: sub-issues, projects, cycles, estimates, Linear
    recurring templates.

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
| Find issue headings | level-2 headings of one file | in-scope headings across files (§3) |
| New heading → create | level 2 under a project | any in-scope heading without `LINEAR_UUID` |
| Project | parent heading | none |
| Body | to next heading | to next heading of any level (children are not body) |
| New Linear issue | insert under project | insert under inbox heading |
| Closed issue | archive/delete heading | keep heading, set done keyword |
| Trashed issue | delete heading | set cancelled keyword, drop `LINEAR_UUID`, keep text, log |
| Conflict winner | Linear | Org |
| Linked heading gone from Org | n/a | archive the issue in Linear |

### 3. Scope and the file scan (in-place)

- Options: `:directories`, `:exclude-files` (regexps), `:exclude-tags`,
  `:exclude-keywords` (e.g. `PROJECT`), habits always excluded.
- Exclusions always apply. A heading is *excluded* if its file matches
  `:exclude-files`, it has an excluded tag (inherited tags count), an excluded
  keyword, or is a habit. A heading that is not excluded is *in scope* if it
  has a not-done TODO keyword, or it has `LINEAR_UUID` (so a heading closed in
  Org still sends its close). (Review finding 1.)
- *Presence* is separate from scope. Every `LINEAR_UUID` found anywhere the
  scan looks (all `*.org` and `*.org_archive` files in `:directories`,
  excluded files and dirty buffers included) counts as present. A present
  issue is never inserted again, whatever its heading's scope. An excluded or
  skipped heading neither pulls nor pushes; the sync logs it once per change.
  (Review finding 2.)
- An issue with a sync base whose UUID is present nowhere was deleted or moved
  out of the scanned directories in Org: the sync archives it in Linear (an
  undoable action) and drops its base. Only issues with no base (created in
  Linear) go to the inbox.
- The index is a hash from UUID to (file . marker) across files, plus new
  headings and duplicates (same UUID twice, e.g. after copying a heading:
  refuse to sync that UUID and log, as today).
- Cost control: list candidate files with a fast text search for TODO lines
  or `LINEAR_UUID`. The cache keeps no markers: per file it stores the
  modification time, the UUIDs present and whether the file has candidate
  headings, keyed also by a hash of the scope options, so changing an
  exclusion invalidates it. A file is visited and indexed afresh (new
  markers) whenever the sync must read or edit its headings. (Review finding
  4.)
- A file that is not already visited is opened, edited, saved and its buffer
  killed; its markers are dropped with it. A file with unsaved edits is
  skipped for this sync and logged; its issues neither pull nor push until it
  is saved, and no edit is made to its buffer. A file changed on disk since
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

### 4a. Merging description edits

Pulling a Linear description replaces the Org body with pandoc's Markdown →
Org conversion. That loses Org-only markup (internal and ID links,
citations, footnotes, some blocks). So when Linear changed the description
since the last sync:

- If the current Org body survives Org → Markdown → Org unchanged, pull as
  today.
- Otherwise call `org-linear-merge-body-function` (new option, nil by
  default) with the Org body, the Markdown last synced, Linear's new
  Markdown, and a callback. It returns, asynchronously, a new Org body that
  applies Linear's edit and keeps the Org-only markup. The user's config
  supplies a model-backed function (gptel); the package does not depend on
  any model client.
- The sync accepts the result only if it converts to Markdown that matches
  Linear's new description (whitespace-normalized). Then the body is set and
  the base advances.
- If the option is nil, the call fails, or the check fails, the heading is
  left untouched, the base does not advance, the log says why, and the next
  sync tries again. Neither side's text is overwritten meanwhile: the body
  field is held out of the push for that issue until a merge lands or the
  Org body is edited (then both sides changed, and the user's Org edit
  wins under decision 3, with Linear's text kept in the log).
- If the Org body also changed since the last sync, both sides changed:
  decision 3 applies (Org wins, Linear's text goes to the log) and no model
  is called.



A TODO with a repeater is one issue. Completing it in Linear makes the sync
run `org-todo` with the done keyword, so Org applies the repeat (keyword
back to open, date moved forward). The next sync sees an open heading with a
new date and reopens the issue with that date. Completing it in Org does the
same in the opposite order.

The repeat must happen once per completion, even if a sync is cut short
between saving the heading and saving the state (review finding 6). The
sync records on the heading, as a `LINEAR_REPEATED` property, the Linear
completion time it acted on, and saves the file before the state; a later
sync that sees the same completion time does not repeat again and only
reopens the issue with the heading's current date.

### 6. Creation and first import

- New in-scope headings get an issue each. Before the create is sent, the
  heading gets a `LINEAR_PENDING_UUID` and its file is saved; if the file has
  unsaved edits the heading waits for a later sync. The create is sent only
  after that save succeeded, and its answer is applied only if the file is
  still unmodified; otherwise the next sync links the heading by its pending
  UUID, wherever it now is and whatever its title. (Review finding 3.)
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
| Repeaters | ERT both directions, plus a sync cut short after the heading save: one repeat only; live: complete a repeating issue in Linear and another in Org, see each date advance once and the issue reopen |
| Exclusions beat `LINEAR_UUID` | ERT: linked heading gains an excluded tag (own and inherited), keyword, habit, file |
| Present-but-skipped never duplicated | ERT over successive syncs: dirty file, excluded refile, archived to `_archive`, then restored |
| Heading deleted in Org | ERT: issue archived in Linear, not reinserted |
| Trashed in Linear | ERT: heading cancelled, text intact, not re-created next sync |
| Creation safe across async edits | ERT: lost create reply; file edited while a create is in flight; heading retitled and refiled before relinking |
| Cache without markers | ERT: two syncs over initially unvisited files; exclusion change invalidates cache |
| First import resumable | ERT: interrupt mid-import, rerun, no duplicates |
| Lossy body merge | ERT with a fake merge function: lossless body pulled directly; lossy body merged; failing check leaves Org and Linear untouched and retries; nil option logs and holds |
| Live acceptance | preview → import one small file → import the rest, checking Linear's issue count against the preview |

## Assumptions (nonblocking)

(Now also listed as fixed decisions 9, 10 and 14.)

- The property stays `LINEAR_UUID`, as in the mirror layout.
- No Linear projects in the in-place layout; the `file:` label stands in.
- Headings whose keyword is a container (e.g. `PROJECT`) are excluded by
  config, not by the package.

## Deferred

- Sub-issues from heading nesting.
- Mapping Org files to Linear projects instead of labels.
- Linear recurring-issue templates.
