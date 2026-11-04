# `org-linear`: Two-way sync between Linear and Org mode

## Overview

`org-linear` keeps the issues of one or more [Linear](https://linear.app) teams in an Org file and syncs the two in both directions. You work on issues with the Org commands you already use, and your colleagues' changes in Linear reach the file at the next sync.

- **Issues as headings.** Each project is a top-level heading and each issue a heading under it. Workflow states are TODO keywords, due dates are deadlines, the assignee and labels are tags, priorities are cookies, the description is the body and comments are notes.
- **Edit with Org.** Change a keyword to move an issue along, set a deadline, add a note with `C-c C-z` to comment, refile to move it to another project, or add a heading to create an issue.
- **Safe merging.** The sync remembers each issue as it was at the last sync and merges three-way, field by field. A change made in Linear meanwhile is never overwritten; when both sides changed the same field, Linear wins and your version is kept in a log.
- **In the background.** It syncs on save, on visit and every few minutes. All network work runs in subprocesses, so Emacs never waits on Linear.
- **In your agenda.** Your issues' deadlines appear with the rest of your tasks, with issues assigned to others hidden if you like.

It is for people who live in Emacs and Org but whose team tracks work in Linear.

## Installation

`org-linear` requires Emacs 29.1 or later and Org 9.7 or later, plus the `curl` and [`pandoc`](https://pandoc.org) programs.

With Elpaca:

```emacs-lisp
(use-package org-linear
  :ensure (:host github :repo "benthamite/org-linear"))
```

With straight.el:

```emacs-lisp
(use-package org-linear
  :straight (:host github :repo "benthamite/org-linear"))
```

With `use-package`'s built-in `:vc` keyword (Emacs 30 or later):

```emacs-lisp
(use-package org-linear
  :vc (:url "https://github.com/benthamite/org-linear"))
```

## Quick start

1. In Linear, create a personal API key (Settings, Security & access, Personal API keys) with access to your teams.
2. Store it where `auth-source` finds it, for example in `~/.authinfo.gpg`:

   ```
   machine api.linear.app password lin_api_YOURKEY
   ```

   Or set `org-linear-api-key-command` to a command that prints it, such as your password manager's.
3. Tell the package which teams to mirror and turn on syncing:

   ```emacs-lisp
   (use-package org-linear
     :ensure (:host github :repo "benthamite/org-linear")
     :custom
     (org-linear-teams '("ENG"))
     :config
     (org-linear-auto-sync-mode))
   ```

4. Run `M-x org-linear-my-issues` to see your open issues; the first sync fills them in the background. `M-x org-linear-menu` gathers the package's commands in a transient menu, and `M-x org-linear-show-log` shows conflicts and errors.

## Documentation

For detailed usage and reference documentation, see the [manual](https://stafforini.com/notes/org-linear/).

## License

`org-linear` is free software under the GNU General Public License, version 3 or later. See [COPYING.txt](COPYING.txt).
