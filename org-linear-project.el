;;; org-linear-project.el --- Projects and milestones in Org -*- lexical-binding: t -*-

;; Copyright (C) 2026 Pablo Stafforini

;; Author: Pablo Stafforini <pablo@stafforini.com>
;; URL: https://github.com/benthamite/org-linear

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Commands for working with projects and milestones as the headings of
;; a profile define them: `org-linear-project-create' adds a project
;; heading with its outcome, target date and parent;
;; `org-linear-roadmap' shows projects with their milestones, dates and
;; progress; and `org-linear-project-context' shows the project and
;; milestone a task serves.  A project heading is one with a keyword of
;; `org-linear-project-keywords', or one linked to a Linear project or
;; initiative; a milestone heading one with a keyword of
;; `org-linear-milestone-keywords', or one linked to a milestone.  In the
;; mirror layout, the projects are those of `org-linear-file'.

;;; Code:

(require 'org-linear)
(require 'org-linear-in-place)

;;;; Profiles and files

(defun org-linear-project--profile (&optional ask)
  "Return the profile whose projects a command works on, nil for the default.
That is the current buffer's, if it has projects, or else the only
profile with projects.  With ASK, or when several profiles have projects
and the buffer belongs to none of them, ask."
  (let* ((candidates (seq-filter #'org-linear-project--profile-has-projects-p
                                 (org-linear--profile-names)))
         ;; A list, since nil, the default profile, is a profile found.
         (own (seq-filter #'org-linear-project--buffer-in-profile-p candidates)))
    (cond
     ((null candidates) (user-error "No profile has projects"))
     (ask (org-linear-project--read-profile candidates))
     (own (car own))
     ((null (cdr candidates)) (car candidates))
     (t (org-linear-project--read-profile candidates)))))

(defun org-linear-project--profile-has-projects-p (name)
  "Return non-nil if profile NAME has projects.
A mirror always has; an in-place profile has when it syncs projects or
milestones."
  (org-linear--with-profile name
    (or (not (eq org-linear-layout 'in-place))
        org-linear-project-keywords org-linear-milestone-keywords)))

(defun org-linear-project--read-profile (names)
  "Ask for one of the profiles NAMES and return it."
  (let ((choices (mapcar (lambda (name) (cons (if name (symbol-name name) "default") name))
                         names)))
    (cdr (assoc (completing-read "Profile: " choices nil t) choices))))

(defun org-linear-project--buffer-in-profile-p (name)
  "Return non-nil if the current buffer's file is in the scope of profile NAME."
  (when-let* ((file (org-linear-project--buffer-file)))
    (org-linear--with-profile name
      (org-linear-project--in-scope-p file))))

(defun org-linear-project--buffer-file ()
  "Return the file of the current buffer, or of the agenda line's heading."
  (let ((buffer (if (derived-mode-p 'org-agenda-mode)
                    (when-let* ((marker (org-get-at-bol 'org-hd-marker)))
                      (marker-buffer marker))
                  (current-buffer))))
    (and buffer (buffer-file-name (buffer-base-buffer buffer)))))

(defun org-linear-project--in-scope-p (file)
  "Return non-nil if FILE is one the profile in effect syncs."
  (let ((file (expand-file-name file)))
    (if (eq org-linear-layout 'in-place)
        (and (string-suffix-p ".org" file)
             (seq-some (lambda (dir) (file-in-directory-p file dir)) org-linear-directories)
             (not (org-linear-in-place--excluded-file-p file)))
      (and org-linear-file (file-equal-p file org-linear-file)))))

(defun org-linear-project--files ()
  "Return the files of the profile in effect that may hold project headings.
In the in-place layout, those are the files the last sync found holding
Linear IDs or headings to create, the files of buffers in scope, and
`org-linear-project-file' and the files in
`org-linear-project-files-directory'; of them, those that hold a project
heading."
  (if (not (eq org-linear-layout 'in-place))
      (and org-linear-file (file-exists-p org-linear-file) (list org-linear-file))
    (org-linear--load-state)
    (let ((files (org-linear-project--candidate-files))
          (regexp (org-linear-project--file-regexp)))
      (seq-filter (lambda (file)
                    (org-linear-project--with-text file
                      (lambda ()
                        (let ((case-fold-search nil))
                          (re-search-forward regexp nil t)))))
                  files))))

(defun org-linear-project--candidate-files ()
  "Return the files in scope that the scan cache or Emacs knows of."
  (let (files)
    (when-let* ((cache (plist-get org-linear--state :files)))
      (maphash (lambda (file entry)
                 (when (or (plist-get entry :uuids) (plist-get entry :new))
                   (push file files)))
               cache))
    (dolist (buffer (buffer-list))
      (when-let* ((file (buffer-file-name buffer)))
        (push file files)))
    (when org-linear-project-file
      (push org-linear-project-file files))
    (when (and org-linear-project-files-directory
               (file-directory-p org-linear-project-files-directory))
      (setq files (nconc (directory-files org-linear-project-files-directory t "\\.org\\'")
                         files)))
    (seq-filter (lambda (file) (and (file-exists-p file) (org-linear-project--in-scope-p file)))
                (delete-dups (mapcar #'expand-file-name files)))))

(defun org-linear-project--file-regexp ()
  "Return a regexp that a file holding a project heading matches."
  (concat "^[ \t]*:LINEAR_\\(?:PENDING_\\)?\\(?:PROJECT\\|INITIATIVE\\):"
          (when org-linear-project-keywords
            (concat "\\|^\\*+ " (regexp-opt org-linear-project-keywords) "\\(?: \\|$\\)"))))

(defun org-linear-project--with-text (file function)
  "Call FUNCTION at the start of an Org buffer holding the text of FILE.
That is the buffer visiting FILE, widened, if there is one, and else a
temporary buffer, so that FILE is not visited.  Return what FUNCTION
returns."
  (if-let* ((buffer (find-buffer-visiting file)))
      (with-current-buffer buffer
        (org-with-wide-buffer
         (goto-char (point-min))
         (funcall function)))
    (with-temp-buffer
      (insert-file-contents file)
      (let ((org-inhibit-startup t))
        (delay-mode-hooks (org-mode))
        (goto-char (point-min))
        (funcall function)))))

;;;; The tree of projects

(defun org-linear-project--forest ()
  "Return the project nodes of the profile in effect that no project holds.
A node is a plist: :kind, `project', `milestone' or `task'; :title;
:done, non-nil if its keyword is a done one; :deadline, an ISO date or
nil; :outcome; :directory, by `org-linear-project-directory-property';
:file, :pos and :end, where its heading and subtree start and end; :id,
its Org ID or nil; :parent, the node of the nearest project or milestone
above it; and :children, the nodes of the nearest headings below it that are
projects, milestones or tasks, in order."
  (seq-mapcat (lambda (file)
                (seq-filter (lambda (node) (eq (plist-get node :kind) 'project))
                            (org-linear-project--with-text
                             file (lambda () (org-linear-project--file-nodes file)))))
              (org-linear-project--files)))

(defun org-linear-project--file-nodes (file)
  "Return the nodes of the current buffer's headings that no node holds.
The buffer holds the text of FILE.
Headings that are neither projects, milestones nor tasks are passed
through: the nodes below them belong to the node above them."
  (let (roots stack)
    (unless (org-at-heading-p)
      (outline-next-heading))
    (while (org-at-heading-p)
      (let ((level (org-outline-level)))
        (while (and stack (>= (car (car stack)) level))
          (pop stack))
        (when-let* ((node (org-linear-project--heading-node file)))
          (let ((above (cdr (car stack))))
            (plist-put node :parent (org-linear-project--parent-of above))
            (if above
                (plist-put above :children (append (plist-get above :children) (list node)))
              (push node roots)))
          (push (cons level node) stack)))
      (outline-next-heading))
    (nreverse roots)))

(defun org-linear-project--parent-of (node)
  "Return NODE if it is a project or milestone, or else its parent."
  (and node (if (eq (plist-get node :kind) 'task) (plist-get node :parent) node)))

(defun org-linear-project--heading-node (file)
  "Return the node of the heading at point, in FILE, or nil if it is no node."
  (let* ((keyword (org-get-todo-state))
         (kind (org-linear-project--heading-kind keyword)))
    (when kind
      (list :kind kind
            :title (substring-no-properties (org-get-heading t t t t))
            :done (and (member keyword org-done-keywords) t)
            :deadline (org-linear--planning-date "DEADLINE")
            :outcome (and (eq kind 'project) (org-linear--heading-outcome))
            :directory (and (eq kind 'project) org-linear-project-directory-property
                            (org-entry-get nil org-linear-project-directory-property))
            :file file :pos (point)
            :end (save-excursion (org-end-of-subtree t t) (point))
            :id (org-entry-get nil "ID")
            :parent nil :children nil))))

(defun org-linear-project--heading-kind (keyword)
  "Return the kind of the heading at point, whose TODO keyword is KEYWORD.
That is `project', `milestone', `task' or nil."
  (cond
   ((or (member keyword org-linear-project-keywords)
        (org-linear-project--has-property-p '("PROJECT" "INITIATIVE")))
    'project)
   ((or (member keyword org-linear-milestone-keywords)
        (org-linear-project--has-property-p '("MILESTONE")))
    'milestone)
   (keyword 'task)))

(defun org-linear-project--has-property-p (kinds)
  "Return non-nil if the heading at point holds the Linear ID of one of KINDS.
KINDS are names such as \"PROJECT\"; pending IDs count."
  (seq-some (lambda (kind)
              (or (org-entry-get nil (concat "LINEAR_" kind))
                  (org-entry-get nil (concat "LINEAR_PENDING_" kind))))
            kinds))

(defun org-linear-project--flatten (nodes kind)
  "Return the nodes of KIND among NODES and the nodes they hold, in order."
  (seq-mapcat (lambda (node)
                (append (and (eq (plist-get node :kind) kind) (list node))
                        (org-linear-project--flatten (plist-get node :children) kind)))
              nodes))

(defun org-linear-project--path (node)
  "Return the titles of NODE and the nodes above it, joined, as a name."
  (let (titles)
    (while node
      (push (plist-get node :title) titles)
      (setq node (plist-get node :parent)))
    (string-join titles " / ")))

(defun org-linear-project--tasks (node)
  "Return the task nodes that NODE holds, at any depth."
  (seq-mapcat (lambda (child)
                (append (and (eq (plist-get child :kind) 'task) (list child))
                        (org-linear-project--tasks child)))
              (plist-get node :children)))

(defun org-linear-project--goto (node)
  "Visit the heading of NODE."
  (find-file (plist-get node :file))
  (widen)
  (goto-char (plist-get node :pos))
  (unless (and (org-at-heading-p)
               (equal (substring-no-properties (org-get-heading t t t t)) (plist-get node :title)))
    (user-error "The heading \"%s\" moved; refresh and try again" (plist-get node :title)))
  (org-fold-show-context)
  (org-fold-show-entry))

;;;; Choosing a project

(defun org-linear-project--read (prompt nodes &optional default extra)
  "Ask with PROMPT for one of the project NODES and return it.
Each is offered by its path.  DEFAULT, a node, is the default.  EXTRA,
a string, is offered first, and returned as is."
  (let* ((choices (mapcar (lambda (node) (cons (org-linear-project--path node) node)) nodes))
         (default-name (if default (org-linear-project--path default) extra))
         (answer (completing-read (if default-name
                                      (format "%s (default %s): " prompt default-name)
                                    (format "%s: " prompt))
                                  (append (and extra (list extra)) (mapcar #'car choices))
                                  nil t nil nil default-name)))
    (if (equal answer extra) extra (cdr (assoc answer choices)))))

(defun org-linear-project--enclosing (nodes)
  "Return the node among NODES of the project that point is in, or nil.
Point is in an Org buffer or on an agenda line."
  (when-let* ((place (org-linear-project--point-place)))
    (let ((projects (org-linear-project--flatten nodes 'project))
          best)
      (dolist (node projects)
        (when (and (org-linear-project--holds-p node place)
                   (or (null best) (> (plist-get node :pos) (plist-get best :pos))))
          (setq best node)))
      best)))

(defun org-linear-project--holds-p (node place)
  "Return non-nil if the subtree of NODE's heading holds PLACE, (FILE . POS)."
  (and (file-equal-p (plist-get node :file) (car place))
       (<= (plist-get node :pos) (cdr place))
       (< (cdr place) (plist-get node :end))))

(defun org-linear-project--point-place ()
  "Return (FILE . POS) for point in an Org buffer, or an agenda line's heading."
  (cond
   ((derived-mode-p 'org-agenda-mode)
    (when-let* ((marker (org-get-at-bol 'org-hd-marker))
                (file (buffer-file-name (buffer-base-buffer (marker-buffer marker)))))
      (cons (expand-file-name file) (marker-position marker))))
   ((and (derived-mode-p 'org-mode) (org-linear-project--buffer-file))
    (cons (expand-file-name (org-linear-project--buffer-file)) (point)))))

;;;; Creating a project

;;;###autoload
(defun org-linear-project-create (&optional ask-profile)
  "Add a project, asking for its name, outcome, parent and target date.
The outcome goes in `org-linear-outcome-property' and the target date in
the heading's deadline.  With a prefix argument ASK-PROFILE, ask for
the profile.

In the in-place layout, where Org is the primary copy, the heading gets
the first of `org-linear-project-keywords' and goes under the parent you
choose, as its last subheading, or else in a file of its own in
`org-linear-project-files-directory', or without that option at the end
of `org-linear-project-file', or of the current file if that is nil too.
With `org-linear-project-directory-property' set, it can also name the
project's directory.  The file is saved, and the next sync creates the
project in Linear.

In the mirror layout, where Linear is the primary copy and projects are
the top-level headings of `org-linear-file', the project is created in
Linear at once, in the team you choose if there are several, and its
heading is added at the end of the file, which is saved.  A mirrored
project has no parent and no directory."
  (interactive "P")
  (let ((profile (org-linear-project--profile ask-profile)))
    (org-linear--with-profile profile
      (if (eq org-linear-layout 'in-place)
          (org-linear-project--create-in-place)
        (org-linear-project--create-in-linear)))))

(defun org-linear-project--create-in-place ()
  "Add a project heading in the in-place layout, asking for its fields."
  (unless org-linear-project-keywords
    (user-error "Creating projects in place needs `org-linear-project-keywords'"))
  (let* ((nodes (org-linear-project--forest))
         (open (seq-remove (lambda (node) (plist-get node :done))
                           (org-linear-project--flatten nodes 'project)))
         (name (org-linear-project--read-name))
         (outcome (org-linear-project--read-outcome))
         (parent (org-linear-project--read-parent open (org-linear-project--enclosing nodes)))
         (deadline (org-linear-project--read-date))
         (directory (org-linear-project--read-directory nodes)))
    (org-linear-project--insert
     (list :name name :outcome outcome :parent parent :deadline deadline
           :directory directory))
    (message "Added the project \"%s\"; the next sync creates it in Linear" name)))

(defun org-linear-project--create-in-linear ()
  "Create a project in Linear for the mirror layout, asking for its fields.
Its heading is added to `org-linear-file' once Linear confirms it."
  (let ((buffer (find-file-noselect org-linear-file)))
    (when (buffer-modified-p buffer)
      (user-error "Save %s first" (buffer-name buffer)))
    (let* ((team (org-linear-project--read-team))
           (spec (list :name (org-linear-project--read-name)
                       :outcome (org-linear-project--read-outcome)
                       :deadline (org-linear-project--read-date)))
           (view (list :title (plist-get spec :name) :body ""
                       :deadline (plist-get spec :deadline)
                       :outcome (plist-get spec :outcome))))
      (message "Creating the project \"%s\" in Linear..." (plist-get spec :name))
      (org-linear--graphql
       (org-linear-in-place--mutation 'project 'create)
       `((input . ,(org-linear-in-place--create-input
                    (list :kind 'project :pending (org-id-uuid) :team team :view view) "")))
       (org-linear--bound
        (lambda (data)
          (let* ((object (org-linear-in-place--object
                          'project (org-linear-in-place--payload 'project :create data)))
                 (id (plist-get object :id)))
            (org-linear-project--record-mirrored object)
            (org-linear-project--insert-mirrored buffer id spec)
            (message "Created the project \"%s\" in Linear" (plist-get spec :name)))))
       (lambda (message &rest _)
         (message "Linear did not create the project \"%s\": %s"
                  (plist-get spec :name) message))))))

(defun org-linear-project--read-team ()
  "Return the ID of the team a new project goes in, asking if there are several.
The teams are those the last sync read; signal an error before any."
  (let ((teams (or (plist-get org-linear--meta :teams)
                   (user-error "Sync once first, so that the teams are known"))))
    (alist-get 'id (if (cdr teams)
                       (let ((key (completing-read "Team: " (mapcar (lambda (team) (alist-get 'key team))
                                                                    teams)
                                                   nil t)))
                         (seq-find (lambda (team) (equal (alist-get 'key team) key)) teams))
                     (car teams)))))

(defun org-linear-project--record-mirrored (object)
  "Record OBJECT, a project just created, as last synced.
Without this, the next sync would take its heading's fields, as edited
meanwhile, for the last synced ones, and Linear's values would replace
them."
  (puthash (plist-get object :id)
           (list :kind 'project :outcome (plist-get object :outcome)
                 :deadline (plist-get object :deadline) :edited (plist-get object :edited))
           (org-linear--state-objects))
  (org-linear--save-state))

(defun org-linear-project--insert-mirrored (buffer id spec)
  "Add to BUFFER, the mirrored file, the heading of the new project ID.
SPEC gives its :name, :outcome and :deadline.  BUFFER is saved."
  (with-current-buffer buffer
    (org-with-wide-buffer
     (goto-char (point-max))
     (unless (bolp) (insert "\n"))
     (insert "* " (plist-get spec :name) "\n:PROPERTIES:\n:LINEAR_PROJECT: " id "\n:END:\n")
     (org-back-to-heading t)
     (org-linear-project--fill-heading spec))
    (save-buffer)))

(defun org-linear-project--read-outcome ()
  "Ask for the outcome of a new project and return it, or nil for none."
  (org-linear--outcome-value
   (read-string "Outcome, the end state it aims at (empty for none): ")))

(defun org-linear-project--read-name ()
  "Ask for the name of a new project and return it."
  (let ((name (string-trim (read-string "Project name: "))))
    (when (string-empty-p name)
      (user-error "A project needs a name"))
    name))

(defun org-linear-project--read-parent (nodes default)
  "Ask for the parent of a new project among NODES; nil means none.
DEFAULT is the node offered first."
  (let ((answer (org-linear-project--read "Parent project" nodes default "(none)")))
    (unless (equal answer "(none)") answer)))

(defun org-linear-project--read-date ()
  "Ask for a target date and return it as an ISO date, or nil for none.
The answer is read as `org-read-date' reads it, so \"+2w\" and \"dec
15\" work."
  (let ((answer (string-trim (read-string "Target date (empty for none): "))))
    (unless (string-empty-p answer)
      (let ((org-read-date-force-compatible-dates nil))
        (org-read-date nil nil answer)))))

(defun org-linear-project--read-directory (nodes)
  "Ask for the directory of a new project, if projects name one.
Return it abbreviated, or nil.  It must be no other project's among
NODES.  See `org-linear-project-directory-property'."
  (when (and org-linear-project-directory-property
             (y-or-n-p "Does the project have a directory of its own? "))
    (let ((directory (abbreviate-file-name
                      (directory-file-name (read-directory-name "Directory: " nil nil t)))))
      (org-linear-project--check-directory directory nodes)
      directory)))

(defun org-linear-project--check-directory (directory nodes)
  "Signal an error if a project among NODES already names DIRECTORY."
  (dolist (node (org-linear-project--flatten nodes 'project))
    (let ((other (plist-get node :directory)))
      (when (and other (file-equal-p other directory))
        (user-error "The project \"%s\" already has the directory %s"
                    (org-linear-project--path node) directory)))))

(defun org-linear-project--insert (spec)
  "Insert the project heading SPEC describes, save its file and show it.
SPEC is a plist: :name, :outcome, :parent (a node or nil), :deadline
and :directory."
  (let ((parent (plist-get spec :parent)))
    (if parent
        (org-linear-project--goto parent)
      (org-linear-project--visit-top-file spec))
    (let ((level (if parent (1+ (org-outline-level)) 1)))
      (if parent
          (org-end-of-subtree t t)
        (widen)
        (goto-char (point-max)))
      (unless (bolp) (insert "\n"))
      (insert (make-string level ?*) " " (car org-linear-project-keywords) " "
              (plist-get spec :name) "\n")
      (forward-line -1)
      (org-linear-project--fill-heading spec)
      (save-buffer)
      (org-fold-show-entry))))

(defun org-linear-project--visit-top-file (spec)
  "Visit the file the project SPEC describes, which has no parent, goes in.
A file of its own that does not exist yet gets the project's name as
its title."
  (let ((file (org-linear-project--top-file spec)))
    (make-directory (file-name-directory file) t)
    (find-file file)
    (when (and org-linear-project-files-directory (= (point-min) (point-max)))
      (insert "#+title: " (plist-get spec :name) "\n"))))

(defun org-linear-project--top-file (spec)
  "Return the file the project SPEC describes, which has no parent, goes in.
Signal an error unless the profile in effect syncs it."
  (let ((file (or (and org-linear-project-files-directory
                       (expand-file-name (concat (org-linear-project--file-name spec) ".org")
                                         org-linear-project-files-directory))
                  org-linear-project-file (org-linear-project--buffer-file)
                  (user-error "Set `org-linear-project-file', or call this from an Org file"))))
    (unless (org-linear-project--in-scope-p file)
      (user-error "%s is not among the files this profile syncs" file))
    file))

(defun org-linear-project--file-name (spec)
  "Return the name, without extension, of the file of its own for SPEC.
See `org-linear-project-files-directory'."
  (let* ((directory (plist-get spec :directory))
         (name (if directory
                   (string-trim-left
                    (file-name-nondirectory (directory-file-name directory)) "\\.+")
                 (string-trim (replace-regexp-in-string
                               "[^[:alnum:]]+" "-" (downcase (plist-get spec :name)))
                              "-+" "-+"))))
    (if (string-empty-p name) "project" name)))

(defun org-linear-project--fill-heading (spec)
  "Give the heading at point the deadline, outcome and directory of SPEC."
  (when (plist-get spec :deadline)
    (org-linear--set-planning 'deadline (plist-get spec :deadline)))
  (when (plist-get spec :outcome)
    (org-linear--set-heading-outcome (plist-get spec :outcome)))
  (when (plist-get spec :directory)
    (org-entry-put nil org-linear-project-directory-property (plist-get spec :directory))))

;;;; Context

;;;###autoload
(defun org-linear-project-context ()
  "Show the milestone and projects the heading at point serves; go to one.
The heading is the one at point in an Org buffer or on an agenda line.
The prompt lists the nearest milestone and each project above it, the
nearest first, with its outcome and target date; the one you choose is
visited.  On a milestone or project heading, the list starts above it:
a milestone serves its project, and a subproject the projects above
it.  A top-level project serves none, which the echo area says, with its
outcome."
  (interactive)
  (let ((place (or (org-linear-project--point-place)
                   (user-error "Point is on no Org heading"))))
    (org-linear--with-profile (org-linear-project--profile)
      (let* ((node (or (org-linear-project--node-at (org-linear-project--forest) place)
                       (user-error "The heading at point is in no project")))
             (chain (org-linear-project--chain-from (plist-get node :parent)))
             (choices (mapcar (lambda (above) (cons (org-linear-project--describe above) above))
                              chain)))
        (if (null chain)
            (message "%s is a top-level project; it serves no other"
                     (org-linear-project--describe node))
          (org-linear-project--goto
           (cdr (assoc (completing-read "Serves, nearest first: " choices nil t nil nil
                                        (caar choices))
                       choices))))))))

(defun org-linear-project--chain-from (node)
  "Return NODE and the nodes above it, the nearest first."
  (let (chain)
    (while node
      (push node chain)
      (setq node (plist-get node :parent)))
    (nreverse chain)))

(defun org-linear-project--node-at (nodes place)
  "Return the node among NODES whose subtree holds PLACE most closely, or nil."
  (let (best)
    (dolist (node (seq-mapcat (lambda (root)
                                (cons root (org-linear-project--descendants root)))
                              nodes))
      (when (and (org-linear-project--holds-p node place)
                 (or (null best) (> (plist-get node :pos) (plist-get best :pos))))
        (setq best node)))
    best))

(defun org-linear-project--descendants (node)
  "Return the nodes below NODE, at any depth."
  (seq-mapcat (lambda (child) (cons child (org-linear-project--descendants child)))
              (plist-get node :children)))

(defun org-linear-project--describe (node)
  "Return a line naming NODE, with its kind, target date and outcome."
  (concat (if (eq (plist-get node :kind) 'milestone) "Milestone: " "Project: ")
          (plist-get node :title)
          (if-let* ((deadline (plist-get node :deadline)))
              (format " (due %s)" deadline)
            "")
          (if-let* ((outcome (plist-get node :outcome)))
              (format " — %s" outcome)
            "")))

;;;; The roadmap

(defvar-local org-linear-roadmap--profile nil
  "The profile whose projects the roadmap buffer shows, nil for the default.")

(defvar-local org-linear-roadmap--root nil
  "The ID or path of the project the roadmap buffer shows, or nil for all.")

(defvar-local org-linear-roadmap--show-tasks nil
  "Non-nil if the roadmap buffer lists the open tasks of each milestone.")

(defvar-keymap org-linear-roadmap-mode-map
  :doc "Keymap of `org-linear-roadmap-mode'."
  "RET" #'org-linear-roadmap-visit
  "o" #'org-linear-roadmap-browse
  "t" #'org-linear-roadmap-toggle-tasks
  "n" #'next-line
  "p" #'previous-line)

(define-derived-mode org-linear-roadmap-mode special-mode "Roadmap"
  "Major mode of the roadmap that `org-linear-roadmap' shows.
Each line's heading can be visited, or its project opened in Linear;
the open tasks can be listed, and the files read again.

\\{org-linear-roadmap-mode-map}"
  (setq-local revert-buffer-function #'org-linear-roadmap--revert)
  (setq truncate-lines t))

;;;###autoload
(defun org-linear-roadmap (&optional ask-profile)
  "Show a project's milestones and subprojects, with dates and progress.
Ask for the project, the one at point by default; the first choice shows
every open project.  Each line gives a project or milestone, its target
date and the days left, its done and total tasks, and a warning when it
is overdue, due after its project, or open with no open task; see
`org-linear-roadmap-mode' for the keys.  With a prefix argument
ASK-PROFILE, ask for the profile."
  (interactive "P")
  (let ((profile (org-linear-project--profile ask-profile)))
    (org-linear--with-profile profile
      (let* ((nodes (org-linear-project--forest))
             (all "(all open projects)")
             (choice (org-linear-project--read
                      "Roadmap of" (org-linear-project--flatten nodes 'project)
                      (org-linear-project--enclosing nodes) all)))
        (with-current-buffer (get-buffer-create "*org-linear roadmap*")
          (org-linear-roadmap-mode)
          (setq org-linear-roadmap--profile profile
                org-linear-roadmap--root (unless (equal choice all)
                                           (org-linear-roadmap--key choice)))
          (org-linear-roadmap--render nodes)
          (pop-to-buffer (current-buffer)))))))

(defun org-linear-roadmap--key (node)
  "Return what finds NODE again after its file changes: its ID, else its path."
  (or (plist-get node :id) (org-linear-project--path node)))

(defun org-linear-roadmap--revert (&rest _)
  "Read the files again and show the roadmap anew."
  (org-linear--with-profile org-linear-roadmap--profile
    (org-linear-roadmap--render (org-linear-project--forest))))

(defun org-linear-roadmap-toggle-tasks ()
  "Show or hide the open tasks of each milestone and project."
  (interactive)
  (setq org-linear-roadmap--show-tasks (not org-linear-roadmap--show-tasks))
  (revert-buffer))

(defun org-linear-roadmap--render (nodes)
  "Show in the current buffer the roadmap of the root project among NODES."
  (let* ((roots (if org-linear-roadmap--root
                    (list (or (seq-find (lambda (node)
                                          (equal (org-linear-roadmap--key node)
                                                 org-linear-roadmap--root))
                                        (org-linear-project--flatten nodes 'project))
                              (user-error "The project is no longer there")))
                  (seq-remove (lambda (node) (plist-get node :done)) nodes)))
         (line (line-number-at-pos))
         (inhibit-read-only t))
    (erase-buffer)
    (insert (propertize (format "Roadmap, %s\n" (format-time-string "%Y-%m-%d"))
                        'face 'org-document-title))
    (dolist (root (org-linear-roadmap--sorted roots))
      (insert "\n")
      (org-linear-roadmap--insert-node root 0 nil))
    (goto-char (point-min))
    (forward-line (1- line))))

(defun org-linear-roadmap--sorted (nodes)
  "Return NODES by target date, those without one last.
Nodes with the same date keep their order."
  (seq-sort-by (lambda (node) (or (plist-get node :deadline) "9999"))
               #'string< nodes))

(defun org-linear-roadmap--insert-node (node depth project)
  "Insert the line of NODE, at DEPTH, and those of what it holds.
PROJECT is the node of the project it belongs to, or nil."
  (org-linear-roadmap--insert-line node depth project)
  (when (and (eq (plist-get node :kind) 'project) (plist-get node :outcome))
    (insert (make-string (* 2 (1+ depth)) ?\s)
            (propertize (concat "Outcome: " (plist-get node :outcome)) 'face 'italic)
            "\n"))
  (let* ((children (plist-get node :children))
         (milestones (org-linear-roadmap--sorted
                      (seq-filter (lambda (n) (eq (plist-get n :kind) 'milestone)) children)))
         (projects (org-linear-roadmap--sorted
                    (seq-filter (lambda (n) (and (eq (plist-get n :kind) 'project)
                                                 (not (plist-get n :done))))
                                children)))
         (owner (if (eq (plist-get node :kind) 'project) node project)))
    (dolist (child milestones)
      (org-linear-roadmap--insert-node child (1+ depth) owner))
    (dolist (child projects)
      (org-linear-roadmap--insert-node child (1+ depth) owner))
    (org-linear-roadmap--insert-loose node depth)))

(defun org-linear-roadmap--insert-loose (node depth)
  "Insert the open tasks NODE holds outside its milestones and subprojects.
They are counted on one line, and listed under it when tasks are shown;
in a milestone they are listed only."
  (let ((loose (seq-remove (lambda (task) (plist-get task :done))
                           (org-linear-roadmap--direct-tasks node)))
        (indent (make-string (* 2 (1+ depth)) ?\s)))
    (when (and loose (eq (plist-get node :kind) 'project)
               (seq-some (lambda (n) (memq (plist-get n :kind) '(milestone project)))
                         (plist-get node :children)))
      (insert indent (propertize (format "%d open %s in no milestone\n" (length loose)
                                         (if (cdr loose) "tasks" "task"))
                                 'face 'shadow)))
    (when org-linear-roadmap--show-tasks
      (dolist (task loose)
        (insert (propertize (concat indent "- " (plist-get task :title)
                                    (if-let* ((deadline (plist-get task :deadline)))
                                        (format "  (due %s)" deadline)
                                      "")
                                    "\n")
                            'org-linear-node task))))))

(defun org-linear-roadmap--direct-tasks (node)
  "Return the tasks under NODE that no milestone or project between holds."
  (seq-mapcat (lambda (child)
                (when (eq (plist-get child :kind) 'task)
                  (cons child (org-linear-project--tasks child))))
              (plist-get node :children)))

(defun org-linear-roadmap--insert-line (node depth project)
  "Insert the line of NODE at DEPTH; PROJECT is the project it belongs to."
  (let* ((tasks (org-linear-project--tasks node))
         (done (seq-count (lambda (task) (plist-get task :done)) tasks))
         (kind (plist-get node :kind))
         (start (point)))
    (insert (make-string (* 2 depth) ?\s)
            (propertize (if (eq kind 'milestone) "◆ " "■ ") 'face 'shadow)
            (propertize (plist-get node :title)
                        'face (cond ((plist-get node :done) 'org-done)
                                    ((eq kind 'project) 'org-level-1)
                                    (t 'org-level-2))))
    (insert (org-linear-roadmap--columns node done (length tasks)))
    (when-let* ((warnings (org-linear-roadmap--warnings node project (- (length tasks) done))))
      (insert "  " (propertize (string-join warnings ", ") 'face 'warning)))
    (insert "\n")
    (put-text-property start (point) 'org-linear-node node)))

(defun org-linear-roadmap--columns (node done total)
  "Return the target date, days left and progress of NODE.
DONE of its TOTAL tasks are done."
  (let ((deadline (plist-get node :deadline)))
    (concat
     (propertize " " 'display '(space :align-to 60))
     (if deadline
         (format "%s %5s" deadline
                 (if (plist-get node :done) "" (format "%dd" (org-linear-roadmap--days-left deadline))))
       (format "%16s" "no date"))
     (format "  %3d/%-3d " done total)
     (org-linear-roadmap--bar done total))))

(defun org-linear-roadmap--days-left (date)
  "Return the days from today to DATE, an ISO date; negative when it is past."
  (- (time-to-days (date-to-time (concat date "T12:00:00")))
     (time-to-days nil)))

(defun org-linear-roadmap--bar (done total)
  "Return a bar of ten cells, DONE of TOTAL of them filled."
  (let ((filled (if (zerop total) 0 (round (* 10.0 done) total))))
    (concat (propertize (make-string filled ?█) 'face 'success)
            (propertize (make-string (- 10 filled) ?░) 'face 'shadow))))

(defun org-linear-roadmap--warnings (node project open)
  "Return the warnings about NODE, of PROJECT, which has OPEN open tasks."
  (let ((deadline (plist-get node :deadline))
        (project-deadline (and project (plist-get project :deadline))))
    (unless (plist-get node :done)
      (delq nil
            (list (and deadline (< (org-linear-roadmap--days-left deadline) 0) "overdue")
                  (and deadline project-deadline (string< project-deadline deadline)
                       "due after its project")
                  (and (eq (plist-get node :kind) 'milestone) (zerop open)
                       "no open tasks"))))))

(defun org-linear-roadmap--node-at-point ()
  "Return the node of the roadmap line at point."
  (or (get-text-property (line-beginning-position) 'org-linear-node)
      (user-error "No project, milestone or task on this line")))

(defun org-linear-roadmap-visit ()
  "Visit the heading of the line at point."
  (interactive)
  (org-linear-project--goto (org-linear-roadmap--node-at-point)))

(defun org-linear-roadmap-browse ()
  "Open in Linear the project of the line at point, or the project above it."
  (interactive)
  (let* ((node (org-linear-roadmap--node-at-point))
         (project (seq-find (lambda (n) (eq (plist-get n :kind) 'project))
                            (org-linear-project--chain-from node)))
         (ids (org-linear-project--with-text (plist-get project :file)
                (lambda ()
                  (goto-char (plist-get project :pos))
                  (cons (org-entry-get nil "LINEAR_PROJECT")
                        (org-entry-get nil "LINEAR_INITIATIVE"))))))
    (unless (or (car ids) (cdr ids))
      (user-error "The project \"%s\" is not in Linear yet" (plist-get project :title)))
    (org-linear--with-profile org-linear-roadmap--profile
      (org-linear-roadmap--browse-object (if (car ids) "project" "initiative")
                                         (or (car ids) (cdr ids))))))

(defun org-linear-roadmap--browse-object (kind id)
  "Ask Linear for the URL of the KIND object with ID, and open it."
  (org-linear--graphql
   (format "query OrgLinearUrl($id: String!) { %s(id: $id) { url } }" kind)
   `((id . ,id))
   (org-linear--bound
    (lambda (data)
      (browse-url (alist-get 'url (alist-get (intern kind) data)))))
   (lambda (message &rest _)
     (message "Could not get the project's address from Linear: %s" message))))

(provide 'org-linear-project)
;;; org-linear-project.el ends here
