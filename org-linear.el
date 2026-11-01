;;; org-linear.el --- Two-way sync between Linear and an Org file -*- lexical-binding: t -*-

;; Copyright (C) 2026 Pablo Stafforini

;; Author: Pablo Stafforini
;; URL: https://github.com/benthamite/org-linear
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.1") (org "9.7"))
;; Keywords: outlines, convenience, tools

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Mirror the issues of one or more Linear teams in an Org file and keep the
;; two in sync in both directions.  Linear stays the authoritative copy; the
;; file is a mirror you edit with ordinary Org commands.  Each project is a
;; level-1 heading and each issue a level-2 heading under it.  Workflow states
;; are TODO keywords, due dates are deadlines, the assignee and labels are
;; tags, priorities are cookies, the description is the body (converted from
;; and to Markdown by pandoc) and comments are notes in the log drawer.
;;
;; A sync fetches the issues, merges them three-way against the state recorded
;; at the previous sync, applies Linear's changes to the file and sends the
;; file's changes to Linear.  When both sides changed the same field, Linear's
;; value wins and yours is kept in the sync log.  Before each write the issue's
;; last edit is checked, so a change made in Linear meanwhile is never
;; overwritten.  All network work runs in subprocesses.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'seq)
(require 'subr-x)

(declare-function auth-source-search "auth-source")

;;;; User options

(defgroup org-linear ()
  "Two-way sync between Linear and an Org file."
  :group 'org
  :prefix "org-linear-")

(defcustom org-linear-teams nil
  "Keys of the Linear teams whose issues the file mirrors, such as (\"ENG\")."
  :type '(repeat string)
  :group 'org-linear)

(defcustom org-linear-file (expand-file-name "linear.org" org-directory)
  "Org file that mirrors the issues."
  :type 'file
  :group 'org-linear)

(defcustom org-linear-file-header
  "#+TITLE: Linear
#+STARTUP: overview logdrawer
#+PRIORITIES: A D D
#+TODO: TRIAGE(r) BACKLOG(b) TODO(t) DOING(g) | DONE(d) CANCELLED(c@)
"
  "Header written to a new `org-linear-file'.
Its TODO line must list every keyword that `org-linear-type-keywords' and
`org-linear-state-keywords' produce, and its priorities must cover
`org-linear-priorities'.  Notes must go into a drawer, here through the
`logdrawer' startup option, or they would count as part of the description."
  :type 'string
  :group 'org-linear)

(defcustom org-linear-api-key nil
  "Your Linear API key, a function that returns it, or nil.
When nil, and `org-linear-api-key-command' is nil too, the key is read
with `auth-source' for the host \"api.linear.app\".  A function is called
at the start of each sync, so it should not block for long."
  :type '(choice (const :tag "Use auth-source" nil) string function)
  :group 'org-linear)

(defcustom org-linear-api-key-command nil
  "Command, as a list of strings, that prints the Linear API key, or nil.
It runs asynchronously at the start of each sync, so it may be slow, as
reading from a password manager can be.  It takes precedence over
`org-linear-api-key'."
  :type '(choice (const nil) (repeat string))
  :group 'org-linear)

(defcustom org-linear-api-key-forget-command nil
  "Command, as a list of strings, that drops a cached API key, or nil.
When Linear rejects the key, the sync runs this command and retries once,
so that a key cached by `org-linear-api-key-command' is read afresh."
  :type '(choice (const nil) (repeat string))
  :group 'org-linear)

(defcustom org-linear-state-file
  (expand-file-name "org-linear/state.eld"
                    (or (getenv "XDG_STATE_HOME") "~/.local/state"))
  "File holding each issue as it was at the last sync."
  :type 'file
  :group 'org-linear)

(defcustom org-linear-type-keywords
  '(("triage" . "TRIAGE")
    ("backlog" . "BACKLOG")
    ("unstarted" . "TODO")
    ("started" . "DOING")
    ("completed" . "DONE")
    ("canceled" . "CANCELLED")
    ("duplicate" . "CANCELLED"))
  "Linear workflow state types and the TODO keywords that stand for them."
  :type '(alist :key-type string :value-type string)
  :group 'org-linear)

(defcustom org-linear-state-keywords nil
  "Workflow state names and their TODO keywords, overriding the state's type.
For example \\='((\"Waiting\" . \"WAITING\")) gives a state named Waiting
its own keyword."
  :type '(alist :key-type string :value-type string)
  :group 'org-linear)

(defcustom org-linear-keyword-states nil
  "TODO keywords and the workflow state name each one is sent as.
A keyword not listed here is sent as the first state of the issue's team
that maps to it, in Linear's order of state types and positions."
  :type '(alist :key-type string :value-type string)
  :group 'org-linear)

(defcustom org-linear-priorities
  '((1 . "A") (2 . "B") (3 . "C") (4 . "D"))
  "Linear priorities (1 urgent, 2 high, 3 medium, 4 low) and their Org cookies.
Linear's \"no priority\" is no cookie.  A cookie not listed here is sent
as the priority of the nearest listed cookie above it."
  :type '(alist :key-type integer :value-type string)
  :group 'org-linear)

(defcustom org-linear-user-tags nil
  "Linear user names and the tags that stand for them.
A user not listed here gets the first word of their name in lower case."
  :type '(alist :key-type string :value-type string)
  :group 'org-linear)

(defcustom org-linear-closed-retention-days 7
  "Days a closed issue stays in the file after it was closed."
  :type 'integer
  :group 'org-linear)

(defcustom org-linear-pull-interval 600
  "Seconds between background syncs in `org-linear-auto-sync-mode'."
  :type 'integer
  :group 'org-linear)

(defcustom org-linear-hide-others-in-agenda t
  "Non-nil means `org-linear-agenda-skip' hides issues assigned to others."
  :type 'boolean
  :group 'org-linear)

(defcustom org-linear-agenda-keep-tags nil
  "Tags that keep an issue in the agenda even when it is assigned to others.
For example (\"joint\"), for a team that labels work it shares."
  :type '(repeat string)
  :group 'org-linear)

(defcustom org-linear-pandoc-program "pandoc"
  "Pandoc executable used to convert between Markdown and Org."
  :type 'string
  :group 'org-linear)

(defcustom org-linear-curl-program "curl"
  "Curl executable used to talk to Linear's API."
  :type 'string
  :group 'org-linear)

(defcustom org-linear-timeout 60
  "Seconds after which a request to Linear is abandoned."
  :type 'integer
  :group 'org-linear)

(defconst org-linear-api-url "https://api.linear.app/graphql"
  "Linear's GraphQL endpoint.")

;;;; Internal state

(defconst org-linear--fields
  '(:title :todo :priority :tags :deadline :project :body)
  "Issue fields merged between the file and Linear.")

(defconst org-linear--type-order
  '("triage" "backlog" "unstarted" "started" "completed" "canceled" "duplicate")
  "Linear's state types in workflow order.")

(defvar org-linear--state nil
  "Sync state, a plist.
:tasks is a hash table from issue ID to the issue as it was at the last
sync; :rejected-adds lists new issues Linear refused to create; :me-tag,
:user-tags and :label-tags record the tags of the last sync, for the
agenda.")

(defvar org-linear--meta nil
  "The teams, states, users, labels and projects of the sync in progress.")

(defvar org-linear--api-key-cache nil
  "The API key, held only while a sync runs.")

(defvar org-linear--busy nil
  "Non-nil while a sync runs.")

(defvar org-linear--pending nil
  "Non-nil when a sync was requested while another one ran.")

(defvar org-linear--saving nil
  "Non-nil while the sync itself saves the file.")

(defvar org-linear--timer nil
  "Timer of `org-linear-auto-sync-mode'.")

(defvar org-linear--stats nil
  "Counts for the sync in progress, a plist.")

(defvar org-linear--rerun nil
  "Non-nil when Linear refused a write and the sync must run again.")

(defvar org-linear--reruns 0
  "Consecutive reruns caused by refused writes.")

(defconst org-linear--log-buffer "*org-linear*"
  "Buffer that records conflicts, refusals and errors.")

;;;; Commands

;;;###autoload
(defun org-linear-sync ()
  "Sync `org-linear-file' with Linear in both directions.
Save the file first, so that its edits are sent."
  (interactive)
  (unless org-linear-teams
    (user-error "Set `org-linear-teams' to the keys of the teams to mirror"))
  (if org-linear--busy
      (setq org-linear--pending t)
    (setq org-linear--busy t
          org-linear--pending nil
          org-linear--rerun nil
          org-linear--stats (list :pulled 0 :pushed 0 :created 0
                                  :conflicts 0 :errors 0))
    (condition-case err
        (org-linear--start)
      (error (org-linear--fail "%s" (error-message-string err))))))

(defun org-linear--start ()
  "Save the file if needed, then fetch the issues."
  (org-linear--load-state)
  (let ((buffer (org-linear--buffer)))
    (with-current-buffer buffer
      (when (buffer-modified-p)
        (org-linear--save buffer)))
    (org-linear--fetch
     buffer (lambda (data) (org-linear--on-fetch buffer data)))))

;;;###autoload
(defun org-linear-browse ()
  "Open the Linear issue of the heading at point."
  (interactive)
  (if-let* ((url (org-entry-get nil "LINEAR_URL")))
      (browse-url url)
    (user-error "No Linear issue at point")))

;;;###autoload
(defun org-linear-show-log ()
  "Show the sync log."
  (interactive)
  (pop-to-buffer (get-buffer-create org-linear--log-buffer)))

;;;###autoload
(defun org-linear-visit ()
  "Visit `org-linear-file'."
  (interactive)
  (find-file (org-linear--ensure-file)))

;;;; Modes

;;;###autoload
(define-minor-mode org-linear-auto-sync-mode
  "Sync `org-linear-file' on save, on visit and every few minutes."
  :global t
  :group 'org-linear
  (when org-linear--timer
    (cancel-timer org-linear--timer)
    (setq org-linear--timer nil))
  (remove-hook 'find-file-hook #'org-linear--maybe-enable-file-mode)
  (when org-linear-auto-sync-mode
    (add-hook 'find-file-hook #'org-linear--maybe-enable-file-mode)
    (setq org-linear--timer
          (run-with-timer 60 org-linear-pull-interval #'org-linear--timer-sync))
    (when-let* ((buffer (find-buffer-visiting org-linear-file)))
      (with-current-buffer buffer (org-linear-file-mode 1)))))

(define-minor-mode org-linear-file-mode
  "Send the file's changes to Linear whenever it is saved."
  :lighter " Linear"
  :group 'org-linear
  (if org-linear-file-mode
      (add-hook 'after-save-hook #'org-linear--after-save nil t)
    (remove-hook 'after-save-hook #'org-linear--after-save t)))

(defun org-linear--maybe-enable-file-mode ()
  "Enable `org-linear-file-mode' in the mirrored file, and sync it."
  (when (org-linear--file-p)
    (org-linear-file-mode 1)
    (unless org-linear--busy
      (org-linear-sync))))

(defun org-linear--after-save ()
  "Sync after the user saves the file."
  (unless org-linear--saving
    (run-with-idle-timer 0.5 nil #'org-linear-sync)))

(defun org-linear--timer-sync ()
  "Sync unless the file has unsaved edits, which a save will send."
  (let ((buffer (find-buffer-visiting org-linear-file)))
    (unless (and buffer (buffer-modified-p buffer))
      (org-linear-sync))))

(defun org-linear--file-p ()
  "Return non-nil if the current buffer visits `org-linear-file'."
  (and buffer-file-name
       (string= (file-name-nondirectory buffer-file-name)
                (file-name-nondirectory org-linear-file))
       (file-equal-p buffer-file-name org-linear-file)))

;;;; Agenda

(defun org-linear-agenda-skip ()
  "Skip issues assigned to others, for `org-agenda-skip-function-global'.
Issues with no assignee, issues assigned to you and issues with a tag in
`org-linear-agenda-keep-tags' stay."
  (when (and org-linear-hide-others-in-agenda (org-linear--file-p))
    (org-linear--load-state)
    (let* ((users (plist-get org-linear--state :user-tags))
           (tags (org-get-tags nil t))
           (people (seq-filter (lambda (tag) (member tag users)) tags)))
      (when (and people
                 (not (member (plist-get org-linear--state :me-tag) people))
                 (not (seq-intersection tags org-linear-agenda-keep-tags)))
        (save-excursion (or (outline-next-heading) (point-max)))))))

;;;; Fetching from Linear

(defconst org-linear--issue-fields
  "id identifier url title description priority dueDate updatedAt completedAt
canceledAt trashed state { id name type } team { id } assignee { id name }
project { id } labels(first: 50) { nodes { id name } }
comments(first: 100) { nodes { id body createdAt parent { id } user { id name } } }"
  "The issue fields the sync reads.")

(defun org-linear--fetch (buffer callback)
  "Fetch the teams' metadata and issues, then call CALLBACK with them.
Issues whose headings are in BUFFER but which fall outside the fetched
set are fetched too, so that their edits still sync."
  (org-linear--graphql
   "query OrgLinearMeta($keys: [String!]) {
  viewer { id name }
  teams(filter: { key: { in: $keys } }) { nodes { id key name
    states { nodes { id name type position } }
    members { nodes { id name } } } }
  issueLabels(first: 250) { nodes { id name team { id } } }
  projects(first: 250) { nodes { id name url teams { nodes { id } } } } }"
   `((keys . ,(vconcat org-linear-teams)))
   (lambda (meta)
     (setq org-linear--meta (org-linear--parse-meta meta))
     (org-linear--fetch-issues
      (org-linear--scope-filter) nil nil
      (lambda (issues)
        (org-linear--fetch-missing
         buffer issues
         (lambda (all)
           (funcall callback (org-linear--normalize all)))))))))

(defun org-linear--scope-filter ()
  "Return the issue filter for the teams' open and recently closed issues."
  (let ((cutoff (format-time-string
                 "%FT%TZ" (time-subtract nil (days-to-time org-linear-closed-retention-days)) t)))
    `((team . ((key . ((in . ,(vconcat org-linear-teams))))))
      (or . [((state . ((type . ((nin . ["completed" "canceled" "duplicate"]))))))
             ((completedAt . ((gt . ,cutoff))))
             ((canceledAt . ((gt . ,cutoff))))]))))

(defun org-linear--fetch-issues (filter after acc callback)
  "Fetch the issues matching FILTER from cursor AFTER onto ACC, then CALLBACK."
  (org-linear--graphql
   (format "query OrgLinearIssues($filter: IssueFilter, $after: String) {
  issues(first: 25, after: $after, filter: $filter, includeArchived: true) {
    pageInfo { hasNextPage endCursor } nodes { %s } } }"
           org-linear--issue-fields)
   `((filter . ,filter) (after . ,(or after :null)))
   (lambda (data)
     (let* ((page (alist-get 'issues data))
            (acc (append acc (alist-get 'nodes page)))
            (info (alist-get 'pageInfo page)))
       (if (eq (alist-get 'hasNextPage info) t)
           (org-linear--fetch-issues filter (alist-get 'endCursor info) acc callback)
         (funcall callback acc))))))

(defun org-linear--fetch-missing (buffer issues callback)
  "Add to ISSUES those with headings in BUFFER but missing; then CALLBACK."
  (let* ((have (make-hash-table :test #'equal))
         (missing nil))
    (dolist (issue issues) (puthash (alist-get 'id issue) t have))
    (maphash (lambda (id _) (unless (gethash id have) (push id missing)))
             (org-linear--uuids-in buffer))
    (if (null missing)
        (funcall callback issues)
      (org-linear--fetch-issues
       `((id . ((in . ,(vconcat missing))))) nil nil
       (lambda (more) (funcall callback (append issues more)))))))

(defun org-linear--parse-meta (data)
  "Return the sync's metadata, a plist, from the OrgLinearMeta DATA."
  (let* ((teams (alist-get 'nodes (alist-get 'teams data)))
         (team-ids (mapcar (lambda (team) (alist-get 'id team)) teams))
         (users (make-hash-table :test #'equal)))
    (dolist (team teams)
      (dolist (user (alist-get 'nodes (alist-get 'members team)))
        (puthash (alist-get 'id user) (org-linear--user-tag (alist-get 'name user)) users)))
    (list :viewer (alist-get 'id (alist-get 'viewer data))
          :teams teams
          :users users
          :labels (seq-filter
                   (lambda (label)
                     (let ((team (alist-get 'id (alist-get 'team label))))
                       (or (null team) (member team team-ids))))
                   (alist-get 'nodes (alist-get 'issueLabels data)))
          :projects (seq-filter
                     (lambda (project)
                       (seq-some (lambda (team) (member (alist-get 'id team) team-ids))
                                 (alist-get 'nodes (alist-get 'teams project))))
                     (alist-get 'nodes (alist-get 'projects data))))))

(defun org-linear--normalize (issues)
  "Return the fetched ISSUES as the sync's data: tasks, comments and projects."
  (let (tasks comments)
    (dolist (issue issues)
      (push (org-linear--task issue) tasks)
      (dolist (comment (alist-get 'nodes (alist-get 'comments issue)))
        (push `((id . ,(alist-get 'id comment))
                (issue_id . ,(alist-get 'id issue))
                (parent_id . ,(alist-get 'id (alist-get 'parent comment)))
                (created_at . ,(alist-get 'createdAt comment))
                (author . ,(or (alist-get 'name (alist-get 'user comment)) "Linear"))
                (body . ,(or (alist-get 'body comment) "")))
              comments)))
    (org-linear--remember-tags)
    `((tasks . ,(nreverse tasks))
      (comments . ,(nreverse comments))
      (projects . ,(mapcar (lambda (project)
                             `((id . ,(alist-get 'id project))
                               (title . ,(alist-get 'name project))
                               (url . ,(alist-get 'url project))))
                           (plist-get org-linear--meta :projects))))))

(defun org-linear--task (issue)
  "Return ISSUE in the shape the merge uses."
  (let* ((state (alist-get 'state issue))
         (type (alist-get 'type state))
         (closed-at (or (alist-get 'completedAt issue) (alist-get 'canceledAt issue))))
    `((id . ,(alist-get 'id issue))
      (identifier . ,(alist-get 'identifier issue))
      (url . ,(alist-get 'url issue))
      (title . ,(alist-get 'title issue))
      (status . ,(alist-get 'name state))
      (state_type . ,type)
      (team_id . ,(alist-get 'id (alist-get 'team issue)))
      (closed . ,(if (member type '("completed" "canceled" "duplicate")) t :false))
      (completed . ,(and closed-at (format-time-string "%F" (date-to-time closed-at))))
      (assignee_id . ,(alist-get 'id (alist-get 'assignee issue)))
      (assignee_name . ,(alist-get 'name (alist-get 'assignee issue)))
      (label_ids . ,(mapcar (lambda (label) (alist-get 'id label))
                            (alist-get 'nodes (alist-get 'labels issue))))
      (priority . ,(alist-get 'priority issue))
      (due . ,(alist-get 'dueDate issue))
      (project_id . ,(alist-get 'id (alist-get 'project issue)))
      (description . ,(or (alist-get 'description issue) ""))
      (trashed . ,(if (eq (alist-get 'trashed issue) t) t :false))
      (last_edited_time . ,(alist-get 'updatedAt issue)))))

(defun org-linear--remember-tags ()
  "Record the tags of people and labels, and yours, for the agenda."
  (let (users)
    (maphash (lambda (_ tag) (push tag users)) (plist-get org-linear--meta :users))
    (plist-put org-linear--state :user-tags users)
    (plist-put org-linear--state :label-tags (mapcar #'org-linear--label-tag
                                                     (plist-get org-linear--meta :labels)))
    (plist-put org-linear--state :me-tag
               (gethash (plist-get org-linear--meta :viewer)
                        (plist-get org-linear--meta :users)))))

;;;; Tags, states and priorities

(defun org-linear--user-tag (name)
  "Return the tag for the Linear user NAME."
  (or (cdr (assoc name org-linear-user-tags))
      (org-linear--sanitize-tag (downcase (car (split-string (or name "someone")))))))

(defun org-linear--label-tag (label)
  "Return the tag for LABEL, an alist with its name."
  (org-linear--sanitize-tag (alist-get 'name label)))

(defun org-linear--sanitize-tag (text)
  "Return TEXT with every character Org forbids in tags replaced by \"_\"."
  (replace-regexp-in-string "[^[:alnum:]_@#%]+" "_" text))

(defun org-linear--managed-tags ()
  "Return every tag that stands for a person or a label."
  (append (plist-get org-linear--state :user-tags)
          (plist-get org-linear--state :label-tags)))

(defun org-linear--remote-tags (task)
  "Return TASK's assignee and label tags, sorted."
  (let ((users (plist-get org-linear--meta :users)))
    (sort (delete-dups
           (delq nil (cons (gethash (alist-get 'assignee_id task) users)
                           (mapcar (lambda (id)
                                     (when-let* ((label (org-linear--label-by-id id)))
                                       (org-linear--label-tag label)))
                                   (alist-get 'label_ids task)))))
          #'string<)))

(defun org-linear--label-by-id (id)
  "Return the label with ID, or nil."
  (seq-find (lambda (label) (equal (alist-get 'id label) id))
            (plist-get org-linear--meta :labels)))

(defun org-linear--foreign-assignee (task)
  "Return the name of TASK's assignee if they are not a member of the teams."
  (when (and (alist-get 'assignee_id task)
             (not (gethash (alist-get 'assignee_id task) (plist-get org-linear--meta :users))))
    (or (alist-get 'assignee_name task) "someone outside the teams")))

(defun org-linear--state-keyword (name type)
  "Return the TODO keyword for the workflow state NAME of TYPE."
  (or (cdr (assoc name org-linear-state-keywords))
      (cdr (assoc type org-linear-type-keywords))
      "TODO"))

(defun org-linear--keyword-state (keyword team-id)
  "Return the state of team TEAM-ID that KEYWORD is sent as, or nil."
  (let* ((team (seq-find (lambda (team) (equal (alist-get 'id team) team-id))
                         (plist-get org-linear--meta :teams)))
         (states (sort (copy-sequence (alist-get 'nodes (alist-get 'states team)))
                       (lambda (a b)
                         (let ((ta (cl-position (alist-get 'type a) org-linear--type-order :test #'equal))
                               (tb (cl-position (alist-get 'type b) org-linear--type-order :test #'equal)))
                           (if (eql ta tb)
                               (< (alist-get 'position a) (alist-get 'position b))
                             (< (or ta 99) (or tb 99)))))))
         (named (cdr (assoc keyword org-linear-keyword-states))))
    (if named
        (seq-find (lambda (state) (equal (alist-get 'name state) named)) states)
      (seq-find (lambda (state)
                  (equal (org-linear--state-keyword (alist-get 'name state)
                                                    (alist-get 'type state))
                         keyword))
                states))))

(defun org-linear--cookie-priority (cookie)
  "Return the Linear priority for the Org priority COOKIE, 0 for none."
  (if (null cookie)
      0
    (or (car (rassoc cookie org-linear-priorities))
        (let ((sorted (sort (copy-sequence org-linear-priorities)
                            (lambda (a b) (string< (cdr a) (cdr b))))))
          (car (or (car (last (seq-filter (lambda (p) (not (string< cookie (cdr p)))) sorted)))
                   (car sorted)))))))

;;;; Sync: fetch done, convert

(defun org-linear--on-fetch (buffer data)
  "Convert the descriptions and comments in DATA that changed, for BUFFER."
  (let ((items (org-linear--conversion-items data (org-linear--uuids-in buffer))))
    (org-linear--md-to-org
     items
     (lambda (converted)
       (org-linear--on-converted buffer data converted)))))

(defun org-linear--uuids-in (buffer)
  "Return a hash table of the Linear issue IDs of the headings in BUFFER."
  (let ((uuids (make-hash-table :test #'equal)))
    (with-current-buffer buffer
      (org-with-wide-buffer
       (goto-char (point-min))
       (while (re-search-forward "^[ \t]*:LINEAR_UUID:[ \t]+\\(\\S-+\\)" nil t)
         (puthash (match-string-no-properties 1) t uuids))))
    uuids))

(defun org-linear--conversion-items (data present)
  "Return (KEY . MARKDOWN) for the texts in DATA to convert.
KEY is (desc . ID) for a description that differs from the last sync's,
and (comment . ID) for a comment not seen before.  PRESENT holds the
issue IDs that have a heading; an issue without one is converted afresh,
comments included, since it will be inserted again."
  (let ((tasks (org-linear--state-tasks))
        items)
    (dolist (task (alist-get 'tasks data))
      (let* ((id (alist-get 'id task))
             (base (and (gethash id present) (gethash id tasks)))
             (md (alist-get 'description task)))
        (when (and (or (gethash id present) (org-linear--in-scope-p task))
                   (not (equal md (plist-get base :desc-md))))
          (push (cons (cons 'desc id) md) items))))
    (dolist (comment (alist-get 'comments data))
      (let ((issue (alist-get 'issue_id comment)))
        (unless (or (alist-get 'parent_id comment)
                    (and (gethash issue present)
                         (member (alist-get 'id comment)
                                 (plist-get (gethash issue tasks) :comment-ids))))
          (push (cons (cons 'comment (alist-get 'id comment))
                      (alist-get 'body comment))
                items))))
    (nreverse items)))

(defconst org-linear--pandoc-jobs 8
  "Pandoc processes run at once when converting a sync's texts.")

(defun org-linear--md-to-org (items callback)
  "Convert the Markdown of ITEMS to Org, then call CALLBACK.
ITEMS is a list of (KEY . MARKDOWN).  Each text gets its own pandoc run,
so one text's markup cannot leak into another's.  CALLBACK gets a hash
table from KEY to the Org text."
  (let ((result (make-hash-table :test #'equal))
        (queue (seq-remove (lambda (item) (string-blank-p (cdr item))) items))
        (running 0)
        (failed nil))
    (dolist (item items)
      (when (string-blank-p (cdr item))
        (puthash (car item) "" result)))
    (cl-labels
        ((launch ()
           (while (and queue (not failed) (< running org-linear--pandoc-jobs))
             (let ((item (pop queue)))
               (cl-incf running)
               (org-linear--run-process
                (list org-linear-pandoc-program "-f" "gfm" "-t" "org" "--wrap=none")
                (org-linear--flatten-md-headings (cdr item))
                (lambda (code out err)
                  (cl-decf running)
                  (cond
                   (failed)
                   ((/= code 0)
                    (setq failed t)
                    (org-linear--fail "pandoc failed: %s" err))
                   (t
                    (puthash (car item) (org-linear--safe-org (string-trim out)) result)
                    (if (and (null queue) (= running 0))
                        (funcall callback result)
                      (launch))))))))))
      (if (null queue)
          (funcall callback result)
        (launch)))))

(defun org-linear--flatten-md-headings (markdown)
  "Turn Markdown headings in MARKDOWN into bold lines.
An Org heading inside an issue's body would split the issue."
  (replace-regexp-in-string "^#\\{1,6\\}[ \t]+\\(.*\\)$" "**\\1**" markdown))

(defun org-linear--safe-org (org)
  "Indent any line of ORG that Org would read as a heading."
  (replace-regexp-in-string "^\\(\\*+ \\)" " \\1" org))

(defun org-linear--org-to-md (org callback)
  "Convert ORG to Markdown, then call CALLBACK with it."
  (if (string-blank-p org)
      (funcall callback "")
    (org-linear--run-process
     (list org-linear-pandoc-program "-f" "org" "-t" "gfm" "--wrap=none")
     org
     (lambda (code out err)
       (if (/= code 0)
           (org-linear--fail "pandoc failed: %s" err)
         (funcall callback (org-linear--label-bare-links (string-trim out))))))))

(defun org-linear--label-bare-links (markdown)
  "Turn MARKDOWN's autolinks, <URL>, into [URL](URL)."
  (replace-regexp-in-string "<\\(https?://[^<> \t\n]+\\)>" "[\\1](\\1)" markdown t))

;;;; Sync: merge and apply

(defun org-linear--on-converted (buffer data converted)
  "Merge DATA, with CONVERTED texts, into BUFFER, then send the edits."
  (cond
   ((buffer-modified-p buffer)
    (org-linear--log "Not applied: the file has unsaved edits; save it to sync")
    (org-linear--finish))
   ((not (verify-visited-file-modtime buffer))
    (org-linear--fail "the file changed on disk; revert its buffer, then sync"))
   (t
    (let (ops)
      (with-current-buffer buffer
        (setq ops (org-linear--with-sync-edits
                   (lambda () (org-linear--apply data converted)))))
      (org-linear--save buffer)
      (org-linear--save-state)
      (org-linear--run-ops ops)))))

(defun org-linear--with-sync-edits (edit)
  "Call EDIT in the current buffer as the sync, and return its value.
The edits ignore the region, so that Org commands touch only the heading
they are given, and they keep tags in the order given.  If they change
the buffer, its undo history is dropped: undoing them would revert
changes made in Linear, and the next save would send the reversal."
  (let ((before (buffer-chars-modified-tick))
        value)
    (let ((mark-active nil)
          (org-loop-over-headlines-in-active-region nil)
          (org-tags-sort-function nil))
      (org-with-wide-buffer
       (save-excursion
         (setq value (funcall edit)))))
    (unless (= before (buffer-chars-modified-tick))
      (setq buffer-undo-list nil))
    value))

(defun org-linear--apply (data converted)
  "Apply DATA to the current buffer and return the edits to send.
CONVERTED maps conversion keys to Org text."
  (let* ((index (org-linear--index))
         (projects (alist-get 'projects data))
         (comments (org-linear--comments-by-issue data converted))
         (seen (make-hash-table :test #'equal))
         ops)
    (org-linear--rename-projects projects index)
    (dolist (task (alist-get 'tasks data))
      (let ((id (alist-get 'id task)))
        (puthash id t seen)
        (setq ops (nconc ops (org-linear--apply-task
                              task converted (gethash id comments) projects index)))))
    (maphash (lambda (id marker)
               (unless (gethash id seen)
                 (org-linear--remove-if-unchanged id marker "left Linear")))
             (plist-get index :tasks))
    (nconc ops (org-linear--creation-ops index))))

(defun org-linear--apply-task (task converted comments projects index)
  "Apply TASK to the file and return the edits to send for it.
CONVERTED maps conversion keys to Org text; COMMENTS are the issue's new
comments; PROJECTS are the teams' projects; INDEX locates headings.  An
issue that has left the file's scope keeps syncing while its heading
holds unsent edits, and its heading goes once it has none."
  (let* ((id (alist-get 'id task))
         (marker (gethash id (plist-get index :tasks)))
         (base (gethash id (org-linear--state-tasks))))
    (cond
     ((gethash id (plist-get index :duplicates))
      (org-linear--log "Not synced: %s has more than one heading; delete the copies"
                       (alist-get 'identifier task))
      nil)
     ((eq (alist-get 'trashed task) t)
      (if marker
          (org-linear--remove-if-unchanged id marker "was deleted in Linear")
        (remhash id (org-linear--state-tasks)))
      nil)
     (marker
      (let ((ops (org-linear--merge task converted comments projects index marker base)))
        (when (and (null ops) (not (org-linear--in-scope-p task)))
          (org-linear--remove-if-unchanged id marker nil))
        ops))
     (t
      (remhash id (org-linear--state-tasks))
      (when (org-linear--in-scope-p task)
        (org-linear--insert-task task converted comments projects index))
      nil))))

(defun org-linear--merge (task converted comments projects index marker base)
  "Merge TASK with its heading at MARKER three-way against BASE.
Apply Linear's changes to the heading and return the edits to send.
CONVERTED, COMMENTS, PROJECTS and INDEX are as in
`org-linear--apply-task'."
  (let* ((id (alist-get 'id task))
         (remote (org-linear--remote-view task converted base))
         (local (org-linear--local-view marker))
         (base (or base (org-linear--base-from-local local marker)))
         (new-base (copy-sequence base))
         pushed)
    (dolist (field org-linear--fields)
      (let ((l (plist-get local field))
            (r (plist-get remote field))
            (b (plist-get base field)))
        (cond
         ((equal l r) (setq new-base (plist-put new-base field r)))
         ((equal l b)
          (org-linear--set-field marker field r task projects index)
          (org-linear--count :pulled)
          (setq new-base (plist-put new-base field r)))
         ((and (equal r b) (eq field :tags) (org-linear--foreign-assignee task))
          (org-linear--log
           "Not sent: %s is assigned to %s, who has no tag here; change it in Linear"
           (alist-get 'identifier task) (org-linear--foreign-assignee task)))
         ((equal r b) (push (cons field l) pushed))
         (t
          (org-linear--set-field marker field r task projects index)
          (org-linear--conflict task field l r)
          (setq new-base (plist-put new-base field r))))))
    (org-linear--set-static-properties marker task)
    (setq new-base (org-linear--insert-comments marker comments new-base))
    (setq new-base (plist-put new-base :desc-md (alist-get 'description task)))
    (setq new-base (plist-put new-base :edited (alist-get 'last_edited_time task)))
    (puthash id new-base (org-linear--state-tasks))
    (org-linear--ops-for task (nreverse pushed) marker new-base)))

(defun org-linear--ops-for (task pushed marker base)
  "Return the edits that send the PUSHED fields and new notes of TASK.
PUSHED is a list of (FIELD . VALUE).  MARKER is the issue's heading and
BASE its state."
  (let ((id (alist-get 'id task))
        ops)
    (when pushed
      (push (list :type 'update :id id :team (alist-get 'team_id task) :fields pushed) ops))
    (dolist (note (org-linear--local-notes marker))
      (unless (member (org-linear--note-key (cdr note)) (plist-get base :note-keys))
        (push (list :type 'note :id id :text (org-linear--note-text note)
                    :key (org-linear--note-key (cdr note)))
              ops)))
    (seq-remove (lambda (op)
                  (when (member (org-linear--op-signature op) (plist-get base :rejected))
                    (org-linear--log "Not sent again: Linear refused this %s for %s"
                                     (plist-get op :type) (alist-get 'identifier task))
                    t))
                (nreverse ops))))

(defun org-linear--op-signature (op)
  "Return a string that identifies the content of OP."
  (format "%S" (pcase (plist-get op :type)
                 ('update (list 'update (plist-get op :fields)))
                 ('note (list 'note (plist-get op :key)))
                 ('add (list 'add (plist-get op :view)))
                 (type (list type)))))

(defun org-linear--creation-ops (index)
  "Return edits that create a Linear issue for each new heading in INDEX."
  (let (ops)
    (dolist (marker (plist-get index :new))
      (let* ((view (org-linear--local-view marker))
             (op (list :type 'add :marker marker :view view
                       :team (org-linear--team-for-project (plist-get view :project)))))
        (if (member (org-linear--op-signature op)
                    (plist-get org-linear--state :rejected-adds))
            (org-linear--log "Not created again: Linear refused \"%s\"; change it to retry"
                             (plist-get view :title))
          (push op ops))))
    (nreverse ops)))

(defun org-linear--team-for-project (project-id)
  "Return the ID of the team a new issue in PROJECT-ID goes to."
  (let* ((teams (plist-get org-linear--meta :teams))
         (team-ids (mapcar (lambda (team) (alist-get 'id team)) teams))
         (project (seq-find (lambda (p) (equal (alist-get 'id p) project-id))
                            (plist-get org-linear--meta :projects))))
    (or (seq-find (lambda (id) (member id team-ids))
                  (mapcar (lambda (team) (alist-get 'id team))
                          (alist-get 'nodes (alist-get 'teams project))))
        (car team-ids))))

;;;; Sending edits

(defun org-linear--run-ops (ops)
  "Send OPS to Linear one after another, then finish the sync."
  (if (null ops)
      (org-linear--finish)
    (org-linear--run-op (car ops) (lambda () (org-linear--run-ops (cdr ops))))))

(defun org-linear--run-op (op next)
  "Send OP to Linear, then call NEXT."
  (pcase (plist-get op :type)
    ('update (org-linear--run-update op next))
    ('note (org-linear--run-note op next))
    ('add (org-linear--run-add op next))))

(defun org-linear--run-update (op next)
  "Send the field changes of OP, then call NEXT.
The issue's last edit is checked first, and the write is refused if it
changed since the sync read it."
  (let* ((id (plist-get op :id))
         (base (gethash id (org-linear--state-tasks))))
    (org-linear--graphql
     "query OrgLinearEdited($id: String!) { issue(id: $id) { updatedAt } }"
     `((id . ,id))
     (lambda (data)
       (if (not (equal (alist-get 'updatedAt (alist-get 'issue data)) (plist-get base :edited)))
           (funcall (org-linear--refused-handler next)
                    (format "%s was edited in Linear meanwhile" id))
         (org-linear--update-input
          op
          (lambda (input)
            (if (null input)
                (funcall next)
              (org-linear--graphql
               "mutation OrgLinearUpdate($id: String!, $input: IssueUpdateInput!) {
  issueUpdate(id: $id, input: $input) { success issue { updatedAt description } } }"
               `((id . ,id) (input . ,input))
               (lambda (result)
                 (let ((issue (alist-get 'issue (alist-get 'issueUpdate result))))
                   (dolist (field (plist-get op :fields))
                     (setq base (plist-put base (car field) (cdr field))))
                   (setq base (plist-put base :desc-md (or (alist-get 'description issue) "")))
                   (org-linear--after-write id base (alist-get 'updatedAt issue))
                   (funcall next)))
               (org-linear--rejected-handler op next)))))))
     (org-linear--rejected-handler op next))))

(defun org-linear--update-input (op callback)
  "Call CALLBACK with the IssueUpdateInput for OP's fields, or nil if none.
A field that cannot be sent is logged and left out."
  (let ((team (plist-get op :team))
        (body (assq :body (plist-get op :fields)))
        input)
    (dolist (field (plist-get op :fields))
      (setq input (append input (org-linear--field-input (car field) (cdr field) team))))
    (if body
        (org-linear--org-to-md
         (cdr body)
         (lambda (md) (funcall callback (cons (cons 'description md) input))))
      (funcall callback input))))

(defun org-linear--field-input (field value team)
  "Return the IssueUpdateInput entries that set FIELD to VALUE in TEAM."
  (pcase field
    (:title `((title . ,value)))
    (:todo (if-let* ((state (org-linear--keyword-state value team)))
               `((stateId . ,(alist-get 'id state)))
             (org-linear--log "Not sent: %s is no workflow state of the team" value)
             nil))
    (:priority `((priority . ,(org-linear--cookie-priority value))))
    (:deadline `((dueDate . ,(or value :null))))
    (:project `((projectId . ,(or value :null))))
    (:tags (org-linear--tags-input value))
    (:body nil)))

(defun org-linear--tags-input (tags)
  "Return the assignee and label entries that TAGS stand for."
  (let* ((users (plist-get org-linear--meta :users))
         (people (seq-filter (lambda (tag) (seq-contains-p (hash-table-values users) tag)) tags))
         (labels (seq-filter (lambda (label) (member (org-linear--label-tag label) tags))
                             (plist-get org-linear--meta :labels)))
         (assignee (and (= (length people) 1)
                        (car (seq-filter (lambda (id) (equal (gethash id users) (car people)))
                                         (hash-table-keys users))))))
    (append
     (cond
      ((> (length people) 1)
       (org-linear--log "Not sent: an issue has one assignee, but the tags name %s"
                        (string-join people ", "))
       nil)
      (t `((assigneeId . ,(or assignee :null)))))
     `((labelIds . ,(vconcat (mapcar (lambda (label) (alist-get 'id label)) labels)))))))

(defun org-linear--run-note (op next)
  "Post the note of OP as a comment, then call NEXT."
  (let ((id (plist-get op :id)))
    (org-linear--org-to-md
     (plist-get op :text)
     (lambda (md)
       (org-linear--graphql
        "mutation OrgLinearComment($input: CommentCreateInput!) {
  commentCreate(input: $input) { success comment { id issue { updatedAt } } } }"
        `((input . ((issueId . ,id) (body . ,md))))
        (lambda (data)
          (let ((comment (alist-get 'comment (alist-get 'commentCreate data)))
                (base (gethash id (org-linear--state-tasks))))
            (setq base (plist-put base :note-keys
                                  (cons (plist-get op :key) (plist-get base :note-keys))))
            (setq base (plist-put base :comment-ids
                                  (cons (alist-get 'id comment) (plist-get base :comment-ids))))
            (org-linear--after-write id base (alist-get 'updatedAt (alist-get 'issue comment))))
          (funcall next))
        (org-linear--rejected-handler op next))))))

(defun org-linear--run-add (op next)
  "Create the Linear issue of OP, then call NEXT."
  (let ((view (plist-get op :view))
        (team (plist-get op :team)))
    (org-linear--org-to-md
     (plist-get view :body)
     (lambda (md)
       (let ((input (append `((teamId . ,team) (title . ,(plist-get view :title))
                              (description . ,md))
                            (org-linear--field-input :todo (plist-get view :todo) team)
                            (org-linear--field-input :priority (plist-get view :priority) team)
                            (when (plist-get view :deadline)
                              (org-linear--field-input :deadline (plist-get view :deadline) team))
                            (when (plist-get view :project)
                              (org-linear--field-input :project (plist-get view :project) team))
                            (org-linear--tags-input (plist-get view :tags)))))
         (org-linear--graphql
          (format "mutation OrgLinearCreate($input: IssueCreateInput!) {
  issueCreate(input: $input) { success issue { %s } } }" org-linear--issue-fields)
          `((input . ,input))
          (lambda (data)
            (org-linear--link-created (plist-get op :marker) view md
                                      (org-linear--task (alist-get 'issue (alist-get 'issueCreate data))))
            (funcall next))
          (org-linear--rejected-handler op next)))))))

(defun org-linear--link-created (marker view md task)
  "Record TASK, created from VIEW at MARKER and sent with description MD."
  (let ((base (append (copy-sequence view)
                      (list :desc-md md
                            :edited (alist-get 'last_edited_time task)
                            :comment-ids nil
                            :note-keys (mapcar (lambda (n) (org-linear--note-key (cdr n)))
                                               (org-linear--local-notes marker))))))
    (org-linear--edit-and-maybe-save
     (marker-buffer marker)
     (lambda ()
       (if-let* ((heading (org-linear--created-heading marker (plist-get view :title))))
           (org-linear--set-static-properties heading task)
         (org-linear--log "Created %s, but its heading \"%s\" moved or changed; the next sync adds it again from Linear, so delete your copy"
                          (alist-get 'identifier task) (plist-get view :title)))))
    (puthash (alist-get 'id task) base (org-linear--state-tasks))
    (org-linear--save-state)
    (org-linear--count :created)))

(defun org-linear--created-heading (marker title)
  "Return a marker at the heading new issue TITLE was created from, or nil.
MARKER is where it was when the sync read it; the user may have moved it
since.  Only a level-2 heading with that title and no Linear ID counts."
  (let ((matches-p (lambda ()
                     (and (= (org-outline-level) 2)
                          (equal (org-get-heading t t t t) title)
                          (not (org-entry-get nil "LINEAR_UUID"))))))
    (or (org-with-point-at marker
          (when (and (org-at-heading-p) (funcall matches-p))
            (org-linear--marker)))
        (save-excursion
          (goto-char (point-min))
          (let (found)
            (while (and (not found) (re-search-forward org-outline-regexp-bol nil t))
              (when (funcall matches-p)
                (setq found (org-linear--marker))))
            found)))))

(defun org-linear--after-write (id base edited)
  "Record BASE for issue ID after a write, which left it last EDITED then."
  (puthash id (plist-put base :edited edited) (org-linear--state-tasks))
  (org-linear--save-state)
  (org-linear--count :pushed))

(defun org-linear--refused-handler (next)
  "Return a handler for a write refused because the issue changed; it calls NEXT."
  (lambda (message &rest _)
    (org-linear--log "Changed in Linear meanwhile, merging again: %s" message)
    (setq org-linear--rerun t)
    (funcall next)))

(defun org-linear--rejected-handler (op next)
  "Return a handler for a write Linear rejected that then calls NEXT.
The rejection of OP is logged and remembered, so that the same content is
not sent again while later edits still go through."
  (lambda (message &rest _)
    (org-linear--count :errors)
    (org-linear--log "Rejected, not retried until you change it: %s %s: %s"
                     (plist-get op :type)
                     (or (plist-get op :id) (plist-get (plist-get op :view) :title))
                     message)
    (org-linear--remember-rejection op)
    (org-linear--save-state)
    (funcall next)))

(defun org-linear--remember-rejection (op)
  "Record that Linear rejected OP."
  (let ((signature (org-linear--op-signature op)))
    (if-let* ((id (plist-get op :id))
              (base (gethash id (org-linear--state-tasks))))
        (puthash id (plist-put base :rejected (cons signature (plist-get base :rejected)))
                 (org-linear--state-tasks))
      (plist-put org-linear--state :rejected-adds
                 (cons signature (plist-get org-linear--state :rejected-adds))))))

(defun org-linear--edit-and-maybe-save (buffer edit)
  "Call EDIT in BUFFER, then save BUFFER if it had no unsaved edits before."
  (with-current-buffer buffer
    (let ((clean (and (not (buffer-modified-p)) (verify-visited-file-modtime buffer))))
      (org-linear--with-sync-edits edit)
      (when clean (org-linear--save buffer)))))

(defun org-linear--finish ()
  "End the sync, report it, and start any sync requested meanwhile."
  (setq org-linear--busy nil
        org-linear--api-key-cache nil)
  (org-linear--report)
  (cond
   ((and org-linear--rerun (< org-linear--reruns 2))
    (cl-incf org-linear--reruns)
    (run-with-timer 1 nil #'org-linear-sync))
   (t (setq org-linear--reruns 0)
      (when org-linear--pending
        (run-with-timer 1 nil #'org-linear-sync)))))

(defun org-linear--fail (format-string &rest args)
  "End the sync with an error described by FORMAT-STRING and ARGS."
  (let ((text (apply #'format format-string args)))
    (org-linear--log "Error: %s" text)
    (setq org-linear--api-key-cache nil)
    (when org-linear--busy
      (setq org-linear--busy nil)
      (message "Linear sync failed: %s (M-x org-linear-show-log)" text)
      (when org-linear--pending
        (run-with-timer 5 nil #'org-linear-sync)))))

(defun org-linear--report ()
  "Report the counts of the sync that ended, if anything happened."
  (let ((s org-linear--stats))
    (when (cl-some (lambda (k) (> (plist-get s k) 0))
                   '(:pulled :pushed :created :conflicts :errors))
      (message "Linear sync: %d from Linear, %d sent, %d created%s"
               (plist-get s :pulled) (plist-get s :pushed) (plist-get s :created)
               (if (> (+ (plist-get s :conflicts) (plist-get s :errors)) 0)
                   (format ", %d conflicts, %d rejected (M-x org-linear-show-log)"
                           (plist-get s :conflicts) (plist-get s :errors))
                 "")))))

;;;; Views of an issue

(defun org-linear--remote-view (task converted base)
  "Return TASK from Linear as Org field values.
CONVERTED maps conversion keys to Org text; BASE supplies the body when
the description is unchanged."
  (list :title (alist-get 'title task)
        :todo (org-linear--state-keyword (alist-get 'status task) (alist-get 'state_type task))
        :priority (cdr (assq (alist-get 'priority task) org-linear-priorities))
        :tags (org-linear--remote-tags task)
        :deadline (alist-get 'due task)
        :project (alist-get 'project_id task)
        :body (or (gethash (cons 'desc (alist-get 'id task)) converted)
                  (plist-get base :body)
                  "")))

(defun org-linear--local-view (marker)
  "Return the issue at MARKER as Org field values."
  (org-with-point-at marker
    (list :title (org-get-heading t t t t)
          :todo (org-get-todo-state)
          :priority (org-linear--heading-priority)
          :tags (sort (seq-filter (lambda (tag) (member tag (org-linear--managed-tags)))
                                  (org-get-tags nil t))
                      #'string<)
          :deadline (org-linear--planning-date "DEADLINE")
          :project (org-linear--heading-project)
          :body (org-linear--body))))

(defun org-linear--base-from-local (local marker)
  "Return a first sync state for the heading at MARKER whose fields are LOCAL.
The heading's notes count as already sent."
  (append (copy-sequence local)
          (list :note-keys (mapcar (lambda (n) (org-linear--note-key (cdr n)))
                                   (org-linear--local-notes marker))
                :comment-ids nil)))

(defun org-linear--heading-priority ()
  "Return the priority cookie of the heading at point, as a string, or nil."
  (save-excursion
    (org-back-to-heading t)
    (when (and (looking-at org-complex-heading-regexp) (match-string 3))
      (let ((cookie (match-string 3)))
        (when (string-match "\\[#\\([^]]+\\)\\]" cookie)
          (match-string 1 cookie))))))

(defun org-linear--planning-date (property)
  "Return the date of planning PROPERTY of the heading at point, or nil."
  (when-let* ((stamp (org-entry-get nil property)))
    (when (string-match "[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}" stamp)
      (match-string 0 stamp))))

(defun org-linear--heading-project ()
  "Return the Linear project ID of the heading at point's project, or nil."
  (save-excursion
    (when (and (org-up-heading-safe) (= (org-outline-level) 1))
      (let ((id (org-entry-get nil "LINEAR_PROJECT")))
        (unless (member id '(nil "" "none")) id)))))

(defun org-linear--body-region ()
  "Return (BEG . END) of the body of the heading at point.
The body runs from after the planning line and drawers to the next heading."
  (save-excursion
    (org-back-to-heading t)
    (let ((end (save-excursion (outline-next-heading) (point))))
      (org-end-of-meta-data t)
      (cons (min (point) end) end))))

(defun org-linear--body ()
  "Return the body of the heading at point, trimmed."
  (let ((region (org-linear--body-region)))
    (string-trim (buffer-substring-no-properties (car region) (cdr region)))))

;;;; Notes

(defconst org-linear--note-regexp
  "^[ \t]*- \\(Note taken on\\|State \"\\([^\"]+\\)\"\\)[^\n]*\\\\\\\\[ \t]*$"
  "Regexp matching the first line of a log item that carries text.")

(defun org-linear--local-notes (marker)
  "Return the notes in the log drawer of the heading at MARKER.
Each is (STATE . TEXT): STATE is the keyword a state change moved to, or
nil for a note."
  (org-with-point-at marker
    (let ((drawer (org-linear--log-drawer-region))
          notes)
      (when drawer
        (save-excursion
          (goto-char (car drawer))
          (while (re-search-forward org-linear--note-regexp (cdr drawer) t)
            (let ((state (match-string 2))
                  (indent (current-indentation))
                  (start (progn (forward-line 1) (point))))
              (while (and (< (point) (cdr drawer))
                          (or (looking-at-p "^[ \t]*$")
                              (> (current-indentation) indent)))
                (forward-line 1))
              (let ((text (org-linear--dedent (buffer-substring-no-properties start (point)))))
                (unless (string-empty-p text)
                  (push (cons state text) notes)))))))
      (nreverse notes))))

(defun org-linear--log-drawer-name ()
  "Return the name of the drawer that holds notes."
  (or (org-log-into-drawer) "LOGBOOK"))

(defun org-linear--log-drawer-region ()
  "Return (BEG . END) of the log drawer of the heading at point, or nil."
  (save-excursion
    (org-back-to-heading t)
    (let ((limit (save-excursion (outline-next-heading) (point)))
          (name (org-linear--log-drawer-name)))
      (when (re-search-forward (format "^[ \t]*:%s:[ \t]*$" (regexp-quote name)) limit t)
        (let ((beg (1+ (line-end-position))))
          (when (re-search-forward "^[ \t]*:END:[ \t]*$" limit t)
            (cons beg (line-beginning-position))))))))

(defun org-linear--dedent (text)
  "Remove from TEXT the indentation its non-blank lines share, and trim it."
  (let* ((lines (split-string text "\n"))
         (indents (mapcar (lambda (line) (- (length line) (length (string-trim-left line))))
                          (seq-remove #'string-blank-p lines)))
         (common (if indents (apply #'min indents) 0)))
    (string-trim
     (mapconcat (lambda (line) (if (string-blank-p line) "" (substring line common)))
                lines "\n"))))

(defun org-linear--note-key (text)
  "Return TEXT with its whitespace collapsed, to recognize a note."
  (string-trim (replace-regexp-in-string "[ \t\n]+" " " text)))

(defun org-linear--note-text (note)
  "Return the comment text that posts NOTE, a (STATE . TEXT) pair."
  (if-let* ((state (car note)))
      (format "%s: %s" state (cdr note))
    (cdr note)))

(defun org-linear--insert-comments (marker comments base)
  "Insert COMMENTS as notes in the heading at MARKER; return the updated BASE.
A comment whose text is already a note there is only recorded."
  (let ((keys (plist-get base :note-keys))
        (ids (plist-get base :comment-ids)))
    (dolist (comment comments)
      (let ((key (org-linear--note-key (plist-get comment :text))))
        (unless (member key keys)
          (org-linear--insert-note marker comment)
          (push key keys))
        (push (plist-get comment :id) ids)))
    (plist-put (plist-put base :note-keys keys) :comment-ids ids)))

(defun org-linear--insert-note (marker comment)
  "Insert COMMENT as a note in the log drawer of the heading at MARKER."
  (org-with-point-at marker
    (let ((org-log-into-drawer (org-linear--log-drawer-name)))
      (goto-char (org-log-beginning t))
      (insert (org-linear--note-string comment)))))

(defun org-linear--note-string (comment)
  "Return the log item for COMMENT, a plist with :time, :author and :text."
  (format "- Note taken on %s (%s) \\\\\n%s\n"
          (format-time-string "[%Y-%m-%d %a %H:%M]" (plist-get comment :time))
          (plist-get comment :author)
          (replace-regexp-in-string "^" "  " (plist-get comment :text))))

(defun org-linear--comments-by-issue (data converted)
  "Return a hash table from issue ID to its new comments in DATA.
Each comment is a plist with :id, :time, :author and :text, oldest first.
CONVERTED maps conversion keys to Org text."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (comment (alist-get 'comments data))
      (when-let* ((org (gethash (cons 'comment (alist-get 'id comment)) converted)))
        (push (list :id (alist-get 'id comment)
                    :time (date-to-time (alist-get 'created_at comment))
                    :author (alist-get 'author comment)
                    :text org)
              (gethash (alist-get 'issue_id comment) table))))
    (maphash (lambda (id list)
               (puthash id (sort list (lambda (a b) (time-less-p (plist-get a :time)
                                                                 (plist-get b :time))))
                        table))
             table)
    table))

;;;; Writing to the file

(defun org-linear--set-field (marker field value task projects index)
  "Set FIELD of the heading at MARKER to VALUE.
TASK is the Linear issue, for the closing date; PROJECTS and INDEX locate
project headings."
  (pcase field
    (:project (org-linear--move-to-project marker value projects index))
    (_ (org-with-point-at marker
         (pcase field
           (:title (org-edit-headline value))
           (:todo (org-linear--set-todo value (alist-get 'completed task)))
           (:priority (org-linear--set-priority value))
           (:tags (org-set-tags (append value (seq-remove
                                               (lambda (tag) (member tag (org-linear--managed-tags)))
                                               (org-get-tags nil t)))))
           (:deadline (org-linear--set-planning 'deadline value))
           (:body (org-linear--set-body value)))))))

(defun org-linear--set-todo (keyword completed)
  "Set the heading at point's TODO KEYWORD without logging or hooks.
COMPLETED is the date the issue was closed, for the CLOSED line."
  (let ((org-inhibit-logging t)
        (org-log-done nil)
        (org-todo-log-states nil)
        (org-after-todo-state-change-hook nil)
        (org-trigger-hook nil)
        (org-blocker-hook nil)
        (org-enforce-todo-dependencies nil))
    (org-todo keyword)
    (if (and completed (member keyword org-done-keywords))
        (org-add-planning-info 'closed (date-to-time (concat completed "T12:00:00")))
      (org-add-planning-info nil nil 'closed))))

(defun org-linear--set-planning (kind date)
  "Set the heading at point's KIND (deadline or scheduled) to DATE, or remove it.
Unlike `org-deadline', this leaves the CLOSED line and logs nothing."
  (if date
      (org-add-planning-info kind (date-to-time (concat date "T12:00:00")))
    (org-add-planning-info nil nil kind)))

(defun org-linear--set-priority (cookie)
  "Set the heading at point's priority to COOKIE, a string, or remove it if nil."
  (org-back-to-heading t)
  (when (looking-at org-complex-heading-regexp)
    (cond
     ((match-beginning 3)
      (let ((beg (match-beginning 3))
            (end (save-excursion (goto-char (match-end 3))
                                 (skip-chars-forward " ") (point))))
        (delete-region beg end)
        (goto-char beg)
        (when cookie (insert (format "[#%s] " cookie)))))
     (cookie
      (goto-char (if (match-end 2) (match-end 2) (match-end 1)))
      (skip-chars-forward " ")
      (insert (format "[#%s] " cookie))))))

(defun org-linear--set-body (body)
  "Replace the body of the heading at point with BODY."
  (let ((region (org-linear--body-region)))
    (delete-region (car region) (cdr region))
    (goto-char (car region))
    (unless (bolp) (insert "\n"))
    (unless (string-blank-p body)
      (insert body "\n"))))

(defun org-linear--set-static-properties (marker task)
  "Set the read-only properties of the heading at MARKER from TASK."
  (org-with-point-at marker
    (dolist (prop `(("LINEAR_ID" . ,(alist-get 'identifier task))
                    ("LINEAR_UUID" . ,(alist-get 'id task))
                    ("LINEAR_URL" . ,(alist-get 'url task))))
      (unless (equal (org-entry-get nil (car prop)) (cdr prop))
        (org-entry-put nil (car prop) (cdr prop))))))

(defun org-linear--insert-task (task converted comments projects index)
  "Insert TASK as a new heading under its project heading.
CONVERTED maps conversion keys to Org text; COMMENTS are its comments;
PROJECTS and INDEX locate the project heading."
  (let* ((view (org-linear--remote-view task converted nil))
         (pos (org-linear--project-end (plist-get view :project) projects index))
         marker)
    (goto-char pos)
    (insert (org-linear--heading-string view (alist-get 'completed task)))
    (setq marker (org-linear--marker pos))
    (org-with-point-at marker (org-align-tags))
    (org-linear--set-static-properties marker task)
    (let ((base (append (copy-sequence view)
                        (list :desc-md (alist-get 'description task)
                              :edited (alist-get 'last_edited_time task)
                              :note-keys nil :comment-ids nil))))
      (setq base (org-linear--insert-comments marker comments base))
      (puthash (alist-get 'id task) base (org-linear--state-tasks)))
    (puthash (alist-get 'id task) marker (plist-get index :tasks))
    (org-linear--count :pulled)))

(defun org-linear--heading-string (view completed)
  "Return the text of a new heading for the issue VIEW.
COMPLETED is its closing date, or nil."
  (let ((planning (string-join
                   (delq nil
                         (list (when (and completed (member (plist-get view :todo)
                                                            (org-linear--done-keywords)))
                                 (format-time-string "CLOSED: [%Y-%m-%d %a]"
                                                     (date-to-time (concat completed "T12:00:00"))))
                               (when (plist-get view :deadline)
                                 (format "DEADLINE: %s" (org-linear--stamp (plist-get view :deadline))))))
                   " ")))
    (concat "** " (plist-get view :todo) " "
            (when (plist-get view :priority) (format "[#%s] " (plist-get view :priority)))
            (plist-get view :title)
            (when (plist-get view :tags) (format " :%s:" (string-join (plist-get view :tags) ":")))
            "\n"
            (unless (string-empty-p planning) (concat planning "\n"))
            (unless (string-blank-p (plist-get view :body)) (concat (plist-get view :body) "\n")))))

(defun org-linear--stamp (date)
  "Return an active Org timestamp for DATE, YYYY-MM-DD."
  (format-time-string "<%Y-%m-%d %a>" (date-to-time (concat date "T12:00:00"))))

(defun org-linear--done-keywords ()
  "Return the TODO keywords of closed state types."
  (delq nil (mapcar (lambda (type) (cdr (assoc type org-linear-type-keywords)))
                    '("completed" "canceled" "duplicate"))))

(defun org-linear--project-end (project projects index)
  "Return where a new issue of PROJECT goes, creating its heading if needed.
PROJECTS are the teams' projects; INDEX maps project IDs to headings."
  (let* ((key (or project "none"))
         (heading (gethash key (plist-get index :projects))))
    (unless heading
      (setq heading (org-linear--insert-project key projects))
      (puthash key heading (plist-get index :projects)))
    (org-with-point-at heading
      (org-end-of-subtree t t)
      (unless (bolp) (insert "\n"))
      (point))))

(defun org-linear--insert-project (key projects)
  "Insert a level-1 heading for project KEY at the end; return its marker.
KEY is a project ID or \"none\"; PROJECTS are the teams' projects."
  (goto-char (point-max))
  (unless (bolp) (insert "\n"))
  (let ((pos (point)))
    (insert "* " (or (alist-get 'title (seq-find (lambda (p) (equal (alist-get 'id p) key))
                                                 projects))
                     "No project")
            "\n:PROPERTIES:\n:LINEAR_PROJECT: " key "\n:END:\n")
    (org-linear--marker pos)))

(defun org-linear--rename-projects (projects index)
  "Rename project headings in INDEX whose project in PROJECTS was renamed."
  (dolist (project projects)
    (when-let* ((marker (gethash (alist-get 'id project) (plist-get index :projects))))
      (org-with-point-at marker
        (unless (equal (org-get-heading t t t t) (alist-get 'title project))
          (org-edit-headline (alist-get 'title project)))))))

(defun org-linear--move-to-project (marker project projects index)
  "Move the issue heading at MARKER under PROJECT's heading.
PROJECTS and INDEX locate project headings."
  (let* ((beg (marker-position marker))
         (end (org-with-point-at marker (org-end-of-subtree t t) (point)))
         (text (buffer-substring beg end)))
    (unless (string-suffix-p "\n" text) (setq text (concat text "\n")))
    (delete-region beg end)
    (let ((pos (org-linear--project-end project projects index)))
      (goto-char pos)
      (insert text)
      (set-marker marker pos))))

(defun org-linear--remove-if-unchanged (id marker reason)
  "Delete the heading at MARKER of issue ID unless it holds anything unsent.
A heading with unsent edits or notes, or with subheadings of its own,
stays.  REASON, if non-nil, says why the issue leaves, for the log line
written when the heading stays."
  (let ((base (gethash id (org-linear--state-tasks))))
    (if (and base (org-linear--unchanged-p marker base)
             (not (org-linear--has-children-p marker)))
        (progn
          (org-with-point-at marker
            (delete-region (point) (progn (org-end-of-subtree t t) (point))))
          (remhash id (org-linear--state-tasks)))
      (when reason
        (org-linear--log "Kept \"%s\": it %s, but the heading holds edits, notes or subheadings not in Linear"
                         (org-with-point-at marker (org-get-heading t t t t))
                         reason)))))

(defun org-linear--unchanged-p (marker base)
  "Return non-nil if the heading at MARKER matches BASE and has no new notes."
  (let ((local (org-linear--local-view marker)))
    (and (cl-every (lambda (field) (equal (plist-get local field) (plist-get base field)))
                   org-linear--fields)
         (cl-every (lambda (note)
                     (member (org-linear--note-key (cdr note)) (plist-get base :note-keys)))
                   (org-linear--local-notes marker)))))

(defun org-linear--has-children-p (marker)
  "Return non-nil if the heading at MARKER has subheadings."
  (org-with-point-at marker
    (save-excursion (org-goto-first-child))))

;;;; Index of the file

(defun org-linear--index ()
  "Index the current buffer's headings.
Return a plist: :tasks maps issue IDs to issue heading markers, :projects
maps project IDs (or \"none\") to project heading markers, :duplicates
holds issue IDs with more than one heading, and :new lists the markers of
issue headings not yet in Linear."
  (let ((tasks (make-hash-table :test #'equal))
        (projects (make-hash-table :test #'equal))
        (duplicates (make-hash-table :test #'equal))
        new)
    (goto-char (point-min))
    (while (re-search-forward org-outline-regexp-bol nil t)
      (let ((level (org-outline-level)))
        (cond
         ((= level 1)
          (when-let* ((id (org-entry-get nil "LINEAR_PROJECT")))
            (puthash id (org-linear--marker) projects)))
         ((= level 2)
          (let ((uuid (org-entry-get nil "LINEAR_UUID")))
            (cond
             ((and uuid (gethash uuid tasks)) (puthash uuid t duplicates))
             (uuid (puthash uuid (org-linear--marker) tasks))
             ((and (org-get-todo-state)
                   (save-excursion (org-up-heading-safe)
                                   (org-entry-get nil "LINEAR_PROJECT")))
              (push (org-linear--marker) new))))))
        (end-of-line)))
    (list :tasks tasks :projects projects :duplicates duplicates :new (nreverse new))))

(defun org-linear--marker (&optional pos)
  "Return a marker at POS, or the current heading, that text inserted there pushes.
Text inserted where a heading starts belongs before that heading."
  (copy-marker (or pos (save-excursion (org-back-to-heading t) (point))) t))

(defun org-linear--in-scope-p (task)
  "Return non-nil if TASK belongs in the file.
Open issues do, and closed ones for `org-linear-closed-retention-days'."
  (and (not (eq (alist-get 'trashed task) t))
       (or (not (eq (alist-get 'closed task) t))
           (let ((completed (alist-get 'completed task)))
             (and completed
                  (<= (- (time-to-days nil)
                         (time-to-days (date-to-time (concat completed "T12:00:00"))))
                      org-linear-closed-retention-days))))))

;;;; Files and state

(defun org-linear--ensure-file ()
  "Create `org-linear-file' with its header if missing; return its name."
  (unless (file-exists-p org-linear-file)
    (make-directory (file-name-directory org-linear-file) t)
    (with-temp-file org-linear-file (insert org-linear-file-header)))
  org-linear-file)

(defun org-linear--buffer ()
  "Return a buffer visiting `org-linear-file', creating the file if needed."
  (let ((file (org-linear--ensure-file)))
    (or (find-buffer-visiting file)
        (find-file-noselect file))))

(defun org-linear--save (buffer)
  "Save BUFFER, if it changed, quietly and without triggering a sync."
  (with-current-buffer buffer
    (when (buffer-modified-p)
      (let ((org-linear--saving t)
            (inhibit-message t))
        (save-buffer)))))

(defun org-linear--state-tasks ()
  "Return the hash table of issue states."
  (plist-get org-linear--state :tasks))

(defun org-linear--load-state ()
  "Load the sync state from `org-linear-state-file' unless loaded."
  (unless org-linear--state
    (let ((tasks (make-hash-table :test #'equal))
          (saved (when (file-exists-p org-linear-state-file)
                   (with-temp-buffer
                     (insert-file-contents org-linear-state-file)
                     (read (current-buffer))))))
      (dolist (entry (plist-get saved :tasks))
        (puthash (car entry) (cdr entry) tasks))
      (setq org-linear--state
            (list :tasks tasks
                  :rejected-adds (plist-get saved :rejected-adds)
                  :me-tag (plist-get saved :me-tag)
                  :user-tags (plist-get saved :user-tags)
                  :label-tags (plist-get saved :label-tags))))))

(defun org-linear--save-state ()
  "Write the sync state to `org-linear-state-file'."
  (let (entries)
    (maphash (lambda (id base) (push (cons id base) entries)) (org-linear--state-tasks))
    (make-directory (file-name-directory org-linear-state-file) t)
    (with-temp-file org-linear-state-file
      (let ((print-length nil) (print-level nil))
        (prin1 (list :version 1
                     :rejected-adds (plist-get org-linear--state :rejected-adds)
                     :me-tag (plist-get org-linear--state :me-tag)
                     :user-tags (plist-get org-linear--state :user-tags)
                     :label-tags (plist-get org-linear--state :label-tags)
                     :tasks entries)
               (current-buffer))))))

;;;; Talking to Linear

(defun org-linear--graphql (query variables callback &optional errback retried)
  "Send QUERY with VARIABLES to Linear, then call CALLBACK with its data.
On failure call ERRBACK with a message, or end the sync if it is nil.
When Linear rejects the key and RETRIED is nil, drop the cached key with
`org-linear-api-key-forget-command' and try once more."
  (org-linear--with-api-key
   (lambda (key)
     (let ((body (let ((inhibit-message t)
                       (message-log-max nil))
                   (make-temp-file "org-linear" nil ".json"
                                   (json-serialize `((query . ,query) (variables . ,variables))
                                                   :null-object :null :false-object :false)))))
       (org-linear--run-process
        (list org-linear-curl-program "-sS" "--max-time" (number-to-string org-linear-timeout)
              "-X" "POST" org-linear-api-url
              "-H" "Content-Type: application/json"
              "--data-binary" (concat "@" body)
              "-w" "\n%{http_code}" "-K" "-")
        (format "header = \"Authorization: %s\"\n" key)
        (lambda (code out err)
          (delete-file body)
          (org-linear--handle-response query variables callback errback retried code out err)))))))

(defun org-linear--handle-response (query variables callback errback retried code out err)
  "Handle Linear's answer to QUERY with VARIABLES.
CODE, OUT and ERR are curl's exit code, output and error output.
CALLBACK, ERRBACK and RETRIED are as in `org-linear--graphql'."
  (let* ((status (and (string-match "\n\\([0-9]+\\)\\'" out) (string-to-number (match-string 1 out))))
         (payload (and status (substring out 0 (match-beginning 0))))
         (json (and payload (ignore-errors
                              (json-parse-string payload :object-type 'alist :array-type 'list
                                                 :null-object nil :false-object :false))))
         (errors (alist-get 'errors json)))
    (cond
     ((/= code 0)
      (org-linear--error errback "curl failed (%d): %s" code (string-trim err)))
     ((and (or (eql status 401) (org-linear--auth-error-p errors))
           (not retried) org-linear-api-key-forget-command)
      (org-linear--log "Linear rejected the key; reading it again")
      (setq org-linear--api-key-cache nil)
      (org-linear--run-process
       org-linear-api-key-forget-command nil
       (lambda (&rest _) (org-linear--graphql query variables callback errback t))))
     (errors
      (org-linear--error errback "%s" (mapconcat (lambda (e) (or (alist-get 'message e) "error"))
                                                  errors "; ")))
     ((not (eql status 200))
      (org-linear--error errback "Linear answered HTTP %s" status))
     (t (funcall callback (alist-get 'data json))))))

(defun org-linear--auth-error-p (errors)
  "Return non-nil if ERRORS report an authentication failure."
  (seq-some (lambda (e)
              (equal (alist-get 'code (alist-get 'extensions e)) "AUTHENTICATION_ERROR"))
            errors))

(defun org-linear--error (errback format-string &rest args)
  "Report FORMAT-STRING and ARGS to ERRBACK, or end the sync with them."
  (let ((text (apply #'format format-string args)))
    (if errback (funcall errback text) (org-linear--fail "%s" text))))

(defun org-linear--with-api-key (callback)
  "Call CALLBACK with the API key, reading it if this sync has not yet."
  (cond
   (org-linear--api-key-cache (funcall callback org-linear--api-key-cache))
   (org-linear-api-key-command
    (org-linear--run-process
     org-linear-api-key-command nil
     (lambda (code out err)
       (if (or (/= code 0) (string-blank-p out))
           (org-linear--fail "the API key command failed: %s" (string-trim err))
         (setq org-linear--api-key-cache (string-trim out))
         (funcall callback org-linear--api-key-cache)))))
   (t
    (let ((key (org-linear--api-key)))
      (if (not key)
          (org-linear--fail "no Linear API key: set `org-linear-api-key' or add one for api.linear.app to auth-source")
        (setq org-linear--api-key-cache key)
        (funcall callback key))))))

(defun org-linear--api-key ()
  "Return the API key from `org-linear-api-key' or auth-source, or nil."
  (cond
   ((stringp org-linear-api-key) org-linear-api-key)
   ((functionp org-linear-api-key) (funcall org-linear-api-key))
   (t (require 'auth-source)
      (when-let* ((found (car (auth-source-search :host "api.linear.app" :max 1)))
                  (secret (plist-get found :secret)))
        (if (functionp secret) (funcall secret) secret)))))

(defun org-linear--run-process (command input callback)
  "Run COMMAND asynchronously, feeding it INPUT if non-nil.
Call CALLBACK with the exit code, the output and the error output.  An
error in CALLBACK ends the sync instead of leaving it running."
  (let* ((out (generate-new-buffer " *org-linear-out*"))
         (err (generate-new-buffer " *org-linear-err*"))
         (proc (make-process
                :name "org-linear"
                :buffer out
                :stderr err
                :command command
                :connection-type 'pipe
                :noquery t
                :sentinel
                (lambda (process _event)
                  (unless (process-live-p process)
                    (let ((stderr-proc (get-buffer-process err)))
                      (when stderr-proc (accept-process-output stderr-proc 0.1)))
                    (let ((code (process-exit-status process))
                          (stdout (with-current-buffer out (buffer-string)))
                          (stderr (with-current-buffer err (buffer-string))))
                      (kill-buffer out)
                      (kill-buffer err)
                      (condition-case failure
                          (funcall callback code stdout stderr)
                        (error (org-linear--fail "%s" (error-message-string failure))))))))))
    (when input (process-send-string proc input))
    (process-send-eof proc)))

;;;; Logging

(defun org-linear--log (format-string &rest args)
  "Add a line made from FORMAT-STRING and ARGS to the sync log."
  (with-current-buffer (get-buffer-create org-linear--log-buffer)
    (goto-char (point-max))
    (insert (format-time-string "%F %T  ") (apply #'format format-string args) "\n")))

(defun org-linear--conflict (task field local remote)
  "Log that FIELD of TASK changed on both sides; LOCAL lost to REMOTE."
  (org-linear--count :conflicts)
  (org-linear--log "Conflict in %s %s: kept Linear's %S; your %S is not sent"
                   (alist-get 'identifier task)
                   (substring (symbol-name field) 1)
                   remote local))

(defun org-linear--count (key)
  "Add one to the count KEY of the sync in progress."
  (plist-put org-linear--stats key (1+ (or (plist-get org-linear--stats key) 0))))

(provide 'org-linear)
;;; org-linear.el ends here
