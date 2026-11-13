;;; org-linear-in-place.el --- Sync Org TODOs with Linear where they are -*- lexical-binding: t -*-

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

;; The in-place layout of org-linear: TODO headings sync with Linear
;; wherever they are in the files under `org-linear-directories', and Org is
;; the primary copy.  A heading is linked to its issue by its LINEAR_UUID
;; property.  `org-linear-sync' loads this file when the profile's
;; `org-linear-layout' is `in-place'.
;;
;; A sync reads the files that changed since the last one, fetches the
;; issues that changed in Linear since then, merges the two three-way as
;; the mirror layout does, and creates issues for new headings.  Issues
;; created in Linear go to `org-linear-inbox-file'.  The sync never deletes
;; heading text, never edits a buffer with unsaved changes, and leaves out
;; excluded headings even when they are linked.

;;; Code:

(require 'org-linear)
(require 'org-habit nil t)

;;;; The scan

(defconst org-linear-in-place--file-regexp "\\.org\\(?:_archive\\)?\\'"
  "Regexp matching the names of the files the scan reads.")

(defvar org-linear-in-place--candidate-cache nil
  "The value of `org-todo-keywords' and the candidate regexp built from it.")

(defun org-linear-in-place--candidate-regexp ()
  "Return a regexp that a file holding a heading the scan must index matches.
That is an open TODO heading, by the keywords of `org-todo-keywords', a
linked heading, or a line that sets the file's own keywords.  Excluded
keywords and headings are left out later, when the file is indexed."
  (let ((keywords (default-value 'org-todo-keywords)))
    (unless (eq (car org-linear-in-place--candidate-cache) keywords)
      (setq org-linear-in-place--candidate-cache
            (cons keywords (org-linear-in-place--build-candidate-regexp))))
    (cdr org-linear-in-place--candidate-cache)))

(defun org-linear-in-place--build-candidate-regexp ()
  "Build the regexp of `org-linear-in-place--candidate-regexp'."
  (concat "^\\*+ +" (regexp-opt (org-linear-in-place--open-keywords) t) "\\(?: \\|$\\)"
          "\\|^[ \t]*:LINEAR_\\(?:PENDING_\\)?UUID:"
          "\\|^#\\+\\(?:SEQ_\\|TYP_\\)?TODO:"))

(defun org-linear-in-place--open-keywords ()
  "Return the open TODO keywords of the default value of `org-todo-keywords'."
  (seq-mapcat (lambda (sequence)
                (let ((keywords (mapcar (lambda (kw) (replace-regexp-in-string "(.*)\\'" "" kw))
                                        (cdr sequence))))
                  (if (member "|" keywords)
                      (seq-take-while (lambda (kw) (not (equal kw "|"))) keywords)
                    (butlast keywords))))
              (default-value 'org-todo-keywords)))

(defun org-linear-in-place--scan ()
  "Read the files and return the sync's view of them, a plist.
:present maps every issue ID found in any file to its file; :tasks maps
the IDs of linked headings in scope to markers; :new lists markers at
headings in scope that have no issue yet; :pending holds the IDs their
creation was sent with; :duplicates holds IDs with several headings;
:buffers lists the buffers the sync may edit and :opened those it
visited itself; :skipped maps files to why they are left out."
  (let ((scan (list :present (make-hash-table :test #'equal)
                    :tasks (make-hash-table :test #'equal)
                    :new nil
                    :pending (make-hash-table :test #'equal)
                    :duplicates (make-hash-table :test #'equal)
                    :buffers nil :opened nil
                    :skipped (make-hash-table :test #'equal)
                    :indexed (make-hash-table :test #'equal)))
        (cache (org-linear-in-place--cache))
        (seen (make-hash-table :test #'equal)))
    (dolist (file (org-linear-in-place--files))
      (puthash file t seen)
      (org-linear-in-place--scan-file file scan cache))
    (maphash (lambda (file _) (unless (gethash file seen) (remhash file cache))) cache)
    scan))

(defun org-linear-in-place--files ()
  "Return the absolute names of the Org files under `org-linear-directories'."
  (seq-mapcat (lambda (dir)
                (when (file-directory-p dir)
                  (seq-filter
                   (lambda (file)
                     (and (not (string-prefix-p ".#" (file-name-nondirectory file)))
                          (file-regular-p file)))
                   (directory-files-recursively
                    (expand-file-name dir) org-linear-in-place--file-regexp nil
                    (lambda (subdir) (not (string-prefix-p "." (file-name-nondirectory subdir))))))))
              org-linear-directories))

(defun org-linear-in-place--cache ()
  "Return the scan cache: a hash table from file name to what it held.
Each value is a plist with :mtime, :uuids and :candidates.  The cache is
dropped when the options that decide the scope change."
  (let ((key (org-linear-in-place--scope-key)))
    (unless (and (equal (plist-get org-linear--state :scope-key) key)
                 (hash-table-p (plist-get org-linear--state :files)))
      (plist-put org-linear--state :scope-key key)
      (plist-put org-linear--state :files (make-hash-table :test #'equal)))
    (plist-get org-linear--state :files)))

(defun org-linear-in-place--scope-key ()
  "Return a value that changes when an option deciding the scope changes."
  (sxhash-equal (list org-linear-directories org-linear-exclude-files
                      org-linear-exclude-tags org-linear-exclude-keywords)))

(defun org-linear-in-place--excluded-file-p (file)
  "Return non-nil if FILE is out of scope: excluded or an archive."
  (or (string-suffix-p "_archive" file)
      (seq-some (lambda (regexp) (string-match-p regexp file)) org-linear-exclude-files)))

(defun org-linear-in-place--scan-file (file scan cache)
  "Add FILE to SCAN, reading it again unless CACHE shows it unchanged."
  (let* ((buffer (find-buffer-visiting file))
         (mtime (file-attribute-modification-time (file-attributes file)))
         (entry (gethash file cache))
         (dirty (and buffer (buffer-modified-p buffer)))
         (changed (not (and entry (not dirty) (equal (plist-get entry :mtime) mtime)))))
    (when changed
      (setq entry (org-linear-in-place--read-file file buffer mtime))
      (puthash file entry cache))
    (dolist (uuid (plist-get entry :uuids))
      (puthash uuid file (plist-get scan :present)))
    (cond
     ((org-linear-in-place--excluded-file-p file))
     (dirty
      (when (plist-get entry :candidates)
        (puthash file "it has unsaved edits" (plist-get scan :skipped))))
     ((and (plist-get entry :candidates)
           (or changed (plist-get entry :new)))
      (org-linear-in-place--index-file file scan)))))

(defun org-linear-in-place--read-file (file buffer mtime)
  "Return a cache entry for FILE, read from BUFFER if it visits FILE.
MTIME is FILE's modification time.  Its :uuids include the IDs of
creations in flight, so that their issues count as present."
  (with-temp-buffer
    (if buffer
        (insert-buffer-substring buffer)
      (insert-file-contents file))
    (goto-char (point-min))
    (let ((candidates (save-excursion
                        (let ((case-fold-search nil))
                          (re-search-forward (org-linear-in-place--candidate-regexp) nil t))))
          uuids)
      (while (re-search-forward "^[ \t]*:LINEAR_\\(?:PENDING_\\)?UUID:[ \t]+\\(\\S-+\\)" nil t)
        (push (match-string-no-properties 1) uuids))
      (list :mtime mtime :uuids (nreverse uuids) :candidates (and candidates t)))))

(defun org-linear-in-place--index-file (file scan)
  "Visit FILE and add its headings to SCAN, unless it cannot be edited now.
Return its buffer, or nil.  A file already indexed, under whatever name,
is not indexed again, so that no heading is counted twice."
  (or (when-let* ((visiting (find-buffer-visiting file)))
        (car (memq visiting (plist-get scan :buffers))))
      (when-let* ((buffer (org-linear-in-place--usable-buffer file scan)))
        (with-current-buffer buffer
          (org-with-wide-buffer
           (org-linear-in-place--index-buffer scan)))
        (push buffer (plist-get scan :buffers))
        buffer)))

(defun org-linear-in-place--usable-buffer (file scan)
  "Return a buffer visiting FILE that the sync may edit, or nil.
A buffer with unsaved edits is not; one whose file changed on disk is
reverted first.  A buffer the sync visits itself is recorded in SCAN."
  (let ((buffer (find-buffer-visiting file)))
    (cond
     ((and buffer (buffer-modified-p buffer))
      (puthash file "it has unsaved edits" (plist-get scan :skipped))
      nil)
     (buffer
      (unless (verify-visited-file-modtime buffer)
        (with-current-buffer buffer
          (let ((inhibit-message t))
            (revert-buffer t t t))))
      buffer)
     (t
      (setq buffer (org-linear-in-place--visit file))
      (with-current-buffer buffer
        (unless (derived-mode-p 'org-mode) (org-mode)))
      (plist-put scan :opened (cons buffer (plist-get scan :opened)))
      buffer))))

(defun org-linear-in-place--index-buffer (scan)
  "Add the headings of the current buffer to SCAN, and record its counts."
  (let ((new 0)
        uuids)
    (goto-char (point-min))
    (while (re-search-forward org-outline-regexp-bol nil t)
      (let ((uuid (org-entry-get nil "LINEAR_UUID"))
            (pending (org-entry-get nil "LINEAR_PENDING_UUID"))
            (keyword (org-get-todo-state)))
        (when uuid (push uuid uuids))
        (when pending (push pending uuids))
        (when (and (or uuid keyword) (not (org-linear-in-place--excluded-heading-p keyword)))
          (cond
           ((and uuid (gethash uuid (plist-get scan :tasks)))
            (puthash uuid t (plist-get scan :duplicates)))
           (uuid (puthash uuid (org-linear--marker) (plist-get scan :tasks)))
           ((member keyword org-not-done-keywords)
            (cl-incf new)
            (when pending
              (puthash pending t (plist-get scan :pending)))
            (plist-put scan :new (append (plist-get scan :new) (list (org-linear--marker)))))))
        (end-of-line)))
    (dolist (uuid uuids)
      (puthash uuid buffer-file-name (plist-get scan :present)))
    (let ((entry (gethash buffer-file-name (org-linear-in-place--cache))))
      (when entry
        (puthash buffer-file-name (plist-put (plist-put entry :new (> new 0)) :uuids uuids)
                 (org-linear-in-place--cache))))))

(defun org-linear-in-place--excluded-heading-p (keyword)
  "Return non-nil if the heading at point, whose keyword is KEYWORD, is excluded."
  (or (member keyword org-linear-exclude-keywords)
      (equal (org-entry-get nil "STYLE") "habit")
      (seq-intersection (org-get-tags) org-linear-exclude-tags)))

;;;; Fetching

(defun org-linear-in-place--start ()
  "Scan the files, then fetch what changed in Linear."
  (let ((started (format-time-string "%FT%TZ" (time-subtract nil 120) t)))
    (setq org-linear--scan (org-linear-in-place--scan)
          org-linear--body-overrides (make-hash-table :test #'equal))
    (maphash (lambda (file reason) (org-linear--log "Skipped %s: %s"
                                                    (abbreviate-file-name file) reason))
             (plist-get org-linear--scan :skipped))
    (org-linear-in-place--fetch
     (lambda (data)
       (org-linear-in-place--locate data)
       (org-linear-in-place--ensure-labels
        (lambda ()
          (org-linear-in-place--on-fetch data started)))))))

(defun org-linear-in-place--fetch (callback)
  "Fetch the metadata and the issues the sync needs, then CALLBACK with them.
These are the issues changed since the last sync, or all open ones the
first time, and those whose headings the scan found."
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
      (org-linear-in-place--filter) nil nil
      (lambda (issues)
        (org-linear-in-place--fetch-ids
         (org-linear-in-place--missing-ids issues) issues
         (lambda (all) (funcall callback (org-linear--normalize all)))))))))

(defun org-linear-in-place--filter ()
  "Return the filter for the issues changed since the last sync.
The first sync takes every open issue."
  (let ((teams `((key . ((in . ,(vconcat org-linear-teams))))))
        (since (plist-get org-linear--state :since)))
    (if since
        `((team . ,teams) (updatedAt . ((gt . ,since))))
      `((team . ,teams)
        (state . ((type . ((nin . ["completed" "canceled" "duplicate"])))))))))

(defun org-linear-in-place--missing-ids (issues)
  "Return the IDs of indexed headings and pending creations not among ISSUES."
  (let ((have (make-hash-table :test #'equal))
        missing)
    (dolist (issue issues) (puthash (alist-get 'id issue) t have))
    (maphash (lambda (id _) (unless (gethash id have) (push id missing)))
             (plist-get org-linear--scan :tasks))
    missing))

(defun org-linear-in-place--fetch-ids (ids acc callback)
  "Fetch the issues with IDS, 50 at a time, onto ACC; then CALLBACK."
  (if (null ids)
      (funcall callback acc)
    (let ((chunk (seq-take ids 50)))
      (org-linear--fetch-issues
       `((id . ((in . ,(vconcat chunk))))) nil nil
       (lambda (more)
         (org-linear-in-place--fetch-ids (seq-drop ids 50) (append acc more) callback))))))

(defun org-linear-in-place--ensure-labels (callback)
  "Create the file labels that indexed headings need and Linear lacks.
Then call CALLBACK."
  (let* ((names (delete-dups
                 (mapcar (lambda (buffer) (org-linear--file-label-name (buffer-file-name buffer)))
                         (plist-get org-linear--scan :buffers))))
         (known (mapcar (lambda (label) (alist-get 'name label)) (plist-get org-linear--meta :labels)))
         (missing (seq-remove (lambda (name) (member name known)) names)))
    (org-linear-in-place--create-labels missing callback)))

(defun org-linear-in-place--create-labels (names callback)
  "Create workspace labels called NAMES one after another, then CALLBACK."
  (if (null names)
      (funcall callback)
    (org-linear--graphql
     "mutation OrgLinearLabel($input: IssueLabelCreateInput!) {
  issueLabelCreate(input: $input) { success issueLabel { id name team { id } } } }"
     `((input . ((name . ,(car names)))))
     (lambda (data)
       (let ((label (alist-get 'issueLabel (alist-get 'issueLabelCreate data))))
         (plist-put org-linear--meta :labels (cons label (plist-get org-linear--meta :labels)))
         (plist-put org-linear--state :label-tags
                    (cons (org-linear--label-tag label) (plist-get org-linear--state :label-tags))))
       (org-linear-in-place--create-labels (cdr names) callback)))))

;;;; Converting and merging bodies

(defun org-linear-in-place--on-fetch (data started)
  "Convert the texts of DATA that changed, then merge it.
STARTED is when the sync began, the next sync's starting point."
  (org-linear--md-to-org
   (org-linear--conversion-items data (plist-get org-linear--scan :present))
   (lambda (converted)
     (org-linear-in-place--merge-bodies
      (org-linear-in-place--body-pulls data converted)
      (lambda ()
        (org-linear-in-place--apply-and-send data converted started))))))

(defun org-linear-in-place--locate (data)
  "Index the files of the issues in DATA whose files the scan did not visit.
The inbox file is indexed too, in case new issues go there."
  (when org-linear-inbox-file
    (let ((inbox (expand-file-name org-linear-inbox-file)))
      (unless (gethash inbox (plist-get org-linear--scan :skipped))
        (org-linear-in-place--index-file inbox org-linear--scan))))
  (dolist (task (alist-get 'tasks data))
    (let* ((id (alist-get 'id task))
           (file (gethash id (plist-get org-linear--scan :present))))
      (when (and file
                 (not (gethash id (plist-get org-linear--scan :tasks)))
                 (not (org-linear-in-place--excluded-file-p file))
                 (not (gethash file (plist-get org-linear--scan :skipped)))
                 (not (gethash file (plist-get org-linear--scan :indexed))))
        (puthash file t (plist-get org-linear--scan :indexed))
        (org-linear-in-place--index-file file org-linear--scan)))))

(defun org-linear-in-place--body-pulls (data converted)
  "Return the description edits in DATA that would replace an unedited Org body.
Each is (ID MARKER BASE MD); CONVERTED holds the converted descriptions."
  (let (pulls)
    (dolist (task (alist-get 'tasks data))
      (let* ((id (alist-get 'id task))
             (marker (gethash id (plist-get org-linear--scan :tasks)))
             (base (gethash id (org-linear--state-tasks)))
             (md (cdr (org-linear--split-description (alist-get 'description task)))))
        (when (and marker base
                   (gethash (cons 'desc id) converted)
                   (not (equal (alist-get 'description task) (plist-get base :desc-md)))
                   (equal (org-linear-in-place--body-at marker) (plist-get base :body)))
          (push (list id marker base md) pulls))))
    (nreverse pulls)))

(defun org-linear-in-place--body-at (marker)
  "Return the body of the heading at MARKER."
  (org-with-point-at marker (org-linear--body)))

(defun org-linear-in-place--merge-bodies (pulls callback)
  "Decide how each of PULLS reaches Org, one after another, then CALLBACK.
Linear's edit is pulled as pandoc converts it when Linear's stored text
of the old description converts back to exactly the Org body: then the
conversion loses nothing.  Otherwise `org-linear-merge-body-function'
merges it, or it is held back.  A Linear text held back once is not
merged again; a later edit on either side is."
  (if (null pulls)
      (funcall callback)
    (pcase-let ((`(,id ,marker ,base ,md) (car pulls))
                (next (lambda () (org-linear-in-place--merge-bodies (cdr pulls) callback))))
      (let ((body (org-linear-in-place--body-at marker))
            (old-md (cdr (org-linear--split-description (plist-get base :desc-md)))))
        (if (equal (plist-get base :held-md) md)
            (progn (puthash id :hold org-linear--body-overrides)
                   (funcall next))
          (org-linear--md-to-org
           (list (cons 'old old-md))
           (lambda (table)
             (if (org-linear-in-place--same-text-p (gethash 'old table) body)
                 (funcall next)
               (org-linear-in-place--merge-body id body base old-md md next)))))))))

(defun org-linear-in-place--same-text-p (a b)
  "Return non-nil if A and B differ at most in trailing whitespace."
  (equal (org-linear-in-place--trim-lines a) (org-linear-in-place--trim-lines b)))

(defun org-linear-in-place--trim-lines (text)
  "Return TEXT without trailing whitespace on its lines or around it."
  (string-trim (replace-regexp-in-string "[ \t]+$" "" (or text ""))))

(defun org-linear-in-place--same-content-p (a b)
  "Return non-nil if A and B differ only in whitespace."
  (equal (org-linear--note-key (or a "")) (org-linear--note-key (or b ""))))

(defun org-linear-in-place--merge-body (id body base old-md md next)
  "Merge Linear's description MD of issue ID into the Org BODY, then call NEXT.
BASE is the issue's state at the last sync and OLD-MD its description
then.  The merge is used only if it says, in Org, what MD says; if not,
or if the merge fails, the body is held back and MD is recorded, so that
it is not sent to the model again."
  (let ((hold (lambda (reason)
                (org-linear--log "Held back %s's description edit from Linear: %s" id reason)
                (puthash id (plist-put (copy-sequence base) :held-md md) (org-linear--state-tasks))
                (puthash id :hold org-linear--body-overrides)
                (funcall next))))
    (if (not org-linear-merge-body-function)
        (funcall hold "the Org body has markup Markdown lacks; set `org-linear-merge-body-function'")
      (funcall org-linear-merge-body-function
               body old-md md
               (org-linear--bound
                (lambda (merged)
                  (if (not (stringp merged))
                      (funcall hold "the merge failed")
                    (org-linear-in-place--check-merge
                     merged md
                     (lambda (ok)
                       (if ok
                           (progn (puthash id (string-trim merged) org-linear--body-overrides)
                                  (funcall next))
                         (funcall hold "the merge does not say what Linear's text says")))))))))))

(defun org-linear-in-place--check-merge (merged md callback)
  "Call CALLBACK with non-nil if the Org text MERGED says what Markdown MD says.
Both are compared as Org, as pandoc reads them, so that differences of
Markdown style between pandoc and Linear do not count."
  (org-linear--org-to-md
   merged
   (lambda (merged-md)
     (org-linear--md-to-org
      (list (cons 'merged merged-md) (cons 'linear md))
      (lambda (table)
        (funcall callback (org-linear-in-place--same-content-p
                           (gethash 'merged table) (gethash 'linear table))))))))

;;;; Applying

(defun org-linear-in-place--apply-and-send (data converted started)
  "Apply DATA, with CONVERTED texts, to the files, save them, and send the edits.
STARTED becomes the next sync's starting point."
  (let (ops)
    (org-linear-in-place--drop-changed-buffers)
    (org-linear-in-place--with-edits
     (lambda () (setq ops (org-linear-in-place--apply data converted))))
    (org-linear-in-place--save-buffers)
    (plist-put org-linear--state :since started)
    (org-linear--save-state)
    (org-linear--run-ops ops)))

(defun org-linear-in-place--drop-changed-buffers ()
  "Leave out of this sync the buffers edited or changed on disk since the scan.
Their headings stay present, so their issues are neither inserted again
nor archived; they sync once the buffer is saved."
  (let* ((scan org-linear--scan)
         (changed (seq-filter (lambda (buffer)
                                (or (not (buffer-live-p buffer))
                                    (buffer-modified-p buffer)
                                    (not (verify-visited-file-modtime buffer))))
                              (plist-get scan :buffers))))
    (dolist (buffer changed)
      (org-linear--log "Skipped %s: it changed during the sync" (buffer-name buffer))
      (maphash (lambda (id marker)
                 (when (eq (marker-buffer marker) buffer)
                   (remhash id (plist-get scan :tasks))))
               (plist-get scan :tasks))
      (plist-put scan :new (seq-remove (lambda (marker) (eq (marker-buffer marker) buffer))
                                       (plist-get scan :new)))
      (plist-put scan :opened (remq buffer (plist-get scan :opened))))
    (plist-put scan :buffers (seq-difference (plist-get scan :buffers) changed))))

(defun org-linear-in-place--with-edits (edit)
  "Call EDIT with the indexed buffers widened, as sync edits.
Undo history is dropped in each buffer EDIT changed, since undoing a
change from Linear would send its reversal."
  (let* ((buffers (plist-get org-linear--scan :buffers))
         (ticks (mapcar (lambda (b) (cons b (buffer-chars-modified-tick b))) buffers))
         (restrictions (mapcar (lambda (b) (with-current-buffer b
                                             (list b (copy-marker (point-min))
                                                   (copy-marker (point-max) t))))
                               buffers))
         (org-loop-over-headlines-in-active-region nil)
         (org-tags-sort-function nil)
         (transient-mark-mode nil))
    (unwind-protect
        (progn
          (dolist (b buffers) (with-current-buffer b (widen)))
          (save-current-buffer (funcall edit)))
      (dolist (r restrictions)
        (with-current-buffer (car r)
          (narrow-to-region (nth 1 r) (nth 2 r))
          (set-marker (nth 1 r) nil)
          (set-marker (nth 2 r) nil)))
      (dolist (tick ticks)
        (unless (= (cdr tick) (buffer-chars-modified-tick (car tick)))
          (with-current-buffer (car tick) (setq buffer-undo-list nil)))))))

(defun org-linear-in-place--apply (data converted)
  "Apply DATA to the files and return the edits to send.
CONVERTED maps conversion keys to Org text."
  (let* ((scan org-linear--scan)
         (comments (org-linear--comments-by-issue data converted))
         (fetched (make-hash-table :test #'equal))
         ops)
    (dolist (task (alist-get 'tasks data))
      (let ((id (alist-get 'id task)))
        (puthash id t fetched)
        (setq ops (nconc ops (org-linear-in-place--apply-task
                              task converted (gethash id comments))))))
    (nconc ops
           (org-linear-in-place--vanished-ops fetched)
           (org-linear-in-place--creation-ops scan))))

(defun org-linear-in-place--apply-task (task converted comments)
  "Apply TASK to its heading and return the edits to send for it.
CONVERTED maps conversion keys to Org text; COMMENTS are the issue's new
comments."
  (let* ((scan org-linear--scan)
         (id (alist-get 'id task))
         (marker (gethash id (plist-get scan :tasks)))
         (base (gethash id (org-linear--state-tasks))))
    (cond
     ((gethash id (plist-get scan :duplicates))
      (org-linear--log "Not synced: %s has more than one heading; delete the copies"
                       (alist-get 'identifier task))
      nil)
     ((eq (alist-get 'trashed task) t)
      (when marker (org-linear-in-place--cancel-trashed marker task))
      (remhash id (org-linear--state-tasks))
      nil)
     (marker (org-linear-in-place--merge task converted comments marker base))
     ((gethash id (plist-get scan :present)) nil)
     ((gethash id (plist-get scan :pending)) nil)
     (base nil)
     ((member id (plist-get org-linear--state :archived)) nil)
     ((not (eq (alist-get 'closed task) t))
      (org-linear-in-place--insert-in-inbox task converted comments)))))

(defun org-linear-in-place--merge (task converted comments marker base)
  "Merge TASK with its heading at MARKER, against BASE; return the edits.
CONVERTED and COMMENTS are as in `org-linear-in-place--apply-task'.  A
description held back keeps its old base, so that a later sync tries it
again."
  (let* ((id (alist-get 'id task))
         (held (eq (gethash id org-linear--body-overrides) :hold)))
    (when held
      (puthash id (or (plist-get base :body) "") org-linear--body-overrides))
    (prog1 (org-linear--merge task converted comments nil nil marker base)
      (when held (remhash id org-linear--body-overrides))
      (when (and held base)
        (let ((new-base (gethash id (org-linear--state-tasks))))
          (puthash id (plist-put new-base :desc-md (plist-get base :desc-md))
                   (org-linear--state-tasks)))))))

(defun org-linear-in-place--cancel-trashed (marker task)
  "Mark the heading at MARKER cancelled, as TASK was deleted in Linear.
The heading keeps its text and loses its link, so it is not created again."
  (org-with-point-at marker
    (let ((keyword (cdr (assoc "canceled" org-linear-type-keywords))))
      (org-linear--set-todo keyword nil))
    (dolist (property '("LINEAR_UUID" "LINEAR_ID" "LINEAR_URL" "LINEAR_REPEATED"))
      (org-entry-delete nil property)))
  (org-linear--log "Cancelled \"%s\": %s was deleted in Linear"
                   (org-with-point-at marker (org-get-heading t t t t))
                   (alist-get 'identifier task))
  (org-linear--count :pulled))

(defun org-linear-in-place--set-todo (keyword task)
  "Set the heading at point's KEYWORD, as TASK in Linear has it.
Closing a repeating heading repeats it, as closing it in Org would, but
only once for each time the issue was closed: the LINEAR_REPEATED
property records the closing it acted on."
  (let ((closed-at (alist-get 'closed_at task)))
    (cond
     ((not (and (member keyword org-done-keywords) (org-get-repeat)))
      (org-linear--set-todo keyword (alist-get 'completed task)))
     ((equal (org-entry-get nil "LINEAR_REPEATED") closed-at))
     (t
      (org-linear--set-todo keyword nil)
      (org-entry-put nil "LINEAR_REPEATED" closed-at)
      (setq org-linear--rerun t)))))

(defun org-linear-in-place--vanished-ops (fetched)
  "Return edits that archive the issues whose headings left Org.
Those are issues synced before whose IDs no file holds now.  FETCHED holds
the IDs this sync fetched; it does not change the answer, since a trashed
issue has already left the state.  More than `org-linear-archive-limit'
archive none."
  (let (gone)
    (ignore fetched)
    (maphash (lambda (id _)
               (unless (gethash id (plist-get org-linear--scan :present))
                 (push id gone)))
             (org-linear--state-tasks))
    (if (> (length gone) org-linear-archive-limit)
        (progn
          (org-linear--log "Not archived: the headings of %d issues are gone, more than `org-linear-archive-limit'; check `org-linear-directories'"
                           (length gone))
          nil)
      (mapcar (lambda (id) (list :type 'archive :id id)) gone))))

(defun org-linear-in-place--run-archive (op next)
  "Archive the issue of OP in Linear, then call NEXT."
  (let ((id (plist-get op :id)))
    (org-linear--graphql
     "mutation OrgLinearArchive($id: String!) { issueArchive(id: $id) { success } }"
     `((id . ,id))
     (lambda (_)
       (remhash id (org-linear--state-tasks))
       (plist-put org-linear--state :archived (cons id (plist-get org-linear--state :archived)))
       (org-linear--save-state)
       (org-linear--log "Archived %s in Linear: its heading is no longer in Org" id)
       (org-linear--count :archived)
       (funcall next))
     (org-linear--rejected-handler op next))))

;;;; Creating issues

(defun org-linear-in-place--creation-ops (scan)
  "Return edits that create issues for the new headings in SCAN.
At most `org-linear-create-limit' are created; the rest wait for the next
scheduled sync, not an immediate rerun, so that a large import keeps to
Linear's hourly request limit.  Each heading gets the ID of its issue
as LINEAR_PENDING_UUID before anything is sent, and its file is saved,
so that a lost answer is found again rather than repeated."
  (let* ((new (plist-get scan :new))
         (now (seq-take new org-linear-create-limit))
         ops)
    (when (nthcdr org-linear-create-limit new)
      (org-linear--log "Creating %d of %d new headings; the rest follow in the next syncs"
                       org-linear-create-limit (length new)))
    (dolist (marker now)
      (let* ((view (org-linear--local-view marker))
             (pending (or (org-entry-get marker "LINEAR_PENDING_UUID")
                          (let ((id (org-id-uuid)))
                            (org-entry-put marker "LINEAR_PENDING_UUID" id)
                            id)))
             (op (list :type 'add
                       :marker (list :file (buffer-file-name (marker-buffer marker))
                                     :pending pending)
                       :view view :pending pending
                       :team (alist-get 'id (car (plist-get org-linear--meta :teams))))))
        (if (member (org-linear--op-signature op) (plist-get org-linear--state :rejected-adds))
            (org-linear--log "Not created again: Linear refused \"%s\"; change it to retry"
                             (plist-get view :title))
          (push op ops))))
    (nreverse ops)))

(defun org-linear-in-place--link-created (place view md task)
  "Link TASK, created from VIEW and sent with description MD, to its heading.
PLACE is a plist with the heading's :file and :pending ID.  A heading
whose file has unsaved edits is linked by a later sync."
  (let ((base (append (copy-sequence view)
                      (list :desc-md (org-linear--stored-description task md)
                            :edited (alist-get 'last_edited_time task)
                            :comment-ids nil :note-keys nil))))
    (puthash (alist-get 'id task) base (org-linear--state-tasks))
    (org-linear--save-state)
    (org-linear--count :created)
    (org-linear-in-place--edit-file
     (plist-get place :file)
     (lambda ()
       (if-let* ((pos (org-find-property "LINEAR_PENDING_UUID" (plist-get place :pending))))
           (let ((marker (org-linear--marker pos)))
             (org-linear--set-static-properties marker task)
             (org-entry-delete marker "LINEAR_PENDING_UUID"))
         (org-linear--log "Created %s, but its heading is no longer in %s"
                          (alist-get 'identifier task) (plist-get place :file)))))))

(defun org-linear-in-place--edit-file (file edit)
  "Call EDIT at the start of FILE, widened, and save it.
Nothing happens if FILE's buffer has unsaved edits; a buffer visited for
this is killed afterwards."
  (let* ((visiting (find-buffer-visiting file))
         (buffer (or visiting (org-linear-in-place--visit file))))
    (if (buffer-modified-p buffer)
        (org-linear--log "Not linked yet: %s has unsaved edits" (abbreviate-file-name file))
      (with-current-buffer buffer
        (unless (derived-mode-p 'org-mode) (org-mode))
        (unless (verify-visited-file-modtime buffer)
          (let ((inhibit-message t)) (revert-buffer t t t)))
        (org-linear--with-sync-edits
         (lambda () (goto-char (point-min)) (funcall edit)))
        (org-linear--save buffer)
        (org-linear-in-place--refresh-cache buffer))
      (unless visiting (kill-buffer buffer)))))

;;;; Visiting files

(defun org-linear-in-place--visit (file)
  "Visit FILE for the sync and return its buffer.
The sync kills the buffer when done, so the hooks that set up a buffer
for editing, version control included, are skipped: a first import
visits a hundred files or more."
  (let ((inhibit-message t)
        (delay-mode-hooks t)
        (find-file-hook nil)
        (vc-handled-backends nil))
    (find-file-noselect file)))

;;;; The inbox

(defun org-linear-in-place--insert-in-inbox (task converted comments)
  "Insert TASK, created in Linear, under the inbox heading; return edits to send.
CONVERTED maps conversion keys to Org text; COMMENTS are its comments.
The edits give the issue the inbox file's label."
  (if-let* ((buffer (and org-linear-inbox-file
                         (org-linear-in-place--index-file
                          (expand-file-name org-linear-inbox-file) org-linear--scan))))
      (with-current-buffer buffer
        (let* ((view (org-linear-in-place--without-file-tags
                      (org-linear--remote-view task converted nil)))
               (heading (org-linear-in-place--inbox-heading))
               (level (1+ (org-with-point-at heading (org-outline-level))))
               (pos (org-with-point-at heading
                      (org-end-of-subtree t t)
                      (unless (bolp) (insert "\n"))
                      (point)))
               (marker (org-linear--marker pos))
               (base (append (copy-sequence view)
                             (list :desc-md (alist-get 'description task)
                                   :edited (alist-get 'last_edited_time task)
                                   :note-keys nil :comment-ids nil))))
          (goto-char pos)
          (insert (org-linear--heading-string view (alist-get 'completed task) level))
          (org-linear--set-static-properties marker task)
          (setq base (org-linear--insert-comments marker comments base))
          (puthash (alist-get 'id task) base (org-linear--state-tasks))
          (puthash (alist-get 'id task) marker (plist-get org-linear--scan :tasks))
          (puthash (alist-get 'id task) buffer-file-name (plist-get org-linear--scan :present))
          (org-linear--count :pulled)
          (org-linear--merge task converted nil nil nil marker base)))
    (org-linear--log "Not added: %s; set `org-linear-inbox-file', or save it"
                     (alist-get 'identifier task))
    nil))

(defun org-linear-in-place--without-file-tags (view)
  "Return VIEW without the tags of file labels, which no heading shows."
  (plist-put view :tags (seq-remove #'org-linear--file-label-tag-p (plist-get view :tags))))

(defun org-linear-in-place--inbox-heading ()
  "Return a marker at the inbox heading of the current buffer.
Add the heading at the end if it is missing."
  (goto-char (point-min))
  (if (re-search-forward (format org-complex-heading-regexp-format
                                 (regexp-quote org-linear-inbox-heading))
                         nil t)
      (org-linear--marker)
    (goto-char (point-max))
    (unless (bolp) (insert "\n"))
    (let ((pos (point)))
      (insert "* " org-linear-inbox-heading "\n")
      (org-linear--marker pos))))

;;;; Saving

(defun org-linear-in-place--save-buffers ()
  "Save the buffers the sync edited and kill those it visited itself."
  (dolist (buffer (plist-get org-linear--scan :buffers))
    (when (buffer-live-p buffer)
      (org-linear--save buffer)
      (org-linear-in-place--refresh-cache buffer)))
  (dolist (buffer (plist-get org-linear--scan :opened))
    (when (and (buffer-live-p buffer) (not (buffer-modified-p buffer)))
      (kill-buffer buffer))))

(defun org-linear-in-place--refresh-cache (buffer)
  "Record BUFFER's file, as saved, in the scan cache."
  (let ((file (buffer-file-name buffer)))
    (puthash file
             (org-linear-in-place--read-file
              file buffer (file-attribute-modification-time (file-attributes file)))
             (org-linear-in-place--cache))))

;;;; Preview

;;;###autoload
(defun org-linear-preview (&optional profile)
  "Show what the next in-place sync of PROFILE would create and skip.
Nothing is sent to Linear and no file is changed; the files the scan
visits are closed again.  Interactively, PROFILE is the current buffer's,
or the one you choose with a prefix argument."
  (interactive (list (if current-prefix-arg
                         (org-linear--read-profile)
                       org-linear--buffer-profile)))
  (org-linear--with-profile profile
    (unless (eq org-linear-layout 'in-place)
      (user-error "Preview is for the in-place layout"))
    (org-linear--load-state)
    (let* ((scan (org-linear-in-place--scan))
           (by-file (make-hash-table :test #'equal)))
      (dolist (marker (plist-get scan :new))
        (push (org-with-point-at marker (org-get-heading t t t t))
              (gethash (buffer-file-name (marker-buffer marker)) by-file)))
      (with-current-buffer (get-buffer-create "*org-linear preview*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Linked headings in scope: %d\nNew headings to create: %d (at most %d per sync)\n\n"
                          (hash-table-count (plist-get scan :tasks))
                          (length (plist-get scan :new)) org-linear-create-limit))
          (maphash (lambda (file titles)
                     (insert (format "%s (%d)\n" (abbreviate-file-name file) (length titles)))
                     (dolist (title (nreverse titles)) (insert "  " title "\n")))
                   by-file)
          (maphash (lambda (file reason)
                     (insert (format "Skipped %s: %s\n" (abbreviate-file-name file) reason)))
                   (plist-get scan :skipped))
          (goto-char (point-min))
          (special-mode))
        (dolist (buffer (plist-get scan :opened))
          (when (and (buffer-live-p buffer) (not (buffer-modified-p buffer)))
            (kill-buffer buffer)))
        (pop-to-buffer (current-buffer))))))

(provide 'org-linear-in-place)
;;; org-linear-in-place.el ends here
