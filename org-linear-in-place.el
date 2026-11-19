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
;;
;; With `org-linear-project-keywords' or `org-linear-milestone-keywords'
;; set, the nesting of headings syncs too: a task under a task is a
;; sub-issue, project headings become Linear projects or initiatives, and
;; milestone headings project milestones.  The hierarchy lives in Org, so
;; moving a heading sends its new relations.

;;; Code:

(require 'org-linear)
(require 'org-habit nil t)

;;;; The scan

(defconst org-linear-in-place--file-regexp "\\.org\\(?:_archive\\)?\\'"
  "Regexp matching the names of the files the scan reads.")

(defvar-local org-linear-in-place--read nil
  "What the file read in the current buffer was like when the sync read it.
A plist, in the buffers of `org-linear-in-place--read-buffer' only: :time
and :size are the file's modification time and size, nil if it did not
exist; :hash is the hash of its text and :truename its true name.")

(defvar org-linear-in-place--candidate-cache nil
  "The keywords and hierarchy the candidate regexp was built for, and the regexp.
The car is (KEYWORDS . HIERARCHY): the value of `org-todo-keywords' and
that of `org-linear-in-place--hierarchy-p'.")

(defun org-linear-in-place--candidate-regexp ()
  "Return a regexp that a file holding a heading the scan must index matches.
That is an open TODO heading, by the keywords of `org-todo-keywords', a
linked heading, or a line that sets the file's own keywords.  Excluded
keywords and headings are left out later, when the file is indexed."
  (let ((keywords (default-value 'org-todo-keywords))
        (hierarchy (org-linear-in-place--hierarchy-p))
        (key (car org-linear-in-place--candidate-cache)))
    (unless (and (consp key) (eq (car key) keywords) (eq (cdr key) hierarchy))
      (setq org-linear-in-place--candidate-cache
            (cons (cons keywords hierarchy) (org-linear-in-place--build-candidate-regexp))))
    (cdr org-linear-in-place--candidate-cache)))

(defun org-linear-in-place--build-candidate-regexp ()
  "Build the regexp of `org-linear-in-place--candidate-regexp'."
  (concat "^\\*+ +" (regexp-opt (org-linear-in-place--open-keywords) t) "\\(?: \\|$\\)"
          "\\|" (org-linear-in-place--id-property-regexp)
          "\\|^#\\+\\(?:SEQ_\\|TYP_\\)?TODO:"))

(defun org-linear-in-place--id-property-regexp ()
  "Return a regexp matching the start of a property line with an ID the sync wrote.
The IDs of projects, initiatives and milestones count only with a
hierarchy; see `org-linear-in-place--hierarchy-p'."
  (if (org-linear-in-place--hierarchy-p)
      "^[ \t]*:LINEAR_\\(?:PENDING_\\)?\\(?:UUID\\|PROJECT\\|INITIATIVE\\|MILESTONE\\):"
    "^[ \t]*:LINEAR_\\(?:PENDING_\\)?UUID:"))

(defun org-linear-in-place--open-keywords ()
  "Return the open TODO keywords of the default value of `org-todo-keywords'."
  (seq-mapcat (lambda (sequence)
                (let ((keywords (mapcar (lambda (kw) (replace-regexp-in-string "(.*)\\'" "" kw))
                                        (cdr sequence))))
                  (if (member "|" keywords)
                      (seq-take-while (lambda (kw) (not (equal kw "|"))) keywords)
                    (butlast keywords))))
              (default-value 'org-todo-keywords)))

;; `find-buffer-visiting' walks the whole buffer list on every call, so a
;; scan that called it once per file cost files x buffers: 14 s for 5,248
;; files and 915 buffers, every sync, with Emacs blocked.
(defvar org-linear-in-place--visiting nil
  "During a scan, the table `org-linear-in-place--visiting-table' built.")

(defun org-linear-in-place--visiting-table ()
  "Return a table from the names of visited files to their buffers.
Keys are (name . FILE-NAME), (truename . TRUENAME) and (number . FILE-NUMBER),
the three ways `find-buffer-visiting' matches a buffer; the first buffer
in `buffer-list' wins, as there."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (buffer (buffer-list) table)
      (when-let* ((name (buffer-local-value 'buffer-file-name buffer)))
        (dolist (key (list (cons 'name name)
                           (cons 'truename (buffer-local-value 'buffer-file-truename buffer))
                           (cons 'number (buffer-local-value 'buffer-file-number buffer))))
          (when (and (cdr key) (not (gethash key table)))
            (puthash key buffer table)))))))

(defun org-linear-in-place--buffer-visiting (file)
  "Return the buffer visiting FILE, as `find-buffer-visiting' does.
During a scan, look FILE up in `org-linear-in-place--visiting'; a buffer
killed since the table was built falls back to `find-buffer-visiting'."
  (let* ((table org-linear-in-place--visiting)
         (buffer
          (and table
               (let ((file (expand-file-name file)))
                 (or (gethash (cons 'name file) table)
                     (gethash (cons 'truename (abbreviate-file-name (file-truename file)))
                              table)
                     (and find-file-existing-other-name
                          (gethash (cons 'number (nthcdr 10 (file-attributes file))) table)))))))
    (if (or (null table) (and buffer (not (buffer-live-p buffer))))
        (find-buffer-visiting file)
      buffer)))

(defconst org-linear-in-place--slice 0.02
  "Seconds of work a step of the scan does before it lets Emacs go on.
The scan reads thousands of files; done at once, it would block Emacs
for seconds.  It works in slices this long, and between them Emacs
handles input and timers.")

(defconst org-linear-in-place--pause 0.01
  "Seconds the scan waits between two of its slices.")

(defun org-linear-in-place--scan ()
  "Read the files and return the sync's view of them, a plist, at once.
This is the scan of `org-linear-in-place--scan-async' run to its end
without pause; `org-linear-preview' uses it."
  (let ((scanner (org-linear-in-place--scanner)))
    (while (org-linear-in-place--scan-step scanner))
    (plist-get scanner :scan)))

(defun org-linear-in-place--scan-async (callback)
  "Read the files in slices, then call CALLBACK with the sync's view of them.
Between slices Emacs handles input, so that the scan never blocks it for
long.  The view becomes `org-linear--scan' at once, so that a sync that
fails meanwhile knows what it read."
  (let ((scanner (org-linear-in-place--scanner)))
    (setq org-linear--scan (plist-get scanner :scan))
    (org-linear-in-place--run-slices
     (lambda () (org-linear-in-place--scan-step scanner))
     (lambda () (funcall callback (plist-get scanner :scan))))))

(defun org-linear-in-place--run-slices (step done)
  "Call STEP until it returns nil, in slices, then call DONE.
Each slice calls STEP for `org-linear-in-place--slice' seconds, and the
next one starts on a timer, so that Emacs handles input in between.  An
error or a quit in a slice ends the sync."
  (let (slice)
    (setq slice
          (org-linear--bound
           (lambda ()
             (org-linear--ending-sync-on-failure
               (org-linear--note-progress "read in slices")
               (let ((deadline (+ (float-time) org-linear-in-place--slice))
                     more)
                 (while (and (setq more (funcall step)) (< (float-time) deadline)))
                 (if more
                     (run-with-timer org-linear-in-place--pause nil slice)
                   (funcall done)))))))
    (funcall slice)))

(defun org-linear-in-place--scanner ()
  "Return a new scan's state, a plist, ready for `org-linear-in-place--scan-step'.
:scan is the sync's view, as `org-linear-in-place--new-scan' describes;
:cache the scan cache; :seen the files found; :visiting the table of
visited files; :dirs the directories still to list; :files the files
found and not yet checked, as (FILE . MTIME); :index the files to index;
and :job the indexing in progress."
  (list :scan (org-linear-in-place--new-scan)
        :cache (org-linear-in-place--cache)
        :seen (make-hash-table :test #'equal)
        :visiting (org-linear-in-place--visiting-table)
        :dirs (seq-filter #'file-directory-p
                          (mapcar #'expand-file-name org-linear-directories))
        :files nil :index nil :job nil))

(defun org-linear-in-place--new-scan ()
  "Return an empty view of the files, a plist.
:present maps every issue ID found in any file to its file; :tasks maps
the IDs of linked headings in scope to markers; :new lists markers at
headings in scope that have no issue yet; :pending holds the IDs their
creation was sent with; :duplicates holds IDs with several headings;
:buffers lists the buffers the sync may edit and :opened those of them
it made itself to read a file nobody visits, which visit nothing; :ticks
maps each indexed buffer to its modification count then; :read lists the
names of the files it indexed; :unfinished lists the files holding
headings whose creation this sync sends or defers, by name, since their
headings may be in another buffer by the time they are linked; :skipped
maps files to why they are left out.  With a hierarchy, :nodes
lists the nodes of task, project and milestone headings, :node-of maps
the markers of tasks to their nodes, :objects maps the IDs of linked
projects, initiatives and milestones to (KIND . NODE), :object-pending
does the same for those being created, and :stale holds (ID . NODE) for
each project or milestone heading that still has the issue it had as a
task."
  (list :present (make-hash-table :test #'equal)
        :tasks (make-hash-table :test #'equal)
        :new nil
        :pending (make-hash-table :test #'equal)
        :duplicates (make-hash-table :test #'equal)
        :nodes nil
        :node-of (make-hash-table :test #'eq)
        :objects (make-hash-table :test #'equal)
        :object-pending (make-hash-table :test #'equal)
        :stale nil
        :buffers nil :opened nil :linked nil :unfinished nil :read nil
        :ticks (make-hash-table :test #'eq)
        :skipped (make-hash-table :test #'equal)
        :indexed (make-hash-table :test #'equal)))

(defun org-linear-in-place--scan-step (scanner)
  "Do one unit of SCANNER's work; return non-nil while work remains.
A unit is a few headings of the file being indexed, the start of the
next file to index, the check of one file found, or the listing of one
directory.  The last step forgets the cache entries of files gone."
  (let ((org-linear-in-place--visiting (plist-get scanner :visiting)))
    (cond
     ((plist-get scanner :job)
      (org-linear-in-place--index-step scanner)
      t)
     ((plist-get scanner :index)
      (let ((file (pop (plist-get scanner :index))))
        (plist-put scanner :job (org-linear-in-place--index-job file (plist-get scanner :scan))))
      t)
     ((plist-get scanner :files)
      (pcase-let ((`(,file . ,mtime) (pop (plist-get scanner :files))))
        (org-linear-in-place--scan-file file mtime scanner))
      t)
     ((plist-get scanner :dirs)
      (org-linear-in-place--list-directory (pop (plist-get scanner :dirs)) scanner)
      t)
     (t
      (when-let* ((seen (plist-get scanner :seen)))
        (let ((cache (plist-get scanner :cache)))
          (maphash (lambda (file _) (unless (gethash file seen) (remhash file cache))) cache)))
      nil))))

(defun org-linear-in-place--list-directory (dir scanner)
  "List DIR for SCANNER: its Org files to check, its subdirectories to list.
Directories whose names start with a dot are left out, as are lock files;
a symbolic link to a directory is not followed."
  (dolist (entry (directory-files-and-attributes dir t nil t))
    (let* ((file (car entry))
           (name (file-name-nondirectory file))
           (type (file-attribute-type (cdr entry))))
      (cond
       ((member name '("." "..")))
       ((eq type t)
        (unless (string-prefix-p "." name)
          (plist-put scanner :dirs (cons file (plist-get scanner :dirs)))))
       ((and (string-match-p org-linear-in-place--file-regexp name)
             (not (string-prefix-p ".#" name))
             (or (null type) (file-regular-p file)))
        (plist-put scanner :files
                   (cons (cons file (file-attribute-modification-time
                                     (if (null type) (cdr entry) (file-attributes file))))
                         (plist-get scanner :files))))))))

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

(defconst org-linear-in-place--cache-version 3
  "Version of the scan cache's entries; a new one makes the scan read again.
Version 3 drops the entries that version 2 wrote from a buffer narrowed
to a subtree: they lack the IDs of the headings outside it.")

(defun org-linear-in-place--scope-key ()
  "Return a value that changes when an option deciding the scope changes.
It also changes with `org-linear-in-place--cache-version'."
  (sxhash-equal (append (list org-linear-in-place--cache-version
                              org-linear-directories org-linear-exclude-files
                              org-linear-exclude-tags org-linear-exclude-keywords)
                        (when (org-linear-in-place--hierarchy-p)
                          (list org-linear-project-keywords
                                org-linear-milestone-keywords)))))

(defun org-linear-in-place--excluded-file-p (file)
  "Return non-nil if FILE is out of scope: excluded or an archive."
  (or (string-suffix-p "_archive" file)
      (seq-some (lambda (regexp) (string-match-p regexp file)) org-linear-exclude-files)))

(defun org-linear-in-place--scan-file (file mtime scanner)
  "Check FILE, last modified at MTIME, for SCANNER, reading it if it changed.
A file the cache shows unchanged is not read; one with headings the sync
must look at is queued for indexing."
  (let* ((scan (plist-get scanner :scan))
         (cache (plist-get scanner :cache))
         (buffer (org-linear-in-place--buffer-visiting file))
         (entry (gethash file cache))
         (dirty (and buffer (buffer-modified-p buffer)))
         (changed (not (and entry (not dirty) (equal (plist-get entry :mtime) mtime)))))
    (puthash file t (plist-get scanner :seen))
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
      (plist-put scanner :index (append (plist-get scanner :index) (list file)))))))

(defun org-linear-in-place--read-file (file buffer mtime)
  "Return a cache entry for FILE, read from BUFFER if it holds unsaved edits.
BUFFER is the buffer visiting FILE, or nil.  Without unsaved edits FILE
itself is read: the buffer may be behind it, since the sync leaves open
the buffers it edits and something else may have written the file since.
MTIME is FILE's modification time.  The entry's :uuids include the IDs
of creations in flight, so that their issues count as present."
  (with-temp-buffer
    (if (and buffer (buffer-modified-p buffer))
        (org-linear-in-place--insert-whole buffer)
      (insert-file-contents file))
    (goto-char (point-min))
    (let ((candidates (save-excursion
                        (let ((case-fold-search nil))
                          (re-search-forward (org-linear-in-place--candidate-regexp) nil t))))
          uuids)
      (while (re-search-forward (concat (org-linear-in-place--id-property-regexp)
                                        "[ \t]+\\(\\S-+\\)")
                                nil t)
        (push (match-string-no-properties 1) uuids))
      (list :mtime mtime :uuids (nreverse uuids) :candidates (and candidates t)
            :new (and candidates (org-linear-in-place--unlinked-heading-p))))))

(defun org-linear-in-place--insert-whole (buffer)
  "Insert at point the whole text of BUFFER, whatever BUFFER is narrowed to.
`insert-buffer-substring' alone takes the part BUFFER is narrowed to, and
the issues of the headings outside it would then count as gone from Org,
which archives them in Linear."
  (let ((target (current-buffer)))
    (with-current-buffer buffer
      (save-restriction
        (widen)
        (with-current-buffer target
          (insert-buffer-substring buffer))))))

(defun org-linear-in-place--unlinked-heading-p ()
  "Return non-nil if the current buffer's text may hold a heading to create.
That is an open TODO heading, by keyword and not excluded by it, whose
entry has no Linear ID, or a line setting the file's own keywords.  With
a hierarchy, a project or milestone heading counts when its entry has no
ID of a project, initiative or milestone.  Tags and habits are not
checked, so this can say yes for a heading the scan then leaves out; it
never says no for one the scan would create, except for a project
heading that needs a second object, which the scan finds when its file
changes and `org-linear-in-place--refresh-cache' remembers."
  (let ((case-fold-search nil)
        (objects (and (org-linear-in-place--hierarchy-p)
                      (append org-linear-project-keywords org-linear-milestone-keywords))))
    (goto-char (point-min))
    (or (and (re-search-forward "^#\\+\\(?:SEQ_\\|TYP_\\)?TODO:" nil t) t)
        (org-linear-in-place--unlinked-p
         (seq-difference (org-linear-in-place--open-keywords)
                         (append org-linear-exclude-keywords objects))
         "^[ \t]*:LINEAR_\\(?:PENDING_\\)?UUID:")
        (and objects
             (org-linear-in-place--unlinked-p
              (seq-difference objects org-linear-exclude-keywords)
              "^[ \t]*:LINEAR_\\(?:PENDING_\\)?\\(?:PROJECT\\|INITIATIVE\\|MILESTONE\\):")))))

(defun org-linear-in-place--unlinked-p (keywords id)
  "Return non-nil if a heading with one of KEYWORDS lacks a line matching ID.
The line must be in the heading's entry, before the next heading."
  (let ((heading (concat "^\\*+ +" (regexp-opt keywords) "\\(?: \\|$\\)"))
        found)
    (goto-char (point-min))
    (while (and (not found) (re-search-forward heading nil t))
      (let ((end (save-excursion
                   (if (re-search-forward "^\\*+ " nil t) (match-beginning 0) (point-max)))))
        (unless (save-excursion (re-search-forward id end t))
          (setq found t))))
    found))

(defconst org-linear-in-place--headings-per-step 25
  "Headings an indexing step reads before the scan checks the time.")

(defun org-linear-in-place--index-file (file scan)
  "Add the headings of FILE to SCAN at once, unless it cannot be edited now.
Return the buffer that holds them, or nil.  A file already indexed,
under whatever name, is not indexed again, so that no heading is counted
twice."
  (or (org-linear-in-place--indexed-buffer file scan)
      (when-let* ((job (org-linear-in-place--index-job file scan)))
        (while (not (org-linear-in-place--index-headings job scan most-positive-fixnum)))
        (org-linear-in-place--finish-job job scan)
        (plist-get job :buffer))))

(defun org-linear-in-place--indexed-buffer (file scan)
  "Return the buffer SCAN indexed FILE in, under whatever name, or nil.
That is a buffer visiting FILE, or one the sync read FILE in."
  (let ((buffers (plist-get scan :buffers)))
    (or (car (memq (find-buffer-visiting file) buffers))
        (let ((truename (file-truename file)))
          (seq-find (lambda (buffer)
                      (and (buffer-live-p buffer)
                           (equal truename
                                  (plist-get (buffer-local-value 'org-linear-in-place--read
                                                                 buffer)
                                             :truename))))
                    buffers)))))

(defun org-linear-in-place--index-job (file scan)
  "Return the state of indexing FILE into SCAN, a plist, or nil.
Nil means FILE is not indexed: it is already, under whatever name, or
its buffer cannot be edited now.  The state holds the :buffer, the
position to go on from, the buffer's modification count, and what the
headings read so far added up to."
  (unless (org-linear-in-place--indexed-buffer file scan)
    (when-let* ((buffer (org-linear-in-place--usable-buffer file scan)))
      (with-current-buffer buffer
        (list :buffer buffer
              :pos (save-restriction (widen) (copy-marker (point-min)))
              :tick (buffer-chars-modified-tick)
              :hierarchy (org-linear-in-place--hierarchy-p)
              :new 0 :uuids nil :stack nil :nodes nil)))))

(defun org-linear-in-place--index-step (scanner)
  "Read the next headings of SCANNER's indexing job; end the job at its file's end.
A buffer edited or killed since the job began is left out of the sync,
and the log says so; its headings neither pull nor push until its next
sync."
  (let* ((job (plist-get scanner :job))
         (scan (plist-get scanner :scan))
         (buffer (plist-get job :buffer)))
    (cond
     ((not (and (buffer-live-p buffer)
                (= (plist-get job :tick) (buffer-chars-modified-tick buffer))))
      (org-linear--log "Skipped %s: it changed while the sync read it"
                       (if (buffer-live-p buffer) (buffer-name buffer) "a killed buffer"))
      (org-linear-in-place--forget-buffer buffer)
      (set-marker (plist-get job :pos) nil)
      (plist-put scanner :job nil))
     ((org-linear-in-place--index-headings job scan org-linear-in-place--headings-per-step)
      (org-linear-in-place--finish-job job scan)
      (plist-put scanner :job nil)))))

(defun org-linear-in-place--index-headings (job scan count)
  "Add at most COUNT more headings of JOB's buffer to SCAN.
Return non-nil once the buffer's last heading is read.  With a
hierarchy, each task, project and milestone heading also gets a node;
see `org-linear-in-place--node'."
  (with-current-buffer (plist-get job :buffer)
    (org-with-wide-buffer
     (goto-char (plist-get job :pos))
     (let ((hierarchy (plist-get job :hierarchy))
           (done nil))
       (while (and (> count 0) (not done))
         (if (not (re-search-forward org-outline-regexp-bol nil t))
             (setq done t)
           (cl-decf count)
           (org-linear-in-place--index-heading job scan hierarchy)
           (end-of-line)))
       (set-marker (plist-get job :pos) (point))
       done))))

(defun org-linear-in-place--index-heading (job scan hierarchy)
  "Add the heading at point to SCAN, recording what it adds up to in JOB.
HIERARCHY is non-nil when projects and milestones sync."
  (let* ((uuid (org-entry-get nil "LINEAR_UUID"))
         (pending (org-entry-get nil "LINEAR_PENDING_UUID"))
         (keyword (org-get-todo-state))
         (objects (and hierarchy (org-linear-in-place--heading-objects)))
         (kind (org-linear-in-place--heading-kind keyword uuid objects))
         (stack (plist-get job :stack)))
    (when uuid (push uuid (plist-get job :uuids)))
    (when pending (push pending (plist-get job :uuids)))
    (dolist (object objects) (push (nth 1 object) (plist-get job :uuids)))
    (when hierarchy
      (setq stack (org-linear-in-place--pop-stack stack)))
    (when (and kind (not (org-linear-in-place--excluded-heading-p keyword)))
      (let ((node (and hierarchy (org-linear-in-place--node kind keyword stack scan))))
        (when node
          (push node stack)
          (push node (plist-get job :nodes)))
        (if (eq kind 'task)
            (when (org-linear-in-place--index-task uuid pending keyword node scan)
              (plist-put job :new (1+ (plist-get job :new))))
          (org-linear-in-place--index-object node objects scan)
          (when uuid
            (plist-put scan :stale (cons (cons uuid node) (plist-get scan :stale)))))))
    (plist-put job :stack stack)))

(defun org-linear-in-place--finish-job (job scan)
  "Record in SCAN what JOB's headings added up to, and that its buffer was indexed.
The scan cache learns the buffer's IDs and whether it holds headings to
create."
  (let* ((buffer (plist-get job :buffer))
         (file (org-linear--buffer-file buffer))
         (nodes (nreverse (plist-get job :nodes)))
         (new (plist-get job :new))
         (uuids (plist-get job :uuids)))
    (set-marker (plist-get job :pos) nil)
    (when (plist-get job :hierarchy)
      (plist-put scan :nodes (append (plist-get scan :nodes) nodes))
      (cl-incf new (seq-count #'org-linear-in-place--missing-objects nodes)))
    (dolist (uuid uuids)
      (puthash uuid file (plist-get scan :present)))
    (when-let* ((entry (gethash file (org-linear-in-place--cache))))
      (puthash file (plist-put (plist-put entry :new (> new 0)) :uuids uuids)
               (org-linear-in-place--cache)))
    (push buffer (plist-get scan :buffers))
    (push file (plist-get scan :read))
    (puthash buffer (buffer-chars-modified-tick buffer) (plist-get scan :ticks))))

(defun org-linear-in-place--forget-buffer (buffer)
  "Leave the headings of BUFFER out of the sync.
Their issues stay present, so they are neither inserted again nor
archived; they sync once the buffer is saved.  Its file is recorded as
dropped, so that creations from it wait.  A buffer the sync made itself
to read the file in is killed."
  (let ((scan org-linear--scan))
    (when-let* ((file (and (buffer-live-p buffer) (org-linear--buffer-file buffer))))
      (plist-put scan :dropped (cons file (plist-get scan :dropped))))
    (maphash (lambda (id marker)
               (when (eq (marker-buffer marker) buffer)
                 (remhash id (plist-get scan :tasks))))
             (plist-get scan :tasks))
    (plist-put scan :new (seq-remove (lambda (marker) (eq (marker-buffer marker) buffer))
                                     (plist-get scan :new)))
    (org-linear-in-place--drop-nodes buffer)
    (plist-put scan :buffers (remq buffer (plist-get scan :buffers)))
    (when (memq buffer (plist-get scan :opened))
      (plist-put scan :opened (remq buffer (plist-get scan :opened)))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(defun org-linear-in-place--usable-buffer (file scan)
  "Return a buffer holding the text of FILE that the sync may work in, or nil.
A buffer visiting FILE with unsaved edits is not one; one whose file
changed on disk is reverted first.  A file nobody visits is read in a
buffer of the sync's own that visits nothing, recorded in SCAN; see
`org-linear-in-place--read-buffer'."
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
      (setq buffer (org-linear-in-place--read-buffer file))
      (plist-put scan :opened (cons buffer (plist-get scan :opened)))
      buffer))))

(defun org-linear-in-place--read-buffer (file)
  "Return a new buffer holding the text of FILE, in Org mode, that visits nothing.
The sync reads the files nobody visits in such buffers.  Nothing finds
one by its file or comes to point into it, so the sync can kill it when
done, which it must never do to a buffer that visits a file.  Nobody
edits in one either, so the hooks that set a buffer up for editing do
not run, and Org skips what it does to show a file.  File and directory
local variables are set as a visit sets them, but only the safe ones, so
that a background sync asks nothing.  A FILE that does not exist yet, as
a new inbox, gives an empty buffer.  `org-linear--file' holds FILE there,
and `org-linear-in-place--read' what FILE was like."
  (let ((buffer (generate-new-buffer
                 (format " *org-linear %s*" (file-name-nondirectory file)) t))
        (attributes (file-attributes file)))
    (with-current-buffer buffer
      (when attributes
        (insert-file-contents file))
      (setq default-directory (file-name-directory (expand-file-name file)))
      (let ((org-inhibit-startup t)
            (enable-local-variables (and enable-local-variables :safe))
            (inhibit-message t))
        (delay-mode-hooks (org-mode))
        (with-demoted-errors "Error in the local variables of an Org file: %S"
          (hack-local-variables 'no-mode)))
      (setq org-linear--file file
            org-linear-in-place--read
            (list :time (file-attribute-modification-time attributes)
                  :size (file-attribute-size attributes)
                  :hash (buffer-hash)
                  :truename (file-truename file)))
      (set-buffer-modified-p nil))
    buffer))

(defun org-linear-in-place--index-task (uuid pending keyword node scan)
  "Add the task heading at point to SCAN; return non-nil if it has no issue.
UUID is its issue's ID, PENDING the ID its creation was sent with, and
KEYWORD its TODO keyword.  NODE is its node, or nil without a hierarchy."
  (let ((marker (lambda () (if node (plist-get node :marker) (org-linear--marker)))))
    (cond
     ((and uuid (gethash uuid (plist-get scan :tasks)))
      (puthash uuid t (plist-get scan :duplicates))
      nil)
     (uuid
      (puthash uuid (funcall marker) (plist-get scan :tasks))
      nil)
     ((member keyword org-not-done-keywords)
      (when pending
        (puthash pending t (plist-get scan :pending)))
      (plist-put scan :new (append (plist-get scan :new) (list (funcall marker))))
      t))))

(defun org-linear-in-place--excluded-heading-p (keyword)
  "Return non-nil if the heading at point, whose keyword is KEYWORD, is excluded."
  (or (member keyword org-linear-exclude-keywords)
      (equal (org-entry-get nil "STYLE") "habit")
      (seq-intersection (org-get-tags) org-linear-exclude-tags)))

;;;; Fetching

(defun org-linear-in-place--start ()
  "Scan the files in slices, then fetch what changed in Linear."
  (let ((started (format-time-string "%FT%TZ" (time-subtract nil 120) t)))
    (setq org-linear--body-overrides (make-hash-table :test #'equal))
    (org-linear-in-place--scan-async
     (lambda (scan)
       (maphash (lambda (file reason) (org-linear--log "Skipped %s: %s"
                                                       (abbreviate-file-name file) reason))
                (plist-get scan :skipped))
       (org-linear-in-place--fetch
        (lambda (data)
          (org-linear-in-place--fetch-objects
           (lambda (objects)
             (push (cons 'objects objects) data)
             (org-linear-in-place--locate
              data
              (lambda ()
                (org-linear-in-place--ensure-labels
                 (lambda ()
                   (org-linear-in-place--on-fetch data started)))))))))))))

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
                 (mapcar (lambda (buffer)
                           (org-linear--file-label-name (org-linear--buffer-file buffer)))
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
   (append (org-linear--conversion-items data (plist-get org-linear--scan :present))
           (org-linear-in-place--object-conversion-items data))
   (lambda (converted)
     (org-linear-in-place--merge-bodies
      (org-linear-in-place--body-pulls data converted)
      (lambda ()
        (org-linear-in-place--apply-and-send data converted started))))))

(defun org-linear-in-place--locate (data callback)
  "Index the files of the issues in DATA whose files the scan did not visit.
The inbox file is indexed too, in case new issues go there, and so are
the files of the projects, initiatives and milestones that changed or
went in Linear.  The files are indexed in slices, as the scan's are;
then CALLBACK is called."
  (let ((scanner (list :scan org-linear--scan :index (org-linear-in-place--files-to-locate data)
                       :job nil)))
    (org-linear-in-place--run-slices
     (lambda () (org-linear-in-place--scan-step scanner))
     callback)))

(defun org-linear-in-place--files-to-locate (data)
  "Return the files `org-linear-in-place--locate' indexes for DATA."
  (let ((files nil))
    (when org-linear-inbox-file
      (let ((inbox (expand-file-name org-linear-inbox-file)))
        (unless (gethash inbox (plist-get org-linear--scan :skipped))
          (push inbox files))))
    (dolist (task (alist-get 'tasks data))
      (when-let* ((file (org-linear-in-place--file-to-locate (alist-get 'id task))))
        (push file files)))
    (when (org-linear-in-place--hierarchy-p)
      (let ((objects (org-linear-in-place--objects-by-id (alist-get 'objects data))))
        (maphash (lambda (id base)
                   (let ((object (gethash id objects)))
                     (when (or (null object)
                               (plist-get object :trashed)
                               (not (equal (plist-get object :edited) (plist-get base :edited))))
                       (when-let* ((file (org-linear-in-place--file-to-locate id)))
                         (push file files)))))
                 (org-linear--state-objects))))
    (delete-dups (nreverse files))))

(defun org-linear-in-place--file-to-locate (id)
  "Return the file holding ID, of an issue or object, if the sync should index it.
That is a file the scan did not index and does not leave out."
  (let ((file (gethash id (plist-get org-linear--scan :present))))
    (when (and file
               (not (gethash id (plist-get org-linear--scan :tasks)))
               (not (gethash id (plist-get org-linear--scan :objects)))
               (not (org-linear-in-place--excluded-file-p file))
               (not (gethash file (plist-get org-linear--scan :skipped)))
               (not (gethash file (plist-get org-linear--scan :indexed))))
      (puthash file t (plist-get org-linear--scan :indexed))
      file)))

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
    (append (nreverse pulls) (org-linear-in-place--object-body-pulls data converted))))

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
                (puthash id (plist-put (copy-sequence base) :held-md md)
                         (or (org-linear--table-of id) (org-linear--state-tasks)))
                (puthash id :hold org-linear--body-overrides)
                (funcall next))))
    (if (not org-linear-merge-body-function)
        (funcall hold "the Org body has markup Markdown lacks; set `org-linear-merge-body-function'")
      (funcall org-linear-merge-body-function
               body old-md md
               (org-linear--bound
                (lambda (merged)
                  ;; The rest of the sync runs in the merge function's
                  ;; callback, a model's reply, outside the sync's own.
                  (org-linear--ending-sync-on-failure
                    (if (not (stringp merged))
                        (funcall hold "the merge failed")
                      (org-linear-in-place--check-merge
                       merged md
                       (lambda (ok)
                         (if ok
                             (progn (puthash id (string-trim merged) org-linear--body-overrides)
                                    (funcall next))
                           (funcall hold "the merge does not say what Linear's text says"))))))))))))

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
The changes are applied in slices; see `org-linear-in-place--apply-in-slices'.
STARTED becomes the next sync's starting point."
  (let ((plan (org-linear-in-place--apply-units data converted)))
    (org-linear-in-place--apply-in-slices
     (car plan)
     (lambda ()
       (let ((ops (org-linear-in-place--without-dropped (funcall (cdr plan)))))
         (org-linear-in-place--save-buffers)
         (plist-put org-linear--state :since started)
         (org-linear--save-state)
         (org-linear--run-ops ops))))))

(defun org-linear-in-place--apply-in-slices (units done)
  "Run UNITS, the steps that apply a sync's changes, in slices; then call DONE.
A unit returns nil, or more units to run before the rest.  Each slice
runs units as sync edits for `org-linear-in-place--slice' seconds, and
at least one, and the next starts on a timer, so that Emacs handles
input in between.  A buffer edited since the sync last touched it
leaves the sync before the next slice, so that the sync never edits or
saves a buffer of yours.  A file the slice edited in a buffer the sync
read it in is visited before the slice ends, and its edits go on in the
visiting buffer; see `org-linear-in-place--visit-edited'."
  (let (slice)
    (setq slice
          (org-linear--bound
           (lambda ()
             (org-linear--ending-sync-on-failure
               (org-linear--note-progress "applied changes in slices")
               (org-linear-in-place--drop-changed-buffers)
               (let ((deadline (+ (float-time) org-linear-in-place--slice)))
                 (org-linear-in-place--with-edits
                  (lambda ()
                    (while (progn
                             (let ((more (and units (funcall (pop units)))))
                               (when (consp more)
                                 (setq units (append more units))))
                             (and units (< (float-time) deadline)))))))
               (org-linear-in-place--visit-edited)
               (org-linear-in-place--record-ticks)
               (if units
                   (run-with-timer org-linear-in-place--pause nil slice)
                 (funcall done))))))
    (funcall slice)))

(defun org-linear-in-place--record-ticks ()
  "Record the modification count of each buffer the sync may edit, as it is now."
  (dolist (buffer (plist-get org-linear--scan :buffers))
    (when (buffer-live-p buffer)
      (puthash buffer (buffer-chars-modified-tick buffer) (plist-get org-linear--scan :ticks)))))

(defun org-linear-in-place--without-dropped (ops)
  "Return OPS without the creations from files that left the sync while it applied.
Their headings' pending IDs were not saved, so the creations wait."
  (let ((dropped (plist-get org-linear--scan :dropped)))
    (if (null dropped)
        ops
      (seq-remove (lambda (op)
                    (let ((place (or (plist-get op :place)
                                     (and (not (markerp (plist-get op :marker)))
                                          (plist-get op :marker)))))
                      (and place (member (plist-get place :file) dropped))))
                  ops))))

(defun org-linear-in-place--drop-changed-buffers ()
  "Leave out of this sync the buffers edited or changed on disk since it read them.
An edit counts whether or not it was saved since: the sync compares each
buffer's modification count with the one it recorded when it read or
last edited the buffer, so that its own edits do not count.  Their
headings stay present, so their issues are neither inserted again nor
archived; they sync once the buffer is saved.  A file the sync read in a
buffer of its own, and that someone visited since, goes on in the
visiting buffer if that one holds what the sync read, and is left out
otherwise."
  (org-linear-in-place--move-to-visitors)
  (mapc #'org-linear-in-place--leave-out
        (seq-filter #'org-linear-in-place--changed-p (plist-get org-linear--scan :buffers))))

(defun org-linear-in-place--leave-out (buffer)
  "Leave BUFFER, which changed during the sync, out of it, and log that."
  (org-linear--log "Skipped %s: it changed during the sync"
                   (cond ((not (buffer-live-p buffer)) "a killed buffer")
                         ((memq buffer (plist-get org-linear--scan :opened))
                          (file-name-nondirectory (org-linear--buffer-file buffer)))
                         (t (buffer-name buffer))))
  (org-linear-in-place--forget-buffer buffer))

(defun org-linear-in-place--changed-p (buffer)
  "Return non-nil if BUFFER no longer holds what the sync read or wrote in it.
A buffer visiting a file changed if it was edited, or its file written,
since the sync last touched it.  A buffer the sync read a file in
changed if the file was written since."
  (cond
   ((not (buffer-live-p buffer)) t)
   ((memq buffer (plist-get org-linear--scan :opened))
    (not (org-linear-in-place--unchanged-on-disk-p buffer)))
   (t (or (not (verify-visited-file-modtime buffer))
          (not (eql (gethash buffer (plist-get org-linear--scan :ticks))
                    (buffer-chars-modified-tick buffer)))))))

(defun org-linear-in-place--unchanged-on-disk-p (reading)
  "Return non-nil if the file of READING is as the sync read it.
READING is a buffer the sync read the file in; the file's modification
time and size are compared with those it had then."
  (let ((read (buffer-local-value 'org-linear-in-place--read reading))
        (attributes (file-attributes (org-linear--buffer-file reading))))
    (and (equal (file-attribute-modification-time attributes) (plist-get read :time))
         (equal (file-attribute-size attributes) (plist-get read :size)))))

(defun org-linear-in-place--move-to-visitors ()
  "Go on in the buffers that by now visit files the sync read itself.
A file someone visited since the sync read it is edited in the visiting
buffer from here on, if that buffer holds what the sync read and has no
unsaved edits; otherwise the file is left out of the sync.  The visiting
buffers are looked up in one table, built here, as the scan does."
  (when-let* ((readings (plist-get org-linear--scan :opened))
              (table (org-linear-in-place--visiting-table)))
    (dolist (reading (copy-sequence readings))
      (when-let* ((visiting (and (buffer-live-p reading)
                                 (org-linear-in-place--visiting-in table reading))))
        (if (org-linear-in-place--holds-what-was-read-p visiting reading)
            (org-linear-in-place--hand-over reading visiting)
          (org-linear-in-place--leave-out reading))))))

(defun org-linear-in-place--visiting-in (table reading)
  "Return the buffer of TABLE that visits the file READING was read from, or nil.
TABLE is as `org-linear-in-place--visiting-table' returns it."
  (or (gethash (cons 'name (expand-file-name (org-linear--buffer-file reading))) table)
      (gethash (cons 'truename
                     (abbreviate-file-name
                      (plist-get (buffer-local-value 'org-linear-in-place--read reading)
                                 :truename)))
               table)))

(defun org-linear-in-place--holds-what-was-read-p (visiting reading)
  "Return non-nil if VISITING holds, saved, the text the sync read in READING.
VISITING visits the file READING was read from."
  (and (not (buffer-modified-p visiting))
       (verify-visited-file-modtime visiting)
       (equal (buffer-hash visiting)
              (plist-get (buffer-local-value 'org-linear-in-place--read reading) :hash))))

(defun org-linear-in-place--visit-edited ()
  "Hand each file the sync edited in a buffer of its own to a buffer visiting it.
The sync reads the files nobody visits in buffers that visit nothing.
Once it has edited one, it visits the file as you would, with your
hooks, gives that buffer the edits and goes on in it; the buffer is
saved when the sync ends and stays open.  If the file is no longer as
the sync read it, the edits have nowhere to go; see
`org-linear-in-place--lose-edits'."
  (dolist (reading (copy-sequence (plist-get org-linear--scan :opened)))
    (when (and (buffer-live-p reading) (buffer-modified-p reading))
      (if-let* ((visiting (org-linear-in-place--visitor reading)))
          (org-linear-in-place--hand-over reading visiting)
        (org-linear-in-place--lose-edits reading)))))

(defun org-linear-in-place--visitor (reading)
  "Return a buffer visiting READING's file that can take the sync's edits, or nil.
A buffer that visits the file already must hold what the sync read, with
no unsaved edits.  If nobody visits the file, and it is on disk as the
sync read it, the sync visits it."
  (let* ((file (org-linear--buffer-file reading))
         (visiting (find-buffer-visiting file)))
    (cond
     (visiting (and (org-linear-in-place--holds-what-was-read-p visiting reading) visiting))
     ((org-linear-in-place--unchanged-on-disk-p reading) (org-linear-in-place--visit file)))))

(defun org-linear-in-place--hand-over (reading visiting)
  "Go on in VISITING with what the sync read, and edited, in READING.
VISITING visits the file READING was read from.  It takes READING's
text, as a sync edit, if the two differ; the scan's markers in READING
move to the same places of VISITING, which takes READING's place among
the buffers the sync may edit; and READING is killed."
  (let ((scan org-linear--scan))
    (with-current-buffer visiting
      (unless (derived-mode-p 'org-mode) (org-mode))
      (org-linear--with-sync-edits
       (lambda () (org-linear-in-place--take-text reading))))
    (dolist (marker (org-linear-in-place--scan-markers scan))
      (when (eq (marker-buffer marker) reading)
        (set-marker marker (marker-position marker) visiting)))
    (plist-put scan :buffers (cl-substitute visiting reading (plist-get scan :buffers)))
    (plist-put scan :opened (remq reading (plist-get scan :opened)))
    (remhash reading (plist-get scan :ticks))
    (puthash visiting (buffer-chars-modified-tick visiting) (plist-get scan :ticks))
    (kill-buffer reading)))

(defun org-linear-in-place--take-text (source)
  "Give the current buffer, widened, the text of the buffer SOURCE.
The text the two share stays where it is, with the markers in it."
  (unless (and (= (buffer-size) (buffer-size source))
               (equal (buffer-hash) (buffer-hash source)))
    (replace-region-contents (point-min) (point-max) (lambda () source) 1)))

(defun org-linear-in-place--scan-markers (scan)
  "Return the markers SCAN holds at headings: those of its tasks and of its nodes."
  (append (hash-table-values (plist-get scan :tasks))
          (plist-get scan :new)
          (mapcar (lambda (node) (plist-get node :marker)) (plist-get scan :nodes))))

(defun org-linear-in-place--lose-edits (reading)
  "End the sync, whose edits to the file of READING cannot reach it.
The file changed, or a buffer visiting it did, while the sync held edits
to it in READING.  What the sync recorded of its merge would then say
Org holds values it does not, and the next sync would send Linear its
old values back.  So the record is dropped, for the next sync to start
from the one saved last, and this sync ends as failed."
  (let ((file (org-linear--buffer-file reading)))
    (setq org-linear--state nil)
    (error "%s changed while the sync edited it, so the sync kept nothing; the next one starts afresh"
           (abbreviate-file-name file))))

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
  "Apply DATA to the files at once and return the edits to send.
CONVERTED maps conversion keys to Org text."
  (let* ((plan (org-linear-in-place--apply-units data converted))
         (units (car plan)))
    (while units
      (let ((more (funcall (pop units))))
        (when (consp more)
          (setq units (append more units)))))
    (funcall (cdr plan))))

(defun org-linear-in-place--apply-units (data converted)
  "Return (UNITS . ASSEMBLE): the steps that apply DATA, and how to get the edits.
Each of UNITS applies a part of DATA to the files; ASSEMBLE, called once
they all ran, returns the edits to send, in the order Linear needs them.
CONVERTED maps conversion keys to Org text."
  (if (org-linear-in-place--hierarchy-p)
      (org-linear-in-place--hierarchy-units data converted)
    (org-linear-in-place--flat-units data converted)))

(defun org-linear-in-place--task-units (data converted fetched collect)
  "Return a unit for each issue of DATA that applies it to its heading.
Each records the issue's ID in FETCHED and calls COLLECT with its edits.
CONVERTED maps conversion keys to Org text."
  (let ((comments (org-linear--comments-by-issue data converted)))
    (mapcar (lambda (task)
              (lambda ()
                (let ((id (alist-get 'id task)))
                  (puthash id t fetched)
                  (funcall collect (org-linear-in-place--apply-task
                                    task converted (gethash id comments)))
                  nil)))
            (alist-get 'tasks data))))

(defun org-linear-in-place--flat-units (data converted)
  "Return the units and assembly that apply DATA without a hierarchy.
See `org-linear-in-place--apply-units'; CONVERTED is as there."
  (let ((fetched (make-hash-table :test #'equal))
        edits tail)
    (cons (append (org-linear-in-place--task-units
                   data converted fetched (lambda (ops) (push ops edits)))
                  (list (lambda ()
                          (setq tail (nconc (org-linear-in-place--vanished-ops fetched)
                                            (org-linear-in-place--creation-ops org-linear--scan)))
                          nil)))
          (lambda () (append (apply #'append (nreverse edits)) tail)))))

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
    (let ((ops (org-linear--merge task converted comments nil nil marker base))
          (node (and (org-linear-in-place--hierarchy-p)
                     (gethash marker (plist-get org-linear--scan :node-of)))))
      (when held (remhash id org-linear--body-overrides))
      (when (and held base)
        (let ((new-base (gethash id (org-linear--state-tasks))))
          (puthash id (plist-put new-base :desc-md (plist-get base :desc-md))
                   (org-linear--state-tasks))))
      (if node
          (append ops (org-linear-in-place--issue-relation-ops task node))
        ops))))

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
      (org-linear--set-todo keyword closed-at))
     ((equal (org-entry-get nil "LINEAR_REPEATED") closed-at))
     (t
      (org-linear--set-todo keyword nil)
      (org-entry-put nil "LINEAR_REPEATED" closed-at)
      (setq org-linear--rerun t)))))

(defun org-linear-in-place--vanished-ops (fetched)
  "Return edits that archive the issues whose headings left Org.
Those are issues synced before whose IDs no file holds now.  FETCHED holds
the IDs this sync fetched; it does not change the answer, since a trashed
issue has already left the state.  With a hierarchy, projects and
initiatives are archived and milestones deleted on the same terms, after
the issues.  More than `org-linear-archive-limit' of them in all archive
none.  One whose archive Linear refused is not sent again."
  (let ((present (plist-get org-linear--scan :present))
        gone objects)
    (ignore fetched)
    (maphash (lambda (id base)
               (unless (or (gethash id present) (org-linear-in-place--archive-refused-p id base))
                 (push id gone)))
             (org-linear--state-tasks))
    (when (org-linear-in-place--hierarchy-p)
      (maphash (lambda (id base)
                 (unless (or (gethash id present) (org-linear-in-place--archive-refused-p id base))
                   (push (cons id (plist-get base :kind)) objects)))
               (org-linear--state-objects)))
    (cond
     ((> (+ (length gone) (length objects)) org-linear-archive-limit)
      (if objects
          (org-linear--log "Not archived: the headings of %d issues and %d projects, initiatives or milestones are gone, more than `org-linear-archive-limit'; check `org-linear-directories'"
                           (length gone) (length objects))
        (org-linear--log "Not archived: the headings of %d issues are gone, more than `org-linear-archive-limit'; check `org-linear-directories'"
                         (length gone)))
      nil)
     (t
      (append (mapcar (lambda (id) (list :type 'archive :id id)) gone)
              (mapcar (lambda (object)
                        (list :type 'object-archive :id (car object) :kind (cdr object)))
                      (sort objects (lambda (a b)
                                      (< (seq-position '(milestone project initiative) (cdr a))
                                         (seq-position '(milestone project initiative) (cdr b)))))))))))

(defun org-linear-in-place--archive-refused-p (id base)
  "Return non-nil if Linear refused to archive ID, as its BASE records."
  (member (org-linear--op-signature (list :type 'archive :id id)) (plist-get base :rejected)))

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
     (org-linear-in-place--archive-refused op next))))

(defun org-linear-in-place--archive-refused (op next)
  "Return a handler for an archive of OP that Linear refused; it calls NEXT.
When Linear no longer has the object, there is nothing left to archive:
the sync forgets it.  Any other refusal is remembered, so that the
archive is not sent again."
  (lambda (message &rest args)
    (if (not (org-linear--not-found-p message))
        (apply (org-linear--rejected-handler op next) message args)
      (let ((id (plist-get op :id)))
        (remhash id (org-linear--state-tasks))
        (remhash id (org-linear--state-objects))
        (org-linear--save-state)
        (org-linear--log "Forgot %s: its heading is no longer in Org, and Linear no longer has it" id)
        (funcall next)))))

;;;; Creating issues

(defun org-linear-in-place--creation-ops (scan &optional limit)
  "Return edits that create issues for the new headings in SCAN.
At most LIMIT, by default `org-linear-create-limit', are created; the
rest wait for the next scheduled sync, not an immediate rerun, so that a
large import keeps to Linear's hourly request limit.  Each heading gets
the ID of its issue as LINEAR_PENDING_UUID before anything is sent, and
its file is saved, so that a lost answer is found again rather than
repeated.  With a hierarchy, each issue is created with the relations
its heading's place gives it."
  (let* ((limit (or limit org-linear-create-limit))
         (new (plist-get scan :new))
         (now (seq-take new limit))
         ops)
    (when (nthcdr limit new)
      (org-linear--log "Creating %d of %d new headings; the rest follow in the next syncs"
                       limit (length new)))
    (dolist (marker (nthcdr limit new))
      (org-linear-in-place--mark-unfinished (marker-buffer marker)))
    (dolist (marker now)
      (let* ((view (org-linear--local-view marker))
             (pending (or (org-entry-get marker "LINEAR_PENDING_UUID")
                          (let ((id (org-id-uuid)))
                            (org-entry-put marker "LINEAR_PENDING_UUID" id)
                            id)))
             (relations (org-linear-in-place--new-issue-relations marker))
             (op (list :type 'add
                       :marker (list :file (org-linear--buffer-file (marker-buffer marker))
                                     :pending pending :relations relations)
                       :view view :pending pending :relations relations
                       :team (alist-get 'id (car (plist-get org-linear--meta :teams))))))
        (if (member (org-linear--op-signature op) (plist-get org-linear--state :rejected-adds))
            (org-linear--log "Not created again: Linear refused \"%s\"; change it to retry"
                             (plist-get view :title))
          (org-linear-in-place--mark-unfinished (marker-buffer marker))
          (push op ops))))
    (nreverse ops)))

(defun org-linear-in-place--mark-unfinished (buffer)
  "Record that BUFFER holds headings whose creation is sent or deferred.
The next sync reads its file again, whatever the file's text suggests, so
that a creation that failed before Linear acted on it is sent again.  A
heading whose creation Linear refused does not count: it waits until it
changes."
  (let ((file (org-linear--buffer-file buffer)))
    (unless (member file (plist-get org-linear--scan :unfinished))
      (plist-put org-linear--scan :unfinished
                 (cons file (plist-get org-linear--scan :unfinished))))))

(defun org-linear-in-place--link-created (place view md task)
  "Link TASK, created from VIEW and sent with description MD, to its heading.
PLACE is a plist with the heading's :file and :pending ID, and the
:relations the issue was created with.  A heading whose file has unsaved
edits is linked by a later sync."
  (let ((base (append (copy-sequence view)
                      (list :desc-md (org-linear--stored-description task md)
                            :edited (alist-get 'last_edited_time task)
                            :comment-ids nil :note-keys nil)
                      (when-let* ((relations (plist-get place :relations)))
                        (list :org-rel relations :lin-rel relations)))))
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
  "Call EDIT at the start of FILE, widened, as a sync edit.
FILE is edited in the buffer that visits it, which the sync opens if
nobody has, and which stays open.  The buffer is saved once, when the
sync ends; see `org-linear-in-place--flush'.  Nothing happens if the
buffer has edits of its own: unsaved ones before the sync touched it, or
any since."
  (let* ((buffer (or (find-buffer-visiting file) (org-linear-in-place--visit file)))
         (entry (seq-find (lambda (e) (eq (plist-get e :buffer) buffer))
                          (plist-get org-linear--scan :linked))))
    (if (if entry
            (/= (plist-get entry :tick) (buffer-chars-modified-tick buffer))
          (buffer-modified-p buffer))
        (org-linear--log "Not linked yet: %s has edits of yours; the next sync links it"
                         (abbreviate-file-name file))
      (with-current-buffer buffer
        (unless entry
          (unless (derived-mode-p 'org-mode) (org-mode))
          (unless (verify-visited-file-modtime buffer)
            (let ((inhibit-message t)) (revert-buffer t t t))))
        (org-linear--with-sync-edits
         (lambda () (goto-char (point-min)) (funcall edit))))
      (if entry
          (plist-put entry :tick (buffer-chars-modified-tick buffer))
        (plist-put org-linear--scan :linked
                   (cons (list :buffer buffer :tick (buffer-chars-modified-tick buffer))
                         (plist-get org-linear--scan :linked)))))))

(defun org-linear-in-place--flush ()
  "Save the buffers the sync linked issues in, each once; they stay open.
A buffer edited by you since the sync's last edit is left unsaved, with
the sync's edits in it, and the log says so."
  (dolist (entry (plist-get org-linear--scan :linked))
    (let ((buffer (plist-get entry :buffer)))
      (when (buffer-live-p buffer)
        (if (/= (plist-get entry :tick) (buffer-chars-modified-tick buffer))
            (org-linear--log "Not saved: %s has edits of yours; save it to keep the new links"
                             (buffer-name buffer))
          (org-linear--save buffer)
          (org-linear-in-place--refresh-cache buffer)))))
  (plist-put org-linear--scan :linked nil))

;;;; Visiting files

(defun org-linear-in-place--visit (file)
  "Visit FILE as you would, for the sync to edit it, and return its buffer.
The buffer is set up by your own hooks and stays open when the sync is
done.  The sync used to visit files without hooks and kill their buffers
afterwards; but another package may come to point into a buffer that
visits a file, and would be left pointing nowhere, or would take over a
buffer your hooks never set up."
  (let ((inhibit-message t))
    (find-file-noselect file t)))

;;;; The inbox

(defun org-linear-in-place--insert-in-inbox (task converted comments)
  "Insert TASK, created in Linear, under the inbox heading; return edits to send.
CONVERTED maps conversion keys to Org text; COMMENTS are its comments.
The edits give the issue the inbox file's label.  The inbox is edited
widened, as a sync edit: when it is read only now, because it had
unsaved edits as the sync began, the sync has not widened it as it does
the buffers it read before applying, and a narrowed inbox would hide its
own heading."
  (if-let* ((buffer (and org-linear-inbox-file
                         (org-linear-in-place--index-file
                          (expand-file-name org-linear-inbox-file) org-linear--scan))))
      (with-current-buffer buffer
        (org-linear--with-sync-edits
         (lambda () (org-linear-in-place--add-to-inbox task converted comments))))
    (org-linear--log "Not added: %s; set `org-linear-inbox-file', or save it"
                     (alist-get 'identifier task))
    nil))

(defun org-linear-in-place--add-to-inbox (task converted comments)
  "Insert TASK under the inbox heading of the current buffer, widened.
Return the edits to send; CONVERTED and COMMENTS are as in
`org-linear-in-place--insert-in-inbox'."
  (let* ((view (org-linear-in-place--without-file-tags
                (org-linear--remote-view task converted nil)))
         (heading (org-linear-in-place--inbox-heading))
         (level (1+ (org-with-point-at heading (org-outline-level))))
         (pos (org-with-point-at heading
                (org-end-of-subtree t t)
                (unless (bolp) (insert "\n"))
                (point)))
         (base (append (copy-sequence view)
                       (list :desc-md (alist-get 'description task)
                             :edited (alist-get 'last_edited_time task)
                             :note-keys nil :comment-ids nil)))
         marker)
    (goto-char pos)
    (insert (org-linear--heading-string view (alist-get 'closed_at task) level))
    ;; Made once the heading is there: inserted text pushes such a
    ;; marker, which would end up at whatever follows the heading.
    (setq marker (org-linear--marker pos))
    (org-linear--set-static-properties marker task)
    (setq base (org-linear--insert-comments marker comments base))
    (puthash (alist-get 'id task) base (org-linear--state-tasks))
    (puthash (alist-get 'id task) marker (plist-get org-linear--scan :tasks))
    (puthash (alist-get 'id task) (org-linear--buffer-file)
             (plist-get org-linear--scan :present))
    (org-linear--count :pulled)
    (org-linear--merge task converted nil nil nil marker base)))

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
  "Save the buffers the sync edited and close those it read files in.
The buffers that visit a file stay open.  The scan cache learns the
saved files afresh; of the others, which did not change, it learns only
whether they still hold headings to create."
  (org-linear-in-place--visit-edited)
  (dolist (buffer (plist-get org-linear--scan :buffers))
    (when (buffer-live-p buffer)
      (if (buffer-modified-p buffer)
          (progn
            (org-linear--save buffer)
            (org-linear-in-place--refresh-cache buffer))
        (org-linear-in-place--refresh-new (org-linear--buffer-file buffer)))))
  (org-linear-in-place--close-reading-buffers))

(defun org-linear-in-place--close-reading-buffers ()
  "Kill the buffers the sync read files in, which nothing else can point into.
One that still holds edits of the sync, as when the sync fails while it
applies Linear's changes, first hands them to a buffer visiting its file,
where they stay unsaved, as the edits of such a sync do in the files you
visit.  If it cannot, the sync's record is dropped, as
`org-linear-in-place--lose-edits' explains."
  (let ((scan org-linear--scan)
        (handed nil))
    (unwind-protect
        (progn
          (when org-linear--state
            (org-linear-in-place--visit-edited))
          (setq handed t))
      (unless handed
        (setq org-linear--state nil))
      (dolist (reading (plist-get scan :opened))
        (when (buffer-live-p reading)
          (kill-buffer reading)))
      (plist-put scan :opened nil)
      (plist-put scan :buffers (seq-filter #'buffer-live-p (plist-get scan :buffers))))))

(defun org-linear-in-place--after-failure ()
  "Close the buffers this sync read files in and mark its files to be read again.
A sync that failed may not have sent their edits, and the next sync
would otherwise take the files for unchanged.  The state is saved,
unless the sync dropped it."
  (when org-linear--scan
    (org-linear-in-place--close-reading-buffers))
  (when (and org-linear--state org-linear--scan)
    (let ((cache (org-linear-in-place--cache)))
      (dolist (file (plist-get org-linear--scan :read))
        (when-let* ((entry (gethash file cache)))
          (puthash file (plist-put entry :new t) cache))))
    (org-linear--save-state)))

(defun org-linear-in-place--refresh-cache (buffer)
  "Record BUFFER's file, as saved, in the scan cache.
Only a file holding headings whose creation this sync sends or defers is
marked to be read again by the next sync: the sync just read it, and the
text alone cannot tell a heading waiting for its creation from one left
out for a reason, by a tag, as a habit, or as an initiative while
initiatives are turned off."
  (let* ((file (org-linear--buffer-file buffer))
         (entry (org-linear-in-place--read-file
                 file buffer (file-attribute-modification-time (file-attributes file)))))
    (puthash file entry (org-linear-in-place--cache))
    (org-linear-in-place--refresh-new file)))

(defun org-linear-in-place--refresh-new (file)
  "Record in the scan cache whether FILE holds headings this sync left to create.
Those are headings whose creation it sent or deferred."
  (when-let* ((entry (gethash file (org-linear-in-place--cache))))
    (plist-put entry :new (and (member file (plist-get org-linear--scan :unfinished)) t))))

;;;; The hierarchy: headings and nodes

(defun org-linear-in-place--hierarchy-p ()
  "Return non-nil if project or milestone headings sync, by their options.
Then the nesting of headings syncs too; see `org-linear-project-keywords'
and `org-linear-milestone-keywords'."
  (and (or org-linear-project-keywords org-linear-milestone-keywords) t))

(defconst org-linear-in-place--object-kinds '(initiative project milestone)
  "The kinds of Linear object a heading other than a task stands for.
They are in the order in which a sync creates them.")

(defun org-linear-in-place--property (kind &optional pending)
  "Return the property that holds the ID of the KIND object of a heading.
KIND is `issue', `project', `initiative' or `milestone'.  With PENDING,
return the property that holds the ID its creation was sent with."
  (concat "LINEAR_" (if pending "PENDING_" "")
          (if (eq kind 'issue) "UUID" (upcase (symbol-name kind)))))

(defun org-linear-in-place--heading-objects ()
  "Return the projects, initiatives and milestones the heading at point holds.
Each is (KIND ID PENDING): PENDING is non-nil when ID is that of a
creation not yet confirmed."
  (let ((properties (org-entry-properties nil 'standard))
        objects)
    (dolist (kind '(project initiative milestone))
      (let ((id (cdr (assoc-string (org-linear-in-place--property kind) properties t)))
            (pending (cdr (assoc-string (org-linear-in-place--property kind t) properties t))))
        (cond (id (push (list kind id nil) objects))
              (pending (push (list kind pending t) objects)))))
    (nreverse objects)))

(defun org-linear-in-place--heading-kind (keyword uuid objects)
  "Return what the heading at point is: `task', `project', `milestone' or nil.
KEYWORD is its TODO keyword, UUID its issue's ID and OBJECTS what
`org-linear-in-place--heading-objects' returns for it.  Without a
hierarchy, a heading with a keyword or an issue is a task.  With one, a
heading is a project or a milestone by its keyword, or else by the
objects it holds, so that a closed one stays what it was."
  (cond
   ((not (org-linear-in-place--hierarchy-p)) (and (or uuid keyword) 'task))
   ((member keyword org-linear-project-keywords) 'project)
   ((member keyword org-linear-milestone-keywords) 'milestone)
   (uuid 'task)
   ((seq-some (lambda (object) (memq (car object) '(project initiative))) objects) 'project)
   (objects 'milestone)
   ((member keyword org-not-done-keywords) 'task)))

(defun org-linear-in-place--pop-stack (stack)
  "Return STACK without the nodes of headings not above the heading at point."
  (let ((level (org-outline-level)))
    (while (and stack (>= (plist-get (car stack) :level) level))
      (pop stack))
    stack))

(defun org-linear-in-place--node (kind keyword stack scan)
  "Return a node for the heading at point, of KIND, with the TODO KEYWORD.
A node is a plist: :kind; :marker, at the heading; :level; :open, non-nil
if KEYWORD is open; :task and :project, the nearest task and project
nodes above it; :milestone, the nearest milestone node above it with no
project heading in between; and :holds-direct and :holds-projects,
non-nil once a task or milestone, or a project, has it as its nearest
project.  STACK holds the nodes above the heading, nearest first.  The
node of a task is recorded in SCAN by its marker."
  (let ((node (list :kind kind :marker (org-linear--marker) :level (org-outline-level)
                    :open (and (member keyword org-not-done-keywords) t)
                    :task (org-linear-in-place--nearest '(task) stack)
                    :project (org-linear-in-place--nearest '(project) stack)
                    :milestone (let ((node (org-linear-in-place--nearest
                                            '(milestone project) stack)))
                                 (and (eq (plist-get node :kind) 'milestone) node))
                    :holds-direct nil :holds-projects nil)))
    (when-let* ((project (plist-get node :project)))
      (plist-put project (if (eq kind 'project) :holds-projects :holds-direct) t))
    (when (eq kind 'task)
      (puthash (plist-get node :marker) node (plist-get scan :node-of)))
    node))

(defun org-linear-in-place--nearest (kinds stack)
  "Return the first node in STACK whose kind is one of KINDS."
  (seq-find (lambda (node) (memq (plist-get node :kind) kinds)) stack))

(defun org-linear-in-place--index-object (node objects scan)
  "Add the project or milestone heading of NODE, holding OBJECTS, to SCAN.
OBJECTS are as from `org-linear-in-place--heading-objects'; those of a
kind the heading cannot stand for are ignored."
  (pcase-dolist (`(,kind ,id ,pending) objects)
    (when (eq (eq kind 'milestone) (eq (plist-get node :kind) 'milestone))
      (cond
       (pending (puthash id (cons kind node) (plist-get scan :object-pending)))
       ((gethash id (plist-get scan :objects)) (puthash id t (plist-get scan :duplicates)))
       (t (puthash id (cons kind node) (plist-get scan :objects)))))))

(defun org-linear-in-place--needed-kinds (node)
  "Return the kinds of Linear object the heading of NODE needs.
A milestone needs a milestone.  A project heading that holds project
headings needs an initiative, and one that holds tasks or milestones
needs a project.  One that holds nothing needs a project, unless it has
an initiative from when it held projects."
  (if (eq (plist-get node :kind) 'milestone)
      '(milestone)
    (append (when (plist-get node :holds-projects) '(initiative))
            (when (or (plist-get node :holds-direct)
                      (not (or (plist-get node :holds-projects)
                               (org-linear-in-place--object-id node 'initiative))))
              '(project)))))

(defun org-linear-in-place--node-kinds (node)
  "Return the kinds of Linear object the heading of NODE needs or holds.
A project heading keeps an object it no longer needs."
  (seq-uniq (append (org-linear-in-place--needed-kinds node)
                    (seq-filter (lambda (kind)
                                  (org-entry-get (plist-get node :marker)
                                                 (org-linear-in-place--property kind)))
                                '(initiative project)))))

(defun org-linear-in-place--missing-objects (node)
  "Return the kinds of Linear object NODE's heading needs and has no ID for.
Only an open project or milestone heading counts; one with only a
pending ID does too, since its creation may need sending again."
  (when (and (memq (plist-get node :kind) '(project milestone)) (plist-get node :open))
    (seq-remove (lambda (kind)
                  (org-entry-get (plist-get node :marker) (org-linear-in-place--property kind)))
                (org-linear-in-place--needed-kinds node))))

(defun org-linear-in-place--drop-nodes (buffer)
  "Leave out of this sync the nodes and objects of the headings in BUFFER."
  (let ((scan org-linear--scan)
        (in-buffer (lambda (node) (eq (marker-buffer (plist-get node :marker)) buffer))))
    (plist-put scan :nodes (seq-remove in-buffer (plist-get scan :nodes)))
    (plist-put scan :stale (seq-remove (lambda (entry) (funcall in-buffer (cdr entry)))
                                       (plist-get scan :stale)))
    (dolist (key '(:objects :object-pending))
      (let ((table (plist-get scan key))
            gone)
        (maphash (lambda (id entry) (when (funcall in-buffer (cdr entry)) (push id gone))) table)
        (dolist (id gone) (remhash id table))))))

;;;; The hierarchy: relations

(defun org-linear-in-place--relations (node kind)
  "Return the relations Org gives the KIND object of NODE's heading, a plist.
An issue has a :parent issue, a :project and a :milestone; a project an
:initiative, its own if its heading has one, else that of the nearest
project heading above; an initiative a :parent initiative; a milestone a
:project.  A value is an ID, nil for none, or :wait while the heading
it points to has no object yet but will get one."
  (pcase kind
    ('issue (list :parent (org-linear-in-place--target (plist-get node :task) 'issue)
                  :project (org-linear-in-place--target (plist-get node :project) 'project)
                  :milestone (org-linear-in-place--target (plist-get node :milestone) 'milestone)))
    ('project (list :initiative (org-linear-in-place--target
                                 (if (memq 'initiative (org-linear-in-place--node-kinds node))
                                     node
                                   (plist-get node :project))
                                 'initiative)))
    ('initiative (list :parent (org-linear-in-place--target (plist-get node :project) 'initiative)))
    ('milestone (list :project (org-linear-in-place--target (plist-get node :project) 'project)))))

(defun org-linear-in-place--object-id (node kind)
  "Return the confirmed or pending ID of the KIND object of NODE's heading."
  (let ((marker (plist-get node :marker)))
    (or (org-entry-get marker (org-linear-in-place--property kind))
        (org-entry-get marker (org-linear-in-place--property kind t)))))

(defun org-linear-in-place--target (node kind)
  "Return the ID of the KIND object of NODE's heading, for a relation to it.
It is the confirmed or pending ID; nil without NODE or when its closed
heading has none; and :wait when its open heading will get one."
  (when node
    (or (org-linear-in-place--object-id node kind)
        (and (plist-get node :open) :wait))))

(defun org-linear-in-place--ready (relations)
  "Return RELATIONS without those waiting for their objects."
  (cl-loop for (key value) on relations by #'cddr
           unless (eq value :wait) append (list key value)))

(defun org-linear-in-place--new-issue-relations (marker)
  "Return the relations to create the issue of the new heading at MARKER with."
  (when-let* ((node (and (org-linear-in-place--hierarchy-p)
                         (gethash marker (plist-get org-linear--scan :node-of)))))
    (org-linear-in-place--ready (org-linear-in-place--relations node 'issue))))

(defun org-linear-in-place--merge-relations (name local remote base)
  "Return (SEND ORG LINEAR): the relations to send and the new relation bases.
LOCAL are the relations Org gives the object NAME and REMOTE those it has
in Linear, plists; BASE holds those of the last sync, Org's as :org-rel
and Linear's as :lin-rel.  The hierarchy lives in Org, so a relation
changed in Org is sent, while one changed only in Linear is kept, since
moving a heading is Org's to do, and the log says so.  When both
changed, `org-linear-conflict-winner' decides.  A relation not yet known
counts as none in Org and as unchanged in Linear, and one waiting for
its object keeps its bases.  A relation to send keeps its bases until
Linear confirms it."
  (let ((org-base (plist-get base :org-rel))
        (lin-base (plist-get base :lin-rel))
        send org lin)
    (cl-loop
     for (key l) on local by #'cddr
     do (let* ((r (plist-get remote key))
               (bo (plist-get org-base key))
               (bl (if (plist-member lin-base key) (plist-get lin-base key) r))
               (field (substring (symbol-name key) 1)))
          (cond
           ((eq l :wait)
            (setq org (plist-put org key bo) lin (plist-put lin key bl)))
           ((equal l r)
            (setq org (plist-put org key l) lin (plist-put lin key r)))
           ((equal l bo)
            (unless (equal r bl)
              (org-linear--log "Kept Linear's %s of %s; move its heading to change it"
                               field name))
            (setq org (plist-put org key l) lin (plist-put lin key r)))
           ((and (not (equal r bl)) (eq org-linear-conflict-winner 'linear))
            (org-linear-in-place--log-conflict name field l r)
            (setq org (plist-put org key l) lin (plist-put lin key r)))
           (t
            (unless (equal r bl)
              (org-linear-in-place--log-conflict name field r l))
            (setq send (plist-put send key l)
                  org (plist-put org key bo)
                  lin (plist-put lin key bl))))))
    (list send org lin)))

(defun org-linear-in-place--log-conflict (name field lost kept)
  "Log that FIELD of NAME changed on both sides; LOST gave way to KEPT."
  (org-linear--count :conflicts)
  (if (eq org-linear-conflict-winner 'org)
      (org-linear--log "Conflict in %s %s: kept yours, %S; Linear's %S is replaced"
                       name field kept lost)
    (org-linear--log "Conflict in %s %s: kept Linear's %S; your %S is not sent"
                     name field kept lost)))

(defun org-linear-in-place--sent-relations (base relations)
  "Return BASE recording that RELATIONS, a plist, now hold in Org and Linear."
  (let ((org (copy-sequence (plist-get base :org-rel)))
        (lin (copy-sequence (plist-get base :lin-rel))))
    (cl-loop for (key value) on relations by #'cddr
             do (setq org (plist-put org key value)
                      lin (plist-put lin key value)))
    (plist-put (plist-put base :org-rel org) :lin-rel lin)))

(defun org-linear-in-place--known-relations (op)
  "Return OP with only the relations whose objects Linear has by now, or nil.
An object Linear has is one the sync state holds.  A relation to a
heading whose creation failed, or was not sent, waits for a later sync,
and the log says so.  An edit of nothing but relations that keeps none
is nil."
  (let (kept)
    (cl-loop for (key value) on (plist-get op :relations) by #'cddr
             do (if (or (null value) (org-linear--table-of value))
                    (setq kept (plist-put kept key value))
                  (org-linear--log "Not sent yet: the %s of %s, which is not in Linear yet"
                                   (substring (symbol-name key) 1)
                                   (org-linear-in-place--op-name op))))
    (let ((op (plist-put (copy-sequence op) :relations kept))
          (place (plist-get op :marker)))
      (when (and place (not (markerp place)))
        (setq op (plist-put op :marker (plist-put (copy-sequence place) :relations kept))))
      (unless (and (null kept)
                   (memq (plist-get op :type) '(update object-relate))
                   (null (plist-get op :fields)))
        op))))

(defun org-linear-in-place--op-name (op)
  "Return a name for what OP edits, for the log."
  (format "%s"
          (or (plist-get op :title) (plist-get (plist-get op :view) :title) (plist-get op :id))))

(defun org-linear-in-place--issue-relation-ops (task node)
  "Merge the relations of TASK with those its heading's NODE gives it.
Record the new relation bases and return the edit that sends Org's
relations, if any.  A new project goes with the milestone Org gives,
since a milestone belongs to one project."
  (let* ((id (alist-get 'id task))
         (base (gethash id (org-linear--state-tasks)))
         (local (org-linear-in-place--relations node 'issue))
         (remote (list :parent (alist-get 'parent_id task)
                       :project (alist-get 'project_id task)
                       :milestone (alist-get 'milestone_id task)))
         (merged (org-linear-in-place--merge-relations
                  (alist-get 'identifier task) local remote base))
         (send (car merged)))
    (when (and (plist-member send :project) (not (plist-member send :milestone))
               (not (eq (plist-get local :milestone) :wait)))
      (setq send (plist-put send :milestone (plist-get local :milestone))))
    (puthash id (plist-put (plist-put base :org-rel (nth 1 merged)) :lin-rel (nth 2 merged))
             (org-linear--state-tasks))
    (when send
      (org-linear-in-place--checked-ops
       base
       (list (list :type 'update :id id :title (alist-get 'identifier task)
                   :team (alist-get 'team_id task) :fields nil
                   :label-ids (alist-get 'label_ids task) :relations send))))))

(defun org-linear-in-place--checked-ops (base ops)
  "Return OPS without nils and without those Linear rejected, as BASE records."
  (seq-filter (lambda (op)
                (and op
                     (if (member (org-linear--op-signature op) (plist-get base :rejected))
                         (progn
                           (org-linear--log "Not sent again: Linear refused this %s for %s"
                                            (plist-get op :type)
                                            (org-linear-in-place--op-name op))
                           nil)
                       t)))
              ops))

;;;; The hierarchy: fetching objects

(defconst org-linear-in-place--object-fields
  '((project . "id name content targetDate updatedAt trashed url status { id type }
initiativeToProjects { nodes { id initiative { id } } }")
    (initiative . "id name content targetDate updatedAt trashed url status
parentInitiative { id }")
    (milestone . "id name description targetDate updatedAt project { id }"))
  "The fields the sync reads of each kind of Linear object.")

(defconst org-linear-in-place--object-api
  '((project :name "Project" :field project :query "project"
             :create "projectCreate" :update "projectUpdate" :archive "projectDelete")
    (initiative :name "Initiative" :field initiative :query "initiative"
                :create "initiativeCreate" :update "initiativeUpdate"
                :archive "initiativeArchive")
    (milestone :name "Milestone" :field projectMilestone :query "projectMilestone"
               :create "projectMilestoneCreate" :update "projectMilestoneUpdate"
               :archive "projectMilestoneDelete"))
  "How Linear's API names each kind of object and what is done to it.
A project has no archive of its own: `projectDelete' moves it to the
trash, which archives it and keeps it restorable.  A milestone can only
be deleted.")

(defun org-linear-in-place--api (kind key)
  "Return the value of KEY in the API entry of KIND."
  (plist-get (alist-get kind org-linear-in-place--object-api) key))

(defun org-linear-in-place--fetch-objects (callback)
  "Fetch the objects the sync manages or is creating, then call CALLBACK.
CALLBACK gets them as from `org-linear-in-place--object'.  Without a
hierarchy nothing is fetched and CALLBACK gets nil."
  (if (not (org-linear-in-place--hierarchy-p))
      (funcall callback nil)
    (org-linear-in-place--fetch-object-chunks
     (or (seq-partition (org-linear-in-place--object-ids) 50) (list nil)) nil callback)))

(defun org-linear-in-place--object-ids ()
  "Return the IDs of the objects the state or the scan knows."
  (let ((ids (make-hash-table :test #'equal)))
    (dolist (table (list (org-linear--state-objects)
                         (plist-get org-linear--scan :objects)
                         (plist-get org-linear--scan :object-pending)))
      (maphash (lambda (id _) (puthash id t ids)) table))
    (hash-table-keys ids)))

(defun org-linear-in-place--fetch-object-chunks (chunks acc callback)
  "Fetch the objects with the IDs in CHUNKS onto ACC, then call CALLBACK.
CHUNKS is a list of lists of at most 50 IDs.  The project statuses, and
whether the workspace has initiatives turned on, come along into
`org-linear--meta', as :project-statuses and :initiatives."
  (org-linear--graphql
   (format "query OrgLinearObjects($ids: [ID!]) {
  organization { roadmapEnabled }
  projectStatuses { nodes { id name type position } }
  projects(first: 50, filter: { id: { in: $ids } }, includeArchived: true) { nodes { %s } }
  initiatives(first: 50, filter: { id: { in: $ids } }, includeArchived: true) { nodes { %s } }
  projectMilestones(first: 50, filter: { id: { in: $ids } }, includeArchived: true) {
    nodes { %s } } }"
           (alist-get 'project org-linear-in-place--object-fields)
           (alist-get 'initiative org-linear-in-place--object-fields)
           (alist-get 'milestone org-linear-in-place--object-fields))
   `((ids . ,(vconcat (car chunks))))
   (lambda (data)
     (plist-put org-linear--meta :project-statuses
                (alist-get 'nodes (alist-get 'projectStatuses data)))
     (plist-put org-linear--meta :initiatives
                (eq (alist-get 'roadmapEnabled (alist-get 'organization data)) t))
     (let ((acc (append acc
                        (mapcar (lambda (node) (org-linear-in-place--object 'project node))
                                (alist-get 'nodes (alist-get 'projects data)))
                        (mapcar (lambda (node) (org-linear-in-place--object 'initiative node))
                                (alist-get 'nodes (alist-get 'initiatives data)))
                        (mapcar (lambda (node) (org-linear-in-place--object 'milestone node))
                                (alist-get 'nodes (alist-get 'projectMilestones data))))))
       (if (cdr chunks)
           (org-linear-in-place--fetch-object-chunks (cdr chunks) acc callback)
         (funcall callback acc))))))

(defun org-linear-in-place--object (kind node)
  "Return NODE, a Linear object of KIND as GraphQL gives it, as the sync uses it.
That is a plist: :kind, :id, :title, :md (its Markdown description),
:deadline, :edited and :trashed; for a project and an initiative, its
:status, `open', `done' or `canceled'; for a project its :links to
initiatives, (LINK-ID . INITIATIVE-ID); for an initiative its :parent;
for a milestone its :project."
  (append
   (list :kind kind
         :id (alist-get 'id node)
         :title (alist-get 'name node)
         :md (or (alist-get (if (eq kind 'milestone) 'description 'content) node) "")
         :deadline (alist-get 'targetDate node)
         :edited (alist-get 'updatedAt node)
         :trashed (eq (alist-get 'trashed node) t))
   (pcase kind
     ('project
      (list :status (pcase (alist-get 'type (alist-get 'status node))
                      ("completed" 'done) ("canceled" 'canceled) (_ 'open))
            :links (mapcar (lambda (link)
                             (cons (alist-get 'id link)
                                   (alist-get 'id (alist-get 'initiative link))))
                           (alist-get 'nodes (alist-get 'initiativeToProjects node)))))
     ('initiative
      (list :status (pcase (alist-get 'status node)
                      ("Completed" 'done) ("Canceled" 'canceled) (_ 'open))
            :parent (alist-get 'id (alist-get 'parentInitiative node))))
     ('milestone
      (list :project (alist-get 'id (alist-get 'project node)))))))

(defun org-linear-in-place--objects-by-id (objects)
  "Return a hash table from the IDs of OBJECTS to them."
  (let ((table (make-hash-table :test #'equal)))
    (dolist (object objects) (puthash (plist-get object :id) object table))
    table))

(defun org-linear-in-place--object-conversion-items (data)
  "Return (KEY . MARKDOWN) for the descriptions of DATA's objects to convert.
Those are the ones that changed since the last sync, of objects linked to
headings the scan found.  The object of a creation whose answer was lost
is linked later, and is not converted: its heading holds what was sent."
  (let (items)
    (dolist (object (alist-get 'objects data))
      (let* ((id (plist-get object :id))
             (base (gethash id (org-linear--state-objects))))
        (when (and (gethash id (plist-get org-linear--scan :objects))
                   (not (equal (plist-get object :md) (plist-get base :desc-md))))
          (push (cons (cons 'desc id) (plist-get object :md)) items))))
    (nreverse items)))

(defun org-linear-in-place--object-body-pulls (data converted)
  "Return the description edits of DATA's objects that would replace a body.
Each is (ID MARKER BASE MD), as in `org-linear-in-place--body-pulls';
CONVERTED holds the converted descriptions."
  (let (pulls)
    (dolist (object (alist-get 'objects data))
      (let* ((id (plist-get object :id))
             (entry (gethash id (plist-get org-linear--scan :objects)))
             (marker (and entry (plist-get (cdr entry) :marker)))
             (base (gethash id (org-linear--state-objects))))
        (when (and marker base
                   (gethash (cons 'desc id) converted)
                   (not (equal (plist-get object :md) (plist-get base :desc-md)))
                   (equal (org-linear-in-place--body-at marker) (plist-get base :body)))
          (push (list id marker base (plist-get object :md)) pulls))))
    (nreverse pulls)))

;;;; The hierarchy: applying

(defun org-linear-in-place--hierarchy-units (data converted)
  "Return the units and assembly that apply DATA with a hierarchy.
See `org-linear-in-place--apply-units'; CONVERTED is as there.  Headings
that became projects or milestones lose their issues, and objects
deleted in Linear cancel their headings, first.  Then the objects and
issues to create get their IDs, so that relations can name them, and the
edits go out in the order Linear needs: new objects, edits of objects,
new issues, edits of issues, and last what is archived."
  (let ((objects (org-linear-in-place--objects-by-id (alist-get 'objects data)))
        (fetched (make-hash-table :test #'equal))
        stale object-creates issue-creates object-edits issue-edits vanished)
    (cons (append
           (list (lambda ()
                   (setq stale (org-linear-in-place--retire-stale-issues))
                   (org-linear-in-place--cancel-deleted-objects objects)
                   (org-linear-in-place--link-found-objects objects)
                   (setq object-creates (org-linear-in-place--object-creation-ops org-linear--scan))
                   (setq issue-creates (org-linear-in-place--creation-ops
                                        org-linear--scan
                                        (max 0 (- org-linear-create-limit (length object-creates)))))
                   nil)
                 (lambda ()
                   (org-linear-in-place--object-merge-units
                    objects converted (lambda (ops) (push ops object-edits)))))
           (org-linear-in-place--task-units data converted fetched
                                            (lambda (ops) (push ops issue-edits)))
           (list (lambda ()
                   (setq vanished (org-linear-in-place--vanished-ops fetched))
                   nil)))
          (lambda ()
            (append object-creates (apply #'append (nreverse object-edits))
                    issue-creates (apply #'append (nreverse issue-edits))
                    stale vanished)))))

(defun org-linear-in-place--retire-stale-issues ()
  "Unlink the issues of headings that became projects or milestones.
Return the edits that archive those issues.  Such a heading was synced
as a task until its keyword, or the hierarchy options, made it a project
or milestone heading; it gets its object as a new one does.  Each
conversion is logged.  More than `org-linear-archive-limit' at once
convert none: their headings keep their issues, get no object yet, and
the log says why."
  (let ((stale (plist-get org-linear--scan :stale)))
    (cond
     ((null stale) nil)
     ((> (length stale) org-linear-archive-limit)
      (org-linear--log "Not converted: %d headings with issues are now projects or milestones, more than `org-linear-archive-limit'"
                       (length stale))
      (dolist (entry stale) (plist-put (cdr entry) :open nil))
      nil)
     (t
      (mapcar (lambda (entry) (org-linear-in-place--retire-stale-issue (car entry) (cdr entry)))
              stale)))))

(defun org-linear-in-place--retire-stale-issue (id node)
  "Unlink the issue ID from the heading of NODE; return the edit archiving it."
  (let ((kind (plist-get node :kind)))
    (org-with-point-at (plist-get node :marker)
      (org-linear--log "Archiving %s: its heading \"%s\" is now a %s heading, which gets a %s instead"
                       (or (org-entry-get nil "LINEAR_ID") id) (org-get-heading t t t t) kind kind)
      (dolist (property '("LINEAR_UUID" "LINEAR_ID" "LINEAR_URL" "LINEAR_REPEATED"))
        (org-entry-delete nil property)))
    (list :type 'archive :id id)))

(defun org-linear-in-place--cancel-deleted-objects (objects)
  "Cancel the headings of synced objects that were deleted in Linear.
OBJECTS maps the IDs of the fetched objects to them; a synced object that
is missing or in the trash was deleted.  Its heading keeps its text,
loses the object's ID and, unless closed, gets the cancelled keyword, so
that the object is not created again."
  (let (deleted)
    (maphash (lambda (id entry)
               (let ((object (gethash id objects)))
                 (when (and (gethash id (org-linear--state-objects))
                            (or (null object) (plist-get object :trashed)))
                   (push (cons id entry) deleted))))
             (plist-get org-linear--scan :objects))
    (pcase-dolist (`(,id ,kind . ,node) deleted)
      (let ((title (org-with-point-at (plist-get node :marker)
                     (unless (member (org-get-todo-state) org-done-keywords)
                       (org-linear--set-todo (org-linear-in-place--status-keyword 'canceled) nil))
                     (org-entry-delete nil (org-linear-in-place--property kind))
                     (org-get-heading t t t t))))
        (plist-put node :open nil)
        (remhash id (org-linear--state-objects))
        (remhash id (plist-get org-linear--scan :objects))
        (org-linear--log "Cancelled \"%s\": its %s was deleted in Linear" title kind)
        (org-linear--count :pulled)))))

(defun org-linear-in-place--link-found-objects (objects)
  "Link the headings whose objects Linear has, though the answer was lost.
OBJECTS maps the IDs of the fetched objects to them.  Each such heading
gets the object's ID, and the state records it as the heading has it,
with Linear's description, so that it merges from there."
  (maphash (lambda (id entry)
             (when-let* ((object (gethash id objects)))
               (let ((kind (car entry))
                     (node (cdr entry)))
                 (org-with-point-at (plist-get node :marker)
                   (org-entry-put nil (org-linear-in-place--property kind) id)
                   (org-entry-delete nil (org-linear-in-place--property kind t)))
                 (puthash id (append (org-linear-in-place--object-view node kind)
                                     (list :kind kind :desc-md (plist-get object :md)
                                           :edited (plist-get object :edited)))
                          (org-linear--state-objects))
                 (puthash id entry (plist-get org-linear--scan :objects))
                 (org-linear--count :created))))
           (plist-get org-linear--scan :object-pending)))

(defun org-linear-in-place--object-merge-units (objects converted collect)
  "Return a unit for each linked object that merges it with its heading.
OBJECTS maps IDs to fetched objects; each unit calls COLLECT with its
edits.  CONVERTED maps conversion keys to Org text."
  (let (units)
    (maphash (lambda (id entry)
               (push (lambda ()
                       (let ((object (gethash id objects)))
                         (cond
                          ((gethash id (plist-get org-linear--scan :duplicates))
                           (org-linear--log "Not synced: the %s %s has more than one heading; delete the copies"
                                            (car entry) id))
                          ((null object)
                           (org-linear--log "Not synced: the %s %s is not in Linear" (car entry) id))
                          ((not (memq (marker-buffer (plist-get (cdr entry) :marker))
                                      (plist-get org-linear--scan :buffers))))
                          (t
                           (funcall collect (org-linear-in-place--merge-object
                                             object (cdr entry) converted)))))
                       nil)
                     units))
             (plist-get org-linear--scan :objects))
    (nreverse units)))

(defun org-linear-in-place--merge-object (object node converted)
  "Merge the Linear OBJECT with the heading of NODE three-way; return the edits.
As with an issue, a field changed on one side takes that side's value,
and `org-linear-conflict-winner' decides when both changed.  The fields
are the title, the body, the deadline and, but for a milestone, the
status.  CONVERTED maps conversion keys to Org text."
  (let* ((id (plist-get object :id))
         (kind (plist-get object :kind))
         (stored (gethash id (org-linear--state-objects)))
         (held (eq (and org-linear--body-overrides (gethash id org-linear--body-overrides))
                   :hold))
         (remote (org-linear-in-place--object-remote-view object converted stored held))
         (local (org-linear-in-place--object-view node kind))
         (base (or stored (append (copy-sequence local) (list :kind kind))))
         (new-base (copy-sequence base))
         (name (format "%s \"%s\"" kind (plist-get local :title)))
         pushed)
    (dolist (field (org-linear-in-place--object-merge-fields kind))
      (let ((l (plist-get local field))
            (r (plist-get remote field))
            (b (plist-get base field)))
        (cond
         ((equal l r) (setq new-base (plist-put new-base field r)))
         ((equal l b)
          (org-linear-in-place--set-object-field node field r)
          (org-linear--count :pulled)
          (setq new-base (plist-put new-base field r)))
         ((equal r b) (push (cons field l) pushed))
         ((eq org-linear-conflict-winner 'org)
          (org-linear-in-place--log-conflict name (substring (symbol-name field) 1) r l)
          (push (cons field l) pushed))
         (t
          (org-linear-in-place--set-object-field node field r)
          (org-linear-in-place--log-conflict name (substring (symbol-name field) 1) l r)
          (setq new-base (plist-put new-base field r))))))
    (setq new-base (plist-put new-base :kind kind))
    (setq new-base (plist-put new-base :desc-md (if held
                                                    (plist-get base :desc-md)
                                                  (plist-get object :md))))
    (setq new-base (plist-put new-base :edited (plist-get object :edited)))
    (let* ((local-relations (org-linear-in-place--relations node kind))
           (merged (org-linear-in-place--merge-relations
                    name local-relations
                    (org-linear-in-place--remote-relations object local-relations base)
                    new-base)))
      (setq new-base (plist-put (plist-put new-base :org-rel (nth 1 merged))
                                :lin-rel (nth 2 merged)))
      (puthash id new-base (org-linear--state-objects))
      (org-linear-in-place--checked-ops
       new-base
       (list (when pushed
               (list :type 'object-update :kind kind :id id :title (plist-get local :title)
                     :fields (nreverse pushed)))
             (when (car merged)
               (list :type 'object-relate :kind kind :id id :title (plist-get local :title)
                     :relations (car merged) :links (plist-get object :links)
                     :parent (plist-get object :parent))))))))

(defun org-linear-in-place--object-merge-fields (kind)
  "Return the fields the merge compares for an object of KIND."
  (if (eq kind 'milestone)
      '(:title :body :deadline)
    '(:title :body :deadline :status)))

(defun org-linear-in-place--object-view (node kind)
  "Return the KIND object of NODE's heading as Org field values."
  (org-with-point-at (plist-get node :marker)
    (append (list :title (substring-no-properties (org-get-heading t t t t))
                  :body (org-linear--body)
                  :deadline (org-linear--planning-date "DEADLINE"))
            (unless (eq kind 'milestone)
              (list :status (org-linear-in-place--status (org-get-todo-state)))))))

(defun org-linear-in-place--object-remote-view (object converted base held)
  "Return OBJECT from Linear as Org field values.
CONVERTED maps conversion keys to Org text, and BASE supplies the body
when the description is unchanged, or HELD back."
  (let ((id (plist-get object :id)))
    (list :title (plist-get object :title)
          :body (or (and held (or (plist-get base :body) ""))
                    (let ((override (and org-linear--body-overrides
                                         (gethash id org-linear--body-overrides))))
                      (and (stringp override) override))
                    (gethash (cons 'desc id) converted)
                    (plist-get base :body)
                    "")
          :deadline (plist-get object :deadline)
          :status (plist-get object :status))))

(defun org-linear-in-place--remote-relations (object local base)
  "Return the relations OBJECT has in Linear, a plist like LOCAL.
A project can be in several initiatives: its initiative is the one Org
gives it when it is among them, else that of the last sync, as BASE
records it, else any."
  (pcase (plist-get object :kind)
    ('project
     (let ((initiatives (mapcar #'cdr (plist-get object :links)))
           (wanted (plist-get local :initiative))
           (last (plist-get (plist-get base :lin-rel) :initiative)))
       (list :initiative (cond ((member wanted initiatives) wanted)
                               ((member last initiatives) last)
                               (t (car initiatives))))))
    ('initiative (list :parent (plist-get object :parent)))
    ('milestone (list :project (plist-get object :project)))))

(defun org-linear-in-place--status (keyword)
  "Return the status of a project or initiative whose heading has KEYWORD.
That is `open' for an open keyword or none, `canceled' for a done one
sent as a canceled workflow state, and `done' for any other done one."
  (cond
   ((not (member keyword org-done-keywords)) 'open)
   ((or (member (alist-get 'type (org-linear--keyword-state
                                  keyword (alist-get 'id (car (plist-get org-linear--meta :teams)))
                                  t))
                '("canceled" "duplicate"))
        (equal keyword (cdr (assoc "canceled" org-linear-type-keywords))))
    'canceled)
   (t 'done)))

(defun org-linear-in-place--status-keyword (status)
  "Return the keyword that gives a project heading STATUS."
  (pcase status
    ('open (or (car org-linear-project-keywords) (org-linear--org-keyword nil nil)))
    ('done (org-linear--org-keyword (cdr (assoc "completed" org-linear-type-keywords)) t))
    (_ (org-linear--org-keyword (cdr (assoc "canceled" org-linear-type-keywords)) t))))

(defun org-linear-in-place--set-object-field (node field value)
  "Set FIELD of the heading of NODE to VALUE, as Linear has it."
  (org-with-point-at (plist-get node :marker)
    (pcase field
      (:title (org-edit-headline value))
      (:body (org-linear--set-body value))
      (:deadline (org-linear--set-planning 'deadline value))
      (:status (org-linear--set-todo (org-linear-in-place--status-keyword value) nil)))))

;;;; The hierarchy: creating objects

(defun org-linear-in-place--object-creation-ops (scan)
  "Return edits that create the objects that new headings in SCAN stand for.
Initiatives come first, then projects, then milestones, each in the
order of their headings, and at most `org-linear-create-limit' are
created; the rest wait for the next syncs.  Each heading gets the
object's ID as a pending property before anything is sent, as a new
issue's does."
  (let ((wanted (org-linear-in-place--wanted-objects scan))
        (count 0)
        chosen ops)
    (pcase-dolist (`(,kind . ,node) wanted)
      (if (< count org-linear-create-limit)
          (progn
            (cl-incf count)
            (org-linear-in-place--ensure-pending node kind)
            (push (cons kind node) chosen))
        (org-linear-in-place--mark-unfinished (marker-buffer (plist-get node :marker)))))
    (when (> (length wanted) count)
      (org-linear--log "Creating %d of %d new projects, initiatives and milestones; the rest follow in the next syncs"
                       count (length wanted)))
    (pcase-dolist (`(,kind . ,node) (nreverse chosen))
      (when-let* ((op (org-linear-in-place--object-add-op kind node)))
        (if (member (org-linear--op-signature op) (plist-get org-linear--state :rejected-adds))
            (org-linear--log "Not created again: Linear refused the %s \"%s\"; change it to retry"
                             kind (plist-get (plist-get op :view) :title))
          (org-linear-in-place--mark-unfinished (marker-buffer (plist-get node :marker)))
          (push op ops))))
    (nreverse ops)))

(defun org-linear-in-place--wanted-objects (scan)
  "Return (KIND . NODE) for each object a heading in SCAN needs and has not got.
They are in the order `org-linear-in-place--object-kinds' gives.
Initiatives are left out while the workspace has them turned off, and
the log says so; the projects that would be in them are created all the
same, and join them once they exist."
  (let (wanted off)
    (dolist (node (plist-get scan :nodes))
      (dolist (kind (org-linear-in-place--missing-objects node))
        (if (and (eq kind 'initiative) (not (plist-get org-linear--meta :initiatives)))
            (push node off)
          (push (cons kind node) wanted))))
    (when off
      (org-linear--log "Not created: %d initiatives, since initiatives are turned off in this Linear workspace (Settings, Initiatives)"
                       (length off)))
    (sort (nreverse wanted)
          (lambda (a b)
            (< (seq-position org-linear-in-place--object-kinds (car a))
               (seq-position org-linear-in-place--object-kinds (car b)))))))

(defun org-linear-in-place--ensure-pending (node kind)
  "Return the ID the KIND object of NODE's heading is created with.
That is the heading's pending ID, or a new one the heading gets."
  (let ((marker (plist-get node :marker))
        (property (org-linear-in-place--property kind t)))
    (or (org-entry-get marker property)
        (let ((id (org-id-uuid)))
          (org-entry-put marker property id)
          id))))

(defun org-linear-in-place--object-add-op (kind node)
  "Return the edit that creates the KIND object of NODE's heading, or nil.
A milestone needs a project heading above it with a project."
  (let* ((marker (plist-get node :marker))
         (pending (org-entry-get marker (org-linear-in-place--property kind t)))
         (view (org-linear-in-place--object-view node kind))
         (relations (org-linear-in-place--ready (org-linear-in-place--relations node kind))))
    (if (and (eq kind 'milestone) (null (plist-get relations :project)))
        (progn
          (org-linear--log "Not created: the milestone \"%s\" needs a project heading above it with a project"
                           (plist-get view :title))
          nil)
      (list :type 'object-add :kind kind :view view :relations relations :pending pending
            :team (alist-get 'id (car (plist-get org-linear--meta :teams)))
            :place (list :file (org-linear--buffer-file (marker-buffer marker))
                         :pending pending)))))

;;;; The hierarchy: sending edits

(defun org-linear-in-place--run-object (op next)
  "Send OP, an edit of a project, initiative or milestone, then call NEXT."
  (pcase (plist-get op :type)
    ('object-add (org-linear-in-place--run-object-add op next))
    ('object-update (org-linear-in-place--run-object-update op next))
    ('object-relate (org-linear-in-place--run-object-relate op next))
    ('object-archive (org-linear-in-place--run-object-archive op next))))

(defun org-linear-in-place--mutation (kind action)
  "Return the GraphQL mutation that does ACTION, `create' or `update', to KIND."
  (let ((name (org-linear-in-place--api kind :name))
        (type (if (eq kind 'milestone) "ProjectMilestone" (org-linear-in-place--api kind :name)))
        (field (org-linear-in-place--api kind :field))
        (fields (alist-get kind org-linear-in-place--object-fields)))
    (pcase action
      ('create
       (format "mutation OrgLinear%sCreate($input: %sCreateInput!) {
  %s(input: $input) { success %s { %s } } }"
               name type (org-linear-in-place--api kind :create) field fields))
      ('update
       (format "mutation OrgLinear%sUpdate($id: String!, $input: %sUpdateInput!) {
  %s(id: $id, input: $input) { success %s { %s } } }"
               name type (org-linear-in-place--api kind :update) field fields)))))

(defun org-linear-in-place--payload (kind action data)
  "Return the object of KIND that DATA, the answer to ACTION, holds.
ACTION is :create or :update."
  (alist-get (org-linear-in-place--api kind :field)
             (alist-get (intern (org-linear-in-place--api kind action)) data)))

(defun org-linear-in-place--status-input (kind status)
  "Return the input entry that gives an object of KIND the STATUS, or nil.
A project takes the first workspace status of the type that STATUS
stands for, `started' for an open one; an initiative takes Active,
Completed or Canceled."
  (pcase kind
    ('initiative `(status . ,(pcase status ('open "Active") ('done "Completed") (_ "Canceled"))))
    ('project
     (let* ((type (pcase status ('open "started") ('done "completed") (_ "canceled")))
            (statuses (seq-filter (lambda (s) (equal (alist-get 'type s) type))
                                  (plist-get org-linear--meta :project-statuses)))
            (first (car (sort (copy-sequence statuses)
                              (lambda (a b) (< (alist-get 'position a) (alist-get 'position b)))))))
       (if first
           `(statusId . ,(alist-get 'id first))
         (org-linear--log "Not sent: Linear has no project status of type %s" type)
         nil)))))

(defun org-linear-in-place--description-key (kind)
  "Return the input key of the Markdown description of an object of KIND."
  (if (eq kind 'milestone) 'description 'content))

(defun org-linear-in-place--run-object-add (op next)
  "Create the object of OP, link its heading and relate it; then call NEXT.
The object is created with the ID its heading holds as a pending
property, so that a lost answer is found by the next sync."
  (let* ((kind (plist-get op :kind))
         (view (plist-get op :view)))
    (if (and (eq kind 'milestone) (null (plist-get (plist-get op :relations) :project)))
        (progn
          (org-linear--log "Not created yet: the milestone \"%s\" waits for its project"
                           (plist-get view :title))
          (funcall next))
      (org-linear--org-to-md
       (plist-get view :body)
       (lambda (md)
         (org-linear--graphql
          (org-linear-in-place--mutation kind 'create)
          `((input . ,(org-linear-in-place--create-input op md)))
          (lambda (data)
            (let ((object (org-linear-in-place--object
                           kind (org-linear-in-place--payload kind :create data))))
              (org-linear-in-place--record-created op object)
              (org-linear-in-place--relate-created op object next)))
          (org-linear--rejected-handler op next)))))))

(defun org-linear-in-place--create-input (op md)
  "Return the input that creates the object of OP, with the description MD."
  (let ((kind (plist-get op :kind))
        (view (plist-get op :view)))
    (append `((id . ,(plist-get op :pending))
              (name . ,(plist-get view :title))
              (,(org-linear-in-place--description-key kind) . ,md))
            (pcase kind
              ('project `((teamIds . ,(vector (plist-get op :team)))))
              ('milestone `((projectId . ,(plist-get (plist-get op :relations) :project)))))
            (when (plist-get view :deadline)
              `((targetDate . ,(plist-get view :deadline))))
            (when-let* ((status (and (plist-get view :status)
                                     (org-linear-in-place--status-input
                                      kind (plist-get view :status)))))
              (list status)))))

(defun org-linear-in-place--record-created (op object)
  "Record OBJECT, just created for OP, and link its heading to it.
The state keeps the description Linear stored, which Linear rewrites,
and the relations the object was created with.  The heading is linked
by its pending ID, wherever it is now, as a new issue's is."
  (let* ((kind (plist-get op :kind))
         (id (plist-get object :id))
         (place (plist-get op :place))
         (sent (and (eq kind 'milestone) (plist-get op :relations))))
    (puthash id (append (copy-sequence (plist-get op :view))
                        (list :kind kind :desc-md (plist-get object :md)
                              :edited (plist-get object :edited)
                              :org-rel sent :lin-rel sent))
             (org-linear--state-objects))
    (org-linear--save-state)
    (org-linear--count :created)
    (org-linear-in-place--edit-file
     (plist-get place :file)
     (lambda ()
       (if-let* ((pos (org-find-property (org-linear-in-place--property kind t)
                                         (plist-get place :pending))))
           (progn
             (org-entry-put pos (org-linear-in-place--property kind) id)
             (org-entry-delete pos (org-linear-in-place--property kind t)))
         (org-linear--log "Created the %s \"%s\", but its heading is no longer in %s"
                          kind (plist-get object :title) (plist-get place :file)))))))

(defun org-linear-in-place--relate-created (op object next)
  "Put OBJECT, just created for OP, where OP's relations say; then call NEXT.
A project goes into its initiative and an initiative under its parent.
A relation Linear refuses is logged and recorded as sent, so that it is
not sent again until the heading moves."
  (let* ((kind (plist-get op :kind))
         (id (plist-get object :id))
         (key (pcase kind ('project :initiative) ('initiative :parent)))
         (target (and key (plist-get (plist-get op :relations) key)))
         (record (lambda (&optional refusal)
                   (let ((base (gethash id (org-linear--state-objects))))
                     (when refusal
                       (org-linear--count :errors)
                       (org-linear--log "Not related: the %s \"%s\" to its %s: %s"
                                        kind (plist-get object :title)
                                        (substring (symbol-name key) 1) refusal))
                     (puthash id (plist-put
                                  (plist-put base :org-rel
                                             (plist-put (copy-sequence (plist-get base :org-rel))
                                                        key target))
                                  :lin-rel (plist-put (copy-sequence (plist-get base :lin-rel))
                                                      key (unless refusal target)))
                              (org-linear--state-objects))
                     (org-linear--save-state)
                     (funcall next))))
         (refused (lambda (message &rest _) (funcall record message))))
    (pcase (and target kind)
      ('project (org-linear-in-place--relink-project id target nil nil record refused))
      ('initiative (org-linear-in-place--reparent-initiative id target nil record refused))
      (_ (funcall next)))))

(defun org-linear-in-place--run-object-update (op next)
  "Send the field changes of OP to its object, then call NEXT.
As with an issue, the object's last edit is checked first, and the
write is refused if it changed since the sync read it."
  (let* ((kind (plist-get op :kind))
         (id (plist-get op :id))
         (base (gethash id (org-linear--state-objects))))
    (org-linear--graphql
     (format "query OrgLinear%sEdited($id: String!) { %s(id: $id) { updatedAt } }"
             (org-linear-in-place--api kind :name) (org-linear-in-place--api kind :query))
     `((id . ,id))
     (lambda (data)
       (if (not (equal (alist-get 'updatedAt
                                  (alist-get (intern (org-linear-in-place--api kind :query)) data))
                       (plist-get base :edited)))
           (funcall (org-linear--refused-handler next)
                    (format "the %s %s was edited in Linear meanwhile"
                            kind (org-linear-in-place--op-name op)))
         (org-linear-in-place--update-input
          kind (plist-get op :fields)
          (lambda (input)
            (org-linear--graphql
             (org-linear-in-place--mutation kind 'update)
             `((id . ,id) (input . ,input))
             (lambda (data)
               (let ((object (org-linear-in-place--object
                              kind (org-linear-in-place--payload kind :update data))))
                 (dolist (field (plist-get op :fields))
                   (setq base (plist-put base (car field) (cdr field))))
                 (setq base (plist-put base :desc-md (plist-get object :md)))
                 (org-linear-in-place--after-object-write id base (plist-get object :edited))
                 (funcall next)))
             (org-linear--rejected-handler op next))))))
     (org-linear--rejected-handler op next))))

(defun org-linear-in-place--update-input (kind fields callback)
  "Call CALLBACK with the input that sets FIELDS of an object of KIND."
  (let ((input (delq nil (mapcar (lambda (field)
                                   (pcase (car field)
                                     (:title `(name . ,(cdr field)))
                                     (:deadline `(targetDate . ,(or (cdr field) :null)))
                                     (:status (org-linear-in-place--status-input
                                               kind (cdr field)))))
                                 fields)))
        (body (assq :body fields)))
    (if body
        (org-linear--org-to-md
         (cdr body)
         (lambda (md)
           (funcall callback (cons (cons (org-linear-in-place--description-key kind) md)
                                   input))))
      (funcall callback input))))

(defun org-linear-in-place--after-object-write (id base edited)
  "Record BASE for object ID after a write, which left it last EDITED then."
  (puthash id (plist-put base :edited edited) (org-linear--state-objects))
  (org-linear--save-state)
  (org-linear--count :pushed))

(defun org-linear-in-place--run-object-relate (op next)
  "Send the relations of OP, its object's place in the hierarchy; then NEXT."
  (let* ((kind (plist-get op :kind))
         (id (plist-get op :id))
         (relations (plist-get op :relations))
         (base (gethash id (org-linear--state-objects)))
         (done (lambda (&rest _)
                 (puthash id (org-linear-in-place--sent-relations base relations)
                          (org-linear--state-objects))
                 (org-linear--save-state)
                 (org-linear--count :pushed)
                 (funcall next)))
         (refused (org-linear--rejected-handler op next)))
    (pcase kind
      ('milestone
       (if-let* ((project (plist-get relations :project)))
           (org-linear--graphql
            (org-linear-in-place--mutation 'milestone 'update)
            `((id . ,id) (input . ((projectId . ,project))))
            done refused)
         (org-linear--log "Not moved: the milestone \"%s\" needs a project heading above it with a project"
                          (org-linear-in-place--op-name op))
         (funcall next)))
      ('project
       (org-linear-in-place--relink-project
        id (plist-get relations :initiative) (plist-get (plist-get base :org-rel) :initiative)
        (plist-get op :links) done refused))
      ('initiative
       (org-linear-in-place--reparent-initiative
        id (plist-get relations :parent) (plist-get op :parent) done refused)))))

(defun org-linear-in-place--relink-project (id new old links then refused)
  "Put the project ID in the initiative NEW instead of OLD, then call THEN.
LINKS are the project's links to initiatives, (LINK-ID . INITIATIVE-ID).
Only the link to OLD, which the sync made, is removed, since a project
can be in several initiatives.  REFUSED handles an error."
  (let ((stale (and old (not (equal old new)) (car (rassoc old links))))
        (link (lambda (&rest _)
                (if (or (null new) (rassoc new links))
                    (funcall then)
                  (org-linear--graphql
                   "mutation OrgLinearProjectLink($input: InitiativeToProjectCreateInput!) {
  initiativeToProjectCreate(input: $input) { success } }"
                   `((input . ((projectId . ,id) (initiativeId . ,new))))
                   (lambda (_) (funcall then))
                   refused)))))
    (if (not stale)
        (funcall link)
      (org-linear--graphql
       "mutation OrgLinearProjectUnlink($id: String!) {
  initiativeToProjectDelete(id: $id) { success } }"
       `((id . ,stale))
       link refused))))

(defun org-linear-in-place--reparent-initiative (id new current then refused)
  "Make NEW the parent of the initiative ID instead of CURRENT; then call THEN.
Either can be nil, for none.  REFUSED handles an error."
  (let ((link (lambda (&rest _)
                (if (or (null new) (equal new current))
                    (funcall then)
                  (org-linear--graphql
                   "mutation OrgLinearInitiativeParent($input: InitiativeRelationCreateInput!) {
  initiativeRelationCreate(input: $input) { success } }"
                   `((input . ((initiativeId . ,new) (relatedInitiativeId . ,id))))
                   (lambda (_) (funcall then))
                   refused)))))
    (if (or (null current) (equal current new))
        (funcall link)
      (org-linear--graphql
       "query OrgLinearInitiativeRelations {
  initiativeRelations(first: 250) { nodes { id initiative { id } relatedInitiative { id } } } }"
       nil
       (lambda (data)
         (if-let* ((relation (seq-find
                              (lambda (r)
                                (and (equal (alist-get 'id (alist-get 'relatedInitiative r)) id)
                                     (equal (alist-get 'id (alist-get 'initiative r)) current)))
                              (alist-get 'nodes (alist-get 'initiativeRelations data)))))
             (org-linear--graphql
              "mutation OrgLinearInitiativeUnparent($id: String!) {
  initiativeRelationDelete(id: $id) { success } }"
              `((id . ,(alist-get 'id relation)))
              link refused)
           (funcall link)))
       refused))))

(defun org-linear-in-place--run-object-archive (op next)
  "Archive the object of OP in Linear, or delete it if a milestone; then NEXT."
  (let ((kind (plist-get op :kind))
        (id (plist-get op :id)))
    (org-linear--graphql
     (format "mutation OrgLinear%sArchive($id: String!) { %s(id: $id) { success } }"
             (org-linear-in-place--api kind :name) (org-linear-in-place--api kind :archive))
     `((id . ,id))
     (lambda (_)
       (remhash id (org-linear--state-objects))
       (org-linear--save-state)
       (org-linear--log "%s the %s %s in Linear: its heading is no longer in Org"
                        (if (eq kind 'milestone) "Deleted" "Archived") kind id)
       (org-linear--count :archived)
       (funcall next))
     (org-linear-in-place--archive-refused op next))))

;;;; Preview

;;;###autoload
(defun org-linear-preview (&optional profile)
  "Show what the next in-place sync of PROFILE would create and skip.
Nothing is sent to Linear, no file is changed and none is visited: the
scan reads the files nobody visits in buffers of its own, which are
killed again.  Interactively, PROFILE is the current buffer's,
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
              (gethash (org-linear--buffer-file (marker-buffer marker)) by-file)))
      (dolist (node (plist-get scan :nodes))
        (when-let* ((kinds (org-linear-in-place--missing-objects node)))
          (push (format "%s (%s)"
                        (org-with-point-at (plist-get node :marker) (org-get-heading t t t t))
                        (mapconcat #'symbol-name kinds " and "))
                (gethash (org-linear--buffer-file (marker-buffer (plist-get node :marker)))
                         by-file))))
      (with-current-buffer (get-buffer-create "*org-linear preview*")
        (let ((inhibit-read-only t))
          (erase-buffer)
          (insert (format "Linked headings in scope: %d\nNew headings to create: %d (at most %d per sync)\n"
                          (hash-table-count (plist-get scan :tasks))
                          (length (plist-get scan :new)) org-linear-create-limit))
          (when (org-linear-in-place--hierarchy-p)
            (insert (format "New projects, initiatives and milestones to create: %d\n"
                            (apply #'+ (mapcar (lambda (node)
                                                 (length (org-linear-in-place--missing-objects node)))
                                               (plist-get scan :nodes))))))
          (insert "\n")
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
          (when (buffer-live-p buffer)
            (kill-buffer buffer)))
        (pop-to-buffer (current-buffer))))))

(provide 'org-linear-in-place)
;;; org-linear-in-place.el ends here
