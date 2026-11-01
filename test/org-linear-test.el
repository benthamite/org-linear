;;; org-linear-test.el --- Tests for org-linear -*- lexical-binding: t -*-

;; Tests for the Linear-Org sync.  `org-linear--graphql' is replaced by an
;; in-memory fake of Linear's API, so no test touches a real workspace;
;; pandoc runs for real.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-linear)

;;;; The fake Linear

(defvar org-linear-test--issues nil
  "Hash table from issue ID to the fake issue, in GraphQL shape.")

(defvar org-linear-test--calls nil
  "Operations the sync sent, newest first: (NAME . VARIABLES).")

(defvar org-linear-test--clock 0
  "Counter for fake edit times.")

(defvar org-linear-test--refuse-next nil
  "Non-nil makes the next edit check report a newer edit.")

(defconst org-linear-test--states
  '(((id . "s-triage") (name . "Triage") (type . "triage") (position . 0))
    ((id . "s-backlog") (name . "Backlog") (type . "backlog") (position . 0))
    ((id . "s-todo") (name . "Todo") (type . "unstarted") (position . 0))
    ((id . "s-doing") (name . "In Progress") (type . "started") (position . 0))
    ((id . "s-waiting") (name . "Waiting") (type . "started") (position . 1))
    ((id . "s-done") (name . "Done") (type . "completed") (position . 0))
    ((id . "s-dup") (name . "Duplicate") (type . "duplicate") (position . 0))
    ((id . "s-canceled") (name . "Canceled") (type . "canceled") (position . 0)))
  "The fake team's workflow states.")

(defconst org-linear-test--users
  '(("u-pablo" . "Pablo Stafforini") ("u-peter" . "Peter Hartree") ("u-ronak" . "Ronak Mehta"))
  "Fake users; Ronak is not a member of the team.")

(defconst org-linear-test--labels
  '(((id . "l-joint") (name . "joint") (team . ((id . "t1"))))
    ((id . "l-req") (name . "requested-by:peter") (team . nil))
    ((id . "l-def") (name . "deferred") (team . nil)))
  "Fake labels.")

(defun org-linear-test--tick ()
  "Return a new fake edit time."
  (format "2026-11-01T10:%02d:%02dZ" (/ (cl-incf org-linear-test--clock) 60)
          (% org-linear-test--clock 60)))

(defun org-linear-test--state (id)
  "Return the fake state with ID."
  (seq-find (lambda (s) (equal (alist-get 'id s) id)) org-linear-test--states))

(defun org-linear-test--user (id)
  "Return the fake user with ID, in GraphQL shape, or nil."
  (when id `((id . ,id) (name . ,(cdr (assoc id org-linear-test--users))))))

(cl-defun org-linear-test--issue (id identifier title &key (state "s-todo") assignee labels
                                     priority due (project "p-a") (description "")
                                     closed-at trashed comments)
  "Return a fake issue ID IDENTIFIER TITLE with the given fields."
  `((id . ,id) (identifier . ,identifier) (url . ,(concat "https://linear.app/x/issue/" identifier))
    (title . ,title) (description . ,description) (priority . ,(or priority 0)) (dueDate . ,due)
    (updatedAt . "2026-10-30T09:00:00Z")
    (completedAt . ,(and closed-at (equal (alist-get 'type (org-linear-test--state state)) "completed")
                         closed-at))
    (canceledAt . ,(and closed-at (not (equal (alist-get 'type (org-linear-test--state state)) "completed"))
                        closed-at))
    (trashed . ,(if trashed t :false))
    (state . ,(org-linear-test--state state)) (team . ((id . "t1")))
    (assignee . ,(org-linear-test--user assignee))
    (project . ,(and project `((id . ,project))))
    (labels . ((nodes . ,(mapcar (lambda (id) (seq-find (lambda (l) (equal (alist-get 'id l) id))
                                                        org-linear-test--labels))
                                 labels))))
    (comments . ((nodes . ,comments)))))

(defun org-linear-test--seed ()
  "Fill the fake board with the issues the tests start from."
  (setq org-linear-test--issues (make-hash-table :test #'equal)
        org-linear-test--calls nil
        org-linear-test--clock 0
        org-linear-test--refuse-next nil)
  (let ((today (format-time-string "%FT10:00:00Z" nil t)))
    (dolist (issue
             (list
              (org-linear-test--issue "i1" "ENG-1" "Task one" :assignee "u-pablo" :priority 2
                                      :due "2026-11-10"
                                      :description "Hello **world** and [a link](https://example.org)."
                                      :comments '(((id . "c-old") (body . "Any news?")
                                                   (createdAt . "2026-10-30T08:00:00Z")
                                                   (parent . nil)
                                                   (user . ((id . "u-peter") (name . "Peter Hartree"))))))
              (org-linear-test--issue "i2" "ENG-2" "Joint task" :state "s-waiting" :assignee "u-pablo"
                                      :labels '("l-joint") :description "Joint.")
              (org-linear-test--issue "i3" "ENG-3" "Closed today" :state "s-done" :closed-at today
                                      :project "p-b" :description "Done.")
              (org-linear-test--issue "i4" "ENG-4" "Closed long ago" :state "s-done"
                                      :closed-at "2026-01-01T10:00:00Z" :description "Old.")
              (org-linear-test--issue "i5" "ENG-5" "Trashed" :trashed t :description "Gone.")
              (org-linear-test--issue "i6" "ENG-6" "Peter's task" :state "s-triage" :assignee "u-peter"
                                      :description "For Peter.")))
      (puthash (alist-get 'id issue) issue org-linear-test--issues))))

(defun org-linear-test--edit (id key value)
  "Set KEY of fake issue ID to VALUE, as an edit made in Linear."
  (let ((issue (gethash id org-linear-test--issues)))
    (setf (alist-get key issue) value)
    (setf (alist-get 'updatedAt issue) (org-linear-test--tick))
    (puthash id issue org-linear-test--issues)))

(defun org-linear-test--in-scope-p (issue cutoff)
  "Return non-nil if fake ISSUE matches the scope filter with CUTOFF."
  (and (not (eq (alist-get 'trashed issue) t))
       (or (not (member (alist-get 'type (alist-get 'state issue)) '("completed" "canceled" "duplicate")))
           (string< cutoff (or (alist-get 'completedAt issue) (alist-get 'canceledAt issue) "")))))

(defun org-linear-test--graphql (query variables callback &optional errback _retried)
  "Answer QUERY with VARIABLES from the fake board, then CALLBACK or ERRBACK.
Like Linear, refuse a nil variable, which `json-serialize' sends as {}."
  (org-linear-test--check-serializable variables)
  (let* ((name (and (string-match "\\(?:query\\|mutation\\) \\(OrgLinear[A-Za-z]+\\)" query)
                    (match-string 1 query)))
         (answer (org-linear-test--answer name variables)))
    (push (cons name variables) org-linear-test--calls)
    (run-at-time 0 nil (lambda ()
                         (if (stringp answer)
                             (if errback (funcall errback answer) (org-linear--fail "%s" answer))
                           (funcall callback answer))))))

(defun org-linear-test--check-serializable (value)
  "Signal an error if the alist VALUE holds a nil, at any depth."
  (when (consp value)
    (dolist (pair value)
      (when (consp pair)
        (when (null (cdr pair))
          (error "Variable %s is nil, which JSON would send as {}" (car pair)))
        (org-linear-test--check-serializable (cdr pair))))))

(defun org-linear-test--answer (name variables)
  "Return the data the fake Linear gives for operation NAME with VARIABLES.
A string is an error message."
  (pcase name
    ("OrgLinearMeta"
     (let* ((members (list (org-linear-test--user "u-pablo") (org-linear-test--user "u-peter")))
            (team `((id . "t1") (key . "ENG") (name . "Engineering")
                    (states . ((nodes . ,org-linear-test--states)))
                    (members . ((nodes . ,members)))))
            (in-team '((nodes . (((id . "t1"))))))
            (projects (list `((id . "p-a") (name . "Project A")
                              (url . "https://linear.app/x/project/a") (teams . ,in-team))
                            `((id . "p-b") (name . "Project B")
                              (url . "https://linear.app/x/project/b") (teams . ,in-team)))))
       `((viewer . ((id . "u-pablo") (name . "Pablo Stafforini")))
         (teams . ((nodes . ,(list team))))
         (issueLabels . ((nodes . ,org-linear-test--labels)))
         (projects . ((nodes . ,projects))))))
    ("OrgLinearIssues"
     (let* ((filter (alist-get 'filter variables))
            (ids (append (alist-get 'in (alist-get 'id filter)) nil))
            (cutoff (and (alist-get 'or filter)
                         (alist-get 'gt (alist-get 'completedAt (aref (alist-get 'or filter) 1)))))
            nodes)
       (maphash (lambda (id issue)
                  (when (if ids (member id ids) (org-linear-test--in-scope-p issue cutoff))
                    (push issue nodes)))
                org-linear-test--issues)
       `((issues . ((pageInfo . ((hasNextPage . :false) (endCursor . nil)))
                    (nodes . ,(nreverse nodes)))))))
    ("OrgLinearEdited"
     (let ((issue (gethash (alist-get 'id variables) org-linear-test--issues)))
       `((issue . ((updatedAt . ,(if org-linear-test--refuse-next
                                     (progn (setq org-linear-test--refuse-next nil) "changed")
                                   (alist-get 'updatedAt issue))))))))
    ("OrgLinearUpdate"
     (let ((input (alist-get 'input variables))
           (id (alist-get 'id variables)))
       (if (string-match-p "REJECT" (or (alist-get 'description input) ""))
           "Argument Validation Error"
         (org-linear-test--apply-input id input)
         (let ((issue (gethash id org-linear-test--issues)))
           `((issueUpdate . ((success . t)
                             (issue . ((updatedAt . ,(alist-get 'updatedAt issue))
                                       (description . ,(alist-get 'description issue)))))))))))
    ("OrgLinearComment"
     (let* ((input (alist-get 'input variables))
            (id (alist-get 'issueId input))
            (issue (gethash id org-linear-test--issues))
            (comment `((id . ,(format "c%d" (cl-incf org-linear-test--clock)))
                       (body . ,(alist-get 'body input)) (createdAt . ,(org-linear-test--tick))
                       (parent . nil) (user . ,(org-linear-test--user "u-pablo")))))
       (setf (alist-get 'comments issue)
             `((nodes . ,(append (alist-get 'nodes (alist-get 'comments issue)) (list comment)))))
       (setf (alist-get 'updatedAt issue) (org-linear-test--tick))
       (puthash id issue org-linear-test--issues)
       `((commentCreate . ((success . t)
                           (comment . ((id . ,(alist-get 'id comment))
                                       (issue . ((updatedAt . ,(alist-get 'updatedAt issue)))))))))))
    ("OrgLinearCreate"
     (let* ((input (alist-get 'input variables))
            (n (1+ (hash-table-count org-linear-test--issues)))
            (id (format "i-new-%d" n))
            (issue (org-linear-test--issue id (format "ENG-%d" (+ 100 n)) (alist-get 'title input)
                                           :project nil)))
       (puthash id issue org-linear-test--issues)
       (org-linear-test--apply-input id input)
       `((issueCreate . ((success . t) (issue . ,(gethash id org-linear-test--issues)))))))
    (_ (format "unknown operation %s" name))))

(defun org-linear-test--apply-input (id input)
  "Apply the issue INPUT to fake issue ID."
  (let ((issue (gethash id org-linear-test--issues)))
    (pcase-dolist (`(,key . ,value) input)
      (let ((value (if (eq value :null) nil value)))
        (pcase key
          ('title (setf (alist-get 'title issue) value))
          ('description (setf (alist-get 'description issue) value))
          ('stateId (setf (alist-get 'state issue) (org-linear-test--state value)))
          ('priority (setf (alist-get 'priority issue) value))
          ('dueDate (setf (alist-get 'dueDate issue) value))
          ('snoozedUntilAt (setf (alist-get 'snoozedUntilAt issue) value))
          ('assigneeId (setf (alist-get 'assignee issue) (org-linear-test--user value)))
          ('projectId (setf (alist-get 'project issue) (and value `((id . ,value)))))
          ('labelIds (setf (alist-get 'labels issue)
                           `((nodes . ,(mapcar (lambda (lid) (seq-find (lambda (l) (equal (alist-get 'id l) lid))
                                                                       org-linear-test--labels))
                                               value))))))))
    (setf (alist-get 'updatedAt issue) (org-linear-test--tick))
    (puthash id issue org-linear-test--issues)))

;;;; Harness

(defvar org-linear-test--dir nil
  "Temporary directory of the running test.")

(defmacro org-linear-test--with-board (&rest body)
  "Run BODY with a fresh fake board, mirrored file and sync state."
  (declare (indent 0))
  `(let* ((org-linear-test--dir (make-temp-file "org-linear-test" t))
          (org-linear-file (expand-file-name "linear.org" org-linear-test--dir))
          (org-linear-state-file (expand-file-name "state.eld" org-linear-test--dir))
          (org-linear-teams '("ENG"))
          (org-linear-state-keywords '(("Waiting" . "WAITING")))
          (org-linear-my-issues-sync nil)
          (org-linear-file-header
           "#+TITLE: Linear\n#+PRIORITIES: A D D\n#+TODO: TRIAGE(r) BACKLOG(b) TODO(t) DOING(g) WAITING(w@) | DONE(d) CANCELLED(c@)\n")
          (org-log-into-drawer "STATES")
          (org-log-done 'time)
          (org-linear--state nil)
          (org-linear--busy nil)
          (org-linear--pending nil)
          (org-linear--rerun nil)
          (org-linear--reruns 2))
     (org-linear-test--seed)
     (cl-letf (((symbol-function 'org-linear--graphql) #'org-linear-test--graphql))
       (unwind-protect
           (progn ,@body)
         (when-let* ((buffer (find-buffer-visiting org-linear-file)))
           (with-current-buffer buffer (set-buffer-modified-p nil))
           (kill-buffer buffer))
         (when (get-buffer org-linear--log-buffer)
           (kill-buffer org-linear--log-buffer))
         (delete-directory org-linear-test--dir t)))))

(defun org-linear-test--sync ()
  "Run a sync and wait until it ends."
  (setq org-linear-test--calls nil)
  (org-linear-sync)
  (let ((deadline (+ (float-time) 60)))
    (while (and org-linear--busy (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (should-not org-linear--busy))

(defun org-linear-test--calls (&optional name)
  "Return the operations of the last sync, oldest first, only NAME if given."
  (seq-filter (lambda (call) (or (null name) (equal (car call) name)))
              (reverse org-linear-test--calls)))

(defun org-linear-test--writes ()
  "Return the names of the write operations of the last sync."
  (seq-filter (lambda (name) (member name '("OrgLinearUpdate" "OrgLinearComment" "OrgLinearCreate")))
              (mapcar #'car (org-linear-test--calls))))

(defun org-linear-test--text ()
  "Return the text of the mirrored file's buffer."
  (with-current-buffer (find-buffer-visiting org-linear-file)
    (buffer-substring-no-properties (point-min) (point-max))))

(defun org-linear-test--log ()
  "Return the sync log's text."
  (with-current-buffer (get-buffer-create org-linear--log-buffer) (buffer-string)))

(defun org-linear-test--goto (identifier)
  "Move point in the mirrored buffer to the heading of IDENTIFIER."
  (set-buffer (find-buffer-visiting org-linear-file))
  (goto-char (point-min))
  (re-search-forward (format ":LINEAR_ID: +%s$" identifier))
  (org-back-to-heading t))

(defmacro org-linear-test--editing (&rest body)
  "Run BODY as an edit by the user that saves nothing by itself."
  (declare (indent 0))
  `(let ((org-linear--saving t)) ,@body))

(defun org-linear-test--input (id)
  "Return the input of the last update sent for issue ID."
  (alist-get 'input (cdr (seq-find (lambda (call) (equal (alist-get 'id (cdr call)) id))
                                   (reverse (org-linear-test--calls "OrgLinearUpdate"))))))

;;;; Mappings

(ert-deftest org-linear-test-cookie-priority ()
  "Org cookies map to Linear priorities, nearest above for unlisted ones."
  (let ((org-linear-priorities '((1 . "1") (2 . "2") (3 . "4") (4 . "6"))))
    (should (= (org-linear--cookie-priority "1") 1))
    (should (= (org-linear--cookie-priority "3") 2))
    (should (= (org-linear--cookie-priority "8") 4))
    (should (= (org-linear--cookie-priority nil) 0))))

(ert-deftest org-linear-test-label-tags ()
  "Label names become valid Org tags."
  (should (equal (org-linear--sanitize-tag "requested-by:peter") "requested_by_peter"))
  (should (equal (org-linear--user-tag "Peter Hartree") "peter"))
  (let ((org-linear-user-tags '(("Peter Hartree" . "ph"))))
    (should (equal (org-linear--user-tag "Peter Hartree") "ph"))))

(ert-deftest org-linear-test-dedent ()
  "Dedenting a note keeps the relative indentation of its lines."
  (should (equal (org-linear--dedent "  First\n  - item\n    more\n") "First\n- item\n  more")))

;;;; Pull

(ert-deftest org-linear-test-first-sync-builds-file ()
  "A first sync writes every issue in scope, mapped to Org, and writes nothing."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (let ((text (org-linear-test--text)))
      (should (string-match-p "^\\* Project A$" text))
      (should (string-match-p "^\\*\\* TODO \\[#B\\] Task one +:pablo:$" text))
      (should (string-match-p "DEADLINE: <2026-11-10 [A-Za-z]+>" text))
      (should (string-match-p "^Hello \\*world\\* and \\[\\[https://example.org\\]\\[a link\\]\\]\\.$" text))
      (should (string-match-p "Note taken on \\[2026-10-30 [^]]+\\] (Peter Hartree) \\\\\\\\\n  Any news\\?" text))
      (should (string-match-p "^\\*\\* WAITING Joint task +:joint:pablo:$" text))
      (should (string-match-p "^\\*\\* DONE Closed today" text))
      (should (string-match-p "CLOSED: \\[" text))
      (should (string-match-p "^\\*\\* TRIAGE Peter's task +:peter:$" text))
      (should-not (string-match-p "Closed long ago" text))
      (should-not (string-match-p "Trashed" text)))
    (should-not (org-linear-test--writes))))

(ert-deftest org-linear-test-second-sync-is-quiet ()
  "A sync with nothing changed on either side changes nothing."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (let ((before (org-linear-test--text)))
      (org-linear-test--sync)
      (should (equal (org-linear-test--text) before))
      (should-not (org-linear-test--writes)))))

(ert-deftest org-linear-test-remote-change-applied ()
  "A change made in Linear reaches the heading when it was not edited here."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--edit "i1" 'title "Task one, renamed")
    (org-linear-test--edit "i1" 'priority 1)
    (org-linear-test--edit "i1" 'description "New *text*.")
    (org-linear-test--sync)
    (let ((text (org-linear-test--text)))
      (should (string-match-p "^\\*\\* TODO \\[#A\\] Task one, renamed +:pablo:$" text))
      (should (string-match-p "^New /text/\\.$" text)))
    (should-not (org-linear-test--writes))))

(ert-deftest org-linear-test-remote-project-move ()
  "An issue moved to another project in Linear moves under that heading."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--edit "i1" 'project '((id . "p-b")))
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-1")
    (should (equal (org-linear--heading-project) "p-b"))))

(ert-deftest org-linear-test-closed-issue-leaves ()
  "An issue closed long ago in Linear leaves the file."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--edit "i2" 'state (org-linear-test--state "s-done"))
    (org-linear-test--edit "i2" 'completedAt "2026-01-02T10:00:00Z")
    (org-linear-test--sync)
    (should-not (string-match-p "Joint task" (org-linear-test--text)))))

;;;; Push

(ert-deftest org-linear-test-local-edits-sent ()
  "Edits to keyword, deadline, body and a new note are sent, once."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-1")
    (org-linear-test--editing
      (org-todo "DOING")
      (org-deadline nil "2026-11-12")
      (org-linear--set-body "A rewritten *body*.")
      (org-linear-test--goto "ENG-1")
      (goto-char (org-log-beginning t))
      (insert "- Note taken on [2026-11-01 Sun 10:00] \\\\\n  Started on it.\n"))
    (org-linear-test--sync)
    (should (equal (org-linear-test--writes) '("OrgLinearUpdate" "OrgLinearComment")))
    (let ((input (org-linear-test--input "i1")))
      (should (equal (alist-get 'stateId input) "s-doing"))
      (should (equal (alist-get 'dueDate input) "2026-11-12"))
      (should (equal (alist-get 'description input) "A rewritten **body**.")))
    (let ((issue (gethash "i1" org-linear-test--issues)))
      (should (equal (alist-get 'name (alist-get 'state issue)) "In Progress"))
      (should (equal (alist-get 'body (car (last (alist-get 'nodes (alist-get 'comments issue)))))
                     "Started on it.")))
    (org-linear-test--sync)
    (should-not (org-linear-test--writes))
    (should (= 1 (with-current-buffer (find-buffer-visiting org-linear-file)
                   (how-many "Started on it" (point-min) (point-max)))))))

(ert-deftest org-linear-test-tags-send-assignee-and-labels ()
  "Tags set the assignee and labels; other tags stay local."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-1")
    (org-linear-test--editing (org-set-tags '("peter" "joint" "mine")))
    (org-linear-test--sync)
    (let ((input (org-linear-test--input "i1")))
      (should (equal (alist-get 'assigneeId input) "u-peter"))
      (should (equal (alist-get 'labelIds input) ["l-joint"])))
    (should (string-match-p ":peter:joint:mine:" (org-linear-test--text)))))

(ert-deftest org-linear-test-two-people-tags-keep-assignee ()
  "Two people tags send no assignee, since an issue has one."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-1")
    (org-linear-test--editing (org-set-tags '("pablo" "peter")))
    (org-linear-test--sync)
    (should-not (assq 'assigneeId (org-linear-test--input "i1")))
    (should (string-match-p "one assignee" (org-linear-test--log)))))

(ert-deftest org-linear-test-conflict-keeps-linear ()
  "When both sides change the title, Linear's wins and the clash is logged."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-1")
    (org-linear-test--editing (org-edit-headline "Local title"))
    (org-linear-test--edit "i1" 'title "Remote title")
    (org-linear-test--sync)
    (should (string-match-p "Remote title" (org-linear-test--text)))
    (should-not (org-linear-test--writes))
    (should (string-match-p "Conflict in ENG-1 title" (org-linear-test--log)))))

(ert-deftest org-linear-test-refused-write-retried ()
  "A write refused because the issue changed is logged; the next sync sends it."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-1")
    (org-linear-test--editing (org-todo "DOING"))
    (setq org-linear-test--refuse-next t)
    (org-linear-test--sync)
    (should-not (org-linear-test--writes))
    (should (string-match-p "Changed in Linear meanwhile" (org-linear-test--log)))
    (org-linear-test--sync)
    (should (equal (alist-get 'name (alist-get 'state (gethash "i1" org-linear-test--issues)))
                   "In Progress"))))

(ert-deftest org-linear-test-new-heading-creates-issue ()
  "A new issue heading under a project becomes a Linear issue, once."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (goto-char (point-min))
      (re-search-forward "^\\* Project A$")
      (org-end-of-subtree t t)
      (insert "** TODO [#C] Write the thing :pablo:\nWhy and *how*.\n"))
    (org-linear-test--sync)
    (let ((input (alist-get 'input (cdar (org-linear-test--calls "OrgLinearCreate")))))
      (should (equal (alist-get 'title input) "Write the thing"))
      (should (equal (alist-get 'teamId input) "t1"))
      (should (equal (alist-get 'projectId input) "p-a"))
      (should (equal (alist-get 'assigneeId input) "u-pablo"))
      (should (equal (alist-get 'priority input) 3))
      (should (equal (alist-get 'stateId input) "s-todo"))
      (should (equal (alist-get 'description input) "Why and **how**.")))
    (should (string-match-p ":LINEAR_ID: +ENG-107" (org-linear-test--text)))
    (org-linear-test--sync)
    (should-not (org-linear-test--writes))))

;;;; Agenda

(ert-deftest org-linear-test-agenda-skip ()
  "Only issues assigned to others are skipped."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-6")
    (should (org-linear-agenda-skip))
    (org-linear-test--goto "ENG-2")
    (should-not (org-linear-agenda-skip))))

(ert-deftest org-linear-test-agenda-keeps-tagged-issues ()
  "A tag in `org-linear-agenda-keep-tags' keeps others' issues in the agenda."
  (org-linear-test--with-board
    (org-linear-test--edit "i2" 'assignee (org-linear-test--user "u-peter"))
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-2")
    (should (org-linear-agenda-skip))
    (let ((org-linear-agenda-keep-tags '("joint")))
      (should-not (org-linear-agenda-skip)))))

(ert-deftest org-linear-test-my-issues ()
  "The view groups your open issues by status and leaves out others'."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (unwind-protect
        (progn
          (org-linear-my-issues)
          (let ((text (with-current-buffer org-agenda-buffer-name
                        (buffer-substring-no-properties (point-min) (point-max)))))
            (should (string-match-p "Task one" text))
            (should (string-match-p "Joint task" text))
            (should (string-match-p "Project A +11-10 +\\(?:\\[#B\\] \\)?Task one" text))
            (let ((case-fold-search nil))
              (should-not (string-match-p "WAITING" text)))
            (should (string-match-p "Waiting\n\\(?:.*\n\\)*?.*Joint task" text))
            (should (string-match-p "To do\n\\(?:.*\n\\)*?.*Task one" text))
            (should-not (string-match-p "Peter's task" text))
            (should-not (string-match-p "Closed today" text))
            (should-not (string-match-p "Backlog" text))))
      (when (get-buffer org-agenda-buffer-name)
        (kill-buffer org-agenda-buffer-name)))))

(ert-deftest org-linear-test-my-issues-fits-project-names ()
  "The project column fits the longest name, untruncated, and aligns titles."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--edit "i3" 'state (org-linear-test--state "s-todo"))
    (org-linear-test--edit "i3" 'completedAt nil)
    (org-linear-test--edit "i3" 'title "Long project task")
    (org-linear-test--sync)
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (goto-char (point-min))
      (re-search-forward "^\\* Project B")
      (org-edit-headline "Project B with a rather long name indeed")
      (let ((org-linear--saving t)) (save-buffer)))
    (unwind-protect
        (progn
          (org-linear-my-issues)
          (let* ((text (with-current-buffer org-agenda-buffer-name
                         (buffer-substring-no-properties (point-min) (point-max))))
                 (column (lambda (title)
                           (string-match (format "^.*%s" (regexp-quote title)) text)
                           (- (match-end 0) (match-beginning 0) (length title)))))
            (should (string-match-p "Project B with a rather long name indeed" text))
            (should (= (funcall column "Long project task") (funcall column "Joint task")))))
      (when (get-buffer org-agenda-buffer-name)
        (kill-buffer org-agenda-buffer-name)))))

(ert-deftest org-linear-test-my-issues-status-change ()
  "Changing a status from the list reaches the heading."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (unwind-protect
        (progn
          (org-linear-my-issues)
          (with-current-buffer org-agenda-buffer-name
            (goto-char (point-min))
            (search-forward "Task one")
            (org-agenda-todo "DOING")
            (should (string-match-p "Task one" (buffer-string))))
          (org-linear-test--goto "ENG-1")
          (should (equal (org-get-todo-state) "DOING")))
      (when (get-buffer org-agenda-buffer-name)
        (kill-buffer org-agenda-buffer-name)))))

(ert-deftest org-linear-test-my-issues-fresh-with-sticky-agendas ()
  "With sticky agendas, a second call shows the file's current state."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (let ((org-agenda-sticky t))
      (unwind-protect
          (progn
            (org-linear-my-issues)
            (should (equal (buffer-name) "*Org Agenda(L)*"))
            (org-linear-test--edit "i1" 'title "Task one, renamed")
            (org-linear-test--sync)
            (org-linear-my-issues)
            (should (string-match-p "Task one, renamed"
                                    (with-current-buffer "*Org Agenda(L)*"
                                      (buffer-substring-no-properties (point-min) (point-max))))))
        (when (get-buffer "*Org Agenda(L)*")
          (kill-buffer "*Org Agenda(L)*"))))))

;;;; Menu

(ert-deftest org-linear-test-menu-commands-exist ()
  "Every key of the menu runs an existing command."
  (dolist (entry '(("m" . org-linear-my-issues)
                   ("v" . org-linear-visit) ("o" . org-linear-browse)
                   ("s" . org-linear-sync) ("a" . org-linear-auto-sync-mode)
                   ("l" . org-linear-show-log)))
    (let ((suffix (transient-get-suffix 'org-linear-menu (car entry))))
      (should (memq (cdr entry) (flatten-tree suffix)))
      (should (commandp (cdr entry))))))

(ert-deftest org-linear-test-menu-issue-predicate ()
  "The open-issue key is offered only on an entry with a Linear issue."
  (with-temp-buffer
    (org-mode)
    (insert "* An issue\n:PROPERTIES:\n:LINEAR_URL: https://linear.app/x/issue/ENG-1\n:END:\n* A note\n")
    (goto-char (point-min))
    (should (org-linear--issue-at-point-p))
    (goto-char (point-max))
    (should-not (org-linear--issue-at-point-p)))
  (with-temp-buffer
    (should-not (org-linear--issue-at-point-p))))

(ert-deftest org-linear-test-menu-auto-sync-label ()
  "The auto-sync label shows the mode's state."
  (let ((org-linear-auto-sync-mode t))
    (should (equal (org-linear--auto-sync-description) "Auto-sync (on)")))
  (let ((org-linear-auto-sync-mode nil))
    (should (equal (org-linear--auto-sync-description) "Auto-sync (off)"))))

;;;; Safety

(ert-deftest org-linear-test-active-region-ignored ()
  "A remote change reaches only its own heading, even with an active region."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--edit "i2" 'state (org-linear-test--state "s-doing"))
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (transient-mark-mode 1)
      (let ((org-loop-over-headlines-in-active-region t))
        (goto-char (point-max))
        (push-mark (point-min) t t)
        (org-linear-test--sync)
        (deactivate-mark)))
    (let ((text (org-linear-test--text)))
      (should (string-match-p "^\\*\\* DOING Joint task" text))
      (should (string-match-p "^\\*\\* TODO \\[#B\\] Task one" text)))
    (org-linear-test--sync)
    (should-not (org-linear-test--writes))))

(ert-deftest org-linear-test-foreign-assignee-kept ()
  "An assignee outside the teams is never removed by the file."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--edit "i1" 'assignee (org-linear-test--user "u-ronak"))
    (org-linear-test--sync)
    (org-linear-test--sync)
    (should-not (org-linear-test--writes))
    (should (equal (alist-get 'id (alist-get 'assignee (gethash "i1" org-linear-test--issues)))
                   "u-ronak"))))

(ert-deftest org-linear-test-deleted-heading-restored-whole ()
  "A deleted heading comes back with its description and notes."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-1")
    (org-linear-test--editing
      (delete-region (point) (progn (org-end-of-subtree t t) (point)))
      (save-buffer))
    (org-linear-test--sync)
    (let ((text (org-linear-test--text)))
      (should (string-match-p "^Hello \\*world\\*" text))
      (should (string-match-p "Any news" text)))
    (org-linear-test--sync)
    (should-not (org-linear-test--writes))))

(ert-deftest org-linear-test-unclosed-html-isolated ()
  "One description's unclosed HTML does not swallow another's."
  (org-linear-test--with-board
    (org-linear-test--edit "i1" 'description "Hello\n\n<!-- unclosed")
    (org-linear-test--sync)
    (should (string-match-p "^Joint\\.$" (org-linear-test--text)))))

(ert-deftest org-linear-test-rejected-write-does-not-block ()
  "A write Linear rejects is logged, not retried, and others still go."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-1")
    (org-linear-test--editing (org-linear--set-body "REJECT me"))
    (org-linear-test--goto "ENG-2")
    (org-linear-test--editing (org-edit-headline "Joint task, renamed"))
    (org-linear-test--sync)
    (should (equal (alist-get 'title (gethash "i2" org-linear-test--issues)) "Joint task, renamed"))
    (should (string-match-p "Rejected, not retried" (org-linear-test--log)))
    (org-linear-test--sync)
    (should-not (org-linear-test--writes))))

(ert-deftest org-linear-test-closed-issue-keeps-unsent-edit ()
  "An issue closed long ago in Linear still gets the file's edit."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-2")
    (org-linear-test--editing (org-edit-headline "Joint task, edited here"))
    (org-linear-test--edit "i2" 'state (org-linear-test--state "s-done"))
    (org-linear-test--edit "i2" 'completedAt "2026-01-02T10:00:00Z")
    (org-linear-test--sync)
    (should (equal (alist-get 'title (gethash "i2" org-linear-test--issues)) "Joint task, edited here"))
    (org-linear-test--sync)
    (should-not (string-match-p "Joint task" (org-linear-test--text)))))

(ert-deftest org-linear-test-subheadings-survive-closing ()
  "A closed issue's heading with a subheading of yours stays in the file."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-2")
    (org-linear-test--editing
      (org-end-of-subtree t t)
      (insert "*** My private notes\nKeep these.\n")
      (save-buffer))
    (org-linear-test--edit "i2" 'state (org-linear-test--state "s-done"))
    (org-linear-test--edit "i2" 'completedAt "2026-01-02T10:00:00Z")
    (org-linear-test--sync)
    (should (string-match-p "Keep these" (org-linear-test--text)))))

(ert-deftest org-linear-test-sync-edits-not-undoable ()
  "Edits the sync applies leave nothing for undo to revert."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--edit "i1" 'title "Renamed in Linear")
    (org-linear-test--sync)
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (should-not buffer-undo-list))))

(ert-deftest org-linear-test-callback-error-releases-sync ()
  "An error inside a sync step ends the sync instead of wedging it."
  (org-linear-test--with-board
    (cl-letf (((symbol-function 'org-linear--apply) (lambda (&rest _) (error "Boom"))))
      (org-linear-test--sync))
    (should-not org-linear--busy)
    (should (string-match-p "Boom" (org-linear-test--log)))))

;;;; Talking to Linear

(ert-deftest org-linear-test-key-stays-off-the-command-line ()
  "The API key reaches curl on its standard input, never as an argument."
  (let ((org-linear-api-key "lin_api_SECRET")
        (org-linear--api-key-cache nil)
        command input)
    (cl-letf (((symbol-function 'org-linear--run-process)
               (lambda (cmd in _callback) (setq command cmd input in))))
      (org-linear--graphql "query OrgLinearMeta { viewer { id } }" nil #'ignore))
    (should-not (seq-some (lambda (arg) (string-match-p "SECRET" arg)) command))
    (should (string-match-p "Authorization: lin_api_SECRET" input))))

(ert-deftest org-linear-test-rejected-key-read-again ()
  "When Linear rejects the key, the forget command runs and the call retries once."
  (let ((org-linear-api-key-forget-command '("forget"))
        (org-linear--api-key-cache "old")
        retried forgot)
    (cl-letf (((symbol-function 'org-linear--run-process)
               (lambda (cmd _in callback) (setq forgot cmd) (funcall callback 0 "" "")))
              ((symbol-function 'org-linear--graphql)
               (lambda (&rest args) (setq retried (nth 4 args)))))
      (org-linear--handle-response "q" nil #'ignore #'ignore nil 0 "{\"errors\":[]}\n401" ""))
    (should (equal forgot '("forget")))
    (should (eq retried t))
    (should-not org-linear--api-key-cache)))

(ert-deftest org-linear-test-graphql-errors-reported ()
  "GraphQL errors reach the error handler with their messages."
  (let (reported)
    (org-linear--handle-response
     "q" nil #'ignore (lambda (message) (setq reported message)) nil 0
     "{\"errors\":[{\"message\":\"Entity not found\"}]}\n200" "")
    (should (equal reported "Entity not found"))))

;;;; Start dates

(defun org-linear-test--seed-started ()
  "Add a deferred issue i7, and a triage one i8, to the fake board."
  (puthash "i7" (org-linear-test--issue "i7" "ENG-7" "Deferred task" :assignee "u-pablo"
                                        :labels '("l-def" "l-joint")
                                        :description "Start: 2099-12-05\n\nThe body.")
           org-linear-test--issues)
  (puthash "i8" (org-linear-test--issue "i8" "ENG-8" "Triage task" :state "s-triage"
                                        :assignee "u-pablo" :description "Inbox.")
           org-linear-test--issues))

(defun org-linear-test--issue-labels (id)
  "Return the label IDs of fake issue ID."
  (mapcar (lambda (label) (alist-get 'id label))
          (alist-get 'nodes (alist-get 'labels (gethash id org-linear-test--issues)))))

(defun org-linear-test--goto-title (title)
  "Move to the heading TITLE in the sync buffer."
  (goto-char (point-min))
  (re-search-forward (concat "^\\*\\* .*" (regexp-quote title)))
  (org-back-to-heading t))

(ert-deftest org-linear-test-start-pulled-as-scheduled ()
  "A Start line becomes SCHEDULED; neither it nor the label shows in the heading."
  (org-linear-test--with-board
    (org-linear-test--seed-started)
    (let ((org-linear-start-date-label "deferred"))
      (org-linear-test--sync)
      (with-current-buffer (org-linear--buffer)
        (org-linear-test--goto-title "Deferred task")
        (should (equal (org-linear--planning-date "SCHEDULED") "2099-12-05"))
        (should (equal (org-linear--body) "The body."))
        (should (member "joint" (org-get-tags nil t)))
        (should-not (member "deferred" (org-get-tags nil t))))
      (setq org-linear-test--calls nil)
      (org-linear-test--sync)
      (should-not (assoc "OrgLinearUpdate" org-linear-test--calls)))))

(ert-deftest org-linear-test-start-off-by-default ()
  "With the option nil, the line and tag stay, and SCHEDULED is not sent."
  (org-linear-test--with-board
    (org-linear-test--seed-started)
    (org-linear-test--sync)
    (with-current-buffer (org-linear--buffer)
      (org-linear-test--goto-title "Deferred task")
      (should (member "deferred" (org-get-tags nil t)))
      (should (string-prefix-p "Start: 2099-12-05" (org-linear--body)))
      (org-linear-test--goto-title "Task one")
      (org-add-planning-info 'scheduled (date-to-time "2099-01-01T12:00:00"))
      (save-buffer))
    (setq org-linear-test--calls nil)
    (org-linear-test--sync)
    (should-not (assoc "OrgLinearUpdate" org-linear-test--calls))))

(ert-deftest org-linear-test-start-pushed-from-scheduled ()
  "Setting SCHEDULED writes the line and label; the body is kept."
  (org-linear-test--with-board
    (org-linear-test--seed-started)
    (let ((org-linear-start-date-label "deferred"))
      (org-linear-test--sync)
      (with-current-buffer (org-linear--buffer)
        (org-linear-test--goto-title "Joint task")
        (org-add-planning-info 'scheduled (date-to-time "2099-03-04T12:00:00"))
        (save-buffer))
      (org-linear-test--sync)
      (let ((issue (gethash "i2" org-linear-test--issues)))
        (should (equal (alist-get 'description issue) "Start: 2099-03-04\n\nJoint."))
        (should (equal (sort (org-linear-test--issue-labels "i2") #'string<) '("l-def" "l-joint"))))
      (setq org-linear-test--calls nil)
      (org-linear-test--sync)
      (should-not (assoc "OrgLinearUpdate" org-linear-test--calls)))))

(ert-deftest org-linear-test-start-removed-and-changed ()
  "Moving SCHEDULED rewrites the line; removing it drops line and label only."
  (org-linear-test--with-board
    (org-linear-test--seed-started)
    (let ((org-linear-start-date-label "deferred"))
      (org-linear-test--sync)
      (with-current-buffer (org-linear--buffer)
        (org-linear-test--goto-title "Deferred task")
        (org-add-planning-info 'scheduled (date-to-time "2099-12-20T12:00:00"))
        (save-buffer))
      (org-linear-test--sync)
      (should (equal (alist-get 'description (gethash "i7" org-linear-test--issues))
                     "Start: 2099-12-20\n\nThe body."))
      (with-current-buffer (org-linear--buffer)
        (org-linear-test--goto-title "Deferred task")
        (org-add-planning-info nil nil 'scheduled)
        (save-buffer))
      (org-linear-test--sync)
      (should (equal (alist-get 'description (gethash "i7" org-linear-test--issues)) "The body."))
      (should (equal (org-linear-test--issue-labels "i7") '("l-joint"))))))

(ert-deftest org-linear-test-start-with-body-edit ()
  "Editing the body keeps the Start line on Linear."
  (org-linear-test--with-board
    (org-linear-test--seed-started)
    (let ((org-linear-start-date-label "deferred"))
      (org-linear-test--sync)
      (with-current-buffer (org-linear--buffer)
        (org-linear-test--goto-title "Deferred task")
        (org-linear--set-body "New body.")
        (save-buffer))
      (org-linear-test--sync)
      (should (equal (alist-get 'description (gethash "i7" org-linear-test--issues))
                     "Start: 2099-12-05\n\nNew body."))
      (should (member "l-def" (org-linear-test--issue-labels "i7"))))))

(ert-deftest org-linear-test-start-changed-in-linear ()
  "A Start line added, moved or removed in Linear reaches SCHEDULED."
  (org-linear-test--with-board
    (org-linear-test--seed-started)
    (let ((org-linear-start-date-label "deferred"))
      (org-linear-test--sync)
      (org-linear-test--edit "i7" 'description "The body.")
      (org-linear-test--edit "i2" 'description "Start: 2099-02-02\n\nJoint.")
      (org-linear-test--sync)
      (with-current-buffer (org-linear--buffer)
        (org-linear-test--goto-title "Deferred task")
        (should-not (org-entry-get nil "SCHEDULED"))
        (org-linear-test--goto-title "Joint task")
        (should (equal (org-linear--planning-date "SCHEDULED") "2099-02-02"))
        (should (equal (org-linear--body) "Joint."))))))

(ert-deftest org-linear-test-start-migration ()
  "Turning the option on moves existing lines and tags without writing to Linear."
  (org-linear-test--with-board
    (org-linear-test--seed-started)
    (org-linear-test--sync)
    (let ((org-linear-start-date-label "deferred"))
      (setq org-linear-test--calls nil)
      (org-linear-test--sync)
      (should-not (assoc "OrgLinearUpdate" org-linear-test--calls))
      (with-current-buffer (org-linear--buffer)
        (org-linear-test--goto-title "Deferred task")
        (should (equal (org-linear--planning-date "SCHEDULED") "2099-12-05"))
        (should (equal (org-linear--body) "The body."))
        (should-not (member "deferred" (org-get-tags nil t))))
      (org-linear-test--sync)
      (should-not (assoc "OrgLinearUpdate" org-linear-test--calls)))
    ;; Turning it off restores the line to the body, again without a write.
    (org-linear-test--sync)
    (should-not (assoc "OrgLinearUpdate" org-linear-test--calls))
    (with-current-buffer (org-linear--buffer)
      (org-linear-test--goto-title "Deferred task")
      (should (string-prefix-p "Start: 2099-12-05" (org-linear--body)))
      (should (member "deferred" (org-get-tags nil t))))))

(ert-deftest org-linear-test-start-snoozes-triage ()
  "SCHEDULED on a triage issue also snoozes it; removing it ends the snooze."
  (org-linear-test--with-board
    (org-linear-test--seed-started)
    (let ((org-linear-start-date-label "deferred"))
      (org-linear-test--sync)
      (with-current-buffer (org-linear--buffer)
        (org-linear-test--goto-title "Triage task")
        (org-add-planning-info 'scheduled (date-to-time "2099-04-04T12:00:00"))
        (save-buffer))
      (org-linear-test--sync)
      (should (equal (alist-get 'snoozedUntilAt (gethash "i8" org-linear-test--issues))
                     "2099-04-04T00:00:00.000Z"))
      (with-current-buffer (org-linear--buffer)
        (org-linear-test--goto-title "Triage task")
        (org-add-planning-info nil nil 'scheduled)
        (save-buffer))
      (org-linear-test--sync)
      (should-not (alist-get 'snoozedUntilAt (gethash "i8" org-linear-test--issues))))))

(ert-deftest org-linear-test-start-on-new-heading ()
  "A new heading with SCHEDULED is created with the line and the label."
  (org-linear-test--with-board
    (let ((org-linear-start-date-label "deferred"))
      (org-linear-test--sync)
      (with-current-buffer (org-linear--buffer)
        (org-linear-test--goto-title "Task one")
        (org-end-of-subtree t t)
        (insert "** TODO Fresh task\nSCHEDULED: <2099-05-05 Tue>\nFresh body.\n")
        (save-buffer))
      (org-linear-test--sync)
      (let ((issue (seq-find (lambda (i) (equal (alist-get 'title i) "Fresh task"))
                             (hash-table-values org-linear-test--issues))))
        (should (equal (alist-get 'description issue) "Start: 2099-05-05\n\nFresh body."))
        (should (member "l-def" (org-linear-test--issue-labels (alist-get 'id issue)))))
      (setq org-linear-test--calls nil)
      (org-linear-test--sync)
      (should-not (assoc "OrgLinearUpdate" org-linear-test--calls)))))

(ert-deftest org-linear-test-my-issues-hides-future-scheduled ()
  "The list leaves out issues scheduled for a later day, and shows past ones."
  (org-linear-test--with-board
    (org-linear-test--seed-started)
    (puthash "i9" (org-linear-test--issue "i9" "ENG-9" "Started task" :assignee "u-pablo"
                                          :labels '("l-def")
                                          :description "Start: 2020-01-01\n\nOld.")
             org-linear-test--issues)
    (let ((org-linear-start-date-label "deferred"))
      (org-linear-test--sync)
      (unwind-protect
          (progn
            (org-linear-my-issues)
            (with-current-buffer org-agenda-buffer-name
              (should-not (string-match-p "Deferred task" (buffer-string)))
              (should (string-match-p "Started task" (buffer-string)))))
        (when (get-buffer "*Org Agenda(L)*") (kill-buffer "*Org Agenda(L)*"))
        (when (get-buffer org-agenda-buffer-name) (kill-buffer org-agenda-buffer-name))))))

;;;; Feedback and refreshing the list

(defun org-linear-test--wait ()
  "Wait until the sync in progress ends."
  (let ((deadline (+ (float-time) 60)))
    (while (and org-linear--busy (< (float-time) deadline))
      (accept-process-output nil 0.05))))

(defmacro org-linear-test--with-messages (var &rest body)
  "Run BODY with the messages shown so far in the list VAR, newest first."
  (declare (indent 1))
  `(let ((,var nil))
     (cl-letf* ((orig (symbol-function 'message))
                ((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (when fmt (push (apply #'format fmt args) ,var))
                   (apply orig fmt args))))
       ,@body)))

(defun org-linear-test--list-text ()
  "Return the text of the list."
  (with-current-buffer org-linear--list-buffer
    (buffer-substring-no-properties (point-min) (point-max))))

(defmacro org-linear-test--with-list (&rest body)
  "Run BODY with the list open, and close it afterwards."
  (declare (indent 0))
  `(unwind-protect
       (progn (org-linear-my-issues) ,@body)
     (when (buffer-live-p org-linear--list-buffer)
       (kill-buffer org-linear--list-buffer))))

(ert-deftest org-linear-test-manual-sync-reports-no-change ()
  "A sync run by hand says when it starts and ends, even with nothing to do."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--with-messages seen
      (org-linear-sync t)
      (org-linear-test--wait)
      (should (member "Linear sync: started" seen))
      (should (member "Linear sync: done, nothing to change" seen)))))

(ert-deftest org-linear-test-background-sync-stays-quiet ()
  "A background sync with nothing to do shows no message."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--with-messages seen
      (org-linear-test--sync)
      (should-not (seq-find (lambda (m) (string-prefix-p "Linear sync" m)) seen)))))

(ert-deftest org-linear-test-list-syncs-and-refreshes ()
  "Opening the list syncs; new issues appear, and point stays on its issue."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (let ((org-linear-my-issues-sync nil))
      (org-linear-test--with-list
        (with-current-buffer org-linear--list-buffer
          (should org-linear-my-issues-mode)
          (should-not (string-match-p "Brand new" (buffer-string))))
        (puthash "i9" (org-linear-test--issue "i9" "ENG-9" "Brand new" :state "s-triage"
                                              :assignee "u-pablo" :description "Fresh.")
                 org-linear-test--issues)
        (let ((org-linear-my-issues-sync t))
          (org-linear-my-issues)
          (with-selected-window (get-buffer-window org-linear--list-buffer)
            (goto-char (point-min))
            (search-forward "Joint task")
            (beginning-of-line))
          (org-linear-test--wait)
          (with-selected-window (get-buffer-window org-linear--list-buffer)
            (should (string-match-p "Brand new" (buffer-string)))
            (should org-linear-my-issues-mode)
            (should (string-match-p "Joint task"
                                    (buffer-substring (line-beginning-position)
                                                      (line-end-position))))))))))

(ert-deftest org-linear-test-list-sync-skips-unsaved-edits ()
  "Opening the list does not send edits that are still unsaved."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (with-current-buffer (org-linear--buffer)
      (org-linear-test--editing
        (org-linear-test--goto-title "Task one")
        (org-edit-headline "Half edited")))
    (let ((org-linear-my-issues-sync t))
      (setq org-linear-test--calls nil)
      (org-linear-test--with-list
        (should-not org-linear--busy)
        (should-not org-linear--refresh-list)
        (should-not org-linear-test--calls)
        (should (buffer-modified-p (org-linear--buffer)))))))

(ert-deftest org-linear-test-list-todo-moves-on ()
  "Accepting an issue moves it out of Triage and leaves point on the next one."
  (org-linear-test--with-board
    (puthash "i7" (org-linear-test--issue "i7" "ENG-7" "Triage A" :state "s-triage"
                                          :assignee "u-pablo" :description "A.")
             org-linear-test--issues)
    (puthash "i8" (org-linear-test--issue "i8" "ENG-8" "Triage B" :state "s-triage"
                                          :assignee "u-pablo" :description "B.")
             org-linear-test--issues)
    (org-linear-test--sync)
    (org-linear-test--with-list
      (with-current-buffer org-linear--list-buffer
        (goto-char (point-min))
        (search-forward "Triage A")
        (org-linear-my-issues-todo "TODO")
        (should org-linear-my-issues-mode)
        (should (string-match-p "Triage B" (buffer-substring (line-beginning-position)
                                                             (line-end-position))))
        (should (string-match-p "To do\n\\(?:.*\n\\)*?.*Triage A" (buffer-string)))))))

(ert-deftest org-linear-test-list-reject-with-reason ()
  "Rejecting from the list asks for a reason, and the reason is posted."
  (org-linear-test--with-board
    (puthash "i7" (org-linear-test--issue "i7" "ENG-7" "Triage A" :state "s-triage"
                                          :assignee "u-pablo" :description "A.")
             org-linear-test--issues)
    (org-linear-test--sync)
    (org-linear-test--with-list
      (with-current-buffer org-linear--list-buffer
        (goto-char (point-min))
        (search-forward "Triage A")
        (org-linear-my-issues-todo "CANCELLED")
        (should-not (string-match-p "Triage A" (buffer-string))))
      ;; The note buffer opens when the command ends.
      (run-hooks 'post-command-hook)
      (with-current-buffer "*Org Note*"
        (goto-char (point-max))
        (insert "A duplicate.")
        (org-ctrl-c-ctrl-c))
      (with-current-buffer (org-linear--buffer)
        (let ((org-linear--saving t)) (save-buffer)))
      (org-linear-test--sync)
      (let ((issue (gethash "i7" org-linear-test--issues)))
        (should (equal (alist-get 'type (alist-get 'state issue)) "canceled"))
        (should (seq-find (lambda (c) (string-match-p "A duplicate" (alist-get 'body c)))
                          (alist-get 'nodes (alist-get 'comments issue))))))))

(provide 'org-linear-test)
;;; org-linear-test.el ends here
