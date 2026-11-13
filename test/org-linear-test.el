;;; org-linear-test.el --- Tests for org-linear -*- lexical-binding: t -*-

;; Tests for the Linear-Org sync.  `org-linear--graphql' is replaced by an
;; in-memory fake of Linear's API, so no test touches a real workspace;
;; pandoc runs for real.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org-linear)
(require 'org-linear-in-place)

;;;; The fake Linear

(defvar org-linear-test--issues nil
  "Hash table from issue ID to the fake issue, in GraphQL shape.")

(defvar org-linear-test--calls nil
  "Operations the sync sent, newest first: (NAME . VARIABLES).")

(defvar org-linear-test--clock 0
  "Counter for fake edit times.")

(defvar org-linear-test--refuse-next nil
  "Non-nil makes the next edit check report a newer edit.")

(defvar org-linear-test--rewrite-descriptions nil
  "Non-nil makes the fake Linear rewrite descriptions as the real one does.
Linear stores a description it is sent in its own Markdown style: a bare
URL becomes [URL](<URL>), a \"- \" bullet a \"* \" one, and trailing
spaces go (observed on 2026-11-13).")

(defun org-linear-test--linear-markdown (md)
  "Return MD as the real Linear stores it, as far as the fake knows."
  (let ((md (replace-regexp-in-string "[ \t]+$" "" md)))
    (setq md (replace-regexp-in-string "^\\([ \t]*\\)- " "\\1* " md))
    (setq md (replace-regexp-in-string "\\[\\([^]]+\\)\\](\\(https?://[^)>]+\\))" "[\\1](<\\2>)" md))
    (replace-regexp-in-string "\\(?:^\\|[ (]\\)\\(https?://[^ \t\n)<>]+\\)"
                              (lambda (m) (replace-regexp-in-string
                                           "https?://.*" (lambda (u) (format "[%s](<%s>)" u u)) m t t))
                              md t t)))

(defvar org-linear-test--lose-next nil
  "Name of an operation whose next answer is lost on the way back.
The fake Linear still carries the operation out.")

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
    (updatedAt . ,(copy-sequence "2026-10-30T09:00:00Z"))
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
        org-linear-test--refuse-next nil
        org-linear-test--lose-next nil)
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
    (when (equal name org-linear-test--lose-next)
      (setq org-linear-test--lose-next nil
            answer 'lost))
    (run-at-time 0 nil (lambda ()
                         (cond
                          ;; As `org-linear--handle-response' treats a timeout.
                          ((eq answer 'lost)
                           (org-linear--fail "curl failed (28): Operation timed out"))
                          ((stringp answer)
                           (if errback (funcall errback answer) (org-linear--fail "%s" answer)))
                          (t (funcall callback answer)))))))

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
            (since (alist-get 'gt (alist-get 'updatedAt filter)))
            nodes)
       (maphash (lambda (id issue)
                  (when (cond (ids (member id ids))
                              (since (string< since (alist-get 'updatedAt issue)))
                              (t (org-linear-test--in-scope-p issue cutoff)))
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
            (id (or (alist-get 'id input) (format "i-new-%d" n)))
            (issue (org-linear-test--issue id (format "ENG-%d" (+ 100 n)) (alist-get 'title input)
                                           :project nil)))
       (puthash id issue org-linear-test--issues)
       (org-linear-test--apply-input id input)
       `((issueCreate . ((success . t) (issue . ,(gethash id org-linear-test--issues)))))))
    ("OrgLinearArchive"
     (let ((issue (gethash (alist-get 'id variables) org-linear-test--issues)))
       (setf (alist-get 'archived issue) t)
       (puthash (alist-get 'id variables) issue org-linear-test--issues)
       '((issueArchive . ((success . t))))))
    ("OrgLinearLabel"
     (let ((label `((id . ,(format "l-new-%d" (cl-incf org-linear-test--clock)))
                    (name . ,(alist-get 'name (alist-get 'input variables)))
                    (team . nil))))
       (setq org-linear-test--labels (append org-linear-test--labels (list label)))
       `((issueLabelCreate . ((success . t) (issueLabel . ,label))))))
    (_ (format "unknown operation %s" name))))

(defun org-linear-test--apply-input (id input)
  "Apply the issue INPUT to fake issue ID."
  (let ((issue (gethash id org-linear-test--issues)))
    (pcase-dolist (`(,key . ,value) input)
      (let ((value (if (eq value :null) nil value)))
        (pcase key
          ('title (setf (alist-get 'title issue) value))
          ('description (setf (alist-get 'description issue)
                              (if (and value org-linear-test--rewrite-descriptions)
                                  (org-linear-test--linear-markdown value)
                                value)))
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
         (dolist (file (list org-linear-file (org-linear--archive-file-name)))
           (when-let* ((buffer (find-buffer-visiting file)))
             (with-current-buffer buffer (set-buffer-modified-p nil))
             (kill-buffer buffer)))
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

(defun org-linear-test--archive-text ()
  "Return the text of the archive file on disk, or nil if there is none."
  (let ((file (org-linear--archive-file-name)))
    (when (file-exists-p file)
      (with-temp-buffer
        (insert-file-contents file)
        (buffer-string)))))

(defun org-linear-test--close-long-ago (id)
  "Close fake issue ID in Linear, long enough ago to leave the file."
  (org-linear-test--edit id 'state (org-linear-test--state "s-done"))
  (org-linear-test--edit id 'completedAt "2026-01-02T10:00:00Z"))

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

;;;; Profiles

(defmacro org-linear-test--with-profiles (&rest body)
  "Run BODY with profiles `work' and `home' mirroring the fake board.
Each mirrors it into its own file, and the default profile has no teams."
  (declare (indent 0))
  `(org-linear-test--with-board
     (let ((org-linear-profiles
            `((work :file ,(expand-file-name "work.org" org-linear-test--dir))
              (home :file ,(expand-file-name "home.org" org-linear-test--dir))))
           (org-linear--profile-states (make-hash-table :test #'eq)))
       (cl-letf (((symbol-function 'org-linear--graphql)
                  (lambda (query variables callback &optional errback retried)
                    (org-linear-test--graphql query variables (org-linear--bound callback)
                                              (and errback (org-linear--bound errback))
                                              retried))))
         (unwind-protect
             (progn ,@body)
           (dolist (name '(work home))
             (org-linear--with-profile name
               (when-let* ((buffer (find-buffer-visiting org-linear-file)))
                 (with-current-buffer buffer (set-buffer-modified-p nil))
                 (kill-buffer buffer))
               (when (get-buffer org-linear--log-buffer)
                 (kill-buffer org-linear--log-buffer)))))))))

(defun org-linear-test--sync-profile (name)
  "Run a sync of profile NAME and wait until it ends."
  (org-linear-sync nil name)
  (let ((deadline (+ (float-time) 60)))
    (while (and (org-linear--with-profile name org-linear--busy)
                (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (should-not (org-linear--with-profile name org-linear--busy)))

(ert-deftest org-linear-test-profiles-keep-separate-state ()
  "Two profiles sync into their own files, state files and logs."
  (org-linear-test--with-profiles
    (org-linear-test--sync-profile 'work)
    (should (org-linear--with-profile 'work (file-exists-p org-linear-file)))
    (should-not (org-linear--with-profile 'home (file-exists-p org-linear-file)))
    (should (org-linear--with-profile 'work (file-exists-p org-linear-state-file)))
    (should-not (equal (org-linear--with-profile 'work org-linear-state-file)
                       (org-linear--with-profile 'home org-linear-state-file)))
    (should (org-linear--with-profile 'work (org-linear--state-tasks)))
    (should-not (org-linear--with-profile 'home org-linear--state))
    (should-not org-linear--state)
    (org-linear-test--sync-profile 'home)
    (should (org-linear--with-profile 'home (file-exists-p org-linear-file)))
    (should-not (equal (org-linear--with-profile 'work org-linear--log-buffer)
                       (org-linear--with-profile 'home org-linear--log-buffer)))))

(ert-deftest org-linear-test-profile-option-falls-back-to-global ()
  "A profile takes the global value of an option it leaves out."
  (org-linear-test--with-profiles
    (should (equal (org-linear--with-profile 'work org-linear-teams) '("ENG")))
    (should (equal (org-linear--with-profile 'work org-linear-file)
                   (expand-file-name "work.org" org-linear-test--dir)))
    (should (equal org-linear-file (expand-file-name "linear.org" org-linear-test--dir)))))

(ert-deftest org-linear-test-process-callback-keeps-profile ()
  "A process started in a profile calls back in that profile."
  (let ((org-linear-profiles '((work :teams ("W"))))
        (org-linear--profile-states (make-hash-table :test #'eq))
        seen)
    (org-linear--with-profile 'work
      (org-linear--run-process '("true") nil
                               (lambda (&rest _) (setq seen (list org-linear--profile org-linear-teams)))))
    (let ((deadline (+ (float-time) 10)))
      (while (and (null seen) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (equal seen '(work ("W"))))))

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

(ert-deftest org-linear-test-closed-issue-archived ()
  "An issue closed long ago moves to the archive, under its project."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (should-not (org-linear-test--archive-text))
    (org-linear-test--close-long-ago "i2")
    (org-linear-test--sync)
    (should-not (string-match-p "Joint task" (org-linear-test--text)))
    (let ((archive (org-linear-test--archive-text)))
      (should (equal (org-linear--archive-file-name)
                     (expand-file-name "linear-archive.org" org-linear-test--dir)))
      (should (string-match-p "^#\\+TODO: TRIAGE" archive))
      (should (string-match-p "^\\* Project A\n+\\*\\* DONE .*Joint task" archive))
      (should (string-match-p ":LINEAR_ID: +ENG-2" archive))
      (should (string-match-p ":ARCHIVE_OLPATH: Project A" archive)))
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (should-not (buffer-modified-p)))
    (with-current-buffer (find-buffer-visiting (org-linear--archive-file-name))
      (should-not (buffer-modified-p)))
    (org-linear-test--sync)
    (should (= 1 (with-temp-buffer
                   (insert (org-linear-test--archive-text))
                   (count-matches "Joint task" (point-min) (point-max)))))))

(ert-deftest org-linear-test-archive-leaves-kill-ring ()
  "Archiving touches neither the kill ring nor the clipboard."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--close-long-ago "i2")
    (let ((kill-ring (list "mine"))
          (kill-ring-yank-pointer nil)
          (copied nil)
          (interprogram-cut-function (lambda (text) (setq copied text))))
      (org-linear-test--sync)
      (should (equal kill-ring '("mine")))
      (should-not copied))))

(ert-deftest org-linear-test-archive-waits-for-unsaved-archive ()
  "An archive with unsaved edits keeps closed issues in the file until saved."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--close-long-ago "i1")
    (org-linear-test--sync)
    (with-current-buffer (find-buffer-visiting (org-linear--archive-file-name))
      (goto-char (point-max))
      (insert "My unsaved note.\n"))
    (org-linear-test--close-long-ago "i2")
    (org-linear-test--sync)
    (should (string-match-p "Joint task" (org-linear-test--text)))
    (should (string-match-p "Not archived: .* has unsaved edits" (org-linear-test--log)))
    (with-current-buffer (find-buffer-visiting (org-linear--archive-file-name))
      (save-buffer))
    (org-linear-test--sync)
    (should-not (string-match-p "Joint task" (org-linear-test--text)))
    (should (string-match-p "Joint task" (org-linear-test--archive-text)))
    (should (string-match-p "My unsaved note" (org-linear-test--archive-text)))))

;;;; Archiving on demand

(ert-deftest org-linear-test-archive-closed-sticks ()
  "Archiving recent closed issues keeps them out until one is reopened."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (should (string-match-p "Closed today" (org-linear-test--text)))
    (org-linear-archive-closed)
    (should-not (string-match-p "Closed today" (org-linear-test--text)))
    (should (string-match-p "^\\* Project B\n+\\*\\* DONE Closed today"
                            (org-linear-test--archive-text)))
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (should-not (buffer-modified-p)))
    ;; The list of archived issues survives a restart.
    (setq org-linear--state nil)
    (org-linear-test--sync)
    (should-not (string-match-p "Closed today" (org-linear-test--text)))
    (org-linear-test--edit "i3" 'state (org-linear-test--state "s-todo"))
    (org-linear-test--edit "i3" 'completedAt nil)
    (org-linear-test--sync)
    (should (string-match-p "TODO Closed today" (org-linear-test--text)))
    (should-not (member "i3" (plist-get org-linear--state :archived)))))

(ert-deftest org-linear-test-archive-closed-keeps-unsent ()
  "A closed issue whose heading holds an unsent edit is not archived."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-3")
    (org-linear-test--editing
      (org-edit-headline "Closed today, renamed")
      (save-buffer))
    (org-linear-archive-closed)
    (should (string-match-p "Closed today, renamed" (org-linear-test--text)))
    (should-not (org-linear-test--archive-text))))

(ert-deftest org-linear-test-archive-at-point ()
  "The archive key archives a closed issue and refuses an open one."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-1")
    (org-linear-file-mode 1)
    (should (eq (key-binding (kbd "C-c C-x C-s")) #'org-linear-archive))
    (should-error (org-linear-archive) :type 'user-error)
    (should (string-match-p "Task one" (org-linear-test--text)))
    (org-linear-test--goto "ENG-3")
    (org-linear-archive)
    (should-not (string-match-p "Closed today" (org-linear-test--text)))
    (should (string-match-p "Closed today" (org-linear-test--archive-text)))
    (org-linear-test--sync)
    (should-not (string-match-p "Closed today" (org-linear-test--text)))))

(ert-deftest org-linear-test-closed-issue-deleted-without-archive ()
  "With no archive file, an issue closed long ago is deleted from the file."
  (org-linear-test--with-board
    (let ((org-linear-archive-file nil))
      (org-linear-test--sync)
      (org-linear-test--close-long-ago "i2")
      (org-linear-test--sync)
      (should-not (string-match-p "Joint task" (org-linear-test--text)))
      (should-not (directory-files org-linear-test--dir nil "archive")))))

(defun org-linear-test--edit-keyword-of (identifier keyword)
  "Set the TODO KEYWORD of the heading of IDENTIFIER, as an edit by the user."
  (org-linear-test--goto identifier)
  (org-linear-test--editing (org-todo keyword)))

(ert-deftest org-linear-test-deleted-issue-deleted ()
  "An issue deleted in Linear whose heading holds nothing unsent is deleted."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--edit "i1" 'trashed t)
    (org-linear-test--sync)
    (should-not (string-match-p "Task one" (org-linear-test--text)))
    (should-not (org-linear-test--archive-text))))

(ert-deftest org-linear-test-deleted-issue-with-edit-archived ()
  "An issue deleted in Linear whose heading holds an unsent edit is archived."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--edit-keyword-of "ENG-1" "DOING")
    (org-linear-test--edit "i1" 'trashed t)
    (org-linear-test--sync)
    (should-not (string-match-p "Task one" (org-linear-test--text)))
    (should (string-match-p "^\\*\\* DOING .*Task one" (org-linear-test--archive-text)))
    (should (string-match-p "Archived \"Task one\": it was deleted in Linear"
                            (org-linear-test--log)))
    (org-linear-test--sync)
    (should-not (string-match-p "Task one" (org-linear-test--text)))))

(ert-deftest org-linear-test-deleted-issue-with-edit-kept-without-archive ()
  "With no archive file, a deleted issue's heading with an unsent edit stays."
  (org-linear-test--with-board
    (let ((org-linear-archive-file nil))
      (org-linear-test--sync)
      (org-linear-test--edit-keyword-of "ENG-1" "DOING")
      (org-linear-test--edit "i1" 'trashed t)
      (org-linear-test--sync)
      (should (string-match-p "^\\*\\* DOING .*Task one" (org-linear-test--text)))
      (should (string-match-p "Kept \"Task one\": it was deleted in Linear"
                              (org-linear-test--log))))))

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
                   ("v" . org-linear-visit) ("b" . org-linear-browse)
                   ("s" . org-linear-sync) ("a" . org-linear-auto-sync-mode)
                   ("l" . org-linear-show-log) ("t" . org-linear-my-issues-todo)
                   ("c" . org-linear-comment) ("p" . org-linear-set-project)))
    (let ((suffix (transient-get-suffix 'org-linear-menu (car entry))))
      (should (memq (cdr entry) (flatten-tree suffix)))
      (should (commandp (cdr entry))))))

(ert-deftest org-linear-test-menu-lists-every-command ()
  "Every command of the package is in the menu, except the modes it manages itself."
  (let ((exempt '(org-linear-menu org-linear-file-mode org-linear-my-issues-mode))
        missing)
    (mapatoms (lambda (sym)
                (when (and (commandp sym)
                           (string-prefix-p "org-linear-" (symbol-name sym))
                           (not (memq sym exempt))
                           (equal (symbol-file sym 'defun)
                                  (symbol-file 'org-linear-sync 'defun))
                           (not (ignore-errors
                                  (catch 'found
                                    (dolist (key '("m" "v" "b" "t" "c" "p" "x" "X" "s" "a" "l"))
                                      (when (memq sym (flatten-tree
                                                       (transient-get-suffix 'org-linear-menu key)))
                                        (throw 'found t)))))))
                  (push sym missing))))
    (should-not missing)))

(ert-deftest org-linear-test-comment-uses-the-right-note-command ()
  "Commenting adds an Org note in the file and an agenda note in the list."
  (require 'org-agenda)
  (let (called)
    (cl-letf (((symbol-function 'org-add-note) (lambda () (setq called 'org)))
              ((symbol-function 'org-agenda-add-note) (lambda (&rest _) (setq called 'agenda))))
      (with-temp-buffer
        (org-mode)
        (org-linear-comment)
        (should (eq called 'org)))
      (with-temp-buffer
        (org-agenda-mode)
        (org-linear-comment)
        (should (eq called 'agenda))))))

(ert-deftest org-linear-test-list-opens-menu ()
  "A dot in the list of your issues opens the menu."
  (should (eq (lookup-key org-linear-my-issues-mode-map ".") #'org-linear-menu)))

(ert-deftest org-linear-test-browse-from-file-and-list ()
  "Opening the issue at point works in the file and in the list of your issues."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (let (opened)
      (cl-letf (((symbol-function 'browse-url) (lambda (url &rest _) (setq opened url))))
        (org-linear-test--goto "ENG-1")
        (org-linear-browse)
        (should (string-match-p "ENG-1" opened))
        (setq opened nil)
        (unwind-protect
            (progn
              (org-linear-my-issues)
              (with-current-buffer org-agenda-buffer-name
                (goto-char (point-min))
                (search-forward "Task one")
                (org-linear-browse)
                (should (string-match-p "ENG-1" opened))))
          (when (get-buffer org-agenda-buffer-name)
            (kill-buffer org-agenda-buffer-name)))
        (with-temp-buffer
          (should-error (org-linear-browse) :type 'user-error))))))

(ert-deftest org-linear-test-menu-shows-every-key ()
  "No menu key is hidden, so every command is offered wherever the menu opens."
  (dolist (key '("m" "v" "b" "t" "c" "p" "x" "X" "s" "a" "l"))
    (should-not (plist-get (cdr (transient-get-suffix 'org-linear-menu key)) :if))))

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

(defun org-linear-test--add-subheading-to (identifier)
  "Give the heading of IDENTIFIER a subheading of the user's, and save."
  (org-linear-test--goto identifier)
  (org-linear-test--editing
    (org-end-of-subtree t t)
    (insert "*** My private notes\nKeep these.\n")
    (save-buffer)))

(ert-deftest org-linear-test-subheadings-archived-with-issue ()
  "A closed issue's subheadings of yours move to the archive with it."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--add-subheading-to "ENG-2")
    (org-linear-test--close-long-ago "i2")
    (org-linear-test--sync)
    (should-not (string-match-p "Keep these" (org-linear-test--text)))
    (should (string-match-p "^\\*\\*\\* My private notes\nKeep these"
                            (org-linear-test--archive-text)))))

(ert-deftest org-linear-test-subheadings-survive-closing ()
  "Without an archive, a closed issue's heading with a subheading stays."
  (org-linear-test--with-board
    (let ((org-linear-archive-file nil))
      (org-linear-test--sync)
      (org-linear-test--add-subheading-to "ENG-2")
      (org-linear-test--close-long-ago "i2")
      (org-linear-test--sync)
      (should (string-match-p "Keep these" (org-linear-test--text))))))

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

(ert-deftest org-linear-test-transient-failures-end-the-sync ()
  "A failure in which Linear judged nothing ends the sync; it is no refusal."
  (dolist (out '("{}\n502" "{\"errors\":[{\"message\":\"slow down\"}]}\n429"
                 "{\"errors\":[{\"message\":\"slow down\",\"extensions\":{\"code\":\"RATELIMITED\"}}]}\n400"
                 "no status"))
    (let (refused failed)
      (cl-letf (((symbol-function 'org-linear--fail)
                 (lambda (&rest args) (setq failed (apply #'format args)))))
        (org-linear--handle-response
         "q" nil #'ignore (lambda (message) (setq refused message)) nil 0 out ""))
      (should failed)
      (should-not refused))))

(ert-deftest org-linear-test-lost-update-sent-again ()
  "An update whose answer is lost is sent again by the next sync."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-1")
    (org-linear-test--editing (org-edit-headline "Task one, edited"))
    (save-buffer)
    (setq org-linear-test--lose-next "OrgLinearEdited")
    (org-linear-test--sync)
    (should (equal (alist-get 'title (gethash "i1" org-linear-test--issues)) "Task one"))
    (org-linear-test--sync)
    (should (equal (alist-get 'title (gethash "i1" org-linear-test--issues)) "Task one, edited"))))

(ert-deftest org-linear-test-lost-create-not-repeated ()
  "A new issue whose creation answer is lost is linked, not created twice."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-1")
    (org-linear-test--editing
      (org-insert-heading-respect-content)
      (insert "TODO Brand new idea"))
    (save-buffer)
    (setq org-linear-test--lose-next "OrgLinearCreate")
    (org-linear-test--sync)
    (org-linear-test--sync)
    (let ((copies 0))
      (maphash (lambda (_ issue)
                 (when (equal (alist-get 'title issue) "Brand new idea") (cl-incf copies)))
               org-linear-test--issues)
      (should (= copies 1)))
    (should (= (how-many "Brand new idea" (point-min) (point-max)) 1))
    (should-not (string-match-p "LINEAR_PENDING_UUID" (org-linear-test--text)))
    (org-linear-test--goto-title "Brand new idea")
    (should (org-entry-get nil "LINEAR_ID"))))

(defmacro org-linear-test--with-second-peter (&rest body)
  "Run BODY with a second team member named Peter, Peter Other."
  (declare (indent 0))
  `(let ((orig (symbol-function 'org-linear-test--answer))
         (org-linear-test--users (cons '("u-peter2" . "Peter Other") org-linear-test--users)))
     (cl-letf (((symbol-function 'org-linear-test--answer)
                (lambda (name vars)
                  (let ((answer (funcall orig name vars)))
                    (when (equal name "OrgLinearMeta")
                      (let ((members (alist-get 'members
                                                (car (alist-get 'nodes (alist-get 'teams answer))))))
                        (setf (alist-get 'nodes members)
                              (append (alist-get 'nodes members)
                                      (list (org-linear-test--user "u-peter2"))))))
                    answer))))
       ,@body)))

(ert-deftest org-linear-test-namesakes-get-full-names ()
  "Members who share a first name get full-name tags; a label edit keeps the assignee."
  (org-linear-test--with-board
    (org-linear-test--with-second-peter
      (puthash "i9" (org-linear-test--issue "i9" "ENG-9" "Peter's task" :assignee "u-peter"
                                            :description "x.")
               org-linear-test--issues)
      (org-linear-test--sync)
      (org-linear-test--goto "ENG-9")
      (should (equal (org-get-tags nil t) '("peter_hartree")))
      (org-linear-test--editing (org-set-tags (cons "joint" (org-get-tags nil t))))
      (save-buffer)
      (org-linear-test--sync)
      (should (equal (alist-get 'assigneeId (org-linear-test--input "i9")) "u-peter")))))

(ert-deftest org-linear-test-shared-tag-not-sent-as-assignee ()
  "A tag the user gives to two people is not sent as an assignee."
  (org-linear-test--with-board
    (org-linear-test--with-second-peter
      (let ((org-linear-user-tags '(("Peter Hartree" . "peter") ("Peter Other" . "peter"))))
        (org-linear-test--sync)
        (org-linear-test--goto "ENG-1")
        (org-linear-test--editing (org-set-tags '("peter")))
        (save-buffer)
        (org-linear-test--sync)
        (should-not (assq 'assigneeId (org-linear-test--input "i1")))
        (should (string-match-p "stands for several people" (org-linear-test--log)))))))

(ert-deftest org-linear-test-unknown-labels-kept ()
  "A tag edit keeps the labels the sync does not know."
  (org-linear-test--with-board
    (setf (alist-get 'labels (gethash "i1" org-linear-test--issues))
          '((nodes . (((id . "l-far") (name . "far away"))))))
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-1")
    (org-linear-test--editing (org-set-tags (cons "joint" (org-get-tags nil t))))
    (save-buffer)
    (org-linear-test--sync)
    (should (equal (alist-get 'labelIds (org-linear-test--input "i1")) ["l-far" "l-joint"]))))

(ert-deftest org-linear-test-label-of-the-issue-team ()
  "A tag naming labels of two teams sends the label of the issue's team."
  (org-linear-test--with-board
    (let ((orig (symbol-function 'org-linear-test--answer)))
      (cl-letf (((symbol-function 'org-linear-test--answer)
                 (lambda (name vars)
                   (let ((answer (funcall orig name vars)))
                     (when (equal name "OrgLinearMeta")
                       (let* ((teams (alist-get 'teams answer))
                              (team (car (alist-get 'nodes teams))))
                         (setf (alist-get 'nodes teams)
                               (list team `((id . "t2") (key . "OPS") (name . "Ops")
                                            (states . ,(alist-get 'states team))
                                            (members . ((nodes . nil)))))))
                       (setf (alist-get 'nodes (alist-get 'issueLabels answer))
                             (append (alist-get 'nodes (alist-get 'issueLabels answer))
                                     '(((id . "l-joint-ops") (name . "joint")
                                        (team . ((id . "t2"))))))))
                     answer))))
        (org-linear-test--sync)
        (org-linear-test--goto "ENG-1")
        (org-linear-test--editing (org-set-tags (cons "joint" (org-get-tags nil t))))
        (save-buffer)
        (org-linear-test--sync)
        (should (equal (alist-get 'labelIds (org-linear-test--input "i1")) ["l-joint"]))))))

(ert-deftest org-linear-test-state-write-cut-short ()
  "A state write cut short leaves the previous state whole."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (let ((before (with-temp-buffer (insert-file-contents org-linear-state-file) (buffer-string)))
          (write (symbol-function 'write-region)))
      (cl-letf (((symbol-function 'write-region)
                 (lambda (start end file &rest args)
                   (apply write (substring (if start (buffer-substring start end) (buffer-string)) 0 10)
                          nil file args)
                   (error "No space left on device"))))
        (should-error (org-linear--save-state)))
      (should (equal (with-temp-buffer (insert-file-contents org-linear-state-file) (buffer-string))
                     before))
      (should (equal (directory-files (file-name-directory org-linear-state-file) nil "\\.tmp\\'")
                     nil)))))

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
  "SCHEDULED on a triage issue snoozes it until local midnight.
Removing it ends the snooze."
  (org-linear-test--with-board
    (org-linear-test--seed-started)
    (let ((org-linear-start-date-label "deferred")
          (tz (getenv "TZ")))
      (set-time-zone-rule "<-03>3")
      (unwind-protect
          (progn
            (org-linear-test--sync)
            (with-current-buffer (org-linear--buffer)
              (org-linear-test--goto-title "Triage task")
              (org-add-planning-info 'scheduled (date-to-time "2099-04-04T12:00:00"))
              (save-buffer))
            (org-linear-test--sync)
            (should (equal (alist-get 'snoozedUntilAt (gethash "i8" org-linear-test--issues))
                           "2099-04-04T03:00:00.000Z"))
            (with-current-buffer (org-linear--buffer)
              (org-linear-test--goto-title "Triage task")
              (org-add-planning-info nil nil 'scheduled)
              (save-buffer))
            (org-linear-test--sync)
            (should-not (alist-get 'snoozedUntilAt (gethash "i8" org-linear-test--issues))))
        (set-time-zone-rule tz)))))

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

(ert-deftest org-linear-test-my-issues-shows-dated-issues ()
  "An overdue issue stays in the list, in the imminent face, when the agenda hides dated tasks."
  (org-linear-test--with-board
    (let ((yesterday (format-time-string "%Y-%m-%d" (time-add nil (days-to-time -1)))))
      (org-linear-test--edit "i1" 'dueDate yesterday)
      (org-linear-test--sync)
      (unwind-protect
          (let ((org-agenda-todo-ignore-with-date t)
                (org-agenda-todo-ignore-deadlines 'all))
            (org-linear-my-issues)
            (with-current-buffer org-agenda-buffer-name
              (goto-char (point-min))
              (should (re-search-forward
                       (format "\\(%s\\) +\\(?:\\[#B\\] \\)?Task one"
                               (regexp-quote (substring yesterday 5)))
                       nil t))
              (should (memq 'org-imminent-deadline
                            (ensure-list (get-text-property (match-beginning 1) 'face))))))
        (when (get-buffer org-agenda-buffer-name)
          (kill-buffer org-agenda-buffer-name))))))

(ert-deftest org-linear-test-due-label-faces ()
  "A due date today or past is imminent, one within the warning days upcoming."
  (let ((org-deadline-warning-days 14)
        (date (lambda (offset)
                (format-time-string "%Y-%m-%d" (time-add nil (days-to-time offset))))))
    (should (eq (get-text-property 0 'org-linear-face (org-linear--due-label (funcall date -1)))
                'org-imminent-deadline))
    (should (eq (get-text-property 0 'org-linear-face (org-linear--due-label (funcall date 0)))
                'org-imminent-deadline))
    (should (eq (get-text-property 0 'org-linear-face (org-linear--due-label (funcall date 3)))
                'org-upcoming-deadline))
    (should-not (get-text-property 0 'org-linear-face (org-linear--due-label (funcall date 60))))
    (should (equal (org-linear--due-label "2026-11-10") "11-10"))))

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

(ert-deftest org-linear-test-list-dies-with-file-buffer ()
  "Killing the file's buffer kills the list, which would lead nowhere."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (let ((org-linear-my-issues-sync nil))
      (org-linear-test--with-list
        (let ((list org-linear--list-buffer))
          (should (buffer-live-p list))
          (kill-buffer (org-linear--buffer))
          (should-not (buffer-live-p list)))))))

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

;;;; Setting the project

(defvar org-linear-test--offered nil
  "The names the last project prompt offered.")

(defmacro org-linear-test--answering-project (name &rest body)
  "Run BODY with the project prompt answered by NAME."
  (declare (indent 1))
  `(cl-letf (((symbol-function 'completing-read)
              (lambda (_prompt collection &rest _)
                (setq org-linear-test--offered (mapcar #'car collection))
                ,name)))
     ,@body))

(defmacro org-linear-test--in-file-at (regexp &rest body)
  "Run BODY in the file with point on the first match of REGEXP."
  (declare (indent 1))
  `(with-current-buffer (find-buffer-visiting org-linear-file)
     (goto-char (point-min))
     (re-search-forward ,regexp)
     ,@body))

(ert-deftest org-linear-test-set-project-moves-saves-and-sends ()
  "Setting the project moves the heading, saves the file and sends the project."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--in-file-at "^\\*\\* .*Task one"
      (org-linear-test--answering-project "Project B"
        (org-linear-set-project))
      (should-not (buffer-modified-p)))
    (should (string-match-p "^\\* Project B\n\\(?:.*\n\\)*?\\*\\* .*Task one"
                            (org-linear-test--text)))
    (org-linear-test--sync)
    (let ((input (alist-get 'input (cdar (org-linear-test--calls "OrgLinearUpdate")))))
      (should (equal (alist-get 'projectId input) "p-b")))
    (org-linear-test--sync)
    (should-not (org-linear-test--writes))))

(ert-deftest org-linear-test-set-project-offers-only-projects ()
  "The prompt offers the other projects and no project, but no issues."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--in-file-at "^\\*\\* .*Task one"
      (org-linear-test--answering-project "Project B"
        (org-linear-set-project)))
    (should (equal (sort (copy-sequence org-linear-test--offered) #'string<)
                   '("No project" "Project B")))))

(ert-deftest org-linear-test-set-project-offers-project-without-heading ()
  "A project with no open issues is offered, and gets its heading when chosen."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (setq org-linear--meta
          (plist-put org-linear--meta :projects
                     (cons '((id . "p-c") (name . "Project C") (url . "u"))
                           (plist-get org-linear--meta :projects))))
    (org-linear-test--in-file-at "^\\*\\* .*Task one"
      (org-linear-test--answering-project "Project C"
        (org-linear-set-project)))
    (should (member "Project C" org-linear-test--offered))
    (should (string-match-p
             "^\\* Project C\n:PROPERTIES:\n:LINEAR_PROJECT: p-c\n:END:\n\\*\\* .*Task one"
             (org-linear-test--text)))))

(ert-deftest org-linear-test-set-project-names-are-plain ()
  "The names offered carry none of the buffer's text properties."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (let ((inhibit-read-only t))
        (add-text-properties (point-min) (point-max) '(face bold)))
      (set-buffer-modified-p nil))
    (org-linear-test--in-file-at "^\\*\\* .*Task one"
      (org-linear-test--answering-project "Project B"
        (org-linear-set-project)))
    (dolist (name org-linear-test--offered)
      (should-not (text-properties-at 0 name)))))

(ert-deftest org-linear-test-set-project-clears-the-project ()
  "Choosing no project moves the issue to that heading and clears it in Linear."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--in-file-at "^\\*\\* .*Task one"
      (org-linear-test--answering-project "No project"
        (org-linear-set-project)))
    (should (string-match-p "^\\* No project\n\\(?:.*\n\\)*?\\*\\* .*Task one"
                            (org-linear-test--text)))
    (org-linear-test--sync)
    (let ((input (alist-get 'input (cdar (org-linear-test--calls "OrgLinearUpdate")))))
      (should (eq (alist-get 'projectId input) :null)))))

(ert-deftest org-linear-test-set-project-from-inside-an-issue ()
  "From a subheading of an issue, the whole issue moves."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--in-file-at "^\\*\\* .*Task one"
      (org-end-of-subtree t t)
      (insert "*** A subheading\n")
      (save-buffer)
      (forward-line -1)
      (org-linear-test--answering-project "Project B"
        (org-linear-set-project)))
    (should (string-match-p
             "^\\* Project B\n\\(?:.*\n\\)*?\\*\\* .*Task one.*\n\\(?:.*\n\\)*?\\*\\*\\* A subheading"
             (org-linear-test--text)))))

(ert-deftest org-linear-test-set-project-of-a-new-heading ()
  "A heading not yet in Linear can move, and is created in its new project."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--in-file-at "^\\* Project A$"
      (org-end-of-subtree t t)
      (insert "** TRIAGE Captured just now\n")
      (save-buffer)
      (forward-line -1)
      (org-linear-test--answering-project "Project B"
        (org-linear-set-project)))
    (org-linear-test--sync)
    (let ((input (alist-get 'input (cdar (org-linear-test--calls "OrgLinearCreate")))))
      (should (equal (alist-get 'title input) "Captured just now"))
      (should (equal (alist-get 'projectId input) "p-b")))))

(ert-deftest org-linear-test-set-project-needs-an-issue ()
  "Off an issue, or outside the file, nothing is asked and nothing moves."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--in-file-at "^\\* Project A$"
      (should-error (org-linear-set-project) :type 'user-error))
    (with-temp-buffer
      (org-mode)
      (insert "* A\n** TODO B\n")
      (should-error (org-linear-set-project) :type 'user-error))))

(ert-deftest org-linear-test-set-project-empty-answer ()
  "An empty answer moves nothing."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (let ((before (org-linear-test--text)))
      (org-linear-test--in-file-at "^\\*\\* .*Task one"
        (org-linear-test--answering-project ""
          (should-error (org-linear-set-project) :type 'user-error)))
      (should (equal (org-linear-test--text) before)))))

(ert-deftest org-linear-test-set-project-prefix-refiles ()
  "With a prefix argument, the commands it replaces run."
  (require 'org-agenda)
  (require 'org-refile)
  (let (called)
    (cl-letf (((symbol-function 'org-refile)
               (lambda (&rest _) (interactive) (setq called 'org)))
              ((symbol-function 'org-agenda-refile)
               (lambda (&rest _) (interactive) (setq called 'agenda))))
      (with-temp-buffer
        (org-mode)
        (org-linear-set-project '(4))
        (should (eq called 'org)))
      (with-temp-buffer
        (org-agenda-mode)
        (org-linear-set-project '(4))
        (should (eq called 'agenda))))))

(ert-deftest org-linear-test-refile-keys-set-the-project ()
  "In the file and the list, the refile commands become this one."
  (should (eq (lookup-key org-linear-file-mode-map [remap org-refile])
              #'org-linear-set-project))
  (should (eq (lookup-key org-linear-my-issues-mode-map [remap org-agenda-refile])
              #'org-linear-set-project)))

(ert-deftest org-linear-test-list-set-project ()
  "From the list, the issue moves, its line shows the project and point stays on it."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--with-list
      (with-current-buffer org-linear--list-buffer
        (goto-char (point-min))
        (search-forward "Task one")
        (org-linear-test--answering-project "Project B"
          (org-linear-set-project))
        (should org-linear-my-issues-mode)
        (let ((line (buffer-substring (line-beginning-position) (line-end-position))))
          (should (string-match-p "Task one" line))
          (should (string-match-p "Project B" line)))))
    (should-not (buffer-modified-p (find-buffer-visiting org-linear-file)))
    (org-linear-test--sync)
    (let ((input (alist-get 'input (cdar (org-linear-test--calls "OrgLinearUpdate")))))
      (should (equal (alist-get 'projectId input) "p-b")))))

;;;; No retention

(ert-deftest org-linear-test-no-retention-archives-closed-today ()
  "With no retention, an issue closed today leaves the file at the next sync."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (should (string-match-p "Closed today" (org-linear-test--text)))
    (let ((org-linear-closed-retention-days 0))
      (org-linear-test--sync)
      (should-not (string-match-p "Closed today" (org-linear-test--text)))
      (should (string-match-p "DONE Closed today" (org-linear-test--archive-text)))
      (org-linear-test--sync)
      (should-not (string-match-p "Closed today" (org-linear-test--text))))))

(ert-deftest org-linear-test-no-retention-archives-after-sending ()
  "With no retention, an issue closed in the file leaves right after it is sent."
  (org-linear-test--with-board
    (let ((org-linear-closed-retention-days 0))
      (org-linear-test--sync)
      (org-linear-test--goto "ENG-1")
      (org-todo "DONE")
      (save-buffer)
      (org-linear-test--sync)
      (should (equal (alist-get 'type (alist-get 'state (gethash "i1" org-linear-test--issues)))
                     "completed"))
      (let ((deadline (+ (float-time) 10)))
        (while (and (or org-linear--busy (string-match-p "Task one" (org-linear-test--text)))
                    (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should-not (string-match-p "Task one" (org-linear-test--text)))
      (should (string-match-p "DONE .*Task one" (org-linear-test--archive-text))))))

(ert-deftest org-linear-test-retention-keeps-issue-closed-in-file ()
  "With retention, an issue closed in the file stays and no extra sync runs."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-1")
    (org-todo "DONE")
    (save-buffer)
    (org-linear-test--sync)
    (should-not org-linear--rerun)
    (should (string-match-p "DONE .*Task one" (org-linear-test--text)))))

;;;; Plain field values

(ert-deftest org-linear-test-local-view-is-plain ()
  "Fields read from a fontified heading carry no text properties.
They end up in the log and in the signatures of refused edits, which must
not change with the state of fontification."
  (org-linear-test--with-board
    (org-linear-test--sync)
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (let ((inhibit-read-only t))
        (add-text-properties (point-min) (point-max) '(face bold)))
      (goto-char (point-min))
      (re-search-forward "^\\*\\* .*Task one")
      (let ((view (org-linear--local-view (org-linear--marker))))
        (should (plist-get view :title))
        (should (plist-get view :todo))
        (should (plist-get view :priority))
        (should (plist-get view :deadline))
        (cl-loop for (field value) on view by #'cddr
                 do (dolist (item (if (listp value) value (list value)))
                      (when (stringp item)
                        (should-not (and (> (length item) 0)
                                         (text-properties-at 0 item)
                                         field)))))))))

;;;; In-place layout

(defvar org-linear-test--notes nil
  "Directory of the notes of the running in-place test.")

(defmacro org-linear-test--with-notes (files &rest body)
  "Run BODY over a notes directory holding FILES, synced in place.
FILES is an alist of (NAME . TEXT).  The fake board is the usual one."
  (declare (indent 1))
  `(org-linear-test--with-board
     (let* ((org-linear-test--notes (file-name-as-directory (expand-file-name "notes" org-linear-test--dir)))
            (org-linear-layout 'in-place)
            (org-linear-directories (list org-linear-test--notes))
            (org-linear-inbox-file (expand-file-name "inbox.org" org-linear-test--notes))
            (org-linear-exclude-tags '("private"))
            (org-linear-exclude-files '("/excluded\\.org\\'"))
            (org-linear-conflict-winner 'org)
            (org-linear-merge-body-function nil)
            (org-todo-keywords '((sequence "TODO(t)" "IMPORTANT(i)" "WAITING(w)"
                                           "|" "DONE(d)" "CANCELLED(c)")))
            (org-linear-test--labels (copy-sequence org-linear-test--labels))
            (org-linear--scan nil)
            (org-linear--body-overrides nil)
            (clock 0))
       (make-directory org-linear-test--notes t)
       (dolist (file ,files)
         (with-temp-file (expand-file-name (car file) org-linear-test--notes) (insert (cdr file))))
       (cl-letf (((symbol-function 'org-linear-test--tick)
                  (lambda () (format-time-string "%FT%TZ" (time-add nil (cl-incf clock)) t))))
         (unwind-protect
             (progn ,@body)
           (dolist (buffer (buffer-list))
             (when (and (buffer-file-name buffer)
                        (string-prefix-p org-linear-test--notes (buffer-file-name buffer)))
               (with-current-buffer buffer (set-buffer-modified-p nil))
               (kill-buffer buffer))))))))

(defun org-linear-test--note (name)
  "Return the text of note file NAME on disk."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name org-linear-test--notes))
    (buffer-string)))

(defun org-linear-test--write-note (name text)
  "Replace note file NAME with TEXT on disk, as an edit from elsewhere."
  (with-temp-file (expand-file-name name org-linear-test--notes) (insert text))
  (set-file-times (expand-file-name name org-linear-test--notes) (time-add nil 5)))

(defun org-linear-test--uuid-of (name title)
  "Return the LINEAR_UUID of the heading TITLE in note file NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name org-linear-test--notes))
    (org-mode)
    (goto-char (point-min))
    (re-search-forward (format "^\\*+ .*%s" (regexp-quote title)))
    (org-entry-get nil "LINEAR_UUID")))

(defun org-linear-test--issue-titled (title)
  "Return the fake issue titled TITLE, or nil."
  (seq-find (lambda (issue) (equal (alist-get 'title issue) title))
            (hash-table-values org-linear-test--issues)))

(defconst org-linear-test--health
  "* Health\n** TODO Book dentist\nCall before noon.\n** TODO Private thing :private:\n** DONE Old thing\n** Plain heading\n"
  "A note file with one task to sync.")

(ert-deftest org-linear-test-in-place-first-sync ()
  "A first sync creates issues for headings and adds Linear's issues to the inbox."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((issue (org-linear-test--issue-titled "Book dentist")))
      (should issue)
      (should (equal (alist-get 'description issue) "Call before noon."))
      (should (member "file:health" (mapcar (lambda (l) (alist-get 'name l))
                                            (alist-get 'nodes (alist-get 'labels issue)))))
      (should (equal (org-linear-test--uuid-of "health.org" "Book dentist")
                     (alist-get 'id issue))))
    (should-not (org-linear-test--issue-titled "Private thing"))
    (should-not (org-linear-test--issue-titled "Old thing"))
    (should-not (string-match-p "LINEAR_PENDING_UUID" (org-linear-test--note "health.org")))
    (let ((inbox (org-linear-test--note "inbox.org")))
      (should (string-match-p "^\\* Linear$" inbox))
      (should (string-match-p "^\\*\\* TODO \\[#B\\] Task one" inbox))
      (should (string-match-p "^\\*\\* TODO Peter's task" inbox))
      (should-not (string-match-p "Closed" inbox))
      (should-not (string-match-p ":pablo:" inbox)))
    (should-not (seq-filter (lambda (b) (and (buffer-file-name b)
                                             (string-prefix-p org-linear-test--notes (buffer-file-name b))))
                            (buffer-list)))))

(ert-deftest org-linear-test-in-place-second-sync-quiet ()
  "A sync right after the first sends nothing and changes no file."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((health (org-linear-test--note "health.org"))
          (inbox (org-linear-test--note "inbox.org")))
      (org-linear-test--sync)
      (should-not (org-linear-test--writes))
      (should (equal health (org-linear-test--note "health.org")))
      (should (equal inbox (org-linear-test--note "inbox.org"))))))

(ert-deftest org-linear-test-in-place-pulls-and-pushes ()
  "Edits made in Linear reach the heading, and edits made in Org reach Linear."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist"))))
      (org-linear-test--edit id 'title "Book the dentist")
      (org-linear-test--sync)
      (should (string-match-p "TODO Book the dentist" (org-linear-test--note "health.org")))
      (org-linear-test--write-note
       "health.org" (replace-regexp-in-string "Call before noon\\." "Call after noon."
                                              (org-linear-test--note "health.org")))
      (org-linear-test--sync)
      (should (equal (alist-get 'description (gethash id org-linear-test--issues))
                     "Call after noon.")))))

(ert-deftest org-linear-test-in-place-org-wins-conflicts ()
  "When both sides change a title, Org's stays and goes to Linear."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist"))))
      (org-linear-test--edit id 'title "Linear title")
      (org-linear-test--write-note
       "health.org" (replace-regexp-in-string "Book dentist" "Org title"
                                              (org-linear-test--note "health.org")))
      (org-linear-test--sync)
      (should (string-match-p "TODO Org title" (org-linear-test--note "health.org")))
      (should (equal (alist-get 'title (gethash id org-linear-test--issues)) "Org title"))
      (should (string-match-p "kept yours" (org-linear-test--log))))))

(ert-deftest org-linear-test-in-place-keyword-of-same-state-kept ()
  "A keyword that maps to the same Linear state as another is not replaced."
  (org-linear-test--with-notes '(("work.org" . "* IMPORTANT Big task\n"))
    (org-linear-test--sync)
    (let ((issue (org-linear-test--issue-titled "Big task")))
      (should (equal (alist-get 'name (alist-get 'state issue)) "Todo")))
    (org-linear-test--sync)
    (org-linear-test--sync)
    (should (string-match-p "^\\* IMPORTANT Big task" (org-linear-test--note "work.org")))))

(ert-deftest org-linear-test-in-place-exclusion-beats-link ()
  "A linked heading that becomes excluded neither syncs nor comes back in the inbox."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist"))))
      (org-linear-test--write-note
       "health.org" (replace-regexp-in-string "Book dentist" "Book dentist :private:"
                                              (org-linear-test--note "health.org")))
      (org-linear-test--sync)
      (org-linear-test--edit id 'title "Changed in Linear")
      (org-linear-test--sync)
      (should-not (string-match-p "Changed in Linear" (org-linear-test--note "health.org")))
      (should-not (string-match-p "Changed in Linear" (org-linear-test--note "inbox.org")))
      (should-not (alist-get 'archived (gethash id org-linear-test--issues))))))

(ert-deftest org-linear-test-in-place-unsaved-file-skipped ()
  "A file with unsaved edits is neither edited nor duplicated in the inbox."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let* ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist")))
           (buffer (find-file-noselect (expand-file-name "health.org" org-linear-test--notes))))
      (with-current-buffer buffer
        (goto-char (point-max))
        (insert "unsaved\n"))
      (org-linear-test--edit id 'title "Changed in Linear")
      (org-linear-test--sync)
      (with-current-buffer buffer
        (should-not (string-match-p "Changed in Linear" (buffer-string))))
      (should-not (string-match-p "Changed in Linear" (org-linear-test--note "inbox.org")))
      (should (string-match-p "unsaved edits" (org-linear-test--log)))
      (with-current-buffer buffer (save-buffer))
      (org-linear-test--sync)
      (should (string-match-p "TODO Changed in Linear" (org-linear-test--note "health.org"))))))

(ert-deftest org-linear-test-in-place-archived-heading-stays-present ()
  "A heading moved to an archive file is not added to the inbox again."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let* ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist")))
           (text (org-linear-test--note "health.org"))
           (start (string-match "^\\*\\* TODO Book dentist" text))
           (end (string-match "^\\*\\* TODO Private" text)))
      (org-linear-test--write-note "health.org_archive" (concat "* Archived\n" (substring text start end)))
      (org-linear-test--write-note "health.org" (concat (substring text 0 start) (substring text end)))
      (org-linear-test--edit id 'title "Changed in Linear")
      (org-linear-test--sync)
      (should-not (string-match-p "Changed in Linear" (org-linear-test--note "inbox.org")))
      (should-not (alist-get 'archived (gethash id org-linear-test--issues))))))

(ert-deftest org-linear-test-in-place-deleted-heading-archives-issue ()
  "A linked heading deleted in Org archives its issue, which does not come back."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist"))))
      (org-linear-test--write-note "health.org" "* Health\n")
      (org-linear-test--sync)
      (should (alist-get 'archived (gethash id org-linear-test--issues)))
      (org-linear-test--sync)
      (should-not (string-match-p "Book dentist" (org-linear-test--note "inbox.org"))))))

(ert-deftest org-linear-test-in-place-mass-vanishing-archives-nothing ()
  "More vanished headings than `org-linear-archive-limit' archive none."
  (org-linear-test--with-notes
      `(("work.org" . ,(mapconcat (lambda (n) (format "* TODO Task %d\n" n)) (number-sequence 1 7) "")))
    (org-linear-test--sync)
    (org-linear-test--write-note "work.org" "")
    (org-linear-test--sync)
    (should-not (org-linear-test--calls "OrgLinearArchive"))
    (should (string-match-p "more than `org-linear-archive-limit'" (org-linear-test--log)))))

(ert-deftest org-linear-test-in-place-trashed-issue-cancels-heading ()
  "An issue deleted in Linear cancels its heading, which keeps its text and is not recreated."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist")))
          (count (hash-table-count org-linear-test--issues)))
      (org-linear-test--edit id 'trashed t)
      (org-linear-test--sync)
      (let ((text (org-linear-test--note "health.org")))
        (should (string-match-p "^\\*\\* CANCELLED Book dentist" text))
        (should (string-match-p "Call before noon" text))
        (should-not (string-match-p (regexp-quote id) text)))
      (org-linear-test--sync)
      (should (= count (hash-table-count org-linear-test--issues))))))

(ert-deftest org-linear-test-in-place-repeat-once ()
  "Closing a repeating issue in Linear repeats its heading once, even if the state is lost."
  (org-linear-test--with-notes
      '(("habits.org" . "* TODO Water plants\nDEADLINE: <2026-11-10 Tue +1w>\n"))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Water plants")))
          (state (with-temp-buffer (insert-file-contents org-linear-state-file) (buffer-string))))
      (org-linear-test--edit id 'state (org-linear-test--state "s-done"))
      (org-linear-test--edit id 'completedAt (org-linear-test--tick))
      (org-linear-test--sync)
      (should (string-match-p "TODO Water plants\nDEADLINE: <2026-11-17" (org-linear-test--note "habits.org")))
      ;; As if the state were never saved after the heading.
      (with-temp-file org-linear-state-file (insert state))
      (setq org-linear--state nil)
      (org-linear-test--sync)
      (org-linear-test--sync)
      (should (string-match-p "DEADLINE: <2026-11-17" (org-linear-test--note "habits.org")))
      (let ((issue (gethash id org-linear-test--issues)))
        (should (equal (alist-get 'type (alist-get 'state issue)) "unstarted"))
        (should (equal (alist-get 'dueDate issue) "2026-11-17"))))))

(ert-deftest org-linear-test-in-place-lossless-body-pulled ()
  "A Linear description edit replaces a plain Org body."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist"))))
      (org-linear-test--edit id 'description "Call **after** lunch.")
      (org-linear-test--sync)
      (should (string-match-p "Call \\*after\\* lunch\\." (org-linear-test--note "health.org"))))))

(ert-deftest org-linear-test-in-place-lossy-body-merged-or-held ()
  "A Linear edit to a body with Org-only markup is merged, or held, and a held text is not retried."
  (org-linear-test--with-notes
      '(("code.org" . "* TODO Fix script\n#+begin_src emacs-lisp :tangle yes\n(foo)\n#+end_src\n"))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Fix script")))
          (header "#+begin_src emacs-lisp :tangle yes")
          (calls 0))
      (org-linear-test--edit id 'description "``` commonlisp\n(bar)\n```")
      (org-linear-test--sync)
      (should (string-match-p "(foo)" (org-linear-test--note "code.org")))
      (should (string-match-p "set `org-linear-merge-body-function'" (org-linear-test--log)))
      (let ((org-linear-merge-body-function
             (lambda (_org _old _new callback) (cl-incf calls) (funcall callback "Something else."))))
        (org-linear-test--sync)
        (should (= calls 0))
        (org-linear-test--edit id 'description "``` commonlisp\n(baz)\n```")
        (org-linear-test--sync)
        (org-linear-test--sync)
        (should (= calls 1))
        (should (string-match-p "(foo)" (org-linear-test--note "code.org")))
        (should (string-match-p "does not say what Linear's text says" (org-linear-test--log))))
      (let ((org-linear-merge-body-function
             (lambda (org old new callback)
               (cl-incf calls)
               (should (string-match-p (regexp-quote header) org))
               (should (string-match-p "(foo)" old))
               (should (string-match-p "(qux)" new))
               (funcall callback "#+begin_src emacs-lisp :tangle yes\n(qux)\n#+end_src"))))
        (org-linear-test--edit id 'description "``` commonlisp\n(qux)\n```")
        (org-linear-test--sync)
        (should (= calls 2))
        (should (string-match-p (regexp-quote "#+begin_src emacs-lisp :tangle yes\n(qux)\n#+end_src")
                                (org-linear-test--note "code.org"))))
      (org-linear-test--sync)
      (should-not (org-linear-test--writes))
      (should (string-match-p "(qux)" (org-linear-test--note "code.org"))))))

(ert-deftest org-linear-test-in-place-lost-create-not-repeated ()
  "A create whose answer was lost is linked by the next sync, not made twice."
  (org-linear-test--with-notes '(("work.org" . "* TODO Only once\n"))
    (setq org-linear-test--lose-next "OrgLinearCreate")
    (org-linear-sync)
    (let ((deadline (+ (float-time) 30)))
      (while (and org-linear--busy (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (string-match-p "LINEAR_PENDING_UUID" (org-linear-test--note "work.org")))
    (org-linear-test--sync)
    (should (= 1 (length (seq-filter (lambda (i) (equal (alist-get 'title i) "Only once"))
                                     (hash-table-values org-linear-test--issues)))))
    (should (org-linear-test--uuid-of "work.org" "Only once"))
    (should-not (string-match-p "LINEAR_PENDING_UUID" (org-linear-test--note "work.org")))))

(ert-deftest org-linear-test-in-place-create-limit ()
  "No more than `org-linear-create-limit' issues are created in one sync."
  (org-linear-test--with-notes
      `(("work.org" . ,(mapconcat (lambda (n) (format "* TODO Task %d\n" n)) (number-sequence 1 5) "")))
    (let ((org-linear-create-limit 2)
          (org-linear--reruns 2))
      (org-linear-test--sync)
      (should (= 2 (length (org-linear-test--calls "OrgLinearCreate"))))
      (org-linear-test--sync)
      (org-linear-test--sync)
      (should (= 5 (length (seq-filter (lambda (i) (string-match-p "\\`Task [0-9]" (alist-get 'title i)))
                                       (hash-table-values org-linear-test--issues))))))))

(ert-deftest org-linear-test-in-place-scope-change-rescans ()
  "Changing an exclusion makes the next sync read unchanged files again."
  (org-linear-test--with-notes '(("a.org" . "* TODO Alpha :later:\n"))
    (let ((org-linear-exclude-tags '("later")))
      (org-linear-test--sync)
      (should-not (org-linear-test--issue-titled "Alpha")))
    (let ((org-linear-exclude-tags nil))
      (org-linear-test--sync)
      (should (org-linear-test--issue-titled "Alpha")))))

(ert-deftest org-linear-test-in-place-preview-changes-nothing ()
  "The preview lists new headings and sends and writes nothing."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (let ((before (org-linear-test--note "health.org")))
      (setq org-linear-test--calls nil)
      (save-window-excursion (org-linear-preview))
      (should-not org-linear-test--calls)
      (should (equal before (org-linear-test--note "health.org")))
      (should-not (find-buffer-visiting (expand-file-name "health.org" org-linear-test--notes)))
      (with-current-buffer "*org-linear preview*"
        (should (string-match-p "New headings to create: 1" (buffer-string)))
        (should (string-match-p "  Book dentist" (buffer-string)))
        (should-not (string-match-p "Private thing" (buffer-string))))
      (kill-buffer "*org-linear preview*"))))

(ert-deftest org-linear-test-in-place-cookie-of-same-priority-kept ()
  "A cookie sent as the same Linear priority as another is not replaced."
  (org-linear-test--with-notes '(("work.org" . "#+PRIORITIES: 1 9 7\n* TODO [#7] Minor task\n"))
    (let ((org-linear-priorities '((1 . "1") (2 . "2") (3 . "4") (4 . "6"))))
      (org-linear-test--sync)
      (should (equal (alist-get 'priority (org-linear-test--issue-titled "Minor task")) 4))
      (org-linear-test--sync)
      (org-linear-test--sync)
      (should (string-match-p "^\\* TODO \\[#7\\] Minor task" (org-linear-test--note "work.org"))))))

(ert-deftest org-linear-test-in-place-first-import-not-in-background ()
  "A background sync does not make the first in-place import; a manual one does."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (setq org-linear-test--calls nil)
    (org-linear--timer-sync)
    (should-not org-linear--busy)
    (should-not org-linear-test--calls)
    (should (string-match-p "first in-place import" (org-linear-test--log)))
    (org-linear-test--sync)
    (should (org-linear-test--issue-titled "Book dentist"))
    (org-linear-test--write-note
     "health.org" (concat (org-linear-test--note "health.org") "** TODO Later task\n"))
    (org-linear--timer-sync)
    (let ((deadline (+ (float-time) 30)))
      (while (and org-linear--busy (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (org-linear-test--issue-titled "Later task"))))

(ert-deftest org-linear-test-in-place-candidates-need-real-keywords ()
  "A file whose headings only start with capitalized words is not a candidate."
  (org-linear-test--with-notes '(("prose.org" . "* AI safety\n* Big Ideas\n") ("todo.org" . "* TODO Real\n"))
    (let ((case-fold-search t))
      (should-not (plist-get (org-linear-in-place--read-file
                              (expand-file-name "prose.org" org-linear-test--notes) nil nil)
                             :candidates))
      (should (plist-get (org-linear-in-place--read-file
                          (expand-file-name "todo.org" org-linear-test--notes) nil nil)
                         :candidates)))))

(ert-deftest org-linear-test-in-place-linear-rewrite-is-not-an-edit ()
  "Linear's rewrite of a description it was sent changes no heading."
  (let ((org-linear-test--rewrite-descriptions t))
    (org-linear-test--with-notes
        '(("links.org" . "* TODO Read up\nSee https://example.org/a and [[https://example.com][x]].\n- one  \n- two\nLine one\nline two\n"))
      (org-linear-test--sync)
      (let ((issue (org-linear-test--issue-titled "Read up"))
            (text (org-linear-test--note "links.org")))
        (should (string-match-p "(<https://example.org/a>)" (alist-get 'description issue)))
        (org-linear-test--sync)
        (org-linear-test--sync)
        (should (equal text (org-linear-test--note "links.org")))
        (should-not (org-linear-test--writes))
        (should (string-match-p "^Line one\nline two$" (org-linear-test--note "links.org")))
        (should-not (string-match-p "Held back" (org-linear-test--log)))))))

(ert-deftest org-linear-test-mirror-linear-rewrite-is-not-an-edit ()
  "In the mirror layout too, a new issue's body is not replaced by Linear's rewrite."
  (let ((org-linear-test--rewrite-descriptions t))
    (org-linear-test--with-board
      (org-linear-test--sync)
      (with-current-buffer (find-buffer-visiting org-linear-file)
        (goto-char (point-min))
        (re-search-forward "^\\* Project A")
        (org-end-of-subtree t t)
        (insert "** TODO Fresh one\nSee https://example.org/b\n- first  \n- second\n")
        (save-buffer))
      (org-linear-test--sync)
      (let ((text (org-linear-test--text)))
        (should (string-match-p "See https://example.org/b\n- first\\s-*\n- second" text))
        (org-linear-test--sync)
        (should (equal text (org-linear-test--text)))
        (should-not (org-linear-test--writes))))))

(ert-deftest org-linear-test-profiles-nested-see-live-state ()
  "A profile entered again inside another one sees, and keeps, its live state."
  (let ((org-linear-profiles '((work :teams ("W")) (home :teams ("H"))))
        (org-linear--profile-states (make-hash-table :test #'eq))
        (org-linear--busy nil)
        inner)
    (org-linear--with-profile 'work
      (setq org-linear--busy 'running)
      (org-linear--with-profile nil
        (should-not org-linear--busy)
        (setq org-linear--busy 'default-busy)
        (org-linear--with-profile 'work
          (setq inner org-linear--busy)
          (setq org-linear--pending t)))
      (should (eq org-linear--busy 'running))
      (should (eq org-linear--pending t)))
    (should (eq inner 'running))
    (should (eq org-linear--busy 'default-busy))
    (should (eq (org-linear--with-profile 'work org-linear--busy) 'running))))

(ert-deftest org-linear-test-in-place-profile-claims-no-file ()
  "An in-place profile that inherits `org-linear-file' does not sync on visiting it."
  (org-linear-test--with-board
    (let ((org-linear-profiles '((notes :layout in-place :teams ("ENG"))))
          (org-linear--profile-states (make-hash-table :test #'eq))
          synced)
      (org-linear-test--sync)
      (cl-letf (((symbol-function 'org-linear-sync)
                 (lambda (&rest _) (push org-linear--profile synced))))
        (with-current-buffer (find-buffer-visiting org-linear-file)
          (org-linear--maybe-enable-file-mode)))
      (should (equal synced '(nil))))))

(ert-deftest org-linear-test-in-place-file-indexed-once ()
  "A file reached under two names is indexed once."
  (org-linear-test--with-notes '(("work.org" . "* TODO Only once\n"))
    (let* ((link (expand-file-name "alias" org-linear-test--dir))
           (scan (list :present (make-hash-table :test #'equal) :tasks (make-hash-table :test #'equal)
                       :new nil :pending (make-hash-table :test #'equal)
                       :duplicates (make-hash-table :test #'equal) :buffers nil :opened nil
                       :skipped (make-hash-table :test #'equal))))
      (make-symbolic-link org-linear-test--notes link)
      (org-linear--load-state)
      (org-linear-in-place--index-file (expand-file-name "work.org" org-linear-test--notes) scan)
      (org-linear-in-place--index-file (expand-file-name "work.org" link) scan)
      (should (= 1 (length (plist-get scan :new))))
      (dolist (buffer (plist-get scan :opened)) (kill-buffer buffer)))))

(ert-deftest org-linear-test-in-place-one-save-per-file ()
  "A sync that links several new issues in one file saves it once."
  (org-linear-test--with-notes
      `(("work.org" . ,(mapconcat (lambda (n) (format "* TODO Task %d\n" n)) (number-sequence 1 4) "")))
    (let ((saves 0))
      (cl-letf (((symbol-function 'org-linear-in-place--refresh-cache)
                 (let ((orig (symbol-function 'org-linear-in-place--refresh-cache)))
                   (lambda (buffer)
                     (when (string-suffix-p "work.org" (buffer-file-name buffer)) (cl-incf saves))
                     (funcall orig buffer)))))
        (org-linear-test--sync))
      ;; One save when the scan's edits (pending IDs) are written, one for the links.
      (should (= saves 2))
      (should (= 4 (with-temp-buffer
                     (insert (org-linear-test--note "work.org"))
                     (how-many "^:LINEAR_UUID:" (point-min) (point-max)))))
      (should-not (string-match-p "PENDING" (org-linear-test--note "work.org")))
      (should-not (find-buffer-visiting (expand-file-name "work.org" org-linear-test--notes))))))

(provide 'org-linear-test)
;;; org-linear-test.el ends here
