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

(defvar org-linear-test--fail-next nil
  "An operation whose call fails before Linear acts on it.
A name means its next call; (NAME COUNT) means its COUNTth call from now.")

(defun org-linear-test--fail-now-p (name)
  "Return non-nil if this call of the operation NAME is to fail."
  (pcase org-linear-test--fail-next
    ((pred stringp)
     (when (equal name org-linear-test--fail-next)
       (setq org-linear-test--fail-next nil)
       t))
    (`(,failing ,count)
     (when (equal name failing)
       (if (> count 1)
           (progn (setq org-linear-test--fail-next (list failing (1- count))) nil)
         (setq org-linear-test--fail-next nil)
         t)))))

(defvar org-linear-test--projects nil
  "Hash table from project ID to the fake project, in GraphQL shape.")

(defvar org-linear-test--initiatives nil
  "Hash table from initiative ID to the fake initiative, in GraphQL shape.")

(defvar org-linear-test--milestones nil
  "Hash table from milestone ID to the fake project milestone, in GraphQL shape.")

(defvar org-linear-test--links nil
  "The fake links of projects to initiatives: (ID PROJECT INITIATIVE).")

(defvar org-linear-test--parents nil
  "The fake relations of initiatives to their parents: (ID PARENT CHILD).")

(defvar org-linear-test--initiatives-disabled nil
  "Non-nil makes the fake refuse initiatives, as a workspace without them does.")

(defconst org-linear-test--project-statuses
  '(((id . "ps-backlog") (name . "Backlog") (type . "backlog") (position . 0))
    ((id . "ps-planned") (name . "Planned") (type . "planned") (position . 1))
    ((id . "ps-started") (name . "In Progress") (type . "started") (position . 2))
    ((id . "ps-done") (name . "Completed") (type . "completed") (position . 3))
    ((id . "ps-canceled") (name . "Canceled") (type . "canceled") (position . 4)))
  "The fake workspace's project statuses.")

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
    (parent . ,nil) (projectMilestone . ,nil)
    (labels . ((nodes . ,(mapcar (lambda (id) (seq-find (lambda (l) (equal (alist-get 'id l) id))
                                                        org-linear-test--labels))
                                 labels))))
    (comments . ((nodes . ,comments)))))

(defun org-linear-test--seed ()
  "Fill the fake board with the issues the tests start from."
  (setq org-linear-test--issues (make-hash-table :test #'equal)
        org-linear-test--calls nil
        org-linear-test--all-calls nil
        org-linear-test--clock 0
        org-linear-test--refuse-next nil
        org-linear-test--lose-next nil
        org-linear-test--fail-next nil
        org-linear-test--projects (make-hash-table :test #'equal)
        org-linear-test--initiatives (make-hash-table :test #'equal)
        org-linear-test--milestones (make-hash-table :test #'equal)
        org-linear-test--links nil
        org-linear-test--parents nil
        org-linear-test--initiatives-disabled nil)
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
         (answer (if (org-linear-test--fail-now-p name)
                     'lost
                   (org-linear-test--answer name variables))))
    (push (cons name variables) org-linear-test--calls)
    (push (cons name variables) org-linear-test--all-calls)
    (when (equal name org-linear-test--lose-next)
      (setq org-linear-test--lose-next nil
            answer 'lost))
    ;; Guarded as `org-linear--run-process' guards Linear's answers.
    (run-at-time 0 nil (org-linear--bound
                        (lambda ()
                          (org-linear--ending-sync-on-failure
                            (cond
                             ;; As `org-linear--handle-response' treats a timeout.
                             ((eq answer 'lost)
                              (org-linear--fail "curl failed (28): Operation timed out"))
                             ((stringp answer)
                              (if errback (funcall errback answer) (org-linear--fail "%s" answer)))
                             (t (funcall callback answer)))))))))

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
     (if (string-match-p "REJECT" (alist-get 'title (alist-get 'input variables)))
         "Argument Validation Error"
       (let* ((input (alist-get 'input variables))
              (n (1+ (hash-table-count org-linear-test--issues)))
              (id (or (alist-get 'id input) (format "i-new-%d" n)))
              (issue (org-linear-test--issue id (format "ENG-%d" (+ 100 n)) (alist-get 'title input)
                                             :project nil)))
         (puthash id issue org-linear-test--issues)
         (org-linear-test--apply-input id input)
         `((issueCreate . ((success . t) (issue . ,(gethash id org-linear-test--issues))))))))
    ("OrgLinearArchive"
     (let ((issue (gethash (alist-get 'id variables) org-linear-test--issues)))
       (cond
        ((null issue) "Entity not found: Issue")
        ((alist-get 'refuse-archive issue) "Forbidden")
        (t (setf (alist-get 'archived issue) t)
           (puthash (alist-get 'id variables) issue org-linear-test--issues)
           '((issueArchive . ((success . t))))))))
    ("OrgLinearLabel"
     (let ((label `((id . ,(format "l-new-%d" (cl-incf org-linear-test--clock)))
                    (name . ,(alist-get 'name (alist-get 'input variables)))
                    (team . nil))))
       (setq org-linear-test--labels (append org-linear-test--labels (list label)))
       `((issueLabelCreate . ((success . t) (issueLabel . ,label))))))
    (_ (or (org-linear-test--answer-object name variables)
           (format "unknown operation %s" name)))))

(defun org-linear-test--answer-object (name variables)
  "Return the fake Linear's answer to the object operation NAME with VARIABLES.
A string is an error message; nil means NAME is no object operation."
  (let ((input (alist-get 'input variables))
        (id (alist-get 'id variables)))
    (pcase name
      ("OrgLinearObjects"
       (let ((ids (append (alist-get 'ids variables) nil)))
         `((organization . ((roadmapEnabled . ,(if org-linear-test--initiatives-disabled :false t))))
           (projectStatuses . ((nodes . ,org-linear-test--project-statuses)))
           (projects . ((nodes . ,(org-linear-test--select
                                   org-linear-test--projects ids #'org-linear-test--project-node))))
           (initiatives . ((nodes . ,(org-linear-test--select
                                      org-linear-test--initiatives ids
                                      #'org-linear-test--initiative-node))))
           (projectMilestones . ((nodes . ,(org-linear-test--select
                                            org-linear-test--milestones ids
                                            (lambda (id) (gethash id org-linear-test--milestones)))))))))
      ("OrgLinearProjectCreate"
       (org-linear-test--put-object org-linear-test--projects (alist-get 'id input)
                                    `((trashed . :false) (status . ,(org-linear-test--project-status "ps-backlog"))
                                      (url . "https://linear.app/x/project/new"))
                                    input)
       `((projectCreate . ((success . t) (project . ,(org-linear-test--project-node
                                                     (alist-get 'id input)))))))
      ("OrgLinearProjectUpdate"
       (org-linear-test--put-object org-linear-test--projects id nil input)
       `((projectUpdate . ((success . t) (project . ,(org-linear-test--project-node id))))))
      ("OrgLinearProjectEdited" `((project . ,(org-linear-test--edited org-linear-test--projects id))))
      ("OrgLinearProjectArchive"
       (org-linear-test--put-object org-linear-test--projects id '((trashed . t)) nil)
       '((projectDelete . ((success . t)))))
      ("OrgLinearInitiativeCreate"
       (if org-linear-test--initiatives-disabled
           "Initiatives are disabled for this workspace."
         (org-linear-test--put-object org-linear-test--initiatives (alist-get 'id input)
                                      '((trashed . :false) (status . "Planned")
                                        (url . "https://linear.app/x/initiative/new"))
                                      input)
         `((initiativeCreate . ((success . t) (initiative . ,(org-linear-test--initiative-node
                                                              (alist-get 'id input))))))))
      ("OrgLinearInitiativeUpdate"
       (org-linear-test--put-object org-linear-test--initiatives id nil input)
       `((initiativeUpdate . ((success . t) (initiative . ,(org-linear-test--initiative-node id))))))
      ("OrgLinearInitiativeEdited"
       `((initiative . ,(org-linear-test--edited org-linear-test--initiatives id))))
      ("OrgLinearInitiativeArchive"
       (org-linear-test--put-object org-linear-test--initiatives id '((archived . t)) nil)
       '((initiativeArchive . ((success . t)))))
      ("OrgLinearMilestoneCreate"
       (if (not (gethash (alist-get 'projectId input) org-linear-test--projects))
           "Entity not found: Project"
         (org-linear-test--put-object org-linear-test--milestones (alist-get 'id input) nil input)
         `((projectMilestoneCreate . ((success . t) (projectMilestone . ,(gethash (alist-get 'id input)
                                                                                   org-linear-test--milestones)))))))
      ("OrgLinearMilestoneUpdate"
       (org-linear-test--put-object org-linear-test--milestones id nil input)
       `((projectMilestoneUpdate . ((success . t) (projectMilestone . ,(gethash id org-linear-test--milestones))))))
      ("OrgLinearMilestones"
       `((projectMilestones
          . ((pageInfo . ((hasNextPage . :false) (endCursor . nil)))
             (nodes . ,(hash-table-values org-linear-test--milestones))))))
      ("OrgLinearMilestoneEdited"
       `((projectMilestone . ,(org-linear-test--edited org-linear-test--milestones id))))
      ("OrgLinearMilestoneArchive"
       (remhash id org-linear-test--milestones)
       (dolist (issue (hash-table-values org-linear-test--issues))
         (when (equal (alist-get 'id (alist-get 'projectMilestone issue)) id)
           (setf (alist-get 'projectMilestone issue) nil)))
       '((projectMilestoneDelete . ((success . t)))))
      ("OrgLinearProjectLink"
       (if (seq-find (lambda (link) (and (equal (nth 1 link) (alist-get 'projectId input))
                                         (equal (nth 2 link) (alist-get 'initiativeId input))))
                     org-linear-test--links)
           "The project is already in this initiative"
         (push (list (format "link-%d" (cl-incf org-linear-test--clock))
                     (alist-get 'projectId input) (alist-get 'initiativeId input))
               org-linear-test--links)
         '((initiativeToProjectCreate . ((success . t))))))
      ("OrgLinearProjectUnlink"
       (setq org-linear-test--links (seq-remove (lambda (link) (equal (car link) id))
                                                org-linear-test--links))
       '((initiativeToProjectDelete . ((success . t)))))
      ("OrgLinearInitiativeParent"
       (let ((child (alist-get 'relatedInitiativeId input)))
         (setq org-linear-test--parents (seq-remove (lambda (r) (equal (nth 2 r) child))
                                                    org-linear-test--parents))
         (push (list (format "rel-%d" (cl-incf org-linear-test--clock))
                     (alist-get 'initiativeId input) child)
               org-linear-test--parents)
         '((initiativeRelationCreate . ((success . t))))))
      ("OrgLinearInitiativeRelations"
       `((initiativeRelations
          . ((nodes . ,(mapcar (lambda (r) `((id . ,(nth 0 r)) (initiative . ((id . ,(nth 1 r))))
                                              (relatedInitiative . ((id . ,(nth 2 r))))))
                               org-linear-test--parents))))))
      ("OrgLinearInitiativeUnparent"
       (setq org-linear-test--parents (seq-remove (lambda (r) (equal (car r) id))
                                                  org-linear-test--parents))
       '((initiativeRelationDelete . ((success . t))))))))

(defun org-linear-test--select (table ids shape)
  "Return the values of TABLE whose keys are among IDS, through SHAPE."
  (delq nil (mapcar (lambda (id) (when (gethash id table) (funcall shape id))) ids)))

(defun org-linear-test--project-status (id)
  "Return the fake project status with ID."
  (seq-find (lambda (s) (equal (alist-get 'id s) id)) org-linear-test--project-statuses))

(defun org-linear-test--put-object (table id defaults input)
  "Store in TABLE the object ID with DEFAULTS and then INPUT applied, as an edit."
  (let ((object (or (copy-alist (gethash id table)) `((id . ,id)))))
    (pcase-dolist (`(,key . ,value) (append defaults input))
      (let ((value (if (eq value :null) nil value)))
        (pcase key
          ('teamIds nil)
          ('statusId (setf (alist-get 'status object) (org-linear-test--project-status value)))
          ('projectId (setf (alist-get 'project object) `((id . ,value))))
          ((or 'content 'description)
           (setf (alist-get key object)
                 (if (and value org-linear-test--rewrite-descriptions)
                     (org-linear-test--linear-markdown value)
                   value)))
          (_ (setf (alist-get key object) value)))))
    (setf (alist-get 'updatedAt object) (org-linear-test--tick))
    (puthash id object table)))

(defun org-linear-test--edited (table id)
  "Return the last edit of object ID of TABLE, as the edit check reads it."
  `((updatedAt . ,(if org-linear-test--refuse-next
                      (progn (setq org-linear-test--refuse-next nil) "changed")
                    (alist-get 'updatedAt (gethash id table))))))

(defun org-linear-test--project-node (id)
  "Return the fake project ID with its links to initiatives."
  (append (gethash id org-linear-test--projects)
          `((initiativeToProjects
             . ((nodes . ,(mapcar (lambda (link) `((id . ,(nth 0 link))
                                                   (initiative . ((id . ,(nth 2 link))))))
                                  (seq-filter (lambda (link) (equal (nth 1 link) id))
                                              org-linear-test--links))))))))

(defun org-linear-test--initiative-node (id)
  "Return the fake initiative ID with its parent."
  (append (gethash id org-linear-test--initiatives)
          `((parentInitiative
             . ,(when-let* ((relation (seq-find (lambda (r) (equal (nth 2 r) id))
                                                org-linear-test--parents)))
                  `((id . ,(nth 1 relation))))))))

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
          ('parentId (setf (alist-get 'parent issue) (and value `((id . ,value)))))
          ('projectMilestoneId (setf (alist-get 'projectMilestone issue)
                                     (and value `((id . ,value)))))
          ('labelIds (setf (alist-get 'labels issue)
                           `((nodes . ,(mapcar (lambda (lid) (seq-find (lambda (l) (equal (alist-get 'id l) lid))
                                                                       org-linear-test--labels))
                                               value))))))))
    (setf (alist-get 'updatedAt issue) (org-linear-test--tick))
    (puthash id issue org-linear-test--issues)))

;;;; Harness

(defvar org-linear-test--dir nil
  "Temporary directory of the running test.")

(defvar org-linear-test--timers nil
  "The timers that existed when the running test began.")

(defmacro org-linear-test--with-board (&rest body)
  "Run BODY with a fresh fake board, mirrored file and sync state.
Timers BODY leaves behind, such as a sync scheduled to run again, are
cancelled, so that they cannot start a sync in a later test."
  (declare (indent 0))
  `(let* ((org-linear-test--timers (append timer-list timer-idle-list))
          (org-linear-test--dir (make-temp-file "org-linear-test" t))
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
         (dolist (timer (append timer-list timer-idle-list))
           (unless (memq timer org-linear-test--timers)
             (cancel-timer timer)))
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

(defvar org-linear-test--all-calls nil
  "Every operation the fake Linear answered in the running test, newest first.")

(defun org-linear-test--calls-ever (name)
  "Return the operations named NAME the fake Linear answered in the running test."
  (seq-filter (lambda (call) (equal (car call) name)) org-linear-test--all-calls))

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

(ert-deftest org-linear-test-failing-cleanup-still-ends-the-sync ()
  "An error while a failed sync cleans up does not leave it running."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (cl-letf (((symbol-function 'org-linear-in-place--flush) (lambda () (error "Flush broke")))
              ((symbol-function 'org-linear-in-place--apply-task) (lambda (&rest _) (error "Boom"))))
      (org-linear-test--sync))
    (should-not org-linear--busy)
    (should (string-match-p "Boom" (org-linear-test--log)))
    (should (string-match-p "Flush broke" (org-linear-test--log)))
    (should-not (org-linear-test--reading-buffers))))

(ert-deftest org-linear-test-callback-error-ends-its-profile-sync ()
  "An error in a callback of a profile's sync ends that profile's sync."
  (org-linear-test--with-profiles
    (org-linear--with-profile 'work
      (setq org-linear--busy t)
      (org-linear--run-process (list "true") nil (lambda (&rest _) (error "Boom"))))
    (let ((deadline (+ (float-time) 10)))
      (while (and (org-linear--with-profile 'work org-linear--busy) (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should-not (org-linear--with-profile 'work org-linear--busy))
    (should (string-match-p "Boom" (org-linear--with-profile 'work (org-linear-test--log))))))

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
               (kill-buffer buffer)))
           ;; A buffer a sync still running read a file in.
           (mapc #'kill-buffer (org-linear-test--reading-buffers)))))))

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
    ;; The files the sync edited stay open, saved, in the buffers that visit
    ;; them, and the buffers it read files in are gone.
    (dolist (name '("health.org" "inbox.org"))
      (let ((buffer (find-buffer-visiting (expand-file-name name org-linear-test--notes))))
        (should buffer)
        (should-not (buffer-modified-p buffer))))
    (should-not (org-linear-test--reading-buffers))))

(ert-deftest org-linear-test-in-place-assigns-new-issues ()
  "With the option set, an issue created from a heading is assigned to the viewer."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (let ((org-linear-assign-new-issues t))
      (org-linear-test--sync))
    (let ((input (alist-get 'input (cdar (org-linear-test--calls "OrgLinearCreate")))))
      (should (equal (alist-get 'assigneeId input) "u-pablo")))))

(ert-deftest org-linear-test-in-place-leaves-new-issues-unassigned ()
  "Without the option, an issue created from a heading has no assignee."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((input (alist-get 'input (cdar (org-linear-test--calls "OrgLinearCreate")))))
      (should-not (assq 'assigneeId input)))))

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

(ert-deftest org-linear-test-in-place-gone-issue-forgotten ()
  "A vanished heading whose issue Linear no longer has is forgotten, not retried."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist"))))
      (remhash id org-linear-test--issues)
      (org-linear-test--write-note "health.org" "* Health\n")
      (org-linear-test--sync)
      (should-not (gethash id (org-linear--state-tasks)))
      (should (string-match-p "Linear no longer has it" (org-linear-test--log)))
      (should-not (string-match-p "Rejected" (org-linear-test--log)))
      (org-linear-test--sync)
      (should-not (org-linear-test--calls "OrgLinearArchive")))))

(ert-deftest org-linear-test-in-place-refused-archive-not-retried ()
  "An archive Linear refuses for another reason is not sent again."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist"))))
      (org-linear-test--edit id 'refuse-archive t)
      (org-linear-test--write-note "health.org" "* Health\n")
      (org-linear-test--sync)
      (should (= 1 (length (org-linear-test--calls "OrgLinearArchive"))))
      (should (string-match-p "Forbidden" (org-linear-test--log)))
      (org-linear-test--sync)
      (should-not (org-linear-test--calls "OrgLinearArchive")))))

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

(ert-deftest org-linear-test-in-place-failed-create-sent-again ()
  "A create that failed before Linear acted on it is sent again by the next sync."
  (org-linear-test--with-notes '(("work.org" . "* TODO Only once\n"))
    (setq org-linear-test--fail-next "OrgLinearCreate")
    (org-linear-sync)
    (let ((deadline (+ (float-time) 30)))
      (while (and org-linear--busy (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should-not (org-linear-test--issue-titled "Only once"))
    (should (string-match-p "LINEAR_PENDING_UUID" (org-linear-test--note "work.org")))
    (org-linear-test--sync)
    (should (org-linear-test--issue-titled "Only once"))
    (should (org-linear-test--uuid-of "work.org" "Only once"))
    (should-not (string-match-p "LINEAR_PENDING_UUID" (org-linear-test--note "work.org")))))

(ert-deftest org-linear-test-in-place-failed-create-after-links-sent-again ()
  "A create that failed after others in its file were linked is sent again."
  (org-linear-test--with-notes '(("work.org" . "* TODO First\n* TODO Second\n"))
    (setq org-linear-test--fail-next '("OrgLinearCreate" 2))
    (org-linear-sync)
    (let ((deadline (+ (float-time) 30)))
      (while (and org-linear--busy (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (org-linear-test--issue-titled "First"))
    (should-not (org-linear-test--issue-titled "Second"))
    ;; As if the next sync came later than the overlap of the fetches.
    (plist-put org-linear--state :since (format-time-string "%FT%TZ" (time-add nil 3600) t))
    (org-linear-test--sync)
    (should (org-linear-test--issue-titled "Second"))
    (should (org-linear-test--uuid-of "work.org" "Second"))))

(ert-deftest org-linear-test-in-place-failed-sync-sends-edits-later ()
  "An Org edit a failed sync did not send is sent by a later one."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist")))
          (later (format-time-string "%FT%TZ" (time-add nil 3600) t)))
      (dolist (failing '("OrgLinearIssues" "OrgLinearUpdate"))
        (org-linear-test--write-note
         "health.org" (replace-regexp-in-string "^Call .*$" (format "Call %s." failing)
                                                (org-linear-test--note "health.org")))
        (setq org-linear-test--fail-next failing)
        (org-linear-sync)
        (let ((deadline (+ (float-time) 30)))
          (while (and org-linear--busy (< (float-time) deadline))
            (accept-process-output nil 0.05)))
        (should-not (equal (alist-get 'description (gethash id org-linear-test--issues))
                           (format "Call %s." failing)))
        ;; As if the next sync came later than the overlap of the fetches.
        (plist-put org-linear--state :since later)
        (org-linear-test--sync)
        (should (equal (alist-get 'description (gethash id org-linear-test--issues))
                       (format "Call %s." failing)))))))

(ert-deftest org-linear-test-in-place-scan-works-in-slices ()
  "The scan reads the files in slices: a sync returns before it has read them."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health)
                                 ("work.org" . "* TODO Write report\n"))
    (let ((org-linear-in-place--slice 0))
      (org-linear-sync)
      (should org-linear--busy)
      (should-not (plist-get org-linear--scan :read))
      (let ((deadline (+ (float-time) 30)))
        (while (and org-linear--busy (< (float-time) deadline))
          (accept-process-output nil 0.05)))
      (should-not org-linear--busy)
      (should (org-linear-test--issue-titled "Book dentist"))
      (should (org-linear-test--issue-titled "Write report"))
      (should (org-linear-test--uuid-of "work.org" "Write report")))))

(ert-deftest org-linear-test-in-place-buffer-edited-while-read-left-out ()
  "A buffer edited while the scan reads it is left out, unedited."
  (org-linear-test--with-notes
      `(("many.org" . ,(mapconcat (lambda (n) (format "* TODO Task %d\n" n)) (number-sequence 1 60) "")))
    (let ((org-linear-in-place--slice 0)
          (buffer (find-file-noselect (expand-file-name "many.org" org-linear-test--notes)))
          (edited nil))
      (cl-letf (((symbol-function 'org-linear-in-place--index-headings)
                 (let ((index (symbol-function 'org-linear-in-place--index-headings)))
                   (lambda (job scan count)
                     (prog1 (funcall index job scan count)
                       (unless edited
                         (setq edited t)
                         (with-current-buffer (plist-get job :buffer)
                           (goto-char (point-max))
                           (insert "Mine.\n"))))))))
        (org-linear-test--sync))
      (should edited)
      (should (string-match-p "changed while the sync read it" (org-linear-test--log)))
      (should-not (org-linear-test--calls "OrgLinearCreate"))
      (with-current-buffer buffer
        (should (buffer-modified-p))
        (should (string-match-p "Mine\\.\n\\'" (buffer-string)))
        (should-not (string-match-p "LINEAR" (buffer-string)))))))

(ert-deftest org-linear-test-in-place-apply-in-slices-keeps-its-edits ()
  "Applying in many slices, the sync does not take its own edits for the user's."
  (org-linear-test--with-hierarchy
      `(("plan.org" . ,org-linear-test--tree)
        ("more.org" . ,(mapconcat (lambda (n) (format "* TODO Task %d\n" n)) (number-sequence 1 8) "")))
    (let ((org-linear-in-place--slice 0))
      (org-linear-test--sync-until-quiet))
    (should-not (string-match-p "changed during the sync" (org-linear-test--log)))
    (should (org-linear-test--property-of "plan.org" "Site" "LINEAR_PROJECT"))
    (should (org-linear-test--uuid-of "more.org" "Task 8"))))

(ert-deftest org-linear-test-in-place-buffer-saved-during-sync-left-out ()
  "A buffer edited and saved while the sync waits for Linear is not edited by it."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist")))
          (buffer (find-file-noselect (expand-file-name "health.org" org-linear-test--notes))))
      (org-linear-test--edit id 'title "Changed in Linear")
      (cl-letf (((symbol-function 'org-linear-in-place--fetch)
                 (let ((fetch (symbol-function 'org-linear-in-place--fetch)))
                   (lambda (callback)
                     (with-current-buffer buffer
                       (goto-char (point-max))
                       (insert "** TODO Added meanwhile\n")
                       (save-buffer))
                     (funcall fetch callback)))))
        (org-linear-test--sync))
      (should (string-match-p "changed during the sync" (org-linear-test--log)))
      (should-not (string-match-p "Changed in Linear" (org-linear-test--note "health.org")))
      (org-linear-test--sync)
      (should (string-match-p "TODO Changed in Linear" (org-linear-test--note "health.org")))
      (should (org-linear-test--issue-titled "Added meanwhile")))))

(ert-deftest org-linear-test-in-place-refused-create-left-alone ()
  "A heading whose creation Linear refused does not make its file be read again."
  (org-linear-test--with-notes '(("work.org" . "* TODO REJECT this\n"))
    (dotimes (_ 4) (org-linear-test--sync))
    (should (= 1 (length (org-linear-test--calls-ever "OrgLinearCreate"))))
    (with-current-buffer (get-buffer-create org-linear--log-buffer)
      (should (= 1 (how-many "Not created again" (point-min) (point-max)))))))

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

(ert-deftest org-linear-test-in-place-merge-callback-error-ends-sync ()
  "An error after the merge function answers ends the sync instead of wedging it.
The function answers later, as a model's reply does, outside the sync's
own callbacks."
  (org-linear-test--with-notes
      '(("code.org" . "* TODO Fix script\n#+begin_src emacs-lisp :tangle yes\n(foo)\n#+end_src\n"))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Fix script")))
          (org-linear-merge-body-function
           (lambda (_org _old _new callback)
             (run-at-time 0 nil (lambda () (funcall callback "Merged."))))))
      (org-linear-test--edit id 'description "``` commonlisp\n(bar)\n```")
      (cl-letf (((symbol-function 'org-linear-in-place--check-merge)
                 (lambda (&rest _) (error "Merge check broke"))))
        (org-linear-sync)
        (let ((deadline (+ (float-time) 10)))
          (while (and org-linear--busy (< (float-time) deadline))
            (accept-process-output nil 0.05))))
      (should-not org-linear--busy)
      (should (string-match-p "Merge check broke" (org-linear-test--log))))))

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
           (scan (org-linear-in-place--new-scan)))
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
      ;; The buffer the sync edited the file in stays, saved.
      (should-not (buffer-modified-p
                   (find-buffer-visiting (expand-file-name "work.org" org-linear-test--notes)))))))

(ert-deftest org-linear-test-in-place-backlog-in-unchanged-file ()
  "Headings left for later syncs are created even if their file does not change."
  (org-linear-test--with-notes
      `(("a.org" . ,(mapconcat (lambda (n) (format "* TODO Task %d\n" n)) (number-sequence 1 3) ""))
        ("b.org" . ,(mapconcat (lambda (n) (format "* TODO Task %d\n" n)) (number-sequence 4 6) "")))
    (let ((org-linear-create-limit 2))
      (dotimes (_ 4) (org-linear-test--sync))
      (should (= 6 (length (seq-filter (lambda (i) (string-match-p "\\`Task [0-9]" (alist-get 'title i)))
                                       (hash-table-values org-linear-test--issues))))))))

;;;; In-place hierarchy

(defmacro org-linear-test--with-hierarchy (files &rest body)
  "Run BODY over notes FILES synced in place, with projects and milestones.
PROJECT and MILESTONE are the keywords of project and milestone headings."
  (declare (indent 1))
  `(org-linear-test--with-notes ,files
     (let ((org-linear-project-keywords '("PROJECT"))
           (org-linear-milestone-keywords '("MILESTONE"))
           (org-todo-keywords '((sequence "TODO(t)" "PROJECT(p)" "MILESTONE(m)" "WAITING(w)"
                                          "|" "DONE(d)" "CANCELLED(c)"))))
       ,@body)))

(defconst org-linear-test--tree
  "* PROJECT Big
Big body.
** TODO Own task
** PROJECT Site
DEADLINE: <2026-12-01 Tue>
*** MILESTONE Launch
**** TODO Write copy
***** TODO Draft headline
*** TODO Plain site task
* PROJECT Area
** PROJECT Garden
*** TODO Water
* PROJECT Empty
* PROJECT Top
** PROJECT Mid
*** PROJECT Leaf
**** TODO Leaf task
* TODO Loose parent
** TODO Loose child
"
  "A note file with every case of the hierarchy.")

(defun org-linear-test--named (table title)
  "Return the fake object of TABLE whose name is TITLE."
  (seq-find (lambda (object) (equal (alist-get 'name object) title)) (hash-table-values table)))

(defun org-linear-test--id-named (table title)
  "Return the ID of the fake object of TABLE whose name is TITLE."
  (alist-get 'id (org-linear-test--named table title)))

(defun org-linear-test--initiatives-of (project-title)
  "Return the IDs of the initiatives the fake project PROJECT-TITLE is in."
  (let ((id (org-linear-test--id-named org-linear-test--projects project-title)))
    (mapcar (lambda (link) (nth 2 link))
            (seq-filter (lambda (link) (equal (nth 1 link) id)) org-linear-test--links))))

(defun org-linear-test--parent-of (initiative-title)
  "Return the ID of the parent of the fake initiative INITIATIVE-TITLE."
  (let ((id (org-linear-test--id-named org-linear-test--initiatives initiative-title)))
    (nth 1 (seq-find (lambda (r) (equal (nth 2 r) id)) org-linear-test--parents))))

(defun org-linear-test--relation (title key)
  "Return the ID that the fake issue TITLE's relation KEY points to."
  (alist-get 'id (alist-get key (org-linear-test--issue-titled title))))

(defun org-linear-test--property-of (name title property)
  "Return PROPERTY of the heading TITLE in note file NAME."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name org-linear-test--notes))
    (org-mode)
    (goto-char (point-min))
    (re-search-forward (format "^\\*+ .*%s$" (regexp-quote title)))
    (org-entry-get nil property)))

(defun org-linear-test--edit-note (name edit)
  "Rewrite note file NAME by calling EDIT at the start of an Org buffer of its text."
  (declare (indent 1))
  (org-linear-test--write-note
   name (let ((kill-ring nil)
              (kill-ring-yank-pointer nil))
          (with-temp-buffer
            (insert (org-linear-test--note name))
            (org-mode)
            (goto-char (point-min))
            (funcall edit)
            (buffer-string)))))

(defun org-linear-test--sync-until-quiet ()
  "Sync until a sync creates nothing, at most five times."
  (let ((n 0))
    (while (progn (org-linear-test--sync)
                  (cl-incf n)
                  (and (< n 5)
                       (seq-some (lambda (call) (string-match-p "Create\\'" (car call)))
                                 (org-linear-test--calls)))))))

(defun org-linear-test--hierarchy-writes ()
  "Return the names of the write operations of the last sync, objects included."
  (seq-filter (lambda (name) (string-match-p "Create\\|Update\\|Link\\|Unlink\\|Parent\\|Archive\\|Comment" name))
              (mapcar #'car (org-linear-test--calls))))

(ert-deftest org-linear-test-hierarchy-off-by-default ()
  "With both options nil, nesting is not synced and no object is fetched."
  (org-linear-test--with-notes '(("work.org" . "* TODO Parent\n** TODO Child\n"))
    (let ((org-linear-exclude-keywords '("PROJECT")))
      (should-not org-linear-project-keywords)
      (should-not org-linear-milestone-keywords)
      (org-linear-test--sync)
      (should-not (org-linear-test--calls "OrgLinearObjects"))
      (should-not (org-linear-test--relation "Child" 'parent))
      (should-not (seq-some (lambda (call) (assq 'parentId (alist-get 'input (cdr call))))
                            (org-linear-test--calls "OrgLinearCreate"))))))

(ert-deftest org-linear-test-hierarchy-maps-every-case ()
  "Tasks, subtasks, projects, initiatives and milestones map as the manual says."
  (org-linear-test--with-hierarchy `(("plan.org" . ,org-linear-test--tree))
    (org-linear-test--sync-until-quiet)
    (let ((projects org-linear-test--projects)
          (initiatives org-linear-test--initiatives)
          (milestones org-linear-test--milestones))
      ;; Project headings are never issues.
      (dolist (title '("Big" "Site" "Area" "Garden" "Empty" "Top" "Mid" "Leaf" "Launch"))
        (should-not (org-linear-test--issue-titled title)))
      ;; 1. A task under a task is its sub-issue.
      (should (equal (org-linear-test--relation "Loose child" 'parent)
                     (alist-get 'id (org-linear-test--issue-titled "Loose parent"))))
      (should (equal (org-linear-test--relation "Draft headline" 'parent)
                     (alist-get 'id (org-linear-test--issue-titled "Write copy"))))
      (should-not (org-linear-test--relation "Write copy" 'parent))
      ;; 2. A project holding tasks or milestones is a Linear project; so is an empty one.
      (dolist (title '("Site" "Garden" "Leaf" "Empty"))
        (should (org-linear-test--named projects title))
        (should-not (org-linear-test--named initiatives title)))
      ;; 3. A project holding only projects is an initiative, nested under its parent's.
      (dolist (title '("Area" "Top" "Mid"))
        (should (org-linear-test--named initiatives title))
        (should-not (org-linear-test--named projects title)))
      (should (equal (org-linear-test--parent-of "Mid") (org-linear-test--id-named initiatives "Top")))
      (should-not (org-linear-test--parent-of "Top"))
      ;; 4. A project holding both is an initiative and a project.
      (should (org-linear-test--named initiatives "Big"))
      (should (org-linear-test--named projects "Big"))
      ;; 5. A project is in the initiative of the project heading above, or its own.
      (should (equal (org-linear-test--initiatives-of "Big") (list (org-linear-test--id-named initiatives "Big"))))
      (should (equal (org-linear-test--initiatives-of "Site") (list (org-linear-test--id-named initiatives "Big"))))
      (should (equal (org-linear-test--initiatives-of "Garden") (list (org-linear-test--id-named initiatives "Area"))))
      (should (equal (org-linear-test--initiatives-of "Leaf") (list (org-linear-test--id-named initiatives "Mid"))))
      (should-not (org-linear-test--initiatives-of "Empty"))
      ;; 6. A milestone belongs to the project above; the issues below it are assigned to it.
      (let ((launch (org-linear-test--named milestones "Launch")))
        (should (equal (alist-get 'id (alist-get 'project launch)) (org-linear-test--id-named projects "Site")))
        (should (equal (org-linear-test--relation "Write copy" 'projectMilestone) (alist-get 'id launch)))
        (should (equal (org-linear-test--relation "Draft headline" 'projectMilestone) (alist-get 'id launch))))
      (should-not (org-linear-test--relation "Plain site task" 'projectMilestone))
      ;; 7. An issue's project is that of the nearest project heading above.
      (should (equal (org-linear-test--relation "Own task" 'project) (org-linear-test--id-named projects "Big")))
      (dolist (title '("Write copy" "Draft headline" "Plain site task"))
        (should (equal (org-linear-test--relation title 'project) (org-linear-test--id-named projects "Site"))))
      (should (equal (org-linear-test--relation "Water" 'project) (org-linear-test--id-named projects "Garden")))
      (should (equal (org-linear-test--relation "Leaf task" 'project) (org-linear-test--id-named projects "Leaf")))
      (should-not (org-linear-test--relation "Loose parent" 'project))
      ;; Fields: body, deadline, open status.
      (should (equal (alist-get 'content (org-linear-test--named projects "Big")) "Big body."))
      (should (equal (alist-get 'content (org-linear-test--named initiatives "Big")) "Big body."))
      (should (equal (alist-get 'targetDate (org-linear-test--named projects "Site")) "2026-12-01"))
      (should (equal (alist-get 'type (alist-get 'status (org-linear-test--named projects "Site"))) "started"))
      (should (equal (alist-get 'status (org-linear-test--named initiatives "Area")) "Active"))
      ;; IDs live in the headings' properties, and no creation is left pending.
      (should (equal (org-linear-test--property-of "plan.org" "Big" "LINEAR_PROJECT")
                     (org-linear-test--id-named projects "Big")))
      (should (equal (org-linear-test--property-of "plan.org" "Big" "LINEAR_INITIATIVE")
                     (org-linear-test--id-named initiatives "Big")))
      (should (equal (org-linear-test--property-of "plan.org" "Launch" "LINEAR_MILESTONE")
                     (org-linear-test--id-named milestones "Launch")))
      (should-not (org-linear-test--property-of "plan.org" "Area" "LINEAR_PROJECT"))
      (should-not (string-match-p "PENDING" (org-linear-test--note "plan.org")))
      ;; An issue created in Linear in a project keeps it in the inbox.
      (should (equal (org-linear-test--relation "Task one" 'project) "p-a")))))

(ert-deftest org-linear-test-hierarchy-second-sync-quiet ()
  "Syncs after the hierarchy is in Linear send nothing and change no file."
  (org-linear-test--with-hierarchy `(("plan.org" . ,org-linear-test--tree))
    (org-linear-test--sync-until-quiet)
    (let ((plan (org-linear-test--note "plan.org"))
          (inbox (org-linear-test--note "inbox.org")))
      (org-linear-test--sync)
      (should-not (org-linear-test--hierarchy-writes))
      (org-linear-test--sync)
      (should-not (org-linear-test--hierarchy-writes))
      (should (equal plan (org-linear-test--note "plan.org")))
      (should (equal inbox (org-linear-test--note "inbox.org"))))))

(ert-deftest org-linear-test-hierarchy-refiling-moves-relations ()
  "Moving headings sends their new relations at the next sync; identity stays."
  (org-linear-test--with-hierarchy `(("plan.org" . ,org-linear-test--tree))
    (org-linear-test--sync-until-quiet)
    (let ((draft (alist-get 'id (org-linear-test--issue-titled "Draft headline")))
          (garden (org-linear-test--id-named org-linear-test--projects "Garden"))
          (count (hash-table-count org-linear-test--issues)))
      ;; The subtask moves under the other task; the Garden project under Big.
      (org-linear-test--edit-note "plan.org"
        (lambda ()
          (re-search-forward "^\\*+ TODO Draft headline")
          (org-cut-subtree)
          (re-search-forward "^\\*+ TODO Plain site task")
          (org-end-of-subtree t t)
          (org-paste-subtree 4)
          (goto-char (point-min))
          (re-search-forward "^\\*+ PROJECT Garden")
          (org-cut-subtree)
          (goto-char (point-min))
          (re-search-forward "^\\* PROJECT Big")
          (org-end-of-subtree t t)
          (org-paste-subtree 2)
          (goto-char (point-min))
          (re-search-forward "^\\*+ MILESTONE Launch")
          (org-cut-subtree)
          (re-search-forward "^\\*+ PROJECT Garden")
          (org-end-of-subtree t t)
          (org-paste-subtree 3)))
      (org-linear-test--sync)
      (should (equal (alist-get 'id (org-linear-test--issue-titled "Draft headline")) draft))
      (should (equal (org-linear-test--relation "Draft headline" 'parent)
                     (alist-get 'id (org-linear-test--issue-titled "Plain site task"))))
      (should-not (org-linear-test--relation "Draft headline" 'projectMilestone))
      (should (equal (org-linear-test--relation "Draft headline" 'project)
                     (org-linear-test--id-named org-linear-test--projects "Site")))
      (should (equal (org-linear-test--id-named org-linear-test--projects "Garden") garden))
      (should (equal (alist-get 'id (alist-get 'project (org-linear-test--named org-linear-test--milestones "Launch")))
                     garden))
      (should (equal (org-linear-test--relation "Write copy" 'project) garden))
      (should (equal (org-linear-test--relation "Write copy" 'projectMilestone)
                     (org-linear-test--id-named org-linear-test--milestones "Launch")))
      (should (equal (org-linear-test--initiatives-of "Garden")
                     (list (org-linear-test--id-named org-linear-test--initiatives "Big"))))
      (should (= count (hash-table-count org-linear-test--issues)))
      (org-linear-test--sync)
      (should-not (org-linear-test--hierarchy-writes)))))

(ert-deftest org-linear-test-hierarchy-fields-both-ways ()
  "Titles, bodies, deadlines and statuses of objects sync both ways."
  (org-linear-test--with-hierarchy
      '(("plan.org" . "* PROJECT Site\nAbout the site.\n** MILESTONE Launch\n*** TODO Copy\n"))
    (org-linear-test--sync-until-quiet)
    (let ((site (org-linear-test--id-named org-linear-test--projects "Site"))
          (launch (org-linear-test--id-named org-linear-test--milestones "Launch")))
      ;; Linear to Org.
      (org-linear-test--put-object org-linear-test--projects site nil
                                   '((name . "Website") (targetDate . "2026-12-24")
                                     (content . "About the **site**.")))
      (org-linear-test--put-object org-linear-test--milestones launch nil '((name . "Go live")))
      (org-linear-test--sync)
      (let ((text (org-linear-test--note "plan.org")))
        (should (string-match-p "^\\* PROJECT Website\nDEADLINE: <2026-12-24" text))
        (should (string-match-p "About the \\*site\\*\\." text))
        (should (string-match-p "^\\*\\* MILESTONE Go live" text)))
      (org-linear-test--put-object org-linear-test--projects site nil '((statusId . "ps-done")))
      (org-linear-test--sync)
      (should (string-match-p "^\\* DONE Website" (org-linear-test--note "plan.org")))
      ;; Org to Linear.
      (org-linear-test--write-note
       "plan.org" (replace-regexp-in-string "^\\* DONE Website" "* CANCELLED Website"
                                            (org-linear-test--note "plan.org")))
      (org-linear-test--sync)
      (should (equal (alist-get 'type (alist-get 'status (gethash site org-linear-test--projects)))
                     "canceled"))
      (org-linear-test--write-note
       "plan.org" (replace-regexp-in-string "Go live" "Ship it" (org-linear-test--note "plan.org")))
      (org-linear-test--sync)
      (should (equal (alist-get 'name (gethash launch org-linear-test--milestones)) "Ship it"))
      (org-linear-test--sync)
      (should-not (org-linear-test--hierarchy-writes)))))

(ert-deftest org-linear-test-hierarchy-conflict ()
  "When both sides rename a project, the conflict winner's title stays."
  (org-linear-test--with-hierarchy '(("plan.org" . "* PROJECT Site\n** TODO Copy\n"))
    (org-linear-test--sync-until-quiet)
    (let ((site (org-linear-test--id-named org-linear-test--projects "Site")))
      (org-linear-test--put-object org-linear-test--projects site nil '((name . "Linear name")))
      (org-linear-test--write-note
       "plan.org" (replace-regexp-in-string "PROJECT Site" "PROJECT Org name"
                                            (org-linear-test--note "plan.org") t))
      (org-linear-test--sync)
      (should (string-match-p "PROJECT Org name" (org-linear-test--note "plan.org")))
      (should (equal (alist-get 'name (gethash site org-linear-test--projects)) "Org name"))
      (should (string-match-p "Conflict in project \"Org name\" title: kept yours" (org-linear-test--log))))))

(ert-deftest org-linear-test-hierarchy-linear-move-kept ()
  "An issue moved to another project in Linear stays there until its heading moves."
  (org-linear-test--with-hierarchy
      '(("plan.org" . "* PROJECT Site\n** TODO Copy\n* PROJECT Shop\n** TODO Price\n"))
    (org-linear-test--sync-until-quiet)
    (let ((copy (alist-get 'id (org-linear-test--issue-titled "Copy")))
          (shop (org-linear-test--id-named org-linear-test--projects "Shop")))
      (org-linear-test--edit copy 'project `((id . ,shop)))
      (org-linear-test--sync)
      (should (equal (org-linear-test--relation "Copy" 'project) shop))
      (should (string-match-p "Kept Linear's project of" (org-linear-test--log)))
      (org-linear-test--sync)
      (should-not (org-linear-test--hierarchy-writes)))))

(ert-deftest org-linear-test-hierarchy-create-limit ()
  "Objects count toward the create limit, before issues; later syncs finish."
  (org-linear-test--with-hierarchy `(("plan.org" . ,org-linear-test--tree))
    (let ((org-linear-create-limit 3))
      (org-linear-test--sync)
      (should (= 3 (+ (length (org-linear-test--calls "OrgLinearProjectCreate"))
                      (length (org-linear-test--calls "OrgLinearInitiativeCreate"))
                      (length (org-linear-test--calls "OrgLinearMilestoneCreate"))
                      (length (org-linear-test--calls "OrgLinearCreate")))))
      (dotimes (_ 8) (org-linear-test--sync))
      (should (= 5 (hash-table-count org-linear-test--projects)))
      (should (= 4 (hash-table-count org-linear-test--initiatives)))
      (should (= 1 (hash-table-count org-linear-test--milestones)))
      (should (equal (org-linear-test--relation "Draft headline" 'parent)
                     (alist-get 'id (org-linear-test--issue-titled "Write copy"))))
      (should (equal (org-linear-test--initiatives-of "Site")
                     (list (org-linear-test--id-named org-linear-test--initiatives "Big"))))
      (should-not (string-match-p "PENDING" (org-linear-test--note "plan.org"))))))

(ert-deftest org-linear-test-hierarchy-vanished-archived ()
  "A deleted project heading archives its issues and project and deletes its milestone."
  (org-linear-test--with-hierarchy
      '(("plan.org" . "* PROJECT Site\n** MILESTONE Launch\n*** TODO Copy\n* PROJECT Shop\n** TODO Price\n"))
    (org-linear-test--sync-until-quiet)
    (let ((site (org-linear-test--id-named org-linear-test--projects "Site"))
          (copy (alist-get 'id (org-linear-test--issue-titled "Copy"))))
      (org-linear-test--edit-note "plan.org"
        (lambda () (re-search-forward "^\\* PROJECT Site") (org-cut-subtree)))
      (org-linear-test--sync)
      (should (alist-get 'archived (gethash copy org-linear-test--issues)))
      (should (eq t (alist-get 'trashed (gethash site org-linear-test--projects))))
      (should-not (org-linear-test--named org-linear-test--milestones "Launch"))
      (should-not (eq t (alist-get 'trashed (org-linear-test--named org-linear-test--projects "Shop"))))
      (org-linear-test--sync)
      (should-not (org-linear-test--hierarchy-writes)))))

(ert-deftest org-linear-test-hierarchy-mass-vanishing-archives-nothing ()
  "More vanished issues and objects than `org-linear-archive-limit' archive none."
  (org-linear-test--with-hierarchy
      '(("plan.org" . "* PROJECT Site\n** MILESTONE Launch\n*** TODO Copy\n*** TODO Edit\n** TODO Plan\n"))
    (org-linear-test--sync-until-quiet)
    (let ((org-linear-archive-limit 4))
      (org-linear-test--write-note "plan.org" "")
      (org-linear-test--sync)
      (should-not (org-linear-test--hierarchy-writes))
      (should (string-match-p "3 issues and 2 projects, initiatives or milestones are gone"
                              (org-linear-test--log))))))

(ert-deftest org-linear-test-hierarchy-deleted-in-linear ()
  "A project deleted in Linear cancels its heading, which keeps its text and is not recreated."
  (org-linear-test--with-hierarchy '(("plan.org" . "* PROJECT Site\nNotes.\n** TODO Copy\n"))
    (org-linear-test--sync-until-quiet)
    (let ((site (org-linear-test--id-named org-linear-test--projects "Site")))
      (org-linear-test--put-object org-linear-test--projects site nil '((trashed . t)))
      (org-linear-test--sync)
      (let ((text (org-linear-test--note "plan.org")))
        (should (string-match-p "^\\* CANCELLED Site\n" text))
        (should (string-match-p "Notes\\." text))
        (should-not (string-match-p (regexp-quote site) text)))
      (org-linear-test--sync)
      (org-linear-test--sync)
      (should (= 1 (hash-table-count org-linear-test--projects))))))

(ert-deftest org-linear-test-hierarchy-lost-create-not-repeated ()
  "A project whose creation answer was lost is linked by the next sync, not made twice."
  (org-linear-test--with-hierarchy '(("plan.org" . "* PROJECT Site\n** TODO Copy\n"))
    (setq org-linear-test--lose-next "OrgLinearProjectCreate")
    (org-linear-sync)
    (let ((deadline (+ (float-time) 30)))
      (while (and org-linear--busy (< (float-time) deadline))
        (accept-process-output nil 0.05)))
    (should (string-match-p "LINEAR_PENDING_PROJECT" (org-linear-test--note "plan.org")))
    (org-linear-test--sync-until-quiet)
    (should (= 1 (hash-table-count org-linear-test--projects)))
    (should (org-linear-test--property-of "plan.org" "Site" "LINEAR_PROJECT"))
    (should-not (string-match-p "PENDING" (org-linear-test--note "plan.org")))
    (should (equal (org-linear-test--relation "Copy" 'project)
                   (org-linear-test--id-named org-linear-test--projects "Site")))
    (org-linear-test--sync)
    (should-not (org-linear-test--hierarchy-writes))))

(ert-deftest org-linear-test-hierarchy-without-initiatives ()
  "Where Linear refuses initiatives, projects still sync and the refusal is not retried."
  (org-linear-test--with-hierarchy `(("plan.org" . ,org-linear-test--tree))
    (setq org-linear-test--initiatives-disabled t)
    (org-linear-test--sync-until-quiet)
    (should-not (org-linear-test--calls-ever "OrgLinearInitiativeCreate"))
    (dolist (title '("Big" "Site" "Garden" "Leaf" "Empty"))
      (should (org-linear-test--named org-linear-test--projects title)))
    (should-not (org-linear-test--initiatives-of "Site"))
    (should (equal (org-linear-test--relation "Own task" 'project)
                   (org-linear-test--id-named org-linear-test--projects "Big")))
    (should (string-match-p "Not created: 4 initiatives, since initiatives are turned off"
                            (org-linear-test--log)))
    (should-not (string-match-p "INITIATIVE" (org-linear-test--note "plan.org")))
    ;; The file is not marked to be read again for the initiatives.
    (org-linear-test--sync)
    (should-not (org-linear-test--hierarchy-writes))
    (should-not (plist-get (gethash (expand-file-name "plan.org" org-linear-test--notes)
                                    (plist-get org-linear--state :files))
                           :new))
    ;; Once initiatives are turned on, they are created and the projects join them.
    (setq org-linear-test--initiatives-disabled nil)
    (org-linear-test--write-note "plan.org" (concat (org-linear-test--note "plan.org") "* TODO Nudge\n"))
    (org-linear-test--sync-until-quiet)
    (should (= 4 (hash-table-count org-linear-test--initiatives)))
    (should (equal (org-linear-test--initiatives-of "Site")
                   (list (org-linear-test--id-named org-linear-test--initiatives "Big"))))
    (should (equal (org-linear-test--parent-of "Mid")
                   (org-linear-test--id-named org-linear-test--initiatives "Top")))))

(ert-deftest org-linear-test-hierarchy-vanished-initiative-archived ()
  "A deleted initiative heading archives the initiative, its project and its issue."
  (org-linear-test--with-hierarchy
      '(("plan.org" . "* PROJECT Area\n** PROJECT Garden\n*** TODO Water\n* TODO Keep\n"))
    (org-linear-test--sync-until-quiet)
    (let ((area (org-linear-test--id-named org-linear-test--initiatives "Area"))
          (garden (org-linear-test--id-named org-linear-test--projects "Garden"))
          (water (alist-get 'id (org-linear-test--issue-titled "Water"))))
      (org-linear-test--edit-note "plan.org"
        (lambda () (re-search-forward "^\\* PROJECT Area") (org-cut-subtree)))
      (org-linear-test--sync)
      (should (alist-get 'archived (gethash water org-linear-test--issues)))
      (should (eq t (alist-get 'trashed (gethash garden org-linear-test--projects))))
      (should (eq t (alist-get 'archived (gethash area org-linear-test--initiatives))))
      (should-not (alist-get 'archived (org-linear-test--issue-titled "Keep"))))))

(ert-deftest org-linear-test-hierarchy-linear-rewrite-is-not-an-edit ()
  "Linear's rewrite of a project's description changes no heading."
  (let ((org-linear-test--rewrite-descriptions t))
    (org-linear-test--with-hierarchy
        '(("plan.org" . "* PROJECT Site\nSee https://example.org/a\n- one  \n- two\n** TODO Copy\n"))
      (org-linear-test--sync-until-quiet)
      (should (string-match-p "(<https://example.org/a>)"
                              (alist-get 'content (org-linear-test--named org-linear-test--projects "Site"))))
      (let ((text (org-linear-test--note "plan.org")))
        (org-linear-test--sync)
        (org-linear-test--sync)
        (should-not (org-linear-test--hierarchy-writes))
        (should (equal text (org-linear-test--note "plan.org")))))))

(ert-deftest org-linear-test-hierarchy-preview ()
  "The preview lists the projects, initiatives and milestones to create."
  (org-linear-test--with-hierarchy `(("plan.org" . ,org-linear-test--tree))
    (setq org-linear-test--calls nil)
    (save-window-excursion (org-linear-preview))
    (should-not org-linear-test--calls)
    (with-current-buffer "*org-linear preview*"
      (should (string-match-p "New projects, initiatives and milestones to create: 10" (buffer-string)))
      (should (string-match-p "  Big (initiative and project)" (buffer-string)))
      (should (string-match-p "  Launch (milestone)" (buffer-string))))
    (kill-buffer "*org-linear preview*")))

(ert-deftest org-linear-test-hierarchy-task-heading-becomes-milestone ()
  "A heading that had an issue and is now a milestone archives the issue and gets a milestone."
  (org-linear-test--with-hierarchy
      '(("plan.org" . "* PROJECT Site\n** MILESTONE Launch\nNotes.\n*** TODO Copy\n"))
    (let ((org-linear-project-keywords nil)
          (org-linear-milestone-keywords nil)
          (org-linear-exclude-keywords '("PROJECT")))
      (org-linear-test--sync))
    (let ((launch (alist-get 'id (org-linear-test--issue-titled "Launch"))))
      (should launch)
      (org-linear-test--sync-until-quiet)
      (should (alist-get 'archived (gethash launch org-linear-test--issues)))
      (should (org-linear-test--named org-linear-test--milestones "Launch"))
      (should-not (org-linear-test--property-of "plan.org" "Launch" "LINEAR_UUID"))
      (should (equal (org-linear-test--property-of "plan.org" "Launch" "LINEAR_MILESTONE")
                     (org-linear-test--id-named org-linear-test--milestones "Launch")))
      (should (string-match-p "Notes\\." (org-linear-test--note "plan.org")))
      (should (equal (org-linear-test--relation "Copy" 'projectMilestone)
                     (org-linear-test--id-named org-linear-test--milestones "Launch")))
      (should (string-match-p "is now a milestone heading" (org-linear-test--log)))
      (org-linear-test--sync)
      (should-not (org-linear-test--hierarchy-writes)))))

(ert-deftest org-linear-test-hierarchy-conversions-over-limit-wait ()
  "More headings to convert than `org-linear-archive-limit' convert none."
  (org-linear-test--with-hierarchy
      '(("plan.org" . "* PROJECT Site\n** MILESTONE Launch\n** MILESTONE Review\n"))
    (let ((org-linear-project-keywords nil)
          (org-linear-milestone-keywords nil)
          (org-linear-exclude-keywords '("PROJECT")))
      (org-linear-test--sync))
    (let ((org-linear-archive-limit 1))
      (org-linear-test--sync-until-quiet)
      (should-not (org-linear-test--calls-ever "OrgLinearArchive"))
      (should-not (org-linear-test--calls-ever "OrgLinearMilestoneCreate"))
      (should (org-linear-test--property-of "plan.org" "Launch" "LINEAR_UUID"))
      (should (string-match-p "Not converted: 2 headings" (org-linear-test--log))))))

(ert-deftest org-linear-test-hierarchy-older-state ()
  "A state loaded before projects synced gets their table when they first do."
  (org-linear-test--with-hierarchy '(("plan.org" . "* PROJECT Site\n** TODO Copy\n"))
    (setq org-linear--state (list :tasks (make-hash-table :test #'equal)))
    (org-linear-test--sync-until-quiet)
    (should (org-linear-test--named org-linear-test--projects "Site"))
    (should (hash-table-p (plist-get org-linear--state :objects)))))

;; The scan finds visiting buffers through a table built once.

(ert-deftest org-linear-test-in-place-buffer-visiting-matches-find-buffer-visiting ()
  "The scan's table finds the buffer `find-buffer-visiting' finds."
  (let* ((dir (file-name-as-directory (make-temp-file "org-linear-visiting" t)))
         (a (expand-file-name "a.org" dir))
         (b (expand-file-name "b.org" dir))
         (c (expand-file-name "c.org" dir))
         (link (expand-file-name "link.org" dir))
         buffers)
    (unwind-protect
        (progn
          (dolist (f (list a b c)) (write-region "* TODO x\n" nil f nil 'silent))
          (make-symbolic-link b link)
          (push (find-file-noselect a) buffers)
          (push (find-file-noselect b) buffers)
          (let ((org-linear-in-place--visiting (org-linear-in-place--visiting-table)))
            (dolist (f (list a b c link))
              (should (eq (org-linear-in-place--buffer-visiting f)
                          (find-buffer-visiting f))))
            (should (org-linear-in-place--buffer-visiting link))
            (should-not (org-linear-in-place--buffer-visiting c))
            ;; A buffer killed after the table was built falls back.
            (kill-buffer (car buffers))
            (should (eq (org-linear-in-place--buffer-visiting b)
                        (find-buffer-visiting b)))))
      (dolist (buffer buffers)
        (when (buffer-live-p buffer)
          (with-current-buffer buffer (set-buffer-modified-p nil))
          (kill-buffer buffer)))
      (delete-directory dir t))))

(ert-deftest org-linear-test-in-place-scan-does-not-search-buffers-per-file ()
  "A scan builds the visiting table once instead of searching per file."
  (let ((calls 0)
        (checked 0)
        (dir (make-temp-file "org-linear-scan" t)))
    (unwind-protect
        (let ((org-linear-directories (list dir)))
          (dolist (name '("a.org" "b.org"))
            (with-temp-file (expand-file-name name dir) (insert "* Note\n")))
          (cl-letf (((symbol-function 'org-linear-in-place--scan-file)
                     (lambda (file _mtime _scanner)
                       (cl-incf checked)
                       (should (hash-table-p org-linear-in-place--visiting))
                       (org-linear-in-place--buffer-visiting file)))
                    ((symbol-function 'find-buffer-visiting)
                     (lambda (&rest _) (cl-incf calls) nil))
                    ((symbol-function 'org-linear-in-place--cache)
                     (lambda () (make-hash-table :test #'equal))))
            (org-linear-in-place--scan)
            (should (= checked 2))
            (should (= calls 0))))
      (delete-directory dir t))))

;; A sync interrupted by a quit or a throw does not stay busy.

(ert-deftest org-linear-test-sync-quit-in-start-ends-sync ()
  "A quit while a sync starts clears `org-linear--busy'."
  (let ((org-linear--busy nil) (org-linear--pending nil) (failed nil)
        (org-linear-teams '("AS")))
    (cl-letf (((symbol-function 'org-linear--start) (lambda () (signal 'quit nil)))
              ((symbol-function 'org-linear--fail)
               (lambda (&rest args) (setq failed args org-linear--busy nil))))
      (should (eq (condition-case nil (org-linear--sync nil) (quit 'quit)) 'quit))
      (should-not org-linear--busy)
      (should (equal failed '("interrupted before it finished"))))))

(ert-deftest org-linear-test-sync-throw-in-callback-ends-sync ()
  "A throw out of a process callback ends the sync it belongs to."
  (let ((org-linear--busy t) (failed nil))
    (cl-letf (((symbol-function 'org-linear--fail)
               (lambda (&rest args) (setq failed args org-linear--busy nil))))
      (catch 'away
        (org-linear--ending-sync-on-failure (throw 'away nil)))
      (should-not org-linear--busy)
      (should (equal failed '("interrupted before it finished"))))))

(ert-deftest org-linear-test-sync-error-and-return-in-callback ()
  "An error is reported with its message; a normal return ends nothing."
  (let ((org-linear--busy t) (failed nil))
    (cl-letf (((symbol-function 'org-linear--fail)
               (lambda (&rest args) (setq failed args org-linear--busy nil))))
      (should (eq (org-linear--ending-sync-on-failure 'done) 'done))
      (should org-linear--busy)
      (should-not failed)
      (org-linear--ending-sync-on-failure (error "Boom"))
      (should-not org-linear--busy)
      (should (equal failed '("%s" "Boom"))))))

;;;; Mirror layout: milestones and sub-issues

(defmacro org-linear-test--with-mirror-hierarchy (&rest body)
  "Run BODY with a fresh board whose mirror shows milestones and sub-issues.
The fake workspace has the projects p-a and p-b, a milestone m1 of p-a
named Launch, and issue ENG-2 in that milestone."
  (declare (indent 0))
  `(org-linear-test--with-board
     (let ((org-linear-milestone-keywords '("MILESTONE"))
           (org-linear-file-header
            "#+TITLE: Linear\n#+PRIORITIES: A D D\n#+TODO: TRIAGE(r) BACKLOG(b) TODO(t) DOING(g) WAITING(w@) MILESTONE(m) | DONE(d) CANCELLED(c@)\n"))
       (puthash "p-a" '((id . "p-a") (name . "Project A")) org-linear-test--projects)
       (puthash "p-b" '((id . "p-b") (name . "Project B")) org-linear-test--projects)
       (puthash "m1" `((id . "m1") (name . "Launch") (description . "Ship **it**.")
                       (targetDate . "2026-12-01") (updatedAt . "2026-10-30T09:00:00Z")
                       (project . ((id . "p-a"))))
                org-linear-test--milestones)
       (setf (alist-get 'projectMilestone (gethash "i2" org-linear-test--issues)) '((id . "m1")))
       ,@body)))

(defun org-linear-test--hierarchy-writes ()
  "Return the names of the write operations of the last sync, milestones included."
  (seq-filter (lambda (name) (member name '("OrgLinearUpdate" "OrgLinearComment" "OrgLinearCreate"
                                            "OrgLinearMilestoneCreate" "OrgLinearMilestoneUpdate")))
              (mapcar #'car (org-linear-test--calls))))

(defun org-linear-test--outline ()
  "Return the file's headings, each as its stars and title, in order."
  (with-current-buffer (find-buffer-visiting org-linear-file)
    (org-with-wide-buffer
     (goto-char (point-min))
     (let (headings)
       (while (re-search-forward org-outline-regexp-bol nil t)
         (push (concat (make-string (org-outline-level) ?*) " "
                       (substring-no-properties (org-get-heading t nil t t)))
               headings)
         (end-of-line))
       (nreverse headings)))))

(defun org-linear-test--move-under (identifier regexp level)
  "Move the heading of IDENTIFIER to the end of the heading matching REGEXP.
It lands at LEVEL, as a user's refile would leave it."
  (org-linear-test--goto identifier)
  (let ((text (buffer-substring (point) (progn (org-end-of-subtree t t) (point)))))
    (org-linear-test--goto identifier)
    (delete-region (point) (progn (org-end-of-subtree t t) (point)))
    (goto-char (point-min))
    (re-search-forward regexp)
    (org-end-of-subtree t t)
    (unless (bolp) (insert "\n"))
    (insert (replace-regexp-in-string "\\`\\*+" (make-string level ?*) text))))

(defun org-linear-test--sorted-input (id)
  "Return the input of the last update sent for issue ID, sorted by key."
  (sort (copy-sequence (org-linear-test--input id))
        (lambda (a b) (string< (car a) (car b)))))

(defun org-linear-test--relations (identifier)
  "Return the relations the heading of IDENTIFIER shows."
  (save-excursion
    (org-linear-test--goto identifier)
    (org-linear--tree-relations)))

(ert-deftest org-linear-test-mirror-hierarchy-first-sync-shows-milestones-and-sub-issues ()
  "Milestones become headings, issues go under them and sub-issues under parents."
  (org-linear-test--with-mirror-hierarchy
    (setf (alist-get 'parent (gethash "i6" org-linear-test--issues)) '((id . "i1")))
    (org-linear-test--sync)
    (should (equal (org-linear-test--outline)
                   '("* Project A" "** TODO Task one" "*** TRIAGE Peter's task"
                     "** MILESTONE Launch" "*** WAITING Joint task"
                     "* Project B" "** DONE Closed today")))
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (goto-char (point-min))
      (re-search-forward "^\\*\\* MILESTONE Launch")
      (should (equal (org-entry-get nil "LINEAR_MILESTONE") "m1"))
      (should (equal (org-linear--planning-date "DEADLINE") "2026-12-01"))
      (should (equal (org-linear--body) "Ship *it*.")))
    (should (equal (org-linear-test--relations "ENG-6") '(:project "p-a" :milestone nil :parent "i1")))
    (should (equal (org-linear-test--relations "ENG-2") '(:project "p-a" :milestone "m1" :parent nil)))
    (should-not (org-linear-test--hierarchy-writes))
    (let ((text (org-linear-test--text)))
      (org-linear-test--sync)
      (should-not (org-linear-test--hierarchy-writes))
      (should (equal (org-linear-test--text) text)))))

(ert-deftest org-linear-test-mirror-hierarchy-off-ignores-relations ()
  "Without milestone keywords, sub-issues stay level-2 headings and nothing is sent."
  (org-linear-test--with-board
    (setf (alist-get 'parent (gethash "i6" org-linear-test--issues)) '((id . "i1")))
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-6")
    (should (= (org-outline-level) 2))
    (should-not (org-entry-get nil "LINEAR_PARENT"))
    (should-not (string-match-p "MILESTONE" (org-linear-test--text)))
    (org-linear-test--sync)
    (should-not (org-linear-test--writes))))

(ert-deftest org-linear-test-mirror-hierarchy-moves-sent ()
  "Moving an issue under a milestone or another issue sends just those relations."
  (org-linear-test--with-mirror-hierarchy
    (org-linear-test--sync)
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (org-linear-test--editing
        (org-linear-test--move-under "ENG-6" "^\\*+ WAITING Joint task" 4)
        (org-linear-test--move-under "ENG-1" "^\\*\\* MILESTONE Launch" 3)))
    (org-linear-test--sync)
    (should (equal (org-linear-test--hierarchy-writes) '("OrgLinearUpdate" "OrgLinearUpdate")))
    (should (equal (org-linear-test--sorted-input "i6") '((parentId . "i2") (projectMilestoneId . "m1"))))
    (should (equal (org-linear-test--input "i1") '((projectMilestoneId . "m1"))))
    (let ((i6 (gethash "i6" org-linear-test--issues)))
      (should (equal (alist-get 'parent i6) '((id . "i2"))))
      (should (equal (alist-get 'name (alist-get 'state i6)) "Triage"))
      (should (equal (alist-get 'assignee i6) (org-linear-test--user "u-peter")))
      (should (equal (alist-get 'project i6) '((id . "p-a")))))
    (org-linear-test--sync)
    (should-not (org-linear-test--hierarchy-writes))
    ;; Moving it back out, to the project, clears both.
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (org-linear-test--editing
        (org-linear-test--move-under "ENG-6" "^\\* Project A" 2)
        (goto-char (point-min))
        (re-search-forward "^\\* Project A")
        (org-end-of-subtree t t)))
    (org-linear-test--sync)
    (should (equal (org-linear-test--sorted-input "i6")
                   '((parentId . :null) (projectMilestoneId . :null))))))

(ert-deftest org-linear-test-mirror-hierarchy-linear-changes-move-headings ()
  "Relations changed in Linear move the headings and send nothing."
  (org-linear-test--with-mirror-hierarchy
    (setf (alist-get 'parent (gethash "i6" org-linear-test--issues)) '((id . "i1")))
    (org-linear-test--sync)
    (org-linear-test--edit "i2" 'projectMilestone nil)
    (org-linear-test--edit "i6" 'parent '((id . "i2")))
    (org-linear-test--edit "i1" 'projectMilestone '((id . "m1")))
    (org-linear-test--sync)
    (should-not (org-linear-test--hierarchy-writes))
    (should (equal (org-linear-test--outline)
                   '("* Project A" "** WAITING Joint task" "*** TRIAGE Peter's task"
                     "** MILESTONE Launch" "*** TODO Task one"
                     "* Project B" "** DONE Closed today")))
    (org-linear-test--sync)
    (should-not (org-linear-test--hierarchy-writes))))

(ert-deftest org-linear-test-mirror-hierarchy-parent-move-leaves-children-alone ()
  "A parent moved to another project in Linear does not carry its sub-issue's project.
The sub-issue stays in its project, with its parent in LINEAR_PARENT, and
nothing is sent for it."
  (org-linear-test--with-mirror-hierarchy
    (setf (alist-get 'parent (gethash "i6" org-linear-test--issues)) '((id . "i1")))
    (org-linear-test--sync)
    (org-linear-test--edit "i1" 'project '((id . "p-b")))
    (org-linear-test--sync)
    (should-not (org-linear-test--hierarchy-writes))
    (should (equal (org-linear-test--relations "ENG-6") '(:project "p-a" :milestone nil :parent "i1")))
    (should (equal (org-linear-test--relations "ENG-1") '(:project "p-b" :milestone nil :parent nil)))
    (org-linear-test--goto "ENG-6")
    (should (= (org-outline-level) 2))
    (should (equal (org-entry-get nil "LINEAR_PARENT") "i1"))
    (org-linear-test--sync)
    (should-not (org-linear-test--hierarchy-writes))
    (should (equal (alist-get 'project (gethash "i6" org-linear-test--issues)) '((id . "p-a"))))))

(ert-deftest org-linear-test-mirror-hierarchy-cross-project-parent ()
  "A sub-issue in another project than its parent keeps the parent in a property."
  (org-linear-test--with-mirror-hierarchy
    (puthash "i7" (org-linear-test--issue "i7" "ENG-7" "Elsewhere" :project "p-b")
             org-linear-test--issues)
    (setf (alist-get 'parent (gethash "i7" org-linear-test--issues)) '((id . "i1")))
    (org-linear-test--sync)
    (should-not (org-linear-test--hierarchy-writes))
    (org-linear-test--goto "ENG-7")
    (should (equal (org-entry-get nil "LINEAR_PARENT") "i1"))
    (should (equal (org-linear-test--relations "ENG-7") '(:project "p-b" :milestone nil :parent "i1")))
    ;; Moving it under its parent sends only the project.
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (org-linear-test--editing
        (org-linear-test--move-under "ENG-7" "^\\*+ TODO .*Task one" 3)))
    (org-linear-test--sync)
    (should (equal (org-linear-test--input "i7") '((projectId . "p-a"))))
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-7")
    (should-not (org-entry-get nil "LINEAR_PARENT"))
    (should-not (org-linear-test--hierarchy-writes))))

(ert-deftest org-linear-test-mirror-hierarchy-conflict-keeps-linear ()
  "A relation changed on both sides keeps Linear's and logs yours."
  (org-linear-test--with-mirror-hierarchy
    (org-linear-test--sync)
    (org-linear-test--edit "i6" 'parent '((id . "i2")))
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (org-linear-test--editing
        (org-linear-test--move-under "ENG-6" "^\\*+ TODO .*Task one" 3)))
    (org-linear-test--sync)
    (should-not (assoc "OrgLinearUpdate" (org-linear-test--calls)))
    (should (string-match-p "Conflict in ENG-6 parent" (org-linear-test--log)))
    (should (equal (org-linear-test--relations "ENG-6") '(:project "p-a" :milestone nil :parent "i2")))
    (org-linear-test--goto "ENG-6")
    (should (equal (org-entry-get nil "LINEAR_PARENT") "i2"))))

(ert-deftest org-linear-test-mirror-hierarchy-closed-parent-frees-sub-issues ()
  "A parent that leaves the file leaves its open sub-issue behind, unchanged."
  (org-linear-test--with-mirror-hierarchy
    (let ((org-linear-closed-retention-days 0))
      (setf (alist-get 'parent (gethash "i6" org-linear-test--issues)) '((id . "i1")))
      (org-linear-test--sync)
      (org-linear-test--close-long-ago "i1")
      (org-linear-test--sync)
      (should-not (org-linear-test--hierarchy-writes))
      (should-not (string-match-p "Task one" (org-linear-test--text)))
      (org-linear-test--goto "ENG-6")
      (should (= (org-outline-level) 2))
      (should (equal (org-entry-get nil "LINEAR_PARENT") "i1"))
      (org-linear-test--sync)
      (should-not (org-linear-test--hierarchy-writes)))))

(ert-deftest org-linear-test-mirror-hierarchy-new-milestone-and-issue-created ()
  "A new milestone heading is created in its project, and a new issue under it joins it."
  (org-linear-test--with-mirror-hierarchy
    (org-linear-test--sync)
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (goto-char (point-min))
      (re-search-forward "^\\* Project B$")
      (org-end-of-subtree t t)
      (insert "** MILESTONE Beta\nDEADLINE: <2026-12-20 Sun>\nFirst users.\n"
              "*** TODO Invite testers\n**** TODO Write the invitation\n"))
    (org-linear-test--sync)
    (should (equal (org-linear-test--hierarchy-writes)
                   '("OrgLinearMilestoneCreate" "OrgLinearCreate" "OrgLinearCreate")))
    (let* ((input (alist-get 'input (cdar (org-linear-test--calls "OrgLinearMilestoneCreate"))))
           (id (alist-get 'id input))
           (creates (mapcar (lambda (call) (alist-get 'input (cdr call)))
                            (org-linear-test--calls "OrgLinearCreate"))))
      (should (equal (alist-get 'projectId input) "p-b"))
      (should (equal (alist-get 'name input) "Beta"))
      (should (equal (alist-get 'targetDate input) "2026-12-20"))
      (should (equal (alist-get 'description input) "First users."))
      (should (equal (alist-get 'projectMilestoneId (car creates)) id))
      (should (equal (alist-get 'projectId (car creates)) "p-b"))
      (should (equal (alist-get 'projectMilestoneId (cadr creates)) id))
      (should (equal (alist-get 'parentId (cadr creates)) (alist-get 'id (car creates))))
      (should (string-match-p (format ":LINEAR_MILESTONE: +%s" id) (org-linear-test--text))))
    (org-linear-test--sync)
    (should-not (org-linear-test--hierarchy-writes))))

(ert-deftest org-linear-test-mirror-hierarchy-issue-moved-under-new-milestone ()
  "An issue moved under a milestone heading not yet in Linear joins it once it exists."
  (org-linear-test--with-mirror-hierarchy
    (org-linear-test--sync)
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (org-linear-test--editing
        (goto-char (point-min))
        (re-search-forward "^\\* Project A$")
        (org-end-of-subtree t t)
        (insert "** MILESTONE Later\n")
        (org-linear-test--move-under "ENG-1" "^\\*\\* MILESTONE Later" 3)))
    (org-linear-test--sync)
    (should (member "OrgLinearMilestoneCreate" (org-linear-test--hierarchy-writes)))
    (should-not (org-linear-test--input "i1"))
    ;; The sync runs again by itself; here it is run by hand.
    (org-linear-test--sync)
    (let ((milestone (alist-get 'id (alist-get 'input (cdar (org-linear-test--calls-ever
                                                            "OrgLinearMilestoneCreate"))))))
      (should (equal (org-linear-test--input "i1") `((projectMilestoneId . ,milestone)))))))

(ert-deftest org-linear-test-mirror-hierarchy-milestone-fields-merge ()
  "A milestone renamed in Linear is renamed here; its date edited here is sent."
  (org-linear-test--with-mirror-hierarchy
    (org-linear-test--sync)
    (org-linear-test--put-object org-linear-test--milestones "m1" nil '((name . "Public launch")))
    (with-current-buffer (find-buffer-visiting org-linear-file)
      (org-linear-test--editing
        (goto-char (point-min))
        (re-search-forward "^\\*\\* MILESTONE Launch")
        (org-linear--set-planning 'deadline "2026-12-15")))
    (org-linear-test--sync)
    (should (string-match-p "^\\*\\* MILESTONE Public launch" (org-linear-test--text)))
    (let ((update (cdar (org-linear-test--calls "OrgLinearMilestoneUpdate"))))
      (should (equal (alist-get 'input update) '((targetDate . "2026-12-15")))))
    (should (equal (alist-get 'name (gethash "m1" org-linear-test--milestones)) "Public launch"))
    (org-linear-test--sync)
    (should-not (org-linear-test--hierarchy-writes))))

(ert-deftest org-linear-test-mirror-hierarchy-milestone-deleted-in-linear ()
  "A milestone deleted in Linear leaves the file, and its issues go to the project."
  (org-linear-test--with-mirror-hierarchy
    (org-linear-test--sync)
    (org-linear-test--answer-object "OrgLinearMilestoneArchive" '((id . "m1")))
    (org-linear-test--edit "i2" 'projectMilestone nil)
    (org-linear-test--sync)
    (should-not (org-linear-test--hierarchy-writes))
    (should-not (string-match-p "Launch" (org-linear-test--text)))
    (should (equal (org-linear-test--relations "ENG-2") '(:project "p-a" :milestone nil :parent nil)))))

(ert-deftest org-linear-test-mirror-hierarchy-edit-during-read-applies-nothing ()
  "An edit to the file while the sync reads it stops the sync from applying anything."
  (org-linear-test--with-mirror-hierarchy
    (org-linear-test--sync)
    (org-linear-test--edit "i1" 'title "Renamed in Linear")
    (let ((read (symbol-function 'org-linear--read-task))
          (typed nil))
      (cl-letf (((symbol-function 'org-linear--read-task)
                 (lambda (&rest args)
                   (unless typed
                     (setq typed t)
                     (with-current-buffer (find-buffer-visiting org-linear-file)
                       (goto-char (point-max))
                       (insert "typing")))
                   (apply read args))))
        (org-linear-test--sync)))
    (should (string-match-p "changed while the sync read it" (org-linear-test--log)))
    (should-not (string-match-p "Renamed in Linear" (org-linear-test--text)))
    (should-not (org-linear-test--hierarchy-writes))))

(ert-deftest org-linear-test-mirror-hierarchy-set-project-keeps-parent ()
  "Moving a sub-issue to another project keeps its parent, in LINEAR_PARENT."
  (org-linear-test--with-mirror-hierarchy
    (setf (alist-get 'parent (gethash "i6" org-linear-test--issues)) '((id . "i1")))
    (org-linear-test--sync)
    (org-linear-test--goto "ENG-6")
    (let ((org-linear--saving t))
      (org-linear--move-and-save (point-marker) "p-b"))
    (should (equal (org-linear-test--relations "ENG-6") '(:project "p-b" :milestone nil :parent "i1")))
    (org-linear-test--sync)
    (should (equal (org-linear-test--input "i6") '((projectId . "p-b"))))))

(ert-deftest org-linear-test-mirror-hierarchy-shift-levels ()
  "Shifting a subtree's levels moves offsets past each changed heading."
  (should (equal (org-linear--shift-levels "** A\nx\n*** B\n" 1 '((m . 7)))
                 '("*** A\nx\n**** B\n" (m . 8))))
  (should (equal (org-linear--shift-levels "*** A\n**** B\n" -1 '((m . 6)))
                 '("** A\n*** B\n" (m . 5)))))

(ert-deftest org-linear-test-read-in-slices-indexes-a-narrowed-buffer-from-its-start ()
  "A sync that starts while the mirror is narrowed still sees every issue.
The index used to start at the narrowed region, so each issue above it
was taken for missing and inserted a second time (17 Nov 2026)."
  (with-temp-buffer
    (org-mode)
    (insert "* Project\n:PROPERTIES:\n:LINEAR_PROJECT: p1\n:END:\n"
            "** TODO Above\n:PROPERTIES:\n:LINEAR_UUID: u-above\n:END:\n"
            "** TODO Inside\n:PROPERTIES:\n:LINEAR_UUID: u-inside\n:END:\nBody.\n")
    (set-buffer-modified-p nil)
    (goto-char (point-min))
    (search-forward "** TODO Inside")
    (org-narrow-to-subtree)
    (let (index)
      (cl-letf (((symbol-function 'org-linear-in-place--run-slices)
                 (lambda (step done)
                   (while (funcall step))
                   (funcall done))))
        (org-linear--read-in-slices (current-buffer) nil (make-hash-table :test #'equal)
                                    (lambda (found &rest _) (setq index found))))
      (should index)
      (should (gethash "u-above" (plist-get index :tasks)))
      (should (gethash "u-inside" (plist-get index :tasks)))
      (should (gethash "p1" (plist-get index :projects)))
      (should (buffer-narrowed-p)))))

;;;; Audit of 17 Nov 2026

(defconst org-linear-test--numeric-header
  "#+TITLE: Linear\n#+PRIORITIES: 1 9 5\n#+TODO: TRIAGE(r) BACKLOG(b) TODO(t) DOING(g) WAITING(w@) | DONE(d) CANCELLED(c@)\n"
  "A header for a mirrored file whose priorities are the numbers 1 to 9.")

(ert-deftest org-linear-test-mirror-cookie-of-same-priority-kept ()
  "In the mirror layout too, a cookie sent as another's priority is not replaced.
A heading another package wrote with `[#9]' was sent as priority 4 and
came back as `[#6]', the cookie that names that priority (17 Nov 2026)."
  (org-linear-test--with-board
    (let ((org-linear-priorities '((1 . "1") (2 . "2") (3 . "4") (4 . "6")))
          (org-linear-file-header org-linear-test--numeric-header))
      (org-linear-test--sync)
      (with-current-buffer (find-buffer-visiting org-linear-file)
        (goto-char (point-min))
        (re-search-forward "^\\* Project A")
        (org-end-of-subtree t t)
        (insert "** TODO [#9] Stranger's task\n")
        (org-linear-test--goto "ENG-1")
        (org-linear--set-priority "3")
        (save-buffer))
      (org-linear-test--sync)
      (should (equal (alist-get 'priority (org-linear-test--issue-titled "Stranger's task")) 4))
      (should (equal (alist-get 'priority (gethash "i1" org-linear-test--issues)) 2))
      (org-linear-test--sync)
      (org-linear-test--sync)
      (should-not (org-linear-test--writes))
      (let ((text (org-linear-test--text)))
        (should (string-match-p "^\\*\\* TODO \\[#9\\] Stranger's task" text))
        (should (string-match-p "^\\*\\* TODO \\[#3\\] Task one" text)))
      ;; A priority changed in Linear still replaces the cookie.
      (org-linear-test--edit "i1" 'priority 1)
      (org-linear-test--sync)
      (should (string-match-p "^\\*\\* TODO \\[#1\\] Task one" (org-linear-test--text))))))

(defun org-linear-test--closed-stamp (closed-at)
  "Return the CLOSED timestamp expected for CLOSED-AT, a time as Linear gives it."
  (format-time-string "[%Y-%m-%d %a %H:%M]" (date-to-time closed-at)))

(ert-deftest org-linear-test-closed-time-is-linears ()
  "An issue closed in Linear is closed in the file at Linear's own time.
The CLOSED line said noon of that day, or the day alone on a heading
that entered the file closed, so nothing could tell a message that came
after the closing from one that came before it (17 Nov 2026)."
  (org-linear-test--with-board
    (let ((org-linear-closed-retention-days 100000)
          (closed-at "2026-11-10T15:23:45.000Z"))
      (org-linear-test--sync)
      (org-linear-test--edit "i1" 'state (org-linear-test--state "s-done"))
      (org-linear-test--edit "i1" 'completedAt closed-at)
      (org-linear-test--sync)
      (org-linear-test--goto "ENG-1")
      (should (equal (org-get-todo-state) "DONE"))
      (should (equal (org-entry-get nil "CLOSED") (org-linear-test--closed-stamp closed-at)))
      ;; A heading that enters the file closed.
      (org-linear-test--goto "ENG-4")
      (should (equal (org-entry-get nil "CLOSED")
                     (org-linear-test--closed-stamp "2026-01-01T10:00:00Z"))))))

(ert-deftest org-linear-test-in-place-closed-time-is-linears ()
  "In place too, an issue closed in Linear closes its heading at Linear's time."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist")))
          (closed-at "2026-11-10T15:23:45.000Z"))
      (org-linear-test--edit id 'state (org-linear-test--state "s-done"))
      (org-linear-test--edit id 'completedAt closed-at)
      (org-linear-test--sync)
      (should (string-match-p
               (regexp-quote (concat "** DONE Book dentist\nCLOSED: "
                                     (org-linear-test--closed-stamp closed-at)))
               (org-linear-test--note "health.org"))))))

(defvar org-agenda-finalize-hook)

(ert-deftest org-linear-test-my-issues-runs-the-global-finalize-hook ()
  "The list runs what other packages put on `org-agenda-finalize-hook'.
It replaced the hook with its own function, so another package's marks
on the list's lines were gone after each rebuild (17 Nov 2026)."
  (require 'org-agenda)
  (org-linear-test--with-board
    (org-linear-test--sync)
    (let* ((runs 0)
           (set-up nil)
           (other (lambda ()
                    (cl-incf runs)
                    (setq set-up (bound-and-true-p org-linear-my-issues-mode))))
           (org-agenda-finalize-hook (list other)))
      (unwind-protect
          (progn
            (org-linear-my-issues)
            (should (= runs 1))
            ;; The list is set up before the others decorate it.
            (should set-up)
            (with-current-buffer org-agenda-buffer-name
              (org-agenda-redo t)
              (should (= runs 2))
              (should org-linear-my-issues-mode))
            (should (equal org-agenda-finalize-hook (list other))))
        (when (get-buffer org-agenda-buffer-name)
          (kill-buffer org-agenda-buffer-name))))))

(ert-deftest org-linear-test-loading-again-sets-the-keys-again ()
  "Loading the package again gives its keymaps the keys of the code loaded.
A `defvar' keeps the keymap the first load built, so after an update the
list kept its old keys until Emacs was restarted (17 Nov 2026)."
  (let ((list-map org-linear-my-issues-mode-map)
        (todo (lookup-key org-linear-my-issues-mode-map "t"))
        (refile (lookup-key org-linear-file-mode-map [remap org-refile])))
    (should (eq todo #'org-linear-my-issues-todo))
    (should (eq refile #'org-linear-set-project))
    (unwind-protect
        (progn
          ;; As the keymaps of a version loaded earlier held them, and with
          ;; a key of the user's own.
          (define-key org-linear-my-issues-mode-map "t" #'ignore)
          (define-key org-linear-file-mode-map [remap org-refile] #'ignore)
          (define-key org-linear-my-issues-mode-map "!" #'ignore)
          (load (locate-library "org-linear") nil t)
          (should (eq (lookup-key org-linear-my-issues-mode-map "t") todo))
          (should (eq (lookup-key org-linear-file-mode-map [remap org-refile]) refile))
          ;; The keymap itself stays, since `minor-mode-map-alist' holds it,
          ;; and so do the keys the package does not set.
          (should (eq org-linear-my-issues-mode-map list-map))
          (should (eq (cdr (assq 'org-linear-my-issues-mode minor-mode-map-alist)) list-map))
          (should (eq (lookup-key org-linear-my-issues-mode-map "!") #'ignore)))
      (define-key org-linear-my-issues-mode-map "t" todo)
      (define-key org-linear-file-mode-map [remap org-refile] refile)
      (define-key org-linear-my-issues-mode-map "!" nil t))))

(ert-deftest org-linear-test-in-place-narrowed-buffer-keeps-its-issues ()
  "A visited file narrowed to a subtree still counts with all its issues.
The scan cache read only the narrowed part of the buffer, so the issues
of the headings outside it looked gone from Org and were archived in
Linear (17 Nov 2026)."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist")))
          (buffer (find-file-noselect (expand-file-name "health.org" org-linear-test--notes))))
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "^\\*\\* Plain heading")
        (org-narrow-to-subtree))
      ;; The sync edits and saves the buffer, then records what its file holds.
      (org-linear-test--edit id 'title "Book the dentist")
      (org-linear-test--sync)
      (should (string-match-p "TODO Book the dentist" (org-linear-test--note "health.org")))
      (org-linear-test--sync)
      (should-not (org-linear-test--calls "OrgLinearArchive"))
      (should-not (alist-get 'archived (gethash id org-linear-test--issues)))
      ;; With unsaved edits, the scan reads the buffer instead of the file.
      (with-current-buffer buffer
        (should (buffer-narrowed-p))
        (goto-char (point-max))
        (insert "Mine.\n"))
      (org-linear-test--sync)
      (should-not (org-linear-test--calls "OrgLinearArchive"))
      (should-not (alist-get 'archived (gethash id org-linear-test--issues)))
      (with-current-buffer buffer
        (should (buffer-narrowed-p))))))

(defun org-linear-test--reading-buffers ()
  "Return the live buffers the in-place layout read a file in without visiting it."
  (seq-filter (lambda (buffer) (local-variable-p 'org-linear--file buffer)) (buffer-list)))

(defun org-linear-test--close-notes ()
  "Kill the buffers visiting the note files, as you do when done with them."
  (dolist (buffer (buffer-list))
    (when (and (buffer-file-name buffer)
               (string-prefix-p org-linear-test--notes (buffer-file-name buffer)))
      (kill-buffer buffer))))

(defun org-linear-test--sync-to-its-end ()
  "Run a sync that may fail, and wait until it ends."
  (org-linear-sync)
  (let ((deadline (+ (float-time) 30)))
    (while (and org-linear--busy (< (float-time) deadline))
      (accept-process-output nil 0.05)))
  (should-not org-linear--busy))

(ert-deftest org-linear-test-in-place-edits-in-visiting-buffers-it-leaves-open ()
  "The sync edits a file nobody visits in a buffer your hooks set up, and leaves it.
It visited such a file without hooks and killed the buffer when done, so
whatever had come to point into it meanwhile, as an agenda built during
the sync does, was left pointing nowhere (17 Nov 2026)."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (let* ((file (expand-file-name "health.org" org-linear-test--notes))
           (set-up nil)
           (killed nil)
           (org-mode-hook (list (lambda () (when buffer-file-name (push buffer-file-name set-up)))))
           (kill-buffer-hook (list (lambda () (when buffer-file-name (push buffer-file-name killed))))))
      (org-linear-test--sync)
      (let ((buffer (find-buffer-visiting file)))
        (should buffer)
        (should (member file set-up))
        (should-not killed)
        (should-not (buffer-modified-p buffer))
        (should (equal (with-current-buffer buffer
                         (buffer-substring-no-properties (point-min) (point-max)))
                       (org-linear-test--note "health.org")))
        (should (org-linear-test--uuid-of "health.org" "Book dentist")))
      (should-not (org-linear-test--reading-buffers)))))

(ert-deftest org-linear-test-in-place-reads-without-visiting ()
  "The sync reads a file nobody visits in a buffer that visits nothing.
A buffer visiting it, made without your hooks, was there for anything
that looked the file up while the sync ran (17 Nov 2026)."
  (org-linear-test--with-notes '(("work.org" . "* TODO Write report\n"))
    (org-linear-test--sync)
    (org-linear-test--close-notes)
    (let ((file (expand-file-name "work.org" org-linear-test--notes))
          (id (alist-get 'id (org-linear-test--issue-titled "Write report")))
          (visiting 'not-asked))
      ;; An edit made elsewhere: the sync has to read it and send it, and
      ;; has nothing to write to the file.
      (org-linear-test--write-note
       "work.org" (replace-regexp-in-string "Write report" "Write the report"
                                            (org-linear-test--note "work.org")))
      (cl-letf (((symbol-function 'org-linear-in-place--fetch)
                 (let ((fetch (symbol-function 'org-linear-in-place--fetch)))
                   (lambda (callback)
                     ;; By now the scan has read the file.
                     (setq visiting (find-buffer-visiting file))
                     (funcall fetch callback)))))
        (org-linear-test--sync))
      (should-not visiting)
      (should (equal (alist-get 'title (gethash id org-linear-test--issues)) "Write the report"))
      (should-not (find-buffer-visiting file))
      (should-not (org-linear-test--reading-buffers)))))

(ert-deftest org-linear-test-in-place-file-visited-during-sync-is-edited-there ()
  "A file you visit while the sync runs takes the sync's edits in your buffer.
The buffer was the sync's own, which it killed when done (17 Nov 2026)."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (org-linear-test--close-notes)
    (let ((file (expand-file-name "health.org" org-linear-test--notes))
          (id (alist-get 'id (org-linear-test--issue-titled "Book dentist")))
          buffer)
      (org-linear-test--edit id 'title "Changed in Linear")
      (cl-letf (((symbol-function 'org-linear-in-place--ensure-labels)
                 (let ((ensure (symbol-function 'org-linear-in-place--ensure-labels)))
                   (lambda (callback)
                     ;; By now the sync has read the file of the changed issue.
                     (setq buffer (find-file-noselect file))
                     (funcall ensure callback)))))
        (org-linear-test--sync))
      (should (buffer-live-p buffer))
      (should (eq buffer (find-buffer-visiting file)))
      (should-not (buffer-modified-p buffer))
      (should (string-match-p "TODO Changed in Linear" (with-current-buffer buffer (buffer-string))))
      (should (string-match-p "TODO Changed in Linear" (org-linear-test--note "health.org")))
      (should-not (org-linear-test--reading-buffers)))))

(ert-deftest org-linear-test-in-place-file-edited-by-you-during-sync-left-out ()
  "A file you visit and edit while the sync runs is left out of that sync."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (org-linear-test--close-notes)
    (let ((file (expand-file-name "health.org" org-linear-test--notes))
          (id (alist-get 'id (org-linear-test--issue-titled "Book dentist")))
          buffer)
      (org-linear-test--edit id 'title "Changed in Linear")
      (cl-letf (((symbol-function 'org-linear-in-place--ensure-labels)
                 (let ((ensure (symbol-function 'org-linear-in-place--ensure-labels)))
                   (lambda (callback)
                     (setq buffer (find-file-noselect file))
                     (with-current-buffer buffer
                       (goto-char (point-max))
                       (insert "Mine.\n"))
                     (funcall ensure callback)))))
        (org-linear-test--sync))
      (should (string-match-p "changed during the sync" (org-linear-test--log)))
      (with-current-buffer buffer
        (should (buffer-modified-p))
        (should (string-match-p "Mine\\.\n\\'" (buffer-string)))
        (should-not (string-match-p "Changed in Linear" (buffer-string)))
        (save-buffer))
      (should-not (org-linear-test--reading-buffers))
      (org-linear-test--sync)
      (should (string-match-p "TODO Changed in Linear" (org-linear-test--note "health.org")))
      (should (equal (alist-get 'title (gethash id org-linear-test--issues)) "Changed in Linear")))))

(ert-deftest org-linear-test-in-place-file-changed-under-the-sync-keeps-nothing ()
  "A file that changes while the sync holds edits to it makes the sync keep nothing.
The edits cannot reach the file, and a record saying they did would make
the next sync send Linear its old values back."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (org-linear-test--sync)
    (org-linear-test--close-notes)
    (let ((id (alist-get 'id (org-linear-test--issue-titled "Book dentist")))
          (written nil))
      (org-linear-test--edit id 'title "Changed in Linear")
      (cl-letf (((symbol-function 'org-linear-in-place--visit-edited)
                 (let ((visit (symbol-function 'org-linear-in-place--visit-edited)))
                   (lambda ()
                     (when (and (not written)
                                (seq-some #'buffer-modified-p (org-linear-test--reading-buffers)))
                       (setq written t)
                       (org-linear-test--write-note
                        "health.org" (concat (org-linear-test--note "health.org")
                                             "** TODO Added elsewhere\n")))
                     (funcall visit)))))
        (org-linear-test--sync-to-its-end))
      (should written)
      (should (string-match-p "changed while the sync edited it" (org-linear-test--log)))
      (should (string-match-p "Added elsewhere" (org-linear-test--note "health.org")))
      (should-not (string-match-p "Changed in Linear" (org-linear-test--note "health.org")))
      (should-not (org-linear-test--reading-buffers))
      (org-linear-test--sync)
      (should (string-match-p "TODO Changed in Linear" (org-linear-test--note "health.org")))
      (should (equal (alist-get 'title (gethash id org-linear-test--issues)) "Changed in Linear"))
      (should (org-linear-test--issue-titled "Added elsewhere")))))

(ert-deftest org-linear-test-in-place-failed-sync-leaves-no-buffer-of-its-own ()
  "A sync that fails leaves behind no buffer it made to read a file.
It left the file visited, without your hooks (17 Nov 2026)."
  (org-linear-test--with-notes `(("health.org" . ,org-linear-test--health))
    (setq org-linear-test--fail-next "OrgLinearIssues")
    (org-linear-test--sync-to-its-end)
    (should (string-match-p "Error" (org-linear-test--log)))
    (should-not (find-buffer-visiting (expand-file-name "health.org" org-linear-test--notes)))
    (should-not (org-linear-test--reading-buffers))))

(ert-deftest org-linear-test-in-place-inbox-issue-linked-to-its-own-heading ()
  "An issue added to the inbox is linked to its own heading, whatever follows it.
Its properties went to the heading after the inbox's, when there was
one, and the next sync sent that heading to Linear as the issue
\(17 Nov 2026)."
  (org-linear-test--with-notes '(("inbox.org" . "* Linear\n* Last\nMine.\n"))
    (org-linear-test--sync)
    (should (equal (org-linear-test--uuid-of "inbox.org" "Task one") "i1"))
    (should (equal (org-linear-test--uuid-of "inbox.org" "Peter's task") "i6"))
    (should-not (org-linear-test--uuid-of "inbox.org" "Last"))
    (should (string-match-p "^\\* Last\nMine\\.\n\\'" (org-linear-test--note "inbox.org")))
    (org-linear-test--sync)
    (should-not (org-linear-test--writes))
    (should (equal (alist-get 'title (gethash "i6" org-linear-test--issues)) "Peter's task"))))

(ert-deftest org-linear-test-in-place-narrowed-inbox-takes-issues-under-its-heading ()
  "An inbox narrowed to a subtree takes new issues under its inbox heading.
The inbox is read while the sync applies when it had unsaved edits as
the sync began and was saved meanwhile.  It was then searched as
narrowed: its heading was not found, and a second one was added at the
end of the subtree (17 Nov 2026)."
  (org-linear-test--with-notes
      '(("inbox.org" . "#+TODO: TODO IMPORTANT WAITING | DONE CANCELLED\n* Other\nMine.\n* Linear\n"))
    (let ((buffer (find-file-noselect org-linear-inbox-file)))
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "^\\* Other")
        (org-narrow-to-subtree)
        (goto-char (point-max))
        (insert "\nMore."))
      (cl-letf (((symbol-function 'org-linear-in-place--ensure-labels)
                 (let ((ensure (symbol-function 'org-linear-in-place--ensure-labels)))
                   (lambda (callback)
                     ;; Saved once the sync is past the files it reads
                     ;; before it applies.
                     (with-current-buffer buffer (save-buffer))
                     (funcall ensure callback)))))
        (org-linear-test--sync))
      (let ((inbox (org-linear-test--note "inbox.org")))
        (should (string-match-p
                 "^\\* Other\nMine\\.\nMore\\.\n\\* Linear\n\\*\\* TODO \\[#B\\] Task one"
                 inbox))
        (should (= 1 (with-temp-buffer
                       (insert inbox)
                       (how-many "^\\* Linear$" (point-min) (point-max))))))
      (with-current-buffer buffer
        (should (buffer-narrowed-p))
        (should (equal (buffer-substring-no-properties (point-min) (point-max))
                       "* Other\nMine.\nMore."))))))

(provide 'org-linear-test)
;;; org-linear-test.el ends here
