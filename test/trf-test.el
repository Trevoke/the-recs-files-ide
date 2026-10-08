;;; trf-test.el --- Tests for trf -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'e-unit)
(require 'trf-test-helper)
(require 'trf)
(require 'trf-emacs)

(defvar trf-test-root nil)

(defconst trf-test-config '((:identifier . fake)))

(defconst trf-test-machine-buffer-name "*trf fake machine*")

(defconst trf-test-record-change '("docs/features/f-1-order.org" . "* F-1 Order\n"))

(setup-each ()
  (setq trf-test-root (trf-test-make-target-product))
  (trf-test-git trf-test-root "update-ref" "refs/recs/implemented" "HEAD"))

(teardown-each ()
  (dolist (buffer (buffer-list))
    (when (and (buffer-file-name buffer)
               (string-prefix-p trf-test-root (buffer-file-name buffer)))
      (kill-buffer buffer)))
  (dolist (name (list trf--record-changes-buffer-name trf-test-machine-buffer-name))
    (when (get-buffer name)
      (kill-buffer name)))
  (delete-directory trf-test-root t))

(deftest trf-test-version-is-set ()
  "trf declares its version."
  (assert-equal "0.1.0" trf-version))

(defmacro trf-test-with-machines (runs finishing &rest body)
  "Run BODY with each machine finishing; RUNS collects (name directory config instructions).
The brainstorming and planning machines produce docs/plans/<slug>.md; the reconciliation
assessor writes the record implementation report.
FINISHING is `now' to finish each machine at once, `later' to finish it from a timer,
or the name of the machine that stops instead of finishing."
  (let ((timers (make-symbol "timers")))
    `(let ((,timers nil))
       (unwind-protect
           (cl-letf (((symbol-function 'trf-session-config) (lambda () trf-test-config))
                     ((symbol-function 'trf-session-run)
                      (lambda (directory config instructions on-finished on-stopped)
                        (let ((name (progn (string-match "\\`You are the \\([^.]+\\)\\." instructions)
                                           (match-string 1 instructions))))
                          (setq ,runs (append ,runs (list (list name directory config
                                                                instructions))))
                          (if (equal ,finishing name)
                              (funcall on-stopped)
                            (let ((produced
                                   (when (member name '("brainstorming machine" "planning machine"))
                                     (list (format "docs/plans/%s.md" (car (split-string name)))))))
                              (when (equal name "reconciliation assessor")
                                (with-temp-file (expand-file-name
                                                 (trf-machines-record-implementation-report-file)
                                                 directory)
                                  (insert "Assessed by the reconciliation assessor.")))
                              (if (eq ,finishing 'later)
                                  (push (run-at-time 0 nil on-finished produced) ,timers)
                                (funcall on-finished produced)))))
                        (get-buffer-create trf-test-machine-buffer-name)))
                     ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
             ,@body)
         (mapc #'cancel-timer ,timers)))))

(defun trf-test-settle (done)
  "Wait until DONE returns non-nil; signal an error after two seconds."
  (with-timeout (2 (error "Timed out waiting for %S" done))
    (while (not (funcall done))
      (accept-process-output nil 0.01))))

(defun trf-test-report-shown-p ()
  (when-let* ((file (buffer-file-name (window-buffer (selected-window)))))
    (string-suffix-p "/.trf/record-implementation-report.org" file)))

(defun trf-test-implement (&optional directory)
  "Ask the IDE to implement the record changes from DIRECTORY; return what it signals.
DIRECTORY defaults to `trf-test-root'."
  (let ((default-directory (or directory trf-test-root)))
    (condition-case err
        (progn (trf-implement-record-changes) nil)
      (error err))))

(defun trf-test-assert-implement-refuses (refusal &optional implement)
  "Assert IMPLEMENT gives REFUSAL, asking nothing and changing nothing.
The target product's head, status and worktrees stay as they were.
IMPLEMENT defaults to `trf-test-implement'."
  (let ((head (trf-test-head trf-test-root))
        (status (trf-test-git trf-test-root "status" "--porcelain"))
        (worktrees (trf-test-git trf-test-root "worktree" "list" "--porcelain")))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked to confirm")))
              ((symbol-function 'trf-session-config) (lambda () (error "Asked for a config"))))
      (assert-equal refusal (funcall (or implement #'trf-test-implement))))
    (assert-equal head (trf-test-head trf-test-root))
    (assert-equal status (trf-test-git trf-test-root "status" "--porcelain"))
    (assert-equal worktrees (trf-test-git trf-test-root "worktree" "list" "--porcelain"))))

(deftest trf-implement-without-record-changes ()
  "With no record changes the IDE gives MSG-1 and does nothing else."
  ;; F-1 §Rules: Ask the IDE to implement the record changes
  ;; UW-1 §Minimal guarantee: Turn record changes into code
  ;; MSG-1: No record changes to implement
  (trf-test-assert-implement-refuses '(user-error "There are no record changes to implement.")))

(deftest trf-implement-mid-merge-without-record-changes ()
  "Mid merge, the IDE gives MSG-3 even when there are no record changes."
  (trf-test-leave-in-progress trf-test-root 'merge)
  ;; UW-1 §Extensions: Turn record changes into code
  ;; MSG-3: Finish the merge or rebase first
  (trf-test-assert-implement-refuses
   '(user-error
     "The target product is in the middle of a merge or rebase; finish it first.")))

(deftest trf-implement-mid-merge-with-record-changes ()
  "Mid merge, the IDE gives MSG-3 and asks nothing."
  (trf-test-leave-in-progress trf-test-root 'merge)
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  ;; UW-1 §Extensions: Turn record changes into code
  ;; MSG-3: Finish the merge or rebase first
  (trf-test-assert-implement-refuses
   '(user-error
     "The target product is in the middle of a merge or rebase; finish it first.")))

(deftest trf-implement-shows-the-record-changes ()
  "The record changes are shown before the record keeper is asked to confirm."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let (shown)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _)
                 (setq shown (with-current-buffer trf--record-changes-buffer-name
                              (buffer-string)))
                 nil)))
      (trf-test-implement))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal "- F-1 Order (added): docs/features/f-1-order.org\n" shown)))

(deftest trf-implement-declined ()
  "Declining to confirm leaves the target product as it was."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let ((head (trf-test-head trf-test-root))
        (status (trf-test-git trf-test-root "status" "--porcelain")))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
              ((symbol-function 'trf-session-config) (lambda () (error "Asked for a config")))
              ((symbol-function 'trf-session-run) (lambda (&rest _) (error "Ran a machine"))))
      (assert-nil (trf-test-implement)))
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-equal head (trf-test-head trf-test-root))
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-equal status (trf-test-git trf-test-root "status" "--porcelain"))
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-nil (file-exists-p (expand-file-name ".worktrees" trf-test-root)))))

(deftest trf-implement-commits-the-record-changes ()
  "Confirming commits the record changes, and the machines run in a worktree from that commit."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let (runs)
    (trf-test-with-machines runs 'now
      (assert-nil (trf-test-implement)))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal "Record changes\n\n- F-1 Order\n"
                  (trf-test-git trf-test-root "log" "-1" "--format=format:%B"))
    (let ((directory (nth 1 (car runs))))
      ;; UW-1 §Main success scenario: Turn record changes into code
      (assert-true (string-prefix-p (expand-file-name ".worktrees/" trf-test-root) directory))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal (trf-test-head trf-test-root)
                    (string-trim (trf-test-git directory "rev-parse" "HEAD"))))))

(deftest trf-implement-runs-the-machines-in-turn ()
  "The implementation workflow's machines run in turn, each given what the one before produced, then the reconciliation assessor."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let (runs)
    (trf-test-with-machines runs 'now
      (trf-test-implement))
    ;; F-1 §Produces: Ask the IDE to implement the record changes
    (assert-equal '("brainstorming machine" "planning machine" "executing machine"
                    "reconciliation assessor")
                  (mapcar #'car runs))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-match "^- docs/plans/brainstorming\\.md$" (nth 3 (nth 1 runs)))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-match "^- docs/plans/planning\\.md$" (nth 3 (nth 2 runs)))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-equal 1 (length (delete-dups (mapcar (lambda (run) (nth 1 run)) runs))))))

(deftest trf-implement-resolves-the-ai-program-once ()
  "Every machine runs with the one AI program config, asked for once."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let (runs (asked 0))
    (trf-test-with-machines runs 'now
      (cl-letf (((symbol-function 'trf-session-config)
                 (lambda () (cl-incf asked) trf-test-config)))
        (trf-test-implement)))
    (assert-equal 1 asked)
    (assert-equal (make-list 4 trf-test-config) (mapcar (lambda (run) (nth 2 run)) runs))))

(deftest trf-implement-opens-the-report ()
  "The record implementation report opens once the reconciliation assessor finishes."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let (runs)
    (trf-test-with-machines runs 'now
      (trf-test-implement))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-true (trf-test-report-shown-p))))

(deftest trf-implement-machines-finishing-later ()
  "Machines that finish after the command returns still run in turn, and the report opens."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let (runs)
    (trf-test-with-machines runs 'later
      (trf-test-implement)
      (assert-equal '("brainstorming machine") (mapcar #'car runs))
      (trf-test-settle #'trf-test-report-shown-p))
    ;; F-1 §Produces: Ask the IDE to implement the record changes
    (assert-equal '("brainstorming machine" "planning machine" "executing machine"
                    "reconciliation assessor")
                  (mapcar #'car runs))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-true (trf-test-report-shown-p))))

(deftest trf-implement-machine-stops ()
  "When a machine stops, no machine after it runs and no report opens."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let (runs)
    (trf-test-with-machines runs "planning machine"
      (assert-nil (trf-test-implement)))
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-equal '("brainstorming machine" "planning machine") (mapcar #'car runs))
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-nil (trf-test-report-shown-p))))

(deftest trf-implement-refused-commit ()
  "When the record changes cannot be committed, MSG-4 is given and no machine runs."
  (trf-test-write-files trf-test-root
                        (list trf-test-record-change
                              '(".git/hooks/pre-commit"
                                . "#!/bin/sh\necho pre-commit refuses >&2\nexit 1\n")))
  (set-file-modes (expand-file-name ".git/hooks/pre-commit" trf-test-root) #o755)
  (let (runs refusal)
    (trf-test-with-machines runs 'now
      (setq refusal (trf-test-implement)))
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-4: Record changes could not be committed
    (assert-equal '(user-error "The record changes could not be committed:\npre-commit refuses")
                  refusal)
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-nil runs)
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-nil (file-exists-p (expand-file-name ".worktrees" trf-test-root)))))

(deftest trf-implement-record-changes-already-committed ()
  "Record changes already committed are not committed again; the machines start from the latest commit."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (trf-test-git trf-test-root "add" "--all")
  (trf-test-git trf-test-root "commit" "--quiet" "--message" "Order")
  (let ((head (trf-test-head trf-test-root))
        runs)
    (trf-test-with-machines runs 'now
      (assert-nil (trf-test-implement)))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal head (trf-test-head trf-test-root))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal head (string-trim (trf-test-git (nth 1 (car runs)) "rev-parse" "HEAD")))))

(deftest trf-implement-from-inside-a-worktree ()
  "Asked from inside a worktree, the IDE implements the target product's record changes."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let (runs)
    (trf-test-with-machines runs 'now
      (trf-test-implement))
    (let* ((first (nth 1 (car runs)))
           (first-head (string-trim (trf-test-git first "rev-parse" "HEAD"))))
      (setq runs nil)
      (trf-test-write-files trf-test-root '(("docs/features/f-2-pay.org" . "* F-2 Pay\n")))
      (trf-test-with-machines runs 'now
        (assert-nil (trf-test-implement (expand-file-name ".trf/" first))))
      (assert-equal "main\n" (trf-test-git trf-test-root "rev-parse" "--abbrev-ref" "HEAD"))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-match "^- F-2 Pay$" (trf-test-git trf-test-root "log" "-1" "--format=format:%B"))
      ;; UW-1 §Extensions: Turn record changes into code
      (assert-equal first-head (string-trim (trf-test-git first "rev-parse" "HEAD")))
      (assert-equal (expand-file-name ".worktrees/" trf-test-root)
                    (file-name-directory (directory-file-name (nth 1 (car runs))))))))

(defun trf-test-implemented-worktree (&optional record-changes)
  "Implement RECORD-CHANGES with machines finishing at once; return the worktree.
RECORD-CHANGES is an alist of (path . content), `trf-test-record-change' alone by default."
  (trf-test-write-files trf-test-root (or record-changes (list trf-test-record-change)))
  (let (runs)
    (trf-test-with-machines runs 'now
      (trf-test-implement))
    (nth 1 (car runs))))

(defun trf-test-report-file (directory)
  (expand-file-name ".trf/record-implementation-report.org" directory))

(defun trf-test-from-file (file command)
  "Run COMMAND from FILE's buffer, running no machine; return what it signals."
  (cl-letf (((symbol-function 'trf-session-config) (lambda () (error "Asked for a config")))
            ((symbol-function 'trf-session-run) (lambda (&rest _) (error "Ran a machine"))))
    (with-current-buffer (find-file-noselect file)
      (condition-case err
          (progn (funcall command) nil)
        (error err)))))

(defun trf-test-widget ()
  "Return the codebase's src/widget.el."
  (with-temp-buffer
    (insert-file-contents (expand-file-name "src/widget.el" trf-test-root))
    (buffer-string)))

(defun trf-test-state (directory)
  "Return the codebase's HEAD, status and refs/recs/, and the work in progress in DIRECTORY."
  (list (trf-test-head trf-test-root)
        (trf-test-git trf-test-root "status" "--porcelain")
        (trf-test-git trf-test-root "for-each-ref" "refs/recs/")
        (file-directory-p directory)
        (trf-test-head directory)
        (trf-test-git directory "status" "--porcelain")))

(defun trf-test-assert-accept-refuses (directory refusal)
  "Assert accepting from DIRECTORY's report gives REFUSAL and leaves the codebase and DIRECTORY as they were."
  (let ((state (trf-test-state directory)))
    (assert-equal refusal (trf-test-from-file (trf-test-report-file directory)
                                              #'trf-accept-record-implementation-report))
    (assert-equal state (trf-test-state directory))))

(defun trf-test-build (directory)
  "Commit an edit to src/widget.el in the worktree DIRECTORY."
  (trf-test-write-files directory '(("src/widget.el" . "(provide 'widget) ; built\n")))
  (trf-test-git directory "commit" "--quiet" "--all" "--message" "Build"))

(deftest trf-accept-outside-work-in-progress ()
  "Outside any work in progress, accepting gives MSG-2."
  ;; MSG-2: No work in progress to accept or reject
  (assert-equal '(user-error "There is no work in progress here to accept or reject.")
                (trf-test-from-file (expand-file-name "docs/vision.org" trf-test-root)
                                    #'trf-accept-record-implementation-report)))

(deftest trf-reject-outside-work-in-progress ()
  "Outside any work in progress, rejecting gives MSG-2."
  ;; MSG-2: No work in progress to accept or reject
  (assert-equal '(user-error "There is no work in progress here to accept or reject.")
                (trf-test-from-file (expand-file-name "docs/vision.org" trf-test-root)
                                    #'trf-reject-record-implementation-report)))

(deftest trf-accept-brings-the-work-in ()
  "Accepting brings the work into the codebase, and the record changes are implemented."
  (let* ((directory (trf-test-implemented-worktree))
         (base (trf-test-head trf-test-root)))
    (trf-test-build directory)
    (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                    #'trf-accept-record-implementation-report))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-equal "(provide 'widget) ; built\n" (trf-test-widget))
    ;; UW-1 §Success guarantee: Turn record changes into code
    (assert-equal base (string-trim (trf-test-git trf-test-root "rev-parse"
                                                  "refs/recs/implemented")))
    ;; UW-1 §Success guarantee: Turn record changes into code
    (assert-nil (trf-record-changes trf-test-root))))

(deftest trf-accept-mid-merge ()
  "Mid merge, accepting gives MSG-3 and keeps the work in progress."
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-leave-in-progress trf-test-root 'merge)
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-3: Finish the merge or rebase first
    (trf-test-assert-accept-refuses
     directory
     '(user-error
       "The target product is in the middle of a merge or rebase; finish it first."))))

(deftest trf-accept-on-another-branch ()
  "With the codebase on another branch, accepting gives MSG-8 and keeps the work in progress."
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-build directory)
    (trf-test-git trf-test-root "checkout" "--quiet" "-b" "side")
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-8: Target product is not on the primary git branch
    (trf-test-assert-accept-refuses
     directory
     '(user-error "The target product is not on the primary git branch:\nmain"))))

(deftest trf-accept-on-a-detached-codebase ()
  "With the codebase detached, accepting gives MSG-8 and keeps the work in progress."
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-build directory)
    (trf-test-git trf-test-root "checkout" "--quiet" "--detach")
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-8: Target product is not on the primary git branch
    (trf-test-assert-accept-refuses
     directory
     '(user-error "The target product is not on the primary git branch:\nmain"))))

(deftest trf-accept-refused-by-the-target-product ()
  "When the work cannot be brought in, accepting gives MSG-5 with git's reason and changes nothing."
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-build directory)
    (trf-test-write-files trf-test-root '(("src/widget.el" . "(provide 'widget) ; local\n")))
    (let* ((state (trf-test-state directory))
           (refusal (trf-test-from-file (trf-test-report-file directory)
                                        #'trf-accept-record-implementation-report)))
      (assert-equal 'user-error (car refusal))
      ;; UW-1 §Extensions: Turn record changes into code
      ;; MSG-5: Work cannot be brought into the codebase
      (assert-true (string-prefix-p "The work cannot be brought into the codebase:\n"
                                    (cadr refusal)))
      ;; UW-1 §Extensions: Turn record changes into code
      (assert-match "src/widget\\.el" (cadr refusal))
      ;; UW-1 §Extensions: Turn record changes into code
      (assert-equal state (trf-test-state directory)))))

(deftest trf-accept-on-a-moved-codebase-reruns ()
  "On a moved codebase, accepting runs the implementation workflow and the assessor again, and brings nothing in."
  (let ((directory (trf-test-implemented-worktree))
        runs)
    (trf-test-write-files trf-test-root '(("src/other.el" . "(provide 'other)\n")))
    (trf-test-git trf-test-root "add" "--all")
    (trf-test-git trf-test-root "commit" "--quiet" "--message" "Other work")
    (let ((state (trf-test-state directory))
          (asked 0)
          (report (find-file-noselect (trf-test-report-file directory))))
      (trf-test-write-files directory '((".trf/record-implementation-report.org" . "Old report.")))
      (set-file-times (trf-test-report-file directory) (time-subtract nil 60))
      (with-current-buffer report (revert-buffer t t t))
      (trf-test-with-machines runs 'now
        (cl-letf (((symbol-function 'trf-session-config)
                   (lambda () (cl-incf asked) trf-test-config))
                  ((symbol-function 'yes-or-no-p) (lambda (&rest _) (error "Asked to reread")))
                  ((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked to reread"))))
          (with-current-buffer report
            (trf-accept-record-implementation-report))))
      ;; UW-1 §Extensions: Turn record changes into code
      (assert-equal '("brainstorming machine" "planning machine" "executing machine"
                      "reconciliation assessor")
                    (mapcar #'car runs))
      ;; UW-1 §Extensions: Turn record changes into code
      (assert-equal (make-list 4 directory) (mapcar (lambda (run) (nth 1 run)) runs))
      (assert-equal 1 asked)
      ;; UW-1 §Extensions: Turn record changes into code
      (assert-match "The codebase has moved since this work started; bring the work up to date with it\\."
                    (nth 3 (car runs)))
      ;; UW-1 §Extensions: Turn record changes into code
      (assert-nil (string-match-p "produced[^\n]*:\nThe codebase has moved" (nth 3 (car runs))))
      ;; UW-1 §Extensions: Turn record changes into code
      (assert-match "^- F-1 Order (added): docs/features/f-1-order\\.org$" (nth 3 (car runs)))
      ;; UW-1 §Main success scenario: Turn record changes into code
      (assert-true (trf-test-report-shown-p))
      ;; UW-1 §Extensions: Turn record changes into code
      (assert-equal "Assessed by the reconciliation assessor."
                    (with-current-buffer (window-buffer (selected-window)) (buffer-string)))
      ;; UW-1 §Minimal guarantee: Turn record changes into code
      (assert-equal state (trf-test-state directory)))))

(deftest trf-reject-keeps-the-work ()
  "Rejecting closes the report and keeps the work in progress; nothing is implemented."
  (let* ((directory (trf-test-implemented-worktree))
         (state (trf-test-state directory)))
    (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                    #'trf-reject-record-implementation-report))
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-nil (find-buffer-visiting (trf-test-report-file directory)))
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-equal state (trf-test-state directory))))

(defconst trf-test-other-record-change '("docs/features/f-2-pay.org" . "* F-2 Pay\n"))

(defun trf-test-rerun-record-changes (directory)
  "Accept DIRECTORY's report on a moved codebase; return the record changes the brainstorming machine is given, or nil."
  (let (runs)
    (trf-test-with-machines runs 'now
      (with-current-buffer (find-file-noselect (trf-test-report-file directory))
        (trf-accept-record-implementation-report)))
    (let ((instructions (nth 3 (car runs))))
      (when (string-match "The record changes:\n\\(\\(?:- .*\n\\)*\\)" instructions)
        (match-string 1 instructions)))))

(deftest trf-accept-reruns-on-the-record-changes-it-started-with-after-later-work-is-accepted ()
  "Work started before other work is accepted reruns on the record changes it started with."
  (let* ((started-first (trf-test-implemented-worktree (list trf-test-other-record-change)))
         (started-later (trf-test-implemented-worktree))
         (base (trf-worktree-base started-later)))
    (assert-nil (trf-test-from-file (trf-test-report-file started-later)
                                    #'trf-accept-record-implementation-report))
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-equal "- F-2 Pay (added): docs/features/f-2-pay.org\n"
                  (trf-test-rerun-record-changes started-first))
    (trf-test-build started-first)
    (trf-test-git started-first "merge" "--quiet" "--no-edit" "main")
    (assert-nil (trf-test-from-file (trf-test-report-file started-first)
                                    #'trf-accept-record-implementation-report))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-equal "(provide 'widget) ; built\n" (trf-test-widget))
    ;; UW-1 §Success guarantee: Turn record changes into code
    (assert-equal base
                  (string-trim (trf-test-git trf-test-root "rev-parse"
                                             "refs/recs/implemented")))))

(deftest trf-accept-reruns-on-the-record-changes-it-started-with-after-earlier-work-is-accepted ()
  "Work started on top of other work reruns on the record changes it started with once that work is accepted."
  (let* ((started-first (trf-test-implemented-worktree))
         (started-later (trf-test-implemented-worktree (list trf-test-other-record-change))))
    (trf-test-build started-first)
    (trf-test-git started-first "merge" "--quiet" "--no-edit" "main")
    (assert-nil (trf-test-from-file (trf-test-report-file started-first)
                                    #'trf-accept-record-implementation-report))
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-equal (concat "- F-1 Order (added): docs/features/f-1-order.org\n"
                          "- F-2 Pay (added): docs/features/f-2-pay.org\n")
                  (trf-test-rerun-record-changes started-later))))

(defmacro trf-test-failing-git (function reason &rest body)
  "Run BODY with FUNCTION signalling `trf-git-error' with REASON."
  `(cl-letf (((symbol-function ,function)
              (lambda (&rest _) (signal 'trf-git-error (list ,reason)))))
     ,@body))

(defmacro trf-test-failing-git-subcommand (subcommand reason &rest body)
  "Run BODY with `trf-git' signalling `trf-git-error' with REASON for SUBCOMMAND.
SUBCOMMAND is a string, or a list of the arguments a call starts with.
Every other call to `trf-git' runs git."
  (declare (indent 2) (debug t))
  (let ((git (make-symbol "git")))
    `(let ((,git (symbol-function 'trf-git)))
       (cl-letf (((symbol-function 'trf-git)
                  (lambda (directory &rest args)
                    (if (let ((prefix (ensure-list ,subcommand)))
                          (equal (seq-take args (length prefix)) prefix))
                        (signal 'trf-git-error (list ,reason))
                      (apply ,git directory args)))))
         ,@body))))

(deftest trf-implement-git-fails-checking-for-an-operation-in-progress ()
  "When git fails checking for a merge or rebase, implementing gives MSG-7 with git's output."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  ;; F-1 §Rules: Ask the IDE to implement the record changes
  ;; MSG-7: Git could not do what the IDE asked
  (trf-test-assert-implement-refuses
   '(user-error "Git could not do what the IDE asked:\nfatal: broken")
   (lambda () (trf-test-failing-git 'trf-git-operation-in-progress-p "fatal: broken" (trf-test-implement)))))

(deftest trf-implement-git-fails-without-a-reason ()
  "When git fails reading the record changes and says nothing, implementing gives MSG-7 with no reason given."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  ;; F-1 §Rules: Ask the IDE to implement the record changes
  ;; MSG-7: Git could not do what the IDE asked
  (trf-test-assert-implement-refuses
   '(user-error "Git could not do what the IDE asked:\nNo reason given.")
   (lambda () (trf-test-failing-git 'trf-record-changes "" (trf-test-implement)))))

(deftest trf-implement-git-fails-creating-the-worktree ()
  "When git fails creating the worktree, implementing gives MSG-7 and keeps the record changes' commit."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let (runs refusal)
    (trf-test-with-machines runs 'now
      (trf-test-failing-git 'trf-worktree-create "fatal: no worktree"
        (setq refusal (trf-test-implement))))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    ;; MSG-7: Git could not do what the IDE asked
    (assert-equal '(user-error "Git could not do what the IDE asked:\nfatal: no worktree") refusal)
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal "Record changes\n\n- F-1 Order\n"
                  (trf-test-git trf-test-root "log" "-1" "--format=format:%B"))
    (assert-nil runs)))

(deftest trf-accept-git-fails-checking-whether-the-codebase-moved ()
  "When git fails checking whether the codebase moved, accepting gives MSG-7 and keeps the work in progress."
  (let* ((directory (trf-test-implemented-worktree))
         (state (trf-test-state directory)))
    (trf-test-failing-git 'trf-worktree-codebase-moved-p "fatal: bad object"
      ;; F-2 §Rules: Accept or reject the record implementation report
      ;; MSG-7: Git could not do what the IDE asked
      (assert-equal '(user-error "Git could not do what the IDE asked:\nfatal: bad object")
                    (trf-test-from-file (trf-test-report-file directory)
                                        #'trf-accept-record-implementation-report)))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-equal state (trf-test-state directory))))

(deftest trf-accept-git-fails-after-bringing-the-work-in ()
  "When git fails marking the record changes implemented after the merge, accepting gives MSG-7, not MSG-5."
  (let* ((directory (trf-test-implemented-worktree))
         (base (trf-worktree-base directory)))
    (trf-test-build directory)
    (trf-test-failing-git-subcommand "update-ref" "fatal: cannot lock ref"
      ;; F-2 §Rules: Accept or reject the record implementation report
      ;; MSG-7: Git could not do what the IDE asked
      (assert-equal '(user-error "Git could not do what the IDE asked:\nfatal: cannot lock ref")
                    (trf-test-from-file (trf-test-report-file directory)
                                        #'trf-accept-record-implementation-report)))
    (assert-equal "(provide 'widget) ; built\n" (trf-test-widget))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-true (file-directory-p directory))
    (let ((head (trf-test-head trf-test-root)))
      (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                      #'trf-accept-record-implementation-report))
      ;; UW-1 §Success guarantee: Turn record changes into code
      (assert-equal base
                    (string-trim (trf-test-git trf-test-root "rev-parse"
                                               "refs/recs/implemented")))
      ;; UW-1 §Main success scenario: Turn record changes into code
      (assert-equal head (trf-test-head trf-test-root)))))

(deftest trf-accept-from-another-file-of-the-work ()
  "Accepting from any file of the work in progress brings the work in."
  (let* ((directory (trf-test-implemented-worktree))
         (base (trf-worktree-base directory)))
    (trf-test-build directory)
    (assert-nil (trf-test-from-file (expand-file-name "src/widget.el" directory)
                                    #'trf-accept-record-implementation-report))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (assert-equal "(provide 'widget) ; built\n" (trf-test-widget))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (assert-equal base
                  (string-trim (trf-test-git trf-test-root "rev-parse" "refs/recs/implemented")))))

(deftest trf-reject-from-another-file-of-the-work ()
  "Rejecting from any file of the work in progress closes the report and keeps the work."
  (let* ((directory (trf-test-implemented-worktree))
         (state (trf-test-state directory))
         (file (expand-file-name "src/widget.el" directory)))
    (assert-nil (trf-test-from-file file #'trf-reject-record-implementation-report))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (assert-nil (find-buffer-visiting (trf-test-report-file directory)))
    (assert-true (find-buffer-visiting file))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (assert-equal state (trf-test-state directory))))

(deftest trf-accept-after-the-rerun-brings-the-work-in ()
  "Once the rerun has brought the work up to date with the codebase, accepting brings it in."
  (let* ((directory (trf-test-implemented-worktree))
         (base (trf-worktree-base directory))
         runs)
    (trf-test-build directory)
    (trf-test-write-files trf-test-root '(("src/other.el" . "(provide 'other)\n")))
    (trf-test-git trf-test-root "add" "--all")
    (trf-test-git trf-test-root "commit" "--quiet" "--message" "Other work")
    (trf-test-with-machines runs 'now
      (with-current-buffer (find-file-noselect (trf-test-report-file directory))
        (trf-accept-record-implementation-report)))
    (assert-equal 4 (length runs))
    (trf-test-git directory "merge" "--quiet" "--no-edit" "main")
    (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                    #'trf-accept-record-implementation-report))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-equal "(provide 'widget) ; built\n" (trf-test-widget))
    ;; UW-1 §Success guarantee: Turn record changes into code
    (assert-equal base
                  (string-trim (trf-test-git trf-test-root "rev-parse" "refs/recs/implemented")))))

(deftest trf-implement-on-a-detached-codebase ()
  "On a detached codebase, the IDE gives MSG-8, asks nothing, commits nothing and starts no work."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (trf-test-git trf-test-root "checkout" "--quiet" "--detach")
  ;; UW-1 §Extensions: Turn record changes into code
  ;; MSG-8: Target product is not on the primary git branch
  (trf-test-assert-implement-refuses '(user-error "The target product is not on the primary git branch:\nmain")))

(deftest trf-implement-on-a-detached-codebase-gives-msg-8-before-msg-1 ()
  "On a detached codebase without record changes, the IDE gives MSG-8 rather than MSG-1."
  (trf-test-git trf-test-root "checkout" "--quiet" "--detach")
  ;; UW-1 §Extensions: Turn record changes into code
  ;; MSG-8: Target product is not on the primary git branch
  (trf-test-assert-implement-refuses '(user-error "The target product is not on the primary git branch:\nmain")))

(deftest trf-implement-mid-rebase-gives-msg-3-before-msg-8 ()
  "Mid rebase, with the codebase detached, the IDE gives MSG-3 rather than MSG-8."
  (trf-test-leave-in-progress trf-test-root 'rebase)
  (assert-equal "" (trf-test-git trf-test-root "branch" "--show-current"))
  ;; UW-1 §Extensions: Turn record changes into code
  ;; MSG-3: Finish the merge or rebase first
  (trf-test-assert-implement-refuses
   '(user-error
     "The target product is in the middle of a merge or rebase; finish it first.")))

(deftest trf-implement-on-another-branch ()
  "On another branch, the IDE gives MSG-8, asks nothing, commits nothing and starts no work."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (trf-test-git trf-test-root "checkout" "--quiet" "-b" "side")
  ;; UW-1 §Extensions: Turn record changes into code
  ;; MSG-8: Target product is not on the primary git branch
  (trf-test-assert-implement-refuses '(user-error "The target product is not on the primary git branch:\nmain")))

(defun trf-test-write-primary-git-branch (directory value)
  "Write a .dir-locals.el in DIRECTORY setting `trf-primary-git-branch' to VALUE."
  (trf-test-write-files directory
                        (list (cons ".dir-locals.el"
                                    (format "((nil . ((trf-primary-git-branch . %S))))\n"
                                            value)))))

(defun trf-test-commit-primary-git-branch ()
  "Commit the target product's .dir-locals.el."
  (trf-test-git trf-test-root "add" ".dir-locals.el")
  (trf-test-git trf-test-root "commit" "--quiet" "--message" "Primary git branch"))

(deftest trf-implement-on-the-primary-git-branch-the-target-product-names ()
  "On the primary git branch the target product names, the IDE runs the implementation workflow."
  (trf-test-write-primary-git-branch trf-test-root "trunk")
  (trf-test-commit-primary-git-branch)
  (trf-test-git trf-test-root "checkout" "--quiet" "-b" "trunk")
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let (runs)
    (trf-test-with-machines runs 'now
      (assert-nil (trf-test-implement)))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-equal 4 (length runs))))

(deftest trf-implement-on-main-when-the-target-product-names-another-primary-git-branch ()
  "On main, when the target product names another primary git branch, the IDE gives MSG-8 with it."
  (trf-test-write-primary-git-branch trf-test-root "trunk")
  (trf-test-commit-primary-git-branch)
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  ;; UW-1 §Extensions: Turn record changes into code
  ;; MSG-8: Target product is not on the primary git branch
  (trf-test-assert-implement-refuses '(user-error "The target product is not on the primary git branch:\ntrunk")))

(deftest trf-accept-on-main-when-the-target-product-names-another-primary-git-branch ()
  "On main, when the target product names another primary git branch, accepting gives MSG-8 with it."
  (trf-test-write-primary-git-branch trf-test-root "trunk")
  (trf-test-commit-primary-git-branch)
  (trf-test-git trf-test-root "checkout" "--quiet" "-b" "trunk")
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-build directory)
    (trf-test-git trf-test-root "checkout" "--quiet" "main")
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-8: Target product is not on the primary git branch
    (trf-test-assert-accept-refuses
     directory
     '(user-error "The target product is not on the primary git branch:\ntrunk"))))

(deftest trf-accept-off-the-primary-git-branch-once-the-codebase-has-moved ()
  "Off the primary git branch, once the codebase has moved, accepting gives MSG-8 and reruns nothing."
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-build directory)
    (trf-test-write-files trf-test-root '(("src/other.el" . "(provide 'other)\n")))
    (trf-test-git trf-test-root "add" "--all")
    (trf-test-git trf-test-root "commit" "--quiet" "--message" "Other work")
    (trf-test-git trf-test-root "checkout" "--quiet" "-b" "side")
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-8: Target product is not on the primary git branch
    (trf-test-assert-accept-refuses
     directory
     '(user-error "The target product is not on the primary git branch:\nmain"))))

(deftest trf-accept-mid-rebase-off-the-primary-git-branch-gives-msg-3-before-msg-8 ()
  "Mid rebase, with the codebase detached, accepting gives MSG-3 rather than MSG-8."
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-leave-in-progress trf-test-root 'rebase)
    (assert-equal "" (trf-test-git trf-test-root "branch" "--show-current"))
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-3: Finish the merge or rebase first
    (trf-test-assert-accept-refuses
     directory
     '(user-error
       "The target product is in the middle of a merge or rebase; finish it first."))))

(deftest trf-accept-reads-the-primary-git-branch-from-the-target-product-root ()
  "When the work in progress and the target product name different primary git branches, accepting reads the target product's."
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-build directory)
    (trf-test-write-primary-git-branch trf-test-root "trunk")
    (trf-test-write-primary-git-branch directory "main")
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-8: Target product is not on the primary git branch
    (trf-test-assert-accept-refuses
     directory
     '(user-error "The target product is not on the primary git branch:\ntrunk"))))

(deftest trf-implement-detached-when-the-target-product-names-no-primary-git-branch ()
  "Detached, with all local variables allowed, when the target product's .dir-locals.el names nil, the IDE gives MSG-9."
  (trf-test-write-primary-git-branch trf-test-root nil)
  (trf-test-git trf-test-root "checkout" "--quiet" "--detach")
  (let ((enable-local-variables :all))
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-9: No primary git branch is named
    (trf-test-assert-implement-refuses '(user-error "Please set a primary git branch first."))))

(deftest trf-implement-evaluates-nothing-from-the-target-products-dir-locals ()
  "Implementing evaluates no eval form in the target product's .dir-locals.el."
  (let ((marker (expand-file-name "evaluated" trf-test-root)))
    (trf-test-write-files trf-test-root
                          (list (cons ".dir-locals.el"
                                      (format "((nil . ((eval . (write-region \"\" nil %S)))))\n"
                                              marker))))
    (let ((enable-local-variables :all))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      ;; UW-1 §Minimal guarantee: Turn record changes into code
      ;; MSG-1: No record changes to implement
      (trf-test-assert-implement-refuses '(user-error "There are no record changes to implement.")))
    (assert-nil (file-exists-p marker))))

(deftest trf-implement-when-the-target-product-names-an-empty-primary-git-branch ()
  "When the target product's .dir-locals.el names an empty string, the IDE gives MSG-9."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (trf-test-write-primary-git-branch trf-test-root "")
  ;; UW-1 §Extensions: Turn record changes into code
  ;; MSG-9: No primary git branch is named
  (trf-test-assert-implement-refuses '(user-error "Please set a primary git branch first.")))

(deftest trf-implement-when-the-target-product-names-no-primary-git-branch ()
  "When the target product's .dir-locals.el names nil, the IDE gives MSG-9."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (trf-test-write-primary-git-branch trf-test-root nil)
  ;; UW-1 §Extensions: Turn record changes into code
  ;; MSG-9: No primary git branch is named
  (trf-test-assert-implement-refuses '(user-error "Please set a primary git branch first.")))

(deftest trf-implement-ignores-the-target-products-dir-locals-with-local-variables-off ()
  "With local variables turned off, the IDE ignores the target product's .dir-locals.el."
  (trf-test-write-primary-git-branch trf-test-root "trunk")
  (let ((enable-local-variables nil))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    ;; MSG-1: No record changes to implement
    (trf-test-assert-implement-refuses '(user-error "There are no record changes to implement."))))

(deftest trf-implement-when-no-primary-git-branch-is-named ()
  "When no primary git branch is named, the IDE gives MSG-9, asks nothing, commits nothing and starts no work."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let ((trf-primary-git-branch nil))
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-9: No primary git branch is named
    (trf-test-assert-implement-refuses '(user-error "Please set a primary git branch first."))))

(deftest trf-implement-when-an-empty-primary-git-branch-is-named ()
  "When the IDE setting names an empty primary git branch, the IDE gives MSG-9."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let ((trf-primary-git-branch ""))
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-9: No primary git branch is named
    (trf-test-assert-implement-refuses '(user-error "Please set a primary git branch first."))))

(deftest trf-implement-detached-when-no-primary-git-branch-is-named-gives-msg-9-before-msg-8 ()
  "Detached, when no primary git branch is named, the IDE gives MSG-9 rather than MSG-8."
  (trf-test-git trf-test-root "checkout" "--quiet" "--detach")
  (let ((trf-primary-git-branch nil))
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-9: No primary git branch is named
    (trf-test-assert-implement-refuses '(user-error "Please set a primary git branch first."))))

(deftest trf-implement-when-no-primary-git-branch-is-named-gives-msg-9-before-msg-1 ()
  "Without record changes, when no primary git branch is named, the IDE gives MSG-9 rather than MSG-1."
  (let ((trf-primary-git-branch nil))
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-9: No primary git branch is named
    (trf-test-assert-implement-refuses '(user-error "Please set a primary git branch first."))))

(deftest trf-implement-mid-merge-when-no-primary-git-branch-is-named-gives-msg-3-before-msg-9 ()
  "Mid merge, when no primary git branch is named, the IDE gives MSG-3 rather than MSG-9."
  (trf-test-leave-in-progress trf-test-root 'merge)
  (let ((trf-primary-git-branch nil))
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-3: Finish the merge or rebase first
    (trf-test-assert-implement-refuses
     '(user-error
       "The target product is in the middle of a merge or rebase; finish it first."))))

(deftest trf-accept-when-no-primary-git-branch-is-named ()
  "When no primary git branch is named, accepting gives MSG-9 and keeps the work in progress."
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-build directory)
    (let ((trf-primary-git-branch nil))
      ;; UW-1 §Extensions: Turn record changes into code
      ;; MSG-9: No primary git branch is named
      (trf-test-assert-accept-refuses directory '(user-error "Please set a primary git branch first.")))))

(deftest trf-accept-mid-merge-when-no-primary-git-branch-is-named-gives-msg-3-before-msg-9 ()
  "Mid merge, when no primary git branch is named, accepting gives MSG-3 rather than MSG-9."
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-leave-in-progress trf-test-root 'merge)
    (let ((trf-primary-git-branch nil))
      ;; UW-1 §Extensions: Turn record changes into code
      ;; MSG-3: Finish the merge or rebase first
      (trf-test-assert-accept-refuses
       directory
       '(user-error
         "The target product is in the middle of a merge or rebase; finish it first.")))))

(deftest trf-accept-outside-work-in-progress-when-no-primary-git-branch-is-named ()
  "Outside any work in progress, when no primary git branch is named, accepting gives MSG-2."
  (let ((trf-primary-git-branch nil))
    ;; F-2 §Rules: Accept or reject the record implementation report
    ;; MSG-2: No work in progress to accept or reject
    (assert-equal '(user-error "There is no work in progress here to accept or reject.")
                  (trf-test-from-file (expand-file-name "docs/vision.org" trf-test-root)
                                      #'trf-accept-record-implementation-report))))

(deftest trf-accept-when-the-target-product-names-no-primary-git-branch ()
  "When the target product's .dir-locals.el names nil, accepting gives MSG-9 and keeps the work in progress."
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-build directory)
    (trf-test-write-primary-git-branch trf-test-root nil)
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-9: No primary git branch is named
    (trf-test-assert-accept-refuses directory '(user-error "Please set a primary git branch first."))))

(deftest trf-accept-refused-by-the-target-product-without-a-reason ()
  "When the work cannot be brought in and git says nothing, accepting gives MSG-5 with no reason given."
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-build directory)
    (trf-test-failing-git-subcommand "merge" ""
      ;; UW-1 §Extensions: Turn record changes into code
      ;; MSG-5: Work cannot be brought into the codebase
      (trf-test-assert-accept-refuses
       directory
       '(user-error "The work cannot be brought into the codebase:\nNo reason given.")))))

(defun trf-test-assert-work-in-progress-removed (directory branch)
  "Assert the worktree DIRECTORY, its BRANCH and every buffer in it are gone."
  (assert-nil (file-exists-p directory))
  (assert-equal "" (trf-test-git trf-test-root "branch" "--list" branch))
  (assert-nil (seq-find (lambda (buffer)
                          (string-prefix-p
                           directory
                           (expand-file-name (or (buffer-file-name buffer)
                                                 (buffer-local-value 'default-directory buffer)))))
                        (buffer-list))))

(deftest trf-accept-removes-the-work-in-progress ()
  "Accepting from the report removes the worktree, its branch and the buffers visiting it."
  (let* ((directory (trf-test-implemented-worktree))
         (branch (trf-worktree-branch directory)))
    (trf-test-build directory)
    (find-file-noselect (expand-file-name "src/widget.el" directory))
    (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                    #'trf-accept-record-implementation-report))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-equal "(provide 'widget) ; built\n" (trf-test-widget))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (trf-test-assert-work-in-progress-removed directory branch)))

(deftest trf-accept-of-work-already-brought-in-removes-the-work-in-progress ()
  "Accepting work already brought into the codebase marks it implemented and removes it."
  (let* ((directory (trf-test-implemented-worktree))
         (branch (trf-worktree-branch directory))
         (base (trf-worktree-base directory)))
    (trf-test-build directory)
    (trf-test-git trf-test-root "merge" "--quiet" "--no-edit" branch)
    (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                    #'trf-accept-record-implementation-report))
    ;; UW-1 §Success guarantee: Turn record changes into code
    (assert-equal base (string-trim (trf-test-git trf-test-root "rev-parse"
                                                  "refs/recs/implemented")))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (trf-test-assert-work-in-progress-removed directory branch)))

(deftest trf-accept-git-fails-removing-the-work-in-progress ()
  "When git refuses to remove the worktree, accepting gives MSG-7; accepting again removes it."
  (let* ((directory (trf-test-implemented-worktree))
         (branch (trf-worktree-branch directory))
         (base (trf-worktree-base directory)))
    (trf-test-build directory)
    (let ((refusal (trf-test-failing-git-subcommand '("worktree" "remove") "fatal: cannot remove"
                     (trf-test-from-file (trf-test-report-file directory)
                                         #'trf-accept-record-implementation-report))))
      ;; F-2 §Rules: Accept or reject the record implementation report
      ;; MSG-7: Git could not do what the IDE asked
      (assert-equal 'user-error (car refusal))
      (assert-true (string-prefix-p "Git could not do what the IDE asked:\n" (cadr refusal))))
    ;; UW-1 §Success guarantee: Turn record changes into code
    (assert-equal base (string-trim (trf-test-git trf-test-root "rev-parse"
                                                  "refs/recs/implemented")))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-true (file-exists-p (trf-test-report-file directory)))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-true (find-buffer-visiting (trf-test-report-file directory)))
    (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                    #'trf-accept-record-implementation-report))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (trf-test-assert-work-in-progress-removed directory branch)))

(deftest trf-reject-keeps-the-worktree-and-its-branch ()
  "Rejecting keeps the worktree and its branch."
  (let* ((directory (trf-test-implemented-worktree))
         (branch (trf-worktree-branch directory)))
    (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                    #'trf-reject-record-implementation-report))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (assert-true (file-exists-p (trf-test-report-file directory)))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (assert-match (regexp-quote branch) (trf-test-git trf-test-root "branch" "--list" branch))))

(deftest trf-accept-git-fails-deleting-the-branch ()
  "When git fails deleting the branch of the work in progress, accepting gives MSG-7; accepting again removes the work."
  (let* ((directory (trf-test-implemented-worktree))
         (branch (trf-worktree-branch directory)))
    (trf-test-build directory)
    (trf-test-failing-git-subcommand '("branch" "--quiet" "-d") "fatal: cannot lock ref"
      ;; F-2 §Rules: Accept or reject the record implementation report
      ;; MSG-7: Git could not do what the IDE asked
      (assert-equal '(user-error "Git could not do what the IDE asked:\nfatal: cannot lock ref")
                    (trf-test-from-file (trf-test-report-file directory)
                                        #'trf-accept-record-implementation-report)))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-true (file-exists-p (trf-test-report-file directory)))
    (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                    #'trf-accept-record-implementation-report))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (trf-test-assert-work-in-progress-removed directory branch)))

(deftest trf-accept-git-fails-removing-work-that-holds-no-commit ()
  "When git refuses to remove a worktree holding no commit, accepting again removes it."
  (let* ((directory (trf-test-implemented-worktree))
         (branch (trf-worktree-branch directory)))
    (let ((refusal (trf-test-failing-git-subcommand '("worktree" "remove") "fatal: cannot remove"
                     (trf-test-from-file (trf-test-report-file directory)
                                         #'trf-accept-record-implementation-report))))
      ;; F-2 §Rules: Accept or reject the record implementation report
      ;; MSG-7: Git could not do what the IDE asked
      (assert-true (string-prefix-p "Git could not do what the IDE asked:\n" (cadr refusal))))
    (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                    #'trf-accept-record-implementation-report))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (trf-test-assert-work-in-progress-removed directory branch)))

(deftest trf-accept-moves-the-record-keepers-buffers-to-the-root ()
  "Accepting moves a buffer of the record keeper's in the worktree to the root, keeping its process."
  (let* ((directory (trf-test-implemented-worktree))
         (branch (trf-worktree-branch directory))
         (shell (generate-new-buffer "*trf test shell*"))
         process)
    (unwind-protect
        (progn
          (trf-test-build directory)
          (with-current-buffer shell
            (setq default-directory directory)
            (setq process (make-process :name "trf-test-shell" :buffer shell
                                        :command '("sleep" "60"))))
          (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                          #'trf-accept-record-implementation-report))
          ;; F-2 §Produces: Accept or reject the record implementation report
          (assert-true (buffer-live-p shell))
          ;; F-2 §Produces: Accept or reject the record implementation report
          (assert-true (process-live-p process))
          ;; F-2 §Produces: Accept or reject the record implementation report
          (assert-equal (file-name-as-directory trf-test-root)
                        (buffer-local-value 'default-directory shell))
          ;; F-2 §Produces: Accept or reject the record implementation report
          (trf-test-assert-work-in-progress-removed directory branch))
      (when (process-live-p process)
        (delete-process process))
      (kill-buffer shell))))

(deftest trf-accept-kills-the-machines-session-buffers ()
  "Accepting kills the session buffers of the machines that worked on it."
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-build directory)
    (with-current-buffer (get-buffer trf-test-machine-buffer-name)
      (setq default-directory trf-test-root))
    (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                    #'trf-accept-record-implementation-report))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-nil (get-buffer trf-test-machine-buffer-name))))

(deftest trf-accept-moves-session-buffers-it-did-not-start-to-the-root ()
  "Accepting moves to the root a session buffer in the worktree that the IDE did not start since it last started."
  (let ((directory (trf-test-implemented-worktree)))
    (trf-test-build directory)
    (clrhash trf--session-buffers)
    (with-current-buffer (get-buffer trf-test-machine-buffer-name)
      (setq default-directory directory))
    (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                    #'trf-accept-record-implementation-report))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-true (get-buffer trf-test-machine-buffer-name))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (assert-equal (file-name-as-directory trf-test-root)
                  (buffer-local-value 'default-directory
                                      (get-buffer trf-test-machine-buffer-name)))))

(defmacro trf-test-offering-saves (answer offered &rest body)
  "Run BODY answering ANSWER to each offer to save a buffer; collect the buffers offered in OFFERED."
  (declare (indent 2) (debug t))
  `(cl-letf (((symbol-function 'map-y-or-n-p)
              (lambda (_prompter actor list &rest _)
                (setq ,offered (append ,offered list))
                (if ,answer
                    (progn (mapc actor list) (length list))
                  0))))
     ,@body))

(defmacro trf-test-with-the-ides-files-kept (&rest body)
  "Run BODY with the IDE keeping its auto-save, lock and backup files in a temp directory."
  (declare (indent 0) (debug t))
  (let ((state (make-symbol "state")))
    `(let* ((,state (make-temp-file "trf-state-" t))
            (process-environment (cons (concat "XDG_STATE_HOME=" ,state) process-environment))
            (auto-save-file-name-transforms auto-save-file-name-transforms)
            (lock-file-name-transforms lock-file-name-transforms)
            (backup-directory-alist backup-directory-alist))
       (unwind-protect
           (progn
             (trf-emacs-keep-files-outside-the-target-product)
             ,@body)
         (delete-directory ,state t)))))

(deftest trf-implement-includes-a-record-saved-when-offered ()
  "A record file saved when the IDE offers to save it is among the record changes."
  (trf-test-with-the-ides-files-kept
    (let ((buffer (progn
                    (make-directory (expand-file-name "docs/features/" trf-test-root) t)
                    (find-file-noselect (expand-file-name (car trf-test-record-change)
                                                          trf-test-root))))
          offered shown)
      (with-current-buffer buffer
        (insert (cdr trf-test-record-change)))
      (trf-test-offering-saves t offered
        (cl-letf (((symbol-function 'y-or-n-p)
                   (lambda (&rest _)
                     (setq shown (with-current-buffer trf--record-changes-buffer-name
                                   (buffer-string)))
                     nil)))
          (assert-nil (trf-test-implement))))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal (list buffer) offered)
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "- F-1 Order (added): docs/features/f-1-order.org\n" shown))))

(deftest trf-implement-leaves-out-a-record-not-saved-when-offered ()
  "A record file not saved when the IDE offers to save it is left out of the record changes."
  (trf-test-with-the-ides-files-kept
    (let ((buffer (progn
                    (make-directory (expand-file-name "docs/features/" trf-test-root) t)
                    (find-file-noselect (expand-file-name (car trf-test-record-change)
                                                          trf-test-root))))
          offered)
      (with-current-buffer buffer
        (insert (cdr trf-test-record-change)))
      (trf-test-offering-saves nil offered
        ;; F-1 §Rules: Ask the IDE to implement the record changes
        ;; MSG-1: No record changes to implement
        (assert-equal '(user-error "There are no record changes to implement.")
                      (trf-test-implement)))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-true (buffer-modified-p buffer)))))

(deftest trf-accept-brings-in-an-edit-saved-when-offered ()
  "An edit in the work in progress saved when the IDE offers to save it is brought into the codebase."
  (let* ((directory (trf-test-implemented-worktree))
         (branch (trf-worktree-branch directory))
         (elsewhere (find-file-noselect (expand-file-name "docs/vision.org" trf-test-root)))
         (buffer (find-file-noselect (expand-file-name "src/widget.el" directory)))
         offered)
    (trf-test-build directory)
    (with-current-buffer buffer
      (revert-buffer t t t)
      (goto-char (point-max))
      (insert ";; saved\n"))
    (with-current-buffer elsewhere
      (insert "Elsewhere.\n"))
    (trf-test-offering-saves t offered
      (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                      #'trf-accept-record-implementation-report)))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-equal (list buffer) offered)
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-equal "(provide 'widget) ; built\n;; saved\n" (trf-test-widget))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (trf-test-assert-work-in-progress-removed directory branch)
    (with-current-buffer elsewhere
      (set-buffer-modified-p nil))))

(deftest trf-accept-brings-in-an-edit-already-saved ()
  "Edits saved in the work in progress but not committed, new files among them, are brought into the codebase."
  (let* ((directory (trf-test-implemented-worktree))
         (branch (trf-worktree-branch directory)))
    (trf-test-build directory)
    (trf-test-write-files directory '(("src/widget.el" . "(provide 'widget) ; saved\n")
                                      ("src/gadget.el" . "(provide 'gadget)\n")))
    (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                    #'trf-accept-record-implementation-report))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-equal "(provide 'widget) ; saved\n" (trf-test-widget))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-true (file-exists-p (expand-file-name "src/gadget.el" trf-test-root)))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-equal "" (trf-test-git trf-test-root "ls-files" ".trf"))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (trf-test-assert-work-in-progress-removed directory branch)))

(deftest trf-accept-leaves-an-edit-not-saved-when-offered ()
  "An edit not saved when the IDE offers to save it stays in its buffer; the work in progress is still removed."
  (trf-test-with-the-ides-files-kept
    (let* ((directory (trf-test-implemented-worktree))
           (branch (trf-worktree-branch directory))
           (buffer (find-file-noselect (expand-file-name "src/widget.el" directory)))
           offered)
      (trf-test-build directory)
      (with-current-buffer buffer
        (revert-buffer t t t)
        (goto-char (point-max))
        (insert ";; unsaved\n"))
      (trf-test-offering-saves nil offered
        (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                        #'trf-accept-record-implementation-report)))
      ;; F-2 §Rules: Accept or reject the record implementation report
      (assert-equal "(provide 'widget) ; built\n" (trf-test-widget))
      ;; F-2 §Rules: Accept or reject the record implementation report
      (assert-true (buffer-live-p buffer))
      ;; F-2 §Rules: Accept or reject the record implementation report
      (assert-true (buffer-modified-p buffer))
      ;; F-2 §Rules: Accept or reject the record implementation report
      (assert-equal "(provide 'widget) ; built\n;; unsaved\n"
                    (with-current-buffer buffer (buffer-string)))
      ;; F-2 §Rules: Accept or reject the record implementation report
      (assert-equal (file-name-as-directory trf-test-root)
                    (buffer-local-value 'default-directory buffer))
      (assert-nil (file-exists-p directory))
      (assert-equal "" (trf-test-git trf-test-root "branch" "--list" branch))
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer))))

(deftest trf-accept-leaves-emacs-lock-and-auto-save-files-out-of-the-codebase ()
  "An edit not saved when offered leaves its lock and auto-save files out of the codebase."
  (let* ((directory (trf-test-implemented-worktree))
         (file (expand-file-name "src/widget.el" directory))
         (buffer (find-file-noselect file))
         offered)
    (trf-test-build directory)
    (with-current-buffer buffer
      (revert-buffer t t t)
      (auto-save-mode 1)
      (goto-char (point-max))
      (insert ";; unsaved\n")
      (do-auto-save t))
    (assert-true (file-symlink-p (expand-file-name "src/.#widget.el" directory)))
    (assert-true (file-exists-p (expand-file-name "src/#widget.el#" directory)))
    (trf-test-offering-saves nil offered
      (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                      #'trf-accept-record-implementation-report)))
    ;; docs/knowledge-base/git-worktree-removal.org §git worktree remove refuses a worktree holding a modified buffer's lock file
    (assert-equal "" (trf-test-git trf-test-root "ls-files" "src/.#widget.el" "src/#widget.el#"))
    (assert-nil (file-exists-p directory))
    (with-current-buffer buffer
      (set-buffer-modified-p nil))
    (kill-buffer buffer)))

(deftest trf-accept-brings-in-an-edit-saved-after-the-work-was-brought-in ()
  "Once the work is brought in and its removal failed, an edit saved before accepting again is brought in too, with no rerun."
  (let* ((directory (trf-test-implemented-worktree))
         (branch (trf-worktree-branch directory)))
    (trf-test-build directory)
    (trf-test-failing-git-subcommand '("worktree" "remove") "fatal: cannot remove"
      (trf-test-from-file (trf-test-report-file directory)
                          #'trf-accept-record-implementation-report))
    (trf-test-write-files directory '(("src/widget.el" . "(provide 'widget) ; saved later\n")))
    (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                    #'trf-accept-record-implementation-report))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-equal "(provide 'widget) ; saved later\n" (trf-test-widget))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (trf-test-assert-work-in-progress-removed directory branch)))

(deftest trf-implement-saves-nothing-outside-the-target-product ()
  "Offering to save before implementing saves no buffer outside the target product, even one saved without asking."
  (let* ((outside (make-temp-file "trf-outside-" nil ".txt"))
         (buffer (find-file-noselect outside)))
    (unwind-protect
        (progn
          (with-current-buffer buffer
            (setq-local buffer-save-without-query t)
            (insert "unsaved"))
          (trf-test-implement)
          ;; F-1 §Rules: Ask the IDE to implement the record changes
          (assert-true (buffer-modified-p buffer)))
      (with-current-buffer buffer
        (set-buffer-modified-p nil))
      (kill-buffer buffer)
      (delete-file outside))))

(deftest trf-accept-keeps-a-session-buffer-the-record-keeper-keeps ()
  "A session buffer the record keeper keeps when it is killed stays recorded for the next accept."
  (let ((directory (trf-test-implemented-worktree)))
    (unwind-protect
        (progn
          (trf-test-build directory)
          (with-current-buffer (get-buffer trf-test-machine-buffer-name)
            (add-hook 'kill-buffer-query-functions #'ignore nil t))
          (assert-nil (trf-test-from-file (trf-test-report-file directory)
                                          #'trf-accept-record-implementation-report))
          ;; F-2 §Rules: Accept or reject the record implementation report
          (assert-true (get-buffer trf-test-machine-buffer-name))
          ;; F-2 §Rules: Accept or reject the record implementation report
          (assert-equal (list (get-buffer trf-test-machine-buffer-name))
                        (delete-dups (copy-sequence
                                      (gethash directory trf--session-buffers)))))
      (when (get-buffer trf-test-machine-buffer-name)
        (with-current-buffer (get-buffer trf-test-machine-buffer-name)
          (remove-hook 'kill-buffer-query-functions #'ignore t)))
      (clrhash trf--session-buffers))))

(provide 'trf-test)
;;; trf-test.el ends here
