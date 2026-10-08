;;; trf-test.el --- Tests for trf -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'e-unit)
(require 'trf-test-helper)
(require 'trf)

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
  (dolist (name (list trf-record-changes-buffer-name trf-test-machine-buffer-name))
    (when (get-buffer name)
      (kill-buffer name)))
  (delete-directory trf-test-root t))

(deftest trf-test-version-is-set ()
  "trf declares its version."
  (assert-equal "0.1.0" trf-version))

(defmacro trf-test-with-machines (runs finishing &rest body)
  "Run BODY with each machine handing off; RUNS collects (name directory config instructions).
FINISHING is `now' to finish each machine at once, `later' to finish it from a timer,
or the name of the machine that stops instead of finishing."
  (let ((timers (make-symbol "timers")))
    `(let ((,timers nil))
       (unwind-protect
           (cl-letf (((symbol-function 'trf-session-config) (lambda () trf-test-config))
                     ((symbol-function 'trf-session-run)
                      (lambda (machine directory config instructions on-finished on-stopped)
                        (let ((name (plist-get machine :name)))
                          (setq ,runs (append ,runs (list (list name directory config
                                                                instructions))))
                          (if (equal ,finishing name)
                              (funcall on-stopped)
                            (let ((text (concat "Handed off by " name ".")))
                              (with-temp-file (expand-file-name
                                               (trf-machines-handoff-file machine) directory)
                                (insert text))
                              (if (eq ,finishing 'later)
                                  (push (run-at-time 0 nil on-finished text) ,timers)
                                (funcall on-finished text)))))
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

(deftest trf-implement-without-record-changes ()
  "With no record changes the IDE gives MSG-1 and does nothing else."
  (let ((head (trf-test-head trf-test-root)))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked to confirm")))
              ((symbol-function 'trf-session-config) (lambda () (error "Asked for a config"))))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      ;; MSG-1: No record changes to implement
      (assert-equal '(user-error "There are no record changes to implement.")
                    (trf-test-implement)))
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    (assert-equal head (trf-test-head trf-test-root))))

(deftest trf-implement-mid-merge-without-record-changes ()
  "Mid merge, the IDE gives MSG-3 even when there are no record changes."
  (trf-test-leave-in-progress trf-test-root 'merge)
  ;; UW-1 §Extensions: Turn record changes into code
  ;; MSG-3: Finish the merge or rebase first
  (assert-equal '(user-error
                  "The target product is in the middle of a merge or rebase; finish it first.")
                (trf-test-implement)))

(deftest trf-implement-mid-merge-with-record-changes ()
  "Mid merge, the IDE gives MSG-3 and asks nothing."
  (trf-test-leave-in-progress trf-test-root 'merge)
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) (error "Asked to confirm"))))
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-3: Finish the merge or rebase first
    (assert-equal '(user-error
                    "The target product is in the middle of a merge or rebase; finish it first.")
                  (trf-test-implement))))

(deftest trf-implement-shows-the-record-changes ()
  "The record changes are shown before the record keeper is asked to confirm."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let (shown)
    (cl-letf (((symbol-function 'y-or-n-p)
               (lambda (&rest _)
                 (setq shown (with-current-buffer trf-record-changes-buffer-name
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
  "The implementation workflow's machines run in turn, each given the handoff before it, then the reconciliation assessor."
  (trf-test-write-files trf-test-root (list trf-test-record-change))
  (let (runs)
    (trf-test-with-machines runs 'now
      (trf-test-implement))
    ;; F-1 §Produces: Ask the IDE to implement the record changes
    (assert-equal '("brainstorming machine" "planning machine" "executing machine"
                    "reconciliation assessor")
                  (mapcar #'car runs))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-match "Handed off by brainstorming machine\\." (nth 3 (nth 1 runs)))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-match "Handed off by planning machine\\." (nth 3 (nth 2 runs)))
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

(provide 'trf-test)
;;; trf-test.el ends here
