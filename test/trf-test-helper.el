;;; trf-test-helper.el --- Fixtures for trf tests -*- lexical-binding: t; -*-

(require 'subr-x)

(setenv "GIT_CONFIG_GLOBAL" "/dev/null")
(setenv "GIT_CONFIG_NOSYSTEM" "1")

(defconst trf-test-agents-md
  (concat
   "- **Vision** — `docs/vision.org`, a single file\n"
   "- **Ubiquitous language** — `docs/ubiquitous-language.org`\n"
   "- **User workflows** (UW-n) — `docs/user-workflows.org`\n"
   "- **Features** (F-n) — `docs/features.org`\n"
   "\n"
   "- User workflows — `docs/knowledge-base/cockburn-use-cases.org`\n"))

(defconst trf-test-records
  '(("docs/vision.org" . "#+TITLE: Vision\n\nWhat it is for.\n")
    ("docs/ubiquitous-language.org"
     . "#+TITLE: Ubiquitous Language\n\n* Records\n\n- widget :: a thing a buyer orders.\n")
    ("docs/user-workflows.org" . "#+TITLE: User Workflows\n\n- UW-1 :: Order a widget\n")
    ("docs/user-workflows/uw-1-order-a-widget.org" . "* UW-1 Order a widget\n\n1. Buyer orders.\n")
    ("src/widget.el" . "(provide 'widget)\n")))

(defun trf-test-git (directory &rest args)
  "Run git with ARGS in DIRECTORY; return its output, or signal on failure."
  (with-temp-buffer
    (let ((default-directory directory))
      (unless (eq 0 (apply #'call-process "git" nil t nil args))
        (error "git %S: %s" args (buffer-string)))
      (buffer-string))))

(defun trf-test-git-expecting (directory expected &rest args)
  "Run git with ARGS in DIRECTORY; signal unless it exits with status EXPECTED."
  (with-temp-buffer
    (let* ((default-directory directory)
           (process-environment (cons "GIT_EDITOR=true" process-environment))
           (status (apply #'call-process "git" nil t nil args)))
      (unless (eq status expected)
        (error "git %S exited %s, not %s: %s" args status expected (buffer-string))))))

(defun trf-test-diverge (root)
  "Commit conflicting edits to src/widget.el in ROOT, on main and on a branch \"other\".
\"other\" then adds src/other.el in a commit of its own."
  (trf-test-git root "checkout" "--quiet" "-b" "other")
  (trf-test-write-files root '(("src/widget.el" . "(provide 'widget) ; other\n")))
  (trf-test-git root "commit" "--quiet" "--all" "--message" "Other")
  (trf-test-write-files root '(("src/other.el" . "(provide 'other)\n")))
  (trf-test-git root "add" "src/other.el")
  (trf-test-git root "commit" "--quiet" "--message" "Other file")
  (trf-test-git root "checkout" "--quiet" "main")
  (trf-test-write-files root '(("src/widget.el" . "(provide 'widget) ; main\n")))
  (trf-test-git root "commit" "--quiet" "--all" "--message" "Main"))

(defconst trf-test-operations
  '((merge "You have unmerged paths\\." (1 "merge" "other"))
    (rebase "^interactive rebase in progress" (1 "rebase" "other"))
    (rebase-apply "^rebase in progress" (1 "rebase" "--apply" "other"))
    (cherry-pick "You are currently cherry-picking" (1 "cherry-pick" "other~1"))
    (cherry-pick-sequence "Cherry-pick currently in progress"
                          (1 "cherry-pick" "other~1" "other")
                          (0 "checkout" "other~1" "--" "src/widget.el")
                          (0 "commit" "--quiet"))
    (revert "You are currently reverting" (0 "revert" "--no-commit" "HEAD"))
    (bisect "You are currently bisecting" (0 "bisect" "start")))
  "Each operation git leaves in progress: what git status says of it, and the
git commands, each with the exit status it gives, that leave it in a diverged ROOT.")

(defun trf-test-leave-in-progress (root operation)
  "Leave OPERATION, a key of `trf-test-operations', in progress in ROOT.
Signal unless git then says it is in progress."
  (pcase-let ((`(,banner . ,commands) (cdr (assq operation trf-test-operations))))
    (trf-test-diverge root)
    (dolist (command commands)
      (apply #'trf-test-git-expecting root (car command) (cdr command)))
    (let ((status (let ((process-environment (cons "LC_ALL=C" process-environment)))
                    (trf-test-git root "status"))))
      (unless (string-match-p banner status)
        (error "git status does not say %s is in progress: %s" operation status)))))

(defun trf-test-under-the-temporary-directory (root)
  "Signal unless ROOT is an absolute directory under `temporary-file-directory'."
  (unless (and (stringp root)
               (file-name-absolute-p root)
               (string-prefix-p (file-name-as-directory (file-truename temporary-file-directory))
                                (file-name-as-directory (file-truename root))))
    (error "Not a directory under %s: %S" temporary-file-directory root)))

(defvar trf-test-saved-directory nil
  "The `default-directory' a test found, to restore once it is over.")

(defun trf-test-enter-a-temporary-directory ()
  "Make `default-directory' the temporary directory for the test about to run."
  (setq trf-test-saved-directory default-directory
        default-directory (file-name-as-directory temporary-file-directory)))

(defun trf-test-leave-the-temporary-directory ()
  "Give back the `default-directory' the test found."
  (when trf-test-saved-directory
    (setq default-directory trf-test-saved-directory
          trf-test-saved-directory nil)))

(defun trf-test-write-files (root files)
  "Write FILES, an alist of (path . content), under ROOT, a directory under the temporary directory."
  (trf-test-under-the-temporary-directory root)
  (dolist (file files)
    (let ((path (expand-file-name (car file) root)))
      (make-directory (file-name-directory path) t)
      (with-temp-file path (insert (cdr file))))))

(defun trf-test-make-target-product (&optional files no-commit)
  "Create a target product holding AGENTS.md and FILES; return its root.
Commit them unless NO-COMMIT."
  (let ((root (file-name-as-directory (make-temp-file "trf-target-" t))))
    (condition-case err
        (progn
          (trf-test-write-files root (cons (cons "AGENTS.md" trf-test-agents-md)
                                           (or files trf-test-records)))
          (trf-test-git root "init" "--quiet" "--initial-branch=main")
          (trf-test-git root "config" "user.email" "record.keeper@example.com")
          (trf-test-git root "config" "user.name" "Record Keeper")
          (unless no-commit
            (trf-test-git root "add" "--all")
            (trf-test-git root "commit" "--quiet" "--message" "Start")))
      (error
       (delete-directory root t)
       (signal (car err) (cdr err))))
    root))

(defun trf-test-head (root)
  "Return the commit at HEAD in ROOT."
  (string-trim (trf-test-git root "rev-parse" "HEAD")))

(provide 'trf-test-helper)
;;; trf-test-helper.el ends here
