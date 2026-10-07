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

(defun trf-test-write-files (root files)
  "Write FILES, an alist of (path . content), under ROOT."
  (dolist (file files)
    (let ((path (expand-file-name (car file) root)))
      (make-directory (file-name-directory path) t)
      (with-temp-file path (insert (cdr file))))))

(defun trf-test-make-target-product (&optional files)
  "Create a committed target product holding AGENTS.md and FILES; return its root."
  (let ((root (file-name-as-directory (make-temp-file "trf-target-" t))))
    (condition-case err
        (progn
          (trf-test-write-files root (cons (cons "AGENTS.md" trf-test-agents-md)
                                           (or files trf-test-records)))
          (trf-test-git root "init" "--quiet" "--initial-branch=main")
          (trf-test-git root "config" "user.email" "record.keeper@example.com")
          (trf-test-git root "config" "user.name" "Record Keeper")
          (trf-test-git root "add" "--all")
          (trf-test-git root "commit" "--quiet" "--message" "Start"))
      (error
       (delete-directory root t)
       (signal (car err) (cdr err))))
    root))

(defun trf-test-head (root)
  "Return the commit at HEAD in ROOT."
  (string-trim (trf-test-git root "rev-parse" "HEAD")))

(provide 'trf-test-helper)
;;; trf-test-helper.el ends here
