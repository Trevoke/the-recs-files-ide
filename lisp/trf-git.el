;;; trf-git.el --- Git access for trf -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)

(define-error 'trf-git-error "Git failed")

(defun trf-git--run (directory args)
  "Run git with ARGS in DIRECTORY.
Return a list of its exit status, standard output and standard error."
  (let ((stderr-file (make-temp-file "trf-git-stderr-")))
    (unwind-protect
        (with-temp-buffer
          (let* ((default-directory directory)
                 (coding-system-for-read 'utf-8-unix)
                 (coding-system-for-write 'utf-8-unix)
                 (status (apply #'process-file "git" nil (list t stderr-file) nil args)))
            (list status
                  (buffer-string)
                  (with-temp-buffer
                    (insert-file-contents stderr-file)
                    (buffer-string)))))
      (delete-file stderr-file))))

(defun trf-git--output (stdout stderr)
  "Return git's own account of a failure: STDERR, or STDOUT when STDERR is empty."
  (string-trim (if (string-empty-p (string-trim stderr)) stdout stderr)))

(defun trf-git (directory &rest args)
  "Run git with ARGS in DIRECTORY and return its standard output.
When git fails, signal `trf-git-error' carrying git's standard error,
or its standard output when that is empty."
  (pcase-let ((`(,status ,stdout ,stderr) (trf-git--run directory args)))
    (unless (eq status 0)
      (signal 'trf-git-error (list (trf-git--output stdout stderr))))
    stdout))

(defun trf-git-check (directory &rest args)
  "Run git with ARGS in DIRECTORY; return t when it exits 0, nil when it exits 1.
On any other exit, signal `trf-git-error' carrying git's standard error,
or its standard output when that is empty."
  (pcase-let ((`(,status ,stdout ,stderr) (trf-git--run directory args)))
    (pcase status
      (0 t)
      (1 nil)
      (_ (signal 'trf-git-error (list (trf-git--output stdout stderr)))))))

(defun trf-git-lines (directory &rest args)
  "Run git with ARGS in DIRECTORY and return its non-empty output lines."
  (split-string (apply #'trf-git directory args) "\n" t))

(defun trf-git-paths (directory &rest args)
  "Run git with ARGS in DIRECTORY and return its NUL-separated output fields."
  (split-string (apply #'trf-git directory args) "\0" t))

(defun trf-git-ref-commit (directory ref)
  "Return the commit REF names in DIRECTORY, or nil when REF does not exist."
  (pcase-let ((`(,status ,stdout ,_stderr)
               (trf-git--run directory (list "rev-parse" "--verify" "--quiet"
                                             (concat ref "^{commit}")))))
    (when (eq status 0)
      (string-trim stdout))))

(defun trf-git-current-branch (directory)
  "Return the branch checked out in DIRECTORY, or nil when it is detached."
  (let ((branch (string-trim (trf-git directory "branch" "--show-current"))))
    (unless (string-empty-p branch) branch)))

;; docs/knowledge-base/git-operations-in-progress.org §Each operation in progress leaves its own marker in the git dir
(defconst trf-git--operation-files
  '("MERGE_HEAD" "rebase-merge" "rebase-apply" "CHERRY_PICK_HEAD" "REVERT_HEAD"
    "sequencer" "BISECT_LOG" "BISECT_START")
  "The files a merge, rebase, cherry-pick, revert or bisect in progress leaves.")

;; UW-1 §Extensions 2b: Turn record changes into code
;; UW-1 §Extensions 7d: Turn record changes into code
;; docs/knowledge-base/git-operations-in-progress.org §Each operation in progress leaves its own marker in the git dir
;; docs/knowledge-base/git-operations-in-progress.org §A linked worktree's markers are in its own git dir, found with --git-path
(defun trf-git-operation-in-progress-p (root)
  "Return non-nil when ROOT is mid merge, rebase, cherry-pick, revert or bisect."
  (seq-some #'file-exists-p
            (apply #'trf-git-lines root "rev-parse" "--path-format=absolute"
                   (mapcan (lambda (file) (list "--git-path" file))
                           trf-git--operation-files))))

(provide 'trf-git)
;;; trf-git.el ends here
