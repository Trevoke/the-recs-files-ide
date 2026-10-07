;;; trf-git.el --- Git access for trf -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)

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

(defun trf-git (directory &rest args)
  "Run git with ARGS in DIRECTORY and return its standard output.
Signal an error carrying git's standard error, or its standard output
when that is empty, when git fails."
  (pcase-let ((`(,status ,stdout ,stderr) (trf-git--run directory args)))
    (unless (eq status 0)
      (error "git %s: %s" (string-join args " ")
             (string-trim (if (string-empty-p (string-trim stderr)) stdout stderr))))
    stdout))

(defun trf-git-check (directory &rest args)
  "Run git with ARGS in DIRECTORY; return t when it exits 0, nil when it exits 1.
Signal an error carrying git's standard error on any other exit."
  (pcase-let ((`(,status ,_stdout ,stderr) (trf-git--run directory args)))
    (pcase status
      (0 t)
      (1 nil)
      (_ (error "git %s: %s" (string-join args " ") (string-trim stderr))))))

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

(defconst trf-git--operation-files
  '("MERGE_HEAD" "rebase-merge" "rebase-apply" "CHERRY_PICK_HEAD" "REVERT_HEAD" "BISECT_LOG")
  "The files a merge, rebase, cherry-pick, revert or bisect in progress leaves.")

;; UW-1 §Extensions: Turn record changes into code
(defun trf-git-operation-in-progress-p (root)
  "Return non-nil when ROOT is mid merge, rebase, cherry-pick, revert or bisect."
  (let ((git-dir (file-name-as-directory
                  (expand-file-name (string-trim (trf-git root "rev-parse" "--git-dir"))
                                    root))))
    (seq-some (lambda (file) (file-exists-p (expand-file-name file git-dir)))
              trf-git--operation-files)))

(provide 'trf-git)
;;; trf-git.el ends here
