;;; trf-git.el --- Git access for trf -*- lexical-binding: t; -*-

(require 'subr-x)

(defun trf-git (directory &rest args)
  "Run git with ARGS in DIRECTORY and return its output.
Signal an error carrying git's output when git fails."
  (with-temp-buffer
    (let* ((default-directory directory)
           (status (apply #'process-file "git" nil t nil args)))
      (unless (eq status 0)
        (error "git %s: %s" (string-join args " ") (string-trim (buffer-string))))
      (buffer-string))))

(defun trf-git-lines (directory &rest args)
  "Run git with ARGS in DIRECTORY and return its non-empty output lines."
  (split-string (apply #'trf-git directory args) "\n" t))

(defun trf-git-ref-commit (directory ref)
  "Return the commit REF names in DIRECTORY, or nil when REF does not exist."
  (with-temp-buffer
    (let ((default-directory directory))
      (when (eq 0 (process-file "git" nil t nil "rev-parse" "--verify" "--quiet"
                                (concat ref "^{commit}")))
        (string-trim (buffer-string))))))

(provide 'trf-git)
;;; trf-git.el ends here
