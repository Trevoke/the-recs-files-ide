;;; trf-project.el --- The target product's projections -*- lexical-binding: t; -*-

(require 'subr-x)

(defconst trf-project--projection-regexp
  "^- \\*\\*[^*\n]+\\*\\*[^`\n]*`\\([^`\n]+\\)\\.org`"
  "A projection line of AGENTS.md; group 1 is its file without \".org\".")

(defun trf-project-root (&optional directory)
  "Return the root of the target product holding DIRECTORY, or nil."
  (when-let* ((root (locate-dominating-file (or directory default-directory)
                                            "AGENTS.md")))
    (file-name-as-directory (expand-file-name root))))

(defun trf-project-projection-paths (root)
  "Return the projection paths named by ROOT's AGENTS.md, relative to ROOT.
Each projection gives its file, then the directory of its records."
  (let ((agents-md (expand-file-name "AGENTS.md" root))
        paths)
    (when (file-exists-p agents-md)
      (with-temp-buffer
        (insert-file-contents agents-md)
        (goto-char (point-min))
        (while (re-search-forward trf-project--projection-regexp nil t)
          (let ((stem (match-string 1)))
            (push (concat stem ".org") paths)
            (push (concat stem "/") paths)))))
    (nreverse paths)))

(provide 'trf-project)
;;; trf-project.el ends here
