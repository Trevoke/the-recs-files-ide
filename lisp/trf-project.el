;;; trf-project.el --- The target product's projections -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)

(defconst trf-project--projection-regexp
  "^- \\*\\*\\([^*\n]+\\)\\*\\*[^`\n]*`\\([^`\n]+\\)\\.org`"
  "A projection line of AGENTS.md; group 1 is its name, group 2 its file without \".org\".")

(defconst trf-project--vision-name "Vision"
  "The name AGENTS.md gives the vision's projection.")

(defun trf-project-root (&optional directory)
  "Return the root of the target product holding DIRECTORY, or nil."
  (when-let* ((root (locate-dominating-file (or directory default-directory)
                                            "AGENTS.md")))
    (file-name-as-directory (expand-file-name root))))

(defun trf-project--projections (root)
  "Return (NAME . STEM) for each projection named by ROOT's AGENTS.md.
STEM is the projection's file relative to ROOT, without \".org\"."
  (let ((agents-md (expand-file-name "AGENTS.md" root))
        projections)
    (when (file-exists-p agents-md)
      (with-temp-buffer
        (insert-file-contents agents-md)
        (goto-char (point-min))
        (while (re-search-forward trf-project--projection-regexp nil t)
          (push (cons (match-string 1) (match-string 2)) projections))))
    (nreverse projections)))

(defun trf-project-vision-path (root)
  "Return the vision's file named by ROOT's AGENTS.md, relative to ROOT, or nil."
  (when-let* ((vision (assoc trf-project--vision-name (trf-project--projections root))))
    (concat (cdr vision) ".org")))

(defun trf-project-record-directories (root)
  "Return the directory of each projection's records, relative to ROOT."
  (mapcar (lambda (projection) (concat (cdr projection) "/"))
          (seq-remove (lambda (projection) (equal (car projection) trf-project--vision-name))
                      (trf-project--projections root))))

(provide 'trf-project)
;;; trf-project.el ends here
