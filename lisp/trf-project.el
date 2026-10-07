;;; trf-project.el --- The target product's projections -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)

(defconst trf-project--projection-regexp
  "^- \\*\\*\\([^*\n]+\\)\\*\\*[^`\n]*`\\([^`\n]+\\)\\.org`"
  "A projection line of AGENTS.md; group 1 is its name, group 2 its file without \".org\".")

(defconst trf-project--vision-name "Vision"
  "The name AGENTS.md gives the vision's projection.")

(defconst trf-project--ubiquitous-language-name "Ubiquitous language"
  "The name AGENTS.md gives the ubiquitous language's projection.")

(defconst trf-project--vision-and-ubiquitous-language-names
  (list trf-project--vision-name trf-project--ubiquitous-language-name)
  "The names AGENTS.md gives the vision's and the ubiquitous language's projections.")

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

(defun trf-project-vision-and-ubiquitous-language-paths (root)
  "Return the files of the vision and the ubiquitous language named by ROOT's AGENTS.md.
Each is relative to ROOT; a file AGENTS.md does not name is left out."
  (let ((projections (trf-project--projections root)))
    (delq nil (mapcar (lambda (name)
                        (when-let* ((projection (assoc-string name projections t)))
                          (concat (cdr projection) ".org")))
                      trf-project--vision-and-ubiquitous-language-names))))

(defun trf-project-projection-files (root)
  "Return (FILE . DIRECTORY) for each projection with a directory of its records.
FILE is the projection's file and DIRECTORY the directory of its records,
each relative to ROOT."
  (mapcar (lambda (projection)
            (cons (concat (cdr projection) ".org") (concat (cdr projection) "/")))
          (seq-remove (lambda (projection)
                        (member-ignore-case (car projection)
                                            trf-project--vision-and-ubiquitous-language-names))
                      (trf-project--projections root))))

(defun trf-project-record-directories (root)
  "Return the directory of each projection's records, relative to ROOT."
  (mapcar #'cdr (trf-project-projection-files root)))

(provide 'trf-project)
;;; trf-project.el ends here
