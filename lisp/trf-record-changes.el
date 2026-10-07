;;; trf-record-changes.el --- The record changes -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'trf-git)
(require 'trf-project)

(defconst trf-record-changes-implemented-ref "refs/recs/implemented")

(defconst trf-record-changes--empty-tree "4b825dc642cb6eb9a060e54bf8d69288fbee4904")

(defun trf-record-changes--base (root)
  "Return what ROOT's record changes are measured from."
  (or (trf-git-ref-commit root trf-record-changes-implemented-ref)
      trf-record-changes--empty-tree))

(defun trf-record-changes--heading (text path)
  "Return (IDENTIFIER . TITLE) for the record TEXT at PATH."
  (cond ((string-match "^\\* \\([A-Z]+-[0-9]+\\) \\(.+\\)$" text)
         (cons (match-string 1 text) (match-string 2 text)))
        ((string-match "^#\\+TITLE: \\(.+\\)$" text)
         (cons nil (match-string 1 text)))
        (t (cons nil (file-name-base path)))))

(defun trf-record-changes--describe (root base path status)
  "Return the record change at PATH in ROOT with git STATUS, measured from BASE."
  (let* ((text (if (equal status "D")
                   (trf-git root "show" (concat base ":" path))
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name path root))
                   (buffer-string))))
         (heading (trf-record-changes--heading text path)))
    (list :path path
          :status (pcase status ("A" 'added) ("D" 'deleted) (_ 'modified))
          :identifier (car heading)
          :title (cdr heading))))

;; F-1 §Takes: Ask the IDE to implement the record changes
(defun trf-record-changes (root)
  "Return the record changes of the target product at ROOT, by path."
  (when-let* ((paths (trf-project-projection-paths root)))
    (let* ((base (trf-record-changes--base root))
           (changed (mapcar (lambda (line)
                              (let ((fields (split-string line "\t")))
                                (cons (car (last fields)) (substring (car fields) 0 1))))
                            (apply #'trf-git-lines root "diff" "--name-status"
                                   "--no-renames" base "--" paths)))
           (untracked (mapcar (lambda (path) (cons path "A"))
                              (apply #'trf-git-lines root "ls-files" "--others"
                                     "--exclude-standard" "--" paths))))
      (mapcar (lambda (entry)
                (trf-record-changes--describe root base (car entry) (cdr entry)))
              (sort (append changed untracked)
                    (lambda (a b) (string< (car a) (car b))))))))

(provide 'trf-record-changes)
;;; trf-record-changes.el ends here
