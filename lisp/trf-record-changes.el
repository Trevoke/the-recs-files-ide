;;; trf-record-changes.el --- The record changes -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)
(require 'trf-git)
(require 'trf-project)

(defconst trf-record-changes-implemented-ref "refs/recs/implemented")

(defun trf-record-changes--base (root)
  "Return what ROOT's record changes are measured from."
  (or (trf-git-ref-commit root trf-record-changes-implemented-ref)
      (string-trim (trf-git root "hash-object" "-t" "tree" "/dev/null"))))

(defun trf-record-changes--heading (text path)
  "Return (IDENTIFIER . TITLE) for the record TEXT at PATH."
  (cond ((let ((case-fold-search nil))
           (string-match "^\\* \\([A-Z]+-[0-9]+\\) \\(.+\\)$" text))
         (cons (match-string 1 text) (match-string 2 text)))
        ((let ((case-fold-search t))
           (string-match "^#\\+TITLE: \\(.+\\)$" text))
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

(defun trf-record-changes--pairs (fields)
  "Return the alternating status and path FIELDS as (PATH . STATUS) pairs."
  (let (pairs)
    (while fields
      (push (cons (cadr fields) (substring (car fields) 0 1)) pairs)
      (setq fields (cddr fields)))
    (nreverse pairs)))

;; F-1 §Takes: Ask the IDE to implement the record changes
(defun trf-record-changes (root)
  "Return the record changes of the target product at ROOT, by path."
  (when-let* ((paths (trf-project-projection-paths root)))
    (let* ((coding-system-for-read 'utf-8)
           (coding-system-for-write 'utf-8)
           (base (trf-record-changes--base root))
           (changed (trf-record-changes--pairs
                     (split-string (apply #'trf-git root "diff" "--name-status" "-z"
                                          "--no-renames" base "--" paths)
                                   "\0" t)))
           (untracked (mapcar (lambda (path) (cons path "A"))
                              (split-string (apply #'trf-git root "ls-files" "-z" "--others"
                                                   "--exclude-standard" "--" paths)
                                            "\0" t)))
           (entries (append (mapcar (lambda (entry)
                                      (if (assoc (car entry) untracked)
                                          (cons (car entry) "M")
                                        entry))
                                    changed)
                            (seq-remove (lambda (entry) (assoc (car entry) changed))
                                        untracked))))
      (mapcar (lambda (entry)
                (trf-record-changes--describe root base (car entry) (cdr entry)))
              (sort entries (lambda (a b) (string< (car a) (car b))))))))

(provide 'trf-record-changes)
;;; trf-record-changes.el ends here
