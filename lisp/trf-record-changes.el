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
  (when-let* ((paths (append (trf-project-record-directories root)
                             (ensure-list (trf-project-vision-path root)))))
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

(defun trf-record-changes--commit-message (changes)
  "Return the commit message naming each of CHANGES."
  (concat "Record changes\n\n"
          (mapconcat (lambda (change)
                       (concat "- " (string-join
                                     (delq nil (list (plist-get change :identifier)
                                                     (plist-get change :title)))
                                     " ")))
                     changes "\n")
          "\n"))

;; F-1 §Rules: Ask the IDE to implement the record changes
(defun trf-record-changes-commit (root changes)
  "Commit CHANGES in ROOT and nothing else; return the commit they are in."
  (let* ((coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (indexed (when changes
                    (split-string (apply #'trf-git root "ls-files" "-z" "--"
                                         (mapcar (lambda (change) (plist-get change :path))
                                                 changes))
                                  "\0" t)))
         (changes (seq-filter (lambda (change)
                                (let ((path (plist-get change :path)))
                                  (or (member path indexed)
                                      (file-exists-p (expand-file-name path root)))))
                              changes))
         (paths (mapcar (lambda (change) (plist-get change :path)) changes)))
    (when paths
      (apply #'trf-git root "add" "--all" "--" paths)
      (unless (string-empty-p (apply #'trf-git root "diff" "--cached" "--name-only" "-z"
                                     "HEAD" "--" paths))
        (apply #'trf-git root "commit" "--quiet" "--only" "--message"
               (trf-record-changes--commit-message changes) "--" paths)))
    (string-trim (trf-git root "rev-parse" "HEAD"))))

(provide 'trf-record-changes)
;;; trf-record-changes.el ends here
