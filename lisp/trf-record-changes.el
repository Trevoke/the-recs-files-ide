;;; trf-record-changes.el --- The record changes -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)
(require 'trf-git)
(require 'trf-project)

(defconst trf-record-changes-implemented-ref "refs/recs/implemented")

(defun trf-record-changes--empty-tree (root)
  "Return the tree holding nothing in ROOT."
  (string-trim (trf-git root "hash-object" "-t" "tree" "/dev/null")))

(defun trf-record-changes--base (root)
  "Return what ROOT's record changes are measured from."
  (or (trf-git-ref-commit root trf-record-changes-implemented-ref)
      (trf-record-changes--empty-tree root)))

(defun trf-record-changes--git (root &rest args)
  "Run git with ARGS in ROOT, reading each pathspec as a literal path."
  (apply #'trf-git root "--literal-pathspecs" args))

(defun trf-record-changes--git-paths (root &rest args)
  "Run git with ARGS in ROOT, reading each pathspec as a literal path; return its paths."
  (apply #'trf-git-paths root "--literal-pathspecs" args))

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

(defun trf-record-changes--untracked (root paths)
  "Return the files under PATHS in ROOT that git does not track."
  (apply #'trf-record-changes--git-paths root
         "ls-files" "-z" "--others" "--exclude-standard" "--" paths))

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
                             (trf-project-vision-and-ubiquitous-language-paths root))))
    (let* ((base (trf-record-changes--base root))
           (changed (trf-record-changes--pairs
                     (apply #'trf-record-changes--git-paths root "diff" "--name-status" "-z"
                            "--no-renames" base "--" paths)))
           (untracked (mapcar (lambda (path) (cons path "A"))
                              (trf-record-changes--untracked root paths)))
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
  (if (null changes)
      (trf-git-ref-commit root "HEAD")
    (let* ((head (trf-git-ref-commit root "HEAD"))
           (paths (mapcar (lambda (change) (plist-get change :path)) changes))
           (untracked (trf-record-changes--untracked root paths)))
      (when untracked
        (apply #'trf-record-changes--git root "add" "--" untracked))
      (condition-case err
          (when-let* ((uncommitted (apply #'trf-record-changes--git-paths root
                                          "diff" "--name-only" "-z" "--no-renames"
                                          (or head (trf-record-changes--empty-tree root))
                                          "--" paths)))
            (apply #'trf-record-changes--git root "commit" "--quiet" "--only" "--message"
                   (trf-record-changes--commit-message
                    (seq-filter (lambda (change)
                                  (member (plist-get change :path) uncommitted))
                                changes))
                   "--" uncommitted))
        (error
         (when untracked
           (ignore-errors
             (apply #'trf-record-changes--git root
                    "rm" "--cached" "--force" "--quiet" "--" untracked)))
         (signal (car err) (cdr err))))
      (trf-git-ref-commit root "HEAD"))))

(provide 'trf-record-changes)
;;; trf-record-changes.el ends here
