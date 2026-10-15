;;; trf-record-changes.el --- The record changes -*- lexical-binding: t; -*-

(require 'seq)
(require 'subr-x)
(require 'trf-git)
(require 'trf-project)

(defconst trf-record-changes-implemented-ref "refs/recs/implemented")

;; MSG-3: Finish the merge or rebase first
(defconst trf-record-changes-operation-in-progress-message
  "The target product is in the middle of a merge or rebase; finish it first.")

;; MSG-4: Record changes could not be committed
(defconst trf-record-changes-not-committed-message
  "The record changes could not be committed:")

;; MSG-4: Record changes could not be committed
;; MSG-5: Work cannot be brought into the codebase
;; MSG-7: Git could not do what the IDE asked
;; MSG-10: Machine stopped before finishing
;; MSG-12: Machine cannot be continued
(defconst trf-record-changes-no-reason-message
  "No reason given.")

(defun trf-record-changes--empty-tree (root)
  "Return the tree holding nothing in ROOT."
  (string-trim (trf-git root "hash-object" "-t" "tree" "/dev/null")))

(defun trf-record-changes-from (root)
  "Return what ROOT's record changes are measured from: the implemented records, or the empty tree."
  (or (trf-git-ref-commit root trf-record-changes-implemented-ref)
      (trf-record-changes--empty-tree root)))

(defun trf-record-changes-reason (output)
  "Return OUTPUT, or the text saying no reason was given when it is blank."
  (if (string-blank-p output)
      trf-record-changes-no-reason-message
    output))

;; UW-1 §Extensions 2b: Turn record changes into code
;; UW-1 §Extensions 7d: Turn record changes into code
(defun trf-record-changes-refuse-operation-in-progress (root)
  "Give MSG-3: Finish the merge or rebase first, when ROOT is mid merge, rebase, cherry-pick, revert or bisect."
  (when (trf-git-operation-in-progress-p root)
    ;; MSG-3: Finish the merge or rebase first
    (user-error "%s" trf-record-changes-operation-in-progress-message)))

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

(defun trf-record-changes--describe (root base path status &optional commit)
  "Return the record change at PATH in ROOT with git STATUS, measured from BASE.
Read PATH as COMMIT holds it, or from disk when COMMIT is nil."
  (let* ((text (cond ((equal status "D")
                      (trf-git root "show" (concat base ":" path)))
                     (commit
                      (trf-git root "show" (concat commit ":" path)))
                     (t
                      (with-temp-buffer
                        (insert-file-contents (expand-file-name path root))
                        (buffer-string)))))
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
(defun trf-record-changes--record-paths (root)
  "Return the paths in ROOT that hold records."
  (append (trf-project-record-directories root)
          (trf-project-vision-and-ubiquitous-language-paths root)))

(defun trf-record-changes--describe-each (root base entries &optional commit)
  "Return the record changes for ENTRIES, (PATH . STATUS) pairs in ROOT measured from BASE, by path.
Read each as COMMIT holds it, or from disk when COMMIT is nil."
  (mapcar (lambda (entry)
            (trf-record-changes--describe root base (car entry) (cdr entry) commit))
          (sort entries (lambda (a b) (string< (car a) (car b))))))

(defun trf-record-changes--diff (root paths &rest commits)
  "Return the (PATH . STATUS) pairs of PATHS in ROOT that differ between COMMITS."
  (trf-record-changes--pairs
   (apply #'trf-record-changes--git-paths root "diff" "--name-status" "-z" "--no-renames"
          (append commits (cons "--" paths)))))

;; F-1 §Takes: Ask the IDE to implement the record changes
(defun trf-record-changes (root)
  "Return the record changes of the target product at ROOT, by path."
  (when-let* ((paths (trf-record-changes--record-paths root)))
    (let* ((base (trf-record-changes-from root))
           (changed (trf-record-changes--diff root paths base))
           (untracked (mapcar (lambda (path) (cons path "A"))
                              (trf-record-changes--untracked root paths))))
      (trf-record-changes--describe-each
       root base
       (append (mapcar (lambda (entry)
                         (if (assoc (car entry) untracked)
                             (cons (car entry) "M")
                           entry))
                       changed)
               (seq-remove (lambda (entry) (assoc (car entry) changed))
                           untracked))))))

;; UW-1 §Extensions 7a: Turn record changes into code
(defun trf-record-changes-in-commit (root from commit)
  "Return the record changes of the target product at ROOT as COMMIT holds them, by path.
Measure them from FROM, a commit or tree; when FROM is nil, from the empty tree."
  (when-let* ((paths (trf-record-changes--record-paths root)))
    (let ((base (or from (trf-record-changes--empty-tree root))))
      (trf-record-changes--describe-each
       root base (trf-record-changes--diff root paths base commit) commit))))

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
(defun trf-record-changes--projection-files (root paths)
  "Return the projection files in ROOT whose records directory holds one of PATHS."
  (delq nil (mapcar (lambda (projection)
                      (when (seq-some (lambda (path) (string-prefix-p (cdr projection) path))
                                      paths)
                        (car projection)))
                    (trf-project-projection-files root))))

(defun trf-record-changes--add (root paths untracked)
  "Add those of PATHS in ROOT that are among UNTRACKED."
  (when-let* ((adding (seq-filter (lambda (path) (member path untracked)) paths)))
    (apply #'trf-record-changes--git root "add" "--" adding)))

(defun trf-record-changes--uncommitted (root head paths)
  "Return those of PATHS in ROOT that differ from HEAD."
  (when paths
    (apply #'trf-record-changes--git-paths root
           "diff" "--name-only" "-z" "--no-renames"
           (or head (trf-record-changes--empty-tree root))
           "--" paths)))

(defun trf-record-changes--unstage (root paths)
  "Remove those of PATHS in ROOT's index from it, keeping them on disk."
  (when paths
    (ignore-errors
      (apply #'trf-record-changes--git root
             "rm" "--cached" "--force" "--quiet" "--ignore-unmatch" "--" paths))))

(defun trf-record-changes--paths (changes)
  "Return the path of each of CHANGES."
  (mapcar (lambda (change) (plist-get change :path)) changes))

;; F-1 §Rules: Ask the IDE to implement the record changes
(defun trf-record-changes--commit-staged (root changes untracked)
  "Stage CHANGES in ROOT and the projection files listing them, and commit them.
UNTRACKED are the files among them that git does not track."
  (let* ((head (trf-git-ref-commit root "HEAD"))
         (paths (trf-record-changes--paths changes)))
    (trf-record-changes--add root paths untracked)
    (let* ((uncommitted (trf-record-changes--uncommitted root head paths))
           (committed-changes (seq-filter (lambda (change)
                                            (member (plist-get change :path) uncommitted))
                                          changes)))
      (when committed-changes
        (let ((projection-files (trf-record-changes--projection-files root uncommitted)))
          (trf-record-changes--add root projection-files untracked)
          (let ((uncommitted-projection-files
                 (trf-record-changes--uncommitted root head projection-files)))
            (apply #'trf-record-changes--git root
                   "commit" "--quiet" "--only" "--message"
                   (trf-record-changes--commit-message committed-changes)
                   "--" (append uncommitted uncommitted-projection-files))))))))

;; F-1 §Rules: Ask the IDE to implement the record changes
(defun trf-record-changes-commit (root changes)
  "Commit CHANGES in ROOT and the projection files listing them, and nothing else.
Return the commit they are in."
  (trf-record-changes-refuse-operation-in-progress root)
  (when changes
    (let ((paths (trf-record-changes--paths changes))
          (head (trf-git-ref-commit root "HEAD"))
          untracked)
      ;; UW-1 §Minimal guarantee: Turn record changes into code
      (unwind-protect
          (condition-case err
              (progn
                (setq untracked (trf-record-changes--untracked
                                 root (append paths (trf-record-changes--projection-files
                                                     root paths))))
                (trf-record-changes--commit-staged root changes untracked))
            (trf-git-error
             ;; UW-1 §Extensions 2c: Turn record changes into code
             (user-error "%s\n%s" trf-record-changes-not-committed-message
                         (trf-record-changes-reason (cadr err)))))
        (when (equal head (trf-git-ref-commit root "HEAD"))
          (trf-record-changes--unstage root untracked)))))
  (trf-git-ref-commit root "HEAD"))

(provide 'trf-record-changes)
;;; trf-record-changes.el ends here
