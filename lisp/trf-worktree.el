;;; trf-worktree.el --- The worktree the implementation workflow runs in -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'trf-git)
(require 'trf-record-changes)

(define-error 'trf-worktree-merge-failed "Git refused the merge" 'trf-git-error)

;; UW-1 §Main success scenario: Turn record changes into code
;; ADR-7 §Decision: Run the implementation workflow in a git worktree of the target product
(defconst trf-worktree-directory ".worktrees/"
  "Where the worktrees of the works in progress sit, under the target product's root.")

;; ADR-7 §Decision: Run the implementation workflow in a git worktree of the target product
(defconst trf-worktree-state-directory ".trf/"
  "Where a worktree keeps the IDE's own files about its work in progress.")

;; ADR-7 §Decision: Run the implementation workflow in a git worktree of the target product
(defun trf-worktree--exclude (root pattern)
  "Make git in ROOT ignore PATTERN, through the repository's info/exclude."
  (let ((exclude (expand-file-name
                  "info/exclude"
                  (string-trim (trf-git root "rev-parse" "--path-format=absolute"
                                        "--git-common-dir")))))
    (make-directory (file-name-directory exclude) t)
    (with-temp-buffer
      (when (file-exists-p exclude)
        (insert-file-contents-literally exclude))
      (goto-char (point-min))
      (unless (re-search-forward (concat "^" (regexp-quote pattern) "\r?$") nil t)
        (let ((separator (if (or (= (point-min) (point-max))
                                 (eq (char-before (point-max)) ?\n))
                             ""
                           "\n")))
          (erase-buffer)
          (insert separator pattern "\n")
          (let ((coding-system-for-write 'utf-8-unix))
            (append-to-file (point-min) (point-max) exclude)))))))

(defun trf-worktree--write (directory file content)
  "Write CONTENT to FILE under DIRECTORY's state directory."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file (expand-file-name (concat trf-worktree-state-directory file) directory)
      (insert content "\n"))))

(defun trf-worktree--read (directory file)
  "Return the content of FILE under DIRECTORY's state directory."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents (expand-file-name (concat trf-worktree-state-directory file) directory)))
    (string-trim (buffer-string))))

;; UW-1 §Main success scenario: Turn record changes into code
;; F-1 §Rules: Ask the IDE to implement the record changes
;; ADR-7 §Decision: Run the implementation workflow in a git worktree of the target product
(defun trf-worktree-create (root commit)
  "Create a worktree of ROOT starting from COMMIT; return its directory."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (from (trf-record-changes-from root))
         (name (make-temp-name (format-time-string "trf-%Y%m%d-%H%M%S-")))
         (branch (concat "trf/" name))
         (directory (file-name-as-directory
                     (expand-file-name (concat trf-worktree-directory name) root))))
    (trf-worktree--exclude root (concat "/" trf-worktree-directory))
    (trf-worktree--exclude root (concat "/" trf-worktree-state-directory))
    (trf-git root "worktree" "add" "--quiet" "-b" branch directory commit)
    (make-directory (expand-file-name trf-worktree-state-directory directory) t)
    (trf-worktree--write directory "root" root)
    (trf-worktree--write directory "base" commit)
    (trf-worktree--write directory "branch" branch)
    (trf-worktree--write directory "from" from)
    directory))

(defun trf-worktree-of (file)
  "Return the worktree holding FILE, or nil."
  (when-let* ((directory (locate-dominating-file
                          file
                          (lambda (directory)
                            (file-regular-p (expand-file-name (concat trf-worktree-state-directory "root") directory))))))
    (file-name-as-directory (expand-file-name directory))))

(defun trf-worktree-root (directory)
  "Return the target product's root for the worktree DIRECTORY."
  (trf-worktree--read directory "root"))

(defun trf-worktree-base (directory)
  "Return the commit of the record changes the worktree DIRECTORY started from."
  (trf-worktree--read directory "base"))

;; UW-1 §Extensions 7a: Turn record changes into code
(defun trf-worktree-from (directory)
  "Return the commit or tree DIRECTORY's record changes were measured from, or nil when unknown."
  (when (file-exists-p (expand-file-name (concat trf-worktree-state-directory "from") directory))
    (trf-worktree--read directory "from")))

(defun trf-worktree-branch (directory)
  "Return the branch the worktree DIRECTORY was created on."
  (trf-worktree--read directory "branch"))

(defun trf-worktree--branch-exists-p (directory)
  "Return non-nil while DIRECTORY's branch exists."
  (trf-git-ref-commit (trf-worktree-root directory)
                      (concat "refs/heads/" (trf-worktree-branch directory))))

;; F-2 §Produces: Accept or reject the record implementation report
(defun trf-worktree--work (directory)
  "Return DIRECTORY's branch, or DIRECTORY's HEAD commit once the branch is gone."
  (if (trf-worktree--branch-exists-p directory)
      (trf-worktree-branch directory)
    (trf-git-ref-commit directory "HEAD")))

;; UW-1 §Extensions 7a: Turn record changes into code
(defun trf-worktree-codebase-moved-p (directory)
  "Return non-nil when the codebase has moved since DIRECTORY's base."
  (not (trf-git-check directory "merge-base" "--is-ancestor"
                      (trf-git-ref-commit (trf-worktree-root directory) "HEAD")
                      (trf-worktree--work directory))))

;; UW-1 §Main success scenario: Turn record changes into code
(defun trf-worktree-brought-in-p (directory)
  "Return non-nil when DIRECTORY's work holds commits and the codebase holds that work."
  (let ((root (trf-worktree-root directory))
        (work (trf-worktree--work directory)))
    (and (not (equal (trf-git-ref-commit root work) (trf-worktree-base directory)))
         (trf-git-check root "merge-base" "--is-ancestor" work "HEAD"))))

;; UW-1 §Success guarantee: Turn record changes into code
(defun trf-worktree-mark-implemented (directory)
  "Mark DIRECTORY's record changes implemented, moving the implemented records only forward."
  (let* ((root (trf-worktree-root directory))
         (base (trf-worktree-base directory))
         (implemented (trf-git-ref-commit root trf-record-changes-implemented-ref)))
    (when (or (not implemented)
              (trf-git-check root "merge-base" "--is-ancestor" implemented base))
      (trf-git root "update-ref" trf-record-changes-implemented-ref base))))

;; UW-1 §Main success scenario: Turn record changes into code
(defun trf-worktree-accept (directory)
  "Bring DIRECTORY's work into the codebase and mark its record changes implemented."
  (let* ((root (trf-worktree-root directory))
         (merging (trf-git-ref-commit root "MERGE_HEAD")))
    (condition-case err
        (trf-git root "merge" "--quiet" "--no-ff" "--no-edit"
                 (trf-worktree--work directory))
      (error
       (when (and (not merging) (trf-git-ref-commit root "MERGE_HEAD"))
         (ignore-errors (trf-git root "merge" "--abort")))
       (signal (if (eq (car err) 'trf-git-error) 'trf-worktree-merge-failed (car err))
               (cdr err))))
    (trf-worktree-mark-implemented directory)))

;; F-2 §Rules: Accept or reject the record implementation report
(defun trf-worktree-commit-saved-edits (directory)
  "Commit to DIRECTORY's work whatever is saved in DIRECTORY and not yet committed."
  (trf-git directory "add" "--all")
  (unless (trf-git-check directory "diff" "--cached" "--quiet")
    (trf-git directory "commit" "--quiet" "--message"
             (format "Save the record keeper's edits to %s" (trf-worktree-branch directory)))))

;; F-2 §Produces: Accept or reject the record implementation report
;; docs/knowledge-base/git-worktree-removal.org §git worktree remove removes a worktree holding only excluded or ignored files
;; docs/knowledge-base/git-worktree-removal.org §git worktree remove refuses a worktree holding untracked or modified files
;; docs/knowledge-base/git-worktree-removal.org §git branch -d refuses a branch checked out in a worktree
;; docs/knowledge-base/git-worktree-removal.org §git branch -d deletes a merged branch once its worktree is detached
(defun trf-worktree-remove (directory)
  "Remove the worktree DIRECTORY and its branch from its root."
  (let ((root (trf-worktree-root directory))
        (branch (trf-worktree-branch directory)))
    (trf-git directory "checkout" "--quiet" "--detach")
    (when (trf-worktree--branch-exists-p directory)
      (trf-git root "branch" "--quiet" "-d" branch))
    (trf-git root "worktree" "remove" directory)))

(provide 'trf-worktree)
;;; trf-worktree.el ends here
