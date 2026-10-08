;;; trf-worktree.el --- The worktree the implementation workflow runs in -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'trf-git)
(require 'trf-record-changes)

(define-error 'trf-worktree-merge-failed "Git refused the merge" 'trf-git-error)

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
  "Write CONTENT to FILE under DIRECTORY's .trf/."
  (let ((coding-system-for-write 'utf-8-unix))
    (with-temp-file (expand-file-name (concat ".trf/" file) directory)
      (insert content "\n"))))

(defun trf-worktree--read (directory file)
  "Return the content of FILE under DIRECTORY's .trf/."
  (with-temp-buffer
    (let ((coding-system-for-read 'utf-8-unix))
      (insert-file-contents (expand-file-name (concat ".trf/" file) directory)))
    (string-trim (buffer-string))))

(defun trf-worktree--root-branch-now (root)
  "Return the branch ROOT is on, or nil when it is detached."
  (let ((branch (string-trim (trf-git root "branch" "--show-current"))))
    (unless (string-empty-p branch) branch)))

;; UW-1 §Main success scenario: Turn record changes into code
;; F-1 §Rules: Ask the IDE to implement the record changes
(defun trf-worktree-create (root commit)
  "Create a worktree of ROOT starting from COMMIT; return its directory."
  (let* ((root (file-name-as-directory (expand-file-name root)))
         (root-branch (trf-worktree--root-branch-now root))
         (from (trf-git-ref-commit root trf-record-changes-implemented-ref))
         (name (make-temp-name (format-time-string "trf-%Y%m%d-%H%M%S-")))
         (branch (concat "trf/" name))
         (directory (file-name-as-directory
                     (expand-file-name (concat ".worktrees/" name) root))))
    (trf-worktree--exclude root "/.worktrees/")
    (trf-worktree--exclude root "/.trf/")
    (trf-git root "worktree" "add" "--quiet" "-b" branch directory commit)
    (make-directory (expand-file-name ".trf/" directory) t)
    (trf-worktree--write directory "root" root)
    (trf-worktree--write directory "base" commit)
    (trf-worktree--write directory "branch" branch)
    (when root-branch
      (trf-worktree--write directory "root-branch" root-branch))
    (when from
      (trf-worktree--write directory "from" from))
    directory))

(defun trf-worktree-of (file)
  "Return the worktree holding FILE, or nil."
  (when-let* ((directory (locate-dominating-file
                          file
                          (lambda (directory)
                            (file-regular-p (expand-file-name ".trf/root" directory))))))
    (file-name-as-directory (expand-file-name directory))))

(defun trf-worktree-root (directory)
  "Return the target product's root for the worktree DIRECTORY."
  (trf-worktree--read directory "root"))

(defun trf-worktree-base (directory)
  "Return the commit of the record changes the worktree DIRECTORY started from."
  (trf-worktree--read directory "base"))

;; UW-1 §Extensions: Turn record changes into code
(defun trf-worktree-from (directory)
  "Return the commit DIRECTORY's record changes were measured from, or nil for nothing."
  (when (file-exists-p (expand-file-name ".trf/from" directory))
    (trf-worktree--read directory "from")))

(defun trf-worktree-branch (directory)
  "Return the branch the worktree DIRECTORY was created on."
  (trf-worktree--read directory "branch"))

;; UW-1 §Extensions: Turn record changes into code
(defun trf-worktree-root-on-its-branch-p (directory)
  "Return non-nil when DIRECTORY's root is on the branch it was on when DIRECTORY was created."
  (when (file-exists-p (expand-file-name ".trf/root-branch" directory))
    (equal (trf-worktree--read directory "root-branch")
           (trf-worktree--root-branch-now (trf-worktree-root directory)))))

;; UW-1 §Extensions: Turn record changes into code
(defun trf-worktree-codebase-moved-p (directory)
  "Return non-nil when the codebase has moved since DIRECTORY's base."
  (not (trf-git-check directory "merge-base" "--is-ancestor"
                      (trf-git-ref-commit (trf-worktree-root directory) "HEAD")
                      (trf-worktree-branch directory))))

;; UW-1 §Main success scenario: Turn record changes into code
(defun trf-worktree-accept (directory)
  "Bring DIRECTORY's work into the codebase and mark its record changes implemented."
  (let* ((root (trf-worktree-root directory))
         (merging (trf-git-ref-commit root "MERGE_HEAD")))
    (condition-case err
        (trf-git root "merge" "--quiet" "--no-ff" "--no-edit"
                 (trf-worktree-branch directory))
      (error
       (when (and (not merging) (trf-git-ref-commit root "MERGE_HEAD"))
         (ignore-errors (trf-git root "merge" "--abort")))
       (signal (if (eq (car err) 'trf-git-error) 'trf-worktree-merge-failed (car err))
               (cdr err))))
    (trf-git root "update-ref" trf-record-changes-implemented-ref
             (trf-worktree-base directory))))

(provide 'trf-worktree)
;;; trf-worktree.el ends here
