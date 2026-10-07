;;; trf-worktree.el --- The worktree the implementation workflow runs in -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'trf-git)
(require 'trf-record-changes)

(defun trf-worktree--exclude (root pattern)
  "Make git in ROOT ignore PATTERN, through the repository's info/exclude."
  (let ((exclude (expand-file-name
                  "info/exclude"
                  (string-trim (trf-git root "rev-parse" "--path-format=absolute"
                                        "--git-common-dir")))))
    (make-directory (file-name-directory exclude) t)
    (unless (and (file-exists-p exclude)
                 (with-temp-buffer
                   (insert-file-contents exclude)
                   (re-search-forward (concat "^" (regexp-quote pattern) "$") nil t)))
      (with-temp-buffer
        (insert pattern "\n")
        (append-to-file (point-min) (point-max) exclude)))))

(defun trf-worktree--write (directory file content)
  "Write CONTENT to FILE under DIRECTORY's .trf/."
  (with-temp-file (expand-file-name (concat ".trf/" file) directory)
    (insert content "\n")))

(defun trf-worktree--read (directory file)
  "Return the content of FILE under DIRECTORY's .trf/."
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat ".trf/" file) directory))
    (string-trim (buffer-string))))

;; UW-1 §Main success scenario: Turn record changes into code
;; F-1 §Rules: Ask the IDE to implement the record changes
(defun trf-worktree-create (root commit)
  "Create a worktree of ROOT starting from COMMIT; return its directory."
  (let* ((name (make-temp-name (format-time-string "trf-%Y%m%d-%H%M%S-")))
         (directory (file-name-as-directory
                     (expand-file-name (concat ".worktrees/" name) root))))
    (trf-worktree--exclude root "/.worktrees/")
    (trf-worktree--exclude root "/.trf/")
    (trf-git root "worktree" "add" "--quiet" "-b" (concat "trf/" name) directory commit)
    (make-directory (expand-file-name ".trf/" directory) t)
    (trf-worktree--write directory "root" root)
    (trf-worktree--write directory "base" commit)
    directory))

(defun trf-worktree-of (file)
  "Return the worktree holding FILE, or nil."
  (when-let* ((directory (locate-dominating-file file ".trf")))
    (file-name-as-directory (expand-file-name directory))))

(defun trf-worktree-root (directory)
  "Return the target product's root for the worktree DIRECTORY."
  (trf-worktree--read directory "root"))

(defun trf-worktree-base (directory)
  "Return the commit of the record changes the worktree DIRECTORY started from."
  (trf-worktree--read directory "base"))

;; UW-1 §Extensions: Turn record changes into code
(defun trf-worktree-codebase-moved-p (directory)
  "Return non-nil when the codebase has moved since DIRECTORY's base."
  (not (trf-git-check directory "merge-base" "--is-ancestor"
                      (trf-git-ref-commit (trf-worktree-root directory) "HEAD")
                      "HEAD")))

;; UW-1 §Main success scenario: Turn record changes into code
(defun trf-worktree-accept (directory)
  "Bring DIRECTORY's work into the codebase and mark its record changes implemented."
  (let ((root (trf-worktree-root directory))
        (branch (string-trim (trf-git directory "branch" "--show-current"))))
    (trf-git root "merge" "--quiet" "--no-ff" "--no-edit" branch)
    (trf-git root "update-ref" trf-record-changes-implemented-ref
             (trf-worktree-base directory))))

(provide 'trf-worktree)
;;; trf-worktree.el ends here
