;;; trf-worktree-test.el --- Tests for trf-worktree -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf-test-helper)
(require 'trf-worktree)

(defvar trf-worktree-test-root nil)

(setup-each () (setq trf-worktree-test-root (trf-test-make-target-product)))
(teardown-each () (delete-directory trf-worktree-test-root t))

(defun trf-worktree-test-read (directory file)
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat ".trf/" file) directory))
    (string-trim (buffer-string))))

(deftest trf-worktree-create-starts-from-the-commit ()
  "The worktree starts from the given commit and records its root and base."
  (let* ((root trf-worktree-test-root)
         (base (trf-test-head root))
         (directory (trf-worktree-create root base)))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal base (trf-test-head directory))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal base (trf-worktree-test-read directory "base"))
    (assert-equal root (trf-worktree-test-read directory "root"))
    (assert-equal "" (trf-test-git directory "status" "--porcelain"))
    (assert-match "^trf/" (string-trim (trf-test-git directory "branch" "--show-current")))))

(deftest trf-worktree-create-leaves-the-codebase-unchanged ()
  "Creating a worktree leaves the target product's working tree clean."
  (let ((root trf-worktree-test-root))
    (trf-worktree-create root (trf-test-head root))
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    (assert-equal "" (trf-test-git root "status" "--porcelain"))))

(deftest trf-worktree-create-twice-in-a-row ()
  "Two worktrees created in a row on the same root are distinct."
  (let* ((root trf-worktree-test-root)
         (first (trf-worktree-create root (trf-test-head root)))
         (second (trf-worktree-create root (trf-test-head root))))
    (assert-nil (equal first second))
    (assert-true (file-directory-p first))
    (assert-true (file-directory-p second))))

(deftest trf-worktree-of-finds-the-worktree ()
  "Any file inside a worktree leads back to it."
  (let ((directory (trf-worktree-create trf-worktree-test-root
                                        (trf-test-head trf-worktree-test-root))))
    (assert-equal directory
                  (trf-worktree-of (expand-file-name "docs/vision.org" directory)))))

(deftest trf-worktree-codebase-moved ()
  "The codebase has moved once a commit lands on it after the worktree's base."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root))))
    (assert-nil (trf-worktree-codebase-moved-p directory))
    (trf-test-write-files root '(("src/other.el" . "(provide 'other)\n")))
    (trf-test-git root "add" "--all")
    (trf-test-git root "commit" "--quiet" "--message" "Other work")
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-true (trf-worktree-codebase-moved-p directory))
    (trf-test-git directory "merge" "--quiet" "--no-edit" "main")
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-nil (trf-worktree-codebase-moved-p directory))
    (trf-test-git directory "checkout" "--quiet" "--detach" "HEAD^")
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-nil (trf-worktree-codebase-moved-p directory))))

(deftest trf-worktree-accept-brings-the-work-in ()
  "Accepting merges the work into the codebase and marks the base implemented."
  (let* ((root trf-worktree-test-root)
         (base (trf-test-head root))
         (directory (trf-worktree-create root base)))
    (trf-test-write-files directory '(("src/widget.el" . "(provide 'widget) ; built\n")))
    (trf-test-git directory "commit" "--quiet" "--all" "--message" "Build")
    (trf-worktree-accept directory)
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-equal "(provide 'widget) ; built\n"
                  (with-temp-buffer
                    (insert-file-contents (expand-file-name "src/widget.el" root))
                    (buffer-string)))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-equal base (string-trim (trf-test-git root "rev-parse" "refs/recs/implemented")))))

(deftest trf-worktree-create-keeps-an-exclude-without-a-final-newline ()
  "An exclude file not ending in a newline keeps its last line intact."
  (let* ((root trf-worktree-test-root)
         (exclude (expand-file-name ".git/info/exclude" root)))
    (make-directory (file-name-directory exclude) t)
    (with-temp-file exclude (insert "*.log"))
    (trf-worktree-create root (trf-test-head root))
    (let ((lines (with-temp-buffer
                   (insert-file-contents exclude)
                   (split-string (buffer-string) "\n" t))))
      (assert-true (member "*.log" lines))
      (assert-true (member "/.worktrees/" lines)))
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    (assert-equal "" (trf-test-git root "status" "--porcelain"))))

(deftest trf-worktree-create-normalizes-the-root ()
  "The worktree records its root as an absolute directory name."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create (directory-file-name root) (trf-test-head root))))
    (assert-equal root (trf-worktree-root directory))))

(deftest trf-worktree-of-is-nil-outside-any-worktree ()
  "A file of the target product itself is in no worktree."
  (let ((root trf-worktree-test-root))
    (trf-worktree-create root (trf-test-head root))
    (assert-nil (trf-worktree-of (expand-file-name "docs/vision.org" root)))))

(deftest trf-worktree-accept-from-a-detached-worktree ()
  "Accepting merges the work even once the worktree is checked out detached."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root))))
    (trf-test-write-files directory '(("src/widget.el" . "(provide 'widget) ; built\n")))
    (trf-test-git directory "commit" "--quiet" "--all" "--message" "Build")
    (trf-test-git directory "checkout" "--quiet" "--detach")
    (trf-worktree-accept directory)
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-equal "(provide 'widget) ; built\n"
                  (with-temp-buffer
                    (insert-file-contents (expand-file-name "src/widget.el" root))
                    (buffer-string)))))

(deftest trf-worktree-accept-that-conflicts-leaves-the-codebase-unchanged ()
  "An accept whose merge conflicts signals and leaves the codebase as it was."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root))))
    (trf-test-write-files directory '(("src/gadget.el" . "(provide 'gadget) ; work\n")))
    (trf-test-git directory "add" "--all")
    (trf-test-git directory "commit" "--quiet" "--message" "Build")
    (trf-test-write-files root '(("src/gadget.el" . "(provide 'gadget) ; other\n")))
    (trf-test-git root "add" "--all")
    (trf-test-git root "commit" "--quiet" "--message" "Other work")
    (let ((head (trf-test-head root))
          (status (trf-test-git root "status" "--porcelain")))
      (assert-raises 'trf-worktree-merge-failed (trf-worktree-accept directory))
      ;; UW-1 §Minimal guarantee: Turn record changes into code
      (assert-nil (file-exists-p (expand-file-name ".git/MERGE_HEAD" root)))
      ;; UW-1 §Minimal guarantee: Turn record changes into code
      (assert-equal head (trf-test-head root))
      ;; UW-1 §Minimal guarantee: Turn record changes into code
      (assert-equal status (trf-test-git root "status" "--porcelain"))
      ;; UW-1 §Minimal guarantee: Turn record changes into code
      (assert-equal "" (trf-test-git root "for-each-ref" "refs/recs/")))))

(deftest trf-worktree-create-appends-to-the-exclude-as-it-is ()
  "Create appends to the exclude file, leaving its line endings as they were."
  (let* ((root trf-worktree-test-root)
         (exclude (expand-file-name ".git/info/exclude" root)))
    (make-directory (file-name-directory exclude) t)
    (let ((coding-system-for-write 'no-conversion))
      (with-temp-file exclude (insert "*.log\r\n")))
    (trf-worktree-create root (trf-test-head root))
    (assert-equal "*.log\r\n/.worktrees/\n/.trf/\n.#*\n\\#*#\n*~\n"
                  (with-temp-buffer
                    (insert-file-contents-literally exclude)
                    (buffer-string)))))

(deftest trf-worktree-accept-leaves-the-record-keepers-merge-alone ()
  "Accept while the record keeper is mid-merge signals and keeps that merge."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root))))
    (trf-test-write-files directory '(("src/widget.el" . "(provide 'widget) ; built\n")))
    (trf-test-git directory "commit" "--quiet" "--all" "--message" "Build")
    (trf-test-git root "checkout" "--quiet" "-b" "side")
    (trf-test-write-files root '(("src/gadget.el" . "(provide 'gadget) ; side\n")))
    (trf-test-git root "add" "--all")
    (trf-test-git root "commit" "--quiet" "--message" "Side")
    (trf-test-git root "checkout" "--quiet" "main")
    (trf-test-write-files root '(("src/gadget.el" . "(provide 'gadget) ; main\n")))
    (trf-test-git root "add" "--all")
    (trf-test-git root "commit" "--quiet" "--message" "Main")
    (ignore-errors (trf-test-git root "merge" "--quiet" "side"))
    (trf-test-write-files root '(("src/gadget.el" . "(provide 'gadget) ; resolved\n")))
    (trf-test-git root "add" "src/gadget.el")
    (assert-raises 'trf-worktree-merge-failed (trf-worktree-accept directory))
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    (assert-true (file-exists-p (expand-file-name ".git/MERGE_HEAD" root)))
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    (assert-equal "(provide 'gadget) ; resolved\n"
                  (trf-test-git root "show" ":src/gadget.el"))
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    (assert-equal "" (trf-test-git root "for-each-ref" "refs/recs/"))))

(deftest trf-worktree-create-records-what-the-record-changes-are-measured-from ()
  "The worktree records the implemented records' commit as it was when the work started."
  (let* ((root trf-worktree-test-root)
         (implemented (trf-test-head root)))
    (trf-test-git root "update-ref" "refs/recs/implemented" implemented)
    (let ((directory (trf-worktree-create root implemented)))
      (trf-test-write-files root '(("src/other.el" . "(provide 'other)\n")))
      (trf-test-git root "add" "--all")
      (trf-test-git root "commit" "--quiet" "--message" "Other work")
      (trf-test-git root "update-ref" "refs/recs/implemented" "HEAD")
      ;; UW-1 §Extensions: Turn record changes into code
      (assert-equal implemented (trf-worktree-from directory)))))

(deftest trf-worktree-create-before-any-implementation ()
  "A worktree started before anything is implemented records the empty tree to measure from."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root)))
         (empty-tree (string-trim (trf-test-git root "hash-object" "-t" "tree" "/dev/null"))))
    (assert-equal empty-tree (trf-worktree-test-read directory "from"))
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-equal empty-tree (trf-worktree-from directory))))

(deftest trf-worktree-from-unknown ()
  "A worktree without a record of what its record changes were measured from gives nil."
  (let ((directory (trf-worktree-create trf-worktree-test-root
                                        (trf-test-head trf-worktree-test-root))))
    (delete-file (expand-file-name ".trf/from" directory))
    (assert-nil (trf-worktree-from directory))))

(deftest trf-worktree-brought-in ()
  "The work is brought in once the worktree's branch, holding work, is in the codebase."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root))))
    (assert-nil (trf-worktree-brought-in-p directory))
    (trf-test-write-files directory '(("src/widget.el" . "(provide 'widget) ; built\n")))
    (trf-test-git directory "commit" "--quiet" "--all" "--message" "Build")
    (assert-nil (trf-worktree-brought-in-p directory))
    (trf-test-git root "merge" "--quiet" "--no-ff" "--no-edit" (trf-worktree-branch directory))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-true (trf-worktree-brought-in-p directory))))

(defun trf-worktree-test-commit (root message)
  (trf-test-write-files root (list (cons "src/widget.el" (concat "; " message "\n"))))
  (trf-test-git root "commit" "--quiet" "--all" "--message" message)
  (trf-test-head root))

(defun trf-worktree-test-implemented (root)
  (string-trim (trf-test-git root "rev-parse" "refs/recs/implemented")))

(deftest trf-worktree-mark-implemented-without-a-ref ()
  "With nothing implemented, marking a worktree's record changes implemented sets the ref to its base."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root))))
    (trf-worktree-mark-implemented directory)
    ;; UW-1 §Success guarantee: Turn record changes into code
    (assert-equal (trf-test-head root) (trf-worktree-test-implemented root))))

(deftest trf-worktree-mark-implemented-moves-the-ref-forward ()
  "A base holding the implemented records moves the ref to it."
  (let* ((root trf-worktree-test-root)
         (earlier (trf-test-head root)))
    (trf-test-git root "update-ref" "refs/recs/implemented" earlier)
    (let* ((base (trf-worktree-test-commit root "later"))
           (directory (trf-worktree-create root base)))
      (trf-worktree-mark-implemented directory)
      ;; UW-1 §Success guarantee: Turn record changes into code
      (assert-equal base (trf-worktree-test-implemented root)))))

(deftest trf-worktree-mark-implemented-never-moves-the-ref-back ()
  "A base the implemented records already hold leaves the ref where it is."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root)))
         (later (trf-worktree-test-commit root "later")))
    (trf-test-git root "update-ref" "refs/recs/implemented" later)
    (trf-worktree-mark-implemented directory)
    ;; UW-1 §Success guarantee: Turn record changes into code
    (assert-equal later (trf-worktree-test-implemented root))))

(deftest trf-worktree-mark-implemented-leaves-a-diverged-ref ()
  "A base that has diverged from the implemented records leaves the ref where it is."
  (let* ((root trf-worktree-test-root)
         (side (progn (trf-test-git root "checkout" "--quiet" "-b" "side")
                      (trf-worktree-test-commit root "side"))))
    (trf-test-git root "checkout" "--quiet" "main")
    (trf-test-git root "update-ref" "refs/recs/implemented" side)
    (let ((directory (trf-worktree-create root (trf-worktree-test-commit root "main"))))
      (trf-worktree-mark-implemented directory)
      (assert-equal side (trf-worktree-test-implemented root)))))

(defun trf-worktree-test-accepted ()
  "Create a worktree of the root, commit work in it and accept it; return its directory."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root))))
    (trf-test-write-files directory '(("src/widget.el" . "(provide 'widget) ; built\n")))
    (trf-test-git directory "commit" "--quiet" "--all" "--message" "Build")
    (trf-worktree-accept directory)
    directory))

(deftest trf-worktree-remove-removes-the-worktree-and-its-branch ()
  "Removing an accepted worktree deletes its directory and its branch."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-test-accepted))
         (branch (trf-worktree-branch directory)))
    (trf-worktree-remove directory)
    ;; F-2 §Produces: Accept or reject the record implementation report
    (assert-nil (file-exists-p directory))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (assert-equal "" (trf-test-git root "branch" "--list" branch))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (assert-equal 1 (length (split-string (trf-test-git root "worktree" "list") "\n" t)))
    ;; UW-1 §Success guarantee: Turn record changes into code
    (assert-equal "(provide 'widget) ; built\n"
                  (with-temp-buffer
                    (insert-file-contents (expand-file-name "src/widget.el" root))
                    (buffer-string)))))

(deftest trf-worktree-remove-with-untracked-files-keeps-the-worktree ()
  "Removing a worktree holding untracked files signals and keeps the worktree."
  (let ((directory (trf-worktree-test-accepted)))
    (trf-test-write-files directory '(("src/scratch.el" . "(provide 'scratch)\n")))
    ;; docs/knowledge-base/git-worktree-removal.org §git worktree remove refuses a worktree holding untracked or modified files
    (assert-equal 'trf-git-error
                  (car (condition-case err (progn (trf-worktree-remove directory) nil)
                         (error err))))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-true (file-exists-p (expand-file-name ".trf/root" directory)))))

(deftest trf-worktree-remove-once-its-branch-is-gone ()
  "Removing a worktree whose branch is already deleted removes the worktree."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-test-accepted)))
    (trf-test-git directory "checkout" "--quiet" "--detach")
    (trf-test-git root "branch" "--quiet" "-d" (trf-worktree-branch directory))
    (trf-worktree-remove directory)
    ;; F-2 §Produces: Accept or reject the record implementation report
    (assert-nil (file-exists-p directory))))

(deftest trf-worktree-brought-in-once-its-branch-is-gone ()
  "The work stays brought in, and moved or not, once its branch is deleted and its worktree is detached."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-test-accepted))
         (moved (trf-worktree-codebase-moved-p directory)))
    (trf-test-git directory "checkout" "--quiet" "--detach")
    (trf-test-git root "branch" "--quiet" "-d" (trf-worktree-branch directory))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-true (trf-worktree-brought-in-p directory))
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-equal moved (trf-worktree-codebase-moved-p directory))))

(deftest trf-worktree-commit-saved-edits-commits-edits-and-new-files ()
  "Saved edits and new files are committed to the work; the worktree's state files are not."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root))))
    (trf-test-write-files directory '(("src/widget.el" . "(provide 'widget) ; saved\n")
                                      ("src/gadget.el" . "(provide 'gadget)\n")))
    (trf-worktree-commit-saved-edits directory)
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-equal "" (trf-test-git directory "status" "--porcelain"))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-equal "src/gadget.el\nsrc/widget.el\n"
                  (trf-test-git directory "diff" "--name-only" (trf-worktree-base directory) "HEAD"))
    (assert-match (regexp-quote (trf-worktree-branch directory))
                  (trf-test-git directory "log" "-1" "--format=%s"))))

(deftest trf-worktree-commit-saved-edits-with-nothing-saved ()
  "With nothing saved since the last commit, nothing is committed."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root))))
    (trf-worktree-commit-saved-edits directory)
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-equal (trf-worktree-base directory) (trf-test-head directory))))

(deftest trf-worktree-create-excludes-emacs-lock-and-auto-save-files ()
  "Creating a worktree makes git ignore Emacs lock and auto-save files."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root))))
    (trf-test-write-files directory '(("src/.#widget.el" . "lock") ("src/#widget.el#" . "auto-save")))
    ;; docs/knowledge-base/git-worktree-removal.org §git worktree remove refuses a worktree holding a modified buffer's lock file
    (assert-equal "" (trf-test-git directory "status" "--porcelain" "--untracked-files=all"))))

(deftest trf-worktree-create-excludes-emacs-backup-files ()
  "Creating a worktree makes git ignore Emacs backup files."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root))))
    (trf-test-write-files directory '(("src/widget.el~" . "backup")))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-equal "" (trf-test-git directory "status" "--porcelain" "--untracked-files=all"))))

(deftest trf-worktree-commit-saved-edits-leaves-out-emacs-files-in-an-older-worktree ()
  "Saved edits are committed without Emacs lock and auto-save files, in a worktree made before they were excluded."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root)))
         (exclude (expand-file-name ".git/info/exclude" root)))
    (with-temp-file exclude
      (insert "/.worktrees/\n/.trf/\n"))
    (trf-test-write-files directory '(("src/widget.el" . "(provide 'widget) ; saved\n")
                                      ("src/.#widget.el" . "lock")
                                      ("src/#widget.el#" . "auto-save")))
    (trf-worktree-commit-saved-edits directory)
    ;; F-2 §Rules: Accept or reject the record implementation report
    (assert-equal "src/widget.el\n"
                  (trf-test-git directory "diff" "--name-only" (trf-worktree-base directory) "HEAD"))))

(provide 'trf-worktree-test)
;;; trf-worktree-test.el ends here
