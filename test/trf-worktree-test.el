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
      (assert-raises 'error (trf-worktree-accept directory))
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
    (assert-equal "*.log\r\n/.worktrees/\n/.trf/\n"
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
    (assert-raises 'error (trf-worktree-accept directory))
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    (assert-true (file-exists-p (expand-file-name ".git/MERGE_HEAD" root)))
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    (assert-equal "(provide 'gadget) ; resolved\n"
                  (trf-test-git root "show" ":src/gadget.el"))
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    (assert-equal "" (trf-test-git root "for-each-ref" "refs/recs/"))))

(provide 'trf-worktree-test)
;;; trf-worktree-test.el ends here
