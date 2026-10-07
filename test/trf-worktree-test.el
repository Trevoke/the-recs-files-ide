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
    (assert-true (trf-worktree-codebase-moved-p directory))))

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

(provide 'trf-worktree-test)
;;; trf-worktree-test.el ends here
