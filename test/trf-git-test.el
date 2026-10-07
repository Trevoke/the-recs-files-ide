;;; trf-git-test.el --- Tests for trf-git -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf-test-helper)
(require 'trf-git)

(defvar trf-git-test-root nil)

(setup-each () (setq trf-git-test-root (trf-test-make-target-product)))
(teardown-each () (delete-directory trf-git-test-root t))

(deftest trf-git-returns-output ()
  "trf-git returns git's standard output."
  (assert-equal (concat (trf-test-head trf-git-test-root) "\n")
                (trf-git trf-git-test-root "rev-parse" "HEAD")))

(defun trf-git-test-stderr (&rest args)
  "Return what git prints to standard error when run with ARGS in the target product."
  (let ((default-directory trf-git-test-root))
    (string-trim (shell-command-to-string
                  (concat "git " (mapconcat #'shell-quote-argument args " ")
                          " 2>&1 >/dev/null")))))

(deftest trf-git-signals-on-failure ()
  "trf-git signals trf-git-error carrying git's own output when git fails."
  (assert-equal (list 'trf-git-error (trf-git-test-stderr "rev-parse" "no-such-ref"))
                (condition-case err
                    (progn (trf-git trf-git-test-root "rev-parse" "no-such-ref") nil)
                  (error err))))

(deftest trf-git-check-answers-true-false-or-signals ()
  "trf-git-check is t when git exits 0, nil when it exits 1, and signals otherwise."
  (let ((head (trf-test-head trf-git-test-root)))
    (assert-true (trf-git-check trf-git-test-root "merge-base" "--is-ancestor" head head))
    (trf-test-write-files trf-git-test-root '(("src/other.el" . "(provide 'other)\n")))
    (trf-test-git trf-git-test-root "add" "--all")
    (trf-test-git trf-git-test-root "commit" "--quiet" "--message" "Other work")
    (assert-nil (trf-git-check trf-git-test-root "merge-base" "--is-ancestor"
                               (trf-test-head trf-git-test-root) head))
    (assert-equal (list 'trf-git-error (trf-git-test-stderr "merge-base" "--is-ancestor"
                                                    "no-such-ref" head))
                  (condition-case err
                      (progn (trf-git-check trf-git-test-root "merge-base" "--is-ancestor"
                                            "no-such-ref" head)
                             nil)
                    (error err)))))

(deftest trf-git-lines-splits-output ()
  "trf-git-lines returns one string per non-empty line."
  (assert-includes (trf-git-lines trf-git-test-root "ls-files") "AGENTS.md"))

(deftest trf-git-paths-splits-nul-separated-output ()
  "trf-git-paths returns one path per NUL-separated field, as UTF-8."
  (trf-test-write-files trf-git-test-root '(("docs/f-2-café.org" . "* F-2 Café\n")))
  (assert-equal '("docs/f-2-café.org")
                (trf-git-paths trf-git-test-root "ls-files" "-z" "--others")))

(deftest trf-git-ref-commit-resolves-and-misses ()
  "trf-git-ref-commit returns the commit a ref names, or nil when it is missing."
  (assert-equal (trf-test-head trf-git-test-root)
                (trf-git-ref-commit trf-git-test-root "HEAD"))
  (assert-nil (trf-git-ref-commit trf-git-test-root "refs/recs/implemented")))

(deftest trf-git-returns-stdout-only ()
  "trf-git returns git's standard output without its standard error."
  (assert-equal ""
                (trf-git trf-git-test-root "-c" "advice.detachedHead=true"
                         "checkout" (trf-test-head trf-git-test-root))))

(deftest trf-git-fixture-has-no-hooks ()
  "A target product made by the fixture holds only git's sample hooks."
  (assert-nil (seq-remove (lambda (file) (string-suffix-p ".sample" file))
                          (directory-files (expand-file-name ".git/hooks/" trf-git-test-root)
                                           nil directory-files-no-dot-files-regexp))))

(deftest trf-git-operation-in-progress-p-when-none ()
  "No operation is in progress in a target product at rest."
  ;; UW-1 §Extensions: Turn record changes into code
  (assert-nil (trf-git-operation-in-progress-p trf-git-test-root)))

(defun trf-git-test-in-progress (operation)
  (trf-test-leave-in-progress trf-git-test-root operation)
  (trf-git-operation-in-progress-p trf-git-test-root))

(deftest trf-git-operation-in-progress-p-merge ()
  "A conflicted merge is an operation in progress."
  ;; UW-1 §Extensions: Turn record changes into code
  (assert-true (trf-git-test-in-progress 'merge)))

(deftest trf-git-operation-in-progress-p-rebase ()
  "A conflicted rebase is an operation in progress."
  ;; UW-1 §Extensions: Turn record changes into code
  (assert-true (trf-git-test-in-progress 'rebase)))

(deftest trf-git-operation-in-progress-p-rebase-apply ()
  "A conflicted rebase by the apply backend is an operation in progress."
  ;; UW-1 §Extensions: Turn record changes into code
  (assert-true (trf-git-test-in-progress 'rebase-apply)))

(deftest trf-git-operation-in-progress-p-cherry-pick ()
  "A conflicted cherry-pick is an operation in progress."
  ;; UW-1 §Extensions: Turn record changes into code
  (assert-true (trf-git-test-in-progress 'cherry-pick)))

(deftest trf-git-operation-in-progress-p-cherry-pick-sequence ()
  "A multi-commit cherry-pick paused between its commits is an operation in progress."
  ;; UW-1 §Extensions: Turn record changes into code
  (assert-true (trf-git-test-in-progress 'cherry-pick-sequence)))

(deftest trf-git-operation-in-progress-p-revert ()
  "An uncommitted revert is an operation in progress."
  ;; UW-1 §Extensions: Turn record changes into code
  (assert-true (trf-git-test-in-progress 'revert)))

(deftest trf-git-operation-in-progress-p-bisect ()
  "A bisect is an operation in progress."
  ;; UW-1 §Extensions: Turn record changes into code
  (assert-true (trf-git-test-in-progress 'bisect)))

(provide 'trf-git-test)
;;; trf-git-test.el ends here
