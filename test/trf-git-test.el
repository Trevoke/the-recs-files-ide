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

(deftest trf-git-signals-on-failure ()
  "trf-git signals an error when git fails."
  (assert-raises 'error (trf-git trf-git-test-root "rev-parse" "no-such-ref")))

(deftest trf-git-lines-splits-output ()
  "trf-git-lines returns one string per non-empty line."
  (assert-includes (trf-git-lines trf-git-test-root "ls-files") "AGENTS.md"))

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

(provide 'trf-git-test)
;;; trf-git-test.el ends here
