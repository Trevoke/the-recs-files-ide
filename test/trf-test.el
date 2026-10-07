;;; trf-test.el --- Tests for trf -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf)

(deftest trf-test-version-is-set ()
  "trf declares its version."
  (assert-equal "0.1.0" trf-version))

(provide 'trf-test)
;;; trf-test.el ends here
