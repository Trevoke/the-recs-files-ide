;;; trf-record-changes-test.el --- Tests for trf-record-changes -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf-test-helper)
(require 'trf-record-changes)

(defvar trf-record-changes-test-root nil)

(setup-each () (setq trf-record-changes-test-root (trf-test-make-target-product)))
(teardown-each () (delete-directory trf-record-changes-test-root t))

(defun trf-record-changes-test-mark-implemented ()
  (trf-test-git trf-record-changes-test-root "update-ref" "refs/recs/implemented" "HEAD"))

(deftest trf-record-changes-all-records-before-any-implementation ()
  "With nothing implemented, every record is a record change; other files are not."
  (assert-equal
   '((:path "docs/user-workflows.org" :status added :identifier nil :title "User Workflows")
     (:path "docs/user-workflows/uw-1-order-a-widget.org" :status added
      :identifier "UW-1" :title "Order a widget")
     (:path "docs/vision.org" :status added :identifier nil :title "Vision"))
   (trf-record-changes trf-record-changes-test-root)))

(deftest trf-record-changes-none-once-implemented ()
  "Once every record is implemented, there are no record changes."
  (trf-record-changes-test-mark-implemented)
  (assert-nil (trf-record-changes trf-record-changes-test-root)))

(deftest trf-record-changes-uncommitted-edit ()
  "An uncommitted edit to a record is a record change."
  (trf-record-changes-test-mark-implemented)
  (trf-test-write-files trf-record-changes-test-root
                        '(("docs/user-workflows/uw-1-order-a-widget.org"
                           . "* UW-1 Order a widget\n\n1. Buyer orders twice.\n")))
  (assert-equal '((:path "docs/user-workflows/uw-1-order-a-widget.org" :status modified
                   :identifier "UW-1" :title "Order a widget"))
                (trf-record-changes trf-record-changes-test-root)))

(deftest trf-record-changes-new-and-deleted-records ()
  "A new record and a deleted record are record changes; a code edit is not."
  (trf-record-changes-test-mark-implemented)
  (trf-test-write-files trf-record-changes-test-root
                        '(("docs/features/f-1-order.org" . "* F-1 Order\n\nTakes: a widget.\n")
                          ("src/widget.el" . "(provide 'widget) ; edited\n")))
  (delete-file (expand-file-name "docs/user-workflows/uw-1-order-a-widget.org"
                                 trf-record-changes-test-root))
  (assert-equal '((:path "docs/features/f-1-order.org" :status added
                   :identifier "F-1" :title "Order")
                  (:path "docs/user-workflows/uw-1-order-a-widget.org" :status deleted
                   :identifier "UW-1" :title "Order a widget"))
                (trf-record-changes trf-record-changes-test-root)))

(provide 'trf-record-changes-test)
;;; trf-record-changes-test.el ends here
