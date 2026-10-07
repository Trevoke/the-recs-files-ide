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
  "With nothing implemented, every record, the vision and the ubiquitous language are record changes; other files are not."
  ;; F-1 §Takes: Ask the IDE to implement the record changes
  (assert-equal
   '((:path "docs/ubiquitous-language.org" :status added
      :identifier nil :title "Ubiquitous Language")
     (:path "docs/user-workflows/uw-1-order-a-widget.org" :status added
      :identifier "UW-1" :title "Order a widget")
     (:path "docs/vision.org" :status added :identifier nil :title "Vision"))
   (trf-record-changes trf-record-changes-test-root)))

(deftest trf-record-changes-none-once-implemented ()
  "Once every record is implemented, there are no record changes."
  (trf-record-changes-test-mark-implemented)
  ;; F-1 §Takes: Ask the IDE to implement the record changes
  (assert-nil (trf-record-changes trf-record-changes-test-root)))

(deftest trf-record-changes-uncommitted-edit ()
  "An uncommitted edit to a record is a record change."
  (trf-record-changes-test-mark-implemented)
  (trf-test-write-files trf-record-changes-test-root
                        '(("docs/user-workflows/uw-1-order-a-widget.org"
                           . "* UW-1 Order a widget\n\n1. Buyer orders twice.\n")))
  ;; F-1 §Takes: Ask the IDE to implement the record changes
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
  ;; F-1 §Takes: Ask the IDE to implement the record changes
  (assert-equal '((:path "docs/features/f-1-order.org" :status added
                   :identifier "F-1" :title "Order")
                  (:path "docs/user-workflows/uw-1-order-a-widget.org" :status deleted
                   :identifier "UW-1" :title "Order a widget"))
                (trf-record-changes trf-record-changes-test-root)))

(deftest trf-record-changes-record-with-non-ascii-name ()
  "A record whose file name is not ASCII is a record change under its own path."
  (trf-record-changes-test-mark-implemented)
  (trf-test-write-files trf-record-changes-test-root
                        '(("docs/features/f-2-café.org" . "* F-2 Café\n\nTakes: a coffee.\n")))
  ;; F-1 §Takes: Ask the IDE to implement the record changes
  (assert-equal '((:path "docs/features/f-2-café.org" :status added
                   :identifier "F-2" :title "Café"))
                (trf-record-changes trf-record-changes-test-root)))

(deftest trf-record-changes-record-removed-from-index ()
  "A record removed from the index but kept on disk is one modified record change."
  (trf-record-changes-test-mark-implemented)
  (trf-test-git trf-record-changes-test-root "rm" "--cached" "--quiet"
                "docs/user-workflows/uw-1-order-a-widget.org")
  ;; F-1 §Takes: Ask the IDE to implement the record changes
  (assert-equal '((:path "docs/user-workflows/uw-1-order-a-widget.org" :status modified
                   :identifier "UW-1" :title "Order a widget"))
                (trf-record-changes trf-record-changes-test-root)))

(deftest trf-record-changes-lowercase-heading-has-no-identifier ()
  "A heading in lowercase names no record identifier."
  (trf-record-changes-test-mark-implemented)
  (trf-test-write-files trf-record-changes-test-root
                        '(("docs/features/notes.org" . "#+TITLE: Notes\n\n* notes-2026 about\n")))
  ;; F-1 §Takes: Ask the IDE to implement the record changes
  (assert-equal '((:path "docs/features/notes.org" :status added
                   :identifier nil :title "Notes"))
                (let ((case-fold-search t))
                  (trf-record-changes trf-record-changes-test-root))))

(deftest trf-record-changes-lowercase-title-keyword ()
  "The vision's #+title: is its title, whatever its case."
  (trf-record-changes-test-mark-implemented)
  (trf-test-write-files trf-record-changes-test-root
                        '(("docs/vision.org" . "#+title: Vision\n\nWhat it is for.\n")))
  ;; F-1 §Takes: Ask the IDE to implement the record changes
  (assert-equal '((:path "docs/vision.org" :status modified
                   :identifier nil :title "Vision"))
                (let ((case-fold-search nil))
                  (trf-record-changes trf-record-changes-test-root))))

(deftest trf-record-changes-not-a-projection-file-edit ()
  "An edit to a projection's file is not a record change."
  (trf-record-changes-test-mark-implemented)
  (trf-test-write-files trf-record-changes-test-root
                        '(("docs/user-workflows.org"
                           . "#+TITLE: User Workflows\n\n- UW-1 :: Order two widgets\n")))
  ;; F-1 §Takes: Ask the IDE to implement the record changes
  (assert-nil (trf-record-changes trf-record-changes-test-root)))

(deftest trf-record-changes-commit-holds-only-the-record-changes ()
  "The commit holds the record changes and leaves other edits uncommitted."
  (trf-record-changes-test-mark-implemented)
  (trf-test-write-files trf-record-changes-test-root
                        '(("docs/user-workflows/uw-1-order-a-widget.org"
                           . "* UW-1 Order a widget\n\n1. Buyer orders twice.\n")
                          ("docs/user-workflows.org"
                           . "#+TITLE: User Workflows\n\n- UW-1 :: Order two widgets\n")
                          ("src/widget.el" . "(provide 'widget) ; edited\n")))
  (trf-test-git trf-record-changes-test-root "add" "src/widget.el")
  (let* ((root trf-record-changes-test-root)
         (commit (trf-record-changes-commit root (trf-record-changes root))))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal (trf-test-head root) commit)
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal "docs/user-workflows/uw-1-order-a-widget.org\n"
                  (trf-test-git root "show" "--name-only" "--format=" commit))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal " M docs/user-workflows.org\nM  src/widget.el\n"
                  (trf-test-git root "status" "--porcelain"))))

(deftest trf-record-changes-commit-names-each-record-change ()
  "The commit message names each record change by identifier, where it has one, and title."
  (let ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files root '(("docs/features/f-1-order.org" . "* F-1 Order\n")
                                 ("docs/vision.org" . "#+TITLE: Vision\n\nMore.\n")))
    (trf-record-changes-commit root (trf-record-changes root))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal "Record changes\n\n- F-1 Order\n- Vision\n"
                  (trf-test-git root "log" "-1" "--format=format:%B"))))

(deftest trf-record-changes-commit-deleted-record ()
  "A deleted record is committed as a deletion."
  (let ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (delete-file (expand-file-name "docs/user-workflows/uw-1-order-a-widget.org" root))
    (let ((commit (trf-record-changes-commit root (trf-record-changes root))))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "D\tdocs/user-workflows/uw-1-order-a-widget.org\n"
                    (trf-test-git root "show" "--name-status" "--format=" commit))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "" (trf-test-git root "status" "--porcelain")))))

(deftest trf-record-changes-commit-record-with-non-ascii-name ()
  "A record whose file name is not ASCII is committed under its own path."
  (let ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files root '(("docs/features/f-2-café.org" . "* F-2 Café\n")))
    (let ((commit (trf-record-changes-commit root (trf-record-changes root))))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "docs/features/f-2-café.org\0"
                    (decode-coding-string
                     (trf-test-git root "show" "--name-only" "-z" "--format=" commit)
                     'utf-8))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "Record changes\n\n- F-2 Café\n"
                    (decode-coding-string (trf-test-git root "log" "-1" "--format=format:%B")
                                          'utf-8)))))

(deftest trf-record-changes-commit-when-already-committed ()
  "When the record changes are already committed, no commit is made."
  (let* ((root trf-record-changes-test-root)
         (head (trf-test-head root)))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal head (trf-record-changes-commit root (trf-record-changes root)))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal head (trf-test-head root))))

(deftest trf-record-changes-ubiquitous-language-edit ()
  "An edit to the ubiquitous language is a record change."
  (trf-record-changes-test-mark-implemented)
  (trf-test-write-files trf-record-changes-test-root
                        '(("docs/ubiquitous-language.org"
                           . "#+TITLE: Ubiquitous Language\n\n* Records\n\n- widget :: a thing a buyer orders twice.\n")))
  ;; F-1 §Takes: Ask the IDE to implement the record changes
  (assert-equal '((:path "docs/ubiquitous-language.org" :status modified
                   :identifier nil :title "Ubiquitous Language"))
                (trf-record-changes trf-record-changes-test-root)))

(defun trf-record-changes-test-commit-deletion ()
  (let ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (trf-test-git root "rm" "--quiet" "docs/user-workflows/uw-1-order-a-widget.org")
    (trf-test-git root "commit" "--quiet" "--message" "Delete UW-1")))

(deftest trf-record-changes-commit-deletion-already-committed ()
  "A record deletion already committed is not committed again."
  (trf-record-changes-test-commit-deletion)
  (let* ((root trf-record-changes-test-root)
         (head (trf-test-head root)))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal head (trf-record-changes-commit root (trf-record-changes root)))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal head (trf-test-head root))))

(deftest trf-record-changes-commit-beside-a-deletion-already-committed ()
  "Beside a record deletion already committed, the commit holds the other record changes."
  (trf-record-changes-test-commit-deletion)
  (let ((root trf-record-changes-test-root))
    (trf-test-write-files root '(("docs/vision.org" . "#+TITLE: Vision\n\nMore.\n")))
    (let ((commit (trf-record-changes-commit root (trf-record-changes root))))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "docs/vision.org\n"
                    (trf-test-git root "show" "--name-only" "--format=" commit))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "" (trf-test-git root "status" "--porcelain")))))

(provide 'trf-record-changes-test)
;;; trf-record-changes-test.el ends here
