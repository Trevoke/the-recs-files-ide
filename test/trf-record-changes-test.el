;;; trf-record-changes-test.el --- Tests for trf-record-changes -*- lexical-binding: t; -*-

(require 'cl-lib)
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

(deftest trf-record-changes-commit-holds-only-the-record-changes ()
  "The commit holds the record changes and the projection files listing them, and leaves other edits uncommitted."
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
    (assert-equal "docs/user-workflows.org\ndocs/user-workflows/uw-1-order-a-widget.org\n"
                  (trf-test-git root "show" "--name-only" "--format=" commit))
    ;; F-1 §Rules: Ask the IDE to implement the record changes
    (assert-equal "M  src/widget.el\n"
                  (trf-test-git root "status" "--porcelain"))))

(deftest trf-record-changes-commit-new-record-with-its-projection-file ()
  "A new record and its projection file's new line make one commit, whose message names only the record."
  (let ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files
     root '(("docs/features/f-1-order.org" . "* F-1 Order\n")
            ("docs/features.org" . "#+TITLE: Features\n\n- F-1 :: Order\n")))
    (let ((commit (trf-record-changes-commit root (trf-record-changes root))))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "docs/features.org\ndocs/features/f-1-order.org\n"
                    (trf-test-git root "show" "--name-only" "--format=" commit))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "Record changes\n\n- F-1 Order\n"
                    (trf-test-git root "log" "-1" "--format=format:%B"))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "" (trf-test-git root "status" "--porcelain")))))

(deftest trf-record-changes-commit-leaves-a-projection-file-listing-none-of-them ()
  "A projection file edit with no record change of that projection is left uncommitted."
  (let ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files
     root '(("docs/features/f-1-order.org" . "* F-1 Order\n")
            ("docs/user-workflows.org"
             . "#+TITLE: User Workflows\n\n- UW-1 :: Order two widgets\n")))
    (let ((commit (trf-record-changes-commit root (trf-record-changes root))))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "docs/features/f-1-order.org\n"
                    (trf-test-git root "show" "--name-only" "--format=" commit))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal " M docs/user-workflows.org\n"
                    (trf-test-git root "status" "--porcelain")))))

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

(deftest trf-record-changes-commit-record-removed-from-index ()
  "A record removed with git rm and not committed is committed as a deletion."
  (let ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (trf-test-git root "rm" "--quiet" "docs/user-workflows/uw-1-order-a-widget.org")
    (let ((commit (trf-record-changes-commit root (trf-record-changes root))))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "D\tdocs/user-workflows/uw-1-order-a-widget.org\n"
                    (trf-test-git root "show" "--name-status" "--format=" commit))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "" (trf-test-git root "status" "--porcelain")))))

(deftest trf-record-changes-commit-record-removed-from-index-beside-an-edit ()
  "A record removed with git rm is committed with an edit to another record."
  (let ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (trf-test-git root "rm" "--quiet" "docs/user-workflows/uw-1-order-a-widget.org")
    (trf-test-write-files root '(("docs/vision.org" . "#+TITLE: Vision\n\nMore.\n")))
    (let ((commit (trf-record-changes-commit root (trf-record-changes root))))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal (concat "D\tdocs/user-workflows/uw-1-order-a-widget.org\n"
                            "M\tdocs/vision.org\n")
                    (trf-test-git root "show" "--name-status" "--format=" commit))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "" (trf-test-git root "status" "--porcelain")))))

(deftest trf-record-changes-commit-names-only-what-it-holds ()
  "Beside a record change already committed, the message names only the uncommitted ones."
  (let ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files root '(("docs/features/f-1-order.org" . "* F-1 Order\n")))
    (trf-test-git root "add" "docs/features/f-1-order.org")
    (trf-test-git root "commit" "--quiet" "--message" "Add F-1")
    (trf-test-write-files root '(("docs/vision.org" . "#+TITLE: Vision\n\nMore.\n")))
    (let ((commit (trf-record-changes-commit root (trf-record-changes root))))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "docs/vision.org\n"
                    (trf-test-git root "show" "--name-only" "--format=" commit))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "Record changes\n\n- Vision\n"
                    (trf-test-git root "log" "-1" "--format=format:%B")))))

(deftest trf-record-changes-commit-refused-leaves-the-index ()
  "When git refuses the commit, the index is as it was before and MSG-4 is given with git's reason."
  (let* ((root trf-record-changes-test-root)
         (hook (expand-file-name ".git/hooks/pre-commit" root)))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files root '(("docs/features/f-1-order.org" . "* F-1 Order\n")
                                 ("docs/vision.org" . "#+TITLE: Vision\n\nMore.\n")))
    (trf-test-write-files
     root '((".git/hooks/pre-commit" . "#!/bin/sh\necho 'pre-commit refuses' >&2\nexit 1\n")))
    (set-file-modes hook #o755)
    (let ((head (trf-test-head root))
          (status (trf-test-git root "status" "--porcelain"))
          (refusal (condition-case err
                       (progn (trf-record-changes-commit root (trf-record-changes root)) nil)
                     (error err))))
      ;; UW-1 §Extensions: Turn record changes into code
      ;; MSG-4: Record changes could not be committed
      (assert-equal '(user-error "The record changes could not be committed:\npre-commit refuses")
                    refusal)
      ;; UW-1 §Minimal guarantee: Turn record changes into code
      (assert-equal status (trf-test-git root "status" "--porcelain"))
      ;; UW-1 §Minimal guarantee: Turn record changes into code
      (assert-equal head (trf-test-head root)))))

(deftest trf-record-changes-commit-refused-by-a-hook-that-stages ()
  "When a hook stages an edit and refuses the commit, the index is as it was and MSG-4 is given with git's reason."
  (let* ((root trf-record-changes-test-root)
         (hook (expand-file-name ".git/hooks/pre-commit" root)))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files root '(("docs/features/f-1-order.org" . "* F-1 Order\n")))
    (trf-test-write-files
     root '((".git/hooks/pre-commit"
             . "#!/bin/sh\necho more >> docs/features/f-1-order.org\ngit add docs/features/f-1-order.org\necho 'pre-commit refuses' >&2\nexit 1\n")))
    (set-file-modes hook #o755)
    (let ((head (trf-test-head root))
          (status (trf-test-git root "status" "--porcelain"))
          (refusal (condition-case err
                       (progn (trf-record-changes-commit root (trf-record-changes root)) nil)
                     (error err))))
      ;; UW-1 §Extensions: Turn record changes into code
      ;; MSG-4: Record changes could not be committed
      (assert-equal '(user-error "The record changes could not be committed:\npre-commit refuses")
                    refusal)
      ;; UW-1 §Minimal guarantee: Turn record changes into code
      (assert-equal status (trf-test-git root "status" "--porcelain"))
      ;; UW-1 §Minimal guarantee: Turn record changes into code
      (assert-equal head (trf-test-head root)))))

(defun trf-record-changes-test-commit-with-stale-index-lock (files)
  "Commit the record changes after writing FILES, with a stale index lock.
Assert MSG-4 is given with git's reason, and the status and HEAD are unchanged."
  (let ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files root files)
    (let ((changes (trf-record-changes root))
          (head (trf-test-head root))
          (status (trf-test-git root "status" "--porcelain")))
      (trf-test-write-files root '((".git/index.lock" . "")))
      (let ((refusal (unwind-protect
                         (condition-case err
                             (progn (trf-record-changes-commit root changes) nil)
                           (error err))
                       (delete-file (expand-file-name ".git/index.lock" root)))))
        ;; UW-1 §Extensions: Turn record changes into code
        ;; MSG-4: Record changes could not be committed
        (assert-equal 'user-error (car refusal))
        ;; UW-1 §Extensions: Turn record changes into code
        ;; MSG-4: Record changes could not be committed
        (assert-true (string-prefix-p
                      (concat "The record changes could not be committed:\n"
                              "fatal: Unable to create '"
                              (expand-file-name ".git/index.lock" root)
                              "': File exists.")
                      (cadr refusal)))
        ;; UW-1 §Minimal guarantee: Turn record changes into code
        (assert-equal status (trf-test-git root "status" "--porcelain"))
        ;; UW-1 §Minimal guarantee: Turn record changes into code
        (assert-equal head (trf-test-head root))))))

(deftest trf-record-changes-commit-refused-new-record-change-with-stale-index-lock ()
  "With a stale index lock, a new record change is not committed and MSG-4 is given with git's reason."
  (trf-record-changes-test-commit-with-stale-index-lock
   '(("docs/features/f-1-order.org" . "* F-1 Order\n"))))

(deftest trf-record-changes-commit-refused-modified-record-change-with-stale-index-lock ()
  "With a stale index lock, a modified record change is not committed and MSG-4 is given with git's reason."
  (trf-record-changes-test-commit-with-stale-index-lock
   '(("docs/vision.org" . "#+TITLE: Vision\n\nMore.\n"))))

(defun trf-record-changes-test-file-bytes (file)
  "Return FILE's bytes."
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally file)
    (buffer-string)))

(defun trf-record-changes-test-git-stderr (root &rest args)
  "Return what git prints to standard error, trimmed, when run with ARGS in ROOT."
  (let ((stderr-file (make-temp-file "trf-test-stderr-")))
    (unwind-protect
        (let ((default-directory root))
          (apply #'call-process "git" nil (list nil stderr-file) nil args)
          (string-trim (with-temp-buffer
                         (insert-file-contents stderr-file)
                         (buffer-string))))
      (delete-file stderr-file))))

(deftest trf-record-changes-commit-refused-with-a-corrupt-index ()
  "With a corrupt index, the record changes are not committed and MSG-4 is given with git's output."
  (let* ((root trf-record-changes-test-root)
         (index (expand-file-name ".git/index" root)))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files root '(("docs/features/f-1-order.org" . "* F-1 Order\n")))
    (let ((changes (trf-record-changes root))
          (head (trf-test-head root)))
      (let ((coding-system-for-write 'no-conversion))
        (write-region (substring (trf-record-changes-test-file-bytes index) 0 10)
                      nil index nil 'silent))
      (let ((corrupt (trf-record-changes-test-file-bytes index))
            (reason (trf-record-changes-test-git-stderr root "ls-files"))
            (refusal (condition-case err
                         (progn (trf-record-changes-commit root changes) nil)
                       (error err))))
        ;; UW-1 §Extensions: Turn record changes into code
        ;; MSG-4: Record changes could not be committed
        (assert-equal (list 'user-error
                            (concat "The record changes could not be committed:\n" reason))
                      refusal)
        ;; UW-1 §Minimal guarantee: Turn record changes into code
        (assert-equal corrupt (trf-record-changes-test-file-bytes index))
        ;; UW-1 §Minimal guarantee: Turn record changes into code
        (assert-equal head (trf-test-head root))))))

(defun trf-record-changes-test-commit-interrupted (condition)
  "Commit a new record change, signalling CONDITION once it is staged.
Return what was signalled, and the status before and after."
  (let ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files root '(("docs/features/f-1-order.org" . "* F-1 Order\n")))
    (let* ((changes (trf-record-changes root))
           (status (trf-test-git root "status" "--porcelain"))
           (signalled (condition-case err
                          (cl-letf (((symbol-function 'trf-record-changes--commit-message)
                                     (lambda (_changes) (signal (car condition) (cdr condition)))))
                            (trf-record-changes-commit root changes)
                            nil)
                        ((debug error quit) err))))
      (list signalled status (trf-test-git root "status" "--porcelain")))))

(deftest trf-record-changes-commit-resignals-an-error-that-is-not-git-s ()
  "An error that is not git's is signalled unchanged, after the staging is rolled back."
  (pcase-let ((`(,signalled ,status-before ,status-after)
               (trf-record-changes-test-commit-interrupted '(error "Not git's"))))
    (assert-equal '(error "Not git's") signalled)
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    (assert-equal status-before status-after)))

(deftest trf-record-changes-commit-rolls-back-on-quit ()
  "A quit while committing leaves nothing staged."
  (pcase-let ((`(,signalled ,status-before ,status-after)
               (trf-record-changes-test-commit-interrupted '(quit))))
    (assert-equal '(quit) signalled)
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    (assert-equal status-before status-after)))

(deftest trf-record-changes-commit-keeps-the-commit-on-a-quit-after-it ()
  "A quit landing once git has committed leaves the commit and nothing staged."
  (let* ((root trf-record-changes-test-root)
         (git (symbol-function 'trf-record-changes--git)))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files root '(("docs/features/f-1-order.org" . "* F-1 Order\n")))
    (let* ((changes (trf-record-changes root))
           (signalled (condition-case err
                          (cl-letf (((symbol-function 'trf-record-changes--git)
                                     (lambda (directory &rest args)
                                       (prog1 (apply git directory args)
                                         (when (member "commit" args)
                                           (signal 'quit nil))))))
                            (trf-record-changes-commit root changes)
                            nil)
                        (quit err))))
      (assert-equal '(quit) signalled)
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "docs/features/f-1-order.org\n"
                    (trf-test-git root "show" "--name-only" "--format=" "HEAD"))
      ;; UW-1 §Minimal guarantee: Turn record changes into code
      (assert-equal "" (trf-test-git root "status" "--porcelain")))))

(deftest trf-record-changes-commit-refused-without-a-reason ()
  "When git refuses the commit and gives no reason, MSG-4 is given alone."
  (let* ((root trf-record-changes-test-root)
         (hook (expand-file-name ".git/hooks/pre-commit" root)))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files root '(("docs/features/f-1-order.org" . "* F-1 Order\n")
                                 (".git/hooks/pre-commit" . "#!/bin/sh\nexit 1\n")))
    (set-file-modes hook #o755)
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-4: Record changes could not be committed
    (assert-equal '(user-error "The record changes could not be committed:")
                  (condition-case err
                      (progn (trf-record-changes-commit root (trf-record-changes root)) nil)
                    (error err)))))

(deftest trf-record-changes-commit-first-commit ()
  "In a target product with no commit yet, the record changes make the first commit."
  (delete-directory trf-record-changes-test-root t)
  (let ((root (setq trf-record-changes-test-root
                    (trf-test-make-target-product nil t))))
    (let ((commit (trf-record-changes-commit root (trf-record-changes root))))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal (trf-test-head root) commit)
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal (concat "docs/ubiquitous-language.org\n"
                            "docs/user-workflows.org\n"
                            "docs/user-workflows/uw-1-order-a-widget.org\n"
                            "docs/vision.org\n")
                    (trf-test-git root "show" "--name-only" "--format=" commit)))))

(deftest trf-record-changes-commit-without-record-changes ()
  "Without record changes, nothing is committed, whatever else has changed."
  (let* ((root trf-record-changes-test-root)
         (head (trf-test-head root)))
    (trf-test-write-files root '(("src/widget.el" . "(provide 'widget) ; edited\n")
                                 ("src/gadget.el" . "(provide 'gadget)\n")))
    (let ((status (trf-test-git root "status" "--porcelain")))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal head (trf-record-changes-commit root nil))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal status (trf-test-git root "status" "--porcelain")))))

(deftest trf-record-changes-commit-record-path-is-literal ()
  "A record change whose path reads as a pattern commits only itself."
  (let ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files root '(("docs/features/f-[ab].org" . "* F-1 Order\n")
                                 ("docs/features/f-a.org" . "* F-2 Pay\n")))
    (let ((commit (trf-record-changes-commit
                   root (seq-filter (lambda (change)
                                      (equal (plist-get change :path) "docs/features/f-[ab].org"))
                                    (trf-record-changes root)))))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "docs/features/f-[ab].org\n"
                    (trf-test-git root "show" "--name-only" "--format=" commit))
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (assert-equal "?? docs/features/f-a.org\n"
                    (trf-test-git root "status" "--porcelain")))))

(defun trf-record-changes-test-commit-in-progress (operation)
  "Commit a record change while OPERATION is in progress.
Return the error signalled, the status before and after, and HEAD before and after."
  (let ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (trf-test-leave-in-progress root operation)
    (trf-test-write-files root '(("docs/vision.org" . "#+TITLE: Vision\n\nMore.\n")))
    (let ((status (trf-test-git root "status" "--porcelain"))
          (head (trf-test-head root))
          (refusal (condition-case err
                       (progn (trf-record-changes-commit root (trf-record-changes root)) nil)
                     (error err))))
      (list refusal
            status (trf-test-git root "status" "--porcelain")
            head (trf-test-head root)))))

(defun trf-record-changes-test-refuses-in-progress (operation)
  (pcase-let ((`(,refusal ,status-before ,status-after ,head-before ,head-after)
               (trf-record-changes-test-commit-in-progress operation)))
    ;; UW-1 §Extensions: Turn record changes into code
    ;; MSG-3: Finish the merge or rebase first
    (assert-equal '(user-error
                    "The target product is in the middle of a merge or rebase; finish it first.")
                  refusal)
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    (assert-equal status-before status-after)
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    (assert-equal head-before head-after)))

(deftest trf-record-changes-commit-refused-mid-merge ()
  "Mid merge, the record changes are not committed and MSG-3 is given."
  (trf-record-changes-test-refuses-in-progress 'merge))

(deftest trf-record-changes-commit-refused-mid-rebase ()
  "Mid rebase, the record changes are not committed and MSG-3 is given."
  (trf-record-changes-test-refuses-in-progress 'rebase))

(deftest trf-record-changes-commit-refused-mid-cherry-pick ()
  "Mid cherry-pick, the record changes are not committed and MSG-3 is given."
  (trf-record-changes-test-refuses-in-progress 'cherry-pick))

(deftest trf-record-changes-commit-refused-mid-revert ()
  "Mid revert, the record changes are not committed and MSG-3 is given."
  (trf-record-changes-test-refuses-in-progress 'revert))

(deftest trf-record-changes-commit-refused-mid-bisect ()
  "Mid bisect, the record changes are not committed and MSG-3 is given."
  (trf-record-changes-test-refuses-in-progress 'bisect))

(provide 'trf-record-changes-test)
;;; trf-record-changes-test.el ends here
