;;; trf-project-test.el --- Tests for trf-project -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'mock-fs)
(require 'trf-test-helper)
(require 'trf-project)

(deftest trf-project-record-directories-reads-agents-md ()
  "Each projection but the vision has a directory of its records."
  (with-mock-fs `(("/target/AGENTS.md" . ,trf-test-agents-md))
    (assert-equal '("docs/user-workflows/" "docs/features/")
                  (trf-project-record-directories (mock-fs-path "/target/")))))

(deftest trf-project-record-directories-without-agents-md ()
  "A target product without AGENTS.md has no directories of records."
  (with-mock-fs '(("/target/" . nil))
    (assert-nil (trf-project-record-directories (mock-fs-path "/target/")))))

(deftest trf-project-vision-path-reads-agents-md ()
  "The vision is the file AGENTS.md names for the Vision projection."
  (with-mock-fs `(("/target/AGENTS.md" . ,trf-test-agents-md))
    (assert-equal "docs/vision.org"
                  (trf-project-vision-path (mock-fs-path "/target/")))))

(deftest trf-project-vision-path-without-vision ()
  "A target product whose AGENTS.md names no Vision projection has no vision."
  (with-mock-fs '(("/target/AGENTS.md" . "- **Features** (F-n) — `docs/features.org`\n"))
    (assert-nil (trf-project-vision-path (mock-fs-path "/target/")))))

(deftest trf-project-root-finds-agents-md ()
  "The target product's root is the directory holding AGENTS.md."
  (with-mock-fs `(("/target/AGENTS.md" . ,trf-test-agents-md)
                  ("/target/docs/vision.org" . "#+TITLE: Vision\n"))
    (assert-equal (mock-fs-path "/target/")
                  (trf-project-root (mock-fs-path "/target/docs/")))))

(deftest trf-project-root-is-expanded ()
  "The target product's root is an expanded directory name."
  (make-directory (expand-file-name ".cache/" "~/") t)
  (let ((root (file-name-as-directory
               (make-temp-file (expand-file-name "trf-root-test-" "~/.cache/") t))))
    (unwind-protect
        (progn
          (trf-test-write-files root `(("AGENTS.md" . ,trf-test-agents-md)
                                       ("docs/vision.org" . "#+TITLE: Vision\n")))
          (let ((found (trf-project-root (expand-file-name "docs/" root))))
            (assert-equal root found)
            (assert-nil (string-prefix-p "~" found))))
      (delete-directory root t))))

(provide 'trf-project-test)
;;; trf-project-test.el ends here
