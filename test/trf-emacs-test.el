;;; trf-emacs-test.el --- Tests for trf-emacs -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf-test-helper)
(require 'trf-emacs)

(defvar trf-emacs-test-root nil)
(defvar trf-emacs-test-state nil)

(setup-each ()
  (trf-test-enter-a-temporary-directory)
  (setq trf-emacs-test-root (trf-test-make-target-product))
  (setq trf-emacs-test-state (file-name-as-directory (make-temp-file "trf-state-" t))))

(teardown-each ()
  (unwind-protect
      (progn
        (trf-test-leave-the-temporary-directory)
        (dolist (buffer (buffer-list))
          (when (and (buffer-file-name buffer)
                     (string-prefix-p trf-emacs-test-root (buffer-file-name buffer)))
            (with-current-buffer buffer
              (set-buffer-modified-p nil))
            (kill-buffer buffer))))
    (delete-directory trf-emacs-test-root t)
    (delete-directory trf-emacs-test-state t)))

(defmacro trf-emacs-test-with-the-ides-files-kept (&rest body)
  "Run BODY with the IDE keeping its files under `trf-emacs-test-state'."
  (declare (indent 0) (debug t))
  `(let ((process-environment (cons (concat "XDG_STATE_HOME=" trf-emacs-test-state)
                                    process-environment))
         (auto-save-file-name-transforms auto-save-file-name-transforms)
         (lock-file-name-transforms lock-file-name-transforms)
         (backup-directory-alist backup-directory-alist))
     (trf-emacs-keep-files-outside-the-target-product)
     ,@body))

(deftest trf-emacs-keeps-files-in-the-state-directory ()
  "The IDE keeps auto-save, lock and backup files under its state directory."
  (trf-emacs-test-with-the-ides-files-kept
    (let ((file (expand-file-name "src/widget.el" trf-emacs-test-root))
          (state (expand-file-name "recs-files-ide/" trf-emacs-test-state)))
      ;; ADR-6 §Decision: Keep the IDE's Emacs files outside the target product
      (assert-true (string-prefix-p state (with-temp-buffer
                                             (setq buffer-file-name file)
                                             (make-auto-save-file-name))))
      ;; ADR-6 §Decision: Keep the IDE's Emacs files outside the target product
      (assert-true (string-prefix-p state (make-lock-file-name file)))
      ;; ADR-6 §Decision: Keep the IDE's Emacs files outside the target product
      (assert-true (string-prefix-p state (make-backup-file-name file))))))

(deftest trf-emacs-leaves-a-worktree-removable-with-a-modified-buffer-open ()
  "With an unsaved, auto-saved, modified buffer open, the worktree holds none of its files."
  (trf-emacs-test-with-the-ides-files-kept
    (let* ((root trf-emacs-test-root)
           (worktree (file-name-as-directory (expand-file-name ".worktrees/work" root)))
           (file (expand-file-name "src/widget.el" worktree)))
      (trf-test-git root "worktree" "add" "--quiet" "-b" "work" worktree "HEAD")
      (with-current-buffer (find-file-noselect file)
        ;; docs/knowledge-base/emacs-auto-save-lock-and-backup-files.org §Batch find-file leaves auto-save-mode off
        (auto-save-mode 1)
        (insert ";; edited\n")
        (do-auto-save t)
        (assert-true (buffer-modified-p))
        (assert-true (file-exists-p buffer-auto-save-file-name))
        (assert-true (file-symlink-p (make-lock-file-name file))))
      ;; ADR-6 §Consequences: Keep the IDE's Emacs files outside the target product
      (assert-equal "" (trf-test-git worktree "status" "--porcelain" "--untracked-files=all"))
      (trf-test-git root "worktree" "remove" worktree)
      (assert-nil (file-exists-p worktree)))))

(deftest trf-emacs-state-directory-ignores-an-empty-or-relative-xdg-state-home ()
  "An empty or relative XDG_STATE_HOME counts as unset."
  (dolist (value '("" "relative/state"))
    (let ((process-environment (cons (concat "XDG_STATE_HOME=" value) process-environment)))
      ;; ADR-6 §Decision: Keep the IDE's Emacs files outside the target product
      (assert-equal (expand-file-name "recs-files-ide/" "~/.local/state/")
                    (trf-emacs-state-directory)
                    (format "XDG_STATE_HOME=%S counts as unset" value)))))

(provide 'trf-emacs-test)
;;; trf-emacs-test.el ends here
