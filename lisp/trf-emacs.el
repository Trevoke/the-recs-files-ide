;;; trf-emacs.el --- Where the IDE's Emacs keeps its own files -*- lexical-binding: t; -*-

(defun trf-emacs-state-directory ()
  "Return the directory the IDE keeps its own files in."
  (expand-file-name "recs-files-ide/" (or (getenv "XDG_STATE_HOME") "~/.local/state/")))

(defun trf-emacs--state-subdirectory (name)
  "Return the subdirectory NAME of the IDE's state directory, creating it."
  (let ((directory (file-name-as-directory
                    (expand-file-name name (trf-emacs-state-directory)))))
    (make-directory directory t)
    directory))

;; docs/knowledge-base/emacs-auto-save-lock-and-backup-files.org §The transforms put auto-save, lock and backup files under the directories named
(defun trf-emacs-keep-files-outside-the-target-product ()
  "Keep auto-save, lock and backup files under the IDE's state directory."
  (setq auto-save-file-name-transforms
        `((".*" ,(trf-emacs--state-subdirectory "auto-save") t)))
  (setq lock-file-name-transforms
        `((".*" ,(trf-emacs--state-subdirectory "lock") t)))
  (setq backup-directory-alist
        `(("." . ,(trf-emacs--state-subdirectory "backup")))))

(provide 'trf-emacs)
;;; trf-emacs.el ends here
