;;; trf.el --- The IDE for projects that keep records -*- lexical-binding: t; -*-

(require 'trf-git)
(require 'trf-project)
(require 'trf-record-changes)
(require 'trf-worktree)
(require 'trf-machines)
(require 'trf-session)

(defconst trf-version "0.1.0")

;; MSG-1: No record changes to implement
(defconst trf-no-record-changes-message "There are no record changes to implement.")

(defconst trf-record-changes-buffer-name "*trf record changes*")

(defun trf--show-record-changes (changes)
  "Show CHANGES in the buffer `trf-record-changes-buffer-name'."
  (with-current-buffer (get-buffer-create trf-record-changes-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (mapconcat #'trf-machines--change-line changes "\n") "\n"))
    (special-mode)
    (display-buffer (current-buffer))))

(defun trf--run-machines (machines directory config changes previous-handoff on-done)
  "Run MACHINES in turn in DIRECTORY with the AI program CONFIG on CHANGES.
Give the first PREVIOUS-HANDOFF; call ON-DONE once the last has finished."
  (if (null machines)
      (funcall on-done)
    (let ((machine (car machines)))
      (trf-session-run
       machine directory config
       (trf-machines-instructions machine changes previous-handoff)
       (lambda (handoff)
         (trf--run-machines (cdr machines) directory config changes handoff on-done))
       #'ignore))))

(defun trf--assess (directory config changes)
  "Run the reconciliation assessor in DIRECTORY with CONFIG on CHANGES, then open its report."
  (let ((assessor (trf-machines-reconciliation-assessor)))
    (trf--run-machines
     (list assessor) directory config changes nil
     (lambda ()
       (pop-to-buffer-same-window
        (find-file-noselect
         (expand-file-name (trf-machines-handoff-file assessor) directory)))))))

(defun trf--target-product-root ()
  "Return the root of the target product holding `default-directory'.
From inside a worktree, return the target product's root it was created from."
  (let ((worktree (trf-worktree-of default-directory)))
    (cond (worktree (trf-worktree-root worktree))
          ((trf-project-root))
          (t default-directory))))

;; F-1 §Rules: Ask the IDE to implement the record changes
;;;###autoload
(defun trf-implement-record-changes ()
  "Ask the IDE to implement the record changes."
  (interactive)
  (let ((root (trf--target-product-root)))
    ;; UW-1 §Extensions: Turn record changes into code
    (when (trf-git-operation-in-progress-p root)
      (user-error "%s" trf-record-changes-operation-in-progress-message))
    (let ((changes (trf-record-changes root)))
      (unless changes
        (user-error "%s" trf-no-record-changes-message))
      (trf--show-record-changes changes)
      (when (y-or-n-p "Implement these record changes? ")
        (let* ((config (trf-session-config))
               (commit (trf-record-changes-commit root changes))
               (directory (trf-worktree-create root commit)))
          (trf--run-machines (trf-machines-implementation-workflow)
                             directory config changes nil
                             (lambda () (trf--assess directory config changes))))))))

(provide 'trf)
;;; trf.el ends here
