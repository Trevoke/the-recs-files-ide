;;; trf.el --- The IDE for projects that keep records -*- lexical-binding: t; -*-

(require 'trf-group)
(require 'trf-git)
(require 'trf-project)
(require 'trf-record-changes)
(require 'trf-worktree)
(require 'trf-machines)
(require 'trf-session)

(defconst trf-version "0.1.0")

;; MSG-1: No record changes to implement
(defconst trf-no-record-changes-message "There are no record changes to implement.")

;; MSG-2: No work in progress to accept or reject
(defconst trf-no-work-in-progress-message
  "There is no work in progress here to accept or reject.")

;; MSG-5: Work cannot be brought into the codebase
(defconst trf-work-not-brought-in-message
  "The work cannot be brought into the codebase:")

;; MSG-8: Target product is not on the primary git branch
(defconst trf-not-on-the-primary-git-branch-message
  "The target product is not on the primary git branch:")

;; MSG-9: No primary git branch is named
(defconst trf-no-primary-git-branch-message
  "Please set a primary git branch first.")

;; MSG-10: Machine stopped before finishing
(defconst trf-machine-stopped-message
  "A machine stopped before finishing:")

;; MSG-7: Git could not do what the IDE asked
(defconst trf-git-failed-message
  "Git could not do what the IDE asked:")

;; UW-1 §Extensions: Turn record changes into code
(defconst trf-codebase-moved-instruction
  "The codebase has moved since this work started; bring the work up to date with it.")

(defcustom trf-primary-git-branch "main"
  "The primary git branch of the target product."
  :type '(choice (const :tag "None" nil) string)
  :safe (lambda (value) (or (null value) (stringp value)))
  :group 'trf)

(defun trf--primary-git-branch (root)
  "Return `trf-primary-git-branch' as it applies to the target product's ROOT."
  (with-temp-buffer
    (setq default-directory (file-name-as-directory root))
    (let ((enable-local-variables (and enable-local-variables :safe)))
      (hack-dir-local-variables-non-file-buffer))
    trf-primary-git-branch))

;; UW-1 §Extensions: Turn record changes into code
(defun trf--refuse-unless-on-the-primary-git-branch (root)
  "Signal a `user-error' unless ROOT is on the primary git branch."
  (let ((primary (trf--primary-git-branch root)))
    (unless (and (stringp primary) (not (string-empty-p primary)))
      ;; MSG-9: No primary git branch is named
      (user-error "%s" trf-no-primary-git-branch-message))
    (unless (equal primary (trf-git-current-branch root))
      ;; MSG-8: Target product is not on the primary git branch
      (user-error "%s\n%s" trf-not-on-the-primary-git-branch-message primary))))

(defmacro trf--giving-git-failures (&rest body)
  "Run BODY; turn a `trf-git-error' it signals into a `user-error'."
  (declare (indent 0) (debug t))
  `(condition-case err
       (progn ,@body)
     ((debug trf-git-error)
      ;; MSG-7: Git could not do what the IDE asked
      (user-error "%s\n%s" trf-git-failed-message (trf-record-changes-reason (cadr err))))))

(defconst trf--record-changes-buffer-name "*trf record changes*")

(defun trf--show-record-changes (changes)
  "Show CHANGES in the buffer `trf--record-changes-buffer-name'."
  (with-current-buffer (get-buffer-create trf--record-changes-buffer-name)
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (mapconcat #'trf-machines-change-line changes "\n") "\n"))
    (special-mode)
    (display-buffer (current-buffer))))

;; F-2 §Rules: Accept or reject the record implementation report
(defvar trf--session-buffers (make-hash-table :test #'equal)
  "The machines' session buffers, keyed by the worktree they work in.")

;; ADR-3 §Consequences: Machines end their work with one JSON line
(defun trf--run-machines (machines directory config changes produced on-done
                                    &optional ide-instruction)
  "Run MACHINES in turn in DIRECTORY with the AI program CONFIG on CHANGES.
Give the first the paths PRODUCED before it and IDE-INSTRUCTION; call ON-DONE once the last
has finished."
  (if (null machines)
      (funcall on-done)
    (let ((machine (car machines)))
      (push (trf-session-run
             directory config
             (trf-machines-instructions machine changes produced ide-instruction)
             (lambda (produced)
               (trf--run-machines (cdr machines) directory config changes produced on-done))
             (lambda (&optional reason)
               ;; MSG-10: Machine stopped before finishing
               ;; UW-1 §Extensions: Turn record changes into code
               ;; ADR-3 §Consequences: Machines end their work with one JSON line
               (message "%s\n%s\n%s" trf-machine-stopped-message (plist-get machine :name)
                        (trf-record-changes-reason (or reason "")))))
            (gethash (file-name-as-directory (expand-file-name directory))
                     trf--session-buffers)))))

(defun trf--assess (directory config changes)
  "Run the reconciliation assessor in DIRECTORY with CONFIG on CHANGES, then open its report."
  (let ((assessor (trf-machines-reconciliation-assessor)))
    (trf--run-machines
     (list assessor) directory config changes nil
     (lambda ()
       (let ((report (expand-file-name (trf-machines-record-implementation-report-file)
                                       directory)))
         ;; UW-1 §Extensions: Turn record changes into code
         (when-let* ((buffer (find-buffer-visiting report)))
           (with-current-buffer buffer
             (revert-buffer t t t)))
         (pop-to-buffer-same-window (find-file-noselect report)))))))

(defun trf--implement (directory config changes &optional ide-instruction)
  "Run the implementation workflow in DIRECTORY with CONFIG on CHANGES, then the assessor.
Give the brainstorming machine IDE-INSTRUCTION."
  (trf--run-machines (trf-machines-implementation-workflow)
                     directory config changes nil
                     (lambda () (trf--assess directory config changes))
                     ide-instruction))

(defun trf--target-product-root ()
  "Return the root of the target product holding `default-directory'.
From inside a worktree, return the target product's root it was created from."
  (let ((worktree (trf-worktree-of default-directory)))
    (cond (worktree (trf-worktree-root worktree))
          ((trf-project-root))
          (t default-directory))))

(defun trf--offer-to-save (directory &optional except)
  "Offer to save each modified buffer visiting a file in DIRECTORY and not in EXCEPT."
  (when-let* ((buffers (seq-filter
                        (lambda (buffer)
                          (let ((file (buffer-file-name buffer)))
                            (and (buffer-modified-p buffer)
                                 (trf--in-directory-p directory file)
                                 (not (and except (trf--in-directory-p except file))))))
                        (buffer-list))))
    (map-y-or-n-p (lambda (buffer) (format "Save file %s? " (buffer-file-name buffer)))
                  (lambda (buffer) (with-current-buffer buffer (save-buffer)))
                  buffers
                  '("file" "files" "save"))))

;; F-1 §Rules: Ask the IDE to implement the record changes
(defun trf-implement-record-changes ()
  "Ask the IDE to implement the record changes."
  (interactive)
  (trf--giving-git-failures
    (let ((root (trf--target-product-root)))
      (trf-record-changes-refuse-operation-in-progress root)
      (trf--refuse-unless-on-the-primary-git-branch root)
      ;; F-1 §Rules: Ask the IDE to implement the record changes
      (trf--offer-to-save root (expand-file-name ".worktrees/" root))
      (let ((changes (trf-record-changes root)))
        (unless changes
          (user-error "%s" trf-no-record-changes-message))
        (trf--show-record-changes changes)
        (when (y-or-n-p "Implement these record changes? ")
          (let* ((config (trf-session-config))
                 (commit (trf-record-changes-commit root changes))
                 (directory (trf-worktree-create root commit)))
            (trf--implement directory config changes)))))))

(defun trf--work-in-progress ()
  "Return the worktree holding the current buffer's file, or signal a `user-error'."
  (or (trf-worktree-of (or buffer-file-name default-directory))
      ;; F-2 §Rules: Accept or reject the record implementation report
      ;; MSG-2: No work in progress to accept or reject
      (user-error "%s" trf-no-work-in-progress-message)))

(defun trf--in-directory-p (directory file)
  "Return non-nil when FILE is in DIRECTORY."
  (and file (string-prefix-p directory (expand-file-name file))))

;; F-2 §Produces: Accept or reject the record implementation report
(defun trf--buffers-visiting (directory)
  "Return the buffers visiting files in DIRECTORY."
  (seq-filter (lambda (buffer) (trf--in-directory-p directory (buffer-file-name buffer)))
              (buffer-list)))

;; F-2 §Produces: Accept or reject the record implementation report
;; docs/knowledge-base/agent-shell-session-buffers.org §Killing a session buffer shuts down its ACP client
;; docs/knowledge-base/agent-shell-session-buffers.org §The ACP process is not the session buffer's process
;; docs/knowledge-base/agent-shell-session-buffers.org §Killing a session buffer asks about queued prompts and an unsaved transcript
(defun trf--kill-the-works-buffers (directory root)
  "Kill the unmodified buffers visiting files in DIRECTORY, and its session buffers.
Move every other buffer whose directory is in DIRECTORY to ROOT."
  (let ((key (file-name-as-directory (expand-file-name directory))))
    (dolist (buffer (trf--buffers-visiting directory))
      (unless (buffer-modified-p buffer)
        (kill-buffer buffer)))
    ;; F-2 §Rules: Accept or reject the record implementation report
    (let ((kept (seq-filter (lambda (buffer)
                              (when (buffer-live-p buffer)
                                (kill-buffer buffer))
                              (buffer-live-p buffer))
                            (delete-dups (copy-sequence (gethash key trf--session-buffers))))))
      (if kept
          (puthash key kept trf--session-buffers)
        (remhash key trf--session-buffers)))
    (dolist (buffer (buffer-list))
      (when (trf--in-directory-p directory (buffer-local-value 'default-directory buffer))
        (with-current-buffer buffer
          (setq default-directory (file-name-as-directory root)))))))

;; F-2 §Produces: Accept or reject the record implementation report
(defun trf--remove-work-in-progress (directory)
  "Remove the work in progress DIRECTORY and kill or move the buffers in it."
  (let ((root (trf-worktree-root directory)))
    (trf-worktree-remove directory)
    (trf--kill-the-works-buffers directory root)))

;; F-2 §Takes: Accept or reject the record implementation report
(defun trf-accept-record-implementation-report ()
  "Accept the record implementation report of the work in progress here."
  (interactive)
  ;; F-2 §Rules: Accept or reject the record implementation report
  (trf--giving-git-failures
    (let* ((directory (trf--work-in-progress))
           (root (trf-worktree-root directory)))
      (trf-record-changes-refuse-operation-in-progress root)
      (trf--refuse-unless-on-the-primary-git-branch root)
      ;; F-2 §Rules: Accept or reject the record implementation report
      (trf--offer-to-save directory)
      ;; UW-1 §Success guarantee: Turn record changes into code
      (let ((brought-in (trf-worktree-brought-in-p directory)))
        ;; F-2 §Rules: Accept or reject the record implementation report
        (trf-worktree-commit-saved-edits directory)
        (if (and (not brought-in) (trf-worktree-codebase-moved-p directory))
            ;; F-2 §Rules: Accept or reject the record implementation report
            ;; UW-1 §Extensions: Turn record changes into code
            (trf--implement directory (trf-session-config)
                            (trf-record-changes-in-commit root (trf-worktree-from directory)
                                                          (trf-worktree-base directory))
                            trf-codebase-moved-instruction)
          (condition-case err
              ;; F-2 §Produces: Accept or reject the record implementation report
              (trf-worktree-accept directory)
            (trf-worktree-merge-failed
             ;; UW-1 §Extensions: Turn record changes into code
             ;; MSG-5: Work cannot be brought into the codebase
             (user-error "%s\n%s" trf-work-not-brought-in-message
                         (trf-record-changes-reason (cadr err)))))
          (trf--remove-work-in-progress directory))))))

;; F-2 §Takes: Accept or reject the record implementation report
(defun trf-reject-record-implementation-report ()
  "Reject the record implementation report of the work in progress here, keeping the work."
  (interactive)
  (let* ((directory (trf--work-in-progress))
         (report (find-buffer-visiting
                  (expand-file-name (trf-machines-record-implementation-report-file)
                                    directory))))
    ;; F-2 §Produces: Accept or reject the record implementation report
    (when report
      (kill-buffer report))))

(provide 'trf)
;;; trf.el ends here
