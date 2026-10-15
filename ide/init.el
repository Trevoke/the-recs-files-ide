;;; init.el --- The IDE's init -*- lexical-binding: t; -*-

(require 'package)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)

;; ADR-1 §Decision: Ship the IDE as a bare Emacs with its own pinned binary
(defconst trf-ide-packages
  '((shell-maker :url "https://github.com/xenodium/shell-maker" :rev "v0.97.5")
    (acp :url "https://github.com/xenodium/acp.el" :rev "v0.15.1")
    ;; ADR-2 §Context: Talk to machines over ACP
    (agent-shell :url "https://github.com/xenodium/agent-shell" :rev "v0.86.2")))

(defun trf-ide-git-rev (dir rev)
  (with-temp-buffer
    (let ((default-directory dir))
      (when (eql 0 (call-process "git" nil t nil "rev-parse" "--verify" "--quiet"
                                 (concat rev "^{commit}")))
        (string-trim (buffer-string))))))

(defun trf-ide-package-at-pin-p (desc rev)
  (let* ((dir (file-name-as-directory (package-desc-dir desc)))
         (head (and (file-exists-p (expand-file-name ".git" dir))
                    (trf-ide-git-rev dir "HEAD"))))
    (and head (equal head (trf-ide-git-rev dir rev)))))

;; ADR-1 §Consequences: Ship the IDE as a bare Emacs with its own pinned binary
(dolist (spec trf-ide-packages)
  (let* ((name (car spec))
         (rev (plist-get (cdr spec) :rev))
         (desc (cadr (assq name package-alist))))
    (unless (and desc (trf-ide-package-at-pin-p desc rev))
      (when desc
        (package-delete desc t))
      (package-vc-install (list name :url (plist-get (cdr spec) :url)) rev))))

(add-to-list 'load-path (expand-file-name "../lisp/" user-emacs-directory))
(require 'trf)
(require 'trf-emacs)
(trf-emacs-keep-files-outside-the-target-product)

(global-set-key (kbd "C-c r i") #'trf-implement-record-changes)
(global-set-key (kbd "C-c r a") #'trf-accept-record-implementation-report)
(global-set-key (kbd "C-c r r") #'trf-reject-record-implementation-report)
(global-set-key (kbd "C-c r c") #'trf-continue-stopped-machine)

;;; init.el ends here
