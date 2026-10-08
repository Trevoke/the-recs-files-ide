;;; init.el --- The IDE's init -*- lexical-binding: t; -*-

(require 'package)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)

;; ADR-1 §Decision: Ship the IDE as a bare Emacs with its own pinned binary
(defconst trf-ide-packages
  '((shell-maker :url "https://github.com/xenodium/shell-maker" :rev "v0.97.5")
    (acp :url "https://github.com/xenodium/acp.el" :rev "v0.15.1")
    ;; ADR-2 §Decision: Talk to machines over ACP
    (agent-shell :url "https://github.com/xenodium/agent-shell" :rev "v0.86.2")))

(dolist (spec trf-ide-packages)
  (unless (package-installed-p (car spec))
    (package-vc-install (list (car spec) :url (plist-get (cdr spec) :url))
                        (plist-get (cdr spec) :rev))))

(add-to-list 'load-path (expand-file-name "../lisp/" user-emacs-directory))
(require 'trf)

(global-set-key (kbd "C-c r i") #'trf-implement-record-changes)
(global-set-key (kbd "C-c r a") #'trf-accept-record-implementation-report)
(global-set-key (kbd "C-c r r") #'trf-reject-record-implementation-report)

;;; init.el ends here
