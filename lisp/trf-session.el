;;; trf-session.el --- Machines over ACP -*- lexical-binding: t; -*-

(require 'agent-shell)
(require 'trf-machines)

(defcustom trf-ai-program-config nil
  "The agent-shell config of the AI program the IDE runs as machines.
When nil, the record keeper picks one through agent-shell."
  :type 'sexp
  :group 'trf)

(defun trf-session--config ()
  (or trf-ai-program-config
      (agent-shell-select-config :prompt "AI program for the machines: ")))

;; ADR-2 §Decision: Talk to machines over ACP
(defun trf-session-run (machine directory instructions on-finished on-stopped)
  "Run MACHINE in DIRECTORY with INSTRUCTIONS; return its session buffer.
Call ON-FINISHED with the handoff file's text once MACHINE writes it.
Call ON-STOPPED, with the session buffer displayed, if the session fails first."
  (let* ((default-directory directory)
         (handoff (expand-file-name (trf-machines-handoff-file machine) directory))
         (buffer (agent-shell-start :config (trf-session--config)))
         (over nil))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'init-finished
     :on-event (lambda (_event)
                 (agent-shell-insert :text instructions :submit t :no-focus t
                                     :shell-buffer buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'turn-complete
     :on-event (lambda (_event)
                 (when (and (not over) (file-exists-p handoff))
                   (setq over t)
                   (funcall on-finished
                            (with-temp-buffer
                              (insert-file-contents handoff)
                              (buffer-string))))))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'error
     :on-event (lambda (_event)
                 (unless over
                   (setq over t)
                   (when (buffer-live-p buffer) (display-buffer buffer))
                   (funcall on-stopped))))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'clean-up
     :on-event (lambda (_event) (setq over t)))
    buffer))

(provide 'trf-session)
;;; trf-session.el ends here
