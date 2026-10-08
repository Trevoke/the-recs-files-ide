;;; trf-session.el --- Machines over ACP -*- lexical-binding: t; -*-

(require 'map)
(require 'agent-shell)
(require 'trf-machines)

(defcustom trf-ai-program-config nil
  "The AI program the IDE runs as machines.
It is a config alist from `agent-shell-agent-configs'.  When nil, the record keeper picks one through agent-shell."
  :type 'sexp
  :group 'trf)

(defun trf-session-config ()
  "Return `trf-ai-program-config', or ask the record keeper for one.
The record keeper picks it through agent-shell."
  (or trf-ai-program-config
      (agent-shell-select-config :prompt "AI program for the machines: ")))

(defun trf-session--handoff-text (handoff)
  (with-temp-buffer
    (insert-file-contents handoff)
    (buffer-string)))

(defun trf-session--stopped-p (stop-reason)
  (not (member stop-reason '(nil "end_turn"))))

;; ADR-2 §Decision: Talk to machines over ACP
(defun trf-session-run (machine directory config instructions on-finished on-stopped)
  "Run MACHINE in DIRECTORY with the AI program CONFIG and INSTRUCTIONS.
Return its session buffer.  MACHINE's handoff file is deleted before the
session starts.  Call ON-FINISHED with the handoff file's text at the end
of the first turn in which the handoff file exists.  Call ON-STOPPED, with
the session buffer displayed, if the session fails, or a turn ends for a
reason other than \"end_turn\", before then.  If the session's buffer is
killed first, call neither."
  (let* ((default-directory directory)
         (handoff (expand-file-name (trf-machines-handoff-file machine) directory))
         (buffer (progn
                   (when (file-exists-p handoff) (delete-file handoff))
                   (let ((agent-shell-session-strategy 'new))
                     (agent-shell-start :config config))))
         (over nil)
         (finish (lambda ()
                   (let ((text (trf-session--handoff-text handoff)))
                     (run-at-time 0 nil on-finished text))))
         (stop (lambda ()
                 (when (buffer-live-p buffer) (display-buffer buffer))
                 (run-at-time 0 nil on-stopped))))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'init-finished
     :on-event (lambda (_event)
                 (agent-shell-insert :text instructions :submit t :no-focus t
                                     :shell-buffer buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'turn-complete
     :on-event (lambda (event)
                 (unless over
                   (cond ((file-exists-p handoff)
                          (setq over t)
                          (funcall finish))
                         ;; UW-1 §Extensions: Turn record changes into code
                         ((trf-session--stopped-p
                           (map-elt (map-elt event :data) :stop-reason))
                          (setq over t)
                          (funcall stop))))))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'error
     :on-event (lambda (_event)
                 (unless over
                   (setq over t)
                   (if (file-exists-p handoff)
                       (funcall finish)
                     (funcall stop)))))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'clean-up
     :on-event (lambda (_event) (setq over t)))
    buffer))

(provide 'trf-session)
;;; trf-session.el ends here
