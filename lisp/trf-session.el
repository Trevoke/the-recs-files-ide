;;; trf-session.el --- Machines over ACP -*- lexical-binding: t; -*-

(require 'map)
(require 'seq)
(require 'agent-shell)
(require 'trf-group)
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

;; ADR-3 §Decision: Machines end their work with one JSON line
(defun trf-session--without-closing-fences (lines)
  "Return LINES without the code fence closings that end them."
  (let ((lines (reverse lines)))
    (while (and lines (string-match-p "\\`\\(?:`\\{3,\\}\\|~\\{3,\\}\\)\\'" (car lines)))
      (setq lines (cdr lines)))
    (nreverse lines)))

;; ADR-3 §Decision: Machines end their work with one JSON line
(defun trf-session--ending (text)
  "Return how the last line of TEXT says the machine ended.
A machine that finished gives (finished PRODUCED), with the paths it produced.
A machine that stopped before finishing gives (stopped REASON), with the reason
it gives, or nil when it gives none as text.  A last line that says neither
gives nil."
  (when-let* ((line (car (last (trf-session--without-closing-fences
                                (split-string (or text "") "[\r\n]+" t "[ \t]+"))))))
    (let ((start 0) found)
      (while (and (not found) (string-match "{" line start))
        (let ((position (match-beginning 0)))
          (setq start (1+ position))
          (when-let* ((object (ignore-errors
                                (json-parse-string (substring line position)
                                                   :object-type 'alist
                                                   :array-type 'list
                                                   :false-object :false
                                                   :null-object nil)))
                      ((consp object)))
            (let ((finished (alist-get 'finished object))
                  (produced (alist-get 'produced object))
                  (reason (alist-get 'reason object)))
              (cond ((eq finished t)
                     (setq found (list 'finished (when (listp produced)
                                                   (seq-filter #'stringp produced)))))
                    ;; ADR-3 §Consequences: Machines end their work with one JSON line
                    ;; UW-1 §Extensions: Turn record changes into code
                    ((eq finished :false)
                     (setq found (list 'stopped (when (stringp reason) reason)))))))))
      found)))

(defun trf-session--stopped-p (stop-reason)
  (not (member stop-reason '(nil "end_turn"))))

;; ADR-4 §Decision: Machines ask the record keeper through ACP elicitation
;; docs/knowledge-base/agent-shell-elicitation.org §The initialize request reads the switch while agent-shell-start runs
;; docs/knowledge-base/agent-shell-elicitation.org §An elicitation request reads the switch in the session buffer
(defun trf-session--start (config)
  "Start a new session of the AI program CONFIG, advertising and answering form elicitation.
Return its buffer."
  (let ((buffer (let ((agent-shell-session-strategy 'new)
                      (agent-shell-elicitation--experimental-feature-enabled t))
                  (agent-shell-start :config config))))
    (with-current-buffer buffer
      (setq-local agent-shell-elicitation--experimental-feature-enabled t))
    buffer))

;; ADR-2 §Decision: Talk to machines over ACP
;; ADR-3 §Decision: Machines end their work with one JSON line
;; ADR-3 §Consequences: Machines end their work with one JSON line
;; docs/knowledge-base/agent-shell-agent-messages.org §agent-shell streams each chunk of an agent message as an event
;; docs/knowledge-base/agent-shell-agent-messages.org §A turn's end carries no message text
(defun trf-session-run (directory config instructions on-finished on-stopped)
  "Run a machine in DIRECTORY with the AI program CONFIG and INSTRUCTIONS.
Return its session buffer.
Call ON-FINISHED with the paths the machine produced, at the end of the first
turn ending with the JSON line saying it has finished.
Call ON-STOPPED with the session buffer displayed, before then, in three cases.
When a turn ends with the JSON line saying the machine stopped before finishing,
pass the reason it gives.  When the session fails, pass nil.  When a turn ends
for a reason other than \"end_turn\", pass nil.
If the session's buffer is killed first, call neither."
  (let* ((default-directory directory)
         (buffer (trf-session--start config))
         (text "")
         (over nil)
         (finish (lambda (produced)
                   (run-at-time 0 nil on-finished produced)))
         ;; ADR-3 §Consequences: Machines end their work with one JSON line
         ;; UW-1 §Extensions: Turn record changes into code
         (stop (lambda (&optional reason)
                 (when (buffer-live-p buffer) (display-buffer buffer))
                 (run-at-time 0 nil on-stopped reason)))
         (end (lambda (ending)
                (pcase ending
                  (`(finished ,produced) (funcall finish produced))
                  (`(stopped ,reason) (funcall stop reason))))))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'init-finished
     :on-event (lambda (_event)
                 (agent-shell-insert :text instructions :submit t :no-focus t
                                     :shell-buffer buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'agent-message-chunk
     :on-event (lambda (event)
                 (setq text (concat text (or (map-elt (map-elt event :data) :text-chunk) "")))))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'turn-complete
     :on-event (lambda (event)
                 (let ((ending (trf-session--ending text)))
                   (setq text "")
                   (unless over
                     (cond (ending
                            (setq over t)
                            (funcall end ending))
                           ;; UW-1 §Extensions: Turn record changes into code
                           ((trf-session--stopped-p
                             (map-elt (map-elt event :data) :stop-reason))
                            (setq over t)
                            (funcall stop)))))))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'error
     :on-event (lambda (_event)
                 (unless over
                   (setq over t)
                   (if-let* ((ending (trf-session--ending text)))
                       (funcall end ending)
                     (funcall stop)))))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'clean-up
     :on-event (lambda (_event) (setq over t)))
    buffer))

(provide 'trf-session)
;;; trf-session.el ends here
