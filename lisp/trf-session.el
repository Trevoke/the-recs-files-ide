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
;; docs/knowledge-base/agent-shell-session-resume.org §The elicitation switch survives a resume
;; docs/knowledge-base/agent-shell-session-resume.org §Minimal verbosity sends session/resume
(defun trf-session--start (config &optional session-id)
  "Start a session of the AI program CONFIG, advertising and answering form elicitation.
With SESSION-ID, resume that session instead of starting a new one.
Return its buffer."
  (let ((buffer (let ((agent-shell-session-strategy 'new)
                      (agent-shell-elicitation--experimental-feature-enabled t))
                  (if session-id
                      (agent-shell-start :config config :session-id session-id)
                    (agent-shell-start :config config)))))
    (with-current-buffer buffer
      (setq-local agent-shell-elicitation--experimental-feature-enabled t))
    buffer))

(defvar-local trf-session--stopped-process nil
  "The agent process the machine in this buffer stopped in.")

;; ADR-3 §Decision: Machines end their work with one JSON line
;; ADR-3 §Consequences: Machines end their work with one JSON line
;; docs/knowledge-base/agent-shell-agent-messages.org §agent-shell streams each chunk of an agent message as an event
;; docs/knowledge-base/agent-shell-agent-messages.org §A turn's end carries no message text
(defun trf-session--listen (buffer on-finished on-stopped)
  "Listen to the machine in BUFFER until it finishes or stops.
Call ON-FINISHED with the paths it produced at the end of the first turn
ending with the JSON line saying it has finished.
Call ON-STOPPED with BUFFER displayed, before then, in three cases.
When a turn ends with the JSON line saying the machine stopped before finishing,
pass the reason it gives.  When the session fails, pass nil.  When a turn ends
for a reason other than \"end_turn\", pass nil.
If BUFFER is killed first, call neither."
  (let* ((text "")
         (over nil)
         (finish (lambda (produced)
                   (run-at-time 0 nil on-finished produced)))
         ;; ADR-3 §Consequences: Machines end their work with one JSON line
         ;; UW-1 §Extensions: Turn record changes into code
         (stop (lambda (&optional reason)
                 (when (buffer-live-p buffer)
                   (with-current-buffer buffer
                     (setq trf-session--stopped-process (trf-session--process buffer)))
                   (display-buffer buffer))
                 (run-at-time 0 nil on-stopped reason)))
         (end (lambda (ending)
                (pcase ending
                  (`(finished ,produced) (funcall finish produced))
                  (`(stopped ,reason) (funcall stop reason))))))
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
     :on-event (lambda (_event) (setq over t)))))

;; ADR-2 §Decision: Talk to machines over ACP
(defun trf-session-run (directory config instructions on-finished on-stopped)
  "Run a machine in DIRECTORY with the AI program CONFIG and INSTRUCTIONS.
Return its session buffer.  Call ON-FINISHED and ON-STOPPED as
`trf-session--listen' does."
  (let* ((default-directory directory)
         (buffer (trf-session--start config)))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'init-finished
     :on-event (lambda (_event)
                 (agent-shell-insert :text instructions :submit t :no-focus t
                                     :shell-buffer buffer)))
    (trf-session--listen buffer on-finished on-stopped)
    buffer))

(defun trf-session--process (buffer)
  "Return the agent process of the session in BUFFER."
  (map-elt (map-elt (buffer-local-value 'agent-shell--state buffer) :client) :process))

;; docs/knowledge-base/agent-shell-session-resume.org §The process status tells an ended session until its buffer is given a message
;; docs/knowledge-base/agent-shell-session-resume.org §A message in a dead buffer reaches a new process without its session
(defun trf-session--going-p (buffer)
  "Return non-nil while the agent process the machine in BUFFER stopped in still runs it."
  (and (buffer-live-p buffer)
       (let ((stopped (buffer-local-value 'trf-session--stopped-process buffer)))
         (and stopped
              (eq stopped (trf-session--process buffer))
              (process-live-p stopped)))))

(defun trf-session--give (buffer message on-finished on-stopped)
  "Give MESSAGE to the machine in BUFFER as its next message, listening afresh."
  (trf-session--listen buffer on-finished on-stopped)
  (agent-shell-insert :text message :submit t :no-focus t :shell-buffer buffer))

;; ADR-5 §Decision: Continue a machine in its own ACP session
;; ADR-5 §Consequences: Continue a machine in its own ACP session
;; docs/knowledge-base/agent-shell-session-resume.org §A live buffer takes a next message
;; docs/knowledge-base/agent-shell-session-resume.org §A message in a dead buffer reaches a new process without its session
;; docs/knowledge-base/agent-shell-session-resume.org §A resume that fails starts a new session
(defun trf-session-continue (buffer config message on-finished on-stopped on-cannot-continue)
  "Continue the machine whose session is in BUFFER, giving it MESSAGE as its next message.
CONFIG is its AI program.  Return the buffer the machine now works in.
While the session is going, give MESSAGE there.  Once it has ended, resume it
in a new buffer, end BUFFER, and give MESSAGE there.
Call ON-FINISHED and ON-STOPPED as `trf-session--listen' does.
Call ON-CANNOT-CONTINUE with the AI program's reason, or nil, when the session
cannot be resumed."
  (let ((cannot (lambda (reason)
                  (run-at-time 0 nil on-cannot-continue reason))))
    (cond
     ((trf-session--going-p buffer)
      (trf-session--give buffer message on-finished on-stopped)
      buffer)
     ((not (and (buffer-live-p buffer) (agent-shell-session-id :shell-buffer buffer)))
      (funcall cannot nil)
      nil)
     (t
      (let* ((asked (agent-shell-session-id :shell-buffer buffer))
             (default-directory (buffer-local-value 'default-directory buffer))
             (resumed (trf-session--start config asked))
             (settled nil))
        (agent-shell-subscribe-to
         :shell-buffer resumed :event 'init-finished
         :on-event (lambda (_event)
                     (unless settled
                       (setq settled t)
                       (if (equal (agent-shell-session-id :shell-buffer resumed) asked)
                           (progn
                             ;; docs/knowledge-base/agent-shell-session-buffers.org §Killing a session buffer asks about queued prompts and an unsaved transcript
                             (with-current-buffer buffer
                               (let ((kill-buffer-query-functions nil))
                                 (kill-buffer buffer)))
                             (trf-session--give resumed message on-finished on-stopped))
                         (kill-buffer resumed)
                         (funcall cannot nil)))))
        (agent-shell-subscribe-to
         :shell-buffer resumed :event 'error
         :on-event (lambda (event)
                     (unless settled
                       (setq settled t)
                       (kill-buffer resumed)
                       (funcall cannot (map-elt (map-elt event :data) :message)))))
        resumed)))))

(provide 'trf-session)
;;; trf-session.el ends here
