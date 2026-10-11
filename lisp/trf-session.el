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

;; UW-1 §Main success scenario: Turn record changes into code
(defun trf-session--without-closing-fences (lines)
  "Return LINES without the code fence closings that end them."
  (let ((lines (reverse lines)))
    (while (and lines (string-match-p "\\`\\(?:`\\{3,\\}\\|~\\{3,\\}\\)\\'" (car lines)))
      (setq lines (cdr lines)))
    (nreverse lines)))

;; UW-1 §Main success scenario: Turn record changes into code
(defun trf-session--finished (text)
  "Return a list holding the paths produced when TEXT ends with the JSON line saying finished.
Return nil when it does not."
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
                                                   :false-object nil
                                                   :null-object nil)))
                      ((consp object))
                      ((eq (alist-get 'finished object) t)))
            (let ((produced (alist-get 'produced object)))
              (setq found (list (when (listp produced)
                                  (seq-filter #'stringp produced))))))))
      found)))

(defun trf-session--stopped-p (stop-reason)
  (not (member stop-reason '(nil "end_turn"))))

;; ADR-2 §Decision: Talk to machines over ACP
;; docs/knowledge-base/agent-shell-agent-messages.org §agent-shell streams each chunk of an agent message as an event
;; docs/knowledge-base/agent-shell-agent-messages.org §A turn's end carries no message text
(defun trf-session-run (directory config instructions on-finished on-stopped)
  "Run a machine in DIRECTORY with the AI program CONFIG and INSTRUCTIONS.
Return its session buffer.  Call ON-FINISHED with the paths the machine
produced at the end of the first turn ending with the JSON line saying it
has finished.  Call ON-STOPPED, with the session buffer displayed, if the
session fails, or a turn ends for a reason other than \"end_turn\",
before then.  If the session's buffer is killed first, call neither."
  (let* ((default-directory directory)
         (buffer (let ((agent-shell-session-strategy 'new))
                   (agent-shell-start :config config)))
         (text "")
         (over nil)
         (finish (lambda (produced)
                   (run-at-time 0 nil on-finished produced)))
         (stop (lambda ()
                 (when (buffer-live-p buffer) (display-buffer buffer))
                 (run-at-time 0 nil on-stopped))))
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
                 (let ((finished (trf-session--finished text)))
                   (setq text "")
                   (unless over
                     (cond (finished
                            (setq over t)
                            (funcall finish (car finished)))
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
                   (if-let* ((finished (trf-session--finished text)))
                       (funcall finish (car finished))
                     (funcall stop)))))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'clean-up
     :on-event (lambda (_event) (setq over t)))
    buffer))

(provide 'trf-session)
;;; trf-session.el ends here
