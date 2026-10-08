;;; trf-session-test.el --- Tests for trf-session -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf-session)

(defvar trf-session-test-directory nil)
(defvar trf-session-test-handlers nil)

(defconst trf-session-test-buffer-name "*trf fake machine*")

(defconst trf-session-test-config '((:identifier . fake)))

(setup-each ()
  (setq trf-session-test-directory (file-name-as-directory (make-temp-file "trf-wt-" t)))
  (make-directory (expand-file-name ".trf/" trf-session-test-directory))
  (setq trf-session-test-handlers nil))
(teardown-each ()
  (delete-directory trf-session-test-directory t)
  (when (get-buffer trf-session-test-buffer-name)
    (kill-buffer trf-session-test-buffer-name)))

(defmacro trf-session-test-with-fake-shell (&rest body)
  "Run BODY with agent-shell replaced; `trf-session-test-handlers' maps event to handler."
  `(cl-letf (((symbol-function 'agent-shell-start)
              (lambda (&rest _) (get-buffer-create trf-session-test-buffer-name)))
             ((symbol-function 'agent-shell-subscribe-to)
              (lambda (&rest args)
                (push (cons (plist-get args :event) (plist-get args :on-event))
                      trf-session-test-handlers)))
             ((symbol-function 'agent-shell-insert) (lambda (&rest _) nil)))
     ,@body))

(defun trf-session-test-fire (event &optional data)
  (funcall (alist-get event trf-session-test-handlers)
           (list (cons :event event) (cons :data data))))

(defun trf-session-test-settle ()
  (accept-process-output nil 0.05))

(defun trf-session-test-write-handoff (text)
  (with-temp-file (expand-file-name ".trf/planning.org" trf-session-test-directory)
    (insert text)))

(defun trf-session-test-run (on-finished on-stopped)
  (trf-session-run trf-session-test-machine trf-session-test-directory
                   trf-session-test-config "Do it." on-finished on-stopped))

(defconst trf-session-test-machine '(:slug planning :name "planning machine"))

(deftest trf-session-config-is-the-customized-one ()
  "The customized AI program config is used without asking."
  (let ((trf-ai-program-config trf-session-test-config))
    (with-spy agent-shell-select-config calls
      (assert-equal trf-session-test-config (trf-session-config))
      (assert-equal 0 (spy-call-count calls)))))

(deftest trf-session-config-asks-agent-shell-when-unset ()
  "With no customized config, the record keeper picks one through agent-shell."
  (let ((trf-ai-program-config nil))
    (with-spy agent-shell-select-config calls '((:identifier . picked))
      (assert-equal '((:identifier . picked)) (trf-session-config))
      (assert-equal 1 (spy-call-count calls)))))

(deftest trf-session-starts-a-new-session-with-the-config ()
  "Each machine runs in a new session of the given AI program."
  (let (strategy config)
    (trf-session-test-with-fake-shell
     (cl-letf (((symbol-function 'agent-shell-start)
                (lambda (&rest args)
                  (setq strategy agent-shell-session-strategy
                        config (plist-get args :config))
                  (get-buffer-create trf-session-test-buffer-name))))
       (trf-session-test-run #'ignore #'ignore)))
    (assert-equal 'new strategy)
    (assert-equal trf-session-test-config config)))

(deftest trf-session-submits-instructions-once-initialized ()
  "The instructions are submitted to the session once it is initialized."
  (trf-session-test-with-fake-shell
   (with-spy agent-shell-insert calls
     (trf-session-test-run #'ignore #'ignore)
     (trf-session-test-fire 'init-finished)
     ;; UW-1 §Main success scenario: Turn record changes into code
     (assert-equal 1 (spy-call-count calls))
     (assert-equal "Do it." (plist-get (spy-last-call calls) :text)))))

(deftest trf-session-finished-when-handoff-exists ()
  "A turn that ends with the handoff file written finishes the machine."
  (trf-session-test-with-fake-shell
   (let (finished)
     (trf-session-test-run (lambda (text) (setq finished text)) #'ignore)
     (trf-session-test-fire 'turn-complete)
     (trf-session-test-settle)
     ;; UW-1 §Main success scenario: Turn record changes into code
     (assert-nil finished)
     (trf-session-test-write-handoff "Plan written.")
     (trf-session-test-fire 'turn-complete)
     (assert-nil finished "ON-FINISHED runs after the event handler returns")
     (trf-session-test-settle)
     ;; UW-1 §Main success scenario: Turn record changes into code
     (assert-equal "Plan written." finished))))

(deftest trf-session-ignores-a-handoff-from-an-earlier-run ()
  "A handoff file left by an earlier run does not finish the machine."
  (trf-session-test-write-handoff "Stale plan.")
  (trf-session-test-with-fake-shell
   (let (finished)
     (trf-session-test-run (lambda (text) (setq finished text)) #'ignore)
     (trf-session-test-fire 'turn-complete)
     (trf-session-test-settle)
     ;; UW-1 §Extensions: Turn record changes into code
     (assert-nil finished))))

(deftest trf-session-stopped-on-error ()
  "An error before the handoff file stops the machine, once, showing its session."
  (trf-session-test-with-fake-shell
   (with-spy display-buffer calls
     (let ((stopped 0))
       (trf-session-test-run #'ignore (lambda () (setq stopped (1+ stopped))))
       (trf-session-test-fire 'error)
       (trf-session-test-fire 'error)
       (assert-equal 0 stopped "ON-STOPPED runs after the event handler returns")
       (trf-session-test-settle)
       ;; UW-1 §Extensions: Turn record changes into code
       (assert-equal 1 stopped)
       (assert-equal 1 (spy-call-count calls))
       (assert-equal (get-buffer trf-session-test-buffer-name)
                     (car (spy-last-call calls)))))))

(deftest trf-session-finished-on-error-after-handoff ()
  "An error once the handoff file exists finishes the machine rather than stopping it."
  (trf-session-test-with-fake-shell
   (let (finished stopped)
     (trf-session-test-run (lambda (text) (setq finished text))
                           (lambda () (setq stopped t)))
     (trf-session-test-write-handoff "Plan written.")
     (trf-session-test-fire 'error)
     (trf-session-test-settle)
     ;; UW-1 §Main success scenario: Turn record changes into code
     (assert-equal "Plan written." finished)
     ;; UW-1 §Extensions: Turn record changes into code
     (assert-nil stopped))))

(defun trf-session-test-turn-ends (stop-reason)
  "Run a machine through one turn ending with STOP-REASON; return (FINISHED STOPPED DISPLAYED)."
  (trf-session-test-with-fake-shell
   (with-spy display-buffer calls
     (let (finished (stopped 0))
       (trf-session-test-run (lambda (text) (setq finished text))
                             (lambda () (setq stopped (1+ stopped))))
       (trf-session-test-fire 'turn-complete (list (cons :stop-reason stop-reason)))
       (trf-session-test-fire 'turn-complete (list (cons :stop-reason stop-reason)))
       (trf-session-test-settle)
       (list finished stopped (mapcar #'car calls))))))

(deftest trf-session-stopped-when-turn-ends-short ()
  "A turn refused or cut off by a limit, without the handoff file, stops the machine once."
  (dolist (reason '("refusal" "max_tokens" "max_turn_requests"))
    (pcase-let ((`(,finished ,stopped ,displayed) (trf-session-test-turn-ends reason)))
      ;; UW-1 §Extensions: Turn record changes into code
      (assert-equal 1 stopped reason)
      (assert-nil finished reason)
      (assert-equal (list (get-buffer trf-session-test-buffer-name)) displayed reason))))

(deftest trf-session-waits-when-turn-ends-normally ()
  "A turn that ends normally without the handoff file leaves the machine waiting."
  (pcase-let ((`(,finished ,stopped ,displayed) (trf-session-test-turn-ends "end_turn")))
    ;; UW-1 §Main success scenario: Turn record changes into code
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-equal 0 stopped)
    (assert-nil finished)
    (assert-nil displayed)))

(deftest trf-session-finished-whatever-the-stop-reason ()
  "A turn that ends with the handoff file finishes the machine, whatever its stop reason."
  (dolist (reason '("end_turn" "refusal" "max_tokens" "max_turn_requests"))
    (setq trf-session-test-handlers nil)
    (trf-session-test-with-fake-shell
     (let (finished stopped)
       (trf-session-test-run (lambda (text) (setq finished text))
                             (lambda () (setq stopped t)))
       (trf-session-test-write-handoff "Plan written.")
       (trf-session-test-fire 'turn-complete (list (cons :stop-reason reason)))
       (trf-session-test-settle)
       ;; UW-1 §Main success scenario: Turn record changes into code
       (assert-equal "Plan written." finished reason)
       ;; UW-1 §Extensions: Turn record changes into code
       (assert-nil stopped reason)))))

(deftest trf-session-abandoned-when-buffer-killed ()
  "A session cleaned up before the handoff calls neither continuation."
  (trf-session-test-with-fake-shell
   (let (called)
     (trf-session-test-run (lambda (_) (setq called t)) (lambda () (setq called t)))
     (trf-session-test-fire 'clean-up)
     (trf-session-test-write-handoff "Plan written.")
     (trf-session-test-fire 'turn-complete)
     (trf-session-test-settle)
     ;; UW-1 §Extensions: Turn record changes into code
     (assert-nil called))))

(provide 'trf-session-test)
;;; trf-session-test.el ends here
