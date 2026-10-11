;;; trf-session-test.el --- Tests for trf-session -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf-session)

(defvar trf-session-test-directory nil)
(defvar trf-session-test-handlers nil)

(defconst trf-session-test-buffer-name "*trf fake machine*")

(defconst trf-session-test-config '((:identifier . fake)))

(setup-each ()
  (setq trf-session-test-directory (file-name-as-directory (make-temp-file "trf-wt-" t)))
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

(defun trf-session-test-say (&rest chunks)
  "Have the machine stream CHUNKS of its message."
  (dolist (chunk chunks)
    (trf-session-test-fire 'agent-message-chunk (list (cons :text-chunk chunk)))))

(defun trf-session-test-finishing (finished)
  "Return an ON-FINISHED that records what it is called with in the cdr of FINISHED."
  (lambda (produced) (setcdr finished (list produced))))

(defun trf-session-test-run (on-finished on-stopped)
  (trf-session-run trf-session-test-directory trf-session-test-config "Do it."
                   on-finished on-stopped))

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

(deftest trf-session-stopped-on-error ()
  "An error before the machine says it has finished stops it, once, showing its session."
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

(defun trf-session-test-turn-ends (stop-reason)
  "Run a machine through one turn ending with STOP-REASON; return (FINISHED STOPPED DISPLAYED)."
  (setq trf-session-test-handlers nil)
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
  "A turn refused or cut off by a limit, without the JSON line, stops the machine once."
  (dolist (reason '("refusal" "max_tokens" "max_turn_requests"))
    (pcase-let ((`(,finished ,stopped ,displayed) (trf-session-test-turn-ends reason)))
      ;; UW-1 §Extensions: Turn record changes into code
      (assert-equal 1 stopped (format "a %s turn stops the machine once" reason))
      (assert-nil finished (format "a %s turn does not finish the machine" reason))
      (assert-equal (list (get-buffer trf-session-test-buffer-name)) displayed
                    (format "a %s turn displays the session buffer" reason)))))

(deftest trf-session-waits-when-turn-ends-normally ()
  "A turn that ends normally without the JSON line leaves the machine at work."
  (dolist (reason '("end_turn" nil))
    (pcase-let ((`(,finished ,stopped ,displayed) (trf-session-test-turn-ends reason)))
      ;; UW-1 §Main success scenario: Turn record changes into code
      ;; UW-1 §Extensions: Turn record changes into code
      (assert-equal 0 stopped (format "a %S turn does not stop the machine" reason))
      (assert-nil finished (format "a %S turn does not finish the machine" reason))
      (assert-nil displayed (format "a %S turn displays no buffer" reason)))))

(deftest trf-session-finished-with-what-it-produced ()
  "A turn ending with the JSON line and what was produced finishes the machine with those paths."
  (trf-session-test-with-fake-shell
   (let ((finished (list 'finished)))
     (trf-session-test-run (trf-session-test-finishing finished) #'ignore)
     (trf-session-test-say "The plan is written.\n"
                           "{\"finished\": true, \"produced\": "
                           "[\"docs/plans/a-plan.md\"]}")
     (trf-session-test-fire 'turn-complete (list (cons :stop-reason "end_turn")))
     (assert-nil (cdr finished) "ON-FINISHED runs after the event handler returns")
     (trf-session-test-settle)
     ;; UW-1 §Main success scenario: Turn record changes into code
     (assert-equal '(("docs/plans/a-plan.md")) (cdr finished)))))

(deftest trf-session-finished-without-anything-produced ()
  "A turn ending with the JSON line alone finishes the machine with nothing produced."
  (trf-session-test-with-fake-shell
   (let ((finished (list 'finished)))
     (trf-session-test-run (trf-session-test-finishing finished) #'ignore)
     (trf-session-test-say "All assessed.\n{\"finished\": true}\n")
     (trf-session-test-fire 'turn-complete (list (cons :stop-reason "end_turn")))
     (trf-session-test-settle)
     ;; UW-1 §Main success scenario: Turn record changes into code
     (assert-equal '(nil) (cdr finished)))))

(defun trf-session-test-turn-says (&rest chunks)
  "Run a machine through one normal turn streaming CHUNKS; return (FINISHED STOPPED)."
  (setq trf-session-test-handlers nil)
  (trf-session-test-with-fake-shell
   (let ((finished (list 'finished)) (stopped 0))
     (trf-session-test-run (trf-session-test-finishing finished)
                           (lambda () (setq stopped (1+ stopped))))
     (apply #'trf-session-test-say chunks)
     (trf-session-test-fire 'turn-complete (list (cons :stop-reason "end_turn")))
     (trf-session-test-settle)
     (list (cdr finished) stopped))))

(deftest trf-session-at-work-after-a-turn-without-the-json-line ()
  "A turn that ends without the JSON line, as when asking the record keeper, neither finishes nor stops the machine."
  ;; UW-1 §Main success scenario: Turn record changes into code
  (assert-equal '(nil 0) (trf-session-test-turn-says "Which of these should the widget do?\n")))

(deftest trf-session-at-work-after-malformed-json ()
  "A last line that is not valid JSON, or does not say finished, does not finish the machine."
  (dolist (line '("{\"finished\": true," "{\"finished\": false}" "{\"produced\": [\"a.md\"]}"
                  "[\"finished\", true]" "finished: true"))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-equal '(nil 0) (trf-session-test-turn-says "Done.\n" line)
                  (format "%S does not finish the machine" line))))

(deftest trf-session-at-work-when-the-json-line-is-not-last ()
  "The JSON line followed by more text does not finish the machine."
  ;; UW-1 §Main success scenario: Turn record changes into code
  (assert-equal '(nil 0) (trf-session-test-turn-says "{\"finished\": true}\nOne more question?\n")))

(deftest trf-session-json-line-of-an-earlier-turn-does-not-count ()
  "Text from an earlier turn does not count toward the JSON line of the next."
  (trf-session-test-with-fake-shell
   (let ((finished (list 'finished)))
     (trf-session-test-run (trf-session-test-finishing finished) #'ignore)
     (trf-session-test-say "{\"finished\":")
     (trf-session-test-fire 'turn-complete (list (cons :stop-reason "end_turn")))
     (trf-session-test-say " true}")
     (trf-session-test-fire 'turn-complete (list (cons :stop-reason "end_turn")))
     (trf-session-test-settle)
     ;; UW-1 §Main success scenario: Turn record changes into code
     (assert-nil (cdr finished)))))

(deftest trf-session-finished-on-error-after-the-json-line ()
  "An error once the machine has said it finished finishes it rather than stopping it."
  (trf-session-test-with-fake-shell
   (let ((finished (list 'finished)) stopped)
     (trf-session-test-run (trf-session-test-finishing finished) (lambda () (setq stopped t)))
     (trf-session-test-say "{\"finished\": true}")
     (trf-session-test-fire 'error)
     (trf-session-test-settle)
     ;; UW-1 §Main success scenario: Turn record changes into code
     (assert-equal '(nil) (cdr finished))
     ;; UW-1 §Extensions: Turn record changes into code
     (assert-nil stopped))))

(deftest trf-session-finished-whatever-the-stop-reason ()
  "A turn that ends with the JSON line finishes the machine, whatever its stop reason."
  (dolist (reason '("end_turn" "refusal" "max_tokens" "max_turn_requests"))
    (setq trf-session-test-handlers nil)
    (trf-session-test-with-fake-shell
     (let ((finished (list 'finished)) stopped)
       (trf-session-test-run (trf-session-test-finishing finished) (lambda () (setq stopped t)))
       (trf-session-test-say "{\"finished\": true}")
       (trf-session-test-fire 'turn-complete (list (cons :stop-reason reason)))
       (trf-session-test-settle)
       ;; UW-1 §Main success scenario: Turn record changes into code
       (assert-equal '(nil) (cdr finished)
                     (format "a %s turn with the JSON line finishes the machine" reason))
       ;; UW-1 §Extensions: Turn record changes into code
       (assert-nil stopped
                   (format "a %s turn with the JSON line does not stop the machine" reason))))))

(deftest trf-session-abandoned-when-buffer-killed ()
  "A session cleaned up before it finishes calls neither continuation."
  (trf-session-test-with-fake-shell
   (let (called)
     (trf-session-test-run (lambda (_) (setq called t)) (lambda () (setq called t)))
     (trf-session-test-fire 'clean-up)
     (trf-session-test-say "{\"finished\": true}")
     (trf-session-test-fire 'turn-complete)
     (trf-session-test-settle)
     ;; UW-1 §Extensions: Turn record changes into code
     (assert-nil called))))

(deftest trf-session-json-line-right-after-an-earlier-message ()
  "The JSON line streamed straight after an earlier message's text still finishes the machine."
  ;; docs/knowledge-base/agent-shell-agent-messages.org §agent-shell streams each chunk of an agent message as an event
  ;; UW-1 §Main success scenario: Turn record changes into code
  (assert-equal '((nil) 0) (trf-session-test-turn-says "All done." "{\"finished\": true}")))

(deftest trf-session-finished-with-the-json-line-fenced-as-json ()
  "The JSON line alone in a code fence opened with ```json finishes the machine."
  ;; UW-1 §Main success scenario: Turn record changes into code
  (assert-equal '((("docs/plans/a-plan.md")) 0)
                (trf-session-test-turn-says
                 "The plan is written.\n\n```json\n"
                 "{\"finished\": true, \"produced\": [\"docs/plans/a-plan.md\"]}\n```\n")))

(deftest trf-session-finished-with-the-json-line-fenced-with-tildes ()
  "The JSON line alone in a code fence of tildes finishes the machine."
  ;; UW-1 §Main success scenario: Turn record changes into code
  (assert-equal '((nil) 0)
                (trf-session-test-turn-says "Done.\n~~~\n{\"finished\": true}\n~~~~\n")))

(deftest trf-session-at-work-when-text-follows-the-fenced-json-line ()
  "The fenced JSON line followed by more text does not finish the machine."
  ;; UW-1 §Main success scenario: Turn record changes into code
  (assert-equal '(nil 0)
                (trf-session-test-turn-says "```\n{\"finished\": true}\n```\nAnything else?\n")))

(deftest trf-session-finished-with-the-json-line-in-an-unclosed-fence ()
  "The JSON line ending a turn inside a code fence never closed still finishes the machine."
  ;; UW-1 §Main success scenario: Turn record changes into code
  (assert-equal '((nil) 0) (trf-session-test-turn-says "```json\n{\"finished\": true}\n")))

(deftest trf-session-at-work-when-the-json-line-is-followed-by-an-opening-fence ()
  "A fence line with an info string after the JSON line is not a closing fence, so the machine is not finished."
  ;; UW-1 §Main success scenario: Turn record changes into code
  (assert-equal '(nil 0) (trf-session-test-turn-says "{\"finished\": true}\n```json\n")))

(deftest trf-session-finished-with-the-json-line-followed-by-a-stray-closing-fence ()
  "The JSON line followed by a closing fence that no fence opened still finishes the machine."
  ;; UW-1 §Main success scenario: Turn record changes into code
  (assert-equal '((nil) 0) (trf-session-test-turn-says "Done.\n{\"finished\": true}\n```\n")))

(provide 'trf-session-test)
;;; trf-session-test.el ends here
