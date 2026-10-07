;;; trf-session-test.el --- Tests for trf-session -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf-session)

(defvar trf-session-test-directory nil)
(defvar trf-session-test-handlers nil)

(setup-each ()
  (setq trf-session-test-directory (file-name-as-directory (make-temp-file "trf-wt-" t)))
  (make-directory (expand-file-name ".trf/" trf-session-test-directory))
  (setq trf-session-test-handlers nil))
(teardown-each () (delete-directory trf-session-test-directory t))

(defmacro trf-session-test-with-fake-shell (&rest body)
  "Run BODY with agent-shell replaced; `trf-session-test-handlers' maps event to handler."
  `(let ((trf-ai-program-config '((:identifier . fake))))
     (cl-letf (((symbol-function 'agent-shell-start)
                (lambda (&rest _) (get-buffer-create "*trf fake machine*")))
               ((symbol-function 'agent-shell-subscribe-to)
                (lambda (&rest args)
                  (push (cons (plist-get args :event) (plist-get args :on-event))
                        trf-session-test-handlers)))
               ((symbol-function 'agent-shell-insert) (lambda (&rest _) nil)))
       ,@body)))

(defun trf-session-test-fire (event &optional data)
  (funcall (alist-get event trf-session-test-handlers)
           (list (cons :event event) (cons :data data))))

(defconst trf-session-test-machine '(:slug planning :name "planning machine"))

(deftest trf-session-submits-instructions-once-initialized ()
  "The instructions are submitted to the session once it is initialized."
  (trf-session-test-with-fake-shell
   (with-spy agent-shell-insert calls
     (trf-session-run trf-session-test-machine trf-session-test-directory "Do it."
                      #'ignore #'ignore)
     (trf-session-test-fire 'init-finished)
     ;; UW-1 §Main success scenario: Turn record changes into code
     (assert-equal 1 (spy-call-count calls))
     (assert-equal "Do it." (plist-get (spy-last-call calls) :text)))))

(deftest trf-session-finished-when-handoff-exists ()
  "A turn that ends with the handoff file written finishes the machine."
  (trf-session-test-with-fake-shell
   (let (finished)
     (trf-session-run trf-session-test-machine trf-session-test-directory "Do it."
                      (lambda (text) (setq finished text)) #'ignore)
     (trf-session-test-fire 'turn-complete)
     ;; UW-1 §Main success scenario: Turn record changes into code
     (assert-nil finished)
     (with-temp-file (expand-file-name ".trf/planning.org" trf-session-test-directory)
       (insert "Plan written."))
     (trf-session-test-fire 'turn-complete)
     ;; UW-1 §Main success scenario: Turn record changes into code
     (assert-equal "Plan written." finished))))

(deftest trf-session-stopped-on-error ()
  "An error before the handoff file stops the machine, once."
  (trf-session-test-with-fake-shell
   (let ((stopped 0))
     (trf-session-run trf-session-test-machine trf-session-test-directory "Do it."
                      #'ignore (lambda () (setq stopped (1+ stopped))))
     (trf-session-test-fire 'error)
     (trf-session-test-fire 'error)
     ;; UW-1 §Extensions: Turn record changes into code
     (assert-equal 1 stopped))))

(deftest trf-session-abandoned-when-buffer-killed ()
  "A session cleaned up before the handoff calls neither continuation."
  (trf-session-test-with-fake-shell
   (let (called)
     (trf-session-run trf-session-test-machine trf-session-test-directory "Do it."
                      (lambda (_) (setq called t)) (lambda () (setq called t)))
     (trf-session-test-fire 'clean-up)
     (trf-session-test-fire 'turn-complete)
     ;; UW-1 §Extensions: Turn record changes into code
     (assert-nil called))))

(provide 'trf-session-test)
;;; trf-session-test.el ends here
