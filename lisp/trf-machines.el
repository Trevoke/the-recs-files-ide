;;; trf-machines.el --- The machines the IDE runs -*- lexical-binding: t; -*-

(require 'subr-x)

(defgroup trf nil "The IDE for projects that keep records." :group 'tools)

(defcustom trf-machines-skills
  '((brainstorming . "recs-brainstorming")
    (planning . "recs-writing-plans")
    (executing . "recs-executing-plans"))
  "The skill each machine of the implementation workflow is configured with."
  :type '(alist :key-type symbol :value-type string))

(defconst trf-machines--work
  '((brainstorming
     . "Use the %s skill to settle with the record keeper how the record changes are to be implemented. Ask the record keeper whatever the records do not settle.")
    (planning
     . "Use the %s skill to write the implementation plan for the record changes.")
    (executing
     . "Use the %s skill to turn the record changes into code by following the implementation plan, committing as you go.")))

(defun trf-machines--make (slug name)
  (list :slug slug :name name
        :skill (alist-get slug trf-machines-skills)
        :work (format (alist-get slug trf-machines--work)
                      (alist-get slug trf-machines-skills))))

;; UW-1 §Main success scenario: Turn record changes into code
(defun trf-machines-implementation-workflow ()
  "Return the machines of the implementation workflow, in the order they run."
  (list (trf-machines--make 'brainstorming "brainstorming machine")
        (trf-machines--make 'planning "planning machine")
        (trf-machines--make 'executing "executing machine")))

;; UW-1 §Extensions: Turn record changes into code
(defun trf-machines-reconciliation-assessor ()
  "Return the reconciliation assessor."
  (list :slug 'record-implementation-report :name "reconciliation assessor" :skill nil
        :work (concat
               "For each of the record changes, find the tests that cite it, run them, "
               "and assess whether the codebase is reconciled with it. Change no file "
               "other than the record implementation report. The report is an org file "
               "with one heading per record change, its identifier and title, listing "
               "each test that cites it and whether it passed; a record change that no "
               "test cites is shown as untested. If you cannot assess at all, say so "
               "and why under every record change.")))

(defun trf-machines-handoff-file (machine)
  "Return MACHINE's handoff file, relative to its worktree."
  (format ".trf/%s.org" (plist-get machine :slug)))

(defun trf-machines--change-line (change)
  (format "- %s (%s): %s"
          (string-join (delq nil (list (plist-get change :identifier)
                                       (plist-get change :title)))
                       " ")
          (plist-get change :status)
          (plist-get change :path)))

(defun trf-machines-instructions (machine changes &optional previous-handoff)
  "Return MACHINE's instructions for CHANGES, given the PREVIOUS-HANDOFF text."
  (string-join
   (delq nil
         (list (format "You are the %s." (plist-get machine :name))
               (concat "The record changes:\n"
                       (mapconcat #'trf-machines--change-line changes "\n"))
               (plist-get machine :work)
               (when previous-handoff
                 (concat "The machine before you handed off:\n" previous-handoff))
               (format "When your work is finished, write %s as your last act, naming what you produced."
                       (trf-machines-handoff-file machine))))
   "\n\n"))

(provide 'trf-machines)
;;; trf-machines.el ends here
