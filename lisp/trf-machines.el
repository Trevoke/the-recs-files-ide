;;; trf-machines.el --- The machines the IDE runs -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'trf-group)

(defcustom trf-machines-skills
  '((brainstorming . "recs-brainstorming")
    (planning . "recs-writing-plans")
    (executing . "recs-executing-plans"))
  "The skill each machine of the implementation workflow is configured with."
  :type '(alist :key-type symbol :value-type string)
  :group 'trf)

(defconst trf-machines--work
  '((brainstorming
     . "Use the %s skill to settle with the record keeper how the record changes are to be implemented. Ask the record keeper whatever the records do not settle.")
    (planning
     . "Use the %s skill to write the implementation plan for the record changes.")
    (executing
     . "Use the %s skill to turn the record changes into code by following the implementation plan, committing as you go.")))

(defconst trf-machines--json-line "{\"finished\": true}"
  "The JSON line a machine that produces nothing for the next ends its last turn with.")

(defconst trf-machines--closing
  '((brainstorming
     . "When your work is finished, end your last turn with one JSON line alone, an object holding \"finished\": true and \"produced\": a list holding the path of each document in which you settled with the record keeper how the record changes are to be implemented, each relative to the root of your working tree.")
    (planning
     . "When your work is finished, end your last turn with one JSON line alone, an object holding \"finished\": true and \"produced\": a list holding the path of the implementation plan, relative to the root of your working tree.")
    (executing
     . "When your work is finished, end your last turn with one JSON line alone:"))
  "What each machine of the implementation workflow is told about finishing.")

;; UW-1 §Main success scenario: Turn record changes into code
;; F-2 §Produces: Accept or reject the record implementation report
(defconst trf-machines--executing-stops-before-bringing-the-work-in
  (concat "Do not use the recs-finishing-a-development-branch skill or any other skill that "
          "finishes a development branch, and do not merge, push, or remove the branch or the "
          "working tree: the record keeper accepts or rejects the record implementation report "
          "afterwards. Once every task of the implementation plan is done, run the tests.")
  "What the executing machine is told about where its work ends.")

(defconst trf-machines--reading
  '((planning . "Read what the brainstorming machine produced; it settles how the record changes are to be implemented:")
    (executing . "Follow the implementation plan the planning machine produced:"))
  "What each machine is told about the paths the machine before it produced.")

;; UW-1 §Main success scenario: Turn record changes into code
(defun trf-machines--make (slug name)
  (let ((machine (list :slug slug :name name
                       :skill (alist-get slug trf-machines-skills))))
    (append machine
            (list :work (format (alist-get slug trf-machines--work)
                                (plist-get machine :skill))
                  :reading (alist-get slug trf-machines--reading)
                  :closing (if (eq slug 'executing)
                               (concat trf-machines--executing-stops-before-bringing-the-work-in
                                       "\n\n" (alist-get slug trf-machines--closing) "\n"
                                       trf-machines--json-line)
                             (alist-get slug trf-machines--closing))))))

;; UW-1 §Main success scenario: Turn record changes into code
(defun trf-machines-implementation-workflow ()
  "Return the machines of the implementation workflow, in the order they run."
  (list (trf-machines--make 'brainstorming "brainstorming machine")
        (trf-machines--make 'planning "planning machine")
        (trf-machines--make 'executing "executing machine")))

;; F-2 §Takes: Accept or reject the record implementation report
;; UW-1 §Main success scenario: Turn record changes into code
(defun trf-machines-record-implementation-report-file ()
  "Return the record implementation report's path, relative to the worktree."
  ".trf/record-implementation-report.org")

;; UW-1 §Main success scenario: Turn record changes into code
;; UW-1 §Extensions: Turn record changes into code
(defun trf-machines-reconciliation-assessor ()
  "Return the reconciliation assessor."
  (let ((report (trf-machines-record-implementation-report-file)))
    (list :slug 'reconciliation-assessor
          :name "reconciliation assessor" :skill nil
          :work (concat
                 "For each of the record changes, find the tests that cite it, run them, "
                 "and assess whether the codebase is reconciled with it. "
                 (format "The record implementation report is %s; change no other file. " report)
                 "The report is an org file "
                 "with one heading per record change, its identifier and title, listing "
                 "each test that cites it and whether it passed; a record change that no "
                 "test cites is shown as untested. If you cannot assess at all, say so "
                 "and why under every record change.")
          :closing (concat "When you have assessed every record change, "
                           "end your last turn with one JSON line alone:\n"
                           trf-machines--json-line))))

(defun trf-machines-change-line (change)
  "Return the line naming CHANGE."
  (format "- %s (%s): %s"
          (string-join (delq nil (list (plist-get change :identifier)
                                       (plist-get change :title)))
                       " ")
          (plist-get change :status)
          (plist-get change :path)))

(defun trf-machines-instructions (machine changes &optional produced ide-instruction)
  "Return MACHINE's instructions for CHANGES, given the paths PRODUCED by the machine before.
IDE-INSTRUCTION, when given, is a paragraph of its own."
  (string-join
   (delq nil
         (list (format "You are the %s. Paths are relative to the root of your working tree."
                       (plist-get machine :name))
               (concat "The record changes:\n"
                       (mapconcat #'trf-machines-change-line changes "\n"))
               (plist-get machine :work)
               ide-instruction
               (when (and produced (plist-get machine :reading))
                 (concat (plist-get machine :reading) "\n"
                         (mapconcat (lambda (path) (concat "- " path)) produced "\n")))
               (plist-get machine :closing)))
   "\n\n"))

(provide 'trf-machines)
;;; trf-machines.el ends here
