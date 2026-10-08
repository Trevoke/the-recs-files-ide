;;; trf-machines-test.el --- Tests for trf-machines -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf-machines)

(defconst trf-machines-test-changes
  '((:path "docs/user-workflows/uw-1-order-a-widget.org" :status modified
     :identifier "UW-1" :title "Order a widget")
    (:path "docs/vision.org" :status modified :identifier nil :title "Vision")))

(deftest trf-machines-implementation-workflow-order ()
  "The implementation workflow is the brainstorming, planning and executing machines, in turn."
  ;; UW-1 §Main success scenario: Turn record changes into code
  (assert-equal '("brainstorming machine" "planning machine" "executing machine")
                (mapcar (lambda (machine) (plist-get machine :name))
                        (trf-machines-implementation-workflow))))

(deftest trf-machines-handoff-files ()
  "Each machine's handoff file sits under .trf/; the assessor's is the report."
  (assert-equal ".trf/brainstorming.org"
                (trf-machines-handoff-file (car (trf-machines-implementation-workflow))))
  (assert-equal ".trf/record-implementation-report.org"
                (trf-machines-handoff-file (trf-machines-reconciliation-assessor))))

(deftest trf-machines-instructions-name-everything ()
  "Instructions name the machine, each record change, its skill, the previous handoff and its own."
  (let ((text (trf-machines-instructions (nth 1 (trf-machines-implementation-workflow))
                                         trf-machines-test-changes
                                         "Design written to docs/plans/x-design.md")))
    (assert-match "You are the planning machine\\." text)
    (assert-match "- UW-1 Order a widget (modified): docs/user-workflows/uw-1-order-a-widget.org" text)
    (assert-match "recs-writing-plans" text)
    (assert-match "Design written to docs/plans/x-design.md" text)
    (assert-match "write \\.trf/planning\\.org" text)))

(deftest trf-machines-instructions-name-a-change-without-identifier-by-title ()
  "A record change without an identifier, such as the vision, is named by its title alone."
  (let ((text (trf-machines-instructions (car (trf-machines-implementation-workflow))
                                         trf-machines-test-changes)))
    (assert-match "^- Vision (modified): docs/vision\\.org$" text)))

(deftest trf-machines-reconciliation-assessor-covers-untested-and-cannot-assess ()
  "The reconciliation assessor is told to show untested record changes and why it cannot assess."
  (let ((text (trf-machines-instructions (trf-machines-reconciliation-assessor)
                                         trf-machines-test-changes)))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-match "For each of the record changes, find the tests that cite it, run them, and assess whether the codebase is reconciled with it\\." text)
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-match "a record change that no test cites is shown as untested\\." text)
    ;; UW-1 §Extensions: Turn record changes into code
    (assert-match "If you cannot assess at all, say so and why under every record change\\." text)))

(deftest trf-machines-reconciliation-assessor-writes-only-the-report ()
  "The reconciliation assessor's handoff file is the record implementation report and nothing else."
  (let ((text (trf-machines-instructions (trf-machines-reconciliation-assessor)
                                         trf-machines-test-changes)))
    (assert-match "The record implementation report is \\.trf/record-implementation-report\\.org; change no other file\\." text)
    (assert-match "write \\.trf/record-implementation-report\\.org as your last act\\." text)
    (assert-nil (string-match-p "naming what you produced" text))))

(deftest trf-machines-skills-are-customizable ()
  "A machine uses the skill the record keeper configured, taken literally."
  (let* ((trf-machines-skills '((brainstorming . "my-brainstorming")
                                (planning . "my-%s-plans")
                                (executing . "my-executing")))
         (text (trf-machines-instructions (nth 1 (trf-machines-implementation-workflow))
                                          trf-machines-test-changes)))
    (assert-match "Use the my-%s-plans skill" text)))

(deftest trf-machines-instructions-say-paths-are-relative-to-the-working-tree ()
  "Every machine is told once that paths are relative to the root of its working tree."
  (dolist (machine (cons (trf-machines-reconciliation-assessor)
                         (trf-machines-implementation-workflow)))
    (let ((text (trf-machines-instructions machine trf-machines-test-changes)))
      (assert-equal 2 (length (split-string
                               text "Paths are relative to the root of your working tree\\."))))))

(deftest trf-machines-instructions-carry-an-instruction-from-the-ide ()
  "An instruction from the IDE is a paragraph of its own, not something a machine handed off."
  (let ((text (trf-machines-instructions (car (trf-machines-implementation-workflow))
                                         trf-machines-test-changes nil "Bring it up to date.")))
    (assert-match "\n\nBring it up to date\\.\n\n" text)
    (assert-nil (string-match-p "handed off" text))))

(deftest trf-machines-instructions-carry-both-a-handoff-and-an-instruction ()
  "A handoff and an instruction from the IDE each keep their own paragraph."
  (let ((text (trf-machines-instructions (car (trf-machines-implementation-workflow))
                                         trf-machines-test-changes "Done." "Bring it up to date.")))
    (assert-match "\n\nBring it up to date\\.\n\n" text)
    (assert-match "The machine before you handed off:\nDone\\." text)))

(provide 'trf-machines-test)
;;; trf-machines-test.el ends here
