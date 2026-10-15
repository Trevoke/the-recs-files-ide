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

(deftest trf-machines-record-implementation-report-file ()
  "The record implementation report sits at a fixed path under .trf/."
  ;; ADR-7 §Decision: Run the implementation workflow in a git worktree of the target product
  (assert-equal ".trf/record-implementation-report.org"
                (trf-machines-record-implementation-report-file)))

(deftest trf-machines-instructions-name-everything ()
  "Instructions name the machine, each record change, its skill, what the machine before produced, and how to finish."
  (let ((text (trf-machines-instructions (nth 1 (trf-machines-implementation-workflow))
                                         trf-machines-test-changes
                                         '("docs/plans/x-design.md"))))
    (assert-match "You are the planning machine\\." text)
    (assert-match "- UW-1 Order a widget (modified): docs/user-workflows/uw-1-order-a-widget.org" text)
    (assert-match "recs-writing-plans" text)
    ;; ADR-3 §Consequences: Machines end their work with one JSON line
    (assert-match "Read what the brainstorming machine produced[^\n]*:\n- docs/plans/x-design\\.md" text)
    ;; ADR-3 §Decision: Machines end their work with one JSON line
    (assert-match "end your last turn with one JSON line alone, an object holding \"finished\": true and \"produced\": a list holding the path of the implementation plan, relative to the root of your working tree\\.$" text)
    (assert-nil (string-match-p "<path>" text))))

(deftest trf-machines-executing-machine-follows-the-implementation-plan-produced ()
  "The executing machine is told to follow the implementation plan the planning machine produced."
  (let ((text (trf-machines-instructions (nth 2 (trf-machines-implementation-workflow))
                                         trf-machines-test-changes
                                         '("docs/plans/x-plan.md"))))
    ;; ADR-3 §Consequences: Machines end their work with one JSON line
    (assert-match "Follow the implementation plan the planning machine produced:\n- docs/plans/x-plan\\.md" text)
    ;; ADR-3 §Decision: Machines end their work with one JSON line
    (assert-match "\n{\"finished\": true}$" text)))

(deftest trf-machines-brainstorming-machine-says-what-it-produced ()
  "The brainstorming machine is told to list in produced where it settled how to implement the record changes."
  (let ((text (trf-machines-instructions (car (trf-machines-implementation-workflow))
                                         trf-machines-test-changes)))
    ;; ADR-3 §Decision: Machines end their work with one JSON line
    (assert-match "end your last turn with one JSON line alone, an object holding \"finished\": true and \"produced\": a list holding the path of each document in which you settled with the record keeper how the record changes are to be implemented, each relative to the root of your working tree\\.$" text)
    (assert-nil (string-match-p "<path>" text))
    (assert-nil (string-match-p "produced:\n" text))))

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
    ;; UW-1 §Extensions 5a: Turn record changes into code
    (assert-match "a record change that no test cites is shown as untested\\." text)
    ;; UW-1 §Extensions 5b: Turn record changes into code
    (assert-match "If you cannot assess at all, say so and why under every record change\\." text)))

(deftest trf-machines-reconciliation-assessor-writes-only-the-report ()
  "The reconciliation assessor writes the record implementation report and nothing else, then says it has finished."
  (let ((text (trf-machines-instructions (trf-machines-reconciliation-assessor)
                                         trf-machines-test-changes)))
    (assert-match "The record implementation report is \\.trf/record-implementation-report\\.org; change no other file\\." text)
    ;; UW-1 §Extensions 5b: Turn record changes into code
    ;; ADR-3 §Decision: Machines end their work with one JSON line
    (assert-match (concat (regexp-quote "\n\nWhen you have written the record implementation report, including when you could not assess, end your last turn with one JSON line alone:\n{\"finished\": true}")
                          "\\'")
                  text)
    (assert-nil (string-match-p "When you have assessed every record change" text))
    (assert-nil (string-match-p "produced" text))))

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
  "An instruction from the IDE is a paragraph of its own."
  (let ((text (trf-machines-instructions (car (trf-machines-implementation-workflow))
                                         trf-machines-test-changes nil "Bring it up to date.")))
    (assert-match "\n\nBring it up to date\\.\n\n" text)))

(deftest trf-machines-instructions-carry-both-what-was-produced-and-an-instruction ()
  "What the machine before produced and an instruction from the IDE each keep their own paragraph."
  (let ((text (trf-machines-instructions (nth 1 (trf-machines-implementation-workflow))
                                         trf-machines-test-changes '("a.md") "Bring it up to date.")))
    (assert-match "\n\nBring it up to date\\.\n\n" text)
    (assert-match "produced[^\n]*:\n- a\\.md\n\n" text)))

(deftest trf-machines-executing-machine-stops-before-bringing-the-work-in ()
  "The executing machine is told not to merge, push, or remove the branch or the working tree."
  (let ((text (trf-machines-instructions (nth 2 (trf-machines-implementation-workflow))
                                         trf-machines-test-changes '("docs/plans/x-plan.md"))))
    ;; UW-1 §Main success scenario: Turn record changes into code
    ;; UW-1 §Minimal guarantee: Turn record changes into code
    ;; ADR-7 §Consequences: Run the implementation workflow in a git worktree of the target product
    (assert-match "Do not use any skill that finishes a development branch, and do not merge, push, or remove the branch or the working tree: the record keeper accepts or rejects the record implementation report afterwards\\." text)
    (assert-nil (string-match-p "recs-finishing-a-development-branch" text))
    ;; ADR-3 §Decision: Machines end their work with one JSON line
    (assert-match "afterwards\\.\n\nWhen your work is finished, end your last turn with one JSON line alone:\n{\"finished\": true}$" text)
    (assert-nil (string-match-p "run the tests" text))))

(deftest trf-machines-only-the-executing-machine-is-told-to-stop-before-bringing-the-work-in ()
  "Only the executing machine is told not to merge, push, or remove the branch or the working tree."
  (dolist (machine (list (car (trf-machines-implementation-workflow))
                         (nth 1 (trf-machines-implementation-workflow))
                         (trf-machines-reconciliation-assessor)))
    (assert-nil (string-match-p "finishes a development branch"
                                (trf-machines-instructions machine trf-machines-test-changes))
                (plist-get machine :name))))

(deftest trf-machines-ask-the-record-keeper-only-through-the-device-for-questions ()
  "Every machine is told to ask only through its AI program's device for questions, and how to stop before finishing."
  (dolist (machine (append (trf-machines-implementation-workflow)
                           (list (trf-machines-reconciliation-assessor))))
    (let ((text (trf-machines-instructions machine trf-machines-test-changes '("a.md"))))
      ;; ADR-4 §Decision: Machines ask the record keeper through ACP elicitation
      ;; ADR-4 §Consequences: Machines ask the record keeper through ACP elicitation
      (assert-match (regexp-quote "\n\nAsk the record keeper questions only through the device your AI program offers for asking questions, not in your message text. If you have a question and your AI program offers no such device, stop, with your question as the reason.\n\nIf you stop before finishing")
                    text
                    (plist-get machine :name))
      ;; ADR-3 §Decision: Machines end their work with one JSON line
      (assert-match (regexp-quote "\n\nIf you stop before finishing, for any reason, end your last turn with one JSON line alone, an object holding \"finished\": false and \"reason\": why you stopped.\n\n")
                    text
                    (plist-get machine :name)))))

(deftest trf-machines-brainstorming-machine-asks-the-record-keeper-what-the-records-do-not-settle ()
  "The brainstorming machine is told to ask the record keeper whatever the records do not settle."
  (let ((text (trf-machines-instructions (car (trf-machines-implementation-workflow))
                                         trf-machines-test-changes)))
    ;; UW-1 §Main success scenario: Turn record changes into code
    (assert-match "Use the recs-brainstorming skill to settle with the record keeper how the record changes are to be implemented\\. Ask the record keeper whatever the records do not settle\\.\n\n" text)
    (assert-nil (string-match-p "settle it on your own" text))))

(provide 'trf-machines-test)
;;; trf-machines-test.el ends here
