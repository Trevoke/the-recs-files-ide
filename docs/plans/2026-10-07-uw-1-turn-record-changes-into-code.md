# UW-1 Turn Record Changes Into Code Implementation Plan

> **For Claude:** REQUIRED SUB-SKILL: Use recs-executing-plans to implement this plan task-by-task.

**Goal:** Build the IDE's first slice: the record keeper asks the IDE to
implement the record changes, answers the machines of the implementation
workflow, and accepts or rejects the record implementation report.

**Architecture:** Elisp under `lisp/`, prefix `trf`, one file per job: git
access, the target product's projections, the record changes, worktrees,
machine definitions, machine sessions over ACP (through agent-shell), and the
UW-1 commands. State lives in git and in files under the worktree's `.trf/`
directory. The IDE's own Emacs configuration is `ide/`, launched by `bin/trf`.

**Tech Stack:** Emacs 30, agent-shell v0.86.2 / acp.el v0.15.1 /
shell-maker v0.97.5, git; tests in e-unit run by elk, mock-fs for file-system
tests.

---

## How to Speak

The ubiquitous language is in `docs/ubiquitous-language.org`. This plan's work
touches these defined terms:

- **record** — one decision important to the delivery or maintenance of the
  target product, under a unique identifier, in words the record keeper has
  agreed to.
- **projection** — the records of one form, such as the user workflows or the
  message catalog.
- **record keeper** — the person who creates, edits and maintains the records.
- **record changes** — the edits to records not yet implemented.
- **target product** — the product the records are about.
- **implementation plan** — the steps for turning record changes into code that
  the planning machine writes and the executing machine follows.
- **record implementation report** — for each of the record changes, the tests
  that cite it and their results.
- **reconciled** — the codebase is reconciled with a record when the truth of
  the record is reflected in the codebase.
- **implemented** — said of record changes once the record keeper has accepted
  their record implementation report into the codebase.
- **codebase** — the target product's code that the record keeper has accepted.
- **machine** — an AI program the IDE has put to one kind of work, through the
  instructions it gives and the devices it configures it with.
- **device** — a tool a machine uses, such as the shell, the file system, the
  test runner or a skill.
- **implementation workflow** — the brainstorming machine, the planning machine
  and the executing machine, run one after another on the record changes.
- **brainstorming machine**, **planning machine**, **executing machine**,
  **reconciliation assessor** — as defined there.

Use them exactly, in code, tests, comments, and commit messages. Coin nothing.
The word "agent" is retired from this project's language: use "machine". It
survives only inside names this project does not own (`agent-shell`, the
Agent Client Protocol, `AGENTS.md`).

Words this plan uses that are not terms and must not become identifiers beyond
what is written here: "handoff file" (the file a machine writes as its last
act; code names it `trf-machines-handoff-file`), "AI program".

## Do Not Modify

- `docs/vision.org`
- `docs/ubiquitous-language.org`
- `docs/user-workflows.org`, `docs/user-workflows/`
- `docs/features.org`, `docs/features/`
- `docs/message-catalog.org`, `docs/message-catalog/`
- `docs/architectural-decisions.org`, `docs/architectural-decisions/`
- `docs/deferred.org`
- `docs/knowledge-base/`
- `AGENTS.md`

If any task seems to require changing one of these, STOP and raise it with the
user. The plan does not proceed around it.

## Reading List

Read before executing any task, in this order:

1. UW-1: Turn record changes into code — the workflow this plan implements
2. F-1: Ask the IDE to implement the record changes — the surface being built
3. MSG-1: No record changes to implement — the exact text Task 8 asserts
4. ADR-2: Talk to machines over ACP — why every machine runs through agent-shell
5. ADR-1: Ship the IDE as a bare Emacs with its own pinned binary — why Task 10 pins packages
6. `docs/plans/2026-10-07-uw-1-design.md` — implementation-level decisions
   (worktrees, the `refs/recs/implemented` ref, state in files)
7. `AGENTS.md` — the comment rule (pointers only) and how tests run

## Cataloged-Text Ordering

Task 8 touches cataloged text (MSG-1: No record changes to implement).

1. The catalog record's wording is settled: it is, as of commit `4579005`.
2. The failing test asserts the record's exact current text, character for
   character — open `docs/message-catalog/msg-1-no-record-changes-to-implement.org`
   and copy the `Text:` line's text; do not copy it from this plan.
3. Only then is the implementation written to emit it.

No other task emits refusal or failure text. A machine that stops shows its own
session buffer (UW-1 3a); the IDE writes no text of its own about it. If a task
seems to need IDE-authored failure text, STOP and raise it: it needs a
message-catalog record first.

## Conventions for every task

- Work in the worktree `/home/stag/src/projects/recs-files-ui/.worktrees/uw-1`.
- Run tests with `$ELK test` from the worktree root, where
  `ELK=/home/stag/src/projects/recs-files-ui/vendor/elk/elk` (elk ≥ v0.1.2,
  which installs agent-shell's dependencies; if `vendor/elk` is older, STOP).
  A single file's tests: `$ELK test --pattern=<regexp>`.
- Every `.el` file starts with `;;; <file> --- <summary> -*- lexical-binding: t; -*-`
  and ends with `(provide '<feature>)` and `;;; <file> ends here`.
- Comments are record pointers and nothing else (AGENTS.md). The snippets below
  carry none; where a task says "cite", add one pointer line of the form
  `;; F-1 §Rules: Ask the IDE to implement the record changes` above the named
  form, naming the section that carries the claim.
- Commit after each task with an imperative subject (~50 chars), a body naming
  the record served, and the attribution lines:

  ```
  Co-Authored-By: Claude Opus 5.5 <noreply@anthropic.com>
  Claude-Session: https://claude.ai/code/session_01AsWSffavjfUiTGqka4HqhF
  ```

---

### Task 0: Scaffold the package and its test run

**Records:** ADR-1: Ship the IDE as a bare Emacs with its own pinned binary

**Files:**
- Create: `Elkfile`
- Create: `lisp/trf.el`
- Create: `test/trf-test.el`
- Modify: `.gitignore`

**Step 1: Write the Elkfile**

```elisp
;;; Elkfile --- elk configuration for trf -*- lexical-binding: t; -*-

(elk-project
 :name "trf"
 :version "0.1.0"
 :source-dirs '("lisp/")
 :test-dirs '("test/"))

(elk-add-to-load-path "lisp/")
(elk-add-to-load-path "test/")

(elk-test-deps
 '((e-unit :git "https://codeberg.org/Trevoke/e-unit.el" :branch "congruence")
   (mock-fs :git "https://codeberg.org/Trevoke/mock-fs.el" :branch "congruence")
   (shell-maker :git "https://github.com/xenodium/shell-maker" :tag "v0.97.5")
   (acp :git "https://github.com/xenodium/acp.el" :tag "v0.15.1")
   (agent-shell :git "https://github.com/xenodium/agent-shell" :tag "v0.86.2")))

(elk-set 'clean-patterns '("lisp/*.elc" "test/*.elc"))

(elk-task test
  "Run trf's e-unit suite"
  :scope test
  :args (pattern)
  :action (lambda (&rest args)
            (require 'e-unit)
            (e-unit-initialize)
            (let* ((test-dir (expand-file-name "test/" (elk--project-root)))
                   (pattern (plist-get args :pattern))
                   (results (if pattern
                                (e-unit-run-pattern pattern test-dir)
                              (e-unit-run-directory test-dir))))
              (when-let* ((failed (cl-remove-if-not
                                   (lambda (r) (memq (plist-get r :status) '(fail error)))
                                   results)))
                (error "elk: %d tests failed" (length failed))))))

;;; Elkfile ends here
```

Check mock-fs's default branch before relying on `"congruence"`:
`git ls-remote --symref https://codeberg.org/Trevoke/mock-fs.el HEAD`; use the
branch it names.

**Step 2: Write the failing smoke test** — `test/trf-test.el`:

```elisp
;;; trf-test.el --- Tests for trf -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf)

(deftest trf-test-version-is-set ()
  "trf declares its version."
  (assert-equal "0.1.0" trf-version))

(provide 'trf-test)
;;; trf-test.el ends here
```

**Step 3: Run it to see it fail**

Run: `$ELK test`
Expected: FAIL — `Cannot open load file ... trf` (first run also bootstraps
elpaca and installs the five dependencies into `.elk/deps/`).

**Step 4: Write `lisp/trf.el`**

```elisp
;;; trf.el --- The IDE for projects that keep records -*- lexical-binding: t; -*-

(defconst trf-version "0.1.0")

(provide 'trf)
;;; trf.el ends here
```

**Step 5: Run the tests** — `$ELK test` → `1 tests: 1 passed`.

**Step 6:** Add `.elk/` and `*.elc` to `.gitignore` if not present.

**Step 7: Commit** — `git add Elkfile lisp test .gitignore`, subject
"Scaffold the trf package and its test run".

---

### Task 1: Git access

**Records:** ADR-1: Ship the IDE as a bare Emacs with its own pinned binary
(git is one of the things ADR-1's context says the IDE runs)

**Files:**
- Create: `lisp/trf-git.el`
- Create: `test/trf-test-helper.el`
- Create: `test/trf-git-test.el`

**Step 1: Write the test helper** — `test/trf-test-helper.el`. It builds a
throwaway target product with its own git, independent of the code under test.

```elisp
;;; trf-test-helper.el --- Fixtures for trf tests -*- lexical-binding: t; -*-

(require 'subr-x)

(defconst trf-test-agents-md
  (concat
   "- **Vision** — `docs/vision.org`, a single file\n"
   "- **User workflows** (UW-n) — `docs/user-workflows.org`\n"
   "- **Features** (F-n) — `docs/features.org`\n"
   "\n"
   "- User workflows — `docs/knowledge-base/cockburn-use-cases.org`\n"))

(defconst trf-test-records
  '(("docs/vision.org" . "#+TITLE: Vision\n\nWhat it is for.\n")
    ("docs/user-workflows.org" . "#+TITLE: User Workflows\n\n- UW-1 :: Order a widget\n")
    ("docs/user-workflows/uw-1-order-a-widget.org" . "* UW-1 Order a widget\n\n1. Buyer orders.\n")
    ("src/widget.el" . "(provide 'widget)\n")))

(defun trf-test-git (directory &rest args)
  "Run git with ARGS in DIRECTORY; return its output, or signal on failure."
  (with-temp-buffer
    (let ((default-directory directory))
      (unless (eq 0 (apply #'call-process "git" nil t nil args))
        (error "git %S: %s" args (buffer-string)))
      (buffer-string))))

(defun trf-test-write-files (root files)
  "Write FILES, an alist of (path . content), under ROOT."
  (dolist (file files)
    (let ((path (expand-file-name (car file) root)))
      (make-directory (file-name-directory path) t)
      (with-temp-file path (insert (cdr file))))))

(defun trf-test-make-target-product (&optional files)
  "Create a committed target product holding AGENTS.md and FILES; return its root."
  (let ((root (file-name-as-directory (make-temp-file "trf-target-" t))))
    (trf-test-write-files root (cons (cons "AGENTS.md" trf-test-agents-md)
                                     (or files trf-test-records)))
    (trf-test-git root "init" "--quiet" "--initial-branch=main")
    (trf-test-git root "config" "user.email" "record.keeper@example.com")
    (trf-test-git root "config" "user.name" "Record Keeper")
    (trf-test-git root "add" "--all")
    (trf-test-git root "commit" "--quiet" "--message" "Start")
    root))

(defun trf-test-head (root)
  "Return the commit at HEAD in ROOT."
  (string-trim (trf-test-git root "rev-parse" "HEAD")))

(provide 'trf-test-helper)
;;; trf-test-helper.el ends here
```

**Step 2: Write the failing tests** — `test/trf-git-test.el`:

```elisp
;;; trf-git-test.el --- Tests for trf-git -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf-test-helper)
(require 'trf-git)

(defvar trf-git-test-root nil)

(setup-each () (setq trf-git-test-root (trf-test-make-target-product)))
(teardown-each () (delete-directory trf-git-test-root t))

(deftest trf-git-returns-output ()
  "trf-git returns git's standard output."
  (assert-equal (concat (trf-test-head trf-git-test-root) "\n")
                (trf-git trf-git-test-root "rev-parse" "HEAD")))

(deftest trf-git-signals-on-failure ()
  "trf-git signals an error when git fails."
  (assert-raises 'error (trf-git trf-git-test-root "rev-parse" "no-such-ref")))

(deftest trf-git-lines-splits-output ()
  "trf-git-lines returns one string per non-empty line."
  (assert-includes (trf-git-lines trf-git-test-root "ls-files") "AGENTS.md"))

(deftest trf-git-ref-commit-resolves-and-misses ()
  "trf-git-ref-commit returns the commit a ref names, or nil when it is missing."
  (assert-equal (trf-test-head trf-git-test-root)
                (trf-git-ref-commit trf-git-test-root "HEAD"))
  (assert-nil (trf-git-ref-commit trf-git-test-root "refs/recs/implemented")))

(provide 'trf-git-test)
;;; trf-git-test.el ends here
```

**Step 3:** `$ELK test --pattern=trf-git` → FAIL, `trf-git` not found.

**Step 4: Write `lisp/trf-git.el`**

```elisp
;;; trf-git.el --- Git access for trf -*- lexical-binding: t; -*-

(require 'subr-x)

(defun trf-git (directory &rest args)
  "Run git with ARGS in DIRECTORY and return its output.
Signal an error carrying git's output when git fails."
  (with-temp-buffer
    (let* ((default-directory directory)
           (status (apply #'process-file "git" nil t nil args)))
      (unless (eq status 0)
        (error "git %s: %s" (string-join args " ") (string-trim (buffer-string))))
      (buffer-string))))

(defun trf-git-lines (directory &rest args)
  "Run git with ARGS in DIRECTORY and return its non-empty output lines."
  (split-string (apply #'trf-git directory args) "\n" t))

(defun trf-git-ref-commit (directory ref)
  "Return the commit REF names in DIRECTORY, or nil when REF does not exist."
  (with-temp-buffer
    (let ((default-directory directory))
      (when (eq 0 (process-file "git" nil t nil "rev-parse" "--verify" "--quiet"
                                (concat ref "^{commit}")))
        (string-trim (buffer-string))))))

(provide 'trf-git)
;;; trf-git.el ends here
```

**Step 5:** `$ELK test --pattern=trf-git` → 4 passed. **Step 6: Commit**
"Add git access for trf".

---

### Task 2: Read the target product's projections

**Records:** UW-1: Turn record changes into code (precondition: the target
product keeps records)

**Files:**
- Create: `lisp/trf-project.el`
- Create: `test/trf-project-test.el`

The projections are the bold-named bullet lines of the target product's
AGENTS.md, each naming a projection file in backquotes. The plain bullet lines
naming knowledge-base entries are not projections.

**Step 1: Write the failing tests** — use mock-fs, since nothing here needs git:

```elisp
;;; trf-project-test.el --- Tests for trf-project -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'mock-fs)
(require 'trf-test-helper)
(require 'trf-project)

(deftest trf-project-projection-paths-reads-agents-md ()
  "Each projection gives its file and the directory of its records."
  (with-mock-fs `(("/target/AGENTS.md" . ,trf-test-agents-md))
    (assert-equal '("docs/vision.org" "docs/vision/"
                    "docs/user-workflows.org" "docs/user-workflows/"
                    "docs/features.org" "docs/features/")
                  (trf-project-projection-paths (mock-fs-path "/target/")))))

(deftest trf-project-projection-paths-without-agents-md ()
  "A target product without AGENTS.md has no projections."
  (with-mock-fs '(("/target/" . nil))
    (assert-nil (trf-project-projection-paths (mock-fs-path "/target/")))))

(deftest trf-project-root-finds-agents-md ()
  "The target product's root is the directory holding AGENTS.md."
  (with-mock-fs `(("/target/AGENTS.md" . ,trf-test-agents-md)
                  ("/target/docs/vision.org" . "#+TITLE: Vision\n"))
    (assert-equal (mock-fs-path "/target/")
                  (trf-project-root (mock-fs-path "/target/docs/")))))

(provide 'trf-project-test)
;;; trf-project-test.el ends here
```

**Step 2:** `$ELK test --pattern=trf-project` → FAIL.

**Step 3: Write `lisp/trf-project.el`**

```elisp
;;; trf-project.el --- The target product's projections -*- lexical-binding: t; -*-

(require 'subr-x)

(defconst trf-project--projection-regexp
  "^- \\*\\*[^*\n]+\\*\\*[^`\n]*`\\([^`\n]+\\)\\.org`"
  "A projection line of AGENTS.md; group 1 is its file without \".org\".")

(defun trf-project-root (&optional directory)
  "Return the root of the target product holding DIRECTORY, or nil."
  (when-let* ((root (locate-dominating-file (or directory default-directory)
                                            "AGENTS.md")))
    (file-name-as-directory root)))

(defun trf-project-projection-paths (root)
  "Return the projection paths named by ROOT's AGENTS.md, relative to ROOT.
Each projection gives its file, then the directory of its records."
  (let ((agents (expand-file-name "AGENTS.md" root))
        paths)
    (when (file-exists-p agents)
      (with-temp-buffer
        (insert-file-contents agents)
        (goto-char (point-min))
        (while (re-search-forward trf-project--projection-regexp nil t)
          (let ((stem (match-string 1)))
            (push (concat stem ".org") paths)
            (push (concat stem "/") paths)))))
    (nreverse paths)))

(provide 'trf-project)
;;; trf-project.el ends here
```

If `locate-dominating-file` does not work through mock-fs, replace that one
test with a real temp directory (`make-temp-file ... t`) and note it in the
commit body; do not change the function to suit the mock.

**Step 4:** `$ELK test --pattern=trf-project` → 3 passed.
**Step 5:** Run this plan's sanity check against the real file:
`emacs --batch -L lisp --eval "(progn (require 'trf-project) (print (trf-project-projection-paths \"$PWD/\")))"`
→ prints the six projection files of this repository and their directories.
**Step 6: Commit** "Read the target product's projections".

---

### Task 3: Find the record changes

**Records:** F-1: Ask the IDE to implement the record changes (§Takes);
UW-1: Turn record changes into code (step 2)

**Files:**
- Create: `lisp/trf-record-changes.el`
- Create: `test/trf-record-changes-test.el`

Record changes are every difference, committed or not, between the commit
`refs/recs/implemented` names and the working tree, under the projection paths.
With no such ref, nothing has been implemented, so the base is git's empty
tree. Each change carries its path, its status (`added`, `modified`,
`deleted`), and the identifier and title of its record, read from its
`* <ID> <title>` heading, or only a title from `#+TITLE:` for a projection file.

**Step 1: Write the failing tests**

```elisp
;;; trf-record-changes-test.el --- Tests for trf-record-changes -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf-test-helper)
(require 'trf-record-changes)

(defvar trf-record-changes-test-root nil)

(setup-each () (setq trf-record-changes-test-root (trf-test-make-target-product)))
(teardown-each () (delete-directory trf-record-changes-test-root t))

(defun trf-record-changes-test-mark-implemented ()
  (trf-test-git trf-record-changes-test-root "update-ref" "refs/recs/implemented" "HEAD"))

(deftest trf-record-changes-all-records-before-any-implementation ()
  "With nothing implemented, every record is a record change; other files are not."
  (assert-equal
   '((:path "docs/user-workflows.org" :status added :identifier nil :title "User Workflows")
     (:path "docs/user-workflows/uw-1-order-a-widget.org" :status added
      :identifier "UW-1" :title "Order a widget")
     (:path "docs/vision.org" :status added :identifier nil :title "Vision"))
   (trf-record-changes trf-record-changes-test-root)))

(deftest trf-record-changes-none-once-implemented ()
  "Once every record is implemented, there are no record changes."
  (trf-record-changes-test-mark-implemented)
  (assert-nil (trf-record-changes trf-record-changes-test-root)))

(deftest trf-record-changes-uncommitted-edit ()
  "An uncommitted edit to a record is a record change."
  (trf-record-changes-test-mark-implemented)
  (trf-test-write-files trf-record-changes-test-root
                        '(("docs/user-workflows/uw-1-order-a-widget.org"
                           . "* UW-1 Order a widget\n\n1. Buyer orders twice.\n")))
  (assert-equal '((:path "docs/user-workflows/uw-1-order-a-widget.org" :status modified
                   :identifier "UW-1" :title "Order a widget"))
                (trf-record-changes trf-record-changes-test-root)))

(deftest trf-record-changes-new-and-deleted-records ()
  "A new record and a deleted record are record changes; a code edit is not."
  (trf-record-changes-test-mark-implemented)
  (trf-test-write-files trf-record-changes-test-root
                        '(("docs/features/f-1-order.org" . "* F-1 Order\n\nTakes: a widget.\n")
                          ("src/widget.el" . "(provide 'widget) ; edited\n")))
  (delete-file (expand-file-name "docs/user-workflows/uw-1-order-a-widget.org"
                                 trf-record-changes-test-root))
  (assert-equal '((:path "docs/features/f-1-order.org" :status added
                   :identifier "F-1" :title "Order")
                  (:path "docs/user-workflows/uw-1-order-a-widget.org" :status deleted
                   :identifier "UW-1" :title "Order a widget"))
                (trf-record-changes trf-record-changes-test-root)))

(provide 'trf-record-changes-test)
;;; trf-record-changes-test.el ends here
```

**Step 2:** `$ELK test --pattern=trf-record-changes` → FAIL.

**Step 3: Write `lisp/trf-record-changes.el`**

```elisp
;;; trf-record-changes.el --- The record changes -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'trf-git)
(require 'trf-project)

(defconst trf-record-changes-implemented-ref "refs/recs/implemented")

(defconst trf-record-changes--empty-tree "4b825dc642cb6eb9a060e54bf8d69288fbee4904")

(defun trf-record-changes--base (root)
  "Return what ROOT's record changes are measured from."
  (or (trf-git-ref-commit root trf-record-changes-implemented-ref)
      trf-record-changes--empty-tree))

(defun trf-record-changes--heading (text path)
  "Return (IDENTIFIER . TITLE) for the record TEXT at PATH."
  (cond ((string-match "^\\* \\([A-Z]+-[0-9]+\\) \\(.+\\)$" text)
         (cons (match-string 1 text) (match-string 2 text)))
        ((string-match "^#\\+TITLE: \\(.+\\)$" text)
         (cons nil (match-string 1 text)))
        (t (cons nil (file-name-base path)))))

(defun trf-record-changes--describe (root base path status)
  "Return the record change at PATH in ROOT with git STATUS, measured from BASE."
  (let* ((text (if (equal status "D")
                   (trf-git root "show" (concat base ":" path))
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name path root))
                   (buffer-string))))
         (heading (trf-record-changes--heading text path)))
    (list :path path
          :status (pcase status ("A" 'added) ("D" 'deleted) (_ 'modified))
          :identifier (car heading)
          :title (cdr heading))))

(defun trf-record-changes (root)
  "Return the record changes of the target product at ROOT, by path."
  (when-let* ((paths (trf-project-projection-paths root)))
    (let* ((base (trf-record-changes--base root))
           (changed (mapcar (lambda (line)
                              (let ((fields (split-string line "\t")))
                                (cons (car (last fields)) (substring (car fields) 0 1))))
                            (apply #'trf-git-lines root "diff" "--name-status"
                                   "--no-renames" base "--" paths)))
           (untracked (mapcar (lambda (path) (cons path "A"))
                              (apply #'trf-git-lines root "ls-files" "--others"
                                     "--exclude-standard" "--" paths))))
      (mapcar (lambda (entry)
                (trf-record-changes--describe root base (car entry) (cdr entry)))
              (sort (append changed untracked)
                    (lambda (a b) (string< (car a) (car b))))))))

(provide 'trf-record-changes)
;;; trf-record-changes.el ends here
```

Cite `F-1 §Takes` above `trf-record-changes`.

**Step 4:** `$ELK test --pattern=trf-record-changes` → 4 passed.
**Step 5: Commit** "Find the record changes".

---

### Task 4: Commit the record changes on confirmation

**Records:** F-1: Ask the IDE to implement the record changes (§Rules: "On
confirmation, the IDE commits the record changes and nothing else, in one
commit naming each by its identifier and title")

**Files:**
- Modify: `lisp/trf-record-changes.el`
- Modify: `test/trf-record-changes-test.el`

**Open point — STOP before Step 3 and ask the user.** When every record change
is already committed (the record keeper committed them before asking), there is
nothing left for "one commit holding the record changes" to hold. F-1 does not
say what happens then. Proposed: no commit is made, and the implementation
workflow starts from HEAD. That is a reading of F-1 the record keeper has not
agreed to; get the answer and, if it changes F-1, have it written through
recs-writing-records before writing the code. The tests below assume the
proposal; adjust them to the answer.

**Step 1: Write the failing tests** (append):

```elisp
(deftest trf-record-changes-commit-holds-only-the-record-changes ()
  "The commit holds the record changes and leaves other edits uncommitted."
  (trf-record-changes-test-mark-implemented)
  (trf-test-write-files trf-record-changes-test-root
                        '(("docs/user-workflows/uw-1-order-a-widget.org"
                           . "* UW-1 Order a widget\n\n1. Buyer orders twice.\n")
                          ("src/widget.el" . "(provide 'widget) ; edited\n")))
  (let* ((root trf-record-changes-test-root)
         (commit (trf-record-changes-commit root (trf-record-changes root))))
    (assert-equal (trf-test-head root) commit)
    (assert-equal "docs/user-workflows/uw-1-order-a-widget.org\n"
                  (trf-test-git root "show" "--name-only" "--format=" commit))
    (assert-equal " M src/widget.el\n" (trf-test-git root "status" "--porcelain"))))

(deftest trf-record-changes-commit-names-each-record-change ()
  "The commit message names each record change by identifier and title."
  (let* ((root trf-record-changes-test-root))
    (trf-record-changes-test-mark-implemented)
    (trf-test-write-files root '(("docs/features/f-1-order.org" . "* F-1 Order\n")
                                 ("docs/vision.org" . "#+TITLE: Vision\n\nMore.\n")))
    (trf-record-changes-commit root (trf-record-changes root))
    (assert-equal "Record changes\n\n- F-1 Order\n- Vision\n"
                  (trf-test-git root "log" "-1" "--format=%B"))))

(deftest trf-record-changes-commit-when-already-committed ()
  "When the record changes are already committed, no commit is made."
  (let* ((root trf-record-changes-test-root)
         (head (trf-test-head root)))
    (assert-equal head (trf-record-changes-commit root (trf-record-changes root)))
    (assert-equal head (trf-test-head root))))
```

**Step 2:** run → FAIL.

**Step 3: Implement** (append to `lisp/trf-record-changes.el`, above `provide`):

```elisp
(defun trf-record-changes-commit-message (changes)
  "Return the commit message naming each of CHANGES."
  (concat "Record changes\n\n"
          (mapconcat (lambda (change)
                       (concat "- " (string-join
                                     (delq nil (list (plist-get change :identifier)
                                                     (plist-get change :title)))
                                     " ")))
                     changes "\n")
          "\n"))

(defun trf-record-changes-commit (root changes)
  "Commit CHANGES in ROOT and nothing else; return the commit they are in."
  (let ((paths (mapcar (lambda (change) (plist-get change :path)) changes)))
    (when (apply #'trf-git-lines root "status" "--porcelain" "--" paths)
      (apply #'trf-git root "add" "--all" "--" paths)
      (apply #'trf-git root "commit" "--quiet" "--only"
             "--message" (trf-record-changes-commit-message changes) "--" paths))
    (string-trim (trf-git root "rev-parse" "HEAD"))))
```

Cite `F-1 §Rules` above `trf-record-changes-commit`.

**Step 4:** run → all pass. **Step 5: Commit** "Commit the record changes on
confirmation".

---

### Task 5: The worktree the implementation workflow runs in

**Records:** UW-1: Turn record changes into code (step 3 "apart from the
codebase", step 7, extensions *a, 6a, 7a); F-1: Ask the IDE to implement the
record changes (§Rules: "the implementation workflow starts from that commit")

**Files:**
- Create: `lisp/trf-worktree.el`
- Create: `test/trf-worktree-test.el`

A worktree is created under the target product's `.worktrees/` on a branch
`trf/<name>`, from the record changes' commit. Its `.trf/` directory, excluded
from git through the repository's `info/exclude`, holds two files: `root` (the
target product's root) and `base` (the record changes' commit).

**Step 1: Write the failing tests**

```elisp
;;; trf-worktree-test.el --- Tests for trf-worktree -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf-test-helper)
(require 'trf-worktree)

(defvar trf-worktree-test-root nil)

(setup-each () (setq trf-worktree-test-root (trf-test-make-target-product)))
(teardown-each () (delete-directory trf-worktree-test-root t))

(defun trf-worktree-test-read (directory file)
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat ".trf/" file) directory))
    (string-trim (buffer-string))))

(deftest trf-worktree-create-starts-from-the-commit ()
  "The worktree starts from the given commit and records its root and base."
  (let* ((root trf-worktree-test-root)
         (base (trf-test-head root))
         (directory (trf-worktree-create root base)))
    (assert-equal base (trf-test-head directory))
    (assert-equal base (trf-worktree-test-read directory "base"))
    (assert-equal root (trf-worktree-test-read directory "root"))
    (assert-equal "" (trf-test-git directory "status" "--porcelain"))
    (assert-match "^trf/" (string-trim (trf-test-git directory "branch" "--show-current")))))

(deftest trf-worktree-of-finds-the-worktree ()
  "Any file inside a worktree leads back to it."
  (let ((directory (trf-worktree-create trf-worktree-test-root
                                        (trf-test-head trf-worktree-test-root))))
    (assert-equal directory
                  (trf-worktree-of (expand-file-name "docs/vision.org" directory)))))

(deftest trf-worktree-codebase-moved ()
  "The codebase has moved once a commit lands on it after the worktree's base."
  (let* ((root trf-worktree-test-root)
         (directory (trf-worktree-create root (trf-test-head root))))
    (assert-nil (trf-worktree-codebase-moved-p directory))
    (trf-test-write-files root '(("src/other.el" . "(provide 'other)\n")))
    (trf-test-git root "add" "--all")
    (trf-test-git root "commit" "--quiet" "--message" "Other work")
    (assert-true (trf-worktree-codebase-moved-p directory))))

(deftest trf-worktree-accept-brings-the-work-in ()
  "Accepting merges the work into the codebase and marks the base implemented."
  (let* ((root trf-worktree-test-root)
         (base (trf-test-head root))
         (directory (trf-worktree-create root base)))
    (trf-test-write-files directory '(("src/widget.el" . "(provide 'widget) ; built\n")))
    (trf-test-git directory "commit" "--quiet" "--all" "--message" "Build")
    (trf-worktree-accept directory)
    (assert-equal "(provide 'widget) ; built\n"
                  (with-temp-buffer
                    (insert-file-contents (expand-file-name "src/widget.el" root))
                    (buffer-string)))
    (assert-equal base (string-trim (trf-test-git root "rev-parse" "refs/recs/implemented")))))

(provide 'trf-worktree-test)
;;; trf-worktree-test.el ends here
```

**Step 2:** run → FAIL.

**Step 3: Write `lisp/trf-worktree.el`**

```elisp
;;; trf-worktree.el --- Where the implementation workflow runs -*- lexical-binding: t; -*-

(require 'subr-x)
(require 'trf-git)
(require 'trf-record-changes)

(defun trf-worktree--exclude (root pattern)
  "Make git in ROOT ignore PATTERN, through the repository's info/exclude."
  (let ((exclude (expand-file-name
                  "info/exclude"
                  (string-trim (trf-git root "rev-parse" "--path-format=absolute"
                                        "--git-common-dir")))))
    (make-directory (file-name-directory exclude) t)
    (unless (and (file-exists-p exclude)
                 (with-temp-buffer
                   (insert-file-contents exclude)
                   (re-search-forward (concat "^" (regexp-quote pattern) "$") nil t)))
      (with-temp-buffer
        (insert pattern "\n")
        (append-to-file (point-min) (point-max) exclude)))))

(defun trf-worktree--write (directory file content)
  (with-temp-file (expand-file-name (concat ".trf/" file) directory)
    (insert content "\n")))

(defun trf-worktree--read (directory file)
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat ".trf/" file) directory))
    (string-trim (buffer-string))))

(defun trf-worktree-create (root commit)
  "Create a worktree of ROOT starting from COMMIT; return its directory."
  (let* ((name (format-time-string "trf-%Y%m%d-%H%M%S"))
         (directory (file-name-as-directory
                     (expand-file-name (concat ".worktrees/" name) root))))
    (trf-worktree--exclude root ".worktrees/")
    (trf-worktree--exclude root ".trf/")
    (trf-git root "worktree" "add" "--quiet" "-b" (concat "trf/" name) directory commit)
    (make-directory (expand-file-name ".trf/" directory) t)
    (trf-worktree--write directory "root" root)
    (trf-worktree--write directory "base" commit)
    directory))

(defun trf-worktree-of (file)
  "Return the worktree holding FILE, or nil."
  (when-let* ((directory (locate-dominating-file file ".trf")))
    (file-name-as-directory (expand-file-name directory))))

(defun trf-worktree-root (directory)
  "Return the target product's root for the worktree DIRECTORY."
  (trf-worktree--read directory "root"))

(defun trf-worktree-base (directory)
  "Return the record changes' commit the worktree DIRECTORY started from."
  (trf-worktree--read directory "base"))

(defun trf-worktree-codebase-moved-p (directory)
  "Return non-nil when the codebase has moved since DIRECTORY's base."
  (not (equal (trf-git-ref-commit (trf-worktree-root directory) "HEAD")
              (trf-worktree-base directory))))

(defun trf-worktree-accept (directory)
  "Bring DIRECTORY's work into the codebase and mark its record changes implemented."
  (let ((root (trf-worktree-root directory))
        (branch (string-trim (trf-git directory "branch" "--show-current"))))
    (trf-git root "merge" "--quiet" "--no-ff" "--no-edit" branch)
    (trf-git root "update-ref" trf-record-changes-implemented-ref
             (trf-worktree-base directory))))

(provide 'trf-worktree)
;;; trf-worktree.el ends here
```

Note on `trf-worktree-codebase-moved-p`: after the record changes' commit (Task
4) the codebase's HEAD *is* the base, so any later commit on it is the codebase
moving (UW-1 7a). Cite `UW-1 7a` there is not allowed — code points at records
it rests on; cite `F-1 §Rules` above `trf-worktree-create`.

**Step 4:** run → 4 passed. **Step 5: Commit** "Run the implementation
workflow in a worktree".

---

### Task 6: The machines and their instructions

**Records:** UW-1: Turn record changes into code (steps 3–5); machine,
implementation workflow, reconciliation assessor (ubiquitous language)

**Files:**
- Create: `lisp/trf-machines.el`
- Create: `test/trf-machines-test.el`

Each machine is data: its name, the slug naming its handoff file, the skill it
is configured with (a device), and its instructions. A machine's handoff file
is `.trf/<slug>.org`; writing it is the machine's last act. The reconciliation
assessor's handoff file is the record implementation report. The skills default
to the recs-* skills and are customizable per target product.

**Step 1: Write the failing tests**

```elisp
;;; trf-machines-test.el --- Tests for trf-machines -*- lexical-binding: t; -*-

(require 'e-unit)
(require 'trf-machines)

(defconst trf-machines-test-changes
  '((:path "docs/user-workflows/uw-1-order-a-widget.org" :status modified
     :identifier "UW-1" :title "Order a widget")
    (:path "docs/vision.org" :status added :identifier nil :title "Vision")))

(deftest trf-machines-implementation-workflow-order ()
  "The implementation workflow is the brainstorming, planning and executing machines, in turn."
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
    (assert-match "- Vision (added): docs/vision.org" text)
    (assert-match "recs-writing-plans" text)
    (assert-match "Design written to docs/plans/x-design.md" text)
    (assert-match "write \\.trf/planning\\.org" text)))
```

**Step 2:** run → FAIL.

**Step 3: Write `lisp/trf-machines.el`**

```elisp
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
     . "Use the %s skill to carry out the implementation plan, committing as you go.")))

(defun trf-machines--make (slug name)
  (list :slug slug :name name
        :skill (alist-get slug trf-machines-skills)
        :work (format (alist-get slug trf-machines--work)
                      (alist-get slug trf-machines-skills))))

(defun trf-machines-implementation-workflow ()
  "Return the machines of the implementation workflow, in the order they run."
  (list (trf-machines--make 'brainstorming "brainstorming machine")
        (trf-machines--make 'planning "planning machine")
        (trf-machines--make 'executing "executing machine")))

(defun trf-machines-reconciliation-assessor ()
  "Return the reconciliation assessor."
  (list :slug 'record-implementation-report :name "reconciliation assessor" :skill nil
        :work (concat
               "For each of the record changes, find the tests that cite it, run them, "
               "and assess whether the codebase is reconciled with it. Change no file "
               "other than the record implementation report. The report is an org file "
               "with one heading per record change, its identifier and title, listing "
               "each test that cites it and whether it passed; a record change that no "
               "test cites is listed as untested. If you cannot assess at all, say so "
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
```

Cite the ubiquitous-language terms nowhere in comments: the code's names carry
them. Cite `UW-1 §Main success scenario` above
`trf-machines-implementation-workflow` only if a pointer is wanted at all.

**Step 4:** run → 3 passed. **Step 5: Commit** "Define the machines and their
instructions".

---

### Task 7: Run a machine over ACP

**Records:** ADR-2: Talk to machines over ACP; UW-1: Turn record changes into
code (steps 3–5, extension 3a)

**Files:**
- Create: `lisp/trf-session.el`
- Create: `test/trf-session-test.el`

A machine runs as an agent-shell session started in the worktree. Once the
session is initialized, the IDE submits the machine's instructions. After each
turn, if the machine's handoff file exists, the machine has finished. If the
session reports an error before then, the machine has stopped (UW-1 3a) and its
session buffer, which shows why, is displayed; the IDE adds no text of its own.
If the session's buffer is killed before then, the record keeper has abandoned
(UW-1 *a) and nothing more runs.

The AI program comes from `trf-ai-program-config`, or, when that is nil, from
agent-shell's own selection (`agent-shell-select-config`), which offers
agent-shell's preferred config first.

**Step 1: Write the failing tests.** agent-shell is replaced by spies: no AI
program runs.

```elisp
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
     (assert-equal 1 (spy-call-count calls))
     (assert-equal "Do it." (plist-get (spy-last-call calls) :text)))))

(deftest trf-session-finished-when-handoff-exists ()
  "A turn that ends with the handoff file written finishes the machine."
  (trf-session-test-with-fake-shell
   (let (finished)
     (trf-session-run trf-session-test-machine trf-session-test-directory "Do it."
                      (lambda (text) (setq finished text)) #'ignore)
     (trf-session-test-fire 'turn-complete)
     (assert-nil finished)
     (with-temp-file (expand-file-name ".trf/planning.org" trf-session-test-directory)
       (insert "Plan written."))
     (trf-session-test-fire 'turn-complete)
     (assert-equal "Plan written." finished))))

(deftest trf-session-stopped-on-error ()
  "An error before the handoff file stops the machine, once."
  (trf-session-test-with-fake-shell
   (let ((stopped 0))
     (trf-session-run trf-session-test-machine trf-session-test-directory "Do it."
                      #'ignore (lambda () (setq stopped (1+ stopped))))
     (trf-session-test-fire 'error)
     (trf-session-test-fire 'error)
     (assert-equal 1 stopped))))

(deftest trf-session-abandoned-when-buffer-killed ()
  "A session cleaned up before the handoff calls neither continuation."
  (trf-session-test-with-fake-shell
   (let (called)
     (trf-session-run trf-session-test-machine trf-session-test-directory "Do it."
                      (lambda (_) (setq called t)) (lambda () (setq called t)))
     (trf-session-test-fire 'clean-up)
     (trf-session-test-fire 'turn-complete)
     (assert-nil called))))
```

**Step 2:** run → FAIL.

**Step 3: Write `lisp/trf-session.el`**

```elisp
;;; trf-session.el --- Machines over ACP -*- lexical-binding: t; -*-

(require 'agent-shell)
(require 'trf-machines)

(defcustom trf-ai-program-config nil
  "The agent-shell config of the AI program the IDE runs as machines.
When nil, the record keeper picks one through agent-shell."
  :type 'sexp
  :group 'trf)

(defun trf-session--config ()
  (or trf-ai-program-config
      (agent-shell-select-config :prompt "AI program for the machines: ")))

(defun trf-session-run (machine directory instructions on-finished on-stopped)
  "Run MACHINE in DIRECTORY with INSTRUCTIONS; return its session buffer.
Call ON-FINISHED with the handoff file's text once MACHINE writes it.
Call ON-STOPPED, with the session buffer displayed, if the session fails first."
  (let* ((default-directory directory)
         (handoff (expand-file-name (trf-machines-handoff-file machine) directory))
         (buffer (agent-shell-start :config (trf-session--config)))
         (over nil))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'init-finished
     :on-event (lambda (_event)
                 (agent-shell-insert :text instructions :submit t :no-focus t
                                     :shell-buffer buffer)))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'turn-complete
     :on-event (lambda (_event)
                 (when (and (not over) (file-exists-p handoff))
                   (setq over t)
                   (funcall on-finished
                            (with-temp-buffer
                              (insert-file-contents handoff)
                              (buffer-string))))))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'error
     :on-event (lambda (_event)
                 (unless over
                   (setq over t)
                   (when (buffer-live-p buffer) (display-buffer buffer))
                   (funcall on-stopped))))
    (agent-shell-subscribe-to
     :shell-buffer buffer :event 'clean-up
     :on-event (lambda (_event) (setq over t)))
    buffer))

(provide 'trf-session)
;;; trf-session.el ends here
```

Cite `ADR-2 §Decision: Talk to machines over ACP` above `trf-session-run`.

Before Step 4, check two claims against agent-shell v0.86.2's source in
`.elk/deps/elpaca/repos/agent-shell/agent-shell.el` and STOP if either is
false: `agent-shell-start` returns the shell buffer, and it starts the session
in `default-directory`'s project (`agent-shell-cwd`).

**Step 4:** run → 4 passed. **Step 5: Commit** "Run machines over ACP".

---

### Task 8: Ask the IDE to implement the record changes

**Records:** F-1: Ask the IDE to implement the record changes; MSG-1: No
record changes to implement; UW-1: Turn record changes into code (steps 1–6,
2a, *a)

**Files:**
- Modify: `lisp/trf.el`
- Modify: `test/trf-test.el`

The command `trf-implement-record-changes`: finds the record changes (none →
MSG-1, and nothing else happens); shows them in the buffer `*trf record
changes*`; asks the record keeper to confirm (declining is *a: nothing
happens); commits them (Task 4); creates the worktree (Task 5); runs the
brainstorming, planning and executing machines in turn, each given the
previous machine's handoff text; then runs the reconciliation assessor; then
opens the record implementation report.

**Step 0:** Open `docs/message-catalog/msg-1-no-record-changes-to-implement.org`
and copy the exact `Text:` into the test below where it says `<MSG-1 text>`.

**Step 1: Write the failing tests** (append to `test/trf-test.el`; add
`(require 'trf-test-helper)` at the top):

```elisp
(defvar trf-test-root nil)

(setup-each () (setq trf-test-root (trf-test-make-target-product)))
(teardown-each () (delete-directory trf-test-root t))

(defmacro trf-test-with-machines (runs &rest body)
  "Run BODY with each machine finishing at once; RUNS collects (name . instructions)."
  `(cl-letf (((symbol-function 'trf-session-run)
              (lambda (machine directory instructions on-finished _on-stopped)
                (push (cons (plist-get machine :name) instructions) ,runs)
                (with-temp-file (expand-file-name (trf-machines-handoff-file machine) directory)
                  (insert "Handed off by " (plist-get machine :name) "."))
                (funcall on-finished (concat "Handed off by " (plist-get machine :name) "."))
                (get-buffer-create "*trf fake machine*")))
             ((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
     ,@body))

(deftest trf-implement-without-record-changes ()
  "With no record changes the IDE gives MSG-1 and does nothing else."
  (trf-test-git trf-test-root "update-ref" "refs/recs/implemented" "HEAD")
  (let ((default-directory trf-test-root))
    ;; MSG-1: No record changes to implement
    (assert-raises-with-message 'user-error "<MSG-1 text>"
      (trf-implement-record-changes))))

(deftest trf-implement-runs-the-machines-in-turn ()
  "The implementation workflow's machines run in turn, then the reconciliation assessor."
  (let ((runs nil) (default-directory trf-test-root))
    (trf-test-with-machines runs
      (trf-implement-record-changes))
    (assert-equal '("brainstorming machine" "planning machine" "executing machine"
                    "reconciliation assessor")
                  (mapcar #'car (reverse runs)))
    (assert-match "Handed off by brainstorming machine\\."
                  (cdr (nth 1 (reverse runs))))))

(deftest trf-implement-commits-and-opens-the-report ()
  "Confirming commits the record changes; the report opens at the end."
  (let ((runs nil) (default-directory trf-test-root))
    (trf-test-with-machines runs
      (trf-implement-record-changes))
    (assert-match "^Record changes" (trf-test-git trf-test-root "log" "-1" "--format=%s"))
    (assert-match "/\\.trf/record-implementation-report\\.org\\'"
                  (buffer-file-name (window-buffer (selected-window))))))

(deftest trf-implement-declined ()
  "Declining to confirm leaves the target product as it was."
  (let ((head (trf-test-head trf-test-root)) (default-directory trf-test-root))
    (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) nil))
              ((symbol-function 'trf-session-run) (lambda (&rest _) (error "Ran a machine"))))
      (trf-implement-record-changes))
    (assert-equal head (trf-test-head trf-test-root))
    (assert-nil (file-exists-p (expand-file-name ".worktrees" trf-test-root)))))
```

Check e-unit for an assertion on an error's message (`assert-raises-with-message`
or similar in `e-unit.el`); if it has none, use `condition-case` and
`assert-equal` on `(cadr err)`. The pointer line above the MSG-1 assertion is
the AGENTS.md test-message rule.

**Step 2:** run → FAIL.

**Step 3: Implement** — replace `lisp/trf.el` with:

```elisp
;;; trf.el --- The IDE for projects that keep records -*- lexical-binding: t; -*-

(require 'trf-project)
(require 'trf-record-changes)
(require 'trf-worktree)
(require 'trf-machines)
(require 'trf-session)

(defconst trf-version "0.1.0")

(defconst trf--no-record-changes "<MSG-1 text>")

(defun trf--show-record-changes (changes)
  (with-current-buffer (get-buffer-create "*trf record changes*")
    (let ((inhibit-read-only t))
      (erase-buffer)
      (insert (mapconcat #'trf-machines--change-line changes "\n") "\n"))
    (special-mode)
    (display-buffer (current-buffer))))

(defun trf--run-machines (machines directory changes previous-handoff on-done)
  "Run MACHINES in turn in DIRECTORY on CHANGES, then call ON-DONE."
  (if (null machines)
      (funcall on-done)
    (let ((machine (car machines)))
      (trf-session-run
       machine directory
       (trf-machines-instructions machine changes previous-handoff)
       (lambda (handoff)
         (trf--run-machines (cdr machines) directory changes handoff on-done))
       #'ignore))))

(defun trf--assess (directory changes)
  "Run the reconciliation assessor in DIRECTORY on CHANGES, then open its report."
  (let ((assessor (trf-machines-reconciliation-assessor)))
    (trf--run-machines
     (list assessor) directory changes nil
     (lambda ()
       (pop-to-buffer-same-window
        (find-file-noselect
         (expand-file-name (trf-machines-handoff-file assessor) directory)))))))

;;;###autoload
(defun trf-implement-record-changes ()
  "Ask the IDE to implement the record changes."
  (interactive)
  (let* ((root (or (trf-project-root) default-directory))
         (changes (trf-record-changes root)))
    (unless changes
      (user-error trf--no-record-changes))
    (trf--show-record-changes changes)
    (when (y-or-n-p "Implement these record changes? ")
      (let* ((commit (trf-record-changes-commit root changes))
             (directory (trf-worktree-create root commit)))
        (trf--run-machines (trf-machines-implementation-workflow) directory changes nil
                           (lambda () (trf--assess directory changes)))))))

(provide 'trf)
;;; trf.el ends here
```

Replace `<MSG-1 text>` with the record's exact text and cite it with a pointer
line `;; MSG-1: No record changes to implement` above `trf--no-record-changes`.
Cite `F-1 §Rules` above `trf-implement-record-changes`.

The `y-or-n-p` prompt is a question, not a refusal or a failure, so it is not
cataloged text. If the user disagrees, STOP: it would need a catalog record.

**Step 4:** run the whole suite: `$ELK test` → all pass.
**Step 5: Commit** "Ask the IDE to implement the record changes".

---

### Task 9: Accept or reject the record implementation report

**Records:** F-1: Ask the IDE to implement the record changes (§Next); UW-1:
Turn record changes into code (step 7, extensions 6a, 7a)

**Files:**
- Modify: `lisp/trf.el`
- Modify: `test/trf-test.el`

Two commands, run from the record implementation report (or any file in the
worktree). `trf-accept-record-implementation-report`: when the codebase has
moved since the worktree's base (UW-1 7a), runs the implementation workflow
again in the worktree, with the record changes re-read from the base, and then
the reconciliation assessor (back to step 5); otherwise brings the work into
the codebase and marks the record changes implemented (Task 5's
`trf-worktree-accept`). `trf-reject-record-implementation-report`: closes the
report's buffer and keeps the worktree (6a).

How the machines learn that 7a applies: the brainstorming machine's
instructions gain the line "The codebase has moved since this work started;
bring the work up to date with it." Pass it as `previous-handoff` to the first
machine.

**Step 1: Write the failing tests** (append):

```elisp
(deftest trf-accept-brings-the-work-in ()
  "Accepting merges the worktree into the codebase and marks the record changes implemented."
  (let ((runs nil) (default-directory trf-test-root))
    (trf-test-with-machines runs (trf-implement-record-changes))
    (let ((base (trf-test-head trf-test-root)))
      (trf-accept-record-implementation-report)
      (assert-equal base (string-trim (trf-test-git trf-test-root "rev-parse"
                                                    "refs/recs/implemented")))
      (assert-nil (trf-record-changes trf-test-root)))))

(deftest trf-accept-on-a-moved-codebase-reruns ()
  "On a moved codebase, accepting runs the implementation workflow and the assessor again."
  (let ((runs nil) (default-directory trf-test-root))
    (trf-test-with-machines runs (trf-implement-record-changes))
    (trf-test-write-files trf-test-root '(("src/other.el" . "(provide 'other)\n")))
    (trf-test-git trf-test-root "add" "--all")
    (trf-test-git trf-test-root "commit" "--quiet" "--message" "Other work")
    (setq runs nil)
    (trf-test-with-machines runs (trf-accept-record-implementation-report))
    (assert-equal '("brainstorming machine" "planning machine" "executing machine"
                    "reconciliation assessor")
                  (mapcar #'car (reverse runs)))
    (assert-match "The codebase has moved" (cdar (last runs)))
    (assert-nil (trf-git-ref-commit trf-test-root "refs/recs/implemented"))))

(deftest trf-reject-keeps-the-work ()
  "Rejecting keeps the worktree and implements nothing."
  (let ((runs nil) (default-directory trf-test-root))
    (trf-test-with-machines runs (trf-implement-record-changes))
    (let ((directory (trf-worktree-of buffer-file-name)))
      (trf-reject-record-implementation-report)
      (assert-true (file-directory-p directory))
      (assert-nil (trf-git-ref-commit trf-test-root "refs/recs/implemented")))))
```

Note `trf-accept-brings-the-work-in` ends with no record changes: that relies on
Task 3 measuring from `refs/recs/implemented` — the records committed by Task 4
are what the ref points at.

**Step 2:** run → FAIL.

**Step 3: Implement** (add to `lisp/trf.el` above `provide`):

```elisp
(defconst trf--codebase-moved
  "The codebase has moved since this work started; bring the work up to date with it.")

(defun trf--worktree-here ()
  (or (trf-worktree-of (or buffer-file-name default-directory))
      (user-error "Not in a worktree of the IDE")))

;;;###autoload
(defun trf-accept-record-implementation-report ()
  "Accept the record implementation report of this worktree."
  (interactive)
  (let ((directory (trf--worktree-here)))
    (if (trf-worktree-codebase-moved-p directory)
        (let ((changes (trf-record-changes--between
                        (trf-worktree-root directory) directory)))
          (trf--run-machines (trf-machines-implementation-workflow) directory changes
                             trf--codebase-moved
                             (lambda () (trf--assess directory changes))))
      (trf-worktree-accept directory))))

;;;###autoload
(defun trf-reject-record-implementation-report ()
  "Reject the record implementation report of this worktree, keeping its work."
  (interactive)
  (trf--worktree-here)
  (kill-buffer))
```

Two things in this step need deciding before writing them, so STOP and ask:

1. `"Not in a worktree of the IDE"` is IDE-authored refusal text. It needs a
   message-catalog record (MSG-2) agreed with the user first, per
   Cataloged-Text Ordering — or the commands can simply do nothing outside a
   worktree. Ask which.
2. The record changes for the rerun: the worktree's base commit holds them, so
   `trf-record-changes--between` must list the records changed between
   `refs/recs/implemented` (or the empty tree) and the base. Write it in
   `lisp/trf-record-changes.el` with a test, reusing `trf-record-changes--describe`
   with `git diff --name-status <from> <base> -- <paths>`.

The 7a instruction line is instruction text for a machine, not text the IDE
gives the record keeper, so it is not cataloged.

**Step 4:** run the whole suite → all pass. **Step 5: Commit** "Accept or
reject the record implementation report".

---

### Task 10: The IDE's own Emacs configuration

**Records:** ADR-1: Ship the IDE as a bare Emacs with its own pinned binary;
ADR-2: Talk to machines over ACP

**Files:**
- Create: `ide/init.el`
- Create: `ide/early-init.el`
- Create: `bin/trf`
- Modify: `.gitignore` (add `ide/elpa/`)

**Step 1: Write `ide/early-init.el`**

```elisp
;;; early-init.el --- The IDE's early init -*- lexical-binding: t; -*-

(setq package-enable-at-startup nil)

;;; early-init.el ends here
```

**Step 2: Write `ide/init.el`** — the packages ADR-1 pins, installed into the
IDE's own directory, then trf:

```elisp
;;; init.el --- The IDE's init -*- lexical-binding: t; -*-

(require 'package)
(setq package-user-dir (expand-file-name "elpa/" user-emacs-directory))
(package-initialize)

(defconst trf-ide-packages
  '((shell-maker :url "https://github.com/xenodium/shell-maker" :rev "v0.97.5")
    (acp :url "https://github.com/xenodium/acp.el" :rev "v0.15.1")
    (agent-shell :url "https://github.com/xenodium/agent-shell" :rev "v0.86.2")))

(dolist (spec trf-ide-packages)
  (unless (package-installed-p (car spec))
    (package-vc-install (list (car spec) :url (plist-get (cdr spec) :url))
                        (plist-get (cdr spec) :rev))))

(add-to-list 'load-path (expand-file-name "../lisp/" user-emacs-directory))
(require 'trf)

(global-set-key (kbd "C-c r i") #'trf-implement-record-changes)
(global-set-key (kbd "C-c r a") #'trf-accept-record-implementation-report)
(global-set-key (kbd "C-c r r") #'trf-reject-record-implementation-report)

;;; init.el ends here
```

Check `package-vc-install`'s signature in Emacs 30 (`C-h f package-vc-install`)
before relying on the REV argument; STOP if it differs.

The key bindings are a proposal; confirm them with the user before committing.

**Step 3: Write `bin/trf`** and `chmod +x bin/trf`:

```sh
#!/bin/sh
here="$(cd "$(dirname "$0")/.." && pwd)"
exec emacs --init-directory="$here/ide" "$@"
```

**Step 4: Verify by hand** — this is the end-to-end run the design calls for:
1. `bin/trf --batch --eval '(message "%s" (featurep (quote trf)))'` → prints `t`
   (first run installs the three packages into `ide/elpa/`).
2. `bin/trf` on a scratch target product made like `trf-test-make-target-product`;
   `C-c r i`; confirm; pick an AI program that speaks ACP; answer the
   brainstorming machine; watch the four machines run and the report open.
   Report what happened, without claiming success for any step not seen.

**Step 5: Commit** "Add the IDE's Emacs configuration and launcher".

---

## Done when

- `$ELK test` passes in the worktree.
- `bin/trf` starts the IDE without reading `~/.emacs.d`.
- The hand run in Task 10 has been reported to the user.
