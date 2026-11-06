;;; test-cmdr.el --- Tests for cmdr -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the cmdr command registry, `cmdr-menu' and `cmdr-dispatch'.
;; `annotated-completing-read' is stubbed so no test reads from the
;; minibuffer.  Nothing here relies on xtd-macro, hud or early-init.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'map)
(require 'seq)

(let ((root (file-name-directory
             (directory-file-name
              (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path root))

(require 'cmdr)

(defvar cmdr-test--calls nil
  "List of (COMMAND PREFIX-ARG BUFFER) records pushed by the test commands.")

(defun cmdr-test--record (name)
  "Record an invocation of test command NAME."
  (push (list name current-prefix-arg (current-buffer)) cmdr-test--calls))

(defun cmdr-test-a ()
  "Test command A."
  (interactive)
  (cmdr-test--record 'a))

(defun cmdr-test-b ()
  "Test command B."
  (interactive)
  (cmdr-test--record 'b))

(defun cmdr-test-c ()
  "Test command C."
  (interactive)
  (cmdr-test--record 'c))

(defun cmdr-test--select (label)
  "Return a stub for `annotated-completing-read' that selects LABEL.
LABEL may be a function called with the candidate table."
  (lambda (table &rest _)
    (if (functionp label)
        (funcall label table)
      label)))

(defun cmdr-test--dispatch-with (label)
  "Run `cmdr-dispatch' with the completion stub selecting LABEL."
  (cl-letf (((symbol-function 'annotated-completing-read)
             (cmdr-test--select label)))
    (call-interactively #'cmdr-dispatch)))

;;;; Struct

(ert-deftest cmdr-test/command-make-minimal ()
  "`make-cmdr-command' creates a valid struct with a non-nil category."
  (let ((cmd (make-cmdr-command)))
    (should (cmdr-command-p cmd))
    (should (null (cmdr-command-command cmd)))
    (should (symbolp (cmdr-command-category cmd)))
    (should (cmdr-command-category cmd))
    (should (null (cmdr-command-transient-key cmd)))
    (should (= 1 (cmdr-command-row cmd)))))

(ert-deftest cmdr-test/command-make-with-all-slots ()
  "All slots are stored and retrieved."
  (let* ((vis (lambda () t))
         (inapt (lambda () nil))
         (cmd (make-cmdr-command :command 'my-cmd :description "do something"
                                 :category 'my-cat :transient-key "mc"
                                 :display-name "mine" :row 2
                                 :visible vis :inapt inapt
                                 :exclude-from-transient t)))
    (should (eq 'my-cmd (cmdr-command-command cmd)))
    (should (equal "do something" (cmdr-command-description cmd)))
    (should (eq 'my-cat (cmdr-command-category cmd)))
    (should (equal "mc" (cmdr-command-transient-key cmd)))
    (should (equal "mine" (cmdr-command-display-name cmd)))
    (should (= 2 (cmdr-command-row cmd)))
    (should (eq vis (cmdr-command-visible cmd)))
    (should (eq inapt (cmdr-command-inapt cmd)))
    (should (cmdr-command-exclude-from-transient cmd))))

(ert-deftest cmdr-test/command-predicate ()
  "`cmdr-command-p' is true only for structs."
  (should (cmdr-command-p (make-cmdr-command)))
  (should-not (cmdr-command-p nil))
  (should-not (cmdr-command-p "string"))
  (should-not (cmdr-command-p '(command . foo))))

(ert-deftest cmdr-test/derive-category-fallback ()
  "An undefined command falls back to the `cmdr' category."
  (should (eq 'cmdr (cmdr--derive-category 'cmdr-test--nonexistent-command-xyz))))

(ert-deftest cmdr-test/derive-category-from-defining-file ()
  "A command defined in a file derives its category from the file name."
  (should (symbolp (cmdr--derive-category 'seq-find)))
  (should (eq 'cmdr (cmdr--derive-category 'cmdr-register-command))))

(ert-deftest cmdr-test/constructor-display-name-defaults ()
  "The display name defaults to the command symbol name."
  (should (equal "seq-find" (cmdr-command-display-name
                             (make-cmdr-command :command 'seq-find))))
  (should (equal "find" (cmdr-command-display-name
                         (make-cmdr-command :command 'seq-find :display-name "find")))))

;;;; Registration

(ert-deftest cmdr-test/register-creates-category ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'tools :command 'my-tool
                           :description "run tool" :transient-key "tt")
    (should (= 1 (length (cdr (assq 'tools cmdr-command-table)))))))

(ert-deftest cmdr-test/register-appends-in-order ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'tools :command 'cmd-a :description "a" :transient-key "ta")
    (cmdr-register-command :category 'tools :command 'cmd-b :description "b" :transient-key "tb")
    (should (equal '(cmd-a cmd-b)
                   (seq-map #'cmdr-command-command (cdr (assq 'tools cmdr-command-table)))))))

(ert-deftest cmdr-test/register-category-order-preserved ()
  "Categories keep insertion order; re-registering does not move a category."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'alpha :command 'cmd-a :description "a" :transient-key "aa")
    (cmdr-register-command :category 'beta :command 'cmd-b :description "b" :transient-key "bb")
    (cmdr-register-command :category 'alpha :command 'cmd-c :description "c" :transient-key "ac")
    (should (equal '(alpha beta) (seq-map #'car cmdr-command-table)))))

(ert-deftest cmdr-test/register-stores-all-fields ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'grp :command 'do-it :description "do it now"
                           :transient-key "di" :display-name "doit" :row 2)
    (let ((cmd (cadr (assq 'grp cmdr-command-table))))
      (should (eq 'do-it (cmdr-command-command cmd)))
      (should (equal "do it now" (cmdr-command-description cmd)))
      (should (eq 'grp (cmdr-command-category cmd)))
      (should (equal "di" (cmdr-command-transient-key cmd)))
      (should (equal "doit" (cmdr-command-display-name cmd)))
      (should (= 2 (cmdr-command-row cmd))))))

(ert-deftest cmdr-test/register-stores-predicates ()
  (let ((cmdr-command-table nil)
        (vis (lambda () t))
        (inapt (lambda () nil)))
    (cmdr-register-command :category 'g :command 'foo :description "f"
                           :transient-key "f" :if vis :ifapt inapt
                           :exclude-from-transient t)
    (let ((cmd (cadr (assq 'g cmdr-command-table))))
      (should (eq vis (cmdr-command-visible cmd)))
      (should (eq inapt (cmdr-command-inapt cmd)))
      (should (cmdr-command-exclude-from-transient cmd)))))

(ert-deftest cmdr-test/register-replacement-keeps-position ()
  "Re-registering (category . command) replaces in place."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'tools :command 'cmd-a :description "a" :transient-key "ta")
    (cmdr-register-command :category 'tools :command 'cmd-b :description "b" :transient-key "tb")
    (cmdr-register-command :category 'tools :command 'cmd-a :description "a2" :transient-key "ta")
    (let ((bucket (cdr (assq 'tools cmdr-command-table))))
      (should (= 2 (length bucket)))
      (should (eq 'cmd-a (cmdr-command-command (nth 0 bucket))))
      (should (equal "a2" (cmdr-command-description (nth 0 bucket))))
      (should (eq 'cmd-b (cmdr-command-command (nth 1 bucket)))))))

(ert-deftest cmdr-test/register-same-command-other-category-is-distinct ()
  "Identity is (category . command), so the same command may appear twice."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'one :command 'cmd-a :description "a"
                           :transient-key "oa" :display-name "a-one")
    (cmdr-register-command :category 'two :command 'cmd-a :description "a"
                           :transient-key "ta" :display-name "a-two")
    (should (= 2 (length (cmdr--flat-entries))))))

(ert-deftest cmdr-test/register-replacement-may-change-row-when-alone ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'g :command 'foo :description "f" :transient-key "f")
    (cmdr-register-command :category 'g :command 'foo :description "f" :transient-key "f" :row 2)
    (should (= 2 (cmdr-command-row (cadr (assq 'g cmdr-command-table)))))))

(ert-deftest cmdr-test/register-rejects-invalid-command ()
  (let ((cmdr-command-table nil))
    (should-error (cmdr-register-command :category 'g :description "f") :type 'user-error)
    (should-error (cmdr-register-command :category 'g :command "foo") :type 'user-error)))

(ert-deftest cmdr-test/register-rejects-invalid-row ()
  (let ((cmdr-command-table nil))
    (should-error (cmdr-register-command :category 'g :command 'foo :row 0) :type 'user-error)
    (should-error (cmdr-register-command :category 'g :command 'foo :row 'x) :type 'user-error)
    (should-error (cmdr-register-command :category 'g :command 'foo :row 99) :type 'user-error)))

(ert-deftest cmdr-test/register-rejects-inconsistent-row ()
  "All entries of one category must share a row."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'g :command 'foo :description "f" :transient-key "f")
    (should-error (cmdr-register-command :category 'g :command 'bar :description "b"
                                         :transient-key "b" :row 2)
                  :type 'user-error)
    (should (= 1 (length (cdr (assq 'g cmdr-command-table)))))))

(ert-deftest cmdr-test/register-same-row-in-different-categories ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'g :command 'foo :description "f" :transient-key "f")
    (cmdr-register-command :category 'h :command 'bar :description "b" :transient-key "b" :row 2)
    (should (= 2 (length cmdr-command-table)))))

;;;; Transient key validation

(ert-deftest cmdr-test/register-rejects-duplicate-key ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'tools :command 'cmd-a :description "a" :transient-key "tt")
    (should-error
     (cmdr-register-command :category 'tools :command 'cmd-b :description "b" :transient-key "tt")
     :type 'user-error)
    (should (= 1 (length (cdr (assq 'tools cmdr-command-table)))))))

(ert-deftest cmdr-test/register-rejects-duplicate-key-across-categories ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'alpha :command 'cmd-a :description "a" :transient-key "xx")
    (should-error
     (cmdr-register-command :category 'beta :command 'cmd-b :description "b" :transient-key "xx")
     :type 'user-error)
    (should-not (assq 'beta cmdr-command-table))))

(ert-deftest cmdr-test/register-normalizes-keys-before-comparing ()
  "\"d v\" and \"dv\" are the same Transient key sequence."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'a :command 'cmd-a :description "a" :transient-key "d v")
    (should-error
     (cmdr-register-command :category 'b :command 'cmd-b :description "b" :transient-key "dv")
     :type 'user-error)))

(ert-deftest cmdr-test/register-rejects-prefix-collisions ()
  "A key that is a prefix of another (either order) is rejected."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'a :command 'cmd-a :description "a" :transient-key "dv")
    (should-error
     (cmdr-register-command :category 'b :command 'cmd-b :description "b" :transient-key "d")
     :type 'user-error))
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'a :command 'cmd-a :description "a" :transient-key "d")
    (should-error
     (cmdr-register-command :category 'b :command 'cmd-b :description "b" :transient-key "dv")
     :type 'user-error)))

(ert-deftest cmdr-test/register-allows-sibling-keys ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'a :command 'cmd-a :description "a" :transient-key "dv")
    (cmdr-register-command :category 'b :command 'cmd-b :description "b" :transient-key "dn")
    (should (= 2 (length (cmdr--flat-entries))))))

(ert-deftest cmdr-test/register-allows-same-key-on-replacement ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'tools :command 'cmd-a :description "v1" :transient-key "ta")
    (cmdr-register-command :category 'tools :command 'cmd-a :description "v2" :transient-key "ta")
    (should (equal "v2" (cmdr-command-description (cadr (assq 'tools cmdr-command-table)))))))

(ert-deftest cmdr-test/register-rejects-reserved-keys ()
  (let ((cmdr-command-table nil))
    (should-error (cmdr-register-command :category 'a :command 'cmd-a :transient-key "C-g")
                  :type 'user-error)
    (should-error (cmdr-register-command :category 'a :command 'cmd-a :transient-key "C-x a")
                  :type 'user-error)))

(ert-deftest cmdr-test/register-rejects-malformed-keys ()
  (let ((cmdr-command-table nil))
    (should-error (cmdr-register-command :category 'a :command 'cmd-a :transient-key "")
                  :type 'user-error)
    (should-error (cmdr-register-command :category 'a :command 'cmd-a :transient-key 'x)
                  :type 'user-error)))

(ert-deftest cmdr-test/completion-only-entries-reserve-no-key ()
  "Entries without a key never conflict, with each other or with keyed ones."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'a :command 'cmd-a :description "a")
    (cmdr-register-command :category 'a :command 'cmd-b :description "b")
    (cmdr-register-command :category 'a :command 'cmd-c :description "c" :transient-key "c")
    (should (= 3 (length (cmdr--flat-entries))))))

(ert-deftest cmdr-test/excluded-entry-with-key-still-reserves-key ()
  "A keyed entry excluded from the menu still reserves its key."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'a :command 'cmd-a :description "a"
                           :transient-key "x" :exclude-from-transient t)
    (should-error
     (cmdr-register-command :category 'b :command 'cmd-b :description "b" :transient-key "x")
     :type 'user-error)))

;;;; Display names

(ert-deftest cmdr-test/register-rejects-duplicate-display-names ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'a :command 'cmd-a :description "a" :display-name "same")
    (should-error
     (cmdr-register-command :category 'b :command 'cmd-b :description "b" :display-name "same")
     :type 'user-error)
    (should-not (assq 'b cmdr-command-table))))

(ert-deftest cmdr-test/register-rejects-default-name-collision ()
  "The same command in two categories collides on its default display name."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'a :command 'cmd-a :description "a")
    (should-error
     (cmdr-register-command :category 'b :command 'cmd-a :description "a again")
     :type 'user-error)))

(ert-deftest cmdr-test/register-allows-display-name-on-replacement ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'a :command 'cmd-a :description "v1" :display-name "same")
    (cmdr-register-command :category 'a :command 'cmd-a :description "v2" :display-name "same")
    (should (= 1 (length (cmdr--flat-entries))))))

;;;; Reset and load

(ert-deftest cmdr-test/reset-command-table-clears-entries ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'x :command 'foo :description "f" :transient-key "f")
    (cmdr-reset-command-table)
    (should (null cmdr-command-table))))

(ert-deftest cmdr-test/reset-on-empty-table ()
  (let ((cmdr-command-table nil))
    (cmdr-reset-command-table)
    (should (null cmdr-command-table))))

(ert-deftest cmdr-test/reloading-library-preserves-registrations ()
  "Loading cmdr.el again never resets the registry."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'x :command 'foo :description "f" :transient-key "f")
    (load (locate-library "cmdr.el") nil t)
    (should (= 1 (length (cmdr--flat-entries))))))

(ert-deftest cmdr-test/library-has-no-personal-registrations-or-xtd ()
  "The source registers nothing at top level and requires no xtd/hud/early-init."
  (with-temp-buffer
    (insert-file-contents (locate-library "cmdr.el"))
    (should-not (re-search-forward "^(cmdr-register-command" nil t))
    (goto-char (point-min))
    (should-not (re-search-forward "(require '\\(xtd\\|hud\\|early-init\\)" nil t))
    (goto-char (point-min))
    (should-not (re-search-forward "(require '[^)]*nil\\s-+\\(t\\|'noerror\\)" nil t))))

;;;; Flat entries

(ert-deftest cmdr-test/flat-entries ()
  (let ((cmdr-command-table nil))
    (should (null (cmdr--flat-entries)))
    (cmdr-register-command :category 'alpha :command 'cmd-a :description "a" :transient-key "aa")
    (cmdr-register-command :category 'beta :command 'cmd-b :description "b" :transient-key "bb")
    (cmdr-register-command :category 'beta :command 'cmd-c :description "c" :transient-key "bc")
    (let ((entries (cmdr--flat-entries)))
      (should (equal '(alpha beta beta) (seq-map #'car entries)))
      (should (seq-every-p (lambda (pair)
                             (and (cmdr-command-p (cdr pair))
                                  (eq (car pair) (cmdr-command-category (cdr pair)))))
                           entries)))))

;;;; Predicates

(ert-deftest cmdr-test/visibility-and-availability ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'g :command 'plain :description "p")
    (cmdr-register-command :category 'g :command 'hidden :description "h" :if (lambda () nil))
    (cmdr-register-command :category 'g :command 'inapt :description "i" :ifapt (lambda () t))
    (let ((pairs (seq-map #'cdr (cmdr--flat-entries))))
      (should (cmdr--entry-visible-p (nth 0 pairs)))
      (should-not (cmdr--entry-visible-p (nth 1 pairs)))
      (should (cmdr--entry-visible-p (nth 2 pairs)))
      (should (cmdr--entry-available-p (nth 0 pairs)))
      (should-not (cmdr--entry-available-p (nth 1 pairs)))
      (should-not (cmdr--entry-available-p (nth 2 pairs))))))

(ert-deftest cmdr-test/predicates-evaluate-in-origin-buffer ()
  (let ((cmdr-command-table nil)
        (origin (generate-new-buffer " *cmdr-origin*")))
    (unwind-protect
        (progn
          (cmdr-register-command :category 'g :command 'foo :description "f"
                                 :if (lambda () (eq (current-buffer) origin))
                                 :ifapt (lambda () (not (eq (current-buffer) origin))))
          (let ((entry (cdr (car (cmdr--flat-entries)))))
            (with-temp-buffer
              (should (cmdr--entry-visible-p entry origin))
              (should (cmdr--entry-available-p entry origin))
              (should-not (cmdr--entry-visible-p entry)))))
      (kill-buffer origin))))

(ert-deftest cmdr-test/predicate-errors-keep-context ()
  "A failing predicate is re-signaled naming the command, category and cause."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'g :command 'foo :description "f"
                           :if (lambda () (error "boom-from-predicate")))
    (let ((err (should-error (cmdr--entry-visible-p (cdr (car (cmdr--flat-entries)))))))
      (should (string-match-p "foo" (error-message-string err)))
      (should (string-match-p "boom-from-predicate" (error-message-string err))))))

(ert-deftest cmdr-test/predicate-quit-is-not-swallowed ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'g :command 'foo :description "f"
                           :if (lambda () (signal 'quit nil)))
    (should (eq 'quit (condition-case nil
                          (cmdr--entry-visible-p (cdr (car (cmdr--flat-entries))))
                        (quit 'quit))))))

;;;; Transient column builder

(ert-deftest cmdr-test/bucket-to-column-omits-excluded ()
  (let* ((cmdr-command-table nil)
         (included (make-cmdr-command :command 'cmd-a :description "A" :category 'g
                                      :transient-key "a"))
         (excluded (make-cmdr-command :command 'cmd-b :description "B" :category 'g
                                      :transient-key "b" :exclude-from-transient t))
         (col (cmdr--bucket-to-column (cons 'g (list included excluded)) (current-buffer))))
    (should (equal "g" (aref col 0)))
    (should (= 2 (length col)))
    (should (equal "a" (nth 0 (aref col 1))))))

(ert-deftest cmdr-test/bucket-to-column-omits-keyless ()
  (let* ((entry (make-cmdr-command :command 'cmd-a :description "A" :category 'g))
         (col (cmdr--bucket-to-column (cons 'g (list entry)) (current-buffer))))
    (should (= 1 (length col)))))

(ert-deftest cmdr-test/bucket-to-column-suffix-shape ()
  "Suffixes are (KEY DESC COMMAND . PLIST) with no :if/:inapt-if unless set."
  (let* ((entry (make-cmdr-command :command 'foo :description "Foo" :category 'g
                                   :transient-key "f"))
         (suffix (aref (cmdr--bucket-to-column (cons 'g (list entry)) (current-buffer)) 1)))
    (should (equal "f" (nth 0 suffix)))
    (should (equal "Foo" (nth 1 suffix)))
    (should (eq 'foo (nth 2 suffix)))
    (should-not (plist-member (nthcdr 3 suffix) :if))
    (should-not (plist-member (nthcdr 3 suffix) :inapt-if))))

(ert-deftest cmdr-test/bucket-to-column-description-falls-back-to-name ()
  (let* ((entry (make-cmdr-command :command 'foo :category 'g :transient-key "f"))
         (suffix (aref (cmdr--bucket-to-column (cons 'g (list entry)) (current-buffer)) 1)))
    (should (equal "foo" (nth 1 suffix)))))

(ert-deftest cmdr-test/bucket-to-column-predicates-run-in-origin ()
  (let* ((origin (generate-new-buffer " *cmdr-origin*"))
         (entry (make-cmdr-command :command 'foo :description "Foo" :category 'g
                                   :transient-key "f"
                                   :visible (lambda () (eq (current-buffer) origin))
                                   :inapt (lambda () (eq (current-buffer) origin))))
         (suffix (aref (cmdr--bucket-to-column (cons 'g (list entry)) origin) 1))
         (plist (nthcdr 3 suffix)))
    (unwind-protect
        (with-temp-buffer
          (should (funcall (plist-get plist :if)))
          (should (funcall (plist-get plist :inapt-if))))
      (kill-buffer origin))))

(ert-deftest cmdr-test/bucket-advice-rechecks-before-invoking ()
  "The suffix advice refuses an entry that became unavailable after setup."
  (let* ((state nil)
         (called nil)
         (entry (make-cmdr-command :command 'cmdr-test-a :description "Foo" :category 'g
                                   :transient-key "f" :inapt (lambda () state)))
         (suffix (aref (cmdr--bucket-to-column (cons 'g (list entry)) (current-buffer)) 1))
         (advice (plist-get (nthcdr 3 suffix) :advice*)))
    (funcall advice (lambda (&rest _) (setq called t)))
    (should called)
    (setq called nil
          state t)
    (should-error (funcall advice (lambda (&rest _) (setq called t))) :type 'user-error)
    (should-not called)))

;;;; Menu

(ert-deftest cmdr-test/menu-empty-registry-message ()
  (let ((cmdr-command-table nil))
    (let ((err (should-error (cmdr-menu) :type 'user-error)))
      (should (string-match-p "no commands" (error-message-string err))))))

(ert-deftest cmdr-test/menu-rows-built-at-invocation-time ()
  "Late registrations appear without redefining the prefix."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'g :command 'cmdr-test-a :description "a"
                           :transient-key "a")
    (should (= 1 (length (cmdr--setup-row-children 1))))
    (should (= 0 (length (cmdr--setup-row-children 2))))
    (cmdr-register-command :category 'h :command 'cmdr-test-b :description "b"
                           :transient-key "b" :row 2)
    (should (= 1 (length (cmdr--setup-row-children 2))))))

(ert-deftest cmdr-test/menu-children-parse-as-suffixes ()
  "Transient accepts the generated columns, predicates and advice included."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'g :command 'cmdr-test-a :description "a"
                           :transient-key "a" :if (lambda () t) :ifapt (lambda () nil))
    (cmdr-register-command :category 'g :command 'cmdr-test-b :description "b"
                           :transient-key "b" :exclude-from-transient t)
    (let ((children (cmdr--setup-row-children 1)))
      (should (= 1 (length children)))
      (should (eq 'transient-column (aref (car children) 0)))
      (should (= 1 (length (aref (car children) 2)))))))

;;;; Dispatch

(ert-deftest cmdr-test/dispatch-invokes-selected-command ()
  (let ((cmdr-command-table nil)
        (cmdr-test--calls nil))
    (cmdr-register-command :category 'g :command 'cmdr-test-a :description "a")
    (cmdr-register-command :category 'g :command 'cmdr-test-b :description "b")
    (cmdr-test--dispatch-with "cmdr-test-b")
    (should (equal '(b) (seq-map #'car cmdr-test--calls)))))

(ert-deftest cmdr-test/dispatch-candidates-group-and-annotation ()
  "Candidates are display names annotated with descriptions."
  (let ((cmdr-command-table nil)
        (table nil))
    (cmdr-register-command :category 'g :command 'cmdr-test-a :description "the a")
    (cmdr-register-command :category 'g :command 'cmdr-test-b :description "the b"
                           :display-name "bee" :exclude-from-transient t)
    (cmdr-test--dispatch-with (lambda (tbl) (setq table tbl) nil))
    (should (equal '("cmdr-test-a" "bee") (seq-map #'car table)))
    (should (string-match-p "the a" (cdr (assoc "cmdr-test-a" table))))))

(ert-deftest cmdr-test/dispatch-passes-group-function ()
  (let ((cmdr-command-table nil)
        (group nil))
    (cmdr-register-command :category 'mygroup :command 'cmdr-test-a :description "a")
    (cl-letf (((symbol-function 'annotated-completing-read)
               (lambda (_table &rest args)
                 (setq group (funcall (plist-get args :group-name) "cmdr-test-a"))
                 nil)))
      (call-interactively #'cmdr-dispatch))
    (should (equal "mygroup" group))))

(ert-deftest cmdr-test/dispatch-hides-invisible-entries ()
  (let ((cmdr-command-table nil)
        (table nil))
    (cmdr-register-command :category 'g :command 'cmdr-test-a :description "a")
    (cmdr-register-command :category 'g :command 'cmdr-test-b :description "b"
                           :if (lambda () nil))
    (cmdr-test--dispatch-with (lambda (tbl) (setq table tbl) nil))
    (should (equal '("cmdr-test-a") (seq-map #'car table)))))

(ert-deftest cmdr-test/dispatch-annotates-unavailable-and-refuses ()
  (let ((cmdr-command-table nil)
        (cmdr-test--calls nil)
        (table nil))
    (cmdr-register-command :category 'g :command 'cmdr-test-a :description "a"
                           :ifapt (lambda () t))
    (cmdr-test--dispatch-with (lambda (tbl) (setq table tbl) nil))
    (should (string-match-p "unavailable" (cdr (assoc "cmdr-test-a" table))))
    (should-error (cmdr-test--dispatch-with "cmdr-test-a") :type 'user-error)
    (should-not cmdr-test--calls)))

(ert-deftest cmdr-test/dispatch-rechecks-after-selection ()
  "State changing while completion is open is honored at invocation."
  (let* ((cmdr-command-table nil)
         (cmdr-test--calls nil)
         (hidden nil)
         (inapt nil))
    (cmdr-register-command :category 'g :command 'cmdr-test-a :description "a"
                           :if (lambda () (not hidden)))
    (cmdr-register-command :category 'g :command 'cmdr-test-b :description "b"
                           :ifapt (lambda () inapt))
    (should-error (cmdr-test--dispatch-with
                   (lambda (_) (setq hidden t) "cmdr-test-a"))
                  :type 'user-error)
    (should-error (cmdr-test--dispatch-with
                   (lambda (_) (setq inapt t) "cmdr-test-b"))
                  :type 'user-error)
    (should-not cmdr-test--calls)))

(ert-deftest cmdr-test/dispatch-empty-registry-message ()
  (let ((cmdr-command-table nil))
    (let ((err (should-error (cmdr-test--dispatch-with "x") :type 'user-error)))
      (should (string-match-p "no commands" (error-message-string err))))))

(ert-deftest cmdr-test/dispatch-sees-late-registrations ()
  (let ((cmdr-command-table nil)
        (cmdr-test--calls nil))
    (cmdr-register-command :category 'g :command 'cmdr-test-a :description "a")
    (cmdr-test--dispatch-with nil)
    (cmdr-register-command :category 'g :command 'cmdr-test-b :description "b")
    (cmdr-test--dispatch-with "cmdr-test-b")
    (should (equal '(b) (seq-map #'car cmdr-test--calls)))))

(ert-deftest cmdr-test/dispatch-missing-command-reports-registration ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'ghosts :command 'cmdr-test--not-defined
                           :description "ghost")
    (let ((err (should-error (cmdr-test--dispatch-with "cmdr-test--not-defined")
                             :type 'user-error)))
      (should (string-match-p "cmdr-test--not-defined" (error-message-string err)))
      (should (string-match-p "ghosts" (error-message-string err))))))

(ert-deftest cmdr-test/dispatch-non-command-is-reported ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'g :command 'car :description "not interactive")
    (should-error (cmdr-test--dispatch-with "car") :type 'user-error)))

(ert-deftest cmdr-test/dispatch-preserves-prefix-argument ()
  (let ((cmdr-command-table nil)
        (cmdr-test--calls nil))
    (cmdr-register-command :category 'g :command 'cmdr-test-a :description "a")
    (let ((current-prefix-arg '(4)))
      (cmdr-test--dispatch-with
       (lambda (_)
         (setq current-prefix-arg nil)
         "cmdr-test-a")))
    (should (equal '(4) (cadr (car cmdr-test--calls))))))

(ert-deftest cmdr-test/dispatch-runs-in-origin-buffer ()
  (let ((cmdr-command-table nil)
        (cmdr-test--calls nil)
        (origin (current-buffer)))
    (cmdr-register-command :category 'g :command 'cmdr-test-a :description "a")
    (cmdr-test--dispatch-with
     (lambda (_)
       (set-buffer (get-buffer-create " *cmdr-other*"))
       "cmdr-test-a"))
    (should (eq origin (nth 2 (car cmdr-test--calls))))
    (kill-buffer " *cmdr-other*")))

(ert-deftest cmdr-test/dispatch-cancel-invokes-nothing ()
  (let ((cmdr-command-table nil)
        (cmdr-test--calls nil))
    (cmdr-register-command :category 'g :command 'cmdr-test-a :description "a")
    (cmdr-test--dispatch-with nil)
    (cmdr-test--dispatch-with "")
    (should-not cmdr-test--calls)))

(ert-deftest cmdr-test/dispatch-quit-propagates ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'g :command 'cmdr-test-a :description "a")
    (should (eq 'quit (condition-case nil
                          (cmdr-test--dispatch-with (lambda (_) (signal 'quit nil)))
                        (quit 'quit))))))

(ert-deftest cmdr-test/dispatch-predicate-error-keeps-context ()
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'g :command 'cmdr-test-a :description "a"
                           :if (lambda () (error "predicate-exploded")))
    (let ((err (should-error (cmdr-test--dispatch-with "cmdr-test-a"))))
      (should (string-match-p "predicate-exploded" (error-message-string err)))
      (should (string-match-p "cmdr-test-a" (error-message-string err))))))

;;;; Transient suffix helper

(transient-define-prefix cmdr-test--prefix ()
  "Prefix used to test `cmdr-transient-insert-suffix-once'."
  [("a" "alpha" cmdr-test-a)])

(ert-deftest cmdr-test/insert-suffix-once-does-not-duplicate ()
  (let ((suffix '("x" "extra" cmdr-test-b)))
    (cmdr-transient-insert-suffix-once 'cmdr-test--prefix '(-1 0) suffix)
    (should (transient-get-suffix 'cmdr-test--prefix "x"))
    (let ((layout (format "%S" (get 'cmdr-test--prefix 'transient--layout))))
      (cmdr-transient-insert-suffix-once 'cmdr-test--prefix '(-1 0) suffix)
      (should (equal layout (format "%S" (get 'cmdr-test--prefix 'transient--layout)))))))

(ert-deftest cmdr-test/insert-suffix-once-noop-without-layout ()
  (should-not (cmdr-transient-insert-suffix-once 'cmdr-test--undefined-prefix '(-1 0)
                                                 '("x" "extra" cmdr-test-b))))

(provide 'test-cmdr)
;;; test-cmdr.el ends here
