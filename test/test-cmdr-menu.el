;;; test-cmdr-menu.el --- Headless Transient tests for cmdr-menu -*- lexical-binding: t; -*-

;;; Commentary:
;; Builds the `cmdr-menu' layout from a populated registry and drives
;; Transient's own non-interactive initialization and menu rendering
;; (`transient--init-objects', `transient--insert-menu') in a scratch
;; buffer.  The popup window, keyboard dispatch and redisplay timers cannot
;; run in batch mode and are not covered here.  These tests use Transient
;; internals, so they are tied to the Transient versions CI installs.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)

(let ((root (file-name-directory
             (directory-file-name
              (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path root))

(require 'cmdr)

(defvar cmdr-menu-test--calls nil
  "Commands run by the menu test commands.")

(defun cmdr-menu-test-one ()
  "Menu test command one."
  (interactive)
  (push 'one cmdr-menu-test--calls))

(defun cmdr-menu-test-two ()
  "Menu test command two."
  (interactive)
  (push 'two cmdr-menu-test--calls))

(defun cmdr-menu-test-three ()
  "Menu test command three."
  (interactive)
  (push 'three cmdr-menu-test--calls))

(defun cmdr-menu-test-four ()
  "Menu test command four."
  (interactive)
  (push 'four cmdr-menu-test--calls))

(defmacro cmdr-menu-test--with-menu (&rest body)
  "Initialize `cmdr-menu' objects in the current buffer, then run BODY.
`transient--suffixes', `transient--layout' and `transient--prefix' are
bound for the duration so nothing leaks into the session."
  (declare (indent 0))
  `(let ((transient--prefix nil)
         (transient--layout nil)
         (transient--suffixes nil)
         (transient--transient-map nil)
         (transient--predicate-map nil)
         (transient--redisplay-map nil))
     (transient--init-objects 'cmdr-menu nil nil)
     (transient--init-keymaps)
     ,@body))

(defun cmdr-menu-test--rendered ()
  "Return the text Transient renders for the initialized `cmdr-menu'."
  (with-temp-buffer
    (transient--insert-menu t)
    (buffer-string)))

(defun cmdr-menu-test--suffix (key)
  "Return the initialized suffix object bound to KEY."
  (seq-find (lambda (suffix) (equal key (oref suffix key))) transient--suffixes))

(defun cmdr-menu-test--populate ()
  "Fill the registry with a layout spanning rows 1 and 3, leaving row 2 empty."
  (setq cmdr-command-table nil)
  (cmdr-register-command :category 'alpha :command 'cmdr-menu-test-one
                         :description "first" :transient-key "a")
  (cmdr-register-command :category 'alpha :command 'cmdr-menu-test-two
                         :description "second" :transient-key "b"
                         :ifapt (lambda () t))
  (cmdr-register-command :category 'alpha :command 'cmdr-menu-test-three
                         :description "hidden" :transient-key "h"
                         :if (lambda () nil))
  (cmdr-register-command :category 'alpha :command 'ignore
                         :description "completion only")
  (cmdr-register-command :category 'beta :command 'cmdr-menu-test-four
                         :description "fourth" :transient-key "d" :row 3))

(ert-deftest cmdr-menu-test/layout-parses-and-renders ()
  "Transient initializes and renders the populated menu."
  (let ((cmdr-command-table nil))
    (cmdr-menu-test--populate)
    (with-temp-buffer
      (cmdr-menu-test--with-menu
        (let ((text (cmdr-menu-test--rendered))
              (keys (seq-map (lambda (suffix) (oref suffix key)) transient--suffixes)))
          (should (member "a" keys))
          (should (member "b" keys))
          (should (member "d" keys))
          (should (string-match-p "alpha" text))
          (should (string-match-p "beta" text))
          (should (string-match-p "first" text))
          (should (string-match-p "second" text))
          (should (string-match-p "fourth" text))
          (should-not (string-match-p "hidden" text))
          (should-not (string-match-p "completion only" text))
          (should (< (string-match "alpha" text) (string-match "beta" text))))))))

(ert-deftest cmdr-menu-test/empty-rows-add-no-groups ()
  "Rows without categories contribute no columns, and an empty registry still renders."
  (let ((cmdr-command-table nil))
    (cmdr-menu-test--populate)
    (should (= 1 (length (cmdr--setup-row-children 1))))
    (should (= 0 (length (cmdr--setup-row-children 2))))
    (should (= 1 (length (cmdr--setup-row-children 3))))
    (setq cmdr-command-table nil)
    (with-temp-buffer
      (cmdr-menu-test--with-menu
        (should-not (string-match-p "alpha\\|beta" (cmdr-menu-test--rendered)))))))

(ert-deftest cmdr-menu-test/category-with-only-hidden-keys-is-omitted ()
  "A category whose entries are all excluded or keyless produces no column."
  (let ((cmdr-command-table nil))
    (cmdr-register-command :category 'quiet :command 'cmdr-menu-test-one :transient-key "q"
                           :exclude-from-transient t)
    (cmdr-register-command :category 'quiet :command 'ignore)
    (should (= 0 (length (cmdr--setup-row-children 1))))))

(ert-deftest cmdr-menu-test/predicates-use-the-invoking-buffer ()
  "`:if' sees the buffer that was current at setup, not the render buffer."
  (let ((cmdr-command-table nil)
        (origin (generate-new-buffer " *cmdr-menu-origin*")))
    (unwind-protect
        (progn
          (cmdr-register-command :category 'alpha :command 'cmdr-menu-test-one
                                 :description "only-in-origin" :transient-key "a"
                                 :if (lambda () (eq (current-buffer) origin)))
          (with-current-buffer origin
            (cmdr-menu-test--with-menu
              (should (string-match-p "only-in-origin" (cmdr-menu-test--rendered))))))
      (kill-buffer origin))))

(ert-deftest cmdr-menu-test/inapt-suffix-is-marked-inapt ()
  "Transient evaluates `:inapt-if' through the registry predicate."
  (let ((cmdr-command-table nil))
    (cmdr-menu-test--populate)
    (with-temp-buffer
      (cmdr-menu-test--with-menu
        (should (transient--inapt-suffix-p (cmdr-menu-test--suffix "b")))
        (should-not (transient--inapt-suffix-p (cmdr-menu-test--suffix "a")))))))

(ert-deftest cmdr-menu-test/advice-rechecks-before-running ()
  "The suffix `:advice*' refuses a command that became unavailable after display."
  (let ((cmdr-command-table nil)
        (unavailable nil)
        (hidden nil)
        (ran nil))
    (cmdr-register-command :category 'alpha :command 'cmdr-menu-test-one :transient-key "a"
                           :ifapt (lambda () unavailable) :if (lambda () (not hidden)))
    (with-temp-buffer
      (cmdr-menu-test--with-menu
        (let ((advice (oref (cmdr-menu-test--suffix "a") advice*))
              (spy (lambda (&rest _) (setq ran t))))
          (should (functionp advice))
          (funcall advice spy)
          (should ran)
          (setq ran nil
                unavailable t)
          (should-error (funcall advice spy) :type 'user-error)
          (setq unavailable nil
                hidden t)
          (should-error (funcall advice spy) :type 'user-error)
          (should-not ran))))))

(ert-deftest cmdr-menu-test/wrapped-command-refuses-inapt-suffix ()
  "Through Transient's own command wrapping, an inapt suffix signals and never runs."
  (let ((cmdr-command-table nil)
        (cmdr-menu-test--calls nil))
    (cmdr-menu-test--populate)
    (with-temp-buffer
      (cmdr-menu-test--with-menu
        (let ((this-command 'cmdr-menu-test-two))
          (transient--wrap-command)
          (should-error (call-interactively 'cmdr-menu-test-two) :type 'user-error))
        (should-not cmdr-menu-test--calls)
        (let ((this-command 'cmdr-menu-test-one))
          (transient--wrap-command)
          (call-interactively 'cmdr-menu-test-one))
        (should (equal '(one) cmdr-menu-test--calls))))))

(provide 'test-cmdr-menu)
;;; test-cmdr-menu.el ends here
