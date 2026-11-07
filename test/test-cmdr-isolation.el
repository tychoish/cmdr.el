;;; test-cmdr-isolation.el --- Load and mode isolation tests for cmdr -*- lexical-binding: t; -*-

;;; Commentary:
;; Each test starts a fresh `emacs --batch -Q' child (see isolation-child.el),
;; so state loaded by other tests cannot mask or cause a difference.  The
;; child compares global keymaps, minibuffer maps, hooks, advice,
;; `display-buffer-alist' and related variables before and after loading
;; every cmdr library and toggling `cmdr-mode'.

;;; Code:

(require 'ert)
(require 'seq)

(defconst cmdr-isolation-test--dir
  (file-name-directory (or load-file-name buffer-file-name))
  "Directory holding the isolation child script.")

(defun cmdr-isolation-test--run-child (function)
  "Run FUNCTION from isolation-child.el in a fresh Emacs.
Return (EXIT-CODE . OUTPUT)."
  (let ((emacs (expand-file-name invocation-name invocation-directory)))
    (with-temp-buffer
      (let ((code (apply #'call-process emacs nil t nil
                         "--batch" "-Q"
                         (append (seq-mapcat (lambda (dir) (list "-L" dir))
                                             (seq-filter #'file-directory-p
                                                         (seq-remove #'null load-path)))
                                 (list "-L" cmdr-isolation-test--dir
                                       "-l" (expand-file-name "isolation-child.el"
                                                              cmdr-isolation-test--dir)
                                       "-f" (symbol-name function))))))
        (cons code (buffer-string))))))

(ert-deftest cmdr-isolation-test/load-and-mode-change-nothing-else ()
  "Loading the libraries and toggling `cmdr-mode' leave global state alone."
  (let ((result (cmdr-isolation-test--run-child 'cmdr-isolation-child-run)))
    (should (equal 0 (car result)))
    (should (string-match-p "RESULT: ok" (cdr result)))
    (should (string-match-p "load: unchanged" (cdr result)))
    (should (string-match-p "enable: unchanged" (cdr result)))
    (should (string-match-p "disable: unchanged" (cdr result)))))

(ert-deftest cmdr-isolation-test/which-key-reads-prefix-label ()
  "which-key, loaded alone, shows the `cmdr-keys-prefix' label."
  (when (locate-library "which-key")
    (let ((result (cmdr-isolation-test--run-child 'cmdr-isolation-child-which-key)))
      (should (equal 0 (car result)))
      (should (string-match-p "\\(\"a\" \\. \"[^\"]*Apps\"\\)" (cdr result)))
      (should (string-match-p "\"f\" \\. \"find-file\"" (cdr result))))))

(ert-deftest cmdr-isolation-test/generated-autoloads-load-cleanly ()
  "Autoloads generated as a package install does must load without error."
  (let ((result (cmdr-isolation-test--run-child 'cmdr-isolation-child-autoloads)))
    (should (string-match-p "load: ok" (cdr result)))
    (should-not (string-match-p "missing autoload" (cdr result)))
    (should (string-match-p "RESULT: ok" (cdr result)))
    (should (equal 0 (car result)))))

(provide 'test-cmdr-isolation)
;;; test-cmdr-isolation.el ends here
