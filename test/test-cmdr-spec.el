;;; test-cmdr-spec.el --- Tests for spec discovery -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'cmdr-spec)
(require 'cmdr)
(require 'cmdr-defaults)
(require 'cmdr-ecosystem)
(require 'cmdr-integrations)

(defvar features)

(defun cmdr-spec-test--bound-p (key)
  "Return non-nil when KEY is bound to a command in `cmdr-mode-map'."
  (commandp (keymap-lookup cmdr-mode-map key)))

(defmacro cmdr-spec-test--with-table (&rest body)
  "Run BODY with an empty spec table, map, pending list and skip log."
  (declare (indent 0))
  `(let ((cmdr-spec-table nil)
         (cmdr-spec-skipped nil)
         (cmdr-spec--pending nil)
         (cmdr-spec-libraries nil)
         (cmdr-disabled-specs nil)
         (cmdr-opt-in-specs nil)
         (cmdr-mode-map (make-sparse-keymap)))
     ,@body))

(ert-deftest cmdr-spec-test/discover-skips-unbound-probe ()
  (cmdr-spec-test--with-table
    (cmdr-register-spec :name present :probe car :bindings (("C-c z a" . ignore)))
    (cmdr-register-spec :name absent :probe cmdr-spec-test-no-such-fn :bindings (("C-c z b" . ignore)))
    (cmdr-spec-discover)
    (should (eq (keymap-lookup cmdr-mode-map "C-c z a") #'ignore))
    (should-not (cmdr-spec-test--bound-p "C-c z b"))
    (should (equal (seq-map #'car cmdr-spec-skipped) '(absent)))
    (should (string-match-p "cmdr-spec-test-no-such-fn" (cdr (car cmdr-spec-skipped))))))

(ert-deftest cmdr-spec-test/opt-in-and-disabled ()
  (cmdr-spec-test--with-table
    (cmdr-register-spec :name opt :opt-in t :bindings (("C-c z o" . ignore)))
    (cmdr-register-spec :name off :bindings (("C-c z f" . ignore)))
    (let ((cmdr-disabled-specs '(off)))
      (cmdr-spec-discover))
    (should-not (cmdr-spec-test--bound-p "C-c z o"))
    (should-not (cmdr-spec-test--bound-p "C-c z f"))
    (let ((cmdr-opt-in-specs '(opt)))
      (cmdr-spec-discover))
    (should (eq (keymap-lookup cmdr-mode-map "C-c z o") #'ignore))))

(ert-deftest cmdr-spec-test/redefine-replaces-in-place ()
  (cmdr-spec-test--with-table
    (cmdr-register-spec :name a :doc "one")
    (cmdr-register-spec
  :name b)
    (cmdr-register-spec :name a :doc "two")
    (should (equal (seq-map #'cmdr-spec-name cmdr-spec-table) '(a b)))
    (should (equal (cmdr-spec-doc (cmdr-spec-get 'a)) "two"))))

(ert-deftest cmdr-spec-test/commands-wait-for-cmdr ()
  (cmdr-spec-test--with-table
    (let ((registered nil)
          (features (remq 'cmdr features)))
      (cl-letf (((symbol-function 'cmdr-register-command)
                 (lambda (&rest args) (push args registered))))
        (cmdr-register-spec
          :name s
          :commands ((cmdr-spec-command :category x
                                        :command ignore
                                        :transient-key "xi")))
        (cmdr-spec-install 's)
        (should-not registered)
        (should cmdr-spec--pending)
        (cmdr-spec-flush-commands)
        (should (equal (plist-get (car registered) :category) 'x))
        (should (equal (plist-get (car registered) :command) 'ignore))
        (should (equal (plist-get (car registered) :transient-key) "xi"))
        (should (equal (plist-get (car registered) :row) 1))
        (should-not cmdr-spec--pending)))))

(ert-deftest cmdr-spec-test/command-predicates-are-functions ()
  (let ((command (cmdr-spec-command :command ignore :if always :ifapt (lambda () nil))))
    (should (eq (cmdr-spec-command-visible command) #'always))
    (should (functionp (cmdr-spec-command-inapt command)))))

(ert-deftest cmdr-spec-test/validate-reports-conflicts ()
  (cmdr-spec-test--with-table
    (cmdr-register-spec :name a
                        :prefixes (("C-c z" . "one"))
                        :bindings (("C-c z a" . ignore) ("C-c y" . ignore)))
    (cmdr-register-spec :name b
                        :prefixes (("C-c z" . "two") ("C-c y" . "y"))
                        :bindings (("C-c z a" . identity) ("C-c y x" . ignore)))
    (let ((conflicts (cmdr-spec-validate)))
      (should (seq-some (lambda (c) (string-match-p "prefix C-c z labelled" c)) conflicts))
      (should (seq-some (lambda (c) (string-match-p "key C-c z a bound to ignore" c)) conflicts))
      (should (seq-some (lambda (c) (string-match-p "key C-c y is a command" c)) conflicts)))))

(ert-deftest cmdr-spec-test/validate-accepts-consistent-specs ()
  (cmdr-spec-test--with-table
    (cmdr-register-spec :name a :prefixes (("C-c z" . "z")) :bindings (("C-c z a" . ignore)))
    (cmdr-register-spec :name b :prefixes (("C-c z" . "z")) :bindings (("C-c z a" . ignore) ("C-c z b" . identity)))
    (should-not (cmdr-spec-validate))))

(ert-deftest cmdr-spec-test/builtin-specs-have-no-conflicts ()
  (should-not (cmdr-spec-validate)))

(ert-deftest cmdr-spec-test/failed-install-does-not-stop-discovery ()
  (cmdr-spec-test--with-table
    (cl-letf (((symbol-function 'display-warning) #'ignore))
      (cmdr-register-spec :name first :bindings (("C-c z a" . ignore)))
      (cmdr-register-spec :name broken :bindings (("C-c z a b" . ignore)))
      (cmdr-register-spec :name last :bindings (("C-c z c" . forward-char)))
      (cmdr-spec-discover)
      (should (cmdr-spec-test--bound-p "C-c z c"))
      (should (string-match-p "install failed" (cdr (assq 'broken cmdr-spec-skipped)))))))

(ert-deftest cmdr-spec-test/discover-does-not-require-probe-packages ()
  (cmdr-spec-test--with-table
    (cmdr-register-spec :name mail :probe mu4e :bindings (("C-c z m" . mu4e)))
    (let ((before (featurep 'mu4e)))
      (cmdr-spec-discover)
      (should (eq before (featurep 'mu4e))))))

(ert-deftest cmdr-spec-test/all-specs-install-without-conflicts ()
  (let ((cmdr-mode-map (make-sparse-keymap))
        (cmdr-command-table nil)
        (cmdr-spec--pending nil))
    (seq-do #'cmdr-spec-install (seq-map #'cmdr-spec-name cmdr-spec-table))
    (should-not cmdr-spec--pending)
    (should cmdr-command-table)
    (should (eq (keymap-lookup cmdr-mode-map "C-c r s n") #'agent-shell-new-shell))
    (should (eq (keymap-lookup cmdr-mode-map "C-c m m") #'mu4e))))

(ert-deftest cmdr-spec-test/elpaish-spec-registers-upgrade-command ()
  (let ((spec (cmdr-spec-get 'elpaish)))
    (should spec)
    (should (eq (cmdr-spec-probe spec) 'elpaish-install-upgrade-packages))
    (let ((cmd (car (cmdr-spec-commands spec))))
      (should (eq (cmdr-spec-command-command cmd) 'elpaish-install-upgrade-packages))
      (should (eq (cmdr-spec-command-category cmd) 'elpaish))
      (should (cmdr-spec-command-exclude-from-transient cmd)))))

(ert-deftest cmdr-spec-test/bindings-have-no-capital-letters ()
  (seq-do (lambda (spec)
            (seq-do (pcase-lambda (`(,key . ,_))
                      (should-not (let ((case-fold-search nil)) (string-match-p "\\`[A-Z]\\'" (car (last (split-string key " ")))))))
                    (cmdr-spec-bindings spec)))
          cmdr-spec-table))

(ert-deftest cmdr-spec-test/name-is-required ()
  (should-error (macroexpand '(cmdr-register-spec :bindings (("C-c z n" . ignore)))) :type 'user-error)
  (should-error (macroexpand '(cmdr-register-spec :name "str")) :type 'user-error))

(ert-deftest cmdr-spec-test/string-probe-uses-locate-library ()
  (cmdr-spec-test--with-table
    (cmdr-register-spec :name here :probe "ert" :bindings (("C-c z h" . ignore)))
    (cmdr-register-spec :name gone :probe "cmdr-no-such-library" :bindings (("C-c z g" . ignore)))
    (cmdr-spec-discover)
    (should (cmdr-spec-test--bound-p "C-c z h"))
    (should-not (cmdr-spec-test--bound-p "C-c z g"))
    (should (equal (cdr (assq 'gone cmdr-spec-skipped)) "library cmdr-no-such-library not found"))))

(ert-deftest cmdr-spec-test/autoloads-library-loaded-when-found ()
  (cmdr-spec-test--with-table
    (let* ((dir (make-temp-file "cmdr-autoloads" t))
           (load-path (cons dir load-path)))
      (unwind-protect
          (progn
            (write-region "(setq cmdr-spec-test-autoloaded t)\n" nil
                          (expand-file-name "cmdr-test-autoloads.el" dir))
            (cmdr-register-spec :name auto :probe "cmdr-test-autoloads"
                                :autoloads "cmdr-test-autoloads"
                                :bindings (("C-c z u" . ignore)))
            (cmdr-register-spec :name absent-auto :autoloads "cmdr-no-such-autoloads")
            (defvar cmdr-spec-test-autoloaded nil)
            (setq cmdr-spec-test-autoloaded nil)
            (cmdr-spec-discover)
            (should cmdr-spec-test-autoloaded)
            (should (cmdr-spec-test--bound-p "C-c z u")))
        (delete-directory dir t)))))

(provide 'test-cmdr-spec)
;;; test-cmdr-spec.el ends here
