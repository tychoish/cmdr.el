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
         (cmdr-spec--saved-settings nil)
         (cmdr-spec--activate-scheduled nil)
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

(ert-deftest cmdr-spec-test/emacs-spec-registers-maintenance-commands ()
  (let ((spec (cmdr-spec-get 'emacs)))
    (should spec)
    (let ((cmds (cmdr-spec-commands spec)))
      (should (= (length cmds) 2))
      (let ((recompile (seq-find (lambda (c) (eq (cmdr-spec-command-command c) 'builder-emacs-conf-recompile-all)) cmds))
            (upgrade (seq-find (lambda (c) (eq (cmdr-spec-command-command c) 'elpaish-install-upgrade-packages)) cmds)))
        (should recompile)
        (should (eq (cmdr-spec-command-category recompile) 'emacs))
        (should (equal (cmdr-spec-command-display-name recompile) "recompile-emacs-conf"))
        (should (equal (cmdr-spec-command-transient-key recompile) "er"))
        (should upgrade)
        (should (eq (cmdr-spec-command-category upgrade) 'emacs))
        (should (equal (cmdr-spec-command-display-name upgrade) "upgrade-emacs-packages"))
        (should (equal (cmdr-spec-command-transient-key upgrade) "eu"))))))

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

(defvar test-cmdr-spec--ext-map (make-sparse-keymap))
(defvar test-cmdr-spec--sub-map nil)

(ert-deftest cmdr-spec-test/keymap-bindings-install ()
  (cmdr-spec-test--with-table
    (provide 'test-cmdr-spec-feature)
    (cmdr-register-spec
      :name ext-feature
      :probe "ert"
      :keymap-bindings
      ((test-cmdr-spec-feature test-cmdr-spec--ext-map
        :prefixes (("/" "test-sub" test-cmdr-spec--sub-map))
        :bindings (("/ x" . (cons "test-cmd" 'ignore))
                   ("/ y" . 'ignore)))))
    (cmdr-spec-install 'ext-feature)
    (should test-cmdr-spec--sub-map)
    (should (eq (keymap-lookup test-cmdr-spec--ext-map "/") test-cmdr-spec--sub-map))
    (should (eq (keymap-lookup test-cmdr-spec--sub-map "x") 'ignore))
    (should (eq (keymap-lookup test-cmdr-spec--sub-map "y") 'ignore))))

(ert-deftest cmdr-spec-test/keymap-bindings-shorthand ()
  (cmdr-spec-test--with-table
    (provide 'test-cmdr-spec-feature2)
    (defvar test-cmdr-spec--shorthand-map (make-sparse-keymap))
    (cmdr-register-spec
      :name ext-feature2
      :probe "ert"
      :keymap-bindings
      ((test-cmdr-spec-feature2 test-cmdr-spec--shorthand-map
        ("a" . 'ignore))))
    (cmdr-spec-install 'ext-feature2)
    (should (eq (keymap-lookup test-cmdr-spec--shorthand-map "a") 'ignore))))

(defvar test-cmdr-spec--deferred-hook nil)
(defvar test-cmdr-spec--deferred-map nil)

(ert-deftest cmdr-spec-test/keymap-bindings-deferred-via-explicit-hook ()
  (cmdr-spec-test--with-table
    (setq test-cmdr-spec--deferred-hook nil
          test-cmdr-spec--deferred-map nil)
    (cmdr-register-spec
      :name ext-deferred
      :probe "ert"
      :keymap-bindings
      ((test-pkg test-cmdr-spec--deferred-map
        :hook test-cmdr-spec--deferred-hook
        ("d" . 'ignore))))
    (cmdr-spec-install 'ext-deferred)
    ;; Map not defined yet, hook should be waiting
    (should test-cmdr-spec--deferred-hook)
    ;; Package/mode activates and initializes the map
    (setq test-cmdr-spec--deferred-map (make-sparse-keymap))
    (run-hooks 'test-cmdr-spec--deferred-hook)
    ;; Key should now be bound
    (should (eq (keymap-lookup test-cmdr-spec--deferred-map "d") 'ignore))
    ;; One-shot hook should have removed itself
    (should-not test-cmdr-spec--deferred-hook)))

(defvar test-cmdr-sample-mode-hook nil)
(defvar test-cmdr-sample-mode-map nil)

(ert-deftest cmdr-spec-test/keymap-bindings-deferred-via-derived-mode-hook ()
  (cmdr-spec-test--with-table
    (setq test-cmdr-sample-mode-hook nil
          test-cmdr-sample-mode-map nil)
    (cmdr-register-spec
      :name ext-derived-hook
      :probe "ert"
      :keymap-bindings
      ((test-pkg test-cmdr-sample-mode-map
        ("m" . 'ignore))))
    (cmdr-spec-install 'ext-derived-hook)
    ;; Map not defined yet, derived test-cmdr-sample-mode-hook should be waiting
    (should test-cmdr-sample-mode-hook)
    ;; Package/mode activates
    (setq test-cmdr-sample-mode-map (make-sparse-keymap))
    (run-hooks 'test-cmdr-sample-mode-hook)
    ;; Key should now be bound
    (should (eq (keymap-lookup test-cmdr-sample-mode-map "m") 'ignore))
    ;; One-shot hook should have removed itself
    (should-not test-cmdr-sample-mode-hook)))

(ert-deftest cmdr-spec-test/transient-suffixes-install-immediate-and-deferred ()
  (cmdr-spec-test--with-table
    (let ((inserted nil)
          (cmdr-spec--pending-transient-suffixes nil))
      (put 'test-cmdr-spec--prefix 'transient--layout t)
      (unwind-protect
          (cl-letf (((symbol-function 'cmdr-transient-insert-suffix-once)
                     (lambda (prefix loc suffix)
                       (push (list prefix loc suffix) inserted))))
            (cmdr-register-spec
              :name test-ts
              :probe "ert"
              :transient-suffixes
              ((test-cmdr-spec--prefix (-1 0) ("x" "ext" ignore))
               (test-cmdr-spec--deferred-prefix (-1) ("y" "def" ignore))))
            (cmdr-spec-install 'test-ts)
            (should (equal inserted
                           '((test-cmdr-spec--prefix (-1 0) ("x" "ext" ignore)))))
            (should (equal cmdr-spec--pending-transient-suffixes
                           '((test-cmdr-spec--deferred-prefix (-1) ("y" "def" ignore)))))
            (put 'test-cmdr-spec--deferred-prefix 'transient--layout t)
            (cmdr-spec--flush-transient-suffixes)
            (should-not cmdr-spec--pending-transient-suffixes)
            (should (equal inserted
                           '((test-cmdr-spec--deferred-prefix (-1) ("y" "def" ignore))
                             (test-cmdr-spec--prefix (-1 0) ("x" "ext" ignore))))))
        (put 'test-cmdr-spec--prefix 'transient--layout nil)
        (put 'test-cmdr-spec--deferred-prefix 'transient--layout nil)
        (remove-hook 'after-load-functions #'cmdr-spec--flush-transient-suffixes)))))

(defvar cmdr-spec-test--scalar nil)
(defvar cmdr-spec-test--collection nil)

(ert-deftest cmdr-spec-test/settings-apply-and-revert ()
  "Specs apply settings on install and revert cleanly."
  (cmdr-spec-test--with-table
    (let ((cmdr-spec-merge-variables '(cmdr-spec-test--collection))
          (cmdr-spec-test--scalar 'orig)
          (cmdr-spec-test--collection '(a b)))
      (unwind-protect
          (progn
            (cmdr-register-spec
              :name test-settings
              :settings ((cmdr-spec-test--scalar . updated)
                         (cmdr-spec-test--collection . (c d))))
            (cmdr-spec-install 'test-settings)
            (should (eq cmdr-spec-test--scalar 'updated))
            (should (member 'c cmdr-spec-test--collection))
            (should (member 'd cmdr-spec-test--collection))
            (should (member 'a cmdr-spec-test--collection))
            ;; Idempotent on second install
            (cmdr-spec-install 'test-settings)
            (should (equal (length cmdr-spec-test--collection) 4))
            (cmdr-spec--revert-settings)
            (should (eq cmdr-spec-test--scalar 'orig))
            (should (equal cmdr-spec-test--collection '(a b))))
        (cmdr-spec--revert-settings)))))

(defvar cmdr-spec-test--inst-count 0)
(defun cmdr-spec-test--inst-fn ()
  (cl-incf cmdr-spec-test--inst-count))

(defvar cmdr-spec-test--external-ran nil)
(defun cmdr-spec-test--external-fn ()
  (setq cmdr-spec-test--external-ran t))

(ert-deftest cmdr-spec-test/install-hook-runs-on-install ()
  "Specs run install hook on installation, both from :install-hook and via add-hook."
  (cmdr-spec-test--with-table
    (setq cmdr-spec-test--inst-count 0
          cmdr-spec-test--external-ran nil)
    (cmdr-register-spec
      :name test-inst
      :install-hook #'cmdr-spec-test--inst-fn)
    (add-hook 'cmdr-spec-test-inst-install-hook #'cmdr-spec-test--external-fn)
    (should (= cmdr-spec-test--inst-count 0))
    (should-not cmdr-spec-test--external-ran)
    (cmdr-spec-install 'test-inst)
    (should (= cmdr-spec-test--inst-count 1))
    (should cmdr-spec-test--external-ran)))

(defvar cmdr-spec-test--already-ran nil)
(defun cmdr-spec-test--already-fn ()
  (setq cmdr-spec-test--already-ran t))

(defvar cmdr-spec-test--deferred-ran nil)
(defun cmdr-spec-test--deferred-fn ()
  (setq cmdr-spec-test--deferred-ran t))

(ert-deftest cmdr-spec-test/activate-hook-runs-on-feature-load ()
  "Specs run activate hook when their feature loads, or immediately if already loaded."
  (cmdr-spec-test--with-table
    ;; 1. Already loaded feature ("ert")
    (setq cmdr-spec-test--already-ran nil)
    (cmdr-register-spec
      :name test-already
      :probe "ert"
      :activate-hook #'cmdr-spec-test--already-fn)
    (should-not cmdr-spec-test--already-ran)
    (cmdr-spec-install 'test-already)
    (should cmdr-spec-test--already-ran)
    ;; 2. Deferred feature loaded later
    (setq cmdr-spec-test--deferred-ran nil)
    (setq features (delq 'cmdr-test-deferred-feature features))
    (unwind-protect
        (progn
          (cmdr-register-spec
            :name test-defer
            :feature cmdr-test-deferred-feature
            :activate-hook #'cmdr-spec-test--deferred-fn)
          (cmdr-spec-install 'test-defer)
          (should-not cmdr-spec-test--deferred-ran)
          (provide 'cmdr-test-deferred-feature)
          (should cmdr-spec-test--deferred-ran))
      (setq features (delq 'cmdr-test-deferred-feature features)))))

(ert-deftest cmdr-spec-test/hook-rejects-anonymous-function ()
  "Reject anonymous lambdas as hook arguments in cmdr-register-spec."
  (should-error
   (macroexpand '(cmdr-register-spec :name bad :install-hook (lambda () nil)))
   :type 'user-error))

(provide 'test-cmdr-spec)
;;; test-cmdr-spec.el ends here
