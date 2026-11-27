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
         (cmdr-spec--pending-by-feature (make-hash-table :test #'eq))
         (cmdr-spec--activated (make-hash-table :test #'eq))
         (cmdr-spec--installed (make-hash-table :test #'eq))
         (cmdr-spec--loaded-autoloads (make-hash-table :test #'eq))
         (cmdr-spec--activating (make-hash-table :test #'eq))
         (cmdr-spec--pending-resources (make-hash-table :test #'eq))
         (after-load-functions (copy-sequence after-load-functions))
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
    (should (eq (keymap-lookup cmdr-mode-map "C-c r m") #'agent-shell-menu-dispatch))
    (should (eq (keymap-lookup cmdr-mode-map "C-c r s m") #'agent-shell-menu-dispatch))
    (should (eq (keymap-lookup cmdr-mode-map "C-c r s u") #'agent-shell-quota))
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

(ert-deftest cmdr-spec-test/magit-dash-open-preserves-activation-feature ()
  "The magit-dash-open integration retains its original activation feature."
  (should (eq (cmdr-spec--resolve-feature (cmdr-spec-get 'magit-dash-open))
              'magit-dash-open)))

(ert-deftest cmdr-spec-test/magit-dash-activates-on-core-library ()
  "The magit-dash spec waits for the full library, not the open command."
  (should (eq (cmdr-spec--resolve-feature (cmdr-spec-get 'magit-dash))
              'magit-dash))
  (should (eq (cmdr-spec-probe (cmdr-spec-get 'magit-dash)) 'magit-dash))
  (should (equal (seq-map #'cmdr-spec-command-command
                          (cmdr-spec-commands (cmdr-spec-get 'magit-dash)))
                 '(magit-dash-sync-all magit-dash-sync-repo)))
  (should (boundp 'cmdr-spec-magit-dash-activate-hook))
  (should (boundp 'cmdr-spec-magit-dash-open-activate-hook)))

(defvar cmdr-spec-magit-dash-activate-hook)
(defvar cmdr-spec-test--magit-dash-registrations 0)

(defun cmdr-spec-test--register-magit-dash-repositories ()
  "Check that registration is available when the core spec activates."
  (should (fboundp 'magit-dash-register))
  (setq cmdr-spec-test--magit-dash-registrations
        (1+ cmdr-spec-test--magit-dash-registrations)))

(ert-deftest cmdr-spec-test/magit-dash-registration-waits-for-core-load ()
  "Loading the open library cannot call unavailable core registration."
  (let ((core (cmdr-spec-get 'magit-dash)))
    (cmdr-spec-test--with-table
      (let ((features (remq 'magit-dash (remq 'magit-dash-open features)))
            (cmdr-spec-magit-dash-activate-hook
             '(cmdr-spec-test--register-magit-dash-repositories))
            (cmdr-spec-test--magit-dash-registrations 0))
        (cl-letf (((symbol-function 'magit-dash-register) nil))
          (setq cmdr-spec-table (list core))
          (cmdr-spec-install 'magit-dash)
          (should (= 0 cmdr-spec-test--magit-dash-registrations))
          (push 'magit-dash-open features)
          (cmdr-spec--after-load "magit-dash-open.el")
          (should (= 0 cmdr-spec-test--magit-dash-registrations))
          (fset 'magit-dash-register #'ignore)
          (push 'magit-dash features)
          (cmdr-spec--after-load "magit-dash.el")
          (should (= 1 cmdr-spec-test--magit-dash-registrations))
          (cmdr-spec--after-load "unrelated.el")
          (should (= 1 cmdr-spec-test--magit-dash-registrations)))))))

(ert-deftest cmdr-spec-test/install-avoids-unnecessary-feature-lookups ()
  "Installing featureless, same-name, and autoload probes avoids disk scans."
  (cmdr-spec-test--with-table
    (unwind-protect
        (progn
          (autoload 'cmdr-spec-test-autoload-probe "cmdr-spec-test-autoload-probe")
          (autoload 'cmdr-spec-test-other-probe "different-library")
          (cmdr-register-spec :name no-probe)
          (cmdr-register-spec :name same-probe :probe same-probe)
          (cmdr-register-spec :name autoload-probe :probe cmdr-spec-test-autoload-probe)
          (cmdr-register-spec :name fallback-name :probe cmdr-spec-test-other-probe)
          (cl-letf (((symbol-function 'locate-library)
                     (lambda (&rest _) (ert-fail "Unexpected library scan"))))
            (cmdr-spec-install 'no-probe)
            (cmdr-spec-install 'same-probe)
            (cmdr-spec-install 'autoload-probe)
            (cmdr-spec-install 'fallback-name)
            (should (eq (cmdr-spec--resolve-feature (cmdr-spec-get 'autoload-probe))
                        'cmdr-spec-test-autoload-probe))
            (should (eq (cmdr-spec--resolve-feature (cmdr-spec-get 'fallback-name))
                        'fallback-name))))
      (fmakunbound 'cmdr-spec-test-autoload-probe)
      (fmakunbound 'cmdr-spec-test-other-probe))))

(ert-deftest cmdr-spec-test/reinstall-activated-spec-skips-feature-resolution ()
  "Do not search for an already activated spec's feature on reinstall."
  (cmdr-spec-test--with-table
    (let ((features (cons 'cmdr-spec-test-probe (copy-sequence features)))
          (calls 0))
      (cl-letf (((symbol-function 'cmdr-spec-test-probe) #'ignore)
                ((symbol-function 'locate-library) (lambda (&rest _) "probe.el")))
        (cmdr-register-spec :name active :probe cmdr-spec-test-probe)
        (add-hook 'cmdr-spec-active-activate-hook (lambda () (cl-incf calls)))
        (cmdr-spec-install 'active)
        (should (= calls 1))
        (cl-letf (((symbol-function 'locate-library)
                   (lambda (&rest _) (ert-fail "Unexpected repeat library scan"))))
          (cmdr-spec-install 'active))
        (should (= calls 1))))))

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

(defvar cmdr-spec-test-autoload-count 0)

(ert-deftest cmdr-spec-test/autoloads-loaded-once-until-reload ()
  "Discovery loads autoloads once; an explicit reload loads them again."
  (cmdr-spec-test--with-table
    (let* ((dir (make-temp-file "cmdr-autoload-count-" t))
           (load-path (cons dir load-path))
           (cmdr-spec-test-autoload-count 0))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "cmdr-test-count-autoloads.el" dir)
              (insert "(setq cmdr-spec-test-autoload-count (1+ cmdr-spec-test-autoload-count))\n"))
            (cmdr-register-spec :name counted :autoloads "cmdr-test-count-autoloads")
            (cmdr-spec-discover)
            (cmdr-spec-discover)
            (should (= cmdr-spec-test-autoload-count 1))
            (cmdr-spec-reload 'counted)
            (should (= cmdr-spec-test-autoload-count 2)))
        (delete-directory dir t)))))

(ert-deftest cmdr-spec-test/autoloads-load-new-library-on-spec-replacement ()
  "A replacement spec loads new libraries and reload reloads both."
  (cmdr-spec-test--with-table
    (let* ((dir (make-temp-file "cmdr-replace-autoload-" t))
           (load-path (cons dir load-path))
           (cmdr-spec-test-autoload-count 0))
      (unwind-protect
          (progn
            (with-temp-file (expand-file-name "cmdr-count-a.el" dir)
              (insert "(setq cmdr-spec-test-autoload-count (+ cmdr-spec-test-autoload-count 1))\n"))
            (with-temp-file (expand-file-name "cmdr-count-b.el" dir)
              (insert "(setq cmdr-spec-test-autoload-count (+ cmdr-spec-test-autoload-count 10))\n"))
            (cmdr-register-spec :name replaced-auto :autoloads "cmdr-count-a")
            (cmdr-spec-discover)
            (cmdr-register-spec :name replaced-auto :autoloads ("cmdr-count-a" "cmdr-count-b"))
            (cmdr-spec-discover)
            (should (= cmdr-spec-test-autoload-count 11))
            (cmdr-spec-reload 'replaced-auto)
            (should (= cmdr-spec-test-autoload-count 22)))
        (delete-directory dir t)))))

(ert-deftest cmdr-spec-test/autoloads-retry-after-library-appears ()
  "A missing autoload library is retried when it becomes available."
  (cmdr-spec-test--with-table
    (let* ((dir (make-temp-file "cmdr-late-autoload-" t))
           (load-path (cons dir load-path))
           (cmdr-spec-test-autoload-count 0))
      (unwind-protect
          (progn
            (cmdr-register-spec :name late-autoload :autoloads "cmdr-late-autoloads")
            (cmdr-spec-discover)
            (with-temp-file (expand-file-name "cmdr-late-autoloads.el" dir)
              (insert "(setq cmdr-spec-test-autoload-count (1+ cmdr-spec-test-autoload-count))\n"))
            (cmdr-spec-discover)
            (should (= cmdr-spec-test-autoload-count 1)))
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

(ert-deftest cmdr-spec-test/reinstall-activated-spec-installs-available-keymap ()
  "Reinstalling an active spec still installs newly available keymaps."
  (cmdr-spec-test--with-table
    (let ((features (delq 'cmdr-spec-test-late-resource (copy-sequence features)))
          (test-cmdr-spec--deferred-map nil))
      (cmdr-register-spec :name active-resource :feature ert
                          :keymap-bindings
                          ((cmdr-spec-test-late-resource
                            test-cmdr-spec--deferred-map ("d" . ignore))))
      (cmdr-spec-install 'active-resource)
      (should (map-elt cmdr-spec--activated 'active-resource))
      (setq test-cmdr-spec--deferred-map (make-sparse-keymap))
      (provide 'cmdr-spec-test-late-resource)
      (cmdr-spec-install 'active-resource)
      (should (eq (keymap-lookup test-cmdr-spec--deferred-map "d") #'ignore)))))

(ert-deftest cmdr-spec-test/keymap-bindings-deferred-until-feature ()
  (cmdr-spec-test--with-table
    (let ((features (delq 'test-pkg (copy-sequence features)))
          (test-cmdr-spec--deferred-map nil))
      (cmdr-register-spec :name deferred-map :feature none
        :keymap-bindings ((test-pkg test-cmdr-spec--deferred-map
                           ("d" . 'ignore))))
      (cmdr-spec-install 'deferred-map)
      (should (memq #'cmdr-spec--after-load after-load-functions))
      (setq test-cmdr-spec--deferred-map (make-sparse-keymap))
      (provide 'test-pkg)
      (cmdr-spec--after-load "test-pkg")
      (should (eq (keymap-lookup test-cmdr-spec--deferred-map "d") 'ignore))
      (should-not (memq #'cmdr-spec--after-load after-load-functions)))))

(ert-deftest cmdr-spec-test/transient-suffixes-deferred ()
  (cmdr-spec-test--with-table
    (let ((inserted nil))
      (unwind-protect
          (cl-letf (((symbol-function 'cmdr-transient-insert-suffix-once)
                     (lambda (prefix loc suffix) (push (list prefix loc suffix) inserted))))
            (cmdr-register-spec :name test-ts :feature none
              :transient-suffixes ((test-cmdr-spec--prefix (-1) ("x" "ext" ignore))))
            (cmdr-spec-install 'test-ts)
            (should (gethash 'test-ts cmdr-spec--pending-resources))
            (put 'test-cmdr-spec--prefix 'transient--layout t)
            (cmdr-spec--after-load "test")
            (should (equal inserted '((test-cmdr-spec--prefix (-1) ("x" "ext" ignore)))))
            (should-not (gethash 'test-ts cmdr-spec--pending-resources)))
        (put 'test-cmdr-spec--prefix 'transient--layout nil)))))

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
    (let ((after-load-alist (copy-tree after-load-alist))
          (feature-file (make-temp-file "cmdr-deferred-" nil ".el")))
      (unwind-protect
        (progn
          (with-temp-file feature-file
            (insert "(provide 'cmdr-test-deferred-feature)\n"))
          (cmdr-register-spec
            :name test-defer
            :feature cmdr-test-deferred-feature
            :activate-hook #'cmdr-spec-test--deferred-fn)
          (cmdr-spec-install 'test-defer)
          (should-not cmdr-spec-test--deferred-ran)
          (load feature-file nil t)
          (should cmdr-spec-test--deferred-ran))
        (setq features (delq 'cmdr-test-deferred-feature features))
        (delete-file feature-file)))))

(defvar cmdr-spec-late-registration-activate-hook nil)
(defvar cmdr-spec-test--late-hook-ran nil)

(defun cmdr-spec-test--late-hook ()
  "Record activation of the late registration test spec."
  (setq cmdr-spec-test--late-hook-ran t))

(ert-deftest cmdr-spec-test/activation-hook-added-after-install ()
  "A hook added after installation runs when the feature is loaded."
  (cmdr-spec-test--with-table
    (let ((cmdr-spec-late-registration-activate-hook nil)
          (cmdr-spec-test--late-hook-ran nil)
          (features (delq 'cmdr-spec-test-late-feature (copy-sequence features)))
          (file (make-temp-file "cmdr-late-feature-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert "(provide 'cmdr-spec-test-late-feature)\n"))
            (cmdr-register-spec :name late-registration :feature cmdr-spec-test-late-feature)
            (cmdr-spec-install 'late-registration)
            (add-hook 'cmdr-spec-late-registration-activate-hook #'cmdr-spec-test--late-hook)
            (load file nil t)
            (should cmdr-spec-test--late-hook-ran)
            (should (map-empty-p cmdr-spec--pending-by-feature))
            (should-not (memq #'cmdr-spec--after-load after-load-functions)))
        (delete-file file)))))

(ert-deftest cmdr-spec-test/reload-and-redefine-activation ()
  (cmdr-spec-test--with-table
    (setq cmdr-spec-test--inst-count 0)
    (cmdr-register-spec :name repeat :feature ert
                        :activate-hook #'cmdr-spec-test--inst-fn)
    (cmdr-spec-install 'repeat)
    (cmdr-spec-install 'repeat)
    (should (= cmdr-spec-test--inst-count 1))
    (cmdr-spec-reload 'repeat)
    (should (= cmdr-spec-test--inst-count 2))
    (cmdr-register-spec :name repeat :feature ert
                        :activate-hook #'cmdr-spec-test--inst-fn)
    (cmdr-spec-install 'repeat)
    (should (= cmdr-spec-test--inst-count 3))))

(ert-deftest cmdr-spec-test/commands-flush-when-cmdr-loads ()
  (cmdr-spec-test--with-table
    (let ((features (delq 'cmdr (copy-sequence features))) registered)
      (cl-letf (((symbol-function 'cmdr-register-command)
                 (lambda (&rest args) (push args registered))))
        (cmdr-register-spec :name commands :feature none
          :commands ((cmdr-spec-command :category test :command ignore)))
        (cmdr-spec-install 'commands)
        (should (memq #'cmdr-spec--after-load after-load-functions))
        (provide 'cmdr)
        (cmdr-spec--after-load "cmdr")
        (should (= (length registered) 1))
        (should-not cmdr-spec--pending)
        (should-not (memq #'cmdr-spec--after-load after-load-functions))))))

(ert-deftest cmdr-spec-test/featureless-spec-does-not-leave-dispatcher ()
  (cmdr-spec-test--with-table
    (cmdr-register-spec :name no-library :bindings (("C-c z" . ignore)))
    (cmdr-spec-install 'no-library)
    (should-not (memq #'cmdr-spec--after-load after-load-functions))))

(ert-deftest cmdr-spec-test/replacement-clears-pending-dispatcher ()
  (cmdr-spec-test--with-table
    (cmdr-register-spec :name replaced :feature never-loaded
                        :activate-hook #'cmdr-spec-test--inst-fn)
    (cmdr-spec-install 'replaced)
    (should (memq #'cmdr-spec--after-load after-load-functions))
    (cmdr-register-spec :name replaced :feature none)
    (should-not (memq #'cmdr-spec--after-load after-load-functions))))

(ert-deftest cmdr-spec-test/install-does-not-use-eval-after-load ()
  (cmdr-spec-test--with-table
    (cl-letf (((symbol-function 'eval-after-load)
               (lambda (&rest _) (ert-fail "Unexpected eval-after-load"))))
      (cmdr-register-spec :name no-eval :feature never-loaded
                          :activate-hook #'cmdr-spec-test--inst-fn)
      (cmdr-spec-install 'no-eval))))

(defvar cmdr-spec-nested-first-activate-hook nil)
(defvar cmdr-spec-nested-second-activate-hook nil)
(defvar cmdr-spec-test--nested-file nil)
(defvar cmdr-spec-test--nested-count 0)
(defvar cmdr-spec-test--nested-second-count 0)

(defun cmdr-spec-test--nested-activate ()
  "Load another feature while the first spec is activating."
  (cl-incf cmdr-spec-test--nested-count)
  (load cmdr-spec-test--nested-file nil t))

(defun cmdr-spec-test--nested-second-activate ()
  "Record activation of the feature loaded by another activation hook."
  (cl-incf cmdr-spec-test--nested-second-count))

(ert-deftest cmdr-spec-test/keymap-appears-after-feature ()
  "Retry a missing map after its already-provided feature loads another file."
  (cmdr-spec-test--with-table
    (let ((features (cons 'cmdr-spec-test-map-package (copy-sequence features)))
          (test-cmdr-spec--deferred-map nil)
          (file (make-temp-file "cmdr-map-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert ";;; -*- lexical-binding: t; -*-\n"
                      "(setq test-cmdr-spec--deferred-map (make-sparse-keymap))\n"))
            (cmdr-register-spec :name late-map :feature none
              :keymap-bindings ((cmdr-spec-test-map-package test-cmdr-spec--deferred-map
                                 ("d" . ignore))))
            (cmdr-spec-install 'late-map)
            (should (gethash 'late-map cmdr-spec--pending-resources))
            (should (memq #'cmdr-spec--after-load after-load-functions))
            (load file nil t)
            (should (eq (keymap-lookup test-cmdr-spec--deferred-map "d") #'ignore))
            (should-not (gethash 'late-map cmdr-spec--pending-resources))
            (should-not (memq #'cmdr-spec--after-load after-load-functions)))
        (delete-file file)))))

(ert-deftest cmdr-spec-test/real-transient-layout-appears-and-reloads-once ()
  "Insert into a late real transient layout and do not duplicate on reload."
  (cmdr-spec-test--with-table
    (let ((features (cons 'cmdr-spec-test-layout-package (copy-sequence features)))
          (file (make-temp-file "cmdr-layout-" nil ".el"))
          (prefix 'cmdr-spec-test--late-prefix))
      (unwind-protect
          (progn
            (with-temp-file file
              (insert ";;; -*- lexical-binding: t; -*-\n"
                      "(transient-define-prefix cmdr-spec-test--late-prefix ()\n"
                      "  [(\"a\" \"alpha\" ignore)])\n"))
            (cmdr-register-spec :name late-layout :feature none
              :transient-suffixes ((cmdr-spec-test-layout-package
                                    cmdr-spec-test--late-prefix (-1 0)
                                    ("x" "extra" ignore))))
            (cmdr-spec-install 'late-layout)
            (should (gethash 'late-layout cmdr-spec--pending-resources))
            (load file nil t)
            (should (transient-get-suffix prefix "x"))
            (should-not (gethash 'late-layout cmdr-spec--pending-resources))
            (should-not (memq #'cmdr-spec--after-load after-load-functions))
            (let ((layout (format "%S" (get prefix 'transient--layout))))
              (cmdr-spec-reload 'late-layout)
              (cmdr-spec-reload 'late-layout)
              (should (transient-get-suffix prefix "x"))
              (should (equal layout (format "%S" (get prefix 'transient--layout))))))
        (delete-file file)
        (setplist prefix nil)
        (when (fboundp prefix) (fmakunbound prefix))))))

(ert-deftest cmdr-spec-test/nested-feature-load-during-activation ()
  "Nested library loads activate each waiting spec exactly once."
  (cmdr-spec-test--with-table
    (let ((features (copy-sequence features))
          (cmdr-spec-test--nested-count 0)
          (cmdr-spec-test--nested-second-count 0)
          (cmdr-spec-nested-first-activate-hook nil)
          (cmdr-spec-nested-second-activate-hook nil)
          (first (make-temp-file "cmdr-first-" nil ".el"))
          (cmdr-spec-test--nested-file (make-temp-file "cmdr-second-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file first
              (insert ";;; -*- lexical-binding: t; -*-\n(provide 'cmdr-spec-test-first)\n"))
            (with-temp-file cmdr-spec-test--nested-file
              (insert ";;; -*- lexical-binding: t; -*-\n(provide 'cmdr-spec-test-second)\n"))
            (cmdr-register-spec :name nested-first :feature cmdr-spec-test-first
                                :activate-hook #'cmdr-spec-test--nested-activate)
            (cmdr-register-spec :name nested-second :feature cmdr-spec-test-second
                                :activate-hook #'cmdr-spec-test--nested-second-activate)
            (cmdr-spec-install 'nested-first)
            (cmdr-spec-install 'nested-second)
            (load first nil t)
            (should (= cmdr-spec-test--nested-count 1))
            (should (= cmdr-spec-test--nested-second-count 1))
            (should (= (hash-table-count cmdr-spec--pending-by-feature) 0))
            (should-not (memq #'cmdr-spec--after-load after-load-functions)))
        (delete-file first)
        (delete-file cmdr-spec-test--nested-file)))))

(ert-deftest cmdr-spec-test/keymap-feature-loads-during-install ()
  "A feature loaded during installation must not lose its pending map."
  (cmdr-spec-test--with-table
    (let ((features (delq 'cmdr-spec-test-nested-map (copy-sequence features)))
          (test-cmdr-spec--deferred-map nil)
          (file (make-temp-file "cmdr-nested-map-" nil ".el"))
          loaded)
      (unwind-protect
          (progn
            (with-temp-file file
              (insert ";;; -*- lexical-binding: t; -*-\n"
                      "(setq test-cmdr-spec--deferred-map (make-sparse-keymap))\n"
                      "(provide 'cmdr-spec-test-nested-map)\n"))
            (cmdr-register-spec :name nested-map :feature none
              :keymap-bindings ((cmdr-spec-test-nested-map test-cmdr-spec--deferred-map
                                 ("d" . ignore))))
            (cl-letf (((symbol-function 'cmdr-spec--activate-transient-suffixes)
                       (lambda (_spec)
                         (unless loaded (setq loaded t) (load file nil t)))))
              (cmdr-spec-install 'nested-map))
            (should (eq (keymap-lookup test-cmdr-spec--deferred-map "d") #'ignore))
            (should-not (memq #'cmdr-spec--after-load after-load-functions)))
        (delete-file file)))))

(ert-deftest cmdr-spec-test/unknown-keymap-option-rejected ()
  "Invalid keymap options should not silently discard configuration."
  (cmdr-spec-test--with-table
    (should-error
     (cmdr-register-spec :name invalid-options
       :keymap-bindings ((ert test-cmdr-spec--deferred-map :bogus something
                              ("d" . ignore))))
     :type 'user-error)))

(defvar cmdr-spec-test--late-map-file nil)
(defvar cmdr-spec-test--late-map-count 0)

(defun cmdr-spec-test--load-late-map ()
  "Load a file which defines a pending keymap."
  (cl-incf cmdr-spec-test--late-map-count)
  (load cmdr-spec-test--late-map-file nil t))

(ert-deftest cmdr-spec-test/nested-load-rechecks-earlier-missing-resource ()
  "A nested load can satisfy a resource already checked in the same pass."
  (cmdr-spec-test--with-table
    (let ((features (cons 'cmdr-spec-test-ready-map (copy-sequence features)))
          (test-cmdr-spec--deferred-map nil)
          (cmdr-spec-test--late-map-count 0)
          (cmdr-spec-test--late-map-file (make-temp-file "cmdr-late-map-" nil ".el"))
          (feature-file (make-temp-file "cmdr-shared-feature-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file cmdr-spec-test--late-map-file
              (insert ";;; -*- lexical-binding: t; -*-\n"
                      "(setq test-cmdr-spec--deferred-map (make-sparse-keymap))\n"))
            (with-temp-file feature-file
              (insert ";;; -*- lexical-binding: t; -*-\n"
                      "(provide 'cmdr-spec-test-shared-feature)\n"))
            (cmdr-register-spec :name nested-resource-b
              :feature cmdr-spec-test-shared-feature
              :activate-hook #'cmdr-spec-test--load-late-map)
            (cmdr-register-spec :name nested-resource-a
              :feature cmdr-spec-test-shared-feature
              :activate-hook #'ignore
              :keymap-bindings ((cmdr-spec-test-ready-map test-cmdr-spec--deferred-map
                                 ("d" . ignore))))
            (cmdr-spec-install 'nested-resource-b)
            (cmdr-spec-install 'nested-resource-a)
            (load feature-file nil t)
            (should (= cmdr-spec-test--late-map-count 1))
            (should (eq (keymap-lookup test-cmdr-spec--deferred-map "d") #'ignore))
            (should-not (memq #'cmdr-spec--after-load after-load-functions)))
        (delete-file cmdr-spec-test--late-map-file)
        (delete-file feature-file)))))

(ert-deftest cmdr-spec-test/reload-picker-annotates-specs ()
  "The reload picker shows descriptions and can select all installed specs."
  (cmdr-spec-test--with-table
    (let (choices)
      (cmdr-register-spec :name picker :doc "Useful description" :feature ert)
      (cmdr-spec-install 'picker)
      (cl-letf (((symbol-function 'annotated-completing-read)
                 (lambda (table &rest _options)
                   (setq choices table)
                   "All installed specs")))
        (call-interactively #'cmdr-spec-reload))
      (should (equal (cdr (assoc "picker" choices)) "Useful description"))
      (should (assoc "All installed specs" choices)))))

(defvar cmdr-spec-test--failure-count 0)
(defvar cmdr-spec-test--successful-count 0)

(defun cmdr-spec-test--fail-once ()
  "Fail on the first activation and succeed on the next attempt."
  (cl-incf cmdr-spec-test--failure-count)
  (when (= cmdr-spec-test--failure-count 1)
    (error "Expected activation failure")))

(defun cmdr-spec-test--succeed ()
  "Record a successful activation."
  (cl-incf cmdr-spec-test--successful-count))

(ert-deftest cmdr-spec-test/failed-activation-is-isolated-and-retried ()
  "A failing activation does not block other specs and can recover later."
  (cmdr-spec-test--with-table
    (let ((features (copy-sequence features))
          (cmdr-spec-test--failure-count 0)
          (cmdr-spec-test--successful-count 0)
          (feature-file (make-temp-file "cmdr-failure-feature-" nil ".el"))
          (retry-file (make-temp-file "cmdr-retry-" nil ".el")))
      (unwind-protect
          (progn
            (with-temp-file feature-file
              (insert ";;; -*- lexical-binding: t; -*-\n(provide 'cmdr-spec-test-failure-feature)\n"))
            (with-temp-file retry-file
              (insert ";;; -*- lexical-binding: t; -*-\n"))
            (cmdr-register-spec :name failure-test :feature cmdr-spec-test-failure-feature
                                :activate-hook #'cmdr-spec-test--fail-once)
            (cmdr-register-spec :name success-test :feature cmdr-spec-test-failure-feature
                                :activate-hook #'cmdr-spec-test--succeed)
            (cmdr-spec-install 'failure-test)
            (cmdr-spec-install 'success-test)
            (load feature-file nil t)
            (should (= cmdr-spec-test--failure-count 1))
            (should (= cmdr-spec-test--successful-count 1))
            (should (gethash 'failure-test cmdr-spec--pending-resources))
            (load retry-file nil t)
            (should (= cmdr-spec-test--failure-count 2))
            (should (= cmdr-spec-test--successful-count 1))
            (should-not (memq #'cmdr-spec--after-load after-load-functions)))
        (delete-file feature-file)
        (delete-file retry-file)))))

(ert-deftest cmdr-spec-test/hook-rejects-anonymous-function ()
  "Reject anonymous lambdas as hook arguments in cmdr-register-spec."
  (should-error
   (macroexpand '(cmdr-register-spec :name bad :install-hook (lambda () nil)))
   :type 'user-error))

(provide 'test-cmdr-spec)
;;; test-cmdr-spec.el ends here
