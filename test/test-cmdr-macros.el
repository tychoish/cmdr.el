;;; test-cmdr-macros.el --- Tests for cmdr-macros -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the self-contained cmdr macros.  Nothing here relies on
;; xtd-macro, early-init, or any personal configuration.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (file-name-directory
             (directory-file-name
              (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path root))

(require 'cmdr-macros)

(defun cmdr-test--capture-idle-timer (thunk)
  "Call THUNK with `run-with-idle-timer' stubbed; return (DELAY . CALLBACK)."
  (let ((captured nil))
    (cl-letf (((symbol-function 'run-with-idle-timer)
               (lambda (delay _repeat function &rest _args)
                 (setq captured (cons delay function)))))
      (funcall thunk))
    captured))

;;; Independence

(ert-deftest cmdr-macros-test/no-xtd-or-hud-dependency ()
  "The library source never requires xtd-macro, hud, or early-init."
  (with-temp-buffer
    (insert-file-contents (locate-library "cmdr-macros.el"))
    (should-not (re-search-forward "(require '\\(xtd\\|hud\\|early-init\\)" nil t))
    (goto-char (point-min))
    (should-not (re-search-forward "with-slow-op-timer" nil t))))

;;; add-lazy-init

(ert-deftest cmdr-macros-test/lazy-init-form-runs-only-on-timer ()
  "A form is evaluated when the idle callback runs, with lexical bindings."
  (let* ((result nil)
         (value 42)
         (timer (cmdr-test--capture-idle-timer
                 (lambda ()
                   (add-lazy-init :name "form" :delay 3 :form (setq result value))))))
    (should (= (car timer) 3))
    (should-not result)
    (funcall (cdr timer))
    (should (= result 42))))

(ert-deftest cmdr-macros-test/lazy-init-default-delay-and-operation ()
  "Operations are invoked by the callback; delay defaults to one second."
  (let* ((called nil)
         (timer (cmdr-test--capture-idle-timer
                 (lambda ()
                   (add-lazy-init :name "operation" :operation (lambda () (setq called t)))))))
    (should (= (car timer) 1))
    (should-not called)
    (funcall (cdr timer))
    (should called)))

(ert-deftest cmdr-macros-test/lazy-init-validates-input ()
  "Require a name and an operation or form, giving the form precedence."
  (should-error (macroexpand-1 '(add-lazy-init :form (message "x"))) :type 'user-error)
  (should-error (macroexpand-1 '(add-lazy-init :name "x")) :type 'user-error)
  (let* ((result nil)
         (timer (cmdr-test--capture-idle-timer
                 (lambda ()
                   (add-lazy-init :name "both" :form (setq result 'form)
                                  :operation (lambda () (setq result 'operation)))))))
    (funcall (cdr timer))
    (should (eq result 'form))))

;;; Helpers

(ert-deftest cmdr-macros-test/resolve-hooks ()
  "Hooks resolve to lists; the frame sentinel follows daemon state."
  (should (equal (cmdr--resolve-hooks 'foo-hook) '(foo-hook)))
  (should (equal (cmdr--resolve-hooks '(a-hook b-hook)) '(a-hook b-hook)))
  (should (equal (cmdr--resolve-hooks 'after-first-frame-created)
                 (if (daemonp) '(server-after-make-frame-hook) '(window-setup-hook))))
  (should-error (cmdr--resolve-hooks '("x")) :type 'user-error))

(ert-deftest cmdr-macros-test/build-symbol-name ()
  "Parts are trimmed, sanitized, and hyphen-joined."
  (should (equal (cmdr--build-symbol-name "turn-on" "my var" nil "") "turn-on-my-var"))
  (should (equal (cmdr--build-symbol-name "a=b" "c_d") "a-b-c-d")))

;;; add-one-shot-hook

(defvar cmdr-test--hook nil)
(defvar cmdr-test--hook-b nil)

(ert-deftest cmdr-macros-test/one-shot-hook-runs-once-and-removes ()
  "A one-shot hook fires once and removes itself."
  (let ((cmdr-test--hook nil)
        (count 0))
    (add-one-shot-hook :name "once" :hook 'cmdr-test--hook :form (cl-incf count))
    (should (= (length cmdr-test--hook) 1))
    (run-hooks 'cmdr-test--hook)
    (run-hooks 'cmdr-test--hook)
    (should (= count 1))
    (should-not cmdr-test--hook)))

(ert-deftest cmdr-macros-test/one-shot-hook-count-and-persist ()
  "COUNT limits the runs; PERSIST never removes."
  (let ((cmdr-test--hook nil)
        (cmdr-test--hook-b nil)
        (n 0)
        (p 0))
    (add-one-shot-hook :name "thrice" :hook 'cmdr-test--hook :count 3 :form (cl-incf n))
    (add-one-shot-hook :name "forever" :hook 'cmdr-test--hook-b :persist t :form (cl-incf p))
    (dotimes (_ 5)
      (run-hooks 'cmdr-test--hook 'cmdr-test--hook-b))
    (should (= n 3))
    (should (= p 5))
    (should-not cmdr-test--hook)
    (should (= (length cmdr-test--hook-b) 1))))

(ert-deftest cmdr-macros-test/one-shot-hook-multiple-hooks ()
  "A list of hooks registers on each and removes from each."
  (let ((cmdr-test--hook nil)
        (cmdr-test--hook-b nil)
        (n 0))
    (add-one-shot-hook :name "multi" :hook '(cmdr-test--hook cmdr-test--hook-b)
                       :form (cl-incf n))
    (should (= (length cmdr-test--hook) 1))
    (should (= (length cmdr-test--hook-b) 1))
    (run-hooks 'cmdr-test--hook)
    (should (= n 1))
    (should-not cmdr-test--hook)
    (should-not cmdr-test--hook-b)))

(ert-deftest cmdr-macros-test/one-shot-hook-local ()
  "A local hook is registered and removed buffer-locally."
  (with-temp-buffer
    (let ((cmdr-test--hook nil)
          (n 0))
      (add-one-shot-hook :name "local" :hook 'cmdr-test--hook :local t :form (cl-incf n))
      (should (local-variable-p 'cmdr-test--hook))
      (should-not (default-value 'cmdr-test--hook))
      (run-hooks 'cmdr-test--hook)
      (should (= n 1))
      (should-not (delq t (copy-sequence cmdr-test--hook))))))

(ert-deftest cmdr-macros-test/one-shot-hook-idle-timer ()
  "With :idle-timer the form is deferred to the timer callback."
  (let ((cmdr-test--hook nil)
        (n 0))
    (let ((timer (cmdr-test--capture-idle-timer
                  (lambda ()
                    (add-one-shot-hook :name "idle" :hook 'cmdr-test--hook
                                       :idle-timer 7 :form (cl-incf n))
                    (run-hooks 'cmdr-test--hook)))))
      (should (= (car timer) 7))
      (should (= n 0))
      (should-not cmdr-test--hook)
      (funcall (cdr timer))
      (should (= n 1)))))

(ert-deftest cmdr-macros-test/one-shot-hook-validates-input ()
  "A hook and a resolvable function are required."
  (should-error (macroexpand-1 '(add-one-shot-hook :name "x" :form (ignore))) :type 'user-error)
  (should-error (macroexpand-1 '(add-one-shot-hook :name "x" :hook 'h)) :type 'user-error)
  (should-error (macroexpand-1 '(add-one-shot-hook :name "x" :hook 'h :form nil :count 'a))
                :type 'user-error))

;;; create-toggle-functions

(defvar cmdr-test--flag nil)

(create-toggle-functions cmdr-test--flag)

(ert-deftest cmdr-macros-test/toggle-functions ()
  "Generated commands set and flip the variable."
  (let ((cmdr-test--flag nil))
    (should (commandp 'turn-on-cmdr-test--flag))
    (turn-on-cmdr-test--flag)
    (should cmdr-test--flag)
    (toggle-cmdr-test--flag)
    (should-not cmdr-test--flag)
    (turn-on-cmdr-test--flag)
    (turn-off-cmdr-test--flag)
    (should-not cmdr-test--flag)))

(ert-deftest cmdr-macros-test/toggle-functions-options ()
  "SHORT-NAME, LOCAL, and KEYMAP/KEY are honored."
  (let ((expansion (macroexpand-1
                    '(create-toggle-functions some-var :short-name "thing" :local t
                                              :keymap m :key "t"))))
    (should (equal (seq-map (lambda (form) (and (eq (car-safe form) 'defun) (cadr form)))
                            (cdr expansion))
                   '(turn-on-thing-local turn-off-thing-local toggle-thing-local nil)))
    (should (equal (car (last expansion)) '(keymap-set m "t" #'toggle-thing-local)))
    (should (string-match-p "setq-local" (prin1-to-string expansion))))
  (should-error (macroexpand-1 '(create-toggle-functions v :keymap m)) :type 'user-error))

(declare-function turn-on-compiled "test-cmdr-macros")

(ert-deftest cmdr-macros-test/toggle-functions-compiled ()
  "Expansion works when byte-compiled."
  (let ((cmdr-test--flag nil)
        (fn (byte-compile (lambda ()
                            (create-toggle-functions cmdr-test--flag :short-name "compiled")
                            (turn-on-compiled)
                            cmdr-test--flag))))
    (should (funcall fn))))

;;; make-read-extended-command-for-prefix

(defvar cmdr-test--prefix-map (make-sparse-keymap))

(make-read-extended-command-for-prefix "cmdr-test"
  :bind-key "x"
  :bind-map cmdr-test--prefix-map
  :key-alias "cmdr-test-commands")

(ert-deftest cmdr-macros-test/extended-command-prefix ()
  "Predicate and command are generated and bound with a label."
  (should (commandp 'execute-extended-cmdr-test-command))
  (should (read-extended-command-for-cmdr-test-prefix-p 'cmdr-test-foo nil))
  (should-not (read-extended-command-for-cmdr-test-prefix-p 'other-foo nil))
  (should-error (macroexpand-1 '(make-read-extended-command-for-prefix "  ")) :type 'user-error)
  (let ((map (make-sparse-keymap)))
    (eval (macroexpand-1 `(make-read-extended-command-for-prefix "cmdr-test"
                            :bind-key "x" :bind-map ',map :key-alias "lbl"))
          t)
    (let ((elt (assq ?x (cdr map))))
      (should (equal (cadr elt) "lbl"))
      (should (eq (cddr elt) 'execute-extended-cmdr-test-command)))))

;;; cmdr-bind-deferred-keymap

(defvar cmdr-test--pkg-dir nil)

(defun cmdr-test--write-package (feature map-symbol map-form)
  "Write FEATURE into `cmdr-test--pkg-dir' defining MAP-SYMBOL as MAP-FORM."
  (with-temp-file (expand-file-name (format "%s.el" feature) cmdr-test--pkg-dir)
    (insert (format ";;; %s.el --- fake -*- lexical-binding: t; -*-\n" feature)
            (format "(defvar %s %s)\n(provide '%s)\n" map-symbol map-form feature))))

(defmacro cmdr-test--with-package-dir (&rest body)
  "Run BODY with a temporary directory on `load-path' for fake packages."
  (declare (indent 0))
  `(let* ((cmdr-test--pkg-dir (make-temp-file "cmdr-test-pkg" t))
          (load-path (cons cmdr-test--pkg-dir load-path))
          (unread-command-events nil))
     (unwind-protect
         (cl-letf (((symbol-function 'this-command-keys-vector)
                    (lambda () [?\C-c ?x])))
           ,@body)
       (delete-directory cmdr-test--pkg-dir t))))

(defun cmdr-test--stand-in (map key)
  "Return the command MAP binds at KEY."
  (lookup-key map (key-parse key)))

(ert-deftest cmdr-macros-test/deferred-macroexpand-without-package ()
  "Expanding and installing never loads the package."
  (let ((expansion (macroexpand-1
                    '(cmdr-bind-deferred-keymap m "x" 'cmdr-test-absent-pkg 'cmdr-test-absent-map "lbl")))
        (map (make-sparse-keymap)))
    (should expansion)
    (should-not (featurep 'cmdr-test-absent-pkg))
    (cmdr-bind-deferred-keymap map "x" 'cmdr-test-absent-pkg 'cmdr-test-absent-map "lbl")
    (should-not (featurep 'cmdr-test-absent-pkg))
    (should (commandp (cmdr-test--stand-in map "x")))))

(ert-deftest cmdr-macros-test/deferred-evaluate-args-once ()
  "Each argument form is evaluated exactly once, at install."
  (let ((counts (list 0 0 0 0 0))
        (map (make-sparse-keymap)))
    (let ((bump (lambda (i value)
                  (cl-incf (nth i counts))
                  value)))
      (cmdr-bind-deferred-keymap (funcall bump 0 map) (funcall bump 1 "x")
                                 (funcall bump 2 'cmdr-test-absent-pkg)
                                 (funcall bump 3 'cmdr-test-absent-map)
                                 (funcall bump 4 "lbl")))
    (should (equal counts '(1 1 1 1 1)))
    (ignore-errors (funcall (cmdr-test--stand-in map "x")))
    (should (equal counts '(1 1 1 1 1)))))

(ert-deftest cmdr-macros-test/deferred-first-use-replaces-and-replays ()
  "First use loads the package, swaps in the real map, keeps the label, replays."
  (cmdr-test--with-package-dir
    (cmdr-test--write-package 'cmdr-test-pkg-a 'cmdr-test-pkg-a-map
                              "(let ((m (make-sparse-keymap))) (keymap-set m \"y\" 'ignore) m)")
    (let ((map (make-sparse-keymap)))
      (cmdr-bind-deferred-keymap map "x" 'cmdr-test-pkg-a 'cmdr-test-pkg-a-map "lbl")
      (should-not (featurep 'cmdr-test-pkg-a))
      (funcall (cmdr-test--stand-in map "x"))
      (should (featurep 'cmdr-test-pkg-a))
      (should (eq (cmdr-test--stand-in map "x") (symbol-value 'cmdr-test-pkg-a-map)))
      (should (equal (cadr (assq ?x (cdr map))) "lbl"))
      (should (equal unread-command-events (list ?\C-c ?x))))))

(ert-deftest cmdr-macros-test/deferred-retry-after-failure ()
  "A load failure keeps the stand-in, does not replay, and can be retried."
  (cmdr-test--with-package-dir
    (let* ((map (make-sparse-keymap))
           (stand-in (cmdr-bind-deferred-keymap map "x" 'cmdr-test-pkg-b 'cmdr-test-pkg-b-map "lbl")))
      (should-error (funcall stand-in) :type 'file-missing)
      (should (eq (cmdr-test--stand-in map "x") stand-in))
      (should-not unread-command-events)
      (cmdr-test--write-package 'cmdr-test-pkg-b 'cmdr-test-pkg-b-map "(make-sparse-keymap)")
      (funcall stand-in)
      (should (eq (cmdr-test--stand-in map "x") (symbol-value 'cmdr-test-pkg-b-map)))
      (should (equal unread-command-events (list ?\C-c ?x))))))

(ert-deftest cmdr-macros-test/deferred-invalid-map-keeps-stand-in ()
  "A package whose symbol is not a keymap leaves the stand-in and does not replay."
  (cmdr-test--with-package-dir
    (cmdr-test--write-package 'cmdr-test-pkg-c 'cmdr-test-pkg-c-map "42")
    (let* ((map (make-sparse-keymap))
           (stand-in (cmdr-bind-deferred-keymap map "x" 'cmdr-test-pkg-c 'cmdr-test-pkg-c-map "lbl")))
      (should-error (funcall stand-in) :type 'user-error)
      (should (eq (cmdr-test--stand-in map "x") stand-in))
      (should-not unread-command-events))))

(ert-deftest cmdr-macros-test/deferred-no-replace-when-rebound ()
  "A binding replaced before first use is left alone; the key is still replayed."
  (cmdr-test--with-package-dir
    (cmdr-test--write-package 'cmdr-test-pkg-d 'cmdr-test-pkg-d-map "(make-sparse-keymap)")
    (let* ((map (make-sparse-keymap))
           (stand-in (cmdr-bind-deferred-keymap map "x" 'cmdr-test-pkg-d 'cmdr-test-pkg-d-map "lbl")))
      (keymap-set map "x" #'ignore)
      (funcall stand-in)
      (should (eq (cmdr-test--stand-in map "x") #'ignore)))))

(ert-deftest cmdr-macros-test/deferred-label-persists ()
  "The stand-in is labeled in the raw keymap, and the label survives the swap."
  (cmdr-test--with-package-dir
    (cmdr-test--write-package 'cmdr-test-pkg-e 'cmdr-test-pkg-e-map "(make-sparse-keymap)")
    (let* ((map (make-sparse-keymap))
           (stand-in (cmdr-bind-deferred-keymap map "x" 'cmdr-test-pkg-e 'cmdr-test-pkg-e-map "lbl")))
      (should (equal (cadr (assq ?x (cdr map))) "lbl"))
      (should (eq (cddr (assq ?x (cdr map))) stand-in))
      (funcall stand-in)
      (should (equal (cadr (assq ?x (cdr map))) "lbl"))
      (should (eq (cddr (assq ?x (cdr map))) (symbol-value 'cmdr-test-pkg-e-map))))))

(defvar cmdr-test--bumps nil)

(defun cmdr-test--bump (&rest args)
  "Record a call with ARGS."
  (push args cmdr-test--bumps))

(ert-deftest cmdr-macros-test/one-shot-hook-body-runs-all-forms ()
  (let ((cmdr-test--hook nil)
        (n 0))
    (add-one-shot-hook :name "body" :hook 'cmdr-test--hook
                       :body ((cl-incf n) (cl-incf n 10)))
    (run-hooks 'cmdr-test--hook)
    (should (= n 11))))

(ert-deftest cmdr-macros-test/one-shot-hook-result-is-evaluated-at-expansion ()
  (let ((cmdr-test--hook nil)
        (n 0))
    (add-one-shot-hook :name "result" :hook 'cmdr-test--hook :result '(cl-incf n 5))
    (run-hooks 'cmdr-test--hook)
    (should (= n 5))))

(ert-deftest cmdr-macros-test/one-shot-hook-operation-with-and-without-args ()
  (let ((cmdr-test--hook nil)
        (cmdr-test--hook-b nil)
        (cmdr-test--bumps nil))
    (add-one-shot-hook :name "op0" :hook 'cmdr-test--hook :operation #'cmdr-test--bump)
    (add-one-shot-hook :name "op1" :hook 'cmdr-test--hook-b :operation #'cmdr-test--bump
                       :args (x &optional y))
    (run-hooks 'cmdr-test--hook)
    (run-hook-with-args 'cmdr-test--hook-b 1 2)
    (should (equal '((1 2) nil) cmdr-test--bumps))))

(ert-deftest cmdr-macros-test/one-shot-hook-function-symbol-with-and-without-args ()
  (let ((cmdr-test--hook nil)
        (cmdr-test--hook-b nil)
        (cmdr-test--bumps nil))
    (add-one-shot-hook :name "fn0" :hook 'cmdr-test--hook :function cmdr-test--bump)
    (add-one-shot-hook :name "fn1" :hook 'cmdr-test--hook-b :function cmdr-test--bump :args (x))
    (run-hooks 'cmdr-test--hook)
    (run-hook-with-args 'cmdr-test--hook-b 7)
    (should (equal '((7) nil) cmdr-test--bumps))))

(ert-deftest cmdr-macros-test/one-shot-hook-function-sharp-quote-calls-function ()
  "Regression: `:function #\\='sym' calls SYM instead of evaluating to it."
  (let ((cmdr-test--hook nil)
        (cmdr-test--bumps nil))
    (add-one-shot-hook :name "sharp" :hook 'cmdr-test--hook :function #'cmdr-test--bump)
    (run-hooks 'cmdr-test--hook)
    (should (equal '(nil) cmdr-test--bumps))))

(ert-deftest cmdr-macros-test/one-shot-hook-function-call-form-is-evaluated ()
  (let ((cmdr-test--hook nil)
        (n 0))
    (add-one-shot-hook :name "form-fn" :hook 'cmdr-test--hook :function (cl-incf n))
    (run-hooks 'cmdr-test--hook)
    (should (= n 1))))

(ert-deftest cmdr-macros-test/one-shot-hook-cleanup-and-make-unique-unintern ()
  (let ((cmdr-test--hook nil)
        (cmdr-test--hook-b nil))
    (add-one-shot-hook :name "clean" :hook 'cmdr-test--hook :cleanup t :form (ignore))
    (add-one-shot-hook :name "uniq" :hook 'cmdr-test--hook-b :make-unique t :form (ignore))
    (let ((names (list (symbol-name (car cmdr-test--hook))
                       (symbol-name (car cmdr-test--hook-b)))))
      (should (seq-every-p #'intern-soft names))
      (run-hooks 'cmdr-test--hook 'cmdr-test--hook-b)
      (should-not (seq-some #'intern-soft names)))))

(ert-deftest cmdr-macros-test/one-shot-hook-computed-hook-expression ()
  "A hook given as an expression is evaluated, and quoted lists are accepted."
  (let ((cmdr-test--hook nil)
        (cmdr-test--hook-b nil)
        (n 0))
    (add-one-shot-hook :name "computed" :hook (list 'cmdr-test--hook) :form (cl-incf n))
    (add-one-shot-hook :name "quoted" :hook '(cmdr-test--hook-b) :form (cl-incf n))
    (run-hooks 'cmdr-test--hook 'cmdr-test--hook-b)
    (should (= n 2))
    (should-not (or cmdr-test--hook cmdr-test--hook-b))))

(ert-deftest cmdr-macros-test/one-shot-hook-first-frame-sentinel ()
  "The `after-first-frame-created' sentinel resolves per daemon status."
  (let ((window-setup-hook nil)
        (server-after-make-frame-hook nil)
        (n 0))
    (add-one-shot-hook :name "frame" :hook after-first-frame-created :form (cl-incf n))
    (if (daemonp)
        (should server-after-make-frame-hook)
      (should window-setup-hook))))

(ert-deftest cmdr-macros-test/resolve-hooks-rejects-bad-input ()
  (should-error (cmdr--resolve-hooks '(a "b")) :type 'user-error)
  (should-error (cmdr--resolve-hooks 5) :type 'user-error)
  (should (equal '(a b) (cmdr--resolve-hooks '(a b)))))

(ert-deftest cmdr-macros-test/toggle-functions-require-key-with-keymap ()
  (should-error (macroexpand-1 '(create-toggle-functions some-var :keymap some-map))
                :type 'user-error))

(ert-deftest cmdr-macros-test/extended-command-default-key-alias ()
  "Without KEY-ALIAS the binding is labelled \"PREFIX-commands\" in `global-map' by default."
  (let ((form (macroexpand-1 '(make-read-extended-command-for-prefix "zzz" :bind-key "x"))))
    (should (string-match-p "global-map" (format "%S" form)))
    (should (string-match-p "zzz-commands" (format "%S" form)))))

(ert-deftest cmdr-macros-test/extended-command-runs-with-prefix-predicate ()
  (let ((seen nil))
    (cl-letf (((symbol-function 'execute-extended-command)
               (lambda (&rest _) (setq seen read-extended-command-predicate))))
      (execute-extended-cmdr-test-command))
    (should (eq seen 'read-extended-command-for-cmdr-test-prefix-p))))

(ert-deftest cmdr-macros-test/deferred-nested-key-sequence ()
  "A multi-key sequence is bound beneath its prefix and swapped in place."
  (cmdr-test--with-package-dir
    (cmdr-test--write-package 'cmdr-test-pkg-e 'cmdr-test-pkg-e-map "(make-sparse-keymap)")
    (let* ((map (make-sparse-keymap))
           (stand-in (cmdr-bind-deferred-keymap map "C-c e" 'cmdr-test-pkg-e 'cmdr-test-pkg-e-map "E")))
      (should (eq stand-in (lookup-key map (kbd "C-c e"))))
      (funcall stand-in)
      (should (eq (symbol-value 'cmdr-test-pkg-e-map) (lookup-key map (kbd "C-c e"))))
      (should (equal "E" (cadr (assq ?e (cdr (lookup-key map (kbd "C-c"))))))))))

(ert-deftest cmdr-macros-test/deferred-second-use-after-swap-is-not-redirected ()
  "Calling the stand-in again after the swap leaves the real map in place."
  (cmdr-test--with-package-dir
    (cmdr-test--write-package 'cmdr-test-pkg-f 'cmdr-test-pkg-f-map "(make-sparse-keymap)")
    (let* ((map (make-sparse-keymap))
           (stand-in (cmdr-bind-deferred-keymap map "x" 'cmdr-test-pkg-f 'cmdr-test-pkg-f-map "F")))
      (funcall stand-in)
      (funcall stand-in)
      (should (eq (symbol-value 'cmdr-test-pkg-f-map) (cmdr-test--stand-in map "x"))))))

(provide 'test-cmdr-macros)
;;; test-cmdr-macros.el ends here
