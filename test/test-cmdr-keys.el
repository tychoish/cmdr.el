;;; test-cmdr-keys.el --- Tests for cmdr-keys and cmdr-mode -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for annotated prefixes, full-sequence binding, and the
;; map-attaching `cmdr-mode'.  which-key is deliberately not required.

;;; Code:

(require 'ert)

(let ((root (file-name-directory
             (directory-file-name
              (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path root))

(defvar cmdr-test-keys--global-snapshot (copy-tree global-map)
  "Copy of `global-map' taken before cmdr-keys and cmdr-mode load.")

(require 'cmdr-keys)
(require 'cmdr-mode)

;; Help and `where-is-internal' machinery in other tests can touch
;; `global-map' on their own, so loading is checked here, at load time.
(defvar cmdr-test-keys--load-left-global-map-intact
  (equal cmdr-test-keys--global-snapshot global-map)
  "Non-nil when requiring cmdr-keys and cmdr-mode left `global-map' equal.")

(defun cmdr-test-keys--raw-label (map key)
  "Return the native menu-item label stored for single KEY in MAP."
  (let ((binding (assq (aref (key-parse key) 0) (cdr map))))
    (when (and binding (consp (cdr binding)) (stringp (cadr binding)))
      (cadr binding))))

(defmacro cmdr-test-keys--with-clean-root (&rest body)
  "Run BODY with an empty `cmdr-mode-map', disabled, restoring afterwards."
  (declare (indent 0))
  `(unwind-protect
       (progn
         (cmdr-mode -1)
         (setcdr cmdr-mode-map nil)
         ,@body)
     (cmdr-mode -1)
     (setcdr cmdr-mode-map nil)))

(defmacro cmdr-test-keys--with-global (bindings &rest body)
  "Run BODY with a throwaway current global map holding BINDINGS."
  (declare (indent 1))
  `(let ((old (current-global-map))
         (fresh (make-sparse-keymap)))
     (seq-do (lambda (pair) (keymap-set fresh (car pair) (cdr pair))) ,bindings)
     (unwind-protect
         (progn (use-global-map fresh) ,@body)
       (use-global-map old))))

;;; Prefix creation and labels

(ert-deftest cmdr-keys-test/prefix-creates-full-sequence ()
  (let ((map (make-sparse-keymap)))
    (cmdr-keys-prefix map "C-c a b" "Deep")
    (should (keymapp (keymap-lookup map "C-c")))
    (should (keymapp (keymap-lookup map "C-c a")))
    (should (keymapp (keymap-lookup map "C-c a b")))
    (should (equal "Deep" (cmdr-test-keys--raw-label
                           (keymap-lookup map "C-c a") "b")))))

(ert-deftest cmdr-keys-test/label-is-native-menu-item ()
  (let ((map (make-sparse-keymap)))
    (cmdr-keys-prefix map "C-c a" "Apps")
    (should (equal "Apps" (cmdr-test-keys--raw-label (keymap-lookup map "C-c") "a")))
    (should (keymapp (keymap-lookup map "C-c a")))))

(ert-deftest cmdr-keys-test/nested-annotations-and-where-is ()
  (let ((map (make-sparse-keymap)))
    (cmdr-keys-prefix map "C-c a" "Apps")
    (cmdr-keys-prefix map "C-c a b" "Browse")
    (cmdr-keys-set map "C-c a b f" #'ignore)
    (should (eq #'ignore (keymap-lookup map "C-c a b f")))
    (should (equal "Apps" (cmdr-test-keys--raw-label (keymap-lookup map "C-c") "a")))
    (should (equal "Browse" (cmdr-test-keys--raw-label (keymap-lookup map "C-c a") "b")))
    (should (member (key-parse "C-c a b f") (where-is-internal #'ignore map)))))

(ert-deftest cmdr-keys-test/help-describes-labelled-prefix ()
  (let ((map (make-sparse-keymap)))
    (cmdr-keys-prefix map "C-c a" "Apps")
    (cmdr-keys-set map "C-c a f" #'ignore)
    (with-temp-buffer
      (use-local-map map)
      (let* ((subject (current-buffer))
             (out (generate-new-buffer " *cmdr-help*")))
        (unwind-protect
            (with-current-buffer out
              (describe-buffer-bindings subject)
              (should (string-match-p "C-c a f" (buffer-string))))
          (kill-buffer out))))))

(ert-deftest cmdr-keys-test/prefix-reuses-existing-submap ()
  (let* ((map (make-sparse-keymap))
         (sub (make-sparse-keymap)))
    (keymap-set map "C-c" sub)
    (keymap-set sub "x" #'ignore)
    (cmdr-keys-prefix map "C-c" "Mine")
    (should (equal "Mine" (cmdr-test-keys--raw-label map "C-c")))
    (should (eq sub (keymap-lookup map "C-c")))
    (should (eq #'ignore (keymap-lookup map "C-c x")))))

(ert-deftest cmdr-keys-test/prefix-conflict-with-command ()
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c a" #'ignore)
    (should-error (cmdr-keys-prefix map "C-c a b" "X") :type 'user-error)
    (should-error (cmdr-keys-set map "C-c a b" #'ignore) :type 'user-error)
    (should (eq #'ignore (keymap-lookup map "C-c a")))))

(ert-deftest cmdr-keys-test/prefix-onto-command-errors ()
  (let ((map (make-sparse-keymap)))
    (keymap-set map "C-c a" #'ignore)
    (should-error (cmdr-keys-prefix map "C-c a" "X") :type 'user-error)))

(ert-deftest cmdr-keys-test/malformed-sequence ()
  (let ((map (make-sparse-keymap)))
    (should-error (cmdr-keys-set map "C-c  <<bad" #'ignore) :type 'user-error)
    (should-error (cmdr-keys-set map "" #'ignore) :type 'user-error)
    (should-error (cmdr-keys-prefix map "" "X") :type 'user-error)))

(ert-deftest cmdr-keys-test/idempotent-configuration ()
  (let ((map (make-sparse-keymap)))
    (cmdr-keys-prefix map "C-c a" "Apps")
    (cmdr-keys-set map "C-c a f" #'ignore)
    (let ((snapshot (copy-tree map)))
      (cmdr-keys-prefix map "C-c a" "Apps")
      (cmdr-keys-set map "C-c a f" #'ignore)
      (should (equal snapshot map)))))

(ert-deftest cmdr-keys-test/relabel-keeps-submap ()
  (let ((map (make-sparse-keymap)))
    (cmdr-keys-prefix map "C-c a" "Old")
    (cmdr-keys-set map "C-c a f" #'ignore)
    (cmdr-keys-prefix map "C-c a" "New")
    (should (equal "New" (cmdr-test-keys--raw-label (keymap-lookup map "C-c") "a")))
    (should (eq #'ignore (keymap-lookup map "C-c a f")))))

(ert-deftest cmdr-keys-test/prefix-rejects-non-string-label ()
  "A prefix label must be a string, and the map is left untouched."
  (let ((map (make-sparse-keymap)))
    (should-error (cmdr-keys-prefix map "C-c a" nil) :type 'user-error)
    (should-error (cmdr-keys-prefix map "C-c a" 'label) :type 'user-error)
    (should-not (lookup-key map (kbd "C-c")))))

(ert-deftest cmdr-keys-test/set-refuses-to-replace-a-prefix ()
  "Binding a command onto an existing prefix signals and keeps the prefix."
  (let ((map (make-sparse-keymap)))
    (cmdr-keys-prefix map "C-c a" "Apps")
    (should-error (cmdr-keys-set map "C-c a" #'ignore) :type 'user-error)
    (should (keymapp (lookup-key map (kbd "C-c a"))))))

(ert-deftest cmdr-keys-test/set-creates-intermediate-prefixes-and-rebinds ()
  "Setting a full sequence creates parents; setting it again replaces the command."
  (let ((map (make-sparse-keymap)))
    (cmdr-keys-set map "C-c a b" #'ignore)
    (should (keymapp (lookup-key map (kbd "C-c a"))))
    (cmdr-keys-set map "C-c a b" #'next-line)
    (should (eq #'next-line (lookup-key map (kbd "C-c a b"))))))

(ert-deftest cmdr-keys-test/prefix-on-sparse-label-update-keeps-bindings ()
  "Relabelling a prefix keeps commands already bound beneath it."
  (let ((map (make-sparse-keymap)))
    (cmdr-keys-set map "C-c a b" #'ignore)
    (cmdr-keys-prefix map "C-c a" "Apps")
    (cmdr-keys-prefix map "C-c a" "Applications")
    (should (eq #'ignore (lookup-key map (kbd "C-c a b"))))
    (should (equal "Applications" (cadr (assq ?a (cdr (lookup-key map (kbd "C-c")))))))))

(ert-deftest cmdr-keys-test/reload-idempotent ()
  (cmdr-test-keys--with-clean-root
    (cmdr-keys-prefix cmdr-mode-map "C-c a" "Apps")
    (let ((root cmdr-mode-map)
          (snapshot (copy-tree cmdr-mode-map)))
      (load (locate-library "cmdr-keys.el") nil t)
      (should (eq root cmdr-mode-map))
      (should (equal snapshot cmdr-mode-map)))))

;;; Mode

(ert-deftest cmdr-mode-test/root-map-has-doc ()
  (should (keymapp cmdr-mode-map))
  (should (stringp (documentation-property 'cmdr-mode-map 'variable-documentation))))

(ert-deftest cmdr-mode-test/enable-disable-reveals-prior-binding ()
  (cmdr-test-keys--with-global '(("C-c y" . ignore))
    (cmdr-test-keys--with-clean-root
      (cmdr-keys-set cmdr-mode-map "C-c y" #'next-line)
      (should (eq #'ignore (key-binding (kbd "C-c y"))))
      (cmdr-mode 1)
      (should (eq #'next-line (key-binding (kbd "C-c y"))))
      (cmdr-mode -1)
      (should (eq #'ignore (key-binding (kbd "C-c y")))))))

(ert-deftest cmdr-mode-test/configure-while-disabled-and-reenable ()
  (cmdr-test-keys--with-global nil
    (cmdr-test-keys--with-clean-root
      (cmdr-keys-set cmdr-mode-map "C-c y" #'next-line)
      (should-not (key-binding (kbd "C-c y")))
      (cmdr-mode 1)
      (cmdr-mode -1)
      (cmdr-keys-set cmdr-mode-map "C-c z" #'previous-line)
      (should-not (key-binding (kbd "C-c z")))
      (cmdr-mode 1)
      (should (eq #'previous-line (key-binding (kbd "C-c z")))))))

(ert-deftest cmdr-mode-test/global-map-unchanged ()
  (should cmdr-test-keys--load-left-global-map-intact)
  (let ((before (copy-tree global-map)))
    (cmdr-test-keys--with-clean-root
      (cmdr-keys-prefix cmdr-mode-map "C-c a" "Apps")
      (cmdr-keys-set cmdr-mode-map "C-c a f" #'ignore)
      (cmdr-mode 1)
      (cmdr-mode -1)
      (should (equal before global-map)))))

(ert-deftest cmdr-keys-test/which-key-not-required ()
  (should-not (featurep 'which-key)))

(provide 'test-cmdr-keys)
;;; test-cmdr-keys.el ends here
