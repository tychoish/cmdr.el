;;; test-cmdr-defaults.el --- Tests for the defaults spec -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'cmdr-keys)
(require 'cmdr-defaults)
(require 'cmdr-ecosystem)

(ert-deftest cmdr-defaults-test/load-does-not-install ()
  (let ((cmdr-mode-map (make-sparse-keymap)))
    (load "cmdr-defaults" nil t)
    (should (equal cmdr-mode-map (make-sparse-keymap)))))

(ert-deftest cmdr-defaults-test/install-is-idempotent ()
  (let ((cmdr-mode-map (make-sparse-keymap)))
    (cmdr-spec-install 'defaults)
    (let ((snapshot (copy-tree cmdr-mode-map)))
      (cmdr-spec-install 'defaults)
      (should (equal snapshot cmdr-mode-map)))
    (should (eq (keymap-lookup cmdr-mode-map "C-x .") #'cmdr-menu))
    (should (eq (keymap-lookup cmdr-mode-map "C-x ,") #'cmdr-dispatch))
    (should (eq (keymap-lookup cmdr-mode-map "C-c f =") #'text-scale-increase))
    (should (eq (keymap-lookup cmdr-mode-map "M-<up>") #'move-text-up))))

(ert-deftest cmdr-defaults-test/specs-install-together ()
  (let ((cmdr-mode-map (make-sparse-keymap)))
    (seq-do #'cmdr-spec-install '(defaults consult revbufs defaults consult revbufs))
    (should (eq (keymap-lookup cmdr-mode-map "C-c h v") #'describe-variable))
    (should (eq (keymap-lookup cmdr-mode-map "C-c h i") #'consult-info))
    (should (eq (keymap-lookup cmdr-mode-map "C-x C-b d") #'kill-buffers-in-directory))
    (should (eq (keymap-lookup cmdr-mode-map "C-x C-b r") #'revbufs))))

(provide 'test-cmdr-defaults)
;;; test-cmdr-defaults.el ends here
