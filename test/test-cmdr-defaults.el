;;; test-cmdr-defaults.el --- Tests for optional default bindings -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'cmdr-keys)

(ert-deftest cmdr-defaults-test/load-does-not-install ()
  (let ((cmdr-mode-map (make-sparse-keymap)))
    (load "cmdr-defaults" nil t)
    (should (equal cmdr-mode-map (make-sparse-keymap)))))

(ert-deftest cmdr-defaults-test/install-is-idempotent ()
  (require 'cmdr-defaults)
  (let ((cmdr-mode-map (make-sparse-keymap)))
    (cmdr-defaults-install)
    (let ((snapshot (copy-tree cmdr-mode-map)))
      (cmdr-defaults-install)
      (should (equal snapshot cmdr-mode-map)))
    (should (eq (keymap-lookup cmdr-mode-map "C-x .") #'cmdr-menu))
    (should (eq (keymap-lookup cmdr-mode-map "C-x ,") #'cmdr-dispatch))
    (should (eq (keymap-lookup cmdr-mode-map "C-c f =") #'text-scale-increase))
    (should (eq (keymap-lookup cmdr-mode-map "M-<up>") #'move-text-up))))

(ert-deftest cmdr-defaults-test/plugins-install-together ()
  (require 'cmdr-defaults)
  (require 'cmdr-ecosystem)
  (let ((cmdr-mode-map (make-sparse-keymap)))
    (cmdr-defaults-install)
    (cmdr-ecosystem-install)
    (cmdr-defaults-install)
    (cmdr-ecosystem-install)
    (should (eq (keymap-lookup cmdr-mode-map "C-c h v") #'describe-variable))
    (should (eq (keymap-lookup cmdr-mode-map "C-c h i") #'consult-info))
    (should (eq (keymap-lookup cmdr-mode-map "C-x C-b d") #'kill-buffers-in-directory))
    (should (eq (keymap-lookup cmdr-mode-map "C-x C-b r") #'revbufs))))

(provide 'test-cmdr-defaults)
;;; test-cmdr-defaults.el ends here
