;;; test-cmdr-ecosystem.el --- Tests for optional ecosystem bindings -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'cmdr-keys)

(ert-deftest cmdr-ecosystem-test/load-does-not-install ()
  (let ((cmdr-mode-map (make-sparse-keymap)))
    (load "cmdr-ecosystem" nil t)
    (should (equal cmdr-mode-map (make-sparse-keymap)))))

(ert-deftest cmdr-ecosystem-test/install-is-idempotent ()
  (require 'cmdr-ecosystem)
  (let ((cmdr-mode-map (make-sparse-keymap)))
    (cmdr-ecosystem-install)
    (let ((snapshot (copy-tree cmdr-mode-map)))
      (cmdr-ecosystem-install)
      (should (equal snapshot cmdr-mode-map)))
    (should (commandp (keymap-lookup cmdr-mode-map "C-c C-d")))
    (should (commandp (keymap-lookup cmdr-mode-map "C-c p")))
    (should (eq (keymap-lookup cmdr-mode-map "C-x x c") #'execute-extended-clipboard-command))
    (should (eq (keymap-lookup cmdr-mode-map "C-x g s") #'magit-status))
    (should (eq (keymap-lookup cmdr-mode-map "C-x d c") #'docker-containers))
    (should (eq (keymap-lookup cmdr-mode-map "C-c s e e") #'eat))
    (should (eq (keymap-lookup cmdr-mode-map "C-c q r") #'anzu-query-replace))))

(provide 'test-cmdr-ecosystem)
;;; test-cmdr-ecosystem.el ends here
