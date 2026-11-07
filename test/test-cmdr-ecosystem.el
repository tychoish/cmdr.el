;;; test-cmdr-ecosystem.el --- Tests for the ecosystem specs -*- lexical-binding: t; -*-

;;; Code:
(require 'ert)
(require 'cmdr-keys)
(require 'cmdr-ecosystem)

(ert-deftest cmdr-ecosystem-test/load-does-not-install ()
  (let ((cmdr-mode-map (make-sparse-keymap)))
    (load "cmdr-ecosystem" nil t)
    (should (equal cmdr-mode-map (make-sparse-keymap)))))

(ert-deftest cmdr-ecosystem-test/install-is-idempotent ()
  (let ((cmdr-mode-map (make-sparse-keymap))
        (names '(clipboard magit smerge docker shell eat anzu dape projectile)))
    (seq-do #'cmdr-spec-install names)
    (let ((snapshot (copy-keymap cmdr-mode-map)))
      (seq-do #'cmdr-spec-install names)
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
