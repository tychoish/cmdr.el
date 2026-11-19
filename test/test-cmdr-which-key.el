;;; test-cmdr-which-key.el --- Cmdr Which Key integration tests -*- lexical-binding: t; -*-
;;; Commentary:
;; Tests for Cmdr's Which Key lifecycle.
;;; Code:
(require 'ert)
(require 'cmdr-mode)
(ert-deftest cmdr/which-key-enabled-by-mode ()
  (let ((cmdr-which-key-integration t))
    (unwind-protect
        (progn (which-key-mode -1) (cmdr-mode 1) (should which-key-mode))
      (cmdr-mode -1))))
(ert-deftest cmdr/which-key-opt-out ()
  (let ((cmdr-which-key-integration nil))
    (unwind-protect
        (progn (which-key-mode -1) (cmdr-mode 1) (should-not which-key-mode))
      (cmdr-mode -1))))

(ert-deftest cmdr/mode-managed-settings-lifecycle ()
  "Enabling cmdr-mode applies managed settings and disabling reverts them."
  (let ((which-key-idle-delay 10.0)
        (cmdr-mode nil))
    (unwind-protect
        (progn
          (cmdr-mode 1)
          (should (= which-key-idle-delay 0.25))
          (cmdr-mode -1)
          (should (= which-key-idle-delay 10.0)))
      (when cmdr-mode (cmdr-mode -1)))))

(provide 'test-cmdr-which-key)
;;; test-cmdr-which-key.el ends here
