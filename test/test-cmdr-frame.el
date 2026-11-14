;;; test-cmdr-frame.el --- Tests for cmdr frame management -*- lexical-binding: t; -*-

(require 'ert)
(require 'cmdr-ops)
(require 'cmdr-frame)

(ert-deftest cmdr-frame-test/disable-all-themes ()
  (let ((disabled nil)
        (custom-enabled-themes '(modus-operandi leuven)))
    (cl-letf (((symbol-function 'disable-theme)
               (lambda (theme) (push theme disabled))))
      (disable-all-themes)
      (should (equal '(leuven modus-operandi) disabled)))))

(ert-deftest cmdr-frame-test/ensure-frame-opacity-sets-default ()
  (let ((default-frame-alist nil)
        (frame-params nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t))
              ((symbol-function 'frame-parameter) (lambda (&rest _) nil))
              ((symbol-function 'set-frame-parameter)
               (lambda (_ param val) (push (cons param val) frame-params))))
      (ensure-frame-opacity 90)
      (should (equal 90 (alist-get 'alpha default-frame-alist)))
      (should (equal 90 (alist-get 'alpha frame-params))))))

(ert-deftest cmdr-frame-test/ensure-frame-font-sets-default ()
  (let ((default-frame-alist nil)
        (applied-font nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t))
              ((symbol-function 'set-frame-font)
               (lambda (font &rest _) (setq applied-font font))))
      (ensure-frame-font "Source Code Pro" 13)
      (should (equal "Source Code Pro-13" (alist-get 'font default-frame-alist)))
      (should (equal "Source Code Pro-13" applied-font))
      ;; Idempotent
      (ensure-frame-font "Other Font" 15)
      (should (equal "Source Code Pro-13" (alist-get 'font default-frame-alist))))))

(ert-deftest cmdr-frame-test/contextual-menubar ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'set-frame-parameter)
               (lambda (frame param val) (push (list frame param val) calls)))
              ((symbol-function 'display-graphic-p)
               (lambda (frame) (eq frame 'gui))))
      (contextual-menubar 'gui)
      (contextual-menubar 'tty)
      (should (equal '((gui menu-bar-lines 1)
                       (tty menu-bar-lines 0))
                     (reverse calls))))))

(ert-deftest cmdr-frame-test/frame-unset-background-for-tty ()
  (let ((bg nil) (fg nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&rest _) nil))
              ((symbol-function 'set-face-background)
               (lambda (_face color _frame) (setq bg color)))
              ((symbol-function 'set-face-foreground)
               (lambda (_face color _frame) (setq fg color))))
      (frame-unset-background-for-tty 'dummy-frame)
      (should (equal "unspecified-bg" bg))
      (should (equal "unspecified-fg" fg)))))

(ert-deftest cmdr-frame-test/cmdr-frame-mode-hooks ()
  (let ((was-on (bound-and-true-p cmdr-frame-mode)))
    (unwind-protect
        (progn
          (cmdr-frame-mode 1)
          (should (memq #'cmdr-frame--setup-frame after-make-frame-functions))
          (should (memq #'cmdr-frame--reapply-after-theme enable-theme-functions))
          (should (member cmdr-frame-readonly-display-buffer-entry display-buffer-alist))
          (cmdr-frame-mode -1)
          (should-not (memq #'cmdr-frame--setup-frame after-make-frame-functions))
          (should-not (memq #'cmdr-frame--reapply-after-theme enable-theme-functions))
          (should-not (member cmdr-frame-readonly-display-buffer-entry display-buffer-alist)))
      (if was-on
          (cmdr-frame-mode 1)
        (cmdr-frame-mode -1)))))

(ert-deftest cmdr-frame-test/buffer-home-frame ()
  (let ((cmdr-frame--buffer-home-frame nil)
        (frame (selected-frame)))
    (cmdr-frame-record-home-frame)
    (should (eq frame cmdr-frame--buffer-home-frame))))

(ert-deftest cmdr-frame-test/buffer-predicate ()
  (let ((b-scratch (generate-new-buffer "*cmdr-scratch*"))
        (b-file (generate-new-buffer "cmdr-file.txt")))
    (unwind-protect
        (progn
          (with-current-buffer b-file
            (setq buffer-file-name "/tmp/cmdr-file.txt"))
          (with-current-buffer b-scratch
            ;; Non-file buffer has no restriction
            (should (cmdr-frame-buffer-predicate b-file))
            (should (cmdr-frame-buffer-predicate b-scratch))))
      (kill-buffer b-scratch)
      (kill-buffer b-file))))

(ert-deftest cmdr-frame-test/readonly-file-buffer-p ()
  (let ((b-read (generate-new-buffer "*cmdr-read*"))
        (b-write (generate-new-buffer "*cmdr-write*")))
    (unwind-protect
        (progn
          (with-current-buffer b-read
            (setq buffer-file-name "/tmp/foo.txt"
                  buffer-read-only t))
          (with-current-buffer b-write
            (setq buffer-file-name "/tmp/bar.txt"
                  buffer-read-only nil))
          (should (cmdr-frame-readonly-file-buffer-p b-read nil))
          (should-not (cmdr-frame-readonly-file-buffer-p b-write nil)))
      (kill-buffer b-read)
      (kill-buffer b-write))))

(ert-deftest cmdr-frame-test/contextual-menubar-defaults-by-system ()
  (should (equal (eq system-type 'darwin) cmdr-frame-contextual-menubar)))

(ert-deftest cmdr-frame-test/ensure-frame-opacity-overrides-stale-default ()
  (let ((default-frame-alist '((alpha . 20)))
        (frame-params nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t))
              ((symbol-function 'set-frame-parameter)
               (lambda (_ param val) (push (cons param val) frame-params))))
      (ensure-frame-opacity 95)
      (should (equal 95 (alist-get 'alpha default-frame-alist)))
      (should (equal 95 (alist-get 'alpha frame-params))))))

(ert-deftest cmdr-frame-test/ensure-frame-opacity-handles-minor-mode-1-arg ()
  (let ((default-frame-alist nil)
        (frame-params nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t))
              ((symbol-function 'set-frame-parameter)
               (lambda (_ param val) (push (cons param val) frame-params))))
      (ensure-frame-opacity 1)
      (should (equal 95 (alist-get 'alpha default-frame-alist)))
      (should (equal 95 (alist-get 'alpha frame-params))))))

(ert-deftest cmdr-frame-test/ensure-frame-opacity-handles-float ()
  (let ((default-frame-alist nil)
        (frame-params nil))
    (cl-letf (((symbol-function 'display-graphic-p) (lambda (&optional _) t))
              ((symbol-function 'set-frame-parameter)
               (lambda (_ param val) (push (cons param val) frame-params))))
      (ensure-frame-opacity 0.90)
      (should (equal 90 (alist-get 'alpha default-frame-alist)))
      (should (equal 90 (alist-get 'alpha frame-params))))))

(ert-deftest cmdr-frame-test/tychoish-aliases-are-absent ()
  (seq-do (lambda (sym)
            (should-not (fboundp sym))
            (should-not (boundp sym)))
          '(tychoish--record-home-frame
            tychoish--frame-buffer-predicate
            tychoish--install-buffer-predicate
            tychoish--readonly-file-buffer-p
            tychoish--reuse-readonly-file-window
            tychoish--buffer-home-frame
            tychoish--readonly-display-buffer-entry)))

(provide 'test-cmdr-frame)
;;; test-cmdr-frame.el ends here
