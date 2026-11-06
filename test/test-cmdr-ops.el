;;; test-cmdr-ops.el --- Tests for cmdr-ops -*- lexical-binding: t; -*-

;;; Commentary:
;; Tests for the reusable interactive operations.  Nothing here relies on
;; xtd-macro, hud, early-init, or any personal configuration.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(let ((root (file-name-directory
             (directory-file-name
              (file-name-directory (or load-file-name buffer-file-name))))))
  (add-to-list 'load-path root))

(require 'cmdr-ops)

(defconst cmdr-ops-test--commands
  '(save-all-buffers kill-buffers-in-directory buffers-matching-path
    buffers-matching-mode kill-eldoc-and-help-buffers move-text-up
    move-text-down toggle-word-wrap turn-on-soft-wrap turn-off-soft-wrap
    turn-on-hard-wrap turn-off-hard-wrap unfill-region uniquify-region-lines
    uniquify-buffer-lines set-tab-width text-scale-reset jump-to-elisp-help
    toggle-local-whitespace-cleanup pin-buffer-to-window-toggle
    increase-window-up increase-window-down increase-window-left
    increase-window-right frame-opacity-increase frame-opacity-decrease
    frame-opacity-reset insert-current-date buffer-line-count buffer-directory
    clean-kill-ring-clean kill-buffers-matching-name
    force-kill-buffers-matching-path kill-buffers-matching-path
    kill-all-reference-and-source-buffers kill-buffers-matching-mode
    kill-buffers-visiting-missing-files kill-buffer-and-delete-file)
  "Public entry points that must carry an autoload cookie.")

(defun cmdr-ops-test--call-with-dir (fn)
  "Call FN with a fresh temporary directory, then delete it and its buffers."
  (let ((dir (file-name-as-directory (make-temp-file "cmdr-ops-" t))))
    (unwind-protect
        (funcall fn dir)
      (let ((kill-buffer-query-functions nil))
        (seq-do (lambda (buf)
                  (when (and (buffer-file-name buf)
                             (file-in-directory-p (buffer-file-name buf) dir))
                    (set-buffer-modified-p nil)
                    (kill-buffer buf)))
                (buffer-list)))
      (delete-directory dir t))))

(defun cmdr-ops-test--visit (dir name)
  "Create file NAME under DIR and return a buffer visiting it."
  (let ((file (expand-file-name name dir)))
    (write-region "x\n" nil file nil 'silent)
    (find-file-noselect file)))

;;; Independence and load hygiene

(ert-deftest cmdr-ops-test/no-forbidden-dependencies ()
  "The source requires no xtd-macro, hud, cmdr-macros, or optional feature."
  (with-temp-buffer
    (insert-file-contents (locate-library "cmdr-ops.el"))
    (should-not (re-search-forward "(require '\\(xtd\\|hud\\|cmdr-macros\\)" nil t))
    (goto-char (point-min))
    (should-not (re-search-forward "(require '[^ )]+ nil" nil t))
    (goto-char (point-min))
    (should-not (re-search-forward "with-slow-op-timer\\|f-ancestor-of-p" nil t))))

(ert-deftest cmdr-ops-test/entry-points-are-autoloaded ()
  (let ((source (with-temp-buffer
                  (insert-file-contents (locate-library "cmdr-ops.el"))
                  (buffer-string))))
    (seq-do (lambda (name)
              (should (string-match-p
                       (format ";;;###autoload\n(\\(defun\\|defalias '\\)%s[ )]"
                               (regexp-quote (symbol-name name)))
                       source)))
            (seq-remove (lambda (name)
                          (eq name 'kill-buffers-matching-name))
                        cmdr-ops-test--commands))
    (should (string-match-p ";;;###autoload\n(defalias 'kill-buffers-matching-name"
                            source))))

(ert-deftest cmdr-ops-test/old-and-dangling-names-are-absent ()
  (seq-do (lambda (name)
            (should-not (fboundp name)))
          '(hud-insert-date hud-increase-window-up hud-increase-window-down
            hud-increase-window-left hud-increase-window-right
            hud-opacity-increase hud-opacity-decrease hud-opacity-reset
            djcb-opacity-modify hud--minimize-path hud-set-tab-width
            hud-run-current-major-mode-hooks hud-describe-symbol-dwim
            toggle-soft-wrap toggle-hard-wrap font-lock-show-tabs
            font-lock-width-keyword ad:kill-new-reject-empty)))

(ert-deftest cmdr-ops-test/load-installs-no-hooks-advice-rules-or-modes ()
  "Loading cmdr-ops in a clean process changes no policy state."
  (let* ((root (file-name-directory
                (directory-file-name
                 (file-name-directory (locate-library "cmdr-ops.el")))))
         (exe (expand-file-name invocation-name invocation-directory))
         (form
          (prin1-to-string
           '(progn
              (require 'seq)
              (require 'map)
              (require 'subr-x)
              (defun cmdr-snap ()
                (list
                 (let (acc)
                   (mapatoms
                    (lambda (s)
                      (when (and (boundp s)
                                 (string-match-p "-\\(hook\\|functions\\)\\'"
                                                 (symbol-name s)))
                        (push (cons s (copy-tree (symbol-value s))) acc))))
                   (sort acc (lambda (a b)
                               (string< (symbol-name (car a))
                                        (symbol-name (car b))))))
                 (let (acc)
                   (mapatoms
                    (lambda (s)
                      (when (and (fboundp s) (advice--p (symbol-function s)))
                        (push s acc))))
                   (sort acc #'string<))
                 (copy-tree display-buffer-alist)
                 (copy-tree display-buffer-base-action)
                 (copy-tree global-minor-modes)
                 (copy-tree minor-mode-list)
                 (copy-tree kill-ring)))
              (let ((before (cmdr-snap)))
                (require 'cmdr-ops)
                (let ((after (cmdr-snap)))
                  (prin1
                   (list
                    (equal (nth 0 before) (nth 0 after))
                    (equal (nth 1 before) (nth 1 after))
                    (equal (nth 2 before) (nth 2 after))
                    (equal (nth 3 before) (nth 3 after))
                    (equal (nth 4 before) (nth 4 after))
                    (equal (nth 5 before) (nth 5 after))
                    (equal (nth 6 before) (nth 6 after))
                    (advice-member-p 'ad:kill-new-reject-empty 'kill-new)
                    (seq-filter #'featurep
                                '(xtd-macro hud hud-mode cmdr-macros
                                  cmdr-mode cmdr-keys f ht))))))))))
    (with-temp-buffer
      (let ((status (call-process exe nil t nil "--batch" "-Q" "-L" root
                                  "--eval" form)))
        (should (eq 0 status))
        (goto-char (point-min))
        (should (equal '(t t t t t t t nil nil) (read (current-buffer))))))))

;;; Window resizing

(ert-deftest cmdr-ops-test/increase-window-directions ()
  (let ((calls nil))
    (cl-letf (((symbol-function 'enlarge-window)
               (lambda (&rest args) (push args calls))))
      (increase-window-up)
      (increase-window-down)
      (increase-window-left)
      (increase-window-right))
    (should (equal '((1 nil) (-1 nil) (1 t) (-1 t)) (reverse calls)))))

;;; Opacity

(defun cmdr-ops-test--opacity-after (alpha fn)
  "Call FN with frame alpha ALPHA; return the alpha it set, or `unchanged'."
  (let ((result 'unchanged))
    (cl-letf (((symbol-function 'frame-parameter) (lambda (&rest _) alpha))
              ((symbol-function 'modify-frame-parameters)
               (lambda (_frame params) (setq result (alist-get 'alpha params)))))
      (funcall fn))
    result))

(ert-deftest cmdr-ops-test/opacity-increase-and-decrease-step ()
  (should (= 0.525 (cmdr-ops-test--opacity-after 0.5 #'frame-opacity-increase)))
  (should (= 0.475 (cmdr-ops-test--opacity-after 0.5 #'frame-opacity-decrease)))
  (should (= 0.525 (cmdr-ops-test--opacity-after 50 #'frame-opacity-increase))))

(ert-deftest cmdr-ops-test/opacity-respects-bounds ()
  (should (eq 'unchanged (cmdr-ops-test--opacity-after nil #'frame-opacity-increase)))
  (should (eq 'unchanged (cmdr-ops-test--opacity-after 1.0 #'frame-opacity-increase)))
  (should (eq 'unchanged (cmdr-ops-test--opacity-after 0.2 #'frame-opacity-decrease))))

(ert-deftest cmdr-ops-test/opacity-accepts-active-inactive-pair ()
  "Regression: a (ACTIVE . INACTIVE) alpha parameter must not signal."
  (should (= 0.525 (cmdr-ops-test--opacity-after '(50 . 40)
                                                 #'frame-opacity-increase))))

(ert-deftest cmdr-ops-test/opacity-reset ()
  (should (= 0.95 (cmdr-ops-test--opacity-after 0.5 #'frame-opacity-reset))))

(ert-deftest cmdr-ops-test/opacity-repeat-map ()
  (should (keymapp frame-opacity-repeat-map))
  (should (eq #'frame-opacity-increase (keymap-lookup frame-opacity-repeat-map "=")))
  (should (eq #'frame-opacity-decrease (keymap-lookup frame-opacity-repeat-map "-")))
  (should (eq #'frame-opacity-reset (keymap-lookup frame-opacity-repeat-map "0")))
  (seq-do (lambda (cmd)
            (should (eq 'frame-opacity-repeat-map (get cmd 'repeat-map))))
          '(frame-opacity-increase frame-opacity-decrease frame-opacity-reset)))

;;; Path helper

(ert-deftest cmdr-ops-test/minimize-path-abbreviates-home ()
  (should (equal "~/x/y" (cmdr--minimize-path (expand-file-name "x/y" "~/"))))
  (should (equal "/nonexistent-cmdr/x" (cmdr--minimize-path "/nonexistent-cmdr/x"))))

;;; save-all-buffers

(ert-deftest cmdr-ops-test/save-all-buffers-does-not-prompt ()
  (let ((args nil))
    (cl-letf (((symbol-function 'save-some-buffers)
               (lambda (&rest a) (setq args a))))
      (save-all-buffers))
    (should (equal '(t t) args))))

;;; Buffer queries

(ert-deftest cmdr-ops-test/buffer-line-count ()
  (with-temp-buffer
    (insert "line1\nline2\nline3")
    (should (= 3 (buffer-line-count (current-buffer))))
    (should (= 3 (buffer-line-count)))))

(ert-deftest cmdr-ops-test/buffer-directory ()
  (should-not (buffer-directory "not-a-buffer"))
  (should-not (buffer-directory nil))
  (with-temp-buffer
    (should-not (buffer-directory (current-buffer))))
  (cmdr-ops-test--call-with-dir
   (lambda (dir)
     (let ((buf (cmdr-ops-test--visit dir "a.txt")))
       (should (equal dir (buffer-directory buf)))))))

(ert-deftest cmdr-ops-test/buffers-matching-mode ()
  (let ((a (generate-new-buffer " *cmdr-mode-a*"))
        (b (generate-new-buffer " *cmdr-mode-b*")))
    (unwind-protect
        (progn
          (with-current-buffer a (emacs-lisp-mode))
          (with-current-buffer b (text-mode))
          (let ((found (buffers-matching-mode 'emacs-lisp-mode)))
            (should (memq a found))
            (should-not (memq b found)))
          (should-not (buffers-matching-mode 'cmdr-ops-test-fake-mode)))
      (kill-buffer a)
      (kill-buffer b))))

(ert-deftest cmdr-ops-test/buffers-matching-path ()
  (cmdr-ops-test--call-with-dir
   (lambda (dir)
     (let ((buf (cmdr-ops-test--visit dir "m.txt")))
       (should (memq buf (buffers-matching-path (regexp-quote dir))))
       (should-not (buffers-matching-path "/__no_such_cmdr_path__/"))
       (with-temp-buffer
         (should-not (memq (current-buffer) (buffers-matching-path ".*"))))))))

(ert-deftest cmdr-ops-test/buffers-matching-path-internal-buffers ()
  "Buffers whose file name starts with a space need INTERNAL-TOO."
  (let ((buf (generate-new-buffer " *cmdr-internal*")))
    (unwind-protect
        (with-current-buffer buf
          (setq buffer-file-name " cmdr-internal-file")
          (should-not (memq buf (buffers-matching-path "cmdr-internal-file")))
          (should (memq buf (buffers-matching-path "cmdr-internal-file" t)))
          (setq buffer-file-name nil))
      (kill-buffer buf))))

;;; Kill operations

(ert-deftest cmdr-ops-test/kill-buffers-in-directory-kills-and-reports ()
  (cmdr-ops-test--call-with-dir
   (lambda (dir)
     (let ((in (cmdr-ops-test--visit dir "in.txt"))
           (out (cmdr-ops-test--visit temporary-file-directory "cmdr-outside.txt")))
       (unwind-protect
           (let ((killed (kill-buffers-in-directory (directory-file-name dir))))
             (should (equal (list (cmdr--minimize-path (expand-file-name "in.txt" dir)))
                            killed))
             (should-not (buffer-live-p in))
             (should (buffer-live-p out)))
         (when (buffer-live-p out) (kill-buffer out))
         (ignore-errors
           (delete-file (expand-file-name "cmdr-outside.txt" temporary-file-directory))))))))

(ert-deftest cmdr-ops-test/kill-buffers-in-directory-honors-vetoes ()
  (cmdr-ops-test--call-with-dir
   (lambda (dir)
     (let ((keep (cmdr-ops-test--visit dir "keep.txt"))
           (drop (cmdr-ops-test--visit dir "drop.txt")))
       (with-current-buffer keep
         (add-hook 'kill-buffer-query-functions #'ignore nil t))
       (let ((killed (kill-buffers-in-directory dir)))
         (should (buffer-live-p keep))
         (should-not (buffer-live-p drop))
         (should (equal 1 (length killed))))))))

(ert-deftest cmdr-ops-test/kill-buffers-in-directory-skips-remote-without-connecting ()
  (let ((buf (generate-new-buffer " *cmdr-remote*"))
        (touched nil))
    (unwind-protect
        (cl-letf (((symbol-function 'file-in-directory-p)
                   (lambda (&rest _) (setq touched t) t)))
          (with-current-buffer buf
            (setq buffer-file-name "/ssh:cmdr-nohost.invalid:/tmp/x.txt"))
          (should-not (kill-buffers-in-directory temporary-file-directory))
          (should (buffer-live-p buf))
          (should-not touched))
      (with-current-buffer buf (setq buffer-file-name nil))
      (kill-buffer buf))))

(ert-deftest cmdr-ops-test/kill-buffers-in-directory-prompts-for-directory ()
  (let ((features (cons 'annotated-completing-read features))
        (asked nil))
    (cl-letf (((symbol-function 'annotated-completing-read-directory)
               (lambda () (setq asked t) "/nonexistent-cmdr-dir/")))
      (should-not (kill-buffers-in-directory))
      (should asked))))

(ert-deftest cmdr-ops-test/kill-eldoc-and-help-buffers ()
  (let ((help (get-buffer-create "*Help*"))
        (eldoc (get-buffer-create "*eldoc for cmdr*"))
        (other (get-buffer-create "*Helpful cmdr*")))
    (unwind-protect
        (progn
          (kill-eldoc-and-help-buffers)
          (should-not (buffer-live-p help))
          (should-not (buffer-live-p eldoc))
          (should (buffer-live-p other)))
      (seq-do (lambda (b) (when (buffer-live-p b) (kill-buffer b)))
              (list help eldoc other)))))

(ert-deftest cmdr-ops-test/kill-buffers-matching-name-is-alias ()
  (should (eq 'kill-matching-buffers (symbol-function 'kill-buffers-matching-name))))

(ert-deftest cmdr-ops-test/kill-buffers-matching-path-asks-unless-no-ask ()
  (cmdr-ops-test--call-with-dir
   (lambda (dir)
     (let ((buf (cmdr-ops-test--visit dir "ask.txt"))
           (asked nil))
       (cl-letf (((symbol-function 'kill-buffer-ask)
                  (lambda (_) (setq asked t) nil)))
         (should-not (kill-buffers-matching-path (regexp-quote dir))))
       (should asked)
       (should (buffer-live-p buf))
       (should (equal (list (buffer-file-name buf))
                      (kill-buffers-matching-path (regexp-quote dir) nil t)))
       (should-not (buffer-live-p buf))))))

(ert-deftest cmdr-ops-test/force-kill-buffers-matching-path-never-asks ()
  (cmdr-ops-test--call-with-dir
   (lambda (dir)
     (let ((buf (cmdr-ops-test--visit dir "force.txt")))
       (cl-letf (((symbol-function 'kill-buffer-ask)
                  (lambda (_) (error "must not ask"))))
         (should (equal (list (buffer-file-name buf))
                        (force-kill-buffers-matching-path (regexp-quote dir)))))
       (should-not (buffer-live-p buf))))))

(ert-deftest cmdr-ops-test/kill-buffers-matching-mode ()
  (let ((a (generate-new-buffer " *cmdr-km-a*"))
        (b (generate-new-buffer " *cmdr-km-b*")))
    (unwind-protect
        (progn
          (with-current-buffer a (emacs-lisp-mode))
          (with-current-buffer b (text-mode))
          (cl-letf (((symbol-function 'message) #'ignore))
            (should (= 1 (kill-buffers-matching-mode 'emacs-lisp-mode))))
          (should-not (buffer-live-p a))
          (should (buffer-live-p b)))
      (when (buffer-live-p a) (kill-buffer a))
      (kill-buffer b))))

(ert-deftest cmdr-ops-test/kill-all-reference-and-source-buffers ()
  "Regression: killed buffers are reported, not silently dropped."
  (require 'package)
  (cmdr-ops-test--call-with-dir
   (lambda (dir)
     (let* ((package-user-dir (directory-file-name dir))
            (package-directory-list nil)
            (buf (cmdr-ops-test--visit dir "ref.el")))
       (should (equal (list (cmdr--minimize-path (buffer-file-name buf)))
                      (kill-all-reference-and-source-buffers)))
       (should-not (buffer-live-p buf))))))

(ert-deftest cmdr-ops-test/kill-missing-file-buffers-finds-missing ()
  (cmdr-ops-test--call-with-dir
   (lambda (dir)
     (let* ((buf (cmdr-ops-test--visit dir "gone.txt"))
            (file (buffer-file-name buf)))
       (delete-file file)
       (cl-letf (((symbol-function 'kill-buffer-ask)
                  (lambda (b) (kill-buffer b) t)))
         (should (member file (kill-buffers-visiting-missing-files))))
       (should-not (buffer-live-p buf))))))

(ert-deftest cmdr-ops-test/kill-missing-file-buffers-skips-existing-and-scratch ()
  (cmdr-ops-test--call-with-dir
   (lambda (dir)
     (let ((buf (cmdr-ops-test--visit dir "here.txt")))
       (with-temp-buffer
         (cl-letf (((symbol-function 'kill-buffer-ask)
                    (lambda (_) (error "must not ask"))))
           (should-not (kill-buffers-visiting-missing-files))))
       (should (buffer-live-p buf))))))

(ert-deftest cmdr-ops-test/kill-missing-file-buffers-respects-no ()
  (cmdr-ops-test--call-with-dir
   (lambda (dir)
     (let ((buf (cmdr-ops-test--visit dir "gone.txt")))
       (delete-file (buffer-file-name buf))
       (cl-letf (((symbol-function 'kill-buffer-ask) (lambda (_) nil)))
         (should-not (kill-buffers-visiting-missing-files)))
       (should (buffer-live-p buf))))))

(ert-deftest cmdr-ops-test/kill-buffer-and-delete-file-deletes ()
  (cmdr-ops-test--call-with-dir
   (lambda (dir)
     (let* ((buf (cmdr-ops-test--visit dir "del.txt"))
            (file (buffer-file-name buf)))
       (kill-buffer-and-delete-file buf)
       (should-not (buffer-live-p buf))
       (should-not (file-exists-p file))))))

(ert-deftest cmdr-ops-test/kill-buffer-and-delete-file-non-file-errors ()
  (with-temp-buffer
    (should-error (kill-buffer-and-delete-file) :type 'user-error)))

(ert-deftest cmdr-ops-test/kill-buffer-and-delete-file-interactive-veto-keeps-file ()
  (cmdr-ops-test--call-with-dir
   (lambda (dir)
     (let* ((buf (cmdr-ops-test--visit dir "keep.txt"))
            (file (buffer-file-name buf)))
       (with-current-buffer buf
         (add-hook 'kill-buffer-query-functions #'ignore nil t)
         (should-error (call-interactively #'kill-buffer-and-delete-file)
                       :type 'user-error))
       (should (buffer-live-p buf))
       (should (file-exists-p file))))))

;;; Text movement

(ert-deftest cmdr-ops-test/move-text-moves-line ()
  (with-temp-buffer
    (insert "a\nb\nc\n")
    (goto-char (point-min))
    (move-text-down 1)
    (should (equal "b\na\nc\n" (buffer-string)))
    (move-text-up 1)
    (should (equal "a\nb\nc\n" (buffer-string)))))

(ert-deftest cmdr-ops-test/move-text-moves-region ()
  (with-temp-buffer
    (insert "a\nb\nc\nd\n")
    (let ((transient-mark-mode t))
      (goto-char (point-min))
      (push-mark (point) t t)
      (forward-line 2)
      (setq mark-active t)
      (move-text-down 1)
      (should (equal "c\na\nb\nd\n" (buffer-string))))))

(ert-deftest cmdr-ops-test/move-text-defers-to-org-in-org-mode ()
  (with-temp-buffer
    (setq-local major-mode 'org-mode)
    (let ((calls nil))
      (cl-letf (((symbol-function 'org-metadown)
                 (lambda (n) (push (list 'down n) calls)))
                ((symbol-function 'org-metaup)
                 (lambda (n) (push (list 'up n) calls))))
        (move-text-down 2)
        (move-text-up 3))
      (should (equal '((down 2) (up 3)) (reverse calls))))))

;;; Wrapping

(ert-deftest cmdr-ops-test/wrap-aliases ()
  (should (eq 'turn-off-soft-wrap (symbol-function 'turn-on-hard-wrap)))
  (should (eq 'turn-on-soft-wrap (symbol-function 'turn-off-hard-wrap))))

(ert-deftest cmdr-ops-test/toggle-word-wrap-rejects-argument ()
  (should-error (toggle-word-wrap t) :type 'user-error)
  (should-error (toggle-word-wrap 1) :type 'user-error))

(ert-deftest cmdr-ops-test/toggle-word-wrap-delegates ()
  (with-temp-buffer
    (let ((calls nil))
      (cl-letf (((symbol-function 'turn-on-soft-wrap)
                 (lambda () (push 'soft calls)))
                ((symbol-function 'turn-on-hard-wrap)
                 (lambda () (push 'hard calls))))
        (setq-local auto-fill-function #'ignore)
        (toggle-word-wrap)
        (setq-local auto-fill-function nil)
        (toggle-word-wrap))
      (should (equal '(soft hard) (reverse calls))))))

(ert-deftest cmdr-ops-test/soft-and-hard-wrap-switch-modes ()
  (with-temp-buffer
    (let ((messages nil))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
        (turn-off-soft-wrap)
        (should auto-fill-function)
        (should-not visual-line-mode)
        (turn-on-soft-wrap)
        (should-not auto-fill-function)
        (should visual-line-mode)
        (should (seq-some (lambda (m) (string-match-p "wrapping mode `soft'" m))
                          messages))
        (setq messages nil)
        (turn-on-soft-wrap)
        (should-not messages)
        (turn-on-hard-wrap)
        (should auto-fill-function)
        (turn-off-hard-wrap)
        (should visual-line-mode)))))

;;; Line and text helpers

(ert-deftest cmdr-ops-test/unfill-region ()
  (with-temp-buffer
    (insert "one\ntwo\n\nthree\n  indented\n* item\n")
    (unfill-region (point-min) (point-max))
    (should (equal "one two\n\nthree\n  indented\n* item\n" (buffer-string)))))

(ert-deftest cmdr-ops-test/uniquify-lines ()
  (with-temp-buffer
    (insert "a\na\na\nb\na\n")
    (uniquify-buffer-lines)
    (should (equal "a\nb\na\n" (buffer-string))))
  (with-temp-buffer
    (uniquify-region-lines (point-min) (point-max))
    (should (equal "" (buffer-string)))))

(ert-deftest cmdr-ops-test/set-tab-width-is-buffer-local ()
  (let ((default tab-width))
    (with-temp-buffer
      (set-tab-width 3)
      (should (= 3 tab-width))
      (should (local-variable-p 'tab-width)))
    (should (= default (default-value 'tab-width)))))

(ert-deftest cmdr-ops-test/text-scale-reset ()
  (with-temp-buffer
    (text-scale-increase 2)
    (should (= 2 text-scale-mode-amount))
    (text-scale-reset)
    (should (= 0 text-scale-mode-amount))))

(ert-deftest cmdr-ops-test/insert-current-date ()
  (with-temp-buffer
    (insert-current-date)
    (should (string-match-p "\\`[0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\'"
                            (buffer-string)))))

(ert-deftest cmdr-ops-test/jump-to-elisp-help-searches-symbol-at-point ()
  (with-temp-buffer
    (insert "car-safe")
    (let ((query nil))
      (cl-letf (((symbol-function 'apropos-documentation)
                 (lambda (pattern &rest _) (setq query pattern))))
        (jump-to-elisp-help))
      (should (equal "car-safe" query)))))

(ert-deftest cmdr-ops-test/toggle-local-whitespace-cleanup ()
  (with-temp-buffer
    (cl-letf (((symbol-function 'message) #'ignore))
      (toggle-local-whitespace-cleanup)
      (should show-trailing-whitespace)
      (should (memq #'whitespace-cleanup before-save-hook))
      (toggle-local-whitespace-cleanup)
      (should-not show-trailing-whitespace)
      (should-not (memq #'whitespace-cleanup before-save-hook)))))

(ert-deftest cmdr-ops-test/pin-buffer-to-window-toggle ()
  "Toggles dedication and reports the state it just set."
  (let* ((window (selected-window))
         (original (window-dedicated-p window))
         (messages nil))
    (unwind-protect
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format fmt args) messages))))
          (set-window-dedicated-p window nil)
          (pin-buffer-to-window-toggle)
          (should (window-dedicated-p window))
          (should (string-match-p "\\`pinned " (car messages)))
          (pin-buffer-to-window-toggle)
          (should-not (window-dedicated-p window))
          (should (string-match-p "\\`unpinned " (car messages))))
      (set-window-dedicated-p window original))))

;;; Kill ring cleaning

(ert-deftest cmdr-ops-test/clean-kill-ring-filter-catch-p ()
  (should (clean-kill-ring-filter-catch-p ""))
  (should (clean-kill-ring-filter-catch-p " \n\t"))
  (should-not (clean-kill-ring-filter-catch-p " padded "))
  (should (clean-kill-ring-filter-catch-p (propertize "  " 'face 'bold)))
  (let ((clean-kill-ring-filters (list (lambda (s) (string= s "x")))))
    (should (eq t (clean-kill-ring-filter-catch-p "x")))
    (should (eq nil (clean-kill-ring-filter-catch-p "y")))))

(ert-deftest cmdr-ops-test/clean-kill-ring-clean ()
  (let ((kill-ring (list "a" "" "b" "  " "a")))
    (let ((clean-kill-ring-prevent-duplicates nil))
      (clean-kill-ring-clean)
      (should (equal '("a" "b" "a") kill-ring)))
    (clean-kill-ring-clean t)
    (should (equal '("a" "b") kill-ring)))
  (let ((kill-ring (list "a" "a"))
        (clean-kill-ring-prevent-duplicates t))
    (clean-kill-ring-clean)
    (should (equal '("a") kill-ring)))
  (let ((kill-ring nil))
    (clean-kill-ring-clean)
    (should-not kill-ring)))

(provide 'test-cmdr-ops)
;;; test-cmdr-ops.el ends here
