;;; isolation-child.el --- Child process for the cmdr isolation test -*- lexical-binding: t; -*-

;;; Commentary:
;; Run as a fresh `emacs --batch -Q' process by `test-cmdr-isolation.el'.
;; Defines only functions; nothing runs at load time.  The entry point
;; `cmdr-isolation-child-run' snapshots global state, loads the cmdr
;; libraries, toggles `cmdr-mode', and prints one line per finding.  It
;; exits 0 when the libraries changed nothing they should not have.

;;; Code:

(require 'cl-lib)
(require 'seq)
(require 'subr-x)

(defvar cmdr-mode-map)
(declare-function cmdr-mode "cmdr-mode")
(declare-function cmdr-keys-prefix "cmdr-keys")
(declare-function cmdr-keys-set "cmdr-keys")
(declare-function which-key--get-keymap-bindings "which-key")

(defconst cmdr-isolation--maps
  '(global-map minibuffer-local-map minibuffer-local-ns-map
    minibuffer-local-completion-map minibuffer-local-must-match-map
    minibuffer-local-filename-completion-map minibuffer-local-shell-command-map
    read-expression-map esc-map ctl-x-map)
  "Keymap variables that must stay unchanged.")

(defconst cmdr-isolation--variables
  '(display-buffer-alist display-buffer-base-action display-buffer-fallback-action
    auto-mode-alist interpreter-mode-alist default-frame-alist initial-frame-alist
    completion-styles completion-category-overrides completion-category-defaults
    read-extended-command-predicate kill-ring kill-buffer-query-functions
    timer-list timer-idle-list global-minor-modes)
  "Other variables that must stay unchanged.")

(defun cmdr-isolation--hooks ()
  "Return an alist of (SYMBOL . VALUE) for every bound hook-like variable."
  (let ((result nil))
    (mapatoms (lambda (sym)
                (when (and (boundp sym)
                           (not (keywordp sym))
                           (string-match-p "-\\(hook\\|functions\\)\\'" (symbol-name sym))
                           (not (string-prefix-p "cmdr" (symbol-name sym))))
                  (push (cons sym (copy-tree (symbol-value sym))) result))))
    (sort result (lambda (a b) (string< (car a) (car b))))))

(defun cmdr-isolation--advice ()
  "Return the sorted list of symbols whose function definition is advised."
  (let ((result nil))
    (mapatoms (lambda (sym)
                (when (and (fboundp sym) (advice--p (symbol-function sym)))
                  (push sym result))))
    (sort result (lambda (a b) (string< a b)))))

(defun cmdr-isolation--snapshot ()
  "Return an alist capturing the state that must not change."
  (append
   (seq-map (lambda (sym) (cons sym (copy-tree (symbol-value sym))))
            (seq-filter #'boundp cmdr-isolation--maps))
   (seq-map (lambda (sym) (cons sym (copy-tree (and (boundp sym) (symbol-value sym)))))
            cmdr-isolation--variables)
   (list (cons 'hooks (cmdr-isolation--hooks))
         (cons 'advice (cmdr-isolation--advice))
         (cons 'minor-mode-map-alist (copy-sequence minor-mode-map-alist))
         (cons 'active-maps (copy-sequence (current-minor-mode-maps))))))

(defun cmdr-isolation--diff (before after ignore)
  "Return the keys whose values differ between BEFORE and AFTER, minus IGNORE."
  (seq-filter (lambda (key)
                (and (not (memq key ignore))
                     (not (equal (alist-get key before) (alist-get key after)))))
              (seq-uniq (append (seq-map #'car before) (seq-map #'car after)))))

(defun cmdr-isolation--report (label diff)
  "Print LABEL and DIFF; return non-nil when DIFF is empty."
  (princ (format "%s: %s\n" label (if diff (format "CHANGED %S" diff) "unchanged")))
  (null diff))

(defun cmdr-isolation-child-run ()
  "Check that loading and toggling cmdr changes only what it should, then exit."
  (require 'transient)
  (require 'annotated-completing-read)
  (let* ((before (cmdr-isolation--snapshot))
         (ok t))
    (seq-do #'require '(cmdr cmdr-keys cmdr-mode cmdr-macros cmdr-ops))
    (cmdr-keys-prefix cmdr-mode-map "C-c a" "Apps")
    (cmdr-keys-set cmdr-mode-map "C-c a f" #'ignore)
    (let* ((loaded (cmdr-isolation--snapshot))
           (added (seq-difference minor-mode-map-alist (alist-get 'minor-mode-map-alist before))))
      (setq ok (and (cmdr-isolation--report "load" (cmdr-isolation--diff before loaded '(minor-mode-map-alist)))
                    ok))
      (princ (format "load: minor-mode-map-alist additions %S\n" (seq-map #'car added)))
      (setq ok (and (equal (seq-map #'car added) '(cmdr-mode)) ok))
      (setq ok (and (not (memq cmdr-mode-map (current-minor-mode-maps))) ok))
      (cmdr-mode 1)
      (let ((on (cmdr-isolation--snapshot)))
        (setq ok (and (cmdr-isolation--report "enable" (cmdr-isolation--diff loaded on '(active-maps global-minor-modes)))
                      ok))
        (princ (format "enable: active map added %S\n"
                       (seq-difference (alist-get 'active-maps on) (alist-get 'active-maps loaded))))
        (setq ok (and (equal (seq-difference (alist-get 'active-maps on) (alist-get 'active-maps loaded))
                             (list cmdr-mode-map))
                      ok)))
      (cmdr-mode -1)
      (setq ok (and (cmdr-isolation--report "disable" (cmdr-isolation--diff loaded (cmdr-isolation--snapshot) nil))
                    ok)))
    (princ (if ok "RESULT: ok\n" "RESULT: FAILED\n"))
    (kill-emacs (if ok 0 1))))

(defun cmdr-isolation-child-which-key ()
  "Print which-key's view of a labelled `cmdr-keys-prefix' binding, then exit."
  (require 'which-key)
  (require 'cmdr-keys)
  (let ((map (make-sparse-keymap)))
    (cmdr-keys-prefix map "C-c a" "Apps")
    (cmdr-keys-set map "C-c a f" #'find-file)
    (princ (format "prefix: %S\n"
                   (which-key--get-keymap-bindings (lookup-key map (kbd "C-c")))))
    (princ (format "leaf: %S\n"
                   (which-key--get-keymap-bindings (lookup-key map (kbd "C-c a"))))))
  (kill-emacs 0))

(provide 'isolation-child)
;;; isolation-child.el ends here
