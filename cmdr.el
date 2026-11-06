;;; cmdr.el --- Command registry with Transient and completing-read frontends -*- lexical-binding: t; -*-

;; Author: sam kleinman <sam@tychoish.com>
;; Maintainer: sam kleinman <sam@tychoish.com>
;; Version: 0.1.0
;; Package-Requires: ((emacs "29.4") (transient "0.7") (annotated-completing-read "0.1"))
;; Keywords: convenience, tools
;; URL: https://github.com/tychoish/cmdr.el

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Registry of categorized commands, exposed as `cmdr-menu' (Transient) and
;; `cmdr-dispatch' (annotated completing-read).

;;; Code:

(require 'map)
(require 'seq)
(require 'subr-x)

(provide 'cmdr)
;;; cmdr.el ends here
