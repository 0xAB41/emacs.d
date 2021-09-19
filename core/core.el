;;; core.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; ------ Better Defaults
;; Don't ring bells
(setq ring-bell-function 'ignore)

;; Reduce startup noise
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Move custom preferences to another file
(setq custom-file (expand-file-name "custom.el" maze-etc-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;; y/n instead of yes/no 
(defalias 'yes-or-no-p 'y-or-n-p)

;; Show path names in buffer if the names are same
(setq uniquify-buffer-name-style 'post-forward-angle-brackets)

;; Deleting files go to OS's trash folder
(setq delete-by-moving-to-trash t)

;; Fix frame titles to display file paths
(setq frame-title-format
       '((:eval (if (buffer-file-name)
                    (abbreviate-file-name (buffer-file-name))
		  "%b")))
       icon-title-format frame-title-format)

(setq initial-buffer-choice "~/.emacs.d/init.el")

(setq find-file-visit-truename t
      vc-follow-symlinks t)

;; Save minibuffer history
(setq savehist-file (expand-file-name ".history" maze-etc-directory))
(ignore-errors (savehist-mode 1))

;; Prevent yank/kill from accessing the clipboard
(setq select-enable-clipboard nil)

(require 'functions)

;; Setup PATH and other vars from shell
(use-package exec-path-from-shell
  :if maze-is-mac
  :config
  (exec-path-from-shell-initialize))

(provide 'core)
;;; core.el ends here
