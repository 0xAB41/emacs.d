;;; core.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(setq user-full-name "Abhilash Meesala"
      user-mail-address "mail@abhilashm.me")

(defvar is-mac (eq system-type 'darwin)
  "Is this environment a mac?")

;; ------ Better Defaults
;; Don't ring bells
(setq ring-bell-function 'ignore)

;; Reduce startup noise
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)

;; Move custom preferences to another file
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;; y/n instead of yes/no 
(defalias 'yes-or-no-p 'y-or-n-p)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder

;; Fix frame titles to display file paths
(setq frame-title-format
       '((:eval (if (buffer-file-name)
                    (abbreviate-file-name (buffer-file-name))
		  "%b")))
       icon-title-format frame-title-format)

(setq initial-buffer-choice
      "~/.emacs.d/init.el")

(setq find-file-visit-truename t
      vc-follow-symlinks t)

(setq savehist-file (expand-file-name ".history" user-emacs-directory))
(ignore-errors (savehist-mode 1))

(provide 'core)
;;; core.el ends here
