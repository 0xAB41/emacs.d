;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Measure and log startup time
(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "Startup took %.3fs" elapsed)))))

;; Adjust garbage collection thresholds during startup
(let ((normal-gc-cons-threshold (* 256 1024 1024))
      (init-gc-cons-threshold (* 1024 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; ------ Vars and Meta
(setq user-full-name "Abhilash Meesala"
      user-mail-address "mail@abhilashm.me")

(defvar is-mac (eq system-type 'darwin)
  "Is this environment a mac?")

;; ------ Package Management
(require 'package)

;; Save all packages under a directory versioned based on major, minor emacs version
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
			 user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

;; gnupg does not honor the `package-user-dir` 
(when (boundp 'package-gnupghome-dir)
  (setq package-gnupghome-dir
        (expand-file-name "gnupg" package-user-dir)))

(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)

(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)		; Always ensure
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(require 'use-package)

(use-package gnu-elpa-keyring-update)

;; ------ Better Defaults
;; Disable useless GUI elements to reduce clutter
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (fboundp 'horizontal-scroll-bar-mode) (horizontal-scroll-bar-mode -1))
(when (fboundp 'tooltip-mode) (tooltip-mode -1))

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

;; Use UTF-8 everywhere
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

;; Remember where the point is in for every visited file
(setq save-place-file (expand-file-name ".places" user-emacs-directory))
(setq save-place-forget-unreadable-files nil)
(save-place-mode 1)

;; Don't write lock-files, I'm the only one here.
(setq create-lockfiles nil)

;; y/n instead of yes/no 
(defalias 'yes-or-no-p 'y-or-n-p)

;; always highlight current line
(global-hl-line-mode t)

(when is-mac
  ;; Make title bars pretty on Mac
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  ;; beautiful fonts on Mac
  (setq mac-allow-anti-aliasing t))

;; Fix frame titles to display file paths
(setq frame-title-format
       '((:eval (if (buffer-file-name)
                    (abbreviate-file-name (buffer-file-name))
		  "%b")))
       icon-title-format frame-title-format)

(setq use-file-dialog nil)
(setq use-dialog-box nil)

;; Draw underlines lower
(setq x-underline-at-descent-line t)

;; Set fonts and line spacings
(set-frame-font "Fantasque Sans Mono-17" nil t)
(setq line-spacing 0.24)

;; Disable bidirectional rendering
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(setq initial-buffer-choice
      "~/.emacs.d/init.el")

;; disk is cheap. Backup ! alot.
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))
(setq backup-by-copying t)
(setq vc-make-backup-files nil) ; Make backups for files under version control as well.
(setq delete-old-versions t)
(setq kept-new-versions 10) ; Number of newest versions to keep when a new numbered backup is made.
(setq kept-old-versions 0) ; Number of oldest versions to keep when a new numbered backup is made.
(setq version-control t) ; Make numeric backup versions unconditionally.


;; Write all autosave files in the tmp dir
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

(setq find-file-visit-truename t
      vc-follow-symlinks t)

(setq uniquify-buffer-name-style 'post-forward-angle-brackets) ; Show path if names are same
(setq delete-by-moving-to-trash t)         ; Deleting files go to OS's trash folder

(use-package diminish)
(use-package bind-key)

;; ------ Evil
(use-package evil
  :init
  (setq evil-want-integration t) ;; This is optional since it's already set to t by default.
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; ------ Editing
(use-package avy
  :custom
  (avy-timeout-seconds 0.2))


(use-package ivy
  :config
  (ivy-mode 1))
(use-package swiper)
(use-package counsel
  :config
  (counsel-mode 1))

(global-auto-revert-mode 1)
(delete-selection-mode 1)
(show-paren-mode 1)
(ignore-errors (savehist-mode 1))

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)


;; ------ Keybindings
(setq mac-option-modifier 'meta
        mac-command-modifier 'super)
(global-set-key [(super a)] #'mark-whole-buffer)
(global-set-key [(super v)] #'yank)
(global-set-key [(super c)] #'kill-ring-save)
(global-set-key [(super s)] #'save-buffer)
(global-set-key [(super l)] #'goto-line)
(global-set-key [(super w)] #'delete-frame)
(global-set-key [(super z)] #'undo)
(global-set-key [(super q)] #'save-buffers-kill-terminal)



(provide 'init)
;;; init.el ends here


