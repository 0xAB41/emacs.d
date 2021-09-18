;;; editing.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Use UTF-8 everywhere
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-language-environment 'utf-8)
(set-default-coding-systems 'utf-8)

(global-auto-revert-mode 1)
(delete-selection-mode 1)
(show-paren-mode 1)

;; Remember where the point is in for every visited file
(setq save-place-file (expand-file-name ".places" user-emacs-directory))
(setq save-place-forget-unreadable-files nil)
(save-place-mode 1)

;; always highlight current line
(global-hl-line-mode t)

;; Don't write lock-files, I'm the only one here.
(setq create-lockfiles nil)

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

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


;; Modal editing everywhere!
(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))


;; Jump using avy
(use-package avy
  :custom
  (avy-timeout-seconds 0.2))


(provide 'editing)
;;; editing.el ends here
