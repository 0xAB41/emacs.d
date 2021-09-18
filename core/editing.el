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

(setq show-paren-delay 0.2
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(show-paren-mode 1)

;; Remember where the point is in for every visited file
(setq save-place-file (expand-file-name ".places" user-emacs-directory))
(setq save-place-forget-unreadable-files nil)
(save-place-mode 1)

;; always highlight current line
(global-hl-line-mode t)

;; Show column as well as line number
(setq column-number-mode t)

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
  :requires evil
  :after evil
  :config
  (evil-collection-init))

(use-package evil-surround
  :requires evil
  :after evil
  :config
  (global-evil-surround-mode 1))

(use-package evil-goggles
  :requires evil
  :config
  (evil-goggles-mode))

(use-package evil-commentary
  :requires evil
  :config
  (evil-commentary-mode))

;; Jump using avy
;; REVIEW: evil-snipe
(use-package avy
  :custom
  (avy-timeout-seconds 0.2))

(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(provide 'editing)
;;; editing.el ends here
