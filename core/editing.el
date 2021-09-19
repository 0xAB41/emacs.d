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

(when (fboundp 'global-so-long-mode)
  (global-so-long-mode))

;; Remember where the point is in for every visited file
(setq save-place-file (expand-file-name ".places" maze-etc-directory)
      save-place-forget-unreadable-files nil)
(save-place-mode 1)

;; always highlight current line
(global-hl-line-mode t)

;; Show line numbers
(global-display-line-numbers-mode 1)

;; Show column as well as line number in modeline
(setq column-number-mode t)

;;; ------ Brackets and delimiters
(use-package rainbow-delimiters
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(setq show-paren-delay 0.2
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(show-paren-mode 1)

(setq electric-pair-inhibit-predicate 'electric-pair-conservative-inhibit)
(electric-pair-mode 1)

;;; ------ Backups and lock files
;; Don't write lock-files, I'm the only one here.
(setq create-lockfiles nil)

;; disk is cheap. Backup ! alot.
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat maze-etc-directory "backups")))))
(setq backup-by-copying t
      vc-make-backup-files nil	; Make backups for files under version control as well.
      delete-old-versions t
      kept-new-versions 10	; Number of newest versions to keep when a new numbered backup is made.
      kept-old-versions 0	; Number of oldest versions to keep when a new numbered backup is made.
      version-control t)	; Make numeric backup versions unconditionally.

;; Write all autosave files in the tmp dir
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;;; ------ Evil
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

(use-package evil-numbers)

;; Jump using avy
;; REVIEW: evil-snipe
(use-package avy
  :custom
  ;; Make avy assign shorter key sequences to things (lines/chars) near the point
  (setq avy-orders-alist
	'((avy-goto-line . avy-order-closest)))
  (setq avy-timeout-seconds 0.2))

(use-package hl-prog-extra
  :init
  (add-hook 'prog-mode-hook #'hl-prog-extra-mode))

;; REVIEW: Crux https://github.com/bbatsov/crux

(use-package sudo-edit)

(use-package undo-tree
  :init
  (setq undo-tree-visualizer-diff t
	undo-tree-visualizer-timestamps t)
  (global-undo-tree-mode))

(provide 'editing)
;;; editing.el ends here
