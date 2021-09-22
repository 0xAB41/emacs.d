;;; project-management.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package find-file-in-project
  :init
  (when (executable-find "fd")
    (setq ffip-use-rust-fd t)))

(setq projectile-known-projects-file (expand-file-name "projectile-bookmarks.eld" maze-etc-directory))
(use-package projectile
  :after (ivy)
  :diminish
  :config
  (setq projectile-completion-system 'ivy
	projectile-enable-caching t
	projectile-cache-file (expand-file-name "projectile.cache" maze-etc-directory)) 
  (dolist (dir '("node_modules" "build" "target" "out"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  (projectile-mode 1))

;; projectile-rg requires rg to be present
(use-package rg)

;; TODO: Snails https://github.com/manateelazycat/snails

;; TODO: Dired

(use-package treemacs
  :config
  (treemacs-follow-mode t))

(use-package treemacs-projectile
  :after (treemacs projectile))

(provide 'project-management)
;;; project-management.el ends here
