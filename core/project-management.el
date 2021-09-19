;;; project-management.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package find-file-in-project)

(use-package projectile
  :after (ivy)
  :diminish
  :config
  (setq projectile-completion-system 'ivy)
  (dolist (dir '("node_modules" "build" "target" "out"))
    (add-to-list 'projectile-globally-ignored-directories dir))
  (projectile-mode 1))

;; projectile-rg requires rg to be present
(use-package rg)
;; REVIEW: Snails https://github.com/manateelazycat/snails

(use-package treemacs
  :config
  (treemacs-follow-mode t))

(use-package treemacs-projectile
  :after (treemacs projectile))

(provide 'project-management)
;;; project-management.el ends here
