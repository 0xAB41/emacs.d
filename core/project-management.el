;;; project-management.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package find-file-in-project)

(use-package projectile
  :after (ivy)
  :diminish
  :config
  (setq projectile-completion-system 'ivy)
  (projectile-mode 1))

;; projectile-rg requires rg to be present
(use-package rg)

(provide 'project-management)
;;; project-management.el ends here
