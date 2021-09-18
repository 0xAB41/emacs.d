;;; completions.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ivy
  :config
  (ivy-mode 1))

(use-package swiper)

(use-package counsel
  :config
  (counsel-mode 1))

(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))

(provide 'completions)
;;; completions.el ends here
