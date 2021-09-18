;;; completions.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ivy
  :defer 0.1
  :diminish
  :custom
  (setq ivy-count-format "[%d/%d] "
	ivy-use-virtual-buffers t
	enable-recursive-minibuffers t)
  :config
  (ivy-mode 1))

(use-package ivy-rich
  :after ivy
  :custom
  (setq ivy-virtual-abbreviate 'full
	ivy-rich-switch-buffer-align-virtual-buffer t
	ivy-rich-path-style 'abbrev
	ivy-height-alist `((t . ,(lambda (_caller) (/ (frame-height) 3))))
	ivy-wrap t)
  :config
  (ivy-rich-mode 1))

(use-package swiper
  :after ivy)

(use-package counsel
  :after ivy
  :config
  (counsel-mode 1))

(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))

(use-package company
  :diminish company-mode
  :hook
  (after-init . global-company-mode))

(use-package lsp-mode)
(use-package lsp-ui
  :requires lsp-mode)
(use-package lsp-ivy
  :requires ivy)

;; No ^ in things like counsel-M-x
(setq ivy-initial-inputs-alist nil)

;; Remember our last M-x command
(use-package smex
  :config
  (smex-initialize))

(setq rotate-text-words '(("true" "false")
                          ("width" "height")
                          ("yes" "no")
                          ("y" "n")
                          ("0" "1")
                          ("row" "col")
                          ("more" "less")
                          ("top" "bottom")
                          ("left" "right")))

(provide 'completions)
;;; completions.el ends here
