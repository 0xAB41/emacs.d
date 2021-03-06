;;; completions.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-s" . swiper)
	 :map ivy-minibuffer-map
	 ("TAB" . ivy-alt-done)
	 ("C-j" . ivy-next-line)
	 ("C-k" . ivy-previous-line)
	 :map ivy-switch-buffer-map
	 ("C-d" . ivy-switch-buffer-kill))
  :config
  (setq ivy-count-format "[%d/%d] "
	ivy-use-virtual-buffers t
	enable-recursive-minibuffers t
	ivy-initial-inputs-alist nil)
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
  :bind (:map minibuffer-local-map
	 ("C-r" . 'counsel-minibuffer-history))
  :config
  (counsel-mode 1))

(use-package ivy-prescient
  :after counsel
  :custom
  (ivy-prescient-enable-filtering nil)
  :config
  (prescient-persist-mode 1)
  (ivy-prescient-mode 1))

;; Remember our last M-x command
;; REVIEW: amx. smex is dead?
(use-package smex
  :config
  (setq smex-save-file (expand-file-name "smex-items" maze-etc-directory))
  (smex-initialize))

;;; ------ Snippets
(use-package yasnippet
  :init
  (setq yas-snippet-dirs '("~/.emacs.d/snippets"))
  :config
  (yas-global-mode 1))

(setq rotate-text-words '(("true" "false")
                          ("width" "height")
                          ("yes" "no")
                          ("y" "n")
                          ("0" "1")
                          ("row" "col")
                          ("more" "less")
                          ("top" "bottom")
                          ("left" "right")))

;;; ------ Auto complete and LSP
(use-package company
  :diminish company-mode
  :hook
  (after-init . global-company-mode))

(use-package lsp-mode
  :init
  (setq lsp-keymap-prefix "C-c l"))

(use-package lsp-ui
  :requires lsp-mode)

(use-package lsp-ivy
  :requires ivy)

(provide 'completions)
;;; completions.el ends here
