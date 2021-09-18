;;; ui.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Disable useless GUI elements to reduce clutter
(dolist (mode '(tool-bar-mode menu-bar-mode scroll-bar-mode tooltip-mode))
  (when (fboundp mode) (funcall mode -1)))

(setq use-file-dialog nil)
(setq use-dialog-box nil)

(when is-mac
  ;; Make title bars pretty on Mac
  (add-to-list 'default-frame-alist '(ns-transparent-titlebar . t))
  (add-to-list 'default-frame-alist '(ns-appearance . light))
  ;; beautiful fonts on Mac
  (setq mac-allow-anti-aliasing t))

;; Draw underlines lower
(setq x-underline-at-descent-line t)

;; Set fonts and line spacings
;; Unfortunately, I could not find a reliable way to center the text when using line spacing
;; so to achieve the centered effect, we fiddle with line-height and line-spacing
;;(add-text-properties (point-min) (point-max)
;;		     '(line-spacing 0.25 line-height 1.25))
;; (add-to-list 'default-frame-alist '(font . "Fantasque Sans Mono 1.2 18"))
;; (face-attribute 'default :font)
;; REVIEW: https://valignatev.com/posts/emacs-font/
(set-frame-font "Fantasque Sans Mono 1.2 18")

;; Disable bidirectional rendering
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)

(use-package diminish)

(use-package all-the-icons)

(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(use-package doom-themes
  :requires all-the-icons
  :config
  (setq doom-themes-enable-bold t
	doom-themes-enable-italic t)
  (load-theme 'doom-nord-light t)
  (doom-themes-org-config))

(use-package doom-modeline
  :config
  (setq doom-modeline-buffer-encoding nil
	doom-modeline-env-version nil
	doom-modeline-height 18)
  :init
  (doom-modeline-mode 1))

(provide 'ui)
;;; ui.el ends here
