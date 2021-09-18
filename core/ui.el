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
(set-frame-font "Fantasque Sans Mono-17" nil t)
(setq line-spacing 0.24)

;; Disable bidirectional rendering
(setq-default bidi-display-reordering 'left-to-right
              bidi-paragraph-direction 'left-to-right)


(use-package diminish)

(provide 'ui)
;;; ui.el ends here
