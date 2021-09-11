;;; early-init.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Defer garbage collection
(setq gc-cons-threshold most-positive-fixnum
      gc-cons-percentage 0.5)

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq package-enable-at-startup nil)

(setq frame-inhibit-implied-resize t)

(provide 'early-init)
;;; early-init.el ends here