;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Reset file-name-handler-alist during initialization
(let ((init-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq file-name-handler-alist init-file-name-handler-alist))))

;; Measure and log startup time
(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "Startup took %.3fs" elapsed)))))

;;; ------ Garbage Collection
(let ((normal-gc-cons-threshold (* 256 1024 1024))
      (init-gc-cons-threshold most-positive-fixnum))

  ;; Set GC threshold wayy too high so there's no GC during startup and restore to
  ;; a normal value after startup
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))

  ;; Raise GC thresholds when minibuffer is active
  (add-hook 'minibuffer-setup-hook
	    (lambda () (setq gc-cons-threshold (* normal-gc-cons-threshold 2))))

  ;; Restore GC thresholds when minibuffer exits
  (add-hook 'minibuffer-exit-hook
	    (lambda ()
	      (progn
		(garbage-collect)
		(setq gc-cons-threshold normal-gc-cons-threshold)))))

;;; ------ Variables
(setq user-full-name "Abhilash Meesala"
      user-mail-address "mail@abhilashm.me")

(defconst maze-themes-directory
  (expand-file-name "themes" user-emacs-directory))

(defconst maze-core-directory
  (expand-file-name "core" user-emacs-directory))

(defconst maze-snippets-directory
  (expand-file-name "snippets" user-emacs-directory))

(defconst maze-etc-directory
  (expand-file-name "etc" user-emacs-directory))

(defconst maze-is-mac (eq system-type 'darwin)
  "Is this environment a mac?")

;;; ------ Lets go!
;; Setup `load-path` so that we can `require` core files
(add-to-list 'load-path maze-core-directory)

(require 'package-management)
(require 'core)
(require 'ui)
(require 'editing)
(require 'completions)
(require 'project-management)
(require 'win)
(require 'keybindings)

(provide 'init)
;;; init.el ends here

