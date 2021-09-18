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

;; Adjust garbage collection thresholds
(let ((normal-gc-cons-threshold (* 256 1024 1024))
      (init-gc-cons-threshold most-positive-fixnum))

  ;; Set GC threshold wayy too high so there's no GC during startup
  (setq gc-cons-threshold init-gc-cons-threshold)

  ;; Restore GC thresholds to reasonable level after startup
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold)))

  ;; Raise GC thresholds when minibuffer is active
  (defun minibuffer-gc-setup-hook()
    (setq gc-cons-threshold (* normal-gc-cons-threshold 2)))

  ;; Restore GC thresholds when minibuffer exits
  (defun minibuffer-gc-exit-hook()
    (garbage-collect)
    (setq gc-cons-threshold normal-gc-cons-threshold))

  (add-hook 'minibuffer-setup-hook #'minibuffer-gc-setup-hook)
  (add-hook 'minibuffer-exit-hook #'minibuffer-gc-exit-hook))

(dolist (dir '("pkg" "core"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

(require 'core)
(require 'package-management)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :config
  (exec-path-from-shell-initialize))


(require 'ui)
(require 'editing)
(require 'completions)
(require 'project-management)
(require 'win)
(require 'keybindings)


(provide 'init)
;;; init.el ends here


