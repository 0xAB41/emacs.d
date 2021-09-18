;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;; Measure and log startup time
(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "Startup took %.3fs" elapsed)))))

;; Adjust garbage collection thresholds during startup
(let ((normal-gc-cons-threshold (* 256 1024 1024))
      (init-gc-cons-threshold (* 1024 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

(dolist (dir '("pkg" "core"))
  (add-to-list 'load-path (expand-file-name dir user-emacs-directory)))

(require 'core)
(require 'package-management)
(require 'ui)
(require 'editing)
(require 'completions)
(require 'keybindings)

(provide 'init)
;;; init.el ends here


