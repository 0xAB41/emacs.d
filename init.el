;;; init.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;(setq debug-on-error t)

;; Measure and log startup time
(let ((emacs-start-time (current-time)))
  (add-hook 'emacs-startup-hook
	    (lambda ()
	      (let ((elapsed (float-time (time-subtract (current-time) emacs-start-time))))
                (message "Startup took %.3fs" elapsed)))))

;; Reset file-name-handler-alist during initialization
(let ((init-file-name-handler-alist file-name-handler-alist))
  (setq file-name-handler-alist nil)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq file-name-handler-alist init-file-name-handler-alist))))

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

(defconst maze-pkg-directory
  (expand-file-name "pkg" user-emacs-directory))

(defconst maze-is-mac (eq system-type 'darwin)
  "Is this environment a mac?")

(defconst maze-gc-threshold (* 256 1024 1024)
  "The value (in bytes) at which GC is trigerred. This value is passed down to `gc-cons-threshold'.
If you experience freezing, decrease this. If you experience stuttering, increase this.")

;;; ------ Garbage Collection
(let ((init-gc-cons-threshold most-positive-fixnum))
  ;; Set GC threshold wayy too high so there's no GC during startup and restore to
  ;; a normal value after startup
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'emacs-startup-hook
	    (lambda () (setq gc-cons-threshold maze-gc-threshold)))

  ;; Raise GC thresholds when minibuffer is active
  (add-hook 'minibuffer-setup-hook
	    (lambda () (setq gc-cons-threshold (* maze-gc-threshold 2))))

  ;; Restore GC thresholds when minibuffer exits
  (defun maze--minibuffer-exit-gc-h ()
    (garbage-collect)
    (setq gc-cons-threshold maze-gc-threshold))
  (add-hook 'minibuffer-exit-hook #'maze--minibuffer-exit-gc-h))

;;; ------ Add required directories to `load-path'
(defun maze--add-to-load-path (f)
    (unless (member f load-path)
    (add-to-list 'load-path f)))

(defun maze--add-to-load-path-with-subdirs (base-folder exclude-list)
  (let ((should-add-file-p (lambda (f)
			     (and (file-directory-p (expand-file-name f base-folder))
				  (not (equal f "."))
				  (not (equal f ".."))
				  (not (member f exclude-list))))))
    
    (maze--add-to-load-path base-folder)

    ;; Add *only* first level folders to load-path
    (dolist (file-name (directory-files base-folder))
      (when (funcall should-add-file-p file-name)
	(maze--add-to-load-path (expand-file-name file-name base-folder))))))

(maze--add-to-load-path-with-subdirs maze-core-directory '())
(maze--add-to-load-path-with-subdirs maze-pkg-directory '())

;;; ------ Let's go!
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
