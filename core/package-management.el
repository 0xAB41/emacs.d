;;; package-management.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(require 'package)

;; Save all packages under a directory versioned based on major, minor emacs version
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
			 user-emacs-directory)))
  (setq package-user-dir versioned-package-dir))

;; gnupg does not honor the `package-user-dir` 
(when (boundp 'package-gnupghome-dir)
  (setq package-gnupghome-dir
        (expand-file-name "gnupg" package-user-dir)))

(add-to-list 'package-archives '( "melpa" . "https://melpa.org/packages/") t)

(unless (bound-and-true-p package--initialized) ; To avoid warnings in 27
  (setq package-enable-at-startup nil)          ; To prevent initializing twice
  (package-initialize))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)		; Always ensure
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(require 'use-package)

(use-package gnu-elpa-keyring-update)

(provide 'package-management)
;;; package-management.el ends here
