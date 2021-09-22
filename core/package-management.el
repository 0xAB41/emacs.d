;;; package-management.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

;;; ------ Configure package manager
(require 'package)

;; Save all packages under a directory versioned based on major, minor emacs version
(let ((versioned-package-dir
       (expand-file-name (format "elpa-%s.%s" emacs-major-version emacs-minor-version)
			 maze-etc-directory)))
  (setq package-user-dir versioned-package-dir))

;; gnupg does not honor the `package-user-dir` 
(when (boundp 'package-gnupghome-dir)
  (setq package-gnupghome-dir
        (expand-file-name "gnupg" package-user-dir)))

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(unless (bound-and-true-p package--initialized)
  (setq package-enable-at-startup nil) 
  (package-initialize))

;;; ------ use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-and-compile
  (setq use-package-always-ensure t)
  (setq use-package-expand-minimally t)
  (setq use-package-enable-imenu-support t))

(require 'use-package)

(use-package gnu-elpa-keyring-update)

;; TODO: straight.el


;;; ------ auto updates
(use-package auto-package-update
  :custom
  (auto-package-update-interval 45)
  (auto-package-update-prompt-before-update t)
  (auto-package-update-hide-results t)
  :config
  (auto-package-update-maybe)
  (auto-package-update-at-time "09:00"))

(provide 'package-management)
;;; package-management.el ends here
