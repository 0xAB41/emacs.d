;;; init.el --- Load the full configuration -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(setq inhibit-compacting-font-caches t
      tmp--file-name-handler-alist file-name-handler-alist
      file-name-handler-alist nil)

(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold 800000
                  gc-cons-percentage 0.1
                  inhibit-compacting-font-caches nil
                  file-name-handler-alist tmp--file-name-handler-alist
                  inhibit-redisplay nil)
            (redisplay :force)
            (makunbound 'tmp--file-name-handler-alist)
            (message "Start up took %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(require 'org)
(org-babel-load-file (expand-file-name (concat user-emacs-directory "config.org")))

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)

(provide 'init)
;;; init.el ends here