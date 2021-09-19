;;; functions.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:
(defun maze/edit-config ()
  "open emacs config file(init.el)"
  (interactive)
  (find-file (expand-file-name "init.el" user-emacs-directory)))

;;; ------ Buffer/File/Window related
(defun maze/save-all-buffers ()
  "save all opened buffers"
  (interactive)
  (save-some-buffers t))

(defun maze/delete-file-and-buffer ()
  "Delte the current file and the buffer"
  (interactive)
  (let ((filename (buffer-file-name)))
    (when filename
      (if (vc-backend filename)
	  (vc-delete-file filename)
	(when (y-or-n-p (format "Delete %s?" filename))
	  (delete-file filename delete-by-moving-to-trash)
	  (message "Deleted %s" filename)
	  (kill-buffer))))))

;;; ------ Clipboard Utilities (Mac)
(defun maze/osx-clipboard-copy (start end)
  "copy region to system clipboard"
  (interactive "r")
  (shell-command-on-region start end "pbcopy"))

(defun maze/osx-clipboard-paste ()
  "paste from system clipboard"
  (interactive)
  (insert (shell-command-to-string "pbpaste")))

(defun maze/osx-clipboard-cut (start end)
  "cut region to system clipboard"
  (interactive "r")
  (osx-clipboard-copy start end)
  (delete-region start end))


(provide 'functions)
;;; functions.el ends here
