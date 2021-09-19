;;; win.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package ace-window)

(use-package winner
  :config
  (setq winner-boring-buffers
	'("*Completions*"
	  "*Compile-Log*"
	  "*inferior-lisp*"
	  "*Fuzzy Completions*"
	  "*Apropos*"
	  "*Help*"
	  "*cvs*"
	  "*Buffer List*"
	  "*Ibuffer*"))
  (winner-mode 1))

(use-package dashboard)

(provide 'win)
;;; win.el ends here
