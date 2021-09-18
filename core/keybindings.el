;;; keybindings.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package bind-key)

(setq mac-option-modifier 'meta
        mac-command-modifier 'super)
(global-set-key [(super a)] #'mark-whole-buffer)
(global-set-key [(super v)] #'yank)
(global-set-key [(super c)] #'kill-ring-save)
(global-set-key [(super s)] #'save-buffer)
(global-set-key [(super l)] #'goto-line)
(global-set-key [(super w)] #'delete-frame)
(global-set-key [(super z)] #'undo)
(global-set-key [(super q)] #'save-buffers-kill-terminal)

(provide 'keybindings)
;;; keybindings.el ends here
