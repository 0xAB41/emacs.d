;;; keybindings.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package bind-key)

(when is-mac
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super)
  (global-set-key [(super c)] #'osx-clipboard-copy)
  (global-set-key [(super v)] #'osx-clipboard-paste)
  (global-set-key [(super x)] #'osx-clipboard-cut)
  (global-set-key [(super a)] #'mark-whole-buffer)
  (global-set-key [(super s)] #'save-buffer)
  (global-set-key [(super l)] #'goto-line)
  (global-set-key [(super w)] #'delete-frame)
  (global-set-key [(super z)] #'undo)
  (global-set-key [(super q)] #'save-buffers-kill-terminal))

(use-package general
  :config
  (general-evil-setup t))

(nvmap :keymaps 'override :prefix "SPC"
  "SPC" '(counsel-M-x :which-key "M-x"))

;; Buffer related keybindings
(nvmap :prefix "SPC"
  "b p" '(previous-buffer :which-key "Previous Buffer")
  "b n" '(next-buffer :which-key "Next Buffer")
  "b d" '(kill-buffer :which-key "Kill Buffer"))

;; File related keybindings
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "f f" '(find-file :which-key "Find file")
  "f s" '(save-buffer :which-key "Save file")
  "f S" '(save-all-buffers :which-key "Save all buffers"))

(use-package which-key
  :init
  (setq which-key-side-window-location 'bottom
        which-key-sort-order #'which-key-key-order-alpha
        which-key-sort-uppercase-first nil
        which-key-add-column-padding 1
        which-key-max-display-columns nil
        which-key-min-display-lines 6
        which-key-side-window-slot -10
        which-key-side-window-max-height 0.25
        which-key-idle-delay 0.8
        which-key-max-description-length 25
        which-key-allow-imprecise-window-fit t
        which-key-separator " → " )
  :config
  (which-key-mode))

(provide 'keybindings)
;;; keybindings.el ends here
