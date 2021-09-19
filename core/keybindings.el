;;; keybindings.el --- -*- lexical-binding: t; -*-
;;; Commentary:
;;; Code:

(use-package bind-key)

(use-package discover-my-major)

(when maze-is-mac
  (setq mac-option-modifier 'meta
        mac-command-modifier 'super)

  (global-set-key [(super c)] #'maze/osx-clipboard-copy)
  (global-set-key [(super v)] #'maze/osx-clipboard-paste)
  (global-set-key [(super x)] #'maze/osx-clipboard-cut)

  (global-set-key [(super a)] #'mark-whole-buffer)
  (global-set-key [(super l)] #'avy-goto-line)

  (global-set-key [(super s)] #'save-buffer)
  (global-set-key [(super z)] #'undo)
  (global-set-key [(super q)] #'save-buffers-kill-terminal)
  (global-set-key [(super w)] #'delete-frame)
  
  (global-set-key [(super f)] #'swiper))

(use-package general
  :config
  (general-evil-setup t))

(nvmap :keymaps 'override :prefix "SPC"
  "SPC" '(counsel-M-x :which-key "M-x"))

;; Buffer menu
(nvmap :prefix "SPC"
  "b b" '(counsel-switch-buffer :which-key "Switch buffer")
  "b r" '(counsel-buffer-or-recentf :which-key "Recent buffers")
  "b p" '(previous-buffer :which-key "Previous Buffer")
  "b n" '(next-buffer :which-key "Next Buffer")
  "b d" '(kill-buffer :which-key "Kill Buffer"))

;; File menu
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "f ." '(find-file-at-point :which-key "Find file at point")
  "f f" '(find-file :which-key "Find file")
  "f s" '(save-buffer :which-key "Save file")
  "f S" '(maze/save-all-buffers :which-key "Save all buffers"))

;; Project menu
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "p ." '(find-file-in-project-at-point :which-key "Find file in project")
  "p f" '(projectile-find-file :which-key "Find file in project")
  "p /" '(projectile-ripgrep :which-key "Search"))

;; Help menu
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "h d f" '(counsel-describe-function :which-key "Describe function")
  "h d v" '(counsel-describe-variable :which-key "Describe variable")
  "h d s" '(counsel-describe-symbol :which-key "Describe symbol")
  "h d k" '(describe-key :which-key "Describe key")
  "h m" '(discover-my-major :which-key "Describe major mode keybindings")
  "h ?" '(counsel-apropos :which-key "Apropos"))

;; Window/Frame menu
(nvmap :states '(normal visual) :keymaps 'override :prefix "SPC"
  "w d" '(ace-delete-window :which-key "Delete window")
  "w D" '(ace-delete-other-windows :which-key "Delete other windows")
  "w t" '(ace-swap-window :which-key "Transpose/Swap windows")
  "w u" '(winner-undo :which-key "Undo window change")
  "w C-r" '(winner-redo :which-key "Redo window change")
  "w h" '(evil-window-split :which-key "Split window horizontally")
  "w v" '(evil-window-vsplit :which-key "Split window vertically"))

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
