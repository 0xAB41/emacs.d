;;; untitled-theme.el --- untitled
;;; Version: 1.0
;;; Commentary:
;;; A theme called untitled
;;; Code:

(deftheme untitled "DOCSTRING for untitled")
  (custom-theme-set-faces 'untitled
   '(default ((t (:foreground "#1F2937" :background "#F3F4F6" ))))
   '(cursor ((t (:background "#000000" ))))
   '(fringe ((t (:background "#F3F4F6" ))))
   '(mode-line ((t (:foreground "#1F2937" :background "#D1D5DB" ))))
   '(region ((t (:background "#FCD34D" ))))
   '(secondary-selection ((t (:background "#fcd34d" ))))
   '(font-lock-builtin-face ((t (:foreground "#1f2937" ))))
   '(font-lock-comment-face ((t (:foreground "#1f2937" ))))
   '(font-lock-function-name-face ((t (:foreground "#1f2937" ))))
   '(font-lock-keyword-face ((t (:foreground "#1f2937" ))))
   '(font-lock-string-face ((t (:foreground "#1f2937" ))))
   '(font-lock-type-face ((t (:foreground "#1f2937" ))))
   '(font-lock-constant-face ((t (:foreground "#1f2937" ))))
   '(font-lock-variable-name-face ((t (:foreground "#1f2937" ))))
   '(minibuffer-prompt ((t (:foreground "#7299ff" :bold t ))))
   '(font-lock-warning-face ((t (:foreground "red" :bold t ))))
   )

;;;###autoload
(and load-file-name
    (boundp 'custom-theme-load-path)
    (add-to-list 'custom-theme-load-path
                 (file-name-as-directory
                  (file-name-directory load-file-name))))
;; Automatically add this theme to the load path

(provide-theme 'untitled)

;;; untitled-theme.el ends here
