(deftheme purple "purple :)")

(let ((class '((class color) (min-colors 89)))
      (bg "#000000")
      (fg "#ffffff")
      (dim "#999999")
      (purple "#af87d8")
      (purple-alt "#71578b")
      (comment "#666666")
      (region-bg "#4a2a6d"))

  (custom-theme-set-faces
   'purple

   `(default ((,class (:background ,bg :foreground ,fg))))
   `(cursor ((,class (:background ,purple))))
   `(fringe ((,class (:background ,bg))))
   `(region ((,class (:background ,region-bg :foreground ,fg))))
   `(highlight ((,class (:background ,region-bg :foreground ,fg))))

   `(show-paren-match ((,class (:background ,purple-alt :foreground ,bg :weight bold))))
   `(show-paren-mismatch ((,class (:background "red" :foreground ,fg :weight bold))))

   `(font-lock-builtin-face ((,class (:foreground ,purple))))
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))))
   `(font-lock-function-name-face ((,class (:foreground ,purple))))
   `(font-lock-keyword-face ((,class (:foreground ,purple :weight bold))))
   `(font-lock-preprocessor-face ((,class (:foreground "#696969" :weight bold))))
   `(font-lock-constant-face ((,class (:foreground ,dim))))
   `(font-lock-string-face ((,class (:foreground "#BBBBBB"))))
   `(font-lock-type-face ((,class (:foreground ,purple-alt))))
   `(font-lock-variable-name-face ((,class (:foreground ,fg))))
   `(font-lock-warning-face ((,class (:foreground "red" :weight bold))))

   `(lsp-face-highlight-write ((,class (:background ,region-bg :foreground ,fg))))
   `(lsp-face-highlight-textual ((,class (:background ,region-bg :foreground ,fg))))
   `(lsp-face-highlight-read ((,class (:background ,region-bg :foreground ,fg))))

   `(hl-line ((,class (:background ,region-bg :foreground ,fg))))

   `(corfu-default ((,class (:background ,bg :foreground ,fg))))
   `(corfu-current ((,class (:background ,region-bg :foreground ,fg :weight bold))))
   `(corfu-border ((,class (:background ,purple-alt))))
   `(corfu-bar ((,class (:background ,purple))))
   `(corfu-annotations ((,class (:foreground ,dim :slant italic))))
   `(corfu-deprecated ((,class (:foreground ,comment :strike-through t))))
   `(corfu-echo ((,class (:foreground ,dim))))

   `(lsp-signature-posframe
     ((,class (:background ,bg :foreground ,purple))))
   `(lsp-signature-posframe-border
     ((,class (:background ,purple-alt))))

   `(mode-line ((,class (:background "#111111" :foreground ,fg))))
   `(mode-line-inactive ((,class (:background "#111111" :foreground ,dim))))
   `(minibuffer-prompt ((,class (:foreground ,purple :bold t))))
   `(link ((,class (:foreground ,purple :underline t))))
   ))

(provide-theme 'purple)
