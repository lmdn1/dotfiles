;; TODO: non-shit theme
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme 'purple t)

(unless (or suffering-mode-gui macbook-mode)
  ;; Transparency
  (set-frame-parameter (selected-frame) 'alpha '(100 . 100))
  (add-to-list 'default-frame-alist '(alpha . (100 . 100)))

  (set-face-background 'default "#000000")
  (set-face-background 'fringe "#000000")
  (set-face-background 'mode-line "#291F2D")
  (set-face-background 'mode-line-inactive "#1B141E")
  (set-face-background 'line-number "#1B141E")
  (set-face-background 'line-number-current-line "#1B141E"))

(set-face-attribute 'default nil
                    :family "Linux Libertine Mono O"
                    :height lmdn/default-font-height)
