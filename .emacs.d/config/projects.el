(setq projectile-completion-system 'default
      projectile-enable-caching t
      projectile-indexing-method 'alien
      projectile-sort-order 'recentf)

;; Use ripgrep if available for faster searching
(when (executable-find "rg")
  (setq projectile-generic-command "rg --files --color=never --null"))

(defun lmdn/update-root-on-project-switch ()
  "Sets project root upon switching"
  (when (projectile-project-p)
    (setq lmdn/project-root-directory (projectile-project-root))
    (let ((treemacs-window (treemacs-get-local-window)))
      (when treemacs-window
        (with-selected-window treemacs-window
                              (treemacs-quit))))))


(require 'projectile)
(projectile-mode 1)

(add-hook 'projectile-after-switch-project-hook
          #'lmdn/update-root-on-project-switch)
