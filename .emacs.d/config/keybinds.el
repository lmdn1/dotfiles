;;; keybinds.el -*- lexical-binding: t; -*-

;; Funny mode keybinds
(with-eval-after-load 'evil
  (evil-set-leader 'normal (kbd "SPC"))
  (evil-define-key 'normal 'global (kbd "<leader>d") 'lmdn/dired-project)
  (evil-define-key 'normal 'global (kbd "<leader>D") 'dired)
  (evil-define-key 'normal 'global (kbd "<leader>cc") 'compile)
  (evil-define-key 'normal 'global (kbd "<leader>rc") 'recompile)
  (evil-define-key 'normal 'global (kbd "<leader>wq") 'delete-window)
  (evil-define-key 'normal 'global (kbd "<leader>-") 'split-window-below)
  (evil-define-key 'normal 'global (kbd "<leader>=") 'split-window-right)
  (evil-define-key 'normal 'global (kbd "<leader>;") 'comment-dwim)
  (evil-define-key 'visual 'global (kbd "<leader>;") 'comment-dwim)
  (evil-define-key 'normal 'global (kbd "<leader>ff") 'find-file)
  (evil-define-key 'normal 'global (kbd "<leader>bb") 'consult-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>bv") 'ibuffer)
(with-eval-after-load 'treemacs
  (evil-define-key 'normal 'global (kbd "<leader>e") 'lmdn/treemacs-toggle)
  (evil-define-key 'normal 'global (kbd "<leader>E") 'lmdn/treemacs-open-here))
(with-eval-after-load 'magit
  (evil-define-key 'normal 'global (kbd "<leader>gs") 'magit-status)
  (evil-define-key 'normal 'global (kbd "<leader>gb") 'magit-blame)
  (evil-define-key 'normal 'global (kbd "<leader>gl") 'magit-log-current)
  (evil-define-key 'normal 'global (kbd "<leader>gf") 'magit-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>gc") 'magit-commit))
(with-eval-after-load 'vterm
  (evil-define-key 'normal 'global (kbd "<leader>ft") 'lmdn/vterm-toggle)
  (evil-define-key 'normal 'global (kbd "<leader>fT") 'lmdn/vterm-project))
(with-eval-after-load 'projectile
  (evil-define-key 'normal 'global (kbd "<leader>pf") 'projectile-find-file)
  (evil-define-key 'normal 'global (kbd "<leader>pd") 'projectile-find-dir)
  (evil-define-key 'normal 'global (kbd "<leader>pr") 'projectile-recentf)
  (evil-define-key 'normal 'global (kbd "<leader>ps") 'projectile-ripgrep)
  (evil-define-key 'normal 'global (kbd "<leader>pa") 'projectile-ag)
  (evil-define-key 'normal 'global (kbd "<leader>pp") 'projectile-switch-project)
  (evil-define-key 'normal 'global (kbd "<leader>pb") 'projectile-switch-to-buffer)
  (evil-define-key 'normal 'global (kbd "<leader>pc") 'projectile-compile-project)
  (evil-define-key 'normal 'global (kbd "<leader>pt") 'projectile-test-project)
  (evil-define-key 'normal 'global (kbd "<leader>pi") 'projectile-invalidate-cache))
(with-eval-after-load 'lsp-mode
  (evil-define-key 'normal 'global (kbd "<leader>cr") 'lsp-rename)
  (evil-define-key 'normal 'global (kbd "<leader>ca") 'lsp-execute-code-action)
  (evil-define-key 'normal 'global (kbd "gd") 'lsp-find-definition)
  (evil-define-key 'normal 'global (kbd "gD") 'lsp-find-declaration)
  (evil-define-key 'normal 'global (kbd "gr") 'lsp-find-references)))

;; Treemacs keybinds
(with-eval-after-load 'treemacs
  (define-key treemacs-mode-map (kbd "a") 'treemacs-create-file)
  (define-key treemacs-mode-map (kbd "A") 'treemacs-create-dir)
  (define-key treemacs-mode-map (kbd "d") 'treemacs-delete-file)
  (define-key treemacs-mode-map (kbd "r") 'treemacs-rename-file)
  (define-key treemacs-mode-map (kbd "P") 'lmdn/treemacs-import-as-project))



(defun lmdn/evil-n-or-i-command (insert-cmd normal-cmd)
  "Allows us to bind a normal action as well as an insert action."
  (lambda ()
    (interactive)
    (if (and (bound-and-true-p evil-mode)
             (eq evil-state 'insert))
        (funcall insert-cmd)
      (funcall normal-cmd))))

;; Standard keybinds
(global-set-key (kbd "C-<left>")
  (lmdn/evil-n-or-i-command #'left-word #'windmove-left))
(global-set-key (kbd "C-<right>")
  (lmdn/evil-n-or-i-command #'right-word #'windmove-right))
(global-set-key (kbd "C-<up>")
  (lmdn/evil-n-or-i-command #'backward-paragraph #'windmove-up))
(global-set-key (kbd "C-<down>")
  (lmdn/evil-n-or-i-command #'forward-paragraph #'windmove-down))
(global-set-key (kbd "C-=") #'lmdn/zoom-in)
(global-set-key (kbd "C--") #'lmdn/zoom-out)
(global-set-key (kbd "C-0") #'lmdn/zoom-reset)

(defun lmdn/dired-project ()
  "Open dired in project root, or PWD if not in a project."
  (interactive)
  (dired (if (and (bound-and-true-p projectile-mode) (projectile-project-p))
             (projectile-project-root)
           default-directory)))
