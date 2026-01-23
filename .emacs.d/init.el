(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

;; Grumpy old vim user
;; I'm sure there exists an actual solution
;; but I am very tired and can't be fucked.
(defvar lmdn/project-root-directory default-directory
  "Stupid 'track where my PWD should be' variable.")

(defvar lmdn/default-font-height 180 "Default font height.")

(defconst dependencies '(evil
                         evil-commentary
                         evil-surround
                         undo-fu
                         magit
                         projectile
                         treemacs
                         treemacs-evil
                         treemacs-magit
                         treemacs-projectile
                         which-key
                         posframe
                         smartparens
                         lsp-mode
                         lsp-ui
                         rustic
                         go-mode
                         lsp-pyright
                         vertico
                         orderless
                         marginalia
                         consult
                         corfu
                         corfu-terminal
                         kind-icon
                         cape
                         vterm
                         fold-this)
  "Packages being used in config.")

(defconst suffering-mode
  (and (eq system-type 'gnu/linux)
       (getenv "WSL_DISTRO_NAME"))
  "Non-nil if running in WSL with -nw mode.")

(defconst suffering-mode-gui
  (and (eq system-type 'gnu/linux)
       (getenv "WSL_DISTRO_NAME")
       (display-graphic-p))
  "Non-nil if running in WSL with X forwarding.")

(defconst macbook-mode
  (string= (system-name) "shitbook-pro")
  "Non-nil if matching my MBP hostname.")

;; Refresh packages if needed
(unless package-archive-contents
  (package-refresh-contents))

;; Add config directory to load path
(add-to-list 'load-path "~/.emacs.d/config/")

(setenv "PATH" (concat (getenv "PATH") ":" (expand-file-name "~/go/bin")))
(setq exec-path (append exec-path (list (expand-file-name "~/go/bin"))))

;; WSL clipboard
(if suffering-mode
    (load "wsl-clipboard"))

;; Terminal mode fixes
(when (not (display-graphic-p))
  (tooltip-mode -1)
  (setq tooltip-use-echo-area t
	mouse-drag-copy-region nil
	eldoc-echo-area-use-multiline-p nil
	eldoc-echo-area-display-truncation-message nil))

;; Basic settings
(setq inhibit-startup-screen t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(fringe-mode 1)
(global-font-lock-mode 1)
(when (display-graphic-p)
  (xterm-mouse-mode 1))

;; Unfuck scrolling
(setq scroll-margin 5
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position t
      auto-window-vscroll nil)

;; No swap files
(setq make-backup-files nil
      auto-save-default nil
      create-lockfiles nil)

(setq display-line-numbers-type 'relative)
(setq-default display-line-numbers-width 4
	      display-line-numbers-width-start t)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      process-adaptive-read-buffering nil)

(global-display-line-numbers-mode t)
(dolist (mode '(org-mode-hook
		term-mode-hook
		shell-mode-hook
		treemacs-mode-hook
		eshell-mode-hook))
  (add-hook mode (lambda () (display-line-numbers-mode 0))))

(dolist (pkg dependencies)
  (unless (package-installed-p pkg)
    (package-install pkg)))

(setq org-directory "~/.org/")

(setq magit-diff-refine-hunk t
      magit-repository-directories '(("/github" . 2))
      epg-pinentry-mode 'loopback
      epa-pinentry-mode 'loopback)

(require 'magit)

;; Load configuration modules
(load "evil-mode")
(load "theme")
(load "completion")
(load "misc")
(load "tree")
(load "projects")
(load "keybinds")
(load "lsp")
(load "terminal")
(load "modeline")
(load "zoom")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(cape consult corfu evil-commentary evil-surround go-mode kind-icon
	  lsp-pyright lsp-ui marginalia orderless rust-mode
	  smartparens treemacs-evil treemacs-magit treemacs-projectile
	  undo-fu vertico vterm)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
