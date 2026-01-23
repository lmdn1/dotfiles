;; Which-key
(require 'which-key)
(which-key-mode 1)
(setq which-key-idle-delay 0.3
      which-key-frame-max-height 50)

;; Smartparens
(require 'smartparens-config)

(smartparens-global-mode t)
(setq show-paren-delay 0)

;; Basic pairing
(sp-pair "(" ")")
(sp-pair "[" "]")
(sp-pair "{" "}")
(sp-pair "\"" "\"")

;; C-like syntax
(sp-with-modes '(c-mode c++-mode go-mode rustic-mode javascript-mode)
               (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
               (sp-local-pair "[" nil :post-handlers '(("||\n[i]" "RET")))
               (sp-local-pair "(" nil :post-handlers '(("||\n[i]" "RET")))
               (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                                         ("* ||\n[i]" "RET"))))

;; org stuff
(require 'org)

(setq org-directory "~/.org"
      org-default-notes-file (concat org-directory "/dumping-ground.org")
      org-startup-indented t
      org-hide-leading-stars t
      org-pretty-entities t
      org-log-done 'time)
