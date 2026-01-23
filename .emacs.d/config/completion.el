(require 'vertico)
(require 'orderless)
(require 'corfu)
(require 'cape)
(require 'consult)

;; Vertico - M-x completion
(vertico-mode 1)
(vertico-multiform-mode 1)

(setq vertico-cycle t)
(setq vertico-count 10)
(setq vertico-resize t)

;; Orderless - Fuzzy finding
(setq completion-styles '(orderless basic))
(setq completion-category-defaults nil)
(setq completion-category-overrides
      '((file (styles basic partial-completion))))

(setq orderless-matching-styles
      '(orderless-prefixes
         orderless-literal
         orderless-flex
         orderless-initialism
         orderless-regexp))

;; Marginalia - Annoy-tations (might remove)
(marginalia-mode 1)

;; Consult - Used for evil command mode completions
(defun lmdn/setup-consult-completion ()
  "Use consult in evil's command mode."
  (when (or (minibufferp)
            (eq major-mode 'evil-ex-mode))
    (setq-local completion-in-region-function 'consult-completion-in-region)))
(add-hook 'minibuffer-setup-hook #'lmdn/setup-consult-completion)

;; Corfu - Main buffer completions
(setq corfu-auto t)                     ; Enable auto completion
(setq corfu-auto-delay 0)               ; Small delay for performance
(setq corfu-auto-prefix 1)              ; Minimum prefix length
(setq corfu-cycle t)                    ; Enable cycling for `corfu-next/previous'
(setq corfu-quit-at-boundary t)         ; Never quit at completion boundary
(setq corfu-quit-no-match 'separator)   ; Quit if no match and separator
(setq corfu-preview-current 'insert)    ; Preview current candidate
(setq corfu-preselect 'first)           ; Preselect the first item, i.e make it useful
(setq corfu-on-exact-match nil)         ; Configure handling of exact matches
(setq corfu-max-width 40)               ; The completions box looks pretty bad
(setq corfu-min-width 40)               ; at least this makes it consistentish

(corfu-history-mode)

(corfu-popupinfo-mode)              ; Enable popup definition info, might remove
(setq corfu-popupinfo-delay 0.5
      corfu-popupinfo-max-width 80
      corfu-popupinfo-max-height 20
      corfu-popupinfo-direction 'top)

(with-eval-after-load 'corfu
                      (define-key corfu-map (kbd "RET") #'corfu-insert)
                      (define-key corfu-map (kbd "<return>") #'corfu-insert))

(require 'kind-icon)          ; after corfu
(setq kind-icon-use-icons nil
      kind-icon-default-face 'corfu-default)
(add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter)

;; Corfu gets possessive over evil mode's command buffer.
;; So we have to do this bs to make it leave us alone.
(add-hook 'prog-mode-hook #'corfu-mode)
(add-hook 'text-mode-hook #'corfu-mode)  
(add-hook 'org-mode-hook #'corfu-mode)
(add-hook 'minibuffer-setup-hook (lambda () (corfu-mode -1)))
(add-hook 'evil-ex-mode-hook (lambda () (corfu-mode -1)))

;; -nw mode support
(when nw-mode
  (require 'corfu-terminal)
  (corfu-terminal-mode +1))

(with-eval-after-load 'cape
                      (advice-add #'lsp-completion-at-point :around #'cape-wrap-noninterruptible)
                      (advice-add #'lsp-completion-at-point :around #'cape-wrap-nonexclusive)
                      (advice-add #'lsp-completion-at-point :around #'cape-wrap-silent))

(defun lmdn/lsp-corfu-setup ()
  "Setup Corfu integration with LSP."
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                      #'lsp-completion-at-point
                      #'cape-keyword
                      #'cape-file))))

(add-hook 'lsp-completion-mode-hook #'lmdn/lsp-corfu-setup)
