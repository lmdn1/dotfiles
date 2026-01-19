(require 'lsp-mode)
(require 'lsp-ui)
(require 'posframe)

(setq lsp-completion-provider :none
      lsp-headerline-breadcrumb-enable nil
      lsp-ui-sideline-enable nil
      lsp-ui-doc-enable nil
      lsp-modeline-code-actions-enable nil
      lsp-modeline-diagnostics-enable nil
      lsp-completion-use-last-result nil
      lsp-auto-configure t
      lsp-keep-workspace-alive nil
      lsp-enable-xref t
      lsp-enable-dap-auto-configure t
      lsp-enable-file-watchers nil
      lsp-enable-indentation t
      lsp-enable-links nil
      lsp-enable-suggest-server-download t
      lsp-enable-symbol-highlighting t
      lsp-completion-enable t
      lsp-completion-enable-additional-text-edit t
      lsp-enable-snippet t
      lsp-completion-show-kind t
      lsp-completion-show-detail t
      lsp-completion-filter-on-incomplete t
      lsp-diagnostics-provider :auto)

(lsp-diagnostics-mode)

(setq lsp-eldoc-enable-hover nil
      lsp-signature-auto-activate nil)

;; Performance optimizations
(setq lsp-idle-delay 0)
(setq lsp-log-io nil)

;; LSP UI settings
(setq lsp-ui-doc-delay 0.5)
(setq lsp-ui-doc-max-width 80)
(setq lsp-ui-doc-max-height 20)
(setq lsp-ui-doc-position 'top-right-corner)

;; Floating function signatures
(add-hook 'lsp-mode-hook #'lsp-signature-mode)
(setq lsp-signature-auto-activate t
      lsp-signature-render-documentation t
      lsp-signature-function 'lmdn/show-signature-posframe)

;; Enable LSP + completion mode (Doom's approach)
(dolist (hook '(c-mode-hook
                c++-mode-hook
                go-mode-hook
                rustic-mode-hook
                python-mode-hook
                js-mode-hook
                sh-mode-hook))
  (add-hook hook #'lsp-deferred))

(add-hook 'lsp-mode-hook #'lsp-completion-mode)


;; Language specific options
(with-eval-after-load 'lsp-mode
  (setq lsp-clients-clangd-args
        '("--background-index"
          "--all-scopes-completion"
          "--limit-results=0"
          "--clang-tidy"
          "--header-insertion=iwyu"
          "--completion-style=detailed"
          "--function-arg-placeholders"
          "--fallback-style=llvm")))

(with-eval-after-load 'lsp-pyright
  (setq lsp-pyright-langserver-command-args '("--stdio" "--verbose")
        lsp-pyright-use-library-code-for-types t
        lsp-pyright-auto-import-completions t
        lsp-pyright-auto-search-paths t))

(defun lsp-go-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'go-mode-hook #'lsp-go-install-save-hooks)

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration '(sh-mode . "shellscript"))
  (lsp-register-client
   (make-lsp-client :new-connection (lsp-stdio-connection "bash-language-server start")
                    :major-modes '(sh-mode)
                    :server-id 'bash-ls)))

(with-eval-after-load 'rustic
  (setq rustic-lsp-client 'lsp-mode
        rustic-format-on-save t
        rustic-indent-method 'rustfmt))
