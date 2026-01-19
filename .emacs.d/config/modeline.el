(setq-default mode-line-format
              '(;; Evil state first
                (:eval (when (bound-and-true-p evil-mode)
                         (concat evil-mode-line-tag " ")))
                ;; Buffer name
                "%b"
                ;; Show * if modified.
                (:eval (when (buffer-modified-p) " *"))
                "   "
                ;; Position info (like "Top (28,0)")
                "%p "
                "(%l,%c)"
                "   "
                ;; Minimal mode info - just the major mode
                "(" mode-name
                ;; Only show if LSP is active
                (:eval (when (bound-and-true-p lsp-mode) " LSP"))
                (:eval (when (and (bound-and-true-p lsp-mode)
                                  (boundp 'lsp--modeline-workspace-status))
                         (concat " " (lsp--workspace-status-string))))
                ")"))

(with-eval-after-load 'lsp-mode
                      (setq lsp-modeline-code-actions-enable nil
                            lsp-modeline-diagnostics-enable nil
                            lsp-modeline-workspace-status-enable nil))

;; Rust LSP has some windup time, might help to have some feedback
(defun lmdn/lsp-enable-modeline-status-if-rust ()
  (when (derived-mode-p 'rust-mode)
    (setq-local lsp-modeline-workspace-status-enable t)))

(add-hook 'lsp-mode-hook #'lmdn/lsp-enable-modeline-status-if-rust)
