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

;; Stupid posframe thing
(defun lmdn/show-documentation-posframe ()
  "Show documentation for symbol at point in posframe."
  (interactive)
  (when (bound-and-true-p lsp-mode)
    (lsp-request-async
      "textDocument/hover"
      (lsp--text-document-position-params)
      (lambda (hover)
        (when hover
          (let* ((contents (lsp:hover-contents hover))
                 (content-str (cond
                                ;; If it's a string, use it directly
                                ((stringp contents) contents)
                                ;; If it's a MarkupContent object
                                ((lsp-markup-content? contents)
                                 (lsp:markup-content-value contents))
                                ;; If it's a list of marked strings
                                ((and (listp contents) (> (length contents) 0))
                                 (mapconcat (lambda (item)
                                              (if (stringp item)
                                                item
                                                (lsp:marked-string-value item)))
                                            contents "\n"))
                                ;; Fallback
                                (t (format "%s" contents)))))
            (when (and content-str (not (string-empty-p (string-trim content-str))))
              (posframe-show (lmdn/create-simple-lsp-posframe-buffer nil content-str)
                             :position (point)
                             :border-width 1
                             :border-color "#6f42c1"
                             :min-width 20
                             :max-width 80)
              (add-hook 'pre-command-hook #'lmdn/hide-posframe-once nil t))))
        :mode 'tick))))

(defun lmdn/show-signature-posframe (signature)
  "Display SIGNATURE (a string, MarkupContent or list of marked strings)
  in our corner posframe, re‑using `lmdn/create-simple-lsp-posframe-buffer`."
  (when signature
    (let* ((raw
             (cond
               ((stringp signature) signature)
               ((lsp-markup-content? signature)
                (lsp:markup-content-value signature))
               ((and (listp signature) (cl-plusp (length signature)))
                (mapconcat
                  (lambda (item)
                    (if (stringp item)
                      item
                      (lsp:marked-string-value item)))
                  signature "\n"))
               (t (format "%s" signature))))
           ;; drop any leading/trailing blank lines
           (doc (string-trim raw)))
      (when (and doc (not (string-empty-p doc)))
        (posframe-show
          (lmdn/create-simple-lsp-posframe-buffer nil doc)
          :poshandler #'lmdn/posframe-top-right-with-gap
          :border-width 1
          :border-color "#6f42c1"
          :min-width    20
          :max-width    160)
        (add-hook 'pre-command-hook #'lmdn/hide-posframe-once nil t)))))  

(defun lmdn/create-simple-lsp-posframe-buffer (_title doc &optional _type-info)
  (with-current-buffer (get-buffer-create " *lmdn-lsp-posframe*")
                       ;; allow edits
                       (let ((inhibit-read-only t))
                         (erase-buffer)
                         (setq-local left-margin-width 1
                                     right-margin-width 1
                                     word-wrap t
                                     truncate-lines nil)
                         (insert doc)
                         ;; remove any completely empty lines
                         (goto-char (point-min))
                         (while (re-search-forward "^[ \t]*\n" nil t)
                                (replace-match ""))
                         ;; enable markdown view if you want link/keymap support
                         (when (fboundp 'gfm-view-mode)
                           (gfm-view-mode))
                         ;; fontify it right now
                         (font-lock-ensure))
                       ;; make it read-only again for safety
                       (read-only-mode 1)
                       (current-buffer)))

(evil-define-key 'normal 'global (kbd "K") 'lmdn/show-documentation-posframe)

(defun lmdn/hide-posframe-once ()
  "Hide the LSP posframe, then remove myself from `pre-command-hook`."
  (posframe-hide " *lmdn-lsp-posframe*")
  (remove-hook 'pre-command-hook #'lmdn/hide-posframe-once t))

(defun lmdn/posframe-top-right-with-gap (info)
  "Position posframe in top-right with custom gap."
  (let* ((window-left (plist-get info :parent-window-left))
         (window-top (plist-get info :parent-window-top))
         (window-width (plist-get info :parent-window-width))
         (window-height (plist-get info :parent-window-height))
         (posframe-width (plist-get info :posframe-width))
         (posframe-height (plist-get info :posframe-height))
         (gap-x 30)  ; Horizontal gap from edge
         (gap-y 20)) ; Vertical gap from edge
    (cons (+ window-left window-width (- posframe-width) (- gap-x))
          (+ window-top gap-y))))

;; org stuff
(require 'org)

(setq org-directory "~/.org"
      org-default-notes-file (concat org-directory "/dumping-ground.org")
      org-startup-indented t
      org-hide-leading-stars t
      org-pretty-entities t
      org-log-done 'time)
