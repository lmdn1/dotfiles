(setq vterm-max-scrollback 10000
      vterm-buffer-name-string "vterm %s")

(defun lmdn/vterm-project ()
  "Open vterm in project root directory"
  (interactive)
  (let ((default-directory (if (boundp 'lmdn/project-root-directory)
                             lmdn/project-root-directory
                             default-directory)))
    (vterm-other-window)))

(defun lmdn/vterm-toggle ()
  "Toggle vterm window"
  (interactive)
  (let ((vterm-buffer (get-buffer "*vterm*")))
    (if (and vterm-buffer (get-buffer-window vterm-buffer))
      (delete-windows-on vterm-buffer)
      (if vterm-buffer
        (pop-to-buffer vterm-buffer)
        (lmdn/vterm-project)))))

(require 'vterm)

(add-hook 'vterm-mode-hook (lambda () (display-line-numbers-mode 0)))
