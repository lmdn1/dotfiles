(defun osc52-copy (text)
  "Copy TEXT to system clipboard using OSC 52 escape sequence."
  (let ((encoded (base64-encode-string (encode-coding-string text 'utf-8) t)))
    (send-string-to-terminal (concat "\e]52;c;" encoded "\a"))))

(defun osc52-interprogram-cut (text &optional _push)
  "Cut function for `interprogram-cut-function' using OSC 52."
  (osc52-copy text))

(setq interprogram-cut-function #'osc52-interprogram-cut)
;; (setq interprogram-paste-function nil)
