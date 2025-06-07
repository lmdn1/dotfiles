(cond
  ((executable-find "win32yank.exe")
   (setq interprogram-cut-function
         (lambda (text &optional push)
           (let ((process-connection-type nil))
             (let ((proc (start-process "win32yank" "*Messages*" "win32yank.exe" "-i" "--crlf")))
               (process-send-string proc text)
               (process-send-eof proc)))))

   (setq interprogram-paste-function
         (lambda ()
           (let ((text (shell-command-to-string "win32yank.exe -o --lf")))
             (if (string= text "") nil text)))))

  ((executable-find "clip.exe")
   (setq interprogram-cut-function
         (lambda (text &optional push)
           (let ((process-connection-type nil))
             (let ((proc (start-process "clip" "*Messages*" "clip.exe")))
               (process-send-string proc text)
               (process-send-eof proc)))))

   (setq interprogram-paste-function
         (lambda ()
           (let ((text (shell-command-to-string "powershell.exe -command 'Get-Clipboard' 2>/dev/null")))
             (if (string= text "") nil (substring text 0 -1)))))))
