(setq evil-want-C-u-scroll t
      select-enable-clipboard t
      select-enable-primary t
      save-interprogram-paste-before-kill t
      evil-want-Y-yank-to-eol t
      evil-undo-system 'undo-fu
      evil-normal-state-tag   " NORMAL "
      evil-insert-state-tag   " INSERT "
      evil-visual-state-tag   " VISUAL "
      evil-replace-state-tag  " REPLACE "
      evil-operator-state-tag " OPERATOR "
      evil-motion-state-tag   " MOTION "
      evil-emacs-state-tag    " EMACS ")

(require 'evil)
(evil-mode)

(require 'evil-commentary)
(evil-commentary-mode)

(require 'evil-surround)
(global-evil-surround-mode)
