;; GUI Emacs doesn't seem to let us do normal people zoom

(defvar lmdn/current-zoom-level 0 "Zoom level.")

(defun lmdn/apply-zoom ()
  "Apply the current zoom level."
  (let ((new-height (+ lmdn/default-font-height (* lmdn/current-zoom-level 10))))
    (set-face-attribute 'default nil :height new-height)
    (message "Zoom level: %d" lmdn/current-zoom-level)))

(defun lmdn/zoom-in ()
  "Zoom in."
  (interactive)
  (setq lmdn/current-zoom-level (1+ lmdn/current-zoom-level))
  (lmdn/apply-zoom))

(defun lmdn/zoom-out ()
  "Zoom out."
  (interactive)
  (setq lmdn/current-zoom-level (1- lmdn/current-zoom-level))
  (lmdn/apply-zoom))

(defun lmdn/zoom-reset ()
  "Reset zoom."
  (interactive)
  (setq lmdn/current-zoom-level 0)
  (lmdn/apply-zoom)
  (message "Zoom reset"))
