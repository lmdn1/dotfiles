(require 'treemacs)
(require 'treemacs-evil)
(require 'treemacs-magit)
(require 'treemacs-projectile)

(treemacs-git-mode 'simple)

(setq treemacs-width 30
      treemacs-follow-mode t
      treemacs-filewatch-mode t
      treemacs-fringe-indicator-mode 'always
      treemacs-collapse-dirs 3
      treemacs-default-visit-action 'treemacs-visit-node-close-treemacs)

(defun lmdn/treemacs-toggle ()
  "Open treemacs in the project root directory"
  (interactive)
  (if (treemacs-is-treemacs-window-selected?)
    (treemacs-quit)
    (let ((default-directory lmdn/project-root-directory))
      (treemacs-add-and-display-current-project-exclusively)
      ;; Ensure we actually switch to the treemacs window
      (when-let ((treemacs-window (treemacs-get-local-window)))
                (select-window treemacs-window)))))

(defun lmdn/treemacs-open-here ()
  "Set treemacs root dir and then open it"
  (interactive)
  (lmdn/treemacs-set-project-root-here)
  (lmdn/treemacs-toggle))

(defun lmdn/treemacs-set-project-root (&optional dir)
  "Set a new project root directory for treemacs"
  (interactive "DNew project root: ")
  (setq lmdn/project-root-directory (or dir default-directory))
  (message "Project root set to: %s" lmdn/project-root-directory))

(defun lmdn/treemacs-set-project-root-here ()
  "Set the project root to the current buffer's directory"
  (interactive)
  (let ((current-dir (or (when buffer-file-name
                           (file-name-directory buffer-file-name))
                         default-directory)))
    (setq lmdn/project-root-directory current-dir)
    (message "Project root set to: %s" lmdn/project-root-directory)))

(defun lmdn/treemacs-import-as-project ()
  "Add the currently selected dir as a projectile project"
  (interactive)
  (if (treemacs-current-button)
    (let* ((node (treemacs-button-get (treemacs-current-button) :path))
           (project-dir (if (file-directory-p node)
                          node
                          (file-name-directory node))))
      (when project-dir
        ;; Create a .projectile file to mark it as a project
        (let ((projectile-file (expand-file-name ".projectile" project-dir)))
          (unless (file-exists-p projectile-file)
            (write-region "" nil projectile-file)))
        (projectile-add-known-project project-dir)
        (setq lmdn/project-root-directory project-dir)
        (projectile-switch-project-by-name project-dir)
        (when (treemacs-is-treemacs-window-selected?)
          (treemacs-quit))
        (lmdn/treemacs-toggle)
        (message "Imported project: %s" project-dir)))
    (message "No directory selected")))
