(defvar gg/todo-file (expand-file-name "~/TODO.org")
  "Location of my TODO file.")

(defun gg/todo ()
  "Open my personal TODO file."
  (interactive)
  (find-file gg/todo-file))
(global-set-key (kbd "<f4>") 'gg/todo)

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/src")
        projectile-shell-backend 'ghostel
        projectile-switch-project-action #'projectile-dired
        projectile-find-dir-includes-top-level t
        projectile-max-file-buffer-count 10
        projectile-search-backend 'ripgrep
        projectile-indexing-method 'hybrid
        projectile-enable-caching t)

  (add-to-list 'projectile-globally-unignored-files ".env")

  :bind ((:map projectile-mode-map
               ("C-x p" . projectile-command-map)
               ("s-p" . projectile-find-file)
               ("s-t" . projectile-test-project)
               ("s-\\" . projectile-run-task)
               ("<f2>" . projectile-run-ghostel))))

(use-package envrc
  :bind (("C-c e" . envrc-command-map)))

(provide 'gg-project)
