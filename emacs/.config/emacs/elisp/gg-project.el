(defvar gg-todo-file (expand-file-name "~/TODO.org")
  "Location of my TODO file.")

(defun gg-todo ()
  "Open my personal TODO file."
  (interactive)
  (find-file gg-todo-file))
(global-set-key (kbd "<f4>") 'gg-todo)

(use-package rg
  :ensure-system-package rg)

(use-package projectile
  :ensure t
  :init (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/src"))
  :bind ((:map projectile-mode-map
               ("C-x p" . projectile-command-map)
               ("s-p" . projectile-find-file)
               ("s-t" . projectile-test-project))))

(use-package envrc
  :bind (("C-c e" . envrc-command-map)))

(provide 'gg-project)
