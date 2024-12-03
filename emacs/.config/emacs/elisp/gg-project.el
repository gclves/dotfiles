(defvar gg-todo-file (expand-file-name "~/TODO")
  "Location of my TODO file.")

(defun gg-todo ()
  "Open my personal TODO file."
  (interactive)
  (find-file gg-todo-file))
(global-set-key (kbd "<f4>") 'gg-todo)

(use-package rg)

(use-package projectile
  :ensure t
  :init
  (projectile-mode +1)
  :config
  (setq projectile-project-search-path '("~/src"))
  :bind ((:map projectile-mode-map
               ("s-p" . projectile-command-map)
               ("C-x p" . projectile-command-map))
         (:map global-map
               ("s-t" . projectile-find-file))))

(with-eval-after-load 'project
  (global-set-key (kbd "s-t") 'project-find-file))

(provide 'gg-project)
