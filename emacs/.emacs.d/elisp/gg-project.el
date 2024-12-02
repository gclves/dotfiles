(defvar gg-todo-file (expand-file-name "~/TODO")
  "Location of my TODO file.")

(defun gg-todo ()
  "Open my personal TODO file."
  (interactive)
  (find-file gg-todo-file))
(global-set-key (kbd "<f4>") 'gg-todo)

(with-eval-after-load 'project
  (global-set-key (kbd "s-t") 'project-find-file))

(provide 'gg-project)
