(require 'gg-scratch)

(with-eval-after-load 'org
  (global-set-key (kbd "C-x C-l") 'org-store-link)
  (setq org-log-done 'time
        org-todo-keywords '((sequence "TODO(t)" "IN_PROGRESS(p)" "|" "DONE(d)"))))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(provide 'gg-notes)
;;; gg-notes.el ends here
