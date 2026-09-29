(require 'gg-scratch)

(with-eval-after-load 'org
  (global-set-key (kbd "C-x C-l") 'org-store-link)
  (setq org-log-done 'time
        org-todo-keywords '((sequence "TODO(t)" "IN_PROGRESS(p)" "|" "DONE(d)"))))

(use-package markdown-ts-mode
  :mode (("\\.md\\'" . markdown-ts-mode)
         ("\\.markdown\\'" . markdown-ts-mode)))

(provide 'gg-notes)
;;; gg-notes.el ends here
