(require 'gg-scratch)

(with-eval-after-load 'org
  (global-set-key (kbd "C-x C-l") 'org-store-link))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(provide 'gg-notes)
;;; gg-notes.el ends here
