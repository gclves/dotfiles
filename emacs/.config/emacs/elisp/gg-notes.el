(require 'gg-scratch)

(with-eval-after-load 'org
  (global-set-key (kbd "C-x C-l") 'org-store-link))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

(use-package denote
  :hook ((dired-mode . denote-dired-mode))
  :config
  (setq denote-directory (expand-file-name "~/Documents/Notes")
        denote-known-keywords '("emacs" "productivity" "programming" "philosophy")
        denote-prompts '(title keywords)
        denote-file-type 'text))

(provide 'gg-notes)
;;; gg-notes.el ends here
