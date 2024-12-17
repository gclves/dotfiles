(require 'gg-scratch)

(use-package deft
  :bind
  (([f5] . deft)
   ("C-x C-g" . deft-find-file))
  :config
  (setq deft-extensions '("md" "org" "txt")
        deft-recursive t
        deft-default-extension "md"
        deft-directory "~/sync/Notes"
        deft-new-file-format "%Y%m%d%H%M"))

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
