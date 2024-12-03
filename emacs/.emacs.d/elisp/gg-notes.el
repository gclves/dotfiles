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

(provide 'gg-notes)
;;; gg-notes.el ends here
