(use-package olivetti
  :ensure t
  :hook text-mode
  :config
  (setq olivetti-style t))

(use-package markdown-ts-mode
  :ensure nil
  :mode ("\\.md\\'" "\\.mdx\\'" "\\.markdown\\'")
  :config
  (require 'markdown-ts-mode-x))

(provide 'gg-prose)
