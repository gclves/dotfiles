(require 'use-package)

(use-package emmet-mode
  :hook ((web-mode sgml-mode css-mode) . emmet-mode)
  :config
  (setq emmet-self-closing-tag-style ""
        emmet-indentation 2)
  (define-key emmet-mode-keymap (kbd "<C-return>") nil))

(provide 'gg-web-dev)
