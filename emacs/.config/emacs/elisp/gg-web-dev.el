(require 'use-package)

(use-package emmet-mode
  :hook ((web-mode sgml-mode css-mode) . emmet-mode)
  :config
  (setq emmet-self-closing-tag-style ""
        emmet-indentation 2)
  (define-key emmet-mode-keymap (kbd "<C-return>") nil))


(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(provide 'gg-web-dev)
