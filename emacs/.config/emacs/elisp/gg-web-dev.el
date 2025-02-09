(require 'use-package)

(use-package emmet-mode
  :hook ((web-mode sgml-mode css-mode) . emmet-mode)
  :config
  (setq emmet-self-closing-tag-style ""
        emmet-indentation 2)
  (define-key emmet-mode-keymap (kbd "<C-return>") nil))


(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(use-package tide
  :init
  (defun setup-tide-mode ()
    (interactive)
    (tide-setup)
    (eldoc-mode +1)
    (tide-hl-identifier-mode +1))

  (add-hook 'before-save-hook 'tide-format-before-save)

  (add-hook 'typescript-ts-mode-hook 'setup-tide-mode)
  (add-hook 'tsx-ts-mode-hook 'setup-tide-mode))

(provide 'gg-web-dev)
