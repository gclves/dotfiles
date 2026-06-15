(require 'use-package)

(use-package emmet-mode
  :hook ((web-mode sgml-mode css-mode) . emmet-mode)
  :config
  (setq emmet-self-closing-tag-style ""
        emmet-indentation 2)
  (define-key emmet-mode-keymap (kbd "<C-return>") nil))

(with-eval-after-load 'eglot
  (defun gg/eglot-ts-add-missing-imports ()
    "Ask the TypeScript language server to add missing imports."
    (interactive)
    (eglot-code-actions nil nil "source.addMissingImports.ts" t))

  (defun gg/eglot-ts-fix-imports ()
    "Add missing TypeScript imports, then organize imports."
    (interactive)
    (when (eglot-managed-p)
      (ignore-errors
        (gg/eglot-ts-add-missing-imports))
      (ignore-errors
        (eglot-code-action-organize-imports))))

  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-ts-mode js-mode)
                 . ("typescript-language-server" "--stdio"))))

(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))

(provide 'gg-web-dev)
