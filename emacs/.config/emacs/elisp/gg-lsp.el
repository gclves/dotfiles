(require 'eglot)

(defvar gg--lsp-enabled-hooks
  '(python-mode-hook ruby-mode-hook go-mode-hook rust-mode-hook
    typescript-ts-mode-hook tsx-ts-mode-hook js-ts-mode-hook js-mode-hook)
  "List of hooks associated with eglot/LSP.")

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c n") 'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-.") 'eglot-code-actions)

  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-ts-mode js-mode)
                 . ("typescript-language-server" "--stdio")))

  (dolist (hook gg--lsp-enabled-hooks)
    (add-hook hook 'eglot-ensure)))


(provide 'gg-lsp)
