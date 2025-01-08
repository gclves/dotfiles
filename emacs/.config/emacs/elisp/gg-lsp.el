(require 'eglot)

(defvar gg--lsp-enabled-hooks
  '(python-mode-hook ruby-mode-hook go-mode-hook rust-mode-hook)
  "List of hooks associated with eglot/LSP.")

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c n") 'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-.") 'eglot-code-actions)

  (dolist (hook gg--lsp-enabled-hooks)
    (add-hook hook 'eglot-ensure)))


(provide 'gg-lsp)
