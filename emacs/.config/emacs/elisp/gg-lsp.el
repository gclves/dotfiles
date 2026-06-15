(require 'eglot)

(defvar gg--lsp-enabled-hooks
  '(python-mode-hook ruby-mode-hook go-mode-hook rust-mode-hook rust-ts-mode-hook
                     typescript-ts-mode-hook tsx-ts-mode-hook js-ts-mode-hook js-mode-hook)
  "List of hooks associated with eglot/LSP.")

(defun gg--eglot-completion-only ()
  "Use only Eglot completion in LSP-managed buffers.
This prevents Cape fallback sources such as dabbrev, keyword, and file
completion from being mixed into LSP completion results."
  (setq-local completion-at-point-functions
              (list #'eglot-completion-at-point)))

(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c f") 'eglot-format-buffer)
  (define-key eglot-mode-map (kbd "C-.") 'eglot-code-actions)

  ;; In LSP buffers, do not mix Eglot completion with Cape fallback completion.
  (add-hook 'eglot-managed-mode-hook #'gg--eglot-completion-only)

  (dolist (hook gg--lsp-enabled-hooks)
    (add-hook hook 'eglot-ensure)))


(provide 'gg-lsp)
