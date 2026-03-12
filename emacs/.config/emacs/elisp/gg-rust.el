(defun gg-rust-ts-mode-setup ()
  "Enable format-on-save for tree-sitter Rust buffers."
  (add-hook 'before-save-hook #'gg-rust-format-buffer nil t))

(defun gg-rust-format-buffer ()
  "Format the current Rust buffer when managed by Eglot."
  (when (and (fboundp 'eglot-managed-p)
             (eglot-managed-p))
    (eglot-format-buffer)))

(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t
        rust-format-on-save t)

  (add-hook 'rust-ts-mode-hook #'gg-rust-ts-mode-setup))

(provide 'gg-rust)
