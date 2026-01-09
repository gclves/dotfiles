(use-package rust-mode
  :init
  (setq rust-mode-treesitter-derive t
        rust-format-on-save t)

  (add-hook 'rust-mode-hook 'eglot-ensure))

(provide 'gg-rust)
