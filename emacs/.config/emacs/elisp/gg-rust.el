(with-eval-after-load 'rust-mode
  (add-to-list 'auto-mode-alist
               '("\\.rs\\'" . rust-ts-mode))

  (add-hook 'rust-ts-mode-hook 'eglot-ensure))


(provide 'gg-rust)
