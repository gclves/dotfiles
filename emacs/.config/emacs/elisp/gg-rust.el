(use-package rustic
  :mode ("\\.rs\\'" . rustic-mode)
  :config
  (define-key rustic-mode-map (kbd "C-S-t") 'rustic-cargo-test))

(provide 'gg-rust)
