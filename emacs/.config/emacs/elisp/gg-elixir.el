(use-package elixir-ts-mode
  :ensure-system-package elixir-ls
  :hook (elixir-ts-mode . eglot-ensure))

(use-package exunit
  :hook (elixir-ts-mode . exunit-mode))

(provide 'gg-elixir)
