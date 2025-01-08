(require 'gg-lsp)
(require 'gg-autocomplete-snippets)
(require 'gg-web-dev)
(require 'gg-ruby)
(require 'gg-scala)
(require 'gg-rust)
(require 'gg-python)
(require 'gg-go)
(require 'gg-lisp)
(require 'gg-elixir)


(setq-default tab-width 4)
(global-set-key (kbd "C-;") 'comment-line)
(electric-pair-mode)
(add-hook 'prog-mode-hook 'subword-mode)

(use-package treesit-auto
  :config
  (global-treesit-auto-mode)
  (setq treesit-auto-install t))

(add-to-list 'auto-mode-alist '("\\.exs?\\'" . elixir-ts-mode))

(provide 'gg-prog)
;;; gg-prog.el ends here
