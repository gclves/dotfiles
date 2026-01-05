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
  (setq treesit-auto-install 'prompt)
  (treesit-auto-add-to-auto-mode-alist 'all))

(provide 'gg-prog)
;;; gg-prog.el ends here
