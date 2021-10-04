(setq exec-path (cons "~/.rbenv/shims" exec-path))

(use-package dumb-jump
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(use-package inf-ruby)
(use-package ruby-test-mode)
(use-package ruby-electric
  :config
  (add-hook 'ruby-mode-hook 'ruby-electric-mode))

(use-package rbenv
  :init
  (setq rbenv-installation-dir "/usr/local")
  :config
  (global-rbenv-mode))

;; TODO sort out rubocop integration

(provide 'gg-ruby)
