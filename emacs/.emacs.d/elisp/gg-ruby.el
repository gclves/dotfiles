(setq exec-path (cons "~/.rbenv/shims" exec-path))

(defvar gg--rbenv-path (concat (getenv "HOME") "/.rbenv")
  "Path to the rbenv installation.")

(use-package rspec-mode
  :config
  (define-key rspec-mode-map (kbd "<f9>") 'rspec-verify-matching)
  (define-key rspec-mode-map (kbd "s-t") 'rspec-toggle-spec-and-target)
  (define-key rspec-mode-keymap (kbd "s-t") 'rspec-toggle-spec-and-target))

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
  (setq rbenv-installation-dir gg--rbenv-path)
  :config
  (setq rbenv-executable "/usr/local/bin/rbenv"
        rbenv-binary-paths (list (cons 'shims-path (concat rbenv-installation-dir "/shims"))
                                 (cons 'bin-path "/usr/local/bin")))
  (global-rbenv-mode))

;; TODO sort out rubocop integration

(provide 'gg-ruby)
