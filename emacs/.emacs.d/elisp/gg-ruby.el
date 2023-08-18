(setq exec-path (cons "~/.rbenv/shims" exec-path))

(defvar gg--rbenv-path (concat (getenv "HOME") "/.rbenv")
  "Path to the rbenv installation.")

(use-package rspec-mode
  :mode "\\.rb\\'"
  :config
  (global-set-key (kbd "<f6>") 'rspec-rerun)
  (define-key rspec-verifiable-mode-map (kbd "s-t") 'rspec-verify)
  (define-key rspec-verifiable-mode-map (kbd "s-T") 'rspec-toggle-spec-and-target)
  (define-key rspec-mode-map (kbd "s-t") 'rspec-verify)
  (define-key rspec-mode-map (kbd "s-T") 'rspec-toggle-spec-and-target))

(use-package inf-ruby
  :hook (ruby-mode . inf-ruby-minor-mode)
  :config
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

(use-package ruby-test-mode
  :hook ruby-mode)

(use-package ruby-electric
  :hook ruby-mode)

(use-package rbenv
  :init
  (setq rbenv-installation-dir gg--rbenv-path)
  :config
  (setq rbenv-executable "/usr/local/bin/rbenv"
        rbenv-binary-paths (list (cons 'shims-path (concat rbenv-installation-dir "/shims"))
                                 (cons 'bin-path "/usr/local/bin")))
  (global-rbenv-mode))

;; TODO sort out rubocop integration

(add-to-list 'auto-mode-alist '("\\.rb\\'" . ruby-mode))

(provide 'gg-ruby)
