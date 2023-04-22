(use-package company
  :hook (scala-mode . company-mode)
  (ruby-mode . company-mode)

  :config
  (global-company-mode)
  (setq company-tooltip-align-annotations t
        company-show-numbers t
        company-idle-delay .1
        company-tooltip-idle-delay .1)
  (add-to-list 'completion-styles 'initials t))

(use-package company-quickhelp
  :config
  (add-hook 'company-mode-hook 'company-quickhelp-mode))

(use-package yasnippet
  :config
  (yas-global-mode)
  (add-hook 'prog-mode-hook 'yas-minor-mode))

(use-package flycheck
  :init (global-flycheck-mode))

(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

(add-hook 'prog-mode-hook 'flymake-mode)

(provide 'gg-autocomplete-snippets)
