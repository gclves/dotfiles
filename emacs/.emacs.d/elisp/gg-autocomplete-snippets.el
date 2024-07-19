(use-package company
  :hook (prog-mode . company-mode)

  :config
  (setq company-tooltip-align-annotations t
        company-show-numbers t
        company-idle-delay .1
        company-tooltip-idle-delay .1)
  (add-to-list 'completion-styles 'initials t))

(use-package company-quickhelp
  :hook (company-mode . company-quickhelp-mode))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package flycheck
  :hook (prog-mode . flycheck-mode))

(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

(add-hook 'prog-mode-hook 'flymake-mode)

(provide 'gg-autocomplete-snippets)
