(setq completions-detailed t
      describe-bindings-outline t)

(global-goto-address-mode)

(use-package which-key :ensure t
  :config (which-key-mode))

(use-package helpful
  :bind
  (("C-h f" . helpful-callable)
   ("C-h v" . helpful-variable)
   ("C-h k" . helpful-key)
   ("C-x C-d" . helpful-at-point)))

(provide 'gg-help)
