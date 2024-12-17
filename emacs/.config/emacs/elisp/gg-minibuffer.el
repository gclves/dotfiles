(setq use-short-answers t
      sentence-end-double-space nil)

(use-package vertico
  :hook (after-init . vertico-mode))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(with-eval-after-load 'savehist
  (add-hook 'after-init-hook 'savehist-mode))


(provide 'gg-minibuffer)
