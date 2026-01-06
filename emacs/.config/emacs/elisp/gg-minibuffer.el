(with-eval-after-load 'minibuffer
  (setq completion-cycle-threshold 1
        completions-detailed 1
        tab-always-indent 'complete

        completion-auto-help 'always
        completions-format 'one-column
        completions-group t
        completion-auto-select 'second-tab)

  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete))

(setq use-short-answers t)

(use-package vertico
  :hook (after-init . vertico-mode)
  :config
  (setq vertico-cycle t))

(use-package marginalia
  :hook (after-init . marginalia-mode))

(use-package orderless
  :config
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides nil))

(use-package consult
  :bind (
         ("C-S-f" . consult-ripgrep)
         ("s-b" . consult-buffer)))

(with-eval-after-load 'savehist
  (add-hook 'after-init-hook 'savehist-mode))


(provide 'gg-minibuffer)
