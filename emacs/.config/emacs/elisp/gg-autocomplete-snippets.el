(use-package corfu
  :hook (prog-mode . corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete
        corfu-preview-current nil
        corfu-min-width 20

        corfu-popupinfo-delay '(1.25 . 0.5))

  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-n") 'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-p") 'flymake-goto-prev-error)
  (add-hook 'prog-mode-hook 'flymake-mode))



(provide 'gg-autocomplete-snippets)
