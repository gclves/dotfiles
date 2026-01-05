(use-package corfu
  :hook (prog-mode . corfu-mode)
  :bind (:map corfu-map ("<tab>" . corfu-complete))
  :config
  (setq tab-always-indent 'complete
        corfu-auto t
        corfu-auto-delay 0.15
        corfu-auto-prefix 2
        corfu-preview-current nil
        corfu-min-width 20

        corfu-popupinfo-delay '(1.25 . 0.5))

  (corfu-popupinfo-mode 1) ; shows documentation after `corfu-popupinfo-delay'

  ;; Sort by input history (no need to modify `corfu-sort-function').
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package cape
  :after eglot
  :config
  (defun gg--eglot-capf-setup ()
    "Combine LSP completion with keyword and buffer completions."
    (setq-local completion-at-point-functions
                (list (cape-capf-super
                       #'eglot-completion-at-point
                       #'cape-keyword
                       #'cape-dabbrev
                       #'cape-file))))
  (add-hook 'eglot-managed-mode-hook #'gg--eglot-capf-setup))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package eldoc
  :config
  (setq eldoc-echo-area-use-multiline-p t))

(global-set-key (kbd "C-c d") 'eldoc-doc-buffer)

(setq-default abbrev-mode t)
(setq save-abbrevs 'silently)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "C-c C-e") 'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "C-c C-E") 'flymake-show-project-diagnostics)
  (add-hook 'prog-mode-hook 'flymake-mode))

(global-set-key (kbd "M-n") 'next-error)
(global-set-key (kbd "M-p") 'previous-error)


(provide 'gg-autocomplete-snippets)
