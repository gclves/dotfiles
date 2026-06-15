(use-package corfu
  :hook (prog-mode . corfu-mode)
  :bind (:map corfu-map
              ("TAB" . corfu-complete)
              ("<tab>" . corfu-complete))
  :custom
  (tab-always-indent 'complete)
  (corfu-auto t)
  (corfu-auto-delay 0.15)
  (corfu-auto-prefix 2)
  (corfu-preview-current nil)
  (corfu-min-width 20)
  (corfu-popupinfo-delay '(1.25 . 0.5))
  :config
  (corfu-popupinfo-mode 1)
  (with-eval-after-load 'savehist
    (corfu-history-mode 1)
    (add-to-list 'savehist-additional-variables 'corfu-history)))

(use-package cape
  :init
  ;; Global fallback completion only. Not using eglot
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-keyword))

(use-package yasnippet
  :hook (prog-mode . yas-minor-mode))

(use-package eldoc
  :config
  (setq eldoc-echo-area-use-multiline-p t))

(global-set-key (kbd "C-c d") 'eldoc-doc-buffer)

(with-eval-after-load 'flymake
  (define-key flymake-mode-map (kbd "M-]") #'flymake-goto-next-error)
  (define-key flymake-mode-map (kbd "M-[") #'flymake-goto-prev-error)

  (define-key flymake-mode-map (kbd "C-c C-e") 'flymake-show-buffer-diagnostics)
  (define-key flymake-mode-map (kbd "C-c C-E") 'flymake-show-project-diagnostics)
  (add-hook 'prog-mode-hook 'flymake-mode)

  (with-eval-after-load 'consult
    (define-key flymake-mode-map (kbd "C-x `") 'consult-flymake)))

(provide 'gg-autocomplete-snippets)
