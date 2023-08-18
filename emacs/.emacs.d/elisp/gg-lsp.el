(with-eval-after-load 'eglot
  (define-key eglot-mode-map (kbd "C-c r") 'eglot-rename)
  (define-key eglot-mode-map (kbd "C-c n") 'eglot-format-buffer))

;; (use-package lsp-mode
;;   :bind
;;   (("C-c r" . lsp-rename)
;;    ("C-c n" . lsp-format-buffer))
;;   ;; You could also swap out lsp for lsp-deffered in order to defer loading
;;   :hook  (scala-mode . lsp)
;;   (lsp-mode . lsp-lens-mode)
;;   (ruby-mode . lsp)
;;   (go-mode . lsp)
;;   :config
;;   ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
;;   (setq gc-cons-threshold 100000000           ;; 100mb
;;         read-process-output-max (* 1024 1024) ;; 1mb
;;         lsp-idle-delay 0.500
;;         lsp-log-io nil
;;         lsp-completion-provider :capf
;;         lsp-prefer-flymake nil)

;;   :custom
;;   (lsp-headerline-breadcrumb-enable nil))

;; Enable nice rendering of documentation on hover
;;   Warning: on some systems this package can reduce your emacs responsiveness significally.
;;   (See: https://emacs-lsp.github.io/lsp-mode/page/performance/)
;;   In that case you have to not only disable this but also remove from the packages since
;;   lsp-mode can activate it automatically.
;; (use-package lsp-ui
;;   :bind (:map lsp-mode
;;               ("M-." . lsp-ui-peek-find-definitions))

;;   :hook (lsp-mode . lsp-ui-mode))

;; Posframe is a pop-up tool that must be manually installed for dap-mode
;; (use-package posframe :defer t)

;; Use the Debug Adapter Protocol for running tests and debugging
;; (use-package dap-mode
;;   :hook
;;   (lsp-mode . dap-mode)
;;   (lsp-mode . dap-ui-mode))

(provide 'gg-lsp)
