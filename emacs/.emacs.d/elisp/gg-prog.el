(require 'gg-lsp)
(require 'gg-autocomplete-snippets)
(require 'gg-web-dev)
(require 'gg-ruby)
(require 'gg-scala)

;; Golang
(use-package go-mode
  :mode "\\.go\\'"
  :hook ((before-save . gofmt-before-save))
  :bind (:map go-mode-map
              ("C-\\" . go-test-current-project))
  :ensure-system-package (goimports . "go install golang.org/x/tools/cmd/goimports@latest")
  :init (add-to-list 'exec-path "~/go/bin")
  :config
  (setq gofmt-command "goimports"))


(use-package paredit
  :hook ((emacs-lisp-mode
          eval-expression-minibuffer-setup
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode) . enable-paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "C-j") nil)
  (define-key paredit-mode-map (kbd "RET") 'paredit-newline)
  (define-key lisp-interaction-mode-map (kbd "C-j") 'eval-print-last-sexp))

(setq-default tab-width 4)

;; Python
(use-package pyvenv
  :hook (python-mode . pyvenv-mode))

(provide 'gg-prog)
;;; gg-prog.el ends here
