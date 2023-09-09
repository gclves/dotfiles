(require 'gg-lsp)
(require 'gg-autocomplete-snippets)
(require 'gg-web-dev)
(require 'gg-ruby)
(require 'gg-scala)

;; Golang
(use-package go-mode
  :mode "\\.go\\'"
  :hook ((go-mode . eglot-ensure)
         (before-save . gofmt-before-save))
  :bind (:map go-mode-map
              ("C-\\" . go-test-current-project))
  :config
  ;; Make sure you have "goimports installed!"
  (setq gofmt-command "goimports"))

(use-package go-playground)

;; Racket
(use-package racket-mode
  :mode ("\\.pie\\'" "\\.rkt\\'")
  :hook (racket-mode . paredit-mode)
  :config
  ;; Configuration for Pie (for The Little Typer)
  (put 'claim 'racket-indent-function 2)
  (put '-> 'racket-indent-function 2)
  (put 'Pi 'racket-indent-function 2))

;; Lisp
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

;; Terraform
(use-package terraform-mode
  :mode "\\.tf\\'")
(use-package terraform-doc
  :mode "\\.tf\\'")

(provide 'gg-prog)
;;; gg-prog.el ends here
