(require 'gg-lsp)
(require 'gg-autocomplete-snippets)
(require 'gg-web-dev)
(require 'gg-ruby)
(require 'gg-scala)

;; Golang
(use-package go-mode
  :config
  (add-hook 'go-mode-hook 'lsp-deferred)
  (add-hook 'before-save-hook 'gofmt-before-save))

(use-package go-playground)

;; Racket
(use-package racket-mode
  :config
  ;; Configuration for Pie (for The Little Typer)
  (put 'claim 'racket-indent-function 2)
  (put '-> 'racket-indent-function 2)
  (put 'Pi 'racket-indent-function 2)

  (add-hook 'racket-mode-hook 'paredit-mode))

;; Lisp
(use-package paredit
  :config
  (progn
    (define-key paredit-mode-map (kbd "C-j") nil)
    (define-key paredit-mode-map (kbd "RET") 'paredit-newline)
    (define-key lisp-interaction-mode-map (kbd "C-j") 'eval-print-last-sexp)

    (cl-loop for hook in '(emacs-lisp-mode-hook
                           eval-expression-minibuffer-setup-hook
                           ielm-mode-hook lisp-mode-hook
                           lisp-interaction-mode-hook
                           scheme-mode-hook)
             do (add-hook hook #'enable-paredit-mode))))

(setq-default tab-width 4)

;; Terraform
(use-package terraform-mode)
(use-package terraform-doc)

(provide 'gg-prog)
;;; gg-prog.el ends here
