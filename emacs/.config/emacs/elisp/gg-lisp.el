(use-package paredit
  :hook ((emacs-lisp-mode
          ielm-mode
          lisp-mode
          lisp-interaction-mode
          scheme-mode) . enable-paredit-mode)
  :config
  (define-key paredit-mode-map (kbd "C-j") nil)
  (define-key paredit-mode-map (kbd "RET") 'paredit-newline)
  (define-key lisp-interaction-mode-map (kbd "C-j") 'eval-print-last-sexp))

(provide 'gg-lisp)
