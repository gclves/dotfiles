(define-derived-mode dotenv-mode conf-mode "Dotenv"
  "Major mode for .env files.")

(add-to-list 'auto-mode-alist
             '("\\.env\\(?:\\..*\\)?\\'" . dotenv-mode))


(provide 'gg-dotenv)
