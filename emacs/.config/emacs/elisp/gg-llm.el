(use-package chatgpt-shell
  :bind (("C-x l" . chatgpt-shell)
         ("C-x e" . chatgpt-shell-prompt-compose))
  :custom
  ((chatgpt-shell-anthropic-key
    (lambda ()
      (auth-source-pass-get 'secret "anthropic-key")))))


(provide 'gg-llm)
