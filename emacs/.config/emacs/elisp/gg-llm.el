(use-package gptel
  :config
  (setq gptel-model 'claude-4.5-sonnet
        gptel-backend (gptel-make-gh-copilot "Copilot")
        gptel-default-mode 'org-mode))


(provide 'gg-llm)
