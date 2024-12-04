(use-package go-mode
  :mode "\\.go\\'"
  :hook ((before-save . gofmt-before-save))
  :bind (:map go-mode-map
              ("C-\\" . go-test-current-project))
  :ensure-system-package (goimports . "go install golang.org/x/tools/cmd/goimports@latest")
  :init (add-to-list 'exec-path "~/go/bin")
  :config
  (setq gofmt-command "goimports"))

(provide 'gg-go)
