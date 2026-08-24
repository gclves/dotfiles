(defun gg/go-mode-setup ()
  "Enable format-on-save only for the current Go buffer."
  (add-hook 'before-save-hook #'gofmt-before-save nil t))

(use-package go-mode
  :mode "\\.go\\'"
  :hook (go-mode . gg/go-mode-setup)
  :bind (:map go-mode-map
              ("C-\\" . go-test-current-project))
  :ensure-system-package (goimports . "go install golang.org/x/tools/cmd/goimports@latest")
  :init (add-to-list 'exec-path "~/go/bin")
  :config
  (setq gofmt-command "goimports"))

(provide 'gg-go)
