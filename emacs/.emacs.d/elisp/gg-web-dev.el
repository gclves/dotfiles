(require 'use-package)

(use-package typescript-mode
  :mode "\\.js\\'"
  :config
  (setq typescript-indent-level 2))

(use-package tide
  :bind
  (:map tide-mode-map
        ("C-c r" . tide-rename-symbol)
        ("C-c C-r" . tide-rename-file)
        ("M-?" . tide-references))
  :config
  (progn
    (setq tide-completion-detailed t
          tide-completion-enable-autoimport-suggestions t
          tide-always-show-documentation t)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))
  :init
  (progn
    (defun setup-tide-mode ()
      (tide-setup)
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      (setq-local company-backend 'tide-company))

    (add-hook 'typescript-mode-hook 'setup-tide-mode)
    (add-hook 'js2-mode-hook 'setup-tide-mode)))

(defun gg-node-repl ()
  "Start a NodeJS REPL in comint mode."
  (interactive)
  (setenv "NODE_NO_READLINE" "1")       ; avoid fancy terminal codes
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

(with-eval-after-load 'js
  (setq js-indent-level 2))

(provide 'gg-web-dev)
