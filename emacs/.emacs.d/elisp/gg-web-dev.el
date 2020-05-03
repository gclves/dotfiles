(require 'use-package)

(use-package js2-mode
  :mode "\\.jsx?\\'"
  :bind
  (:map js2-mode-map
        ("C-c C-c" . js-send-region)
        ("M-." . js2-jump-to-definition)
        ("M-," . pop-tag-mark))
  :config
  (setq js2-basic-offset 2
        js2-strict-trailing-comma-warning nil)
  (add-hook 'js2-mode-hook 'js2-imenu-extras-mode)
  (define-key js2-mode-map (kbd "M-j") nil))

(use-package js2-refactor
  :bind
  (:map js2-refactor-mode-map
        ("C-k" . js2r-kill)
        ("<M-S-up>" . js2r-move-line-up)
        ("<M-S-down>" . js2r-move-line-down)
        ("s-r" . js2r-rename-var))
  :init
  (defun setup-js2r-mode ()
    (js2-refactor-mode +1)
    (js2r-add-keybindings-with-prefix "C-c C-r"))
  (add-hook 'js2-mode-hook 'setup-js2r-mode))

(use-package typescript-mode
  :mode "\\.js\\'"
  :config
  (setq typescript-indent-level 2))

(use-package prettier-js
  :config
  (progn
    (add-hook 'tide-mode-hook 'prettier-js-mode)))

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

(use-package mocha)

(defun gg-node-repl ()
  "Start a NodeJS REPL in comint mode."
  (interactive)
  (setenv "NODE_NO_READLINE" "1")       ; avoid fancy terminal codes
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

(provide 'gg-web-dev)
