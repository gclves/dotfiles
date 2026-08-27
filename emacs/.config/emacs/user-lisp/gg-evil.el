(use-package evil
  :config
  (add-hook 'evil-local-mode-hook 'turn-on-undo-tree-mode)
  (setq-default evil-undo-system 'undo-redo)

  (add-hook 'prog-mode-hook 'turn-on-evil-mode)
  (add-hook 'dotenv-mode-hook 'turn-on-evil-mode)

  (evil-define-key 'motion 'global (kbd "DEL") "%")

  (evil-set-leader 'normal (kbd "SPC"))

  (evil-global-set-key 'normal (kbd "<leader>s") 'save-buffer)
  (evil-global-set-key 'normal (kbd "<leader>gb") 'magit-blame-addition)

  (add-hook 'lisp-interaction-mode-hook (lambda () (interactive) (evil-mode -1)))
  (add-hook 'lisp-mode-hook (lambda () (interactive) (evil-mode -1))))

(use-package evil-matchit
  :config
  (add-hook 'evil-mode-hook 'turn-on-evil-matchit-mode))

(use-package evil-surround
  :ensure t
  :config
  (global-evil-surround-mode 1))

(use-package evil-escape
  :ensure t
  :after evil
  :init
  (setq-default evil-escape-key-sequence "jk"
                evil-escape-delay 0.15)
  :config
  (evil-escape-mode 1))

(provide 'gg-evil)
