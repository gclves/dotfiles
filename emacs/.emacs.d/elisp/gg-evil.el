(use-package evil)

(use-package evil
  :config
  (add-hook 'text-mode-hook 'turn-on-evil-mode)
  (add-hook 'prog-mode-hook 'turn-on-evil-mode)

  (evil-define-key 'motion 'global (kbd "DEL") "%")

  (evil-set-leader 'normal (kbd "SPC"))

  (evil-global-set-key 'normal (kbd "<leader>s") 'save-buffer)

  (evil-global-set-key 'normal (kbd "<leader>gb") 'magit-blame-addition))

(require 'key-chord)
(key-chord-mode 1)
(key-chord-define-global "jk" 'evil-normal-state)

(use-package evil-matchit
  :config
  (add-hook 'evil-mode-hook 'turn-on-evil-matchit-mode))

(provide 'gg-evil)
