(use-package ghostel
  :bind (("C-x m" . ghostel)
         :map ghostel-semi-char-mode-map
         ("C-s"  . consult-line)
         ("M-<backspace>" . (lambda () (interactive) (ghostel-send-key "w" "ctrl")))
         ;; ;; I'm used to go up/down the shell history with M-n/p from eshell
         ;; ;; Simulate this behavior in ghostel by sending C-p and C-n
         ("M-p" . (lambda () (interactive) (ghostel-send-key "p" "ctrl")))
         ("M-n" . (lambda () (interactive) (ghostel-send-key "n" "ctrl")))
         :map project-prefix-map
         ("m" . ghostel-project)
         ("M" . ghostel-project-list-buffers))
  :config
  (defun ghostel-send-C-k-and-kill ()
    "Send `C-k' to ghostel.
Like normal Emacs `C-k'.  Kill to end of line and put content in kill-ring."
    (interactive)
    (kill-ring-save (point) (line-end-position))
    (ghostel-send-key "k" "ctrl"))

  (add-to-list 'project-switch-commands '(ghostel-project "Ghostel") t)
  (add-to-list 'project-switch-commands '(ghostel-project-list-buffers "Ghostel buffers") t)
  (add-to-list 'ghostel-eval-cmds '("magit-status-setup-buffer" magit-status-setup-buffer))

  (require 'ghostel-eshell)
  (require 'ghostel-compile)
  (require 'ghsotel-comint))


(with-eval-after-load 'ghostel-eshell
  (add-hook 'eshell-load-hook 'ghostel-eshell-visual-command-mode))

(with-eval-after-load 'ghostel-compile
  (add-hook 'after-init-hook 'ghostel-compile-global-mode))

(with-eval-after-load 'ghostel-comint
  (add-hook 'after-init-hook 'ghostel-comint-global-mode))

(provide 'gg-terminal)
