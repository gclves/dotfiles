(autoload 'aweshell-new "aweshell")
(autoload 'aweshell-next "aweshell")
(autoload 'aweshell-dedicated-toggle "aweshell")

(with-eval-after-load 'esh-opt
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
        eshell-prompt-function 'epe-theme-lambda))

(global-set-key (kbd "<f12>") 'aweshell-dedicated-toggle)
(global-set-key (kbd "<f1>") 'gg-next-aweshell)

(with-eval-after-load 'aweshell
  (setq aweshell-auto-suggestion-p nil)

  (defun gg-next-aweshell (arg)
    "Switch to next aweshell.  If called with `ARG', create a new one."
    (interactive "P")
    (let ((open-new? (and arg t)))
      (if open-new? (aweshell-new) (aweshell-next)))))


(with-eval-after-load 'eshell
  (defun eshell/d (&rest args)
    (dired (pop args) "."))

  (defun eshell/clear ()
    "Really clear the eshell buffer, including scrollback."
    (let ((eshell-buffer-maximum-lines 0)) (eshell-truncate-buffer)))

  (defun eshell/x ()
    (delete-window)
    (eshell/exit)))

(with-eval-after-load 'eshell
  (defun eshell-there (host)
    (interactive "sHost: ")
    (let ((default-directory (format "/%s:" host)))
      (eshell host)))

  (defun eshell-here ()
    "Opens up a new shell in the directory associated with the
  current buffer's file. The eshell is renamed to match that
  directory to make multiple eshell windows easier."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (height (/ (window-total-height) 3))
           (name   (car (last (split-string parent "/" t)))))
      (split-window-vertically (- height))
      (other-window 1)
      (eshell "new")
      (rename-buffer (concat "*eshell: " name "*"))

      (insert "ls")
      (eshell-send-input)))

  (global-set-key (kbd "C-!") 'eshell-here)


  (add-hook 'eshell-mode-hook (lambda ()
                                (add-to-list 'eshell-visual-commands "ssh")
                                (add-to-list 'eshell-visual-commands "tail"))))


(provide 'gg-eshell)
