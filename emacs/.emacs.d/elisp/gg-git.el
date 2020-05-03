(require 'use-package)

(use-package git-gutter
    :init (global-git-gutter-mode +1)
    :config
    (setq git-gutter:update-interval 2)
    (mapc (lambda (pair)
            (set-face-background (car pair) (cdr pair))
            (set-face-foreground (car pair) (cdr pair)))
          '((git-gutter:added . "#8bc34a")
            (git-gutter:modified . "#b39ddb")
            (git-gutter:deleted . "#f36c60")))
    :bind
    (("C-c C-n" . git-gutter:next-hunk)
     ("C-c C-p" . git-gutter:previous-hunk)))

(use-package magit
    :bind
    (([f8] . magit-status)
     ("C-x g" . magit-status)
     ("M-s-b" . magit-blame))
    :config
    ;; magit windows should open in the current window
    (add-to-list 'same-window-regexps "^magit: "))

(provide 'gg-git)
