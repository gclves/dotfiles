(require 'use-package)

(use-package git-gutter
    :config
    (setq git-gutter:update-interval 2
          git-gutter:hide-gutter t
          git-gutter:linum-enabled nil)
    (mapc (lambda (pair)
            (set-face-background (car pair) (cdr pair))
            (set-face-foreground (car pair) (cdr pair)))
          '((git-gutter:added . "#8bc34a")
            (git-gutter:modified . "#b39ddb")
            (git-gutter:deleted . "#f36c60")))
    (global-git-gutter-mode +1)
    :bind
    (("C-c C-n" . git-gutter:next-hunk)
     ("C-c C-p" . git-gutter:previous-hunk)))

(use-package magit
    :bind
    (([f8] . magit-status)
     ("C-x g" . magit-status)
     ("C-M-S-s-b" . magit-blame-addition))
    :config
    ;; magit windows should open in the current window
    (add-to-list 'same-window-regexps "^magit: "))

(use-package browse-at-remote
  :bind
  (("C-c g g" . browse-at-remote)
   ("C-c g G" . browse-at-remote-kill))
  :config
  (setq browse-at-remote-add-line-number-if-no-region-selected nil))

(provide 'gg-git)
