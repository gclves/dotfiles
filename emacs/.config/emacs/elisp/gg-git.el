(require 'use-package)

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

(on-macOS
 (shell-command "ssh-add --apple-load-keychain"))

(provide 'gg-git)
