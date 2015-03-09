(require 'evil)

(require 'evil-leader)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(setq evil-leader/in-all-states 1)

(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist 1)
(evil-leader/set-key "SPC" 'evil-search-highlight-persist-remove-all)

(evil-mode 1)

(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
