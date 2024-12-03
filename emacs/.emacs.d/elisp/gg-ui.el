;; Window setup
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(fringe-mode 1) ; 1px fringe
(column-number-mode)
(blink-cursor-mode +1)
(tooltip-mode -1)
(menu-bar-mode -1)
(setq inhibit-startup-message t
      initial-scratch-message ""
      mode-line-default-help-echo nil
      indicate-buffer-boundaries nil
      indicate-empty-lines nil
      cursor-in-non-selected-windows t
      highlight-nonselected-windows nil
      bidi-display-reordering nil
      blink-matching-paren 'jump
      help-window-select t
      x-underline-at-descent-line t
      switch-to-buffer-obey-display-actions t)

(pixel-scroll-precision-mode +1)

;; Highlight current line, but only in text or prog modes
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;;  __  __           _      _ _
;; |  \/  | ___   __| | ___| |_)_ __   ___
;; | |\/| |/ _ \ / _` |/ _ \ | | '_ \ / _ \
;; | |  | | (_) | (_| |  __/ | | | | |  __/
;; |_|  |_|\___/ \__,_|\___|_|_|_| |_|\___|

(setq display-time-default-load-average 0 ; 1-minute load average
      display-time-24hr-format t)
(display-time-mode)
(display-battery-mode)
(use-package minions :config (minions-mode 1))

(defvar gg--modeline-font "Roboto-13"
  "The font face used for the modeline.")

(set-face-attribute 'mode-line-inactive nil :font gg--modeline-font)
(set-face-attribute 'mode-line nil :font gg--modeline-font)

(use-package hide-mode-line
  :hook ((completion-list-mode shell-mode eshell-mode) . hide-mode-line-mode))

;;  __  __ _       _ _            __  __
;; |  \/  (_)_ __ (_) |__  _   _ / _|/ _| ___ _ __
;; | |\/| | | '_ \| | '_ \| | | | |_| |_ / _ \ '__|
;; | |  | | | | | | | |_) | |_| |  _|  _|  __/ |
;; |_|  |_|_|_| |_|_|_.__/ \__,_|_| |_|  \___|_|
(setq completion-cycle-threshold 1
      completions-detailed 1
      tab-always-indent 'complete
      completion-styles '(basic initials substring)

      completion-auto-help 'always
      completions-format 'one-column
      completions-group t
      completion-auto-select 'second-tab)
(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete)


;;   ____      _                     _
;;  / ___|___ | | ___  _ __ ___  ___| |__   ___ _ __ ___   ___
;; | |   / _ \| |/ _ \| '__/ __|/ __| '_ \ / _ \ '_ ` _ \ / _ \
;; | |__| (_) | | (_) | |  \__ \ (__| | | |  __/ | | | | |  __/
;;  \____\___/|_|\___/|_|  |___/\___|_| |_|\___|_| |_| |_|\___|

;; Load the themes
(defvar gg--dark-theme 'modus-vivendi)
(defvar gg--light-theme 'modus-operandi)


(defun gg--reset-themes ()
  "Disable all currently enabled themes."
  (dolist (theme custom-enabled-themes)
    (disable-theme theme)))

(defun gg--load-dark-theme ()
  "Load the configured dark theme."
  (interactive)
  (gg--reset-themes)
  (load-theme gg--dark-theme t))

(defun gg--load-light-theme ()
  "Load the configured light theme."
  (interactive)
  (gg--reset-themes)
  (load-theme gg--light-theme t))

 ;; Switch between light and dark themes
(run-at-time "07:00" (* 60 60 24) (lambda () (gg--load-light-theme)))
(run-at-time "18:00" (* 60 60 24) (lambda () (gg--load-dark-theme)))

;;   ___  _   _                     _          __  __
;;  / _ \| |_| |__   ___ _ __   ___| |_ _   _ / _|/ _|
;; | | | | __| '_ \ / _ \ '__| / __| __| | | | |_| |_
;; | |_| | |_| | | |  __/ |    \__ \ |_| |_| |  _|  _|
;;  \___/ \__|_| |_|\___|_|    |___/\__|\__,_|_| |_|

(when (display-graphic-p)
  (context-menu-mode))

;; Highlight todo entries
(use-package hl-todo
  :config (global-hl-todo-mode)
  :bind
  (:map hl-todo-mode-map
        ("C-c <up>" . hl-todo-previous)
        ("C-c <down>" . hl-todo-next)
        ("C-c T" . hl-todo-occur)))

(setq-default fill-column 80)
(setq async-shell-command-display-buffer nil)

(provide 'gg-ui)
;;; gg-ui.el ends here
