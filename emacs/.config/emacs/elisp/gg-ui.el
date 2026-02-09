(require 'gg-macos)

;; Window setup
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(fringe-mode 1) ; 1px fringe
(column-number-mode)
(blink-cursor-mode +1)
(tooltip-mode -1)

(unless-on-macOS
 (menu-bar-mode -1))

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

;; Open some special buffers in popup windows
(with-eval-after-load 'window
  ;; Only a single side window on each side
  (setq window-sides-slots '(1 1 1 1))

  (defvar gg--special-display-regexps
    '("^\\*Async Shell Command\\*\\(<[0-9]+>\\)?$"
      "^\\*webpack\\*$"
      "^\\*server\\*$"
      "^\\*Completions\\*$"
      "^\\*Help\\*$"
      "^\\*grep\\*$"
      "^\\*Apropos\\*$"
      "^\\*elisp macroexpansion\\*$"
      "^\\*local variables\\*$"
      "^\\*Compile-Log\\*$"
      "^\\*Quail Completions\\*$"
      "^\\*Occur\\*$"
      "^\\*frequencies\\*$"
      "^\\*compilation\\*$"
      "^\\*Locate\\*$"
      "^\\*Colors\\*$"
      "^\\*tumme-display-image\\*$"
      "^\\*SLIME Description\\*$"
      "^\\*.* output\\*$"               ; tex compilation buffer
      "^\\*TeX Help\\*$"
      "^\\*Shell Command Output\\*$"
      "^\\*Backtrace\\*$"
      "^\\*helpful .*\\*$"
      "^\\*tide-.*\\*$"
      "^TODO$")
    "List of regular expressions for buffer names. Such buffers will be opened in a special pop-up window.")

  (defun gg--is-special-buffer (buffer-name _action)
    "Return t if BUFFER-NAME matches any of our popup buffer patterns."
    (cl-some (lambda (regex)
               (string-match-p regex buffer-name)) gg--special-display-regexps))

  (add-to-list 'display-buffer-alist
               '(gg--is-special-buffer
                 (display-buffer-reuse-window display-buffer-pop-up-window)
                 (side . right)
                 (slot . -1)
                 (window-min-width . 50))))

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

;;   ____      _                     _
;;  / ___|___ | | ___  _ __ ___  ___| |__   ___ _ __ ___   ___
;; | |   / _ \| |/ _ \| '__/ __|/ __| '_ \ / _ \ '_ ` _ \ / _ \
;; | |__| (_) | | (_) | |  \__ \ (__| | | |  __/ | | | | |  __/
;;  \____\___/|_|\___/|_|  |___/\___|_| |_|\___|_| |_| |_|\___|

(defvar gg--light-theme 'modus-operandi-tinted)
(defvar gg--dark-theme 'modus-vivendi-tinted)

;; TODO: polyfill the 'ns-system-appearance-change function with a timer
;; if not on MacOS
(unless-on-macOS
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
 (run-at-time "18:00" (* 60 60 24) (lambda () (gg--load-dark-theme))))

(on-macOS
 (defun gg--sync-theme (appearance)
   "Load theme, taking current system APPEARANCE into consideration."
   (mapc #'disable-theme custom-enabled-themes)
   (pcase appearance
     ('light (load-theme gg--light-theme t))
     ('dark (load-theme gg--dark-theme t))))

 (add-hook 'ns-system-appearance-change-functions #'gg--sync-theme))


;; ___
;;|_ _|___ ___  _ __  ___
;; | |/ __/ _ \| '_ \/ __|
;; | | (_| (_) | | | \__ \
;;|___\___\___/|_| |_|___/

(use-package nerd-icons)

(use-package nerd-icons-completion
  :after marginalia
  :config
  (add-hook 'marginalia-mode-hook #'nerd-icons-completion-marginalia-setup))

(use-package nerd-icons-corfu
  :after corfu
  :config
  (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

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
(global-set-key (kbd "<escape>") 'keyboard-escape-quit)
(setq sentence-end-double-space nil)

(provide 'gg-ui)
;;; gg-ui.el ends here
