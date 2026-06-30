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

(setq pixel-scroll-precision-interpolate-mice nil
      mouse-wheel-scroll-amount '(3
                                  ((shift) . hscroll)
                                  ((meta))
                                  ((control meta) . global-text-scale)
                                  ((control) . text-scale)))

;; Highlight current line, but only in text or prog modes
(add-hook 'text-mode-hook 'hl-line-mode)
(add-hook 'prog-mode-hook 'hl-line-mode)

(use-package popper
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
          "^\\*Completions\\*$"
          "^\\*Help\\*$"
          "^\\*grep\\*$"
          "^\\*Apropos\\*$"
          "^\\*elisp macroexpansion\\*$"
          "^\\*local variables\\*$"
          "^\\*Compile-Log\\*$"
          "^\\*Occur\\*$"
          "^\\*frequencies\\*$"
          "^\\*compilation\\*$"
          "^\\*Locate\\*$"
          "^\\*Colors\\*$"
          "^\\*tumme-display-image\\*$"
          "^\\*SLIME Description\\*$"
          "^\\*Backtrace\\*$"
          "^\\*helpful .*\\*$"
          "^\\*tide-.*\\*$"
          "^TODO$"
          "^\\*rg\\*$" rg-mode
          help-mode
          compilation-mode
          "^\\*eshell.*\\*$" eshell-mode ;eshell as a popup
          "^\\*shell.*\\*$"  shell-mode  ;shell as a popup
          "^\\*term.*\\*$"   term-mode   ;term as a popup
          "^\\*vterm.*\\*$"  vterm-mode  ;vterm as a popup
          ))
  (popper-mode +1)
  (popper-echo-mode +1)

  (add-hook 'popper-mode-hook #'font-size-decrease)

  (setq popper-window-height
        (lambda ()
          (floor (frame-height) 4))))



;;  __  __           _      _ _
;; |  \/  | ___   __| | ___| |_)_ __   ___
;; | |\/| |/ _ \ / _` |/ _ \ | | '_ \ / _ \
;; | |  | | (_) | (_| |  __/ | | | | |  __/
;; |_|  |_|\___/ \__,_|\___|_|_|_| |_|\___|

(defmacro gg--add-nano-modeline-hook (hook mode-fn)
  "Enable MODE-FN from HOOK while hiding the standard mode line."
  `(add-hook ,hook
             (lambda ()
               (,mode-fn)
               (setq-local mode-line-format nil))))

(defun gg--sync-nano-modeline-faces ()
  "Refresh `nano-modeline' faces to match the current theme."
  (when (featurep 'nano-modeline)
    (let ((default-fg (face-foreground 'default nil t))
          (default-bg (face-background 'default nil t))
          (header-bg (face-background 'header-line nil t))
          (shadow-fg (face-foreground 'shadow nil t)))
      (set-face-attribute 'nano-modeline-active nil
                          :foreground default-fg
                          :background header-bg
                          :box `(:line-width 1 :color ,default-bg))
      (set-face-attribute 'nano-modeline-inactive nil
                          :inherit '(shadow nano-modeline-active))
      (set-face-attribute 'nano-modeline-status nil
                          :foreground default-bg
                          :background shadow-fg
                          :inherit 'bold)
      (set-face-attribute 'nano-modeline-button-active-face nil
                          :foreground default-fg
                          :background default-bg
                          :family "Roboto Mono"
                          :weight 'regular
                          :box `(:line-width 2 :color ,default-fg :style flat-button))
      (set-face-attribute 'nano-modeline-button-inactive-face nil
                          :foreground shadow-fg
                          :background header-bg
                          :family "Roboto Mono"
                          :weight 'regular
                          :box `(:line-width 2 :color ,default-fg :style flat-button))
      (set-face-attribute 'nano-modeline-button-highlight-face nil
                          :foreground default-bg
                          :background default-fg
                          :family "Roboto Mono"
                          :weight 'bold)
      (set-face-attribute 'nano-modeline--empty-face nil
                          :foreground default-fg))))

(use-package nano-modeline
  :config
  (setq nano-modeline-position #'nano-modeline-header)
  (gg--sync-nano-modeline-faces)
  (gg--add-nano-modeline-hook 'prog-mode-hook nano-modeline-prog-mode)
  (gg--add-nano-modeline-hook 'text-mode-hook nano-modeline-text-mode)
  (gg--add-nano-modeline-hook 'org-mode-hook nano-modeline-org-mode)
  (gg--add-nano-modeline-hook 'pdf-view-mode-hook nano-modeline-pdf-mode)
  (gg--add-nano-modeline-hook 'mu4e-headers-mode-hook nano-modeline-mu4e-headers-mode)
  (gg--add-nano-modeline-hook 'mu4e-view-mode-hook nano-modeline-mu4e-message-mode)
  (gg--add-nano-modeline-hook 'elfeed-show-mode-hook nano-modeline-elfeed-entry-mode)
  (gg--add-nano-modeline-hook 'elfeed-search-mode-hook nano-modeline-elfeed-search-mode)
  (gg--add-nano-modeline-hook 'term-mode-hook nano-modeline-term-mode)
  (gg--add-nano-modeline-hook 'xwidget-webkit-mode-hook nano-modeline-xwidget-mode)
  (gg--add-nano-modeline-hook 'messages-buffer-mode-hook nano-modeline-message-mode)
  (gg--add-nano-modeline-hook 'org-capture-mode-hook nano-modeline-org-capture-mode)
  (gg--add-nano-modeline-hook 'org-agenda-mode-hook nano-modeline-org-agenda-mode))

;;   ____      _                     _
;;  / ___|___ | | ___  _ __ ___  ___| |__   ___ _ __ ___   ___
;; | |   / _ \| |/ _ \| '__/ __|/ __| '_ \ / _ \ '_ ` _ \ / _ \
;; | |__| (_) | | (_) | |  \__ \ (__| | | |  __/ | | | | |  __/
;;  \____\___/|_|\___/|_|  |___/\___|_| |_|\___|_| |_| |_|\___|

(defvar gg--light-theme 'modus-operandi-tinted)
(defvar gg--dark-theme 'modus-vivendi-tinted)



(if (boundp 'ns-system-appearance-change-functions)
  (progn
    (on-macOS
     (defun gg--sync-theme (appearance)
       "Load theme, taking current system APPEARANCE into consideration."
       (mapc #'disable-theme custom-enabled-themes)
       (pcase appearance
         ('light (load-theme gg--light-theme t))
         ('dark (load-theme gg--dark-theme t)))
       (gg--sync-nano-modeline-faces))

     (add-hook 'ns-system-appearance-change-functions #'gg--sync-theme)))
  (progn
    (defun gg--reset-themes ()
      "Disable all currently enabled themes."
      (dolist (theme custom-enabled-themes)
        (disable-theme theme)))

    (defun gg--load-dark-theme ()
      "Load the configured dark theme."
      (interactive)
      (gg--reset-themes)
      (load-theme gg--dark-theme t)
      (gg--sync-nano-modeline-faces))

    (defun gg--load-light-theme ()
      "Load the configured light theme."
      (interactive)
      (gg--reset-themes)
      (load-theme gg--light-theme t)
      (gg--sync-nano-modeline-faces))

    ;; Switch between light and dark themes
    (run-at-time "07:00" (* 60 60 24) (lambda () (gg--load-light-theme)))
    (run-at-time "18:00" (* 60 60 24) (lambda () (gg--load-dark-theme)))))


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
