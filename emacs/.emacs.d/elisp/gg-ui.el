(require 'cl)

;; Window setup
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(global-hl-line-mode t)
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
      help-window-select t)

(global-prettify-symbols-mode 1)
(pixel-scroll-precision-mode +1)

;; Modeline
(setq display-time-default-load-average 0 ; 1-minute load average
      display-time-24hr-format t)
(display-time-mode)
(display-battery-mode)
(use-package minions :config (minions-mode 1))

(defvar gg--modeline-font "Roboto-15"
  "The font face used for the modeline.")

(set-face-attribute 'mode-line-inactive nil :font gg--modeline-font)
(set-face-attribute 'mode-line nil :font gg--modeline-font)

(use-package hide-mode-line
  :config
  (setq-local hide-mode-line-hooks '(completion-list-mode-hook
                                     neotree-mode-hook
                                     eshell-mode-hook))
  (dolist (hook hide-mode-line-hooks)
    (add-hook hook #'hide-mode-line-mode)))

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

;; Look & Feel for long-form writing
(use-package olivetti
  :hook text-mode
  :config
  (defun gg--setup-olivetti-mode ()
    (interactive)
    (olivetti-mode +1)
    (olivetti-set-width 80)))

;; Modus themes
;; Tweak the themes
(setq modus-themes-hl-line '(underline accented)
      modus-themes-italic-constructs t
      modus-themes-region '(no-extend)
      modus-themes-variable-pitch-ui t
      modus-themes-subtle-line-numbers t
      modus-themes-org-blocks 'gray-background
      modus-themes-subtle-line-numbers nil)

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

(use-package hide-mode-line
  :hook ((completion-list-mode shell-mode eshell-mode) . hide-mode-line-mode))

(defun setup-text-mode ()
  "Set up aesthetic adaptations for dealing with text.
This includes `variable-pitch-mode' and a bar cursor."
  (interactive)
  (variable-pitch-mode +1)
  (setq cursor-type 'bar)
  (company-mode -1))

(add-hook 'text-mode-hook 'setup-text-mode)

(defun setup-org-typography ()
  "Set up typography for Org-mode."
  ;; Set up the typography
  (defvar gg--monospace-font "Go Mono-18"
    "The font used for Monospace text within prose.")
  (defvar gg--body-font "Go-18"
    "The font used for body text within prose.")

  (cl-loop for face in '(org-code org-block org-table org-checkbox)
           do (set-face-attribute face nil :font gg--monospace-font))

  ;; set the `fixed-pitch' to be the same family as the default
  (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))
  (set-face-attribute 'variable-pitch nil :font gg--body-font)
  (set-face-attribute 'org-quote nil :font gg--body-font :slant 'italic))

;; XXX: Do we really need to run all that as a hook?!
(with-eval-after-load 'org
  (setq org-startup-indented t)         ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook 'setup-org-typography))

(defvar gg--font-list
  '(
    ("Fira Code" . 16)
    ("Fantasque Sans Mono" . 19)
    ("Go Mono" . 17)
    ("PT Mono" . 17)
    ("Cascadia Code" . 15)
    ("Monaco" . 15)
    ("Inconsolata" . 19))
  "List (Font_Family . Font_Size) pairs to use, in order of preference.")

;; TODO: receive the FRAME as a parameter here
(defun load-font-from-options (font-list)
  "Set the default font to the first available from FONT-LIST.
Given a list of cons cells containing font name and font size,
call `set-default-font' on the first one that's available"
  (let ((supported-fonts (font-family-list))
        (format-font-name (lambda (font)
                            (cl-destructuring-bind (font-name . font-size) font
                              (concat font-name "-" (number-to-string font-size))))))
    (cl-some (lambda (font) (when (member (car font) supported-fonts)
                      (set-frame-font (funcall format-font-name font))
                      t))
          font-list)))

(defun gg--load-fonts-for-frame (frame)
  "Set the preferred fonts for a newly-created frame.  Actually disregards FRAME."
  (load-font-from-options gg--font-list))

;; TODO: figure out why the hook doesn't get invoked on initialization
(load-font-from-options gg--font-list)

(add-to-list 'after-make-frame-functions 'gg--load-fonts-for-frame t)

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(defun font-size-reset ()
  "Reset the text-scale to zero."
  (interactive)
  (text-scale-set 0))

(defvar font-size--increment 0.5
  "The increment value for `font-size-increase' and `font-size-decrease'.")

(defun font-size-increase ()
  "Increase the text-scale by `font-size--increment'."
  (interactive)
  (text-scale-increase font-size--increment))

(defun font-size-decrease ()
  "Decrease the text-scale by `font-size--increment'."
  (interactive)
  (text-scale-decrease font-size--increment))

(global-set-key (kbd "s-0") 'font-size-reset)
(global-set-key (kbd "s-=") 'font-size-increase)
(global-set-key (kbd "s--") 'font-size-decrease)

(setq fill-column 80)
(setq-default indent-tabs-mode nil)

;;; Whitespace
;; Render all whitespace: useful, but busy
;; (setq whitespace-style '(face trailing tabs newline tab-mark space-mark))
(setq whitespace-style '(face trailing tabs newline)
      whitespace-display-mappings
      '((tab-mark 9 [8594 9])
        (space-mark 32 [183] [46])
        (space-mark 160 [164])
        (newline-mark 10 [8617 10])))
(add-hook 'prog-mode-hook 'whitespace-mode)

(provide 'gg-ui)
;;; gg-ui.el ends here
