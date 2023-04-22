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

;; Modeline
(setq display-time-default-load-average 0 ; 1-minute load average
      display-time-24hr-format t)
(display-time-mode)
(display-battery-mode)
(use-package minions :config (minions-mode 1))

;; Highlight todo entries
(use-package hl-todo
  :init (global-hl-todo-mode)
  :bind
  (:map hl-todo-mode-map
        ("C-c <up>" . hl-todo-previous)
        ("C-c <down>" . hl-todo-next)
        ("C-c T" . hl-todo-occur)))

(setq-default fill-column 80)
(setq async-shell-command-display-buffer nil)

;; Look & Feel for long-form writing
(use-package olivetti
  :config
  (defun gg--setup-olivetti-mode ()
    (interactive)
    (olivetti-mode +1)
    (olivetti-set-width 80)))

;;   (add-hook 'text-mode-hook 'gg--setup-olivetti-mode))
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

(defun gg--load-dark-theme ()
  "Load the configured dark theme."
  (disable-theme gg--light-theme)
  (load-theme gg--dark-theme t))

(defun gg--load-light-theme ()
  "Load the configured light theme."
  (disable-theme gg--dark-theme)
  (load-theme gg--light-theme t))

 ;; Switch between light and dark themes
(run-at-time "07:00" (* 60 60 24) (lambda () (gg--load-light-theme)))
(run-at-time "18:00" (* 60 60 24) (lambda () (gg--load-dark-theme)))

(use-package hide-mode-line
  :config
  (add-hook 'completion-list-mode-hook #'hide-mode-line-mode)
  (add-hook 'shell-mode-hook #'hide-mode-line-mode)
  (add-hook 'eshell-mode-hook #'hide-mode-line-mode))

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
  (setq org-startup-indented t) ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook 'setup-org-typography))

(defvar gg--font-list
  '(
    ("JetBrains Mono" . 14)
    ("Fira Code" . 14)
    ("Go Mono" . 14)
    ("Fantasque Sans Mono" . 17)
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

(provide 'gg-ui)
;;; gg-ui.el ends here
