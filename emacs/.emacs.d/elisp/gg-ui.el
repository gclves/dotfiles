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
  (defun setup-olivetti-mode ()
    (interactive)
    (olivetti-mode +1)
    (olivetti-set-width 80))

  (add-hook 'text-mode-hook 'setup-olivetti-mode))

;; Switch between light and dark themes
(run-at-time "07:00" (* 60 60 24) (lambda ()
                                    (disable-theme 'twilight)
                                    (load-theme 'parchment t)))
(run-at-time "18:00" (* 60 60 24) (lambda ()
                                    (disable-theme 'parchment)
                                    (load-theme 'twilight t)))

(use-package parchment-theme
  :config
  (load-theme 'parchment t))

(defun setup-text-mode ()
  "Set up aesthetic adaptations for dealing with text.  This includes `variable-pitch-mode' and a bar cursor."
  (interactive)
  (variable-pitch-mode +1)
  (setq cursor-type 'bar)
  (company-mode -1))

(add-hook 'text-mode-hook 'setup-text-mode)

(defun setup-org-typography ()
  "Set up typography for Org-mode."
;;; Set up the typography
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

(defun load-font-from-options (font-list)
  "Set the default font to the first available from FONT-LIST.
Given a list of cons cells containing font name and font size,
call `set-default-font' on the first one that's available"
  (let ((supported-fonts (font-family-list))
        (format-font-name (lambda (font)
                            (destructuring-bind (font-name . font-size) font
                              (concat font-name "-" (number-to-string font-size))))))
    (some (lambda (font) (when (member (car font) supported-fonts)
                           (set-frame-font (funcall format-font-name font))
                           t))
          font-list)))


(provide 'gg-ui)
;;; gg-ui.el ends here
