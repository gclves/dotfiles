(defvar gg--font-list
  '(
    ("JetBrains Mono" . 110)
    ("Cascadia Code" . 140)
    ("Fira Code" . 110)
    ("Fantasque Sans Mono" . 110)
    ("Inconsolata" . 130)
    ("Go Mono" . 120)
    ("PT Mono" . 110)
    ("Monaco" . 110)
    ("Monospace" . 120))
  "List (Font_Family . Font_Size) pairs to use, in order of preference.")

(defvar gg--variable-pitch-font-list
  '(
    ("Roboto" . 1.2)
    ("Go" . 1.0))
  "List of variable-pitch fonts to use, in order of preference.")

(defun gg--first-available-font (font-list)
  "Get the default font to the first available from FONT-LIST.
Given a list of cons cells containing font name and font size."
  (let ((supported-fonts (font-family-list)))
    (seq-find (lambda (font) (member (car font) supported-fonts))
              font-list)))

(let* ((monospaced (gg--first-available-font gg--font-list))
       (monospaced-family (car monospaced))
       (monospaced-height (cdr monospaced))
       (proportional (gg--first-available-font gg--variable-pitch-font-list))
       (proportional-family (car proportional))
       (proportional-height (cdr proportional)))
  (set-face-attribute 'default nil :family monospaced-family :height monospaced-height)
  (set-face-attribute 'fixed-pitch nil :family monospaced-family :height monospaced-height)
  (set-face-attribute 'variable-pitch nil :family proportional-family :height proportional-height))

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(global-prettify-symbols-mode 1)

;; Look & Feel for prose writing
(use-package olivetti
  :ensure t
  :hook text-mode)

(with-eval-after-load 'text-mode
  (defun gg--set-up-text-mode ()
    "Set up aesthetic adaptations for dealing with text.
This includes `variable-pitch-mode' and a bar cursor."
    (interactive)
    (variable-pitch-mode +1)
    (setq cursor-type 'bar))

  (add-hook 'text-mode-hook 'gg--set-up-text-mode))

(add-hook 'git-commit-mode-hook (lambda () (interactive) (variable-pitch-mode -1)))

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

(setq-default indent-tabs-mode nil)

(with-eval-after-load 'whitespace
  ;; Render all whitespace: useful, but busy
  ;; (setq whitespace-style '(face trailing tabs newline tab-mark space-mark))
  (setq whitespace-style '(face trailing tabs newline)
        whitespace-display-mappings
        '((tab-mark 9 [8594 9])
          (space-mark 32 [183] [46])
          (space-mark 160 [164])
          (newline-mark 10 [8617 10])))
  (add-hook 'prog-mode-hook 'whitespace-mode))


(provide 'gg-typography)
