(defvar gg-monospaced-font-family "Aporetic Serif Mono"
  "Default monospaced font family.")
(defvar gg-monospaced-font-height 180
  "Default monospaced font height.")

(if (member gg-monospaced-font-family (font-family-list))
    (progn
      (set-face-attribute 'default nil :family gg-monospaced-font-family :height gg-monospaced-font-height)
      (set-face-attribute 'fixed-pitch nil :family gg-monospaced-font-family :height 1.0))
  (message (format "Font `%s' is not installed. Please pick a different font." gg-monospaced-font-family)))

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

(global-prettify-symbols-mode 1)

;; Look & Feel for prose writing
(use-package olivetti
  :ensure t
  :hook text-mode)

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
