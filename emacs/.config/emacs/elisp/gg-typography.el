(defvar gg/monospaced-font-family "Aporetic Serif Mono"
  "Default monospaced font family.")

(defvar gg/monospaced-font-height 190
  "Default monospaced font height.")

(defvar gg/ui-font-family "Aporetic Sans Mono"
  "Default UI font family.")

(defvar gg/ui-font-height 170
  "Default UI font height.")

(defmacro with-font-available (font-family &rest body)
  "Execute `BODY' if `FONT-FAMILY' is available. Otherwise display an error."
  (declare (indent 1))
  `(if (member ,font-family (font-family-list))
       (progn
         ,@body)
     (message "Font `%s' is not installed. Please pick a different font." ,font-family)))

(defun gg/set-face-font (face family height &rest attrs)
  (when (facep face)
    (apply #'set-face-attribute
           face nil
           :family family
           :height height
           attrs)))

(defun gg/apply-editing-fonts ()
  "Apply the main monospaced editing fonts."
  (gg/set-face-font 'default
                    gg/monospaced-font-family
                    gg/monospaced-font-height)
  (gg/set-face-font 'fixed-pitch
                    gg/monospaced-font-family
                    1.0))

(defun gg/apply-ui-fonts ()
  "Apply UI font faces."
  (dolist (face '(mode-line mode-line-inactive))
    (gg/set-face-font face
                      gg/ui-font-family
                      gg/ui-font-height
                      :weight 'regular
                      :box nil))

  (gg/set-face-font 'minibuffer-prompt
                    gg/ui-font-family
                    gg/ui-font-height))

(with-font-available gg/monospaced-font-family
  (gg/apply-editing-fonts))

(with-font-available gg/ui-font-family
  (gg/apply-ui-fonts))

(defun gg/use-ui-font-in-minibuffer ()
  "Use the UI font in the minibuffer."
  (when (member gg/ui-font-family (font-family-list))
    (face-remap-add-relative
     'default
     `(:family ,gg/ui-font-family
       :height ,gg/ui-font-height))))

(add-hook 'minibuffer-setup-hook #'gg/use-ui-font-in-minibuffer)

;; Do not set a different font on the selected Vertico candidate.
(with-eval-after-load 'vertico
  (set-face-attribute 'vertico-current nil
                      :inherit 'highlight
                      :family 'unspecified
                      :height 'unspecified))

;; Marginalia annotations
(with-eval-after-load 'marginalia
  (with-font-available gg/ui-font-family
    (dolist (face '(marginalia-documentation
                    marginalia-key
                    marginalia-symbol
                    marginalia-type
                    marginalia-value))
      (gg/set-face-font face
                        gg/ui-font-family
                        gg/ui-font-height))))

(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; Look & Feel for prose writing
(use-package olivetti
  :ensure t
  :hook text-mode
  :config
  (setq olivetti-style t))

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
