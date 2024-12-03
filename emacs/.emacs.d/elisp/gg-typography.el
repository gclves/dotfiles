(global-prettify-symbols-mode 1)

;; Look & Feel for long-form writing
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

(defun gg--set-up-org-typography ()
  "Set up typography for Org-mode."
  ;; Set up the typography
  (defvar gg--prose-monospace-font "Go Mono-18"
    "The font used for Monospace text within prose.")
  (defvar gg--prose-font "Go-18"
    "The font used for body text within prose.")

  (dolist (face '(org-code org-block org-table org-checkbox))
    (set-face-attribute face nil :font gg--prose-monospace-font))

  ;; set the `fixed-pitch' to be the same family as the default
  (set-face-attribute 'fixed-pitch nil :family (face-attribute 'default :family))
  (set-face-attribute 'variable-pitch nil :font gg--prose-font)
  (set-face-attribute 'org-quote nil :font gg--prose-font :slant 'italic))

;; XXX: Do we really need to run all that as a hook?!
(with-eval-after-load 'org
  (setq org-startup-indented t)         ; Enable `org-indent-mode' by default
  (add-hook 'org-mode-hook 'gg--set-up-org-typography))

(defvar gg--font-list
  '(
    ("Cascadia Code" . 14)
    ("Fira Code" . 11)
    ("JetBrains Mono" . 11)
    ("Fantasque Sans Mono" . 11)
    ("Inconsolata" . 13)
    ("Go Mono" . 12)
    ("PT Mono" . 11)
    ("Monaco" . 11))
  "List (Font_Family . Font_Size) pairs to use, in order of preference.")

;; TODO: receive the FRAME as a parameter here
(defun load-font-from-options (font-list)
  "Set the default font to the first available from FONT-LIST.
Given a list of cons cells containing font name and font size,
call `set-default-font' on the first one that's available."
  (let ((supported-fonts (font-family-list))
        (format-font-name (lambda (font)
                            (let ((font-name (car font))
                                  (font-size (cdr font)))
                              (concat font-name "-" (number-to-string font-size))))))
    (seq-some (lambda (font)
                (when (member (car font) supported-fonts)
                  (set-frame-font (funcall format-font-name font) nil t)
                  t))
              font-list)))

(defun gg--load-fonts-for-frame (frame)
  "Set the preferred fonts for a newly-created frame.  Actually disregards FRAME."
  (set-face-attribute 'mode-line-inactive frame :font gg--modeline-font)
  (set-face-attribute 'mode-line frame :font gg--modeline-font)
  (load-font-from-options gg--font-list))

;; TODO: figure out why the hook doesn't get invoked on initialization
(gg--load-fonts-for-frame nil)

(defun gg-load-fonts ()
  "Reload the fonts configuration."
  (interactive)
  (gg--load-fonts-for-frame nil))

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


(provide 'gg-typography)
