(defvar gg--light-theme 'alabaster-themes-light-bg
  "Theme to use in light mode.")
(defvar gg--dark-theme 'alabaster-themes-dark
  "Theme to use in dark mode.")

(defun gg--sync-theme (appearance)
  "Load theme, taking current system APPEARANCE into consideration."
  (mapc #'disable-theme custom-enabled-themes)
  (pcase appearance
    ('light (load-theme gg--light-theme t))
    ('dark (load-theme gg--dark-theme t))
    (_ (load-theme gg--dark-theme t))))

(if (boundp 'ns-system-appearance-change-functions)
    (add-hook 'ns-system-appearance-change-functions #'gg--sync-theme)
  (progn
    ;; Fallback: switch between light and dark themes on a timer
    (run-at-time "07:00" (* 60 60 24) (lambda () (gg--sync-theme 'light)))
    (run-at-time "18:00" (* 60 60 24) (lambda () (gg--sync-theme 'dark)))))

(use-package alabaster-themes
  :ensure t
  :commands (alabaster-themes-select))

(provide 'gg-colorscheme)
