(use-package deft
  :bind
  (([f5] . deft)
   ("C-x C-g" . deft-find-file))
  :config
  (setq deft-extensions '("org" "md" "txt")
        deft-recursive t
        deft-default-extension "org"
        deft-directory "~/sync/Notes"
        deft-new-file-format "%Y%m%d%H%M"))

(use-package org-roam
  :bind
  (:map org-mode-map
        ("C-c i" . org-roam-insert))
  :config
  (setq org-roam-directory "~/sync/Notes"
        org-roam-db-update-method 'immediate)
  (add-hook 'after-init-hook 'org-roam-mode))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; Look & Feel for long-form writing
(use-package olivetti
  :config
  (defun setup-olivetti-mode ()
    (interactive)
    (olivetti-mode +1)
    (olivetti-set-width 80))

  (add-hook 'text-mode-hook 'setup-olivetti-mode))

(use-package modus-themes)

(defun setup-text-mode ()
  "Set up aesthetic adaptations for dealing with text.  This includes `variable-pitch-mode' and a bar cursor."
  (interactive)
  (variable-pitch-mode +1)
  (setq cursor-type 'bar)
  (company-mode -1))

(add-hook 'text-mode-hook 'setup-text-mode)

;;; Set up the typography
(set-face-attribute 'variable-pitch nil :font "iA Writer Quattro V-14")
(set-face-attribute 'fixed-pitch nil :font "Go Mono-13")
(set-face-attribute 'org-quote nil :font "iA Writer Duo V-14" :slant 'italic)

(provide 'gg-notes)
;;; gg-notes.el ends here
