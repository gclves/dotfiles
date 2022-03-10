(use-package deft
  :bind
  (([f5] . deft)
   ("C-x C-g" . deft-find-file))
  :config
  (setq deft-extensions '("org" "md" "txt")
        deft-recursive t
        deft-default-extension "org"
        deft-directory "~/Notes"
        deft-new-file-format "%Y%m%d%H%M"))

(use-package org-roam
  :bind
  (:map org-mode-map
        ("C-c i" . org-roam-insert))
  :config
  (setq org-roam-directory "~/Notes"
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

(use-package modus-themes
  :init
  (setq modus-themes-hl-line 'underline-accented
        modus-themes-italic-constructs t
        modus-themes-region 'no-extend
        modus-themes-variable-pitch-ui t
        modus-themes-subtle-line-numbers t)
  (modus-themes-load-themes)
  :config
  (modus-themes-load-operandi))

(defun setup-text-mode ()
  "Set up aesthetic adaptations for dealing with text.  This includes `variable-pitch-mode' and a bar cursor."
  (interactive)
  (variable-pitch-mode +1)
  (setq cursor-type 'bar)
  (company-mode -1))

(add-hook 'text-mode-hook 'setup-text-mode)

(defun setup-org ()
  "Set up typography for Org-mode."
;;; Set up the typography
  (defvar gg--monospace-font "Go Mono-18"
    "The font used for Monospace text within prose.")
  (defvar gg--body-font "Go-18"
    "The font used for body text within prose.")

  (cl-loop for face in '(fixed-pitch org-code org-block)
           do (set-face-attribute face nil :font gg--monospace-font))

  (set-face-attribute 'variable-pitch nil :font gg--body-font)
  (set-face-attribute 'org-quote nil :font gg--body-font :slant 'italic)
  ;; org-table needs to be monospaced to be aligned
  (set-face-attribute 'org-table nil :font "Monaco-19")
  (set-face-attribute 'org-checkbox nil :font "Monaco-19"))

;; XXX: Do we really need to run all that as a hook?!
(add-hook 'org-mode-hook 'setup-org)

(use-package neuron-mode
  :config
  (setq neuron-executable "~/.nix-profile/bin/neuron"
        neuron-default-zettelkasten-directory "~/src/neuron-site"))

;; Org-powered presentations
(require 'epresent)

(provide 'gg-notes)
;;; gg-notes.el ends here
