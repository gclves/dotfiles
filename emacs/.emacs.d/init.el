(setq user-full-name "Guilherme Goncalves"
      user-email-address "gsg@ggoncalves.me"
      system-time-locale "C")

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'gg-vendor)
(require 'gg-use-package)
; (require 'gg-evil)
(require 'gg-ui)
(require 'gg-git)
(require 'gg-prog)
(require 'gg-eshell)
(require 'gg-notes)
(require 'gg-help)
(require 'gg-mail)

(when (string-equal system-type "darwin")
  (require 'gg-osx-config))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

(load-file (expand-file-name "config.el" user-emacs-directory))
