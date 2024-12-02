(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Sane defaults
(setq ring-bell-function 'ignore
      use-short-answers t)

(require 'gg-packages)

(require 'gg-ui)
(require 'gg-git)
(require 'gg-prog)
(require 'gg-eshell)
(require 'gg-notes)
(require 'gg-help)
(require 'gg-project)
(require 'gg-mail)

(when (string-equal system-type "darwin")
  (require 'gg-osx-config))

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file :noerror)

;; TODO: move all of these into the "modules" above
(load-file (expand-file-name "config.el" user-emacs-directory))
