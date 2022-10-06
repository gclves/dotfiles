(require 'org)
(require 'ob-tangle)

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'gg-vendor)
(require 'gg-use-package)
; (require 'gg-evil)
(require 'gg-ui)
(require 'gg-git)
(require 'gg-prog)
(require 'gg-web-dev)
(require 'gg-eshell)
(require 'gg-notes)
(require 'gg-ruby)
(require 'gg-help)
(require 'gg-scala)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
