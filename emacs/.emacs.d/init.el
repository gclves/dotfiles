(package-initialize)
(require 'org)
(require 'ob-tangle)

(add-to-list 'load-path (concat user-emacs-directory "elisp"))

(require 'gg-vendor)
(require 'gg-git)
(require 'gg-web-dev)
(require 'gg-eshell)

(org-babel-load-file (expand-file-name "config.org" user-emacs-directory))
