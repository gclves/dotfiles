(require 'package)  ;; Initialize the packages, avoiding a re-initialization
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ("melpa" . "https://melpa.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))

(require 'use-package-ensure)
(setq use-package-always-ensure t)
(use-package use-package-ensure-system-package)

(add-to-list 'load-path (concat user-emacs-directory "elisp/vendor"))

(provide 'gg-packages)
