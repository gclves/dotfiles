(require 'package)
(setq package-archives '(("gnu" . "https://elpa.gnu.org/packages/")
                         ("org" . "https://orgmode.org/elpa/")
                         ;; ("marmalade" . "https://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(if (package-installed-p 'use-package)
    (setq use-package-always-ensure t)
  (progn (package-initialize)
         (package-install 'use-package)))

(provide 'gg-use-package)
