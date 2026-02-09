(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Sane defaults
(setq ring-bell-function 'ignore
      use-short-answers t)

(require 'gg-packages)

(use-package exec-path-from-shell
  :init
  (when (or (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))

(require 'gg-macos)

(defun gg/byte-compile-elisp ()
  "Byte-compile ~/.emacs.d/elisp, excluding elisp/vendor/."
  (interactive)
  (let* ((root (expand-file-name "elisp" user-emacs-directory))
         (vendor (file-name-as-directory (expand-file-name "vendor" root)))
         (byte-compile-warnings '(not obsolete))
         (n 0))
    (dolist (f (directory-files-recursively root "\\.el\\'"))
      (unless (string-prefix-p vendor f)
        (byte-compile-file f)
        (setq n (1+ n))))
    (message "Byte-compiled %d files (excluding %s)" n vendor)))

(require 'gg-ui)
(require 'gg-typography)
(require 'gg-editing)
(require 'gg-minibuffer)
(require 'gg-git)
(require 'gg-prog)
(require 'gg-eshell)
(require 'gg-notes)
(require 'gg-help)
(require 'gg-project)
;; (require 'gg-mail)
(require 'gg-llm)
(require 'gg-rss)

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

;; TODO: move all of these into the "modules" above
(load-file (expand-file-name "config.el" user-emacs-directory))

(message "Hack the planet!")
