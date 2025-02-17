(add-to-list 'load-path (expand-file-name "elisp" user-emacs-directory))

;; Sane defaults
(setq ring-bell-function 'ignore
      use-short-answers t)

(require 'gg-packages)

;; GC Magic Hack is supposed to improve perf by dynamically
;; adjusting the GC thresholds depending on activity level
(use-package gcmh
  :config
  (gcmh-mode 1))

(use-package exec-path-from-shell
  :init
  (when (or (daemonp) (memq window-system '(mac ns x)))
    (exec-path-from-shell-initialize)))

;; TODO: Move this somewhere else
(defmacro on-macOS (&rest body)
  "Evaluate BODY if running on macOS."
  `(when (eq system-type 'darwin)
     ,@body))

(defmacro unless-on-macOS (&rest body)
  "Evaluate BODY if not running on macOS."
  `(unless (eq system-type 'darwin)
     ,@body))

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


(on-macOS
  (require 'gg-osx-config))

(setq custom-file
      (expand-file-name "custom.el" user-emacs-directory))
(load custom-file :noerror)

;; TODO: move all of these into the "modules" above
(load-file (expand-file-name "config.el" user-emacs-directory))

(message "Hack the planet!")
