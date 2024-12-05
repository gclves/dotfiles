(setq use-short-answers t
      sentence-end-double-space nil)

(fido-vertical-mode)
(global-set-key (kbd "C-x C-f") 'find-file)
(global-set-key (kbd "s-o") 'find-file)
(global-set-key (kbd "s-b") 'ido-switch-buffer)
(setq ido-use-filename-at-point 'guess)

(defun another-window ()
  "Select the previous window in the current frame.
Uses `other-window' with an argument -1."
  (interactive)
  (other-window -1))

(global-set-key (kbd "C-S-w") (lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "s-w") (lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "s-]") 'other-window)
(global-set-key (kbd "s-[")  'another-window)
(global-set-key (kbd "M-.") 'xref-find-definitions-other-window)

(global-set-key (kbd "M-4") 'split-window-below)
(global-set-key (kbd "M-$") 'split-window-right)
(global-set-key (kbd "M-9") 'delete-window)

(windmove-default-keybindings 'super)
(setq windmove-wrap-around t)

(setq next-line-add-newlines nil
      scroll-margin 10
      scroll-step 1
      scroll-conservatively 100
      scroll-preserve-screen-position 1)


(defun gg-edit-emacs-config ()
  "Edit the Emacs configuration file."
  (interactive)
  (find-file user-init-file))

(defun reload-emacs-config ()
  "Reload the Emacs configuration."
  (interactive)
  (load user-init-file))


(global-set-key (kbd "C--") 'bury-buffer)

(global-set-key (kbd "s-r") 'rename-buffer)

(setq async-shell-command-buffer 'rename-buffer)

(defvar --backup-directory (concat user-emacs-directory "backups"))
(if (not (file-exists-p --backup-directory))
    (make-directory --backup-directory t))

(setq make-backup-files t               ; back up a file the first time it is saved
      backup-by-copying t               ; don't clobber symlinks
      version-control t                 ; version numbers for backup files
      delete-old-versions t             ; delete excess backup files silently
      delete-by-moving-to-trash t
      trash-directory (expand-file-name "~/.Trash")
      kept-old-versions 6               ; oldest versions to keep when a new numbered backup is made (default: 2)
      kept-new-versions 9               ; newest versions to keep when a new numbered backup is made (default: 2)
      auto-save-default nil             ; don't auto-save every buffer that visits a file
      auto-save-timeout 20              ; number of seconds idle time before auto-save (default: 30)
      auto-save-interval 200            ; number of keystrokes between auto-saves (default: 300)

      backup-directory-alist `((".*" . ,(expand-file-name --backup-directory)))
      auto-save-file-name-transforms `((".*" ,temporary-file-directory t))

      create-lockfiles nil)

(global-auto-revert-mode)               ; revert a file’s buffer automatically when it’s been changed on disk

(use-package all-the-icons-dired
  :hook (dired-mode . all-the-icons-dired-mode))

(with-eval-after-load 'dired
  (add-hook 'dired-mode-hook 'dired-hide-details-mode)
  (setq dired-dwim-target t))              ; move/copy files across dired buffers

(use-package restclient
  :commands (restclient-mode))

(global-set-key (kbd "M-\\") 'compile)
(global-set-key (kbd "C-\\") 'recompile)

(require 'ansi-color)
(defun gg--colorize-compilation-buffer ()
  "Remove escape codes from compilation buffer."
  (read-only-mode -1)
  (ansi-color-apply-on-region compilation-filter-start (point))
  (read-only-mode))
(add-hook 'compilation-filter-hook 'gg--colorize-compilation-buffer)
(setq compilation-scroll-output t
      next-error-message-highlight t)

(defun has-special-buffer (window)
  "Return non-nil if WINDOW contains a buffer matching `special-display-regexps'."
  (let ((name (buffer-name(window-buffer window))))
    (seq-some (lambda (regexp) (string-match-p regexp name)) special-display-regexps)))

(defun display-special-buffer (buf list-of-what)
  "put the special buffers in the right spot (top-left)"
  (let ((target-window (window-at 0 0))
        (pop-up-windows t))
    (if (has-special-buffer target-window)
        (let ((second-window (window-at 0 (- (frame-height) 10))))
          (message (buffer-name (window-buffer second-window)))
          (set-window-buffer second-window (window-buffer target-window))))
    (set-window-buffer target-window buf)
    target-window))

(setq special-display-regexps
      '("^\\*Async Shell Command\\*\\(<[0-9]+>\\)?$"
        "^\\*webpack\\*$"
        "^\\*server\\*$"
        "^\\*Completions\\*$"
        "^\\*Help\\*$"
        "^\\*grep\\*$"
        "^\\*Apropos\\*$"
        "^\\*elisp macroexpansion\\*$"
        "^\\*local variables\\*$"
        "^\\*Compile-Log\\*$"
        "^\\*Quail Completions\\*$"
        "^\\*Occur\\*$"
        "^\\*frequencies\\*$"
        "^\\*compilation\\*$"
        "^\\*Locate\\*$"
        "^\\*Colors\\*$"
        "^\\*tumme-display-image\\*$"
        "^\\*SLIME Description\\*$"
        "^\\*.* output\\*$"             ; tex compilation buffer
        "^\\*TeX Help\\*$"
        "^\\*Shell Command Output\\*$"
        "^\\*Backtrace\\*$"
        "^\\*helpful .*\\*$"
        "^\\*tide-.*\\*$"
        "^TODO$"))
(setq special-display-function 'display-special-buffer)

(global-set-key (kbd "C-x C-b") 'ibuffer)

(global-unset-key (kbd "s-t"))

(setq mode-line-compact t)

(require 'windower)
(global-set-key (kbd "<M-tab>") 'windower-switch-to-last-buffer)
(global-set-key (kbd "M-1") 'windower-toggle-single)
(global-set-key (kbd "s-|") 'windower-toggle-split)

(global-set-key (kbd "<s-M-left>") 'windower-move-border-left)
(global-set-key (kbd "<s-M-down>") 'windower-move-border-below)
(global-set-key (kbd "<s-M-up>") 'windower-move-border-above)
(global-set-key (kbd "<s-M-right>") 'windower-move-border-right)

(global-set-key (kbd "<s-S-left>") 'windower-swap-left)
(global-set-key (kbd "<s-S-down>") 'windower-swap-below)
(global-set-key (kbd "<s-S-up>") 'windower-swap-above)
(global-set-key (kbd "<s-S-right>") 'windower-swap-right)

;; epub reader on emacs!
(use-package nov
  :mode ("\\.epub\\'" . nov-mode))

;; Very useful for profiling the init script
(use-package esup
  :commands (esup)
  :pin melpa)
