;; packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    paredit
    clojure-mode
    clojure-mode-extra-font-locking
    cider
    rainbow-delimiters
    haskell-mode
    magit
    evil
    htmlize
    org
    web-mode
    key-chord
    fiplr
    projectile
    helm
    helm-projectile
    markdown-mode
    powerline
    solarized-theme
    leuven-theme
    sql-indent
    sqlup-mode
    ;; my own time sinks
    jabber
    hackernews
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

(setq magit-auto-revert-mode nil)
(setq magit-last-seen-setup-instructions "1.4.0")
;; org-mode
(require 'org)
(setq org-agenda-include-diary t)
(setq org-todo-keywords
  '((sequence "TODO" "DOING" "WAITING" "DONE")))
(define-key global-map (kbd "C-c a") 'org-agenda)
(require 'htmlize)
(setq org-src-fontify-natively t)
;; org exports
(setq org-html-doctype-alist "html5")
(setq org-export-html-style
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />")
;; capture mode
(setq org-default-notes-file "~/org/everything.org")
(define-key global-map (kbd "C-c c") 'org-capture)
;; org-redmine
(require 'org-redmine)
(setq org-redmine-uri "http://redmine.runweb.com.br")
(setq org-redmine-auth-api-key "7c21e9193a428945be837cd589a5b8d617a53955")

;; GO TO in web browser
(define-key global-map (kbd "C-M-g") 'eww)

;; Evil
(require 'evil)
;; Enable evil, but only for prog or text buffers
;; (add-hook 'prog-mode-hook 'evil-local-mode)
(mapcar (lambda (hook)
          (add-hook hook 'evil-local-mode))
        '(prog-mode-hook text-mode-hook))

(define-key evil-insert-state-map (kbd "C-e") 'move-end-of-line)
; Move around
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
;; Remap org-mode meta keys for convenience
(mapcar (lambda (state)
          (evil-declare-key state org-mode-map
            (kbd "M-l") 'org-metaright
            (kbd "M-h") 'org-metaleft
            (kbd "M-k") 'org-metaup
            (kbd "M-j") 'org-metadown
            (kbd "M-L") 'org-shiftmetaright
            (kbd "M-H") 'org-shiftmetaleft
            (kbd "M-K") 'org-shiftmetaup
            (kbd "M-J") 'org-shiftmetadown))
        '(normal insert))
(define-key evil-normal-state-map (kbd ":") 'helm-M-x)

(require 'key-chord)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

;; view settings
(setq inhibit-splash-screen t)

(set-default-font "SourceCodePro-9")
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(load-theme 'solarized-dark t)
(add-hook 'text-mode-hook 'visual-line-mode)
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(require 'powerline)
(powerline-default-theme)

;; DocView
(setq doc-view-continuous t)

(require 'iso-transl)
(transient-mark-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Web development
(require 'web-mode)

(require 'haskell-mode)
(add-hook 'haskell-mode-hook 'turn-on-haskell-indentation)
;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)
(define-key global-map (kbd "RET") 'newline-and-indent)

;; SQL mode
(setq sql-postgres-login-params
      '((user :default "guilherme")
	(database :default "cap")
	(server :default "localhost")
	(port :default 5433)))
(add-hook 'sql-interactive-mode-hook
	  (lambda ()
	    (sqlup-mode)
	    (toggle-truncate-lines t)))
(add-hook 'sql-mode-hook 'sqlup-mode)

;; Helm
(helm-mode +1)
(setq helm-quick-update t)
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(helm-autoresize-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x C-b") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)

(projectile-global-mode)
(setq projectile-enable-caching t)
(global-set-key (kbd "C-x f") 'fiplr-find-file)

;; C-; to comment one line
(defun toggle-comment-on-line ()
  "Comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; SHELL MODE
;; Use bash as my shell
(setq explicit-shell-file-name "/bin/bash")
(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))
(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))
(global-set-key (kbd "C-!") 'eshell-here)
(global-set-key [f1] 'eshell)

(setq browse-url-text-browser "w3m")
(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)
(load "~/.emacs.d/jabber.el")

;; A saner backup policy
(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.emacs.d/backup"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)
