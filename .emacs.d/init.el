;; packages
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                        ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa-stable" . "http://melpa-stable.milkbox.net/packages/")))
(package-initialize)

(defun require-package (package)
  (setq-default highlight-tabs t)
  "Install given PACKAGE."
  (unless (package-installed-p package)
    (unless (assoc package package-archive-contents)
      (package-refresh-contents))
    (package-install package)))

;; org-mode
(require 'org)
(setq org-todo-keywords
  '((sequence "TODO" "DOING" "WAITING" "DONE")))
(add-hook 'org-mode-hook
	  (lambda () (define-key global-map (kbd "C-c a") 'org-agenda)))
(require 'htmlize)
(setq org-src-fontify-natively t)
;; org exports
(setq org-html-doctype-alist "html5")
(setq org-export-html-style
      "<link rel=\"stylesheet\" type=\"text/css\" href=\"style.css\" />")

;; Evil
(require 'evil)
(evil-mode)
; Move around
(define-key evil-normal-state-map (kbd "C-h") 'evil-window-left)
(define-key evil-normal-state-map (kbd "C-j") 'evil-window-down)
(define-key evil-normal-state-map (kbd "C-k") 'evil-window-up)
(define-key evil-normal-state-map (kbd "C-l") 'evil-window-right)
(define-key evil-normal-state-map (kbd "C-p") 'fiplr-find-file)

(require 'key-chord)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)
(key-chord-mode 1)

;; view settings
(setq inhibit-splash-screen t)
(load-theme 'solarized-dark t)
(set-face-attribute 'default nil
		    :family "Monaco"
		    :height 80
		    :weight 'normal
		    :width 'normal)

(linum-mode)
(when (window-system)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (menu-bar-mode -1))

(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(require 'iso-transl)
(transient-mark-mode 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Web development
(require 'php-mode)
(require 'web-mode)

;; Markdown
(autoload 'markdown-mode "markdown-mode"
  "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(defvaralias 'c-basic-offset 'tab-width)

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

(setq custom-file "~/.emacs.d/custom.el")
(load custom-file)

;; A saner backup policy
(setq
 backup-by-copying t
 backup-directory-alist
 '(("." . "~/.emacs.d/backup"))
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)
