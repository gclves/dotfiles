;;;; FIXME: This config file is completely out of hand -- split this into modules

;; Hackery needed to get lexical binding from Common Lisp
(require 'cl)
(defmacro lexical-defun (name args &rest body)
  `(defun ,name ,args
     (lexical-let ,(mapcar (lambda (arg) (list arg arg))
                           (remove-if (lambda (a) (equal a '&rest)) args))
       ,@body)))

;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(toggle-frame-fullscreen)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)
(set-face-attribute 'default (selected-frame) :height 102) ; I finally found the perfect font!

(setq custom-file "~/.emacs.d/custom.el")

(global-hl-line-mode t)
(blink-cursor-mode -1)
(setq initial-scratch-message "")

;; packages
(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "https://melpa.org/packages/")))
(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(paredit
    cider
    yasnippet
    js2-mode
    tern
    tern-auto-complete
    undo-tree
    ac-js2
    js-comint
    auto-complete
    rainbow-delimiters
    rainbow-mode
    magit
    git-gutter
    htmlize
    org
    capture
    web-mode
    ag
    projectile
    helm
    helm-projectile
    helm-ag
    powerline
    solarized-theme
    leuven-theme
    slime
    emmet-mode
    idle-highlight-mode
    multiple-cursors
    expand-region
    ace-window
    which-key
    ws-butler
    exec-path-from-shell
    zoom-window
    cycbuf
    discover))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(projectile-global-mode)
(setq projectile-enable-caching t)

;; Inherit SSH agent so Magit doesn't keep prompting us
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

(global-set-key (kbd "RET") 'newline-and-indent)
(global-undo-tree-mode)

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  ;; (delete-trailing-whitespace)          ; Removed: experimenting with ws-butler
  (set-buffer-file-coding-system 'utf-8))

;; Various superfluous white-space. Just say no.
;; (add-hook 'before-save-hook 'cleanup-buffer-safe)
(ws-butler-global-mode)

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (whitespace-cleanup)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c n") 'cleanup-buffer)

(add-hook 'prog-mode-hook 'auto-complete-mode)

(defun delete-current-buffer-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (ido-kill-buffer)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-x C-k") 'delete-current-buffer-file)

(defun rename-current-buffer-file ()
  "Renames current buffer and file it is visiting."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (if (get-buffer new-name)
            (error "A buffer named '%s' already exists!" new-name)
          (rename-file filename new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil)
          (message "File '%s' successfully renamed to '%s'"
                   name (file-name-nondirectory new-name)))))))
(global-set-key (kbd "C-x C-r") 'rename-current-buffer-file)

;; Helm
(helm-mode +1)
(setq helm-quick-update t)
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(helm-autoresize-mode 1)

(lexical-defun disabled-keybinding (old-keybinding new-keybinding)
  "Notify that a keybinding is disabled and lets me know the new binding. Used to adapt my muscle memory."
  (lambda ()
    (interactive)
    (message (format "Command %s is disabled. Use %s instead." old-keybinding new-keybinding))))

(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-b") 'helm-mini)
(global-set-key (kbd "C-x b") (disabled-keybinding "C-x b" "C-b"))
(global-set-key (kbd "C-S-w") (lambda () (interactive) (kill-buffer nil)))
(global-set-key (kbd "C-x C-f") 'helm-find-files)

;; Don't forget about cycbuf-switch-to-{next,previous}-buffer-no-timeout
(global-set-key (kbd "<C-tab>")       'cycbuf-switch-to-next-buffer)
(global-set-key (kbd "<C-iso-lefttab>")       'cycbuf-switch-to-previous-buffer)
;; (global-set-key [(meta super right)] 'cycbuf-switch-to-next-buffer-no-timeout)
;; (global-set-key [(meta super left)]  'cycbuf-switch-to-previous-buffer-no-timeout)

(define-key projectile-mode-map (kbd "C-x C-S-f") 'projectile-ag)
(define-key projectile-mode-map (kbd "C-S-f") 'helm-projectile-ag)
(define-key projectile-mode-map (kbd "C-c p p") 'helm-projectile-switch-project)
(define-key projectile-mode-map (kbd "C-\\") 'helm-projectile)

;; Quickly reach the scratch buffer
(defun jump-to-scratch ()
  "Quickly jump to the *scratch* buffer"
  (interactive)
  (switch-to-buffer-other-window "*scratch*"))
(global-set-key (kbd "M-0") 'jump-to-scratch)

(setq whitespace-style '(empty tabs trailing face))
(global-whitespace-mode)

(setq aw-keys '(?a ?o ?e ?u ?i ?d ?h ?t ?n ?s)
      aw-scope 'frame)
(global-set-key (kbd "M-o") 'other-window)
(global-set-key (kbd "M-O") 'ace-window)

(require 'zoom-window)
(global-set-key (kbd "C-x C-z") 'zoom-window-zoom)

(setq fill-column 80)
(setq-default indent-tabs-mode nil)     ; no tabs please

(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.5)

(require 'discover)
(global-discover-mode t)

;;; Editing goodies
(defun open-line-below ()
  (interactive)
  (end-of-line)
  (newline)
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(electric-pair-mode t)
(global-set-key [f7] 'call-last-kbd-macro)

(setq ac-auto-start 4)                  ; show ac candidates when I type 4 chars instead of 2

(add-hook 'prog-mode-hook 'subword-mode) ; properly navigate through camelCase

;; So I don't have to open Slack
(defun shrug ()
  "Insert ¯\\_(ツ)_/¯ at point"
  (interactive)
  (insert "¯\\_(ツ)_/¯"))
(defun lenny ()
  "Insert ( ͡° ͜ʖ ͡°) at point"
  (interactive)
  (insert "( ͡° ͜ʖ ͡°)"))

(defun back-to-indentation-or-beginning ()
  "Move point to beginning of line, or to first non-space character"
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))
(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning)

(defun copy-line (arg)
  "Copy lines (as many as prefix argument) in the kill ring.
      Ease of use features:
      - Move to start of next line.
      - Appends the copy on sequential calls.
      - Use newline as last char even on the last line of the buffer.
      - If region is active, copy its lines."
  (interactive "p")
  (let ((beg (line-beginning-position))
        (end (line-end-position arg)))
    (when mark-active
      (if (> (point) (mark))
          (setq beg (save-excursion (goto-char (mark)) (line-beginning-position)))
        (setq end (save-excursion (goto-char (mark)) (line-end-position)))))
    (if (eq last-command 'copy-line)
        (kill-append (buffer-substring beg end) (< end beg))
      (kill-ring-save beg end)))
  (kill-append "\n" nil)
  (beginning-of-line (or (and arg (1+ arg)) 2))
  (if (and arg (not (= 1 arg))) (message "%d lines copied" arg)))
(global-set-key (kbd "C-c C-y") 'copy-line)

(global-set-key (kbd "C-;") 'comment-line)

(add-to-list 'auto-mode-alist '("\\.hdl\\'" . vhdl-mode))

(pending-delete-mode t)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Replace upcase/downcase word with their dwim counterparts
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "C-x C-u") 'upcase-initials-region)
(global-set-key (kbd "C-x C-l") nil)

(global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

(defun move-line-down ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines 1))
    (forward-line)
    (move-to-column col)))

(defun move-line-up ()
  (interactive)
  (let ((col (current-column)))
    (save-excursion
      (forward-line)
      (transpose-lines -1))
    (forward-line -2)
    (move-to-column col)))

(global-set-key (kbd "<M-up>") 'move-line-up)
(global-set-key (kbd "<M-down>") 'move-line-down)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input"
  (interactive)
  (unwind-protect
      (progn
        (linum-mode 1)
        (goto-line (read-number "Goto line: ")))
    (linum-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole buffer (respects `narrow-to-region')."
  (interactive)
  (if current-prefix-arg
      (progn ; not using kill-region because we don't want to include previous kill
        (kill-new (buffer-string))
        (delete-region (point-min) (point-max)))
    (progn (if (use-region-p)
               (kill-region (region-beginning) (region-end) t)
             (kill-whole-line)))))

(defun xah-copy-line-or-region ()
  "Copy current line, or text selection.
When called repeatedly, append copy subsequent lines.
When `universal-argument' is called first, copy whole buffer (respects `narrow-to-region')."
  (interactive)
  (let (-p1 -p2)
    (if current-prefix-arg
        (setq -p1 (point-min) -p2 (point-max))
      (if (use-region-p)
          (setq -p1 (region-beginning) -p2 (region-end))
        (setq -p1 (line-beginning-position) -p2 (line-end-position))))
    (if (eq last-command this-command)
        (progn
          (progn ; hack. exit if there's no more next line
            (end-of-line)
            (forward-char)
            (backward-char))
          (push-mark (point) "NOMSG" "ACTIVATE")
          (kill-append "\n" nil)
          (kill-append (buffer-substring-no-properties (line-beginning-position) (line-end-position)) nil)
          (message "Line copy appended"))
      (progn
        (kill-ring-save -p1 -p2)
        (if current-prefix-arg
            (message "Buffer text copied")
          (message "Text copied"))))
    (end-of-line)
    (forward-char)))

(global-set-key (kbd "C-w") 'xah-cut-line-or-region)
(global-set-key (kbd "M-w") 'xah-copy-line-or-region)

(defun xah-toggle-letter-case ()
  "Toggle the letter case of current word or text selection.
Always cycle in this order: Init Caps, ALL CAPS, all lower.

URL `http://ergoemacs.org/emacs/modernization_upcase-word.html'
Version 2016-01-08"
  (interactive)
  (let ((deactivate-mark nil)
        -p1 -p2)
    (if (use-region-p)
        (setq -p1 (region-beginning)
              -p2 (region-end))
      (save-excursion
        (skip-chars-backward "[:alnum:]")
        (setq -p1 (point))
        (skip-chars-forward "[:alnum:]")
        (setq -p2 (point))))
    (when (not (eq last-command this-command))
      (put this-command 'state 0))
    (cond
     ((equal 0 (get this-command 'state))
      (upcase-initials-region -p1 -p2)
      (put this-command 'state 1))
     ((equal 1  (get this-command 'state))
      (upcase-region -p1 -p2)
      (put this-command 'state 2))
     ((equal 2 (get this-command 'state))
      (downcase-region -p1 -p2)
      (put this-command 'state 0)))))
(global-set-key (kbd "M-c") 'xah-toggle-letter-case)

(defun quote-previous-word ()
  "Wrap the previous word in quotes"
  (interactive)
  (let ((deactivate-mark nil)
        -p1 -p2)
    (if (use-region-p)
        (progn
          (setq -p1 (region-beginning)
                -p2 (region-end))
          (message "Not implemented :("))
      (progn
        (save-excursion
          (skip-chars-backward "[:alnum:]")
          (insert "'")
          (skip-chars-forward "[:alnum:]")
          (insert "'"))
        (forward-char)))))
(global-set-key (kbd "M-'") 'quote-previous-word)

(define-key undo-tree-map (kbd "C-z") 'undo-tree-undo)
(define-key undo-tree-map (kbd "C-S-z") 'undo-tree-redo)
(define-key undo-tree-map (kbd "C-/") (disabled-keybinding "C-/" "C-z"))
(define-key undo-tree-map (kbd "M-_") (disabled-keybinding "M-_" "C-S-Z"))


;;; Specific modes
;; JS2 Mode
(require 'js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . js2-mode))
(setq js2-basic-offset 2)
(setq js2-strict-trailing-comma-warning nil)
(define-key js2-mode-map (kbd "M-j") nil)
(define-key js2-mode-map (kbd "C-c C-c") 'js-send-region)
(add-hook 'js2-mode-hook 'tern-mode)

(require 'tern)
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)))

;; Web mode
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'" . web-mode))
(setq web-mode-enable-css-colorization t
      web-mode-enable-current-element-highlight t
      web-mode-markup-indent-offset 2
      web-mode-css-indent-offset 2
      web-mode-code-indent-offset 2
      css-indent-offset 2
      emmet-indentation 2
      js-indent-level 2
      web-mode-auto-close-style 1
      web-mode-code-indent-offset 4
      web-mode-enable-auto-indentation t
      web-mode-enable-auto-opening t
      web-mode-enable-auto-pairing t
      web-mode-enable-auto-quoting t)

(define-key web-mode-map (kbd "C-M-u") 'web-mode-element-parent)
(define-key web-mode-map (kbd "C-M-d") 'web-mode-element-child)
(define-key web-mode-map (kbd "C-M-n") 'web-mode-element-next)
(define-key web-mode-map (kbd "C-M-p") 'web-mode-element-previous)

(add-hook 'web-mode-hook 'emmet-mode)

;; We already bound C-return to something else and can expand with C-j
(eval-after-load 'emmet-mode
  '(progn (define-key emmet-mode-keymap (kbd "<C-return>") nil)))

(defun php-tpl-localize (p1 p2)
  "Wrap region in a PHP call to xgettext"
  (interactive "r")
  (save-mark-and-excursion
   (goto-char p1)
   (insert "<?= _('")
   (setq begin (point))
   (forward-char (- p2 p1))
   (setq end (point))
   (insert "') ?>")
   (replace-string "'" "\\'" nil begin end)
   (message "Localized region")))

;; Org-mode
(setq org-agenda-include-diary t)
(setq org-todo-keywords
      '((sequence "TODO" "DOING" "WAITING" "SOMEDAY" "DONE")))
(global-set-key (kbd "C-c a") 'org-agenda)

;; Archive DONE items
(defun org-archive-done-tasks ()
  (interactive)
  (org-map-entries (lambda ()
                     (org-archive-subtree)
                     (setq org-map-continue-from (outline-previous-heading)))
                   "/DONE" 'tree))
(require 'org)
(define-key org-mode-map (kbd "C-c s") 'org-sort)
(define-key org-mode-map (kbd "C-S-SPC") 'org-archive-done-tasks)
(define-key org-mode-map (kbd "C-c <C-dead-tilde>") 'org-up-element)

;; org exports
(setq org-html-doctype-alist "html5")
;; capture mode
(setq org-default-notes-file "~/everything.org")
(define-key global-map (kbd "C-c c") 'org-capture)

(setq org-src-fontify-natively t)       ; Syntax highlighting in code blocks

;; YASnippets
(require 'yasnippet)
(yas/reload-all)
(add-hook 'prog-mode-hook 'yas/minor-mode)

;; Eshell
(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

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

(global-set-key (kbd "C-!") 'eshell-here)

;; Magit
(global-set-key (kbd "<f8>") 'magit-status)

(global-git-gutter-mode t)              ; Highlight changes in the gutter
(mapc (lambda (pair)
        (set-face-background (car pair) (cdr pair))
        (set-face-foreground (car pair) (cdr pair)))
      '((git-gutter:added . "#8bc34a")
        (git-gutter:modified . "#b39ddb")
        (git-gutter:deleted . "#f36c60")))

(global-set-key (kbd "C-x C-p") 'git-gutter:previous-hunk)
(global-set-key (kbd "C-x C-n") 'git-gutter:next-hunk)
(setq git-gutter:update-interval 2)     ; This may not be working?

(defalias 'yes-or-no-p 'y-or-n-p)       ; Ain't nobody got time for yes/no
(setq x-selection-timeout 300)          ; Make Emacs freeze LESS when
                                        ; pasting from X

;; Override some defaults
(setq epg-gpg-program "/usr/bin/gpg2")  ; Use gpg2 instead of (default) gpg

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Make tooltips appear in the echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)
(setq redisplay-dont-pause t)           ; smoother drawing (maybe remove this?)

;; specify font for all unicode characters
(when (member "Symbola" (font-family-list))
  (set-fontset-font t 'unicode "Symbola" nil 'prepend))

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(global-set-key (kbd "<f1>") 'eshell)
;; (add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(load custom-file)

;;; Fira code font
;; This works when using emacs --daemon + emacsclient
(add-hook 'after-make-frame-functions (lambda (frame) (set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")))
;; This works when using emacs without server/client
(set-fontset-font t '(#Xe100 . #Xe16f) "Fira Code Symbol")
;; I haven't found one statement that makes both of the above situations work, so I use both for now

(defconst fira-code-font-lock-keywords-alist
  (mapcar (lambda (regex-char-pair)
            `(,(car regex-char-pair)
              (0 (prog1 ()
                   (compose-region (match-beginning 1)
                                   (match-end 1)
                                   ;; The first argument to concat is a string containing a literal tab
                                   ,(concat "	" (list (decode-char 'ucs (cadr regex-char-pair)))))))))
          '(("\\(www\\)"                   #Xe100)
            ("[^/]\\(\\*\\*\\)[^/]"        #Xe101)
            ("\\(\\*\\*\\*\\)"             #Xe102)
            ("\\(\\*\\*/\\)"               #Xe103)
            ("\\(\\*>\\)"                  #Xe104)
            ("[^*]\\(\\*/\\)"              #Xe105)
            ("\\(\\\\\\\\\\)"              #Xe106)
            ("\\(\\\\\\\\\\\\\\)"          #Xe107)
            ("\\({-\\)"                    #Xe108)
            ("\\(\\[\\]\\)"                #Xe109)
            ("\\(::\\)"                    #Xe10a)
            ("\\(:::\\)"                   #Xe10b)
            ("[^=]\\(:=\\)"                #Xe10c)
            ("\\(!!\\)"                    #Xe10d)
            ("\\(!=\\)"                    #Xe10e)
            ("\\(!==\\)"                   #Xe10f)
            ("\\(-}\\)"                    #Xe110)
            ("\\(--\\)"                    #Xe111)
            ("\\(---\\)"                   #Xe112)
            ("\\(-->\\)"                   #Xe113)
            ("[^-]\\(->\\)"                #Xe114)
            ("\\(->>\\)"                   #Xe115)
            ("\\(-<\\)"                    #Xe116)
            ("\\(-<<\\)"                   #Xe117)
            ("\\(-~\\)"                    #Xe118)
            ("\\(#{\\)"                    #Xe119)
            ("\\(#\\[\\)"                  #Xe11a)
            ("\\(##\\)"                    #Xe11b)
            ("\\(###\\)"                   #Xe11c)
            ("\\(####\\)"                  #Xe11d)
            ("\\(#(\\)"                    #Xe11e)
            ("\\(#\\?\\)"                  #Xe11f)
            ("\\(#_\\)"                    #Xe120)
            ("\\(#_(\\)"                   #Xe121)
            ("\\(\\.-\\)"                  #Xe122)
            ("\\(\\.=\\)"                  #Xe123)
            ("\\(\\.\\.\\)"                #Xe124)
            ("\\(\\.\\.<\\)"               #Xe125)
            ("\\(\\.\\.\\.\\)"             #Xe126)
            ("\\(\\?=\\)"                  #Xe127)
            ("\\(\\?\\?\\)"                #Xe128)
            ("\\(;;\\)"                    #Xe129)
            ("\\(/\\*\\)"                  #Xe12a)
            ("\\(/\\*\\*\\)"               #Xe12b)
            ("\\(/=\\)"                    #Xe12c)
            ("\\(/==\\)"                   #Xe12d)
            ("\\(/>\\)"                    #Xe12e)
            ("\\(//\\)"                    #Xe12f)
            ("\\(///\\)"                   #Xe130)
            ;; ("\\(&&\\)"                    #Xe131)
            ;; ("\\(||\\)"                    #Xe132)
            ("\\(||=\\)"                   #Xe133)
            ("[^|]\\(|=\\)"                #Xe134)
            ("\\(|>\\)"                    #Xe135)
            ("\\(\\^=\\)"                  #Xe136)
            ("\\(\\$>\\)"                  #Xe137)
            ("\\(\\+\\+\\)"                #Xe138)
            ("\\(\\+\\+\\+\\)"             #Xe139)
            ("\\(\\+>\\)"                  #Xe13a)
            ("\\(=:=\\)"                   #Xe13b)
            ("[^!/]\\(==\\)[^>]"           #Xe13c)
            ("\\(===\\)"                   #Xe13d)
            ("\\(==>\\)"                   #Xe13e)
            ;; ("[^=]\\(=>\\)"                #Xe13f)
            ("\\(=>>\\)"                   #Xe140)
            ("\\(<=\\)"                    #Xe141)
            ("\\(=<<\\)"                   #Xe142)
            ("\\(=/=\\)"                   #Xe143)
            ("\\(>-\\)"                    #Xe144)
            ("\\(>=\\)"                    #Xe145)
            ("\\(>=>\\)"                   #Xe146)
            ("[^-=]\\(>>\\)"               #Xe147)
            ("\\(>>-\\)"                   #Xe148)
            ("\\(>>=\\)"                   #Xe149)
            ("\\(>>>\\)"                   #Xe14a)
            ("\\(<\\*\\)"                  #Xe14b)
            ("\\(<\\*>\\)"                 #Xe14c)
            ("\\(<|\\)"                    #Xe14d)
            ("\\(<|>\\)"                   #Xe14e)
            ("\\(<\\$\\)"                  #Xe14f)
            ("\\(<\\$>\\)"                 #Xe150)
            ("\\(<!--\\)"                  #Xe151)
            ("\\(<-\\)"                    #Xe152)
            ("\\(<--\\)"                   #Xe153)
            ("\\(<->\\)"                   #Xe154)
            ("\\(<\\+\\)"                  #Xe155)
            ("\\(<\\+>\\)"                 #Xe156)
            ("\\(<=\\)"                    #Xe157)
            ("\\(<==\\)"                   #Xe158)
            ("\\(<=>\\)"                   #Xe159)
            ("\\(<=<\\)"                   #Xe15a)
            ("\\(<>\\)"                    #Xe15b)
            ("[^-=]\\(<<\\)"               #Xe15c)
            ("\\(<<-\\)"                   #Xe15d)
            ("\\(<<=\\)"                   #Xe15e)
            ("\\(<<<\\)"                   #Xe15f)
            ("\\(<~\\)"                    #Xe160)
            ("\\(<~~\\)"                   #Xe161)
            ("\\(</\\)"                    #Xe162)
            ("\\(</>\\)"                   #Xe163)
            ("\\(~@\\)"                    #Xe164)
            ("\\(~-\\)"                    #Xe165)
            ("\\(~=\\)"                    #Xe166)
            ("\\(~>\\)"                    #Xe167)
            ("[^<]\\(~~\\)"                #Xe168)
            ("\\(~~>\\)"                   #Xe169)
            ("\\(%%\\)"                    #Xe16a)
            ;;("\\(x\\)"                     #Xe16b)
            ("[^:=]\\(:\\)[^:=]"           #Xe16c)
            ("[^\\+<>]\\(\\+\\)[^\\+<>]"   #Xe16d)
            ("[^\\*/<>]\\(\\*\\)[^\\*/<>]" #Xe16f))))

(defun add-fira-code-symbol-keywords ()
  (font-lock-add-keywords nil fira-code-font-lock-keywords-alist))

(add-hook 'prog-mode-hook
          #'add-fira-code-symbol-keywords)

;; More mathy symbols
(global-prettify-symbols-mode 1)

(add-hook
 'python-mode-hook
 (lambda ()
   (mapc (lambda (pair) (push pair prettify-symbols-alist))
         '(;; Syntax
           ("def" .      #x2131)
           ("not" .      #x2757)
           ("in" .       #x2208)
           ("not in" .   #x2209)
           ("return" .   #x27fc)
           ("yield" .    #x27fb)
           ("for" .      #x2200)
           ;; Base Types
           ("int" .      #x2124)
           ("float" .    #x211d)
           ("str" .      #x1d54a)
           ("True" .     #x1d54b)
           ("False" .    #x1d53d)
           ;; Mypy
           ("Dict" .     #x1d507)
           ("List" .     #x2112)
           ("Tuple" .    #x2a02)
           ("Set" .      #x2126)
           ("Iterable" . #x1d50a)
           ("Any" .      #x2754)
           ("Union" .    #x22c3)))))

(add-hook
 'js2-mode-hook
 (lambda ()
   (mapc (lambda (pair) (push pair prettify-symbols-alist))
         '(;; Syntax
           ("!" .      #x2757)
           ("in" .       #x2208)
           ("for" .      #x2200)
           ("=>" . #x27fc)
           ("&&" . #x2227)
           ("||" . #x2228)
           ;; Base Types
           ("Number" .    #x211d)
           ("String" .      #x1d54a)
           ;; Mypy
           ("Array" .     #x2112)))))
