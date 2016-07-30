;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

(toggle-frame-fullscreen)

;; No splash screen please ... jeez
(setq inhibit-startup-message t)
(set-face-attribute 'default (selected-frame) :height 103) ; I finally found the perfect font!

(setq custom-file "~/.emacs.d/custom.el")

(global-hl-line-mode t)

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
    ac-js2
    js-comint
    auto-complete
    rainbow-delimiters
    rainbow-mode
    magit
    htmlize
    org
    capture
    web-mode
    ag
    projectile
    helm
    helm-projectile
    powerline
    solarized-theme
    leuven-theme
    slime
    emmet-mode
    idle-highlight-mode
    multiple-cursors
    expand-region
    ace-window
    key-chord
    which-key
    hackernews))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(projectile-global-mode)
(setq projectile-enable-caching t)

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

;; Helm
(helm-mode +1)
(setq helm-quick-update t)
(setq helm-M-x-fuzzy-match t)
(setq helm-buffers-fuzzy-matching t)
(helm-autoresize-mode 1)
(global-set-key (kbd "M-x") 'helm-M-x)
(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "<C-tab>") 'helm-mini)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(define-key projectile-mode-map (kbd "C-c p s s") 'helm-projectile-ag)
(define-key projectile-mode-map (kbd "C-c p p") 'helm-projectile-switch-project)

(key-chord-mode t)

(setq whitespace-style '(empty tabs trailing face))
(global-whitespace-mode)

(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l ?ç)
      aw-scope 'frame)
(global-set-key (kbd "M-o") 'ace-window)

(setq fill-column 80)
(setq-default indent-tabs-mode nil)     ; no tabs please

(require 'which-key)
(which-key-mode)
(setq which-key-idle-delay 0.5)

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

(defun back-to-indentation-or-beginning ()
  "Move point to beginning of line, or to first non-space character"
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

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

;; C-; to comment one line
(defun toggle-comment-on-line ()
  "Comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

(global-set-key (kbd "M-9") 'kill-whole-line)
(pending-delete-mode t)
(global-set-key (kbd "C-=") 'er/expand-region)

;; Replace upcase/downcase word with their dwim counterparts
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "C-x C-u") 'upcase-initials-region)
(global-set-key (kbd "C-x C-l") nil)

(global-set-key (kbd "s-j") 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-:") 'avy-goto-char)
(key-chord-define-global "jk" 'avy-goto-word-or-subword-1)
(global-set-key (kbd "C-'") 'avy-pop-mark)

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
(define-key web-mode-map (kbd "C-c <C-dead-tilde>") 'web-mode-element-parent)
(add-hook 'web-mode-hook 'emmet-mode)

; We already bound C-return to something else and can expand with C-j
(eval-after-load 'emmet-mode
  '(progn (define-key emmet-mode-keymap (kbd "<C-return>") nil)))

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
(setq org-default-notes-file "~/org/everything.org")
(define-key global-map (kbd "C-c c") 'org-capture)

(defun org-dblock-write:rangereport (params)
  "Display day-by-day time reports."
  (let* ((ts (plist-get params :tstart))
         (te (plist-get params :tend))
         (start (time-to-seconds
                 (apply 'encode-time (org-parse-time-string ts))))
         (end (time-to-seconds
               (apply 'encode-time (org-parse-time-string te))))
         day-numbers)
    (setq params (plist-put params :tstart nil))
    (setq params (plist-put params :end nil))
    (while (<= start end)
      (save-excursion
        (insert "\n\n"
                (format-time-string (car org-time-stamp-formats)
                                    (seconds-to-time start))
                "----------------\n")
        (org-dblock-write:clocktable
         (plist-put
          (plist-put
           params
           :tstart
           (format-time-string (car org-time-stamp-formats)
                               (seconds-to-time start)))
          :tend
          (format-time-string (car org-time-stamp-formats)
                              (seconds-to-time end))))
        (setq start (+ 86400 start))))))

;; YASnippets
(require 'yasnippet)
(yas/reload-all)
(add-hook 'prog-mode-hook 'yas/minor-mode)

;; Magit
(global-set-key (kbd "C-x g") 'magit-status)

(defalias 'yes-or-no-p 'y-or-n-p)
(setq x-selection-timeout 300)          ; Make Emacs freeze LESS when
                                        ; pasting from X
;; Use gpg2 instead of gpg
(setq epg-gpg-program "/usr/bin/gpg2")

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Make tooltips appear in the echo area
(tooltip-mode -1)
(setq tooltip-use-echo-area t)
(setq redisplay-dont-pause t)           ; smoother drawing (maybe remove this?)

;; Write backup files to own directory
(setq backup-directory-alist
      `(("." . ,(expand-file-name
                 (concat user-emacs-directory "backups")))))

;; Make backups of files, even when they're in version control
(setq vc-make-backup-files t)

(global-set-key (kbd "<f1>") 'eshell)
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)


(defun watch-for-changes ()
  "Asynchronously run watch.sh in the project root directory"
  (projectile-with-default-dir (projectile-project-root)
    (async-shell-command "./watch.sh")))

(setq-default grunt-watch-cmd
              "/usr/bin/docker run -it --rm -v /etc/localtime:/etc/localtime:ro -v $(pwd):/app:Z lastline")
(defun project-grunt-watch ()
  "Run `grunt watch` in the project root"
  (projectile-with-default-dir (projectile-project-root)
    (start-process "grunt" "grunt" grunt-watch-cmd)))

;; (load custom-file)
(load-theme 'gruvbox t)
