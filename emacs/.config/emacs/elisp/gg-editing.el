(use-package undo-tree
  :bind
  (("C-z" . undo-tree-undo)
   ("C-S-z" . undo-tree-redo)
   ("s-z" . undo-tree-undo)
   ("s-S-z" . undo-tree-redo)
   ("s-Z" . undo-tree-redo)
   ("C-x u" . undo-tree-visualize))
  :config
  (setq undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t
        undo-tree-history-directory-alist `(("." . ,(concat user-emacs-directory "undo-tree"))))
  (global-undo-tree-mode))

(defun back-to-indentation-or-beginning ()
  "Move point to beginning of line, or to first non-space character."
  (interactive)
  (if (= (point) (progn (back-to-indentation) (point)))
      (beginning-of-line)))

(global-set-key (kbd "<home>") 'back-to-indentation-or-beginning)
(global-set-key (kbd "<end>") 'move-end-of-line)
(global-set-key (kbd "C-a") 'back-to-indentation-or-beginning)

(defun goto-line-with-feedback ()
  "Show line numbers temporarily, while prompting for the line number input."
  (interactive)
  (unwind-protect
      (progn
        (display-line-numbers-mode 1)
        (goto-line (read-number "Goto line: ")))
    (display-line-numbers-mode -1)))
(global-set-key [remap goto-line] 'goto-line-with-feedback)

(global-set-key (kbd "M-<up>") 'backward-paragraph)
(global-set-key (kbd "M-<down>") 'forward-paragraph)

(use-package ws-butler
  :config (ws-butler-global-mode))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content.
For a more agressive cleanup that also does indentation, use
`cleanup-buffer'."
  (interactive)
  (untabify (point-min) (point-max))
  (set-buffer-file-coding-system 'utf-8))

(defun cleanup-buffer ()
  "Perform a bunch of operations on the whitespace content of a buffer.
Including indent-buffer, which should not be called automatically on save."
  (interactive)
  (whitespace-cleanup)
  (cleanup-buffer-safe)
  (indent-region (point-min) (point-max)))

(global-set-key (kbd "C-c N") 'cleanup-buffer)
(global-set-key (kbd "C-c n") 'cleanup-buffer-safe)

(global-set-key (kbd "RET") 'newline-and-indent)
(defun xah-cut-line-or-region ()
  "Cut current line, or text selection.
When `universal-argument' is called first, cut whole
buffer (respects `narrow-to-region')."
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
When called repeatedly, append copy subsequent lines.  When
`universal-argument' is called first, copy whole buffer (respects
`narrow-to-region')."
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

(global-set-key (kbd "C-x C-k") 'xah-cut-line-or-region)
(global-set-key (kbd "s-x") 'xah-cut-line-or-region)
(global-set-key (kbd "M-w") 'xah-copy-line-or-region)
(global-set-key (kbd "s-c") 'xah-copy-line-or-region)

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

(global-set-key (kbd "<S-return>") 'open-line-below)
(global-set-key (kbd "<C-S-return>") 'open-line-above)
(global-set-key (kbd "M-j") (lambda () (interactive) (join-line -1)))
(global-set-key [f7] 'call-last-kbd-macro)


(use-package expand-region
  :config (pending-delete-mode t)
  :bind
  (("C-=" . er/expand-region)))

(use-package multiple-cursors
  :bind
  (("C-S-l" . mc/edit-lines)
   ("C->" . mc/mark-next-like-this)
   ("C-<" . mc/mark-previous-like-this)))

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

(global-set-key (kbd "<M-S-up>") 'move-line-up)
(global-set-key (kbd "<M-S-down>") 'move-line-down)

;; Replace upcase/downcase word with their dwim counterparts
(global-set-key (kbd "M-u") 'upcase-dwim)
(global-set-key (kbd "M-l") 'downcase-dwim)
(global-set-key (kbd "C-x C-u") 'upcase-initials-region)
(global-set-key (kbd "C-x C-l") nil)

(global-set-key (kbd "M-s s") 'imenu)

(defun arrayify (start end quote)
  "Turn strings on newlines into a QUOTEd, comma-separated one-liner."
  (interactive "r\nMQuote: ")
  (let ((insertion
         (mapconcat
          (lambda (x) (format "%s%s%s" quote x quote))
          (split-string (buffer-substring start end)) ", ")))
    (delete-region start end)
    (insert insertion)))

;;; Super important!!
(defun shrug ()
  "Insert ¯\\_(ツ)_/¯ at point"
  (interactive)
  (insert "¯\\_(ツ)_/¯"))
(defun lenny ()
  "Insert ( ͡° ͜ʖ ͡°) at point"
  (interactive)
  (insert "( ͡° ͜ʖ ͡°)"))

(use-package visual-regexp
  :bind
  (("C-M-%" . vr/replace)
   ("M-%" . vr/query-replace)))

(defun xah-save-all-unsaved ()
  "Save all unsaved files. no ask.
Version 2019-11-05"
  (interactive)
  (save-some-buffers t ))

;; when switching out of emacs, all unsaved files will be saved
;; TODO: replace this with after-focus-change-function
(add-hook 'focus-out-hook 'xah-save-all-unsaved)

(use-package delsel
  :hook (after-init . delete-selection-mode))

(provide 'gg-editing)
