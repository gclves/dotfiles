(defvar gg--scratch-buffer "*scratch*"
  "The buffer currently marked as scratch.  Used by `gg-quick-switch-to-scratch'.")

(defvar gg--last-visited-buffer nil
  "The last visited buffer before jumping to *scratch*.
Used by `gg-quick-switch-to-scratch'.")

(defun gg-quick-switch-to-scratch ()
  "Quickly jump to the *scratch* buffer and back."
  (interactive)
  (let ((buf (current-buffer)))
    (if (string= (buffer-name buf) gg--scratch-buffer)
        (when gg--last-visited-buffer
            (progn
              (switch-to-buffer gg--last-visited-buffer)
              (setq gg--last-visited-buffer nil)))
      (progn
        (switch-to-buffer gg--scratch-buffer)
        (setq gg--last-visited-buffer buf)))))

(defun gg-mark-buffer-as-scratch ()
  "Mark the currently visited buffer as the scratch one."
  (interactive)
  (setq gg--scratch-buffer (buffer-name (current-buffer))))

(global-set-key (kbd "M-_") 'gg-mark-buffer-as-scratch)
(global-set-key (kbd "M--") 'gg-quick-switch-to-scratch)

(defvar gg-scratch-buffer-mode 'lisp-interaction-mode
  "Major mode to be used in temporary buffers.")

(defun make-new-buffer-or-frame (arg)
  "Create a new buffer.
If ARG is non-nil, the new buffer is placed in a new frame."
  (interactive "P")
  (let ((make-frame? (and arg t)))
    (if make-frame? (make-frame-command)
      (progn
        (switch-to-buffer (generate-new-buffer "*New*"))
        (funcall gg-scratch-buffer-mode)))))

(global-set-key (kbd "s-n") 'make-new-buffer-or-frame)


(provide 'gg-scratch)
