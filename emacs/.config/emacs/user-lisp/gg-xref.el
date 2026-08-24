;;; gg-xref.el --- Xref helpers -*- lexical-binding: t; -*-

(require 'seq)

;;;###autoload
(defun gg/xref-find-definitions-smart (&optional identifier)
  "Jump to the definition of IDENTIFIER.  Stay in same window if defs stay in current buffer/file; otherwise use other window."
  (interactive)

  (require 'xref)

  (let* ((backend (xref-find-backend))
         (id (or identifier
                 (xref-backend-identifier-at-point backend)))
         (defs (xref-backend-definitions backend id))
         (curfile (buffer-file-name (current-buffer)))
         (curbuf  (current-buffer)))
    (unless defs
      (user-error "No definitions found for: %s" id))
    (let ((same-target
           (seq-every-p
            (lambda (xref-item)
              (let* ((loc   (xref-item-location xref-item))
                     (group (xref-location-group loc)))
                (cond
                 ((and curfile (stringp group))
                  (string-equal (expand-file-name group)
                                (expand-file-name curfile)))
                 ;; If no file, fall back to "same buffer" when possible.
                 ;; Some backends use buffer name as group.
                 ((and (null curfile) (stringp group))
                  (string-equal group (buffer-name curbuf)))
                 (t nil))))
            defs)))
      (if same-target
          (xref-find-definitions id)
        (xref-find-definitions-other-window id)))))

(provide 'gg-xref)
;;; gg-xref.el ends here
