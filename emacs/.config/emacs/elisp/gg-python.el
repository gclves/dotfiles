(use-package pyvenv
  :hook (python-mode . pyvenv-mode))

(defun gg/ruff-format-buffer ()
  "Format the buffer using ruff."
  (interactive)
  (when (derived-mode-p 'python-base-mode)
    (let ((p (point)))
      (call-process-region
       (point-min) (point-max)
       "ruff" t (current-buffer) nil
       "format" "--stdin-filename" (or buffer-file-name "stdin"))
      (goto-char p))))

(add-hook 'python-base-mode-hook
          (lambda ()
            (add-hook 'before-save-hook #'gg/ruff-format-buffer nil t)))

(with-eval-after-load 'eglot
  (add-to-list 'eglot-server-programs
               '((python-mode python-ts-mode) . ("pyright-langserver" "--stdio"))))

(provide 'gg-python)
