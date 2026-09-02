(require 'use-package)

(use-package emmet-mode
  :hook ((web-mode sgml-mode css-mode) . emmet-mode)
  :config
  (setq emmet-self-closing-tag-style ""
        emmet-indentation 2)
  (define-key emmet-mode-keymap (kbd "<C-return>") nil))

(with-eval-after-load 'eglot
  (defun gg/eglot-ts-add-missing-imports ()
    "Ask the TypeScript language server to add missing imports."
    (interactive)
    (eglot-code-actions nil nil "source.addMissingImports.ts" t))

  (defun gg/eglot-ts-fix-imports ()
    "Add missing TypeScript imports, then organize imports."
    (interactive)
    (when (eglot-managed-p)
      (ignore-errors
        (gg/eglot-ts-add-missing-imports))
      (ignore-errors
        (eglot-code-action-organize-imports))))

  (add-to-list 'eglot-server-programs
               '((typescript-ts-mode tsx-ts-mode js-ts-mode js-mode)
                 . ("typescript-language-server" "--stdio"))))

(use-package prettier-js
  :init
  (defun maybe-use-prettier ()
    "Enable `prettier-js-mode' if an rc file is located."
    (if (locate-dominating-file default-directory ".prettierrc")
        (prettier-js-mode +1)))

  (add-hook 'js-ts-mode-hook 'maybe-use-prettier)
  (add-hook 'js-mode-hook 'maybe-use-prettier)
  (add-hook 'typescript-ts-mode-hook 'maybe-use-prettier)

  :config
  (setq prettier-js-use-modules-bin t))

(defun gg--base64url-decode-string (string)
  (base64-decode-string
   (concat (replace-regexp-in-string
            "_" "/"
            (replace-regexp-in-string "-" "+" string))
           (make-string (mod (- 4 (mod (length string) 4)) 4) ?=))))

(defun gg/time-remaining-in-jwt (token)
  "Return the amount of time until TOKEN expires, or NIL if exp is past."
  (let ((exp
         (gethash "exp" (json-parse-string (gg--base64url-decode-string
                                            (nth 1 (split-string token "\\."))))))
        (now (current-time)))
    (unless (time-less-p exp now)
        (format-seconds "%dd %hh %mm"
                        (float-time
                         (time-subtract exp now))))))

(defun jwt-time-remaining (token)
  "Display how long remains until TOKEN expires.

Interactively, use the active region as TOKEN, or prompt for it if
there is no active region."
  (interactive
   (list
    (if (use-region-p)
        (buffer-substring-no-properties
         (region-beginning)
         (region-end))
      (read-passwd "JWT: "))))
  (if-let* ((remaining (gg/time-remaining-in-jwt token)))
      (message "JWT expires in %s" remaining)
    (message "JWT has expired")))


(provide 'gg-web-dev)
