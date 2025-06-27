(use-package elixir-ts-mode
  :config
  (with-eval-after-load 'eglot
    ;; FIXME: this shouldn't be hardcoded
    (add-to-list 'eglot-server-programs
                 `(elixir-ts-mode "nextls")))
  :hook (elixir-ts-mode . eglot-ensure))

(use-package exunit
  :hook (elixir-ts-mode . exunit-mode))

(add-to-list 'auto-mode-alist '("\\.exs?\\'" . elixir-ts-mode))

(defun gg--iex-buffer-for-project (project-root args)
  "Return the iex buffer for the project `PROJECT-ROOT', creating one with `ARGS' if it does not exist."
  ;; FIXME: this does not support multiple project yet (bc. *iex* is hardcoded)
  (let ((buffer-name "*iex*"))
    (if (comint-check-proc buffer-name)
        buffer-name
      (let ((default-directory project-root))
        (apply 'make-comint-in-buffer "iex" nil "iex" nil args)))))

(with-eval-after-load 'comint
  (defun gg-run-iex ()
    "Runs an IEx sesssion with `comint-mode' on the current project."
    (interactive)
    ;; FIXME: breaks if projectile is not loaded
    (let* ((project-root (or (projectile-project-root) default-directory))
           (args-str (read-string "Run iex with arguments: "))
           (args (if (string= args-str "") nil (split-string args-str)))
           (buffer (gg--iex-buffer-for-project project-root args)))
      (pop-to-buffer buffer))))

(provide 'gg-elixir)
