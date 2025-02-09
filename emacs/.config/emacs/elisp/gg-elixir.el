(use-package elixir-ts-mode
  :config
  (with-eval-after-load 'eglot
    ;; FIXME: this shouldn't be hardcoded
    (add-to-list 'eglot-server-programs
                 `(elixir-ts-mode "~/src/elixir-ls/release/language_server.sh")))
  :hook (elixir-ts-mode . eglot-ensure))

(use-package exunit
  :hook (elixir-ts-mode . exunit-mode))

(add-to-list 'auto-mode-alist '("\\.exs?\\'" . elixir-ts-mode))

(with-eval-after-load 'comint
  (defun gg-run-iex ()
    "Runs an IEx sesssion with `comint-mode' on the current project."
    (interactive)
    (let ((buffer-name "*iex*")
          (project-root (or (projectile-project-root) default-directory)))
      (if (comint-check-proc buffer-name)
          (pop-to-buffer buffer)
        (let ((default-directory project-root))
          (pop-to-buffer
           (apply 'make-comint-in-buffer "iex" nil "iex" nil '("-S" "mix"))))))))

(provide 'gg-elixir)
