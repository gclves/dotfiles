(require 'use-package)

(use-package emmet-mode
  :config
  (progn
    (add-hook 'web-mode-hook 'emmet-mode)
    (add-hook 'sgml-mode-hook 'emmet-mode)
    (add-hook 'css-mode-hook emmet-mode)
    (setq emmet-self-closing-tag-style ""
          emmet-indentation 2
          css-mode-indent-offset 2))
  (define-key emmet-mode-keymap (kbd "<C-return>") nil))

(use-package web-mode
  :mode ("\\.html\\'" "\\.html\\.erb\\'" "\\.tsx\\'" "\\.jsx\\'" "\\.php\\'" "\\.css\\'" "\\.tpl\\'" "\\.less\\'")
  :bind
  (:map web-mode-map
        ("C-M-u" . web-mode-element-parent)
        ("C-M-d" . web-mode-element-child)
        ("C-M-n" . web-mode-element-next)
        ("C-M-p" . web-mode-element-previous))
  :config
  (setq-default web-mode-css-indent-offset 2
                web-mode-code-indent-offset 2
                web-mode-markup-indent-offset 2
                web-mode-attr-indent-offset nil
                css-indent-offset 2)
  ;; the docs say these have to be defined in a hook
  (add-hook 'web-mode-hook (lambda ()
                             (setq web-mode-enable-css-colorization t
                                   web-mode-enable-current-element-highlight t
                                   web-mode-code-indent-offset 2
                                   js-indent-level 2
                                   web-mode-auto-close-style 1
                                   web-mode-enable-auto-indentation t
                                   web-mode-enable-auto-opening t
                                   web-mode-enable-auto-pairing t
                                   web-mode-enable-auto-quoting t
                                   web-mode-attr-indent-offset nil
                                   web-mode-attr-indent-offset nil)

                             (when (string-equal "tsx" (file-name-extension buffer-file-name))
                               (setup-tide-mode))

                             (when (string-equal "jsx" (file-name-extension buffer-file-name))
                               (js2-mode +1))))
  (flycheck-add-mode 'typescript-tslint 'web-mode)
  (flycheck-add-mode 'typescript-tslint 'tide-mode)
  (setq web-mode-ac-sources-alist
        '(("css" . (ac-source-css-property))
          ("html" . (ac-source-words-in-buffer ac-source-abbrev)))))

(use-package typescript-mode
  :mode "\\.js\\'"
  :config
  (setq typescript-indent-level 2))

(use-package tide
  :bind
  (:map tide-mode-map
        ("C-c r" . tide-rename-symbol)
        ("C-c C-r" . tide-rename-file)
        ("M-?" . tide-references))
  :config
  (progn
    (setq tide-completion-detailed t
          tide-completion-enable-autoimport-suggestions t
          tide-always-show-documentation t)
    (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append))
  :init
  (progn
    (defun setup-tide-mode ()
      (tide-setup)
      (eldoc-mode +1)
      (tide-hl-identifier-mode +1)
      (setq-local company-backend 'tide-company))

    (add-hook 'typescript-mode-hook 'setup-tide-mode)
    (add-hook 'js2-mode-hook 'setup-tide-mode)))

(defun gg-node-repl ()
  "Start a NodeJS REPL in comint mode."
  (interactive)
  (setenv "NODE_NO_READLINE" "1")       ; avoid fancy terminal codes
  (pop-to-buffer (make-comint "node-repl" "node" nil "--interactive")))

(with-eval-after-load 'js
  (setq js-indent-level 2))

(provide 'gg-web-dev)
