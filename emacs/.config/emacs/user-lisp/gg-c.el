(defun gg--c-ts-indent-style ()
  "Use BSD indentation, but indent call arguments simply."
  `(((node-is ")") parent-bol 0)
    ((parent-is "argument_list")
     parent-bol
     c-ts-mode-indent-offset)
    ,@(alist-get 'bsd
                 (c-ts-mode--indent-styles
                  (if (derived-mode-p 'c++-ts-mode)
                      'cpp
                    'c)))))

(use-package c-ts-mode
  :config
  (setq c-ts-mode-indent-offset 4
        c-ts-mode-indent-style #'gg--c-ts-indent-style)

  (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
  (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
  (add-to-list 'major-mode-remap-alist
               '(c-or-c++-mode . c-or-c++-ts-mode)))

(provide 'gg-c)
