((nil . ((eval . (setq-local consult-ripgrep-args
                             (if (boundp 'consult-ripgrep-args)
                                 (concat consult-ripgrep-args " --hidden")
                               "rg --null --hidden"))))))
