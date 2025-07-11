;; use emacs itself for GnuPG pw entry
;; TODO: move this elsewhere
(setq epa-pinentry-mode 'loopback
      epg-pinentry-mode 'loopback)

(use-package chatgpt-shell
  :bind (("C-x l s" . chatgpt-shell)
         ("C-x l e" . chatgpt-shell-prompt-compose))
  :custom
  ((chatgpt-shell-anthropic-key
    (lambda ()
      (auth-source-pass-get 'secret "anthropic-key")))))


(provide 'gg-llm)
