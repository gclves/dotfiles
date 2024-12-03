(with-eval-after-load 'notmuch
  (setq
   notmuch-hello-sections
   '(notmuch-hello-insert-saved-searches notmuch-hello-insert-search notmuch-hello-insert-alltags notmuch-hello-insert-footer)
   notmuch-saved-searches
        '((:name "inbox" :query "tag:inbox" :key
                 [105])
          (:name "newsletters" :query "tag:newsletters")
          (:name "unread" :query "tag:unread" :key
                 [117])
          (:name "flagged" :query "tag:flagged" :key
                 [102])
          (:name "sent" :query "tag:sent" :key
                 [116])
          (:name "drafts" :query "tag:draft" :key
                 [100])
          (:name "all mail" :query "*" :key
                 [97])))

  (defun gg--notmuch-delete-message ()
      "Mark a message to be deleted"
    (interactive)
    (notmuch-search-tag '("+deleted" "-inbox"))
    (notmuch-search-next-thread))

  (defun gg--notmuch-delete-from-message ()
    "Mark a message to be deleted, then jump to the next"
    (interactive)
    (notmuch-show-tag '("+deleted" "-inbox"))
    (notmuch-show-next-thread-show))

  (define-key notmuch-search-mode-map (kbd "d") 'gg--notmuch-delete-message)
  (define-key notmuch-show-mode-map (kbd "d") 'gg--notmuch-delete-from-message))


(provide 'gg-mail)
