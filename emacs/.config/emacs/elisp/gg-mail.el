(add-to-list 'load-path "/usr/share/emacs/site-lisp/elpa-src/mu4e-1.12.6")
(add-to-list 'load-path "/usr/share/emacs/site-lisp/mu4e/")

(require 'mu4e)

(with-eval-after-load 'mu4e
  (setq mu4e-get-mail-command "mbsync -Va"
        mu4e-drafts-folder "/Drafts"
        mu4e-sent-folder "/Sent"
        mu4e-trash-folder "/Trash"
        mu4e-refile-folder "/Archive"
        mu4e-use-fancy-chars nil
        mu4e-change-filenames-when-moving t
        mu4e-update-interval 300
        mu4e-headers-fields '((:human-date . 12)
                              (:flags . 6)
                              (:mailing-list . 10)
                              (:from-or-to . 22)
                              (:thread-subject)))

  (add-to-list 'mu4e-bookmarks
               '( :name "Newsletters"
                  :key ?n
                  :query "maildir:/Newsletters"))

  (add-to-list 'mu4e-bookmarks
               '( :name "Inbox"
                  :key ?i
                  :query "maildir:/Inbox"))

  (setq mu4e-maildir-shortcuts
        '(( :maildir "/Inbox"
            :key ?i
            :name: "Inbox")
          ( :maildir "/Archive"
            :key ?a
            :name "Archive")
          ( :maildir "/GMail_All"
            :key ?,
            :name "Old Mail"))))


(with-eval-after-load 'sendmail
  (setq mail-host-address "gclv.es"
        user-full-name "Gui Goncalves"
        user-mail-address "_@gclv.es")

  (setq send-mail-function 'sendmail-send-it
        message-send-mail-function 'sendmail-send-it
        sendmail-program "/usr/bin/msmtp"
        mail-specify-envelope-from t
        message-sendmail-envelope-from 'header
        mail-envelope-from 'header)

  (defun gg-message-mode-setup ()
    (setq fill-column 72)
    (turn-on-auto-fill))

  (add-hook 'message-mode-hook 'gg-message-mode-setup))


(provide 'gg-mail)
;;; gg-mail.el ends here
