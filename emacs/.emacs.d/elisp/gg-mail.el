(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu/mu4e")
(require 'mu4e)

;; Largely based on the following MacOS guide:
;; https://macowners.club/posts/email-emacs-mu4e-macos/
(setq mu4e-mu-binary (executable-find "mu")
      mu4e-maildir "~/.mail"
      mu4e-get-mail-command (concat (executable-find "mbsync") " -a")
      mu4e-update-interval 300
      mu4e-attachment-dir "~/Downloads"
      mu4e-change-filenames-when-moving t

      mu4e-user-mail-address-list '("/.*@gclv\.es/"
                                    "/.*@ggoncalves\.me/"
                                    "/.*@.*\.ggoncalves\.me/")

      mu4e-maildir-shortcuts '(("/p/INBOX" . ?i)
                               ("/p/Sent" . ?I))

      mu4e-context-policy 'pick-first
      mu4e-compose-context-policy 'ask)

(setq mu4e-contexts
      `(,(make-mu4e-context
          :name "Migadu"
          :enter-func
          (lambda () (mu4e-message "Enter Migadu context"))
          :leave-func
          (lambda () (mu4e-message "Leave Migadu context"))
          :match-func
          (lambda (msg)
            (when msg
              (mu4e-message-contact-field-matches msg
                                                  :to "/.*@gclv\.es/")))
          :vars '((user-mail-address . "_@gclv.es" )
                  (user-full-name . "Gui Goncalves")
                  (mu4e-drafts-folder . "/p/drafts")
                  (mu4e-refile-folder . "/p/Archive")
                  (mu4e-sent-folder . "/p/Sent")
                  (mu4e-trash-folder . "/p/Trash")))))

;; store link to message if in header view, not to header query:
(setq org-mu4e-link-query-in-headers-mode nil)
;; don't have to confirm when quitting:
(setq mu4e-confirm-quit nil)
;; number of visible headers in horizontal split view:
(setq mu4e-headers-visible-lines 20)
;; don't show threading by default:
(setq mu4e-headers-show-threads nil)
;; hide annoying "mu4e Retrieving mail..." msg in mini buffer:
(setq mu4e-hide-index-messages t)
;; customize the reply-quote-string:
(setq message-citation-line-format "%N @ %Y-%m-%d %H:%M :\n")
;; M-x find-function RET message-citation-line-format for docs:
(setq message-citation-line-function 'message-insert-formatted-citation-line)
;; by default do not show related emails:
(setq mu4e-headers-include-related nil)
;; by default do not show threads:
(setq mu4e-headers-show-threads nil)

;; gpg encryptiom & decryption:
;; this can be left alone
(require 'epa-file)
(epa-file-enable)
(setq epa-pinentry-mode 'loopback)
(auth-source-forget-all-cached)

;; don't keep message compose buffers around after sending:
(setq message-kill-buffer-on-exit t)

;; send function:
(setq send-mail-function 'sendmail-send-it
      message-send-mail-function 'sendmail-send-it)

;; send program:
;; this is exeranal. remember we installed it before.
(setq sendmail-program (executable-find "msmtp"))

;; select the right sender email from the context.
(setq message-sendmail-envelope-from 'header)

;; mu4e cc & bcc
;; this is custom as well
(add-hook 'mu4e-compose-mode-hook
          (defun timu/add-cc-and-bcc ()
            "My Function to automatically add Cc & Bcc: headers.
    This is in the mu4e compose mode."
            (save-excursion (message-add-header "Cc:\n"))
            (save-excursion (message-add-header "Bcc:\n"))))

;; mu4e address completion
(add-hook 'mu4e-compose-mode-hook 'company-mode)

;; (setq mu4e-contexts
;;       `( ,(make-mu4e-context
;;            :name "Migadu"
;;            :match-func (lambda (msg) (when msg
;;                                   (string-prefix-p "/Migadu" (mu4e-message-field msg :maildir))))
;;            :vars '(
;;                    (mu4e-trash-folder . "/Trash")
;;                    (mu4e-refile-folder . "/Archive")))))

(provide 'gg-mail)
