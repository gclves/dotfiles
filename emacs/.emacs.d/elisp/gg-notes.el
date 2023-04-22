;;; Org-mode
(use-package htmlize
  :hook org-mode)

;; FIXME: I don't use org-mode anymore --> get rid of all of this
(with-eval-after-load 'org
  (require 'ox-md)

  ;; XXX these commands are global and shouldn't really be under C-c therefore
  (global-set-key (kbd "C-c l") 'org-store-link)
  (global-set-key (kbd "C-c b") 'org-switchb)
  (global-set-key (kbd "C-c j") 'org-clock-goto)
  (global-set-key (kbd "<f2>") 'org-capture)
  (global-set-key (kbd "<f3>") 'org-agenda)
  (global-set-key (kbd "C-c c") 'org-capture)
  (global-set-key (kbd "C-c a") 'org-agenda)

  (setq org-agenda-include-diary t
        org-log-reschedule 'note
        org-log-done 'time
        org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t
        org-fontify-whole-heading-line t
        org-fontify-done-headline t
        org-fontify-quote-and-verse-blocks t
        org-hide-emphasis-markers t
        org-agenda-restore-windows-after-quit t
        org-src-fontify-natively t     ; syntax highlight in code blocks
        org-return-follows-link t      ; return opens links
        org-confirm-babel-evaluate nil ; stop prompting for confirmation on eval
        org-src-tab-acts-natively t    ; make TAB behave as expected in src blocks
        org-support-shift-select nil
        org-image-actual-width nil
        org-html-doctype "html5"
        org-startup-folded nil
        org-refile-targets '((nil :maxlevel . 3)
                             ("~/sync/Notes/work.org" :maxlevel . 1)
                             ("~/sync/Notes/personal.org" :maxlevel . 3)
                             ("~/sync/Notes/someday.org" :maxlevel . 1)
                             ("~/sync/Notes/tickler.org" :maxlevel . 2))
        org-outline-path-complete-in-steps nil ; Refile in a single go
        org-refile-use-outline-path 'file      ; Refile to top-level
        org-todo-keywords
        '((sequence "TODO(t)" "WAITING(w)" "|" "DONE(d)" "CANCELLED(c)"))
        org-highlight-latex-and-related '(latex script))

  ;; run shell commands from org-babel
  (defvar -org-babel-langs '((shell . t) (python . t) (ruby . t)))
  (setq org-babel-python-command "python3")
  (org-babel-do-load-languages 'org-babel-load-languages -org-babel-langs)

  ;; display/update images in the buffer after I evaluate
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  ;; Link to manpages from org
  (org-add-link-type "man" 'org-man-open)
  (add-hook 'org-store-link-functions 'org-man-store-link)

  (defcustom org-man-command 'man
    "The Emacs command to be used to display a man page."
    :group 'org-link
    :type '(choice (const man) (const woman)))

  (defun org-man-open (path)
    "Visit the manpage on PATH.
PATH should be a topic that can be thrown at the man command."
    (funcall org-man-command path))

  (defun org-man-store-link ()
    "Store a link to a manpage."
    (when (memq major-mode '(Man-mode woman-mode))
      ;; This is a man page, we do make this link
      (let* ((page (org-man-get-page-name))
             (link (concat "man:" page))
             (description (format "Manpage for %s" page)))
        (org-store-link-props
         :type "man"
         :link link
         :description description))))

  (defun org-man-get-page-name ()
    "Extract the page name from the buffer name."
    ;; This works for both `Man-mode' and `woman-mode'.
    (if (string-match " \\(\\S-+\\)\\*" (buffer-name))
        (match-string 1 (buffer-name))
      (error "Cannot create link to this man page"))))

(use-package deft
  :bind
  (([f5] . deft)
   ("C-x C-g" . deft-find-file))
  :config
  (setq deft-extensions '("md" "org" "txt")
        deft-recursive t
        deft-default-extension "md"
        deft-directory "~/sync/Notes"
        deft-new-file-format "%Y%m%d%H%M"))

(use-package markdown-mode
  :commands (markdown-mode gfm-mode)
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;; Org-powered presentations
(with-eval-after-load 'org
  (require 'epresent))

(provide 'gg-notes)
;;; gg-notes.el ends here
