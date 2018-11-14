;;; blog -- summary
;;; Commentary:
;;; These are the functions defined for org-publish to create the static HTML files
;;; for my blog.

(require 'org)
(require 'ox-publish)
(require 'ox-html)

;;; Code:
(defvar gg-blog-extra-header "<link rel='stylesheet' href='/static/styles.css'>
<link id='page_favicon' href='data:image/x-icon;base64,R0lGODlhEAAQAPIAAAAAAAcwV/z8/P///wAAAAAAAAAAAAAAACH5BAkyAAQAIf8LTkVUU0NBUEUyLjADAQAAACwAAAAAEAAQAAAD6hgREREhhBBCCCEIIUQAAAAEgiAIQSAIghAAAAAEgiAIQSAIghAAAAAgIBAIBIGAQCAQBAAAACAgEAgEgUAgEAgEAoFAIBAIBAKBgEAgEAQEAoEgAAAAAIGAQCAQBAQCAgIBAAAAAAEBAgICAgECAgICAQAAAAABAQICAgIBAgICAgEAAAAAAQEBAQEBAQEBAQEBAQEBAQEBAAAAAAEAAAAAAQAAAAABAQAAAAABAAAAAAEAAAAAAQEAAAAAAQAAAAABAAAAAAEBAAAAAAEAAAAAAQAAAAABAQEBAQEBAQECBAgQIECAAAESAAAh+QQJMgAEACwAAAAAEAAQAAAD6hgREREhhBBCCCEAAEAQgiAEAAAAQQAAABAIgiAEAAAAQQAAABAIAoEgAAAAAIEAAAAABAQCgSAAAAAAgUAgEAgEAoFAIBAIBAKBgEAgEAQAAAAgAAAAAIGAQCAQBAAAAAABAAAAAAEBAgICAgEAAAAAAQAAAAABAQICAgIBAAAAAAEAAAAAAQEBAQEBAQEBAQEBAQEBAQEBAgICAgEAAAAAAQICAgIBAQICAgIBAAAAAAECAgICAQECAgICAQAAAAABAgICAgEBAgICAgEAAAAAAQICAgIBAQEBAQEBAQECBAgQIECAAAESAAAh+QQJMgADACwAAAAAEAAQAIEAAAAHMFf8/PwAAAAC50wSEREREUIIIQghRBBCiAAAABAEgiAIgSAIQgAAABAEgiAIgSAIQgAAAACBgEAgEAQEAoEgAAAAAIFAIBAIBAKBQCAQCAQCgQAAAAAEBAKBIAAAAACBAAAAAAQEAoEgAAAAABAQAAAAABAgICAgEAAAAAAQEAAAAAAQICAgIBAAAAAAEBAQEBAQEBAQEBAQEBAQEBAQICAgIBAAAAAAEAAAAAAQECAgICAQAAAAABAAAAAAEBAgICAgEAAAAAAQAAAAABAQICAgIBAAAAAAEAAAAAAQEBAQEBAQEBAQEBAQIECAAAEKAAAh+QQJMgAEACwAAAAAEAAQAAAD6hgREREhhBBCCCEAAEAQgiAEgiAIQQAAABAIgiAEgiAIQQAAABAIAoEgIBAIBIEAAAAABAQCgSAgEAgEgUAgEAgEAoFAIBAIBAKBgEAgEAQAAAAgAAAAAIGAQCAQBAAAAAABAAAAAAEBAgICAgEAAAAAAQAAAAABAQICAgIBAAAAAAEAAAAAAQEBAQEBAQEBAQEBAQEBAQEBAAAAAAEAAAAAAQICAgIBAQAAAAABAAAAAAECAgICAQEAAAAAAQAAAAABAgICAgEBAAAAAAEAAAAAAQICAgIBAQEBAQEBAQECBAgQIECAAAESAAA7' rel='icon' type='image/x-icon'>
\n"
  "HTML that'll be appended to the blog's HEAD element.")
(defvar gg-blog-emacs-config-name "emacsconfig.org"
  "The settings that'll be used for the pages -- wat.")
(defvar gg-blog-process-emacs-config nil
  "Whether `gg-blog-emacs-config-name' should be used for settings.org.")

(defvar gg-blog-header-file "~/Org/blog/header.html"
  "File containing the header HTML -- not sure it's actually used.")
(defun gg-blog-header (arg)
  (with-temp-buffer
    (insert-file-contents gg-blog-header-file)
    (buffer-string)))

(setq org-html-divs '((preamble "header" "preamble")
                      (content "article" "content")
                      (postamble "footer" "postamble")))

;;; Sitemap-related stuff
(defun gg-blog-get-preview (file)
  "The comments in FILE have to be on their own lines, prefereably before and after paragraphs."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (let ((beg (+ 1 (re-search-forward "^#\\+BEGIN_PREVIEW$")))
          (end (progn (re-search-forward "^#\\+END_PREVIEW$")
                      (match-beginning 0))))
      (buffer-substring beg end))))

(defun gg-blog-sitemap (project &optional sitemap-filename)
  "Generate the sitemap for my blog."
  (let* ((project-plist (cdr project))
         (dir (file-name-as-directory
               (plist-get project-plist :base-directory)))
         (localdir (file-name-directory dir))
         (exclude-regexp (plist-get project-plist :exclude))
         (files (nreverse
                 (org-publish-get-base-files project exclude-regexp)))
         (sitemap-filename (concat dir (or sitemap-filename "sitemap.org")))
         (sitemap-sans-extension
          (plist-get project-plist :sitemap-sans-extension))
         (visiting (find-buffer-visiting sitemap-filename))
         file sitemap-buffer)
    (with-current-buffer
        (let ((org-inhibit-startup t))
          (setq sitemap-buffer
                (or visiting (find-file sitemap-filename))))
      (erase-buffer)
      ;; loop through all of the files in the project
      (while (setq file (pop files))
        (let ((fn (file-name-nondirectory file))
              (link ;; changed this to fix links. see postprocessor.
               (file-relative-name file (file-name-as-directory
                                         (expand-file-name (concat (file-name-as-directory dir) "..")))))
              (oldlocal localdir))
          (when sitemap-sans-extension
            (setq link (file-name-sans-extension link)))
          ;; sitemap shouldn't list itself
          (unless (equal (file-truename sitemap-filename)
                         (file-truename file))
            (let (;; get the title and date of the current file
                  (title (org-publish-format-file-entry "%t" file project-plist))
                  (date (org-publish-format-file-entry "%d" file project-plist))
                  ;; get the preview section from the current file
                  (preview (gg-blog-get-preview file))
                  (regexp "\\(.*\\)\\[\\([^][]+\\)\\]\\(.*\\)"))
              ;; insert a horizontal line before every post, kill the first one
              ;; before saving
              (insert "-----\n")
              (cond ((string-match-p regexp title)
                     (string-match regexp title)
                     ;; insert every post as headline
                     (insert (concat"* " (match-string 1 title)
                                    "[[file:" link "]["
                                    (match-string 2 title)
                                    "]]" (match-string 3 title) "\n")))
                    (t (insert (concat "* [[file:" link "][" title "]]\n"))))
              ;; insert the date, preview, & read more link
              (insert (concat date "\n\n"))
              (insert preview)
              (insert (concat "[[file:" link "][Read More...]]\n"))))))
      ;; kill the first hrule to make this look OK
      (goto-char (point-min))
      (let ((kill-whole-line t)) (kill-line))
      (let ((title (plist-get project-plist :sitemap-title)))
        (when title
            (insert (format "#+TITLE: %s\n" title))))
      (save-buffer))
    (or visiting (kill-buffer sitemap-buffer))))


(defun gg-blog-pages-preprocessor ()
  "Move a fresh version of the settings.org file to the pages directory."
  (when gg-blog-process-emacs-config
    (let* ((cfg-file (expand-file-name (concat (file-name-as-directory user-emacs-directory)
                                               "settings.org")))
           (destdir (file-name-as-directory (plist-get project-plist :base-directory)))
           (cfg-file-dest (expand-file-name (concat destdir gg-blog-emacs-config-name))))
      (copy-file cfg-file cfg-file-dest t))))

(defun gg-blog-pages-postprocessor ()
  (message "In the pages postprocessor."))

(defun gg-blog-articles-preprocessor ()
  (message "In the articles preprocessor."))

(defun gg-blog-articles-postprocessor ()
  "Massage the sitemap file and move it up one directory.

for this to work, we have already fixed the creation of the
relative link in the sitemap-publish function"
  (let* ((sitemap-fn (concat (file-name-sans-extension (plist-get project-plist :sitemap-filename)) ".html"))
         (sitemap-olddir (plist-get project-plist :publishing-directory))
         (sitemap-newdir (expand-file-name (concat (file-name-as-directory sitemap-olddir) "..")))
         (sitemap-oldfile (expand-file-name sitemap-fn sitemap-olddir))
         (sitemap-newfile (expand-file-name (concat (file-name-as-directory sitemap-newdir) sitemap-fn))))
    (with-temp-buffer
      (goto-char (point-min))
      (insert-file-contents sitemap-oldfile)
      ;; massage the sitemap if wanted

      ;; delete the old file and write the correct one
      (delete-file sitemap-oldfile)
      (write-file sitemap-newfile))))

(setq org-publish-project-alist
      `(("blog"
         :components ("blog-articles", "blog-pages", "blog-static", "blog-images", "blog-dl"))
        ("blog-articles"
         :base-directory "~/Org/blog/articles/"
         :base-extension "org"
         :exclude ".draft.org$"
         :publishing-directory "~/blog_html/articles/"
         :publishing-function org-html-publish-to-html
         :preparation-function gg-blog-articles-preprocessor
         :completion-function gg-blog-articles-postprocessor
         :htmlized-source t ;; this enables htmlize, which means that I can use css for code!

         :with-author t
         :with-creator nil
         :with-date t
         :with-email nil

         :headline-level 4
         :section-numbers nil
         :with-toc nil
         :with-drawers t
         :with-sub-superscript nil ;; important!!

         ;; the following removes extra headers from HTML output -- important!
         :html-link-home "/"
         :html-head nil ;; cleans up anything that would have been in there.
         :html-head-extra ,gg-blog-extra-header
         :html-head-include-default-style nil
         :html-head-include-scripts nil
         :html-viewport nil

         :html-home/up-format ""
         :html-footnotes-section "<div id='footnotes'><!--%s-->%s</div>"
         :html-link-up ""
         :html-link-home ""
         :html-preamble gg-blog-header

         ;; sitemap - list of blog articles
         :auto-sitemap t
         :sitemap-filename "index.org"
         :sitemap-title "Guilherme Gonçalves"
         ;; custom sitemap generator function
         :sitemap-function gg-blog-sitemap
         :sitemap-sort-files anti-chronologically
         :sitemap-date-format "*Published:* /%a %b %d %Y/")
        ("blog-pages"
         :base-directory "~/Org/blog/pages/"
         :base-extension "org"
         :publishing-directory "~/blog_html/"
         :publishing-function org-html-publish-to-html
         :preparation-function gg-blog-pages-preprocessor
         :completion-function gg-blog-pages-postprocessor
         :htmlized-source t

         :with-author t
         :with-creator nil
         :with-date t

         :headline-level 4
         :section-numbers nil
         :with-toc nil
         :with-drawers t
         :with-sub-superscript nil ;; important!!
         :html-viewport nil ;; hasn't worked yet

         ;; the following removes extra headers from HTML output -- important!
         :html-link-home "/"
         :html-head nil ;; cleans up anything that would have been in there.
         :html-head-extra ,gg-blog-extra-header
         :html-head-include-default-style nil
         :html-head-include-scripts nil

         :html-home/up-format ""
         :html-footnotes-section "<div id='footnotes'><!--%s-->%s</div>"
         :html-link-up ""
         :html-link-home ""

         :html-preamble gg-blog-header)

        ("blog-static"
         :base-directory "~/Org/blog/static/"
         :base-extension ".*"
         :publishing-directory "~/blog_html/static/"
         :publishing-function org-publish-attachment)
        ("blog-images"
         :base-directory "~/Org/blog/img/"
         :base-extension ".*"
         :publishing-directory "~/blog_html/img/"
         :publishing-function org-publish-attachment
         :recursive t)
        ("blog-dl"
         :base-directory "~/Org/blog/dl/"
         :base-extension ".*"
         :publishing-directory "~/blog_html/dl/"
         :publishing-function org-publish-attachment
         :Recursive t)))

(provide 'blog)
;;; blog.el ends here
