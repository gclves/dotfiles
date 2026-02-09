(use-package elfeed
  :config
  (setq elfeed-feeds
        '(("https://lobste.rs/rss" firehose tech)
          ("https://ciechanow.ski/atom.xml" long-form tech)
          ("https://samwho.dev/rss.xml" long-form tech)
          ("https://pudding.cool/feed/index.xml" long-form tech)
          ("https://pagedout.institute/atom.xml" long-form tech)
          ("https://fasterthanli.me/index.xml" long-form tech)
          ("https://solar.lowtechmagazine.com/feeds" lifestyle)
          ("http://feeds2.feedburner.com/NoTechMagazine" lifestyle)
          "https://www.plough.com/en/plough-rss-feed"))

  ;; TODO: Add more sophisticated filters here
  ;; (add-hook 'elfeed-new-entry-hook
  ;; (elfeed-make-tagger :feed-url "example\\.com"
  ;; :entry-title '(not "something interesting")
  ;; :add 'junk
  ;; :remove 'unread))

  ;; Mark all YouTube entries
  (add-hook 'elfeed-new-entry-hook
            (elfeed-make-tagger :feed-url "youtube\\.com"
                                :add '(video youtube))))

(with-eval-after-load 'browse-url
  (setq browse-url-browser-function 'eww))


(provide 'gg-rss)
