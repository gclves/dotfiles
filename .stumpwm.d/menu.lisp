(defparameter *start-menu* '(("Spotify" "chromium-browser --app=https://play.spotify.com")
                             ("Audio/Video"
                              ("VLC" "vlc")
                              ("Pavu Control" "pavucontrol"))
                             ("Courses"
                              ("Destroy All Software" "emacsclient ~/Videos/DAS")
                              ("Mastering Python" "emacsclient '~/Videos/Mastering Python'")
                              ("Python Design Patters" "emacsclient '~/Videos/Python Design Patterns'")
                              ("Video Library" "emacsclient ~/Videos"))
                             ("BookShelf"
                              ("Bible Study" "xiphos")
                              ("Build Your Own AngularJS" "evince ~/Library/angularjs.pdf")
                              ("Haskell Programming From First Principles" "evince ~/Library/haskell-book.pdf")
                              ("Kindle library" "chromium-browser --app=https://read.amazon.com/")
                              ("On LISP" "evince ~/Library/onlisp.pdf")
                              ("Pocket list" "emacsclient -e '(pocket-reader)'"))
                             ("Internet"
                              ("Chromium" "chromium-browser")
                              ("Firefox" "firefox")
                              ("Qutebrowser" "qutebrowser"))
                             ("Games"
                              ("GnuChess" "xboard"))
                             ("Mathematics"
                              ("Octave" "qoctave")
                              ("Maxima" "wxmaxima")
                              ("Sage" "urxvt +sb -fn \"xft:Bitstream Vera Sans Mono:pixelsize=20\" -e /home/bzimmerly/src/sage-5.6/sage"))
                             ("Office Applications"
                              ("Emacs" "emacs")
                              ("Libre Office" "libreoffice"))
                             ("System Tools"
                              ("Network Connections" "nm-connection-editor")
                              ("VirtualBox" "VirtualBox"))))

(defcommand menu () ()
            "Start menu-like application launcher"
            (labels ((pick (options)
                       (let ((selection (stumpwm::select-from-menu
                                         (current-screen) options "")))
                         (cond
                           ((null selection)
                            (throw 'stumpwm::error "Abort."))
                           ((stringp (second selection))
                            (second selection))
                           (t
                            (pick (cdr selection)))))))
              (let ((choice (pick *start-menu*)))
                (run-shell-command choice))))

(define-key *root-map* (kbd ".") "menu")
