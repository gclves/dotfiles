;; Cool stuff to do:
;; - manage VirtualBox VMs
;; - integrate with pass (partially done with dmenu)
;; - manage docker containers
;; - connect to Bluetooth devices (in my dreams maybe)

(in-package :stumpwm)

(ql:quickload 'cl-ppcre)

(setf *mouse-focus-policy* :sloppy)     ; focus follows mouse
(setf *startup-message* "Hack the planet")
(setf *timeout-wait* 10)
(set-font "-xos4-terminus-medium-r-normal--14-140-72-72-c-80-iso8859-15")

(defparameter *TERMINAL* (cons "st" "st-256color"))
(defparameter *BROWSER* "firefox")

(bind "w" "windowlist")
(bind "C-w" "windows")

(defcommand chromium () ()
            (run-or-raise "chromium-browser"
                          ;; the :role prop rules out the apps
                          '(:class "Chromium" :role "browser"))
            (message "Chromium"))
(bind "z" "chromium")
(bind "C-z" "exec chromium-browser")

(defcommand chromium-app (app-id) ((:string "What's the app's id? "))
            (run-or-raise
             (concat "chromium-browser --app-id=" app-id) `(:instance ,(concat "crx_" app-id))))

(defcommand spotify () ()
            "Run or raise the Spotify client"
            (run-or-raise "spotify" '(:class "Spotify")))

(bind "[" "spotify")
(bind "]" "chromium-app jeogkiiogjbmhklcnbgkdcjoioegiknm") ; slack

(defcommand emacs () ()
            "Like the native Emacs command, but using emacsclient instead"
            (run-or-raise "emacsclient -nc -a \"\"" '(:class "Emacs")))

(defcommand emacs-server () ()
            "Start the Emacs server and switch to it"
            (message "Starting the Emacs server")
            (run-commands "exec emacs -e 'server-start'"
                          "emacs"))

(defmacro define-on-top (key command)
  `(define-key *top-map* (kbd ,key) ,command))

(define-on-top "F3" "fother")
(define-on-top "S-F3" "fnext")
(define-on-top "F4" "pull-hidden-other")
(define-on-top "S-F4" "gother")
(define-on-top "F5" "pull-hidden-next")
(define-on-top "S-F5" "gnext")
(define-on-top "F6" "delete")

(defcommand web-browser () ()
            (run-or-raise *BROWSER* '(:role "browser"))
            (message "Web browser"))
(define-on-top "F9" "web-browser")
(define-on-top "S-F9" (format nil "exec ~a" *BROWSER*))

(define-on-top "F10" "emacs")

(defcommand terminal () ()
            (destructuring-bind (terminal-cmd . terminal-class) *TERMINAL*
              (run-or-raise terminal-cmd `(:class ,terminal-class))))
(bind "c" (concat "exec " (car *TERMINAL*)))
(defcommand emacsshell () ()
            (run-commands "exec emacsclient -e '(shell)'"
                          "emacs"))
(bind "`" "terminal")
(define-on-top "F11" "emacsshell")
(define-on-top "F12" "exec")

(define-key *top-map* (kbd "XF86AudioLowerVolume") "exec /usr/bin/pactl set-sink-volume @DEFAULT_SINK@ '-5%'")
(define-key *top-map* (kbd "XF86AudioRaiseVolume") "exec /usr/bin/pactl set-sink-volume @DEFAULT_SINK@ '+5%'")
(define-key *top-map* (kbd "XF86AudioMute") "exec /usr/bin/pactl set-sink-mute @DEFAULT_SINK@ toggle")

(define-key *top-map* (kbd "XF86AudioPlay") "exec playerctl play-pause")
(define-key *top-map* (kbd "XF86AudioNext") "exec playerctl next")
(define-key *top-map* (kbd "XF86AudioPrev") "exec playerctl previous")

(define-key *top-map* (kbd "XF86MonBrightnessUp") "exec lux -a 20")
(define-key *top-map* (kbd "XF86MonBrightnessDown") "exec lux -s 20")

;; prompt the user for an interactive command. The first arg is an
;; optional initial contents.
(defcommand colon1 (&optional (initial "")) (:rest)
            (let ((cmd (read-one-line (current-screen) ": " :initial-input initial)))
              (when cmd
                (eval-command cmd t))))

;; Browse somewhere
(bind "b" (format nil "colon1 exec ~a http://www." *BROWSER*))
;; Ssh somewhere
(bind "C-s" (format nil "colon1 exec ~a -e ssh " (car *TERMINAL*)))

(defcommand keyboard () ()
            (run-shell-command "setxkbmap -layout us,us -variant ,alt-intl -option grp:ctrls_toggle -option ctrl:swapcaps"))

(bind "C-x" "exec ~/bin/passmenu")
(bind "C-l" "exec slock")

;; Misc interactive commands
(defcommand randr (hdmi?) ((:y-or-n "Is the HDMI cable on? "))
            (let ((base-cmd "xrandr --output eDP-1 --auto --output HDMI-1 ")
                  (hdmi-cmd (if hdmi? "--auto --right-of eDP-1" "--off")))
              (run-shell-command (concat base-cmd hdmi-cmd)))
            (run-commands "refresh-heads"))

(defun make-menu (options description)
  "Helper to show a menu on the current screen based on shell output"
  (select-from-menu (current-screen) (cl-ppcre:split "\\n" options) description))

(defcommand
    audio-sink () ()
    "Show a menu to enable a PulseAudio sink"
    (let* ((sinks (run-shell-command "pactl list sinks | grep -i name: | awk '{print $2}'" t))
           (sink (make-menu sinks "Sink")))
      (run-shell-command (format nil "pactl set-default-sink ~s" sink))))

(defparameter *vpn-name* "'Lastline Goleta'")
(defcommand vpn (up?) ((:y-or-n "Do you want to turn the VPN on? "))
            (run-shell-command
             (concat "nmcli connection " (if up? "up " "down ") *vpn-name*)))

;; TODO: show status, toggle status
(defcommand
    network () ()
    "Show a menu to manage network connections (WiFi/VPN)"
    (let* ((connections (run-shell-command "nmcli conn | egrep '(vpn|wireless)' | awk -F '  ' '{print $1}'" t))
           (net (make-menu connections "Connection"))
           (command (select-from-menu (current-screen) '("up" "down") "Command")))
      (run-shell-command (format nil "nmcli connection ~a ~s" command net))))
(bind "," "network")

(defcommand suspend (confirm) ((:y-or-n "Are you sure you want to suspend?"))
            (when confirm (run-shell-command "systemctl suspend")))

(defcommand poweroff (confirm) ((:y-or-n "Are you sure you want to poweroff?"))
            (when confirm (run-shell-command "systemctl poweroff")))

(defcommand run-in-terminal (command) ((:string "Run in terminal: "))
            (run-shell-command (concat (car *TERMINAL*) " -e " command)))
(define-on-top "S-F12" "run-in-terminal")

(define-on-top "s-;" "mode-line")

;;; Super + F<n> switches groups
(dotimes (i 13)
  (unless (eq i 0) ; F0 is non-existent and will error.
    (define-key *top-map* (kbd (format nil "s-F~a" i)) (format nil "gselect ~a" i))))

(defcommand reload-menu () ()
            "Reload the menu configuration"
            (load "~/.stumpwm.d/menu.lisp"))

(when *initializing*
  (run-commands "emacs-server"
                "keyboard"
                "exec dropbox start"
                "exec xsetroot -cursor_name left_ptr -solid black -name root-window"
                "reload-menu"))
