;(unless (server-running-p)
(server-start)

(unless (package-installed-p 'exwm)
  (package-install 'exwm))

(require 'exwm)
(require 'exwm-config)
(require 'exwm-systemtray)

(exwm-config-default)
(exwm-systemtray-enable)

(display-battery-mode t)
(display-time-mode t)

(message "Before setting up bindings")
(exwm-input-set-key (kbd "s-&")
                    (lambda (command)
                      (interactive (list (read-shell-command "$ ")))
                      (start-process-shell-command command nil command)))

(exwm-input-set-key (kbd "s-<f2>")
                    (lambda () (interactive) (start-process "" nil "slock")))

(exwm-input-set-key (kbd "<f4>") 'ivy-switch-buffer)
(exwm-input-set-key (kbd "s-R") 'exwm-reset)
(message "After setting up bindings")
