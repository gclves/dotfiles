;;; Commentary:
;;; A webserver in Emacs, because why not.
;;; Basically a fast replacement for serve_this in Fish.

(use-package web-server
  :config
  (defvar my/file-server nil "Is the file server running? Holds an instance if so.")

  (defun my/ws-start (handlers port &optional log-buffer &rest network-args)
    "Like `ws-start', but unbroken for Emacs 25+."
    (let ((server (make-instance 'ws-server :handlers handlers :port port))
          (log (when log-buffer (get-buffer-create log-buffer))))
      (setf (process server)
            (apply
             #'make-network-process
             :name "ws-server"
             :service (port server)
             :filter 'ws-filter
             :server t
             :nowait nil
             :family 'ipv4
             :coding 'no-conversion
             :plist (append (list :server server)
                            (when log (list :log-buffer log)))
             :log (when log
                    (lambda (proc request message)
                      (let ((c (process-contact request))
                            (buf (plist-get (process-plist proc) :log-buffer)))
                        (with-current-buffer buf
                          (goto-char (point-max))
                          (insert (format "%s\t%s\t%s\t%s"
                                          (format-time-string ws-log-time-format)
                                          (first c) (second c) message))))))
             network-args))
      (push server ws-servers)
      server))

  (defun my/serve-this (&optional port)
    "Start a file server on a `PORT', serving the content of directory
associated with the current buffer's file."
    (interactive "nPort: ")
    ;; Taken from http://eschulte.github.io/emacs-web-server/File-Server.html#File-Server.
    (if my/file-server
        (message "File server is already running!")
      (progn
        (setf my/file-server
              (lexical-let ((docroot (if (buffer-file-name)
                                         (file-name-directory (buffer-file-name))
                                       (expand-file-name default-directory))))
                (my/ws-start
                 (lambda (request)
                   (with-slots (process headers) request
                     (let ((path (substring (cdr (assoc :GET headers)) 1)))
                       (if (ws-in-directory-p docroot path)
                           (if (file-directory-p path)
                               ;; TODO a better ws-send-directory-list
                               (ws-send-directory-list process
                                                       (expand-file-name path docroot)
                                                       "^[^\.]")
                             (ws-send-file process (expand-file-name path docroot)))
                         (ws-send-404 process)))))
                 port
                 nil                    ;no log buffer
                 :host "0.0.0.0")))
        (message "Serving files on port %d" port))))

  (defun my/stop-server ()
    "Stop the file server if running."
    (interactive)
    (if my/file-server
        (progn
          (ws-stop my/file-server)
          (setf my/file-server nil)
          (message "Stopped the file server."))
      (message "No file server is running."))))


(provide 'init-web-server)
