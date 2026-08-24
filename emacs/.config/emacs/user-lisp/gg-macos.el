(defmacro on-macOS (&rest body)
  "Evaluate BODY if running on macOS."
  `(when (eq system-type 'darwin)
     ,@body))

(defmacro unless-on-macOS (&rest body)
  "Evaluate BODY if not running on macOS."
  `(unless (eq system-type 'darwin)
     ,@body))

(on-macOS
 (setq ns-use-proxy-icon  nil
       frame-title-format nil
       locate-command "mdfind"))

(provide 'gg-macos)
