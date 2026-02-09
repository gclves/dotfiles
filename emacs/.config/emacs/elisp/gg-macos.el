(defmacro on-macOS (&rest body)
  "Evaluate BODY if running on macOS."
  `(when (eq system-type 'darwin)
     ,@body))

(defmacro unless-on-macOS (&rest body)
  "Evaluate BODY if not running on macOS."
  `(unless (eq system-type 'darwin)
     ,@body))


(provide 'gg-macos)
