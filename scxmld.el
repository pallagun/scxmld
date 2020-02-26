
;;; Code:

(defsubst scxmld-error (msg)
  "Call scxmld-log with MSG and a level of 'error."
  (scxmld-log msg 'error))
(defun scxmld-log (msg &optional level)
  "Inform the user about MSG with severity level LEVEL."
  (message (format "%s: %s"
                   level
                   msg)))

(require 'scxmld-element)
(require 'scxmld-elements)
(require 'scxmld-diagram)
(require 'scxmld-xml)
(require 'scxmld-mode)


(provide 'scxmld)
;;; scxmld.el ends here
