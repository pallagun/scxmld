
(require '2dd)
(require 'scxml)


(defclass scxmld-diagram (2dd-diagram)
  ((marked-element :initarg :marked-element
                   :initform nil
                   :reader scxmld-get-marked
                   :writer scxmld-set-marked
                   :type (or null scxmld-element)))
  :documentation "An scxml diagram")

(cl-defmethod scxmld-plot ((diagram 2dd-diagram))
  "Plot drawings for this diagram."
  (2dd-plot (2dd-get-root diagram)
            (2dd-get-canvas diagram)
            #'scxml-children
            (lambda (_) t)))

(cl-defmethod scxmld-render ((diagram 2dd-diagram))
  "Render the scxmld diagram."
  (2dd-render-all diagram #'scxml-children))

(provide 'scxmld-diagram)
