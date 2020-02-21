
(require '2dd)
(require 'scxml)


(defclass scxmld-diagram (2dd-diagram)
  ((marked-element :initform nil
                   :reader scxmld-get-marked
                   :writer scxmld-set-marked
                   :type (or null scxmld-element)))
  :documentation "An scxml diagram")

(cl-defmethod scxmld-set-marked :before ((this scxmld-diagram) value)
  "Before marking one element, properly unmark the other."
  (let ((currently-marked (oref this marked-element)))
    (when (and currently-marked (not (eq currently-marked value)))
      ;; Put currently-marked back to normal display mode.
      (scxmld-set-highlight currently-marked nil)
      (2dd-set-edit-idx currently-marked nil))))
(cl-defmethod scxmld-set-marked :after ((this scxmld-diagram) value)
  "After marking element VALUE, highlight it."
  (scxmld-set-highlight value t)
  (scxmld-log (format "Selected element: %s"
                      (scxmld-pprint value))))

(cl-defmethod scxmld-plot ((diagram 2dd-diagram))
  "Plot drawings for this diagram."
  (2dd-plot (2dd-get-root diagram)
            (2dd-get-canvas diagram)
            #'scxml-children
            (lambda (_) t)))
(cl-defmethod scxmld-render ((diagram 2dd-diagram))
  "Render the scxmld diagram."
  (2dd-render-all diagram #'scxml-children))
(cl-defmethod scxmld-find-drawing-selection ((diagram scxmld-diagram) (selection-rect 2dg-rect))
  (2dd-find-drawing-selection diagram selection-rect #'scxml-children))

(provide 'scxmld-diagram)
