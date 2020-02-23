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
  (when value
    (scxmld-set-highlight value t)
    (scxmld-log (format "Selected element: %s"
                        (scxmld-pprint value))
                'info)))

;; Editing helpers
(cl-defmethod scxmld--toggle-marked-element-edit-idx-mode ((diagram scxmld-diagram) &optional force-on-or-off)
  "Enable edit idx mode on DIAGRAM's marked drawing if there is one.

Will return non-nil if something has been done meriting a rerender.

Setting force-on-or-off to 'off or 'on will force edit-idx mode
on or off."
  (with-slots (marked-element) diagram
    (if marked-element
        (let ((edit-idx (2dd-get-edit-idx marked-element)))
          (2dd-set-edit-idx marked-element
                            (cond ((eq force-on-or-off 'on)
                                   (or edit-idx 0))
                                  ((eq force-on-or-off 'off)
                                   nil)
                                  (edit-idx
                                   nil)
                                  (t
                                   0)))
          t)
      nil)))
(cl-defmethod scxmld--incf-selection ((diagram scxmld-diagram) &optional increment)
   "Whatever is marked in DIAGRAM, move to the next reasonable thing.

When INCREMENT is present and has a value of -1, move to the
previous reasonable thing.  Returns non-nil if any changes were
made."
   (let ((increment (max -1 (min 1 (or increment 1))))
         (marked-element (scxmld-get-marked diagram)))
     (if marked-element
       (let ((edit-idx (2dd-get-edit-idx marked-element)))
         (if edit-idx
             ;; An edit indxed is marked, move to the next one.
             (let* ((num-idxs (2dd-num-edit-idxs marked-element))
                    (next-idx (mod (+ edit-idx increment ) num-idxs)))
               (2dd-set-edit-idx marked-element next-idx))
           ;; An element is marked, move to the next one.
           (error "not yet implemented"))
         t)
       nil)))
(cl-defmethod scxmld--modify ((diagram scxmld-diagram) x-scratch y-scratch)
  "Move whatever is selected in DIAGRAM by X-SCRATCH and Y-SCRATCH coordinates."

  (let ((marked-element (scxmld-get-marked diagram)))
    (if marked-element
        (let* ((scratch-delta (2dg-point- x-scratch y-scratch))
               (scaling (2dd-get-point-scaling (2dd-get-viewport diagram)))
               (delta (2dg-scaled scratch-delta scaling))
               (edit-idx (2dd-get-edit-idx marked-element))
               (edited-geometry (if edit-idx
                                    (2dd-build-idx-edited-geometry marked-element
                                                                   edit-idx
                                                                   delta)
                                  (2dd-build-move-edited-geometry marked-element
                                                                  delta))))
          (if edited-geometry
              ;; TODO - make sure edited geometry doesn't clash with any siblings
              (progn (2dd-set-geometry marked-element edited-geometry)
                     t)
            nil))
      nil)))

;; actions
(cl-defmethod scxmld-plot ((diagram 2dd-diagram))
  "Plot drawings for this diagram."
  (2dd-plot (2dd-get-root diagram)
            (2dd-get-canvas diagram)
            #'scxml-children
            (lambda (_) t)))
(cl-defmethod scxmld-render ((diagram 2dd-diagram))
  "Render the scxmld diagram."
  (2dd-render-all diagram #'scxml-children))
(cl-defmethod scxmld-reset-viewport ((diagram 2dd-diagram))
  "Reset DIAGRAM's viewport to default extents and zoom."
  (2dd-set-viewport diagram
                    (2dd-build-viewport (2dd-get-canvas diagram))))


(cl-defmethod scxmld-find-drawing-selection ((diagram scxmld-diagram) (selection-rect 2dg-rect))
  (2dd-find-drawing-selection diagram selection-rect #'scxml-children))

(provide 'scxmld-diagram)
