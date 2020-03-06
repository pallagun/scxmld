;; -*- lexical-binding: t; -*-

;;; Code:

(require '2dd)
(require 'scxml)
(require 'scxmld-elements)
(require 'scxmld-xml)

(defvar-local scxmld-diagram-link nil
  "Indicates, for an xml buffer, which scxmld-diagram object it is linked to.")
(defconst scxmld-diagram-linked-xml-idle 0.5
  "Amount of idle time to wait before updating the linked xml document.")

(defconst scxmld-plot-settings
  (list :sibling-margin-vertical 6
        :sibling-margin-horizontal 10
        :inner-padding-horizontal 3
        :inner-padding-vertical 2)
  "Default plot settings.")

(defclass scxmld-diagram (2dd-diagram)
  ((marked-element :initform nil
                   :reader scxmld-get-marked
                   :writer scxmld-set-marked
                   :type (or null scxmld-element))
   (linked-xml-buffer :initform nil
                      :reader scxmld-get-linked-xml-buffer
                      :writer scxmld-set-linked-xml-buffer
                      :type (or null buffer))
   (queued-xml-updates :initform nil
                       :reader scxmld-get-queued-xml-updates
                       :type (or null list))
   (queued-xml-timer :initform nil
                     :reader scxmld-get-queued-xml-timer
                     :writer scxmld-set-queued-xml-timer))
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
(cl-defmethod scxmld-set-linked-xml-buffer :after ((this scxmld-diagram) value)
  "When setting the buffer, leave a local link variable."
  (with-current-buffer value
    (setq-local scxmld-diagram-link this)))

;; Editing helpers
(cl-defmethod scxmld-set-marked-element-edit-idx ((diagram scxmld-diagram) edit-idx)
  "Enable edit idx mode on DIAGRAM's marked drawing if there is one.

Will return non-nil if something has been done meriting a rerender.

Setting force-on-or-off to 'off or 'on will force edit-idx mode
on or off.

Note: if the element's parent is <parallel> this will never
enable edit-idx mode as that's not allowed."
  (with-slots (marked-element) diagram
    (if marked-element
        (if (null edit-idx)
            (progn
              (2dd-set-edit-idx marked-element nil)
              t)
          ;; validate that you can go into edit-idx mode.
          (if (scxmld--parent-is-parallel-p marked-element)
              ;; Parent is a parallel, unable to enter edit-idx mode.
              nil
            ;; parent is non-parallel, you may enter edit-idx mode.
            (2dd-set-edit-idx marked-element edit-idx)
            t))
      nil)))

(cl-defmethod scxmld-toggle-marked-element-edit-idx-mode ((diagram scxmld-diagram) &optional force-on-or-off)
  "Enable edit idx mode on DIAGRAM's marked drawing if there is one.

Will return non-nil if something has been done meriting a rerender.

Setting force-on-or-off to 'off or 'on will force edit-idx mode
on or off.

Note: if the element's parent is <parallel> this will never
enable edit-idx mode as that's not allowed."
  (with-slots (marked-element) diagram
    (if marked-element
        (let ((edit-idx (2dd-get-edit-idx marked-element)))
          (scxmld-set-marked-element-edit-idx
           diagram
           (cond ((eq force-on-or-off 'off)
                  nil)
                 ((eq force-on-or-off 'on)
                  (or edit-idx 0))
                 (edit-idx
                  nil)
                 (t
                  0))))
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
(cl-defmethod scxmld-modify-drawing ((diagram scxmld-diagram) x-scratch y-scratch)
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
              (progn (scxmld-update-drawing diagram marked-element edited-geometry)
                     (scxmld--queue-update-linked-xml diagram marked-element t)
                     t)
            nil))
      nil)))
(cl-defmethod scxmld-modify-attribute ((diagram scxmld-diagram) (attribute-name string) attribute-value)
  "Modify the ATTRIBUTE-NAME'd attribute to be ATTRIBUTE-VALUE of the marked drawing in DIAGRAM.

Setting ATTRIBUTE-VALUE to nil should cause the attribute to be deleted."
  (let ((marked-element (scxmld-get-marked diagram)))
    (if (null marked-element)
        ;; No element, nothing to update
        nil
      ;; Element marked, update the attribute.
      (scxmld-put-attribute marked-element attribute-name attribute-value)
      (scxmld--queue-update-linked-xml diagram marked-element t)
      t)))
(cl-defmethod scxmld-add-child ((diagram scxmld-diagram) (parent scxmld-element) (new-child scxmld-element))
  "Add NEW-CHILD to PARENT in DIAGRAM."
  (let ((success))
    (condition-case err
        (progn
          (scxml-validate-add-child parent new-child)
          ;; if the child comes with a geometry already set, validate that against drawing constraints.
          (when (2dd-geometry new-child)
            ;; Adding a child with geometry to a parallel parent is not allowed.
            ;; parallel children have their geometry managed for them by the parent paralell.
            (when (scxml-parallel-class-p parent)
              (error "Unable to add a drawing with geometry to a parallel element."))
            (unless (2dd-validate-constraints new-child parent (scxml-children parent))
              (error "Child drawing geometry violates drawing constraints.")))
          (scxml-add-child parent new-child t)
          (scxmld--queue-update-linked-xml diagram parent t)
          (setq success t))
      (error (scxmld-log (format "Unable to add child: %s" err) 'error)))
    success))
(cl-defmethod scxmld-delete-element ((diagram scxmld-diagram) (element-to-delete scxmld-element))
  "Delete ELEMENT-TO-DELETE from DIAGRAM"
  (let ((parent (scxml-parent element-to-delete)))
    (if (null parent)
        ;; Can't delete the root scxml
        nil
      ;; If the element has a parent, it can be deleted.
      (scxml-make-orphan element-to-delete)
      (scxmld--queue-update-linked-xml diagram parent t)
      t)))

;; XML update handling.
(cl-defmethod scxmld--queue-update-linked-xml ((diagram scxmld-diagram) (changed-element scxmld-element) &optional include-children)
  "Debounce diagram updates to xml."
  (push `(,changed-element ,include-children) (oref diagram queued-xml-updates))
  (unless (scxmld-get-queued-xml-timer diagram)
    (scxmld-set-queued-xml-timer
     diagram
     (run-with-idle-timer scxmld-diagram-linked-xml-idle
                          nil
                          #'scxmld--run-queued-linked-xml-updates
                          diagram)))
  )
(cl-defmethod scxmld--run-queued-linked-xml-updates ((diagram scxmld-diagram))
  "Run all queued linked xml updates."
  ;; first, take care of the timer.
  (cancel-timer (scxmld-get-queued-xml-timer diagram))
  (scxmld-set-queued-xml-timer diagram nil)

  ;; now, filter updates:
  ;; Update filtering is currently only a repeat compressor.  this
  ;; could be made more intelligent but I think a repeat compressor is
  ;; likely all that is needed.
  (let ((unfiltered-updates (nreverse (scxmld-get-queued-xml-updates diagram)))
        (updates))
    (cl-loop for update in unfiltered-updates
             for update-element = (car update)
             for last-update-element = (car (first updates))
             do (if (eq update-element last-update-element)
                    ;; repeated update - condense them
                    (let ((include-children (cdr update))
                          (last-include-children (cdr (first updates))))
                      (setcdr (first updates)
                              (or include-children last-include-children)))
                  ;; new update, push it on.
                  (push update updates)))
    ;; (message (format "got %d updates down to %d"
    ;;                  (length unfiltered-updates)
    ;;                  (length updates)))
    (mapc (lambda (changed-element-and-include-children)
            (scxmld-update-linked-xml diagram
                                      (car changed-element-and-include-children)
                                      (cdr changed-element-and-include-children)))
          updates)))
(cl-defmethod scxmld-update-linked-xml ((diagram scxmld-diagram) (changed-element scxmld-element) &optional include-children start-at-point)
  "Given a DIAGRAM and a newly updated CHANGED-ELEMENT, update the linked xml buffer.

It is assumed that xmltok has already been initialized for this buffer."
  (let ((buffer (scxmld-get-linked-xml-buffer diagram)))
    (when buffer
      (with-current-buffer buffer
        (unless start-at-point
          (goto-char (point-min)))

        (let ((tag (scxmld--xmltok-find-element changed-element t)))
          (if tag
              (progn
                (scxmld-update-attributes tag (scxml-xml-attributes changed-element))
                (setq tag (scxmld--refresh tag)))

            ;; unable to find element, must add it.
            (let ((parent-range (scxmld--xmltok-find-element
                                 (scxml-parent changed-element))))
              ;; Insert just this parent tag, I'll recurse to add children.
              (setq tag
                    (scxmld-add-child parent-range
                                      (scxml-xml-string changed-element t)
                                      changed-element))))
          (scxmld-mark tag changed-element)

          ;; tag can now be trusted - handle children
          (when include-children
            ;; scan for all child xml-tags and delete any that are missing.
            (let ((child-tags (scxmld-children tag))
                  (child-elements (scxml-children changed-element))
                  (tags-to-prune))
              (cl-loop for child in child-tags
                       for linked-element = (scxmld-get-mark child)
                       when (not (memq linked-element child-elements))
                       do (push child tags-to-prune))
              (when tags-to-prune
                (sort tags-to-prune (lambda (a b)
                                      (> (scxmld-start a) (scxmld-start b))))
                (mapc #'scxmld-delete tags-to-prune))

              ;; update all the children or create them if they're new.
              (cl-loop for child in child-elements
                       ;; TODO: change that last nil back to a t when debugging is easier.
                       do (scxmld-update-linked-xml diagram child t nil)))))))))

;; actions
(cl-defgeneric scxmld-plot ((diagram scxmld-diagram))
  "Plot drawings for this DIAGRAM.")
(cl-defmethod scxmld-plot ((diagram scxmld-diagram))
  "Plot drawings for this DIAGRAM.

PRESERVE-PREDICATE must be a function and will be called
as (preserve-predicate drawing) to determine if the drawing
should be preserved or replotted.

PRESERVE-PREDICATE defaults to preserving all drawings."
  (2dd-plot (2dd-get-root diagram)
            (2dd-get-canvas diagram)
            #'scxml-children
            (lambda (_) t)
            scxmld-plot-settings))
(cl-defgeneric scxmld-update-drawing ((diagram scxmld-diagram) (element scxmld-element) updated-geometry)
  "Update ELEMENT in DIAGRAM to have UPDATED-GEOMETRY.

Return non-nil if update completes.  Update may not complete.")
(cl-defmethod scxmld-update-drawing ((diagram scxmld-diagram) (element scxmld-element) updated-geometry)
  "Update ELEMENT in DIAGRAM to have UPDATED-GEOMETRY.
Return non-nil if update completes, nil otherwise.  Update may
not complete."
  (let ((parent (scxml-parent element))
        (siblings (scxml-siblings element)))
    (2dd-update-plot element
                     updated-geometry
                     #'scxml-children
                     parent
                     siblings)))

(cl-defmethod scxmld-render ((diagram scxmld-diagram))
  "Render the scxmld diagram."
  (2dd-render-all diagram #'scxml-children))
(cl-defmethod scxmld-reset-viewport ((diagram scxmld-diagram))
  "Reset DIAGRAM's viewport to default extents and zoom."
  (2dd-set-viewport diagram
                    (2dd-build-viewport (2dd-get-canvas diagram))))
(cl-defmethod scxmld-initialize-linked-xml-buffer ((diagram scxmld-diagram))
  "Initialize linked buffer in DIAGRAM from the <scxml> elements.

Warning: will destroy all contents of the linked buffer."
  (let ((buffer (scxmld-get-linked-xml-buffer diagram))
        (root-element (2dd-get-root diagram)))
    (when (and buffer root-element)
      (with-current-buffer buffer
        (delete-region (point-min) (point-max))
        (xml-mode)
        (insert (scxml-xml-string root-element))
        (scxmld-relink-xml-buffer diagram)))))
(cl-defmethod scxmld-relink-xml-buffer ((diagram scxmld-diagram))
  "If DIAGRAM has a linked xml buffer, re-tag-link that buffer."
  (let ((root (2dd-get-root diagram))
        (buffer (scxmld-get-linked-xml-buffer diagram)))
    (when (and root buffer)
      (with-current-buffer buffer
        (when (and (boundp 'scxmld-diagram-link) scxmld-diagram-link)
          (error "The buffer this diagram is attempting to link to (%s) already has a linked diagram"
                 buffer))

        (setq-local scxmld-diagram-link diagram)

        (goto-char (point-min))
        (scxmld--xmltok-init)
        ;; scxmld-visit is depth first and the order of the document is depth first
        ;; so let's just go with the assumption that this will work... for now.
        (scxml-visit
         root
         (lambda (element)
           ;; This algorithm is very easy to trick, it only goes by
           ;; element name and order.  TODO: fix that.
           (let ((xml-tag (scxmld--xmltok-find-next-by-name
                           (scxml-xml-element-name element))))
             (unless xml-tag
               (error "Unable to find an XML element for %s"
                      (scxmld--pprint element)))
             (scxmld-mark xml-tag element t))))))))

(cl-defmethod scxmld-find-drawing-selection ((diagram scxmld-diagram) (selection-rect 2dg-rect))
  ;; TODO - I'm pretty sure this should be a defalias?
  (2dd-find-drawing-selection diagram selection-rect #'scxml-children))

(provide 'scxmld-diagram)
