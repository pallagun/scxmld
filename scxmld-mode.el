;;; scxmld-mode --- SCXML Diagram Mode -*- lexical-binding: t -*-;

;;; Commentary:
;; This is the major mode for interacting with diagrams produced of scxml documents

;;; Code:
(require 'scxmld-diagram)
(require 'scxmld-elements)

(defvar-local scxmld--diagram nil
  "Buffer local variable holding the diagram data for rendering.")

(defvar scxmld---debug 't
  "Display or do not display scxml diagram debugging information.")
(defun scxmld-toggle-debug ()
  "Toggle scxmld debugging info on and off."
  (interactive)
  (setq scxmld---debug (not scxmld---debug))
  (message "SCXMLd debuger mode set to %s" scxmld---debug))

(defvar scxmld--debug-var nil
  "when you debug an element this var will be set to that element for further inspection")
  

(defvar scxmld-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-SPC") 'scxmld-mark-at-point)
    (define-key map (kbd "s c") 'scxmld-cycle-mark-at-point)
    (define-key map (kbd "C-M-f") 'scxmld-move-next)
    (define-key map (kbd "C-M-b") 'scxmld-move-prev)

    (define-key map (kbd "d e") 'scxmld-toggle-edit-idx-mode)
    (define-key map (kbd "d s") 'scxmld-simplify)
    (define-key map (kbd "d d") 'scxmld-drawing-debug)
    (define-key map (kbd "d a") 'scxmld-drawing-autoplot)

    (define-key map (kbd "l t") 'scxmld-linking-toggle)

    (define-key map (kbd "M-f") 'scxmld-modify-right)
    (define-key map (kbd "M-b") 'scxmld-modify-left)
    (define-key map (kbd "M-p") 'scxmld-modify-up)
    (define-key map (kbd "M-n") 'scxmld-modify-down)

    (define-key map (kbd "g") 'scxmld-rerender-and-refresh-xml)
    (define-key map (kbd "G") 'scxmld-pan-zoom-reset)
    (define-key map (kbd "+") 'scxmld-zoom-in)
    (define-key map (kbd "-") 'scxmld-zoom-out)

    (define-key map (kbd "C-d") 'scxmld-delete-marked)
    (define-key map (kbd "a S") 'scxmld-add-child-state-to-marked)
    (define-key map (kbd "a F") 'scxmld-add-child-final-to-marked)
    (define-key map (kbd "a T") 'scxmld-add-child-transition-to-marked)
    (define-key map (kbd "a I") 'scxmld-add-child-initial-to-marked)
    (define-key map (kbd "e a") 'scxmld-edit-attribute)
    (define-key map (kbd "e i") 'scxmld-edit-id-attribute)
    (define-key map (kbd "e n") 'scxmld-edit-name-attribute)

    ;; mouse handler routing.
    (define-key map (kbd "<mouse-1>") #'2dd-mouse-handler)
    (define-key map (kbd "<double-mouse-1>") #'2dd-mouse-handler)
    (define-key map (kbd "<down-mouse-1>") #'2dd-mouse-handler)
    (define-key map (kbd "<drag-mouse-1>") #'2dd-mouse-handler)

    (define-key map (kbd "<mouse-2>") #'2dd-mouse-handler)
    (define-key map (kbd "<double-mouse-2>") #'2dd-mouse-handler)
    (define-key map (kbd "<down-mouse-2>") #'2dd-mouse-handler)
    (define-key map (kbd "<drag-mouse-2>") #'2dd-mouse-handler)

    (define-key map (kbd "<mouse-3>") #'2dd-mouse-handler)
    (define-key map (kbd "<double-mouse-3>") #'2dd-mouse-handler)
    (define-key map (kbd "<down-mouse-3>") #'2dd-mouse-handler)
    (define-key map (kbd "<drag-mouse-3>") #'2dd-mouse-handler)

    (define-key map (kbd "<mouse-4>") #'2dd-mouse-handler)
    (define-key map (kbd "<mouse-5>") #'2dd-mouse-handler)

    map)
  "Keymap for scxml-diagram major mode")
(defun scxmld-mode ()
  "Major mode for editing scxml diagrams"
  (interactive)
  (use-local-map scxmld-mode-map)
  (setq major-mode 'scxmld-mode)
  (setq mode-name "SCXMLdiag")
  (setq-local truncate-lines 't)

  ;; setup mouse handlers
  (setq 2dd-mouse-hooks
        '(
          ;; Mouse-1 is the main button for editing and such.
          (down-mouse-1 . scxmld-mouse-mark-at-point)
          (double-mouse-1 . scxmld-mouse-begin-edit-at-pixel)
          (drag-increment-mouse-1 . scxmld-mouse-drag-edit)
          (drag-increment-mouse-2 . scxmld-mouse-pan)
          (drag-mouse-1 . scxmld-mouse-after-edit-at-pixel)

          ;; Mouse 3 is the secondary mouse button
          (mouse-3 . scxmld-mouse-menu)

          ;; Mouse 4/5 are mouse wheel buttons.
          (mouse-4 . scxmld-mouse-zoom-in)
          (mouse-5 . scxmld-mouse-zoom-out)
          (error . scxmld-error))))

(defun scxmld-new-empty-diagram (name)
  "Make a brand new drawing of an empty <scxml> with NAME."
  (interactive "s<scxml> name: ")
  (let* ((root-element (scxmld-scxml :name name))
         (canvas (2dd-canvas- 0 100 0 40))
         (viewport (2dd-build-viewport canvas))
         (diagram (scxmld-diagram :root root-element
                                  :canvas canvas
                                  :viewport viewport))
         (buffer (get-buffer-create (format "*SCXML:%s*" name)))
         (link-buffer (get-buffer-create (format "*scxml*%s.xml" name))))
    (delete-other-windows)
    (switch-to-buffer link-buffer)
    (split-window-right)
    (switch-to-buffer buffer)
    (setq-local scxmld--diagram diagram)
    (scxmld-mode)
    (scxmld-plot diagram)
    (scxmld-render diagram)
    (scxmld-set-linked-xml-buffer diagram link-buffer)
    (scxmld-initialize-linked-xml-buffer diagram)))

(defun scxmld-write-diagram (&optional buffer-or-buffer-name)
  "Write out the <scxml> of the current diagram to a new buffer."
  (interactive)
  (let ((root-element (2dd-get-root scxmld--diagram))
        (buffer (get-buffer-create "*scxmld-diagram-output*")))
    (switch-to-buffer buffer)
    (delete-region (point-min) (point-max))
    (xml-mode)
    (insert (scxml-xml-string root-element))))

(cl-defmethod scxmld-goto-point ((point 2dg-point))
  "Move cursor to where POINT is."
  (scxmld-goto-pixel (2dd-get-pixel (2dd-get-viewport scxmld--diagram)
                                    point)))
(cl-defmethod scxmld-goto-pixel ((pixel 2dg-pixel))
  "Go to PIXEL location on the current buffer."
  (goto-char (point-min))
  (with-slots (x y) pixel
    (let ((additional-lines (forward-line y)))
      (when (> additional-lines 0)
        (insert (mapconcat 'identity (make-list additional-lines "\n") ""))))
    (move-to-column (max 0 x) 't)))
(cl-defmethod scxmld-point-at-pixel ((pixel 2dg-pixel))
  "Go to X Y and return buffer point."
  (scxmld--goto-pixel pixel)
  (point))
(defsubst scxmld--get-pixel-at-point ()
  "Return the pixel at the current point in the buffer."
  (2dg-pixel :x (current-column)
             :y (- (line-number-at-pos) 1)))
(defsubst scxmld--get-selection-rect-at-pixel (pixel)
  "Return the area covered by PIXEL."
  (2dd-get-coord (2dd-get-viewport scxmld--diagram)
                 pixel))
(defsubst scxmld--get-selection-rect-at-point ()
  "Return the area covered by the cursor (a 2dd-rect)."
  (scxmld--get-selection-rect-at-pixel (scxmld--get-pixel-at-point)))

(defmacro scxmld-save-excursion (&rest forms)
  "Execute FORMS and keep the cursor in the same place in the diagram."
  `(let ((position-sym (make-symbol "--pos-sym--"))
         (value-sym (make-symbol "--val-sym--")))
     (setf (symbol-value position-sym) (scxmld--get-pixel-at-point))
     (setf (symbol-value value-sym)
           (progn ,@forms))
     (let* ((marked-element (scxmld-get-marked scxmld--diagram))
            (edit-idx (and (2dd-editable-drawing-class-p marked-element)
                           (2dd-get-edit-idx marked-element))))
       (if edit-idx
           (scxmld-goto-point (2dd-edit-idx-point marked-element edit-idx))
         (scxmld-goto-pixel (symbol-value position-sym))))
     (symbol-value value-sym)))
(defun scxmld-rerender-and-refresh-xml (&optional replot)
  "Rerender (optionally REPLOT) and refresh linked xml."
  (interactive)
  (scxmld-rerender replot)
  (scxmld-update-linked-xml scxmld--diagram
                            (2dd-get-root scxmld--diagram)
                            t))
(defsubst scxmld-rerender (&optional replot)
  "Rerender the current diagram, optionall REPLOT if non-nil."
  (when replot
    (scxmld-plot scxmld--diagram))
  (let ((current-point (point)))
    (delete-region (point-min) (point-max))
    (scxmld-render scxmld--diagram)
    (let* ((marked-element (scxmld-get-marked scxmld--diagram))
           (edit-idx (and (2dd-editable-drawing-class-p marked-element)
                          (2dd-get-edit-idx marked-element))))
      (if edit-idx
          (scxmld-goto-point (2dd-edit-idx-point marked-element edit-idx))
        (goto-char current-point)))))

(defun scxmld-mouse-begin-edit-at-pixel (clicked-pixel &rest drag-info)
  "Mouse hook for a user double clicking on CLICKED-PIXEL.

This will select the element and force it into edit-idx mode.
Additionally it should begin edit-idx selection at the edit-idx
closest to the CLICKED-PIXEL."
  (let* ((selection-area (scxmld--get-selection-rect-at-pixel clicked-pixel))
         (marked-element (scxmld--get-selection-from-area selection-area)))
    (when (2dd-editable-drawing-class-p marked-element)
      (let* ((coordinate-area (2dd-get-coord (2dd-get-viewport scxmld--diagram) ;
                                             clicked-pixel))
             (click-centroid (2dg-centroid coordinate-area))
             (closest-info (2dd-get-closest-edit-idx marked-element click-centroid)))
        (scxmld-set-marked-element-edit-idx scxmld--diagram (car closest-info))
        (scxmld-rerender)))))
(defun scxmld-mouse-after-edit-at-pixel (clicked-pixel last-drag total-drag)
  "Complete a mouse drag edit saga.

It is possible that the drag edit performed may have requested
that a transition drawing was connected to a state/final/parallel
drawing.  If so, complete that connection."
  (when (scxmld-commit-possible-connection scxmld--diagram)
    (scxmld-rerender)))
(defun scxmld-mouse-mark-at-point (clicked-pixel &rest drag-info)
  "Mouse hook for a user clicking on CLICKED-PIXEL.

DRAG-INFO will always be a list of nil as clicks don't carry drag
information."
  (scxmld-mark-at-point))
(defun scxmld-mouse-drag-edit (clicked-pixel last-drag total-drag)
  "Handle a mouse drag event.

CLICKED-PIXEL will contain the current location of the cursor.
Presently this is ignored.

LAST-DRAG will contain the pixel delta of the last update of this
drag event.  This is used to modify the diagram.

TOTAL-DRAG will contain the drag's entire delta from start of the
drag to CURRENT-PIXEL.  Presently this is ignored."
  ;; Note - mouse handlers receive pixel delta information but the
  ;; digram modify function expects scratch coordinate deltas.  I'll
  ;; convert inline here as it's just negating the y component.
  (scxmld-modify (2dg-x last-drag) (* -1 (2dg-y last-drag))))
(defun scxmld-mouse-pan (clicked-pixel last-drag total-drag)
  "Handle a mouse pan event.

Current implementation only regards LAST-DRAG."
  ;; Note: -1 * y coordinate is an inline pixel-to-scratch conversion.
  (scxmld-pan (* -1 (2dg-x last-drag)) (2dg-y last-drag)))
(defun scxmld-mouse-zoom-in (clicked-pixel last-drag total-drag)
  "Handle mouse zoom in requests."
  (scxmld-zoom-in clicked-pixel))
(defun scxmld-mouse-zoom-out (clicked-pixel last-drag total-drag)
  "Handle mouse zoom out requests."
  (scxmld-zoom-out clicked-pixel))
(defun scxmld-mouse-menu (clicked-pixel last-drag total-drag)
  "Handle mouse menu requests.

The mouse menu is element and location sensitive.  When the
CLICKED-PIXEL is within the currently marked element that element
will be used to drive the menu.  When the CLICKED-PIXEL is not
within the marked element, the element at CLICKED-PIXEL will be
used.  The only exception is the root <scxml> element which must
be selected directly."
  (let* ((selection-area (scxmld--get-selection-rect-at-point))
         (selection-element (scxmld--get-selection-from-area selection-area))
         (menu-header "scxmld-mode")
         (refresh-menu (list "Refresh"
                             '("Rerender" . (scxmld-rerender-and-refresh-xml))
                             '("Reset Zoom" . (scxmld-pan-zoom-reset))))
         (add-children-menu)
         (edit-attribute-menu)
         (element-selection-menu))

    (let ((selectable-elements (scxmld-find-drawing-selection scxmld--diagram selection-area)))
      (when selectable-elements
        (setq element-selection-menu
              (cons "Select:"
                    (mapcar (lambda (element)
                              (cons (format "%s: %s"
                                            (scxml-core-type element)
                                            (scxmld-short-name element))
                                    `(scxmld-mark-element ,element)))
                            selectable-elements)))))
    (when selection-element

      (let* ((core-type (scxml-core-type selection-element))
             (valid-children (scxml-get-valid-child-types core-type))
             (valid-attributes (mapcar #'symbol-name
                                       (scxml-get-defined-attributes core-type))))
        (setq menu-header (format "%s menu" core-type))
        (when valid-attributes
          (setq edit-attribute-menu
                (cons "Edit attribute:"
                      (mapcar (lambda (attrib)
                                (cons attrib
                                      `(scxmld-mouse-begin-edit-attribute ,selection-element
                                                                          ,attrib)))
                        valid-attributes))))
        (when valid-children
          (setq add-children-menu
                (cons "Add child:"
                      (append
                       (if (scxml-element-with-initial-class-p selection-element)
                           (list
                            (cons "intial="
                                  `(scxmld-mouse-begin-add-new ,selection-element
                                                               synthetic-initial)))
                         nil)
                       (mapcar (lambda (child-type)
                                 (cons (format "<%s>" child-type)
                                       `(scxmld-mouse-begin-add-new ,selection-element
                                                                    ,child-type)))
                               valid-children)))))))

    (let* ((valid-menus (seq-filter 'identity `(,edit-attribute-menu
                                                ,add-children-menu
                                                ,element-selection-menu
                                                ,refresh-menu)))
           (selection (x-popup-menu t (cons menu-header valid-menus))))
      (when selection
        (apply (first selection) (rest selection))))))
(defun scxmld-mouse-begin-add-new (selected-element type)
  "Begin the new-element mouse saga."
  (let ((is-selected-parallel (scxml-parallel-class-p selected-element)))
    (if (and is-selected-parallel
             (memq type '(state parallel)))
        ;; When adding a <state> or <parallel> to an existing
        ;; <parallel> element the normal mouse saga is not run.
        ;; Instead the new element is just placed into the parent.
        (scxmld-mouse-add-child-to-parallel selected-element type)
      ;; Otherwise, conduct the normal click-add-drag-resize mouse saga
      (2dd-mouse-set-override 'down-mouse-1
                              (lambda (clicked-pixel drag-pixel total-drag)
                                (scxmld-mouse-continue-add-new type clicked-pixel))))))
(defun scxmld-mouse-continue-add-new (type pixel-location)
  ;; TODO - should I have a factory for these?
  (2dd-mouse-clear-override 'down-mouse-1)
  (let* ((marked (scxmld-get-marked scxmld--diagram))
         (selection-area (2dd-get-coord (2dd-get-viewport scxmld--diagram)
                                        pixel-location))
         (type (if (scxmld-synthetic-element-class-p marked)
                   (format "synthetic-%s" type)
                 type))
         (constructor (intern (format "scxmld-%s" type)))
         (new-element (funcall constructor)))
    (unless marked
      (error "No parent found for new element addition"))

    ;; set the geometry to be 1 pixel - the selection area
    ;; TODO - it may not be possible to set from a selection-area.
    (2dd-set-geometry new-element selection-area)
    ;; add the child to the currently marked element.
    (scxmld-diagram-add-child scxmld--diagram marked new-element)
    ;; set the new-element to be the marked element
    (scxmld-set-marked scxmld--diagram new-element)
    ;; maybe you were adding a transition? if so try to put the end of
    ;; the transition at the cursor
    
    
    ;; rerender with a replot to get the new element on the drawing
    (scxmld-rerender t)
    ;; Drop the new element into edit-idx mode if possible
    (cond ((2dd-rect-class-p new-element)
           ;; for a rectangle it's idx #2 (bottom right corner)
           (2dd-set-edit-idx new-element 2))
          ((2dd-link-class-p new-element)
           ;; this is a link, set the edit idx to the last valid idx
           ;; and then move that edit idx to the current pixel
           ;; 
           ;; TODO: this seems like a pretty bad way to do this.  I
           ;; should be able to tell the plotter to lock the end point
           ;; to some pixel before hand (before I even plot it with
           ;; the (scxmld-rerender t) call above) and the plotter
           ;; should figure the rest out for me.
           (let* ((last-idx (1- (2dd-num-edit-idxs new-element)))
                  (last-idx-pt (2dd-edit-idx-point new-element last-idx)))
             (2dd-set-edit-idx new-element last-idx)
             (scxmld--modify-drawing scxmld--diagram
                                     new-element
                                     (2dg-subtract (2dg-centroid selection-area)
                                                   last-idx-pt))
             ;; and now because we've done all this we need to
             ;; rerender _again_ but at least this time we don't have
             ;; to replot.
             (scxmld-rerender)
             ))                         
          ((2dd-editable-drawing-child-p new-element)
           ;; I'm not certain what type of drawing this is, but it's
           ;; editable so select the first edit index
           (2dd-set-edit-idx new-element 0)))
    ))
(defun scxmld-mouse-add-child-to-parallel (selected-element child-type)
  "Add a child <state> or <parallel> to an existing <parallel>"
  (scxmld-set-marked scxmld--diagram
                     selected-element)
  (case child-type
    ('state (call-interactively #'scxmld-add-child-state-to-marked))
    ('parallel (call-interactively #'scmxld-add-child-parallel-to-marked))
    (_ (scxmld-log
        (format "Unknown type for scxmld-mouse-add-child-to-parallel: %s"
                child-type)
        'error))))
(defun scxmld-mouse-begin-edit-attribute (element-to-mark attribute-to-edit)
  "Mark ELEMENT-TO-MARK and begin attribute editing of ATTRIBUTE-TO-EDIT."
  (let ((currently-marked-element (scxmld-get-marked scxmld--diagram)))
    (unless (eq element-to-mark currently-marked-element)
      (scxmld-set-marked scxmld--diagram
                         element-to-mark)
      (scxmld-rerender))
    (scxmld-edit-named-attribute attribute-to-edit)))

(defsubst scxmld--get-selection-from-area (selection-area)
  "Return the element that should be marked if the user selected SELECTION-AREA."
  (let* ((marked-element (scxmld-get-marked scxmld--diagram))
         (selectable-elements (scxmld-find-drawing-selection scxmld--diagram
                                                             selection-area))
         (cycle-list (scxmld--cycle-list selectable-elements)))

    (if (memq marked-element cycle-list)
        marked-element
      (first selectable-elements))))

(defun scxmld--cycle-list (list-of-elements)
  "Accepts a LIST-OF-ELEMENTS from most specific to least
specific and returns a list of cycleable selection elements if
found."
  (let ((first-element (first list-of-elements)))
    (if (and first-element
             (scxmld--parent-is-parallel-p first-element))
        (cl-loop with cycle-list = (list first-element)
                 for next-element in (rest list-of-elements)
                 if (scxmld-parallel-p next-element)
                   do (push next-element cycle-list)
                 else
                   return (nreverse cycle-list)
                 finally return (nreverse cycle-list))
      nil)))
(defun scxmld-cycle-mark-at-point ()
  "If the cursor is in an area where there is a cycle list, cycle it."
  (interactive)
  (scxmld-cycle-mark-at-selection (scxmld--get-selection-rect-at-point)))
(defun scxmld-cycle-mark-at-selection (selection-area)
  "If the cursor is in an area where there is a cycle list, cycle it."
  (let* ((marked-element (scxmld-get-marked scxmld--diagram))
         (selectable-elements (scxmld-find-drawing-selection scxmld--diagram
                                                             selection-area))
         (cycle-list (scxmld--cycle-list selectable-elements)))
    (when cycle-list
      (cl-loop with previous-element = nil
               for element in cycle-list
               when (eq element marked-element)
                 do (let ((element-to-mark (or previous-element
                                               (car (last cycle-list)))))
                      (scxmld-set-marked scxmld--diagram element-to-mark)
                      (cl-return))
               do (setq previous-element element))
      (scxmld-rerender))))
(defun scxmld-mark-element (element)
  "Mark ELEMENT and rerender."
  (scxmld-set-marked scxmld--diagram element)
  (scxmld-rerender))

(defun scxmld-mark-at-point ()
  "Wherever the cursor is, mark what is there.

If there is more than one thing there and one of them is already marked, leave it marked."
  (interactive)
  ;; clear the last element marked cycle tracker.
  (let ((marked-element (scxmld-get-marked scxmld--diagram))
        (selection-area (scxmld--get-selection-rect-at-point)))
    (block scxmld-marking-block
      ;; if there is a selected drawing already and that drawing is in
      ;; edit-idx mode check to see if the user selected any of the
      ;; visible edit-idx pixels.  If so, switch to that edit-idx
      (when (and (2dd-editable-drawing-class-p marked-element)
                 (2dd-get-edit-idx marked-element))
        (let ((edit-idx-points (2dd-edit-idx-points marked-element))
              (clicked-idx))
          (cl-loop for edit-idx-point in edit-idx-points
                   for edit-idx from 0 to (1- (length edit-idx-points))
                   when (2dg-contains selection-area edit-idx-point 'stacked)
                   do (progn
                        (2dd-set-edit-idx marked-element edit-idx)
                        (return-from scxmld-marking-block)))))

      ;; this was not a click on a currently marked elements edit idx
      ;; points.  Select whatever element was at the point.
      (let ((selection (scxmld--get-selection-from-area selection-area)))
        (scxmld-set-marked scxmld--diagram
                           selection)
        (when (2dd-editable-drawing-class-p selection)
          (scxmld-set-marked-element-edit-idx scxmld--diagram nil))))
    (scxmld-rerender)))
(defun scxmld-toggle-edit-idx-mode (&optional force-on)
  "Enable edit idx mode to modify a drawing."
  (interactive)
  (when (scxmld-toggle-marked-element-edit-idx-mode scxmld--diagram
                                                     (if force-on
                                                         'on
                                                       nil))
    (scxmld-rerender)))

(defun scxmld-modify (x-scratch y-scratch)
  (when (scxmld-modify-drawing scxmld--diagram x-scratch y-scratch)
    (scxmld-rerender)))
(defun scxmld-modify-up ()
  (interactive)
  (scxmld-modify 0 1))
(defun scxmld-modify-down ()
  (interactive)
  (scxmld-modify 0 -1))
(defun scxmld-modify-left ()
  (interactive)
  (scxmld-modify -1 0))
(defun scxmld-modify-right ()
  (interactive)
  (scxmld-modify 1 0))
(defun scxmld-simplify ()
  "Whatever drawing is marked, attempt to simplify it."
  (interactive)
  (when (scxmld-simplify-drawing scxmld--diagram)
    (scxmld-rerender)))
(defun scxmld-linking-toggle ()
  "Toggle xml document linking"
  (interactive)
  (let* ((currently-enabled (scxmld-get-linking-enabled scxmld--diagram))
         (new-enabled (not currently-enabled)))
    (scxmld-set-linking-enabled scxmld--diagram new-enabled)
    (message ("Document linking set to %s" new-enabled))))
    
(defun scxmld-drawing-debug ()
  "Whatever drawing is marked, pop up a buffer with debug information."
  (interactive)
  (let ((marked (scxmld-get-marked scxmld--diagram)))
    (setq scxmld--debug-var marked)
    (message (prin1-to-string marked))))
(defun scxmld-drawing-autoplot ()
  "Autoplot the marked drawing if possible."
  (interactive)
  (when (scxmld-autoplot-drawing scxmld--diagram)
    (scxmld-rerender)))

(defun scxmld-move-next ()
  "Whatever is selected, move to the next reasonable thing."
  (interactive)
  (when (scxmld--incf-selection scxmld--diagram 1)
    (scxmld-rerender)))
(defun scxmld-move-prev ()
  "Whatever is selected, move to the next reasonable thing."
  (interactive)
  (when (scxmld--incf-selection scxmld--diagram -1)
    (scxmld-rerender)))

(defsubst scxmld--log-viewport ()
  "Make an info log for the current viewport settings."
  (let* ((viewport (2dd-get-viewport scxmld--diagram))
         (scaling (2dd-scaling viewport))
         (scaling-x (2dg-x scaling))
         (scaling-y (2dg-y scaling)))
    (scxmld-log (format "Current Viewport: X[%.2f -> %.2f], Y[%.2f -> %.2f], zoom ratio: %s"
                        (2dg-x-min viewport)
                        (2dg-x-max viewport)
                        (2dg-y-min viewport)
                        (2dg-y-max viewport)
                        (if (2dg-almost-equal scaling-x scaling-y)
                            (format "%.2f" (/ (+ scaling-x scaling-y) 2.0))
                          (format "%.2f, %.2f" scaling-x scaling-y)))
                'info)))

(defun scxmld-pan (x-scratch y-scratch)
  "Move the viewport by X-SCRATCH, Y-SCRATCH scratch pixels."
  (2dd-pan-scratch (2dd-get-viewport scxmld--diagram) x-scratch y-scratch)
  (scxmld-rerender)
  (scxmld--log-viewport))
(defun scxmld-pan-right ()
  "Move the viewport to the right."
  (interactive)
  (scxmld-pan 1 0))
(defun scxmld-pan-left ()
  "Move the viewport to the left."
  (interactive)
  (scxmld-pan -1 0))
(defun scxmld-pan-up ()
  "Move the viewport up."
  (interactive)
  (scxmld-pan 0 1))
(defun scxmld-pan-down ()
  "Move the viewport down."
  (interactive)
  (scxmld-pan 0 -1))
(defun scxmld-pan-zoom-reset ()
  "Reset the viewport pan and zoom to defaults."
  (interactive)
  (scxmld-reset-viewport scxmld--diagram)
  (scxmld-rerender)
  (scxmld--log-viewport))

(defun scxmld-zoom (alpha)
  "Scale the current viewport zoom by ALPHA."
  (2dd-zoom (2dd-get-viewport scxmld--diagram) alpha)
  (scxmld-rerender)
  (scxmld--log-viewport))
(defun scxmld-zoom-in (&optional pixel)
  "Zoom in on the current viewport, optionally zoom in based on PIXEL.

Note: zooming based on pixel does not yet work."
  (interactive)
  (scxmld-zoom 1.05))
(defun scxmld-zoom-out (&optional pixel)
  "Zoom out on the current viewport, optionally zoom out based on PIXEL.

Note: zooming based on pixel does not yet work."
  (interactive)
  (scxmld-zoom 0.95))

(defun scxmld-delete-marked ()
  "Delete the marked element and all it's child elements."
  (interactive)
  (let ((marked (scxmld-get-marked scxmld--diagram)))
    (when (and marked (scxmld-delete-element scxmld--diagram marked))
      (scxmld-rerender))))
(defun scxmld-add-child-to-marked (new-element)
  "Add NEW-ELEMENT to the currently marked element."
  (when
      (scxmld-diagram-add-child scxmld--diagram
                                (scxmld-get-marked scxmld--diagram)
                                new-element)
    (scxmld-rerender t)))
(defun scxmld-add-child-state-to-marked (id)
  "Add a child state with ID to marked element."
  (interactive "sNew <state> id: ")
  (scxmld-add-child-to-marked (scxmld-state :id id)))
(defun scxmld-add-child-final-to-marked (id)
  "Add a child final with ID to marked element."
  (interactive "sNew <final> id: ")
  (scxmld-add-child-to-marked (scxmld-final :id id)))
(defun scxmld-add-child-transition-to-marked (target-id)
  (interactive "sNew <transition> target: ")
  (let* ((marked-element (scxmld-get-marked scxmld--diagram))
         (child-factory (if (scxmld-synthetic-element-child-p marked-element)
                            'scxmld-synthetic-transition
                          'scxmld-transition)))
    (scxmld-add-child-to-marked
     (funcall child-factory
              :target (if (seq-empty-p (string-trim target-id))
                          nil
                        target-id)))))
(defun scxmld-add-child-initial-to-marked (target-id)
  (interactive "sNew <initial> transition target: ")
  (when (seq-empty-p target-id)
    (error "An <initial> element must have a <transition> with a valid target."))
  (let* ((initial (scxmld-initial))
         (transition (scxmld-transition :target target-id)))
    (scxml-diagram-add-child scxmld--diagram
                             initial
                             transition)
    (scxmld-add-child-to-marked initial)))

(defun scxmld-delete-attribute (attribute-name)
  (interactive "sAttribute Name:")
  (let ((marked (scxmld-get-marked scxmld--diagram)))
    (when marked
      (when (scxmld-modify-attribute scxmld--diagram
                                     attribute-name
                                     nil)
          (scxmld-rerender)))))
(defun scxmld-edit-attribute (attribute-name)
  "Edit the ATTRIBUTE-NAME named attribute of the currently marked element."
  (interactive "sAttribute Name:")
  (scxmld-edit-named-attribute attribute-name))
(defun scxmld-edit-named-attribute (attribute-name)
  "Edit an attribute with ATTRIBUTE-NAME of the currently marked element."
  ;; use read string.
  (let* ((marked (scxmld-get-marked scxmld--diagram))
         (attribute-value (scxmld-get-attribute marked attribute-name)))
    (let ((new-value (read-string (format "set %s= " attribute-name)
                                  attribute-value)))
      (when (not (equal new-value attribute-value))
        (when (scxmld-modify-attribute scxmld--diagram
                                       attribute-name
                                       new-value)
          (scxmld-rerender t))))))
(defun scxmld-edit-id-attribute (new-id)
  (interactive (list
                (read-string "New id: "
                             (scxmld-get-attribute (scxmld-get-marked scxmld--diagram)
                                                   "id"))))
  (scxmld-modify-attribute scxmld--diagram
                           "id"
                           new-id)
  (scxmld-rerender))
(defun scxmld-edit-name-attribute (new-name)
  (interactive (list
                (read-string "New name: "
                             (scxmld-get-attribute (scxmld-get-marked scxmld--diagram)
                                                   "name"))))
  (scxmld-modify-attribute scxmld--diagram
                           "name"
                           new-name)
  (scxmld-rerender))



(defun eieio-object-to-alist (object &optional depth)
  "Given some object, spit out json for it"
  (unless depth
    (setq depth 0))
  (if (not (eieio-object-p object))
      ;; not a class, just return it.
      object
    ;; it's a class, pick it apart.
    (let* ((class (eieio-object-class object))
           (class-name (eieio-class-name class))
           (class-slots (eieio-class-slots class))
           (slot-vals))
      (cl-loop for slot in class-slots
               for name = (cl-struct-slot-value 'cl-slot-descriptor 'name slot)
               for raw-val = (if (slot-boundp object name)
                                 (slot-value object name)
                               '*UNBOUND-SLOT*)
               for val = (if (> depth 0)
                             (eieio-object-to-alist raw-val (1- depth))
                           raw-val)
               do (push (cons name val) slot-vals)
               finally return slot-vals))))



(provide 'scxmld-mode)
;;; scxmld-mode.el ends here
