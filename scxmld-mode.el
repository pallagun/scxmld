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

(defvar scxmld-mode-map
  (let ((map (make-keymap)))
    (define-key map (kbd "C-SPC") 'scxmld-mark-at-point)
    (define-key map (kbd "d") 'scxmld-toggle-edit-idx-mode)
    (define-key map (kbd "C-M-f") 'scxmld-move-next)
    (define-key map (kbd "C-M-b") 'scxmld-move-prev)

    (define-key map (kbd "M-f") 'scxmld-modify-right)
    (define-key map (kbd "M-b") 'scxmld-modify-left)
    (define-key map (kbd "M-p") 'scxmld-modify-up)
    (define-key map (kbd "M-n") 'scxmld-modify-down)

    (define-key map (kbd "g") 'scxmld-rerender)
    (define-key map (kbd "G") 'scxmld-pan-zoom-reset)
    (define-key map (kbd "+") 'scxmld-zoom-in)
    (define-key map (kbd "-") 'scxmld-zoom-out)

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
  (setq 2dd-mouse-hooks '((down-mouse-1 . scxmld-mouse-mark-at-point)
                          (double-mouse-1 . scxmld-mouse-mark-and-begin-edit-at-point)
                          (drag-increment-mouse-1 . scxmld-mouse-drag-edit)
                          (drag-increment-mouse-3 . scxmld-mouse-pan)
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
(defsubst scxmld--get-selection-rect-at-point ()
  "Return the area covered by the cursor (a 2dd-rect)."
  (2dd-get-coord (2dd-get-viewport scxmld--diagram)
                 (scxmld--get-pixel-at-point)))

(defmacro scxmld-save-excursion (&rest forms)
  "Execute FORMS and keep the cursor in the same place in the diagram."
  `(let ((position-sym (make-symbol "--pos-sym--"))
         (value-sym (make-symbol "--val-sym--")))
     (setf (symbol-value position-sym) (scxmld--get-pixel-at-point))
     (setf (symbol-value value-sym)
           (progn ,@forms))
     (let* ((marked-element (scxmld-get-marked scxmld--diagram))
            (edit-idx (and marked-element (2dd-get-edit-idx marked-element))))
       (if edit-idx
           (scxmld-goto-point (2dd-edit-idx-point marked-element edit-idx))
         (scxmld-goto-pixel (symbol-value position-sym))))
     (symbol-value value-sym)))
(defsubst scxmld-rerender ()
  "Rerender (but not replot) the current diagram."
  (interactive)
  (let ((current-point (point)))
    (delete-region (point-min) (point-max))
    (scxmld-render scxmld--diagram)

    (let* ((marked-element (scxmld-get-marked scxmld--diagram))
           (edit-idx (and marked-element (2dd-get-edit-idx marked-element))))
      (if edit-idx
          (scxmld-goto-point (2dd-edit-idx-point marked-element edit-idx))
        (goto-char current-point)))))

(defun scxmld-mouse-mark-and-begin-edit-at-point (clicked-pixel &rest drag-info)
  "Mouse hook for a user double clicking on CLICKED-PIXEL.

This will select the element and force it into edit-idx mode.
Additionally it should begin edit-idx selection at the edit-idx
closest to the CLICKED-PIXEL."
  (scxmld-mark-at-point)
  (let* ((marked-element (scxmld-get-marked scxmld--diagram))
         (coordinate-area (2dd-get-coord (2dd-get-viewport scxmld--diagram)
                                         clicked-pixel))
         (click-centroid (2dg-centroid coordinate-area))
         (closest-info (2dd-get-closest-edit-idx marked-element click-centroid)))
    (2dd-set-edit-idx marked-element (car closest-info))
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
(defun scxmld-mouse-zoom (clicked-pixel last-drag total-drag)
  (message "I wish I could zoom..."))

(defun scxmld-mark-at-point ()
  "Wherever the cursor is, mark what is there."
  (interactive)
  (let ((selection-area (scxmld--get-selection-rect-at-point))
        (marked-element (scxmld-get-marked scxmld--diagram)))
    (block scxmld-marking-block
      ;; if there is a selected drawing already and that drawing is in
      ;; edit-idx mode check to see if the user selected any of the
      ;; visible edit-idx pixels.  If so, switch to that edit-idx
      (when (and marked-element (2dd-get-edit-idx marked-element))
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
      (let ((selected-element (scxmld-find-drawing-selection scxmld--diagram
                                                             selection-area)))
        ;; even if selected-element is nil, still mark it.  That'll
        ;; just clear the mark.
        (scxmld-set-marked scxmld--diagram
                           selected-element)
        (when selected-element
          (2dd-set-edit-idx selected-element nil))))
    (scxmld-rerender)))
(defun scxmld-toggle-edit-idx-mode (&optional force-on)
  "Enable edit idx mode to modify a drawing."
  (interactive)
  (when (scxmld--toggle-marked-element-edit-idx-mode scxmld--diagram
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
(defun scxmld-zoom-in ()
  "Zoom in on the current viewport"
  (interactive)
  (scxmld-zoom 1.05))
(defun scxmld-zoom-out ()
  "Zoom out on the current viewport"
  (interactive)
  (scxmld-zoom 0.95))

(provide 'scxmld-mode)
;;; scxmld-mode.el ends here
