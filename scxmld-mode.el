;;; scxmld-mode --- SCXML Diagram Mode -*- lexical-binding: t -*-;

;;; Commentary:
;; This is the major mode for interacting with diagrams produced of scxml documents

;;; Code:
(require 'scxmld-diagram)
(require 'scxmld-elements)

(defvar scxmld--diagram nil
  "Buffer local variable holding the diagram data for rendering.")
(make-variable-buffer-local 'scxmld--diagram)
(defvar scxmld--last-click-pixel 'nil
  "Holds the location of the last 'clicked' pixel.

This could either be a mouse click or a current cursor position
select.")
(make-variable-buffer-local 'scxmld--last-click-pixel)

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

    map)
  "Keymap for scxml-diagram major mode")
(defun scxmld-mode ()
  "Major mode for editing scxml diagrams"
  (interactive)
  ;; (artist-mode-init)
  ;; (set-syntax-table wpdl-mode-syntax-table)
  (use-local-map scxmld-mode-map)
  ;; (picture-mode)
  (setq major-mode 'scxmld-mode)
  (setq mode-name "SCXMLdiag")
  (setq-local truncate-lines 't)
  ;; (setq truncate-partial-width-windows ????) ; maybe not
  ;; (run-hooks 'scxmld-mode-hook)
  )

(defun scxmld-new-empty-diagram (name)
  "Make a brand new drawing of an empty <scxml> with NAME."
  (interactive "s<scxml> name: ")
  (let* ((root-element (scxmld-scxml :name name))
         (canvas (2dd-canvas- 0 100 0 40))
         (viewport (2dd-build-viewport canvas))
         (diagram (scxmld-diagram :root root-element
                                  :canvas canvas
                                  :viewport viewport))
         (buffer (get-buffer-create (format "*SCXML:%s*" name))))
    (2dd-set-edit-idx root-element 3)
    (switch-to-buffer buffer)
    (setq-local scxmld--diagram diagram)
    (scxmld-mode)
    (scxmld-plot diagram)
    (scxmld-render diagram)))

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
    (move-to-column x 't)))
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
  (let ((current-point (point)))
    (delete-region (point-min) (point-max))
    (scxmld-render scxmld--diagram)

    (let* ((marked-element (scxmld-get-marked scxmld--diagram))
           (edit-idx (and marked-element (2dd-get-edit-idx marked-element))))
      (if edit-idx
          (scxmld-goto-point (2dd-edit-idx-point marked-element edit-idx))
        (goto-char current-point)))))

(defun scxmld-mark-at-point ()
  "Wherever the cursor is, mark what is there."
  (interactive)
  (let* ((selection-area (scxmld--get-selection-rect-at-point))
         (selected-drawing (scxmld-find-drawing-selection scxmld--diagram
                                                          selection-area)))
    (if selected-drawing
        (progn
          (scxmld-set-marked scxmld--diagram selected-drawing)
          (scxmld-rerender))
      (scxmld-message "No element found at location."))))
(defun scxmld-toggle-edit-idx-mode ()
  "Enable edit idx mode to modify a drawing."
  (let ((marked (scxmld-get-marked scxmld--diagram)))
    (when marked
      (let ((edit-idx (2dd-get-edit-idx marked)))
        (2dd-set-edit-idx marked
                          (if edit-idx
                              nil
                            0))
        (scxmld-rerender)))))

(defun scxmld-move-next ()
  "Whatever is selected, move to the next reasonable thing."
  (interactive)
  (let ((marked (scxmld-get-marked scxmld--diagram)))
    (when marked
      (let ((edit-idx (2dd-get-edit-idx marked)))
        (if edit-idx
            ;; An edit indxed is marked, move to the next one.
            (let* ((num-idxs (2dd-num-edit-idxs marked))
                   (next-idx (mod (1+ edit-idx) num-idxs)))
              (2dd-set-edit-idx marked next-idx)
              (scxmld-goto-point (2dd-edit-idx-point marked next-idx)))
          ;; An element is marked, move to the next one.
          (scxmld-message "Implement go-to-next-element functionality.")
          nil))
      (scxmld-rerender))))



(provide 'scxmld-mode)
;;; scxmld-mode.el ends here
