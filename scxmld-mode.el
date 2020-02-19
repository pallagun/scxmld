;;; scxmld-mode --- SCXML Diagram Mode -*- lexical-binding: t -*-;

;;; Commentary:
;; This is the major mode for interacting with diagrams produced of scxml documents

;;; Code:
(require 'scxmld-diagram)
(require 'scxmld-elements)

(defvar scxmld--marked-element 'nil)
(make-variable-buffer-local 'scxmld--marked-element)
(defvar scxmld--last-click-pixel 'nil)
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
    (define-key map (kbd "C-SPC") 'scxmld-mode--mark-at-point)
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
         (diagram (2dd-diagram :root root-element
                               :canvas canvas
                               :viewport viewport))
         (buffer (get-buffer-create (format "*SCXML:%s*" name))))
    (2dd-set-edit-idx root-element 3)
    (switch-to-buffer buffer)
    (scxmld-mode)
    (scxmld-plot diagram)
    (scxmld-render diagram)))

  ;; HERE - go fix scxmld-diagram
  ;; ;; TODO - I don't think I need the IF on the line below.
  ;; ;; (let* ((root-element (if name (scxml-drawable-scxml :name name)
  ;; ;;                        (scxml-drawable-scxml)))
  ;; ;;        (canvas (scxml-canvas :x-min 0.0 :x-max 100.0
  ;; ;;                              :y-min 0.0 :y-max 40.0));; (scxml-canvas--default));
  ;; ;;        (viewport (scxml-build-viewport canvas))
  ;; ;;        (buffer (scxml-draw--get-buffer name)))
  ;; ;;   (scxml--init-buffer buffer)
  ;; ;;   (let ((diagram (scxml-diagram :canvas canvas
  ;; ;;                                 :viewport viewport
  ;; ;;                                 :root root-element
  ;; ;;                                 :display-element root-element
  ;; ;;                                 :buffer buffer)))
  ;; ;;     (scxml-draw diagram)
  ;; ;;     (switch-to-buffer (scxml-buffer diagram)))))
  ;; )


;; (defun scxml-do-diagram (&optional drawing-buffer canvas)
;;   "Make a diagram of whatever buffer you're on."
;;   (interactive)
;;   (when (and drawing-buffer (not (bufferp drawing-buffer)))
;;     (error "Invalid buffer: %s" drawing-buffer))
;;   (let ((buffer (or drawing-buffer
;;                     (scxml-draw--get-buffer (format "%s" (current-buffer)))))
;;         (root-element (scxml-read-buffer nil #'scxml--drawable-element-factory)))
;;     (unless (object-of-class-p root-element 'scxml-scxml)
;;       (error "Unable to parse buffer as <scxml>"))
;;     (let* ((canvas (or canvas (scxml-canvas--default)))
;;            (viewport (scxml-build-viewport canvas)))
;;       (scxml--init-buffer buffer)
;;       (let ((diagram (scxml-diagram :canvas canvas
;;                                     :viewport viewport
;;                                     :root root-element
;;                                     :display-element root-element
;;                                     :buffer buffer)))
;;         (scxml-draw diagram)
;;         (switch-to-buffer (scxml-buffer diagram))))))
;; (defun scxml-do-link (&optional xml-buffer drawing-buffer canvas)
;;   "build a diagram based off the SCXML in xml-buffer and link them."
;;   (interactive)
;;   (when (not xml-buffer)
;;     (setq xml-buffer (current-buffer)))
;;   (unless (bufferp xml-buffer)
;;     (error "Invalid buffer: %s" xml-buffer))
;;   (when (and drawing-buffer (not (bufferp drawing-buffer)))
;;     (error "Invalid buffer: %s" drawing-buffer))
;;   (let ((buffer (or drawing-buffer (scxml-draw--get-buffer (format "%s"
;;                                                                    xml-buffer))))
;;         (root-element (scxml-read-buffer nil #'scxml--drawable-element-factory)))
;;     (unless (object-of-class-p root-element 'scxml-scxml)
;;       (error "Unable to parse buffer as <scxml>"))
;;     (split-window-right)
;;     (let* ((canvas (or canvas (scxml-canvas--default)))
;;            (viewport (scxml-build-viewport canvas)))
;;       (scxml--init-buffer buffer)
;;       (let ((diagram (scxml-diagram :canvas canvas
;;                                     :xml-buffer xml-buffer
;;                                     :viewport viewport
;;                                     :root root-element
;;                                     :display-element root-element
;;                                     :buffer buffer)))
;;         (scxml-draw diagram)
;;         (scxml-link-xml-buffer diagram)
;;         (switch-to-buffer (scxml-buffer diagram))))))

(defmacro scxml-save-excursion (&rest forms)
  "Execute FORMS and keep the cursor in the same place in the diagram."
  `(let ((position-sym (make-symbol "--pos-sym--"))
         (value-sym (make-symbol "--val-sym--")))
     (setf (symbol-value position-sym) (scxml-draw--get-pixel-at-point))
     (setf (symbol-value value-sym)
           (progn ,@forms))
     (scxml-draw--goto-pixel
      (let ((edit-idx (scxmld-mode--edit-idx)))
        (if edit-idx
            (let* ((drawing (scxml-element-drawing scxmld-mode--marked-element))
                   (edit-idx-point (scxml-edit-idx-point drawing edit-idx)))
              (scxml-get-pixel (scxmld-mode--viewport)
                               edit-idx-point))
          (symbol-value position-sym))))
     (symbol-value value-sym)))

;; Helper functions
;; TODO - do I actually need these helper functions?
;; (defun scxmld-mode--canvas ()
;;   "Grab the canvas"
;;   (scxml-diagram-canvas scxml-draw--diagram))
;; (defun scxmld-mode--viewport ()
;;   "Grab the viewport"
;;   (scxml-diagram-viewport scxml-draw--diagram))
;; (defun scxmld-mode--display-element ()
;;   "Grab the element to display"
;;   (scxml-diagram-display-element scxml-draw--diagram))
;; (defun scxmld-mode--root ()
;;   "Grab the root of the display diagram"
;;   (scxml-diagram-root scxml-draw--diagram))
;; (defun scxmld-mode--marked-drawing ()
;;   "Get marked element's drawing if it exists"
;;   (if scxmld-mode--marked-element
;;       (scxml-element-drawing scxmld-mode--marked-element)
;;     'nil))
;; (defun scxmld-mode--edit-idx ()
;;   "Is the marked element in edit-mode?"
;;   (let ((element scxmld-mode--marked-element))
;;     (and element (scxml--edit-idx element))))

;; (defun scxmld-mode--next-element ()
;;   "Head to the next thing"
;;   (interactive)
;;   (scxml-record 'scxmld-mode--next-element)
;;   (when scxmld-mode--marked-element
;;     (if (scxmld-mode--edit-idx)
;;         (scxmld-mode--edit-idx-next)
;;       (scxmld-mode--mark-next))))
;; (defun scxmld-mode--prev-element ()
;;   "Head to the previous thing"
;;   (interactive)
;;   (scxml-record 'scxmld-mode--prev-element)
;;   (when scxmld-mode--marked-element
;;     (if (scxmld-mode--edit-idx)
;;         (scxmld-mode--edit-idx-prev)
;;       (scxmld-mode--mark-prev))))
;; (defun scxmld-mode--descend-element ()
;;   "Head to the first child of the thing"
;;   (interactive)
;;   (scxml-record 'scxmld-mode--descend-element)
;;   (when (and scxmld-mode--marked-element
;;              (not (scxmld-mode--edit-idx)))
;;     (scxmld-mode--mark-first-child)))
;; (defun scxmld-mode--ascend-element ()
;;   "Head to the parent of the thing."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--ascend-element)
;;   (when scxmld-mode--marked-element
;;     (if (scxmld-mode--edit-idx)
;;         (scxmld-mode--disable-edit-mode)
;;       (scxmld-mode--mark-parent))))
;; (defun scxmld-mode--modify-larger (&optional increment)
;;   "Modify currently focused thing to be larger, optionall by INCREMENT.

;; Currently only able to zoom in when in viewport mode."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--modify-larger increment)
;;   (when (eq scxmld-mode--mouse-mode 'viewport)
;;     (scxmld-mode--zoom-in increment)))

;; (defun scxmld-mode--modify-smaller (&optional increment)
;;   "Modify currently focused thing to be smaller, optionall by INCREMENT.

;; Currently only able to zoom out when in viewport mode."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--modify-smaller increment)
;;   (when (eq scxmld-mode--mouse-mode 'viewport)
;;     (scxmld-mode--zoom-out increment)))

;; (defun scxmld-mode--modify (move-vector)
;;   "Modify the selected drawing element by move-vector"
;;   (if (eq scxmld-mode--mouse-mode 'viewport)
;;       ;; You're in viewport mode, modify the viewport.
;;       (let ((flipped (2dg-additive-inverse move-vector)))
;;         (scxmld-mode--pan (2dg-x flipped) (2dg-y flipped)))
;;     ;; else, normal view/edit mode.
;;     (when scxmld-mode--marked-element
;;       (unless (2dg-point-p move-vector)
;;         (error "Must supply an 2dg-point as MOVE-VECTOR"))
;;       (scxml-save-excursion
;;        (scxmld-mode--move move-vector)))))
;; (defun scxmld-mode--modify-right ()
;;   "Modify rightward"
;;   (interactive)
;;   (scxml-record 'scxmld-mode--modify-right)
;;   (scxmld-mode--modify (2dg-point :x 1.0 :y 0.0)))
;; (defun scxmld-mode--modify-left ()
;;   "Modify leftward"
;;   (interactive)
;;   (scxml-record 'scxmld-mode--modify-left)
;;   (scxmld-mode--modify (2dg-point :x -1.0 :y 0.0)))
;; (defun scxmld-mode--modify-up ()
;;   "Modify upward"
;;   (interactive)
;;   (scxml-record 'scxmld-mode--modify-up)
;;   (scxmld-mode--modify (2dg-point :x 0.0 :y 1.0)))
;; (defun scxmld-mode--modify-down ()
;;   "Modify downward"
;;   (interactive)
;;   (scxml-record 'scxmld-mode--modify-down)
;;   (scxmld-mode--modify (2dg-point :x 0.0 :y -1.0)))

;; (defun scxmld-mode--redraw ()
;;   "Redraw the screen."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--redraw)
;;   (save-excursion
;;     ;; (let ((start (float-time)))
;;       (scxml-draw scxml-draw--diagram)
;;       ;; (let ((duration-ms (- (float-time) start)))
;;       ;;   (message "Render time: %s ms" duration-ms))
;;       (when scxmld-mode--debug
;;         (scxmld-mode--debug-barf))))

;; (defvar scxml-test-counter 0)
;; (defun scxmld-mode--clear-mouse-hooks ()
;;   "Clear out all mouse sequence hooks."
;;   (setf scxmld-mode--mark-element-catch nil
;;         scxmld-mode--down-mouse-1-catch nil
;;         scxmld-mode--up-mouse-1-catch nil))

;; (defun scxmld-mode--mouse-handler (event)
;;   "Handle any arbitrary non-movement mouse event"
;;   ;; TODO - Not sure if I'm approaching mouse handling properly.
;;   (interactive "e")
;;   ;; (message "Random mouse event: %s" event)
;;   (incf scxml-test-counter)
;;   (let ((bubble-up-error)
;;         (current-window (first (second event))))
;;     (cl-flet ((pixel-from-event
;;                (event)
;;                ;; If you're not in the current window (where the event was started), do not produce a pixel.
;;                (when (eq (first (second event)) current-window)
;;                  (let ((col-row-cell (first (nthcdr 6 (second event)))))
;;                    (2dg-pixel :x (car col-row-cell)
;;                                 :y (cdr col-row-cell))))))
;;       (let ((event-type (car event)))
;;         ;; mouse down handlers
;;         ;; (message "(%s event-type %s)" (gensym) event-type)
;;         (when (or (eq event-type 'down-mouse-1)
;;                   (eq event-type 'down-mouse-2)
;;                   (eq event-type 'down-mouse-3))
;;           (message "%s down" scxml-test-counter)
;;           (mouse-set-point event)
;;           (let* ((start-pixel (pixel-from-event event))
;;                  (last-pixel start-pixel)
;;                  (event-count 0))
;;             (setq scxmld-mode--last-click-pixel start-pixel)

;;             (when (and scxmld-mode--down-mouse-1-catch
;;                        (eq event-type 'down-mouse-1))
;;               ;; if a mouse-down event causes an error and aborts this
;;               ;; function the error will be displayed.  At that point
;;               ;; when the mouse is released another event will be
;;               ;; generated (a mouse click event, which I'm starting to
;;               ;; understand is a bit more of a mouse-up event).  That
;;               ;; mouse-click(mouse-up) event is another event and will
;;               ;; clear the error displayed in the minibuffer.
;;               ;; However, I wish to show the error message from the
;;               ;; mouse-down event in the minibuffer after the mouse is
;;               ;; released.  Therefore I will catch any errors from the
;;               ;; mouse-down event here, allow the (track-mouse) form
;;               ;; below to catch all mouse movement and even the
;;               ;; mouse-click (mouse-up) event later.  After all mouse
;;               ;; interactions I will then display the error.
;;               ;;
;;               ;; So
;;               ;; - mouse down
;;               ;; -  error generated -> capture error
;;               ;; - enter (track-mouse) form which takes no actions and simply absorbs mouse events
;;               ;; -  when that (track-mouse) form detects a click (mouse-up) exit.
;;               ;; - Execute no functionality but finally signal the error captured.
;;               (condition-case caught-error
;;                   (progn
;;                     (funcall scxmld-mode--down-mouse-1-catch start-pixel)
;;                     (setq scxmld-mode--down-mouse-1-catch nil))
;;                 (error (progn
;;                          (scxmld-mode--clear-mouse-hooks)
;;                          (setq bubble-up-error (second caught-error)))))
;;               (setq event-count 1)

;;               ;; (let ((hook scxmld-mode--down-mouse-1-catch))
;;               ;;   (setq scxmld-mode--down-mouse-1-catch nil)
;;               ;;   (message "%s prehook" scxml-test-counter)
;;               ;;   (funcall hook start-pixel)
;;               ;;   (setq event-count 1))

;;               ;; (unwind-protect
;;               ;;     (progn
;;               ;;       (funcall scxmld-mode--down-mouse-1-catch start-pixel)
;;               ;;       ;; Skip the initial mouse down behavior.
;;               ;;       (setq event-count 1))
;;               ;;   (setq scxmld-mode--down-mouse-1-catch nil))

;;               )
;;             ;; (message "Mouse Down px: %s, %s"
;;             ;;          (scxml-print start-pixel)
;;             ;;          (scxml-print last-pixel))
;;             (if bubble-up-error
;;                 (track-mouse
;;                   ;; This is the do-nothing and absorb mouse events (mouse-track) form.
;;                   (while (and (setq event (read-event))
;;                               (mouse-movement-p event))
;;                     (incf event-count)))
;;               (track-mouse
;;                 ;; real mouse track-mouse form - no errors have yet happened.
;;                 (while (and (setq event (read-event))
;;                             (mouse-movement-p event))
;;                   ;; ok, you've started moving....
;;                   (when (and (not (eq scxmld-mode--mouse-mode 'viewport))
;;                              (eq 0 event-count))
;;                     (scxmld-mode--mark-at-point)) ;mark whatever was where you first clicked, get ready to try and move it.
;;                   (incf event-count)
;;                   ;; (message "event count: %s" event-count)
;;                   (let* ((current-pixel (pixel-from-event event)))
;;                     ;; Only process when the 'pixel' changes.  That's the smallest unit of distance a user can change something by
;;                     (when (and current-pixel ;pixel must be valid and exist (it won't exist if you leave the window)
;;                                (not (equal current-pixel last-pixel)))
;;                       (let* ((current-delta (2dg-subtract current-pixel last-pixel))
;;                              (total-delta (2dg-subtract current-pixel start-pixel))
;;                              (start (scxml-get-coord-centroid (scxmld-mode--viewport) last-pixel))
;;                              (end (scxml-get-coord-centroid (scxmld-mode--viewport) current-pixel))
;;                              (delta (2dg-subtract end start)))

;;                         ;; (message "delta pixel raw: %s" (2dg-subtract end start))
;;                         ;; (message "Mouse Event[%d]: start: %s, delta %s, t-delta %s, dir %s"
;;                         ;;          event-count
;;                         ;;          (scxml-print start-pixel)
;;                         ;;          (scxml-print current-delta)
;;                         ;;          (scxml-print total-delta)
;;                         ;;          (scxml--direction-name  (2dg-coarse-direction current-delta)))
;;                         ;; (message "delta: %s" (scxml-print delta))
;;                         ;; (message "type: %s" event-type)
;;                         (if (eq scxmld-mode--mouse-mode 'viewport)
;;                             ;; viewport mode can pan or zoom
;;                             (cond ((eq event-type 'down-mouse-1)
;;                                    (scxmld-mode--pan (* -1 (2dg-x delta))
;;                                                             (* -1 (2dg-y delta))))
;;                                   ((eq event-type 'down-mouse-3)
;;                                    (when (>= (abs (2dg-y delta)) (abs (2dg-x delta)))
;;                                      (scxmld-mode--zoom (if (> (2dg-y delta) 0)
;;                                                                    0.95
;;                                                                  1.05))))
;;                                   (t
;;                                    (error "Unknown mouse event type: %s" event-type)))
;;                           ;; default mouse mode can only modify.
;;                           (scxmld-mode--modify delta)))
;;                       (setq last-pixel current-pixel))))
;;                 (when (and (eq (car event) 'drag-mouse-1)
;;                            scxmld-mode--up-mouse-1-catch)
;;                   ;; drag-mouse-1 is complete which means you must have let go of the
;;                   ;; mouse button.
;;                   (unwind-protect
;;                       (funcall scxmld-mode--up-mouse-1-catch last-pixel)
;;                     (setq scxmld-mode--up-mouse-1-catch nil)))

;;                 ;; (message "Exit mouse event: %s" event)
;;                 ))
;;             (setq event-type (car event))))

;;         ;; handle a click event (not a drag or down event)
;;         ;; (message "%s Handle Event: %s" scxml-test-counter event)
;;         (setq scxmld-mode--last-click-pixel (pixel-from-event event))
;;         (if bubble-up-error
;;             (error bubble-up-error)
;;           (if (eq scxmld-mode--mouse-mode 'viewport)
;;               ;; viewport mode - currently don't do anything
;;               nil
;;             (progn
;;               (mouse-set-point event)
;;               (cond
;;                ((eq event-type 'mouse-1)
;;                 (scxml-record 'goto-char (point))
;;                 (scxml-record 'scxmld-mode--mark-at-point)
;;                 (scxmld-mode--mark-at-point))
;;                ((eq event-type 'double-mouse-1)
;;                 (scxml-record 'goto-char (point))
;;                 (scxml-record 'scxmld-mode--mark-at-point t)
;;                 (scxmld-mode--mark-at-point t))
;;                ((eq event-type 'mouse-3)
;;                 (scxmld-mode--show-mouse-menu))))))))))
;; (defun scxmld-mode--show-mouse-menu ()
;;   "Show a menu at mouse location dependent on what is under the pointer."
;;   (let ((add-box-menu '(""
;;                         ("New <state>" . (new . scxml-drawable-state))
;;                         ("New <parallel>" . (new . scxml-drawable-parallel))
;;                         ("New <final>" . (new . scxml-drawable-final))
;;                         ("New <initial>" . (new . scxml-drawable-initial))
;;                         ("Set initial=\"\"" . (new . scxml-drawable-synthetic-initial)))))
;;     (let* ((selection (x-popup-menu t (list "" add-box-menu)))
;;            (command (car selection))
;;            (argument (cdr selection)))
;;       (cond ((eq command 'new)
;;              (scxmld-mode--begin-add-child-element-with-mouse argument))
;;             (t
;;              ;; handle no selection?
;;              nil)))))
;; (defun scxmld-mode--toggle-mouse-mode ()
;;   (interactive)
;;   (scxml-record 'scxmld-mode--toggle-mouse-mode)
;;   (setq scxmld-mode--mouse-mode
;;         (if (eq scxmld-mode--mouse-mode 'viewport)
;;             nil
;;           'viewport))
;;   (message "Toggle mouse mode to: %s"
;;            (or scxmld-mode--mouse-mode 'default)))

;; (defun scxmld-mode--pan-left (&optional delta)
;;   (interactive)
;;   (scxml-record 'scxmld-mode--pan-left)
;;   (scxmld-mode--pan (- (or delta 1)) 0))
;; (defun scxmld-mode--pan-right (&optional delta)
;;   (interactive)
;;   (scxml-record 'scxmld-mode--pan-right)
;;   (scxmld-mode--pan (or delta 1) 0))
;; (defun scxmld-mode--pan-up (&optional delta)
;;   (interactive)
;;   (scxml-record 'scxmld-mode--pan-up)
;;   (scxmld-mode--pan 0 (or delta 1)))
;; (defun scxmld-mode--pan-down (&optional delta)
;;   (interactive)
;;   (scxml-record 'scxmld-mode--pan-down)
;;   (scxmld-mode--pan 0 (- (or delta 1))))
;; (defun scxmld-mode--pan (delta-scratch-x delta-scratch-y)
;;   "Pan display by DELTA-SCRATCH-X, DELTA-SCRATCH-Y pixel in scratch coordinates."
;;   (oset scxml-draw--diagram
;;         viewport
;;         (scxml-pan-scratch (scxml-diagram-viewport scxml-draw--diagram)
;;                            (round delta-scratch-x)
;;                            (round delta-scratch-y)))
;;   (scxmld-mode--redraw))

;; (defun scxmld-mode--zoom-reset ()
;;   "Reset viewport to be exactly the display element canvas"
;;   (interactive)
;;   (scxml-record 'scxmld-mode--zoom-reset)
;;   (oset scxml-draw--diagram
;;         viewport
;;         (scxml-build-viewport (scxmld-mode--canvas)))
;;   (scxmld-mode--redraw))
;; (defun scxmld-mode--zoom-in (&optional ratio)
;;   "Zoom in by RATIO, defaulting to 10%"
;;   (interactive)
;;   (scxml-record 'scxmld-mode--zoom-in)
;;   (scxmld-mode--zoom (+ 1.0 (or ratio 0.1))))
;; (defun scxmld-mode--zoom-out (&optional ratio)
;;   "Zoom out by RATIO, defaulting to 10%"
;;   (interactive)
;;   (scxml-record 'scxmld-mode--zoom-out)
;;   (scxmld-mode--zoom (- 1.0 (or ratio 0.1))))
;; (defun scxmld-mode--zoom (alpha)
;;   "Zoom the viewport by alpha"
;;   (scxml-zoom (scxml-diagram-viewport scxml-draw--diagram) alpha)
;;   (scxmld-mode--redraw))

;; (defun scxmld-mode--set-root-canvas-size (columns lines)
;;   "Reset the size of the root canvavs to be COLUMNS by LINES characters."
;;   (interactive
;;    (let* ((margin 2)
;;           (current-canvas (scxmld-mode--canvas))
;;           (default-width (- (window-body-width) margin))
;;           (default-height (- (window-height nil 'floor) margin))
;;           (current-width (round (2dg-width current-canvas)))
;;           (current-height (round (2dg-height current-canvas)))
;;           (width-prompt (format "Width (current:%d, default:%d): " current-width default-width))
;;           (height-prompt (format "Height (current:%d, default:%d): " current-height default-height)))
;;      (list (read-string width-prompt nil nil default-width)
;;            (read-string height-prompt nil nil default-height))))
;;   (scxml-record 'scxmld-mode--set-root-canvas-size columns lines)
;;   (when (not (numberp columns))
;;     (setq columns (string-to-number columns)))
;;   (when (not (numberp lines))
;;     (setq lines (string-to-number lines)))
;;   (let ((canvas (scxmld-mode--canvas)))
;;     (setf (2dg-x-max canvas) (+ columns (2dg-x-min canvas))
;;           (2dg-y-max canvas) (+ lines (2dg-y-min canvas)))
;;     (scxmld-mode--redraw)))

;; (defun scxmld-mode--unmark-all (&optional do-redraw)
;;   "Unmark all elements"
;;   (let ((element (scxmld-mode--display-element)))
;;     (when element
;;       (scxml-visit
;;        element
;;        (lambda (e) (scxml--set-highlight e 'nil))
;;        (lambda (e) (object-of-class-p e 'scxml-drawable-element)))
;;       (when do-redraw (scxmld-mode--redraw)))))
;; (defun scxmld-mode--mark-next ()
;;   "whatever is marked, mark the next one at the same level"
;;   ;; TODO - refactor this, it's almost the same as mark-prev
;;   (interactive)
;;   (scxml-record 'scxmld-mode--mark-next)
;;   (let* ((parent (scxml-parent scxmld-mode--marked-element))
;;          (children (scxml-children parent))
;;          (current-element scxmld-mode--marked-element)
;;          (next-element 'nil)
;;          (found-current 'nil))
;;     (mapc (lambda (child)
;;             (when (and (null next-element)
;;                        found-current)
;;               (setq next-element child))
;;             (when (eq current-element child)
;;               (setq found-current 't)))
;;           children)
;;     (when (null next-element)
;;       (setq next-element (car children)))
;;     (scxmld-mode--mark-element next-element)))
;; (defun scxmld-mode--mark-prev ()
;;   "Whatever is marked, mark the previous one at the same level"
;;   (interactive)
;;   (scxml-record 'scxmld-mode--mark-prev)
;;   (let* ((parent (scxml-parent scxmld-mode--marked-element))
;;          (children (scxml-children parent))
;;          (current-element scxmld-mode--marked-element)
;;          (prev-element 'nil)
;;          (last-element 'nil))
;;     (mapc (lambda (child)
;;             (when (eq current-element child)
;;               (setq prev-element last-element))
;;             (setq last-element child))
;;           children)
;;     (when (null prev-element)
;;       (setq prev-element (nth (- (length children) 1) children)))
;;     (scxmld-mode--mark-element prev-element)))
;; (defun scxmld-mode--mark-first-child ()
;;   "Whatever is marked, mark the first child of it"
;;   (interactive)
;;   (scxml-record 'scxmld-mode--mark-first-child)
;;   (let ((children (scxml-children scxmld-mode--marked-element)))
;;     (if children
;;         (scxmld-mode--mark-element (car children))
;;       (message "No children to mark"))))
;; (defun scxmld-mode--mark-parent ()
;;   "Whatever is marked, mark the parent of it"
;;   (interactive)
;;   (scxml-record 'scxmld-mode--mark-parent)
;;   (let* ((parent (scxml-parent scxmld-mode--marked-element)))
;;     (when parent
;;       (scxmld-mode--mark-element parent))))
;; (defun scxmld-mode--mark-at-point (&optional double-mark)
;;   "Mark whatever your cursor is on.

;; When DOUBLE-MARK is non-nil mark sub elements of the element
;; marked.  DOUBLE-MARK should be though of as a double click.  Two
;; single mark events on the same thing should be roughly the same
;; as a single DOUBLE-MARK event.

;; If no elements are marked, attempt to mark thing at point.
;; If an element is already marked _and_ in edit-mode then see if
;; the user is attempting to mark an edit idx."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--mark-at-point)
;;   (let* ((pixel (scxml-draw--get-pixel-at-point))
;;          (viewport (scxmld-mode--viewport))
;;          (drawing-coord (scxml-get-coord viewport pixel)))
;;     (block scxml--found
;;       ;; first, if a current element is marked and you're
;;       ;; double marking, prefer that element's edit idxs.
;;       (when (and (scxmld-mode--edit-idx)
;;                  scxmld-mode--marked-element)
;;         (cl-loop with edit-pts = (scxml-edit-idx-points
;;                                   (scxmld-mode--marked-drawing))
;;                  for edit-pt in edit-pts
;;                  for edit-idx from 0 to (1- (length edit-pts))
;;                  if (2dg-contains drawing-coord edit-pt 'stacked)
;;                  do (progn
;;                       (scxml--set-edit-idx scxmld-mode--marked-element edit-idx)
;;                       (return-from scxml--found))))
;;       (let ((element (scxmld-mode--get-element drawing-coord)))
;;         (unless (eq scxmld-mode--marked-element element)
;;           (scxmld-mode--mark-element element t))

;;         ;; when using-double mark - bounce to edit idx marking.
;;         (when (or (scxmld-mode--edit-idx)
;;                   double-mark)
;;           (let ((edit-pts (scxml-edit-idx-points (scxmld-mode--marked-drawing))))
;;             (when edit-pts
;;               ;; mark the edit-idx you clicked on or whichever one you're closest to.
;;               (cl-loop with best-idx = 0
;;                        with best-distance = 999999
;;                        with best-pt = nil
;;                        for edit-pt in edit-pts
;;                        for edit-idx from 0 to (1- (length edit-pts))
;;                        if (2dg-contains drawing-coord edit-pt 'stacked)
;;                        do (progn
;;                             (scxml--set-edit-idx element edit-idx)
;;                             (setq pixel (scxml-get-pixel viewport edit-pt))
;;                             (cl-return))
;;                        else
;;                        do (let ((distance (2dg-distance (2dg-centroid drawing-coord)
;;                                                           edit-pt)))
;;                             (when (< distance best-distance)
;;                               (setq best-distance distance)
;;                               (setq best-pt edit-pt)
;;                               (setq best-idx edit-idx)))
;;                        finally (if double-mark
;;                                    (progn
;;                                      (scxml--set-edit-idx element best-idx)
;;                                      (setq pixel (scxml-get-pixel viewport best-pt)))
;;                                  (scxml--set-edit-idx element))))))))
;;     (scxmld-mode--redraw)
;;     (scxml-draw--goto-pixel pixel)))
;; (defun scxmld-mode--mark-element (element &optional do-not-redraw)
;;   "Mark the ELEMENT specified and redraw the display!"
;;   (if scxmld-mode--mark-element-catch
;;       ;; Mark element catch
;;       (unwind-protect
;;           (funcall scxmld-mode--mark-element-catch element)
;;         (setq scxmld-mode--mark-element-catch nil))
;;     ;; normal mark element
;;     (scxmld-mode--unmark-all)
;;     (setq scxmld-mode--marked-element element)
;;     (scxml--set-highlight element 't)

;;     (when (not do-not-redraw)
;;       (scxmld-mode--redraw))

;;     (when scxmld-mode--debug
;;       (message "Marking %s" (scxml-print element)))))

;; (defun scxmld-mode--move (move-vector)
;;   "Whatever edit-idx you're at (or not at), move it by MOVE-VECTOR."
;;   ;; TODO - should this be a cl-defmethod?
;;   (unless (2dg-point-p move-vector)
;;     (error "Must supply a 2dg-point to specify move vector"))
;;   ;; pass in viewport here, it has to get all the way to drawings.
;;   (scxml--modify-drawing-hint scxmld-mode--marked-element
;;                               move-vector
;;                               (scxmld-mode--viewport))
;;   (scxmld-mode--apply-edit scxmld-mode--marked-element)
;;   (scxmld-mode--redraw))
;; (defun scxmld-mode--simplify ()
;;   "Simplify the marked drawing if possible."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--simplify)
;;   (when scxmld-mode--marked-element
;;     (let ((past-edit-idx (scxml--edit-idx scxmld-mode--marked-element)))
;;       (scxml-simplify-drawing scxmld-mode--marked-element
;;                               (scxmld-mode--viewport))
;;       (scxml--set-edit-idx scxmld-mode--marked-element nil t)
;;       ;; TODO - don't redraw the whole thing, just the marked elemnt's drawing
;;       ;; to get a reliable num-edit-idxs.
;;       (scxmld-mode--redraw)
;;       (when past-edit-idx
;;         (let* ((num-edit-idxs (scxml-num-edit-idxs scxmld-mode--marked-element))
;;                (correct-idx (min past-edit-idx (1- num-edit-idxs))))
;;           (scxml--set-edit-idx scxmld-mode--marked-element 0)
;;           (scxmld-mode--edit-idx-increment correct-idx))))))

;; (defun scxmld-mode--automatic ()
;;   "Set the marked element and all siblings to 'automatic' mode (not manually hinted)."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--automatic)
;;   (when scxmld-mode--marked-element
;;     ;; TODO - this should ensure that no collisions occur when
;;     ;; toggling to automatic mode.  It'll do that now by making
;;     ;; all the siblings go to automatic as well.
;;     (scxml--set-edit-idx scxmld-mode--marked-element nil)
;;     (let ((parent (scxml-parent scxmld-mode--marked-element)))
;;       ;; If you find a parent invalid all siblings, otherwise just you.
;;       (if parent
;;           (mapc (lambda (sibling)
;;                   (scxml--set-hint sibling nil)
;;                   (scxml--set-drawing-invalid sibling t)
;;                   (scxmld-mode--apply-edit sibling))
;;                 (scxml-children parent))
;;         (scxml--set-hint scxmld-mode--marked-element nil)
;;         (scxml--set-drawing-invalid scxmld-mode--marked-element t)
;;         (scxmld-mode--apply-edit scxmld-mode--marked-element)))
;;     (scxmld-mode--redraw)))
;; (defun scxmld-mode--all-automatic ()
;;   "Set every drawing to 'automatic' mode (not manually hinted)."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--all-automatic)
;;   (let ((root (scxmld-mode--root)))
;;     (scxml-visit-all root
;;                      (lambda (element)
;;                        (scxml--set-edit-idx element nil)
;;                        (scxml--set-hint element nil)
;;                        (scxml--set-drawing-invalid element t))
;;                      (lambda (element)
;;                        (object-of-class-p element 'scxml-drawable-element)))
;;     (scxmld-mode--apply-edit root t))
;;   (scxmld-mode--redraw))

;; (defun scxmld-mode--move-to-edit-idx (drawing)
;;   "Move to the currently selected edit-idx of the DRAWING if it is set."
;;   (when (and drawing (scxml-drawing-edit-idx drawing))
;;     (let* ((point (scxml-edit-idx-point (scxmld-mode--marked-drawing)
;;                                         (scxmld-mode--edit-idx)))
;;            (pixel (scxml-get-pixel (scxmld-mode--viewport) point)))
;;       (scxml-draw--goto-pixel pixel))))

;; (defun scxmld-mode--disable-edit-mode ()
;;   ;; TODO - can this function be removed?
;;   "Disable edit mode, if you're in edit mode mark the parent."
;;   (when (null scxmld-mode--marked-element)
;;     (error "Unable to un-edit drawing, no selection"))
;;   (scxml-save-excursion
;;    (scxml--set-edit-idx scxmld-mode--marked-element 'nil)
;;    (scxmld-mode--redraw)))
;; (defun scxmld-mode--enable-edit-mode ()
;;   ;; TODO - can this function be removed?
;;   "If you have a marked drawing, enter edit mode."
;;   (when (null scxmld-mode--marked-element)
;;     (error "Unable to edit drawing, no element marked"))
;;   (if (> (scxml-num-edit-idxs (scxmld-mode--marked-drawing)) 0)
;;       (scxml-save-excursion
;;        (scxml--set-edit-idx scxmld-mode--marked-element 0)
;;        (scxmld-mode--redraw))
;;     (message "Unable to edit drawing without edit indices")))
;; (defun scxmld-mode--toggle-edit-mode ()
;;   ;; TODO - can this function be removed?
;;   "Toggle edit idx flag of the drawing of the currently marked element."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--toggle-edit-mode)
;;   (if (scxmld-mode--edit-idx)
;;       (scxmld-mode--disable-edit-mode)
;;     (scxmld-mode--enable-edit-mode)))

;; (defun scxmld-mode--edit-idx-next ()
;;   ;; TODO - this is duplicated with the edit-idx-prev code.
;;   "Jump to the next edit idx point."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--edit-idx-next)
;;   (when (or (null scxmld-mode--marked-element)
;;             (not (scxml--edit-idx scxmld-mode--marked-element)))
;;     (error "Unable to move to next edit index, not in edit mode"))
;;   (scxmld-mode--edit-idx-increment 1))
;; (defun scxmld-mode--edit-idx-prev ()
;;   "Jump to the prev edit idx point."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--edit-idx-prev)
;;   (when (or (null scxmld-mode--marked-element)
;;             (not (scxml--edit-idx scxmld-mode--marked-element)))
;;     (error "Unable to move to next edit index, not in edit mode"))
;;   (scxmld-mode--edit-idx-increment -1))
;; (defun scxmld-mode--edit-idx-increment (increment)
;;   "Increment the edit-idx of the currently marked element by INCREMENT."
;;   (scxml-save-excursion
;;    (scxml--increment-edit-idx scxmld-mode--marked-element increment)
;;    (scxmld-mode--redraw)))

;; (defun scxmld-mode--add-child-element-with-mouse (pixel constructor-fn)
;;   "Add element specified by CONSTRUCTOR-FN to drawing at PIXEL and begin editng.

;; For elements represented as a box this function will terminate
;; with the drawing being resized."
;;   ;; resolve wherever point is.
;;   ;; add a child state to it @ that point with size zero.
;;   ;; turn on edit mode.
;;   ;; assign edit idx to BR - (2)
;;   ;; continue?
;;   (let* ((pixel (scxml-draw--get-pixel-at-point))
;;          (drawing-coord (scxml-get-coord (scxmld-mode--viewport) pixel))
;;          (parent-element (scxml-find-element-selection scxml-draw--diagram drawing-coord)))
;;     ;; find the closest coordinate to drawing-coord within parent's inner-canvas
;;     ;; and begin the element there.
;;     (unless parent-element
;;       (error "Unable to determine where to add new element"))

;;     (let ((valid-area (scxml-get-inner-canvas (scxml-element-drawing parent-element))))

;;       ;; TODO - this check is very constraining. but without it there would need
;;       ;; to be very smart resolving of collisions.
;;       ;; e.g. it would be possible to start a drawing rect in an inner canvas
;;       ;;      and that inner-canvas in entirely consumed by children.


;;       (unless (2dg-contains valid-area drawing-coord)
;;         (error "Must select a pixel entirely inside a valid inner canvas"))


;;       ;; check valid area.
;;       (let ((new-element (funcall constructor-fn)))
;;         ;; (scxml-add-child parent-element new-element)
;;         (scxmld-mode--add-child-element parent-element new-element nil t)
;;         (scxml--set-drawing-invalid new-element t)
;;         (scxmld-mode--mark-element new-element t)
;;         (if (scxml-initial-class-p new-element)
;;             ;; when initial, add an unconnected transition and exit.
;;             (progn
;;               (scxml-add-child new-element (if (object-of-class-p new-element 'scxml-synthetic-drawing)
;;                                                (scxml-drawable-synthetic-transition)
;;                                              (scxml-drawable-transition)))
;;               (scxml--set-hint new-element (scxml-build-hint (2dg-centroid drawing-coord) valid-area)))
;;           ;; When non-initial (state, parallel or final) immediately
;;           ;; go to drawing resize mode and select the 2nd edit idx.
;;           ;; when the mouse button is release exit resizing mode.
;;           (scxml--set-hint new-element (scxml-build-hint drawing-coord valid-area))
;;           (scxml--set-edit-idx new-element 2)
;;           (setq scxmld-mode--up-mouse-1-catch
;;                 (lambda (pixel) (scxmld-mode--disable-edit-mode))))
;;         (scxmld-mode--redraw)))))
;; (defun scxmld-mode--begin-add-child-element-with-mouse (constructor-fn)
;;   "Begin box-add-and-resize work"
;;   (interactive)
;;   (setq scxmld-mode--down-mouse-1-catch
;;         (lambda (pixel)
;;           (scxmld-mode--add-child-element-with-mouse pixel constructor-fn))))

;; (defun scxmld-mode--add-child-element (parent child &optional prepend-child do-not-replot)
;;   "Add the CHILD element to PARENT element.

;; When PREPEND-CHILD is non-nil the child will be prependend to the
;; PARENT's child elements.  Default behavior is to append.

;; When DO-NOT-REPLOT is non-nil no drawings will be invalidated and
;; the display will not be redrawn.

;; When DO-NOT-REPLOT is nil all children of PARENT will have their
;; drawings invalidated and drawing hints erased, reverting them to
;; automatic mode."
;;   (scxml--validate-parent-child-types parent child)
;;   ;; (scxml--validate-parent-child-counts parent child)
;;   (scxml-add-child parent child (not prepend-child))
;;   (when (and (scxml-element-with-initial-class-p parent)
;;              (object-of-class-p child 'scxml-drawable-synthetic-initial))
;;     ;; you're adding an initial attribute to the parent.
;;     )

;;   (scxmld-mode--apply-edit parent t)
;;   (unless do-not-replot
;;     (scxml-visit parent
;;                  (lambda (descendant)
;;                    (scxml--set-hint descendant nil)
;;                    (scxml--set-drawing-invalid descendant 't))
;;                  (lambda (descendant)
;;                    (and (scxml-drawable-element-class-p descendant)
;;                         (not (eq descendant parent)))))
;;     (scxmld-mode--redraw)))
;; (defun scxmld-mode--add-child-state (id)
;;   "Add a child <state> element to the marked element"
;;   (interactive "sNew <state> id: ")
;;   (scxml-record 'scxmld-mode--add-child-state id)
;;   (let ((parent (or scxmld-mode--marked-element
;;                     (scxmld-mode--display-element))))
;;     (scxmld-mode--add-child-element parent (scxml-drawable-state :id id))))
;; (defun scxmld-mode--add-child-final (id)
;;   "Add a child <state> element to the marked element"
;;   (interactive "sNew <final> id: ")
;;   (scxml-record 'scxmld-mode--add-child-final id)
;;   (let ((parent (or scxmld-mode--marked-element
;;                     (scxmld-mode--display-element))))
;;     (scxmld-mode--add-child-element parent (scxml-drawable-final :id id))))
;; (defun scxmld-mode--add-child-parallel (id)
;;   "Add a child <parallel> element to the marked element"
;;   (interactive "sNew <parallel> id: ")
;;   (scxml-record 'scxmld-mode--add-child-parallel id)
;;   (let ((parent (or scxmld-mode--marked-element
;;                     (scxmld-mode--display-element))))
;;     (scxmld-mode--add-child-element parent (scxml-drawable-parallel :id id))))
;; (defun scxmld-mode--add-child-initial ()
;;   "Begin an <initial> adding mouse saga where the initial parent is the currently marked element."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--add-child-initial)
;;   (message "Mark the element to be the initial target.")
;;   (setq scxmld-mode--mark-element-catch
;;         'scxmld-mode--add-initial-with-transition-to))
;; (defun scxmld-mode--add-initial-with-transition-to (target)
;;   "Add an <initial> child in currently marked element to TARGET.

;; If you're a human you probably want to call the interactive scxmld-mode--add-child-initial."
;;   (let* ((parent scxmld-mode--marked-element)
;;          (new-transition (scxml-drawable-transition :target (scxml-element-id target)))
;;          (new-initial (scxml-drawable-initial)))
;;     (scxml-add-child new-initial new-transition)
;;     (scxmld-mode--add-child-element parent new-initial t)))
;; (defun scxmld-mode--add-child-transition ()
;;   "Begin a <transition> adding mouse saga where the transition parent is the currently marked element."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--add-child-transition)
;;   (message "Mark the element to be the transition target.")
;;   (setq scxmld-mode--mark-element-catch
;;         'scxmld-mode--add-child-transition-to))
;; (defun scxmld-mode--add-child-transition-to (target)
;;   "Add transition from currently marked element to TARGET.

;; If you're a human you probably want to call the interactive scxmld-mode--add-child-transition."
;;   (unless (object-of-class-p target 'scxml-element-with-id)
;;     (error "Invalid target for transition."))
;;   (when (seq-empty-p (scxml-element-id target))
;;     (error "Transition targets must have a valid id"))
;;   (let ((parent scxmld-mode--marked-element))
;;     ;; TODO - this seems unsafe, validate that the target can be the
;;     ;; the target of a transition
;;     (scxml-add-child parent (scxml-drawable-transition :target (scxml-element-id target)))
;;     (scxml--set-drawing-invalid target 't)
;;     (scxml-visit parent
;;                  (lambda (child)
;;                    (scxml--set-drawing-invalid child 't))
;;                  #'scxml-drawable-element-class-p)
;;     (scxmld-mode--redraw)
;;     (scxmld-mode--apply-edit parent t)))

;; (defun scxmld-mode--edit-id (new-id)
;;   "Edit the xml 'id' attribute of the currently marked element."
;;   (interactive "sNew Id:")
;;   (scxml-record 'scxmld-mode--edit-id new-id)
;;   (let ((element scxmld-mode--marked-element))
;;     (unless (object-of-class-p element 'scxml-element-with-id)
;;       (error "Currently selected element does not have an 'id' attribute to set."))
;;     (let ((old-id (scxml-element-id element))
;;           (edited-elements (list element)))
;;       ;; ensure all transitions which reference this id are also updated.
;;       (setf (scxml-element-id element) new-id)
;;       (scxml--set-drawing-invalid element t)

;;       (scxml-visit-all element
;;                        (lambda (transition)
;;                          (setf (scxml-target-id transition) new-id)
;;                          (scxml--set-drawing-invalid transition t)
;;                          (push transition edited-elements))
;;                        (lambda (element)
;;                          (and (object-of-class-p element 'scxml-transition)
;;                               (equal (scxml-target-id element) old-id))))

;;       (scxmld-mode--redraw)
;;       (mapc (lambda (element)
;;               (scxmld-mode--apply-edit element nil))
;;             edited-elements)
;;       )))
;; (defun scxmld-mode--edit-name (new-name)
;;   "Edit the xml 'name' attribute of the currently marked element."
;;   (interactive (let* ((element scxmld-mode--marked-element))
;;                  (unless (object-of-class-p element 'scxml-scxml)
;;                    (error "This element does not have a settable name attribute"))
;;                  (list (read-string "Name: " (scxml-element-name element)))))
;;   (scxml-record 'scxmld-mode--edit-name new-name)
;;   (let ((element scxmld-mode--marked-element))
;;     (setf (scxml-element-name element) new-name)
;;     (scxml--set-drawing-invalid element t)
;;     (scxmld-mode--redraw)
;;     (scxmld-mode--apply-edit element nil)))
;; (defun scxmld-mode--edit-target (new-target-id)
;;   "Edit the xml 'target' attribute of the currently marked element."
;;   (interactive (let* ((element scxmld-mode--marked-element))
;;                  (unless (scxml-transition-class-p element)
;;                    (error "This element does not have a settable name attribute"))
;;                  (list (read-string "Target: " (scxml-target-id element)))))
;;   (scxml-record 'scxmld-mode--edit-target new-target-id)
;;   (let ((element scxmld-mode--marked-element))
;;     (setf (scxml-target-id element) new-target-id)
;;     (scxml--set-drawing-invalid element t)
;;     (scxml--set-hint element nil)
;;     (when (scxmld-mode--edit-idx)
;;       ;; If there is an edit index set, clear it.  The target jump could cause you to lose edit idxs.
;;       (scxml--set-edit-idx scxmld-mode--marked-element 'nil))
;;     (scxmld-mode--redraw)
;;     (scxmld-mode--apply-edit element nil)))

;; (defun scxmld-mode--edit-initial (new-initial)
;;   "Edit the xml 'initial' attribute of the currently marked element."
;;   (interactive (let* ((element scxmld-mode--marked-element))
;;                  (unless (object-of-class-p element 'scxml-element-with-initial)
;;                    (error "This element does not have a settable initial attribute"))
;;                  (list (read-string "Initial: " (scxml-element-initial element)))))
;;   (scxml-record 'scxmld-mode--edit-initial new-initial)
;;   (let ((element scxmld-mode--marked-element))
;;     ;; Might not have been called interactively, validate again.
;;     ;; TODO - is there a better way to do this?
;;     (unless (object-of-class-p element 'scxml-element-with-initial)
;;       (error "This element does not have a settable initial attribute"))
;;     ;; remove any existing initial synthetic drawings.
;;     ;; ok - so this should throw if there is a child <initial> already.
;;     ;; additionally - should be reusing this with adding initial by mouse click.
;;     (mapc (lambda (child)
;;             (when (object-of-class-p child 'scxml-initial)
;;               (scxml-make-orphan child)))
;;           (scxml-children element))
;;     ;; set the initial attribute.
;;     (setf (scxml-element-initial element) new-initial)
;;     ;; create a synthetic drawing node for it.
;;     (let ((new-transition (scxml-drawable-synthetic-transition :target new-initial))
;;           (new-initial (scxml-drawable-synthetic-initial)))
;;       (scxml-add-child new-initial new-transition)
;;       (scxml-add-child element new-initial)
;;       (scxml-visit element
;;                    (lambda (child)
;;                      (scxml--set-hint child nil) ;TODO - fix this, I'm bumping every child to auto-plotting.
;;                      (scxml--set-drawing-invalid child 't))
;;                    (lambda (child)
;;                      (object-of-class-p child 'scxml-drawable-element))))
;;     (scxmld-mode--redraw)
;;     (scxmld-mode--apply-edit element nil)))
;; (defun scxmld-mode--edit-events (new-events)
;;   "Edit the xml \"event\" attribute of the currently marked element."
;;   (interactive (let* ((element scxmld-mode--marked-element))
;;                  (unless (object-of-class-p element 'scxml-transition)
;;                    (error "This element does not have a settable event attribute"))
;;                  (let ((events (scxml-events element)))
;;                    (list (read-string "Events: " (mapconcat #'identity events " "))))))
;;   (scxml-record 'scxmld-mode--edit-events new-events)
;;   (let ((element scxmld-mode--marked-element)
;;         (events-list (if (seq-empty-p new-events)
;;                          nil
;;                        (split-string new-events nil t nil))))

;;     ;; Might not have been called interactively, validate again.
;;     ;; TODO - is there a better way to do this?
;;     (unless (object-of-class-p element 'scxml-transition)
;;       (error "This element does not have a settable event attribute"))

;;     (setf (scxml-events element) events-list)
;;     (scxml--set-drawing-invalid element t)
;;     (scxmld-mode--redraw)
;;     (scxmld-mode--apply-edit element nil)))

;; (defun scxmld-mode--edit-events (new-events)
;;   "Edit the xml \"event\" attribute of the currently marked element."
;;   (interactive (let* ((element scxmld-mode--marked-element))
;;                  (unless (object-of-class-p element 'scxml-transition)
;;                    (error "This element does not have a settable event attribute"))
;;                  (let ((events (scxml-events element)))
;;                    (list (read-string "Events: " (mapconcat #'identity events " "))))))
;;   (scxml-record 'scxmld-mode--edit-events new-events)
;;   (let ((element scxmld-mode--marked-element)
;;         (events-list (split-string new-events nil t nil)))

;;     ;; Might not have been called interactively, validate again.
;;     ;; TODO - is there a better way to do this?
;;     (unless (object-of-class-p element 'scxml-transition)
;;       (error "This element does not have a settable event attribute"))

;;     (setf (scxml-events element) events-list)
;;     (scxml--set-drawing-invalid element t)
;;     (scxmld-mode--redraw)
;;     (scxmld-mode--apply-edit element nil)))

;; (defun scxmld-mode--delete-marked ()
;;   "Delete the marked element, mark the parent."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--delete-marked)
;;   (let* ((element scxmld-mode--marked-element)
;;          (parent (scxml--find-first-non-synthetic-ancestor element)))
;;     (scxmld-mode--delete scxmld-mode--marked-element)
;;     (when parent
;;       (scxmld-mode--mark-element parent))))
;; (defun scxmld-mode--delete (element)
;;   "Delete ELEMENT from the document."
;;   ;; TODO - should this be a cl-defmethod?
;;   (let ((parent (scxml--find-first-non-synthetic-ancestor element)))
;;     (when (null parent)
;;       (error "Unable to find non-synthetic parent of %s" (scxml-print element)))
;;     ;; This is a hack to handle invalidation.
;;     (mapc (lambda (sibling)
;;             (scxml--set-drawing-invalid sibling 't))
;;           (seq-filter (lambda (sibling)
;;                         (object-of-class-p sibling 'scxml-drawable-element))
;;                       (scxml-children parent)))
;;     ;; (scxml--set-drawing-invalid element)

;;     ;; TODO - make-orphan should be overridden for drawable-elements
;;     ;; and should handle invalidation.  If the element is ever removed
;;     ;; the drawing should all be invalidated.
;;     (scxml-make-orphan element)
;;     ;; (scxml-visit parent
;;     ;;              (lambda (sibling)
;;     ;;                (scxml--set-drawing-invalid sibling 't))
;;     ;;              (lambda (child)
;;     ;;                (object-of-class-p child 'scxml-drawable-element)))
;;     (scxmld-mode--apply-edit parent t)))

;; (defun scxmld-mode--apply-edit (element &optional include-children)
;;   "Check ELEMENT in linked XML buffer and apply changes from the diagram."
;;   ;; TODO - debounce this, it gets bad when your doing mouse dragging.
;;   (scxml-xml-update-element scxml-draw--diagram
;;                             (if (object-of-class-p element 'scxml-synthetic-drawing)
;;                                 (scxml--find-first-non-synthetic-ancestor element)
;;                               element)
;;                             include-children))
;; (defun scxmld-mode--sync-linked-xml ()
;;   "Sync the entire diagram to the xml buffer if it exists."
;;   (interactive)
;;   (scxml-record 'scxmld-mode--sync-linked-xml)
;;   (scxml-xml-update-element scxml-draw--diagram
;;                             (scxml-diagram-root scxml-draw--diagram)
;;                             t))

;; (cl-defgeneric scxmld-mode--get-element ((selection-rect 2dg-rect))
;;   "Return the element inside the SELECTION-RECT."
;;   (scxml-find-element-selection scxml-draw--diagram selection-rect))

;; (defun scxml--map-plist (fn plist)
;;   "Map over the members of PLIST calling FN as FN(key, val)"
;;   (let ((current plist)
;;         (accumulator 'nil))
;;     (while (and current (cdr current))
;;       (push (funcall fn (car current) (cadr current)) accumulator)
;;       (setq current (cddr current)))
;;     accumulator))
;; (defun scxmld-mode--debug-hint (element)
;;   (let ((hint (scxml--hint element)))
;;     (if hint
;;         (cond ((or (2dg-rect-p hint)
;;                    (scxml-arrow-hint-p hint)
;;                    (2dg-point-p hint))
;;                (scxml-print hint))
;;               ((scxml---drawing-nest-rect-hint-p hint)
;;                "it's a divided nest rect")
;;               ('t                       ;probably a plist :(
;;                (mapconcat 'identity
;;                           (scxml--map-plist (lambda (key val) (format "%s=%s" key val))
;;                                             hint)
;;                           "\n          ")))

;;       "nil")))
;; (cl-defmethod scxml-print ((rect 2dg-rect))
;;   "Apparently I need this here - TODO - find out why."
;;   (2dg-pprint rect))
;; (cl-defmethod scxml-print ((path 2dg-path))
;;   "Another one that needs to be fixed."
;;   (2dg-pprint path))
;; (defun scxmld-mode--debug-barf ()
;;   "Barf out a ton of debug info at the bottom of the diagram"

;;   (save-excursion
;;     (scxml-draw--goto-pixel
;;      (2dg-pixel :x 0
;;                   :y (round (+ 2 (scxml-required-pixel-height (scxmld-mode--viewport))))))
;;     (delete-region (point) (point-max))
;;     (scxml-draw--goto-pixel
;;      (2dg-pixel :x 0
;;                   :y (round(+ 3 (scxml-required-pixel-height (scxmld-mode--viewport))))))

;;     (insert
;;      (format "Viewport: %s\n" (scxml-diagram-viewport scxml-draw--diagram))
;;      (format "mainCanv: %s\n" (scxmld-mode--canvas))
;;      (format "linkBuff: %s\n" (scxml-xml-buffer scxml-draw--diagram)))

;;     (let ((marked scxmld-mode--marked-element))
;;       (if marked
;;           (insert
;;            (format "lastClik: %s\n" (if scxmld-mode--last-click-pixel
;;                                         (format "%s -> %s -> %s{%s} -> %s/%s"
;;                                                 (2dg-pprint scxmld-mode--last-click-pixel)
;;                                                 (2dg-pprint (scxml-get-scratch-coord (scxmld-mode--viewport)
;;                                                                                       scxmld-mode--last-click-pixel))
;;                                                 (2dg-pprint (scxml-get-coord (scxmld-mode--viewport)
;;                                                                               scxmld-mode--last-click-pixel))
;;                                                 (2dg-pprint (scxml-get-scratch-coord (scxmld-mode--viewport)
;;                                                                                       (2dg-BL (scxml-get-coord (scxmld-mode--viewport)
;;                                                                                                                  scxmld-mode--last-click-pixel))))
;;                                                 (2dg-pprint (scxml-get-pixel (scxmld-mode--viewport)
;;                                                                               (2dg-BL (scxml-get-coord (scxmld-mode--viewport)
;;                                                                                                          scxmld-mode--last-click-pixel))))
;;                                                 (2dg-pprint (scxml-get-pixel (scxmld-mode--viewport)
;;                                                                               (2dg-centroid (scxml-get-coord (scxmld-mode--viewport)
;;                                                                                                                scxmld-mode--last-click-pixel)))))
;;                                       "none"))
;;            (format "Marked:   %s\n" (scxml-print marked))
;;            (format "-EditIdx: %s @ %s \n" (scxmld-mode--edit-idx)
;;                    (when (scxmld-mode--edit-idx)
;;                      (scxml-edit-idx-point (scxml-element-drawing marked) (scxmld-mode--edit-idx))))
;;            (format "-Hint   : %s\n" (scxmld-mode--debug-hint marked))
;;            (format "-GeoType: %s\n" (eieio-object-class (scxml-element-drawing marked)))
;;            (format "-Geometf: %s\n" (scxml-print (scxml-element-drawing marked))))
;;         (insert "No marked element\n\n")))
;;     (when scxml-recording
;;       (let ((step 0))
;;         (insert (mapconcat (lambda (x) (format "REC[%d]: %s" (incf step) x)) scxml-recording "\n"))))))

(provide 'scxmld-mode)
;;; scxmld-mode.el ends here
