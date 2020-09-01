
;;; Code:


;; where you left off


;; (error "this")
;; make a new drawing, make a state, add a transition off that state going nowhere.
;; note that make-instance for scxmld-transition is called and then make-instance for 2dd-link is called.

;; now, make an initial attribute in the parent scxmld, add a transition (it'll be synthetic) off that.  Note that make-instance for scxmld-transition is called but make-instance for 2dd-link is not???!!! what?

;; link manual tests
;; - move the rect that the link is connected to
;; - move the parent rect of the rect the link is connected to.
;; -- non endpoint edix idx moves
;; - move any edit idx that won't cause and edge jump or endpoint move.
;; - move any edit idx that will cause an end point move but not an edge jump
;; - move any edit idx that will cause an endpoint move.
;; -- end point edit idx moves
;; - move free edit idxs anywhere
;; - move rect edit idxs without edge jumps
;; - move rect edit idxs with edge jumps

;; bug:
;; 1- draw a single parallel in an scxml
;; 2- place a single state in that paralle
;; 3- attempt to draw a state in *that* state
;; 4- attach a debugger to the plotter, it's forcefully setting the coordinates of the parallel?  That seems incorrect.

;; (error "get the relative geomemtry via lambda working.")

;; (error "Figure out if that handle-parent-change function is needed, if not remove it entirely.")
;; (error "The 2dd-rect render and 2dd-division-rect render are identical?  fix that, subfn them out and call the parent.")

;; (error "get selection working when you have one state in a parallel and you want to double click to get to edit mode.
;;   then get selection working when you have two states as the children of a parallel and you want to get edit mode working on the parent.
;;   Then add children to the states childs of the parallel and get that selection working.  Then draw the border between the parallel sectors.")


;; (I think I need to remember my last click, in scxmld

(defsubst scxmld-error (msg)
  "Call scxmld-log with MSG and a level of 'error."
  (scxmld-log msg 'error))
(defun scxmld-log (msg &optional level)
  "Inform the user about MSG with severity level LEVEL."
  (let ((route (if (eq level 'error)
                   'error
                 'message)))
    (funcall route (format "%s: %s"
                           level
                           msg))))

(require 'scxmld-element)
(require 'scxmld-elements)
(require 'scxmld-diagram)
(require 'scxmld-xml)
(require 'scxmld-mode)


(provide 'scxmld)
;;; scxmld.el ends here
