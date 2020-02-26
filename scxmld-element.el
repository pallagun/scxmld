;;; scxml-drawable-element.el --- scxml drawable element functions -*- lexical-binding: t -*-

;;; Commentary:
;; Represents a concrete csxml element which can be drawn.

;;; Code:
(require 'eieio)
(require 'scxml)
(require '2dd)

(defconst scxmld-drawing-attribute "scxml---drawing"
  "The xml attribute name used to save scxmld drawing information
in an xml document.")

(defclass scxmld-element (scxml-element 2dd-drawing)
  ()
  :abstract 't
  :documentation "This is an element that can be drawn and which
  has a 1 to 1 relationship with an element in a valid scxml
  document.")
(defsubst scxmld-element-class-p (any)
  "Equivalent of (object-of-class-p ANY 'scxml-drawable-element)."
  (object-of-class-p any 'scxmld-element))
(cl-defmethod scxml-xml-attributes ((element scxmld-element))
  "Return the xml attributes for ELEMENT, a drawing with a geometry attribute.

This function contains a hack to force all symbols to strings.  See todo to correct this."
  (nconc (cl-call-next-method)
         (list (cons scxmld-drawing-attribute
                     (prin1-to-string (2dd-geometry element))))))
(defclass scxmld-synthetic-element (scxml--core-nil scxml-element 2dd-drawing)
  ()
  :documentation "This class signifies that the object which is
  drawn does not have a 1 to 1 correspondence with an scxml
  element.")
(defsubst scxmld-synthetic-element-class-p (any)
  "Equivalent of (object-of-class-p ANY 'scxmld-synthetic-element element)."
  (object-of-class-p any 'scxml-synthetic-element))

(cl-defgeneric scxmld-pprint (element)
  "Pretty print ELEMENT for human eyeballs.")

(cl-defgeneric scxmld-find-first-non-synthetic-ancestor (element)
  "Find first ancestor that is not synthetic."
  (error "Must implement for type: %s"
         (eieio-object-class-name element)))
(cl-defmethod scxmld-find-first-non-synthetic-ancestor ((element scxmld-element))
  "Find first ancestor that is not synthetic."
  (scxml-parent element))
(cl-defmethod scxmld-find-first-non-synthetic-ancestor ((element scxmld-synthetic-element))
  "Find first ancestor that is not synthetic."
  (scxml-find-ancestor-if element
                          (lambda (e)
                            (not (scxmld-synthetic-element-class-p e)))))

(defclass scxmld-with-highlight ()
  ((highlight :initarg :highlight
              :initform nil
              :reader scxmld-get-highlight
              :writer scxmld-set-highlight))
  :abstract t
  :documentation "Indicates that an scxmld element can be
  highlighted and if it currently is or is not.")
(cl-defmethod scxmld-set-highlight :before ((element scxmld-with-highlight) value)
  "Validate the highlight value being set as strictly 't or 'nil."
  (unless (or (eq value nil) (eq value t))
    (error "Highlight values must be strictly 't or 'nil")))


(provide 'scxmld-element)
;;; scxmld-element.el ends here
