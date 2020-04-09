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
(cl-defgeneric scxmld-children ((element scxmld-element))
  "Return the children of the ELEMENT.

Defaults to using scxml-children.")
(cl-defmethod scxmld-children ((element scxmld-element))
  "Return the children of the ELEMENT."
  (scxml-children element))

(cl-defmethod scxml-xml-attributes ((element scxmld-element))
  "Return the xml attributes for ELEMENT, a drawing with a geometry attribute.

This function contains a hack to force all symbols to strings.  See todo to correct this."
  (nconc (cl-call-next-method)
         (list (cons scxmld-drawing-attribute
                     (2dd-serialize-geometry element)))))
(cl-defgeneric scxmld-get-attribute ((element scxmld-element) (attribute-name string))
  "Return the value of ELEMENT's ATTRIBUTE-NAME'd attribute.

This function works regardless of the attribute being special or
not (e.g. a <state> element's id is special and a <transition>'s
target is special).")
(cl-defmethod scxmld-get-attribute ((element scxmld-element) (attribute-name string))
  "Return the value of ELEMENT's ATTRIBUTE-NAME'd attribute.

Will treat special attributes the same as non-special ones.

Current special attributes:
'scxml-element-with-id (id)
'scxml-element-with-initial (initial)l
'scxml-scxml (name, datamodel binding)
'scxml-transition (target, event (as 'events'), cond (as 'cond-expr'), type)
"
  ;; Currently this works off the xml writer's infrastructure.  While
  ;; that's accurate I'm not sure if it's the best approach.
  (let ((attributes (scxml-xml-attributes element)))
    (alist-get attribute-name attributes nil nil #'equal)))
(cl-defgeneric scxmld-put-attribute ((element scxmld-element) (attribute-name string) attribute-value)
  "Set ELEMENT's attribute named ATTRIBUTE-NAME to be ATTRIBUTE-VALUE.

When ATTRIBUTE-VALUE is nil the attribute will be deleted if possible.

This function works regardless of the attribute being special or
not (e.g. a <state> element's id is special and a <transition>'s
target is special).")
(cl-defgeneric scxmld-short-name (element)
  "Produce a short human readable string identifying ELEMENT.

This string should not have the element type in it."
  (error "Unable scxmld-short-name for element of type: %s"
         (eieio-object-class-name element)))
(cl-defmethod scxmld-short-name ((element scxml-element-with-id))
  "Return a string with the id of ELEMENT for display."
  (let ((id (scxml-get-id element)))
    (if id
        id
      "?No-Id?")))

;; (defclass scxmld-synthetic-element (scxml--core-nil scxml-element 2dd-drawing)
;;   ()
;;   :documentation "This class signifies that the object which is
;;   drawn does not have a 1 to 1 correspondence with an scxml
;;   element.")
;; (defsubst scxmld-synthetic-element-class-p (any)
;;   "Equivalent of (object-of-class-p ANY 'scxmld-synthetic-element element)."
;;   (object-of-class-p any 'scxml-synthetic-element))

(cl-defgeneric scxmld-pprint (element)
  "Pretty print ELEMENT for human eyeballs.")

;; (cl-defgeneric scxmld-find-first-non-synthetic-ancestor (element)
;;   "Find first ancestor that is not synthetic."
;;   (error "Must implement for type: %s"
;;          (eieio-object-class-name element)))
;; (cl-defmethod scxmld-find-first-non-synthetic-ancestor ((element scxmld-element))
;;   "Find first ancestor that is not synthetic."
;;   (scxml-parent element))
;; (cl-defmethod scxmld-find-first-non-synthetic-ancestor ((element scxmld-synthetic-element))
;;   "Find first ancestor that is not synthetic."
;;   (scxml-find-ancestor-if element
;;                           (lambda (e)
;;                             (not (scxmld-synthetic-element-class-p e)))))

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

(defclass scxmld-with-synthetic-initial ()
  ((point :initform nil
          :reader scxmld-set-synthetic-initial-point
          :writer scxmld-get-synthetic-initial-point
          :type (or null 2dd-point))
   (link :initform nil
         :reader scxmld-set-synthetic-initial-link
         :writer scxmld-get-synthetic-initial-link
         :type (or null 2dd-link)))
  :abstract t
  :documentation "Indicates that an scxmld element has an
  'initial' attribute of significance and that this attribute
  should be drawn as a point and link indicating the target of
  the initial attribute.")


(provide 'scxmld-element)
;;; scxmld-element.el ends here
