;;; scxml-drawable-element.el --- scxml drawable element functions -*- lexical-binding: t -*-

;;; Commentary:
;; Represents scxml (and synthetic) elements which can be drawn

;; There are two object graphs maintained here: scxml and scxmld.
;; * The scxml graph strictly represents the graph of all objects in the
;; scxml document.  Some scxmld elements may be excluded.
;; * The scxmld graph represents all the drawings shown to the user.
;; This graph may contain addional objects or may be missing objects when
;; compared to the scxml graph.

;;; Code:
(require 'eieio)
(require 'scxml)
(require '2dd)

(defconst scxmld-drawing-attribute "scxml---drawing"
  "The xml attribute name used to save scxmld drawing information
in an xml document.")

(cl-defgeneric scxmld-pprint (element)
  "Pretty print ELEMENT for human eyeballs."
  (error "scxmld-print not implemented for this object type."))

(defclass scxmld-element (scxml-element 2dd-drawing)
  ()
  :abstract 't
  :documentation "This is an element that can be drawn and which
  has a 1 to 1 relationship with an element in a valid scxml
  document.")

;; core scxmld-element function declarations
(cl-defmethod make-instance ((class (subclass scxmld-element)) &rest slots)
  "make it"
  ;; TODO: I dont't even think I need this anymore.
  (message "make-instance scxmld-element")
  (cl-call-next-method))
(defsubst scxmld-element-class-p (any)
  "Equivalent of (object-of-class-p ANY 'scxml-element)."
  (and any (object-of-class-p any 'scxmld-element)))
(cl-defgeneric scxmld-get-attribute ((element scxmld-element) (attribute-name string))
  "Return the value of ELEMENT's ATTRIBUTE-NAME'd attribute.

This function works regardless of the attribute being special or
not (e.g. a <state> element's id is special and a <transition>'s
target is special).")
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
(cl-defgeneric scxmld-add-child ((parent scxmld-element) (new-child scxmld-element) &optional append)
  "Make NEW-CHILD a child element of PARENT, returning PARENT.

This function operates on the scxmld object graph.

When APPEND is non-nil NEW-CHILD will become the last child.
When APPEND is nil NEW-CHILD will become the first child.")
(cl-defgeneric scxmld-children ((element scxmld-element))
  "Return the children of the ELEMENT.

Defaults to using scxml-children."
  (error "Not implemented"))
(cl-defgeneric scxmld-parent ((element scxmld-element))
  "Return the parent of ELEMENT"
  (error "Not implemented"))
(cl-defgeneric scxmld-root-element ((element scxmld-element))
  "Given any ELEMENT in an scxmld-element tree, find the root of the tree."
  (error "Not implemented"))
(cl-defgeneric scxmld-visit ((element scxmld-element) visitor &optional filter child-fn)
  "Visit all children of ELEMENT with VISITOR and optionally FILTER first.

See 'scxml-visit for details, this implementation is the same in
purpose."
  (error "Not implemented"))
(cl-defgeneric scxmld-visit-all ((element scxmld-element) visitor &optional filter child-fn)
  "Visit all elements (parent or child, recursively) starting at the root element.

See scxml-visit-all for details, this implementation is the same
in purpose."
  (error "Not implemented"))

;; implementations
(cl-defmethod scxml-xml-attributes ((element scxmld-element))
  "Return the xml attributes for ELEMENT, a drawing with a geometry attribute.

This function contains a hack to force all symbols to strings.  See todo to correct this."
  ;; TODO - maybe this should be scxmld-xml-attributes??
  (nconc (cl-call-next-method)nn
         (list (cons scxmld-drawing-attribute
                     (2dd-serialize-geometry element)))))
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
(cl-defmethod scxmld-short-name ((element scxml-element-with-id))
  "Return a string with the id of ELEMENT for display."
  (or (scxml-get-id element) "?No-Id?"))
(cl-defmethod scxmld-add-child ((parent scxmld-element) (new-child scxmld-element) &optional append)
  "Make NEW-CHILD a child element of PARENT, returning PARENT.

This function operates on the scxmld object graph."
  (scxml-add-child parent new-child append))

(cl-defmethod scxmld-children ((element scxmld-element))
  "Return the children of the ELEMENT."
  (scxml-children element))
(cl-defmethod scxmld-parent ((element scxmld-element))
  "Return the parent of ELEMENT, defaults to `scxml-parent'."
  (scxml-parent element))
(cl-defmethod scxmld-root-element ((element scxml-element))
  "Given any ELEMENT in an scxmld-element tree, find the root of the tree."
  (let ((last element)
        (parent (scxmld-parent element)))
    (while parent
      (setq last parent)
      (setq parent (scxmld-parent last)))
    last))

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
(cl-defmethod make-instance ((class (subclass scxmld-with-highlight)) &rest slots)
  "make it"
  ;; TODO - I don't think I need this anymore.
  (message "make-instance scxmld-with-highlight")
  (cl-call-next-method))

(defclass scxmld-synthetic-element ()
  ((_synth-parent :initform nil
                  :reader scxmld-get-synth-parent
                  :writer scxmld-set-synth-parent
                  :type (or null scxmld-element)
                  :documentation "Synthetic parent slot, distinct
                  from the normal scxml-parent slot.")
   (_synth-child :initform nil
                 :type (or null scxmld-synthetic-element)
                 :writer scxmld-set-synth-child
                 :documentation "Synthetic child (of which there
                 may only be one), distinct from the normal
                 scxml-children slot."))
  :abstract t
  :documentation "Marker class which indicates that an element is synthetic.  A synthetic element is one which is not a real <scxml> xml element, but is an object which needs to be drawn.")
(cl-defmethod make-instance ((class (subclass scxmld-synthetic-element)) &rest slots)
  "make it"
  ;; TODO - I don't think I need this anymore.
  (message "make-instance scxmld-synthetic-element")
  (cl-call-next-method))
(defsubst scxmld-synthetic-element-class-p (any)
  "Equivalent of (object-of-class-p ANY 'scxml-synthetic-element)."
  (and any (object-of-class-p any 'scxmld-synthetic-element)))
(cl-defmethod scxml-parent ((element scxmld-synthetic-element))
  "Always returns null.

synthetic elements do not have parents (or children) in the scxml
graph."
  nil)
(cl-defmethod scxmld-parent ((element scxmld-synthetic-element))
  "Return the parent of ELEMENT.

Synthetic elements only exist in the drawing graph."
  (scxmld-get-synth-parent element))
(cl-defmethod scxmld-children ((element scxmld-synthetic-element))
  "Return the children of the ELEMENT."
  ;; TODO: I think this implementation is probably overkill
  (let ((child (oref element _synth-child)))
    (if child
        (list child)
      nil)))
(cl-defmethod scxmld-add-child :before ((synth-parent scxmld-synthetic-element) (element scxmld-element) &optional _unused)
  (unless (scxmld-synthetic-element-child-p element)
    (error "Unable to add a scxmld element to a scxmld-synthetic parent.")))
(cl-defmethod scxmld-add-child ((synth-parent scxmld-synthetic-element) (synth-child scxmld-synthetic-element) &optional _usused)
  "Add SYNTH-CHILD to the parent ELEMENT."
  (assert (null (oref synth-parent _synth-child))
          t
          "Unable to add synthetic element child to synthetic element parent, parent already has a synthetic child")
  (assert (null (oref synth-child _synth-parent))
          t
          "Unable to set sythetic element parent of the child eelement, child already has a synthetic parent")
  (scxmld-set-synth-parent synth-child synth-parent)
  (scxmld-set-synth-child synth-parent synth-child))
(cl-defmethod scxmld-make-orphan ((element scxmld-synthetic-element))
  "Break ELEMENT away from any parent elements."
  (let ((synth-parent (scxmld-get-synth-parent element)))
    (when synth-parent
      ;; element has a parent, break the link from parent->child
      (cond
       ((scxmld-synthetic-element-class-p synth-parent)
        ;; remove from synth parent.
        (oset synth-parent _synth-child nil))
       ((scxmld-with-synthetic-initial-class-p synth-parent)
        (scxmld-set-synth-initial synth-parent nil))
       (t (error "Unknown parent type for a synthetic element?"))))
    
    (scxmld-set-synth-parent element nil)))

(defclass scxmld-with-synthetic-initial ()
  ((synth-initial :initform nil
                  :type (or null scxmld-synthetic-element)
                  :reader scxmld-get-synth-initial
                  :writer scxmld-set-synth-initial))
  :abstract t
  :documentation "Indicates that an scxmld element has an
  'initial' attribute of significance and that this attribute
  should be drawn as a point and link indicating the target of
  the initial attribute.")
(defsubst scxmld-with-synthetic-initial-class-p (any)
  "Equivalent of (object-of-class-p ANY 'scxml-drawable-element)."
  (and any (object-of-class-p any 'scxmld-with-synthetic-initial)))
(cl-defmethod scxmld-children ((element scxmld-with-synthetic-initial))
  "Return the children of ELEMENT given it may have synthetic children."
  (let ((synth-initial (scxmld-get-synth-initial element)))
    (if synth-initial
        (append (list synth-initial)
                (cl-call-next-method))
      (cl-call-next-method))))
(cl-defmethod scxmld-add-child ((element scxmld-with-synthetic-initial) (synth-child scxmld-synthetic-element) &optional _usused)
  "Add SYNTH-CHILD to the parent ELEMENT."
  (assert (null (scxmld-get-synth-initial element))
          t
          "Unable to set synthetic initial for this element, it's already set")
  (assert (null (oref synth-child _synth-parent))
          t
          "Unable to set synthetic parent for synth element, already has one")
  (scxmld-set-synth-parent synth-child element)
  (scxmld-set-synth-initial element synth-child))

;; (cl-defgeneric scxmld-get-synthetic-initial-target-id ((element scxmld-with-synthetic-initial))
;;   "Return the current target of ELEMENT's synthetic initial drawings."
;;   (let ((synth-initial (oref element synthetic-initial)))
;;     (if synth-initial
;;         (let ((synth-transitions (scxml-children synth-initial)))
;;           (assert (eq (length synth-transitions) 1)
;;                 t
;;                 "A synthetic initial element must have exactly one sythetic transition.")
;;           (assert (scxml-transition-class-p (first synth-transitions))
;;                 t
;;                 "A synthetic initial element must have a child which is a transition")
;;           (scxml-get-target-id (first synth-transitions)))
;;       nil)))


(provide 'scxmld-element)
;;; scxmld-element.el ends here
