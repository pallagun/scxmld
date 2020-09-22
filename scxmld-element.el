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
;; implementations
(cl-defmethod scxml-xml-attributes ((element scxmld-element))
  "Return the xml attributes for ELEMENT, a drawing with a geometry attribute.

This function contains a hack to force all symbols to strings.  See todo to correct this."
  ;; TODO - maybe this should be scxmld-xml-attributes??
  (nconc (cl-call-next-method)
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
(cl-defgeneric scxmld-add-child ((parent scxmld-element) (child scxmld-element) &optional append)
  "Make NEW-CHILD a child element of PARENT, returning PARENT.

This function operates on the scxmld object graph.

When APPEND is non-nil NEW-CHILD will become the last child.
When APPEND is nil NEW-CHILD will become the first child.")
(cl-defmethod scxmld-add-child ((parent scxmld-element) (child scxmld-element) &optional append)
  "Make CHILD a child element of PARENT, returning PARENT.

This function operates on the scxmld object graph, a superset of
the scxml graph."
  (scxml-add-child parent child append))
(cl-defgeneric scxmld-make-orphan ((child scxmld-element) &optional parent)
  "Make CHILD element an orphan.

When CHILD has multiple parents then a PARENT element must be supplied.")
(cl-defgeneric scxmld-make-orphan ((child scxmld-element) &optional parent)
  "Make CHILD element an orphan.

If child has multiple parents then PARENT must be specified"
  (assert (eq parent (scxml-parent parent))
          t
          "If parent is supplied, it must be correct")
  (assert (>= 1 (length (scxmld-parents child)))
          t
          "If this function is called, there must be only one parent for the child.")
  (scxml-make-orphan child))


(cl-defgeneric scxmld-children ((element scxmld-element))
  "Return the children of the ELEMENT.

Defaults to using scxml-children."
  (error "Not implemented"))
(cl-defmethod scxmld-children ((element scxmld-element))
  "Return the children of the ELEMENT.

This function assumes there are no extra diagram children and will only return the normal <scxml> tree children."
  ;; (message "scxmld-children (scxmld-element)")
  (scxml-children element))

(cl-defgeneric scxmld-parents ((element scxmld-element))
  "Returns the parents of ELEMENT as a list."
  (error "Not implemented"))
(cl-defmethod scxmld-parents ((element scxmld-element))
  "Return the parents of ELEMENT as a list.

Defaults to a list containing the output of `scxml-parent'."
  (let ((parent (scxml-parent element)))
    (if parent
        (list parent)
      nil)))
(cl-defgeneric scxmld-root-element ((element scxmld-element))
  "Given any ELEMENT in an scxmld-element tree, find the root of the tree."
  (error "Not implemented"))
(cl-defmethod scxmld-root-element ((element scxmld-element))
  "Given any ELEMENT in an scxmld-element tree, find the root of the tree."
  (let ((last element)
        (parent (car (scxmld-parents element))))
    (while parent
      (setq last parent)
      (setq parent (car (scxmld-parents last))))
    last))
(cl-defgeneric scxmld-visit ((element scxmld-element) visitor &optional filter)
  "Visit all children of ELEMENT with VISITOR and optionally FILTER first.

See 'scxml-visit for details, this implementation is the same in
purpose."
  (error "Not implemented"))
(cl-defmethod scxmld-visit ((element scxmld-element) visitor &optional filter)
  "Visit all children of ELEMENT with VISITOR and optionally FILTER first.

See 'scxml-visit for details, this implementation is the same in
purpose.

The scxmld graph could have one node with two parents, this
function promises to visit each node exactly once."
  
  (scxmld---visit element
                  visitor
                  filter
                  nil))
(defun scxmld---visit (element visitor filter visited-elements)
  "Visit ELEMENT (and all children of it) only once.

Only call VISITOR on ELEMENT if it passes FILTER.  This function
will return the updated version of VISITED-ELEMENTS as it
processes."
  (assert element
          t
          "scxmld---visit has recieved a null element?")
  (unless (member* element visited-elements :test 'eq)
    ;; element not yet visited, visit it if applicable.
    (push element visited-elements)
    (when (or (null filter) (funcall filter element))
      (funcall visitor element))
    (mapc (lambda (child)
            (assert element
                    t
                    "scxmld---visit has found an element with a null child?")
            (setq visited-elements
                  (scxmld---visit child visitor filter visited-elements)))
          (scxmld-children element)))
  visited-elements)
(cl-defgeneric scxmld-visit-all ((element scxmld-element) visitor &optional filter)
  "Visit all elements (parent or child, recursively) starting at the root element.

See scxml-visit-all for details, this implementation is the same
in purpose."
  (error "Not implemented"))
(cl-defmethod scxmld-visit-all ((element scxmld-element) visitor &optional filter)
  "Visit all elements (parent or child, recursively) starting at the root element.

See scxml-visit-all for details, this implementation is the same
in purpose."
  (scxmld-visit (scxmld-root-element element)
                visitor
                filter))

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

(defclass scxmld-with-diagram-parents ()
  ((diagram-parents :initarg :diagram-parents
                    :initform nil
                    :reader scxmld-get-diagram-parents))
  :abstract t
  :documentation "A mix-in class which indicates that the element
  may have a diagram graph only parent.  This may be in addition
  to parent elements on the <scxml> graph/tree.

There are currently three elements which can have diagram graph
parents:

* synthetic initial elements (one parent, a state or scxml
  element)

* synthetic transitions (two parents, the first a synthetic
  initial and the second a target state)

* transitions (N parents, one from the normal graph and N-1
  parents (potentially) from the diagram graph for transitions
  that have multiple targets)
")
(cl-defmethod make-instance ((class (subclass scxmld-with-diagram-parents)) &rest slots)
  "make it"
  ;; TODO - I don't think I need this anymore
  (message "make-instance scxmld-with-diagram-parents")
  (message "------------- has next method: %s" (cl-next-method-p))
  (cl-call-next-method))
(cl-defmethod scxmld-parents ((element scxmld-with-diagram-parents))
  "Return the parents of ELEMENT as a list including any diagram-only parents.

The scxml tree parent will always be first."
  (let ((normal-parent (scxml-parent element)))
    (if normal-parent
        (cons normal-parent (oref element diagram-parents))
      (oref element diagram-parents))))
(cl-defgeneric scxmld-make-orphan ((child scxmld-with-diagram-parents) &optional parent)
  "Break the connection between CHILD and PARENT"
  (unless parent
    (error "For now, you need to specify a parent"))
  (assert (member* child (scxmld-children parent) :test 'eq)
          t
          "Child is not related to parent")
  (assert (member* parent (scxmld-parents child) :test 'eq)
          t
          "Parent is not related to child")

  ;; first, determine if this is a diagram or normal scxml graph relationship
  (cond
   ;; scxml tree case
   ((and (eq parent (scxml-parent child))
         (member* child (scxml-children parent) :test 'eq))
    (scxml-make-orphan child))
   ;; diagram graph case
   ((and (scxmld-with-diagram-children-child-p parent)
         (member* parent (scxmld-parents child) :test 'eq)
         (member* child (scxmld-children parent) :test 'eq))
    (oset child
          diagram-parents
          (seq-filter (lambda (element)
                        (not (eq element parent)))
                      (oref child diagram-parents)))
    (oset parent
          diagram-children
          (seq-filter (lambda (element)
                        (not (eq element child)))
                      (oref parent diagram-children))))
   (t
    (error "Unknown relationship between parent and child elements"))))
;; (cl-defmethod scxmld-add-diagram-parent :before ((element scxmld-with-diagram-parent) anything)
;;   "Throw an error if ELEMENT already has a parent set"
;;   (when (and anything (oref element diagram-parent))
;;     (error "Element already has a diagram parent set and may not set another")))

(defclass scxmld-with-diagram-children ()
  ((diagram-children :initarg :diagram-children
                     :initform nil
                     :reader scxmld-get-diagram-children))
  :abstract t
  :documentation
  "A mix-in class which indicates that, on the scxmld diagram
graph, this element can have children in addition to the ones
from the normal scxml graph. 

There are currently three situations where this can happen:

* elements (<scxml> and <state>) which support synthetic initial
  children

* synthetic elements which have other synthetic child elements.
  Right now this is only the parent-child relationship between
  scxmld-synthetic-initial and scxmld-synthetic-transition

* elements which can be the targets of any transition (synthetic
  or not).")
(cl-defmethod make-instance ((class (subclass scxmld-with-diagram-children)) &rest slots)
  "make it"
  ;; TODO - I don't think I need this anymore
  (message "make-instance scxmld-with-diagram-children")
  (message "------------- has next method: %s" (cl-next-method-p))
  (cl-call-next-method))
(cl-defmethod scxmld-children ((element scxmld-with-diagram-children))
  "Return the children of ELEMENT which are exclusively on the diagram graph.

This function will not return any children from the normal
`scxml-children' function."
  ;; (message "scxmld-children (scxmld-with-diagram-children)")
  (let ((diagram-children (oref element diagram-children))
        (children (condition-case err
                      (cl-call-next-method)
                    (cl-no-next-method nil))))
    (seq-filter 'identity
                (append diagram-children
                        children))))
(cl-defgeneric scxmld-add-diagram-child ((parent scxmld-with-diagram-children) (child scxmld-with-diagram-parents) &optional append)
  "Add CHILD as a diagram graph child of PARENT.

When APPEND is non-nill CHILD will become the last child.  When
APPEND is nil CHILD will become th efirst child.

Much like scxml-add-child but for the diagram graph")
(cl-defmethod scxmld-add-diagram-child ((parent scxmld-with-diagram-children) (child scxmld-with-diagram-parents) &optional append)
  "Add CHILD as a diagram graph child of PARENT.

Much like scxml-add-child but for the diagram graph"
  ;; TODO - protect against circular graphs some day?
  (when (or (member* child (oref parent diagram-children) :test 'eq)
            (member* child (scxml-children parent) :test 'eq))
    (error "Unable to add child parent relationship on diagram graph, the parent seems to already have a relationship with the child."))
  (when (member* parent (scxmld-parents child) :test 'eq)
    (error "Unable to add child parent relationship on diagram graph, the child seems to already have a relationship with the parent."))

  (if append
      (progn
        (oset parent
              diagram-children
              (nconc (oref parent diagram-children)
                     (list child)))
        (oset child
              diagram-parents
              (nconc (oref child diagram-parents)
                     (list parent))))
    (push child (oref parent diagram-children))
    (push parent (oref child diagram-parents))))

(cl-defgeneric scxmld-remove-diagram-child ((parent scxmld-with-diagram-children) (child scxmld-with-diagram-parents))
  "Break CHILD away from ELEMENT on the diagramr graph.")
(cl-defmethod scxmld-remove-diagram-child ((parent scxmld-with-diagram-children) (child scxmld-with-diagram-parents))
  "Break CHILD away from ELEMENT on the diagramr graph."
  (unless (member* child (oref parent diagram-children) :test 'eq)
    (error "Unable to remove child, the parent does not know this child on the diagram graph"))
  (unless (member* parent (oref child diagram-parents) :test 'eq)
    (error "Unabel to remove child, the child does not know this parent on the diagram graph"))

  (oset parent diagram-children (seq-filter (lambda (element)
                                              (not (eq element child)))
                                            (oref parent diagram-children)))
  (oset child diagram-parents (seq-filter (lambda (element)
                                            (not (eq element parent)))
                                          (oref child diagram-parents))))

(defclass scxmld-synthetic-element (scxmld-with-diagram-parents scxmld-with-diagram-children)
  ()
  :abstract t
  :documentation "Marker class which indicates that an element is
  synthetic.  A synthetic element is one which is not a real
  <scxml> xml element, but is an object which needs to be
  drawn.")
(cl-defmethod make-instance ((class (subclass scxmld-synthetic-element)) &rest slots)
  "make it"
  ;; TODO - I don't think I need this anymore.
  (message "make-instance scxmld-synthetic-element")
  (message "------------- has next method: %s" (cl-next-method-p))
  (cl-call-next-method))
(defsubst scxmld-synthetic-element-class-p (any)
  "Equivalent of (object-of-class-p ANY 'scxml-synthetic-element)."
  (and any (object-of-class-p any 'scxmld-synthetic-element)))
(cl-defmethod scxml-parent ((element scxmld-synthetic-element))
  "Always returns nil.

Synthetic elements do not have parents (or children) in the scxml
graph."
  nil)
(cl-defmethod scxml-children ((element scxmld-synthetic-element))
  "Always returns nil.

Synthetic elements do not have parents (or children) in the scxml
graph."
  nil)
(cl-defmethod scxml-add-child :before ((parent scxml-element) (child scxmld-synthetic-element) &optional append)
  "Always throw an error.

It is never possible to have a synthetic element participate in the real <scxml> tree/graph."
  (error "Unable to add a synthetic child element into the scxmld tree"))
(cl-defmethod scxmld-add-child ((parent scxmld-element) (child scxmld-synthetic-element) &optional append)
  "Place the synthetic CHILD element as a diagram child."
  (unless (scxmld-with-diagram-children-child-p parent)
    (error "Unable to add a synthetic element to this parent element as it is not supported"))
  (scxmld-add-diagram-child parent child))
;; TODO - implement this as a minor optimization
;; (cl-defmethod scxmld-parents ((element scxmld-synthetic-element))
;;   "Return the parent of ELEMENT.

;; Synthetic elements only exist in the drawing graph."
  
;;   (scxmld-get-synth-parent element))
(cl-defmethod scxmld-add-child :before ((synth-parent scxmld-synthetic-element) (element scxmld-element) &optional _unused)
  (unless (scxmld-synthetic-element-child-p element)
    (error "Unable to add a non-synthetic scxmld-element as the child of a synthetic element.")))
;; (cl-defmethod scxmld-add-child ((synth-parent scxmld-synthetic-element) (synth-child scxmld-synthetic-element) &optional _usused)
;;   "Add SYNTH-CHILD to the parent ELEMENT."
;;   (when (oref synth-parent _synth-child))
;;   t
;;           "Unable to add synthetic element child to synthetic element parent, parent already has a synthetic child")
;;   (assert (null (oref synth-child _synth-parent))
;;           t
;;           "Unable to set sythetic element parent of the child eelement, child already has a synthetic parent")
;;   (scxmld-set-synth-parent synth-child synth-parent)
;;   (scxmld-set-synth-child synth-parent synth-child))
(cl-defmethod scxmld-add-child :before ((parent scxmld-element) (child scxmld-synthetic-element) &optional append)
  "Add a synthetic child to a scxmld element, this is the same as setting the diagram child."
  ;; TODO - low priority but there's probably a better way to do this.
  (unless (scxmld-with-diagram-children-child-p parent)
    (error "Unable to add a synthetic child to this element, it is not able to hold diagram graph only children")))
(cl-defmethod scxml-root-element ((element scxmld-synthetic-element))
  "The scxml root for a synthetic element will always be nil"
  nil)

(defclass scxmld-with-synthetic-initial (scxmld-with-diagram-children)
  ()
  :abstract t
  :documentation "A marker class that indicates that this element
  can have synthetic diagram-only initial element."
  ;; TODO - this class needs protections such that only one synth
  ;; initial can be in the child list at once.
  )
(defsubst scxmld-with-synthetic-initial-class-p (any)
  "Equivalent of (object-of-class-p ANY 'scxml-drawable-element)."
  (and any (object-of-class-p any 'scxmld-with-synthetic-initial)))
(cl-defgeneric scxmld-get-synth-initial ((element scxmld-element))
  "Return the synthetic-initial element child of ELEMENT if present."
  (error "Not implemented for element of this type"))



;; (cl-defmethod scxmld-add-child :before ((parent scxmld-with-synthetic-initial) (element scxmld-synthetic-element) &optional append)
;;   "Validation only function."
;;   ;; TODO: this is bad design, I'm using the idea of scxmld-sythetic-initial before it's defined.
;;   (unless (scxmld-synthetic-initial-p element)
;;     (error "Only able to add sythetic elements of type scxmld-synthetic-initial")))
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

