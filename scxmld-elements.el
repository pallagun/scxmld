;;; scxmld-elements.el --- scxmld drawable elements -*- lexical-binding: t -*-

;;; Commentary:
;; Drawable versions of what can be found in scxml-elements.el

;;; Code:
(eval-when-compile (require 'subr-x))

(require '2dd)
(require 'scxml)
(require 'scxmld-element)

;; Note: M-x (list-colors-display) to see all colors
(defface scxmld-edit-idx
  '((t :foreground "yellow"))
  "Edit idx style for any drawing."
  :group 'scxmld-faces)
(defface scxmld-scxml-outline
  '((t :foreground "orange"))
  "scxmld-scxml outlines style."
  :group 'scxmld-faces)
(defface scxmld-state-outline
  '((t :foreground "gold"))
  "scxmld-state outlines style."
  :group 'scxmld-faces)
(defface scxmld-final-outline
  '((t :foreground "brown"))
  "scxmld-final outlines style."
  :group 'scxmld-faces)
(defface scxmld-parallel-outline
  '((t :foreground "DeepSkyBlue"))
  "scxmld-parallel outlines style."
  :group 'scxmld-faces)
(defface scxmld-transition-outline
  '((t :foreground "DarkMagenta"))
  "scxmld-transition outlines style."
  :group 'scxmld-faces)
(defface scxmld-transition-arrow
  '((t :foreground "magenta"))
  "scxmld-transition outlines style."
  :group 'scxmld-faces)
(defface scxmld-outline-marked
  '((t :foreground "green"))
  "scxmld-scxml outlines style."
  :group 'scxmld-faces)
(defface scxmld-label-marked
  '((t :foreground "yellow"))
  "scxmld-scxml outlines style."
  :group 'scxmld-faces)

;; Drawable elements
;; (defclass scxmld-scxml (2dd-rect scxmld-element scxml-scxml scxmld-with-highlight scxmld-with-synthetic-initial)
;;   ())
(defclass scxmld-scxml (2dd-rect
                        scxmld-with-synthetic-initial
                        scxmld-with-diagram-children
                        scxmld-element
                        scxml-scxml
                        scxmld-with-highlight)
  ())
(cl-defmethod scxmld-pprint ((element scxmld-scxml))
  "Pretty print this <scxml> ELEMENT."
  (format "scxml[name:%s,[%s] %s]"
          (scxml-get-name element)
          (if (scxmld-get-highlight element) "H" "")
          (2dd-pprint element)))
(cl-defmethod make-instance ((class (subclass scxmld-scxml)) &rest slots)
  "Ensure the drawing label matches the <scxml> element's name attribute."
  (let ((name (plist-get slots :name))
        (instance (cl-call-next-method)))
    (2dd-set-constraint instance 'free)
    (when name
      (2dd-set-label instance name))
    instance))
(cl-defmethod 2dd-render ((rect scxmld-scxml) scratch x-transformer y-transformer viewport &rest style-plist)
  (let ((has-highlight (scxmld-get-highlight rect)))
    (cl-call-next-method rect
                         scratch
                         x-transformer
                         y-transformer
                         viewport
                         (list :outline-style (if has-highlight 'scxmld-outline-marked 'scxmld-scxml-outline)
                               :label-style (if has-highlight 'scxmld-label-marked nil)
                               :edit-idx-style 'scxmld-edit-idx))))
(cl-defmethod scxml-set-name :after ((element scxmld-scxml) value)
  "Set the scxml-drawing label to match ELEMENT's new name VALUE."
  (2dd-set-label element value))
(cl-defmethod scxmld-put-attribute ((element scxmld-scxml) (attribute-name string) attribute-value)
  "Set ELEMENT's attribute with name ATTRIBUTE-NAME to be ATTRIBUTE-VALUE.

When ATTRIBUTE-VALUE is nil the attribute will be deleted if possible.

Empty strings will be converted to nil values.

Special cases here are: name, initial, datamodel and binding."
  (when (seq-empty-p attribute-value)
    (setq attribute-value nil))
  (pcase attribute-name
    ("name" (scxml-set-name element attribute-value))
    ("initial" (scxml-set-initial element attribute-value))
    ("datamodel" (scxml-set-datamodel element attribute-value))
    ("binding" (scxml-set-binding element attribute-value))
    ;; else, put it into the attrib hash.
    (_ (if attribute-value
           (scxml-put-attrib element attribute-name attribute-value)
         (scxml-delete-attrib element attribute-name)))))
(cl-defmethod scxmld-short-name ((element scxmld-scxml))
  "Return a string with the name of ELEMENT for display."
  (let ((name (scxml-get-name element)))
    (if name
        name
      "?No-Name?")))
(cl-defmethod scxml-set-initial :after ((scxml scxml-scxml) initial)
  ;; TODO: why is this here?
  (message "make a synth element"))
(cl-defmethod 2dd-serialize-geometry ((scxml scxmld-scxml))
  "serialize multiple geometry objects if possible?

TODO - this really needs to be refactored."
  (let* ((parent-geo-string (cl-call-next-method))
         (synth-initial (scxmld-get-synth-initial scxml))
         (synth-initial-geo-string (when synth-initial
                                     (2dd-serialize-geometry synth-initial)))
         (synth-transition (when synth-initial
                             (first (scxmld-children synth-initial))))
         (synth-transition-geo-string (when synth-transition
                                        (2dd-serialize-geometry synth-transition))))
    (mapconcat #'identity
               (list parent-geo-string
                     synth-initial-geo-string
                     synth-transition-geo-string)
               " ")))

(defsubst scxmld--parent-is-parallel-p (any)
  "Return true if ANY's parent is a <parallel> element."
  (let ((parent (scxml-parent any)))
    (if (and parent (scxml-parallel-class-p parent))
        t
      nil)))


(defun scxmld--get-targeting-transitions (element)
  "If ELEMENT has an id attribute, return all transition elements which target it.

Returns a list of elements.  May return nil if there are no
targeting transitions."
  (error "this has to be reimplemented.... it can't work like this.")
  ;; TODO - setup a single state in an scxml, give the scxml an
  ;; initial attribute that references that state.  Modify the state
  ;; drawing such that the synthetic initial's transition arrow will
  ;; be reploted - note this function get's hit twice.  Why??  It
  ;; should only be getting hit once.
  (when (scxml-element-with-id-class-p element)
    (let ((id (scxml-get-id element)))
      (unless (seq-empty-p id)
        (let ((targeting-transitions))
          (scxmld-visit-all element
                            (lambda (transition)
                              (when (equal id (scxml-get-target-id transition))
                                (push transition targeting-transitions)))
                            (lambda (element)
                              (or (scxml-transition-class-p element)
                                  (scxmld-synthetic-transition-p element))))
          targeting-transitions)))))

(defclass scxmld-state (2dd-rect
                        scxmld-with-synthetic-initial
                        scxmld-with-diagram-children
                        scxmld-element
                        scxml-state
                        scxmld-with-highlight)
  ())
(cl-defmethod scxmld-pprint ((element scxmld-state))
  "Pretty print this <state> ELEMENT."
  (format "state[id:%s,[%s] %s]"
          (scxml-get-id element)
          (if (scxmld-get-highlight element) "H" "")
          (2dd-pprint element)))
(cl-defmethod make-instance ((class (subclass scxmld-state)) &rest slots)
  "Ensure the drawing label matches the <state> element's id attribute."
  (let ((id (plist-get slots :id))
        (instance (cl-call-next-method)))
    (2dd-set-constraint instance 'captive+exclusive)
    (when id
      (2dd-set-label instance id))
    instance))
;; (cl-defmethod scxmld-children ((state scxmld-state))
;;   "Return the children of the STATE element."
;;   ;; TODO - I think I can do this with an nconc if the last list is the real children.
;;   (append (scxmld--get-targeting-transitions state)
;;           (cl-call-next-method)))
(cl-defmethod 2dd-render ((rect scxmld-state) scratch x-transformer y-transformer viewport &rest style-plist)
  (let ((has-highlight (scxmld-get-highlight rect)))
    (cl-call-next-method rect
                         scratch
                         x-transformer
                         y-transformer
                         viewport
                         (list :outline-style (if has-highlight
                                                  'scxmld-outline-marked
                                                'scxmld-state-outline)
                               :no-outline (if (scxmld--parent-is-parallel-p rect)
                                               (not has-highlight)
                                             nil)
                               :label-style (if has-highlight 'scxmld-label-marked nil)
                               :edit-idx-style 'scxmld-edit-idx))))
(cl-defmethod scxml-set-id :after ((element scxmld-state) value)
  "Set the scxml-drawing label to match ELEMENT's new id VALUE."
  (2dd-set-label element value))
(cl-defmethod scxmld-put-attribute ((element scxmld-state) (attribute-name string) attribute-value)
  "Set ELEMENT's attribute with name ATTRIBUTE-NAME to be ATTRIBUTE-VALUE.

When ATTRIBUTE-VALUE is nil the attribute will be deleted if possible.

Special cases here are: id, initial."
  (pcase attribute-name
    ("id" (scxml-set-id element attribute-value))
    ("initial" (scxml-set-initial element attribute-value))
    ;; else, put it into the attrib hash.
    (_ (if attribute-value
           (scxml-put-attrib element attribute-name attribute-value)
         (scxml-delete-attrib element attribute-name)))))
(cl-defmethod 2dd-set-edit-idx :before ((element scxmld-state) value)
  "If this state is a child of a parallel, do not enable edit idxs."
  (when (and value (scxmld--parent-is-parallel-p element))
    (error "Unable to enter edit-idx mode, parent of this element is a <parallel>")))
;; (cl-defmethod scxmld-short-name ((state scxmld-state))
;;   "Return a short string identifying STATE."
;;   (scxmld--get-short-name-from-id state))

(defclass scxmld-final (2dd-rect
                        scxmld-with-diagram-children
                        scxmld-element
                        scxml-final
                        scxmld-with-highlight)
  ())
(cl-defmethod scxmld-pprint ((element scxmld-final))
  "Pretty print this <final> ELEMENT."
  (format "final[id:%s,[%s] %s]"
          (scxml-get-id element)
          (if (scxmld-get-highlight element) "H" "")
          (2dd-pprint element)))
(cl-defmethod make-instance ((class (subclass scxmld-final)) &rest slots)
  "Ensure the drawing label matches the <final> element's id attribute."
  (let ((id (plist-get slots :id))
        (instance (cl-call-next-method)))
    (2dd-set-constraint instance 'captive+exclusive)
    (when id
      (2dd-set-label instance id))
    instance))
;; (cl-defmethod scxmld-children ((final scxmld-final))
;;   "Return the children of the FINIAL element."
;;   ;; TODO - I think I can do this with an nconc if the last list is the real children.
;;   (append (scxmld--get-targeting-transitions final)
;;           (cl-call-next-method)))
(cl-defmethod 2dd-render ((rect scxmld-final) scratch x-transformer y-transformer viewport &rest style-plist)
  (let ((has-highlight (scxmld-get-highlight rect)))
    (cl-call-next-method rect
                         scratch
                         x-transformer
                         y-transformer
                         viewport
                         (list :outline-style (if (scxmld-get-highlight rect)
                                                  'scxmld-outline-marked
                                                'scxmld-final-outline)
                               :label-style (if has-highlight 'scxmld-label-marked nil)
                               :edit-idx-style 'scxmld-edit-idx))))
(cl-defmethod scxml-set-id :after ((element scxmld-final) value)
  "Set the scxml-drawing label to match ELEMENT's new id VALUE."
  (2dd-set-label element value))
(cl-defmethod scxmld-put-attribute ((element scxmld-final) (attribute-name string) attribute-value)
  "Set ELEMENT's attribute with name ATTRIBUTE-NAME to be ATTRIBUTE-VALUE.

When ATTRIBUTE-VALUE is nil the attribute will be deleted if possible.

Special cases here are: id"
  (pcase attribute-name
    ("id" (scxml-set-id element attribute-value))
    (_ (if attribute-value
           (scxml-put-attrib element attribute-name attribute-value)
         (scxml-delete-attrib element attribute-name)))))

(defclass scxmld-parallel (2dd-division-rect
                           scxmld-with-diagram-children
                           scxmld-element
                           scxml-parallel
                           scxmld-with-highlight)
  ())
(cl-defmethod scxmld-pprint ((element scxmld-parallel))
  "Pretty print this <parallel> ELEMENT."
  (format "parallel[id:%s,[%s] %s]"
          (scxml-get-id element)
          (if (scxmld-get-highlight element) "H" "")
          (2dd-pprint element)))
(cl-defmethod make-instance ((class (subclass scxmld-parallel)) &rest slots)
  "Ensure the drawing label matches the <parallel> element's id attribute."
  (let ((id (plist-get slots :id))
        (instance (cl-call-next-method)))
    (2dd-set-constraint instance 'captive+exclusive)
    (when id
      (2dd-set-label instance id))
    instance))
;; (cl-defmethod scxmld-children ((parallel scxmld-parallel))
;;   "Return the children of the ELEMENT."
;;   ;; TODO - I think I can do this with an nconc if the last list is the real children.
;;   (append (scxmld--get-targeting-transitions parallel)
;;           (cl-call-next-method)))
(cl-defmethod 2dd-render ((rect scxmld-parallel) scratch x-transformer y-transformer viewport &rest style-plist)
  (let ((has-highlight (scxmld-get-highlight rect)))
    (cl-call-next-method rect
                         scratch
                         x-transformer
                         y-transformer
                         viewport
                         (list :outline-style (if has-highlight
                                                  'scxmld-outline-marked
                                                'scxmld-parallel-outline)
                               :no-outline (if (scxmld--parent-is-parallel-p rect)
                                               (not has-highlight)
                                             nil)
                               :label-style (if has-highlight 'scxmld-label-marked nil)
                               :edit-idx-style 'scxmld-edit-idx))))
(cl-defmethod scxml-set-id :after ((element scxmld-parallel) value)
  "Set the scxml-drawing label to match ELEMENT's new id VALUE."
  (2dd-set-label element value))
(cl-defmethod scxmld-put-attribute ((element scxmld-parallel) (attribute-name string) attribute-value)
  "Set ELEMENT's attribute with name ATTRIBUTE-NAME to be ATTRIBUTE-VALUE.

When ATTRIBUTE-VALUE is nil the attribute will be deleted if possible.

Special cases here are: id"
  (pcase attribute-name
    ("id" (scxml-set-id element attribute-value))
    (_ (if attribute-value
           (scxml-put-attrib element attribute-name attribute-value)
         (scxml-delete-attrib element attribute-name)))))

(defclass scxmld-transition (2dd-link
                             scxmld-with-diagram-parents
                             scxmld-element
                             scxml-transition
                             scxmld-with-highlight)
  ())
(cl-defmethod make-instance ((class (subclass scxmld-transition)) &rest slots)
  "Ensure the transition is set up correctly and poperly setup source and target drawing connectors.

Find the :target in SLOTS and properly set the 2dd-link drawing to use it as well"
  ;; something is wrong here.
  (message "make-instance scxmld-transition")
  (message "------------- has next method: %s" (cl-next-method-p))

  
  (let (;; (what (cl-next-method-p))
        ;; (other (cl-generic-current-method-specializers))
        (instance (cl-call-next-method)))
    (2dd-set-constraint instance 'free)
    ;; The below line will always fail, when the transition is first
    ;; created it won't yet be a part of the element graph and will
    ;; not be able to find its target.
    ;; (scxmld--transition-update-target-drawing instance (plist-get slots :target))
    instance))
(defsubst scxmld-transition-class-p (any-object)
  "Equivalent of (object-of-class-p ANY-OBJECT 'scxml-transition)"
  (and any-object
       (object-of-class-p any-object 'scxmld-transition)))
(cl-defmethod scxmld-pprint ((element scxmld-transition))
  "Pretty print this <transition> ELEMENT."
  (let ((parent (scxml-parent element)))
    (format "transition[from:%s, to:%s, dr:%s]"
            (if parent
                (scxmld-short-name parent)
              "UNK?")
            (scxml-get-target-id element)
            (2dd-pprint element))))
(cl-defmethod 2dd-render ((element scxmld-transition) scratch x-transformer y-transformer viewport &rest style-plist)
  (let ((has-highlight (scxmld-get-highlight element))
        (parent (car (scxmld-parents element))))
    (assert parent t "Unable to determine parent in scxml tree")
    (cl-call-next-method element
                         scratch
                         x-transformer
                         y-transformer
                         viewport
                         (list :connector-offset (2dd-get-point-scaling viewport)
                               :link-start nil ;nothing at the start of transitions
                               :link-end 'arrow ;arrows at the ends of transitions
                               :link-source (if (scxml-initial-class-p parent);; (scxmld-initial-p parent)
                                                nil
                                              'circle)
                               :link-target 'circle
                               :end-style (if has-highlight
                                              'scxmld-outline-marked
                                            'scxmld-transition-arrow)
                               :outline-style (if has-highlight
                                                  'scxmld-outline-marked
                                                'scxmld-transition-outline)
                               :edit-idx-style 'scxmld-edit-idx))))
(cl-defmethod scxmld-put-attribute ((element scxmld-transition) (attribute-name string) attribute-value)
  "Set ELEMENT's attribute with name ATTRIBUTE-NAME to be ATTRIBUTE-VALUE.

When ATTRIBUTE-VALUE is nil the attribute will be deleted if possible.

Special cases here are: target, event, cond, type"
  (pcase attribute-name
    ("target" (scxml-set-target-id element attribute-value))
    ("event" (scxml-set-events element attribute-value))
    ("cond" (scxml-set-cond-expr element attribute-value))
    ("type" (scxml-set-type element attribute-value))
    (_ (if attribute-value
           (scxml-put-attrib element attribute-name attribute-value)
         (scxml-delete-attrib element attribute-name)))))
(cl-defmethod scxml-add-child :after ((parent scxmld-element) (transition scxmld-transition) &optional append)
  "Ensure that the 2dd drawing connections are updated after this add."
  ;; TODO - there should be another update for make-orphan
  (2dd-set-source transition parent)
  (scxmld--transition-update-target-drawing transition
                                            (scxml-get-target-id transition)))
(cl-defmethod scxml-make-orphan :after ((element scxmld-transition))
  "When orphaning an element, ensure the drawing is separated as well."
  (2dd-set-source element nil)
  (2dd-set-target element nil)
  (2dd-clear-inner-path element))
(cl-defmethod scxml-set-target-id :before ((element scxmld-transition) target-id)
  "Remove any diagram graph connections before altering the transition target"
 
  ;; this should _not_ run for synthetic transitions which are handled
  ;; a good bit differently.
  (unless (scxmld-synthetic-transition-p element)
    
    ;; if the transition has a diagram graph parent, orphan it.
    (let ((diagram-parents (scxmld-get-diagram-parents element)))
      (assert (<= (length diagram-parents) 1)
              t
              "Transitions should not have 2 or more diagram graph parents.")
      (let ((diagram-parent (first diagram-parents)))
        (when diagram-parent
          (scxmld-make-orphan element diagram-parent))))))
(cl-defmethod scxml-set-target-id :after ((element scxmld-transition) target-id)
  "Update diagram graph and drawings when changing a transition target"
  (scxmld--update-diagram-graph-after-add
   ;; (scxml-parent element)
   (first (scxmld-parents element))
   element)
  (scxmld--transition-update-target-drawing element target-id))
(defun scxmld--transition-update-target-drawing (transition target-id)
  "Update TRANSITION's drawing to properly reflect a new TARGET-ID"
  (if (and target-id (not (seq-empty-p target-id)))
      (let ((target-transition (scxml-element-find-by-id
                                (scxml-root-element transition)
                                target-id)))
        (when target-transition
          (2dd-set-target transition target-transition)))
    (2dd-set-target transition nil)))
;; because transitions can have more than one parent, we must handle those.
(cl-defmethod scxmld-add-child :after ((parent scxmld-element) (child scxmld-element) &optional append)
  (scxmld--update-diagram-graph-after-add parent child))
(defun scxmld--update-diagram-graph-after-add (parent child)
  "Call this function after CHILD has been added to PARENT to update the diagram graph"
  ;; evaluate all elements in the child tree to find if any of them
  ;; are either transitions (of any type) or elements with a valid id.
  ;; These are things which could link to another thing or be linked
  ;; to by another thing.
  ;;
  ;; Then, evaluate all elements in the parent tree which are not in
  ;; the child tree.  Any of them which are transitions (of any type)
  ;; or elements which have a valid id could be linked.
  ;;
  ;; The linkings will only exist between these two groups (though in either direction)
  ;;
  (assert (and parent child)
          t
          "Parent and child must be valid")
          
  (let ((child-tree-elements)
        (parent-tree-elements)
        (child-tree-elements-by-id)
        (parent-tree-elements-by-id)
        (child-tree-transitions)
        (parent-tree-transitions))
    (scxmld-visit child
                  (lambda (element)
                    (push element child-tree-elements)
                    (when (scxml-element-with-id-class-p element)
                      (let ((id (scxml-get-id element)))
                        (unless (seq-empty-p id)
                          (setf (alist-get id child-tree-elements-by-id nil nil 'equal)
                                element))))
                    (when (scxmld-transition-class-p element)
                      (push element child-tree-transitions))))
    (scxmld-visit-all parent
                      (lambda (element)
                        (push element parent-tree-elements)
                        (when (scxml-element-with-id-class-p element)
                          (let ((id (scxml-get-id element)))
                            (unless (seq-empty-p id)
                              (setf (alist-get id parent-tree-elements-by-id nil nil 'equal)
                                    element))))
                        (when (scxmld-transition-class-p element)
                          (push element parent-tree-transitions)))
                      (lambda (element)
                        (not (member* element child-tree-elements :test 'eq))))
    ;; find any transitions in the child tree which direct to elements
    ;; existing in the parent tree and link them up
    (mapc (lambda (child-tree-transition)
            (let ((transition-target-id (scxml-get-target-id child-tree-transition)))
              (unless (seq-empty-p transition-target-id)
                (let ((parent-tree-target (alist-get transition-target-id
                                                     parent-tree-elements-by-id
                                                     nil
                                                     nil
                                                     'equal)))
                  (when parent-tree-target
                    ;; a child tree has a transition to a parent tree target,
                    ;; link them!
                    ;; (assert (not (member* child-tree-transition
                    ;;                       (scxmld-get-diagram-childen parent-tree-target)
                    ;;                       :test 'eq))
                    ;;         t
                    ;;         "Found child tree transition is _already_ linked to parent tree target?")
                    (scxmld-add-diagram-child parent-tree-target child-tree-transition))))))
          child-tree-transitions)
    ;; find any transitions in the parent tree which direct to
    ;; elements existing in the child tree and link them up
    (mapc (lambda (parent-tree-transition)
            (let ((transition-target-id (scxml-get-target-id parent-tree-transition)))
              (unless (seq-empty-p transition-target-id)
                (let ((child-tree-target (alist-get transition-target-id
                                                    child-tree-elements-by-id
                                                    nil
                                                    nil
                                                    'equal)))
                  (when child-tree-target
                    ;; a parent tree has a transition to a child tree target,
                    ;; link them!
                    ;; (assert (not (eq parent-tree-transition
                    ;;                  (scxmld-get-diagram-children child-tree-target)))
                    ;;         t
                    ;;         "Found parent tree transition is _already_ linked to child tree target?")
                    (scxmld-add-diagram-child child-tree-target parent-tree-transition))))))
          parent-tree-transitions)
    ))


(defclass scxmld-initial (2dd-point
                          scxmld-element
                          scxml-initial
                          scxmld-with-highlight)
  ())
(cl-defmethod scxmld-short-name ((element scxmld-initial))
  "Return a string with the name of ELEMENT for display."
  ;; TODO - this could be better.
  "initial")
(cl-defmethod 2dd-get-edit-idx ((initial scxmld-initial))
  (error "scxmld-initial does not have any edit-idxs."))
(cl-defmethod scxmld-pprint ((element scxmld-initial))
  "Pretty print this <initial> ELEMENT."
  (format "initial[[%s] %s]"
          (if (scxmld-get-highlight element) "H" "")
          (2dd-pprint element)))
(cl-defmethod make-instance ((class (subclass scxmld-initial)) &rest slots)
  "Ensure the drawing label matches the <final> element's id attribute."
  (let ((instance (cl-call-next-method)))
    (2dd-set-constraint instance 'captive+exclusive)
    (2dd-set-label instance "I")
    instance))
(cl-defmethod 2dd-render ((point scxmld-initial) scratch x-transformer y-transformer viewport &rest style-plist)
  (let ((has-highlight (scxmld-get-highlight point)))
    (cl-call-next-method point
                         scratch
                         x-transformer
                         y-transformer
                         viewport
                         (list :label-style (if has-highlight 'scxmld-label-marked nil)))))
(cl-defmethod scxmld-put-attribute ((element scxmld-initial) (attribute-name string) attribute-value)
  "Set ELEMENT's attribute with name ATTRIBUTE-NAME to be ATTRIBUTE-VALUE.

When ATTRIBUTE-VALUE is nil the attribute will be deleted if possible.

There are no special cases for <initial> elements."
  (if attribute-value
      (scxml-put-attrib element attribute-name attribute-value)
    (scxml-delete-attrib element attribute-name)))

;; TODO - these two elements really need their own graph edges.  Right
;; now there is a barely sufficient isolation, that's not ideal.
(defclass scxmld-synthetic-transition (;; scxmld-with-diagram-parents
                                       ;; nothing
                                       scxmld-transition
                                       ;; expected
                                       scxmld-synthetic-element
                                       ) ;
  ()
  :documentation "Used to represent a transition from a scxmld-synthetic-initial.")
(cl-defmethod make-instance ((class (subclass scxmld-synthetic-transition)) &rest slots)
  "make it"
  ;; TODO - I don't think I need this anymore
  (message "make-instance scxmld-synthetic-transition")
  (cl-call-next-method))
(cl-defmethod scxml-set-target-id :before ((element scxmld-synthetic-transition) target-id)
  "Remove the second diagram graph connections before altering the transition target.

Because this is a synthetic transition there will be no real
scxml tree parents.  However, the diagram graph parents are
ordered.  The first will always be the true parent of the
transition and the second will be the target (if it exists)."
  ;; if the transition has a diagram graph parent, orphan it.
  (let ((diagram-parents (scxmld-get-diagram-parents element)))
    (assert (<= 1 (length diagram-parents) 2)
            t
            "Transitions should not have 2 or more diagram graph parents.")
    (let ((diagram-parent (second diagram-parents)))
      (when diagram-parent
        (scxmld-make-orphan element diagram-parent)))))


(defclass scxmld-synthetic-initial (scxmld-with-diagram-parents
                                    scxmld-with-diagram-children
                                    scxmld-initial
                                    scxmld-synthetic-element)
  ()
  :documentation "Used to represent an initial attribute, not an element.")
(cl-defmethod make-instance ((class (subclass scxmld-synthetic-initial)) &rest slots)
  "Ensure the drawing label matches the <final> element's id attribute."
  (let ((instance (cl-call-next-method)))
    ;; (2dd-set-constraint instance 'captive+exclusive)
    (assert (slot-boundp instance '_constraint)
            t
            "The constraints slot needs to be bound here.")
    (assert (eq (2dd-get-constraint instance) 'captive+exclusive)
            t
            "The constraints for this should be setup and should be captive+exclusive")
    (2dd-set-label instance "i")
    instance))
(defsubst scxmld-synthetic-initial-class-p (any-object)
  "Equivalent of (object-of-class-p ANY-OBJECT 'scxmld-synthetic-initial)"
  (and any-object
       (object-of-class-p any-object 'scxmld-synthetic-initial)))
(cl-defmethod scxmld-get-synth-initial ((element scxmld-with-synthetic-initial))
  "Return the synthetic-initial element child of ELEMENT if present."
  (first (seq-filter 'scxmld-synthetic-initial-class-p
                     (scxmld-get-diagram-children element))))

;; (cl-defmethod scxml-parent ((element scxmld-synthetic-initial))
;;   ;; TODO - For now I'm hijacking the scxml graph to handle this.
;;   ;; this needs to be fixed because I'm not doing the same for
;;   ;; scxmld-synthetic-transition
;;   "Return the parent of ELEMENT."
;;   (scxmld-get-synth-parent element))
(cl-defgeneric scxmld-get-synthetic-target-id ((element scxmld-synthetic-initial))
  "Return the current target of ELEMENT's synthetic transition drawings."
  (let ((synth-transitions (scxml-children synth-initial)))
    (assert (eq (length synth-transitions) 1)
            t
            "A synthetic initial element must have exactly one sythetic transition.")
    (assert (scxml-transition-class-p (first synth-transitions))
            t
            "A synthetic initial element must have a child which is a transition")
    (scxml-get-target-id (first synth-transitions))))





(provide 'scxmld-elements)
;;; scxmld-elements.el ends here
