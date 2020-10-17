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
  ;; TODO - this can be a one line or sexp??
  (let ((name (scxml-get-name element)))
    (if name
        name
      "?No-Name?")))
(cl-defmethod scxml-set-initial :after ((scxml scxmld-scxml) initial &optional do-not-validate)
  "Check a bunch of things???? not sure why this function exists"
  ;; TODO: why is this here?
  ;; ensure there's a synth initial and synth transition with the proper
  ;; configuration, if not, add them.

  ;; real quick, assert you have 1 or zero existing synthetic-initial-elements
  (assert (<= (length (seq-filter 'scxmld-synthetic-initial-p
                                  (scxmld-children scxml)))
              1)
          t
          "<scxml> element failed a basic sanity check, it has > 1 synthetic initial")
  (let* ((synth-initial (first (seq-filter 'scxmld-synthetic-initial-p
                                            (scxmld-children scxml))))
         (synth-transition (and synth-initial
                                (first (scxmld-children synth-initial)))))
    (assert (or (null synth-transition)
                (scxmld-synthetic-transition-p synth-transition))
            t
            "The first child of a synthetic-initial must always be a synthetic-transition")
    (if initial
        ;; You have an initial value set, make sure it's there.
        (assert (equal (scxml-get-target-id synth-transition)
                       initial)
                t
                "synthetic-transition's target does not match <scxml>'s initial= attribute")
      ;; you have no initial value set, make sure you have (no
      ;; initial/transition) or (no target set on your transition)
      (assert (or (and (null synth-initial)
                       (null synth-transition))
                  (null (scxml-get-target-id synth-transition)))
              t
              "<scxml> has no initial= attribute, but an active synthetic-transition exists?"))))

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

;; this was scxml-add-child :after, but it was changed
(cl-defmethod scxmld-add-child :after ((parent scxmld-element) (transition scxmld-transition) &optional append)
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
(cl-defmethod scxmld-make-orphan :after ((element scxmld-transition) &optional parent)
  "If PARENT represents the target= of ELEMENT, remove the 2dd connector for the target."
  (unless parent
    (error "orphaning a transition requires a parent"))

  ;; The case that needs to be handled here is that the transition was
  ;; separated from it's target.  This can happen if the targeted
  ;; state is deleted (or made orphan).  In that case we'll have to
  ;; clean up the 2dd drawing linkage as well.
  (when (eq parent (2dd-get-target element))
    ;; the target _was_ removed from the drawing, set the target of
    ;; this connector to be unlinked
    (2dd-disconnect (2dd-get-target-connector element))))

(cl-defmethod scxml-set-target-id :before ((element scxmld-transition) target-id)
  "Remove any existing diagram graph connections of ELEMENT due to the old TARGET-ID."
 
  ;; this should _not_ run for synthetic transitions which are handled
  ;; a good bit differently.
  
  ;; TODO - this is a bit weird to have a parent class
  ;; (scxmld-transition) know about a child class
  ;; (scxmld-synthetic-transition).  Maybe it could be phrased in a
  ;; better way.
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
                                (scxmld-root-element transition)
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
        (child-tree-elements-by-id)
        (child-tree-transitions)
        (parent-tree-elements)
        (parent-tree-elements-by-id)
        (parent-tree-transitions))
    
    ;; collect all child-tree-elements by id and child-tree transitions
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
    ;; collect all parent-tree-elements by id and parent-tree transitions
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
    
    ;; find any transitions in the child tree which target elements
    ;; existing in the parent tree and link them up
    (mapc (lambda (child-tree-transition)
            (let ((transition-target-id (scxml-get-target-id child-tree-transition)))
              (unless (seq-empty-p transition-target-id)
                (when-let ((parent-tree-target (alist-get transition-target-id
                                                          parent-tree-elements-by-id
                                                          nil
                                                          nil
                                                          'equal)))
                  ;; found the target of this child-tree transition in
                  ;; the parent-tree, link it up.
                  (scxmld-add-diagram-child parent-tree-target child-tree-transition t)
                  ;; if this child-tree transition is a synthetic one:
                  (when-let (((scxmld-synthetic-transition-p child-tree-transition))
                             ;; and there is a parent
                             (parent (first (scxmld-parents child-tree-transition)))
                             ;; and there is a grandparent
                             (grandparent (first (scxmld-parents parent))))
                    (assert (scxmld-synthetic-initial-p parent)
                            t
                            "Detected an invalid graph, primary parent of a synthetic transition should be a synthetic initial")
                    
                    (assert (scxml-element-with-initial-child-p grandparent)
                            t
                            "Detected an invalid graph, primary grandparent of a synthetic transition should be a scxml-element-with-initial")
                    (when (member* grandparent parent-tree-elements :test 'eq)
                      ;; this is a valid synthetic transition and the
                      ;; attributed element is in the other tree
                      ;; (parent tree), set the initial attribute
                      ;; value.
                      
                      (assert
                       ;; This assert will fire if you're adding in
                       ;; some synthetic elements to a <scxml> or
                       ;; <state> but the elements your adding
                       ;; indicate that the <scxml> or <state> should
                       ;; have an initial= attribute value of
                       ;; something other than what it already is.
                       ;; This seems... bad?  Not sure what to do here
                       (or (null (scxml-get-initial grandparent))
                           (equal transition-target-id (scxml-get-initial grandparent)))
                       t
                       "I don't think this is an error, but it's something")
                      
                      (scxml-set-initial grandparent transition-target-id t)))))))
          child-tree-transitions)
    ;; find any transitions in the parent tree which direct to
    ;; elements existing in the child tree and link them up
    (mapc (lambda (parent-tree-transition)
            (let ((transition-target-id (scxml-get-target-id parent-tree-transition)))
              (unless (seq-empty-p transition-target-id)
                (when-let ((child-tree-target (alist-get transition-target-id
                                                    child-tree-elements-by-id
                                                    nil
                                                    nil
                                                    'equal)))
                  ;; found the target of this parent-tree transition
                  ;; in the child-tree, link it up
                  (scxmld-add-diagram-child child-tree-target parent-tree-transition t)
                  ;; if this parent-tree transition is a synthetic one:
                  (when-let (((scxmld-synthetic-transition-p parent-tree-transition))
                             ;; and there is a parent
                             (parent (first (scxmld-parents parent-tree-transition)))
                             ;; and there is a grandparent
                             (grandparent (first (scxmld-parents parent))))
                    (assert (scxmld-synthetic-initial-p parent)
                            t
                            "Detected an invalid graph, primary parent of a synthetic transition should be a synthetic initial")
                    
                    (assert (scxml-element-with-initial-child-p grandparent)
                            t
                            "Detected an invalid graph, primary grandparent of a synthetic transition should be a scxml-element-with-initial")
                    (when (member* grandparent child-tree-elements :test 'eq)
                      ;; this is a valid synthetic transition and the
                      ;; attributed element is in the other tree
                      ;; (child tree), set the initial attribute
                      ;; value.
                      
                      (assert
                       ;; This assert will fire if you're adding in
                       ;; some synthetic elements to a <scxml> or
                       ;; <state> but the elements your adding
                       ;; indicate that the <scxml> or <state> should
                       ;; have an initial= attribute value of
                       ;; something other than what it already is.
                       ;; This seems... bad?  Not sure what to do here
                       (or (null (scxml-get-initial grandparent))
                           (equal transition-target-id (scxml-get-initial grandparent)))
                       t
                       "I don't think this is an error, but it's something")
                      
                      (scxml-set-initial grandparent transition-target-id t)))))))
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
  (when-let ((anchor-element
              (scxmld--get-first-and-primary-non-synthetic-parent element)))
    (assert (scxml-element-with-initial-child-p anchor-element)
            t
            "Synthetic transition does not have a reasonable <scxml> or <state> element as a grandparent")
    (scxmld--clear-initial-attribute anchor-element))
  (let ((diagram-parents (scxmld-get-diagram-parents element)))
    (assert (<= 1 (length diagram-parents) 2)
            t
            "Transitions should not have 2 or more diagram graph parents.")
    (when-let ((diagram-parent (second diagram-parents)))
      (scxmld-make-orphan element diagram-parent))))
(cl-defmethod scxml-set-target-id :after ((element scxmld-synthetic-transition) target-id)
  "Update diagram graph and drawings when changing a transition target"
  (let ((anchor-element
         (scxmld--get-first-and-primary-non-synthetic-parent element)))
    (when anchor-element
      (assert (scxml-element-with-initial-child-p anchor-element)
              t
              "Synthetic transition does not have a reasonable <scxml> or <state> element as a grandparent")
      (scxml-set-initial anchor-element target-id))))
(cl-defmethod scxmld-make-orphan :before ((element scxmld-synthetic-transition) &optional parent)
  "Set the initial= attribute to nil on attributed <element> of ELEMENT.

When you have a synthetic transition (a child of a
synthetic-initial) which represents (graphically) the initial=
attribute of it's grandparent changes in the synthetic initial
are reflected in the grandparent.  In this case we're removing
the synthetic-transition which means there's no longer a valid
linkage for any synthetic-initial element.  This means the parent
of the synthetic-initial (the attributed element) has to have its
initial= attribute wiped out as there's no place to represent it
graphically.

Huh, this is kinda hard to describe.

TODO: make this discription less terrible."

  (when-let ((parent (first (scxmld-parents element)))
             (grandparent (first (scxmld-parents parent))))
    (assert (scxmld-synthetic-initial-p parent)
            t
            "Parent of a synthetic transition must be a synthetic initial")
    (assert (and (scxmld-with-synthetic-initial-child-p grandparent)
                 (scxml-element-with-initial-child-p grandparent))
            t
            "Parent of a synthetic initial must be able to hold a synthetic initial")
    (scxmld--clear-initial-attribute grandparent)))

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
(defun scxmld-synthetic-initial-class-p (any-object)
  "Equivalent of (object-of-class-p ANY-OBJECT 'scxmld-synthetic-initial)"
  (and any-object
       (object-of-class-p any-object 'scxmld-synthetic-initial)))
(cl-defmethod scxmld-get-synth-initial ((element scxmld-with-synthetic-initial))
  "Return the synthetic-initial element child of ELEMENT if present."
  (first (seq-filter 'scxmld-synthetic-initial-class-p
                     (scxmld-get-diagram-children element))))
(cl-defmethod scxmld-make-orphan :before ((element scxmld-synthetic-initial) &optional parent)
  "Set the initial= attribute to nil on the parent of ELEMENT.

If you have any <scxml> or <state> element, and it has a properly
set up synthetic-initial element in the diagram graph, when you
orphan that element you should also clear out the initial=
attribute on the <scxml> or <state> as it now does not have any
representation on the diagram graph."

  (when-let ((parent (first (scxmld-parents element))))
    (assert (and (scxmld-with-synthetic-initial-child-p parent)
                 (scxml-element-with-initial-child-p parent))
            t
            "Parent of a synthetic initial must be able to hold a synthetic initial")
    (scxmld--clear-initial-attribute parent))

  ;; also if this element has a child transition then that transition
  ;; should be made orphan from everything but ELEMENT itself.
  (assert (<= (length (scxmld-children element)) 1)
          t
          "a synthetic-initial should have at most one child element.")
  (when-let ((child (first (scxmld-children element))))
    ;; orphan child from every element except ELEMENT
    (cl-loop for parent in (scxmld-parents child)
             unless (eq parent element)
             do (scxmld-make-orphan child parent))))
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


;; serialization changes
(defun scxmld--element-factory (type attrib-alist &optional skip-slots)
  "Build scxmld elements by TYPE and ATTRIB-ALIST.

The elements will be built without scxml tree children but _with_
scxmld graph children when applicable.

Optionally, if the slot name is in skip-slots (as a symbol) then
forcefully put it in t he element's attribute hash table, not in
the slot (even if a proper slot is found.

Does not build recursively."
  (unless (symbolp type)
    (error "Type must be a symbol"))
  (let* ((type-string (symbol-name type))
         (class-name (format "scxml-%s" type-string))
         (drawing-class-name (format "scxmld-%s" type-string))
         (class (intern-soft class-name))
         (drawing-class (intern-soft drawing-class-name))
         (slots (eieio-class-slots class))
         (slot-names (mapcar (lambda (slot)
                               ;; TODO - probably shouldn't use a cl--* function.
                               (let ((slot-symbol (cl--slot-descriptor-name slot)))
                                 (symbol-name slot-symbol)))
                             slots))
         (attribute-slots (seq-filter
                           (lambda (slot-name)
                             (not (eq (aref slot-name 0) (aref "_" 0))))
                           slot-names))
         (attribute-slot-symbols (mapcar 'intern attribute-slots)))
    ;; TODO - this will only work if the slot name and the eieio
    ;; initarg are the same.
    ;; split up everything in attrib-list
    (let ((constructor-params nil)
          (attribute-params nil))
      (cl-loop for cell in attrib-alist
               for attrib-name-symbol = (car cell)
               when (and (memq attrib-name-symbol attribute-slot-symbols)
                         (not (memq attrib-name-symbol skip-slots)))
               do (let ((initarg-sym (intern
                                      (format ":%s"
                                              (symbol-name attrib-name-symbol)))))
                    (setq constructor-params
                          (plist-put constructor-params initarg-sym (cdr cell))))
               else
               do (push cell attribute-params))
      (let ((element (apply drawing-class constructor-params)))
        (mapc (lambda (cell)
                (scxml-put-attrib element (car cell) (cdr cell)))
              attribute-params)
      element))))
(defun scxmld-reconstitute-drawings (root-element canvas)
  "Rebuild any 2dd drawing properties in root-elemenet based off saved attributes

This should happen in 3 passes:

Rectangles
Points
Links
Anything that's synthetic."
  (unless (scxmld-element-child-p root-element)
    (error "root element should be an scxmld-element instance"))
  (unless (2dd-canvas-p canvas)
    (error "canvas must be a 2dd-canvas"))
  (cl-flet ((reconstitute-rect
             (rectangle canvas)
             (when-let ((drawing-info-string (scxml-get-attrib
                                              rectangle
                                              (intern scxmld-drawing-attribute)))
                        (drawing-info (car (read-from-string drawing-info-string))))
               (2dd-set-from rectangle drawing-info canvas))))
    ;; establish the root element's drawing first with the passed in canvas
    (assert (2dd-rect-child-p root-element)
            t
            "The root element has to be a rectangle for now")
    (reconstitute-rect root-element canvas)
    (scxml-visit root-element
                 (lambda (rect)
                   (reconstitute-rect rect
                                      (2dd-get-inner-canvas (scxml-parent rect))))
                 (lambda (element)
                   (and (2dd-rect-child-p element)
                        (not (eq element root-element)))))
    root-element))
  
  




(provide 'scxmld-elements)
;;; scxmld-elements.el ends here
