;;; scxmld-elements.el --- scxmld drawable elements -*- lexical-binding: t -*-

;;; Commentary:
;; Drawable versions of what can be found in scxml-elements.el

;;; Code:
(eval-when-compile (require 'subr-x))

(require '2dd)
(require 'scxml)
(require 'scxmld-element)

;; Note: M-x list-colors-display to see all colors
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
(defface scxmld-outline-marked
  '((t :foreground "green"))
  "scxmld-scxml outlines style."
  :group 'scxmld-faces)
(defface scxmld-label-marked
  '((t :foreground "yellow"))
  "scxmld-scxml outlines style."
  :group 'scxmld-faces)

;; Drawable elements
(defclass scxmld-scxml (2dd-rect scxmld-element scxml-scxml scxmld-with-highlight)
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
(cl-defmethod 2dd-render ((rect scxmld-scxml) scratch x-transformer y-transformer &rest style-plist)
  (let ((has-highlight (scxmld-get-highlight rect)))
    (cl-call-next-method rect
                         scratch
                         x-transformer
                         y-transformer
                         :outline-style (if has-highlight 'scxmld-outline-marked 'scxmld-scxml-outline)
                         :label-style (if has-highlight 'scxmld-label-marked nil)
                         :edit-idx-style 'scxmld-edit-idx)))
(cl-defmethod scxml-set-name :after ((element scxmld-scxml) value)
  "Set the scxml-drawing label to match ELEMENT's new name VALUE."
  (2dd-set-label element value))
(cl-defmethod scxmld-put-attribute ((element scxmld-scxml) (attribute-name string) attribute-value)
  "Set ELEMENT's attribute with name ATTRIBUTE-NAME to be ATTRIBUTE-VALUE.

When ATTRIBUTE-VALUE is nil the attribute will be deleted if possible.

Special cases here are: name, initial, datamodel and binding."
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


(defsubst scxmld--parent-is-parallel-p (any)
  "Return true if ANY's parent is a <parallel> element."
  (let ((parent (scxml-parent any)))
    (if (and parent (scxml-parallel-class-p parent))
        t
      nil)))
(defclass scxmld-state (2dd-rect scxmld-element scxml-state scxmld-with-highlight)
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
(cl-defmethod 2dd-render ((rect scxmld-state) scratch x-transformer y-transformer &rest style-plist)
  (let ((has-highlight (scxmld-get-highlight rect)))
    (cl-call-next-method rect
                         scratch
                         x-transformer
                         y-transformer
                         :outline-style (if has-highlight
                                            'scxmld-outline-marked
                                          'scxmld-state-outline)
                         :no-outline (if (scxmld--parent-is-parallel-p rect)
                                         (not has-highlight)
                                       nil)
                         :label-style (if has-highlight 'scxmld-label-marked nil)
                         :edit-idx-style 'scxmld-edit-idx)))
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

(defclass scxmld-final (2dd-rect scxmld-element scxml-final scxmld-with-highlight)
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
(cl-defmethod 2dd-render ((rect scxmld-final) scratch x-transformer y-transformer &rest style-plist)
  (let ((has-highlight (scxmld-get-highlight rect)))
    (cl-call-next-method rect
                         scratch
                         x-transformer
                         y-transformer
                         :outline-style (if (scxmld-get-highlight rect)
                                            'scxmld-outline-marked
                                          'scxmld-final-outline)
                         :label-style (if has-highlight 'scxmld-label-marked nil)
                         :edit-idx-style 'scxmld-edit-idx)))
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

(defclass scxmld-parallel (2dd-division-rect scxmld-element scxml-parallel scxmld-with-highlight)
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
(cl-defmethod 2dd-render ((rect scxmld-parallel) scratch x-transformer y-transformer &rest style-plist)
  (let ((has-highlight (scxmld-get-highlight rect)))
    (cl-call-next-method rect
                         scratch
                         x-transformer
                         y-transformer
                         :outline-style (if has-highlight
                                            'scxmld-outline-marked
                                          'scxmld-parallel-outline)
                         :no-outline (if (scxmld--parent-is-parallel-p rect)
                                         (not has-highlight)
                                       nil)
                         :label-style (if has-highlight 'scxmld-label-marked nil)
                         :edit-idx-style 'scxmld-edit-idx)))
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



(provide 'scxmld-elements)
;;; scxmld-elements.el ends here
