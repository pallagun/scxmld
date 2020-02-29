;;; scxmld-elements.el --- scxmld drawable elements -*- lexical-binding: t -*-

;;; Commentary:
;; Drawable versions of what can be found in scxml-elements.el

;;; Code:
(eval-when-compile (require 'subr-x))

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
  "scxmld-scxml outlines style."
  :group 'scxmld-faces)

(defface scxmld-outline-marked
  '((t :foreground "green"))
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
  (cl-call-next-method rect
                       scratch
                       x-transformer
                       y-transformer
                       :outline-style (if (scxmld-get-highlight rect)
                                          'scxmld-outline-marked
                                        'scxmld-scxml-outline)
                       :edit-idx-style 'scxmld-edit-idx))
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

(defclass scxmld-state (2dd-rect scxmld-element scxml-state scxmld-with-highlight)
  ())
(cl-defmethod scxmld-pprint ((element scxmld-state))
  "Pretty print this <state> ELEMENT."
  (format "state[name:%s,[%s] %s]"
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
  (cl-call-next-method rect
                       scratch
                       x-transformer
                       y-transformer
                       :outline-style (if (scxmld-get-highlight rect)
                                          'scxmld-outline-marked
                                        'scxmld-state-outline)
                       :edit-idx-style 'scxmld-edit-idx))
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

(provide 'scxmld-elements)
;;; scxmld-elements.el ends here
