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

(defclass scxmld-state (scxml-state 2dd-rect scxmld-element)
  ())


;; (defun scxmld-element-factory (type attrib-alist)
;;   "Build a drawable element of TYPE and having ATTRIB-ALIST properties."
;;   (let* ((base-xml-element-name (symbol-name type))
;;          (base-class (intern (format "scxmld-%s" base-xml-element-name)))
;;          (base-slots (eieio-class-slots base-class))
;;          ;; TODO - Is there something more appropriate than a cl--* function?
;;          (base-slot-symbols (mapcar 'cl--slot-descriptor-name base-slots))
;;          (drawable-class (intern (format "scxml-drawable-%s" base-xml-element-name)))
;;          (drawable-slots (eieio-class-slots drawable-class))
;;          (drawable-slot-symbols (mapcar 'cl--slot-descriptor-name drawable-slots))
;;          (skip-slots))
;;     ;; Build a list of all drawable-slots that aren't in teh base slots and
;;     ;; use them as as a skip-slots list.
;;     (mapc (lambda (slot-sym)
;;             (when (not (memq slot-sym base-slot-symbols))
;;               (push slot-sym skip-slots)))
;;           drawable-slot-symbols)
;;     (let ((element (scxml--element-factory (intern (format "drawable-%s" base-xml-element-name))
;;                                            attrib-alist
;;                                            skip-slots)))
;;       (scxml--set-hint-from-attrib-list element attrib-alist)
;;       (scxml--build-synthetic-children element attrib-alist)
;;       element)))


(provide 'scxmld-elements)
;;; scxmld-elements.el ends here
