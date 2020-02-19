;;; scxmld-elements.el --- scxmld drawable elements -*- lexical-binding: t -*-

;;; Commentary:
;; Drawable versions of what can be found in scxml-elements.el

;;; Code:
(eval-when-compile (require 'subr-x))

(require 'scxml)
(require 'scxmld-element)

;; Drawable elements
(defclass scxmld-scxml (scxml-scxml 2dd-rect scxmld-element)
  ())

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
