;;; scxmld-xml.el --- xml helper functions -*- lexical-binding: t -*-

;;; Commentary:
;; Some wrappers around xmltok and other nxml-mode functions.  Note
;; that all of these functions are entirely unaware of what buffer
;; they're applied in.  That needs to be coordinated by calling code.

;;; Code:
(require 'nxml-rap)
(require 'nxml-mode)                    ;for nxml-forward-element
(require 'eieio)
(require 'cl)
(require 'scxml)

(defconst scxmld-xml-marker 'scxmld--element
  "XML text property marker tag used by this module.")

(defun scxmld--xmltok-init ()
  "Initialize the xmltok processing in current-buffer.

I'm not sure why I have to do this, but I think it's because I'm
hot wiring the whole thing.  I think if I use nXML properly I
probably won't need to do this."
  (when (or (not (boundp 'nxml-prolog-end))
            (null nxml-prolog-end))
    (setq-local nxml-scan-end
                (set-marker (make-marker) (point-min)))
    (nxml-scan-prolog)))

(defclass scxmld-xmltok ()
  ((type :reader scxmld-type
         :documentation "One of 'start-tag, 'end-tag,
         'empty-element, etc.  Defined in nxml-mode near \";;
         Token types returned by xmltok-forward.\"")
   (start :reader scxmld-start)
   (name-end :reader scxmld-name-end)
   (name-colon :reader scxmld-name-colon)
   (attributes :reader scxmld-attributes)
   (namespace-attributes :reader scxmld-namespace-attributes)
   (next-token-pos :reader scxmld-next-token-pos)
   (scan-direction :reader scxmld-scan-direction
                   :documentation "Record which way the xml
                   parser was scaning when this token was found.
                   May be one of 'before or 'after."))
  :documentation "Wrap up the results from xmltok operations.

Note: tokens represent the state of a token at a point in time.
Modifying the base document in any way may cause scxmld-xmltok
data to become invalid.")
(cl-defmethod scxmld-pprint ((tag scxmld-xmltok))
  "Pretty print this TAG."
  (scxmld-type tag)
  (buffer-substring-no-properties (scxmld-start tag)
                                  (scxmld-next-token-pos tag)))
(defun scxmld-xmltok-before ()
  "Wrap up nxml-token-before."
  (when (> (point) (point-min))
    (scxmld---gather-xmltok (nxml-token-before) 'before)))
(defun scxmld-xmltok-after ()
  "Wrap up nxml-token-after."
  (when (< (point) (point-max))
    (scxmld---gather-xmltok (nxml-token-after) 'after)))
(defun scxmld---gather-xmltok (next-token-pos scan-direction)
  "Create an scxmld-xmltok object capturing the current state of xmltok processing"
  ;; TODO - capture if you're scanning forwards or backwards.
  ;; that has an impact on what next-token-pos means.
  (let ((parser-state (scxmld-xmltok)))
    (oset parser-state type xmltok-type)
    (oset parser-state start xmltok-start)
    (oset parser-state name-end xmltok-name-end)
    (oset parser-state name-colon xmltok-name-colon)
    (oset parser-state attributes xmltok-attributes)
    (oset parser-state namespace-attributes xmltok-namespace-attributes)
    (oset parser-state next-token-pos next-token-pos)
    (oset parser-state scan-direction scan-direction)
    parser-state))

;; queries
(cl-defmethod scxmld-tag-name ((tag scxmld-xmltok))
  "Get the tag name of this TAG."
  (buffer-substring-no-properties (1+ (scxmld-start tag))
                                  (scxmld-name-end tag)))
(cl-defmethod scxmld-get-mark ((tag scxmld-xmltok))
  "Get the mark for this TAG if it exists."
  (cl-loop for char-idx from (scxmld-start tag) to (scxmld-next-token-pos tag)
           for text-prop = (get-text-property char-idx scxmld-xml-marker)
           when text-prop
           return text-prop))
(cl-defmethod scxmld-find-end ((tag scxmld-xmltok))
  "Get the beginning of the end tag for this TAG if there is one."
  (when (eq 'start-tag (scxmld-type tag))
    (goto-char (scxmld-start tag))
    (nxml-forward-element 1)
    (scxmld-xmltok-before)))
(cl-defmethod scxmld--refresh ((tag scxmld-xmltok))
  "Attempt to refresh TAG from source document changes.

This function is only valid when you've modified attributes
of the TAG and not modified *anything* else in the document."
  (goto-char (scxmld-start tag))
  (scxmld-xmltok-after))
(defun scxmld--attribute-key-value (attribute-marker)
  "Return a alist cons cell describing this ATTRIBUTE-MARKER."
  (cons (buffer-substring-no-properties (elt attribute-marker 0)
                                        (elt attribute-marker 2))
        (buffer-substring-no-properties (elt attribute-marker 3)
                                        (elt attribute-marker 4))))
(cl-defmethod scxmld-attributes-alist ((tag scxmld-xmltok))
  "Return an alist of all attributes of TAG."
  (with-slots (attributes) tag
    (when attributes
      (cl-loop for attrib in attributes
               with attrib-alist = nil
               do (push (scxmld--attribute-key-value attrib)
                        attrib-alist)
               finally return attrib-alist))))
(cl-defmethod scxmld-children ((tag scxmld-xmltok))
  "Get all the direct child tags of TAG."
  (let ((end-tag (and (eq (scxmld-type tag) 'start-tag)
                      (scxmld-find-end tag))))
    (when end-tag
      (goto-char (scxmld-next-token-pos tag))
      (let ((children nil)
            (end-point (scxmld-start end-tag))
            (child-tag (scxmld-xmltok-after)))
        (while (and child-tag
                    (< (scxmld-start child-tag) end-point))
          (when (and (memq (scxmld-type child-tag)
                           (list 'start-tag 'empty-element))
                     (scxmld-get-mark child-tag))
            (push child-tag children))
          (goto-char (scxmld-next-token-pos
                      (if (eq (scxmld-type child-tag) 'start-tag)
                          (scxmld-find-end child-tag)
                        child-tag)))
          (setq child-tag (scxmld-xmltok-after)))
        (nreverse children)))))
(defun scxmld--xmltok-find-next-by-name (name)
  "Search forward in the current buffer for an xml element with name NAME."
  (let ((found-tag)
        (search-end)
        (last-point (point-max)))
    (while (not search-end)
      (let ((xml-tag (scxmld-xmltok-after)))
        (if (null xml-tag)              ;no more tags found
            (setq search-end t)
          ;; TODO - what do I do, if anything when I see 'end-tag?
          (if (and (memq (scxmld-type xml-tag)
                         (list 'start-tag 'empty-element))
                   (equal name
                          (scxmld-tag-name xml-tag)))
              (setf found-tag xml-tag
                    search-end t)
            (goto-char (scxmld-next-token-pos xml-tag))))))
    found-tag))
(defun scxmld--xmltok-find-element (element &optional start-at-point)
  "Search forward in the current buffer for an xml element tagged for ELEMENT.

When START-AT-POINT is t seach from the current point in the
buffer otherwise search forward from the beginning of the
buffer."
  (when (not start-at-point)
    (goto-char (point-min)))
  (let ((element-name (scxml-xml-element-name element))
        (search-done nil)
        (result nil))
    (while (not search-done)
      (let ((candidate (scxmld--xmltok-find-next-by-name element-name)))
        (if candidate
            (if (eq (scxmld-get-mark candidate) element)
                (setf search-done t
                      result candidate)
              (goto-char (scxmld-next-token-pos candidate)))
          (setf search-done t))))
    result))
(cl-defmethod scxmld-children ((tag scxmld-xmltok))
  "Get all the direct child tags of TAG."
  (let ((end-tag (and (eq (scxmld-type tag) 'start-tag)
                      (scxmld-find-end tag))))
    (when end-tag
      (goto-char (scxmld-next-token-pos tag))
      (let ((children)
            (end-point (scxmld-start end-tag))
            (child-tag (scxmld-xmltok-after)))
        (while (and child-tag
                    (< (scxmld-start child-tag) end-point))
          (when (and (memq (scxmld-type child-tag)
                           '(start-tag empty-element))
                     (scxmld-get-mark child-tag))
            (push child-tag children))
          (goto-char (scxmld-next-token-pos
                      (if (eq (scxmld-type child-tag) 'start-tag)
                          (scxmld-find-end child-tag)
                        child-tag)))
          (setq child-tag (scxmld-xmltok-after)))
        (nreverse children)))))

;; modifiers
(cl-defmethod scxmld-find-or-create-end ((tag scxmld-xmltok))
  "Find the end tag of TAG (if it exists, otherwise create it) and return it.

This will modify the buffer and possibly invalidate other scxmld-xmltok objects!"
  (let ((tag-type (scxmld-type tag)))
    (cond ((eq tag-type 'start-tag)
           ;; this is a start tag, it should have an end.
           (let ((end-tag (scxmld-find-end tag)))
             (unless end-tag
               (error "xml parsing error, unable to find end tag?"))
             end-tag))
          ((eq tag-type 'empty-element)
           (let ((original-end (scxmld-next-token-pos tag))
                 (tag-name (scxmld-tag-name tag)))
             (goto-char (1- original-end))
             (delete-char -1)
             (forward-char 1)
             (insert "\n</" tag-name ">")
             (goto-char original-end)
             (scxmld-xmltok-after)))
          (t
           (error "Unable to find or create end point for %s" tag)))))
(cl-defmethod scxmld-add-child ((parent-start-tag scxmld-xmltok) (child string) &optional tracking-property-value)
  "Insert the XML of CHILD as the last child of PARENT-START-TAG returning the new scxmld-xmltok.

This will modify the buffer and possibly invalidate other scxmld-xmltok objects!"
  ;; TODO - this should be called scxmld-add-child like the other functions
  (let* ((parent-end-tag (scxmld-find-or-create-end parent-start-tag))
         (insert-start (scxmld-start parent-end-tag)))
    (goto-char insert-start)
    (insert child "\n")
    (let ((insert-end (point)))
      (when tracking-property-value
        (put-text-property insert-start
                           insert-end
                           scxmld-xml-marker
                           tracking-property-value))
      (indent-region insert-start (1+ (point))))
    (goto-char insert-start)
    (scxmld-xmltok-after)))
(cl-defmethod scxmld-delete ((tag scxmld-xmltok))
  "Prune TAG from the current buffer.

This will modify the buffer and possibly invalidate other scxmld-xmltok objects!"
  (cond ((eq (scxmld-type tag) 'empty-element)
         (delete-region (scxmld-start tag)
                        (scxmld-next-token-pos tag)))
        ((eq (scxmld-type tag) 'start-tag)
         (let ((end-tag (scxmld-find-end tag)))
           (unless end-tag
             (error "Unable to prune tag[%s], unable to find end" tag))
           (delete-region (scxmld-start tag)
                          (scxmld-next-token-pos end-tag))))
        (t
         (error "Currently unable to delete this type of tag: %s" tag))))
(cl-defmethod scxmld-mark ((tag scxmld-xmltok) tag-value &optional goto-end-of-tag)
  "Given a TAG, mark it for TAG-VALUE."
  (let ((end-pos (scxmld-next-token-pos tag)))
    (put-text-property (scxmld-start tag)
                       end-pos
                       scxmld-xml-marker
                       tag-value)
    (when goto-end-of-tag
      (goto-char end-pos))))
(cl-defmethod scxmld-update-attributes ((tag scxmld-xmltok) (attributes list))
  "Ensure the element in TAG has exactly the attributes from ATTRIBUTES.

ATTRIBUTES should be an alist with string keys and string values."

  ;; TODO: This needs some attention
  (let ((attribute-markers (scxmld-attributes tag))
        ;; Apparently when parsing xml, the 'xmlns' attribute is
        ;; somehow special and not a real attribute.  Because of that
        ;; I'll have to pretend it doesn't exist here as well.
        (proper-attributes (seq-filter (lambda (attrib)
                                         (not (equal (car attrib) "xmlns")))
                                       attributes))
        (valid-markers)
        (found-attributes)
        (addition-point (- (scxmld-next-token-pos tag)
                           (if (eq (scxmld-type tag) 'empty-element)
                               2
                             1)))
        (non-additive-operations)
        (additive-operations))

    (while (equal " " (buffer-substring (1- addition-point) addition-point))
      (incf addition-point -1))

    ;; Go through every attribute in attributes and make sure they're
    ;; in attribute-markers.  Then anything that's in
    ;; attribute-markers that's not in attributes and remove it.
    (cl-loop for marker in attribute-markers
             for marker-keyval = (scxmld--attribute-key-value marker)
             for marker-key = (car marker-keyval)
             for marker-val = (cdr marker-keyval)
             for proper-keyval = (assoc marker-key
                                        proper-attributes)
             do (if proper-keyval
                    (let ((proper-val (cdr proper-keyval)))
                      (push marker-key found-attributes)
                      (when (not (equal proper-val marker-val))
                        ;; found it but it's wrong
                        (push `(modify ,marker ,proper-val) non-additive-operations)))
                  ;; no proper attribute found here, this needs to be removed.
                  (push `(remove ,marker ,marker-val) non-additive-operations)))
    ;; for all attributes not found, add them.

    (mapc (lambda (add-keyval)
            (goto-char addition-point)
            (insert (format " %s=\"%s\"" (car add-keyval) (cdr add-keyval))))
          (seq-filter
           (lambda (attribute-keyval)
             (let ((attribute-key (car attribute-keyval)))
               (and (not (member attribute-key found-attributes))
                    (cdr attribute-keyval))))
           proper-attributes))

    (mapc (lambda (command-list)
            (let ((type (first command-list))
                  (marker (second command-list))
                  (other-value (third command-list)))
              (cond ((eq type 'remove)
                     (delete-region (1- (elt marker 0)) (1+ (elt marker 4))))
                    ((eq type 'modify)
                     (delete-region (elt marker 3) (elt marker 4))
                     (goto-char (elt marker 3))
                     (insert other-value))
                    (t
                     (error "Unknown operation: %s" type)))))
          (sort non-additive-operations
                (lambda (a b)
                  (let ((marker-a (second a))
                        (marker-b (second b)))
                    (> (elt marker-a 0) (elt marker-b 0))))))))
(cl-defmethod scxmld-insert-new-child ((parent-start-tag scxmld-xmltok) (child string))
  "Insert CHILD (a strified xml element) as a child of PARENT-START-TAG.

Returns an scxmld-xmltok of the added CHILD."
  (let* ((parent-end-tag (scxmld-find-or-create-end parent-start-tag))
         (insert-start (scxmld-start parent-end-tag)))
    (goto-char insert-start)
    (insert child "\n")
    (let ((insert-end (point)))
      (indent-region insert-start (1+ (point))))
    (goto-char insert-start)
    (scxmld-xmltok-after)))

;; document linking

;; random
(defun scxmld--xmltok-debug-at-point ()
  "Quick debugging function for humans."
  (interactive)
  (scxmld--xmltok-init)
  (let ((tag (scxmld-xmltok-after)))

     (message "%s:%s: %s\n tag:%s\nMarker:%s"
             (scxmld-type tag)
             (scxmld-tag-name tag)
             (scxmld-attributes-alist tag)
             (scxmld-pprint tag)
             (scxmld-get-mark tag))
    (goto-char (scxmld-next-token-pos tag))))




(provide 'scxmld-xml)
;;; scxmld-xml.el ends here
