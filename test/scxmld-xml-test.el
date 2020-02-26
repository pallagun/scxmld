(require 'ert)
(require 'scxmld-xml)


(defun scxmld--apply-update-attributes (xml-string attributes-alist tag-fn)
  "Put xml-string into a buffer, run scxmld-update-attributes on it.

tag-fn will be called from the start of the buffer to get the proper tag."
  (with-temp-buffer
    (insert xml-string)
    (goto-char (point-min))
    (scxmld--xmltok-init)
    (let ((tag (funcall tag-fn)))
      (scxmld-update-attributes tag attributes)
      (buffer-string))))
(defun scxmld---all-combinations (some-list)
  (if (null (cdr some-list))
      ;; only one entry, only one combination
      (list some-list)
    (let ((all-lists))
      (cl-loop for entry in some-list
               for rest-of-list = (seq-filter (lambda (x) (not (eq x entry))) some-list)
               for all-rest-combos = (scxmld---all-combinations rest-of-list)
               do (cl-loop for rest-entry in all-rest-combos
                           do (push (cons entry rest-entry) all-lists)))
      all-lists)))

(ert-deftest scxmld-update-attributes-001 ()
  "Element with no children, no changes expected."
  (let ((attributes '(("some-thing" . "1")
                      ("someThingElse" . "here or there")))
        (tag-fn 'scxmld-xmltok-after))
    (let ((xml-string "<scxml some-thing=\"1\" someThingElse=\"here or there\" />"))
      (mapc (lambda (attributes)
              (should (equal xml-string
                             (scxmld--apply-update-attributes xml-string
                                                              attributes
                                                              tag-fn))))
            (scxmld---all-combinations attributes)))
    (let ((xml-string "<scxml some-thing=\"1\" someThingElse=\"here or there\"></scxml>"))
      (mapc (lambda (attributes)
              (should (equal xml-string
                             (scxmld--apply-update-attributes xml-string
                                                              attributes
                                                              tag-fn))))
            (scxmld---all-combinations attributes)))))
(ert-deftest scxmld-update-attributes-002 ()
  "Element with no children that needs one attribute added."
  (let ((attributes '(("some-thing" . "1")
                      ("someThingElse" . "here or there")
                      ("new" . "new-value")))
        (tag-fn 'scxmld-xmltok-after)
        (cases (list (list :input "<scxml some-thing=\"1\" someThingElse=\"here or there\" />"
                           :expected "<scxml some-thing=\"1\" someThingElse=\"here or there\" new=\"new-value\" />")
                     (list :input "<scxml some-thing=\"1\" someThingElse=\"here or there\"/>"
                           :expected "<scxml some-thing=\"1\" someThingElse=\"here or there\" new=\"new-value\"/>")
                     (list :input "<scxml some-thing=\"1\" someThingElse=\"here or there\"></scxml>"
                           :expected "<scxml some-thing=\"1\" someThingElse=\"here or there\" new=\"new-value\"></scxml>")
                     (list :input "<scxml some-thing=\"1\" someThingElse=\"here or there\"><!-- comment --></scxml>"
                           :expected "<scxml some-thing=\"1\" someThingElse=\"here or there\" new=\"new-value\"><!-- comment --></scxml>")
                     (list :input "<scxml some-thing=\"1\" someThingElse=\"here or there\">anything</scxml>"
                           :expected "<scxml some-thing=\"1\" someThingElse=\"here or there\" new=\"new-value\">anything</scxml>")
                     (list :input "<scxml some-thing=\"1\" someThingElse=\"here or there\" ></scxml>"
                           :expected "<scxml some-thing=\"1\" someThingElse=\"here or there\" new=\"new-value\" ></scxml>"))))

    (mapc (lambda (test-case)
            (let ((xml-string (plist-get test-case :input))
                  (expected-string (plist-get test-case :expected)))
              (mapc (lambda (attributes)
                      (should (equal expected-string
                                     (scxmld--apply-update-attributes xml-string
                                                                      attributes
                                                                      tag-fn))))
                    (scxmld---all-combinations attributes))))
          cases)))
(ert-deftest scxmld-update-attributes-003 ()
  "Element with no children that needs one attribute removed."
  (let ((attributes '(("first" . "true")
                      ("someThingElse" . "here or there")))
        (tag-fn 'scxmld-xmltok-after)
        (cases (list (list :input "<scxml first=\"true\" some-thing=\"1\" someThingElse=\"here or there\" />"
                           :expected "<scxml first=\"true\" someThingElse=\"here or there\" />")
                     (list :input "<scxml first=\"true\" some-thing=\"1\" someThingElse=\"here or there\"/>"
                           :expected "<scxml first=\"true\" someThingElse=\"here or there\"/>")
                     (list :input "<scxml first=\"true\" some-thing=\"1\" someThingElse=\"here or there\"></scxml>"
                           :expected "<scxml first=\"true\" someThingElse=\"here or there\"></scxml>")
                     (list :input "<scxml first=\"true\" some-thing=\"1\" someThingElse=\"here or there\"><!-- comment --></scxml>"
                           :expected "<scxml first=\"true\" someThingElse=\"here or there\"><!-- comment --></scxml>")
                     (list :input "<scxml first=\"true\" some-thing=\"1\" someThingElse=\"here or there\">anything</scxml>"
                           :expected "<scxml first=\"true\" someThingElse=\"here or there\">anything</scxml>")
                     (list :input "<scxml first=\"true\" some-thing=\"1\" someThingElse=\"here or there\" ></scxml>"
                           :expected "<scxml first=\"true\" someThingElse=\"here or there\" ></scxml>"))))

    (mapc (lambda (test-case)
            (let ((xml-string (plist-get test-case :input))
                  (expected-string (plist-get test-case :expected)))
              (mapc (lambda (attributes)
                      (should (equal expected-string
                                     (scxmld--apply-update-attributes xml-string
                                                                      attributes
                                                                      tag-fn))))
                    (scxmld---all-combinations attributes))))
          cases)))
(ert-deftest scxmld-update-attributes-004 ()
  "Element with no children that needs one attribute altered."
  (let ((attributes '(("first" . "true")
                      ("some-thing" . "new")
                      ("someThingElse" . "here or there")))
        (tag-fn 'scxmld-xmltok-after)
        (cases (list (list :input "<scxml first=\"true\" some-thing=\"1\" someThingElse=\"here or there\" />"
                           :expected "<scxml first=\"true\" some-thing=\"new\" someThingElse=\"here or there\" />")
                     (list :input "<scxml first=\"true\" some-thing=\"1\" someThingElse=\"here or there\"/>"
                           :expected "<scxml first=\"true\" some-thing=\"new\" someThingElse=\"here or there\"/>")
                     (list :input "<scxml first=\"true\" some-thing=\"1\" someThingElse=\"here or there\"></scxml>"
                           :expected "<scxml first=\"true\" some-thing=\"new\" someThingElse=\"here or there\"></scxml>")
                     (list :input "<scxml first=\"true\" some-thing=\"1\" someThingElse=\"here or there\"><!-- comment --></scxml>"
                           :expected "<scxml first=\"true\" some-thing=\"new\" someThingElse=\"here or there\"><!-- comment --></scxml>")
                     (list :input "<scxml first=\"true\" some-thing=\"1\" someThingElse=\"here or there\">anything</scxml>"
                           :expected "<scxml first=\"true\" some-thing=\"new\" someThingElse=\"here or there\">anything</scxml>")
                     (list :input "<scxml first=\"true\" some-thing=\"1\" someThingElse=\"here or there\" ></scxml>"
                           :expected "<scxml first=\"true\" some-thing=\"new\" someThingElse=\"here or there\" ></scxml>"))))

    (mapc (lambda (test-case)
            (let ((xml-string (plist-get test-case :input))
                  (expected-string (plist-get test-case :expected)))
              (mapc (lambda (attributes)
                      (should (equal expected-string
                                     (scxmld--apply-update-attributes xml-string
                                                                      attributes
                                                                      tag-fn))))
                    (scxmld---all-combinations attributes))))
          cases)))
(ert-deftest scxmld-update-attributes-005 ()
  "Element with no children that needs one attribute added, one modified and one deleted."
  (let ((attributes '(("first" . "ok")
                      ("second" . "changed")
                      ("fourth" . "ok")
                      ("fifth" . "new")))
        (tag-fn 'scxmld-xmltok-after)
        (cases (list (list :input "<scxml first=\"ok\" second=\"unchanged\" third=\"remove\" fourth=\"ok\" />"
                           :expected "<scxml first=\"ok\" second=\"changed\" fourth=\"ok\" fifth=\"new\" />")
                     (list :input "<scxml first=\"ok\" second=\"unchanged\" third=\"remove\" fourth=\"ok\"/>"
                           :expected "<scxml first=\"ok\" second=\"changed\" fourth=\"ok\" fifth=\"new\"/>")
                     (list :input "<scxml first=\"ok\" second=\"unchanged\" third=\"remove\" fourth=\"ok\"></scxml>"
                           :expected "<scxml first=\"ok\" second=\"changed\" fourth=\"ok\" fifth=\"new\"></scxml>")
                     (list :input "<scxml first=\"ok\" second=\"unchanged\" third=\"remove\" fourth=\"ok\"><!-- comment --></scxml>"
                           :expected "<scxml first=\"ok\" second=\"changed\" fourth=\"ok\" fifth=\"new\"><!-- comment --></scxml>")
                     (list :input "<scxml first=\"ok\" second=\"unchanged\" third=\"remove\" fourth=\"ok\">anything</scxml>"
                           :expected "<scxml first=\"ok\" second=\"changed\" fourth=\"ok\" fifth=\"new\">anything</scxml>")
                     (list :input "<scxml first=\"ok\" second=\"unchanged\" third=\"remove\" fourth=\"ok\" ></scxml>"
                           :expected "<scxml first=\"ok\" second=\"changed\" fourth=\"ok\" fifth=\"new\" ></scxml>"))))

    (mapc (lambda (test-case)
            (let ((xml-string (plist-get test-case :input))
                  (expected-string (plist-get test-case :expected)))
              (mapc (lambda (attributes)
                      (should (equal expected-string
                                     (scxmld--apply-update-attributes xml-string
                                                                      attributes
                                                                      tag-fn))))
                    (scxmld---all-combinations attributes))))
          cases)))

(provide 'scxmld-xml-test)
