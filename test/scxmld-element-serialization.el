(require 'ert)
(require 'scxml)
(require 'scxmld)



(ert-deftest scxmld-deserialize-test-001 ()
  "A single <scxml /> element"
  (let* ((document-string "<scxml name=\"test1\" scxml---drawing=\"(:absolute #s(2dg-rect 0.0 0.0 40.0 100.0))  \" />")
         (element (scxml-read-string document-string 'scxmld--element-factory)))
    (should element)
    (should (scxmld-scxml-p element))
    (should (2dd-rect-child-p element))
    (should (seq-empty-p (scxmld-children element)))
    (should (seq-empty-p (scxml-children element)))
    (let* ((canvas (2dd-canvas- 0 10 0 10))
           (with-drawings (scxmld-reconstitute-drawings element canvas)))
      (should with-drawings)
      (let ((geometry (2dd-geometry with-drawings)))
        (should geometry)
        (should (2dg-rect-p geometry))
        (should (2dg-almost-equal 0 (2dg-x-min geometry)))
        (should (2dg-almost-equal 100 (2dg-x-max geometry)))
        (should (2dg-almost-equal 0 (2dg-y-min geometry)))
        (should (2dg-almost-equal 40 (2dg-y-max geometry)))))))

(ert-deftest scxmld-deserialize-test-002 ()
  "A <scxml /> with one <state /> element inside"
  (let* ((document-string "<scxml name=\"test1\" scxml---drawing=\"(:absolute #s(2dg-rect 0.0 0.0 40.0 100.0))  \" >
                             <state scxml---drawing=\"(:relative #s(2dg-rect 0.14893617021276595 0.2777777777777778 0.75 0.5))\" id=\"inside-test1\" />
                           </scxml>")
         (no-drawing-element (scxml-read-string document-string 'scxmld--element-factory))
         (canvas (2dd-canvas- 0 100 0 40))
         (root-element (scxmld-reconstitute-drawings no-drawing-element canvas)))
      ;; the root element should be a 100x40 rectangler.
      (should root-element)
      (let ((geometry (2dd-geometry root-element)))
        (should geometry)
        (should (2dg-rect-p geometry))
        (should (2dg-almost-equal 0 (2dg-x-min geometry)))
        (should (2dg-almost-equal 100 (2dg-x-max geometry)))
        (should (2dg-almost-equal 0 (2dg-y-min geometry)))
        (should (2dg-almost-equal 40 (2dg-y-max geometry))))

      ;; There should be a single child.
      (should (eq 1 (length (scxmld-children root-element))))
      (should (eq 1 (length (scxml-children root-element))))
      (should (eq (first (scxmld-children root-element))
                  (first (scxml-children root-element))))
      (let ((child (first (scxmld-children root-element))))
        ;; it should be the state
        (should (scxmld-state-p child))
        (should (2dd-rect-child-p child))
        (should (seq-empty-p (scxmld-children child)))
        (should (seq-empty-p (scxml-children child)))
        ;; It should have a size of:
        (let ((geometry (2dd-geometry child)))
          (should geometry)
          (should (2dg-rect-p geometry))
          (should (2dg-almost-equal 14.893617021276595 (2dg-x-min geometry)))
          (should (2dg-almost-equal 50 (2dg-x-max geometry)))
          (should (2dg-almost-equal 11.111111111111 (2dg-y-min geometry)))
          (should (2dg-almost-equal 30 (2dg-y-max geometry)))))))
