(require 'ert)
(require 'scxml)
(require 'scxmld)





(setq scxmld-test-a-and-b-document "<scxml name=\"test1\" scxml---drawing=\"(:absolute #s(2dg-rect 0.0 0.0 40.0 100.0)) (:relative #s(2dg-point 0.22872340425531915 0.9583333333333334)) (:source (:edge down) :target (:edge up :relative-coord 0.41304347826086957) :path (#s(2dg-point 24.5 36.5) #s(2dg-point 24.5 25.0)) :edit-history (target))\" initial=\"A\" >
  <state scxml---drawing=\"(:relative #s(2dg-rect 0.0851063829787234 0.2777777777777778 0.6388888888888888 0.32978723404255317))\" id=\"A\" >
    <transition scxml---drawing=\"(:source (:edge right :relative-coord 0.34615384615384615) :target (:edge left :relative-coord 0.6428571428571429) :path (#s(2dg-point 34.0 16.5) #s(2dg-point 46.5 16.5) #s(2dg-point 46.5 18.0) #s(2dg-point 59.0 18.0)) :edit-history (source target))\" target=\"B\" events=\"go.b\" />
  </state>
  <state scxml---drawing=\"(:relative #s(2dg-rect 0.5957446808510638 0.3055555555555556 0.6944444444444444 0.8617021276595744))\" id=\"B\" >
    <transition target=\"A\" scxml---drawing=\"(:source (:edge left :relative-coord 0.35714285714285715) :target (:edge right :relative-coord 0.6538461538461539) :path (#s(2dg-point 59.0 22.0) #s(2dg-point 46.5 22.0) #s(2dg-point 46.5 20.5) #s(2dg-point 34.0 20.5)) :edit-history (source target))\" events=\"go.a\" />
  </state>
</scxml>")


(setq test (let ((element
                  (with-temp-buffer
                    (insert scxmld-test-a-and-b-document)
                    (scxml-read-buffer (current-buffer) 'scxmld--element-factory))))
             
             (2dd-pprint element)
             (eieio-object-class element)
             
             (setq some-canvas (2dd-canvas- 0 100 0 40))
             
             (scxmld-reconstitute-drawings element some-canvas)))

(2dd-geometry test)
      
(2dd-rect- 0 10 20 30)

