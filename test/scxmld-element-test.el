(require 'ert)
(require 'scxmld-elements)



(ert-deftest scxmld-root-element-test ()
  "make sure you can always find the root element"
  (let ((scxml (scxmld-scxml))
        (target-state (scxmld-state :id "myId"))
        (another-state (scxmld-state))
        (transition (scxmld-transition :target "myId"))
        (synth-initial (scxmld-synthetic-initial))
        (synth-transition (scxmld-synthetic-transition :target "myId")))

    (scxml-add-child synth-initial synth-transition)
    (scxmld-set-synth-initial scxml synth-initial)
    (scxml-add-child another-state transition)
    (scxml-add-child scxml target-state)
    (scxml-add-child scxml another-state)

    (should (eq scxml (scxmld-root-element synth-initial)))
   
    (should (eq scxml (scxmld-root-element scxml)))
    (should (eq scxml (scxmld-root-element target-state)))
    (should (eq scxml (scxmld-root-element another-state)))
    (should (eq scxml (scxmld-root-element transition)))
    (should (eq scxml (scxmld-root-element synth-initial)))
    (should (eq scxml (scxmld-root-element synth-transition)))))
               
               

