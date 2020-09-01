(require 'ert)
(require 'scxmld-elements)


(ert-deftest scxmld-parent-child-tests ()
  "Make sure parent and child elements are what they should be"
  (let ((scxml (scxmld-scxml))
        (target-state (scxmld-state :id "myId"))
        (another-state (scxmld-state))
        (transition (scxmld-transition :target "myId"))
        (synth-initial (scxmld-synthetic-initial))
        (synth-transition (scxmld-synthetic-transition :target "myId")))

    (scxmld-add-child scxml synth-initial)
    (scxmld-add-child synth-initial synth-transition)
    (scxmld-add-child another-state transition)
    (scxmld-add-child scxml target-state)
    (scxmld-add-child scxml another-state)

    ;; ensure that scxml knows it's scxmld graph and scxml graph children
    (progn
      (should (eq (length (scxmld-children scxml)) 3))
      (should (member* synth-initial (scxmld-children scxml) :test 'eq))
      (should (member* target-state (scxmld-children scxml) :test 'eq))
      (should (member* another-state (scxmld-children scxml) :test 'eq))
      (should (eq (scxmld-get-synth-initial scxml) synth-initial)))
    (progn
      (should (eq (length (scxml-children scxml)) 2))
      (should (member* target-state (scxml-children scxml) :test 'eq))
      (should (member* another-state (scxml-children scxml) :test 'eq)))


    ;; ensure that the synthetic initial knows it has a child but only
    ;; in the scxmld graph.
    (progn
      (should (eq (length (scxmld-children synth-initial)) 1))
      (should (member* synth-transition (scxmld-children synth-initial) :test 'eq))
      (should (eq (scxmld-parent synth-initial) scxml)))
    (progn
      (should (seq-empty-p (scxml-children synth-initial)))
      (should (null (scxml-parent synth-initial))))

    ;; ensure that the synthetic transition knows its scxmld and scxml
    ;; graph
    (progn
      (should (seq-empty-p (scxmld-children synth-transition)))
      (should (eq (scxmld-parent synth-transition) synth-initial)))
    (progn
      (should (seq-empty-p (scxml-children synth-transition)))
      (should (null (scxml-parent synth-transition))))
    

    ))

  

(ert-deftest scxmld---get-targeting-transitions ()
  "make sure this picks up both transitions and synthetic transitions."

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

    (setq targeting-transitions (scxmld--get-targeting-transitions target-state))

    (should (eq 2 (length targeting-transitions)))))


(ert-deftest scxmld-synthetic-transition-test ()
  (let ((transition (scxmld-synthetic-transition :target "something")))
    (should (null (scxmld-parent transition)))
    (setq synth-initial (scxmld-synthetic-initial))
    (scxml-add-child synth-initial transition)
    (should (eq synth-initial (scxmld-parent transition)))))
               
               


(ert-deftest scmxld-scxml-scxmld-children-test ()
  (let ((scxml (scxmld-scxml))
        (synth-initial (scxmld-synthetic-initial)))
    (scxmld-add-child scxml synth-initial)
    (should (eq (length (scxmld-children scxml)) 1))))
        
