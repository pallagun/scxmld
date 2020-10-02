;; -*- lexical-binding: t; -*-
(require 'ert)
(require 'scxmld-elements)

(ert-deftest scxmld-transition-unlink-test ()
  (let ((scxml (scxmld-scxml))
        (stateA (scxmld-state :id "A"))
        (stateB (scxmld-state :id "B"))
        (transition (scxmld-transition :target "B")))
    (scxmld-add-child scxml stateA)
    (scxmld-add-child scxml stateB)
    (scxmld-add-child stateA transition)

    (should (member* transition (scxmld-children stateA)))
    (should (member* transition (scxmld-children stateB)))
    (should (eq stateA (first (scxmld-parents transition))))
    (should (eq stateB (second (scxmld-parents transition))))
    (should (eql 2 (length (scxmld-parents transition))))

    (scxmld-make-orphan stateB scxml)
    
    (should (member* transition (scxmld-children stateA)))
    (should (eql 1 (length (scxmld-children stateA))))
    
    (should (eq stateA (first (scxmld-parents transition))))
    (should (eql 1 (length (scxmld-parents transition))))
))


(ert-deftest scxmld-synthetic-initial-property-change-test ()
  ;; initial attributes should be set and unset when a transition has a target
  ;; change
  (let ((scxml (scxmld-scxml))
        (state (scxmld-state :id "myState"))
        (synth-initial (scxmld-synthetic-initial))
        (synth-transition (scxmld-synthetic-transition)))

    ;; I should be able to add a state to this scxml element.
    (scxmld-add-child scxml state)
    (scxmld-add-child scxml synth-initial)
    (scxmld-add-child synth-initial synth-transition)

    ;; should start as null
    (should (null (scxml-get-initial scxml)))

    (scxml-set-target-id synth-transition "myState")
    (should (equal "myState" (scxml-get-initial scxml)))

    (scxml-set-target-id synth-transition nil)
    (should (null (scxml-get-initial scxml)))

    (scxml-set-target-id synth-transition "myState")
    (should (equal "myState" (scxml-get-initial scxml)))))

(ert-deftest scxmld-synthetic-initial-add-remove-transition-test ()
  ;; initials should be set and unset when a transition beneath
  ;; the initial element is added or removed and it already has a
  ;; target set.
  (let ((scxml (scxmld-scxml))
        (state (scxmld-state :id "myState"))
        (synth-initial (scxmld-synthetic-initial))
        (synth-transition (scxmld-synthetic-transition :target "myState")))
    
    ;; I should be able to add a state to this scxml element.
    (scxmld-add-child scxml state)
    (scxmld-add-child scxml synth-initial)

    ;; should start as null
    (should (null (scxml-get-initial scxml)))

    (scxmld-add-child synth-initial synth-transition)
    (should (equal "myState" (scxml-get-initial scxml)))

    (scxmld-make-orphan synth-transition synth-initial)
    (should (null (scxml-get-initial scxml)))

    (scxmld-add-child synth-initial synth-transition)
    (should (equal "myState" (scxml-get-initial scxml))))
  )

(ert-deftest scxmld-synthetic-initial-add-remove-initial-test ()
  ;; initials should be set and unset when an initial (with a
  ;; transition) is added or removed and the transition has a target
  ;; set.
  (let ((scxml (scxmld-scxml))
        (state (scxmld-state :id "myState"))
        (synth-initial (scxmld-synthetic-initial))
        (synth-transition (scxmld-synthetic-transition :target "myState")))
    
    ;; I should be able to add a state to this scxml element.
    (scxmld-add-child scxml state)
    ;; and I'll separately add the synthetic elements together.
    (scxmld-add-child synth-initial synth-transition)

    ;; should start as null
    (should (null (scxml-get-initial scxml)))

    (scxmld-add-child scxml synth-initial)
    (should (equal "myState" (scxml-get-initial scxml)))

    (scxmld-make-orphan synth-initial scxml)
    (should (null (scxml-get-initial scxml)))

    (scxmld-add-child scxml synth-initial)
    (should (equal "myState" (scxml-get-initial scxml))))
  )
  


(ert-deftest scxmld-differential-graph-tests ()
  (let ((scxml (scxmld-scxml))
        (state (scxmld-state :id "myState"))
        (another-state (scxmld-state))
        (transition (scxmld-transition :target "myState")))

    ;; I should not be able to add synthetic elements to the scxml tree
    (should-error (scxml-add-child scxml (scxmld-synthetic-transition)))
    (should-error (scxml-add-child scxml (scxmld-synthetic-initial)))

    ;; I should be able to add a state to this scxml element.
    (scxmld-add-child scxml state)

    (should (eq scxml (scxml-parent state)))
    (should (eq 1 (length (scxmld-parents state))))
    (should (member* scxml (scxmld-parents state) :test 'eq))
    (should-error (scxmld-get-diagram-parents scxml))

    (should (eq 1 (length (scxml-children scxml))))
    (should (member* state (scxml-children scxml) :test 'eq))
    (should (eq 1 (length (scxmld-children scxml))))
    (should (member* state (scxmld-children scxml) :test 'eq))
    (should (null (scxmld-get-diagram-children scxml)))

    (should (eq scxml (scxmld-root-element scxml)))
    (should (eq scxml (scxml-root-element scxml)))

    (should (eq scxml (scxmld-root-element state)))
    (should (eq scxml (scxml-root-element state)))
    (should (null (scxml-children state)))
    (should (null (scxmld-children state)))

    ;; build up the subtree of another-state > transition separately.
    (scxmld-add-child another-state transition)
    (should (null (scxml-parent another-state)))
    (should (null (scxmld-parents another-state)))

    (should (eq 1 (length (scxml-children another-state))))
    (should (member* transition (scxml-children another-state)))
    (should (eq 1 (length (scxmld-children another-state))))
    (should (member* transition (scxmld-children another-state)))
    (should (eq another-state (scxmld-root-element another-state)))
    (should (eq another-state (scxml-root-element another-state)))

    (should (eq another-state (scxml-parent transition)))
    (should (eq 1 (length (scxmld-parents transition))))
    (should (member* another-state (scxmld-parents transition) :test 'eq))
    
    (should (null (scxml-children transition)))
    (should (null (scxmld-children transition)))
    (should (eq another-state (scxmld-root-element transition)))
    (should (eq another-state (scxml-root-element transition)))

    ;; now put both those trees together.
    (scxmld-add-child scxml another-state)

    ;; make sure the diagram links were established
    (should (member* transition
                     (scxmld-get-diagram-children state)
                     :test 'eq))
    (should (member* state
                     (scxmld-get-diagram-parents transition)
                     :test 'eq))

    ;; the transition should now have two parents in the scxmld tree.
    (should (eq another-state (scxml-parent transition)))
    
    (should (eq 2 (length (scxmld-parents transition))))
    (should (member* state (scxmld-parents transition) :test 'eq))
    (should (member* another-state (scxmld-parents transition) :test 'eq))

    (should (eq 1 (length (scxml-children another-state))))
    (should (member* transition (scxml-children another-state) :test 'eq))
    (should (eq 1 (length (scxmld-children another-state))))
    (should (member* transition (scxmld-children another-state) :test 'eq))

    (should (null (scxml-children state)))
    (should (eq 1 (length (scxmld-children state))))
    (should (member* transition (scxmld-children state) :test 'eq))

    ;; finally let's add in a synthetic transition from a synthetic initial to the state as well.
    (should (null (scxmld-get-synth-initial scxml)))

    (let ((synth-initial (scxmld-synthetic-initial))
          (synth-transition (scxmld-synthetic-transition :target "myState")))
      (scxmld-add-child synth-initial synth-transition)

      (should (null (scxml-parent synth-initial)))
      (should (null (scxmld-parents synth-initial)))
      
      (should (null (scxml-children synth-initial)))
      (should (eq 1 (length (scxmld-children synth-initial))))
      (should (member* synth-transition (scxmld-children synth-initial) :test 'eq))
      
      (should (null (scxml-children synth-transition)))
      (should (null (scxmld-children synth-transition)))
      (should (null (scxml-parent synth-transition)))
      (should (eq 1 (length (scxmld-parents synth-transition))))
      (should (member* synth-initial (scxmld-parents synth-transition) :test 'eq))

      (scxmld-add-child scxml synth-initial)

      (should (eq synth-initial (scxmld-get-synth-initial scxml)))
      (should (eq 3 (length (scxmld-children scxml))))
      (should (member* state (scxmld-children scxml) :test 'eq))
      (should (member* another-state (scxmld-children scxml) :test 'eq))
      (should (member* synth-initial (scxmld-children scxml) :test 'eq))
      
      (should (eq 2 (length (scxml-children scxml))))
      (should (member* state (scxml-children scxml) :test 'eq))
      (should (member* another-state (scxml-children scxml) :test 'eq))

      ;; at this point the synth transition should have two parents,
      ;; both of them diagram graph edges.
      (should (eq 2 (length (scxmld-parents synth-transition))))
      (should (eq synth-initial (first (scxmld-parents synth-transition))))
      (should (member* synth-initial (scxmld-parents synth-transition) :test 'eq))
      (should (member* state (scxmld-parents synth-transition) :test 'eq))
      
      (should (eq 2 (length (scxmld-get-diagram-parents synth-transition))))
      (should (member* synth-initial (scxmld-parents synth-transition) :test 'eq))
      (should (member* state (scxmld-parents synth-transition) :test 'eq))
      (should (null (scxml-parent synth-transition)))

      ;; additionally state should now have 2 children, both of them
      ;; transitions and they should be only diagram graph edges
      (should (eq 2 (length (scxmld-children state))))
      (should (member* synth-transition (scxmld-children state) :test 'eq))
      (should (member* transition (scxmld-children state) :test 'eq))

      (let ((all-elements (list scxml state another-state transition synth-initial synth-transition))
            (all-real-elements (list scxml state another-state transition))
            (all-synth-elements (list synth-initial synth-transition)))
        (cl-loop for element in all-elements
                 do (should (eq scxml (scxmld-root-element element))))
        (cl-loop for element in all-real-elements
                 do (should (eq scxml (scxml-root-element element))))
        (cl-loop for element in all-synth-elements
                 do (should (null (scxml-root-element element))))

        ;; visit all scxml elements
        (cl-loop for start-element in all-real-elements
                 do (let ((visit-list))
                      (scxml-visit-all scxml
                                       (lambda (element)
                                         (push element visit-list)))
                      (should (eq (length all-real-elements)
                                  (length visit-list)))
                      (cl-loop for element in all-real-elements
                               do (should (member* element
                                                   visit-list
                                                   :test 'eq)))))
        
        ;; visit all elements
        (cl-loop for start-element in all-elements
                 do (let ((visit-list))
                      (scxmld-visit-all scxml
                                        (lambda (element)
                                          (push element visit-list)))
                      (should (eq (length all-elements)
                                  (length visit-list)))
                      (cl-loop for element in all-elements
                               do (should (member* element
                                                   visit-list
                                                   :test 'eq)))))
        ))))

(ert-deftest scxmld-differential-graph-tests-from-property-change ()
  (let ((scxml (scxmld-scxml))
        (state (scxmld-state :id "myState"))
        (another-state (scxmld-state))
        (transition (scxmld-transition)))

    ;; I should be able to add a state to this scxml element.
    (scxmld-add-child scxml state)
    (scxmld-add-child scxml another-state)
    (scxmld-add-child another-state transition)

    ;; I should now have two states, one with a transition going nowhere.
    (should (eq 2 (length (scxml-children scxml))))
    (should (member* state (scxml-children scxml) :test 'eq))
    (should (member* another-state (scxml-children scxml) :test 'eq))
    (should (eq 2 (length (scxmld-children scxml))))
    (should (member* state (scxmld-children scxml) :test 'eq))
    (should (member* another-state (scxmld-children scxml) :test 'eq))

    (should (null (scxml-children state)))
    (should (null (scxmld-children state)))
    
    (should (eq 1 (length (scxml-children another-state))))
    (should (member* transition (scxml-children another-state)))
    (should (eq 1 (length (scxmld-children another-state))))
    (should (member* transition (scxmld-children another-state)))

    ;; set the transition
    (scxml-set-target-id transition "myState")

    ;; and now state should have a child on the diagram graph
    (should (eq 1 (length (scxmld-children state))))
    (should (member* transition (scxmld-children state)))
    (should (null (scxml-children state)))


    (should (eq 2 (length (scxmld-parents transition))))
    (should (member* state (scxmld-parents transition) :test 'eq))
    (should (member* another-state (scxmld-parents transition) :test 'eq))
    (should (eq another-state (scxml-parent transition)))

    ;; and now if I unset the transition target, the link should be severed.
    (scxml-set-target-id transition nil)
    (should (null (scxmld-children state)))
    (should (null (scxml-children state)))

    (should (eq 1 (length (scxmld-parents transition))))
    (should (member* another-state (scxmld-parents transition) :test 'eq))
    (should (eq another-state (scxml-parent transition)))

    
    ))

(ert-deftest scxmld-simple-diagram-graph-tests ()
  ;; here we'll add a transition to the tree last
  (let ((scxml (scxmld-scxml))
        (target-state (scxmld-state :id "myId"))
        (another-state (scxmld-state))
        (transition (scxmld-transition :target "myId")))
    
    (scxmld-add-child scxml target-state)
    (scxmld-add-child scxml another-state)
    
    ;; finally add the transition 
    (scxmld-add-child another-state transition)
    ;; (scxmld--update-diagram-graph-after-add another-state transition)

    (should (null (scxml-children target-state)))
    (should (eq 1 (length (scxmld-children target-state))))
    (should (member* transition (scxmld-children target-state)))

    (should (null (scxml-children transition)))
    (should (null (scxmld-children transition)))

    (should (eq 1 (length (scxml-children another-state))))
    (should (member* transition (scxml-children another-state)))
    (should (eq 1 (length (scxmld-children another-state))))
    (should (member* transition (scxmld-children another-state)))

    (should (eq 2 (length (scxml-children scxml))))
    (should (eq 2 (length (scxmld-children scxml))))
    (let ((visited-elements))
      (scxmld-visit scxml (lambda (element) (push element visited-elements)))
      (should (eq (length visited-elements) 4)))
    (let ((visited-elements))
      (scxml-visit scxml (lambda (element) (push element visited-elements)))
      (should (eq (length visited-elements) 4))))

  ;; here we'll add a state to the tree last
  (let ((scxml (scxmld-scxml))
        (target-state (scxmld-state :id "myId"))
        (another-state (scxmld-state))
        (transition (scxmld-transition :target "myId")))
    
    (scxmld-add-child another-state transition)
    (scxmld-add-child scxml another-state)
    
    ;; finally add the state
    (scxmld-add-child scxml target-state)
    ;; (scxmld--update-diagram-graph-after-add scxml target-state)

    (should (null (scxml-children target-state)))
    (should (eq 1 (length (scxmld-children target-state))))
    (should (member* transition (scxmld-children target-state)))

    (should (null (scxml-children transition)))
    (should (null (scxmld-children transition)))

    (should (eq 1 (length (scxml-children another-state))))
    (should (member* transition (scxml-children another-state)))
    (should (eq 1 (length (scxmld-children another-state))))
    (should (member* transition (scxmld-children another-state)))

    (should (eq 2 (length (scxml-children scxml))))
    (should (eq 2 (length (scxmld-children scxml))))

    (let ((visited-elements))
      (scxmld-visit scxml (lambda (element) (push element visited-elements)))
      (should (eq (length visited-elements) 4)))
    (let ((visited-elements))
      (scxml-visit scxml (lambda (element) (push element visited-elements)))
      (should (eq (length visited-elements) 4))))

  ;; now we'll throw in some synthetic elements on the diagram graph.
  (let ((scxml (scxmld-scxml))
        (target-state (scxmld-state :id "myId"))
        (another-state (scxmld-state))
        (transition (scxmld-transition :target "myId"))
        (synth-initial (scxmld-synthetic-initial))
        (synth-transition (scxmld-synthetic-transition :target "myId")))
    
    (scxmld-add-child scxml target-state)
    (scxmld-add-child scxml another-state)
    (scxmld-add-child another-state transition)

    ;; build up the synthetic elements
    (scxmld-add-child synth-initial synth-transition)
    ;; then add them.
    (scxmld-add-child scxml synth-initial)

    (should (null (scxml-children target-state)))
    (should (eq 2 (length (scxmld-children target-state))))
    (should (member* transition (scxmld-children target-state)))
    (should (member* synth-transition (scxmld-children target-state)))

    (should (null (scxml-children transition)))
    (should (null (scxmld-children transition)))

    (should (eq 1 (length (scxml-children another-state))))
    (should (member* transition (scxml-children another-state)))
    (should (eq 1 (length (scxmld-children another-state))))
    (should (member* transition (scxmld-children another-state)))

    (should (eq 2 (length (scxml-children scxml))))
    (should (eq 3 (length (scxmld-children scxml))))

    (should (null (scxml-children synth-initial)))
    (should (eq 1 (length (scxmld-children synth-initial))))
    ;; verify who the diagram child is.
    
    (let ((visited-elements))
      (scxmld-visit scxml (lambda (element) (push element visited-elements)))
      (should (eq (length visited-elements) 6)))
    (let ((visited-elements))
      (scxml-visit scxml (lambda (element) (push element visited-elements)))
      (should (eq (length visited-elements) 4))))
  )

(ert-deftest scxmld-visit-tests ()
  "Test out the graph traversal functionality"
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

    (should (eq scxml (scxmld-root-element scxml)))
    (should (eq scxml (scxml-root-element scxml)))

    (should (eq scxml (scxmld-root-element target-state)))
    (should (eq scxml (scxml-root-element target-state)))

    (should (eq scxml (scxmld-root-element another-state)))
    (should (eq scxml (scxml-root-element another-state)))

    (should (eq scxml (scxmld-root-element transition)))
    (should (eq scxml (scxml-root-element transition)))

    (should (eq scxml (scxmld-root-element synth-initial)))
    (should (null (scxml-root-element synth-initial)))

    (should (eq scxml (scxmld-root-element synth-transition)))
    (should (null (scxml-root-element synth-transition)))

    (let ((collector))
      (scxmld-visit scxml
                    (lambda (element)
                      (push element collector))
                    nil)
      (should (eq 6 (length collector)))
      (should (member* scxml collector :test 'eq))
      (should (member* synth-initial collector :test 'eq))
      (should (member* synth-transition collector :test 'eq))
      (should (member* target-state collector :test 'eq))
      (should (member* another-state collector :test 'eq))
      (should (member* transition collector :test 'eq)))))

(ert-deftest scxmld-synthetic-transition-instance-create ()
  "Make sure synthetic transitions are correctly initialized"

  (let ((transition (scxmld-transition))
        (synth-transition (scxmld-synthetic-transition)))
    (should (2dd-get-target-connector transition))
    (should (2dd-get-target-connector synth-transition))))
