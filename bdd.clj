(ns bdd
  (:use clojure.contrib.test-is))

(with-test
	(defn defstep []
	  )
  )



(defmacro scenario "The BDD scenario definition macro"
  [ & body]
  (list '+ 1 1))




(deftest integration-test
	(scenario "the bdd library runs tests in a given, when, then format"
			  Given a precondition with some data in
			  and another precondition
			  When an action happens
			  and another action happens that requires some input
			  Then a result is true
			  and another result is equal to some data
			  and a final test that we received some input))

  
