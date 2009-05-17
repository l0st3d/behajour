(ns bdd
  (:use clojure.contrib.test-is))

(with-test
	(defn defstep []
	  )
  )



(with-test
	(defn- parse-scenario [ & test-clauses]
	  (let [token (first test-clauses)
			remaining-clauses (rest test-clauses)]
		(cond (= "Given" token) (do ()))))
  (is (= {:given [""] :when [""] :then [""]} 
		 (parse-scenario 'Given "" 'When "" 'Then ""))))


(defmacro scenario "The BDD scenario definition macro"
  [ & body]
  (let [title (first body)
		test-clauses (rest body)
		current-phase :scenario]

	(parse-scenario test-clauses)

	(println ".")

	(list '+ 0 (count test-clauses))))




(deftest integration-test
  (scenario "the bdd library runs tests in a given, when, then format"
			Given a precondition with some data in
			and another precondition
			When an action happens
			and another action happens that requires some input
			Then a result is true
			and another result is equal to some data
			and a final test that we received some input))

(deftest test-multiple-actions
  (scenario "the + and * functions"
			Given a number 1
			and a number 2
			When the numbers are summed
			Then a result is 3
			When the numbers are multiplied
			Then a result is 2))

  
(deftest test-strings-group-tokens
  (scenario "strings are one token"
			Given a string "with some spaces"
			When the tokens are counted
			Then there is 1))