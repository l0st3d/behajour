(ns bdd
  (:use clojure.contrib.test-is))

(def steps (ref []))

(defstruct step :stage)

(defmacro defstep "Use this to create the steps that implement the scenarios" [stage & elements]
  (dosync (ref-set steps (conj @steps (struct step stage)))))


(defn- tokenize-scenario 
  ([test-clauses]
	 (tokenize-scenario nil [] test-clauses))
  ([stage accum test-clauses]
	 (if (= 0 (count test-clauses))
	   accum
	   (let [token (. (str (first test-clauses)) toLowerCase)]
		 (if stage
		   (if (. "and" equalsIgnoreCase token)
			 (recur stage (conj accum stage) (vec (rest test-clauses)))
			 (if (or (. "then" equals token) (. "when" equals token))
			   (recur stage (conj accum (keyword token)) (vec (rest test-clauses)))
			   (recur stage (conj accum token) (vec (rest test-clauses)))))
		   (if (not (= token "given")) ;stage not known and not a given clause
			 (throw (new IllegalArgumentException (str "missing given - found " token)))
			 (recur :given [:given] (vec (rest test-clauses)))))))))

(defn group-tokens 
  ([tokens]
	 (group-tokens [] tokens))
  ([accum tokens]
	 (if (= 0 (count tokens))
	   accum
	   (let [token (first tokens)
			 last-accum-element-index (- (count accum) 1)]
		 (if (keyword? token)
		   (recur (conj accum [token]) (rest tokens))
		   (recur (assoc accum last-accum-element-index
						 (conj (get accum last-accum-element-index) token))
				  (rest tokens)))))))

(with-test
	(defn- parse-scenario [ & test-clauses]
	  (group-tokens (tokenize-scenario test-clauses)))
  (is (= [[:given "a"] [:when "b"] [:then "c"]]
		 (parse-scenario "Given" "a" "When" "b" "Then" "c"))))

(defmacro scenario "The BDD scenario definition macro"
  [ & body]
  (let [title (first body)
		test-clauses (rest body)]
	(conj (map (fn [a] (str a)) test-clauses) `parse-scenario)))

(deftest integration-test
  (scenario "the bdd library runs tests in a given, when, then format"
			given a precondition with some data in
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
