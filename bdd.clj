(ns bdd
  (:use clojure.contrib.test-is))

(def steps (ref []))
(def *test-output* *out*)
(defstruct step :stage :keywords :implementation)

(with-test
	(defn- map-elements-to-strings [[ & elements]]
	  (map (fn [a] (str a)) elements))
  (is (= ["a" "b" "c"] (map-elements-to-strings ['a 'b 'c]))))

(with-test
	(defn defstep "Use this to create the steps that implement the scenarios" [[stage & keywords] fn]
	  (dosync (ref-set steps (conj @steps (struct step stage keywords fn)))))
  (binding [steps (ref [])]
	(let [impl (fn [] 1)]
	  (defstep [:given "a" "b" "c"] impl)
	  (is (= @steps [{:stage :given :keywords ["a" "b" "c"] :implementation impl}])))))

(with-test
	(defn- match-step-keywords? [scenario-keywords step-keywords]
	  (let [scenario-keyword (first scenario-keywords)
			step-keyword (first step-keywords)
			compare-as-strings (fn [sc st] (and (string? st) (. st equalsIgnoreCase sc)))]
		(cond
		  (and (= 0 (count scenario-keywords) (count step-keywords)))
		  true

		  (and (fn? step-keyword) (= 1 (count step-keywords))
			   (= 0 (count scenario-keywords)))
		  true

		  (compare-as-strings scenario-keyword step-keyword)
		  (recur (rest scenario-keywords) (rest step-keywords))
		  
		  (fn? step-keyword)
		  (if (compare-as-strings (second scenario-keywords) (second step-keywords))
			(recur (rest scenario-keywords) (rest step-keywords))
			(recur (rest scenario-keywords) step-keywords))
		  
		  :else false)))

  (is (= false (match-step-keywords? ["a"] ["a" "b"])))
  (is (= true (match-step-keywords? ["a" "b" "c"] ["a" "b" "c"])))
  (is (= true (match-step-keywords? ["aaa" "bbb" "ccc"] ["aaa" "bbb" "ccc"])))
  (is (= false (match-step-keywords? ["a" "b" "Q"] ["a" "b" "c"])))
  (is (= true (match-step-keywords? ["a" "b" "c" "d" "e"] ["a" (fn [b c d] 1) "e"])))
  (is (= false (match-step-keywords? ["a" "b" "c" "d" "e"] ["a" (fn [b c] 1) "d"])))
  (is (= true (match-step-keywords? ["a" "b" "c" "d" "e"] ["a" (fn [b c d e] 1)]))))

(defn- get-matching-steps [stage keywords]
  (filter (fn [step]
			(and (= stage (step :stage))
				 (match-step-keywords? keywords (step :keywords))))
		  steps))

(with-test
	(defn- match-steps? [stage & keywords]
	  (let [matching-steps (get-matching-steps stage keywords)]
		(if (= 1 (count matching-steps))
		  (first matching-steps)
		  false)))

  (let [impl-1 (fn [] 1)
		impl-2 (fn [] 2)
		impl-3 (fn [] 3)]
	(binding [steps [{:stage :given :keywords ["a" "b" "c"] :implementation impl-1}
					 {:stage :when :keywords ["a" "b" "c"] :implementation impl-2}
					 {:stage :then :keywords ["a" "b" "c"] :implementation impl-3}]]
	  (is (= impl-1 (match-steps? :given "a" "b" "c")))
	  (is (= impl-2 (match-steps? :when "a" "b" "c")))
	  (is (= impl-3 (match-steps? :then "a" "b" "c"))))))
  
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
		   (if (not (= token "given"))
			 (throw (new IllegalArgumentException (str "missing given - found " token)))
			 (recur :given [:given] (vec (rest test-clauses)))))))))

(defn- group-tokens 
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

(defn- execute-scenario [tests]
  (doseq [test-line tests]
	(let [[stage test-clauses] test-line
		  step (match-steps? stage test-clauses)]
	  (if step
		((step :implementation) test-clauses)
		()))))



  
(defmacro scenario "The BDD scenario definition macro"
  [title & test-clauses]
  (list `execute-scenario (conj (map-elements-to-strings test-clauses) `parse-scenario)))

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

(def test-fn-map (new java.util.HashMap))

(deftest test-strings-group-tokens
  (binding [steps (ref [])
			test-fn-map (new java.util.HashMap)]

	(defstep [:given 'a 'string (fn [t] (println t))]
	  (fn [arg]
		(. test-fn-map put :given arg)))
	(defstep [:when 'the 'tokens 'are 'counted]
	  (fn []
		(. test-fn-map put :when "when")))
	(defstep [:then 'there 'is (fn [n] n)]
	  (fn [arg]
		(. test-fn-map put :then arg)))

	(scenario "strings are one token"
			  Given a string "with some spaces"
			  When the tokens are counted
			  Then there is 1)

	(is (= 3 (count @steps)))
	(is (= "with some spaces" (. test-fn-map get :given)))
	(is (= "when" (. test-fn-map get :when)))
	(is (= "1" (. test-fn-map get :then)))))
