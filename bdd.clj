(ns behajour
  (:use clojure.contrib.test-is))

(import 'java.io.StringWriter)

(def *steps* (ref []))
(defstruct step :stage :keywords :implementation)

(with-test
	(defn- map-elements-to-strings [[ & elements]]
	  (map (fn [a] (str a)) elements))
  (is (= ["a" "b" "c"] (map-elements-to-strings ['a 'b 'c]))))

(with-test
	(defn defstep "Use this to create the steps that implement the scenarios" [[stage & keywords] fn]
	  (dosync (alter *steps* conj (struct step stage keywords fn))))
  (binding [*steps* (ref [])]
	(let [impl (fn [] 1)]
	  (defstep [:given "a" "b" "c"] impl)
	  (is (= @*steps* [{:stage :given :keywords ["a" "b" "c"] :implementation impl}])))))

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

(with-test
	(defn- get-matching-steps [stage keywords]
	  (filter (fn [step]
				(and (= stage (step :stage))
					 (match-step-keywords? keywords (step :keywords))))
			  @*steps*))
  (binding [*steps* (ref [{:stage :given :keywords ["a" "b" "c"] :implementation (fn [] 1)}
						  {:stage :when  :keywords ["a" "b" "c"] :implementation (fn [] 2)}
						  {:stage :then  :keywords ["a" "b" "c"] :implementation (fn [] 3)}])]
	(is (= [(first @*steps*)]
		   (get-matching-steps :given ["a" "b" "c"])))))

(with-test
	(defn- match-steps? [stage keywords]
	  (let [matching-steps (get-matching-steps stage keywords)]
		(if (= 1 (count matching-steps))
		  (first matching-steps)
		  false)))

  (binding [*steps* (ref [{:stage :given :keywords ["a" "b" "c"] :implementation (fn [] 1)}
						  {:stage :when  :keywords ["a" "b" "c"] :implementation (fn [] 2)}
						  {:stage :then  :keywords ["a" "b" "c"] :implementation (fn [] 3)}])]
	(is (= (first @*steps*) (match-steps? :given ["a" "b" "c"])))
	(is (= (second @*steps*) (match-steps? :when ["a" "b" "c"])))
	(is (= (nth @*steps* 2) (match-steps? :then ["a" "b" "c"])))))
  
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

(defn- print-step [stage test-clauses]
  (print (str stage " "))
  (doseq [clause test-clauses]
	(print (str clause " ")))
  (println "(PENDING)"))

(with-test
	(defn- get-test-fn-args 
	  ([scenario-keywords step-keywords]
		 (get-test-fn-args [] scenario-keywords step-keywords))
	  ([accum scenario-keywords step-keywords]
		 (if (or (= 0 (count scenario-keywords))
				 (= 0 (count step-keywords)))
		   accum
		   (if (= (first step-keywords) (first scenario-keywords))
			 (recur accum (rest scenario-keywords) (rest step-keywords))
			 (if (= (second step-keywords)
					(second scenario-keywords))
			   (recur (conj accum (first scenario-keywords)) (rest scenario-keywords) (rest step-keywords))
			   (recur (conj accum (first scenario-keywords)) (rest scenario-keywords) step-keywords))))))
  
  (is (= [] (get-test-fn-args ["a" "b" "c"] ["a" "b" "c"])))
  (is (= ["b" "c" "d"] (get-test-fn-args ["a" "b" "c" "d" "e"] ["a" (fn [b c d] [b c d]) "e"])))
  (is (= ["b"] (get-test-fn-args ["a" "b" "c"] ["a" (fn [a] a) "c"]))))

(defn- execute-scenario [tests]
  (doseq [test-line tests]
	(let [[stage & test-clauses] test-line
		  step (match-steps? stage test-clauses)]
	  (with-test-out
		(if step
		  (apply (step :implementation) (get-test-fn-args test-clauses (step :keywords)))
		  (print-step stage test-clauses))))))
  
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

(deftest test-steps-are-displayed-as-pending
  (binding [*test-out* (new StringWriter)]
	(scenario "test pending" Given something)
	(is (. (str *test-out*) matches ".*PENDING.*"))))

(def test-fn-map (new java.util.HashMap))

(deftest test-strings-group-tokens
  (binding [*steps* (ref [])
			test-fn-map (new java.util.HashMap)]

	(defstep [:given "a" "string" (fn [t] (println t))]
	  (fn [arg]
		(. test-fn-map put :given arg)))
	(defstep [:when "the" "tokens" "are" "counted"]
	  (fn []
		(. test-fn-map put :when "when")))
	(defstep [:then "there" "is" (fn [n] n)]
	  (fn [arg]
		(. test-fn-map put :then arg)))

	(scenario "strings are one token"
			  Given a string "with some spaces"
			  When the tokens are counted
			  Then there is 1)

	(is (= 3 (count @*steps*)))
	(is (= "with some spaces" (. test-fn-map get :given)))
	(is (= "when" (. test-fn-map get :when)))
	(is (= "1" (. test-fn-map get :then)))))
