(ns behajour
  (:use clojure.contrib.test-is)
  (:use clojure.contrib.str-utils))

(import 'java.io.StringWriter)
(import 'java.util.HashMap)

(defstruct step :stage :keywords :implementation)

(with-test
	(defn are-keywords-same? [first-keywords second-keywords]
	  (if (or (not (= (count first-keywords) (count second-keywords)))
			  (some false? (map #(or (and (fn? %1) (fn? %2)) (= %1 %2)) first-keywords second-keywords)))
		false
		true))
  (is (true? (are-keywords-same? ["a" "b" "c"] ["a" "b" "c"])))
  (is (true? (are-keywords-same? ["a" #(str %) "c"] ["a" #(num %) "c"])))
  (is (false? (are-keywords-same? ["a" #(str %) "c" "d"] ["a" #(num %) "c"])))
  (is (false? (are-keywords-same? ["a" #(str %) "c" "d"] ["a" "c" #(num %) "d"])))
  (is (false? (are-keywords-same? ["a" "b" "c" "d"] ["a" "b" "c"]))))

(with-test
	(defn check-first-step-has-different-keywords-to-the-rest [new-list]
		(if (< 1 (count new-list))
		  (let [new-step (first new-list)
				old-steps (rest new-list)]
			(not (some #(are-keywords-same? (new-step :keywords) (:keywords %)) old-steps)))
		  true))
  (is (true? (check-first-step-has-different-keywords-to-the-rest (list (struct step :given ["a" "b"]),
																		(struct step :given ["a" "c"]))))))

(def *steps* (ref () :validator (fn [new-list-of-steps]
								  (if (check-first-step-has-different-keywords-to-the-rest new-list-of-steps)
									true
									(throw (IllegalArgumentException. (str "Matches existing step : " (:keywords (first new-list-of-steps)))))))))

(def test-fn-map (new HashMap))

(with-test
	(defn- map-elements-to-strings [[ & elements]]
	  (map #(str %) elements))
  (is (= ["a" "b" "c"] (map-elements-to-strings ['a 'b 'c]))))

(with-test
	(defn- map-elements-to-fns-or-strings [[ & elements]]
	  (map #(try (do (if (fn? (eval %)) % (str %))) (catch java.lang.Throwable e (str %))) elements))
	  ;; (map (fn [el] (try (do (if (and (some (fn [sym] (= sym el)) (keys (ns-interns *ns*)))
	  ;; 								  (fn? (eval el)))
	  ;; 						   el
	  ;; 						   (str el)))
	  ;; 					 (catch java.lang.Throwable e (str el)))) elements))
  (is (= ["a" "b" "c"] (map-elements-to-fns-or-strings ['a 'b 'c])))
  (let [func #(num %)]
	(is (= ["a" "b" func] (map-elements-to-fns-or-strings ['a 'b func]))))
  (binding [test-fn-map 1]
	(is (= ["a" "b" "test-fn-map"] (map-elements-to-fns-or-strings ['a 'b 'test-fn-map])))))

(with-test
	(defn define-step [stage keywords fn]
	  (dosync (alter *steps* conj (struct step stage keywords fn))))
  (binding [*steps* (ref [])]
	(let [impl (fn [] 1)]
	  (define-step :given ["a" "b" "c"] impl)
	  (is (= @*steps* [{:stage :given :keywords ["a" "b" "c"] :implementation impl}])))))

(with-test
	(defn- make-keyword [keywd]
	  (let [k (. (str keywd) toLowerCase)]
		(if (some #(= k %) ["given" "when" "then"])
		  (keyword k)
		  (throw (new IllegalArgumentException (str "unexpected keyword : " keyword))))))
  (is (thrown? IllegalArgumentException (make-keyword 'asdf)))
  (is (= :given (make-keyword 'Given)))
  (is (= :when (make-keyword "when")))
  (is (= :then (make-keyword 'THEN))))

(defmacro defstep "Use this to create the steps that implement the scenarios"
  [[stage & keywords] args & body]
  `(define-step ~(make-keyword stage) ~(vec (map-elements-to-fns-or-strings keywords))
	 (fn ~args ~@body)))

(deftest test-defstep
  (is (= `(define-step :given ["a" "thing"]
			(fn [] (println "fish")))
		 (macroexpand-1
		  '(defstep
			   [Given a thing]
			   []
			 (clojure.core/println "fish")))))
  (is (vector? (nth (macroexpand-1 '(defstep [given a thing] [] (println "fish"))) 2)))
  (let [func #(num %)]
	(is (= `(define-step :given ["a" ~func]
			  (fn [] (println "frog")))
		   (macroexpand-1
			(list 'defstep
				  ['given 'a func]
				  []
				  '(clojure.core/println "frog")))))))

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
  (is (= true (match-step-keywords? ["a" "b" "c" "d" "e"] ["a" (fn [b c d e] 1)])))
  (is (= true (match-step-keywords? ["a" "b" "c" "d" "e"] ["a" #(%) "c" #(%) "e"])))
  (is (= true (match-step-keywords? ["a" "b" "c" "d"] ["a" #(%) "c" #(%)]))))

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
			   (recur (make-keyword token) (conj accum (keyword (. token toLowerCase))) (vec (rest test-clauses)))
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
	(defn parse-scenario [ & test-clauses]
	  (group-tokens (tokenize-scenario test-clauses)))
  (is (= [[:given "a"] [:when "b"] [:then "c"]]
		 (parse-scenario "Given" "a" "When" "b" "Then" "c"))))

(defn- print-step [stage test-clauses]
  (print (str stage " " (apply str test-clauses) " ")))

(with-test
	(defn- get-test-fn-args
	  ([scenario-keywords step-keywords]
		 (get-test-fn-args [] scenario-keywords step-keywords []))
	  ([accum scenario-keywords step-keywords args-to-be-converted]
		 (if (or (= 0 (count scenario-keywords))
				 (= 0 (count step-keywords)))
		   accum
		   (if (= (first step-keywords) (first scenario-keywords))
			 (recur accum (rest scenario-keywords) (rest step-keywords) [])
			 (if (= (second step-keywords)
					(second scenario-keywords))
			   (recur (conj accum (apply (first step-keywords) (conj args-to-be-converted (first scenario-keywords)))) (rest scenario-keywords) (rest step-keywords) [])
			   (recur accum (rest scenario-keywords) step-keywords (conj args-to-be-converted (first scenario-keywords))))))))

  (is (= [] (get-test-fn-args ["a" "b" "c"] ["a" "b" "c"])))
  (is (= [["d" "c" "b"]] (get-test-fn-args ["a" "b" "c" "d" "e"] ["a" (fn [b c d] [d c b]) "e"])))
  (is (= ["b" "d"] (get-test-fn-args ["a" "b" "c" "d" "e"] ["a" #(str %) "c" #(str %) "e"])))
  (is (= ["b"] (get-test-fn-args ["a" "b" "c"] ["a" #(str %) "c"]))))

(defn execute-scenario
  ([title tests]
	 (binding [*out* (new StringWriter)]
	   (println (str "\n Scenario : " title " (PENDING)\n"))
	   (if (execute-scenario title tests false)
		 (let [pending-test-info (str *out*)]
		   (with-test-out
			 (println pending-test-info))))))
  ([_ tests test-known-pending]
	 (if (< 0 (count tests))
	   (let [test-line (first tests)
			 [stage & test-clauses] test-line
			 step (match-steps? stage test-clauses)]
		 (print-step stage (str-join " " test-clauses))
		 (if step 
		   (do (if (not test-known-pending)
				 (apply (step :implementation) (get-test-fn-args test-clauses (step :keywords))))
			   (println " "))
		   (println "(PENDING)"))
		 (if step
		   (recur 't (rest tests) test-known-pending)
		   (recur 't (rest tests) true)))
	   test-known-pending)))

(defmacro scenario "The BDD scenario definition macro"
  [title & test-clauses]
  (let [test-name (re-gsub #"'" "-" (re-gsub #"[^a-zA-Z ?+*/!_-]" "" (re-gsub #" " "-" (str "behajour-test-" title))))]
	`(deftest ~(symbol test-name)
	   (execute-scenario ~title (parse-scenario ~@(map-elements-to-strings test-clauses))))))

(defn run-behajour-tests
  ([] (run-tests))
  ([& namespaces]
	 (run-tests namespaces)))

(deftest integration-test
  (binding [*steps* (ref [])
			test-fn-map (new HashMap)]
	(try
	 (defstep
		 [given a precondition with (fn [& args] (str args))]
		 [data]
	   (print " -- " data " --"))
	 (defstep
		 [then another result is equal to some data]
		 []
	   (print " -- should not be executed --"))

	 (scenario "the bdd library runs tests in a given, when, then format"
			   given a precondition with a bit of data in
			   and another precondition
			   When an action happens
			   and another action happens that requires some input
			   Then a result is true
			   and another result is equal to some data
			   and a final test that we received some input)

	 (behajour-test-the-bdd-library-runs-tests-in-a-given-when-then-format)

	 (finally (def behajour-test-the-bdd-library-runs-tests-in-a-given-when-then-format nil)))))

(deftest test-multiple-actions
  (binding [*steps* (ref [])
			test-fn-map (new HashMap)]
	(try
	 (defstep
		 [given a #(str %) number #(. Integer parseInt %)]
		 [k n]
	   (. test-fn-map put k n))

	 (defstep
		 [When the numbers are summed]
		 []
	   (. test-fn-map put "answer" (+ (. test-fn-map get "first")
									  (. test-fn-map get "second"))))

	 (defstep
		 [When the numbers are multiplied]
		 []
	   (. test-fn-map put "answer" (* (. test-fn-map get "first")
									  (. test-fn-map get "second"))))

	 (defstep
		 [Then a result is #(. Integer parseInt %)]
		 [n]
	   (is (= n (. test-fn-map get "answer"))))

	 (scenario "the + and * functions"
			   Given a first number 1
			   and a second number 2
			   When the numbers are summed
			   Then a result is 3
			   When the numbers are multiplied
			   Then a result is 2)

	 (behajour-test-the-+-and-*-functions)

	 (finally (def behajour-test-the-+-and-*-functions nil)))))

(deftest test-steps-are-displayed-as-pending
  (try
   (let [test-results (new StringWriter)]
	 (binding [*steps* (ref ())
			   *test-out* test-results]
	   (defstep [given one that does] [& args] (println args))
	   (scenario "test pending" Given a step that does not exist and one that does)
	   (behajour-test-test-pending))
	 (is (. (str test-results) contains "Scenario : test pending (PENDING)

:given a step that does not exist (PENDING)
:given one that does")))
   (finally (def behajour-test-test-pending nil))))

(deftest test-strings-group-tokens
  (binding [*steps* (ref [])
			test-fn-map (new HashMap)]
	(try
	 (defstep
		 [given a string #(do (. test-fn-map put "first conversion string called" "true") %)]
		 [arg]
	   (. test-fn-map put :given arg))
	 (defstep
		 [when the tokens are counted]
		 []
	   (. test-fn-map put :when "when"))
	 (defstep
		 [then there is #(do (. test-fn-map put "second conversion string called" "true") %) token]
		 [arg]
	   (. test-fn-map put :then arg))

	 (scenario "strings are one token"
			   Given a string "with some spaces"
			   When the tokens are counted
			   Then there is 1 token)

	 (behajour-test-strings-are-one-token)

	 (is (= 3 (count @*steps*)))
	 (is (= "with some spaces" (. test-fn-map get :given)))
	 (is (= "when" (. test-fn-map get :when)))
	 (is (= "1" (. test-fn-map get :then)))
	 (is (= "true" (. test-fn-map get "first conversion string called")))
	 (is (= "true" (. test-fn-map get "second conversion string called")))
	 (finally (def behajour-test-strings-are-one-token nil)))))

