(ns behajour
  (:use clojure.contrib.test-is)
  (:use clojure.contrib.str-utils))

(import 'java.io.StringWriter)
(import 'java.util.HashMap)

(def *steps* (ref []))
(defstruct step :stage :keywords :implementation)

(def test-fn-map (new HashMap))

(with-test
	(defn- map-elements-to-strings [[ & elements]]
	  (map (fn [a] (str a)) elements))
  (is (= ["a" "b" "c"] (map-elements-to-strings ['a 'b 'c]))))

(with-test
	(defn- map-elements-to-fns-or-strings [[ & elements]]
	  (map #(if (fn? %) % (str %)) elements))
  (is (= ["a" "b" "c"] (map-elements-to-fns-or-strings ['a 'b 'c])))
  (let [func #(num %)]
	(is (= ["a" "b" func] (map-elements-to-fns-or-strings ['a 'b func])))))

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
		(if (some #(= % k) ["given" "when" "then"])
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
			   (recur stage (conj accum (keyword (. token toLowerCase))) (vec (rest test-clauses)))
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
  (print (str stage " " (apply str test-clauses) " ")))

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

(with-test
	(defn- execute-scenario
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
			 (if (and step (not test-known-pending))
			   (apply (step :implementation) (get-test-fn-args test-clauses (step :keywords)))
			   (println "(PENDING)"))
			 (if step
			   (recur 't (rest tests) test-known-pending)
			   (recur 't (rest tests) true)))
		   test-known-pending)))
  
  (is (= 1 1)))

(defmacro scenario "The BDD scenario definition macro"
  [title & test-clauses]
  (let [test-name (re-gsub #"[^a-zA-Z -?]" "" (re-gsub #" " "-" (str "behajour-test-" title)))]
	`(deftest ~(symbol test-name)
	   (execute-scenario ~title (parse-scenario ~@(map-elements-to-strings test-clauses))))))

(deftest integration-test
  (binding [*steps* (ref [])
			test-fn-map (new HashMap)]
	(defstep
		[given a precondition with some data in]
		[]
	  (println "a"))


	(scenario "the bdd library runs tests in a given, when, then format"
			  given a precondition with some data in
			  and another precondition
			  When an action happens
			  and another action happens that requires some input
			  Then a result is true
			  and another result is equal to some data
			  and a final test that we received some input)))

(deftest test-multiple-actions
  (binding [*steps* (ref [])
			test-fn-map (new HashMap)]

	(defstep
		[given a #(str %) number #(num %)]
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
		[Then a result is #(num %)]
		[n]
	  (is (= n (. test-fn-map get "answer"))))

	(scenario "the + and * functions"
			  Given a first number 1
			  and a second number 2
			  When the numbers are summed
			  Then a result is 3
			  When the numbers are multiplied
			  Then a result is 2))
	)

(deftest test-steps-are-displayed-as-pending
  (binding [*test-out* (new StringWriter)]
	(scenario "test pending" Given something)
	(behajour-test-test-pending)
	(def behajour-test-test-pending nil)
	(is (. (str *test-out*) matches ".*PENDING.*"))))

(deftest test-strings-group-tokens
  (binding [*steps* (ref [])
			test-fn-map (new HashMap)]

	(defstep
		[given a string (fn [t] (. test-fn-map put "first conversion string called" "true") t)]
		[arg]
	  (. test-fn-map put :given arg))
	(defstep
		[when the tokens are counted]
		[]
	  (. test-fn-map put :when "when"))
	(defstep
		[then there is (fn [n] (. test-fn-map put "second conversion string called" "true") n) token]
		[arg]
	  (. test-fn-map put :then arg))

	(println (str @*steps*))

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
	(is (= "true" (. test-fn-map get "second conversion string called")))))
