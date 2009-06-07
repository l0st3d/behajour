;;; These are examples on how to use the Behajour lib.  Enter these
;;; forms into the REPL, to play along ...

;; some classpath monkeying - there's probably a better way of doing
;; this, but I haven't sorted that out yet ...
(def dir (clojure.contrib.str-utils/re-sub #"[^/]*$" "" (str *file*)))
(add-classpath (str "file://" dir))

;; namespace declaration
(ns behajour-examples (:require clojure.contrib.test-is) (:use behajour))


;; import libs
(use 'behajour)

;; create first scenario
(scenario "My first scenario"
		  Given a predicate
		  When an action happens
		  Then a result)

;; define some steps
(defstep
	[Given a predicate]
	[] (print " -- my first step"))

(defstep
	[Then a result]
	[] (print " -- my third step"))

;; run the tests at this point to see the test framework report on
;; which steps have yet to be implemented
(run-behajour-tests)

;; you should see something like this:
;;
;; user> (run-behajour-tests)
;;
;; Testing user
;;
;;  Scenario : My first scenario (PENDING)
;;
;; :given a predicate  -- my first step 
;; :when an action happens (PENDING)
;; :then a result  
;;
;;
;; Ran 1 tests containing 0 assertions.
;; 0 failures, 0 errors.
;; nil
;; user> 
;;
;; The (PENDING) comment appended to the name of the scenario tells
;; you that the it has not been fully implemented yet.  You can see
;; that the first step has been executed, and the print statement has
;; been interwoven with the test output.  The second step is the one
;; that is pending, and the third one was not executed, since it
;; depends on the second one.
;;
;; Let's implement the second step.

(defstep
	[When an action happens]
	[] (print " -- my second step"))

;; Now, when you run the tests with:

(run-behajour-tests)

;; you should see something like:
;; 
;; user> (run-behajour-tests)
;; 
;; Testing user
;; 
;; Ran 1 tests containing 0 assertions.
;; 0 failures, 0 errors.
;; nil
;; user> 
;;
;; Now you see that it's gone quiet.  This is because the test has
;; passed.  Time to move on to the next test.


;; Next we'll look at conversion functions, so we'll need a new
;; scenario:

(use 'clojure.contrib.test-is)

(scenario "the + function"
		  Given a first number 1
		  and a second number 2
		  When the numbers are summed
		  Then a result is 3)


(def test-data (new java.util.ArrayList))

(defstep
	[given a ~#(str %) number ~#(. Integer parseInt %)]
	[k n]
  (. test-data add n))

(defstep
	[When the numbers are summed]
	[]
  (print " -- " (seq test-data)))

