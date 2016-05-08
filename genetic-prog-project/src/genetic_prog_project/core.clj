(ns genetic-prog-project.core)



(defn program-size
  "Finds the size of the program, i.e. number of nodes in its tree."
  [prog]
  (if (not (seq? prog))            ; if its not a sequence it will return 1.. this lets us evalute the size of a terminal
    1
    (count (flatten prog))))       ;flatten essentially removes all parenthesis so you can easily evaluate size of the tree


(def prog
  '(+ (* x 5) (- (+ x x) 3)))

(program-size prog)
(program-size 5)

(defn select-random-subtree
  "Given a program, selects a random subtree and returns it."
  ([prog]
    (select-random-subtree prog (rand-int (program-size prog))))
  ([prog subtree-index]
    (cond
      (not (seq? prog)) prog
      (and (zero? subtree-index)
           (some #{(first prog)} (keys instructions))) prog
      (< subtree-index (program-size (first prog))) (recur (first prog)
                                                           subtree-index)
      :else (recur (rest prog)
                   (- subtree-index (program-size (first prog)))))))

(select-random-subtree prog) ; gives random subtree

(select-random-subtree prog 0) ;gives subtree at index 0
(select-random-subtree prog 1) ;gives subtree at index 1
(select-random-subtree prog 2) ;gives subtree at index 2

(defn replace-random-subtree                                                                   ; basically pt mutation
  "Given a program and a replacement-subtree, replace a random node
   in the program with the replacement-subtree."
  ([prog replacement-subtree]
      (replace-random-subtree prog replacement-subtree (rand-int (program-size prog))))
  ([prog replacement-subtree subtree-index]
    (cond
      (not (seq? prog)) replacement-subtree
      (zero? subtree-index) replacement-subtree
      :else (map (fn [element start-index]
                   (if (<= start-index
                           subtree-index
                           (+ start-index -1 (program-size element)))
                     (replace-random-subtree element
                                             replacement-subtree
                                             (- subtree-index start-index))
                     element))
                 prog
                 (cons 0 (reductions + (map program-size prog)))))))

prog

(replace-random-subtree prog '(+ x 5))



(defn deep-map                                                             ; might need in future
  "deep-map takes a function and a nested collection as input arguments
   and applies the function to each element within the colection and 
   nested collections"
  [function nested-collection]
  (map (fn [element] 
         (if (= (coll? element) false)                            
           (function element)                                     
           (deep-map function element)                            
         )
        )
       nested-collection)                                         
)


(defn in                                         ;might need
  "Returns true if value is in collection"
  [value collection]
  (loop [i 0]
    (cond
      (= (nth collection i) value) true
      (= i (- (count collection) 1)) false
      :else (recur (inc i))
      )
    )
  )


















;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Authors: Dan Claroni and Rob Jones

;start of our basic implementaion project: "genetic-prog-project"

; want to solve the problem x^3 + x + 3
;
;   levels
;   0                      +
;                        /   \
;   1                  *      +
;                    /  \    / \
;   2              x    *   x  3
;                      / \
;   3                 x  x

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; helper functions

(defn abs
  "Absolute value of x"
  [x]
  (max x (- 0 x)))

(defn list-subtraction
  "Subtracts the value of two lists and returns a list of the absolute value of those subtractions"
  [list1 list2]
  (loop [i (- (count list1) 1)
         lst '()]
    (if (< i 0)
      lst
      (recur (dec i)
             (conj lst (abs (- (nth list1 i) (nth list2 i))))
             )
      )
    )
  )



;solution function

(def solution '(+ (* x (* x x)) (+ x 3)))
(def solution-set (evaluate map solution -5 6))


;definitions of terminals and functions

;;; evaluate, safe-divide, program-to-fn, exponent and erc ;;; 

(defn exp
  "Rasies the base to the pwr"
  [base-i pwr]
  (loop [base base-i 
         pwr pwr]
    (cond 
      (= pwr 0) 1
      (= pwr 1) (* base 1)
      (> pwr 1) (recur
                  (* base base-i)
                  (dec pwr))
      )
    )
  )

(defn safe-divide
  "Division that handels case of divide by 0"
  [dividend divisor]
  (if (= divisor 0)
    1
    (/ dividend divisor)))

(defn erc
  "Generates a random number between min and max (inclusive)"
  [min max]
  (rand-nth (range min (+ max 1))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;;;;;;;;;;;;; evaluating programs


(defn program-to-fn                                                  ;this  will turn the prog above into an actual function using eval
  "Takes a GP program represented as a list, with input x,                  
   and transforms it into a function that can be called on an input.
   NOTE: If your GP uses variables other than x, will need to change
         the argument list below to something other than [x]!"
  [program]                                                                ; prog is (+ (* x 5) (- (+ x x) 3))
  (eval (list 'fn                                                          ; is basiccally the same thing as doung (fn [x] program))
              '[x]
              program)))


(defn evaluate 
  "Evaluates a program with a given x-value.
   With 3 inputs, the first one should be map, second should be the program and
   the thrid should be the desired range to map the function along.
   With 4 inputs, the first is the map function, second is the program, third is the 
   start of the range (inclusive), fourth is the end of the range function (exclusive)"
  ([program x-value]
  (let [prog-fn (program-to-fn program)]
  (prog-fn x-value)))
  ([map program end]
  (let [prog-fn (program-to-fn program)]
  (map prog-fn (range end))))
  ([map program start end]
  (let [prog-fn (program-to-fn program)]
  (map prog-fn (range start end))))
  )

(defn evaluate-population
  "Takes a population and an x-value and evaluates every individual program."
  [pop x-value]
  (map (fn [x] (evaluate x x-value)) pop))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Terminal, function, and primitive sets  and decided range for max-depth ;;;

; list of terminals
; erc [-10, 10], x

; list of functions
; +, -, /, *, exp

(def terminal-set
  '((erc -10 10) x))

(defn rand-term
  "returns a ranom value in the terminal set"
  []
  (rand-nth terminal-set))




(def function-set
  '(+ - safe-divide *))

(defn rand-fn
  "returns a ranom value in the function set"
  []
  (rand-nth function-set))




(def primitive-set
  (concat terminal-set function-set))

(defn rand-prim
  "returns a ranom value in the primitive set"
  []
  (rand-nth primitive-set))




(defn depth-range 
  "Returns a number from range 2-5"
  []
  (rand-nth (range 2 5))) ; this might be better because the function is only depth 3, and need a range for ramped half-half


;;;;;;;;;;;;;;;;;;;;;  Initialization methods and helper functions  ;;;;;;;;;;;;;;;;;;;;;;;;



(defn full                                        ; yay!!! full works
  "Builds a function using Full method"
  [max-d]
  (let [d         max-d
        terminal1 (if (= (rand-term) '(erc -10 10))
                    (erc -10 10)
                    'x)
        terminal2 (if (= (rand-term) '(erc -10 10))
                    (erc -10 10)
                    'x)]
    (cond
      (= d 0) terminal1
      (= d 1) (list (rand-fn) terminal1 terminal2)
      :else (list (rand-fn) (full (dec d)) (full (dec d)))
      )
    )
  )

   

(defn grow                                                                   ; yay! grow works
  "Builds a function using Grow method, by returning one value at a time"
  [max-d]
  (let [d max-d
        odds (rand)
        terminal1 (if (= (rand-term) '(erc -10 10))
                    (erc -10 10)
                    'x)
        terminal2 (if (= (rand-term) '(erc -10 10))
                    (erc -10 10)
                    'x)]
    (cond
      (= d 0) terminal1
      (= d 1) (if (< odds 0.5)
                (list (rand-fn) terminal1 terminal2)
                terminal1)
      :else (if (< odds 0.5)
              (list (rand-fn) (grow (dec d)) (grow (dec d)))
              terminal1)
      )
    )
  )




(defn ramped-h-h                                              
  "Builds a program tree using ramped half and half"
  []
  (let [odds (rand)]
    (if (< odds 0.5)
      (grow (depth-range))
      (full (depth-range)))
    )
  )


(defn generate-init-population
  [pop-size]
  (take pop-size (repeatedly ramped-h-h)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; fitness functions

(defn program-fitness                                       ; fitnees is the absolute deviation from the solution at a given x-value
  "Returns the fitness of a program ."
  [program]
  (let [prog-values (evaluate map program -5 6)]                   ;the evaluated program from -5 to 5 (our predetermined range)
    (apply + (list-subtraction prog-values solution-set))))
;tester
solution-set

(defn population-fitness
  "Returns a list of the fitness of each individual population"
  [population]
  (map (fn [x] (program-fitness x)) population))
;tester
(def poppi (generate-init-population 2))
poppi
(population-fitness poppi)
solution-set
    



(defn tournament-selection
  "Returns a list of the individual functions in the population that have a fitness below the given
   fitness value"
  [value fn-pop]
  (filter (fn [x] (< (program-fitness x) value)) fn-pop))   
;tester
(tournament-selection 10 (generate-init-population 10000))

  

                  
