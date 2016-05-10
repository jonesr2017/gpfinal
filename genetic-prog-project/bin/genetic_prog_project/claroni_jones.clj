(ns genetic-prog-project.claroni_jones)


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
  "Subtracts the values of two lists and returns a list of the absolute value
   of the subtraction"
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



;;;  safe-divide, erc ;;; 


(defn safe-divide
  "Division that handels case of divide by 0
   by returning 1 if 0 is the divisor"
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
  [program]                                                                
  (eval (list 'fn                                                          ; is basiccally the same thing as doung (fn [x] program))
              '[x]
              program)))


(defn evaluate 
  "Evaluates a program with a given x-value.
   With 3 inputs, the first one should be map, second should be the program and
   the thrid should be the desired range to map the function along.
   With 4 inputs, the first is the map function, second is the program, third is the 
   start of the range (inclusive), fourth is the end of the range function (exclusive)"
  ([program x-value]                    ;evaluates with x-value
  (let [prog-fn (program-to-fn program)]
  (prog-fn x-value)))
  ([map program end]                    ;"map" argument can be anything... it just performs the map function 
  (let [prog-fn (program-to-fn program)]
  (map prog-fn (range end))))
  ([map program start end]              ; allows you to map a range of x-values from any start-end pair
  (let [prog-fn (program-to-fn program)]
  (map prog-fn (range start end))))
  )

(defn evaluate-population
  "Takes a population and an x-value and evaluates every individual program."
  [pop x-value]
  (map (fn [x] (evaluate x x-value)) pop))



;solution function


(def solution '(+ (* x (* x x)) (+ x 3))) 
(def solution-set (evaluate map solution -5 6))


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
  "Returns a number from range 2-4"
  []
  (rand-nth (range 2 5))) ; this might be better because the function is only depth 3, and need a range for ramped half-half


;;;;;;;;;;;;;;;;;;;;;  Initialization methods and helper functions  ;;;;;;;;;;;;;;;;;;;;;;;;


(defn full                                        ; yay!!! full works
  "Builds a function using Full method"
  [max-d]
  (let [d         max-d
        terminal1 (if (= (rand-term) '(erc -10 10)) ;made this so erc can be evaluated
                    (erc -10 10)
                    'x)
        terminal2 (if (= (rand-term) '(erc -10 10)) ;made this so erc can be evaluated
                    (erc -10 10)
                    'x)]
    (cond
      (= d 0) terminal1                              ;terminal if leave
      (= d 1) (list (rand-fn) terminal1 terminal2)   ; function with two terminals if 1 up from frontier
      :else (list (rand-fn) (full (dec d)) (full (dec d)))  ;recursion otherwise
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
  (take pop-size (repeatedly ramped-h-h)))               ;we used ramped-h-h to help vary the functions


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; fitness functions

(defn program-fitness                                       ; fitnees is the sum of absolute deviation from the solution at a given x-value
  "Returns the fitness of a program ."
  [program]
  (let [prog-values (evaluate map program -5 6)]                   ;the evaluated program from -5 to 5 (our predetermined range)
    (apply + (list-subtraction prog-values solution-set))))


(defn population-fitness                                                  ;mostly helpful for visualization of best functions
  "Returns a list of the fitness of each individual population"
  [population]
  (map (fn [x] (program-fitness x)) population))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;parent selection


(defn threshold-selection                       ;could use this functionn to reduce the population to 
                                                ;semi-decent programs and then do tourni and such
  "Returns a list of the individual programs in the population that have a fitness below or equal to the given
   fitness value"
  [value prog-pop]
  (filter (fn [x] (<= (program-fitness x) value)) prog-pop))   ;we found that this function greatly reduces diversity in population, 
                                                               ;so we didnt include it in our genetic-programming fn but wanted it to
                                                               ;be in here in-case we needed it for the next project
(defn tournament-selection 
  "Randomly selects 7 programs from the population input and outputs the best one with the best fitness"
  [prog-pop]
  (loop [i 0
         lst '()]
    (if (not (>= i 7))
      (recur (inc i)
             (conj lst (rand-nth prog-pop)))
      (first (sort-by program-fitness lst)))))


(defn best-n-progs                          ;this helps us guide our evolution towards better programs
  "Returns the best n number of programs from the given program population"
  [prog-pop n]
  (take n (sort-by program-fitness prog-pop)))

             

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  
  
;mutations helpers


(def instructions
  '{+ 2                               ; ' mark is important because it allows each to be a symbol
    * 2
    - 2
    safe-divide 2})


(defn program-size
  "Finds the size of the program, i.e. number of nodes in its tree."
  [prog]
  (if (not (seq? prog))            ; if its not a sequence it will return 1.. this lets us evalute the size of a terminal
    1
    (count (flatten prog))))       ;flatten essentially removes all parenthesis so you can easily evaluate size of the tree


(defn select-random-subtree                                         
  "Given a program, selects a random subtree and returns it."         ;prof helmuths code 
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


(defn replace-random-subtree                                                                   ; basically pt mutation
  "Given a program and a replacement-subtree, replace a random node       
   in the program with the replacement-subtree."
  ([prog replacement-subtree]
      (replace-random-subtree prog replacement-subtree (rand-int (program-size prog))))        ;prof helmuths code
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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; mutation or variation


(defn replication                                  ;basically reproduction
  "Returns clone of the input program."
  [prog]
  prog)


(defn subtree-mutation
  "Selects a random node from an input program and replaces it with a random subtree
   of max-depth 3"
  [program]
  (replace-random-subtree program (grow 3)))    ;arbitrarily selected max-depth 3


(defn hoist-mutation                           ;helps with our bloat problem
  "Selects root-node from the input program and replaces it with a random subtree
   from the program"
  [program]
  (replace-random-subtree program (select-random-subtree program) 0))


(defn pt-mutation
  "Selects a random node in a program and replaces it with a new node of the same type"
  [prog]
  (let [node (rand-int (program-size prog))              ;selects random number in prog size
        sub-tree (select-random-subtree prog node)       ;selects the subtree at that number
        terminal1 (if (= (rand-term) '(erc -10 10))      ; makes it so we can eval erc
                    (erc -10 10)
                    'x)]
    (if (seq? sub-tree)
      (replace-random-subtree prog (conj (rest sub-tree) (rand-fn)) node) ;replace with a random fn if a fn
      (replace-random-subtree prog terminal1 node) ;replace with terminal if terminal
      )
    ))


(defn cross-over
  "Performs cross-over mutation on two input parents
   by replacing a random subtree from parent 1 with a
   random subtree in parent2"
  [parent1 parent2]
  (replace-random-subtree parent1 (select-random-subtree parent2)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;mutate an entire generation

(defn mutate-generation
  "Takes an entire population of programs and produces a population of mutated children.
   Parameters: pt-mutation: 10%
               subtree-mutation: 10%
               cross-over: 55%
               hoist-mutation: 23%
               replication: 2%"
  [population]
  (loop [pop (best-n-progs population 30) ;takes the best 30 of the population to move evolution towards better programs
         n (rand-int 100)
         next-gen '()
         count 50]                               ;produces 50 children every time
            
    (if (= 0 count)
      next-gen
      (cond
        (< n 10) (recur  ;10% point mutation on a random program selected via tournament selection
                         pop
                         (rand-int 100)
                         (conj next-gen (pt-mutation (tournament-selection pop)))
                         (dec count))
        (< n 20) (recur ;10% sub-tree mutation on a random program selected via tournament selection
                        pop
                        (rand-int 100)
                        (conj next-gen (subtree-mutation (tournament-selection pop)))
                        (dec count))
        (< n 75) (recur ;55% cross over on two random programs selected via tournament selection
                        pop
                        (rand-int 100)
                        (conj next-gen (cross-over (tournament-selection pop) (tournament-selection pop)))
                        (dec count))
        (< n 98) (recur ;23% hoist mutation on a random program selected via tourny selection 
                        pop
                        (rand-int 100)
                        (conj next-gen (hoist-mutation (tournament-selection pop)))
                        (dec count))
        (< n 100) (recur ;5% replication
                         pop
                         (rand-int 100)
                         (conj next-gen (replication (tournament-selection pop)))
                         (dec count))
        )
      )
    )
  )
    
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;main

(defn genetic-programming
  "This function takes no inputs.
   It generates an initial population size of 1000, and produces
   new generations via different methods of mutation of population size 50.
   Each generation prints the generation number, the best program
   in that generation and the total error of that program.
   Will terminate at 50 generations or when a solution is found."
  []
  (loop [pop (generate-init-population 1000)          ;makes the first two runs have a longer runtime but gives better overall results
         gen-number 1]
    (let [best-prog (first(best-n-progs pop 1))
          best-err (float(program-fitness best-prog))]    
      (println "Generation number: " gen-number)
      (println "Best program this generation:" best-prog)
      (println "Total error of that program:"  best-err)
      (println "\n############################################################\n")
      (cond 
        (= best-err 0.0) (println "*****Solution Found!*****\n Solution is:" best-prog "\n")
        (= gen-number 50) (println "Max generations reached (" gen-number
                                   ").\nBest program:" best-prog
                                   "\nIt's error:" best-err 
                                   "\n\n############################################################\n")
        :else (recur
                (mutate-generation pop)
                (inc gen-number))
        )
      )
    )
  )


        
      
  
