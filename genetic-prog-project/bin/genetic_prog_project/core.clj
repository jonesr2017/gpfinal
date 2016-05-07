(ns genetic-prog-project.core)



;genetic 

;========================================

; Next, we'll show how to select a random subtree from
; a program, and how to replace a random subtree with
; a given subtree.

(def instructions
  '{+ 2                               ; ' mark is important because it allows each to be a symbol
    * 2
    - 2
    inc 1})

(defn program-size
  "Finds the size of the program, i.e. number of nodes in its tree."
  [prog]
  (if (not (seq? prog))            ; if its not a sequence it will return 1.. this lets us evalute the size of a terminal
    1
    (count (flatten prog))))       ;flatten essentially removes all parenthesis so you can easily evaluate size of the tree

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

;;; evaluate, program-to-fn, exponent and erc ;;; 

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

(defn erc
  "Generates a random number between min and max (inclusive)"
  [min max]
  (rand-nth (range min (+ max 1))))

; Let's say we have a program in the form of a list;
; this one is 5x + (2x - 3) = 7x - 3

(def prog
  '(+ (* x 5) (- (+ x x) 3)))

prog

;(eval prog) ; error: we haven't said what x should be!

;(prog 5) ; error: prog isn't a function, it is just a list

; We need to turn the program into a function, where
; x is the input to the function. Then, we could run it
; on the input.

(defn program-to-fn                                                  ;this  will turn the prog above into an actual function using eval
  "Takes a GP program represented as a list, with input x,                  
   and transforms it into a function that can be called on an input.
   NOTE: If your GP uses variables other than x, will need to change
         the argument list below to something other than [x]!"
  [program]                                                                ; prog is (+ (* x 5) (- (+ x x) 3))
  (eval (list 'fn                                                          ; is basiccally the same thing as doung (fn [x] program))
              '[x]
              program)))

; Now, we can get a function version of prog, and then apply
; it to input 2:

(let [prog-fn (make-program-into-fn prog)]
  (prog-fn 2))

; Q: Let's say the inputs are (range 10)
; How can we get the outputs of the program on those inputs?
; A:
(let [prog-fn (make-program-into-fn prog)]
  (map prog-fn (range 10)))

(defn evaluate 
  "Evaluates a program with a given x-value.
   With 3 inputs, the first one should be map, second should be the program and
   the thrid should be the desired range to map the function along."
  ([program x-value]
  (let [prog-fn (make-program-into-fn program)]
  (prog-fn x-value)))
  ([map program x-value]
  (let [prog-fn (make-program-into-fn program)]
  (map prog-fn (range x-value))))
  )

prog
(evaluate prog 2)
(evaluate map prog 10)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; GP constants ;;;

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
;
(rand-term)



(def function-set
  '(exp + - / *))

(defn rand-fn
  "returns a ranom value in the function set"
  []
  (rand-nth function-set))
;
(rand-fn)



(def primitive-set
  (concat terminal-set function-set))

(defn rand-prim
  "returns a ranom value in the primitive set"
  []
  (rand-nth primitive-set))
;
(rand-prim)



(defn max-depth 
  "Returns a number from range 2-5"
  []
  (rand-nth (range 2 6))) ; this might be better because the function is only depth 3, and need a range for ramped half-half



;;;;;;;;;;;;;;;;
;helmuth help

;So, the way you're building up a program, you'll only be able to add things onto a linear vector (or list).
;
;But, you want to create a nested structure. To do that, when you select a function, you'll need to stick it
;in a list along with its two arguments (or some other if it takes a different number of arguments).
;
;The question then is what those arguments will be. My suggestion would be that they should be created in recursive
;calls to the same function, adjusting the depth accordingly.
;
;Think about it this way: if the function is called with depth of 0, it should produce a single terminal, like x or 3.
;If depth is 1, it will choose one function, and the recursive calls for its arguments will have depth 0 and will be terminals,
;creating something like (+ 3 x).

;;;;;;;;;;;;;;;;;;;;;  Initialization methods and helper functions  ;;;;;;;;;;;;;;;;;;;;;;;;



(defn full                                        ; yay!!! full works
  "Builds a function using Full method"
  [max-d]
  (let [d max-d]
    (cond
      (= d 0) (rand-term)
      (= d 1) (list (rand-fn)(rand-term) (rand-term))
      :else (list (rand-fn) (full (dec d)) (full (dec d)))
      )
    )
  )
(full 2)
(full (max-depth))

(def full1 (full 1))
full1
(evaluate full1 2)               ;need to fix the divide by 0 problem
(evaluate map prog 10)
    



(defn grow                                                                   ; yay! grow works
  "Builds a function using Grow method, by returning one value at a time"
  [max-d]
  (let [d max-d
        odds (rand)
        lst '()]
    (cond
      (= d 0) (rand-term)
      (= d 1) (if (< odds 0.5)
                (list (rand-fn) (rand-term) (rand-term))
                (rand-term))
      :else (if (< odds 0.5)
              (list (rand-fn) (grow (dec d)) (grow (dec d)))
              (rand-term))
      )
    )
  )

(grow 2)

(def grow2 (grow 2))
grow2
(evaluate grow2 2)
(evaluate map grow2 6)   ;evaluates grow2 for 0-5



(defn ramped-h-h                                              ; does ramped half and half either do full or grow? or does it decide between full and grow as it goes down the tree
  "Builds a program tree using ramped half and half"
  [max-dp fns terms prims]
  (loop [program []
         d 0]
    (cond
      (isFull? program) program
      (< (rand) 0.5) (recur 
                       (conj program (full program d function-set terminal-set))
                       (evaluate-depth)
      (< (rand) 0.5) (recur
                       (conj program (grow program d primitive-set terminal-set))
                       (evaluate-depth)))
    )
  )
      
  
                  
