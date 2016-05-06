(ns genetic-prog-project.core)



;genetic programming


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

(defn make-program-into-fn                                                  ;this  will turn the prog above into an actual function using eval
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

(replace-random-subtree prog 99)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;start of our project: "genetic-prog-project"

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; helper functions/ function definitions ;;; 

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
  (rand-nth (range min (+ max 1)))
)




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; GP implementations ;;;

; list of terminals
; erc [-10, 10], x

; list of functions
; +, -, /, *, exp

(def terminal-set
  '((erc -10 10) x))

(def function-set
  '(exp + - / *))

(def primitive-set
  (concat terminal-set function-set))

(def max-depth 6)




(defn genetic-programming
  []
  "dingle-dongle")

(defn full
  [depth fns terms]
  (loop [prog [] 
         d 0]
    (let [cur (eval(rand-nth terms))]
      
      (cond
        (= d  depth) (conj prog cur) ; if we are at max depth, add a terminal... wont work because wont make a full tree...yet
        (= d (+ depth 1)) prog
        :else (recur (conj prog (rand-nth fns)) (inc d))))))

      

                  
