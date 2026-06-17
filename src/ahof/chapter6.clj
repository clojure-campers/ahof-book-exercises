(ns ahof.chapter6
  (:require
   [hyperfiddle.rcf :as rcf]
   [ahof.chapter5 :as chp5]))

;; Level 1: The Violation Function.
;; Write a constraint function c1 that enforces x + y ≤ 5. It should take a vector [x y] and return the violation amount (0 if satisfied, positive if violated).
(defn c1 [[x y]]
  (max 0 (- (+ x y) 5)))

(rcf/tests
 (c1 [0 0]) := 0
 (c1 [2 2]) := 0
 (c1 [3 2]) := 0

 (c1 [10 0]) := 5)

;; Level 2: The Penalty.
;; Write a simple penalize HOF that takes an objective function f, a list of constraints, and a multiplier ρ. It should return a new function that adds the squared violations multiplied by ρ to the objective.

;; This is provided in Listing 6.1
(defn penalize
  "Transform a constrained problem into an unconstrained one.
   constraints: seq of (fn [x] -> violation-amount).
   rho: penalty multiplier."
  [f constraints rho]
  (fn [x]
    (+ (f x)
       (* rho (reduce + (map (fn [c] (let [v (c x)] (* v v)))
                             constraints))))))

;; Level 3: Unconstrained Testing.
;; Define f (x, y) = x2 + y2. Use your penalize function with c1 and ρ= 100. Evaluate the penalized function at [3 3].

(defn f [[x y]] (+ (* x x) (* y y)))

(def pf (penalize f [c1] 100))

(rcf/tests
 (f [3 3])
 := 18

 ;; [3 3] violates by 1, so expect 18 + 100*1*1 = 118
 (pf [3 3])
 := 118

 (f [0 8])
 := 64

 ;; [0 8] violates by 3, so expect 64 + 100*3*3 = 964
 (pf [0 8])
 := 964)

;; Level 4: The Barrier.
;; Write a log-barrier HOF that takes f, a list of constraints, and a multiplier µ. It should return a new function that subtracts µ*ln(−g_i(x)) for each constraint.

;; This is provided by Listing 6.4
(defn log-barrier
  "Transform a constrained problem using a log barrier.
   Only works when starting inside the feasible region."
  [f constraints mu]
  (fn [x]
    (let [violations (map (fn [c] (c x)) constraints)]
      (if (every? neg? violations)
        ;; Inside feasible region: add barrier
        (- (f x) (* mu (reduce + (map #(Math/log (- %))
                                      violations))))
        ;; Outside: return a very large value
        Double/MAX_VALUE))))

;; Note: log-barrier expects the constraint in Level 1 to return negatives
(defn c1 [[x y]]
  (- (+ x y) 5))

(rcf/tests
 (c1 [0 0]) := -5
 (c1 [2 2]) := -1
 (c1 [3 2]) := 0

 (c1 [10 0]) := 5)


;; Level 5: Barrier Testing.
;; Evaluate your log-barrier function at [2 2] with µ = 1.0. What happens if you evaluate it at [3 3]?

(def bf (log-barrier f [c1] 1.0))

(rcf/tests
 (f [2 2])
 := 8

 ;; inside
 (bf [2 2])
 := 8.0

 ;; outside
 (bf [3 3])
 := Double/MAX_VALUE

 ;; near the barrier
 (f [3 1.999])
 := 12.996001

 ;; on the barrier
 (< 19 (bf [3 1.999]) 20)
 := true)

;; Level 6: Progressive Penalty.
;; Implement a progressive-penalty function that starts with a small ρ and doubles it every k iterations. Compare its convergence behavior to the fixed-penalty method.

;; TODO

;; Level 7: Phase I / Phase II.
;; The log-barrier method requires a feasible starting point. Write a “Phase I” function that uses the penalty method to find a feasible point, then passes it to the barrier method for refinement.

;; TODO

;; Level 8: Scalarization.
;; Implement the scalarizeHOF from Section 6.12. Use it to combine f1(x) = x2 and f2(x) = (x−2)2 with weights 0.5 and 0.5.

;; Listing 6.8
(defn scalarize
 "Combine multiple objectives into a weighted sum."
  [objectives weights]
  (fn [x]
    (reduce + (map (fn [f w] (* w (f x)))
                   objectives
                   weights))))

(def fscale
  (scalarize [(fn [x] (Math/pow x 2))
              (fn [x] (Math/pow (- x 2) 2))]
             [0.5
              0.5]))

(rcf/tests
 (fscale 5)
 := 17.0)

;; Level 9: Pareto Front Tracer.
;; Write a function that takes two objectives and returns a list of Pareto-optimal points by running scalarize with 20 different weight vectors evenly spaced between [1, 0] and [0, 1].

(comment
  (last (chp5/adam (fn [[x]] (Math/pow x 2)) [0.5] {:max-iter 10000})) ;; 0
  (last (chp5/adam (fn [[x]] (Math/pow (- x 2) 2)) [0.5] {:max-iter 10000})) ;; 2

  ;; expect to get a linear mix of the optimal answers for each function, ie. [0 .. 2]
  (for [i (range 0 1 1/20)]
    (let [j (- 1.0 i)
          f (scalarize [(fn [[x]] (Math/pow x 2))
                        (fn [[x]] (Math/pow (- x 2) 2))]
                       [i j])]
      (last (chp5/adam f [0.0] {:max-iter 10000})))))

;; Level 10: Generic Nested Solver.
;; The Augmented Lagrangian and the log-barrier method have the same nested-iteration structure but different transformers. Write a generic nested-constrained-solver that takes a transformer function as an argument, so that the same outer loop can be used with either transformer.

;; TODO

;; Level 11: The Augmented Lagrangian.
;; Implement the augmented-lagrangian function from Section 6.7. Use it to minimize f (x, y) = x2 + y2 subject to x + y= 2. What are the final values of the Lagrange multipliers?

;; TODO

;; Level 12: Equality Constraints.
;; The log-barrier method only works for inequality constraints (gi(x) ≤0). How would you modify the barrier method to handle equality constraints (hj(x) = 0)? Can you combine a penalty method for equalities with a barrier method for inequalities?

;; TODO
