(ns ahof.chapter6
  (:require
   [hyperfiddle.rcf :as rcf]))

;; Level 1: The Violation Function.
;; Write a constraint function c1 that enforces x + y ≤ 5. It should take a vector [x y] and return the violation amount (0 if satisfied, positive if violated).
(defn c1 [[x y]]
  (- (+ x y) 5))

(rcf/tests
 (c1 [3 2]) := 0
 (c1 [0 0]) := 0

 (c1 [10 0]) := 10)
