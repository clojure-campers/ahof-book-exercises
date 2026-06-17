(ns ahof.chapter5
  (:require
   [hyperfiddle.rcf :as rcf]
   [ahof.chapter5-util :as util5]))

;; Level 1: The Gradient.
;; Implement the gradient function from Section 5.1. Test it on f (x, y) = x2 + y2 at the point [3.0 4.0]. The gradient should be approximately [6.0 8.0].

(defn gradient
  "Function transformer: (R^n -> R) -> (R^n -> R^n).
  Returns the gradient of f, computed numerically.
  "
  ([f] (gradient f 1e-5))
  ([f h]
   (fn [x]
     (->> (range (count x))
          (mapv (fn [i]
                  (let [x+ (update x i + h)
                        x- (update x i - h)]
                    (/ (- (f x+)
                          (f x-))
                       (* 2.0 h)))))))))

#_((gradient (fn [[x y]]
               (+ (* x x)
                  (* y y))))
            [3.0 4.0])

;; Level 2: Rosenbrock.
;; Implement the rosenbrock function. Compute its gradient at [0.0 0.0] and [1.0 1.0]. Verify that the gradient at the minimum [1.0 1.0] is approximately [0.0 0.0].

(defn rosenbrock [[x y]]
  (+ (Math/pow (- 1.0 x) 2)
     (* 100.0 (Math/pow (- y (* x x)) 2))))

(rcf/tests
 (rosenbrock [1.0 1.0]) := 0.0 ;; (the minimum)
 (rosenbrock [0.0 0.0]) := 1.0
 (rosenbrock [5.0 5.0]) := 40016.0

 ((gradient rosenbrock) [0.0 0.0]) ;; [-2.0 0.0]
 ((gradient rosenbrock) [1.0 1.0])) ;; [0.0 0.0]

;; Level 3: Gradient Descent.
;; Implement gradient descent using the descent framework. Run it on Rosenbrock from [5.0 5.0] with learning rate 0.001. How many iterations does it take to reach f (x) < 10−4?

(defn descent
  [{:keys [direction-fn step-fn f x0
           grad-fn tol max-iter]
    :or {tol 1e-100
         max-iter 1000}}]
  (let [grad (or grad-fn (gradient f))
        init-val (f x0)
        init-grad (grad x0)
        step
        (fn [state]
          (let [dir (direction-fn state)
                alpha (step-fn state dir)
                x-new (mapv +
                            (:x state)
                            (mapv #(* alpha %) dir))
                v-new (f x-new)
                g-new (grad x-new)]
            (assoc state
                   :x x-new
                   :value v-new
                   :grad g-new
                   :prev-value (:value state)
                   :prev-x (:x state)
                   :iterations (inc (:iterations state)))))]
    (->> {:x x0
          :value init-val
          :grad init-grad
          :prev-value Double/MAX_VALUE
          :iterations 0}
         (iterate step)
         (take-while
          (fn [s]
            (and (< (:iterations s) max-iter)
                 (> (Math/abs (- (:value s)
                                 (:prev-value s)))
                    tol)))))))

(defn gradient-descent
  [{:keys [f x0 lr max-iter]}]
  (descent
   {:f f
    :x0 x0
    :max-iter max-iter
    :direction-fn (fn [s] (mapv - (:grad s)))
    :step-fn (fn [_ _] lr)}))

#_(gradient-descent {:f rosenbrock
                     :x0 [5.0 5.0]
                     :lr 0.001
                     :max-iter 5000})
;; 4 iter

;; Level 4: Learning Rate Sensitivity. Run gradient descent on Rosenbrock with learning rates 10−1, 10−2, 10−3, 10−4, 10−5. Which diverges? Which converges fastest?

#_(gradient-descent rosenbrock [5.0 5.0] 0.1) ;; 3 iter, [0 0] X
#_(gradient-descent rosenbrock [5.0 5.0] 0.01) ;; 4 iter, [0 300000] X
#_(gradient-descent rosenbrock [5.0 5.0] 0.001) ;; 4 iter, [0 0] X
#_(gradient-descent rosenbrock [5.0 5.0] 0.0001) ;; 5000 iter, [2 4] X

;; ADAM

(defn adam
  [f x0 & {:keys [lr beta1 beta2 eps max-iter tol]
           :or {lr 0.001 beta1 0.9
                max-iter 1000
                beta2 0.999 eps 1e-8 tol 1e-8}}]
  (let [grad-f (gradient f)]
    (->> {:x x0 :value (f x0) :grad (grad-f x0)
          :m (vec (repeat (count x0) 0.0))
          :v (vec (repeat (count x0) 0.0))
          :t 1}
         (iterate
          (fn [{:keys [x grad m v t] :as state}]
            (let [;; Update biased first moment
                  m-new (mapv (fn [mi gi]
                                (+ (* beta1 mi)
                                   (* (- 1 beta1) gi)))
                              m grad)
                  ;; Update biased second moment
                  v-new (mapv (fn [vi gi]
                                (+ (* beta2 vi)
                                   (* (- 1 beta2) gi gi)))
                              v grad)
                  ;; Bias correction
                  m-hat (mapv #(/ % (- 1 (Math/pow beta1 t)))
                              m-new)
                  v-hat (mapv #(/ % (- 1 (Math/pow beta2 t)))
                              v-new)
                  ;; Update parameters
                  x-new (mapv (fn [xi mi vi]
                                (- xi (/ (* lr mi)
                                         (+ (Math/sqrt vi) eps)
                                         )))
                              x m-hat v-hat)
                  v-val (f x-new)]
              (assoc state
                     :x x-new :value v-val :grad (grad-f x-new)
                     :m m-new :v v-new :t (inc t)
                     :prev-value (:value state)))))
         (take-while #(and (< (:t %) max-iter)
                           (> (Math/abs (- (:value %)
                                           (or (:prev-value %)
                                               ##Inf)))
                              tol))))))

#_(adam rosenbrock [5.0 5.0] {:max-iter 100})

;; ex
[{:x [4.902244295647663 5.098085865906503], :value 35864.53467570703, :grad [37135.27181316749 -3786.782653696718], :m [37415.43258144515 -3807.756135579503], :v [1.4016920483182794E8 1428689.1268685556], :t 100, :prev-value 35904.29387510085} ,,,]


(comment
  (->> {:f rosenbrock
        :x0 [5.0 5.0]
        :lr 0.0001
        :max-iter 25000}
       gradient-descent
       (util5/plot rosenbrock)
       (spit "plot.svg"))

  (->> {:max-iter 25000}
       (adam rosenbrock [5.0 5.0])
       (util5/plot rosenbrock)
       (spit "plot.svg"))

  (->> {:max-iter 5000
        :lr 1.5
        :beta1 0.9
        :beta2 0.999
        :eps 1e-8
        :tol 1e-8}
       (adam rosenbrock
             [(- (rand 20.0) 10.0)
              (- (rand 20.0) 10.0)])
       (util5/plot rosenbrock)
       (spit "plot.svg")))





