

;; Level 1: The Gradient. Implement the gradient function from Section 5.1. Test it on f (x, y) = x2 + y2 at the point [3.0 4.0]. The gradient should be approximately [6.0 8.0].

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

;; Level 2: Rosenbrock. Implement the rosenbrock function. Compute its gradient at [0.0 0.0] and [1.0 1.0]. Verify that the gradient at the minimum [1.0 1.0] is approximately [0.0 0.0].

(defn rosenbrock [[x y]]
  (+ (Math/pow (- 1.0 x) 2)
     (* 100.0 (Math/pow (- y (* x x)) 2))))

#_(rosenbrock [1.0 1.0]) ;; => 0.0 (the minimum)
#_(rosenbrock [0.0 0.0]) ;; => 1.0
#_(rosenbrock [5.0 5.0]) ;; => 40016.0

#_((gradient rosenbrock) [0.0 0.0]) ;; [-2.0 0.0]
#_((gradient rosenbrock) [1.0 1.0]) ;; [0.0 0.0]

;; Level 3: Gradient Descent. Implement gradient descent using the descent framework. Run it on Rosenbrock from [5.0 5.0] with learning rate 0.001. How many iterations does it take to reach f (x) < 10−4?

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

(require '[hiccup.core :as hiccup])

(defn plot [f vals]
  (let [xs    (mapv (fn [s] (first (:x s))) vals)
        ys    (mapv (fn [s] (second (:x s))) vals)
        pad   0.5
        x-min (- (apply min (conj xs 1)) pad)
        x-max (+ (apply max (conj xs 1)) pad)
        y-min (- (apply min (conj ys 1)) pad)
        y-max (+ (apply max (conj ys 1)) pad)
        w     600
        h     600
        n     80
        dx    (/ (- x-max x-min) n)
        dy    (/ (- y-max y-min) n)
        grid  (for [j (range n)
                    i (range n)]
                (f [(+ x-min (* i dx))
                    (+ y-min (* j dy))]))
        log1p (fn [v] (Math/log (+ 1.0 (max 0.0 v))))
        lv-lo (log1p (apply min grid))
        lv-hi (log1p (apply max grid))
        v->t  (fn [v]
                (if (= lv-hi lv-lo)
                  0.5
                  (/ (- (log1p v) lv-lo) (- lv-hi lv-lo))))
        t->c  (fn [t] (str "hsl(" (int (* (- 1.0 t) 240)) ",90%,45%)"))
        cw    (/ w n)
        ch    (/ h n)
        sx    (fn [x] (* w (/ (- x x-min) (- x-max x-min))))
        sy    (fn [y] (* h (- 1.0 (/ (- y y-min) (- y-max y-min)))))
        rects (map-indexed
               (fn [idx v]
                 [:rect {:x      (* (mod idx n) cw)
                         :y      (* (- n 1 (quot idx n)) ch)
                         :width  (+ cw 1)
                         :height (+ ch 1)
                         :fill   (t->c (v->t v))}])
               grid)
        pts   (->> vals
                   (map (fn [s]
                          (str (sx (first (:x s))) "," (sy (second (:x s))))))
                   (clojure.string/join " "))
        s0    (first vals)
        sN    (last vals)]
    (hiccup/html
     [:svg {:xmlns  "http://www.w3.org/2000/svg"
            :width  w
            :height h}
      [:g rects]
      [:polyline {:points          pts
                  :fill            "none"
                  :stroke          "white"
                  :stroke-width    2
                  :stroke-linecap  "round"
                  :stroke-linejoin "round"}]
      [:g
       (let [n (count vals)]
         (map-indexed
          (fn [i s]
            [:circle {:cx             (sx (first (:x s)))
                      :cy             (sy (second (:x s)))
                      :r              3
                      :fill           "none"
                      :stroke         "white"
                      :stroke-width   1}])
          vals))]

      [:circle {:cx   (sx (first (:x s0)))
                :cy   (sy (second (:x s0)))
                :data-x (str (:x s0))
                :r    5
                :fill "lime"}]
      [:circle {:cx   (sx (first (:x sN)))
                :cy   (sy (second (:x sN)))
                :data-x (str (:x sN))
                :r    5
                :fill "red"}]

      [:circle {:cx   (sx 1)
                :cy   (sy 1)
                :data-x (str [1 1])
                :r    5
                :fill "pink"}]
      ])))


#_(spit "plot.svg" (plot rosenbrock (gradient-descent {:f rosenbrock
                                                       :x0 [5.0 5.0]
                                                       :lr 0.0001
                                                       :max-iter 25000})))

#_(spit "plot.svg" (plot rosenbrock (adam rosenbrock [5.0 5.0]
                                          {:max-iter 25000})))
#_(spit "plot.svg" (plot rosenbrock (adam rosenbrock
                                          [(- (rand 20.0) 10.0)
                                           (- (rand 20.0) 10.0)] #_[5.0 5.0]
                                          {:max-iter 5000
                                           :lr 1.5
                                           :beta1 0.9
                                           :beta2 0.999
                                           :eps 1e-8
                                           :tol 1e-8})))


