(ns ahof.chapter5-util
  (:require
   [hiccup.core :as hiccup]))
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
