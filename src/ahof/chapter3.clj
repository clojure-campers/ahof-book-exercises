

;;

{:x 0
 :y 0
 :heading :north}

(defn turn-right [s]
  (update s :heading {:north :east
                      :east :south
                      :south :west
                      :west :north}))

(defn move-forward [s]
  (case (:heading s)
    :north
    (update s :x inc)
    :east
    (update s :y inc)
    :south
    (update s :x dec)
    :west
    (update s :y dec)))

(defn with-logging [f]
  (fn [s]
    (println s)
    (f s)))

(defn with-history [f]
  (fn [s]
    (f (update s :history (fnil conj []) s))))

(def move-in-circle (with-history
                      (comp turn-right
                            move-forward
                            turn-right
                            move-forward
                            turn-right
                            move-forward
                            turn-right
                            move-forward)))


