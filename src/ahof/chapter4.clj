
;; Level 1: The Initial Population. Write a function that generates a random initial population of 100 individuals. Each individual should be a vector of 5 random numbers between -10.0 and 10.0.

(defn population
  [n]
  (take n (repeatedly (fn []
                        (mapv (fn [_] (- (rand 20) 10.0))
                              (range 5))))))

#_(population 100)

;; Level 2: Evaluation. Implement the evaluate-population function from Section 4.3.1. Test it on your initial population using the sphere function f(x) = ∑ xi2.

(defn evaluate-population
  [fitness-fn population]
  (map (fn [individual]
         {:chromosome individual
          :fitness (fitness-fn individual)})
       population))

(defn sphere-fitness
  [individual]
  (reduce + (map (fn [x] (Math/pow x 2)) individual)))

#_(->> (population 100)
       (evaluate-population sphere-fitness))

;; Level 3: Tournament Selection as a Strategy. Implement tournament-select from Section 4.3.2. Then implement roulette-select (probability proportional to fitness). Plug both into the same population-step call and verify that only the selection function changes—the pipeline structure is identical. This is the HOF insight: the strategy is a pluggable argument.

(defn tournament-select
  [k pop-size evaluated-pop]
  (repeatedly pop-size
              (fn []
                (let [contenders (take k (shuffle evaluated-pop))]
                  (:chromosome
                   (apply max-key :fitness contenders))))))

#_(->> (population 100)
       (evaluate-population sphere-fitness)
       (tournament-select 5 50))

(defn population-step
 [evaluate-fn select-fn recombine-fn mutate-fn]
 (fn [population]
   (->> population
        evaluate-fn
        select-fn
        recombine-fn
        mutate-fn)))

#_(->> (population 50)
       ((population-step
         (partial evaluate-population sphere-fitness)
         (partial tournament-select 5 50)
         identity
         identity)))

(defn roulette-select
  [pop-size evaluated-pop]
  (let [total-fitness (reduce + (map :fitness evaluated-pop))]
    (repeatedly pop-size
                (fn []
                  (let [r (* (rand) total-fitness)]
                    (->> evaluated-pop
                         (reduce (fn [memo ind]
                                   (if (< r (+ memo (:fitness ind)))
                                     (reduced (:chromosome ind))
                                     (+ memo (:fitness ind)))) 0)))))))
;; TODO ^ need to add up each

;; Level 4: Crossover as a Strategy. Implement single-point crossover from Section 4.3.3. Then implement uniform-crossover (each gene independently chosen from either parent). Show that both plug into the same pipeline slot. The pipeline does not know which crossover you are using.

(defn crossover
  [crossover-rate selected-pop]
  (->> selected-pop
       (partition 2)
       (map (fn [[p1 p2]]
         (if (< (rand) crossover-rate)
           (let [split (rand-int (count p1))]
             (vec (concat (take split p1)
                          (drop split p2))))
           p1)))))

(defn uniform-crossover
  []
  ;; TODO
  )

#_(->> (population 50)
       ((population-step
         (partial evaluate-population sphere-fitness)
         (partial tournament-select 5 50)
         (partial crossover 0.5)
         identity)))

;; Level 5: Mutation as a Strategy. Implement gaussian-mutate (add Gaussian noise) and uniform-mutate (replace gene with random value). Plug both into the same pipeline. Run the GA with each and compare results.

(defn gaussian-mutate
  []
  ;; TODO
  )

(defn uniform-mutate
  [mutation-rate offspring-pop]
  (->> offspring-pop
       (map (fn [ind]
              (mapv (fn [gene]
                      (if (< (rand) mutation-rate)
                        (rand)
                        gene))
                    ind)))))


#_(->> (population 50)
       ((population-step
         (partial evaluate-population sphere-fitness)
         (partial tournament-select 5 50)
         (partial crossover 0.5)
         (partial uniform-mutate 0.1))))

;; Level 6: The Full GA. Assemble the pipeline using population-step and iterate. Run it for 50 generations and find the best individual.

(defn best [fitness-fn population]
  (apply max-key fitness-fn population))

#_(let [n 100
        step-fn (population-step
                 (partial evaluate-population sphere-fitness)
                 #_(partial roulette-select n)
                 (partial tournament-select 5 n)
                 (partial crossover 0.5)
                 (partial uniform-mutate 0.1))]
    (->> (population n)
         (iterate step-fn)
         (take 50)
         last
         (best sphere-fitness)
         sphere-fitness))
