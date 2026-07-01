(ns ahof.chapter7
  (:require
   [hyperfiddle.rcf :as rcf]))

;; Level 1: The Grid States.
;; Write a function that generates the states for a 4×4 grid world as a sequence of [row col] vectors.

;; From Listing 7.2:
(def grid-states
  (for [r (range 4) c (range 4)] [r c]))

;; Reformatted:
(def grid-states
  (for [r (range 4)
        c (range 4)]
    [r c]))

;; Level 2: The Transition Function.
;; Implement the deterministic grid-transition function from Section 7.2.2. Test it manually: what happens when you move :up from [0 0]?

;; From Listing 7.2:

(defn grid-transition [s a]
  (let [[r c] s
        [r' c'] (case a
                  :up [(max 0 (dec r)) c]
                  :down [(min 3 (inc r)) c]
                  :left [r (max 0 (dec c))]
                  :right [r (min 3 (inc c))])]
    [[[r' c'] 1.0]]))

(rcf/tests
 (grid-transition [0 0] :up)
 :=
 [[[0 0] 1.0]]

 (grid-transition [0 0] :down)
 :=
 [[[1 0] 1.0]])

;; Level 3: The Reward Function.
;; Implement the grid-reward function. It should return 0 at the goal [3 3] and -1 everywhere else.

;; From Listing 7.2:
(defn grid-reward [s a]
  (if (= s [3 3]) 0.0 -1.0))

;; Level 4: The MDP Map.
;; Combine your states, actions, transition, and reward functions into an MDP map using make-mdp with γ= 0.9.

;; From Listing 7.1:

(defn make-mdp [states actions transition reward gamma]
  {:states states ; collection of states
   :actions actions ; (fn [s] -> coll of actions)
   :transition transition ; (fn [s a] -> [[s' prob] ...])
   :reward reward ; (fn [s a] -> double)
   :gamma gamma}) ; double in [0, 1)

;; From Listing 7.3:

(defn grid-actions [s] [:up :down :left :right])

(def grid-mdp
  (make-mdp grid-states
            grid-actions
            grid-transition
            grid-reward
            0.9))

;; Level 5: One-Step Lookahead.
;; Implement the lookahead function. Create a dummy value function that returns 0 for all states. Calculate the Q-value of moving :right from [3 2].

;; From Listing 7.3:

(defn lookahead [mdp V s a]
  (let [{:keys [transition reward gamma]} mdp]
    (+ (reward s a)
       (* gamma
          (reduce + (map (fn [[s' p]] (* p (V s')))
                         (transition s a)))))))

(def dummy-value (constantly 0))

(rcf/tests
 (lookahead grid-mdp dummy-value [3 2] :right)
 :=
 -1.0)


;; Level 6: The Bellman Operator.
;; Implement the bellman-operator HOF. Apply it to your dummy value function. Evaluate the resulting function at state [3 2].

;; From Listing 7.4:
(defn bellman-operator [mdp]
  (fn [V]
    (fn [s]
      (apply max
             (map (fn [a] (lookahead mdp V s a))
                  ((:actions mdp) s))))))

(rcf/tests
 (((bellman-operator grid-mdp) dummy-value) [3 2])
 :=
 -1.0)

;; Level 7: Value Iteration.
;; Implement the value-iteration function. Run it on your grid world MDP. How many iterations does it take to converge?

;; From Listing 7.3:
(defn greedy-policy [mdp V]
  (fn [s]
    (apply max-key
           (fn [a] (lookahead mdp V s a))
           ((:actions mdp) s))))

;; From Listing 7.5:
(defn value-iteration [mdp & {:keys [tol max-iter]
                              :or {tol 1e-6 max-iter 1000}}]
  (let [{:keys [states]} mdp
        states-vec (vec states)
        bellman (bellman-operator mdp)
        init-V (zipmap states-vec (repeat 0.0))
        update-values
        (fn [V]
          (let [V-fn (fn [s] (get V s 0.0))
                V-next-fn (bellman V-fn)]
            (zipmap states-vec (map V-next-fn states-vec))))
        max-delta
        (fn [V-old V-new]
          (apply max
                 (map (fn [s]
                        (Math/abs (- (get V-new s 0.0)
                                     (get V-old s 0.0))))
                      states-vec)))
        result
        (reduce
         (fn [{:keys [V iter]} _]
           (let [V' (update-values V)
                 next-iter (inc iter)]
             (if (< (max-delta V V') tol)
               (reduced {:V V' :iter next-iter :converged true})
               {:V V' :iter next-iter :converged false})))
         {:V init-V :iter 0 :converged false}
         (range max-iter))]
    {:V (:V result)
     :iterations (:iter result)
     :converged (:converged result)
     :policy (greedy-policy mdp
                            (fn [s] (get (:V result) s 0.0)))}))

(rcf/tests
 (:iterations (value-iteration grid-mdp))
 := 7)

;; Level 8: Extracting the Policy.
;; Use the greedy-policy function to extract the optimal policy from the converged value function. Test it: what is the optimal action at [0 0]?

;; value iteration above already returns the policy

(rcf/tests
 ((:policy (value-iteration grid-mdp)) [0 0])
 := :right)

;; Level 9: Policy Evaluation.
;; Write a bellman-operator-for-policy function that constructs the Bellman operator for a fixed policy (no max over actions). Show that Policy Evaluation is just iterate applied to this operator.

(defn bellman-operator-for-policy [mdp policy]
  (fn [V]
    (fn [s]
      (lookahead mdp V s (policy s)))))


;; claude:
(defn policy-evaluation' [mdp policy & {:keys [tol max-iter]
                                        :or {tol 1e-6 max-iter 1000}}]
  (let [states-vec (vec (:states mdp))
        operator   (bellman-operator-for-policy mdp policy)
        step (fn [V]
               (let [V-fn   (fn [s] (get V s 0.0))
                     V-next (operator V-fn)]   ;; V → V'
                 (zipmap states-vec (map V-next states-vec))))]
    (->> (zipmap states-vec (repeat 0.0))
         (iterate step)    ;; <-- iterate
         (partition 2 1)
         (drop-while (fn [[a b]]
                       (>= (apply max (map (fn [s] (Math/abs (- (get b s 0.0)
                                                                (get a s 0.0))))
                                           states-vec))
                           tol)))
         (ffirst))))


(rcf/tests
 (policy-evaluation' grid-mdp (:policy (value-iteration grid-mdp)))
 := (:V (value-iteration grid-mdp)))


;; Level 10: Stochastic Grid World.
;; Modify the grid world to be stochastic: when the agent chooses to move in a direction, there is a 0.8 probability of moving in that direction and a 0.1 probability of moving in each of the two perpendicular directions. Solve the stochastic MDP and compare the optimal policy to the deterministic case.

(defn stochastic-grid-transition [s a]
  (let [[r c] s
        move (fn [[r c] a]
               (case a
                 :up [(max 0 (dec r)) c]
                 :down [(min 3 (inc r)) c]
                 :left [r (max 0 (dec c))]
                 :right [r (min 3 (inc c))]))
        perpendiculars {:up [:left :right]
                        :down [:left :right]
                        :left [:up :down]
                        :right [:up :down]}]
    (into
     [[(move s a) 0.8]]
     (let [[ap1 ap2] (perpendiculars a)]
       [[(move s ap1) 0.1]
        [(move s ap2) 0.1]]))))

(rcf/tests
 (stochastic-grid-transition
  [0 0]
  :right)
 :=
 [[[0 1] 0.8]
  [[0 0] 0.1]
  [[1 0] 0.1]])

(def stochastic-grid-mdp
  (make-mdp grid-states
            grid-actions
            stochastic-grid-transition
            grid-reward
            0.9))

(def deterministic-policy (:policy (value-iteration grid-mdp)))
(def stochastic-policy (:policy (value-iteration stochastic-grid-mdp)))

(defn policy-map [mdp policy]
  (into {} (map (fn [s] [s (policy s)]) (:states mdp))))

(defn print-policy [mdp policy]
  (let [m (policy-map mdp policy)]
    (for [r (range 4)]
      (->> (for [c (range 4)]
             (m [r c]))
           (map {:up "↑" :down "↓" :left "←" :right "→"})
           (apply str)
           println))))

#_(print-policy grid-mdp deterministic-policy)
; →→→↓
; →→→↓
; →→→↓
; →→→→

#_(print-policy stochastic-grid-mdp stochastic-policy)
; →→↓↓
; ↓→↓↓
; →→→↓
; →→→→

;; stochastic tends towards the center
;; because edges have 0.1 chance of 0 progress (hitting the wall)

;; Level 11: Policy Iteration.
;; Implement the policy-evaluation and policy-iteration functions from Section 7.10. Run Policy Iteration on the stochastic grid world. How many iterations does it take compared to Value Iteration?

;; From Listing 7.6:

(defn policy-evaluation [mdp policy
                         & {:keys [tol max-iter]
                            :or {tol 1e-6 max-iter 500}}]
  (let [{:keys [states transition reward gamma]} mdp
        states-vec (vec states)
        init-V (zipmap states-vec (repeat 0.0))
        update-values
        (fn [V]
          (into {}
                (map (fn [s]
                       (let [a (policy s)
                             expected-next
                             (reduce +
                                     (map (fn [[s' p]]
                                            (* p (get V s' 0.0))
                                            )
                                          (transition s a)))]
                         [s (+ (reward s a)
                               (* gamma expected-next))]))
                     states-vec)))
        max-delta
        (fn [V-old V-new]
          (apply max
                 (map (fn [s]
                        (Math/abs (- (get V-new s)
                                     (get V-old s 0.0))))
                      states-vec)))]
    (reduce (fn [V _]
              (let [V' (update-values V)]
                (if (< (max-delta V V') tol)
                  (reduced V')
                  V')))
            init-V
            (range max-iter))))

;; From Listing 7.7:
(defn policy-iteration [mdp & {:keys [max-iter]
                               :or {max-iter 100}}]
  (let [{:keys [states actions]} mdp
        states-vec (vec states)
        init-policy (fn [s] (first (actions s)))
        policy-changed?
        (fn [old-policy new-policy]
          (boolean
           (some (fn [s] (not= (old-policy s) (new-policy s)))
                 states-vec)))
        step-policy
        (fn [{:keys [policy iter]} _]
          (let [V (policy-evaluation mdp policy)
                V-fn (fn [s] (get V s 0.0))
                improved-policy (greedy-policy mdp V-fn)
                changed? (policy-changed? policy improved-policy
                                          )
                next-iter (inc iter)
                state {:V V
                       :policy improved-policy
                       :iterations next-iter
                       :converged (not changed?)}]
            (if changed?
              (assoc state :iter next-iter)
              (reduced state))))]
    (reduce step-policy
            {:V nil :policy init-policy
             :iter 0 :iterations 0 :converged false}
            (range max-iter))))

(rcf/tests
 (:iterations (policy-iteration stochastic-grid-mdp))
 := 5
 (:iterations (value-iteration stochastic-grid-mdp))
 := 113)

;; Level 12: Continuous States.
;; Our MDP implementation assumes discrete, enumerable states. How would you modify the Value Iteration algorithm if the state space was continuous (e.g., a car’s position and velocity)? What mathematical techniques from previous chapters would you need to combine with the Bellman Operator?

;; TODO
