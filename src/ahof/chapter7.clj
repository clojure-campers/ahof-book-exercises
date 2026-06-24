(ns ahof.chapter7)

;; Level 1: The Grid States.
;; Write a function that generates the states for a 4×4 grid world as a sequence of [row col] vectors.

;; TODO

;; Level 2: The Transition Function.
;; Implement the deterministic grid-transition function from Section 7.2.2. Test it manually: what happens when you move :up from [0 0]?

;; TODO

;; Level 3: The Reward Function.
;; Implement the grid-reward function. It should return 0 at the goal [3 3] and -1 everywhere else.

;; TODO

;; Level 4: The MDP Map.
;; Combine your states, actions, transition, and reward functions into an MDP map using make-mdp with γ= 0.9.

;; TODO

;; Level 5: One-Step Lookahead.
;; Implement the lookahead function. Create a dummy value function that returns 0 for all states. Calculate the Q-value of moving :right from [3 2].

;; TODO

;; Level 6: The Bellman Operator.
;; Implement the bellman-operator HOF. Apply it to your dummy value function. Evaluate the resulting function at state [3 2].

;; TODO

;; Level 7: Value Iteration.
;; Implement the value-iteration function. Run it on your grid world MDP. How many iterations does it take to converge?

;; TODO

;; Level 8: Extracting the Policy.
;; Use the greedy-policy function to extract the optimal policy from the converged value function. Test it: what is the optimal action at [0 0]?

;; TODO

;; Level 9: Policy Evaluation.
;; Write a bellman-operator-for-policy function that constructs the Bellman operator for a fixed policy (no max over actions). Show that Policy Evaluation is just iterate applied to this operator.

;; TODO

;; Level 10: Stochastic Grid World.
;; Modify the grid world to be stochastic: when the agent chooses to move in a direction, there is a 0.8 probability of moving in that direction and a 0.1 probability of moving in each of the two perpendicular directions. Solve the stochastic MDP and compare the optimal policy to the deterministic case.

;; TODO

;; Level 11: Policy Iteration.
;; Implement the policy-evaluation and policy-iteration functions from Section 7.10. Run Policy Iteration on the stochastic grid world. How many iterations does it take compared to Value Iteration?

;; TODO

;; Level 12: Continuous States.
;; Our MDP implementation assumes discrete, enumerable states. How would you modify the Value Iteration algorithm if the state space was continuous (e.g., a car’s position and velocity)? What mathematical techniques from previous chapters would you need to combine with the Bellman Operator?

;; TODO
