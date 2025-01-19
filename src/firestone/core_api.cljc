(ns firestone.core-api
  "The public api of the game."
  (:require [ysera.test :refer [is= error?]]
            [ysera.error :refer [error]]
            [firestone.core :refer [draw-card
                                    get-valid-attacks]]
            [firestone.construct :refer [create-game
                                         update-hero
                                         get-player-id-in-turn
                                         reset-player-mana
                                         should-take-fatigue?
                                         handle-fatigue
                                         clear-sleepy-status-from-minions
                                         reset-can-attack
                                         remove-can-attack
                                         reset-attacks-performed-this-turn
                                         get-hero-player-id
                                         get-max-mana]]))


(defn end-turn
  [state player-id]
  ;; 1) Verify that the correct player is in turn
  (when-not (= (get-player-id-in-turn state) player-id)
    (error "The player with id " player-id " is not in turn."))

  (println "State before end-turn:" state)

  (let [player-change-fn {"p1" "p2"
                          "p2" "p1"}
        next-player-id   (player-change-fn player-id)

        ;; 2) First, apply the sequence of transformations via ->
        ;;    (excluding the if, since if is a special form).
        updated-state
        (-> state
            (clear-sleepy-status-from-minions)
            (reset-can-attack next-player-id)
            (remove-can-attack player-id)
            (reset-attacks-performed-this-turn next-player-id)
            ;; Note: Update the hero for the *next* player rather than the current one.
            (update-hero (:id (get-hero-player-id state next-player-id)) :has-used-your-turn false)
            (assoc :minion-ids-summoned-this-turn [])
            (assoc :player-id-in-turn next-player-id)
            (reset-player-mana next-player-id)
            (get-valid-attacks))

        ;; 3) Now handle the branching after we've gotten updated-state
        final-state
        (if (should-take-fatigue? updated-state next-player-id)
          (handle-fatigue updated-state next-player-id)
          (draw-card updated-state next-player-id))

        ;; 4) Update hero's max mana (or whatever else comes after)
        updated-max-mana (get-max-mana final-state next-player-id)

        ;; 5) Apply the final hero-mana update
        final-state      (update-hero final-state next-player-id :mana updated-max-mana)]

    (println "State after end-turn:" final-state)
    final-state))






