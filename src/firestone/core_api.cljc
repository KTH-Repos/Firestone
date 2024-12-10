(ns firestone.core-api
  "The public api of the game."
  (:require [ysera.test :refer [is= error?]]
            [ysera.error :refer [error]]
            [firestone.core :refer [draw-card]]
            [firestone.construct :refer [create-game
                                         get-player-id-in-turn
                                         reset-player-mana
                                         should-take-fatigue?
                                         handle-fatigue]]))


(defn end-turn
  [state player-id]
  (when-not (= (get-player-id-in-turn state) player-id)
    (error "The player with id " player-id " is not in turn."))

  (println "State before end-turn:" state)

  (let [player-change-fn {"p1" "p2"
                          "p2" "p1"}
        next-player-id (player-change-fn player-id)
        state-after-switch (-> state
                               (assoc :player-id-in-turn next-player-id)
                               (reset-player-mana next-player-id))
        _ (println "This is after player switch and mana-reset:" state-after-switch)
        state-after-draw (if (should-take-fatigue? state-after-switch next-player-id)
                           (handle-fatigue state-after-switch next-player-id)
                           (draw-card state-after-switch next-player-id))
        ;; Reset hero-power-used flag for the next player
        final-state (assoc-in state-after-draw [:players next-player-id :hero :hero-power-used] false)]
    (println "State after end-turn:" final-state)
    final-state))