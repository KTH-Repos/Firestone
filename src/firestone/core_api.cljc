(ns firestone.core-api
  "The public api of the game."
  (:require [ysera.test :refer [is= error?]]
            [ysera.error :refer [error]]
            [firestone.core :refer [draw-card
                                    get-valid-attacks]]
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
        next-player-id (player-change-fn player-id)]
    (as-> state $
          (assoc $ :player-id-in-turn next-player-id)
          (do (println "This is after player switch:" $) $)
          (reset-player-mana $ next-player-id)
          (do (println "This is after mana reset:" $) $)
          (get-valid-attacks $)
          (if (should-take-fatigue? $ next-player-id)
            (handle-fatigue $ next-player-id)
            (draw-card $ next-player-id))
          (assoc-in $ [:players next-player-id :hero :hero-power-used] false)
          (do (println "State after end-turn:" $) $))))
