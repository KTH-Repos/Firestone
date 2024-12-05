(ns firestone.core-api
  "The public api of the game."
  (:require [ysera.test :refer [is= error?]]
            [ysera.error :refer [error]]
            [firestone.core :refer  [draw-card]]
            [firestone.construct :refer [create-game
                                         get-player-id-in-turn
                                         should-take-fatigue?
                                         handle-fatigue
                                         reset-player-mana
                                         get-mana
                                         get-max-mana
                                         set-player-mana
                                         set-player-max-mana]]))


(defn end-turn
  {:test (fn []
           (is= (-> (create-game)
                    (end-turn "p1")
                    (get-player-id-in-turn))
                "p2")
           (is= (-> (create-game)
                    (end-turn "p1")
                    (end-turn "p2")
                    (get-player-id-in-turn))
                "p1")
           (error? (-> (create-game)
                       (end-turn "p2")))
           ; Test mana reset when max mana is less than 10
           (let [state (-> (create-game)
                           (set-player-max-mana "p2" 5)
                           (set-player-mana "p2" 3)
                           (end-turn "p1"))]
             (is= (get-max-mana state "p2") 6)
             (is= (get-mana state "p2") 6))
           ;; Test mana reset when max mana is already 10
           (let [state (-> (create-game)
                           (set-player-max-mana "p2" 10)
                           (set-player-mana "p2" 5)
                           (end-turn "p1"))]
             (is= (get-max-mana state "p2") 10)
             (is= (get-mana state "p2") 10)))}
  [state player-id]
  (when-not (= (get-player-id-in-turn state) player-id)
    (error "The player with id " player-id " is not in turn."))
  (let [player-change-fn {"p1" "p2"
                          "p2" "p1"}
        next-player-id (player-change-fn player-id)]
    (-> state
        (update :player-id-in-turn player-change-fn)
        (reset-player-mana next-player-id)
        (as-> s
              (if (should-take-fatigue? s next-player-id)
                (handle-fatigue s next-player-id)
                (draw-card s next-player-id))))))