(ns firestone.client.kth.edn-api
  (:require [firestone.construct :refer [create-game]]
            [firestone.core-api :as engine-api]
            [firestone.definitions-loader]
            [firestone.client.kth.mapper :as mapper]))

(def state-atom (atom nil))

(defn create-game!
  [params]
  (let [mana (:mana params)
        state (create-game)]
    [(mapper/game->client-game (reset! state-atom state))]))

(defn end-turn!
  [player-id]
  (let [state (swap! state-atom engine-api/end-turn player-id)]
    [(mapper/game->client-game state)]))