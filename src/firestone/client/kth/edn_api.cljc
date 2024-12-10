(ns firestone.client.kth.edn-api
  (:require [firestone.construct :refer [create-game]]
            [firestone.core :refer  [attack
                                     play-card]]
            [firestone.core-api :as engine-api]
            [firestone.definitions-loader]
            [firestone.client.kth.mapper :as mapper]))

(def state-atom (atom nil))

(defn create-game!
  [players-data]
  (let [state (create-game players-data)]
    [(mapper/game->client-game (reset! state-atom state))]))

(defn end-turn!
  [player-id]
  (let [state (swap! state-atom engine-api/end-turn player-id)]
    [(mapper/game->client-game state)]))

(defn play-minion-card!
  [player-id card-id position]
  (let [state (swap! state-atom play-card player-id card-id position)]
    [(mapper/game->client-game state)]))

(defn attack!
  [player-id attacker-id target-id]
  (let [state (swap! state-atom attack player-id attacker-id target-id)]
    [(mapper/game->client-game state)]))