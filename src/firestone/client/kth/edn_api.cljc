(ns firestone.client.kth.edn-api
  (:require [firestone.construct :refer [create-game]]
            [firestone.core :refer [attack play-card]]
            [firestone.core-api :as engine-api]
            [firestone.construct :as construct]
            [firestone.definitions-loader]
            [firestone.client.kth.mapper :as mapper]
            [ysera.error :refer [error]]))

;; Global state storage: a map keyed by game-id
(def game-states (atom {}))

;; State management helper functions
(defn add-new-game [initial-state game-id]
  (swap! game-states assoc game-id initial-state)
  initial-state)

(defn get-game-state [game-id]
  (@game-states game-id))

(defn update-game-state [game-id updated-state]
  (swap! game-states assoc game-id updated-state))

;; This function stays the same
(defn map-game-input
  "Given a game state and a game-body vector (one entry per player), updates the state by
   processing the keys :board, :mana, and :max-mana.
   Expects game-body to be a vector whose entries will be paired with player ids in order,
   e.g. player \"p1\", then \"p2\"."
  [state game-body]
  (reduce
    (fn [acc [data player-id]]
      (let [board   (get data :board)
            mana    (get data :mana)
            max-mana (get data :max-mana)]
        (-> acc
            ;; If there is a :board key, add those minions to this player's board.
            (cond-> board (construct/add-minions-to-board player-id board))
            ;; If a max-mana is provided then update hero max mana.
            (cond-> max-mana (construct/set-max-mana player-id max-mana))
            ;; If a mana value is provided then update hero current mana.
            (cond-> mana (construct/update-hero-mana player-id mana)))))
    state
    (map vector game-body ["p1" "p2"])))

;; Create game function: creates a new game, maps its input, and stores it keyed by game-id.
(defn create-game!
  [players-data]
  (let [state (create-game players-data)
        state' (map-game-input state players-data)
        game-id "the-game-id"]
    (add-new-game state' game-id)
    [(mapper/game->client-game state')]))


;; Modified end-turn function: it retrieves, updates, and writes the state for a given game id.
(defn end-turn!
  [player-id]
  (let [game-id "the-game-id"  ; Replace with dynamic game-id in production
        current-game-state (get-game-state game-id)]
    (if current-game-state
      (let [updated-game-state (engine-api/end-turn current-game-state player-id)
            new-player-in-turn (get updated-game-state :player-id-in-turn)]
        (update-game-state game-id updated-game-state)
        (println "Turn ended. New player in turn:" new-player-in-turn)
        [(mapper/game->client-game updated-game-state)])
      (error "No game found for game-id" game-id))))

;; Modified play-minion-card function: uses the keyed state.
(defn play-minion-card!
  [player-id card-id position]
  (let [game-id "the-game-id"          ; Replace with dynamic game-id if needed
        current-game-state (get-game-state game-id)]
    (if current-game-state
      (let [updated-game-state (play-card current-game-state player-id card-id position)]
        (update-game-state game-id updated-game-state)
        [(mapper/game->client-game updated-game-state)])
      (error "No game found for game-id" game-id))))

;; Modified attack function: uses the keyed state.
(defn attack!
  [player-id attacker-id target-id]
  (let [game-id "the-game-id"          ; Replace with dynamic game-id if needed
        current-game-state (get-game-state game-id)]
    (if current-game-state
      (let [updated-game-state (attack current-game-state player-id attacker-id target-id)]
        (update-game-state game-id updated-game-state)
        [(mapper/game->client-game updated-game-state)])
      (error "No game found for game-id" game-id))))
