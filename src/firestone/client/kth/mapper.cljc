(ns firestone.client.kth.mapper
  (:require [clojure.spec.alpha :as s]
            [firestone.construct :as construct]
            [ysera.test :refer [is]]
            [firestone.client.kth.spec]))


(defn check-spec
  [spec value]
  (or (s/valid? spec value)
      (s/explain spec value)))

(defn hero->client-hero
  {:test (fn []
           (let [game (construct/create-game)]
             (is (check-spec :firestone.client.kth.spec/hero
                             (hero->client-hero game
                                                (construct/get-hero game "p1")
                                                "p1")))))}
  [game hero player-id]
  {:armor            0
   :owner-id         player-id
   :entity-type      :hero
   :attack           3
   :can-attack       true
   :health           30
   :id               (:id hero)
   :mana             10
   :max-health       30
   :max-mana         10
   :name             (:name hero)
   :states           []
   :valid-attack-ids []})

(defn player->client-player
  {:test (fn []
           (let [game (construct/create-game)]
             (is (check-spec :firestone.client.kth.spec/player
                             (player->client-player game
                                                    (construct/get-player game "p1"))))))}
  [game player]
  (let [player-id (:id player)]
    {:board-entities []
     :active-secrets []
     :deck-size      (count (construct/get-deck game player-id))
     :hand           (construct/get-hand game player-id)
     :hero           (hero->client-hero game
                                        (construct/get-hero game player-id)
                                        player-id)
     :id             player-id}))

(defn game->client-game
  [game]
  {:action-index   0
   :id             "the-game-id"
   :player-in-turn (construct/get-player-id-in-turn game)
   :players        (->> ["p1" "p2"]
                        (map (fn [player-id]
                               (player->client-player game
                                                      (construct/get-player game player-id)))))})

(defn client-input->internal-game
  "Transforms the client-provided game input into the format required by create-game."
  [client-input]
  (-> client-input
      :game ; Extract the :game key
      (map (fn [player]
             (-> player
                 (update :hand #(mapv str %)) ; Ensure :hand contains strings in a vector
                 (update :deck #(mapv str %)) ; Ensure :deck contains strings in a vector
                 (update :board #(mapv str %)) ; Ensure :board contains strings in a vector
                 (update :hero #(or % "Jaina Proudmoore"))))))) ; Provide default hero if not specified