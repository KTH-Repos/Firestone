(ns firestone.construct
  "A namespace for constructions and basic manipulations of the entities in the state"
  (:require [ysera.test :refer [is is-not is= error?]]
            [firestone.definitions :refer [get-definition]]))


(defn create-hero
  "Creates a hero from its definition by the given hero name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-hero "Jaina Proudmoore")
                {:name         "Jaina Proudmoore"
                 :entity-type  :hero
                 :damage-taken 0
                 :fatigue      0})
           (is= (create-hero "Jaina Proudmoore" :damage-taken 10)
                {:name         "Jaina Proudmoore"
                 :entity-type  :hero
                 :damage-taken 10
                 :fatigue      0}))}
  ; Variadic functions [https://clojure.org/guides/learn/functions#_variadic_functions]
  [name & kvs]
  (let [definition (get-definition name)
        hero {:name         name
              :entity-type  :hero
              :health       (:health definition)
              :hero-power-used false
              :damage-taken 0}]
    (if (empty? kvs)
      hero
      (apply assoc hero kvs))))


(defn create-card
  "Creates a card from its definition by the given card name. The additional key-values will override the default values."
  {:test (fn []
           (is= (-> (create-card "Boulderfist Ogre" :id "bo")
                    (select-keys [:id :entity-type :name :attack :health :mana-cost :type :set]))
                {:id "bo"
                 :entity-type :card
                 :name "Boulderfist Ogre"
                 :attack 6
                 :health 7
                 :mana-cost 6
                 :type :minion
                 :set :basic}))}
  [name & kvs]
  (let [definition (get-definition name)
        card {:name name
              :entity-type :card
              :attack (:attack definition)
              :health (:health definition)
              :mana-cost (:mana-cost definition)
              :original-mana-cost (:mana-cost definition)
              :original-attack (:attack definition)
              :original-health (:health definition)
              :type (:type definition)
              :set (:set definition)
              :rarity (:rarity definition)
              :description (or (:description definition) "")
              :ability (:ability definition)
              :race (:race definition)
              :class (:class definition)
              :deathrattle (:deathrattle definition)
              :battlecry (:battlecry definition)
              :playable false
              :valid-target-ids []}]
    (as-> card $
          (if (:stealth definition)
            (assoc $ :stealth (:stealth definition))
            $)
          (if (empty? kvs)
            $
            (apply assoc $ kvs)))))

(defn create-minion
  "Creates a minion from its definition by the given minion name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-minion "Sheep"
                               :id "m"
                               :attacks-performed-this-turn 1)
                {:attacks-performed-this-turn 1
                 :damage-taken                0
                 :entity-type                 :minion
                 :name                        "Sheep"
                 :id                          "m"
                 :attack                      1
                 :health                      1
                 :mana-cost                   1
                 :type                        :minion
                 :race                        :beast
                 :set                         :basic}))}
  [name & kvs]
  (let [definition (get-definition name)
        minion (merge {:damage-taken                0
                       :entity-type                 :minion
                       :name                        name
                       :attacks-performed-this-turn 0
                       :type                        :minion}
                      (select-keys definition [:attack :health :mana-cost :ability :race :set :rarity :description :deathrattle :battlecry :class]))]
    (if (empty? kvs)
      minion
      (apply assoc minion kvs))))

(defn create-empty-state
  "Creates an empty state with the given heroes."
  {:test (fn []
           ; Jaina Proudmoore will be the default hero
           (is= (create-empty-state [(create-hero "Jaina Proudmoore")
                                     (create-hero "Jaina Proudmoore")])
                (create-empty-state))

           (is= (create-empty-state [(create-hero "Jaina Proudmoore" :id "r")
                                     (create-hero "Gul'dan")])
                {:player-id-in-turn             "p1"
                 :players                       {"p1" {:id      "p1"
                                                       :mana    10
                                                       :max-mana 10
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :hero    {:name         "Jaina Proudmoore"
                                                                 :id           "r"
                                                                 :damage-taken 0
                                                                 :fatigue      0
                                                                 :entity-type  :hero}}
                                                 "p2" {:id      "p2"
                                                       :mana    10
                                                       :max-mana 10
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :hero    {:name         "Gul'dan"
                                                                 :id           "h2"
                                                                 :damage-taken 0
                                                                 :fatigue      0
                                                                 :entity-type  :hero}}}
                 :counter                       1
                 :minion-ids-summoned-this-turn []}))}
  ; Multiple arity of a function [https://clojure.org/guides/learn/functions#_multi_arity_functions]
  ([]
   (create-empty-state []))
  ([heroes]
   ; Creates Jaina Proudmoore heroes if heroes are missing.
   (let [heroes (->> (concat heroes [(create-hero "Jaina Proudmoore")
                                     (create-hero "Jaina Proudmoore")])
                     (take 2))]
     {:player-id-in-turn             "p1"
      :players                       (->> heroes
                                          (map-indexed (fn [index hero]
                                                         {:id      (str "p" (inc index))
                                                          :mana    10
                                                          :max-mana 10
                                                          :deck    []
                                                          :hand    []
                                                          :minions []
                                                          :hero    (if (contains? hero :id)
                                                                     hero
                                                                     (assoc hero :id (str "h" (inc index))))}))
                                          (reduce (fn [a v]
                                                    (assoc a (:id v) v))
                                                  {}))
      :counter                       1
      :minion-ids-summoned-this-turn []})))

(defn get-player
  "Returns the player with the given id."
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-player "p1")
                    (:id))
                "p1"))}
  [state player-id]
  (get-in state [:players player-id]))


(defn get-player-id-in-turn
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-player-id-in-turn))
                "p1"))}
  [state]
  (:player-id-in-turn state))


(defn get-minions
  "Returns the minions on the board for the given player-id or for both players."
  {:test (fn []
           ; Getting minions is also tested in add-minion-to-board.
           (is= (-> (create-empty-state)
                    (get-minions "p1"))
                [])
           (is= (-> (create-empty-state)
                    (get-minions))
                [])
           (is= (as-> (create-empty-state) $
                      (assoc-in $ [:players "p1" :minions] [(create-minion "Sheep")])
                      (get-minions $ "p1")
                      (map :name $))
                ["Sheep"]))}
  ([state player-id]
   (:minions (get-player state player-id)))
  ([state]
   (->> (:players state)
        (vals)
        (map :minions)
        (apply concat))))


(defn get-deck
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-deck "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :deck]))


(defn get-hand
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-hand "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :hand]))


(defn- generate-id
  "Generates an id and returns a tuple with the new state and the generated id."
  {:test (fn []
           (is= (generate-id {:counter 6})
                [{:counter 7} 6]))}
  [state]
  {:pre [(contains? state :counter)]}
  [(update state :counter inc) (:counter state)])


(defn- generate-time-id
  "Generates a number and returns a tuple with the new state and the generated number."
  {:test (fn []
           (is= (generate-time-id {:counter 6})
                [{:counter 7} 6]))}
  [state]
  {:pre [(contains? state :counter)]}
  [(update state :counter inc) (:counter state)])

(declare battlecry)
(declare trigger-spell)


(defn add-minion-to-board
  "Adds a minion with a given position to a player's minions and updates the other minions' positions."
  {:test (fn []
           ; Adding a minion to an empty board
           (is= (as-> (create-empty-state) $
                      (add-minion-to-board $ "p1" (create-minion "Sheep" :id "m") 0)
                      (get-minions $ "p1")
                      (map (fn [m] {:id (:id m) :name (:name m)}) $))
                [{:id "m" :name "Sheep"}])
           ; Adding a minion and update positions
           (let [minions (-> (create-empty-state)
                             (add-minion-to-board "p1" (create-minion "Sheep" :id "m1") 0)
                             (add-minion-to-board "p1" (create-minion "Sheep" :id "m2") 0)
                             (add-minion-to-board "p1" (create-minion "Sheep" :id "m3") 1)
                             (get-minions "p1"))]
             (is= (map :id minions) ["m1" "m2" "m3"])
             (is= (map :position minions) [2 0 1]))
           ; Generating an id for the new minion
           (let [state (-> (create-empty-state)
                           (add-minion-to-board "p1" (create-minion "Sheep") 0))]
             (is= (-> (get-minions state "p1")
                      (first)
                      (:name))
                  "Sheep")
             (is= (:counter state) 3)))}
  [state player-id minion position]
  {:pre [(map? state) (string? player-id) (map? minion) (number? position)]}
  (let [[state id] (if (contains? minion :id)
                     [state (:id minion)]
                     (let [[state value] (generate-id state)]
                       [state (str "m" value)]))
        [state time-id] (generate-time-id state)
        ready-minion (assoc minion :position position
                                   :owner-id player-id
                                   :id id
                                   :added-to-board-time-id time-id)
        state-after-add (update-in state
                                   [:players player-id :minions]
                                   (fn [minions]
                                     (conj (->> minions
                                                (mapv (fn [m]
                                                        (if (< (:position m) position)
                                                          m
                                                          (update m :position inc)))))
                                           ready-minion)))]
    (if (= (:ability ready-minion) :battlecry)
      (battlecry state-after-add ready-minion)
      state-after-add)))



(defn add-minions-to-board
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-minions-to-board $ "p1" [(create-minion "Boulderfist Ogre")
                                                    "Sheep"
                                                    (create-minion "Leper Gnome")])
                      (get-minions $ "p1")
                      (map :name $))
                ["Boulderfist Ogre" "Sheep" "Leper Gnome"]))}
  [state player-id minions]
  (->> minions
       (reduce-kv (fn [state index minion]
                    (add-minion-to-board state
                                         player-id
                                         (if (string? minion)
                                           (create-minion minion)
                                           minion)
                                         index))
                  state)))


(defn- add-card-to
  "Adds a card to either the hand or the deck."
  {:test (fn []
           ; Adding cards to deck
           (is= (as-> (create-empty-state) $
                      (add-card-to $ "p1" "Leper Gnome" :deck)
                      (add-card-to $ "p1" "Sheep" :deck)
                      (get-deck $ "p1")
                      (map :name $))
                ["Leper Gnome" "Sheep"])
           ; Adding cards to hand
           (is= (as-> (create-empty-state) $
                      (add-card-to $ "p1" "Leper Gnome" :hand)
                      (add-card-to $ "p1" "Sheep" :hand)
                      (get-hand $ "p1")
                      (map :name $))
                ["Leper Gnome" "Sheep"]))}
  [state player-id card-or-name place]
  (let [card (if (string? card-or-name)
               (create-card card-or-name)
               card-or-name)
        [state id] (if (contains? card :id)
                     [state (:id card)]
                     (let [[state value] (generate-id state)]
                       [state (str "c" value)]))
        ready-card (assoc card :owner-id player-id
                               :id id)]
    (update-in state [:players player-id place] conj ready-card)))


(defn add-card-to-deck
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-card-to-deck $ "p1" "Leper Gnome")
                      (get-deck $ "p1")
                      (map :name $))
                ["Leper Gnome"]))}
  [state player-id card]
  (add-card-to state player-id card :deck))


(defn add-card-to-hand
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-card-to-hand $ "p1" "Leper Gnome")
                      (get-hand $ "p1")
                      (map :name $))
                ["Leper Gnome"]))}
  [state player-id card]
  (add-card-to state player-id card :hand))


(defn add-cards-to-deck
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-cards-to-deck $ "p1" ["Leper Gnome" "Loot Hoarder"])
                      (get-deck $ "p1")
                      (map :name $))
                ["Leper Gnome" "Loot Hoarder"]))}
  [state player-id cards]
  (reduce (fn [state card]
            (add-card-to-deck state player-id card))
          state
          cards))


(defn add-cards-to-hand
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-cards-to-hand $ "p1" ["Leper Gnome" "Loot Hoarder"])
                      (get-hand $ "p1")
                      (map :name $))
                ["Leper Gnome" "Loot Hoarder"]))}
  [state player-id cards]
  (reduce (fn [state card]
            (add-card-to-hand state player-id card))
          state
          cards))

(defn set-max-mana
  [state player-id value]
  (assoc-in state [:players player-id :max-mana] value))

(defn set-mana
  [state player-id value]
  (assoc-in state [:players player-id :mana] value))


(defn create-game
  "Creates a game with the given deck, hand, minions (placed on the board), and heroes."
  {:test (fn []
           (is= (create-game) (create-empty-state))

           (is= (create-game [{:hero (create-hero "Gul'dan")}])
                (create-game [{:hero "Gul'dan"}]))

           (is= (create-game [{:minions [(create-minion "Leper Gnome")]}])
                (create-game [{:minions ["Leper Gnome"]}]))

           ; This test is showing the state structure - otherwise avoid large assertions
           (is= (create-game [{:minions ["Leper Gnome"]
                               :deck    ["Loot Hoarder"]
                               :hand    ["Sheep"]}
                               {:hero "Gul'dan"}]
                             :player-id-in-turn "p2")
                {:player-id-in-turn             "p2"
                 :players                       {"p1" {:id      "p1"
                                                       :mana 10
                                                       :max-mana 10
                                                       :deck    [{:entity-type :card
                                                                  :id          "c3"
                                                                  :name        "Loot Hoarder"
                                                                  :owner-id    "p1"}]
                                                       :hand    [{:entity-type :card
                                                                  :id          "c4"
                                                                  :name        "Sheep"
                                                                  :owner-id    "p1"}]
                                                       :minions [{:damage-taken                0
                                                                  :attacks-performed-this-turn 0
                                                                  :added-to-board-time-id      2
                                                                  :entity-type                 :minion
                                                                  :name                        "Leper Gnome"
                                                                  :id                          "m1"
                                                                  :ability                     :deathrattle
                                                                  :attack                      1
                                                                  :health                      1
                                                                  :mana-cost                   1
                                                                  :position                    0
                                                                  :owner-id                    "p1"}]
                                                       :hero    {:name         "Jaina Proudmoore"
                                                                 :id           "h1"
                                                                 :entity-type  :hero
                                                                 :damage-taken 0
                                                                 :fatigue      0}}
                                                 "p2" {:id      "p2"
                                                       :mana 10
                                                       :max-mana 10
                                                       :deck    []
                                                       :hand    []
                                                       :minions []
                                                       :hero    {:name         "Gul'dan"
                                                                 :id           "h2"
                                                                 :entity-type  :hero
                                                                 :damage-taken 0
                                                                 :fatigue      0}}}
                 :counter                       5
                 :minion-ids-summoned-this-turn []}))}
  ([data & kvs]
   (let [players-data (map-indexed (fn [index player-data]
                                     (assoc player-data :player-id (str "p" (inc index))))
                                   data)
         state (as-> (create-empty-state (map (fn [player-data]
                                                (cond (nil? (:hero player-data))
                                                      (create-hero "Jaina Proudmoore")

                                                      (string? (:hero player-data))
                                                      (create-hero (:hero player-data))

                                                      :else
                                                      (:hero player-data)))
                                              data)) $
                     (reduce (fn [state {player-id :player-id
                                         minions   :minions
                                         deck      :deck
                                         hand      :hand
                                         mana      :mana
                                         max-mana  :max-mana}]
                               (as-> state $
                                     (if mana (set-mana $ player-id mana) $)
                                     (if max-mana (set-max-mana $ player-id max-mana) $)
                                     (add-minions-to-board $ player-id minions)
                                     (add-cards-to-deck $ player-id deck)
                                     (add-cards-to-hand $ player-id hand)))
                             $
                             players-data))]
     (if (empty? kvs)
       state
       (apply assoc state kvs))))
  ([]
   (create-game [])))


(defn get-max-mana
  "Return the max-mana of a player"
  [state player-id]
  (get-in state [:players player-id :max-mana]))

(defn get-mana
  "Return the current mana of a player"
  [state player-id]
  (get-in state [:players player-id :mana]))

(defn set-player-mana
  "Sets the current mana for a specified player. If mana is provided, it updates the player's mana in the state"
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (set-player-mana $ "p1" 3)
                      (get-in $ [:players "p1" :mana]))
                3))}
  [state player-id mana]
  (if mana
    (assoc-in state [:players player-id :mana] mana)
    state))

(defn set-player-max-mana
  "Sets the maximum mana for a specified player. If max-mana is provided, it updates the player's max mana in the state."
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (set-player-max-mana $ "p1" 3)
                      (get-in $ [:players "p1" :max-mana]))
                3))}
  [state player-id max-mana]
  (if max-mana
    (assoc-in state [:players player-id :max-mana] max-mana)
    state))

(defn reset-player-mana
  "Resets a player's mana at the end of their turn. Increases max mana by 1 if it's less than 10, and sets current mana to max mana."
  {:test (fn []
           ; Test case where max mana is less than 10
           (is= (as-> (create-empty-state) $
                      (set-player-max-mana $ "p1" 8)
                      (set-player-mana $ "p1" 5)
                      (reset-player-mana $ "p1")
                      [(get-max-mana $ "p1") (get-mana $ "p1")])
                [9 9]) ; Max mana increased to 9, current mana set to 9

           ; Test case where max mana is already 10
           (is= (as-> (create-empty-state) $
                      (set-player-max-mana $ "p1" 10)
                      (set-player-mana $ "p1" 5)
                      (reset-player-mana $ "p1")
                      [(get-max-mana $ "p1") (get-mana $ "p1")])
                [10 10]))}
  [state player-id]
  (let [current-max-mana (get-max-mana state player-id)]
    (if (< current-max-mana 10)
      (-> state
          (set-player-max-mana player-id (inc current-max-mana)) ; Increase max mana by 1
          (set-player-mana player-id (inc current-max-mana)))   ; Set current mana to the new max
      (set-player-mana state player-id current-max-mana))))     ; If max mana is 10, set current mana to max mana



(defn get-minion
  "Returns the minion with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Leper Gnome" :id "m")]}])
                    (get-minion "m")
                    (:name))
                "Leper Gnome"))}
  [state id]
  (->> (get-minions state)
       (filter (fn [m] (= (:id m) id)))
       (first)))


(defn get-players
  {:test (fn []
           (is= (->> (create-game)
                     (get-players)
                     (map :id))
                ["p1" "p2"]))}
  [state]
  (->> (:players state)
       (vals)))

(defn get-hero
  "Returns the hero of the player with the given player-id from the game state."
  {:test (fn []
           (let [game-state {:player-id-in-turn "p1"
                             :players {"p1" {:id "p1"
                                             :hero {:name "Jaina Proudmoore"
                                                    :id "h1"
                                                    :entity-type :hero
                                                    :damage-taken 0
                                                    :fatigue 0}}
                                       "p2" {:id "p2"
                                             :hero {:name "Gul'dan"
                                                    :id "h2"
                                                    :entity-type :hero
                                                    :damage-taken 0
                                                    :fatigue 0}}}}]
             (is (= (get-hero game-state "p1")
                    {:name "Jaina Proudmoore"
                     :id "h1"
                     :entity-type :hero
                     :damage-taken 0
                     :fatigue 0}))
             (is (= (get-hero game-state "p2")
                    {:name "Gul'dan"
                     :id "h2"
                     :entity-type :hero
                     :damage-taken 0
                     :fatigue 0}))
             (is (nil? (get-hero game-state "p3")))))}
  [state player-id]
  (get-in state [:players player-id :hero]))


(defn get-heroes
  {:test (fn []
           (is= (->> (create-game [{:hero "Gul'dan"}])
                     (get-heroes)
                     (map :name))
                ["Gul'dan" "Jaina Proudmoore"]))}
  [state]
  (->> (get-players state)
       (map :hero)))

(defn update-hero
  "Updates the value of the given key for the hero with the given id. If function-or-value is a value it will be the
   new value, else if it is a function it will be applied on the existing value to produce the new value."
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (update-hero "p1" :damage-taken inc)
                    (get-in [:players "p1" :hero :damage-taken]))
                1))}
  [state player-id key function-or-value]
  (update-in state [:players player-id :hero]
             (fn [hero]
               (if (fn? function-or-value)
                 (update hero key function-or-value)
                 (assoc hero key function-or-value)))))


(defn replace-minion
  "Replaces a minion with the same id by the given new-minion."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Leper Gnome" :id "m")]}])
                    (replace-minion (create-minion "Sheep" :id "m"))
                    (get-minion "m")
                    (:name))
                "Sheep"))}
  [state new-minion]
  (let [owner-id (or (:owner-id new-minion)
                     (:owner-id (get-minion state (:id new-minion))))]
    (update-in state
               [:players owner-id :minions]
               (fn [minions]
                 (map (fn [m]
                        (if (= (:id m) (:id new-minion))
                          new-minion
                          m))
                      minions)))))


(defn update-minion
  "Updates the value of the given key for the minion with the given id. If function-or-value is a value it will be the
   new value, else if it is a function it will be applied on the existing value to produce the new value."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Leper Gnome" :id "m")]}])
                    (update-minion "m" :damage-taken inc)
                    (get-minion "m")
                    (:damage-taken))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Leper Gnome" :id "m")]}])
                    (update-minion "m" :damage-taken 2)
                    (get-minion "m")
                    (:damage-taken))
                2))}

  [state id key function-or-value]
  (let [minion (get-minion state id)]
    (if minion
      (let [updated-minion (if (fn? function-or-value)
                             (update minion key function-or-value)
                             (assoc minion key function-or-value))]
        (replace-minion state updated-minion))
      state)))

(defn should-take-fatigue?
  "Returns truthy if the player should take fatigue damage (i.e., their deck is empty), falsey otherwise."
  {:test (fn []
           ;; Test when the deck is empty
           (let [state (create-game)]
             (is (should-take-fatigue? state "p1")))
           ;; Test when the deck has one card
           (let [state (-> (create-game)
                           (add-card-to-deck "p1" "Leper Gnome"))]
             (is-not (should-take-fatigue? state "p1")))
           ;; Test when the deck has multiple cards
           (let [state (-> (create-game)
                           (add-card-to-deck "p1" "Leper Gnome")
                           (add-card-to-deck "p1" "Boulderfist Ogre"))]
             (is-not (should-take-fatigue? state "p1"))))}
  [state player-id]
  (empty? (get-in state [:players player-id :deck])))

(defn handle-fatigue
  "Handles fatigue when a player's deck is empty, and they need to draw a card.
   Increases fatigue damage by 1 each time it occurs."
  {:test (fn []
           (let [initial-state (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1" :fatigue 0)}])
                 state-after-first-fatigue (handle-fatigue initial-state "p1")
                 state-after-second-fatigue (handle-fatigue state-after-first-fatigue "p1")]
             (is= (get-in state-after-first-fatigue [:players "p1" :hero :damage-taken]) 1)
             (is= (get-in state-after-first-fatigue [:players "p1" :hero :fatigue]) 1)
             (is= (get-in state-after-second-fatigue [:players "p1" :hero :damage-taken]) 3)
             (is= (get-in state-after-second-fatigue [:players "p1" :hero :fatigue]) 2)))}
  [state player-id]
  (let [current-fatigue (get-in state [:players player-id :hero :fatigue] 0)
        new-fatigue (inc current-fatigue)]
    (-> state
        (update-in [:players player-id :hero :damage-taken] + new-fatigue)
        (assoc-in [:players player-id :hero :fatigue] new-fatigue))))

(defn trigger-deathrattle
  "Triggers the Deathrattle effect of a minion"
  {:test (fn []
           (let [initial-state (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1" :damage-taken 0)}
                                             {:hero (create-hero "Gul'dan" :id "h2" :damage-taken 0)}])
                 leper-gnome (create-minion "Leper Gnome" :id "lg" :owner-id "p1")
                 loot-hoarder (create-minion "Loot Hoarder" :id "lh" :owner-id "p2")
                 state-with-minions (-> initial-state
                                        (add-minion-to-board "p1" leper-gnome 0)
                                        (add-minion-to-board "p2" loot-hoarder 0)
                                        (add-card-to-deck "p2" "Test Card"))
                 state-after-leper-gnome (trigger-deathrattle state-with-minions leper-gnome)
                 state-after-loot-hoarder (trigger-deathrattle state-after-leper-gnome loot-hoarder)]
             ; Test Leper Gnome Deathrattle
             (is= (get-in state-after-leper-gnome [:players "p2" :hero :damage-taken]) 2)
             ; Test Loot Hoarder Deathrattle
             (is= (count (get-hand state-after-loot-hoarder "p2")) 1)
             (is= (count (get-deck state-after-loot-hoarder "p2")) 0)
             ; Test minion without Deathrattle
             (let [boulderfist-ogre (create-minion "Boulderfist Ogre" :id "bo" :owner-id "p1")
                   state-after-boulderfist (trigger-deathrattle state-with-minions boulderfist-ogre)]
               (is= state-after-boulderfist state-with-minions))))}
  [state minion]
  (case (:name minion)
    "Leper Gnome"
    (let [enemy-id (if (= (:owner-id minion) "p1") "p2" "p1")]
      (update-in state [:players enemy-id :hero :damage-taken] + 2))
    "Loot Hoarder"
    (let [owner-id (:owner-id minion)]
      (if (empty? (get-deck state owner-id))
        (handle-fatigue state owner-id)
        (-> state
            (update-in [:players owner-id :hand] conj (first (get-deck state owner-id)))
            (update-in [:players owner-id :deck] rest))))

    state))

(defn remove-card-from-deck
  {:test (fn []
           (let [state (-> (create-game [{:deck [(create-card "Leper Gnome" :id "c1")
                                                 (create-card "Boulderfist Ogre" :id "c2")
                                                 (create-card "Leper Gnome" :id "c3")]}]))
                 card (->> (get-deck state "p1")
                           (second))]
             (is= (as-> state $
                        (remove-card-from-deck $ "p1" card)
                        (get-deck $ "p1")
                        (map :id $))
                  ["c1" "c3"])))}
  [state player-id card]
  (update-in state [:players player-id :deck]
             (fn [deck]
               (->> deck
                    (remove (fn [c] (= (:id card) (:id c))))))))

(defn play-minion-card
  "Allows a player to play a minion card from their hand to the board if they have enough mana."
  {:test (fn []
           ; Test playing a card with sufficient mana
           (let [initial-state (-> (create-game [{:hand [(create-card "Leper Gnome" :id "c1" :mana-cost 5)]
                                                  :mana 10}]))
                 state-after-play (play-minion-card initial-state "p1" "c1" 0)]
             (is= (get-in state-after-play [:players "p1" :mana]) 5)
             (is= (count (get-hand state-after-play "p1")) 0)
             (is= (get-in state-after-play [:players "p1" :minions 0 :name]) "Leper Gnome")))}
  [state player-id card-id position]
  (let [hand (get-hand state player-id)
        card (some #(when (= (:id %) card-id) %) hand)
        mana-cost (:mana-cost card)
        player-mana (get-in state [:players player-id :mana])]

    (if (and card (>= player-mana mana-cost))
      (let [minion (create-minion (:name card))]
        (-> state
            (update-in [:players player-id :mana] - mana-cost)
            (update-in [:players player-id :hand] #(remove (fn [c] (= (:id c) card-id)) %))
            (add-minion-to-board player-id minion position)))
      state)))



(defn trigger-spell
  "Triggers the effect of a spell when it's played on the board."
  {:test (fn []
           ; Test Consecration
           (let [initial-state (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1" :damage-taken 0)}
                                             {:hero (create-hero "Gul'dan" :id "h2" :damage-taken 0)
                                              :minions [(create-minion "Leper Gnome" :id "m1" :damage-taken 0)]}])
                 spell-card (create-card "Consecration" :owner-id "p1")
                 result-state (trigger-spell initial-state spell-card)]
             (is= (get-in result-state [:players "p2" :hero :damage-taken]) 2)
             (is= (get-in result-state [:players "p2" :minions 0 :damage-taken]) 2))

           ; Test Equality
           (let [initial-state (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "m1" :health 7)]}
                                             {:minions [(create-minion "Leper Gnome" :id "m2" :health 5)]}])
                 spell-card (create-card "Equality" :owner-id "p1")
                 result-state (trigger-spell initial-state spell-card)]
             (is= (get-in result-state [:players "p1" :minions 0 :health]) 1)
             (is= (get-in result-state [:players "p2" :minions 0 :health]) 1))

           ; Test Feign Death
           (let [initial-state (create-game [{:minions [(create-minion "Leper Gnome" :id "m1" :ability :deathrattle)
                                                        (create-minion "Boulderfist Ogre" :id "m2")]}])
                 spell-card (create-card "Feign Death" :owner-id "p1")
                 result-state (trigger-spell initial-state spell-card)]
             (is (not= result-state initial-state)) ; Assuming trigger-deathrattle changes the state
             (is= (count (get-in result-state [:players "p1" :minions])) 2)))}
  [state spell-card]
  (let [spell-name (:name spell-card)
        player-id (:owner-id spell-card)]
    (case spell-name
      "Consecration"
      (let [enemy-id (if (= player-id "p1") "p2" "p1")]
        (-> state
            ; Damage enemy hero by 2
            (update-in [:players enemy-id :hero :damage-taken] + 2)
            ; Damage all enemy minions by 2
            (update-in [:players enemy-id :minions]
                       (fn [minions]
                         (vec (map #(update % :damage-taken + 2) minions))))))

      "Equality"
      (reduce
        (fn [s player-id]
          (update-in s [:players player-id :minions]
                     (fn [minions]
                       (mapv #(assoc % :health 1) minions))))
        state
        (keys (:players state)))

      "Feign Death"
      (let [player-minions (get-in state [:players player-id :minions])
            deathrattle-minions (filter #(= (:ability %) :deathrattle) player-minions)]
        (reduce
          (fn [s minion]
            (trigger-deathrattle s minion))
          state
          deathrattle-minions))

      state)))

(defn trigger-stealth
  "Triggers the stealth effect for minions with the stealth ability"
  {:test (fn []
           ; Test Moroes
           (let [initial-state (create-game [{:minions [(create-minion "Moroes" :id "m1" :owner-id "p1")]}])
                 result-state (trigger-stealth initial-state "m1")
                 player-minions (get-minions result-state "p1")]
             (is= (count player-minions) 2)
             (is= (:name (first player-minions)) "Moroes")
             (is= (:name (second player-minions)) "Steward"))

           ; Test Blood Imp
           (let [initial-state (create-game [{:minions [(create-minion "Blood Imp" :id "m1" :owner-id "p1")
                                                        (create-minion "Boulderfist Ogre" :id "m2" :owner-id "p1" :health 7)]}])
                 result-state (trigger-stealth initial-state "m1")
                 ogre (get-minion result-state "m2")]
             (is= (:health ogre) 8))

           ; Test Blood Imp with no other minions
           (let [initial-state (create-game [{:minions [(create-minion "Blood Imp" :id "m1" :owner-id "p1")]}])
                 result-state (trigger-stealth initial-state "m1")]
             (is= result-state initial-state)))}
  [state minion-id]
  (let [minion (get-minion state minion-id)
        player-id (:owner-id minion)]
    (case (:name minion)
      "Moroes"
      (let [[new-state steward-id] (generate-id state)
            steward (create-minion "Steward" :id steward-id :owner-id player-id)]
        (add-minion-to-board new-state player-id steward (count (get-minions new-state player-id))))

      "Blood Imp"
      (let [friendly-minions (filter #(and (= (:owner-id %) player-id)
                                           (not= (:id %) minion-id))
                                     (get-minions state player-id))]
        (if (seq friendly-minions)
          (let [random-minion (rand-nth friendly-minions)]
            (update-minion state (:id random-minion) :health inc))
          state))

      state)))