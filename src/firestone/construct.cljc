(ns firestone.construct
  "A namespace for constructions and basic manipulations of the entities in the state"
  (:require [ysera.test :refer [is is-not is= error?]]
            [ysera.error :refer [error]]
            [firestone.definitions :refer [get-definition]]))


(defn create-hero
  "Creates a hero from its definition by the given hero name. The additional key-values will override the default values."
  {:test (fn []
           (is= (create-hero "Jaina Proudmoore")
                {:name             "Jaina Proudmoore"
                 :entity-type      :hero
                 :health           30
                 :hero-power       "Fireblast"
                 :hero-power-used  false
                 :power            (:power (get-definition "Fireblast"))
                 :damage-taken     0
                 :fatigue          0})
           (is= (create-hero "Gul'dan" :damage-taken 10)
                {:name             "Gul'dan"
                 :entity-type      :hero
                 :health           30
                 :hero-power       "Life Tap"
                 :hero-power-used  false
                 :power            (:power (get-definition "Life Tap"))
                 :damage-taken     10
                 :fatigue          0}))}
  [name & kvs]
  (let [definition (get-definition name)
        hero {:name               name
              :entity-type        :hero
              :damage-taken       0
              :max-mana           10
              :mana               10
              :health             (:health definition)
              :class              (:class definition)
              :type               (:type definition)
              :stealth            nil
              :effects            []
              :has-used-your-turn false
              :fatigue-counter    0
              :hero-power         {
                                   :class       (:class (get-definition (:hero-power definition)))
                                   :description (:description (get-definition (:hero-power definition)))
                                   :mana-cost   (:mana-cost (get-definition (:hero-power definition)))
                                   :name        (:name (get-definition (:hero-power definition)))
                                   :type        (:type (get-definition (:hero-power definition)))
                                   }
              }]
    (if (empty? kvs)
      hero
      (apply assoc hero kvs))))


(defn create-card
  "Creates a card from its definition by the given card name. The additional key-values will override the default values."
  {:test (fn []
           (is= (-> (create-card "Boulderfist Ogre" :id "bo")
                    (:id))
                "bo"))}
  [name & kvs]
  (let [definition (get-definition name)
        card {:name              name
              :type              (:type definition)
              :mana-cost         (:mana-cost definition)
              :original-mana-cost (:mana-cost definition)
              :playable          false
              :valid-target-ids  []
              :attack            (:attack definition)
              :original-attack   (:attack definition)
              :health            (:health definition)
              :original-health   (:health definition)
              :description       (or (:description definition) "")
              :entity-type       :card}]
    (if (empty? kvs)
      card
      (apply assoc card kvs))))

(defn get-mana-cost
  "Returns the mana cost of the minion (or card) with the given name."
  {:test (fn []
           ; Test case for minion by name
           (is= (-> (get-mana-cost "Loot Hoarder"))
                2)

           ;; Test case for card by ID
           (is= (-> (get-mana-cost "Boulderfist Ogre"))
                6))}
  [name]
  (let [definition (get-definition name)]
    (:mana-cost definition)))

(defn get-original-attack
  "Returns the attack of the minion with the given id."
  [name]
  (let [definition (get-definition name)]
    (:attack definition)))

(defn get-original-health
  "Returns the attack of the minion with the given id."
  [name]
  (let [definition (get-definition name)]
    (:health definition)))

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
                 :mana-cost                   1}))}
  [name & kvs]
  (let [definition (get-definition name)
        minion {:damage-taken                0
                :entity-type                 :minion
                :attack                      (:attack definition)
                :health                      (:health definition)
                :name                        name
                :attacks-performed-this-turn 0
                :can-attack                  (or (:can-attack definition) nil)
                :description                 (or (:description definition) "")
                :mana-cost                   (:mana-cost definition)
                :max-health                  (:health definition)
                :original-attack             (:attack definition)
                :original-health             (:health definition)
                :race                        (:race definition)
                :set                         (:set definition)}]
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
                                                          :deck    []
                                                          :hand    []
                                                          :minions []
                                                          :secrets []
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

(defn get-secrets
  {:test (fn []
           (is= (-> (create-empty-state)
                    (get-secrets "p1"))
                []))}
  [state player-id]
  (get-in state [:players player-id :secrets]))


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
  (let [minions (get-in state [:players player-id :minions])
        current-count (count minions)]
    (if (>= current-count 7)
      state                                                 ; If there are already 7 minions, return the state unchanged.
      (let [[state id] (if (contains? minion :id)
                         [state (:id minion)]
                         (let [[state value] (generate-id state)]
                           [state (str "m" value)]))
            [state time-id] (generate-time-id state)
            ready-minion (assoc minion :position position
                                       :effects (if (contains? minion :effects) (:effects minion) (let [effect (get (get-definition minion) :effect)] (if effect [effect] [])))
                                       :owner-id player-id
                                       :id id
                                       :added-to-board-time-id time-id)]
        (-> state (update-in
                    [:players player-id :minions]
                    (fn [minions]
                      (conj (->> minions
                                 (mapv (fn [m]
                                         (if (< (:position m) position)
                                           m
                                           (update m :position inc)))))
                            ready-minion)))
            ;(effects-parser (get-all-characters state) player-id :summon-minion)
            )
        ))))



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
        card-to-be-added (assoc card :owner-id player-id
                               :id id)]
    (condp = place
      :hand
      (let [hand-current (get-hand state player-id)
            hand-count (count hand-current)]
        (if (>= hand-count 10)
          state                                             ; If hand size is 10 or more, return the state unchanged.
          (update-in state [:players player-id place] conj card-to-be-added)))

      :secrets
      (let [current-secrets (get-secrets state player-id)
            card-name (if (string? card-or-name) card-or-name (:name card-or-name))]
        (if (or (empty? current-secrets)
                (not (some #(= (:name %) card-name) current-secrets)))
          (update-in state [:players player-id place] conj card-to-be-added)
          state))

      (update-in state [:players player-id place] conj card-to-be-added))))


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

(defn add-card-to-secrets
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (add-card-to-secrets $ "p1" (create-card "Cat Trick"))
                      (get-secrets $ "p1")
                      (map :name $))
                ["Cat Trick"]))}
  [state player-id card]
  (add-card-to state player-id card :secrets))

(defn add-cards-to-secrets
  [state player-id cards]
  (reduce (fn [state card]
            (add-card-to-secrets state player-id card))
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
  (-> state
      (assoc-in [:players player-id :hero :max-mana] (min 10 value))))

(defn set-mana
  [state player-id value]
  (assoc-in state [:players player-id :hero :mana] value))


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
                                                (let [overrides (select-keys player-data [:mana :max-mana])]
                                                  (cond
                                                    (nil? (:hero player-data))
                                                    (create-hero "Jaina Proudmoore"
                                                                 :mana (or (:mana player-data) 10)
                                                                 :max-mana (or (:max-mana player-data) 10))

                                                    (string? (:hero player-data))
                                                    (create-hero (:hero player-data)
                                                                 :mana (or (:mana player-data) 10)
                                                                 :max-mana (or (:max-mana player-data) 10))

                                                    :else
                                                    (merge (:hero player-data) overrides))))
                                              data)) $
                     (reduce (fn [state {player-id :player-id
                                         minions   :minions
                                         deck      :deck
                                         board     :board
                                         hand      :hand
                                         secrets   :secrets}]
                               (-> state
                                   (add-minions-to-board player-id minions)
                                   (add-cards-to-deck player-id deck)
                                   (add-cards-to-hand player-id hand)
                                   (add-cards-to-secrets player-id secrets)))
                             $
                             players-data))]
     (if (empty? kvs)
       state
       (apply assoc state kvs))))
  ([]
   (create-game [])))


(defn get-max-mana
  [state player-id]
  (get-in state [:players player-id :hero :max-mana]))

(defn get-mana
  [state player-id]
  (get-in state [:players player-id :hero :mana]))

(defn set-player-mana
  "Sets the current mana for a specified player. If mana is provided, it updates the player's mana in the state"
  {:test (fn []
           (is= (as-> (create-empty-state) $
                      (set-player-mana $ "p1" 3)
                      (get-in $ [:players "p1" :mana]))
                3))}
  [state player-id mana]
  (if mana
    (assoc-in state [:players player-id :hero :mana] mana)
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
    (assoc-in state [:players player-id :hero :max-mana] max-mana)
    state))

(defn reset-player-mana
  "Resets a player's mana at the end of their turn. Increases max mana by 1 if it's less than 10, and sets current mana to that new max."
  [state player-id]
  (let [old-max-mana (get-max-mana state player-id)
        new-max-mana (min 10 (inc old-max-mana))]
    (-> state
        (assoc-in [:players player-id :hero :max-mana] new-max-mana)
        (assoc-in [:players player-id :hero :mana] new-max-mana))))


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

(defn get-hero-player-id
  [state player-id]
  (:hero (get-player state player-id)))


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
  (let [fatigue-damage (+ (get-in state [:players player-id :hero :fatigue-counter]) 1)
        current-damage-taken (get-in state [:players player-id :hero :damage-taken])]
    (-> state
        (assoc-in [:players player-id :hero :fatigue-counter] fatigue-damage)
        (assoc-in [:players player-id :hero :damage-taken] (+ current-damage-taken fatigue-damage)))))

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

(defn remove-minion
  "Removes a minion with the given id from the state, triggering its Deathrattle if applicable."
  {:test (fn []
           (let [initial-state (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1" :damage-taken 0)}
                                             {:hero (create-hero "Gul'dan" :id "h2" :damage-taken 0)}])
                 leper-gnome (create-minion "Leper Gnome" :id "lg" :owner-id "p1")
                 loot-hoarder (create-minion "Loot Hoarder" :id "lh" :owner-id "p1")
                 boulderfist-ogre (create-minion "Boulderfist Ogre" :id "bo" :owner-id "p1")
                 state-with-minions (-> initial-state
                                        (add-minion-to-board "p1" leper-gnome 0)
                                        (add-minion-to-board "p1" loot-hoarder 1)
                                        (add-minion-to-board "p1" boulderfist-ogre 2)
                                        (add-card-to-deck "p1" "Test Card"))
                 state-after-leper-gnome (remove-minion state-with-minions "lg")
                 state-after-loot-hoarder (remove-minion state-after-leper-gnome "lh")
                 state-after-boulderfist (remove-minion state-after-loot-hoarder "bo")]
             ; Test Leper Gnome removal and Deathrattle
             (is= (count (get-minions state-after-leper-gnome "p1")) 2)
             (is-not (get-minion state-after-leper-gnome "lg"))
             (is= (get-in state-after-leper-gnome [:players "p2" :hero :damage-taken]) 2)
             ; Test Loot Hoarder removal and Deathrattle
             (is= (count (get-minions state-after-loot-hoarder "p1")) 1)
             (is-not (get-minion state-after-loot-hoarder "lh"))
             (is= (count (get-hand state-after-loot-hoarder "p1")) 1)
             (is= (count (get-deck state-after-loot-hoarder "p1")) 0)
             ; Test Boulderfist Ogre removal (no Deathrattle)
             (is= (count (get-minions state-after-boulderfist "p1")) 0)
             (is-not (get-minion state-after-boulderfist "bo"))
             ; Test removing non-existent minion
             (is= state-after-boulderfist (remove-minion state-after-boulderfist "non-existent"))))}
  [state id]
  (let [minion (get-minion state id)
        owner-id (:owner-id minion)]
    (if minion
        (let [; Check if the minion has a deathrattle ability
              state-after-deathrattle (if (= :deathrattle (:ability minion))
                                        (trigger-deathrattle state minion)
                                        state)]
          (update-in state-after-deathrattle [:players owner-id :minions]
                     (fn [minions]
                       (remove (fn [m] (= (:id m) id)) minions))))
      state)))



(defn remove-minions
  "Removes the minions with the given ids from the state."
  {:test (fn []
           (is= (as-> (create-game [{:minions [(create-minion "Leper Gnome" :id "n1")
                                               (create-minion "Leper Gnome" :id "n2")]}
                                    {:minions [(create-minion "Leper Gnome" :id "n3")
                                               (create-minion "Leper Gnome" :id "n4")]}]) $
                      (remove-minions $ "n1" "n4")
                      (get-minions $)
                      (map :id $))
                ["n2" "n3"]))}
  [state & ids]
  (reduce remove-minion state ids))


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


(defn remove-card-from-hand
  "Removes a specified card from the player's hand."
  {:test (fn []
           (let [state (-> (create-game [{:hand [(create-card "Leper Gnome" :id "c1")
                                                 (create-card "Boulderfist Ogre" :id "c2")
                                                 (create-card "Leper Gnome" :id "c3")]}]))
                 card (->> (get-hand state "p1")
                           (second))]
             (is= (as-> state $
                        (remove-card-from-hand $ "p1" card)
                        (get-hand $ "p1")
                        (map :id $))
                  ["c1" "c3"])))}
  [state player-id card]
  (update-in state [:players player-id :hand]
             (fn [hand]
               (->> hand
                    (remove (fn [c] (= (:id card) (:id c))))))))

(defn get-card
  [state card-id]
  {:pre [(map? state) (string? card-id)]}
  (-> (filter (fn [x] (= (:id x) card-id)) (into [] (concat (get-in state [:players "p1" :hand]) (get-in state [:players "p2" :hand]))))
      (first)))

(defn can-play-minion?
  "Determines if a player can play a specified minion card"
  {:test (fn []
           (is= (-> (create-game [{:mana 5}])
                    (can-play-minion? "p1" (create-card "Boulderfist Ogre")))
                false)
           (is= (-> (create-game [{:mana 6}])
                    (can-play-minion? "p1" (create-card "Boulderfist Ogre")))
                true)
           (is= (-> (create-game [{:mana 7}])
                    (can-play-minion? "p1" (create-card "Boulderfist Ogre")))
                true))}
  [state player-id card]
  {:pre [(map? state) (string? player-id) (map? card)]}
  (and
    ;; Check if it's the player's turn
    (= (:player-id-in-turn state) player-id)

    ;; Check if the player has fewer than 7 minions on the board
    (< (count (get-in state [:players player-id :minions])) 7)

    ;; Check if the player has enough mana to play the card
    (<= (-> card
            (get-definition)
            (:mana-cost))  ;; Retrieve the card's mana cost
        (get-in state [:players player-id :hero :mana]))))  ;; Retrieve the player's current mana

(defn convert-card-to-minion
  "Creates a minion out of a card using the create-minion function."
  {:test (fn []
           (is= (-> (convert-card-to-minion (create-card "Boulderfist Ogre"))
                    (:damage-taken))
                0)
           (is= (-> (convert-card-to-minion (create-card "Boulderfist Ogre"))
                    (:entity-type))
                :minion))}
  [card]
  {:pre [(map? card)]}
  (let [definition (get-definition (:name card))
        base-states (filter identity [(when (:stealth definition) :stealth)
                                      (when (:deathrattle definition) :deathrattle)])]
    (create-minion (:name card)
                   :states base-states
                   :damage-taken 0
                   :attacks-performed-this-turn 0
                   :sleepy true
                   :valid-attack-ids {})))

(defn deduct-player-mana
  "Reduces a player's mana by the specified cost and returns the updated game state."
  {:test (fn []
           (is= (-> (create-game [{:mana 7}])
                    (deduct-player-mana "p1" 6)
                    (get-in [:players "p1" :mana]))
                1))}
  [state player-id mana-cost]
  {:pre [(map? state) (string? player-id) (int? mana-cost)]}
  (let [player-mana (get-in state [:players player-id :hero :mana])
        updated-mana (- player-mana mana-cost)]
    (update-in state [:players player-id :hero :mana] (constantly updated-mana))))

(defn put-card-on-board
  "Transforms a card into a minion using convert-card-to-minion and places it on the board for the specified player at the given position.
   Handles special effects like reaction and end-of-turn effects."
  {:test (fn []
           (is= (let [card (create-card "Boulderfist Ogre" :id "bo")]
                  (-> (create-game [{:hand [card]}])
                      (put-card-on-board "p1" card 1)
                      (get-in [:players "p1" :minions])
                      (first)
                      (:name)))
                "Boulderfist Ogre"))}
  [state player-id card position]
  {:pre [(map? state) (string? player-id) (map? card) (int? position)]}
  (let [minion (convert-card-to-minion card)]
    (-> state
        (add-minion-to-board player-id minion position))))



(defn play-minion-card
  "Handles playing a card by its type (minion or spell). Incorporates logic to check if the card is playable and deduct mana."
  [state player-id card-id position]
  ;; Retrieve the card directly within the function
  (let [card (-> (filter (fn [x] (= (:id x) card-id))
                         (into []
                               (concat (get-in state [:players "p1" :hand])
                                       (get-in state [:players "p2" :hand]))))
                 (first))]
    ;; Handle minion cards
    (cond
      (= (:type card) :minion)
      (if (can-play-minion? state player-id card)
        (-> (deduct-player-mana state player-id (-> (get-definition (:name card))
                                                    (:mana-cost)))
            (remove-card-from-hand player-id card)
            (put-card-on-board player-id card position)))
      ;handle battlecry
      ;handle combo, update playable cards, update combo cards and the cards-played-this-turn key


      ; (= (:type card) :spell)       ....implementation left for spells
      )))


(defn play
  "Allows a player to play a minion card from their hand to the board if they have enough mana."
  {:test (fn []
           ; Test playing a card with sufficient mana
           (let [initial-state (-> (create-game [{:hand [(create-card "Leper Gnome" :id "c1" :mana-cost 5)]
                                                  :mana 10}]))
                 state-after-play (play initial-state "p1" "c1" 0)]
             (is= (get-in state-after-play [:players "p1" :mana]) 5)
             (is= (count (get-hand state-after-play "p1")) 0)
             (is= (get-in state-after-play [:players "p1" :minions 0 :name]) "Leper Gnome")))}
  [state player-id card-id position]
  (let [hand (get-hand state player-id)
        card (some #(when (= (:id %) card-id) %) hand)
        mana-cost (:mana-cost card)
        player-mana (get-in state [:players player-id :hero :mana])]

    ;TODO: Check if player has 7 cards on board or not.
    (if (and card (>= player-mana mana-cost))
      (let [minion (create-minion (:name card))]
        (-> state
            (update-in [:players player-id :hero :mana] - mana-cost)
            (update-in [:players player-id :hand] #(remove (fn [c] (= (:id c) card-id)) %))
            (add-minion-to-board player-id minion position)))
      state)))

(defn trigger-battlecry
  "Triggers the Battlecry effect of a minion"
  {:test (fn []
           (let [initial-state (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1" :damage-taken 0)}
                                             {:hero (create-hero "Gul'dan" :id "h2" :damage-taken 0)
                                              :minions [(create-minion "Sheep" :id "s1" :attack 1 :owner-id "p2")]}])
                 silver-hand-knight (create-minion "Silver Hand Knight" :id "shk" :owner-id "p1")
                 state-with-knight (add-minion-to-board initial-state "p1" silver-hand-knight 0)

                 stampeding-kodo (create-minion "Stampeding Kodo" :id "sk" :owner-id "p1")
                 state-with-kodo (add-minion-to-board state-with-knight "p1" stampeding-kodo 1)
                 twilight-drake (create-minion "Twilight Drake" :id "td" :owner-id "p1")
                 state-with-card (add-card-to-hand state-with-kodo "p1" "Test Card")
                 state-with-drake (add-minion-to-board state-with-card "p1" twilight-drake 2)
                 dr-boom (create-minion "Dr. Boom" :id "db" :owner-id "p1")
                 state-with-boom (add-minion-to-board initial-state "p1" dr-boom 0)
                 ]

             (is= (count (get-minions state-with-knight "p1")) 2)
             (is= (get-in (get-minions state-with-knight "p1") [1 :name]) "Squire")
             (is= (count (get-minions state-with-kodo "p2")) 0)
             (is= (get-in (get-minion state-with-drake "td") [:health]) 2)
             ; Test Dr. Boom Battlecry (summoning two Boom Bots)
             (is= (count (get-minions state-with-boom "p1")) 3)))}
  [state minion]
  (let [minion-name (:name minion)
        owner-id    (:owner-id minion)
        card-def    (get-definition minion-name)  ;; or however you retrieve definitions
        bc-fn       (:battlecry card-def)]
    (if bc-fn
      ;; Call the battlecry function with the appropriate params
      (bc-fn state :player-id owner-id)
      ;; If there is no :battlecry in the definition, just return the state
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

(defn get-attack
  "Returns the attack of the minion with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Sheep" :id "m")]}])
                    (get-attack "m"))
                1))}
  [state id]
  (let [minion (get-minion state id)
        definition (get-definition minion)]
    (println "definition of minion " definition)
    (:attack definition)))

(defn mark-minion-attacked
  "Marks the minion as having attacked by setting its :attacks-performed-this-turn to 1."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bg")]}])
                    (mark-minion-attacked "bg")
                    (get-in [:players "p1" :minions 0 :attacks-performed-this-turn]))
                1)
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bg")]}])
                    (get-in [:players "p1" :minions 0 :attacks-performed-this-turn]))
                0))}
  [state id]
  {:pre [(map? state) (string? id)]}
  (update-minion state id :attacks-performed-this-turn 1))

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

(defn get-player-id-by-hero-id
  "Returns the player ID that owns the hero with the given hero ID."
  [state hero-id]
  (some (fn [[player-id player-data]]
          (when (= (:id (:hero player-data)) hero-id)
            player-id))
        (:players state)))

(defn trigger-minion-damaged
  "Triggers the :minion-damaged event for a specific minion."
  [state minion-id]
  (let [minion (get-minion state minion-id)]
    (if minion
      (let [handler (:on-minion-damaged minion)]
        (if handler
          (handler state minion)
          state))
      state)))

(defn trigger-minion-damaged
  "Triggers the :minion-damaged event for a specific minion, ensuring state is updated properly."
  [state minion-id]
  (let [minion (get-minion state minion-id)]
    (if minion
      (let [handler (:on-minion-damaged minion)]
        (if handler
          (let [new-state (handler state minion)]
            (if new-state
              new-state
              (do (println "Warning: Handler did not return a new state for minion" minion-id)
                  state)))
          state))
      (do (println "Warning: Minion not found with id:" minion-id)
          state))))

;TODO: Handle stealth
(defn handle-minion-attack-on-minion
  "Handles the attack logic when a minion attacks another minion, including event dispatching."
  [state attacker-id target-id]
  (let [attacker (get-minion state attacker-id)
        target (get-minion state target-id)]
    (if (and attacker target)
      (let [attacker-damage (:attack attacker)
            target-damage (:attack target)
            state-after-attack (-> state
                                   (update-minion attacker-id :health #(- % target-damage))
                                   (update-minion target-id :health #(- % attacker-damage))
                                   (update-minion attacker-id :attacks-performed-this-turn inc))]
        ;; Dispatch :minion-damaged for attacker and target
        (-> state-after-attack
            (trigger-minion-damaged attacker-id)
            (trigger-minion-damaged target-id)
            ;; Remove minions with health <= 0
            (cond-> (and (get-minion state-after-attack attacker-id)
                         (<= (:health (get-minion state-after-attack attacker-id)) 0))
                    (remove-minion attacker-id))
            (cond-> (and (get-minion state-after-attack target-id)
                         (<= (:health (get-minion state-after-attack target-id)) 0))
                    (remove-minion target-id))))
      state)))

;TODO: Handle stealth
(defn handle-minion-attack-on-hero
  "Handles the attack logic when a minion attacks a hero."
  [state attacker-id target-id]
  (println "gamestate before attack on hero" state)
  (let [player-id (get-player-id-by-hero-id state target-id)] ; Retrieve player ID based on hero ID
    (if player-id
      (let [attack-value (get-attack state attacker-id)]
        (if (nil? attack-value)
          (do
            ;(println "Attack value is nil for attacker-id:" attacker-id)
            (error "Invalid attack value."))
          (let [updated-state (update-hero state player-id :damage-taken #(+ % attack-value))]
            ;(println "gamestate after attack on hero" updated-state)
            (mark-minion-attacked updated-state attacker-id))))
      (do
        (println "Player owning the target hero not found for hero-id:" target-id)
        (error "Player owning the target hero not found.")))))

(defn use-hero-power
  "Allows a player to use their hero power.
   - For Jaina Proudmoore's Fireblast, a target-id must be provided.
   - For Gul'dan's Life Tap, no target is needed.
   Ensures the hero power is used only once per turn.
   Returns the updated state."
  [state player-id & [target-id]]
  (let [hero (:hero (get-player state player-id))
        power-name (:hero-power hero)
        power-def (get-definition power-name) ;; Retrieves from hero-power-definitions
        current-mana (get-mana state player-id)]
    (cond
      ;; Check if hero power is already used this turn
      (:hero-power-used hero)
      (do
        (println "Hero power has already been used this turn.")
        state)

      ;; Check if player has enough mana
      (< current-mana (:mana-cost power-def))
      (do
        (println "Not enough mana to use hero power.")
        state)

      :else
      (let [power-fn (:power power-def)
            state-after-mana (deduct-player-mana state player-id (:mana-cost power-def))
            ;; Invoke the power function with appropriate arguments
            state-after-use (cond
                              (= power-name "Fireblast")
                              (if target-id
                                (power-fn state-after-mana target-id)
                                (do
                                  (println "Fireblast requires a target.")
                                  state-after-mana))

                              (= power-name "Life Tap")
                              (power-fn state-after-mana player-id)

                              :else
                              (do
                                (println "Unknown hero power.")
                                state-after-mana))
            ;; Mark hero power as used
            updated-state (assoc-in state-after-use [:players player-id :hero :hero-power-used] true)]
        updated-state))))


(defn clear-sleepy-status-from-minions
  "Removes sleepy from minion-ids-summoned-this-turn list.
   Logs before and after updating each minion."
  [state]
  (println "Entering clear-sleepy-status-from-minions with state:" (pr-str state))
  (let [minion-ids (:minion-ids-summoned-this-turn state)]
    (reduce
      (fn [current-state minion-id]
        (if (get-minion current-state minion-id)              ; Check if minion still exists
          (do
            (println "Clearing sleepy status for minion" minion-id)
            (-> current-state
                (update-minion minion-id :sleepy false)          ; Clear sleepy status by setting false
                (update-minion minion-id :can-attack 1)))         ; Ensure can-attack is consistent
          (do
            (println "Minion" minion-id "not found, skipping")
            current-state)))
      state
      minion-ids)))


(defn get-enemy-ids
  "Fetches all enemy entity IDs (minions and hero) for the given player-id."
  [state player-id]
  (->> (get-players state)
       (filter #(not= (:id %) player-id)) ; Exclude the current player
       (mapcat (fn [enemy]
                 (concat
                   (map :id (:board-entities enemy)) ; Minions on board
                   [(:id (:hero enemy))]))))) ; Hero ID

(defn reset-can-attack
  "Resets can-attack for the minions of the given player-id to 1 if appropriate.
   Logs the decision for each minion."
  [state player-id]
  (println "Entering reset-can-attack for player" player-id "with state:" (pr-str state))
  (let [minions (get-minions state player-id)]
    (reduce
      (fn [updated-state minion]
        (let [id (:id minion)
              sleepy (:sleepy minion)
              attack (:attack minion)
              can-attack (and (not sleepy) (> attack 0))
              enemy-ids (get-enemy-ids state player-id)]
          (println "Processing minion" id "=> sleepy:" sleepy ", attack:" attack ", can-attack computed as:" (if can-attack 1 0))
          (-> updated-state
              (update-minion id :can-attack (if can-attack 1 0))
              (update-minion id :valid-attack-ids
                             (if can-attack
                               (do
                                 (println "Setting valid-attack-ids for minion" id "to enemy ids:" enemy-ids)
                                 enemy-ids)
                               (do
                                 (println "Minion" id "cannot attack, empty valid-attack-ids")
                                 []))))))
      state                       ; use state as the initial accumulator
      minions)))                  ; reduce over the minions




(defn remove-can-attack
  "Sets can-attack for the minions of the given player-id to nil"
  [state player-id]
  (let [minions (get-minions state player-id)]              ; Fetch minions for the given player from the state
    (reduce
      (fn [updated-state minion]
        (update-minion updated-state (:id minion) :can-attack nil)
        )
      state
      minions)))

(defn reset-attacks-performed-this-turn
  "Resets attacks-performed-this-turn for the minions of the given player-id"
  [state player-id]
  (let [minions (get-minions state player-id)]
    (reduce
      (fn [updated-state minion]
        (update-minion updated-state (:id minion) :attacks-performed-this-turn 0)
        )
      state
      minions)))


(defn get-hero-mana
  "Returns the hero's mana"
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (get-hero-mana "p1"))
                10))}
  [state player-id]
  (get-in state [:players player-id :hero :mana]))

(defn update-hero-mana
  "Updates the mana of the hero for the given player to either a specified value or applies a function to the current mana."
  {:test (fn []
           ;; Test setting a fixed value
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (update-hero-mana "p1" 4)
                    (get-hero-mana "p1"))
                4)
           ;; Test using a function to modify the current mana
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1" :mana 10)}])
                    (update-hero-mana "p1" #(- % 2))
                    (get-hero-mana "p1"))
                8))}
  [state player-id function-or-value]
  (let [current-mana (get-in state [:players player-id :hero :mana])
        new-mana (if (fn? function-or-value) (function-or-value current-mana) function-or-value)]
    (-> state
        (assoc-in [:players player-id :hero :mana] new-mana))))

(defn hero-can-use-power?
  [state player-id hero]
  (let [hero-mana (get-hero-mana state player-id)]
    (and (<= (:mana-cost (:hero-power hero) hero) hero-mana)
         (= (:player-id-in-turn state) player-id)
         (= (:has-used-your-turn hero) false)
         )))

(defn get-valid-target-ids
  "Returns a collection of valid target IDs based on the card or hero's target definitions."
  [state player-id hero-or-card]
  (let [definition (get-definition (:name hero-or-card))
        hero-ids [(get-in (get-hero-player-id state "p1") [:id])
                  (get-in (get-hero-player-id state "p2") [:id])]
        minion-ids (map :id (get-minions state))
        enemy-player-id (if (= player-id "p1") "p2" "p1")
        enemy-minion-ids (map :id
                              (filter #(not (some #{:stealth} (:states %)))
                                      (get-minions state enemy-player-id)))
        friendly-minion-ids (map :id (get-minions state player-id))
        stealth-minion-ids (map :id
                                (filter #(some #{:stealth} (:states %))
                                        (get-minions state)))]
    (cond-> []
            (some #{:all-heroes} (:target definition)) (into hero-ids)
            (some #{:all-minions} (:target definition)) (into (concat friendly-minion-ids enemy-minion-ids))
            (some #{:friendly-minions} (:target definition)) (into friendly-minion-ids)
            (some #{:stealth-minions} (:target definition)) (into stealth-minion-ids))))


(defn get-attackable-ids
  "Returns the valid attackable ids for the given player-id, excluding minions with :stealth not nil. Test in cardtests.cljc"
  [state player-id]
  (let [minion-ids (reduce
                     (fn [acc minion]
                       (if (not (some #(= % :stealth) (:states minion)))
                         (conj acc (:id minion))
                         acc))
                     []
                     (get-minions state (if (= player-id "p1") "p2" "p1")))]
    (concat minion-ids
            [(:id (get-hero-player-id state (if (= player-id "p1") "p2" "p1")))])))

(defn is-card-playable?
  "Returns true if the given card is playable for the specified player, else false."
  {:test (fn []
           (let [state (-> (create-game [{:hand [(create-minion "Sheep" :id "m1")]}
                                         {:hand [(create-minion "Sheep" :id "m2")]}]))
                 hand (get-hand state "p1")
                 sheep (first hand)]
             (is= (is-card-playable? state "p2" sheep) false) ; can't play a minion that doesn't belong to us
             )
           )}
  [state player-id card]
  (let [hero-mana (get-hero-mana state player-id)
        minion-count (count (get-minions state player-id))
        current-secrets (get-secrets state player-id)]
    (and (<= (:mana-cost card) hero-mana)
         (< minion-count 7)
         (= (:owner-id card) player-id)
         (= (:player-id-in-turn state) player-id)
         (or (empty? current-secrets)
             (not (some #(= (:name %) (:name card)) current-secrets))))))
