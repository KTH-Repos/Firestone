(ns firestone.core
  "A namespace for the business logic of the game."
  (:require [ysera.test :refer [is is-not is=]]
            [ysera.collections :refer [seq-contains?]]
            [ysera.error :refer [error]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [create-game
                                         create-hero
                                         create-minion
                                         get-heroes
                                         get-minion
                                         get-minions
                                         get-deck
                                         get-hand
                                         add-card-to-hand
                                         remove-card-from-deck
                                         handle-fatigue
                                         set-mana
                                         get-max-mana
                                         trigger-spell
                                         handle-minion-attack-on-minion
                                         handle-minion-attack-on-hero]]))


(defn get-character
  "Returns the character with the given id from the state."
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (get-character "h1")
                    (:name))
                "Jaina Proudmoore")
           (is= (-> (create-game [{:minions [(create-minion "Sheep" :id "m")]}])
                    (get-character "m")
                    (:name))
                "Sheep"))}
  [state id]
  (or (some (fn [m] (when (= (:id m) id) m))
            (get-minions state))
      (some (fn [h] (when (= (:id h) id) h))
            (get-heroes state))))

(defn get-entity-type
  "Returns the damage taken by an entity (hero or minion) given its ID."
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (get-entity-type "h1"))
                :hero))}
  [state id]
  (let [entity (get-character state id)]
    (cond
      (= (:entity-type entity) :hero) :hero
      (= (:entity-type entity) :minion) :minion
      :else (error "Unknown entity type"))))

(defn get-health
  "Returns the health of the character."
  {:test (fn []
           ; Uninjured minion
           (is= (-> (create-minion "Sheep")
                    (get-health))
                1)
           ; Injured minion
           (is= (-> (create-minion "Boulderfist Ogre" :damage-taken 1)
                    (get-health))
                6)
           ; Minion in a state
           (is= (-> (create-game [{:minions [(create-minion "Sheep" :id "m")]}])
                    (get-health "m"))
                1)
           ; Uninjured hero
           (is= (-> (create-hero "Jaina Proudmoore")
                    (get-health))
                30)
           ; Injured hero
           (is= (-> (create-hero "Jaina Proudmoore" :damage-taken 2)
                    (get-health))
                28)
           ; Hero in a state
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "h1")}])
                    (get-health "h1"))
                30))}
  ([character]
   {:pre [(map? character) (contains? character :damage-taken)]}
   (let [definition (get-definition character)]
     (- (:health definition) (:damage-taken character))))
  ([state id]
   (get-health (get-character state id))))


(defn get-attack
  "Returns the attack of the minion with the given id."
  {:test (fn []
           (is= (-> (create-game [{:minions [(create-minion "Sheep" :id "m")]}])
                    (get-attack "m"))
                1))}
  [state id]
  (let [minion (get-minion state id)
        definition (get-definition minion)]
    (:attack definition)))


(defn sleepy?
  "Checks if the minion with given id is sleepy."
  {:test (fn []
           (is (-> (create-game [{:minions [(create-minion "Sheep" :id "m")]}]
                                :minion-ids-summoned-this-turn ["m"])
                   (sleepy? "m")))
           (is-not (-> (create-game [{:minions [(create-minion "Sheep" :id "m")]}])
                       (sleepy? "m"))))}
  [state id]
  (seq-contains? (:minion-ids-summoned-this-turn state) id))


(defn valid-attack?
  "Checks if the attack is valid"
  {:test (fn []
           ; Should be able to attack an enemy minion
           (is (-> (create-game [{:minions [(create-minion "Sheep" :id "m1")]}
                                 {:minions [(create-minion "Boulderfist Ogre" :id "m2")]}])
                   (valid-attack? "p1" "m1" "m2")))
           ; Should be able to attack an enemy hero
           (is (-> (create-game [{:minions [(create-minion "Sheep" :id "m")]}])
                   (valid-attack? "p1" "m" "h2")))
           ; Should not be able to attack your own minions
           (is-not (-> (create-game [{:minions [(create-minion "Sheep" :id "m1")
                                                (create-minion "Boulderfist Ogre" :id "m2")]}])
                       (valid-attack? "p1" "m1" "m2")))
           ; Should not be able to attack if it is not your turn
           (is-not (-> (create-game [{:minions [(create-minion "Sheep" :id "m1")]}
                                     {:minions [(create-minion "Boulderfist Ogre" :id "m2")]}]
                                    :player-id-in-turn "p2")
                       (valid-attack? "p1" "m1" "m2")))
           ; Should not be able to attack if you are sleepy
           (is-not (-> (create-game [{:minions [(create-minion "Sheep" :id "m1")]}
                                     {:minions [(create-minion "Boulderfist Ogre" :id "m2")]}]
                                    :minion-ids-summoned-this-turn ["m1"])
                       (valid-attack? "p1" "m" "bo")))
           ; Should not be able to attack if you already attacked this turn
           (is-not (-> (create-game [{:minions [(create-minion "Sheep" :id "m1" :attacks-performed-this-turn 1)]}
                                     {:minions [(create-minion "Boulderfist Ogre" :id "m2")]}])
                       (valid-attack? "p1" "m1" "m2"))))}
  [state player-id attacker-id target-id]
  (let [attacker (get-minion state attacker-id)
        target (get-character state target-id)]
    (println "Attacker:" attacker)
    (and (do (println "Attacker exists:" (boolean attacker)) attacker)
         (do (println "Target exists:" (boolean target)) target)
         (do (println "Correct player's turn:" (= (:player-id-in-turn state) player-id))
             (= (:player-id-in-turn state) player-id))
         (do (println "Attacks performed:" (:attacks-performed-this-turn attacker))
             (< (:attacks-performed-this-turn attacker) 1))
         (do (println "Not sleepy:" (not (sleepy? state attacker-id)))
             (not (sleepy? state attacker-id)))
         (do (println "Target not owned by attacker:" (not= (:owner-id target) (:player-id-in-turn state)))
             (not= (:owner-id target) (:player-id-in-turn state)))
         (do (println "Attacker and target have different owners:" (not= (:owner-id attacker) (:owner-id target)))
             (not= (:owner-id attacker) (:owner-id target))))))

(defn draw-card
  {:test (fn []
           (let [state (-> (create-game [{:deck ["Boulderfist Ogre"]}])
                           (draw-card "p1"))]
             (is (empty? (get-deck state "p1")))
             (is= (->> (get-hand state "p1")
                       (map :name))
                  ["Boulderfist Ogre"]))
           ; Test fatigue damage when the deck is empty
           (let [state (-> (create-game [{:deck [] :hero (create-hero "Jaina Proudmoore" :id "h1" :damage-taken 0 :fatigue 0)}])
                           (draw-card "p1"))]
             (is= (get-in state [:players "p1" :hero :damage-taken]) 1)
             (is= (get-in state [:players "p1" :hero :fatigue]) 1))
           ; Test consecutive fatigue
           (let [state (-> (create-game [{:deck [] :hero (create-hero "Jaina Proudmoore" :id "h1" :damage-taken 0 :fatigue 1)}])
                           (draw-card "p1"))]
             (is= (get-in state [:players "p1" :hero :damage-taken]) 2)
             (is= (get-in state [:players "p1" :hero :fatigue]) 2))
           )}
  [state player-id]
  (let [deck (get-in state [:players player-id :deck])
        hand (get-in state [:players player-id :hand])]
    (if (not (empty? deck))
      (let [drawn-card (first deck)
            new-deck (rest deck)
            new-hand (if (>= (count hand) 10)
                       hand  ; Hand is full, card is burned
                       (conj (vec hand) drawn-card))]  ; Add card to the hand
        (-> state
            (assoc-in [:players player-id :deck] new-deck)
            (assoc-in [:players player-id :hand] new-hand)))
      state)))


(defn refresh-mana
  "Refreshes the player's mana to the maximum value."
  {:test (fn []

           (let [initial-state (-> (create-game [{:hero (create-hero "Jaina Proudmoore")}])
                                   (assoc-in [:players "p1" :max-mana] 10)  ; Set max mana to 10
                                   (set-mana "p1" 3))       ; Set mana to 3

                 refreshed-state (refresh-mana initial-state "p1")]        ; Refresh mana for player "p1"

             ;; Ensure the player's mana is refreshed to max-mana.
             (is= (get-in refreshed-state [:players "p1" :mana]) 10)))}
  [state player-id]
  (let [value (get-max-mana state player-id)]
    (set-mana state player-id value)))

(defn attack
  "Allows a minion to attack another minion or a hero after validating the attack."
  [state player-id attacker-id target-id]
  {:pre [(map? state)                     ; Ensure state is a map
         (string? player-id)              ; Ensure player-id is a string
         (string? attacker-id)            ; Ensure attacker-id is a string
         (string? target-id)]}            ; Ensure target-id is a string
  ;(println "Game state: " state)
  (if (valid-attack? state player-id attacker-id target-id)
    (let [type (get-entity-type state target-id)]
      (cond
        (= type :minion) (handle-minion-attack-on-minion state attacker-id target-id)
        (= type :hero)   (handle-minion-attack-on-hero state attacker-id target-id)
        :else            (error "card doesn't exist")))
    (error "Invalid attack")))

