(ns firestone.core
  "A namespace for the business logic of the game."
  (:require [ysera.test :refer [is is-not is=]]
            [ysera.error :refer [error]]
            [ysera.collections :refer [seq-contains?]]
            [firestone.definitions :refer [get-definition]]
            [firestone.construct :refer [create-game
                                         create-hero
                                         create-minion
                                         get-heroes
                                         get-minion
                                         get-minions
                                         create-card
                                         get-deck
                                         get-hand
                                         add-card-to-hand
                                         remove-card-from-deck
                                         handle-fatigue
                                         set-mana
                                         get-max-mana
                                         trigger-spell
                                         trigger-deathrattle
                                         update-minion
                                         add-minion-to-board
                                         update-hero]]))


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
  (-> (get-character state id)
      (:name)
      (get-definition)
      (:type)))


(defn get-owner
  "Return the id of the owner"
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "jp")}])
                    (get-owner "jp"))
                "p1")
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo")]}])
                    (get-owner "bo"))
                "p1"))}
  [state id]
  {:pre [(map? state) (string? id)]}
  (let [entity-type (get-entity-type state id)]
    (case entity-type
      :minion (:owner-id (get-minion state id))
      :hero (some (fn [[player-id player]]
                    (when (= (get-in player [:hero :id]) id)
                      player-id))
                  (:players state)))))


(defn get-damage-taken
  "Returns the damage taken by an entity (hero or minion) given its ID."
  {:test (fn []
           (is= (-> (create-game [{:hero (create-hero "Jaina Proudmoore" :id "jp" :damage-taken 25)}])
                    (get-damage-taken "jp"))
                25)
           (is= (-> (create-game [{:minions [(create-minion "Boulderfist Ogre" :id "bo" :damage-taken 2)]}])
                    (get-damage-taken "bo"))
                2))}
  [state id]
  {:pre [(map? state) (string? id)]}
  (let [entity-type (get-entity-type state id)
        owner-id (get-owner state id)]
    (case entity-type
      :minion (->> (get-in state [:players owner-id :minions])
                   (some #(when (= (:id %) id) (:damage-taken %))))
      :hero (get-in state [:players owner-id :hero :damage-taken]))))

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
    (and attacker
         target
         (= (:player-id-in-turn state) player-id)
         (< (:attacks-performed-this-turn attacker) 1)
         (not (sleepy? state attacker-id))
         (not= (:owner-id attacker) (:owner-id target)))))


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


;TODO - CREATE TESTS
(defn draw-card
  "Draws a card from the player's deck and adds it to the bottom of their hand.
   If the hand is full (10 cards), the drawn card is burned."
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

(defn remove-minion
  "Removes a minion with the given id from the state if its health is 0 or less, triggering its Deathrattle if applicable."
  {:test (fn []
           ; Remove a minion with 0 health
           (let [initial-state (create-game [{:minions [(create-minion "Test Minion" :id "m1" :owner-id "p1" :health 0)]}])
                 result-state (remove-minion initial-state "m1")]
             (is (empty? (get-in result-state [:players "p1" :minions])))
             (is-not (get-minion result-state "m1")))

           ; Attempt to remove a minion with positive health
           (let [initial-state (create-game [{:minions [(create-minion "Test Minion" :id "m2" :owner-id "p1" :health 1)]}])
                 result-state (remove-minion initial-state "m2")]
             (is (= 1 (count (get-in result-state [:players "p1" :minions]))))
             (is (get-minion result-state "m2"))))}
  [state id]
  (let [minion (get-minion state id)]
    (if (and minion (<= (get-health state id) 0))
      (let [owner-id (:owner-id minion)
            ; Check if the minion has a deathrattle ability
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

;TODO: Handle stealth
(defn handle-minion-attack-on-minion
  "Handles the attack logic when a minion attacks another minion."
  [state attacker-id target-id]
  (as-> state $
        ;; Apply damage to the target and attacker minion
        (update-minion $ target-id :damage-taken #(+ % (get-attack state attacker-id)))
        (update-minion $ attacker-id :damage-taken #(+ % (get-attack state target-id)))
        ;; Remove attacker minion if its health is zero or less
        (remove-minion $ attacker-id)
        ;; Remove target minion if its health is zero or less
        (remove-minion $ target-id)
        ;; Mark the attacker as having attacked this turn
        (mark-minion-attacked $ attacker-id)))


;TODO: Handle stealth
(defn handle-minion-attack-on-hero
  "Handles the attack logic when a minion attacks a hero."
  [state attacker-id target-id]
  (as-> (update-hero state target-id :damage-taken #(+ % (get-attack state attacker-id))) $
        (mark-minion-attacked $ attacker-id)))

(defn attack
  "Allows a minion to attack another minion or a hero after validating the attack."
  [state player-id attacker-id target-id]
  (if (valid-attack? state player-id attacker-id target-id)
    (let [type (get-entity-type state target-id)]
      (cond
        (= type :minion) (handle-minion-attack-on-minion state attacker-id target-id)
        (= type :hero)   (handle-minion-attack-on-hero state attacker-id target-id)
        :else            (error "Type of the card is unrecognized")))
    (error "Invalid attack")))


(defn battlecry
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
  (case (:name minion)
    "Silver Hand Knight"
    (let [owner-id (:owner-id minion)]
      (add-minion-to-board state owner-id (create-minion "Squire") (inc (:position minion))))
    "Mad Bomber"
    (let [owner-id (:owner-id minion)
          enemy-id (if (= owner-id "p1") "p2" "p1")
          all-targets (concat (get-minions state "p1") (get-minions state "p2") [{:owner-id "p1" :entity-type :hero} {:owner-id "p2" :entity-type :hero}])
          damage-distribution (take 3 (repeatedly #(rand-nth all-targets)))]
      (reduce (fn [acc-state target]
                (if (= (:entity-type target) :hero)
                  (update-in acc-state [:players (:owner-id target) :hero :damage-taken] + 1)
                  (update-in acc-state [:players (:owner-id target) :minions]
                             (fn [minions]
                               (mapv (fn [m] (if (= (:id m) (:id target)) (update m :damage-taken + 1) m)) minions)))))
              state
              damage-distribution))
    "Stampeding Kodo"
    (let [enemy-id (if (= (:owner-id minion) "p1") "p2" "p1")
          enemy-minions (get-minions state enemy-id)
          target (some #(when (<= (:attack %) 2) %) enemy-minions)]
      (if target
        (remove-minion state (:id target))
        state))
    "Twilight Drake"
    (let [owner-id (:owner-id minion)
          health-bonus (count (get-hand state owner-id))]
      (update-in state [:players owner-id :minions] #(mapv (fn [m] (if (= (:id m) (:id minion)) (update m :health + health-bonus) m)) %)))
    "Dr. Boom"
    (let [owner-id (:owner-id minion)
          current-minion-count (count (get-minions state owner-id))
          ; Ensure we don't exceed the max minion count (usually 7)
          max-minions 7
          space-left (- max-minions current-minion-count)]
      (if (>= space-left 2)
        (-> state
            (add-minion-to-board owner-id (create-minion "Boom Bot") (inc (:position minion)))
            (add-minion-to-board owner-id (create-minion "Boom Bot") (+ 2 (:position minion))))
        state))
    state))