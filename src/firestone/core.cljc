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
                                         handle-minion-attack-on-hero
                                         can-play-minion?
                                         deduct-player-mana
                                         remove-card-from-hand
                                         put-card-on-board
                                         update-minion
                                         get-player-id-in-turn
                                         trigger-battlecry]]))


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

(defn play-spell
  "Applies the effects of playing a given spell card."
  [state player-id card]
  (let [enemy-id (if (= player-id "p1") "p2" "p1")]
    (cond
      ;; Consecration: Deal 2 damage to all enemies (opponent's hero and all their minions).
      (= (:name card) "Consecration")
      (-> state
          ;; Damage enemy hero by 2
          (update-in [:players enemy-id :hero :damage-taken] (fnil + 0) 2)
          ;; Damage each enemy minion by 2
          (update-in [:players enemy-id :board]
                     (fn [board]
                       (mapv #(update % :damage-taken (fnil + 0) 2) board))))

      ;; Equality: Change the Health of ALL minions to 1.
      (= (:name card) "Equality")
      (letfn [(set-minion-health-to-one [board]
                (mapv (fn [m]
                        (let [max-health (-> (get-definition (:name m)) :health)]
                          ;; Adjust damage-taken so current health = 1
                          (assoc m :damage-taken (max 0 (dec max-health)))))
                      board))]
        (-> state
            (update-in [:players "p1" :board] set-minion-health-to-one)
            (update-in [:players "p2" :board] set-minion-health-to-one)))

      ;; Feign Death: Trigger all Deathrattles on your minions.
      (= (:name card) "Feign Death")
      (let [player-minions (get-in state [:players player-id :board])
            with-deathrattles (filter :deathrattle player-minions)]
        (reduce (fn [acc-state minion]
                  ((:deathrattle minion) acc-state minion))
                state
                with-deathrattles))

      :else state)))


(defn play-card
  "Handles playing a card by its type (minion or spell). Ensures that it is the player's turn,
   that the card is in hand, deducts mana, and handles end-turn related updates (e.g., removing sleepy)."
  [state player-id card-id position]
  ;; 1) Verify it is the player's turn.
  (if (not= (get-player-id-in-turn state) player-id)
    (error "It's not the player's turn.")
    (let [hand (get-in state [:players player-id :hand])
          card (first (filter #(= (:id %) card-id) hand))]
      (if (nil? card)
        (error "The player does not have the specified card in their hand.")

        (cond
          ;; 2) If this is a MINION card:
          (= (:type card) :minion)
          (if (can-play-minion? state player-id card)
            (let [mana-cost         (-> (get-definition (:name card)) :mana-cost)
                  state-after-play  (-> state
                                        ;; Deduct the mana
                                        (deduct-player-mana player-id mana-cost)
                                        ;; Remove the card from hand
                                        (remove-card-from-hand player-id card)
                                        ;; Put the card on board (creates a minion with :sleepy true)
                                        (put-card-on-board player-id card position))
                  ;; Grab the newly placed minion from the board
                  new-minions      (get-minions state-after-play player-id)
                  new-minion       (if (seq new-minions)
                                     (last new-minions)
                                     (error "No minion found on board after playing card"))
                  ;; Trigger battlecry if present
                  state-after-bc   (trigger-battlecry state-after-play new-minion)]
              ;; Mark that this minion was summoned THIS turn
              (-> state-after-bc
                  (update :minion-ids-summoned-this-turn conj (:id new-minion))))
            (error "Cannot play the minion card (e.g., insufficient mana or full board)."))

          ;; 3) If this is a SPELL card:
          (= (:type card) :spell)
          (-> state
              (deduct-player-mana player-id (-> (get-definition (:name card))
                                                :mana-cost))
              (remove-card-from-hand player-id card)
              (play-spell player-id card)
              (draw-card player-id))

          ;; 4) Otherwise: unrecognized card type
          :else
          (error "Unknown card type."))))))





(defn get-valid-attacks
  "Updates valid attack targets for each minion of the player in turn.
   Logs the computed valid targets."
  [state]
  (let [player-id-in-turn (get state :player-id-in-turn)
        player-change-fn {"p1" "p2"
                          "p2" "p1"}
        opponent-id (player-change-fn player-id-in-turn)
        opponent-minions (get-minions state opponent-id)
        opponent-minion-ids (map :id (filter (fn [m] (not (:stealth m))) opponent-minions))
        hero-id (get-in state [:players opponent-id :hero :id])]
    (println "Entering get-valid-attacks for player in turn:" player-id-in-turn)
    (println "Opponent id:" opponent-id ", opponent minion ids:" opponent-minion-ids ", hero id:" hero-id)
    (reduce
      (fn [new-state minion]
        (let [minion-id (:id minion)
              valid-ids (conj opponent-minion-ids hero-id)]
          (println "Setting valid-attack-ids for minion" minion-id "to:" valid-ids)
          (update-minion new-state minion-id :valid-attack-ids valid-ids)))
      state
      (get-minions state player-id-in-turn))))