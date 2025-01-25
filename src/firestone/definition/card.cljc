(ns firestone.definition.card
  (:require [firestone.definitions :refer [add-definitions!]]
            [firestone.core :refer [draw-card]]
            [firestone.construct :refer [create-minion
                                         get-minions
                                         add-minion-to-board
                                         remove-minion
                                         get-hand
                                         get-hero
                                         get-minion
                                         update-minion]]))

(def card-definitions
  {

   "Boulderfist Ogre"
   {:name      "Boulderfist Ogre"
    :attack    6
    :health    7
    :mana-cost 6
    :type      :minion
    :set       :basic}

   "Leper Gnome"
   {:name        "Leper Gnome"
    :attack      1
    :health      1
    :mana-cost   1
    :type        :minion
    :set         :classic
    :rarity      :common
    :deathrattle (fn [state minion]
                   (let [enemy-id (if (= (:owner-id minion) "p1") "p2" "p1")]
                     (update-in state [:players enemy-id :hero :damage-taken] + 2)))
    :description "Deathrattle: Deal 2 damage to the enemy hero."}

   "Loot Hoarder"
   {:name        "Loot Hoarder"
    :attack      2
    :health      1
    :mana-cost   2
    :type        :minion
    :set         :classic
    :rarity      :common
    :deathrattle (fn [state & {player-id :player-id}]
                   (draw-card state player-id))
    :description "Deathrattle: Draw a card."}

   "Sheep"
   {:name      "Sheep"
    :attack    1
    :health    1
    :mana-cost 1
    :race      :beast
    :type      :minion
    :set       :basic}

   "Moroes"
   {:name        "Moroes"
    :attack      1
    :health      1
    :mana-cost   3
    :ability     :stealth
    :description "Stealth. At the end of your turn, summon a 1/1 Steward."
    :rarity      :legendary
    :set         :one-night-in-karazhan
    :type        :minion}

   "Consecration"
   {:name        "Consecration"
    :mana-cost   4
    :description "Deal 2 damage to all enemies."
    :class       :paladin
    :set         :basic
    :type        :spell}

   "Kill Command"
   {:name        "Kill Command"
    :mana-cost   3
    :class       :hunter
    :set         :basic
    :type        :spell
    :description "Deal 3 damage. If you control a Beast deal 5 damage instead."}

   "Silver Hand Knight"
   {:name        "Silver Hand Knight"
    :attack      4
    :health      4
    :mana-cost   5
    :ability     :battlecry
    :description "Battlecry: Summon a 2/2 Squire."
    :rarity      :common
    :set         :classic
    :type        :minion
    :battlecry   (fn [state & {:keys [player-id]}]
                   (let [squire (create-minion "Squire")]
                     (add-minion-to-board state player-id squire (count (get-minions state player-id)))))}

   "Abusive Sergeant"
   {:name        "Abusive Sergeant"
    :attack      1
    :health      1
    :mana-cost   1
    :rarity      :common
    :set         :classic
    :type        :minion
    :description "Battlecry: Give a minion +2 Attack this turn."

    :battlecry   (fn [state & {:keys [target-id]}]
                   (print "target-id is following: " target-id)
                   (if (nil? target-id)
                     ;; No target chosen => do nothing
                     state
                     ;; Otherwise, buff the targeted minion
                     (let [minion (get-minion state target-id)]
                       (-> state
                           (update-minion (:id minion) :attack #(+ % 2))))))}

"Stampeding Kodo"
   {:name        "Stampeding Kodo"
    :attack      3
    :mana-cost   5
    :health      5
    :ability     :battlecry
    :description "Battlecry: Destroy a random enemy minion with 2 or less Attack."
    :type        :minion
    :race        :beast
    :set         :classic
    :rarity      :rare
    :battlecry   (fn [state & {:keys [player-id]}]
                   (let [enemy-id (if (= player-id "p1") "p2" "p1")
                         eligible-minions (filter #(<= (:attack %) 2) (get-minions state enemy-id))]
                     (if (seq eligible-minions)
                       (let [current-seed (get state :seed 1234)
                             selected-index (mod (rand-int current-seed) (count eligible-minions))
                             target (nth eligible-minions selected-index)
                             new-seed (inc current-seed)]
                         (-> state
                             (remove-minion (:id target))
                             (assoc :seed new-seed)))
                       state)))}

   "Mad Bomber"
   {:name        "Mad Bomber"
    :attack      3
    :health      2
    :mana-cost   2
    :ability     :battlecry
    :description "Battlecry: Deal 3 damage randomly split between all other characters."
    :rarity      :common
    :set         :classic
    :type        :minion
    :battlecry   (fn [state minion]
                   (let [player-id (:owner-id minion)
                         enemy-id (if (= player-id "p1") "p2" "p1")
                         eligible-targets (concat (get-minions state enemy-id)
                                                  [(get-hero state enemy-id)]
                                                  (filter #(not= (:id %) (:id minion)) (get-minions state player-id))
                                                  [(get-hero state player-id)])
                         current-seed (get state :seed 1234)]
                     (-> (reduce (fn [s _]
                                   (if (seq eligible-targets)
                                     (let [selected-index (mod (rand-int current-seed) (count eligible-targets))
                                           target (nth eligible-targets selected-index)
                                           new-s (if (= (:entity-type target) :hero)
                                                   (update-in s [:players (:owner-id target) :hero :damage-taken] inc)
                                                   (update-minion s (:id target) :damage-taken inc))]
                                       (update new-s :seed inc))
                                     s))
                                 state
                                 (range 3))
                         (update :seed #(+ % 3)))))}

   "Twilight Drake"
   {:name        "Twilight Drake"
    :attack      4
    :health      1
    :mana-cost   4
    :ability     :battlecry
    :description "Battlecry: Gain +1 Health for each card in your hand."
    :race        :dragon
    :type        :minion
    :set         :classic
    :rarity      :rare
    :battlecry   (fn [state minion]
                   (let [player-id (:owner-id minion)
                         minion-id (:id minion)
                         hand-size (count (get-hand state player-id))]
                     (update-minion state minion-id :health #(+ % hand-size))))}

   "Dr. Boom"
   {:name        "Dr. Boom"
    :attack      7
    :health      7
    :mana-cost   7
    :ability     :battlecry
    :description "Battlecry: Summon two 1/1 Boom Bots. WARNING: Bots may explode."
    :rarity      :legendary
    :set         :goblins-vs-gnomes
    :type        :minion
    :battlecry   (fn [state minion]
                   (let [player-id (:owner-id minion)
                         minion-position (:position minion)
                         ;; Create two Boom Bots
                         boom-bot-1 (create-minion "Boom Bot")
                         boom-bot-2 (create-minion "Boom Bot")
                         ;; Calculate positions for the Boom Bots
                         position-1 (inc minion-position)
                         position-2 (inc position-1)]
                     ;; Add Boom Bots to the board
                     (-> state
                         (add-minion-to-board player-id boom-bot-1 position-1)
                         (add-minion-to-board player-id boom-bot-2 position-2))))}

   "Blood Imp"
   {:name        "Blood Imp"
    :attack      0
    :health      1
    :mana-cost   1
    :ability     :stealth
    :description "Stealth. At the end of your turn give another random friendly minion +1 Health."
    :race        :demon
    :type        :minion
    :class       :warlock
    :set         :classic
    :rarity      :common}

   "Equality"
   {:name        "Equality"
    :mana-cost   2
    :description "Change the Health of ALL minions to 1."
    :class       :paladin
    :set         :classic
    :type        :spell}

   "Feign Death"
   {:name        "Feign Death"
    :mana-cost   2
    :description "Trigger all Deathrattles on your minions."
    :class       :hunter
    :rarity      :epic
    :set         :goblins-vs-gnomes
    :type        :spell}

   "Squire"
   {:name      "Squire"
    :attack    2
    :health    2
    :mana-cost 1
    :set       :classic
    :rarity    :common
    :type      :minion}

   "Boom Bot"
   {:name        "Boom Bot"
    :attack      1
    :health      1
    :mana-cost   1
    :type        :minion
    :race        :mech
    :set         :goblins-vs-gnomes
    :deathrattle (fn [state minion]
                   (let [enemy-id (if (= (:owner-id minion) "p1") "p2" "p1")
                         enemy-minions (get-minions state enemy-id)]
                     (if (seq enemy-minions)
                       (let [target-minion (rand-nth enemy-minions)
                             damage (rand-int 4)]
                         (update-in state [:players enemy-id :minions]
                                    (mapv (fn [m]
                                            (if (= (:id m) (:id target-minion))
                                              (update m :health - damage)
                                              m))
                                          enemy-minions)))
                       state)))
    :description "Deathrattle: Deal 1-4 damage to a random enemy."}

   "Steward"
   {:name          "Steward"
    :attack        1
    :health        1
    :mana-cost     1
    :type          :minion
    :set           :one-night-in-karazhan}

   "Shadow Sensei"
   {:name        "Shadow Sensei"
    :attack      4
    :health      4
    :mana-cost   4
    :type        :minion
    :class       :rogue
    :set         :mean-streets-of-gadgetzan
    :rarity      :rare
    :description "Battlecry: Give a Stealthed minion +2/+2."
    :battlecry   (fn [state player-id minion]
                   (let [board-path [:players player-id :board]
                         board (get-in state board-path)]
                     (if-let [stealth-index (->> board
                                                 (map-indexed vector)
                                                 (some (fn [[i m]]
                                                         (when (= (:ability m) :stealth)
                                                           i))))]
                       ;; Found a stealthed minion at stealth-index. Update its stats.
                       (update-in state (conj board-path stealth-index)
                                  (fn [m]
                                    (-> m
                                        (update :attack + 2)
                                        (update :health + 2))))
                       ;; No stealthed minion found; return state unchanged.
                       state)))}

   "Alexstrasza"
   {:name        "Alexstrasza"
    :attack      8
    :mana-cost   9
    :health      8
    :race        :dragon
    :type        :minion
    :set         :classic
    :rarity      :legendary
    :description "Battlecry: Set a hero's remaining Health to 15."
    :battlecry   (fn [state player-id]
                   (let [enemy-id (if (= player-id "p1") "p2" "p1")]
                     (assoc-in state [:players enemy-id :hero :health] 15)))}

   "Knife Juggler"
   {:name        "Knife Juggler"
    :attack      3
    :health      2
    :mana-cost   2
    :rarity      :rare
    :set         :classic
    :type        :minion
    :description "After you summon a minion, deal 1 damage to a random enemy."
    :on-minion-summon (fn [state player-id]
                        (let [enemy-id (if (= player-id "p1") "p2" "p1")
                              enemy-hero (get-in state [:players enemy-id :hero])
                              enemy-board (get-in state [:players enemy-id :board])

                              ;; Targets: enemy hero + all enemy minions
                              possible-targets (concat
                                                 [{:entity-type :hero :owner-id enemy-id}]
                                                 (map (fn [m] (assoc m :entity-type :minion)) enemy-board))

                              current-seed (get state :seed 1234)

                              ;; Determine the index of the chosen target using the seed
                              selected-index (mod (rand-int current-seed) (count possible-targets))
                              target (nth possible-targets selected-index)]

                          (-> (if (= (:entity-type target) :hero)
                                ;; Deal 1 damage to the enemy hero
                                (update-in state [:players (:owner-id target) :hero :damage-taken]
                                           (fnil inc 0))
                                ;; Deal 1 damage to an enemy minion
                                (let [target-id (:id target)
                                      updated-board (mapv (fn [m]
                                                            (if (= (:id m) target-id)
                                                              (update m :damage-taken (fnil inc 0))
                                                              m))
                                                          enemy-board)]
                                  (assoc-in state [:players enemy-id :board] updated-board)))
                              ;; Increment the seed so the next random choice differs
                              (update :seed inc))))}

   "Questing Adventurer"
   {:name        "Questing Adventurer"
    :attack      2
    :health      2
    :mana-cost   3
    :rarity      :rare
    :set         :classic
    :type        :minion
    :description "Whenever you play a card, gain +1/+1."
    :on-minion-summon (fn [state player-id played-card]
                        ;; For every Questing Adventurer on the player's board, increase its attack and health by 1.
                        (update-in state [:players player-id :board]
                                   (fn [board]
                                     (mapv (fn [m]
                                             (if (= (:name m) "Questing Adventurer")
                                               (-> m
                                                   (update :attack + 1)
                                                   (update :health + 1))
                                               m))
                                           board))))}

   "Dire Wolf Alpha"
   {:name        "Dire Wolf Alpha"
    :attack      2
    :health      2
    :mana-cost   2
    :set         :classic
    :race        :beast
    :type        :minion
    :rarity      :common
    :description "Adjacent minions have +1 Attack."
    :on-minion-summon (fn [state player-id]
                        (let [board-path [:players player-id :board]
                              board (get-in state board-path)]
                          (reduce (fn [acc-state [i m]]
                                    (if (= (:name m) "Dire Wolf Alpha")
                                      (let [left-i (dec i)
                                            right-i (inc i)
                                            ;; Increase attack of left neighbor if it exists
                                            acc-state (if (and (>= left-i 0) (< left-i (count board)))
                                                        (update-in acc-state (conj board-path left-i :attack) inc)
                                                        acc-state)
                                            ;; Increase attack of right neighbor if it exists
                                            acc-state (if (and (>= right-i 0) (< right-i (count board)))
                                                        (update-in acc-state (conj board-path right-i :attack) inc)
                                                        acc-state)]
                                        acc-state)
                                      acc-state))
                                  state
                                  (map-indexed vector board))))}
   })

(add-definitions! card-definitions)