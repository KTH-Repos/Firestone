(ns firestone.definition.hero
  (:require [firestone.definitions :refer [add-definitions!]]
            [firestone.core :refer [draw-card]]
            [firestone.construct :refer [get-minion
                                         get-hero
                                         update-hero
                                         update-minion
                                         get-player-id-by-hero-id]]))

(def hero-definitions
  {"Jaina Proudmoore"
   {:name        "Jaina Proudmoore"
    :health      30
    :class       :mage
    :entity-type :hero
    :hero-power  "Fireblast"}

   "Gul'dan"
   {:name        "Gul'dan"
    :health      30
    :class       :warlock
    :entity-type :hero
    :hero-power  "Life Tap"}})

(def hero-power-definitions
  {"Life Tap"
   {:name        "Life Tap"
    :mana-cost   2
    :class       :warlock
    :entity-type :hero-power
    :description "Draw a card and take 2 damage."
    :power       (fn [state player-id]
                   (-> state
                       (draw-card player-id)
                       (update-hero player-id :damage-taken #(+ % 2))))}

   "Fireblast"
   {:name        "Fireblast"
    :mana-cost   2
    :class       :mage
    :entity-type :hero-power
    :description "Deal 1 damage."
    :power (fn [state target-id]
             (let [target (or (get-minion state target-id)
                              (when-let [player-id (get-player-id-by-hero-id state target-id)]
                                (get-hero state player-id)))]
               (if target
                 (cond
                   (= (:entity-type target) :hero)
                   (let [player-id (get-player-id-by-hero-id state target-id)]
                     (update-hero state player-id :damage-taken #(+ % 1)))

                   (= (:entity-type target) :minion)
                   (update-minion state target-id :damage-taken #(+ % 1))

                   :else (do (println "Invalid target for Fireblast.") state))
                 (do (println "Target not found for Fireblast.") state))))}})

(add-definitions! hero-definitions)
(add-definitions! hero-power-definitions)