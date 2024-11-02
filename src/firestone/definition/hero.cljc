(ns firestone.definition.hero
  (:require [firestone.definitions :refer [add-definitions!]]))

(def hero-definitions
  {

   "Jaina Proudmoore"
   {:name       "Jaina Proudmoore"
    :health     30
    :class      :mage
    :type       :hero
    :hero-power "Fireblast"}

   "Gul'dan"
   {:name       "Gul'dan"
    :health     30
    :class      :warlock
    :type       :hero
    :hero-power "Life Tap"}

   })

(add-definitions! hero-definitions)