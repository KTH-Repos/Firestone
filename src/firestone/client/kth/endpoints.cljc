(ns firestone.client.kth.endpoints
  (:require [clojure.data.json :as json]
            [firestone.construct :refer [create-game]]
            [firestone.client.kth.mapper :refer [game->client-game]]))

(def cors-headers {"Access-Control-Allow-Origin"  "*"
                   "Access-Control-Allow-Methods" "*"})

(defn transform-client-input
  "Transforms the client input to the format expected by create-game."
  [client-input]
  (let [game-data (or client-input [])] ; Extract `:game` or default to an empty array
    {:game (mapv (fn [player]
                   {:hand     (mapv #(if (string? %) % (str %)) (get player :hand [])) ; Validate strings
                    :deck     (mapv #(if (string? %) % (str %)) (get player :deck []))
                    :minions  (mapv #(if (string? %) % (str %)) (get player :board [])) ; Convert `:board` to `:minions`
                    :hero     (get player :hero "Jaina Proudmoore") ; Default hero
                    :mana     10                                   ; Default mana
                    :max-mana 10})                                ; Default max mana
                 game-data)}))

(defn handler
  [request]
  (let [uri (:uri request)
        request-method (:request-method request)]
    (when-not (= uri "/favicon.ico")
      (clojure.pprint/pprint request))

    (cond
      (= request-method :options)
      {:status 204}

      (= uri "/engine-settings")
      {:status  200
       :headers (merge cors-headers {"Content-Type" "application/json"})
       :body    (json/write-str {:supports-redo false
                                 :supports-undo false
                                 :audio         :auto})}

      (= uri "/create-game")
      (let [body (when (:body request)
                   (-> (:body request)
                       (slurp)
                       (json/read-str :key-fn keyword)))
            transformed-game-input (transform-client-input (:game body))] ; Transform input
        (println "Transformed input for create-game:" transformed-game-input) ; Debugging log
        {:status  200
         :headers (merge cors-headers {"Content-Type" "application/json"})
         :body    (-> (create-game transformed-game-input) ; Use transformed input
                      (game->client-game)
                      (vector)
                      (json/write-str))})

      :else
      {:status  200
       :headers {"Content-Type" "text/html"}
       :body    "<h1>hello</h1>"})))