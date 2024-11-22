(ns firestone.client.kth.endpoints
  (:require [clojure.data.json :as json]
            [firestone.construct :refer [create-game]]
            [firestone.client.kth.mapper :refer [game->client-game]]))

(def cors-headers {"Access-Control-Allow-Origin"  "*"
                   "Access-Control-Allow-Methods" "*"})

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
            game-input (->> (:game body) ; Extract the :game key
                            (map (fn [player]
                                   {:hand (mapv str (or (:hand player) [])) ; Ensure :hand contains strings
                                    :deck (mapv str (or (:deck player) [])) ; Ensure :deck contains strings
                                    :board (mapv str (or (:board player) [])) ; Ensure :board contains strings
                                    :hero (or (:hero player) "Jaina Proudmoore")})))] ; Default hero
        (println "Transformed input for create-game:" game-input)
        {:status  200
         :headers (merge cors-headers {"Content-Type" "application/json"})
         :body    (-> (create-game game-input) ; Use transformed input
                      (game->client-game)
                      (vector)
                      (json/write-str))})


      :else
      {:status  200
       :headers {"Content-Type" "text/html"}
       :body    "<h1>hello</h1>"})))