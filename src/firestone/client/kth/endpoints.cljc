(ns firestone.client.kth.endpoints
  (:require [clojure.data.json :as json]
            [firestone.client.kth.edn-api :refer [create-game! end-turn! play-minion-card!]]
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
                       slurp
                       (json/read-str :key-fn keyword)))
            players-data (:game body) ; Just extract the :game key
            result (create-game! players-data)]
        {:status  200
         :headers (merge cors-headers {"Content-Type" "application/json"})
         :body    (json/write-str result)})

      (= uri "/play-minion-card")
      (let [body (when (:body request)
                   (-> (:body request)
                       slurp
                       (json/read-str :key-fn keyword)))
            player-id (:player-id body)
            card-id   (:card-id body)
            position  (:position body)
            result    (play-minion-card! player-id card-id position)]
        {:status  200
         :headers (merge cors-headers {"Content-Type" "application/json"})
         :body    (json/write-str result)})

      :else
      {:status  200
       :headers {"Content-Type" "text/html"}
       :body    "<h1>hello</h1>"})))
