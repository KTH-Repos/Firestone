(ns firestone.client.kth.endpoints
  (:require [clojure.data.json :as json]
            [firestone.construct :refer [create-game]]
            [firestone.client.kth.mapper :refer [game->client-game
                                                 client-input->internal-game]]))

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
            internal-game (-> body
                              (client-input->internal-game) ; Transform client input
                              (create-game))] ; Pass transformed data to create-game
        {:status  200
         :headers (merge cors-headers {"Content-Type" "application/json"})
         :body    (-> (vector (game->client-game internal-game))
                      (json/write-str))})


      :else
      {:status  200
       :headers {"Content-Type" "text/html"}
       :body    "<h1>hello</h1>"})))