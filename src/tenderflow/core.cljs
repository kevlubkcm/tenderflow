(ns ^:figwheel-hooks tenderflow.core
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require
    [goog.dom :as gdom]
    [cljsjs.d3]
    [cljs-http.client :as http]
    [cljs.core.async :refer [<!]]
    [wscljs.client :as ws]
    [wscljs.format :as fmt]
   ))

(def host "localhost:26657")
(defonce app-state (atom {}))

(defn indices [pred coll]
   (keep-indexed #(when (pred %2) %1) coll))

(defn parse-consensus-dump [msg]
    (let [s (get-in msg [:result :round_state])
          {:keys [height round step]} s
          val-info (:validators s)
          validators (map #(select-keys % [:address :voting_power]) (:validators val-info))
          proposer-addr (get-in val-info [:proposer :address])
          proposer-idx (first (indices #(= (:address %) proposer-addr) validators))
          votes (first (filter #(= (:round %) round) (:votes val-info)))
          pre-votes (:prevotes votes)
          pre-commits (:precommits votes)]
      {:height height
       :round round
       :step step
       :validators validators
       :proposer-idx proposer-idx
       :proposal nil
       :pre-votes pre-votes
       :pre-commits pre-commits}))

(defn get-consensus-dump
  []
  (go (let [url (str "http://" host "/dump_consensus_state")
            resp (<! (http/get url))
            d (parse-consensus-dump (:body resp))]
        (reset! app-state d))))

(defn get-app-element []
  (gdom/getElement "app"))

(defn append-svg [div-id height width]
  (-> js/d3
      (.select div-id)
      (.append "svg")
      (.attr "height" height)
      (.attr "width" width)))

(defn remove-svg [div-id]
  (-> js/d3
      (.selectAll (str div-id " svg"))
      (.remove)))

(defn update-header 
  [svg _ _ old-state new-state]
  (let [{:keys [height round step]} new-state
        ]
    (prn (str height "/" round "/" step))
    (-> svg
        (.selectAll "circle")
        (.data (into-array [height round step]))
        (.enter)
        (.append "circle")
        (.attr "cx" (fn [d i] d))
        (.attr "cy" (fn [d i] i))
        (.attr "r" (fn [d i] d))
        )
  ))

(defn compute-locations
  [[x y] n r]
  (vec (for [i (range n)
        :let [o (* (/ i n) 2 Math/PI)]]
    [(+ x (* r (Math/cos o))) 
     (+ y (* r (Math/sin o)))])))

(defn create-circles
  [svg validators proposer-idx locs]
  (let [data (into-array locs)
        addrs (vec (map :address validators))
        svg (-> svg 
                (.selectAll "circle") 
                (.data data (fn [d i] (get addrs i))))]
    (-> svg
        (.exit)
          (.remove))
    (-> svg
        (.enter)
          (.append "circle")
          (.attr "cx" (fn [[x _] _] x))
          (.attr "cy" (fn [[_ y] _] y))
          (.attr "r" (fn [_ _] 10))
          (.attr "fill" (fn [_ i] (if (= i proposer-idx) "green" "red"))))
    (-> svg
        (.transition)
          (.attr "cx" (fn [[x _] _] x))
          (.attr "cy" (fn [[_ y] _] y))
          (.attr "r" (fn [_ _] 10))
          (.attr "fill" (fn [_ i] (if (= i proposer-idx) "green" "red"))))
  )
)

(def vote-styles
  {:proposal [5 "black"]
   :pre-vote [3 "blue"]
   :pre-commit [2 "green"]})

(defn create-vote
  [svg idx vote-type locs [vote-x vote-y]]
  (let [[val-x val-y] (nth locs idx)
        [r fill] (vote-type vote-styles)]
    (-> svg
        (.selectAll "circle")
        (.remove))
    (-> svg
        (.append "circle")
          (.attr "cx" val-x)
          (.attr "cy" val-y)
          (.attr "r" r)
          (.attr "fill" fill))
    (-> svg
        (.select "circle")
        (.transition)
          (.attr "cx" vote-x)
          (.attr "cy" vote-y))
    ))

(def radius 300)
(def center [500 500])

(defn clear-votes
  [svg-groups]
  (doseq [g [:proposal :pre-vote]]
    (-> (g svg-groups)
        (.selectAll "circle")
        (.remove))))

(defn update-graphics
  [svg-groups _ _ old-state new-state]
  (prn new-state)
  (let [current-validators (:validators new-state)
        n (count current-validators)
        proposer-idx (:proposer-idx new-state)
        locs (compute-locations center n radius)
        new-proposal (and (nil? (:proposal old-state)) (some? (:proposal new-state)))
        old-hr (select-keys old-state [:height :round])
        new-hr (select-keys new-state [:height :round])
        new-round (not= old-hr new-hr)
        new-pre-votes (:pre-votes new-state)]
    (create-circles (:validators svg-groups) current-validators proposer-idx locs)
    (if new-round
      (clear-votes svg-groups))
    (if new-proposal
      (create-vote (:proposal svg-groups) proposer-idx :proposal locs center))
    (doseq [i new-pre-votes] 
      (create-vote (:pre-vote svg-groups) i :pre-vote locs center))
  ))

(defn generate-random-validator
  [addr]
  {:address addr
   :voting_power (rand-int 100)
   })

(defn set-random-state
  [n]
  (let [vs (vec (for [i (range n)] (generate-random-validator (str i))))
        proposer-idx (rand-int n)
        h 0
        r 0]
    (reset! app-state {:validators vs 
                       :proposer-idx proposer-idx 
                       :height 0 
                       :round 0
                       :pre-votes #{}})))

(defn compute-random-proposer
  [state]
  (if (nil? (:proposal state))
    (assoc state :proposal "blah")
    (let [n (count (:validators state))
          p (:proposer-idx state)
          new-p (rand-int n)
          h (:height state)]
      (assoc state 
             :proposer-idx new-p 
             :proposal nil 
             :height (+ h 1)
             :pre-votes #{}))))

(defn set-random-proposer
  []
  (swap! app-state compute-random-proposer))

(defn compute-random-vote
  [state vote-type]
  (if (nil? (:proposal state))
    state
    (let [n (count (:validators state))
          v (rand-int n)
          votes (vote-type state)
          new-votes (conj votes v)]
      (assoc state vote-type new-votes))))

(defn add-random-vote
  [vote-type]
  (swap! app-state #(compute-random-vote % vote-type)))

(defonce rand-prop (js/setInterval set-random-proposer 1000))
(defonce rand-pre-vote (js/setInterval #(add-random-vote :pre-votes) 200))

(defn ^:export main []
  (let [svg-main (append-svg "#app" 1000 1000)
        svg-groups {:validators (.append svg-main "g")
                    :proposal (.append svg-main "g")
                    :pre-vote (.append svg-main "g")
                    }
        ]
    (add-watch app-state :update-header (partial update-graphics svg-groups))
    (set-random-state 10)
    (js/setTimeout set-random-proposer 50)
  ))

(defn ^:after-load on-reload []
  ;; (swap! app-state update-in [:__figwheel_counter] inc)
)

(defn subscribe-msg
  [id evt]
  {:jsonrpc "2.0" 
   :method "subscribe" 
   :id id 
   :params {:query (str "tm.event='" evt "'")}})

(defn subscribe
  [socket id evt]
  (let [msg (subscribe-msg id evt)]
    (prn (str "Subscribing: " msg))
    (ws/send socket (subscribe-msg id evt) fmt/json)))

#_(def handlers {:on-message dispatch-ws-event
               :on-open #(prn "opening connection")})

;;(defonce socket (ws/create (str "ws://" host "/websocket") handlers))
;;(defonce subscribe-proposals (subscribe socket "0" "CompleteProposal"))
;;(defonce subscribe-votes (subscribe socket "1" "Vote"))
