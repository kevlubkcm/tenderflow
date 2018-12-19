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
          validators (map #(select-keys % [:address :voting_power]) (:validators val-info))]
      {:height height
       :first-height height
       :round round
       :step step
       :validators validators}))

(defn get-consensus-dump
  []
  (go (let [url (str "http://" host "/dump_consensus_state")
            resp (<! (http/get url {:with-credentials? false}))
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


(def val-radius 300)
(def vote-radius 20)
(def center [500 500])
(def blocks-max 10)
(def blocks-head [900 700])
(def block-space 50)
(def vote-styles
  {:proposal [30 "red"]
   :pre-vote [3 "blue"]
   :pre-commit [2 "green"]})


(defn block-locations
  [head spacing n]
  (let [[x head-y] head
        dys (range n)
        dys (map #(* % spacing) dys)]
    (map #(vector x (- head-y %)) dys)))


(defn draw-blockchain
  [svg heights [prop-x prop-y]]
  (let [locs (block-locations blocks-head block-space (count heights))
        data (into-array (map vector heights locs))
        [r _] (:proposal vote-styles)
        s (/ r 2)
        fill "green"
        svg (-> svg 
                (.selectAll "rect") 
                (.data data (fn [[h _]] h)))]
    (-> svg
        (.enter)
          (.append "rect")
            (.attr "x" (- prop-x s))
            (.attr "y" (- prop-y s))
            (.attr "width" r)
            (.attr "height" r)
            (.attr "fill" fill)
          (.transition)
            (.attr "x" (fn [[_ [x _]]] x))
            (.attr "y" (fn [[_ [_ y]]] y)))
    (-> svg
        (.transition)
          (.attr "x" (fn [[_ [x _]]] x))
          (.attr "y" (fn [[_ [_ y]]] y)))
    (-> svg
        (.exit)
          (.remove))))

(defn draw-validators
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
          (.attr "fill" (fn [_ i] (if (= i proposer-idx) "green" "red"))))))

(defn draw-proposal
  [svg prop height round idx locs [vote-x vote-y]]
  (let [[val-x val-y] (nth locs idx)
        [r fill] (:proposal vote-styles)
        s (/ r 2)
        data (if (nil? prop) [] [(str height "#" round)])
        data (into-array data)
        svg (-> svg 
                (.selectAll "rect") 
                (.data data (fn [h] h)))]
    (-> svg
        (.enter)
          (.append "rect")
            (.attr "x" (- val-x s))
            (.attr "y" (- val-y s))
            (.attr "width" r)
            (.attr "height" r)
            (.attr "fill" fill)
          (.transition)
            (.attr "x" (- vote-x s))
            (.attr "y" (- vote-y s)))
    (-> svg
        (.exit)
          (.remove))))

(defn vote-id
  [idx height vote-type]
  (str height "#" idx "#" vote-type))

(defn draw-votes
  [svg pre-votes pre-commits val-locs vote-locs height]
  (let [locs (vec (map vector val-locs vote-locs))
        pre-votes (for [i pre-votes] [(vote-id i height 0) (locs i) (:pre-vote vote-styles)])
        pre-commits (for [i pre-commits] [(vote-id i height 1) (locs i) (:pre-commit vote-styles)])
        data (into-array (concat pre-votes pre-commits))
        svg (-> svg 
                (.selectAll "circle") 
                (.data data (fn [[d _ _]] d)))]
    (-> svg
        (.enter)
          (.append "circle")
          (.attr "cx" (fn [[_ [[x _] _] _]] x))
          (.attr "cy" (fn [[_ [[_ y] _] _]] y))
          (.attr "r" (fn [[_ _ [r _]]] r))
          (.attr "fill" (fn [[_ _ [_ f]]] f))
          (.transition)
            (.attr "cx" (fn [[_ [_ [x _]] _]] x))
            (.attr "cy" (fn [[_ [_ [_ y]] _]] y)))
    (-> svg
        (.exit)
          (.remove))))

(defn update-graphics
  [svg-groups _ _ _ new-state]
  (let [current-validators (:validators new-state)
        n (count current-validators)
        proposer-idx (:proposer-idx new-state)
        val-locs (compute-locations center n val-radius)
        vote-locs (compute-locations center n vote-radius)
        {:keys [height round step pre-votes pre-commits proposal first-height]} new-state
        complete-blocks (take blocks-max (range height first-height -1))]
    (draw-validators (:validators svg-groups) current-validators proposer-idx val-locs)
    (draw-blockchain (:blockchain svg-groups) complete-blocks center)
    (draw-proposal (:proposal svg-groups) proposal height round proposer-idx val-locs center)
    (draw-votes (:votes svg-groups) pre-votes pre-commits val-locs vote-locs height)))

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

; (defonce rand-prop (js/setInterval set-random-proposer 10000))
; (defonce rand-pre-vote (js/setInterval #(add-random-vote :pre-votes) 200))

(defn ^:export main []
  (let [svg-main (append-svg "#app" 1000 1000)
        svg-groups {:validators (.append svg-main "g")
                    :proposal (.append svg-main "g")
                    :votes (.append svg-main "g")
                    :blockchain (.append svg-main "g")}]
    (add-watch app-state :update-header (partial update-graphics svg-groups))
    ;(set-random-state 10)
    ;(js/setTimeout set-random-proposer 50)
    (js/setTimeout get-consensus-dump 50)
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
    (ws/send socket msg fmt/json)))

(defn handle-new-round
  [d]
  (let [{:keys [height round step]} d
        proposer-idx (get-in d [:proposer :index])
        proposer-idx (js/parseInt proposer-idx)
        height (js/parseInt height)
        round (js/parseInt round)]
    (swap! app-state assoc 
           :height height 
           :round round 
           :step step 
           :proposer-idx proposer-idx
           :proposal nil
           :pre-votes []
           :pre-commits [])))

(defn handle-complete-proposal
  [d]
  (let [proposal (get-in d [:block_id :hash])]
    ; TODO: check that height and round match
    (swap! app-state assoc :proposal proposal)))

(def vote-type-map
  {1 :pre-votes
   2 :pre-commits})

(defn add-vote
  [state vote-type idx]
  (let [old-votes (vote-type state)
        new-votes (conj old-votes idx)]
    (assoc state vote-type new-votes)))

(defn handle-vote
  [d]
  (let [d (:Vote d)
        idx (:validator_index d)
        idx (js/parseInt idx)
        vote-type (get vote-type-map (:type d))]
    (swap! app-state #(add-vote % vote-type idx))))


(def event-handlers
  {"tendermint/event/NewRound" handle-new-round
   "tendermint/event/CompleteProposal" handle-complete-proposal
   "tendermint/event/Vote" handle-vote})

(defn handle-message
  [e]
  (let [data (.-data e)
        data (.parse js/JSON data)
        data (js->clj data :keywordize-keys true)
        data (get-in data [:result :data])
        event-type (:type data)
        handler (get event-handlers event-type)
        data (:value data)]
    (if (some? handler)
      (handler data)
      (prn (str "Unhandled Type: " event-type "\n" data)))))

(def handlers {:on-message handle-message
               :on-open #(prn "opening connection")})

(defonce socket (ws/create (str "ws://" host "/websocket") handlers))
(defonce subscribe-proposals (subscribe socket "0" "CompleteProposal"))
(defonce subscribe-votes (subscribe socket "1" "Vote"))
(defonce subscribe-new-round (subscribe socket "2" "NewRound"))

