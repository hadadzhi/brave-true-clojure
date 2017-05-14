(ns concurrent)

(defn give-gift [giver-ref receiver-ref]
  (dosync
    (if-let [gift (first (ensure giver-ref))]
      (do
        (alter receiver-ref conj gift)
        (alter giver-ref disj gift)))))

(defn give-gifts []
  (let [giver (ref #{:gift1 :gift2})
        recv1 (ref #{})
        recv2 (ref #{})
        recv3 (ref #{})]
    (future (give-gift giver recv1))
    (future (give-gift giver recv2))
    (future (give-gift giver recv3))
    (Thread/sleep 10)
    {:giver @giver :recv1 @recv1 :recv2 @recv2 :recv3 @recv3}))
