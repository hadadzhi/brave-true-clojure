(ns wrand)

(defn wrand [slices]
  (let [total (reduce + slices), r (rand total)]
    (loop [i 0, sum 0]
      (let [sum (+ (slices i) sum)]
        (if (< r sum) i (recur (inc i) sum))))))
