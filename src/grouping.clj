(ns grouping
  (:require [clojure.string :as str]))

(defn group-randomly
  "Randomly groups a list of names into n groups."
  [names n-groups]
  (let [shuffled (shuffle names)
        group-size (int (Math/ceil (/ (count names) (double n-groups))))]
    (partition-all group-size shuffled)))

;; Example usage:
(def people ["Alice" "Bob" "Carol" "Dan" "Eve" "Frank" "Grace" "Heidi"])
(def num-groups 3)

(println "Random groups:")
(doseq [[idx group] (map-indexed vector (group-randomly people num-groups))]
  (println (str "Group " (inc idx) ": " (str/join ", " group))))