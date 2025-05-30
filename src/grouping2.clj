(ns grouping2
  (:require [clojure.string :as str]
            [clojure.set :as set]))

(defn parse-names [input]
  (->> (str/split input #",")
       (map str/trim)
       (remove empty?)))

(defn assign-balanced-groups
  "Distributes people with weights into n groups, balancing total weights. Doubles shuffled, singles shuffled."
  [singles doubles n-groups]
  (let [shuffled-doubles (shuffle doubles)
        shuffled-singles (shuffle singles)
        people (concat (map #(vector % 2) shuffled-doubles)
                       (map #(vector % 1) shuffled-singles))]
    (reduce (fn [groups [name weight]]
              (let [idx (->> groups
                             (map-indexed (fn [i g] [i (:weight g)]))
                             (apply min-key second)
                             first)]
                (update groups idx (fn [g]
                                     {:members (conj (:members g) [name weight])
                                      :weight (+ (:weight g) weight)}))))
            (vec (repeat n-groups {:members [] :weight 0}))
            people)))

(defn -main []
  (println "Enter singles (comma-separated):")
  (let [singles (parse-names (read-line))]
    (println "Enter doubles (comma-separated):")
    (let [doubles (parse-names (read-line))
          overlapping (set/intersection (set singles) (set doubles))]
      (when (seq overlapping)
        (println "Warning: These names appear in both singles and doubles; treating them as doubles:" (str/join ", " overlapping)))
      (println "Enter the number of groups:")
      (let [n-groups (Integer/parseInt (str/trim (read-line)))
            singles-list (remove (set overlapping) singles)
            doubles-list doubles
            groups (assign-balanced-groups singles-list doubles-list n-groups)]
        (println "\nGroups (showing members and group weight):")
        (doseq [[idx group] (map-indexed vector groups)]
          (let [members-str (str/join ", " (map (fn [[name weight]]
                                                  (if (= weight 2)
                                                    (str name " (x2)")
                                                    name))
                                                (:members group)))]
            (println (format "Group %d (weight %d): %s"
                             (inc idx) (:weight group) members-str))))))))