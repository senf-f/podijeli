(ns grouping2
  (:require [clojure.string :as str]
           [clojure.set :as set]))

(defn parse-names [input]
  (->> (str/split input #",")
       (map str/trim)
       (remove empty?)))

(defn assign-balanced-groups
  "Distributes people with weights into n groups, balancing total group weights."
  [people n-groups]
  (let [sorted (sort-by (comp - second) people)] ; sort by weight descending (greedy assignment)
    (reduce (fn [groups [name weight]]
              (let [idx (->> groups
                             (map-indexed (fn [i g] [i (:weight g)]))
                             (apply min-key second)
                             first)]
                (update groups idx (fn [g]
                                     {:members (conj (:members g) [name weight])
                                      :weight (+ (:weight g) weight)}))))
            (vec (repeat n-groups {:members [] :weight 0}))
            sorted)))

(defn -main []
  (println "Enter singles (comma-separated):")
  (let [singles (parse-names (read-line))]
    (println "Enter doubles (comma-separated):")
    (let [doubles (parse-names (read-line))
          overlapping (clojure.set/intersection (set singles) (set doubles))]
      (when (seq overlapping)
        (println "Warning: These names appear in both singles and doubles; treating them as doubles:" (str/join ", " overlapping)))
      (println "Enter the number of groups:")
      (let [n-groups (Integer/parseInt (str/trim (read-line)))
            singles-list (map #(vector % 1) (remove (set overlapping) singles))
            doubles-list (map #(vector % 2) doubles)
            all-people (concat singles-list doubles-list)
            groups (assign-balanced-groups all-people n-groups)]
        (println "\nGroups (showing members and group weight):")
        (doseq [[idx group] (map-indexed vector groups)]
          (let [members-str (str/join ", " (map (fn [[name weight]]
                                                  (if (= weight 2)
                                                    (str name " (x2)")
                                                    name))
                                                (:members group)))]
            (println (format "Group %d (weight %d): %s"
                             (inc idx) (:weight group) members-str))))))))

(when (= *file* (System/getProperty "babashka.file" *file*))
  (-main))