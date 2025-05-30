(ns grouping
  (:require [clojure.string :as str]))

(defn group-randomly
  "Randomly groups a list of names into n groups."
  [names n-groups]
  (let [shuffled (shuffle names)
        group-size (int (Math/ceil (/ (count names) (double n-groups))))]
    (partition-all group-size shuffled)))

(defn -main []
  (println "Enter names, separated by commas (e.g., Alice,Bob,Carol):")
  (let [names-line (read-line)
        people (->> (str/split names-line #",")
                    (map str/trim)
                    (remove empty?))]
    (println "Enter the number of groups:")
    (let [num-groups-line (read-line)
          num-groups (Integer/parseInt (str/trim num-groups-line))]
      (println "\nRandom groups:")
      (doseq [[idx group] (map-indexed vector (group-randomly people num-groups))]
        (println (str "Group " (inc idx) ": " (str/join ", " group)))))))

;; Required for running via `clojure -M -m grouping`
(when (= *file* (System/getProperty "babashka.file" *file*))
  (-main))