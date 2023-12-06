^{:nextjournal.clerk/visibility {:code :hide}}
(ns day06
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]
            [clojure.java.io :as io]))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn solve-with [filename solution]
  (with-open [rdr (io/reader filename)]
    (solution (line-seq rdr))))

;; ## Day 06: Wait For It

;; ### Part 01 

;; #### Problem statement

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
^::clerk/no-cache
(clerk/html (slurp "problems/06/part01.html"))

;; #### Solution

;; From the first glance problem looks very simple (which it is).

;; We try every possible variant of waiting and see how far we get.
;; Don't think it makes sense to overthink at this point, so let's just implement the bruteforce solution first and see whether it still will be usable in the second part.

;; Function parse-lines will be extracting required information from input lines and returning two vectors with times and distances.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn parse-lines-part01 [lines]
  (let [time-str (first lines)
        distance-str (first (rest lines))
        parse-line (fn [line]
                     (->> line
                          (#(str/split % #":"))
                          (rest)
                          (first)
                          (str/trim)
                          (#(str/split % #" "))
                          (filter (comp not empty?))
                          (map #(Integer/parseInt %))
                          (vec)))]
    (vec (map parse-line [time-str distance-str]))))
(parse-lines-part01 '("Time: 7 15 30" "Distance: 9 40 201"))

;; Next we write a helper function which will be trying every possible amounts of waiting time and computing final distance.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn variants [t]
  (->> (range t)
       (map #(* % (- t %)))))
(variants 7)

;; We then can use everything together to sketch a solutino.

(defn part01 [lines]
  (->> (parse-lines-part01 lines)
       ;; Zip numbers together
       (apply map vector)
       ;; Compute number of winning tries
       (map (fn [[t d]]
              (count (filter #(> % d) (variants t)))))
       ;; Multiple everything together       
       (reduce *)))

(solve-with "06/input.txt" part01)

;; We send an output to AoC and get our first easy star in the day.

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html "<img src='https://raw.githubusercontent.com/DrearyLisper/aoc-2023/master/images/first_part.png' alt='first_part' style='width: 100%; text-align: center;'/>")

;; ### Part 02 

;; #### Problem statement

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
^::clerk/no-cache
(clerk/html (slurp "problems/06/part02.html"))

;; #### Solution

;; The change seems to be not that huge. 

;; I guess authors want us to optimize original solution and use the fact that function from waiting times is convex, but can we try to use bruteforce solution first?

;; Surely we can!

;; We need to change a bit our parsing to join numbers together instead of converting them independently.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn parse-lines-part02 [lines]
  (let [time-str (first lines)
        distance-str (first (rest lines))
        parse-line (fn [line]
                     (->> line
                          (#(str/split % #":"))
                          (rest)
                          (first)
                          (str/trim)
                          (#(str/split % #" "))
                          (filter (comp not empty?))
                          (str/join #"")
                          (bigint)))]
    (vec (map parse-line [time-str distance-str]))))
(parse-lines-part02 '("Time: 7 15 30" "Distance: 9 40 201"))

;; We will use BigInteger as some multiplications probably won't fit into regular int.

;; And that should be enough for the second part.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part02 [lines]
  (->> (parse-lines-part02 lines)
       ((fn [[t d]]
          (count (filter #(> % d) (variants t)))))))

(solve-with "06/input.txt" part02)

;; Sending new number to AoC and get the second and final star for day!

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html "<img src='https://raw.githubusercontent.com/DrearyLisper/aoc-2023/master/images/second_part_06.png' alt='first_part' style='width: 100%; text-align: center;'/>")

;; I guess some calculations of how difficult it's to brutforce a solution went off today. 

;; Which is obviously good for us! :) 

;; If you liked my solution please star me on [GitHub](https://github.com/DrearyLisper/aoc-2023)!

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn solve [filename]
  (println (solve-with filename part01))
  (println (solve-with filename part02)))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn run [_]
  (solve "06/input.txt"))