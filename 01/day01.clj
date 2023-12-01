^{:nextjournal.clerk/visibility {:code :hide}}
(ns day01
  (:require [nextjournal.clerk :as clerk]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(comment
  (clerk/serve! {:watch-paths ["01"]}))

;; # Day 01: Trebuchet?! 

;; ## Part 1

;; Helper function for using solutions on different type of inputs
(defn solve-with [filename solution]
  (with-open [rdr (io/reader filename)]
    (solution (line-seq rdr))))

;; First function we need is which will be extracting digits from the line
(defn parse-line-part01 [line]
  (->> line
       (filter #(Character/isDigit %))
       (map #(Character/digit % 10))))

(parse-line-part01 "1adad2asdad3")

;; After we can use this function which takes
;; first and last integer in the list 
;; and combines them into new integer
(defn first-plus-last [l]
  (+ (* (first l) 10) (last l)))

(first-plus-last '(1 2 3 4))

;; Solution for first part combines 
;; these two function applying them to the whole input 
(defn part01 [lines]
  (->> lines
       (map parse-line-part01)
       (map first-plus-last)
       (reduce +)))

;; Answer for the first part!
(solve-with "01/input.txt" part01)

;; To solve second part we need extracting overlapping 
;; string that may define numbers as well.

;; This function will help us to search those strings in line
(defn re-find-all [re s]
  (loop [m (re-matcher re s) pos 0 res nil]
    (if
     (.find m pos)
      (recur m (+ 1 (.start m)) (cons (.group m) res))
      (reverse res))))

;; We then can use this function to find all required patterns using regex
(defn parse-line-part02 [line]
  (let [patterns #"one|two|three|four|five|six|seven|eight|nine|[0-9]"]
    (re-find-all patterns line)))

;; We need separate function to be able to search through overlapping patterns.
(parse-line-part02 "zoneight234")

;; This function will be translating found patterns into numbers
(defn replace-string-with-digit [s]
  (match [s]
    ["one"]   1
    ["two"]   2
    ["three"] 3
    ["four"]  4
    ["five"]  5
    ["six"]   6
    ["seven"] 7
    ["eight"] 8
    ["nine"]  9
    :else (Integer/parseInt s)))

(replace-string-with-digit "one")

;; And then we can combine all helper functions to get a solution for the second part
(defn part02 [lines]
  (->> lines
       (map parse-line-part02)
       (map #(map replace-string-with-digit %))
       (map first-plus-last)
       (reduce +)))

;; Here is an answer for the second part!
(solve-with "01/input.txt" part02)

(defn solve [filename]
  (println (solve-with filename part01))
  (println (solve-with filename part02)))

(defn run [_]
  (solve "01/input.txt"))