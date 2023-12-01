^{:nextjournal.clerk/visibility {:code :hide}}
(ns day01
  (:require [nextjournal.clerk :as clerk]
            [clojure.java.io :as io]
            [clojure.core.match :refer [match]]))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(comment
  (clerk/serve! {:watch-paths ["01"]}))

;; # Day 01: Trebuchet?! 

;; ## Part 01

;; You try to ask why they can't just use a [weather machine](https://adventofcode.com/2015/day/1) ("not powerful enough") and where they're even sending you ("the sky") and why your map looks mostly blank ("you sure ask a lot of questions") and hang on did you just say the sky ("of course, where do you think snow comes from") when you realize that the Elves are already loading you into a [trebuchet](https://en.wikipedia.org/wiki/Trebuchet) ("please hold still, we need to strap you in").

;; As they're making the final adjustments, they discover that their calibration document (your puzzle input) has been **amended** by a very young Elf who was apparently just excited to show off her art skills. Consequently, the Elves are having trouble reading the values on the document.

;; The newly-improved calibration document consists of lines of text; each line originally contained a specific **calibration value** that the Elves now need to recover. On each line, the calibration value can be found by combining the **first digit** and the **last digit** (in that order) to form a single **two-digit number.**

;; For example:

;; ```
;; 1abc2
;; pqr3stu8vwx
;; a1b2c3d4e5f
;; treb7uchet
;; ```

;; In this example, the calibration values of these four lines are 12, 38, 15, and 77. Adding these together produces **142**.

;; Consider your entire calibration document. **What is the sum of all of the calibration values?**


;; Helper function for using solutions on different type of inputs
(defn solve-with [filename solution]
  (with-open [rdr (io/reader filename)]
    (solution (line-seq rdr))))

;; The first function that we need will be extracting digits from the input line
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

;; ## Part 02

;; Your calculation isn't quite right. It looks like some of the digits are actually **spelled out with letters**: one, two, three, four, five, six, seven, eight, and nine also count as valid "digits".

;; Equipped with this new information, you now need to find the real first and last digit on each line. For example:

;; ```
;; two1nine
;; eightwothree
;; abcone2threexyz
;; xtwone3four
;; 4nineeightseven2
;; zoneight234
;; 7pqrstsixteen
;; ```

;; In this example, the calibration values are 29, 83, 13, 24, 42, 14, and 76. Adding these together produces **281**.

;; **What is the sum of all of the calibration values?**

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