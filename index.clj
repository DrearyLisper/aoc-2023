^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns index
  (:require [nextjournal.clerk :as clerk]))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(comment
  (clerk/serve! {:paths [".", "01"] :watch-paths ["."]}))

;; # 🎄 Advent of Code 2023 in Clojure
;;
;;

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html "<img src='https://raw.githubusercontent.com/DrearyLisper/aoc-2023/master/images/logo.png' alt='logo' style='width:600px; text-align: center;'/>")

;; You can find solution with detailed explanations below.
;;
;; Please enjoy!
;;
;; Solutions are authored by [@DrearyLisper](https://x.com/drearylisper)
;;
;; Don't forget to star my [Project on GitHub](https://github.com/DrearyLisper/aoc-2023)!

;; ## [Day 01: Trebuchet?!](01/day01)
;; ``` clojure
;; aoc-2023$ clj -X day01/run
;; 55017
;; 53539
;; ```


