^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(ns index
  (:require [nextjournal.clerk :as clerk]))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(comment
  (clerk/serve! {:paths [".", "01", "02", "03", "04"] :watch-paths ["."] :host "0.0.0.0"}))

;; # 🎄 Advent of Code 2023 in Clojure
;;
;;

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html "<img src='https://raw.githubusercontent.com/DrearyLisper/aoc-2023/master/images/logo.png' alt='logo' style='width: 100%; text-align: center;'/>")

;; Solutions are authored by [@DrearyLisper](https://x.com/drearylisper)
;;
;; Don't forget to star my [Project on GitHub](https://github.com/DrearyLisper/aoc-2023)!

;; ## [Day 01: Trebuchet?!](01/day01)
;; ``` clojure
;; aoc-2023$ clj -X day01/run
;; 55017
;; 53539
;; ```

;; ## [Day 02: Cube Conundrum](02/day02)
;; ``` clojure
;; aoc-2023$ clj -X day02/run
;; 2617
;; 59795
;; ```

;; ## [Day 03: Gear Ratios](03/day03)
;; ``` clojure
;; aoc-2023$ clj -X day02/run
;; 539713
;; 84159075
;; ```

;; ## [Day 04: Scratchcards](04/day04)
;; ``` clojure
;; aoc-2023$ clj -X day04/run
;; 22674
;; 5747443
;; ```
