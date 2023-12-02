^{:nextjournal.clerk/visibility {:code :hide}}
(ns day02
  (:require [nextjournal.clerk :as clerk]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn solve-with [filename solution]
  (with-open [rdr (io/reader filename)]
    (solution (line-seq rdr))))

;; ## Day 02: Cube Conundrum

;; ### Part 01 
;; #### Problem statement

;; You're launched high into the atmosphere! The apex of your trajectory just barely reaches the surface of a large island floating in the sky. You gently land in a fluffy pile of leaves. It's quite cold, but you don't see much snow. An Elf runs over to greet you.

;; The Elf explains that you've arrived at **Snow Island** and apologizes for the lack of snow. He'll be happy to explain the situation, but it's a bit of a walk, so you have some time. They don't get many visitors up here; would you like to play a game in the meantime?

;; As you walk, the Elf shows you a small bag and some cubes which are either red, green, or blue. Each time you play this game, he will hide a secret number of cubes of each color in the bag, and your goal is to figure out information about the number of cubes.

;; To get information, once a bag has been loaded with cubes, the Elf will reach into the bag, grab a handful of random cubes, show them to you, and then put them back in the bag. He'll do this a few times per game.

;; You play several games and record the information from each game (your puzzle input). Each game is listed with its ID number (like the 11 in Game 11: ...) followed by a semicolon-separated list of subsets of cubes that were revealed from the bag (like 3 red, 5 green, 4 blue).

;; For example, the record of a few games might look like this:

;; ```
;; Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
;; Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
;; Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
;; Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
;; Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
;; ```

;; In game 1, three sets of cubes are revealed from the bag (and then put back again). The first set is 3 blue cubes and 4 red cubes; the second set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is only 2 green cubes.

;; The Elf would first like to know which games would have been possible if the bag contained **only 12 red cubes, 13 green cubes, and 14 blue cubes**?

;; In the example above, games 1, 2, and 5 would have been **possible** if the bag had been loaded with that configuration. However, game 3 would have been **impossible** because at one point the Elf showed you 20 red cubes at once; similarly, game 4 would also have been **impossible** because the Elf showed you 15 blue cubes at once. If you add up the IDs of the games that would have been possible, you get **8**.

;; Determine which games would have been possible if the bag had been loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. **What is the sum of the IDs of those games?**

;; #### Solution

;; So the goal of the first part is to check whether games are possible given the constraints to number of cubes that we can pull out of the bag.
;; The game only will make sense if we have:
;; - Less than 12 number of red cubes
;; - Less than 13 number of green cubes
;; - Less than 14 number of blue cubes

;; Let's breakdown solution into smaller parts.
;; We will need to be able to:
;; - Parse each line and extract game
;; - Parse hand and convert it into the vector
;; - Compare all hands in the game to see whether game is possible

;; We start with the function that parses single hand.
^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn parse-hand-part01 [game]
  (let [cube-to-vector
        (fn [cube]
          (match cube
            [n "red"] (vector (Integer/parseInt n) 0 0)
            [n "green"] (vector 0 (Integer/parseInt n) 0)
            [n "blue"] (vector  0 0 (Integer/parseInt n))))]
    (->> (str/split game #", ")
         (map #(str/split % #" "))
         (map cube-to-vector)
         (apply map +)
         (vec))))

(parse-hand-part01 "1 red, 2 green, 3 blue")

;; It takes a string representation of a hand and returns vector with number of cubes in particular order **[red green blue]**.

;; Next we are going to use this function to parse whole game that takes single line.
^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn parse-line [line]
  (let [[_ game-id cubes] (re-matches #"^Game ([0-9]+): (.+)$" line)
        cube-games (map str/trim (str/split cubes #";"))] 
    (vector (Integer/parseInt game-id) (map parse-hand-part01 cube-games))))

(parse-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green")

;; It returns
;; - **Id of a game** (as we need to sum it up) as a first element
;; - **List of parsed hands** as a second element

;; Last function that we need to solve first part will be deciding whether the game possible.
;; It will be iterating through all hands and compare the numbers to constant vector *[12 13 14]*.
;; If game is possible it will return id of a game otherwise it will return zero.
^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn check-game-part01 [game]
  (let [[game-id cubes] game
        compare-game (fn compare-game [a b]
                       (every? identity (map <= a b)))
        bad-games (->> cubes
                       (map #(compare-game % (vector 12 13 14)))
                       (filter #(not %))
                       (count))]
    (if (> bad-games 0) 0 game-id)))

(check-game-part01
 (parse-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))

;; As this game is possible **1** (id of a game) is returned.

;; Applying all helper functions in correct order gets as answer for the first part!

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part01 [lines]
  (->> (vec lines)
       (map parse-line)
       (map check-game-part01)
       (reduce +)))

(solve-with "02/input.txt" part01)

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html "<img src='https://raw.githubusercontent.com/DrearyLisper/aoc-2023/master/images/first_part.png' alt='first_part' style='width: 100%; text-align: center;'/>")

;; ### Part 02 
;; #### Problem statement

;; The Elf says they've stopped producing snow because they aren't getting any water! He isn't sure why the water stopped; however, he can show you how to get to the water source to check it out for yourself. It's just up ahead!

;; As you continue your walk, the Elf poses a second question: in each game you played, what is the **fewest number of cubes of each color** that could have been in the bag to make the game possible?

;; Again consider the example games from earlier:

;; ```
;; Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
;; Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
;; Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
;; Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
;; Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
;; ```

;; - In game 1, the game could have been played with as few as 4 red, 2 green, and 6 blue cubes. If any color had even one fewer cube, the game would have been impossible.
;; - Game 2 could have been played with a minimum of 1 red, 3 green, and 4 blue cubes.
;; - Game 3 must have been played with at least 20 red, 13 green, and 6 blue cubes.
;; - Game 4 required at least 14 red, 3 green, and 15 blue cubes.
;; - Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the bag.

;; The power of a set of cubes is equal to the numbers of red, green, and blue cubes multiplied together. The power of the minimum set of cubes in game 1 is 48. In games 2-5 it was 12, 1560, 630, and 36, respectively. Adding up these five powers produces the sum **2286**.

;; For each game, find the minimum set of cubes that must have been present. **What is the sum of the power of these sets?**

;; #### Solution

;; Luckuly for us we don't need to change much of a logic that we created in first part.
;; The only thing which is differs is the way how we aggregate hands.

;; Instead of comparing with a constant hand we will be taking component-wise maximum of the hands,
;; and the multiplying them to each other.
^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn check-game-part02 [game]
  (let [[_ cubes] game]
    (->> cubes
         (apply map max)
         (reduce *))))

(check-game-part02
 (parse-line "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"))

;; Max components are 4 red, 2 green, 6 blue getting us 48 as a result.

;; Again applying all helper functions in correct order gets as answer for the second part as well!

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part02 [lines]
  (->> (vec lines)
       (map parse-line)
       (map check-game-part02)
       (reduce +)))

(solve-with "02/input.txt" part02)

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html "<img src='https://raw.githubusercontent.com/DrearyLisper/aoc-2023/master/images/second_part_02.png' alt='first_part' style='width: 100%; text-align: center;'/>")

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn solve [filename]
  (println (solve-with filename part01))
  (println (solve-with filename part02)))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn run [_]
  (solve "02/input.txt"))