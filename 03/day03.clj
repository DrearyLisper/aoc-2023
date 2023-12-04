^{:nextjournal.clerk/visibility {:code :hide}}
(ns day03
  (:require [nextjournal.clerk :as clerk]
            [clojure.core.match :refer [match]]
            [clojure.string :as str]
            [clojure.java.io :as io]))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn solve-with [filename solution]
  (with-open [rdr (io/reader filename)]
    (solution (line-seq rdr))))

;; ## Day 03: Gear Ratios

;; ### Part 01 
;; #### Problem statement

;; You and the Elf eventually reach a [gondola lift](https://en.wikipedia.org/wiki/Gondola_lift) station; he says the gondola lift will take you up to the **water source**, but this is as far as he can bring you. You go inside.

;; It doesn't take long to find the gondolas, but there seems to be a problem: they're not moving.

;; "Aaah!"

;; You turn around to see a slightly-greasy Elf with a wrench and a look of surprise. "Sorry, I wasn't expecting anyone! The gondola lift isn't working right now; it'll still be a while before I can fix it." You offer to help.

;; The engineer explains that an engine part seems to be missing from the engine, but nobody can figure out which one. If you can **add up all the part numbers** in the engine schematic, it should be easy to work out which part is missing.

;; The engine schematic (your puzzle input) consists of a visual representation of the engine. There are lots of numbers and symbols you don't really understand, but apparently **any number adjacent to a symbol**, even diagonally, is a "part number" and should be included in your sum. (Periods (.) do not count as a symbol.)

;; Here is an example engine schematic:

;; ```
;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..
;; ```

;; In this schematic, two numbers are not part numbers because they are not adjacent to a symbol: 114 (top right) and 58 (middle right). Every other number is adjacent to a symbol and so is a part number; their sum is **4361**.

;; Of course, the actual engine schematic is much larger. **What is the sum of all of the part numbers in the engine schematic?**

;; #### Solution

;; So, in this problem we need add up all numbers that have any kind of symbols near by.

;; I decomposed this problem into smaller once
;; - Forming map with indexed cells
;; - Finding all the symbols in the map
;; - Finding all unique numbers near symbols

;; We will use this small type **Cell** to keep everything we need to now about the each cell in the map.

;; I haven't figure outed user defined types in **Clojure** yet, but I think it will do a required job.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(deftype Cell [x y v]
  Object
  (toString [_]
    (str/join " " [x y v])))
(->Cell 1 3 "*")

;; This type will be storting *(x,y)* coordinates of the cell as well as the value itself.

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn isDigit [str]
  (every? #(Character/isDigit %) str))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn isSymbol [cell]
  (not (or
        (isDigit (.v cell))
        (= "." (.v cell)))))

;; We will be using *parse-lines* to convert input data into matrix of cells.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn parse-lines [lines]
  (let [ parse-line (fn [x line]
                     (->> (str/split line #"")
                          ;; Add information about the x-index
                          (map vector (repeat x) (range))
                          ;; Construct cells
                          (map #(apply ->Cell %))
                          (vec)))
        parse-row (fn [x line] (parse-line x line))]
    (vec (->> lines
              ;; Parse lines per row
              (map #(apply parse-row %))))))
(parse-lines [[0 "123*.321"]])

;; The result will be a matrix with parsed cells.

;; Next function will be extracting all interesting symbols.

;; We can control additional filters we want to apply using *predicate* parameter. It will be clear in the future why we need it.
^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn filter-symbols [rows predicate]
  (->> rows
       (map #(filter isSymbol %))
       (apply concat)
       (filter predicate)))
(filter-symbols (parse-lines [[0 "123*.321"]]) any?)

;; This gets us a vector of all symbols. Now we need to find all numbers that are close to each symbol.

;; But before we do that let's write a helper function that extract the number from the map based on its coordinates.

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn get-cell [rows x y]
  (get (get rows x) y))

;; We can provide coordinate of any part of the number, as we trace back to its start always.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn extract-number [rows x y]
  (cond
    ;; Return nil if we out of boundary of the map
    (or (< x 0) (>= x (count rows))) nil
    (or (< y 0) (>= x (count (first rows)))) nil
    ;; We also return nil if we are not pointing to the number
    (not (isDigit (.v (get-cell rows x y)))) nil
    :else
    (let [;; We first trace to the beginning of the number
          first-y (loop [cy y]
                    (if (and (>= cy 0) (isDigit (.v (get-cell rows x cy))))
                      (recur (- cy 1))
                      (+ cy 1)))
          pred #(isDigit (.v %))
          ;; Take all its digits
          number-cells (take-while pred (subvec (get rows x) first-y))
          ;; And construct the number itself
          number (Integer/parseInt (str/join "" (map #(.v %) number-cells)))]
      [x first-y number])))
(extract-number
 (parse-lines [[0 "123*.321"]]) 0 1)

;; We also will be adding coordinates of found number to help us distinguishing the same numbers placed in different locations.

;; Then we can use this function to peek around the coordinate and gather everything that looks like number.

^{:nextjournal.clerk/visibility {:code :show  :result :hide}}
(defn numbers-around [rows x y]
  (let [places (for [dx [-1 0 1]
                     dy [-1 0 1]
                     :when (not= 0 (+ (abs dx) (abs dy)))]
                 [(+ x dx) (+ y dy)])
        numbers (map #(apply (partial extract-number) rows %) places)]
    (distinct (filter identity numbers))))
(numbers-around
 (parse-lines [[0 "123*.321"]]) 0 4)

;; Finally we can combine all the pieces to get the final algorithm.

^{:nextjournal.clerk/visibility {:code :show  :result :hide}}
(defn part01 [lines]
  (let [index-lines (vec (map vector (lazy-seq (range)) lines))
        rows (parse-lines index-lines)]
    (->>
     ;; Get positions of the symbols
     (filter-symbols rows any?)
     ;; Extract coordinates
     (map #(vector (.x %) (.y %)))
     ;; Collect numbers with their coordinates around the symbols
     (map #(apply (partial numbers-around rows) %))
     (map vec)
     ;; Collect all of them
     (apply concat)
     ;; Take a number
     (map #(get % 2))
     ;; Final sum
     (reduce +))))

(solve-with "03/input.txt" part01)

;; And this get us a first start in the day.

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html "<img src='https://raw.githubusercontent.com/DrearyLisper/aoc-2023/master/images/first_part.png' alt='first_part' style='width: 100%; text-align: center;'/>")

;; ### Part 02 
;; #### Problem statement

;; The engineer finds the missing part and installs it in the engine! As the engine springs to life, you jump in the closest gondola, finally ready to ascend to the water source.

;; You don't seem to be going very fast, though. Maybe something is still wrong? Fortunately, the gondola has a phone labeled "help", so you pick it up and the engineer answers.

;; Before you can explain the situation, she suggests that you look out the window. There stands the engineer, holding a phone in one hand and waving with the other. You're going so slowly that you haven't even left the station. You exit the gondola.

;; The missing part wasn't the only issue - one of the gears in the engine is wrong. A **gear** is any * symbol that is adjacent to **exactly two part numbers.** Its **gear ratio** is the result of multiplying those two numbers together.

;; This time, you need to find the gear ratio of every gear and add them all up so that the engineer can figure out which gear needs to be replaced.

;; Consider the same engine schematic again:

;; ```
;; 467..114..
;; ...*......
;; ..35..633.
;; ......#...
;; 617*......
;; .....+.58.
;; ..592.....
;; ......755.
;; ...$.*....
;; .664.598..
;; ```

;; In this schematic, there are **two** gears. The first is in the top left; it has part numbers 467 and 35, so its gear ratio is 16345. The second gear is in the lower right; its gear ratio is 451490. (The * adjacent to 617 is **not** a gear because it is only adjacent to one part number.) Adding up all of the gear ratios produces **467835**.

;; **What is the sum of all of the gear ratios in your engine schematic?**

;; #### Solution

;; So the change is that we need to consider only * symbol, and multiple the numbers before finally summing them up.

;; Luckily our earlier implemented functions are reach enough to only modify a final function.

^{:nextjournal.clerk/visibility {:code :show  :result :hide}}
(defn part02 [lines]
  (let [index-lines (vec (map vector (lazy-seq (range)) lines)) 
        rows (parse-lines index-lines)
        ;; Take only * symbols
        gear-predicate #(= "*" (.v %))]
    (->> (filter-symbols rows gear-predicate)
         (map #(vector (.x %) (.y %)))
         (map #(apply (partial numbers-around rows) %))
         ;; Leave only set of numbers with size of two
         (filter #(= 2 (count %)))
         (map vec)
         (map #(map (fn [c] (get c 2)) %))
         ;; Multiply them
         (map #(reduce * %))
         ;; Before reducing into sum
         (reduce +))))

(solve-with "03/input.txt" part02)

;; Which gets up a second star for the day!

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html "<img src='https://raw.githubusercontent.com/DrearyLisper/aoc-2023/master/images/second_part_03.jpeg' alt='first_part' style='width: 100%; text-align: center;'/>")

;; If you liked my solution please star me on [GitHub](https://github.com/DrearyLisper/aoc-2023)!

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn solve [filename]
  (println (solve-with filename part01))
  (println (solve-with filename part02)))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn run [_]
  (solve "03/input.txt"))