^{:nextjournal.clerk/visibility {:code :hide}}
(ns day05
  (:require [nextjournal.clerk :as clerk]
            [clojure.string :as str]
            [clojure.java.io :as io]))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn solve-with [filename solution]
  (with-open [rdr (io/reader filename)]
    (solution (line-seq rdr))))

;; ## Day 05: If You Give A Seed A Fertilizer

;; ### Part 01 

;; #### Problem statement

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
^::clerk/no-cache
(clerk/html (slurp "problems/05/part01.html"))

;; #### Solution

;; In this problem we need to pass a seed value through number of maps.
;; Each map defines where different regions shoud be moved to. If value is not getting covered by any region it stays the same.

;; We will parse each map into a function which then could be combined into one function mapping the value from beginning to the end.

;; But first let's implement parsing for the seed values.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn parse-seeds [line]
  (let [[_ seeds-str] (str/split line #":")]
    (map #(bigint %) (str/split (str/trim seeds-str) #" "))))
(parse-seeds "seeds: 79 14 55 13")

;; It will be returning just a vector of values. **N** suffix means that those are big integers, it will be required to handle large ints from the input.txt 

;; Parsing of the maps will be a bit more tricky. I will add comments to important parts of this big function.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn parse-map [line]
  (let [components (rest (str/split line #"\n"))
        ;; Each map will be combination of simpler components.
        ;; Each component is responsible for mapping one region to another.
        ;; {:from-offset 1 :to-offset 3 :range 2} mean that we will map [1, 2] -> [3, 4]
        parse-component (fn
                          [component]
                          (let [[a b c] (str/split component #" ")]
                            {:from-offset (bigint b)
                             :to-offset (bigint a)
                             :range (bigint c)}))
        ;; Map is represented with a list of components
        parsed-components (map parse-component components)
        ;; First we need create an application of one single component to the seed value
        ;; It's implemented simply by checking whether the seed value is within a range of the component
        apply-component (fn
                          [component seed]
                          (if (and
                               (>= seed (:from-offset component))
                               (< seed (+ (:from-offset component) (:range component))))
                            (+ (:to-offset component) (- seed (:from-offset component)))
                            nil))
        ;; And then we apply all components at the same time
        ;; If no components were able to map a seed we return it as is as problem states
        apply-components (fn
                           [seed]
                           (let [mapped (->> parsed-components
                                             (map #(apply-component % seed))
                                             (filter (comp not nil?)))]
                             (if (empty? mapped)
                               seed
                               (first mapped))))]
    ;; We will be return our map as a function that map seed values
    apply-components))

(parse-map "\n50 98 2")

;; The output of the function is not that transparent in this case, but we can apply it to some values to see how it works. 

(map (parse-map "\n50 98 2") [97N 98N 99N 100N])

;; As you can see we mapped a region of size two into a different one [98, 99] -> [50, 51]

;; Now we can put everything together to solve the first part.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part01 [lines]
  (let [content (str/join "\n" lines)
        components (str/split content #"\n\n")
        seeds (parse-seeds (first components))
        maps (map parse-map (rest components))
        ;; Combine all maps into a single map using function composition
        f (->> maps reverse (reduce comp))]
    (->> seeds
         ;; Map every seed
         (map f)
         ;; And find a minimum across mapped values
         (apply min))))

(solve-with "05/input.txt" part01)

;; And this get us a first star in the day.

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html "<img src='https://raw.githubusercontent.com/DrearyLisper/aoc-2023/master/images/first_part.png' alt='first_part' style='width: 100%; text-align: center;'/>")

;; ### Part 02 

;; #### Problem statement


^{:nextjournal.clerk/visibility {:code :hide :result :show}}
^::clerk/no-cache
(clerk/html (slurp "problems/05/part02.html"))

;; #### Solution

;; The change in second part is quite drastic!

;; Instead of doing mapping for one seed at the time we need to operate with ranges of seeds. Theoretically we just could run solution from the first part for every seed value in all ranges in the second part, but if you take a look to your input.txt you will see that those regions contain billions of values.
;; Making this path unfeasible.

;; So instead we need to change our mapping unit from one value to a range of values. Let's see how it parsing of seeds changed given new information.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn parse-seeds-range [line]
  (let [[_ seeds-str] (str/split line #":")]
    (->> (str/trim seeds-str)
         (#(str/split % #" "))
         (map bigint)
         (partition 2)
         (map vec)
         (map (fn [[a b]] {:left a :right (- (+ a b) 1)})))))
(parse-seeds-range "seeds: 79 14 55 13")

;; Now instead of single values we have two ranges of values defined by their left and right ends.

;; The function to parse and create maps will become even more trickier than before. I will leave comments inside of the code to explain its details.

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn parse-map-range [line]
  (let [components (rest (str/split line #"\n"))
        ;; Parsing of the components stays the same
        parse-component (fn
                          [component]
                          (let [[a b c] (str/split component #" ")]
                            {:from-offset (bigint b)
                             :to-offset (bigint a)
                             :range (bigint c)})) 
        ;; Map is still being represented by set of components                     
        parsed-components (map parse-component components)
        ;; This function removes impossible ranges, when right end is greater than the left end.
        filter-impossible (fn [ranges]
                            (vec (filter #(<= (:left %) (:right %)) ranges))) 
        ;; Now in apply-component we need to handle spliting of ranges into multiple ones.
        ;; It will be returning two set of ranges:
        ;; - First set will contain ranges which were mapped succesfully                    
        ;; - Second set will contain ranges that were left unmapped
        apply-component (fn
                          [component seed-range]
                          (let [seed-left (:left seed-range)
                                seed-right (:right seed-range)
                                map-from-left (:from-offset component)
                                map-from-right (- (+ (:from-offset component) (:range component)) 1)
                                map-to-left (:to-offset component)
                                map-to-right (- (+ (:to-offset component) (:range component)) 1)]
                            ;; In total we need to cover 4 cases.
                            (cond
                              ;; If left end of the mapped region is inside the range
                              (and (>= map-from-left seed-left) (<= map-from-left seed-right))
                              [
                               [
                                {:left map-to-left :right (+ map-to-left (- (min seed-right map-from-right) map-from-left))}
                               ]
                               (filter-impossible [{:left seed-left :right (- map-from-left 1)}
                                                   {:left (+ 1 map-from-right) :right seed-right}])]

                              ;; If right end of the mapped region is inside the range
                              (and (>= map-from-right seed-left) (<= map-from-right seed-right))
                              [
                               [
                                {:left (+ map-to-left (- (max map-from-left seed-left) map-from-left)) :right map-to-right}
                               ]
                               (filter-impossible [{:left (max seed-left map-from-left) :right (- map-from-left 1)}
                                                   {:left (+ 1 map-from-right) :right seed-right}])]

                              ;; If range is fully covered by a mapping
                              (and (< map-from-left seed-left) (> map-from-right seed-right))
                              [
                               [
                                {:left (+ map-to-left (- seed-left map-from-left)) :right (+ map-to-left (- seed-right map-from-left))}
                               ]
                               []]

                              ;; If range is fully not covered by a mapping
                              :else
                              [[]
                               [seed-range]])))

        ;; This function will be taking all components of the mapping and applying them to all ranges we have in a vector.
        apply-components' (fn
                            [f components seed-ranges]
                            (if (empty? components)
                              seed-ranges
                              (let [component (first components)
                                    mapping-results (map (partial apply-component component) seed-ranges)
                                    a (apply concat (map #(get % 0) mapping-results))
                                    b (apply concat (map #(get % 1) mapping-results))
                                    r (f f (rest components) b)]
                                (concat a r))))
        apply-components (fn
                           [seed-ranges]
                           (apply-components' apply-components' parsed-components seed-ranges))]
    ;; Again as in previous part we will be returning a function than can map regions instead of single values.
    apply-components))

(parse-map-range "\n50 98 2")

;; The output of the function will be a another function again. So let's try to use it to see how it works.

(map (parse-map-range "\n50 98 2") [[{:left 96N :right 100N}]])

;; This map split a range into three ranges. Where only one of them was succesfully mapped.

;; Let's put everything together to solve a second part!

^{:nextjournal.clerk/visibility {:code :show :result :hide}}
(defn part02 [lines]
  (let [content (str/join "\n" lines)
        components (str/split content #"\n\n")
        seeds (vec (parse-seeds-range (first components)))
        maps (map parse-map-range (rest components))
        ;; Combining all maps into one with composition
        f (->> maps reverse (reduce comp))]
    (->> seeds
         (f)
         ;; Take left ends of the regions
         (map #(:left %))
         ;; And find minimum across them
         (apply min))))

(solve-with "05/input.txt" part02)

;; And finally after all struggle we got a second star!

^{:nextjournal.clerk/visibility {:code :hide :result :show}}
(clerk/html "<img src='https://raw.githubusercontent.com/DrearyLisper/aoc-2023/master/images/second_part_05.png' alt='first_part' style='width: 100%; text-align: center;'/>")

;; If you liked my solution please star me on [GitHub](https://github.com/DrearyLisper/aoc-2023)!

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn solve [filename]
  (println (solve-with filename part01))
  (println (solve-with filename part02)))

^{:nextjournal.clerk/visibility {:code :hide :result :hide}}
(defn run [_]
  (solve "05/input.txt"))