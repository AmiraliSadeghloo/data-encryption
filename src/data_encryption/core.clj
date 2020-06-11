(ns data-encryption.core
  (:gen-class))

(defn -main
  "I don't do a whole lot ... yet."
  [& args]
  (println "Hello, World!"))
;;//////////////////////
(def LazySquare
  ((fn sqr [n]
     (lazy-seq (cons (* n n) (sqr (inc n)))))
   1))

(def squareList (into [] (take 50 LazySquare)))             ;;vector of first 50 square nums
;;//////////////////////
(def LazyRectangle
  ((fn sqr [n]
     (lazy-seq (cons (* n (dec n)) (sqr (inc n)))))
   1))

(def rectangleList (into [] (take 50 LazyRectangle)))       ;; vector of first 50 ((n-1) * n)
;;//////////////////////
(defn inp []                                                ;;get vector input / count elements
  (println "Input data vector")
  (let [input (read)]
    input))

(def input (inp))
(def inputCount (count input))
(println input)
(println inputCount)
;;//////////////////////
(def countToSquare                                          ;;returns min sqaurelist above inputcount[row square]
  (fn []
    (loop [i 0 square squareList]
      (if (or (= (nth square i) inputCount) (> (nth square i) inputCount))
        [(inc i) (nth square i)]
        (recur (inc i) square)))))
(countToSquare)
;;//////////////////////
(def countToRectangle                                       ;;returns min rectanglelist above inputcount[row rectangle]
  (fn []
    (loop [i 0 rectangle rectangleList]
      (if (or (= (nth rectangle i) inputCount) (> (nth rectangle i) inputCount))
        [i (nth rectangle i)]
        (recur (inc i) rectangle)))))
(countToRectangle)
;;//////////////////////
(def whichOne                                               ;;if true -> square , false -> rectangle
  (fn []
    (true? (> (last (countToRectangle)) (last (countToSquare))))))
(whichOne)
;;//////////////////////
(def diff                                                   ;;difference between inputCount and (rectangle or square)
  (fn []
    (if (= true (whichOne))
      (- (last (countToSquare)) inputCount)
      (- (last (countToRectangle)) inputCount))))
(diff)
;;//////////////////////
(defn squareAdd []                                          ;; adds "" to input to make it square
  (loop [i (diff) newSquare input]
    (if (= i 0)
      newSquare
      (recur (dec i) (conj newSquare "fillfillfill")))))
(squareAdd)
;;//////////////////////
(defn rectangleAdd []                                       ;;adds "fillfillfill" to input to make it rectangle
  (loop [i (diff) newRectangle input]
    (if (= i 0)
      newRectangle
      (recur (dec i) (conj newRectangle "fillfillfill")))))
(rectangleAdd)
;;//////////////////////
(defn parting [whichOne]                                    ;;partitionates input based on rec/sqr
  (if (= true whichOne)
    (mapv vec (partition (first (countToSquare)) (squareAdd)))
    (mapv vec (partition (inc (first (countToRectangle))) (rectangleAdd)))))
(parting whichOne)
;;//////////////////////
(def saveLast (drop-last (diff) (last (parting whichOne)))) ;;saves last real row
(println saveLast)
;;//////////////////////
(def droping (drop-last 1 (parting whichOne)))              ;;drops last row
(println droping)
;;//////////////////////
(defn result []                                             ;;conj all = result
  (conj (into [] droping) (into [] saveLast)))
(result)
;;//////////////////////
(defn applyMap [result]                                     ;;maps all vectors cols
  (apply map vector result))
;;//////////////////////
(def normalize (reduce into [] (applyMap (parting whichOne)))) ;;turns all vectors to 1
;;//////////////////////
(defn removeJunk []                                         ;;filters "fillfillfill"
  (reduce (fn [a b]
            (if (not= b "fillfillfill")
              (conj a b)
              a))
          []
          normalize))
(removeJunk)                                                ;;output
;;///////done///////////


