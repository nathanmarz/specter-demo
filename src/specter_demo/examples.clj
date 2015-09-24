(ns specter-demo.examples
  (:use [com.rpl.specter]
        [com.rpl.specter.macros]
        [clojure.pprint :only [pprint]]
        [com.rpl.specter.impl :only [benchmark]]))

(defn print-results [val]
  (pprint val)
  (println " "))

(comment
  (print-results
   (select [ALL :a even?]
           [{:a 1} {:a 2} {:a 4} {:a 3}]))


  (print-results
   (transform [ALL :a even?]
              inc
              [{:a 1} {:a 2} {:a 4} {:a 3}]))

  (print-results
   (transform [(filterer odd?) LAST]
              inc
              [2 1 3 6 9 4 8]))

  (print-results
   (transform (srange 3 8)
              reverse
              [1 2 3 4 5 6 7 8 9 10 11 12]))

  (print-results
   (transform [(srange 1 4) ALL odd?]
              inc
              [0 1 2 3 4 5 6 7]))

  (print-results
   (transform (srange 2 4)
              (fn [_] [-1 -1 -1])
              [0 1 2 3 4 5 6 7 8 9]))

  (print-results
   (setval (srange 2 4)
           [-1 -1 -1]
           [0 1 2 3 4 5 6 7 8 9]))

  (print-results
   (setval (srange 2 4)
           []
           [0 1 2 3 4 5 6 7 8 9]))

  (print-results
   (transform [(srange 4 11) (filterer even?)]
              reverse
              [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15]))

  (print-results
   (setval [ALL END]
           [:a :b]
           [[1] '(1 2) [:c]]))

  (print-results
   (transform [ALL (collect-one :b) :a even?]
              +
              [{:a 1 :b 3} {:a 2 :b -10} {:a 4 :b 10} {:a 3}]))

  (print-results
   (setval [ALL
            (selected? (filterer even?) (view count) #(>= % 2))
            END]
           [:c :d]
           [[1 2 3 4 5 6] [7 0 -1] [8 8] []]))

  )

;; show implementations of keyword and ALL

(def DATA {:a {:b {:c 1}}})

(def s (comp-paths :a :b :c))

(def c (comp vector :c :b :a))

(def p (comp-paths keypath keypath keypath))

(comment

  (benchmark 1000000 #(select [:a :b :c] DATA))

  (benchmark 1000000 #(select s DATA))

  (benchmark 1000000 #(compiled-select s DATA))

  (benchmark 1000000 #(get-in DATA [:a :b :c]))

  (benchmark 1000000 #(c DATA))

  (benchmark 1000000 #(-> DATA :a :b :c vector))

  (benchmark 1000000 #(compiled-select (p :a :b :c) DATA))

  (benchmark 1000000 #(update-in DATA [:a :b :c] inc))

  (benchmark 1000000 #(transform [:a :b :c] inc DATA))

  (benchmark 1000000 #(transform s inc DATA))

  (benchmark 1000000 #(compiled-transform s inc DATA))

  (benchmark 1000000 #(compiled-transform (p :a :b :c) inc DATA))






  )
