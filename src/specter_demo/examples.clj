(ns specter-demo.examples
  (:use [com.rpl.specter]
        [com.rpl.specter.macros]
        [clojure.pprint :only [pprint]]
        [com.rpl.specter.impl :only [benchmark]])
  (:require [clojure.string :as str]))

(defmacro print-results [form]
  (let [val (last form)]
    `(let [res# ~form]
       (println " ")
       (println " ")
       (pprint ~val)
       (println "->")
       (pprint res#)
       )))

(comment
  (print-results
   (select [ALL :a even?]
           [{:a 2 :b 3} {:a 1} {:a 4}]))


  (print-results
   (transform [ALL :a even?]
              dec
              [{:a 2 :b 3} {:a 1} {:a 4}]))

  ;;=>input
  [{:a 2 :b 3} {:a 1} {:a 4}]
  ;;=>ALL
  {:a 2 :b 3}
  {:a 1}
  {:a 4}
  ;;=>:a
  2
  1
  4
  ;;=>even?
  2
  4
  ;;=> dec
  1
  3
  ;;=>even?
  1
  1
  3
  ;;=>:a
  {:a 1 :b 3}
  {:a 1}
  {:a 3}
  ;;=>ALL
  [{:a 1 :b 3} {:a 1} {:a 3}]
  ;;=>output


  (print-results
   (transform [ALL :a even?]
              dec
              '({:a 2 :b 3} {:a 1} {:a 4})))

  (print-results
   (transform [(filterer odd?) LAST]
              inc
              [1 2 3 4 5 6 7 8 9 18 12 14]))

  ;;=>input
  [1 2 3 4 5 6 7 8 9 18 12 14]
  ;;=>(filterer odd?)
  [1 3 5 7 9]
  ;;=>LAST
  9
  ;;=>inc
  10
  ;;=>LAST
  [1 3 5 7 10]
  ;;=>(filterer odd?)
  [1 2 3 4 5 6 7 8 10 18 12 14]
  ;;=>output


  (print-results
   (transform (srange 3 9)
              reverse
              [1 2 3 4 5 6 7 8 9 10 11 12]))

  (print-results
   (transform [(srange 1 4) ALL odd?]
              #(+ % 10)
              [0 1 2 3 4 5 6 7]))

  (print-results
   (transform (srange 2 4)
              (fn [_] [-1 -1 -1 -1 -1])
              [0 1 2 3 4 5 6 7 8 9]))

  (print-results
   (setval (srange 2 4)
           [-1 -1 -1 -1 -1]
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
            (selected? (filterer even?) (view count)
                       #(>= % 2))
            END]
           [:c :d]
           [[1 2 3 4 5 6] [7 0 -1] [8 8] []]))

  )

;; show implementations of keyword and ALL

;; back to bank examples

(def DATA {:a {:b {:c 1}}})


;; comp-paths strips protocol invocation and directly connects
;; the selectors to each other
(def compiled-path (comp-paths :a :b :c))

(defn manual-transform [data]
  (update data
          :a
          (fn [d1]
            (update d1
                    :b
                    (fn [d2]
                      (update d2 :c inc))))))

(comment
  (benchmark 1000000 #(get-in DATA [:a :b :c]))

  (benchmark 1000000 #(select [:a :b :c] DATA))

  (benchmark 1000000 #(select compiled-path DATA))

  (benchmark 1000000 #(compiled-select compiled-path DATA))

  (benchmark 1000000 #(-> DATA :a :b :c vector))

  (benchmark 1000000 #(update-in DATA [:a :b :c] inc))

  (benchmark 1000000 #(transform [:a :b :c] inc DATA))

  (benchmark 1000000 #(transform compiled-path inc DATA))

  (benchmark 1000000 #(compiled-transform compiled-path inc DATA))

  (benchmark 1000000 #(manual-transform DATA))

  )

;; example of late-bound parameterization

(defn reverse-matching-in-range [aseq start end predicate]
  (transform [(srange start end) (filterer predicate)]
             reverse
             aseq))

(def MATCHING-RANGE (comp-paths srange (filterer pred)))
(defn reverse-matching-in-range-fast [aseq start end predicate]
  (compiled-transform (MATCHING-RANGE start end predicate)
                      reverse
                      aseq))

(def RANGE [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 20])

(comment
  (benchmark 100000 #(reverse-matching-in-range RANGE 4 11 odd?))
  (benchmark 100000 #(reverse-matching-in-range-fast RANGE 4 11 odd?))
  )


(def param-compiled (comp-paths keypath keypath keypath))

(comment
  (benchmark 1000000 #(update-in DATA [:a :b :c] inc))
  (benchmark 1000000 #(compiled-transform compiled-path inc DATA))
  (benchmark 1000000 #(compiled-transform (param-compiled :a :b :c) inc DATA))
  )

;; back to bank examples


(comment
  (transform [TOPSORT
              (collect PARENTS NODE :name)
              NODE
              (collect-one :name)
              :royal-name
              ]
             (fn [parent-names name _]
               (str name " of " (str/join ", " parent-names)))
             ancestry-graph
             ))
