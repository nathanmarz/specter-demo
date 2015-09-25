(ns specter-demo.bank
  (:use [com.rpl.specter]
        [com.rpl.specter.macros]
        [clojure.pprint :only [pprint]]
        [com.rpl.specter.impl :only [benchmark]])
  (:require [com.rpl.specter [protocols :as p]]
            [clojure.core.reducers :as r]))

(declare world)
(defn print-results [val]
  (println " ")
  (pprint world)
  (println "->")
  (pprint val)
  (println " "))

(def world
  {:people [{:money 129827 :name "Alice Brown"}
            {:money 100 :name "John Smith"}
            {:money 6821212339 :name "Donald Trump"}
            {:money 2870 :name "Charlie Johnson"}
            {:money 8273821 :name "Charlie Rose"}
            ]
   :bank {:funds 4782328748273}}
  )

(defn user->bank [world name amt]
  (let [curr-funds (->> world
                        :people
                        (filter (fn [user] (= (:name user) name)))
                        first
                        :money
                        )]
    (if (< curr-funds amt)
     (throw (IllegalArgumentException. "Not enough funds!"))
     (-> world
         (update
          :people
          (fn [user-list]
            (mapv (fn [user]
                    (if (= (:name user) name)
                      (update user :money #(- % amt))
                      user
                      ))
                  user-list)))
         (update-in
          [:bank :funds]
          #(+ % amt))
         ))))

(comment
  (print-results
   (user->bank world "John Smith" 25))
  )


(defn transfer
  [world from-path to-path amt]
  (let [givers (select from-path world)

        receivers (select to-path world)

        total-receive (* amt (count givers))

        total-give (* amt (count receivers))]
    (if (every? #(>= % total-give) givers)
      (->> world
           (transform from-path #(- % total-give))
           (transform to-path #(+ % total-receive))
           )
      (throw (IllegalArgumentException. "Not enough funds!"))
      )))

;; show examples.clj

(defn pay-fee [world]
  (transfer world
            [:people ALL :money]
            [:bank :funds]
            1)
  )

(comment
  (print-results
   (pay-fee world))
  )

(defn bank-give-dollar [world]
  (transfer world
            [:bank :funds]
            [:people ALL :money]
            1)
  )

(comment
  (print-results
   (bank-give-dollar world))
  )

(defn pay-poor-fee [world]
  (transfer world
            [:people ALL :money #(< % 3000)]
            [:bank :funds]
            50)
  )

(comment
  (print-results
   (pay-poor-fee world))
  )

(defn rich-people [world]
  (select [:people
           ALL
           (selected? :money #(>= % 1000000000))
           :name]
          world))

(comment
  (print-results
   (rich-people world))
  )

(defn user [name]
  [:people
   ALL
   #(= (:name %) name)])

(defn transfer-users [world from to amt]
  (transfer world
            [(user from) :money]
            [(user to) :money]
            amt))

(comment
  (print-results
   (transfer-users world "Alice Brown" "John Smith" 10))
  )

(defn bank-loyal-bonus
  "Bank gives $5000 to earliest three users"
  [world]
  (transfer world
            [:bank :funds]
            [:people (srange 0 3) ALL :money]
            5000))

(comment
  (print-results
   (bank-loyal-bonus world))
  )

(defn add-person [world person]
  (setval [:people END]
          [person]
          world)
  )

(defn bank-recent-charity-bonus
  "Bank gives $1000 to most recent person with less than 5000 dollars"
  [world]
  (transfer world
            [:bank :funds]
            [:people
             (filterer [:money #(< % 5000)])
             LAST
             :money]
            1000))

(comment
  (print-results
   (bank-recent-charity-bonus world))
  )

(defn mark-wealth-status [world]
  (setval [:people
           ALL
           (if-path [:money #(>= % 100000)]
                    :rich
                    :poor)]
          true
          world))

(comment
  (print-results
   (mark-wealth-status world))
  )

(defn user->bank-uncompiled
  [world name amt]
  (transfer world [(user name) :money] [:bank :funds] amt))



(deftype AllVStructurePath [])

(extend-protocol p/StructurePath
  AllVStructurePath
  (select* [this structure next-fn]
    (into [] (r/mapcat next-fn structure)))
  (transform* [this structure next-fn]
    (mapv next-fn structure)
    ))

(def ALLV (->AllVStructurePath))

(def user-compiled
  (comp-paths :people
              ALLV
              (paramsfn [name]
                        [elem]
                        (= name (:name elem)))
              ))

(def user-money-compiled (comp-paths user-compiled :money))

(def BANK-MONEY (comp-paths :bank :funds))

(defn user->bank-compiled [world name amt]
  (transfer world (user-money-compiled name) BANK-MONEY amt)
  )


;; Not a direct comparison since Specter versionis built on top of
;; a *much* more general way of doing transfers
(comment
  (benchmark 100000 #(user->bank world "John Smith" 25))
  (benchmark 100000 #(user->bank-uncompiled world "John Smith" 25))
  (benchmark 100000 #(user->bank-compiled world "John Smith" 25))
  )

;; show graph example in examples.clj
