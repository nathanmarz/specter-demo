(ns specter-demo.bank
  (:use [com.rpl.specter]
        [com.rpl.specter.macros]
        [clojure.pprint :only [pprint]]
        [com.rpl.specter.impl :only [benchmark]]))

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

(defn pay-fee [world]
  (transfer world
            [:people ALL :money]
            [:bank :funds]
            1)
  )

(defn bank-give-dollar [world]
  (transfer world
            [:bank :funds]
            [:people ALL :money]
            1)
  )

(defn pay-poor-fee [world]
  (transfer world
            [:people ALL :money #(< % 3000)]
            [:bank :funds]
            50)
  )

(defn rich-people [world]
  (select [:people
           ALL
           (selected? :money #(>= % 1000000000))
           :name]
          world))


(defn user [name]
  [:people
   ALL
   (selected? :name
              #(= % name))])

(defn transfer-users [world from to amt]
  (transfer world
            [(user from) :money]
            [(user to) :money]
            amt))

(defn bank-loyal-bonus
  "Bank gives $5000 to earliest three users"
  [world]
  (transfer world
            [:bank :funds]
            [:people (srange 0 3) ALL :money]
            5000))

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
             (selected? (view empty?) not)
             LAST
             :money]
            1000))

(defn mark-wealth-status [world]
  (setval [:people
           ALL
           (if-path [:money #(>= % 100000)]
                    :rich
                    :not-so-rich)]
          true
          world))



(def user-compiled
  (comp-paths :people
              ALL
              (selected? :name
                         (paramsfn [name]
                           [elem]
                           (= name elem))
                           )))

(def user-money-compiled (comp-paths user-compiled :money))

(def BANK-MONEY (comp-paths :bank :funds))

(defn user->bank-compiled [world name amt]
  (transfer world (user-money-compiled name) BANK-MONEY amt)
  )


(comment
  (benchmark 100000 #(user->bank world "John Smith" 25))
  (benchmark 100000 #(user->bank-fast world "John Smith" 25))
  )
