(ns lab3.properties-test
  (:require [clojure.test :refer :all]
            [clojure.test.check.generators :as gen]
            [clojure.test.check.properties :as prop]
            [clojure.test.check.clojure-test :refer [defspec]]
            [lab3.interpolation :as interp]))

; Если функция линейная, linear-value должен попадать точно

(defspec linear-exactness-test
  100
  (prop/for-all
   [a gen/int
    b gen/int
    x1 (gen/double* {:min -10 :max  0})
    x2 (gen/double* {:min   1 :max 10})]
   (let [f #(double (+ (* a %) b))
         p1 {:x x1 :y (f x1)}
         p2 {:x x2 :y (f x2)}
         mid (/ (+ x1 x2) 2)
         expected (f mid)
         actual (interp/linear-value [p1 p2] mid)]
     (= expected actual))))

; Монотонность: y_mid лежит между y_1 и y_2

(defspec linear-between-test
  100
  (prop/for-all
   [x1 (gen/double* {:min -10 :max 10})
    x2 (gen/double* {:min -10 :max 10})
    y1 (gen/double* {:min -10 :max 10})
    y2 (gen/double* {:min -10 :max 10})]
   (let [a (min x1 x2)
         b (max x1 x2)
         p1 {:x a :y y1}
         p2 {:x b :y y2}
         mid (/ (+ a b) 2)
         ymid (interp/linear-value [p1 p2] mid)]
     (<= (min y1 y2) ymid (max y1 y2)))))
