(ns lab3.core-test
  (:require [clojure.test :refer :all]
            [lab3.core :as core]
            [lab3.interpolation :as interp]))

(deftest parse-point-test
  (testing "Parsing input strings"
    (is (= {:x 1.0 :y 2.0} (core/parse-point "1 2")))
    (is (= {:x 1.0 :y 2.0} (core/parse-point "1;2")))
    (is (= {:x 1.0 :y 2.0} (core/parse-point "1,2")))
    (is (nil? (core/parse-point "")))
    (is (nil? (core/parse-point "abc def")))))

(deftest process-point-linear-test
  (testing "Stream processing: linear interpolation"
    (let [opts   {:linear? true :newton? false :step 1 :n 4}
          state0 (interp/init-state)

          ; первая точка, еще ничего не готово
          {:keys [state outputs]} (interp/process-point opts state0 {:x 0 :y 0})
          _ (is (= [] outputs))

          ; после второй можем начинать
          {:keys [state outputs]} (interp/process-point opts state {:x 1 :y 1})]

      (is (= [{:alg :linear :x 0.0 :y 0.0}
              {:alg :linear :x 1.0 :y 1.0}]
             outputs)))))
