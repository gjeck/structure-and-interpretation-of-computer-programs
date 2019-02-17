(ns project-01.core-test
  (:require [clojure.test :refer :all]
            [project-01.core :refer :all]))

(deftest position-calculaton
  (testing "position-calculation"
    (is (= (position 0 0 0 0) 0.0))
    (is (= (position 0 0 20 0) 20.0))
    (is (= (position 0 5 10 10) 60.0))
    (is (= (position 2 2 2 2) 10.0))
    (is (= (position 5 5 5 5) 92.5))))
