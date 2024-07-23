(defpackage day10/tests/main
  (:use :cl
        :day10
        :rove))
(in-package :day10/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :day10)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
