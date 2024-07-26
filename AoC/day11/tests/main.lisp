(defpackage day11/tests/main
  (:use :cl
        :day11
        :rove))
(in-package :day11/tests/main)

;; NOTE: To run this test file, execute `(asdf:test-system :day11)' in your Lisp.

(deftest test-target-1
  (testing "should (= 1 1) to be true"
    (ok (= 1 1))))
