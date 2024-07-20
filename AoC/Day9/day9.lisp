(ql:quickload "trivia")

(defpackage :day9
  (:use :cl :trivia))

(in-package :day9)

(defparameter *data-file*
  "/home/stephane/GitRepos/LearningLisp/AoC/Day9/aoc_9.txt"
  "This puzzle's data file")

(defun parse-line (line)
  (let ((split-line (uiop:split-string line :separator " ")))
    (list (intern (car split-line))
	  (parse-integer (cadr split-line)))))

(defun parse-lines (lines)
  (cond ((null lines) nil)
	(t (cons (parse-line (car lines)) (parse-lines (cdr lines))))))

(defun compute-step-x (direction x)
  (cond ((eq direction 'U) (1+ x))
	((eq direction 'D) (1- x))
	(t x)))

(defun compute-step-y (direction y)
  (cond ((eq direction 'L) (1- y))
	((eq direction 'R) (1+ y))
	(t y)))


(defun compute-steps (positions instruction)
  (let* ((current-position (car positions))
	 (pos-x (car current-position))
	 (pos-y (cadr current-position)))
    (match instruction
      ((list _ 0) positions)
      ((list direction steps) (let ((new-x (compute-step-x direction pos-x))
				    (new-y (compute-step-y direction pos-y)))
				(compute-steps (cons (list new-x new-y) positions)
					       (list direction (1- steps))))))))

(defun compute-head-positions (positions instructions)
  (cond ((null instructions) positions)
	(t (let ((updated-positions (compute-steps positions (car instructions))))
	     (compute-head-positions updated-positions (cdr instructions))))))

(defun unit (d)
  (cond ((< d 0) -1)
	((> d 0) 1)
	(t 0)))

(defun move-tail (tail-position head-position)
  (let* ((pos-x (car tail-position))
	 (pos-y (cadr tail-position))
	 (head-x (car head-position))
	 (head-y (cadr head-position))
	 (dx (- head-x pos-x))
	 (dy (- head-y pos-y)))
    (cond ((and (<= (abs dx) 1)
	       (<= (abs dy) 1)) (list pos-x pos-y)) ; no need to move the tail
	  (t (list (+ pos-x (unit dx)) (+ pos-y (unit dy)))))))


(defun compute-tail-positions (tail-positions head-positions)
  (cond ((null head-positions) tail-positions)
	(t (let* ((tail-position (car tail-positions))
		  (new-tail-pos (move-tail tail-position (car head-positions))))
	     (compute-tail-positions (cons new-tail-pos tail-positions) (cdr head-positions))))))

(defun count-tail-positions (lines)
  (let* ((parsed-lines (parse-lines lines))
	 (head-positions (reverse (compute-head-positions '((0 0)) parsed-lines)))
	 (tail1-positions (reverse (compute-tail-positions '((0 0)) (cdr head-positions))))
	 (tail2-positions (reverse (compute-tail-positions '((0 0)) (cdr tail1-positions))))
	 (tail3-positions (reverse (compute-tail-positions '((0 0)) (cdr tail2-positions))))
	 (tail4-positions (reverse (compute-tail-positions '((0 0)) (cdr tail3-positions))))
	 (tail5-positions (reverse (compute-tail-positions '((0 0)) (cdr tail4-positions))))
	 (tail6-positions (reverse (compute-tail-positions '((0 0)) (cdr tail5-positions))))
	 (tail7-positions (reverse (compute-tail-positions '((0 0)) (cdr tail6-positions))))
	 (tail8-positions (reverse (compute-tail-positions '((0 0)) (cdr tail7-positions))))
	 (tail9-positions (reverse (compute-tail-positions '((0 0)) (cdr tail8-positions))))
	 (unique-tail-positions (remove-duplicates tail9-positions :test #'equal))
	 (count-unique-tail-positions (length unique-tail-positions)))
    count-unique-tail-positions))


(let* ((lines (uiop:read-file-lines *data-file*))
       (tail-position-count (count-tail-positions lines)))
  (format t "The tail took ~A unique positions~%" tail-position-count)
  tail-position-count)
