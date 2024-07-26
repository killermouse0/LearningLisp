(in-package :day11)

;; blah blah blah.

(defun main ()
  (let ((monkey-array
	  (make-array
	   8
	   :element-type 'monkey::monkey
	   :initial-contents
	   (list
	    (monkey::make-monkey
	      '(65 58 93 57 66)
	      (lambda (old) (* old 7))
	      (lambda (worry) (zerop (rem worry 19))))
	     (monkey::make-monkey
	      '(76 97 58 72 57 92 82)
	      (lambda (old) (+ old 4))
	      (lambda (worry) (zerop (rem worry 3))))
	     (monkey::make-monkey
	      '(90 89 96)
	      (lambda (old) (* old 5))
	      (lambda (worry) (zerop (rem worry 13))))
	     (monkey::make-monkey
	      '(72 63 72 99)
	      (lambda (old) (* old old))
	      (lambda (worry) (zerop (rem worry 17))))
	     (monkey::make-monkey
	      '(65)
	      (lambda (old) (1+ old))
	      (lambda (worry) (zerop (rem worry 2))))
	     (monkey::make-monkey
	      '(97 71)
	      (lambda (old) (+ old 8))
	      (lambda (worry) (zerop (rem worry 11))))
	     (monkey::make-monkey
	      '(83 68 88 55 87 67)
	      (lambda (old) (+ old 2))
	      (lambda (worry) (zerop (rem worry 5))))
	     (monkey::make-monkey
	      '(64 81 50 96 82 53 62 92)
	      (lambda (old) (+ old 5))
	      (lambda (worry) (zerop (rem worry 7))))))))
    (setf (monkey::monkey-true (aref monkey-array 0)) (aref monkey-array 6))
    (setf (monkey::monkey-false (aref monkey-array 0)) (aref monkey-array 4))

    (setf (monkey::monkey-true (aref monkey-array 1)) (aref monkey-array 7))
    (setf (monkey::monkey-false (aref monkey-array 1)) (aref monkey-array 5))

    (setf (monkey::monkey-true (aref monkey-array 2)) (aref monkey-array 5))
    (setf (monkey::monkey-false (aref monkey-array 2)) (aref monkey-array 1))

    (setf (monkey::monkey-true (aref monkey-array 3)) (aref monkey-array 0))
    (setf (monkey::monkey-false (aref monkey-array 3)) (aref monkey-array 4))

    (setf (monkey::monkey-true (aref monkey-array 4)) (aref monkey-array 6))
    (setf (monkey::monkey-false (aref monkey-array 4)) (aref monkey-array 2))

    (setf (monkey::monkey-true (aref monkey-array 5)) (aref monkey-array 7))
    (setf (monkey::monkey-false (aref monkey-array 5)) (aref monkey-array 3))

    (setf (monkey::monkey-true (aref monkey-array 6)) (aref monkey-array 2))
    (setf (monkey::monkey-false (aref monkey-array 6)) (aref monkey-array 1))

    (setf (monkey::monkey-true (aref monkey-array 7)) (aref monkey-array 3))
    (setf (monkey::monkey-false (aref monkey-array 7)) (aref monkey-array 0))

    (dotimes (n 10000)
      (dotimes (m 8)
	(monkey::process-items (aref monkey-array m))))

    (dotimes (m 8)
      (format t "Monkey ~A inspected items ~A times.~%" m (monkey::monkey-inspected (aref monkey-array m))))
    (apply #'* (subseq
		(sort (loop for m from 0 upto 7
			    collect (monkey::monkey-inspected (aref monkey-array m))) #'>) 0 2))))

(defun main-test ()
  (let ((monkey-array
	  (make-array
	   4
	   :element-type 'monkey::monkey
	   :initial-contents
	   (list
	    (monkey::make-monkey
	     '(79 98)
	     (lambda (old) (* old 19))
	     (lambda (worry) (zerop (rem worry 23))))
	    (monkey::make-monkey
	     '(54 65 75 74)
	     (lambda (old) (+ old 6))
	     (lambda (worry) (zerop (rem worry 19))))
	    (monkey::make-monkey
	     '(79 60 97)
	     (lambda (old) (* old old))
	     (lambda (worry) (zerop (rem worry 13))))
	    (monkey::make-monkey
	     '(74)
	     (lambda (old) (+ old 3))
	     (lambda (worry) (zerop (rem worry 17))))))))
    (setf (monkey::monkey-true (aref monkey-array 0)) (aref monkey-array 2))
    (setf (monkey::monkey-false (aref monkey-array 0)) (aref monkey-array 3))

    (setf (monkey::monkey-true (aref monkey-array 1)) (aref monkey-array 2))
    (setf (monkey::monkey-false (aref monkey-array 1)) (aref monkey-array 0))

    (setf (monkey::monkey-true (aref monkey-array 2)) (aref monkey-array 1))
    (setf (monkey::monkey-false (aref monkey-array 2)) (aref monkey-array 3))

    (setf (monkey::monkey-true (aref monkey-array 3)) (aref monkey-array 0))
    (setf (monkey::monkey-false (aref monkey-array 3)) (aref monkey-array 1))
    

    (dotimes (n 10000)
      (dotimes (m 4)
	(monkey::process-items (aref monkey-array m))))

    (dotimes (m 4)
      (format t "Monkey ~A inspected items ~A times.~%" m (monkey::monkey-inspected (aref monkey-array m))))
    (apply #'* (subseq
		(sort (loop for m from 0 upto 3
			    collect (monkey::monkey-inspected (aref monkey-array m))) #'>) 0 2))))
