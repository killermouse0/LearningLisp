(in-package :day10)

(defun parse-line (line)
  (let ((parsed-line (uiop:split-string line :separator " ")))
    (match parsed-line
      ((list "noop")
       (list 1 0))
      ((list "addx" x)
       (list 2 (parse-integer x))))))


(defun parse-lines (lines)
  (when lines
    (cons (parse-line (car lines))
	  (parse-lines (cdr lines)))))


(defun append-op (acc op)
  (let* ((last-duration (car op))
	 (last-value (cadr op))
	 (total-duration (caar acc))
	 (total-value (cadar acc)))
    (cons (list (+ last-duration total-duration) (+ last-value total-value))
	  acc)))


(defun generate-timeline (parsed-lines)
  (iter (for p in parsed-lines)
    (reducing p by #'append-op initial-value '((0 1)))))


(defun parse-file (filename)
  (iter
    (for line in-file filename :using #'read-line)
    (if line (collect (parse-line line)))))


(defun x-at (ticks parsed-file)
  (when ticks
    (cond ((null (cdr parsed-file))
	   (cons (cadar parsed-file) (x-at (cdr ticks) parsed-file)))
	  ((<= (car ticks) (caadr parsed-file))
	   (cons (cadar parsed-file) (x-at (cdr ticks) parsed-file)))
	  (t
	   (x-at ticks (cdr parsed-file))))))


(defun day-10-1 (filename)
  (let* ((parsed-file (parse-file filename))
	 (timeline (cdr (reverse (generate-timeline parsed-file))))
	 (ticks-of-interest '(20 60 100 140 180 220))
	 (values-of-interest (x-at ticks-of-interest timeline))
	 (signals-of-interest
	   (iter (for tick in ticks-of-interest)
	     (for x in values-of-interest)
	     (collect (* tick x))))
	 (total (iter (for s in signals-of-interest) (summing s))))
    total))

(defun list-to-screen (screen-list n_cols j row)
  (if screen-list
    (if (equal n_cols j)
	(cons (coerce (reverse row) 'string) (list-to-screen screen-list n_cols 0 '()))
	(list-to-screen (cdr screen-list) n_cols (1+ j) (cons (car screen-list) row)))
    (list (coerce (reverse row) 'string))))

(defun day-10-2 (filename)
  (let* ((parsed-file (parse-file filename))
	 (timeline (reverse (generate-timeline parsed-file)))
	 (ticks (iter (for i from 0 to 240) (collect i)))
	 (values-x (cdr (x-at ticks timeline)))
	 (screen-list (iter
			(for tck in ticks)
			(for x in values-x)
			(collect (if (<= (1- x) (mod tck 40) (1+ x)) #\# #\.))))
	 (screen (list-to-screen screen-list 40 0 '())))
    (format t "~{~A~%~}~%" screen)))
    
