(defparameter *data-file*
  "/home/stephane/GitRepos/LearningLisp/AoC/Day8/aoc_8.txt"
  "The input file for the Day 8 puzzle")

(defun parse-line (line)
  (map 'list #'digit-char-p line))

(defun parse-lines (lines)
  (cond (lines (cons
		(parse-line (car lines))
		(parse-lines (cdr lines))))
	(t nil)))

(defun bigger-than-list (a elements)
  (cond ((null elements) t)
	((> a (car elements)) (bigger-than-list a (cdr elements)))
	(t nil)))

(defun collect-up (forest x y)
  (loop for _x from 0 below x collect (aref forest _x y)))

(defun collect-down (forest x y)
  (loop for _x from (1- (array-dimension forest 0)) above x collect (aref forest _x y)))

(defun collect-left (forest x y)
  (loop for _y from 0 below y collect (aref forest x _y)))

(defun collect-right (forest x y)
  (loop for _y from (1- (array-dimension forest 1)) above y collect (aref forest x _y)))

(defun visible (forest x y)
  (let ((dim-x (array-dimension forest 0))
	(dim-y (array-dimension forest 1))
	(tree-size (aref forest x y)))
    (cond ((equal x (1- dim-x)) t) ;; the tree is visible because at the border of the forest
	  ((equal x 0) t)
	  ((equal y (1- dim-y)) t)
	  ((equal y 0) t)
	  ((or (bigger-than-list tree-size (collect-up forest x y))
		(bigger-than-list tree-size (collect-down forest x y))
		(bigger-than-list tree-size (collect-left forest x y))
		(bigger-than-list tree-size (collect-right forest x y))) t)
	  (t nil))))

(defun get-visible-trees (forest)
  (loop for x from 0 below (array-dimension forest 0)
	append
	(loop for y from 0 below (array-dimension forest 1)
	      when (visible forest x y) collect (cons x y))))

(defun count-visible-trees-up (forest x y)
  (cond ((equal x 0) 0)
	(t (loop for _x from (1-  x) downto 0
		 summing 1 into visible-trees
		 while (> (aref forest x y) (aref forest _x y))
		 finally (return visible-trees)))))

(defun count-visible-trees-down (forest x y)
  (let ((dim-x (array-dimension forest 0)))
    (cond ((equal x (1- dim-x)) 0)
	  (t (loop for _x from (1+  x) below dim-x
		   summing 1 into visible-trees
		   while (> (aref forest x y) (aref forest _x y))
		   finally (return visible-trees))))))

(defun count-visible-trees-left (forest x y)
  (cond ((equal y 0) 0)
	(t (loop for _y from (1-  y) downto 0
		 summing 1 into visible-trees
		 while (> (aref forest x y) (aref forest x _y))
		 finally (return visible-trees)))))

(defun count-visible-trees-right (forest x y)
  (let ((dim-y (array-dimension forest 1)))
    (cond ((equal y (1- dim-y)) 0)
	  (t (loop for _y from (1+  y) below dim-y
		   summing 1 into visible-trees
		   while (> (aref forest x y) (aref forest x _y))
		   finally (return visible-trees))))))

(defun scenic-score (forest x y)
  (let* ((view-up (count-visible-trees-up forest x y))
	(view-down (count-visible-trees-down forest x y))
	(view-left (count-visible-trees-left forest x y))
	(view-right (count-visible-trees-right forest x y))
	(scenic-score (* view-up view-down view-left view-right)))
    scenic-score))


(defun find-best-scenic-score (forest)
  (let ((dim-x (array-dimension forest 0))
	(dim-y (array-dimension forest 1)))
    (loop for scenic-score
	    in 
	    (loop for x from 0 below dim-x
		  append
		  (loop for y from 0 below dim-y collect (scenic-score forest x y)))
	  maximizing scenic-score)))

;; part 1
(let* ((lines (uiop:read-file-lines *data-file*))
       (forest-list (parse-lines lines))
       (dim-x (length forest-list))
       (dim-y (length (car forest-list)))
       (forest (make-array
		(list dim-x dim-y)
		:initial-contents forest-list))
       (visible-trees (get-visible-trees forest))
       (max-scenic-score (find-best-scenic-score forest)))
  (format t "There are ~a visible trees in the forest~%" (length visible-trees))
  (format t "The max scenic score in the forest is ~a~%" max-scenic-score))
