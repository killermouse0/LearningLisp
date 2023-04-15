(ql:quickload "str")

(defparameter *data-file* "AoC/Day5/aoc_5.txt")

(defun parse-crate (c)
  (if (equal c #\Space)
      nil
      (intern (string-upcase (string c)))))

(defun parse-line (line-str)
  (loop :for i :from 1 :to (length line-str) :by 4 :collect (parse-crate (aref line-str i))))

(defun parse-initial-arrangement-file (in &optional (arrangement nil) (max-length 0))
  (let ((line (read-line in)))
    (if (equal line "")
        (values max-length (cdr arrangement))
        (let ((parsed-line (parse-line line)))
          (parse-initial-arrangement-file
            in
            (cons parsed-line arrangement)
            (max max-length (length parsed-line)))))))

(defun fill-row (v row)
  (loop
 for e :in row
 for i :from 0 :to (length v)
   :if e :do (setf (aref v i) (cons e (aref v i)))))

(defun transpose-arrangement (v arrangement)
  (if (not arrangement)
      v
      (progn (fill-row v (car arrangement))
             (transpose-arrangement v (cdr arrangement)))))

(defun read-move (in)
  (let ((move (read-line in nil :eof)))
    (if (equal move :eof)
        nil
        (let ((move-list (str:split " " move)))
          (mapcar #'parse-integer (list (nth 1 move-list) (nth 3 move-list) (nth 5 move-list)))))))


(defun displace-crates-9000 (count arrangement from to)
  (when (> count 0)
        (setf
          (aref arrangement to)
          (cons (car (aref arrangement from)) (aref arrangement to)))
        (setf (aref arrangement from) (cdr (aref arrangement from)))
        (displace-crates-9000 (1- count) arrangement from to)))

(defun stack (l1 l2)
    (if l1
        (stack (cdr l1) (cons (car l1) l2))
        l2))

(stack '(a b c d) '(1 2 3 4))


(defun displace-crates-9001 (count arrangement from to &optional (acc nil))
  (if (> count 0)
      (let ((top-from (car (aref arrangement from))))
        (setf (aref arrangement from) (cdr (aref arrangement from)))
        (displace-crates-9001 (1- count) arrangement from to (cons top-from acc)))
      (setf (aref arrangement to) 
            (stack acc (aref arrangement to)))))

(defun apply-moves (in arrangement)
  (let ((move (read-move in)))
    (if move
        (multiple-value-bind (count from to) (values-list move)
          (displace-crates-9001 count arrangement (- from 1) (- to 1))
          (apply-moves in arrangement))
        arrangement)))

(print (map 'list #'car
         (with-open-file (in *data-file*)
           (multiple-value-bind (max-length arrangement) (parse-initial-arrangement-file in)
             (let ((transposed-arrangement (transpose-arrangement (make-array max-length :initial-element nil)
                                                                  arrangement)))
               (apply-moves in transposed-arrangement))))))
