(ql:quickload "str")

(defparameter *data-file* "AoC/Day4/aoc_4.txt")

(defun is-range-contained (r1 r2)
  (destructuring-bind ((s1 . e1) (s2 . e2)) (list r1 r2)
    (if (or
         (and (<= s1 s2) (<= e2 e1))
         (and (<= s2 s1) (<= e1 e2)))
        1
        0)))

(defun is-range-overlapping (r1 r2)
  (destructuring-bind ((s1 . e1) (s2 . e2)) (list r1 r2)
    (if (or (<= s1 s2 e1) (<= s1 e2 e1) (<= s2 s1 e2) (<= s2 e1 e2))
        1
        0)))

(defun str-to-range (str)
  (let ((l1 (str:split "-" str)))
    (cons (parse-integer (first l1)) (parse-integer (second l1)))))

(defun process-file-contained-ranges (in &optional (acc 0))
  (let ((line (read-line in nil :eof)))
    (if (eql line :eof)
        acc
        (let* ((pairs (str:split "," line))
               (r1 (str-to-range (first pairs)))
               (r2 (str-to-range (second pairs))))
          (process-file-included-ranges in (+ (is-range-contained r1 r2) acc))))))

(defun process-file-overlapping-ranges (in &optional (acc 0))
  (let ((line (read-line in nil :eof)))
    (if (eql line :eof)
        acc
        (let* ((pairs (str:split "," line))
               (r1 (str-to-range (first pairs)))
               (r2 (str-to-range (second pairs))))
          (process-file-overlapping-ranges in (+ (is-range-overlapping r1 r2) acc))))))


(with-open-file (in *data-file*)
  (process-file-contained-ranges in))

(with-open-file (in *data-file*)
  (process-file-overlapping-ranges in))