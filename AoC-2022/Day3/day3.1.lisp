(defparameter *data-file* "AoC/Day3/aoc_3.txt")

(defun find-single-duplicate-distinct (l1 l2)
  (let ((m (member (car l1) l2)))
    (cond ((not m) (find-single-duplicate-distinct (cdr l1) l2))
          (t (car m)))))

(defun find-single-duplicate (l1 l2)
  (let ((distinct-l1 (remove-duplicates l1)))
    (find-single-duplicate-distinct distinct-l1 l2)))


(defun check-rucksack (content-str)
  (let* ((content-list (coerce content-str 'list))
         (content-length (length content-list))
         (compartment-a (subseq content-list 0 (/ content-length 2)))
         (compartment-b (subseq content-list (/ content-length 2))))
    (find-single-duplicate compartment-a compartment-b)))

(defun compute-priority (l)
  (let ((l-char-code (char-code l)))
    (cond ((<= (char-code #\a) l-char-code (char-code #\z)) (1+ (- l-char-code (char-code #\a))))
              ((<= (char-code #\A) l-char-code (char-code #\Z)) (+ 27 (- l-char-code (char-code #\A)))))))

(defun process-stream (in &optional (acc 0))
    (let ((line (read-line in nil :eof)))
        (cond ((eql line :eof) acc)
              (t (process-stream in (+ acc (compute-priority (check-rucksack line)))))))
    )

(with-open-file (in *data-file*)
    (process-stream in))


(* 1 (compute-priority #\r))

#|
(check-rucksack "vJrwpWtwJgWrhcsFMMfFFhFp")
(check-rucksack "jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL")
(check-rucksack "PmmdzqPrVvPwwTWBwg")
(check-rucksack "wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn")
(check-rucksack "ttgJtRGJQctTZtZT")
(check-rucksack "CrZsJsPPZsGzwwsLwLmpwMDw")
|#
