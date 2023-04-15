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

(defun identify-sticker (u1 u2 u3)
  (cond ((and (member (car u1) u2) (member (car u1) u3)) (car u1))
        (t (identify-sticker (cdr u1) u2 u3))))

(defun compute-group-sticker (str1 str2 str3)
  (let ((u1 (remove-duplicates (coerce str1 'list)))
        (u2 (remove-duplicates (coerce str2 'list)))
        (u3 (remove-duplicates (coerce str3 'list))))
    (identify-sticker u1 u2 u3)))

(defun process-stream (in &optional (acc 0))
  (let ((str1 (read-line in nil :eof))
        (str2 (read-line in nil))
        (str3 (read-line in nil)))
    (cond ((eql str1 :eof) acc)
          (t (process-stream in (+ acc (compute-priority (compute-group-sticker str1 str2 str3))))))))

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
