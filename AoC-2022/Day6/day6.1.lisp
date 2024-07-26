(defparameter *data-file* "AoC/Day6/aoc_6.txt")

(defun is-start-marker (l1 marker-length)
  (= marker-length (length (remove-duplicates l1))))

(defun locate-start-marker (l1 marker-length &optional (index 0) (last-four nil))
  (if l1
      (let ((new-last-four (subseq
                             (cons (car l1) last-four)
                             0 (min marker-length (1+ (length last-four))))))
        (if (is-start-marker new-last-four marker-length)
            (1+ index)
            (locate-start-marker (cdr l1) marker-length (1+ index) new-last-four)))
      nil))

#|
(locate-start-marker (coerce "bvwbjplbgvbhsrlpgdmjqwftvncz" 'list))
(locate-start-marker (coerce "nppdvjthqldpwncqszvftbrmjlhg" 'list))
(locate-start-marker (coerce "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg" 'list))
(locate-start-marker (coerce "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw" 'list))
|#


(locate-start-marker (coerce "mjqjpqmgbljsphdztnvjfqwrcgsmlb" 'list) 14)


(with-open-file (in *data-file*)
  (let ((line (read-line in)))
    (locate-start-marker (coerce line 'list) 14)))
