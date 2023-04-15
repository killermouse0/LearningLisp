(defparameter *data-file* "AoC/Day1/aoc_1.txt")

(defun process (stream acc i-max i-total)
  (let* ((str-val (read-line stream nil :eof))
         (val (read-from-string (if (eq str-val :eof) "" str-val) nil :eol)))
    (cond ((equal str-val :eof) (cons i-max i-total))
          ((numberp val) (process stream (+ val acc) i-max i-total))
          ((eq val :eol)
            (cond ((<= acc i-total) (process stream 0 i-max i-total))
                  (t (process stream 0 (1+ i-max) acc)))))))


(let* ((in (open *data-file*)))
  (print (process in 0 0 0))
  (close in))