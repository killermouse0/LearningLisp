(ql:quickload "str")

(defparameter *max-level* 100)
(defparameter *data-file* "poem2.txt")

(defun read-level ()
  (print "Choose a level between 0 and 100:")
  (terpri)
  (let ((l (parse-integer (read-line) :junk-allowed t)))
    (cond ((and (numberp l) (<= l *max-level*)) l)
          (t (print l) (read-level)))))

(defun biased-random (level)
  (> (random *max-level*) (- *max-level* level)))

(defun split-poem (lst)
  (if (car lst)
      (cons (str:split " " (car lst)) (split-poem (cdr lst)))
      nil))

(defun read-file (in &optional (lines '()))
  (let ((line (read-line in nil :eof)))
    (cond ((eq line :eof) (reverse lines))
          (t (read-file in (cons line lines))))))

(defun load-file (filename)
  (with-open-file (in filename)
    (split-poem (read-file in))))

(defun mask-word (word)
  (format NIL "[~{~A~}]" (make-list (length word) :initial-element "x")))

(defun mask-words-line (level line)
  (if (car line)
      (cons
        (if (str:blankp (car line))
            (car line)
            (if (biased-random level)
                (mask-word (car line))
                (car line)))
        (mask-words-line level (cdr line)))))

(defun mask-words-poem (level poem)
  (if (car poem)
      (cons (mask-words-line level (car poem)) (mask-words-poem level (cdr poem)))
      nil))

(defun print-poem-1 (poem &optional (row-num 1))
  (if (car poem)
      (progn (format T "~2,'0d. ~{~a~^ ~}" row-num (car poem))
             (terpri)
             (print-poem-1 (cdr poem) (1+ row-num)))))

(defun print-poem (poem)
  (format T ">>>>>>>>>>~%")
  (print-poem-1 poem)
  (format T "<<<<<<<<<<~%"))

(defun teach-poem (poem)
  (let ((l (read-level)))
    (print-poem poem)
    (print-poem (mask-words-poem l poem))
    (teach-poem poem)))

(teach-poem (load-file *data-file*))
