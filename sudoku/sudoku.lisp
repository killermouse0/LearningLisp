(proclaim '(debug 3))

(ql:quickload "str")

(defparameter *data-file* "sudoku/sudoku.3.txt")
(defparameter *sudoku-template*
              '("==========="
                (nil nil nil "|" nil nil nil "|" nil nil nil)
                (nil nil nil "|" nil nil nil "|" nil nil nil)
                (nil nil nil "|" nil nil nil "|" nil nil nil)
                "---+---+---"
                (nil nil nil "|" nil nil nil "|" nil nil nil)
                (nil nil nil "|" nil nil nil "|" nil nil nil)
                (nil nil nil "|" nil nil nil "|" nil nil nil)
                "---+---+---"
                (nil nil nil "|" nil nil nil "|" nil nil nil)
                (nil nil nil "|" nil nil nil "|" nil nil nil)
                (nil nil nil "|" nil nil nil "|" nil nil nil)
                "==========="))

(defun read-file (in)
  (let ((line (read-line in nil nil)))
    (when line
          (cons
            (mapcar (lambda (x) (if (digit-char-p x)
                                    (- (char-code x) (char-code #\0))
                                    NIL))
                (coerce line 'list))
            (read-file in)))))

(defun to-array (ll)
  (let ((a (make-array '(9 9))))
    (loop :for l :in ll
          :for i :from 0 :below 9 :do
            (loop :for c :in l
                  :for j :from 0 :below 9 :do (setf (aref a i j) c)))
    a))

(defun print-row (row content)
  (cond (row
          (if (car row)
              (progn (princ (car row))
                     (print-row (cdr row) content))
              (progn (princ (if (car content) (car content) "*"))
                     (print-row (cdr row) (cdr content)))))
        (t (terpri)
           content)))


(defun print-sudoku (content &optional (template *sudoku-template*))
  (terpri)
  (cond ((not template) nil)
        ((arrayp content) (print-sudoku (array-to-list content)))
        ((stringp (car template))
          (princ (car template))
          (terpri)
          (print-sudoku content (cdr template)))
        ((listp (car template))
          (print-sudoku
           (print-row (car template) content)
           (cdr template)))))

(defun array-to-list (sudoku)
  (loop :for i :below 9
          :nconc (loop :for j :below 9
                       :collect (aref sudoku i j))))

(defun n-to-section (i)
  (let ((section (floor (/ i 3))))
    (cons (* 3 section) (1- (* (1+ section) 3)))))

(defun find-row-neighbors (sudoku-grid i)
  (loop :for j :below 9 :if (aref sudoku-grid i j) :collect (aref sudoku-grid i j)))

(defun find-column-neighbors (sudoku-grid j)
  (loop :for i :below 9 :if (aref sudoku-grid i j) :collect (aref sudoku-grid i j)))

(defun find-block-neighbors (sudoku-grid i j)
  (let ((row-range (n-to-section i))
        (column-range (n-to-section j)))
    (loop :for i :from (car row-range) :upto (cdr row-range)
            :nconc (loop :for j :from (car column-range) :upto (cdr column-range)
                           :if (aref sudoku-grid i j) :collect (aref sudoku-grid i j)))))

(defun analyze-position (sudoku-grid i j)
  (let* ((row-neigbors (find-row-neighbors sudoku-grid i))
         (columns-neighbors (find-column-neighbors sudoku-grid j))
         (block-neighbors (find-block-neighbors sudoku-grid i j))
         (distinct-neighbors (remove-duplicates (append row-neigbors columns-neighbors block-neighbors)))
         (candidates (set-difference '(1 2 3 4 5 6 7 8 9) distinct-neighbors)))
    (list (length candidates) candidates)))

(defun locate-missing (sudoku-grid)
  (loop :for i :below 9 :nconc
          (loop :for j :below 9 :if (not (aref sudoku-grid i j)) :collect (cons i j))))

(defun analyze-missing (sudoku-grid)
  (let ((missing (locate-missing sudoku-grid)))
    (sort (loop :for x :in missing :collect (cons x (analyze-position sudoku-grid (car x) (cdr x)))) (lambda (x y) (< (second x) (second y))))))

(defun copy-update-sudoku (sudoku-grid i j val)
  (let ((sudoku-clone (make-array '(9 9))))
    (loop :for i :below 9 :do
            (loop :for j :below 9 :do
                    (setf (aref sudoku-clone i j)
                      (aref sudoku-grid i j))))
    (setf (aref sudoku-clone i j) val)
    sudoku-clone))

(defun try (sudoku-grid i j candidates)
  (if candidates
      (let* ((sudoku-copy (copy-update-sudoku sudoku-grid i j (car candidates)))
               (solution (solve sudoku-copy)))
        (cond (solution solution)
              (t (try sudoku-grid i j (cdr candidates)))))
      nil))

(defun solve (sudoku-grid)
  (let ((analysis (analyze-missing sudoku-grid)))
    (cond ((not analysis) sudoku-grid)
          ((= 0 (second (first analysis))) nil)
          ((= 1 (second (first analysis)))
            (loop :for a :in analysis :if (= 1 (second a)) :do
                    (setf (aref sudoku-grid (car (first a)) (cdr (first a)))
                      (car (third a))))
            (solve sudoku-grid))
          (t (let* ((a (car analysis))
                    (i (car (first a)))
                    (j (cdr (first a)))
                    (candidates (third a)))
               (try sudoku-grid i j candidates))))))


(defun run () (with-open-file (in *data-file*)
                (let* ((sudoku-grid (to-array (read-file in))))
                  (solve sudoku-grid))))

(print-sudoku (run))