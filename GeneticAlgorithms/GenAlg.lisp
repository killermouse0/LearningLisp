; a population is a vector of individuals
; a fitness function takes an individual and returns its fitness value

; 0. Eval
; 1. Select
; 2. Crossover
; 3. Mutate

(defparameter *target* (coerce "stephane" 'list))
(defparameter *population-size* 1000)
(defparameter *cross-over-rate* 0.10)
(defparameter *mutation-rate* 0.01)

(defun eval-population (fitness-func population)
  (let ((res (make-array (length population) :initial-element 0.0)))
    (loop :for i :from 0 :and individual :across population
          :do (setf (aref res i) (funcall fitness-func individual)))
    res))

(defun tournament (population population-fitness &optional (rounds-number 20) (winner nil))
  (cond
   ((= rounds-number 0) winner)
   ((not winner) (tournament population population-fitness (1- rounds-number) (random (length population))))
   (t (let ((challenger (random (length population))))
        (if (> (aref population-fitness challenger) (aref population-fitness winner))
            (tournament population population-fitness (1- rounds-number) challenger)
            (tournament population population-fitness (1- rounds-number) winner))))))

(defun select-next-generation (population population-fitness select-func)
  (let ((new-generation (make-array (length population))))
    (loop :for i :from 0 :and individual :across new-generation
          :do (setf
                (aref new-generation i)
                (aref population (funcall select-func population population-fitness))))
    new-generation))

(defun cross-over (individual1 individual2)
  (let* ((locus (random (length individual1)))
         (p1 (copy-list (nthcdr locus individual1)))
         (p2 (copy-list (nthcdr locus individual2))))
    (replace individual1 p2 :start1 locus)
    (replace individual2 p1 :start1 locus)))

(defun cross-over-population (population cross-over-rate)
  (loop :for i :from 0 :to (floor (1- (length population)) 2)
        :do (if (< (random 1.0) cross-over-rate)
                (cross-over
                  (aref population (* i 2))
                  (aref population (1+ (* i 2)))))))

(defun mutate-individual (individual mutation-per-gene-rate random-gene-generator)
  (if (cdr individual)
      (cons (if (< (random 1.0) mutation-per-gene-rate)
                (funcall random-gene-generator)
                (car individual))
            (mutate-individual (cdr individual) mutation-per-gene-rate random-gene-generator))
      individual))

(defun mutate-population (population mutation-per-gene-rate random-gene-generator)
  (loop :for i :from 0 :and individual :across population
        :do (setf (aref population i) (mutate-individual individual mutation-per-gene-rate random-gene-generator))))

(defun generate-initial-population (generate-random-individual)
  (let ((population (make-array *population-size*)))
    (loop :for i :from 0 :and individual :across population
          :do (setf (aref population i) (funcall generate-random-individual)))
    population))

(defun find-best-individual (population-fitness &optional (current-index 0) (best-index 0))
  (if (< current-index (length population-fitness))
      (if (> (aref population-fitness current-index) (aref population-fitness best-index))
          (find-best-individual population-fitness (1+ current-index) current-index)
          (find-best-individual population-fitness (1+ current-index) best-index))
      best-index))

(defun genetic-optimizer (population random-gene-generator fitness-func selection-func fitness-target max-iterations)
  (format t "Generation ~A~%" max-iterations)
  (let* ((population-fitness (eval-population fitness-func population))
         (best-individual-index (find-best-individual population-fitness)))
    (format T "Best index ~A, value ~A~%" best-individual-index (aref population best-individual-index))
    (if (or (>= (aref population-fitness best-individual-index) fitness-target)
            (zerop max-iterations))
        (aref population best-individual-index)
        (let* ((next-generation (select-next-generation population population-fitness selection-func)))
          (cross-over-population next-generation *cross-over-rate*)
          (mutate-population next-generation *mutation-rate* random-gene-generator)
          (genetic-optimizer next-generation random-gene-generator fitness-func selection-func fitness-target (1- max-iterations))))))

(defun genetic-solver (random-gene-generator random-individual-generator fitness-func selection-func fitness-target max-iterations)
  (let* ((population (generate-initial-population random-individual-generator))
         (best (genetic-optimizer population random-gene-generator fitness-func selection-func fitness-target max-iterations)))
    (print best)))

(defun fitness-name (individual &optional (target *target*) (fitness-level 0))
  (if (car individual)
      (if (eq (car individual) (car target))
          (fitness-name (cdr individual) (cdr target) (1+ fitness-level))
          (fitness-name (cdr individual) (cdr target) fitness-level))
      fitness-level))

(defun generate-random-gene ()
  (code-char (+ (char-int #\a) (random 26))))

(defun generate-random-individual ()
  (loop :for i :from 0 :below (length *target*) :collect (generate-random-gene)))

(genetic-solver #'generate-random-gene #'generate-random-individual #'fitness-name #'tournament 8 1000)