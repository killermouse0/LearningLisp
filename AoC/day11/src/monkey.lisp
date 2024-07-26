(in-package :monkey)

(defclass monkey ()
  ((items
    :initarg :items
    :accessor monkey-items
    :initform nil)
   (operation
    :initarg :operation
    :accessor monkey-operation)
   (test
    :initarg :test
    :accessor monkey-test)
   (monkey-true
    :initarg :monkey-true
    :accessor monkey-true
    :initform nil)
   (monkey-false
    :initarg :monkey-false
    :accessor monkey-false
    :initform nil)
   (inspected
    :accessor monkey-inspected
    :initform 0)))


(defun make-monkey (items operation test)
  (make-instance 'monkey :items items :operation operation :test test))


(defgeneric process-item (obj worry))


(defmethod process-item ((m monkey) worry)
  (let* ((new-worry (funcall (monkey-operation m) worry))
	 ;; (div-3 (floor new-worry 3))
	 (worry-level (mod new-worry 9699690))
	 ;; (target-monkey (if (funcall (monkey-test m) worry-level)
	 (target-monkey (if (funcall (monkey-test m) worry-level)
			    (monkey-true m)
			    (monkey-false m))))
    (receive target-monkey worry-level)
    (setf (monkey-inspected m) (1+ (monkey-inspected m)))))


(defgeneric process-items (obj))


(defmethod process-items ((m monkey))
  (labels ((run-list (lst) (when lst
			     (process-item m (car lst))
			     (run-list (cdr lst)))))
    (run-list (monkey-items m)))
  (setf (monkey-items m) '()))

(defgeneric receive (obj worry))

(defmethod receive ((m monkey) worry)
  (setf (monkey-items m)
	(append (monkey-items m) (list worry))))
