(declaim (optimize (speed 3) (debug 0) (safety 0)))

(ql:quickload "zpng")
(ql:quickload "bordeaux-threads")

(defparameter *pic-coef* 8)
(defparameter *pic-width* (* 1024 *pic-coef*))
(defparameter *pic-height* (* 768 *pic-coef*))

(defparameter *x-o* -0.8)
(defparameter *y-o* 0.0)

(defparameter *x-l* 1.5)
(defparameter *xmin* (- *x-o* *x-l*))
(defparameter *xmax* (+ *x-o* *x-l*))

(defparameter *y-l* (/ (* *x-l* (/ *pic-height* 2)) (/ *pic-width* 2)))
(defparameter *ymin* (- *y-o* *y-l*))
(defparameter *ymax* (+ *y-o* *y-l*))

(defparameter *rows-per-thread* 250)

(defparameter *mandelbrot-max-iter* 100)
(defparameter *mandelbrot-escape-threshold* 2.0)

;; color coding

(defun hsv-to-rgb (H S V)
  (declare (type single-float H S V))
  (let* ((M (* 255 V))
         (mm (* M (- 1.0 S)))
         (z (* (- M mm)
               (- 1
                  (abs (- (mod (/ H 60) 2) 1))))))
    (cond
     ((< H 60) (values M (+ z mm) mm))
     ((< H 120) (values (+ z mm) M mm))
     ((< H 180) (values mm M (+ z mm)))
     ((< H 240) (values mm (+ z mm) M))
     ((< H 300) (values (+ z mm) m M))
     (t (values M mm (+ z mm))))))

(defun escape-to-color (n)
  (declare (type integer n))
  (let* ((h (* n (/ 360.0 *mandelbrot-max-iter*)))
         (color (multiple-value-bind (r g b) (hsv-to-rgb h 1.0 1.0) (vector r g b))))
    (map 'vector #'round color)))

;; coordinate mapping from picture coords to complex plane
(defun j_to_x (j)
  (declare (type integer j))
  (+ *xmin*
     (* (/ (- *xmax* *xmin*) *pic-width*) j)))

(defun i_to_y (i)
  (declare (type integer i))
  (let ((b *ymax*)
        (a (/ (- *ymin* *ymax*) *pic-height*)))
    (+ b (* a i))))

(defun pix_to_c (i j)
  (complex (j_to_x j) (i_to_y i)))

;; mandelbrot series computation
(defun mandelbrot (z_n c n n-max)
  (declare (type complex z_n c))
  (declare (type integer n n-max))
  (cond
   ((= n n-max) n)
   ((> (abs z_n) *mandelbrot-escape-threshold*) n)
   (t (mandelbrot (+ (* z_n z_n) c) c (+ n 1) n-max))))

;; image computation
(defun compute-image-sector (row-min col-min row-max col-max image)
  (declare (type integer row-min col-min row-max col-max))
  (loop for i from row-min below row-max do
          (loop for j from col-min below col-max do
                  (let* ((c (pix_to_c i j))
                         (final-iter (mandelbrot (complex 0.0 0.0) c 0 *mandelbrot-max-iter*))
                         (color (cond
                                 ((eq final-iter *mandelbrot-max-iter*) (vector 0 0 0)) ;; ne converge pas 
                                 (t (escape-to-color final-iter))))) ;; converge
                    (setf (aref image i j 0) (aref color 0))
                    (setf (aref image i j 1) (aref color 1))
                    (setf (aref image i j 2) (aref color 2)))))
  (format t "Done computing from ~A to ~A.~%" row-min row-max))

(defun compute-splits (mini maxi step acc)
  (declare (type integer mini maxi step))
  (cond ((>= (+ mini step) maxi) (append acc (list (cons mini maxi))))
        (t (compute-splits (+ mini step) maxi step (append acc (list (cons mini (+ mini step))))))))

(defun thread-reaper (threads)
  (when threads
        (format t "Waiting for thread ~A...~%" (bt:thread-name (car threads)))
        (bt:join-thread (car threads))
        (thread-reaper (cdr threads))))

(defun compute-image (row-max col-max image)
  (let ((threads (loop for s in (compute-splits 0 row-max *rows-per-thread* '()) collect
                         (let ((thread-name (format nil "thread-~A-~A" (car s) (cdr s))))
                           (bt:make-thread (let ((row-start (car s))
                                                 (row-end (cdr s)))
                                             (lambda ()
                                               (compute-image-sector row-start 0 row-end col-max image))) :name thread-name)))))
    (thread-reaper threads)))

(defun save_to_file (file)
  (let* ((png (make-instance 'zpng::png
                :color-type :Truecolor
                :width *pic-width*
                :height *pic-height*))
         (image (zpng::data-array png)))
    (compute-image *pic-height* *pic-width* image)
    (zpng::write-png png file :if-exists :supersede)))
