(ql:quickload "zpng")
(ql:quickload "bordeaux-threads")

(defparameter *pic-width* 3840)
(defparameter *pic-height* 2160)
(defparameter *xmin* -2.0)
(defparameter *xmax* +0.7)
(defparameter *ymin* -1.5)
(defparameter *ymax* +1.5)
(defparameter *rows-per-thread* 100)
(defparameter *mandelbrot-max-iter* 255)

;; coordinate mapping from picture coords to complex plane
(defun j_to_x (j)
  (+ *xmin*
     (* (/ (- *xmax* *xmin*) *pic-width*) j)))

(defun i_to_y (i)
  (let ((b *ymax*)
        (a (/ (- *ymin* *ymax*) *pic-height*)))
    (+ b (* a i))))

(defun pix_to_c (i j)
  (complex (j_to_x j) (i_to_y i)))

;; mandelbrot series computation
(defun mandelbrot (z_n c n n-max)
  (cond
   ((= n n-max) n)
   ((> (abs z_n) 2) n)
   (t (mandelbrot (+ (* z_n z_n) c) c (+ n 1) n-max))))


;; image computation
(defun compute-image-sector (row-min col-min row-max col-max image)
  (loop for i from row-min below row-max do
          (loop for j from col-min below col-max do
                  (let* ((c (pix_to_c i j))
                         (final-iter (mandelbrot 0 c 0 *mandelbrot-max-iter*)))
                    (setf (aref image i j 0)
                      (cond
                       ((eq final-iter *mandelbrot-max-iter*) 0) ;; ne converge pas 
                       (t final-iter)) ;; converge
                        ))))
  (format nil "Done computing from ~A to ~A" row-min row-max))

(defun compute-splits (mini maxi step acc)
  (cond ((> (+ mini step) maxi) (append acc (list (cons mini maxi))))
        (t (compute-splits (+ mini step) maxi step (append acc (list (cons mini (+ mini step))))))))

(defun thread-ripper (threads)
  (when threads
        (print (format nil "Waiting for thread ~A" (bt:thread-name (car threads))))
        (print (bt:join-thread (car threads)))
        (thread-ripper (cdr threads))))

(defun compute-image (row-max col-max image)
  (let ((threads (loop for s in (compute-splits 0 row-max *rows-per-thread* '()) collect
                         (let ((thread-name (format nil "thread-~A-~A" (car s) (cdr s))))
                           (bt:make-thread (let ((row-start (car s))
                                                 (row-end (cdr s)))
                                             (lambda ()
                                               (compute-image-sector row-start 0 row-end col-max image))) :name thread-name)))))
    (thread-ripper threads)))

(defun save_to_file (file)
  (let* ((png (make-instance 'zpng::png
                :color-type :grayscale
                :width *pic-width*
                :height *pic-height*))
         (image (zpng::data-array png)))
    (compute-image *pic-height* *pic-width* image)
    (zpng::write-png png file :if-exists :supersede)))

(save_to_file "mandelbrot.png")
(print "We're done!")
(quit)