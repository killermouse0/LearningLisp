(defparameter *data-file* "aoc_7.txt")

(defclass node ()
  ((name
    :initarg :name
    :accessor name)
   (size
    :initarg :size
    :initform 0
    :accessor size)))

(defclass dir (node)
  ((content
    :initarg :content
    :initform nil
    :accessor content)))

(defmethod is-dir ((d dir))
  t)

(defmethod is-dir ((d t))
  nil)

(defmethod print-object ((d dir) str)
  (format str "dirname: ~a size: ~a content: ~a" (name d) (size d) (content d)))

(defclass file (node) ())

(defmethod is-file ((f file))
  t)

(defmethod is-file ((f t))
  nil)

(defmethod print-object ((f file) str)
  (format str "filename: ~a size: ~a" (name f) (size f)))

(defmethod append-content ((d dir) (n node))
  (setf (content d) (cons n (content d)))
  (setf (size d) (+ (size d) (size n))))

(defun process-file (stream current-dir)
;;  (print current-dir)
  (let ((line (read-line stream nil :eof)))
    (cond ((equal line :eof) current-dir)
	  (t (let ((tokens (uiop:split-string line :separator " ")))
	       (cond ((and (equal "$" (car tokens))
			   (equal "cd" (cadr tokens)))
		      (cond
			;; $ cd ..
			(  (equal ".." (caddr tokens))
			   current-dir)
			;; $ cd XYZ
			(t (let ((d (make-instance 'dir :name (caddr tokens))))
			     (progn
			       (process-file stream d)
			       (append-content current-dir d)
			       (process-file stream current-dir)
			       d)))))
		     ;; $ ls
		     ((and (equal "$" (car tokens))
			   (equal "ls" (cadr tokens))) (process-file stream current-dir))
		     ;; dir
		     ((equal "dir" (car tokens)) (process-file stream current-dir))
		     ;; 1234 file.txt
		     (t
		      (append-content
		       current-dir
		       (make-instance 'file :name (cadr tokens) :size (parse-integer (car tokens))))
		      (process-file stream current-dir))))))))


(defun smaller-dirs (n)
  (<= (size n) 100000))

(defun bigger-than-dirs (threshold)
  (lambda (n)
    (>= (size n) threshold)))

(defun travel-node (n f)
  (cond ((null n) nil)
	((is-dir n) (if (funcall f n)
			(cons (size n) (travel-dir-content (content n) f))
			(travel-dir-content (content n) f)))))

(defun travel-dir-content (c f)
  (cond ((null c) nil)
	(t (append (travel-node (car c) f) (travel-dir-content (cdr c) f)))))

(defun tabulate (n)
  (cond ((zerop n) '(""))
	(t (cons " " (tabulate (1- n))))))

(defun pretty-print-content (content indent)
  (cond ((null content) nil)
	(   (is-file (car content))
	    (format t "~A~A: ~A~%" (tabulate indent) (name (car content)) (size (car content)))
	    (pretty-print-content (cdr content) indent))
	(   (is-dir (car content))
	    (pretty-print-dir (car content) indent)
	    (pretty-print-content (cdr content) indent))))

(defun pretty-print-dir (d indent)
  (format t "~A~A: ~A~%" (tabulate indent) (name d) (size d))
  (pretty-print-content (content d) (1+ indent)))



(defun read-file (data-file)
  (let* ((in (open data-file))
	 (root (process-file in (make-instance 'dir :name "ROOT"))))
    (close in)
    root))



(let ((root (read-file *data-file*)))

  ;; part 1
  (format t "Total is ~A~%"
	  (apply #'+ (travel-node root #'smaller-dirs)))

  ;; part 2
  ;; -------------------------------------------- 70000000  total
  ;; ---------------------------------            ???       root size
  ;;                             ---------------- 30000000  needed
  ;; ----------------------------                 70000000 - 30000000 can-keep
  (let* ((total 70000000)
	 (needed 30000000)
	 (can-keep (- total needed))
	 (to-remove (- (size root) can-keep)))
    (format t "The smallest over XX is ~A~%"
	    (car (sort (travel-node root
				    (bigger-than-dirs to-remove)) #'<)))))
