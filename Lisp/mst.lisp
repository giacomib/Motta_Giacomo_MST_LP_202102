;; Grillo Antonio 851970
;; Motta Giacomo 851887

(defparameter *vertices* (make-hash-table :test #'equal))
(defparameter *arcs* (make-hash-table :test #'equal))
(defparameter *graphs* (make-hash-table :test #'equal))
(defparameter *vertex-key* (make-hash-table :test #'equal))
(defparameter *previous* (make-hash-table :test #'equal))
(defparameter *heaps* (make-hash-table :test #'equal))
(defparameter *positions* (make-hash-table :test #'equal))
(defparameter *neigh* (make-hash-table :test #'equal))

(defun is-graph (graph-id)
  (gethash graph-id *graphs*))

(defun new-graph (graph-id)
  (or (gethash graph-id *graphs*)
      (setf (gethash graph-id *graphs*) graph-id)))

(defun delete-graph (graph-id)
  (cond ((is-graph graph-id)
         (remhash graph-id *graphs*)
	 (maphash #'(lambda (k v)
		      (cond ((equal (second k)
				    graph-id)
			     (remhash v *vertices*))))
		  *vertices*)
	 (maphash #'(lambda (k v)
		      (cond ((equal (second k)
				    graph-id)
			     (remhash k *arcs*)
			     v)))
		  *arcs*)
	 (maphash #'(lambda (k v)
		      (cond ((and (not (null v))
				  (equal (first k)
					 graph-id))
			     (remhash k *neigh*))))
		  *neigh*)
	 (maphash #'(lambda (k v)
		      (cond ((and (not (null v))
				  (equal (first k)
					 graph-id))
			     (remhash k *vertex-key*))))
		  *vertex-key*)
	 (maphash #'(lambda (k v)
		      (cond ((equal (first k) graph-id)
			     (remhash k *previous*)
			     (cond ((or (null v) (not (null v))) nil)))))
		  *previous*))))

(defun new-vertex (graph-id vertex-id)
  (new-graph graph-id)
  (setf (gethash (list 'vertex graph-id vertex-id) *vertices*)
	(list 'vertex graph-id vertex-id)))


(defun graph-vertices (graph-id)
  (cond ((is-graph graph-id)
	 (let ((tmp ()))
	   (maphash #'(lambda (k v)
			(cond ((equal (second k) graph-id)
			       (push v tmp))))
		    *vertices*)
	   tmp))))

(defun get-weight (graph-id u v)
  (let ((w (gethash (list 'arc graph-id u v) *arcs*)))
    (cond ((null w) (gethash (list 'arc graph-id v u) *arcs*))
	  (T w))))

(defun new-neigh (graph-id u v)
  (cond ((not (equal u v))
	 (setf (gethash (list graph-id u) *neigh*)
	       (remove-duplicates
		(append (list v) (gethash (list graph-id u) *neigh*))
		:test #'equal))
	 (setf (gethash (list graph-id v) *neigh*)
	       (remove-duplicates
		(append (list u) (gethash (list graph-id v) *neigh*))
		:test #'equal)))))

(defun new-arc (graph-id u v &optional (weight 1))
  (cond ((not (equal u v))
	 (new-vertex graph-id u)
	 (new-vertex graph-id v)
	 (new-neigh graph-id u v)
	 (cond ((gethash (list 'arc graph-id u v) *arcs*)
		(setf (gethash (list 'arc graph-id u v) *arcs*) weight))
	       ((gethash (list 'arc graph-id v u) *arcs*)
		(setf (gethash (list 'arc graph-id v u) *arcs*) weight))
	       (T (setf (gethash (list 'arc graph-id u v) *arcs*) weight)))))
  (list 'arc graph-id u v weight))

(defun graph-arcs (graph-id)
  (cond ((is-graph graph-id)
	 (let ((tmp ()))
	   (maphash #'(lambda (k v)
			(cond ((equal (second k) graph-id)
			       (push (append k (list v)) tmp))))
		    *arcs*)
	   tmp))))

(defun graph-vertex-neighbors (graph-id vertex-id)
  (let ((arc ()))
    (cond ((is-graph graph-id)
	   (mapcar #'(lambda (x)
		       (push (list 'arc
				   graph-id
				   vertex-id
				   x
				   (get-weight graph-id vertex-id x))
			     arc))
		   (gethash (list graph-id vertex-id) *neigh*))))
    arc))

(defun graph-vertex-adjacent (graph-id vertex-id)
  (let ((vertex ()))
    (cond ((is-graph graph-id)
	   (mapcar #'(lambda (x)
		       (push (list 'vertex graph-id x) vertex))
		   (gethash (list graph-id vertex-id) *neigh*))))
    vertex))

(defun graph-print (graph-id)
  (cond ((is-graph graph-id)
	 (progn
	   (maphash #'(lambda (k v)
			(cond ((equal (second k) graph-id)
			       (print v))))
		    *vertices*)
	   (maphash #'(lambda (k v)
			(cond ((equal (second k) graph-id)
			       (print (append k (list v))))))
		    *arcs*)))))


(defun new-heap (heap-id &optional (capacity 42))
  (or (gethash heap-id *heaps*)
      (setf (gethash heap-id *heaps*)
	    (list 'heap heap-id 0 (make-array capacity)))))

(defun heap-delete (heap-id)
  (remhash heap-id *heaps*) T)

(defun heap-size (heap-id)
  (third (gethash heap-id *heaps*)))

(defun heap-empty (heap-id)
  (cond ((= (heap-size heap-id)
	    0)
	 T)
	(T NIL)))

(defun heap-not-empty (heap-id)
  (not (heap-empty heap-id)))

(defun heap-id (heap-id)
  (second (gethash heap-id *heaps*)))

(defun heap-actual-heap (heap-id)
  (fourth (gethash heap-id *heaps*)))

(defun parent (i)
  (cond ((= (mod i 2)
	    0)
	 (1- (truncate i 2)))
	(T (truncate i 2))))

(defun left (i)
  (1+ (* 2 i)))

(defun right (i)
  (+ 2 (* 2 i)))

(defun swap (array i1 i2)
  (let ((tmp (aref array i1)))
    (setf (aref array i1) (aref array i2)
	  (aref array i2) tmp)))

(defun heapify (heap-id i)
  (let ((temp-min
	 (cond ((and
		 (< (left i)
		    (heap-size heap-id))
		 (<
		  (first (aref (heap-actual-heap heap-id) (left i)))
		  (first (aref (heap-actual-heap heap-id) i))))
		(left i))
	       (T i))))
    (let ((min (cond ((and
		       (< (right i)
			  (heap-size heap-id))
		       (< (first (aref (heap-actual-heap heap-id) (right i)))
			  (first (aref (heap-actual-heap heap-id) temp-min))))
		      (right i))
		     (T temp-min))))
      (cond ((/= min i)
	     (swap (heap-actual-heap heap-id) i min)
	     (setf (gethash (list
			     heap-id
			     (first (aref (heap-actual-heap heap-id)
					  i))
			     (second (aref (heap-actual-heap heap-id)
					   i)))
			    *positions*)
		   i)
	     (setf (gethash (list
			     heap-id
			     (first (aref (heap-actual-heap heap-id)
					  min))
			     (second (aref (heap-actual-heap heap-id)
					   min)))
			    *positions*)
		   min)
	     (heapify heap-id min))))))

(defun heap-head (heap-id)
  (cond ((heap-not-empty heap-id)
	 (aref (heap-actual-heap heap-id) 0))))


(defun heap-decrease-key (heap-id i)
  (cond ((> i 0)
	 (cond ((>
		 (first (aref (heap-actual-heap heap-id) (parent i)))
		 (first (aref (heap-actual-heap heap-id) i)))
		(swap (heap-actual-heap heap-id) i (parent i))
		(setf (gethash (list
				heap-id
				(first (aref (heap-actual-heap heap-id)
					     (parent i)))
				(second (aref (heap-actual-heap heap-id)
					      (parent i))))
			       *positions*)
		      (parent i))
		(setf (gethash (list
				heap-id
				(first (aref (heap-actual-heap heap-id)
					     i))
				(second (aref (heap-actual-heap heap-id)
					      i)))
			       *positions*)
		      i)
		(1- (heap-size heap-id))
		(heap-decrease-key heap-id (parent i)))))))

(defun heap-insert (heap-id k v)
  (cond ((= (heap-size heap-id)
	    (length (heap-actual-heap heap-id)))
	 Nil)
	(T 
	 (setf (gethash heap-id *heaps*)
	       (list 'heap
		     heap-id
		     (1+ (heap-size heap-id))
		     (heap-actual-heap heap-id)))
	 (setf (aref (heap-actual-heap heap-id)
		     (1- (heap-size heap-id)))
	       (list k v))
	 (setf (gethash (list heap-id k v) *positions*)
	       (1- (heap-size heap-id)))
	 (heap-decrease-key heap-id (1- (heap-size heap-id)))
	 T)))


(defun heap-extract (heap-id)
  (cond ((< (heap-size heap-id) 1)
	 (error "Underflow heap"))
	(T
	 (let ((min (aref (heap-actual-heap heap-id) 0))
	       (tmp (aref (heap-actual-heap heap-id)
			  (1- (heap-size heap-id)))))
	   (progn
	     (setf (aref (heap-actual-heap heap-id) 0)
		   (aref (heap-actual-heap heap-id)
			 (1- (heap-size heap-id))))
	     (setf (gethash (list heap-id
				  (first tmp)
				  (second tmp))
			    *positions*)
		   0)
	     (setf (aref (heap-actual-heap heap-id)
			 (1- (heap-size heap-id)))
		   0)
	     (setf (gethash heap-id *heaps*)
		   (list 'heap
			 heap-id
			 (1- (heap-size heap-id))
			 (heap-actual-heap heap-id)))
	     (remhash (list heap-id
			    (first min)
			    (second min))
		      *positions*)
	     (heapify heap-id 0))
	   min))))

(defun heap-modify-key (heap-id new-key old-key v)
  (cond ((gethash (list heap-id
			old-key
			v)
		  *positions*)
	 (setf (aref (heap-actual-heap heap-id)
		     (gethash (list heap-id old-key v) *positions*))
	       (list new-key v))
	 (setf (gethash (list heap-id new-key v) *positions*)
	       (gethash (list heap-id old-key v) *positions*))
	 (remhash (list heap-id old-key v) *positions*)
	 (heap-decrease-key heap-id
			    (gethash (list heap-id new-key v) *positions*))
	 (heapify heap-id
		  (gethash (list heap-id new-key v) *positions*))
	 T)
	(T NIL)))

(defun heap-print (heap-id)
  (cond ((gethash heap-id *heaps*)
	 (print (gethash heap-id *heaps*))
	 T)
	(T Nil)))


(defun mst-vertex-key (graph-id vertex-id)
  (gethash (list graph-id vertex-id) *vertex-key*))

(defun mst-previous (graph-id v)
  (gethash (list graph-id v) *previous*))

(defun mst-recursion-heap (heap-id graph-id)
  (cond ((heap-not-empty heap-id)
	 (let ((u (heap-extract heap-id)))
	   (mapcar #'(lambda (x)
		       (cond ((gethash (list heap-id
					     (gethash (list (second x)
							    (third x))
						      *vertex-key*)
					     (third x))
				       *positions*)
			      (let ((w (get-weight graph-id
						   (third x)
						   (second u))))
				(cond ((< w
					  (gethash (list (second x)
							 (third x))
						   *vertex-key*))
				       (setf (gethash (list graph-id
							    (third x))
						      *previous*)
					     (second u))
				       (heap-modify-key
					heap-id
					w
					(gethash (list
						  graph-id
						  (third x))
						 *vertex-key*)
					(third x))
				       (setf (gethash (list (second x)
							    (third x))
						      *vertex-key*)
					     w)))))))
		   (graph-vertex-adjacent graph-id (second u))))
	 (mst-recursion-heap heap-id graph-id))
	(T NIL)))

(defun mst-prim (graph-id source)
  (mapcar #'(lambda (x)
	      (setf (gethash (list graph-id
				   (third x))
			     *vertex-key*)
		    most-positive-double-float))
	  (graph-vertices graph-id))
  (setf (gethash (list graph-id source) *vertex-key*) 0)
  (setf (gethash (list graph-id source) *previous*) nil)
  (heap-delete 'q)
  (new-heap 'q (length (graph-vertices graph-id)))
  (maphash #'(lambda (k v)
	       (cond ((equal (first k) graph-id)
		      (heap-insert 'q
				   v
				   (second k)))))
	   *vertex-key*)
  (mst-recursion-heap 'q graph-id))


(defun get-sons (graph-id source)
  (let ((sons ()))
    (mapcar #'(lambda (x)
		(cond ((equal (mst-previous graph-id (fourth x))
			      source)
		       (push x sons))))
	    (graph-vertex-neighbors graph-id source))
    (sort
     (sort sons
	   #'(lambda (x y)
	       (cond ((and (numberp x)
			   (numberp y))
		      (< x y))
		     ((and (stringp x)
			   (stringp y))
		      (string< x y))
		     (T (string< (write-to-string x)
				 (write-to-string y)))))
	   :key #'fourth)
     #'<
     :key #'fifth)))

(defun mst-get (graph-id source)
  (cond ((is-graph graph-id)
	 (let ((preorder (get-sons graph-id source)))
	   (cond ((/=
		   (length preorder)
		   0)
		  (remove nil
			  (mapcan #'(lambda (x)
				      (append (list x)
					      (mst-get graph-id
						       (fourth x))))
				  preorder))))))))
