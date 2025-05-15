

(defun random-of-size (bit-size) (random (expt 2 bit-size)))

(defun random-hex-key (bit-size)
  (format nil "~x" (random-of-size bit-size)))

(defparameter *64-bit-digits* 
  (concatenate 'string "0123456789abcdefghijklmnopqrstuvwxyz"
	       "ABCDEFGHIJKLMNOPQRSTUVWXYZ_!"))

(defun byte-val (byte-num number &optional (byte-size 8))
  (ldb (byte byte-size (* byte-size byte-num )) number))

(defun indices-64 (&optional (bit-count 64))
  (let ((res ()) 
	(num (random-of-size bit-count) ))
    (dotimes (n (+ 1 (round (log num 64))))
      (push (byte-val n num 6) res))
    (if (zerop (car res)) (cdr res) res)))


(defun 64-bit-index (&optional (bit-count 64))
  (let ((chars (mapcar 
		(lambda (n) (aref *64-bit-digits* n)) 
		(indices-64 bit-count))))
    (format nil "~{~A~}" chars)))

(defun compound-key (&rest keys)
  (format nil "~{~A~^.~}" keys))

(defun new-object-key (cls) 
  (compound-key (id cls) (64-bit-index)))

(defun new-tag-key () (64-bit-index)))

(defun new-role-key () (64-bit-index)))

(defun new-class-key () (64-bit-index 48)))

(defun new-group-key () (64-bit-index)))

(defun object-role-key (obj role) 
  (compound-key (id obj) (id role)))
