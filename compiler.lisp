(defclass bitvec-holder ()
  ((core-bit-vector
    :initarg :core
    :accessor core
    :initform (error "Binvec-holder without proper core"))
   (dependant-bit-vec
    :initarg :dependant
    :accessor dependant-bit-vec
    :initform nil)))

(defun throw-no-matching-register-error (word)
  (error 'no-matching-register :text (format nil "Could not find a register matching ~a" word)))


(defun function-integer->bitvec-holder (num)
  (declare (type integer))
  (if (< num 32)
      (make-instance 'bitvec-holder 
		     :core (buffer-bit-vector-to-length 6 (integer->bit-vector num)))
      (make-instance 'bitvec-holder
		     :core #*011110
		     :dependant (list (buffer-bit-vector-to-length 16 (integer->bit-vector num))))))
  
(defun next-word-function-integer->bitvec-holder (num)
  (declare (type integer))
  (make-instance 'bitvec-holder
		 :core #*011111
		 :dependant (list (buffer-bit-vector-to-length 16 (integer->bit-vector num)))))

(defun find-register-ref (word)
  (case (intern (string-upcase word)) 
      (a 0)
      (b 1)
      (c 2)
      (x 3)
      (y 4)
      (z 5)
      (i 6)
      (j 7)))

(defun parse-register-ref (word)
  (let ((register-ref (or (find-register-ref word)
		       (case (intern (string-upcase word))
			 ([a] 8)
			 ([b] 9)
			 ([c] 10)
			 ([x] 11)
			 ([y] 12)
			 ([z] 13)
			 ([i] 14)
			 ([j] 15)
			 (pop 24)
			 (peek 25)
			 (push 26)
			 (sp 27)
			 (pc 28)
			 (o 29)))))
    (when register-ref
      (make-instance 
       'bitvec-holder 
       :core (buffer-bit-vector-to-length 
	      6
	      (integer->bit-vector register-ref))))))

(defun parse-special-reg-function (word)
  "parses an assembly command and outputs a list of bit-vectors and symbols.
The bitvectors correspond with assembly code
The symbols are locations/positions in ram that haven't yet been defined"
  (let* ((hex (get-first-hex word))
	 (var-word (remove #\+ (remove-first-hex (but-last2 (but-first2 word))) :count 1))
	 (func (+ 16 (find-register-ref var-word))))
    (make-instance 'bitvec-holder 
		   :core (buffer-bit-vector-to-length
			  6 (integer->bit-vector func))
		   :dependant (list (buffer-bit-vector-to-length
				     16 (integer->bit-vector hex))))))

(defun parse-opp (word)
  (declare (type string))
  (let ((op (case (intern (string-upcase word))
	      (set 1)
	      (add 2)
	      (sub 3)
	      (mul 4)
	      (div 5)
	      (mod 6)
	      (shl 7)
	      (shr 8)
	      (and 9)
	      (bor 10)
	      (xor 11)
	      (ife 12)
	      (ifn 13)
	      (ifg 14)
	      (ifb 15))))
    (when op
      (make-instance 'bitvec-holder :core
		     (buffer-bit-vector-to-length 
		      4 (integer->bit-vector op))))))

;;TODO: Fix this monstrosity
(defun parse-function (word)
  (or (parse-register-ref word) 
      (cond ((and (surrounded-by-p word #\[ #\]) 
		  (find #\+ word))
	     (parse-special-reg-function word))
	    ((and (surrounded-by-p word #\[ #\[)
		  (hex-p (but-first2 (but-last2 word))))
	     (next-word-function-integer->bitvec-holder (parse-hex (but-first2 (but-last2 word)))))
	    ((hex-p word)
	     (function-integer->bitvec-holder (parse-hex word)))
	    (T (make-instance 'bitvec-holder :core (intern (string-upcase word)) 
			      :dependant nil)))))

(defun parse-operation (line) 
  (let ((op (parse-opp (car line)))
	(arg1 (parse-function (cadr line)))
	(arg2 (parse-function (caddr line))))
    (if (loop for i in (list op arg1 arg2) do
	     (when (symbolp (core i))
	       (return T)))
	(list op arg1 arg2)
	(concatenate 'list (list (concatenate 'bit-vector (core arg2) (core arg1) (core op)))
		     (dependant-bit-vec arg1)
		     (dependant-bit-vec arg2)))))

(defun update-locations (word-count amount locations)
  locations)

(defun location-symbols->addresses (word-set old-locations)
  (let ((locations old-locations)
	(word-count 0))
    (loop for words being the elements of word-set collect
	 (if (not (equal (type-of (car words)) 'bitvec-holder))
	     (progn (setf word-count (+ word-count (length words)))
		    words)
	     (let* ((new-words
		     (loop for word being the elements of words collect
			  (if (not (symbolp (core word)))
			      word
			      (progn 
				(when (not (assoc (core word) locations))
				  (error (format nil "Symbol '~a' unkown" (core word))))
				(let ((location (cadr (assoc (core word) locations))))
				  (make-instance 'bitvec-holder 
						 :core (if (>= location 32)
							   #*011110
							   (concatenate 'bit-vector #*1
									(buffer-bit-vector-to-length 
									 5 (integer->bit-vector location))))
						 :dependant (if (< location 32)
								nil
								(list (buffer-bit-vector-to-length
								       16  (integer->bit-vector location))))))))))
		    (finished-new-words
		     (concatenate 'list (list (concatenate 'bit-vector (core (caddr new-words))
							   (core (cadr new-words))
							  (core (car new-words)))) 
				  (dependant-bit-vec (cadr new-words))
				  (dependant-bit-vec (caddr new-words)))))
	       (setf locations (update-locations word-count (- (length finished-new-words) 3) locations))
	       (setf word-count (+ word-count (length finished-new-words)))
	       finished-new-words)))))
  
(defun find-location-symbols (word-set)
  (let ((locations nil)
	(word-count 0))
    (loop for words in word-set do
	 (print words)
	 (if (symbolp (car words))
	     (progn (push (list (car words) word-count) locations)
		    (setf word-count (+ word-count (length (cdr words)))))
	     (setf word-count (+ word-count (length words)))))
    locations))

;;Bugs: New locations can be created ending with ":"
(defun parse-assembly (string)
  (let* ((output nil)
	 (annotated-output)
	 (ret-output nil))
    (loop for line being the elements of (split string #\newline) 
       for i from 0 do
	 (let ((words (split (return-after line #\;) #\space)))
	   (cond ((and (= (length words) 4) (ends-with-p (car words) #\:))
		  (push (list (intern (string-upcase (but-last2 (car words)))) 
			      (car (push (parse-operation (cdr words)) output)))
			annotated-output))
		 ((= (length words) 3) 
		  (push (car (push (parse-operation words) annotated-output)) output))
		 ((= (length words) 0) nil)
		 (T (error (format nil "Error! ~a at ~a isn't a good line length" line i))))))
    (setf output (nreverse output))
    (setf annotated-output (nreverse annotated-output))
    (loop for words in (location-symbols->addresses output (find-location-symbols annotated-output)) do
	 (loop for word in words do
	      (push word ret-output)))
    (nreverse ret-output)))
