(defun split (string character)
    "Returns a list of substrings of string
divided by one <character> each.
Note: Two consecutive spaces will be seen as
if there were an empty string between them."
    (loop for string in 
	 (loop for i = 0 then (1+ j)
	    as j = (position character string :start i)
	    collect (subseq string i j)
	    while j)
       when (> (length string) 0) collect string))

(defun return-after (string character)
  "Returns a string having removed
all text after the specified character"
  (let ((new-string string))
    (loop for chr being the elements of string for j from 0 do
	 (if (eq chr character)
	     (return (setf new-string (subseq string 0 j)))))
    new-string))

(defun starts-with-p (sequence predicate)
  "Returns true if a sequence begins with a predicate specified character"
  (cond ((typep sequence 'simple-array) (equal (aref sequence 0) predicate))
	((typep sequence 'list) (equal (car sequence) predicate))))

(defun ends-with-p (sequence predicate)
  "Returns true if a sequence ends with a predicate"
  (cond ((typep sequence 'simple-array) (equal (aref sequence (1- (array-dimension sequence 0))) predicate))
	((typep sequence 'list) (equal (car (last sequence)) predicate))))

(defun surrounded-by-p (string left right)
  "Returns true if a string has specified characters on
its left and right ends"
  (and (starts-with-p string left)
       (ends-with-p string right)))

(defun but-last2 (sequence)
  "Removes the last element of an array or list"
  (cond ((typep sequence 'simple-array) (subseq sequence 0 (1- (length sequence))))
	((typep sequence 'list) (butlast sequence))))

;;This is a stupid function, but it adds some clarity at one point in my code.
(defun but-first2 (sequence)
  "Removes the first element of an array or list"
  (cond ((typep sequence 'simple-array) (subseq sequence 1))
	((typep sequence 'list) (car sequence))))

(defun integer->bit-vector (num)
  "Converts and integer to a bit-vector"
  (declare (type integer))
  (labels ((integer->bit-list (int &optional accum)
	     (cond ((> int 0)
		    (multiple-value-bind (i r) (truncate int 2)
		      (integer->bit-list i (push r accum))))
		   ((null accum) (push 0 accum))
		   (t accum))))
    (coerce (integer->bit-list num) 'bit-vector)))

(defun buffer-bit-vector-to-length (min-length bitv)
  "Increases the size of a bit-vector to a specified length.
Errors on length size being larger than specified"
  (if (<= (length bitv) min-length)
      (let ((new-bitv (make-array min-length :initial-element 0)))
	(loop for b being the elements of (reverse bitv)
	      for i from 0 to (1- (length bitv)) do
	     (setf (aref new-bitv i) b))
	(coerce (nreverse new-bitv) 'bit-vector))
      (error 'bit-vector-larger-than-expected :text 
	     (format nil "Expected bitvector ~a to be size ~a" bitv min-length))))


(defun remove-first-hex (rstring)
  "Removes the first hexidecimal or decimal number from a string"
  (let ((return-string rstring)
	(hex-start (position-if #'digit-char-p rstring)))
    (when hex-start
      (when (and (eql #\0 (aref rstring hex-start))
		 (equalp #\x (aref rstring (1+ hex-start))))
	(setf return-string (remove #\x (remove #\0 rstring :count 1) :count 1 :test #'equalp)))
      (let ((sentinal nil))
	(coerce (loop for char being the elements of return-string
		   for i from 0 when (or sentinal (not (digit-char-p char))) collect 
		     (progn (when (or sentinal
				      (and (not (digit-char-p char))
					   (> i hex-start)))
			      (setf sentinal T))
			    char))  
		'string)))))

(defun get-first-hex (string)
  "Returns the integer value of the first hex or decimal number in a string"
  (let ((hex-start (position-if #'digit-char-p string))
	(radix 10))
    (when hex-start
      (when (and (eql #\0 (aref string hex-start))
		 (equalp #\x (aref string (1+ hex-start))))
	(setf radix 16))
      (let ((first-num nil))
	(loop for char being the elements of (if (= radix 16)
						 (remove #\x (remove #\0 string :count 1):count 1 :test #'equalp)
						 string)
	   do (cond ((digit-char-p char)
		     (push char first-num))
		    (first-num (return nil))))
	(parse-integer (coerce (nreverse first-num) 'string) :radix radix)))))


(defun hex-p (string) 
  "Checks if a string contains a valid hex value
[declared by an 0x prefix applied to a hex value]"
  (and (eql (char string 0) #\0)
       (equalp (char string 1) #\x)
       (loop for letter being the elements of (subseq string 2) do
	    (when (not (find letter "abcdefABCDEF0123456789"))
	      (return nil))
	    finally (return T))))

(defun parse-hex (string)
  "Converts a hex string to an integer.
Valid hex strings begin with the prefix 0x"
  (if (hex-p string)
      (parse-integer (subseq string 2) :radix 16)
      (error 'hex-string-input-invalid :text (format nil "The string '~a' is not a valid hex number" string))))


(defun bit-vector->integer (bitv)
  (loop for i from 0
     for element being the elements of (reverse bitv) sum
       (* element (expt 2 i))))

(defun bit-vector->hex (bitv)
  "Converts a bit-vector into a hexidecimal"
  (concatenate 'string "0x" (write-to-string (bit-vector->integer bitv) :base 16)))

(defun remove-all (predic seq &optional res)
  "Removes all elements matching the predicate from a list"
  (if (null seq)
      (reverse res)
      (cond ((and (not (null (car seq))) (listp (car seq)))
	     (remove-all predic (cdr seq)
			 (cons (remove-all predic (car seq)) res)))
	    ((funcall predic (car seq))
	     (remove-all predic (cdr seq) res))
	    (t (remove-all predic (cdr seq) (cons (car seq) res))))))


(defun within-range (num start end)
  "Checks if a number is equal to, or between to numbers"
  (and (<= start num) (>= end num)))