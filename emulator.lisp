;Constants
(defparameter *max-value* 65536)

;;Class definitions
(defclass mem-type ()
  ((value :type 'bit-vector
	  :initarg :value
	  :accessor value-accessor)))

(defclass register (mem-type)
  ((name :accessor name-accessor
         :initarg :name)
   (pointer-name :accessor pointer-name-accessor
		 :initarg :pointer-name)
   (next-pointer-name :accessor next-pointer-name-accessor
		      :initarg :next-pointer-name)
   (value :type bit-vector)))

(defclass ram-word (mem-type)
  ())

(defclass op-code (mem-type)
  ((name :accessor name-accessor
	 :initarg :name)
   (func-def :accessor fuct-def
	     :initarg :func-def)))

(defclass emulator ()
  ((ram :accessor ram)
   (registers :accessor registers)
   (op-codes :accessor op-codes)
   (symbol-locations :accessor symbol-locations)
   (sp :accessor sp
       :initform 65535
       :type 'bit-vector)
   (pc :accessor pc
       :initform 0
       :type 'bit-vector)
   (key-list :accessor key-list
	     :initform nil
	     :type 'list)
   (overload :accessor overload
      :initform 0
      :type 'bit-vector)))

;;Constructors
(defmethod initialize-instance :after ((em emulator) &key)
  (setf (op-codes em) (make-array 16 :element-type 'op-code :initial-contents
				  (loop
				     for op-name in '(non set add sub mul div mod shl 
						      shr and bor xor ife ifn ifg ifb)
				     for i from 0 to 15 collect
				       (make-instance 'op-code :name op-name
						      :func-def op-name
						      :value (integer->bit-vector i)))))
  (setf (ram em) (make-array *max-value* :initial-element (copy-seq #*0000000000000000)))
  (setf (registers em) (make-array 8 :element-type 'bit-vector 
				    :initial-contents (loop for i from 0 to 7 collect
							   (copy-seq #*0000000000000000)))))

;;Predicates

(defun next-word-p (bitv)
  (= 1 (aref bitv (1- (length bitv)))))

(defun register-p (bitv)
  (= 0 (aref bitv 0)))

(defun read-from-next-word-p (bitv)
  (equal (subseq bitv 0 5) #*01111))

;;Mem utils

(defun bit-vector->value1-vector (bitv)
  (subseq bitv 0 (- (length bitv) 10)))

(defun bit-vector->value2-vector (bitv)
  (subseq bitv (- (length bitv) 10) (- (length bitv) 4)))

(defun bit-vector->op-code-vector (bitv)
  (subseq bitv (- (length bitv) 4)))

;;Memory access

(defgeneric ram-at (emulator num)
  (:documentation "Show the ram at a given location"))

(defmethod ram-at ((em emulator) num)
  (aref (ram em) num))

(defgeneric get-word-at-pc (emulator)
  (:documentation "Gets the whole word at the pointer"))

(defmethod get-word-at-pc ((em emulator))
  (aref (ram em) (pc em)))

(defgeneric value1-at-pc (emulator)
  (:documentation "Gets the first value at the pointer"))

(defmethod value1-at-pc ((em emulator))
  (bit-vector->value1-vector (get-word-at-pc em)))

(defgeneric value2-at-pc (emulator)
  (:documentation "Gets the second value at the pointer"))

(defmethod value2-at-pc ((em emulator))
  (bit-vector->value2-vector (get-word-at-pc em)))


(defmethod translate-ram-address ((em emulator) (mem bit-vector))
  (aref (ram em) (bit-vector->integer mem)))

(defmethod translate-reg-address ((em emulator) (mem bit-vector))
  (aref (registers em) (bit-vector->integer mem)))

(defmethod translate-op-address ((em emulator) (mem bit-vector))
  (aref (op-codes em) (bit-vector->integer mem)))

(defgeneric op-code-at-pc (emulator)
  (:documentation "Gets the op code value at the pointer"))

(defmethod op-code-at-pc ((em emulator))
  (bit-vector->op-code-vector (get-word-at-pc em)))

(defgeneric get-memory-object (emulator bits value-ref)
  (:documentation "Gets the op code value at the pointer"))

(defmethod get-memory-object ((emu emulator) (bits bit-vector) value-ref)
	     (cond ((and (read-from-next-word-p bits)
			 (next-word-p bits))
		    (list (ram-at emu (bit-vector->integer (ram-at emu value-ref))) (bit-vector->integer (ram-at emu value-ref))))
		   ((read-from-next-word-p bits)
		    (list (ram-at emu value-ref)))
		   ((register-p bits)
		    (list (bit-vector->register-mem emu bits)))
		   (T (list (subseq bits 1 5)))))

(defgeneric get-memory-objects (emulator ref)
  (:documentation "Gets the op code value at the pointer"))

(defmethod get-memory-objects ((em emulator) ref)
  (let* ((command (ram-at em ref))
	 (value1 (bit-vector->value1-vector command))
	 (value2 (bit-vector->value2-vector command))
	 (value2-ref-mod (if (read-from-next-word-p value2)
			     1 0))
	 (value1-ref-mod (if (read-from-next-word-p value1)
			     1 0))
	 (value2-ref (+ ref value2-ref-mod))
	 (value1-ref (+ ref value1-ref-mod value2-ref-mod))) 
    (list (get-memory-object em value1 value1-ref) 
	  (get-memory-object em value2 value2-ref)
	  (+ value1-ref-mod value2-ref-mod))))

;;System ops:
(defgeneric op-is-settable (em)
  (:documentation "Checks of the op at PC is settable"))

(defmethod op-is-settable ((em emulator))
  (> 31 (bit-vector->integer (value2-at-pc em))))

(defmacro with-op-scope (em &rest body)
  `(when (op-is-settable ,em)
     (let* ((mem-objects (get-memory-objects ,em (pc ,em)))
	    (a-vec (caadr mem-objects))
	    (b-vec (caar mem-objects))
	    (a-val (bit-vector->integer a-vec))
	    (b-val (bit-vector->integer b-vec))
	    (evaluation (progn ,@body)))
       (when evaluation
	 (replace a-vec (buffer-bit-vector-to-length 
		     16
		     (integer->bit-vector evaluation)))))))

(defgeneric set-op (em)
  (:documentation "Sets a to b"))

(defmethod set-op (em)
  (with-op-scope em
    b-val))

(defgeneric add-op (em)
  (:documentation "Adds b to a"))

(defmethod add-op ((em emulator))
  (with-op-scope em
   (let ((sum (+ a-val b-val)))
     (when (< *max-value* sum)
       (setf (overload em) #*0000000000000001))
     (mod sum *max-value*))))

(defun add-op-test ()
  (let* ((code "add a 0x21")
	 (compiled-code (parse-assembly code))
	 (em (make-instance 'emulator)))
    (insert-code-into-emulator em compiled-code)
    (setf (aref (registers em) 0) #*0000000000000001)
    (add-op em)
    (format t "val at 0: ~a~%" (ram-at em 0))
    (format t "registers: ~a~%" (registers em))))

(defgeneric sub-op (em)
  (:documentation "Subtracts b from a"))

(defmethod sub-op (em)
  (with-op-scope em
    (let ((sum (- a-val b-val)))
      (when (> sum 0)
	(setf sum (- *max-value* sum))
	(setf (overload em) #*1111111111111111))
      sum)))

(defgeneric mul-op (em)
  (:documentation "Sets a to b"))

(defmethod mul-op (em)
  (with-op-scope em
    (let* ((mul (* a-val b-val))
	   (overload (mod (truncate (/ mul *max-value*)) *max-value*)))
      (setf (overload em) (buffer-bit-vector-to-length
			   16
			   (integer->bit-vector  
			    (mod overload *max-value*))))
      (format t "aval: ~a bval: ~a mul:~a overload:~a~%" a-val b-val mul overload)
      (mod mul *max-value*))))

(defun mul-op-test ()
  (let* ((code "add a 0x21")
	 (compiled-code (parse-assembly code))
	 (em (make-instance 'emulator)))
    (insert-code-into-emulator em compiled-code)
    (setf (aref (registers em) 0) #*0000000000000001)
    (mul-op em)
    (format t "val at 0: ~a~%" (ram-at em 0))
    (format t "registers: ~a~% Overload~a~%" (registers em) (overload em))))

			     
(defgeneric div-op (em)
  (:documentation "Sets a to b"))

(defgeneric mod-op (em)
  (:documentation "Sets a to b"))

(defgeneric shl-op (em)
  (:documentation "Sets a to b"))

(defgeneric shr-op (em)
  (:documentation "Sets a to b"))

(defgeneric shr-op (em)
  (:documentation "Sets a to b"))

(defgeneric bor-op (em)
  (:documentation "Sets a to b"))

(defgeneric xor-op (em)
  (:documentation "Sets a to b"))

(defgeneric ife-op (em)
  (:documentation "Sets a to b"))

(defgeneric ifn-op (em)
  (:documentation "Sets a to b"))

(defgeneric ifg-op (em)
  (:documentation "Sets a to b"))

(defgeneric ifb-op (em)
  (:documentation "Sets a to b"))

;;Util
(defun bit-vector->command (bitv)
  (case (bit-vector->integer bitv)
    (0 'non-basic)
    (1 'set-op)
    (2 'add-op)
    (3 'sub-op)
    (4 'mul-op)
    (5 'div-op)
    (6 'mod-op)
    (7 'shl-op)
    (8 'shr-op)
    (9 'and-op)
    (10 'bor-op)
    (11 'xor-op)
    (12 'ife-op)
    (13 'ifn-op)
    (14 'ifg-op)
    (15 'ifb-op)))

;;Memory writing

(defgeneric increment-pc (emulator)
  (:documentation "Increments the pointer by the amount the op it currently is resting on uses"))

(defmethod increment-pc ((em emulator))
  (setf (pc em) (1+ (loop for i in (list (value1-at-pc em) (value2-at-pc em)) sum
			 (if (or (read-from-next-word-p i)
				 (within-range (bit-vector->integer i) 16 23)
				 (next-word-p i))
			     1
			     0)))))

(defgeneric execute-input-word (emulator)
  (:documentation "Exectutes the word at pc"))

(defmethod execute-input-word ((em emulator))
    (funcall (bit-vector->command (op-code-at-pc em)) em)
    (increment-pc em))




(defmethod pointer-value ((em emulator) (reg register))
  (translate-memory-address em (value-accessor reg)))

(defmethod next-pointer-value ((em emulator) (reg register))
  (setf (value-accessor reg) (1+ (value-accessor reg)))
  (pointer-value em reg))


(defmethod insert-code-into-emulator ((em emulator) list-of-bit-vectors)
  (loop for bitv in list-of-bit-vectors
     for i from 0 do
       (setf (aref (ram em) i) bitv)))

(defmethod bit-vector->register-mem ((em emulator) (bitv bit-vector))
  (let* ((reg-int (bit-vector->integer bitv))
	 (register (aref (registers em) (mod reg-int 8))))
    (cond ((< reg-int 8) register)
	  ((< reg-int 16) (ram-at em (bit-vector->integer register)))
	  ((< reg-int 23) (ram-at em (1+ (bit-vector->integer register)))))))

(defmethod setc ((em emulator) (bit1 bit-vector) (bit2 bit-vector))
  (loop 
     for x being the elements of (buffer-bit-vector-to-minlength 16 bit2)
     for i from 0 to 15 do
       (setf (aref bit1 i) x)))
		    
