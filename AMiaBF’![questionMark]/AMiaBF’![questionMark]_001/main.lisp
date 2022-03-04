;; Author: Kaveh Yousefi
;; Date:   2022-03-04
;; 
;; Sources:
;;   [esolang2021amiabf'!?]
;;   -> "https://esolangs.org/wiki/AMiaBF%E2%80%99!%3F"
;;       o Specification of the esoteric programming language
;;         "AMiaBF’!?".
;;   
;;   [mccallum2006exploration]
;;   -> "https://www.math.arizona.edu/~ura-reports/063/Sandhu.Aliana/Midterm.pdf"
;;       o Description and discussion of the arithmetic derivative.
;;   
;;   [number2012arithmeticderivative]
;;   -> "https://number.subwiki.org/wiki/Arithmetic_derivative"
;;       o Pellucid description and discussion of the arithmetic
;;         derivative.
;;   
;;   [oeiswiki2019arithmeticderivative]
;;   -> "http://oeis.org/wiki/Arithmetic_derivative"
;;       o Description of the arithmetic derivative.
;;   
;;   [rod2018factoringinalgebra]
;;   -> "https://www.mathsisfun.com/algebra/factoring.html"
;;       o Describes factorization.
;;   
;;   [rod2021fundtheoremofarithm]
;;   -> "https://www.mathsisfun.com/numbers/fundamental-theorem-arithmetic.html"
;;       o Describes the Fundamental Theorem of Arithmetic in simple
;;         terms.
;;       o The theorem states that:
;;           "Any integer greater than 1 is either a prime number, or
;;            can be written as a unique product of prime numbers
;;            (ignoring the order)."
;;   
;;   [rod2021primefactorization]
;;   -> "https://www.mathsisfun.com/prime-factorization.html"
;;       o Describes prime numbers and prime factorization in simple
;;         terms.
;;   
;;   [wikipedia2021arithmeticderivate]
;;   -> "https://en.wikipedia.org/wiki/Arithmetic_derivative"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype list-of (&optional (element-type T))
  "The ``list-of'' type defines a list of zero or more elements, each
   of which conforms to the ELEMENT-TYPE, the same defaults to the
   comprehensive ``T''."
  (let ((predicate (gensym)))
    (declare (type symbol predicate))
    (setf (symbol-function predicate)
      #'(lambda (object)
          (declare (type T object))
          (and
            (listp object)
            (every
              #'(lambda (element)
                  (declare (type T element))
                  (typep element element-type))
              (the list object)))))
    `(satisfies ,predicate)))

;;; -------------------------------------------------------

(deftype prime-generator ()
  "The ``prime-generator'' type defines a niladic function which,
   starting with the value two (2), upon each invocation returns the
   next prime number."
  '(function () (integer 2 *)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of arithmetic derivative.                     -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun prime-p (candidate)
  "Checks whether the CANDIDATE integer represents a prime number,
   returning on confirmation a ``boolean'' value of ``T'', otherwise
   ``NIL''."
  (declare (type (integer 2 *) candidate))
  (the boolean
    (loop
      for divisor
        of-type (integer 1 *)
        from    candidate
        downto  2
      when (zerop (mod candidate divisor))
        count 1 into number-of-divisors
      when (>= number-of-divisors 2)
        do (return NIL)
      finally
        (return T))))

;;; -------------------------------------------------------

(defun make-prime-generator ()
  "Creates and returns a niladic function which, starting with the value
   two (2), returns upon each invocation the next prime number.
   ---
   Example:
     ;; Print the first ten prime numbers to the standard output, that
     ;; is: 2, 3, 5, 7, 11, 13, 17, 19, 23, 29.
     (let ((generator (make-prime-generator)))
       (declare (type prime-generator generator))
       (loop repeat 10 do
         (print (funcall generator))))"
  (let ((next-prime 2))
    (declare (type (integer 2 *) next-prime))
    (the function
      #'(lambda ()
          (the (integer 2 *)
            (prog1 next-prime
              (loop
                do    (incf next-prime)
                until (prime-p next-prime))))))))

;;; -------------------------------------------------------

(defun get-next-prime (prime-generator)
  "Returns the next prime number from the PRIME-GENERATOR."
  (declare (type prime-generator))
  (the (integer 2 *) (funcall prime-generator)))

;;; -------------------------------------------------------

(defun get-exponent-for-prime (number prime)
  "Returns the exponent by which the PRIME number was raised in order
   to contribute to the product constructing the NUMBER.
   ---
   A concomitant of its arithmetic notion, the exponent represents the
   tally of occurrences of the PRIME in the NUMBER."
  (declare (type (integer 0 *) number))
  (declare (type (integer 2 *) prime))
  (the (integer 0 *)
    (loop
      with  remainder of-type (integer 0 *) = number
      while (zerop (mod remainder prime))
      do    (setf remainder (/ remainder prime))
      count 1)))

;;; -------------------------------------------------------

(defun get-prime-summands (number)
  "Returns for the NUMBER a list of those prime terms, each composed of
   a prime number p[i] divided into its exponent k[i] as the ratio
   k[i]/p[i], which by summation accumulate into the value of the
   NUMBER.
   ---
   As postulated by the fundamental theorem of arithmetic, any positive
   integer number can be represented as a product of one or more prime
   numbers {p[1], p[2], ..., p[r]}, with r being the tally of the
   constituent primes, contributing one or more times k[i], with k >= 1,
   in the form of an exponent to the prime, to the operation. In order
   to compute the arithmetic derivative, each prime number p[i] from
   {p[1], p[2], ..., p[r]} and its respective exponent k[i] from
   {k[1], k[2], ..., k[r]} are subjected to a division operation
   k[i]/p[i]. The members of this set {k[1]/p[1], k[1]/p[2], ...,
   k[1]/p[r]} are finally summed into the arithmetic derivative.
   ---
   This function, ``get-prime-summands'', returns the set {k[1]/p[1],
   k[1]/p[2], ..., k[1]/p[r]} as a list of ``rational''-typed elements.
   The desistence from solving the quotient's floating-point value
   conduces the obviation of rounding errors."
  (declare (type (integer 0 *) number))
  
  (let ((prime-generator (make-prime-generator)))
    (declare (type prime-generator))
    
    (the (list-of rational)
      (loop
        ;; The PRODUCT realizes the iteration's termination condition:
        ;; It is multiplied by all prime numbers contained in the NUMBER
        ;; a tally of times equal to their exponent. The moment the
        ;; PRODUCT equals the NUMBER, we can be ascertained that all
        ;; prime factors of the latter have been discovered, as the
        ;; PRODUCT has been constructed using the same.
        with product
          of-type (integer 1 *)
          =       1
        
        ;; As long as the PRODUCT does not equal the input NUMBER, not
        ;; all prime factors have been discovered yet.
        while (< product number)
        
        ;; Load the next prime number.
        for prime
          of-type (integer 2 *)
          =       (get-next-prime prime-generator)
        ;; Compute the exponent (tally) of the current PRIME in the
        ;; NUMBER.
        for prime-exponent
          of-type (integer 0 *)
          =       (get-exponent-for-prime number prime)
        
        ;; If the PRIME contributes to the NUMBER, that is, displays an
        ;; exponent (tally) greater than zero, collect the ratio
        ;;   k[i]
        ;;   ----
        ;;   p[i]
        ;; 
        ;; Update the PRODUCT to reflect the contribution of the current
        ;; PRIME by inducing it the tally PRIME-EXPONENT number of
        ;; times.
        when (plusp prime-exponent)
          collect (/ prime-exponent prime)
            into summands
          and do
            (setf product
              (* product
                 (expt prime prime-exponent)))
        
        finally
          (return summands)))))

;;; -------------------------------------------------------

(defun get-arithmetic-derivative (number)
  "Calculates and returns the arithmetic derivative of the NUMBER using
   prime factorization."
  (declare (type (integer 0 *) number))
  (the (integer 0 *)
    (cond
      ((zerop number)
        0)
      ((= number 1)
        0)
      ((prime-p number)
        1)
      (T
        (* number (reduce #'+ (get-prime-summands number)))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun whitespace-character-p (character)
  "Checks whether the CHARACTER represents a whitespace, returning on
   confirmation a ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null
      (member character '(#\Space #\Tab #\Newline) :test #'char=)))))

;;; -------------------------------------------------------

(defun command-character-p (character)
  "Checks whether the CHARACTER represents a member of the AMiaBF’!?
   command set, returning on confirmation a ``boolean'' value of ``T'',
   otherwise ``NIL''."
  (declare (type character character))
  (the boolean
    (not (null (find character "AMiaBF’!?" :test #'char=)))))

;;; -------------------------------------------------------

(defun interpret-AMiaBF’!? (code)
  "Interprets the piece of AMiaBF’!? CODE and returns no value."
  (declare (type string code))
  
  (when (plusp (length code))
    (let ((position    0)
          (character   (char code 0))
          (accumulator 0))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (declare (type integer             accumulator))
      
      (flet
          ((advance ()
            "Moves the POSITION cursor to the next character, if
             possible, updates the current CHARACTER, and returns no
             value."
            (setf character
              (when (< position (1- (length code)))
                (char code (incf position))))
            (values))
           
           (recede ()
            "Moves the POSITION cursor to the previous character, if
             possible, updates the current CHARACTER, and returns no
             value."
            (setf character
              (when (plusp position)
                (char code (decf position))))
            (values)))
        
        (loop do
          (case character
            ;; End of file? => Terminate the program.
            ((NIL)
              (loop-finish))
            
            ;; Skip whitespaces.
            ((#\Space #\Tab #\Newline)
              (advance))
            
            ;; If the accumulator is 0, jump forward to the next "B"
            ;; command.
            (#\A
              (cond
                ((zerop accumulator)
                  (advance)
                  (loop with level of-type integer = 0 do
                    (case character
                      ((NIL)
                        (error "Unmatched 'A' command."))
                      (#\A
                        (incf level)
                        (advance))
                      (#\B
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (advance))))
                      (otherwise
                        (advance)))))
                (T
                  (advance))))
            
            ;; Print the character "#".
            (#\M
              (write-char #\#)
              (advance))
            
            ;; Increase the accumulator by 1.
            (#\i
              (incf accumulator 1)
              (advance))
            
            ;; Print the accumulator in caps.
            (#\a
              (write accumulator)
              (advance))
            
            ;; If the accumulator is not 0, jump back to the previous
            ;; "A" command.
            (#\B
              (cond
                ((not (zerop accumulator))
                  (recede)
                  (loop with level of-type integer = 0 do
                    (case character
                      ((NIL)
                        (error "Unterminated 'B' command."))
                      (#\A
                        (cond
                          ((zerop level)
                            (advance)
                            (loop-finish))
                          (T
                            (decf level)
                            (recede))))
                      (#\B
                        (incf level)
                        (recede))
                      (otherwise
                        (recede)))))
                (T
                  (advance))))
            
            ;; Increase the accumulator by 5.
            (#\F
              (incf accumulator 5)
              (advance))
            
            ;; Set the accumulator to its arithmetic derivative.
            (#\‘
              (setf accumulator
                (if (minusp accumulator)
                  (- (get-arithmetic-derivative (abs accumulator)))
                  (get-arithmetic-derivative accumulator)))
              (advance))
            
            ;; Skip the next command.
            (#\!
              (advance)
              ;; Find the next command.
              (loop
                until
                  (or (null character)
                      (command-character-p character))
                do
                  (advance))
              ;; If a command is found, skip it.
              (when (and character (command-character-p character))
                (advance)))
            
            ;; Input number and store in the accumulator.
            (#\?
              (format T "~&Please input an integer: ")
              (let ((input (read)))
                (declare (type integer input))
                (clear-input)
                (setf accumulator input))
              (advance))
            
            ;; Any other character is prohibited.
            (otherwise
              (error "Invalid character ~s at position ~d."
                character position)))))))
  
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine.
(interpret-AMiaBF’!? "?AaBa")

;;; -------------------------------------------------------

;; Print the character "#" infinitely to the console.
(interpret-AMiaBF’!? "FAMB")

;;; -------------------------------------------------------

;; Input a number, compute its arithmetic derivative, and print it.
(interpret-AMiaBF’!? "?‘a")

;;; -------------------------------------------------------

;; Infinitely repeating cat program.
(interpret-AMiaBF’!? "?aA?aB")
