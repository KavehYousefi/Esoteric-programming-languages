;; Date: 2022-01-10
;; 
;; Sources:
;;   -> "https://esolangs.org/wiki/%60LML"



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Declaration of types.                                        -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(deftype element ()
  "The ``element'' type specifies the compatible data types for the
   storage in the `LML list LI."
  '(or character string integer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of interpreter.                               -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-lowercase-character-for (index)
  "Returns the lowercase character associated with the INDEX."
  (declare (type (integer 0 *) index))
  (the character (char " abcdefghijklmnopqrstuvwxyz" index)))

;;; -------------------------------------------------------

(defun get-uppercase-character-for (index)
  "Returns the uppercase character associated with the INDEX."
  (declare (type (integer 0 *) index))
  (the character (char " ABCDEFGHIJKLMNOPQRSTUVWXYZ" index)))

;;; -------------------------------------------------------

(defun interpret-LML (code)
  "Interprets the piece of `LML CODE and returns no value.
   ---
   The list datastructure LI is realized as an adjustable vector in
   order to permit efficient insertions at the tail."
  (declare (type string code))
  (when (plusp (length code))
    (let ((position  0)
          (character (char code 0))
          (LI        (make-array 0
                       :element-type 'element
                       :adjustable   T
                       :fill-pointer 0))
          (X         0))
      (declare (type fixnum              position))
      (declare (type (or null character) character))
      (declare (type (vector element *)  LI))
      (declare (type integer             X))
      
      (flet
          ((advance ()
            "Moves the POSITION cursor to the next character in the
             CODE, if possible, updates the current CHARACTER, and
             returns no value."
            (setf character
              (when (< position (1- (length code)))
                (char code (incf position))))))
        
        (loop do
          (case character
            ;; End of CODE.
            ((NIL)
              (loop-finish))
            
            ;; Increment X by one.
            (#\+
              (incf X)
              (advance))
            
            ;; Decrement X by one.
            (#\-
              (decf X)
              (advance))
            
            ;; Append the numeric value stored in X to LI.
            (#\<
              (vector-push-extend X LI)
              (advance))
            
            ;; Append the minuscular character associated with X to LI.
            (#\(
              (vector-push-extend (get-lowercase-character-for X) LI)
              (advance))
            
            ;; Append the majuscular character associated with X to LI.
            (#\[
              (vector-push-extend (get-uppercase-character-for X) LI)
              (advance))
            
            ;; Prompt the user for a string or integer and append it
            ;; to LI.
            (#\:
              (format T "~&Please input a string: ")
              (let ((input (read-line)))
                (declare (type string input))
                (clear-input)
                (vector-push-extend input LI))
              (advance))
            
            ;; Join all elements of LI and print the result.
            (#\;
              (loop for element of-type element across LI do
                (write element :escape NIL :readably NIL))
              (advance))
            
            ;; Reset X to zero (0).
            (#\R
              (setf X 0)
              (advance))
            
            ;; Ignore non-command characters.
            (otherwise
              (advance)))))))
  (values))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "6 abb".
(interpret-LML
  "
  +++ +++< sets the value of X to 6 and adds 6 to LI
  R( resets the value of X and adds ' ' to LI
  +( sets X to 1 and adds 'a' to LI
  +((; sets X to 2 and adds 'b'to LI 2 times and then joins the elements and prints them
  ")

;;; -------------------------------------------------------

;; Print "Hello World".
(interpret-LML "++++++++[---(+++++++((+++(R(+++++++++++++++++++++++[--------(+++(------(--------(;")

;;; -------------------------------------------------------

;; Prompts the user for his name and print the message "Hi " followed by
;; the induced input.
(interpret-LML
  "
  ++++ ++++[ sets X to 8 and adds 'H' to LI
  +( sets X to 9 and adds 'i' to LI
  R( resets X to 0 and adds ' ' to LI
  : ; takes input in this case your name and adds it to LI then joins the elements and prints them
  ")

;;; -------------------------------------------------------

;; The same program as above, printing "Hi " followed by the input name,
;; represented without comments and further superfluous characters.
(interpret-LML "++++++++[+(R(:;")
