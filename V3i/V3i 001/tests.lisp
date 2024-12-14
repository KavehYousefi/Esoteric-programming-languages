;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file applies itself to the definition of the test cases, its
;; bailiwick the endeictic dever of proving the interpreter
;; implementation's conformance to the V3i standard's stipulations.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Truth-machine.
(interpret-V3i
  "if input = 0;
   print \"0\"; end;
   else;
   iloop print \"1\";")

;;; -------------------------------------------------------

;; One-time cat program.
(interpret-V3i "print input;")

;;; -------------------------------------------------------

;; Fibonacci sequence printer.
(interpret-V3i
  "def x 1;
   def y 1;
   print 0;
   print 1;
   print 1;
   iloop; def x x+y; print x; def y y+x; print y;")

;;; -------------------------------------------------------

;; Fibonacci sequence printer which abides one second after each output
;; issuance.
(interpret-V3i
  "def x 1;
   def y 1;
   print 0;
   print 1;
   print 1;
   iloop; def x x+y; print x; def y y+x; print y; wait 1000;")

;;; -------------------------------------------------------

;; Lyrics of the song "99 Bottles of Beer".
(interpret-V3i
  "
  def x 99;
  loop 99;
  if x = 1;
  print \"1 bottle of beer on the wall\";
  print \"1 bottle of beer\";
  print \"Take one down, pass it around\";
  print \"No more bottles of beer on the wall\";
  else;
  print x \" bottles of beer on the wall\";
  print x \" bottles of beer\";
  print \"Take one down, pass it around\";
  def x x-1;
  print x \" bottles of beer on the wall\";
  ")

;;; -------------------------------------------------------

;; Simple, non-conformant Deadfish interpreter.
(interpret-V3i
  "def x 0;                 # Simulates the accumulator.     #
   def y \"\";              # Stores the current user input. #
   
   iloop;
   def y input;
   
   if y = \"o\";
     print x;
   else if y = \"i\";
     def x x+1;
   else if y = \"d\";
     def x x-1;
   else if y = \"s\";
     def x x*x;
   else;
     print \"Invalid command\";")

;;; -------------------------------------------------------

;; Conformant Deadfish interpreter.
(interpret-V3i
  "def x 0;                 # Simulates the accumulator.     #
   def y \"\";              # Stores the current user input. #
   
   iloop;
   def y input;
   
   if y = \"o\";
     print x;
   else if y = \"i\";
     def x x+1;
   else if y = \"d\";
     def x x-1;
   else if y = \"s\";
     def x x*x;
   else;
     print \"\";
   end;
   
   # Normalize the accumulator. #
   if x = -1;
     def x 0;
   else if x = 256;
     def x 0;")
