;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file furnishes the command definitions, each such a Tuvars
;; operation's encapsulation in a class, ensconcing in particular the
;; requisite information in the form of arguments.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of commands.                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct (Command
  (:copier NIL))
  "The ``Command'' interface furnishes a common foundry for all concrete
   classes intent on the encapsulation of Tuvars instructions.")

;;; -------------------------------------------------------

(defmacro define-command (class-name
                          &optional (class-documentation "")
                                    (argument-slot NIL))
  "Defines a ``defstruct'' based command class, nevened by the
   CLASS-NAME and always inheriting from the ``Command'' superclass,
   the nature of itself being delineated by the optional
   CLASS-DOCUMENTATION, and, if an ARGUMENT-SLOT participates, its
   conformation is expected to design a two-element list, bearing the
   slot name in its sinistral and the type in its dextral moiety."
  `(defstruct (,class-name
     (:include Command)
     (:copier NIL))
     ,class-documentation
     ,@(when argument-slot
         (destructuring-bind (slot-name slot-type)
             argument-slot
           (declare (type symbol slot-name))
           (declare (type T      slot-type))
           `((,slot-name (error "Missing value for slot ~a of class ~a."
                           (quote ,slot-name)
                           (quote ,class-name))
                         :type      ,slot-type
                         :read-only T))))))

;;; -------------------------------------------------------

(define-command Clear-Command
  "The ``Clear-Command'' class encapsulates the notion of the Tuvars
   \"clear\" command, responsible for the console content's purge in its
   entirety.")

;;; -------------------------------------------------------

(define-command Close-Command
  "The ``Close-Command'' class encapsulates the notion of the Tuvars
   \"close\" command, responsible for the console closure.")

;;; -------------------------------------------------------

(define-command Goto-Command
  "The ``Goto-Command'' class encapsulates the notion of the Tuvars
   \"goto\" command, responsible for the unconditional relocation of the
   program's instruction pointer (IP) to a line number communicated by
   its one-based line number's adminiculum."
  (line-number integer))

;;; -------------------------------------------------------

(define-command If-Command
  "The ``If-Command'' class encapsulates the notion of the Tuvars \"if\"
   command, responsible for the conditional execution of the subsequent
   command by a docimasy's adhibition to a specified variable's value."
  (variable character))

;;; -------------------------------------------------------

(define-command If-Not-Command
  "The ``If-Not-Command'' class encapsulates the notion of the Tuvars
   \"ifnot\" command, responsible for the conditional elision of the
   subsequent command by a docimasy's adhibition to a specified
   variable's value."
  (variable character))

;;; -------------------------------------------------------

(define-command Print-Character-Command
  "The ``Print-Character-Command'' class encapsulates the notion of the
   Tuvars \"char\" command, responsible for the issuance of a character
   obtained from a variable as its provenance to the standard output."
  (variable character))

;;; -------------------------------------------------------

(define-command Print-Linebreak-Command
  "The ``Print-Linebreak-Command'' class encapsulates the notion of the
   Tuvars \"linebreak\" command, responsible for the issuance of a
   newline character to the standard output.")

;;; -------------------------------------------------------

(define-command Print-Number-Command
  "The ``Print-Number-Command'' class encapsulates the notion of the
   Tuvars \"number\" command, responsible for the issuance of a number
   obtained from a variable as its provenance to the standard output."
  (variable character))

;;; -------------------------------------------------------

(define-command Print-String-Command
  "The ``Print-String-Command'' class encapsulates the notion of the
   Tuvars \"print\" command, responsible for the issuance of a string
   literal to the standard output."
  (message string))

;;; -------------------------------------------------------

(define-command Read-Command
  "The ``Read-Command'' class encapsulates the notion of the Tuvars
   \"read\" command, responsible for a character's obtention from the
   standard input and its induction into a specified variable."
  (variable character))

;;; -------------------------------------------------------

(define-command Title-Command
  "The ``Title-Command'' class encapsulates the notion of the Tuvars
   \"title\" command, responsible for the modification of the console
   title in order to emulate a specified string."
  (text string))
