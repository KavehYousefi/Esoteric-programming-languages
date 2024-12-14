;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file contributes the "Token-Stream" class, a reification of the
;; eponymous token stream concept, itself a twifaced entity in the
;; agency of a sequence of tokens' provision, being, imprimis, of such
;; abstract conception as to furnish an opaque ensconcement concerning
;; the underlying lexer opifice; while, on the other hand, tholing no
;; divestiture of potential nor entelech in its pragmatism's bailiwick.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Implementation of token stream.                              -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass Token-Stream ()
  ((lexer
    :initarg       :lexer
    :initform      (error "No lexer provided.")
    :type          Lexer
    :documentation "The token provision's provenance.")
   (next-token
    :initform      (make-eof-token)
    :type          Token
    :documentation "The most recently acquired token from the LEXER."))
  (:documentation
    "The ``Token-Stream'' class furnishes a tier edified upon the
     token purveyance as a simultaneously abstract and potent act."))

;;; -------------------------------------------------------

(defmethod initialize-instance :after ((tokens Token-Stream) &key)
  "Queries the first token from the lexer stored in the TOKENS stream,
   stores the same, and returns no value."
  (declare (type Token-Stream tokens))
  (setf (slot-value tokens 'next-token)
    (get-next-token
      (slot-value tokens 'lexer)))
  (values))

;;; -------------------------------------------------------

(defun make-token-stream (lexer)
  "Creates and returns a fresh ``Token-Stream'' nuncupated to the
   LEXER's token castaldy."
  (declare (type Lexer lexer))
  (the Token-Stream
    (make-instance 'Token-Stream :lexer lexer)))

;;; -------------------------------------------------------

(defun consume-token (tokens)
  "Returns the next token from the stream of TOKENS and consumes the
   same."
  (declare (type Token-Stream tokens))
  (the Token
    (prog1
      (slot-value tokens 'next-token)
      (setf (slot-value tokens 'next-token)
        (get-next-token
          (slot-value tokens 'lexer))))))

;;; -------------------------------------------------------

(defun peek-token (tokens)
  "Returns the next token from the stream of TOKENS without its
   concomitant consumption."
  (declare (type Token-Stream tokens))
  (the Token
    (slot-value tokens 'next-token)))

;;; -------------------------------------------------------

(defun next-token-is-of-type-p (tokens expected-token-type)
  "Determines whether the next element from the stream of TOKENS
   conforms to the EXPECTED-TOKEN-TYPE, returning on confirmation a
   ``boolean'' value of ``T'', otherwise ``NIL''."
  (declare (type Token-Stream tokens))
  (declare (type keyword      expected-token-type))
  (the boolean
    (token-is-of-type-p (peek-token tokens) expected-token-type)))

;;; -------------------------------------------------------

(defun expect-token (tokens expected-token-type)
  "Determines whether the next element from the stream of TOKENS
   conforms to the EXPECTED-TOKEN-TYPE, returning on confirmation the
   probed token, while concomitantly consuming and thus removing it from
   the TOKENS; otherwise an error of an unspecified type is signaled."
  (declare (type Token-Stream tokens))
  (declare (type keyword      expected-token-type))
  (let ((next-token (peek-token tokens)))
    (declare (type Token next-token))
    (if (token-is-of-type-p next-token expected-token-type)
      (consume-token tokens)
      (error "Expected a token of the type ~a, but encountered ~a."
        expected-token-type next-token))
    (the Token next-token)))
