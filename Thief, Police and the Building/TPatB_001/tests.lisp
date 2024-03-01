;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the test cases which serve in a twireason
;; purpose as a endeictic warklume for the
;; "Thief, Police and the Building" programming language's
;; demonstration, as well as confirmation of the interpreter's correct
;; operation.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(interpret-Thief-Police-and-the-Building
 "A thief on G/F
  Set SoE -> 3F/s
  Set SoS -> 1F/s
  top: G-th floor
  btm: -3-th floor
  G/F H e l l o ,
  a b c d e f
  2 3 j k l m
  W o r l d !
  He climbs into 1-th room and steals
  He climbs into 2-th room and steals
  He climbs into 3-th room and steals
  He climbs into 4-th room and steals
  He climbs into 5-th room and steals
  He climbs into 6-th room and steals
  He gets into the elevator and gets down
  He stays in the elevator for 1s
  He gets out
  He climbs into 1-th room and steals
  He climbs into 2-th room and steals
  He climbs into 3-th room and steals
  He climbs into 4-th room and steals
  He climbs into 5-th room and steals
  He climbs into 6-th room and steals
  The police have come")

;;; -------------------------------------------------------

;; Print "kiwi".
;; 
;; The thief starts in the cellar, designated by the floor number -1;
;; the elevator moves by two floors, while the stair room merely by one.
(interpret-Thief-Police-and-the-Building
  "
  A thief on -1/F
  
  Set SoE -> 2F/s
  Set SoS -> 1F/s
  
  top:  3-th floor
  btm: -1-th floor
      q w e r t y u i o p
      a s d f g h j k l ]
      z x c v b n m , . !
  G/F 0 1 2 3 4 5 6 7 8 9
      _ _ _ _ _ _ _ _ _ _
  
  He gets into the elevator and gets up
  He stays in the elevator for 2s
  He gets out
  He gets into the stair room and gets down
  He stays in the stair room for 1s
  He gets out
  He climbs into 8-th room and steals
  He gets into the stair room and gets down
  He stays in the stair room for 1s
  He gets out
  He gets into the elevator and gets up
  He stays in the elevator for 1s
  He gets out
  He climbs into 8-th room and steals
  He climbs into 2-th room and steals
  He climbs into 8-th room and steals
  The police have come
  ")
