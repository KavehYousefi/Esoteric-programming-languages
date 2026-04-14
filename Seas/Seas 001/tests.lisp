;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file implements the Seas interpreter's test cases, each
;; specimen's plasmature a dedicated function.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; One-time cat program.
(defun demonstrate-a-one-time-cat-program ()
  "This exemplary Seas program executes a one-time character-based cat
   program and returns no value."
  (interpret-the-Seas-lines
    "🌊🌊🌊"
    "📖📅🔼")
  (values))

;;; -------------------------------------------------------

;; Olamic cat program which never ceases to operate.
(defun demonstrate-a-repeating-cat-program ()
  "This exemplary Seas program executes an olamic character-based cat
   program and, if aborted in any fashion, returns no value."
  (interpret-the-Seas-lines
    "🌊🌊🌊🌊"
    "▶️📖📅🔽"
    "🔼🟦🟦◀️")
  (values))

;;; -------------------------------------------------------

(defun demonstrate-a-truth-machine ()
  "This examplary Seas program executes a truth-machine and returns no
   value."
  (interpret-the-Seas-lines
    "🌊🌊🌊🌊🌊🌊🌊"
    "💡🔽🗳🟦🟦🟦🟦"
    "🟦🟦0️⃣🟦🟦🟦🟦"
    "🟦▶️📍🟦🟦🟦🟦"
    "🟦🟦▶️1️⃣🗳1️⃣◀️")
  (values))
