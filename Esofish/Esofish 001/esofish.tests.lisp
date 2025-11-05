;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file serves in the Esofish interpreter test cases' furnishment,
;; thilk act in the agency as an affedavit to the conformity with the
;; language standard stipulations' sickerness.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Selection of the package.                                    -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :esofish)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-the-15-hello-world-program ()
  "Executes the Esofish interpreter in conjunction with the \"15\"
   dialect of the \"5\" programming language family in the context of a
   \"Hello, World!\" printer and returns no value."
  (interpret-esofish "iissdr`Hello, World!`")
  (values))

;;; -------------------------------------------------------

  ;; Truth-machine which deploys the "15" language dialect.
(defun test-the-15-truth-machine ()
  "Executes the Esofish interpreter in conjunction with the \"15\"
   dialect of the \"5\" programming language family in the context of a
   truth-machine and returns no value."
  (interpret-esofish "iissdr;_@{#!}")
  (values))

;;; -------------------------------------------------------

;; Truth-machine which deploys the "35" language dialect.
(defun test-the-35-truth-machine ()
  "Executes the Esofish interpreter in conjunction with the \"35\"
   dialect of the \"5\" programming language family in the context of a
   truth-machine and returns no value."
  (interpret-esofish "iiiiiisd@{#!}"))
