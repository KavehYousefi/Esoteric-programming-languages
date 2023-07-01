;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; This file comprehends the test and example programs, serving both to
;; verify the SOAP interpreter's operations and to demonstrate the SOAP
;; programming language's diorisms and capabilities.
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; -- Test cases.                                                  -- ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Print "Hello, World!".
(interpret-SOAP "\"H\"e\"l\"l\"o\",\" \"W\"o\"r\"l\"d\"!")

;;; -------------------------------------------------------

;; Query the user for a character and, if this input equals the letter
;; "A", print the message "Z".
(interpret-SOAP "~'A/\"Z\\")

;;; -------------------------------------------------------

;; Infinitely print the character "1".
(interpret-SOAP "⊆Ø[\"1]")

;;; -------------------------------------------------------

;; Truth-machine.
(interpret-SOAP "*1~'0/\"0\\'1/⊇{1}[\"1]\\")

;;; -------------------------------------------------------

;; Print the message "HaHaHa" by employing the variable "%" in
;; conjunction with a superset iteration.
;; 
;; The program comprehends a twain of stages:
;;   (1) The main set M is set to M = {1, 2, 3}.
;;   (2) As long as the value of variable "%", initially assuming 1, is
;;       comprehended in M, the following loop operates:
;;       (2.1) "Ha" is printed to the standard output.
;;       (2.2) The variable "%" is incremented.
;;             If "%" acquires the value 4, an object not listed in the
;;             main set M, the iteration ceases.
(interpret-SOAP "*1*2*10
                 ⊇{%}[\"H\"a :]")

;;; -------------------------------------------------------

;; Print the message "HaHaHa" by employing the variable "%" in
;; conjunction with a superset iteration.
;; 
;; The program comprehends a treble of stages:
;;   (1) The main set M is set to M = {2, 3, 4}.
;;   (2) The variable "%", initially assuming 1, is increment to the
;;       value 4.
;;   (3) As long as the value of variable "%", is comprehended in M, the
;;       following loop operates:
;;       (3.1) "Ha" is printed to the standard output.
;;       (3.2) The variable "%" is decremented.
;;             If "%" acquires the value 1, an object not listed in the
;;             main set M, the iteration ceases.
(interpret-SOAP "*2*10*11
                 :::
                 ⊇{%}[\"H\"a ;]")

;;; -------------------------------------------------------

;; One-time cat program which recognizes the printable characters in the
;; ASCII code range [32, 255].
(interpret-SOAP
  "
  ~
  ' /\" \\
  '!/\"!\\
  '\"/\"\"\\
  '#/\"#\\
  '$/\"$\\
  '%/\"%\\
  '&/\"&\\
  ''/\"'\\
  '(/\"(\\
  ')/\")\\
  '*/\"*\\
  '+/\"+\\
  ',/\",\\
  '-/\"-\\
  './\".\\
  '//\"/\\
  '0/\"0\\
  '1/\"1\\
  '2/\"2\\
  '3/\"3\\
  '4/\"4\\
  '5/\"5\\
  '6/\"6\\
  '7/\"7\\
  '8/\"8\\
  '9/\"9\\
  ':/\":\\
  ';/\";\\
  '</\"<\\
  '=/\"=\\
  '>/\">\\
  '?/\"?\\
  '@/\"@\\
  'A/\"A\\
  'B/\"B\\
  'C/\"C\\
  'D/\"D\\
  'E/\"E\\
  'F/\"F\\
  'G/\"G\\
  'H/\"H\\
  'I/\"I\\
  'J/\"J\\
  'K/\"K\\
  'L/\"L\\
  'M/\"M\\
  'N/\"N\\
  'O/\"O\\
  'P/\"P\\
  'Q/\"Q\\
  'R/\"R\\
  'S/\"S\\
  'T/\"T\\
  'U/\"U\\
  'V/\"V\\
  'W/\"W\\
  'X/\"X\\
  'Y/\"Y\\
  'Z/\"Z\\
  '[/\"[\\
  '\\/\"\\\\
  ']/\"]\\
  '^/\"^\\
  '_/\"_\\
  '`/\"`\\
  'a/\"a\\
  'b/\"b\\
  'c/\"c\\
  'd/\"d\\
  'e/\"e\\
  'f/\"f\\
  'g/\"g\\
  'h/\"h\\
  'i/\"i\\
  'j/\"j\\
  'k/\"k\\
  'l/\"l\\
  'm/\"m\\
  'n/\"n\\
  'o/\"o\\
  'p/\"p\\
  'q/\"q\\
  'r/\"r\\
  's/\"s\\
  't/\"t\\
  'u/\"u\\
  'v/\"v\\
  'w/\"w\\
  'x/\"x\\
  'y/\"y\\
  'z/\"z\\
  '{/\"{\\
  '|/\"|\\
  '}/\"}\\
  '~/\"~\\
  ' /\" \\
  '¡/\"¡\\
  '¢/\"¢\\
  '£/\"£\\
  '¤/\"¤\\
  '¥/\"¥\\
  '¦/\"¦\\
  '§/\"§\\
  '¨/\"¨\\
  '©/\"©\\
  'ª/\"ª\\
  '«/\"«\\
  '¬/\"¬\\
  '­/\"­\\
  '®/\"®\\
  '¯/\"¯\\
  '°/\"°\\
  '±/\"±\\
  '²/\"²\\
  '³/\"³\\
  '´/\"´\\
  'µ/\"µ\\
  '¶/\"¶\\
  '·/\"·\\
  '¸/\"¸\\
  '¹/\"¹\\
  'º/\"º\\
  '»/\"»\\
  '¼/\"¼\\
  '½/\"½\\
  '¾/\"¾\\
  '¿/\"¿\\
  'À/\"À\\
  'Á/\"Á\\
  'Â/\"Â\\
  'Ã/\"Ã\\
  'Ä/\"Ä\\
  'Å/\"Å\\
  'Æ/\"Æ\\
  'Ç/\"Ç\\
  'È/\"È\\
  'É/\"É\\
  'Ê/\"Ê\\
  'Ë/\"Ë\\
  'Ì/\"Ì\\
  'Í/\"Í\\
  'Î/\"Î\\
  'Ï/\"Ï\\
  'Ð/\"Ð\\
  'Ñ/\"Ñ\\
  'Ò/\"Ò\\
  'Ó/\"Ó\\
  'Ô/\"Ô\\
  'Õ/\"Õ\\
  'Ö/\"Ö\\
  '×/\"×\\
  'Ø/\"Ø\\
  'Ù/\"Ù\\
  'Ú/\"Ú\\
  'Û/\"Û\\
  'Ü/\"Ü\\
  'Ý/\"Ý\\
  'Þ/\"Þ\\
  'ß/\"ß\\
  'à/\"à\\
  'á/\"á\\
  'â/\"â\\
  'ã/\"ã\\
  'ä/\"ä\\
  'å/\"å\\
  'æ/\"æ\\
  'ç/\"ç\\
  'è/\"è\\
  'é/\"é\\
  'ê/\"ê\\
  'ë/\"ë\\
  'ì/\"ì\\
  'í/\"í\\
  'î/\"î\\
  'ï/\"ï\\
  'ð/\"ð\\
  'ñ/\"ñ\\
  'ò/\"ò\\
  'ó/\"ó\\
  'ô/\"ô\\
  'õ/\"õ\\
  'ö/\"ö\\
  '÷/\"÷\\
  'ø/\"ø\\
  'ù/\"ù\\
  'ú/\"ú\\
  'û/\"û\\
  'ü/\"ü\\
  'ý/\"ý\\
  'þ/\"þ\\
  'ÿ/\"ÿ\\
  ")

;;; -------------------------------------------------------

;; Infinitely repeating cat program which recognizes the printable
;; characters in the ASCII code range [32, 255].
(interpret-SOAP
  "
  *1
  ⊇{1}[~
  ' /\" \\
  '!/\"!\\
  '\"/\"\"\\
  '#/\"#\\
  '$/\"$\\
  '%/\"%\\
  '&/\"&\\
  ''/\"'\\
  '(/\"(\\
  ')/\")\\
  '*/\"*\\
  '+/\"+\\
  ',/\",\\
  '-/\"-\\
  './\".\\
  '//\"/\\
  '0/\"0\\
  '1/\"1\\
  '2/\"2\\
  '3/\"3\\
  '4/\"4\\
  '5/\"5\\
  '6/\"6\\
  '7/\"7\\
  '8/\"8\\
  '9/\"9\\
  ':/\":\\
  ';/\";\\
  '</\"<\\
  '=/\"=\\
  '>/\">\\
  '?/\"?\\
  '@/\"@\\
  'A/\"A\\
  'B/\"B\\
  'C/\"C\\
  'D/\"D\\
  'E/\"E\\
  'F/\"F\\
  'G/\"G\\
  'H/\"H\\
  'I/\"I\\
  'J/\"J\\
  'K/\"K\\
  'L/\"L\\
  'M/\"M\\
  'N/\"N\\
  'O/\"O\\
  'P/\"P\\
  'Q/\"Q\\
  'R/\"R\\
  'S/\"S\\
  'T/\"T\\
  'U/\"U\\
  'V/\"V\\
  'W/\"W\\
  'X/\"X\\
  'Y/\"Y\\
  'Z/\"Z\\
  '[/\"[\\
  '\\/\"\\\\
  ']/\"]\\
  '^/\"^\\
  '_/\"_\\
  '`/\"`\\
  'a/\"a\\
  'b/\"b\\
  'c/\"c\\
  'd/\"d\\
  'e/\"e\\
  'f/\"f\\
  'g/\"g\\
  'h/\"h\\
  'i/\"i\\
  'j/\"j\\
  'k/\"k\\
  'l/\"l\\
  'm/\"m\\
  'n/\"n\\
  'o/\"o\\
  'p/\"p\\
  'q/\"q\\
  'r/\"r\\
  's/\"s\\
  't/\"t\\
  'u/\"u\\
  'v/\"v\\
  'w/\"w\\
  'x/\"x\\
  'y/\"y\\
  'z/\"z\\
  '{/\"{\\
  '|/\"|\\
  '}/\"}\\
  '~/\"~\\
  ' /\" \\
  '¡/\"¡\\
  '¢/\"¢\\
  '£/\"£\\
  '¤/\"¤\\
  '¥/\"¥\\
  '¦/\"¦\\
  '§/\"§\\
  '¨/\"¨\\
  '©/\"©\\
  'ª/\"ª\\
  '«/\"«\\
  '¬/\"¬\\
  '­/\"­\\
  '®/\"®\\
  '¯/\"¯\\
  '°/\"°\\
  '±/\"±\\
  '²/\"²\\
  '³/\"³\\
  '´/\"´\\
  'µ/\"µ\\
  '¶/\"¶\\
  '·/\"·\\
  '¸/\"¸\\
  '¹/\"¹\\
  'º/\"º\\
  '»/\"»\\
  '¼/\"¼\\
  '½/\"½\\
  '¾/\"¾\\
  '¿/\"¿\\
  'À/\"À\\
  'Á/\"Á\\
  'Â/\"Â\\
  'Ã/\"Ã\\
  'Ä/\"Ä\\
  'Å/\"Å\\
  'Æ/\"Æ\\
  'Ç/\"Ç\\
  'È/\"È\\
  'É/\"É\\
  'Ê/\"Ê\\
  'Ë/\"Ë\\
  'Ì/\"Ì\\
  'Í/\"Í\\
  'Î/\"Î\\
  'Ï/\"Ï\\
  'Ð/\"Ð\\
  'Ñ/\"Ñ\\
  'Ò/\"Ò\\
  'Ó/\"Ó\\
  'Ô/\"Ô\\
  'Õ/\"Õ\\
  'Ö/\"Ö\\
  '×/\"×\\
  'Ø/\"Ø\\
  'Ù/\"Ù\\
  'Ú/\"Ú\\
  'Û/\"Û\\
  'Ü/\"Ü\\
  'Ý/\"Ý\\
  'Þ/\"Þ\\
  'ß/\"ß\\
  'à/\"à\\
  'á/\"á\\
  'â/\"â\\
  'ã/\"ã\\
  'ä/\"ä\\
  'å/\"å\\
  'æ/\"æ\\
  'ç/\"ç\\
  'è/\"è\\
  'é/\"é\\
  'ê/\"ê\\
  'ë/\"ë\\
  'ì/\"ì\\
  'í/\"í\\
  'î/\"î\\
  'ï/\"ï\\
  'ð/\"ð\\
  'ñ/\"ñ\\
  'ò/\"ò\\
  'ó/\"ó\\
  'ô/\"ô\\
  'õ/\"õ\\
  'ö/\"ö\\
  '÷/\"÷\\
  'ø/\"ø\\
  'ù/\"ù\\
  'ú/\"ú\\
  'û/\"û\\
  'ü/\"ü\\
  'ý/\"ý\\
  'þ/\"þ\\
  'ÿ/\"ÿ\\
  ]")

;;; -------------------------------------------------------

;; Load and execute the SOAP script "resources/Deadfish.soap", an
;; interpreter developed by Esolang user BoundedBeans for the esoteric
;; programming language "Deadfish".
;; 
;; Please do not forget to insert your own file path under which the
;; "Deadfish.soap" file is located in lieu of this exemplary
;; specification.
(load-SOAP-script
  (make-pathname
    :device    "C"
    :directory '(:relative "resources")
    :name      "Deadfish"
    :type      "soap"))
