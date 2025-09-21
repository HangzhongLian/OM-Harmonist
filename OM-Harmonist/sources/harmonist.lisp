
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;------------------------copyright © 2024 Hangzhong LIAN---------------------
;----------------------------All rights reserved.----------------------------
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------
;----------------------------------------------------------------------------


(in-package :harm)


#|------------------------this is all global variable-----------------------|#

(defparameter *note-midic* 
          '(("C" . 0) ("D" . 200) ("E" . 400) ("F" . 500) ("G" . 700) ("A" . 900) ("B" . 1100)))

(defparameter *accidental-midic* 
          '(("bb" . -200) ("b" . -100) (nil . 0) ("=" . 0) ("#" . 100) ("x" . 200)))

(defparameter *accidental_display-midic*
          '(("bb" . 12) ("b" . 38) (nil . 0) ("=" . 50) ("#" . 62) ("x" . 88)))

(defparameter *enharmonic-notes*
          '((0 . "B#") (0 . "C") (0 . "Dbb") (100 . "Bx") (100 . "C#") (100 . "Db") (200 . "Cx") (200 . "D") (200 . "Ebb") (300 . "D#") (300 . "Eb") (300 . "Fbb")
            (400 . "Dx") (400 . "E") (400 . "Fb") (500 . "E#") (500 . "F") (500 . "Gbb") (600 . "Ex") (600 . "F#") (600 . "Gb") (700 . "Fx") (700 . "G") (700 . "Abb")
            (800 . "G#") (800 . "Ab") (900 . "Gx") (900 . "A") (900 . "Bbb") (1000 . "A#") (1000 . "Bb") (1000 . "Cbb") (1100 . "Ax") (1100 . "B") (1100 . "Cb")))

(defparameter *number-of-semitones-in-interval*
          '(("p1" . 0) ("a1" . 1) ("d2" . 0) ("m2" . 1) ("M2" . 2) ("a2" . 3) ("d3" . 2) ("m3" . 3) ("M3" . 4) ("a3" . 5) ("d4" . 4) ("p4" . 5) ("a4" . 6) ("d5" . 6) ("p5" . 7) ("a5" . 8)
            ("d6" . 7) ("m6" . 8) ("M6" . 9) ("a6" . 10) ("d7" . 9) ("m7" . 10) ("M7" . 11) ("a7" . 12) ("d8" . 11) ("p8" . 12) ("a8" . 13)))
; "d" = diminished, "m" = minor, "p" = perfect, "M" = Major, "a" = augmented


(defparameter *semitones-and-interval-name*
          '((0 . "Perfect unison") (1 . "Augmented unison") (0 . "Diminished second") (1 . "Minor second") (2 . "Major second") (3 . "Augmented second")
            (2 . "Diminished third") (3 . "Minor third") (4 . "Major third") (5 . "Augmented third") (4 . "Diminished fourth") (5 . "Perfect fourth")
            (6 . "Augmented fourth") (6 . "Diminished fifth") (7 . "Perfect fifth") (8 . "Augmented fifth") (7 . "Diminished sixth") (8 . "Minor sixth")
            (9 . "Major sixth") (10 . "Augmented sixth") (9 . "Diminished seventh") (10 . "Minor seventh") (11 . "Major seventh") (12 . "Augmented seventh")
            (11 . "Diminished octave") (12 . "Perfect octave") (13 . "Augmented octave")))


(defparameter *interval-name*
          '(("p1" . "Perfect unison") ("a1" . "Augmented unison") ("d2" . "Diminished second") ("m2" . "Minor second") ("M2" . "Major second") ("a2" . "Augmented second")
            ("d3" . "Diminished third") ("m3" . "Minor third") ("M3" . "Major third") ("a3" . "Augmented third") ("d4" . "Diminished fourth") ("p4" . "Perfect fourth")
            ("a4" . "Augmented fourth") ("d5" . "Diminished fifth") ("p5" . "Perfect fifth") ("a5" . "Augmented fifth") ("d6" . "Diminished sixth") ("m6" . "Minor sixth")
            ("M6" . "Major sixth") ("a6" . "Augmented sixth") ("d7" . "Diminished seventh") ("m7" . "Minor seventh") ("M7" . "Major seventh") ("a7" . "Augmented seventh")
            ("d8" . "Diminished octave") ("p8" . "Perfect octave") ("a8" . "Augmented octave")))


(defparameter *semitones-in-compound-interval*
          '(("d9" . 12) ("m9" . 13) ("M9" . 14) ("a9" . 15) ("d10" . 14) ("m10" . 15) ("M10" . 16) ("a10" . 17) ("d11" . 16) ("p11" . 17)
            ("a11" . 18) ("d12" . 18) ("p12" . 19) ("a12" . 20) ("d13" . 19) ("m13" . 20) ("M13" . 21) ("a13" . 22) ("d14" . 21) ("m14" . 22) 
            ("M14" . 23) ("a14" . 24) ("d15" . 23) ("p15" . 24) ("a15" . 25)))
; "d" = diminished, "m" = minor, "p" = perfect, "M" = Major, "a" = augmented


(defparameter *compound-interval-name*
          '(("d9" . "Diminished ninth") ("m9" . "Minor ninth") ("M9" . "Major ninth") ("a9" . "Augmented ninth") ("d10" . "Diminished tenth") 
            ("m10" . "Minor tenth") ("M10" . "Major tenth") ("a10" . "Augmented tenth") ("d11" . "Diminished eleventh") ("p11" . "Perfect eleventh") 
            ("a11" . "Augmented eleventh") ("d12" . "Diminished twelfth") ("p12" . "Perfect twelfth") ("a12" . "Augmented twelfth") ("d13" . "Diminished thirteenth")
            ("m13" . "Minor thirteenth") ("M13" . "Major thirteenth") ("a13" . "Augmented thirteenth") ("d14" . "Diminished fourteenth") ("m14" . "Minor fourteenth") 
            ("M14" . "Major fourteenth") ("a14" . "Augmented fourteenth") ("d15" . "Diminished fifteenth") ("p15" . "Perfect fifteenth") ("a15" . "Augmented fifteenth")))

#|------------------------this is all global variable-----------------------|#



#|---------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
--------------------------1. pitch data receiver system----------------------------
-----------------------------------------------------------------------------------
---------------------------------------------------------------------------------|#


(om::defmethod! pitch-receiver (note accidental register)
         :icon 40001
         :initvals '("A" "" "4")
         :menuins '(
                    (0 (("C" "C") ("D" "D") ("E" "E") ("F" "F") ("G" "G") ("A" "A") ("B" "B")))
                    (1 (("bb" "bb") ("b" "b") ("" "") ("=" "=") ("#" "#") ("x" "x")))
                    (2 (("0" "0")("1" "1") ("2" "2") ("3" "3") ("4" "4") ("5" "5") ("6" "6") ("7" "7") ("8" "8")))
                    )
         :doc "<input1>: pitch
                  <input2>: accidental
                  <input3>: register, C4 means middle C, C1 => 2400 cent, C4 => 6000 cent, C8 => 10800 cent, and so on..."
         (concatenate 'string note accidental register))

                         
(defun string-separator (string)
           "split a string into a single character"
           (loop for i from 0 to (- (length string) 1)
                   collect (subseq string i (+ i 1))))
;(string-separator "bach") => ("b" "a" "c" "h")

         

(defun pitch-info (pitch-string)
           "split the pitch-string into three parts: note, accidental and register"
           (let* ((sep-str (string-separator pitch-string))
                    (note (car sep-str))
                    (register (car (reverse sep-str)))
                    (accidental-original (butlast (cdr sep-str)))
                    (accidental (if (= (length accidental-original) 2) 
                                    (om::string+ (car accidental-original) (cadr accidental-original)) (car accidental-original))))
             (list note accidental register)))
;(pitch-info "Cbb4") => ("C" "bb" "4")
             


(defun pitch->midic (pitch-info)
           "convert the result of <pitch-info> to normal midicent value"
           (let ((note-midic (cdr (assoc (car pitch-info) *note-midic* :test #'equal)))
                  (accidental-midic (cdr (assoc (cadr pitch-info) *accidental-midic* :test #'equal)))
                  (register-num (car (multiple-value-list (read-from-string (caddr pitch-info))))))
             (+ note-midic (* 1200 (+ register-num 1)) accidental-midic)))         ; midicent = note-midic + 1200 * (register-num + 1) + accidental-midic

;(pitch->midic '("C" "bb" "4")) => 5800




;;---------------this is auxiliary functions--------------

(defun find-key-pairs (key pairs)
            "find all key-value pairs with a specific key"
            (remove-if-not (lambda (pair) (eql (car pair) key))
                           pairs))
;(find-key-pairs 0 *enharmonic-notes*) => ((0 . "B#") (0 . "C") (0 . "Dbb"))


(defun find-key-pairs-last (key pairs)
            "find all key-value pairs with a specific key after the dot"
            (remove-if-not (lambda (pair) (eql (cdr pair) key))
                           pairs))
;(find-key-pairs-last 5 *number-of-semitones-in-interval*) => (("a3" . 5) ("p4" . 5))


(defun cdr-pro (a-lists)
            "returns the last item of all key-value pairs"
            (loop for a in a-lists
                  collect (cdr a) into ok-list
                  finally (return ok-list))) 
;(cdr-pro '((0 . "B#") (0 . "C") (0 . "Dbb"))) => ("B#" "C" "Dbb")


(defun car-pro (a-lists)
            "returns the first item of all key-value pairs"
            (loop for a in a-lists
                  collect (car a) into ok-list
                  finally (return ok-list)))
;(car-pro '((0 . "B#") (0 . "C") (0 . "Dbb"))) => (0 0 0)


(defun str-list-prefix (string-list)
            "returns the prefix of each item in the string-list"
            (loop for a in string-list
                   collect (car (string-separator a)) into ok-str-list
                   finally (return ok-str-list)))  
;(str-list-prefix '("B#" "C" "Dbb")) => ("B" "C" "D")


(defun chord-suffix (chord)
            "returns the suffix of each pitch in the chord"
            (loop for pitch in chord
                    collect (second (pitch-info pitch))))
;(chord-suffix '("B#4" "C4" "Dbb4"))
;(chord-suffix '("B#5" "Cx4" "Dbb4"))
;(chord-suffix '("Bx5"))



(defun string-prefix-p (prefix string)
            "check if STRING starts with PREFIX"
            (and (>= (length string) (length prefix))
                 (string= prefix (subseq string 0 (length prefix)))))
;(string-prefix-p "B""B#") => t


(defun string-suffix-p (suffix string)
            "check if STRING ends with SUFFIX"
            (if (string-equal suffix
                              (second (pitch-info (om::string+ string "4"))))
                t
              nil))
;(string-suffix-p "#" "B#") => t
;(string-suffix-p nil "B") => t


(defun string-suffix-p-special (string)
  (if (or (string-suffix-p "x" string)
          (string-suffix-p "bb" string))
      t nil))
;(string-suffix-p-special "B#")
;(string-suffix-p-special "Bx")


(defun double-accidental? (pitch)
  (let ((acci (second (pitch-info pitch))))
    (if (or (string-equal acci "bb")
            (string-equal acci "x"))
        t nil)))
;(double-accidental? "B#4")
;(double-accidental? "Bbb4")
;(double-accidental? "Bx4")


(defun find-string-with-prefix (prefix list)
            "Find the first string in LIST that starts with PREFIX"
            (find prefix list :test (lambda (prefix str) (string-prefix-p prefix str))))  
;(find-string-with-prefix "B" '("C#" "Bx" "B#")) => "Bx"


(defun string-to-number (num-string)
            "Converts a number-string to a number"
            (car (multiple-value-list (read-from-string num-string))))
;(string-to-number "5201314") => 5201314
                               
;;--------------------------------------------------------




(defun midic->enharmonic (midic)
           "find the corresponding enharmonic notes based on the midicent"
           (let ((mod-midic (mod midic 1200)))
               (cdr-pro (find-key-pairs mod-midic *enharmonic-notes*))))
;(midic->enharmonic 6000) => ("B#" "C" "Dbb")




(defun convert-double-accidental (pitch)
           "convert the double sharp or flat to normal accidental"
           (let* (
                  (pitch-midic (pitch->midic (pitch-info pitch)))
                  (eligible-enharmonic (remove-if 
                                        #'string-suffix-p-special 
                                        (midic->enharmonic (pitch->midic (pitch-info pitch)))))
                  (substitute-string (car eligible-enharmonic))
                  (temporary-oct (third (pitch-info pitch)))
                  (temporary-midic (pitch->midic 
                                    (pitch-info 
                                     (om::string+ substitute-string temporary-oct))))
                  (right-oct (cond ((eq temporary-midic pitch-midic) 
                                    temporary-oct)
                                   ((eq (pitch->midic 
                                         (pitch-info 
                                          (om::string+ substitute-string (write-to-string 
                                                                          (1+ (string-to-number temporary-oct))))))
                                        pitch-midic)
                                    (write-to-string (1+ (string-to-number temporary-oct))))
                                   (t (write-to-string (1- (string-to-number temporary-oct))))))
                  (right-pitch (om::string+ substitute-string right-oct))
                  )
             right-pitch))
;(convert-double-accidental "Bx2")
             



(defun convert-chord (chord)
           "convert all the double accidental pitch in chord"
           (loop for pitch in chord
                 collect (if (double-accidental? pitch)
                             (convert-double-accidental pitch)
                           pitch)))
;(convert-chord '("E4" "G#4" "B#4" "D#5" "Fx5"))
;(convert-chord '("Ex4" "Gbb4" "Bx4" "Dbb5" "F5"))





(defun pitch->display (pitch-info)
           "convert the result of <pitch-info> to the abnormal midicent value used by the score displayer"
           (let ((note-midic (cdr (assoc (car pitch-info) *note-midic* :test #'equal)))
                  (ab-accidental-midic (cdr (assoc (cadr pitch-info) *accidental_display-midic* :test #'equal)))
                  (register-num (car (multiple-value-list (read-from-string (caddr pitch-info))))))
             (+ note-midic (* 1200 (+ register-num 1)) ab-accidental-midic)))
;(pitch->display (pitch-info "B#4")) => 7162
;(pitch->display (pitch-info "Cbb5")) => 7212
;(pitch->display (pitch-info "Ax2")) => 4588





(defun display->pitch (display)
           "convert the abnormal midicent used by the score displayer to the PITCH"
           (let* ( 
                  (accidental (car (car (find-key-pairs-last (mod display 100) *accidental_display-midic*))))
                  (midicent (mod (- display (mod display 100)) 1200))
                  (octave-info (write-to-string (- (car (multiple-value-list (om::om// display 1200))) 1)))
                  (note-name (car (car (find-key-pairs-last midicent *note-midic*))))
                  )
             (om::string+ note-name accidental octave-info)))                 
;(display->pitch 7162) => "B#4"
;(display->pitch 7212) => "Cbb5"
;(display->pitch 4588) => "Ax2"





(defun multiple-pitch->midic (pitch-list)
           "convert the list of pitch-strings to list of midicent"
           (loop for a in pitch-list
                 collect (pitch->midic (pitch-info a))))

;(multiple-pitch->midic '("C=4" "E4" "G4" "Bb4")) => (6000 6400 6700 7000)                




(defun multiple-pitch->display (pitch-list)
           "convert the list of pitch-strings to abnormal midicent used by the score displayer"
           (loop for a in pitch-list
                 collect (pitch->display (pitch-info a))))

;(multiple-pitch->display '("C=4" "E4" "G4" "Bb4")) => (6050 6400 6700 7138)                


(defun multiple-display->pitch (display-list)
           "convert the list of abnormal midicent used by the score displayer to the PITCH-LIST"
           (loop for a in display-list
                 collect (display->pitch a)))

;(multiple-display->pitch '(6050 6400 6700 7138)) => ("C=4" "E4" "G4" "Bb4")




(defun multiple-chord->midic (chord-list)
           "convert the list of chord to the list of MIDIC"
           (loop for chord in chord-list
                 collect (multiple-pitch->midic chord)))

; (multiple-chord->midic '(("D3" "F=3" "D4" "Bb4") ("D2" "F=3" "D4" "Bb4") ("D3" "D4" "F=4" "Bb4") ("D2" "D4" "F=4" "Bb4")))            




(defun multiple-chord->display (chord-list)
           "convert the list of chord to the list of MIDIC"
           (loop for chord in chord-list
                 collect (multiple-pitch->display chord)))

; (multiple-chord->display '(("D3" "F=3" "D4" "Bb4") ("D2" "F=3" "D4" "Bb4") ("D3" "D4" "F=4" "Bb4") ("D2" "D4" "F=4" "Bb4"))) 


           

(defun size? (pitch-1 pitch-2)
           "determine the size of two notes
            (If pitch-1 < pitch-2, output T
             If pitch-1 > pitch-2, output NIL
             If pitch-1 = pitch-2, output = )"
           (cond ((< (pitch->midic (pitch-info pitch-1)) (pitch->midic (pitch-info pitch-2))) t)
                 ((> (pitch->midic (pitch-info pitch-1)) (pitch->midic (pitch-info pitch-2))) nil)
                 (t (let (
                          (note1-name (car (pitch-info pitch-1)))
                          (note2-name (car (pitch-info pitch-2)))
                          (note1-oct (string-to-number (car (reverse (string-separator pitch-1)))))
                          (note2-oct (string-to-number (car (reverse (string-separator pitch-2)))))              
                          )
                      (cond ((< note1-oct note2-oct) t)
                            ((> note1-oct note2-oct) nil)
                            ((< (position note1-name '("C" "D" "E" "F" "G" "A" "B") :test #'equal)
                                (position note2-name '("C" "D" "E" "F" "G" "A" "B") :test #'equal)) t)
                            ((> (position note1-name '("C" "D" "E" "F" "G" "A" "B") :test #'equal)
                                (position note2-name '("C" "D" "E" "F" "G" "A" "B") :test #'equal)) nil)
                            (t "="))))))


; (size? "B#3" "C=4")
; (size? "A#3" "Bb3")
; (size? "Bb3" "A#3")
; (size? "C4" "C4")
; (size? "Bx3" "Db4")
; (size? "A4" "C4")

                            
                 
             









#|---------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-------------------------------2. interval system----------------------------------
-----------------------------------------------------------------------------------
---------------------------------------------------------------------------------|#


; "d" = diminished, "m" = minor, "p" = perfect, "M" = Major, "a" = augmented

;------construct the interval according to the given size and quality------

(om::defmethod! simple-interval-maker-up (root number quality)     
         :icon 40041
         :initvals '("C4" 5 "p")
         :doc "write an simple interval of specified quality upwards according to the root
               Attention please: If you enter an interval that does not exist, an error will be reported!!!!
                                 If there is a ternary flat or ternary sharp, the output will be wrong!!
                       <input1> : root (must be a string! for the format, please see <pitch-receiver>)
                       <input2> : number (<=8)
                       <input3> : quality (must be a string!)"
         (let* (
                 (root-midic (pitch->midic (pitch-info root)))                  
                 (interval-str (concatenate 'string quality (write-to-string number)))                  
                 (interval-midic (* (cdr (assoc interval-str *number-of-semitones-in-interval* :test #'equal)) 100)) 
                 (upper-note-midic (+ root-midic interval-midic))  
                 (all-upper-notes (midic->enharmonic upper-note-midic))
                 (upper-notes-prefix (str-list-prefix all-upper-notes))     ; get the note name information for all enharmonic notes                  
                 (octave-scale (om::flat (om::repeat-n '("C" "D" "E" "F" "G" "A" "B") 4)))
                 (calculate-register (lambda (root number)
                                       (let ((octave-position (car (multiple-value-list (read-from-string (car (reverse (string-separator root)))))))
                                             (note-name (car (string-separator root))))
                                         (cond
                                          ((= number 8)
                                           (1+ octave-position))
                                          ((and (= number 7) (not (string= note-name "C")))
                                           (1+ octave-position))
                                          ((and (= number 6) (not (member note-name '("C" "D") :test 'string=)))
                                           (1+ octave-position))
                                          ((and (= number 5) (not (member note-name '("C" "D" "E") :test 'string=)))
                                           (1+ octave-position))
                                          ((and (= number 4) (not (member note-name '("C" "D" "E" "F") :test 'string=)))
                                           (1+ octave-position))
                                          ((and (= number 3) (member note-name '("A" "B") :test 'string=))
                                           (1+ octave-position))
                                          ((and (= number 2) (string= note-name "B"))
                                           (1+ octave-position))
                                          ((= number 1)
                                           octave-position)
                                          (t
                                           octave-position)))))
                 (correct-note-name (nth (- number 1)
                                         (subseq octave-scale
                                                 (position (car (string-separator root)) octave-scale :test #'equal)
                                                 (length octave-scale))))             ; get the correct note name
                 (correct-pitch (find-string-with-prefix correct-note-name all-upper-notes))         ;get the correct pitch
                 (register-info (funcall calculate-register root number))      ;get the register information                 
                 )
             (list root (concatenate 'string correct-pitch (write-to-string register-info))))
         )

; (simple-interval-maker-up "C4" 5 "p") => ("C4" "G4")




(om::defmethod! simple-interval-maker-down (root number quality)     
         :icon 40041
         :initvals '("C4" 5 "p")
         :doc "write an simple interval of specified quality downwards according to the root
               Attention please: If you enter an interval that does not exist, an error will be reported!!!!
                                 If there is a ternary flat or ternary sharp, the output will be wrong!!
                       <input1> : root (must be a string! for the format, please see <pitch-receiver>)
                       <input2> : number (<=8)
                       <input3> : quality (must be a string!)"
         (let* (
                 (root-midic (pitch->midic (pitch-info root)))                  
                 (interval-str (concatenate 'string quality (write-to-string number)))                  
                 (interval-midic (* (cdr (assoc interval-str *number-of-semitones-in-interval* :test #'equal)) 100)) 
                 (down-note-midic (- root-midic interval-midic))  
                 (all-down-notes (midic->enharmonic down-note-midic))
                 (down-notes-prefix (str-list-prefix all-down-notes))     ; get the note name information for all enharmonic notes                  
                 (octave-scale (om::flat (om::repeat-n '("C" "D" "E" "F" "G" "A" "B") 4)))
                 (calculate-register (lambda (root number)
                                       (let ((octave-position (car (multiple-value-list (read-from-string (car (reverse (string-separator root)))))))
                                             (note-name (car (string-separator root))))
                                         (cond
                                          ((= number 8)
                                           (1- octave-position))
                                          ((and (= number 7) (not (string= note-name "B")))
                                           (1- octave-position))
                                          ((and (= number 6) (not (member note-name '("B" "A") :test 'string=)))
                                           (1- octave-position))
                                          ((and (= number 5) (not (member note-name '("B" "A" "G") :test 'string=)))
                                           (1- octave-position))
                                          ((and (= number 4) (not (member note-name '("B" "A" "G" "F") :test 'string=)))
                                           (1- octave-position))
                                          ((and (= number 3) (member note-name '("D" "C") :test 'string=))
                                           (1- octave-position))
                                          ((and (= number 2) (string= note-name "C"))
                                           (1- octave-position))
                                          ((= number 1)
                                           octave-position)
                                          (t
                                           octave-position)))))
                 (correct-note-name (nth (- 8 number)
                                         (subseq octave-scale
                                                 (position (car (string-separator root)) octave-scale :test #'equal)
                                                 (length octave-scale))))             ; get the correct note name
                 (correct-pitch (find-string-with-prefix correct-note-name all-down-notes))         ;get the correct pitch
                 (register-info (funcall calculate-register root number))      ;get the register information                 
                 )
             (list root (concatenate 'string correct-pitch (write-to-string register-info))))
         )

; (simple-interval-maker-down "C4" 5 "p") => ("C4" "F3")




(om::defmethod! simple-interval-maker (root number quality towards)
         :icon 40041
         :initvals '("C4" 5 "p" "up")
         :menuins '(
                    (3 (("up" "up") ("down" "down")))
                    )
         :doc "write an simple interval of specified quality according to the root and direction
               Attention please: If you enter an interval that does not exist, an error will be reported!!!!
                                 If there is a ternary flat or ternary sharp, the output will be wrong!!
                       <input1> : root (must be a string! for the format, please see <pitch-receiver>)
                       <input2> : number (<=8)
                       <input3> : quality (must be a string!)"
         (if (string-equal towards "up")
             (simple-interval-maker-up root number quality)
           (simple-interval-maker-down root number quality)))

; (simple-interval-maker "C4" 5 "p" "up") => ("C4" "G4")
; (simple-interval-maker "C4" 5 "p" "down") => ("C4" "F3")





(om::defmethod! compound-interval-maker-up (root number quality)
         :icon 40041
         :initvals '("C4" 12 "p")
         :doc "write an compound interval of specified quality upwards according to the root
               Attention please: If you enter an interval that does not exist, an error will be reported!!!!
                                 If there is a ternary flat or ternary sharp, the output will be wrong!!
                       <input1> : root (must be a string! for the format, please see <pitch-receiver>)
                       <input2> : number (>8)
                       <input3> : quality (must be a string!)"
         (let* (
                (simple-interval (car (cdr (simple-interval-maker-up root (mod number 7) quality))))
                (octave-number (car (multiple-value-list (om::om// number 7))))
                (octave-info (write-to-string (+ octave-number (car (multiple-value-list (read-from-string (car (reverse (pitch-info root)))))))))
                (compound-interval (om::string+ (om::string+ (car (pitch-info simple-interval)) (cadr (pitch-info simple-interval))) octave-info))
                )
           (list root compound-interval)
           ))

; (compound-interval-maker-up "C4" 10 "M") => ("C4" "E5")
; (compound-interval-maker-up "C4" 10 "m") => ("C4" "Eb5")




(om::defmethod! compound-interval-maker-down (root number quality)
         :icon 40041
         :initvals '("C4" 12 "p")
         :doc "write an compound interval of specified quality downwards according to the root
               Attention please: If you enter an interval that does not exist, an error will be reported!!!!
                                 If there is a ternary flat or ternary sharp, the output will be wrong!!
                       <input1> : root (must be a string! for the format, please see <pitch-receiver>)
                       <input2> : number (>8)
                       <input3> : quality (must be a string!)"
         (let* (
                (simple-interval (car (cdr (simple-interval-maker-down root (mod number 7) quality))))
                (octave-number (car (multiple-value-list (om::om// number 7))))
                (octave-info (write-to-string (1- (car (multiple-value-list (read-from-string (car (reverse (pitch-info simple-interval)))))))))
                (compound-interval (om::string+ (om::string+ (car (pitch-info simple-interval)) (cadr (pitch-info simple-interval))) octave-info))
                )
           (list root compound-interval)
           ))

; (compound-interval-maker-down "C4" 10 "M") => ("C4" "Ab2")
; (compound-interval-maker-down "C4" 10 "m") => ("C4" "A2")




(om::defmethod! compound-interval-maker (root number quality towards)
         :icon 40041
         :initvals '("C4" 12 "p" "up")
         :menuins '(
                    (3 (("up" "up") ("down" "down")))
                    )
         :doc "write an compound interval of specified quality according to the root and towards
               Attention please: If you enter an interval that does not exist, an error will be reported!!!!
                                 If there is a ternary flat or ternary sharp, the output will be wrong!!
                       <input1> : root (must be a string! for the format, please see <pitch-receiver>)
                       <input2> : number (>8)
                       <input3> : quality (must be a string!)"
         (if (string-equal towards "up")
             (compound-interval-maker-up root number quality)
           (compound-interval-maker-down root number quality)))

; (compound-interval-maker "C4" 10 "M" "up") => ("C4" "E5")
; (compound-interval-maker "C4" 10 "M" "down") => ("C4" "Ab2")




(om::defmethod! interval-maker-up (root number quality)
         :icon 40041
         :initvals '("C4" 5 "p")
         :doc "Write an interval of specified quality upwards according to the root. Handles both simple and compound intervals.
               Attention please: If you enter an interval that does not exist, an error will be reported!!!!
                                 If there is a ternary flat or ternary sharp, the output will be wrong!!
                <input1> : root (must be a string! for the format, please see <pitch-receiver>)
                <input2> : number
                <input3> : quality (must be a string!)"
         (let* (
                (root-midic (pitch->midic (pitch-info root)))
                (octave-scale (om::flat (om::repeat-n '("C" "D" "E" "F" "G" "A" "B") 4)))
                (calculate-register (lambda (root number)
                                      (let ((octave-position (car (multiple-value-list (read-from-string (car (reverse (string-separator root)))))))
                                            (note-name (car (string-separator root))))
                                        (cond
                                         ((= number 8)
                                          (1+ octave-position))
                                         ((and (= number 7) (not (string= note-name "C")))
                                          (1+ octave-position))
                                         ((and (= number 6) (not (member note-name '("C" "D") :test 'string=)))
                                          (1+ octave-position))
                                         ((and (= number 5) (not (member note-name '("C" "D" "E") :test 'string=)))
                                          (1+ octave-position))
                                         ((and (= number 4) (not (member note-name '("C" "D" "E" "F") :test 'string=)))
                                          (1+ octave-position))
                                         ((and (= number 3) (member note-name '("A" "B") :test 'string=))
                                          (1+ octave-position))
                                         ((and (= number 2) (string= note-name "B"))
                                          (1+ octave-position))
                                         ((= number 1)
                                          octave-position)
                                         (t
                                          octave-position)))))
                (interval-str (concatenate 'string quality (write-to-string (if (> number 8) (mod number 7) number))))
                (interval-midic (* (cdr (assoc interval-str *number-of-semitones-in-interval* :test #'equal)) 100))
                (upper-note-midic (+ root-midic interval-midic))
                (all-upper-notes (midic->enharmonic upper-note-midic))
                (upper-notes-prefix (str-list-prefix all-upper-notes))
                (correct-note-name (nth (- (if (> number 8) (mod number 7) number) 1)
                                        (subseq octave-scale
                                                (position (car (string-separator root)) octave-scale :test #'equal)
                                                (length octave-scale))))
                (correct-pitch (find-string-with-prefix correct-note-name all-upper-notes))
                (register-info (if (> number 8)
                                   (+ (car (multiple-value-list (om::om// number 7)))
                                      (car (multiple-value-list (read-from-string (car (reverse (pitch-info root)))))))
                                 (funcall calculate-register root number)))
                )
           (if (> number 8)
               (list root (concatenate 'string correct-pitch (write-to-string register-info)))
             (list root (concatenate 'string correct-pitch (write-to-string register-info))))))

; (interval-maker-up "C4" 10 "M") => ("C4" "E5")
; (interval-maker-up "C4" 3 "m") => ("C4" "Eb4")






(om::defmethod! interval-maker-down (root number quality)
         :icon 40041
         :initvals '("C4" 5 "p")
         :doc "Write an interval of specified quality downwards according to the root. Handles both simple and compound intervals.
               Attention please: If you enter an interval that does not exist, an error will be reported!!!!
                                 If there is a ternary flat or ternary sharp, the output will be wrong!!
                <input1> : root (must be a string! for the format, please see <pitch-receiver>)
                <input2> : number
                <input3> : quality (must be a string!)"
         (if (<= number 8)
             (let* (
                    (root-midic (pitch->midic (pitch-info root)))
                    (interval-str (concatenate 'string quality (write-to-string number)))
                    (interval-midic (* (cdr (assoc interval-str *number-of-semitones-in-interval* :test #'equal)) 100))
                    (down-note-midic (- root-midic interval-midic))
                    (all-down-notes (midic->enharmonic down-note-midic))
                    (down-notes-prefix (str-list-prefix all-down-notes))
                    (octave-scale (om::flat (om::repeat-n '("C" "D" "E" "F" "G" "A" "B") 4)))
                    (calculate-register (lambda (root number)
                                          (let ((octave-position (car (multiple-value-list (read-from-string (car (reverse (string-separator root)))))))
                                         (note-name (car (string-separator root))))
                                            (cond
                                             ((= number 8)
                                              (1- octave-position))
                                             ((and (= number 7) (not (string= note-name "B")))
                                              (1- octave-position))
                                             ((and (= number 6) (not (member note-name '("B" "A") :test 'string=)))
                                              (1- octave-position))
                                             ((and (= number 5) (not (member note-name '("B" "A" "G") :test 'string=)))
                                              (1- octave-position))
                                             ((and (= number 4) (not (member note-name '("B" "A" "G" "F") :test 'string=)))
                                              (1- octave-position))
                                             ((and (= number 3) (member note-name '("D" "C") :test 'string=))
                                              (1- octave-position))
                                             ((and (= number 2) (string= note-name "C"))
                                              (1- octave-position))
                                             ((= number 1)
                                              octave-position)
                                             (t
                                              octave-position)))))
                    (correct-note-name (nth (- 8 number)
                                            (subseq octave-scale
                                                    (position (car (string-separator root)) octave-scale :test #'equal)
                                                    (length octave-scale))))
                    (correct-pitch (find-string-with-prefix correct-note-name all-down-notes))
                    (register-info (funcall calculate-register root number))
                    )
               (list root (concatenate 'string correct-pitch (write-to-string register-info))))

           (let* (
                  (simple-interval (car (cdr (simple-interval-maker-down root (mod number 7) quality))))
                  (octave-number (car (multiple-value-list (om::om// number 7))))
                  (octave-info (write-to-string (1- (car (multiple-value-list (read-from-string (car (reverse (pitch-info simple-interval)))))))))
                  (compound-interval (om::string+ (om::string+ (car (pitch-info simple-interval)) (cadr (pitch-info simple-interval))) octave-info))
                  )
             (list root compound-interval))))

; (interval-maker-down "C4" 10 "M") => ("C4" "Ab2")
; (interval-maker-down "C4" 3 "m") => ("C4" "A3")




(om::defmethod! interval-maker (root number quality towards)
         :icon 40041
         :initvals '("C4" 5 "p" "up")
         :menuins '(
                    (3 (("up" "up") ("down" "down")))
                    )
         :doc "write an compound interval of specified quality according to the root and towards
               Attention please: If you enter an interval that does not exist, an error will be reported!!!!
                                 If there is a ternary flat or ternary sharp, the output will be wrong!!
                       <input1> : root (must be a string! for the format, please see <pitch-receiver>)
                       <input2> : number (>8)
                       <input3> : quality (must be a string!)"
         (if (string-equal towards "up")
             (interval-maker-up root number quality)
           (interval-maker-down root number quality)))

; (interval-maker "C4" 10 "M" "up") => ("C4" "E5")
; (interval-maker "C4" 10 "M" "down") => ("C4" "Ab2")
; (interval-maker "C4" 3 "m" "up") => ("C4" "Eb4")
; (interval-maker "C4" 3 "m" "down") => ("C4" "A3")






;--------------interval identification within two octaves-----------------


(om::defmethod! simple-interval-identify (root upper)
         :icon 40031
         :initvals '("C4" "G4")
         :doc "Identify the size and quality of simple intervals according to the input root and upper note."
         (let* (
                (octave-scale (om::flat (om::repeat-n '("C" "D" "E" "F" "G" "A" "B") 4)))
                (upper-position (1+ (position (car (string-separator upper)) 
                                         (subseq octave-scale (position (car (string-separator root)) octave-scale :test #'equal) 
                                                 (length octave-scale)) :test #'equal)))                
                (semitones (/ (- (pitch->midic (pitch-info upper)) (pitch->midic (pitch-info root))) 100))
                (interval-size-simple (if (and (= upper-position 1) (> semitones 1))
                                          8 upper-position))
                (possibilities (car-pro (find-key-pairs-last semitones *number-of-semitones-in-interval*)))
                (quality-searcher (lambda (lists)
                                    (loop for a in lists
                                            if (string= (cadr (string-separator a)) (write-to-string interval-size-simple))
                                            do (return a))))
                (correct-quality (funcall quality-searcher possibilities))
                )
           (cdr (assoc correct-quality *interval-name* :test #'equal))
           ))

; (simple-interval-identify "C4" "Gb4") => "Diminished fifth"
; (simple-interval-identify "C4" "Bb4") => "Minor seventh"




(om::defmethod! interval-identify (root upper)
         :icon 40031
         :initvals '("C4" "C6")
         :doc "Identify the size and quality of intervals according to the input root and upper note."
         (if (stringp (simple-interval-identify root upper))
             (simple-interval-identify root upper)
           (let* (
                  (octave-scale (om::flat (om::repeat-n '("C" "D" "E" "F" "G" "A" "B") 4))) 
                  (upper-note (car (string-separator upper)))
                  (root-note (car (string-separator root)))                                   
                  (semitones (/ (- (pitch->midic (pitch-info upper)) (pitch->midic (pitch-info root))) 100))
                  (upper-position (1+ (position upper-note 
                                                (subseq octave-scale (position root-note octave-scale :test #'equal) 
                                                        (length octave-scale)) :test #'equal)))
                  (interval-size-simple (if (= upper-position 1)
                                            8 upper-position))
                  (simple-possibilities (car-pro (find-key-pairs-last (- semitones 12) *number-of-semitones-in-interval*)))
                  (quality-searcher (lambda (lists)
                                      (loop for a in lists
                                            if (string= (cadr (string-separator a)) (write-to-string interval-size-simple))
                                              do (return a))))
                  (correct-quality (car (string-separator (funcall quality-searcher simple-possibilities))))
                  (correct-size (write-to-string
                                 (if (= interval-size-simple 8)
                                     15 (+ interval-size-simple 7))))
                  (correct-interval (om::string+ correct-quality correct-size))
                  )
             (cdr (assoc correct-interval *compound-interval-name* :test #'equal))
             )))

; (interval-identify "C4" "F#4") => "Augmented fourth"
; (interval-identify "C4" "Bb5") => "Minor fourteenth"




(om::defmethod! simple-interval-identify-2 (root upper)
         :icon 40031
         :initvals '("C4" "G4")
         :doc "Identify the size and quality of simple intervals according to the input root and upper note."
         (let* (
                (octave-scale (om::flat (om::repeat-n '("C" "D" "E" "F" "G" "A" "B") 4)))
                (upper-position (1+ (position (car (string-separator upper)) 
                                         (subseq octave-scale (position (car (string-separator root)) octave-scale :test #'equal) 
                                                 (length octave-scale)) :test #'equal)))                
                (semitones (/ (- (pitch->midic (pitch-info upper)) (pitch->midic (pitch-info root))) 100))
                (interval-size-simple (if (and (= upper-position 1) (> semitones 1))
                                          8 upper-position))
                (possibilities (car-pro (find-key-pairs-last semitones *number-of-semitones-in-interval*)))
                (quality-searcher (lambda (lists)
                                    (loop for a in lists
                                            if (string= (cadr (string-separator a)) (write-to-string interval-size-simple))
                                            do (return a))))
                (correct-quality (funcall quality-searcher possibilities))
                )
           correct-quality))

; (simple-interval-identify-2 "C4" "Gb4") => "d5"
; (simple-interval-identify-2 "C4" "Bb4") => "m7"





(om::defmethod! interval-identify-2 (root upper)
         :icon 40031
         :initvals '("C4" "C6")
         :doc "Identify the size and quality of intervals according to the input root and upper note."
         (if (stringp (simple-interval-identify-2 root upper))
             (simple-interval-identify-2 root upper)
           (let* (
                  (octave-scale (om::flat (om::repeat-n '("C" "D" "E" "F" "G" "A" "B") 4))) 
                  (upper-note (car (string-separator upper)))
                  (root-note (car (string-separator root)))                                   
                  (semitones (/ (- (pitch->midic (pitch-info upper)) (pitch->midic (pitch-info root))) 100))
                  (upper-position (1+ (position upper-note 
                                                (subseq octave-scale (position root-note octave-scale :test #'equal) 
                                                        (length octave-scale)) :test #'equal)))
                  (interval-size-simple (if (= upper-position 1)
                                            8 upper-position))
                  (simple-possibilities (car-pro (find-key-pairs-last (- semitones 12) *number-of-semitones-in-interval*)))
                  (quality-searcher (lambda (lists)
                                      (loop for a in lists
                                            if (string= (cadr (string-separator a)) (write-to-string interval-size-simple))
                                              do (return a))))
                  (correct-quality (car (string-separator (funcall quality-searcher simple-possibilities))))
                  (correct-size (write-to-string
                                 (if (= interval-size-simple 8)
                                     15 (+ interval-size-simple 7))))
                  (correct-interval (om::string+ correct-quality correct-size))
                  )
             (if (numberp (string-to-number correct-interval)) nil correct-interval)
             )))

; (interval-identify-2 "C4" "F#4") => "a4"
; (interval-identify-2 "C4" "Bb5") => "m14"






#|---------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-------------------------------3. tonality system----------------------------------
-----------------------------------------------------------------------------------
---------------------------------------------------------------------------------|#



;;--------------Define a data type named key-list-pair---------------

(defstruct (key-list-pair (:constructor make-pair (key values)))
  key
  values)


;;------------this is all global variable (key-list-pair)------------


;;This data type library is built with the make-pair function to store scale information
(defparameter *natural-major-scale-pairs*
          (list (make-pair "Cb" '("Cb4" "Db4" "Eb4" "Fb4" "Gb4" "Ab4" "Bb4" "Cb5"))
                (make-pair "C" '("C4" "D4" "E4" "F4" "G4" "A4" "B4" "C5"))
                (make-pair "C#" '("C#4" "D#4" "E#4" "F#4" "G#4" "A#4" "B#4" "C#5"))
                (make-pair "Db" '("Db4" "Eb4" "F4" "Gb4" "Ab4" "Bb4" "C5" "Db5"))
                (make-pair "D" '("D4" "E4" "F#4" "G4" "A4" "B4" "C#5" "D5")) 
                (make-pair "Eb" '("Eb4" "F4" "G4" "Ab4" "Bb4" "C5" "D5" "Eb5"))
                (make-pair "E" '("E4" "F#4" "G#4" "A4" "B4" "C#5" "D#5" "E5"))
                (make-pair "F" '("F4" "G4" "A4" "Bb4" "C5" "D5" "E5" "F5"))
                (make-pair "F#" '("F#4" "G#4" "A#4" "B4" "C#5" "D#5" "E#5" "F#5"))
                (make-pair "Gb" '("Gb3" "Ab3" "Bb3" "Cb4" "Db4" "Eb4" "F4" "Gb4"))
                (make-pair "G" '("G3" "A3" "B3" "C4" "D4" "E4" "F#4" "G4"))
                (make-pair "Ab" '("Ab3" "Bb3" "C4" "Db4" "Eb4" "F4" "G4" "Ab4"))
                (make-pair "A" '("A3" "B3" "C#4" "D4" "E4" "F#4" "G#4" "A4"))
                (make-pair "Bb" '("Bb3" "C4" "D4" "Eb4" "F4" "G4" "A4" "Bb4"))
                (make-pair "B" '("B3" "C#4" "D#4" "E4" "F#4" "G#4" "A#4" "B4"))
                ))

(defparameter *natural-minor-scale-pairs*
          (list (make-pair "C" '("C4" "D4" "Eb4" "F4" "G4" "Ab4" "Bb4" "C5"))
                (make-pair "C#" '("C#4" "D#4" "E4" "F#4" "G#4" "A4" "B4" "C#5"))
                (make-pair "D" '("D4" "E4" "F4" "G4" "A4" "Bb4" "C5" "D5"))
                (make-pair "D#" '("D#4" "E#4" "F#4" "G#4" "A#4" "B4" "C#5" "D#5"))
                (make-pair "Eb" '("Eb4" "F4" "Gb4" "Ab4" "Bb4" "Cb5" "Db5" "Eb5"))
                (make-pair "E" '("E4" "F#4" "G4" "A4" "B4" "C5" "D5" "E5"))
                (make-pair "F" '("F4" "G4" "Ab4" "Bb4" "C5" "Db5" "Eb5" "F5"))
                (make-pair "F#" '("F#4" "G#4" "A4" "B4" "C#5" "D5" "E5" "F#5"))
                (make-pair "G" '("G3" "A3" "Bb3" "C4" "D4" "Eb4" "F4" "G4"))
                (make-pair "G#" '("G#3" "A#3" "B3" "C#4" "D#4" "E4" "F#4" "G#4"))
                (make-pair "Ab" '("Ab3" "Bb3" "Cb4" "Db4" "Eb4" "Fb4" "Gb4" "Ab4"))
                (make-pair "A" '("A3" "B3" "C4" "D4" "E4" "F4" "G4" "A4"))
                (make-pair "A#" '("A#3" "B#3" "C#4" "D#4" "E#4" "F#4" "G#4" "A#4"))
                (make-pair "Bb" '("Bb3" "C4" "Db4" "Eb4" "F4" "Gb4" "Ab4" "Bb4"))
                (make-pair "B" '("B3" "C#4" "D4" "E4" "F#4" "G4" "A4" "B4"))                                            
                ))

(defparameter *harmonic-major-scale-pairs*
          (list (make-pair "Cb" '("Cb4" "Db4" "Eb4" "Fb4" "Gb4" "Abb4" "Bb4" "Cb5"))
                (make-pair "C" '("C4" "D4" "E4" "F4" "G4" "Ab4" "B4" "C5"))
                (make-pair "C#" '("C#4" "D#4" "E#4" "F#4" "G#4" "A=4" "B#4" "C#5"))
                (make-pair "Db" '("Db4" "Eb4" "F4" "Gb4" "Ab4" "Bbb4" "C5" "Db5"))
                (make-pair "D" '("D4" "E4" "F#4" "G4" "A4" "Bb4" "C#5" "D5")) 
                (make-pair "Eb" '("Eb4" "F4" "G4" "Ab4" "Bb4" "Cb5" "D5" "Eb5"))
                (make-pair "E" '("E4" "F#4" "G#4" "A4" "B4" "C=5" "D#5" "E5"))
                (make-pair "F" '("F4" "G4" "A4" "Bb4" "C5" "Db5" "E5" "F5"))
                (make-pair "F#" '("F#4" "G#4" "A#4" "B4" "C#5" "D=5" "E#5" "F#5"))
                (make-pair "Gb" '("Gb3" "Ab3" "Bb3" "Cb4" "Db4" "Ebb4" "F4" "Gb4"))
                (make-pair "G" '("G3" "A3" "B3" "C4" "D4" "Eb4" "F#4" "G4"))
                (make-pair "Ab" '("Ab3" "Bb3" "C4" "Db4" "Eb4" "Fb4" "G4" "Ab4"))
                (make-pair "A" '("A3" "B3" "C#4" "D4" "E4" "F=4" "G#4" "A4"))
                (make-pair "Bb" '("Bb3" "C4" "D4" "Eb4" "F4" "Gb4" "A4" "Bb4"))
                (make-pair "B" '("B3" "C#4" "D#4" "E4" "F#4" "G=4" "A#4" "B4"))
                ))    

(defparameter *harmonic-minor-scale-pairs*
          (list (make-pair "C" '("C4" "D4" "Eb4" "F4" "G4" "Ab4" "B=4" "C5"))
                (make-pair "C#" '("C#4" "D#4" "E4" "F#4" "G#4" "A4" "B#4" "C#5"))
                (make-pair "D" '("D4" "E4" "F4" "G4" "A4" "Bb4" "C#5" "D5"))
                (make-pair "D#" '("D#4" "E#4" "F#4" "G#4" "A#4" "B4" "Cx5" "D#5"))
                (make-pair "Eb" '("Eb4" "F4" "Gb4" "Ab4" "Bb4" "Cb5" "D=5" "Eb5"))
                (make-pair "E" '("E4" "F#4" "G4" "A4" "B4" "C5" "D#5" "E5"))
                (make-pair "F" '("F4" "G4" "Ab4" "Bb4" "C5" "Db5" "E=5" "F5"))
                (make-pair "F#" '("F#4" "G#4" "A4" "B4" "C#5" "D5" "E#5" "F#5"))
                (make-pair "G" '("G3" "A3" "Bb3" "C4" "D4" "Eb4" "F#4" "G4"))
                (make-pair "G#" '("G#3" "A#3" "B3" "C#4" "D#4" "E4" "Fx4" "G#4"))
                (make-pair "Ab" '("Ab3" "Bb3" "Cb4" "Db4" "Eb4" "Fb4" "G=4" "Ab4"))
                (make-pair "A" '("A3" "B3" "C4" "D4" "E4" "F4" "G#4" "A4"))
                (make-pair "A#" '("A#3" "B#3" "C#4" "D#4" "E#4" "F#4" "Gx4" "A#4"))
                (make-pair "Bb" '("Bb3" "C4" "Db4" "Eb4" "F4" "Gb4" "A=4" "Bb4"))
                (make-pair "B" '("B3" "C#4" "D4" "E4" "F#4" "G4" "A#4" "B4"))                                            
                ))    
     
(defparameter *melodic-major-scale-pairs*
          (list (make-pair "Cb" '("Cb4" "Db4" "Eb4" "Fb4" "Gb4" "Ab4" "Bb4" "Cb5" "Bbb4" "Abb4" "Gb4" "Fb4" "Eb4" "Db4" "Cb4"))
                (make-pair "C" '("C4" "D4" "E4" "F4" "G4" "A4" "B4" "C5" "Bb4" "Ab4" "G4" "F4" "E4" "D4" "C4"))
                (make-pair "C#" '("C#4" "D#4" "E#4" "F#4" "G#4" "A#4" "B#4" "C#5" "B=4" "A=4" "G#4" "F#4" "E#4" "D#4" "C#4"))
                (make-pair "Db" '("Db4" "Eb4" "F4" "Gb4" "Ab4" "Bb4" "C5" "Db5" "Cb5" "Bbb4" "Ab4" "Gb4" "F4" "Eb4" "Db4"))
                (make-pair "D" '("D4" "E4" "F#4" "G4" "A4" "B4" "C#5" "D5" "C=5" "Bb4" "A4" "G4" "F#4" "E4" "D4")) 
                (make-pair "Eb" '("Eb4" "F4" "G4" "Ab4" "Bb4" "C5" "D5" "Eb5" "Db5" "Cb5" "Bb4" "Ab4" "G4" "F4" "Eb4"))
                (make-pair "E" '("E4" "F#4" "G#4" "A4" "B4" "C#5" "D#5" "E5" "D=5" "C=5" "B4" "A4" "G#4" "F#4" "E4"))
                (make-pair "F" '("F4" "G4" "A4" "Bb4" "C5" "D5" "E5" "F5" "Eb5" "Db5" "C5" "Bb4" "A4" "G4" "F4"))
                (make-pair "F#" '("F#4" "G#4" "A#4" "B4" "C#5" "D#5" "E#5" "F#5" "E=5" "D=5" "C#5" "B4" "A#4" "G#4" "F#4"))
                (make-pair "Gb" '("Gb3" "Ab3" "Bb3" "Cb4" "Db4" "Eb4" "F4" "Gb4" "Fb4" "Ebb4" "Db4" "Cb4" "Bb3" "Ab3" "Gb3"))
                (make-pair "G" '("G3" "A3" "B3" "C4" "D4" "E4" "F#4" "G4" "F=4" "Eb4" "D4" "C4" "B3" "A3" "G3"))
                (make-pair "Ab" '("Ab3" "Bb3" "C4" "Db4" "Eb4" "F4" "G4" "Ab4" "Gb4" "Fb4" "Eb4" "Db4" "C4" "Bb3" "Ab3"))
                (make-pair "A" '("A3" "B3" "C#4" "D4" "E4" "F#4" "G#4" "A4" "G=4" "F=4" "E4" "D4" "C#4" "B3" "A3"))
                (make-pair "Bb" '("Bb3" "C4" "D4" "Eb4" "F4" "G4" "A4" "Bb4" "Ab4" "Gb4" "F4" "Eb4" "D4" "C4" "Bb3"))
                (make-pair "B" '("B3" "C#4" "D#4" "E4" "F#4" "G#4" "A#4" "B4" "A=4" "G=4" "F#4" "E4" "D#4" "C#4" "B3"))
                ))

(defparameter *melodic-minor-scale-pairs*
          (list (make-pair "C" '("C4" "D4" "Eb4" "F4" "G4" "A=4" "B=4" "C5" "Bb4" "Ab4" "G4" "F4" "Eb4" "D4" "C4"))
                (make-pair "C#" '("C#4" "D#4" "E4" "F#4" "G#4" "A#4" "B#4" "C#5" "B=4" "A=4" "G#4" "F#4" "E4" "D#4" "C#4"))
                (make-pair "D" '("D4" "E4" "F4" "G4" "A4" "B=4" "C#5" "D5" "C=5" "Bb4" "A4" "G4" "F4" "E4" "D4"))
                (make-pair "D#" '("D#4" "E#4" "F#4" "G#4" "A#4" "B#4" "Cx5" "D#5" "C#5" "B=4" "A#4" "G#4" "F#4" "E#4" "D#4"))
                (make-pair "Eb" '("Eb4" "F4" "Gb4" "Ab4" "Bb4" "C=5" "D=5" "Eb5" "Db5" "Cb5" "Bb4" "Ab4" "Gb4" "F4" "Eb4"))
                (make-pair "E" '("E4" "F#4" "G4" "A4" "B4" "C#5" "D#5" "E5" "D=5" "C=5" "B4" "A4" "G4" "F#4" "E4"))
                (make-pair "F" '("F4" "G4" "Ab4" "Bb4" "C5" "D=5" "E=5" "F5" "Eb5" "Db5" "C5" "Bb4" "Ab4" "G4" "F4"))
                (make-pair "F#" '("F#4" "G#4" "A4" "B4" "C#5" "D#5" "E#5" "F#5" "E=5" "D=5" "C#5" "B4" "A4" "G#4" "F#4"))
                (make-pair "G" '("G3" "A3" "Bb3" "C4" "D4" "E=4" "F#4" "G4" "F=4" "Eb4" "D4" "C4" "Bb3" "A3" "G3"))
                (make-pair "G#" '("G#3" "A#3" "B3" "C#4" "D#4" "E#4" "Fx4" "G#4" "F#4" "E=4" "D#4" "C#4" "B3" "A#3" "G#3"))
                (make-pair "Ab" '("Ab3" "Bb3" "Cb4" "Db4" "Eb4" "F=4" "G=4" "Ab4" "Gb4" "Fb4" "Eb4" "Db4" "Cb4" "Bb3" "Ab3"))
                (make-pair "A" '("A3" "B3" "C4" "D4" "E4" "F#4" "G#4" "A4" "G=4" "F=4" "E4" "D4" "C4" "B3" "A3"))
                (make-pair "A#" '("A#3" "B#3" "C#4" "D#4" "E#4" "Fx4" "Gx4" "A#4" "G#4" "F#4" "E#4" "D#4" "C#4" "B#3" "A#3"))
                (make-pair "Bb" '("Bb3" "C4" "Db4" "Eb4" "F4" "G=4" "A=4" "Bb4" "Ab4" "Gb4" "F4" "Eb4" "Db4" "C4" "Bb3"))
                (make-pair "B" '("B3" "C#4" "D4" "E4" "F#4" "G#4" "A#4" "B4" "A=4" "G=4" "F#4" "E4" "D4" "C#4" "B3"))                                            
                ))



;;------------this is all global variable (key-list-pair)------------




;;------this is auxiliary functions for key-list-pair-----
                  
(defun find-pair (key key-list-pair)
            "specific find function to find the key-list-pair data type"
            (find key key-list-pair :key #'key-list-pair-key :test #'equal))

;;(key-list-pair-values (find-pair "B" *natural-major-scale-pairs*))
;;(key-list-pair-key (find-pair "B" *natural-major-scale-pairs*))




;;--------------------------------------------------------
;;---------------------scale production-------------------
;;--------------------------------------------------------


;;-------1. natural major scale-------

(om::defmethod! natural-major-scale (tonic)
         :icon 40051
         :initvals '("C")
         :menuins '(
                    (0 (("Cb" "Cb") ("C" "C") ("C#" "C#") ("Db" "Db") ("D" "D") 
                        ("Eb" "Eb")  ("E" "E") ("F" "F") ("F#" "F#") ("Gb" "Gb")
                        ("G" "G") ("Ab" "Ab") ("A" "A") ("Bb" "Bb") ("B" "B")))
                    )
         :doc "generate a natural major scale from the tonic"         
         (cond ((string-equal tonic "Cb") (key-list-pair-values (find-pair "Cb" *natural-major-scale-pairs*)))
               ((string-equal tonic "C") (key-list-pair-values (find-pair "C" *natural-major-scale-pairs*)))
               ((string-equal tonic "C#") (key-list-pair-values (find-pair "C#" *natural-major-scale-pairs*)))
               ((string-equal tonic "Db") (key-list-pair-values (find-pair "Db" *natural-major-scale-pairs*)))
               ((string-equal tonic "D") (key-list-pair-values (find-pair "D" *natural-major-scale-pairs*)))
               ((string-equal tonic "Eb") (key-list-pair-values (find-pair "Eb" *natural-major-scale-pairs*)))
               ((string-equal tonic "E") (key-list-pair-values (find-pair "E" *natural-major-scale-pairs*)))
               ((string-equal tonic "F") (key-list-pair-values (find-pair "F" *natural-major-scale-pairs*)))
               ((string-equal tonic "F#") (key-list-pair-values (find-pair "F#" *natural-major-scale-pairs*)))
               ((string-equal tonic "Gb") (key-list-pair-values (find-pair "Gb" *natural-major-scale-pairs*)))
               ((string-equal tonic "G") (key-list-pair-values (find-pair "G" *natural-major-scale-pairs*)))
               ((string-equal tonic "Ab") (key-list-pair-values (find-pair "Ab" *natural-major-scale-pairs*)))
               ((string-equal tonic "A") (key-list-pair-values (find-pair "A" *natural-major-scale-pairs*)))
               ((string-equal tonic "Bb") (key-list-pair-values (find-pair "Bb" *natural-major-scale-pairs*)))
               ((string-equal tonic "B") (key-list-pair-values (find-pair "B" *natural-major-scale-pairs*)))))


;;-------2. natural minor scale-------

(om::defmethod! natural-minor-scale (tonic)
         :icon 40051
         :initvals '("A")
         :menuins '(
                    (0 (("C" "C") ("C#" "C#") ("D" "D") ("D#" "D#") ("Eb" "Eb") 
                        ("E" "E")  ("F" "F") ("F#" "F#") ("G" "G") ("G#" "G#")
                        ("Ab" "Ab") ("A" "A") ("A#" "A#") ("Bb" "Bb") ("B" "B")))
                    )
         :doc "generate a natural minor scale from the tonic"         
         (cond ((string-equal tonic "C") (key-list-pair-values (find-pair "C" *natural-minor-scale-pairs*)))
               ((string-equal tonic "C#") (key-list-pair-values (find-pair "C#" *natural-minor-scale-pairs*)))
               ((string-equal tonic "D") (key-list-pair-values (find-pair "D" *natural-minor-scale-pairs*)))
               ((string-equal tonic "D#") (key-list-pair-values (find-pair "D#" *natural-minor-scale-pairs*)))
               ((string-equal tonic "Eb") (key-list-pair-values (find-pair "Eb" *natural-minor-scale-pairs*)))
               ((string-equal tonic "E") (key-list-pair-values (find-pair "E" *natural-minor-scale-pairs*)))
               ((string-equal tonic "F") (key-list-pair-values (find-pair "F" *natural-minor-scale-pairs*)))
               ((string-equal tonic "F#") (key-list-pair-values (find-pair "F#" *natural-minor-scale-pairs*)))
               ((string-equal tonic "G") (key-list-pair-values (find-pair "G" *natural-minor-scale-pairs*)))
               ((string-equal tonic "G#") (key-list-pair-values (find-pair "G#" *natural-minor-scale-pairs*)))
               ((string-equal tonic "Ab") (key-list-pair-values (find-pair "Ab" *natural-minor-scale-pairs*)))
               ((string-equal tonic "A") (key-list-pair-values (find-pair "A" *natural-minor-scale-pairs*)))
               ((string-equal tonic "A#") (key-list-pair-values (find-pair "A#" *natural-minor-scale-pairs*)))
               ((string-equal tonic "Bb") (key-list-pair-values (find-pair "Bb" *natural-minor-scale-pairs*)))
               ((string-equal tonic "B") (key-list-pair-values (find-pair "B" *natural-minor-scale-pairs*)))))


;;-------3. harmonic major scale-------

(om::defmethod! harmonic-major-scale (tonic)
         :icon 40051
         :initvals '("C")
         :menuins '(
                    (0 (("Cb" "Cb") ("C" "C") ("C#" "C#") ("Db" "Db") ("D" "D") 
                        ("Eb" "Eb")  ("E" "E") ("F" "F") ("F#" "F#") ("Gb" "Gb")
                        ("G" "G") ("Ab" "Ab") ("A" "A") ("Bb" "Bb") ("B" "B")))
                    )
         :doc "generate a harmonic major scale from the tonic"         
         (cond ((string-equal tonic "Cb") (key-list-pair-values (find-pair "Cb" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "C") (key-list-pair-values (find-pair "C" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "C#") (key-list-pair-values (find-pair "C#" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "Db") (key-list-pair-values (find-pair "Db" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "D") (key-list-pair-values (find-pair "D" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "Eb") (key-list-pair-values (find-pair "Eb" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "E") (key-list-pair-values (find-pair "E" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "F") (key-list-pair-values (find-pair "F" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "F#") (key-list-pair-values (find-pair "F#" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "Gb") (key-list-pair-values (find-pair "Gb" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "G") (key-list-pair-values (find-pair "G" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "Ab") (key-list-pair-values (find-pair "Ab" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "A") (key-list-pair-values (find-pair "A" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "Bb") (key-list-pair-values (find-pair "Bb" *harmonic-major-scale-pairs*)))
               ((string-equal tonic "B") (key-list-pair-values (find-pair "B" *harmonic-major-scale-pairs*)))))


;;-------4. harmonic minor scale-------

(om::defmethod! harmonic-minor-scale (tonic)
         :icon 40051
         :initvals '("A")
         :menuins '(
                    (0 (("C" "C") ("C#" "C#") ("D" "D") ("D#" "D#") ("Eb" "Eb") 
                        ("E" "E")  ("F" "F") ("F#" "F#") ("G" "G") ("G#" "G#")
                        ("Ab" "Ab") ("A" "A") ("A#" "A#") ("Bb" "Bb") ("B" "B")))
                    )
         :doc "generate a harmonic minor scale from the tonic"         
         (cond ((string-equal tonic "C") (key-list-pair-values (find-pair "C" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "C#") (key-list-pair-values (find-pair "C#" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "D") (key-list-pair-values (find-pair "D" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "D#") (key-list-pair-values (find-pair "D#" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "Eb") (key-list-pair-values (find-pair "Eb" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "E") (key-list-pair-values (find-pair "E" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "F") (key-list-pair-values (find-pair "F" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "F#") (key-list-pair-values (find-pair "F#" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "G") (key-list-pair-values (find-pair "G" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "G#") (key-list-pair-values (find-pair "G#" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "Ab") (key-list-pair-values (find-pair "Ab" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "A") (key-list-pair-values (find-pair "A" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "A#") (key-list-pair-values (find-pair "A#" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "Bb") (key-list-pair-values (find-pair "Bb" *harmonic-minor-scale-pairs*)))
               ((string-equal tonic "B") (key-list-pair-values (find-pair "B" *harmonic-minor-scale-pairs*)))))


;;-------5. melodic major scale-------

(om::defmethod! melodic-major-scale (tonic)
         :icon 40051
         :initvals '("C")
         :menuins '(
                    (0 (("Cb" "Cb") ("C" "C") ("C#" "C#") ("Db" "Db") ("D" "D") 
                        ("Eb" "Eb")  ("E" "E") ("F" "F") ("F#" "F#") ("Gb" "Gb")
                        ("G" "G") ("Ab" "Ab") ("A" "A") ("Bb" "Bb") ("B" "B")))
                    )
         :doc "generate a melodic major scale from the tonic"         
         (cond ((string-equal tonic "Cb") (key-list-pair-values (find-pair "Cb" *melodic-major-scale-pairs*)))
               ((string-equal tonic "C") (key-list-pair-values (find-pair "C" *melodic-major-scale-pairs*)))
               ((string-equal tonic "C#") (key-list-pair-values (find-pair "C#" *melodic-major-scale-pairs*)))
               ((string-equal tonic "Db") (key-list-pair-values (find-pair "Db" *melodic-major-scale-pairs*)))
               ((string-equal tonic "D") (key-list-pair-values (find-pair "D" *melodic-major-scale-pairs*)))
               ((string-equal tonic "Eb") (key-list-pair-values (find-pair "Eb" *melodic-major-scale-pairs*)))
               ((string-equal tonic "E") (key-list-pair-values (find-pair "E" *melodic-major-scale-pairs*)))
               ((string-equal tonic "F") (key-list-pair-values (find-pair "F" *melodic-major-scale-pairs*)))
               ((string-equal tonic "F#") (key-list-pair-values (find-pair "F#" *melodic-major-scale-pairs*)))
               ((string-equal tonic "Gb") (key-list-pair-values (find-pair "Gb" *melodic-major-scale-pairs*)))
               ((string-equal tonic "G") (key-list-pair-values (find-pair "G" *melodic-major-scale-pairs*)))
               ((string-equal tonic "Ab") (key-list-pair-values (find-pair "Ab" *melodic-major-scale-pairs*)))
               ((string-equal tonic "A") (key-list-pair-values (find-pair "A" *melodic-major-scale-pairs*)))
               ((string-equal tonic "Bb") (key-list-pair-values (find-pair "Bb" *melodic-major-scale-pairs*)))
               ((string-equal tonic "B") (key-list-pair-values (find-pair "B" *melodic-major-scale-pairs*)))))


;;-------6. melodic minor scale-------

(om::defmethod! melodic-minor-scale (tonic)
         :icon 40051
         :initvals '("A")
         :menuins '(
                    (0 (("C" "C") ("C#" "C#") ("D" "D") ("D#" "D#") ("Eb" "Eb") 
                        ("E" "E")  ("F" "F") ("F#" "F#") ("G" "G") ("G#" "G#")
                        ("Ab" "Ab") ("A" "A") ("A#" "A#") ("Bb" "Bb") ("B" "B")))
                    )
         :doc "generate a melodic minor scale from the tonic"         
         (cond ((string-equal tonic "C") (key-list-pair-values (find-pair "C" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "C#") (key-list-pair-values (find-pair "C#" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "D") (key-list-pair-values (find-pair "D" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "D#") (key-list-pair-values (find-pair "D#" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "Eb") (key-list-pair-values (find-pair "Eb" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "E") (key-list-pair-values (find-pair "E" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "F") (key-list-pair-values (find-pair "F" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "F#") (key-list-pair-values (find-pair "F#" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "G") (key-list-pair-values (find-pair "G" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "G#") (key-list-pair-values (find-pair "G#" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "Ab") (key-list-pair-values (find-pair "Ab" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "A") (key-list-pair-values (find-pair "A" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "A#") (key-list-pair-values (find-pair "A#" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "Bb") (key-list-pair-values (find-pair "Bb" *melodic-minor-scale-pairs*)))
               ((string-equal tonic "B") (key-list-pair-values (find-pair "B" *melodic-minor-scale-pairs*)))))


;;-------7. major scale production-------

(om::defmethod! major-scale-production (tonic pattern)
         :icon 40051
         :initvals '("C" "Natural")
         :menuins '(
                    (0 (("Cb" "Cb") ("C" "C") ("C#" "C#") ("Db" "Db") ("D" "D") 
                        ("Eb" "Eb")  ("E" "E") ("F" "F") ("F#" "F#") ("Gb" "Gb")
                        ("G" "G") ("Ab" "Ab") ("A" "A") ("Bb" "Bb") ("B" "B")))
                    (1 (("Natural" "Natural") ("Harmonic" "Harmonic") ("Melodic" "Melodic"))) 
                    )
         :doc "generates a major scale based on the specified TONIC and PATTERN"        
         (cond ((string-equal pattern "Natural") (natural-major-scale tonic))
               ((string-equal pattern "Harmonic") (harmonic-major-scale tonic))
               ((string-equal pattern "Melodic") (melodic-major-scale tonic))))
               

;;-------8. minor scale production-------

(om::defmethod! minor-scale-production (tonic pattern)
         :icon 40051
         :initvals '("A" "Natural")
         :menuins '(
                    (0 (("C" "C") ("C#" "C#") ("D" "D") ("D#" "D#") ("Eb" "Eb") 
                        ("E" "E")  ("F" "F") ("F#" "F#") ("G" "G") ("G#" "G#")
                        ("Ab" "Ab") ("A" "A") ("A#" "A#") ("Bb" "Bb") ("B" "B")))
                    (1 (("Natural" "Natural") ("Harmonic" "Harmonic") ("Melodic" "Melodic")))
                    )
         :doc "generates a minor scale based on the specified TONIC and PATTERN"         
         (cond ((string-equal pattern "Natural") (natural-minor-scale tonic))
               ((string-equal pattern "Harmonic") (harmonic-minor-scale tonic))
               ((string-equal pattern "Melodic") (melodic-minor-scale tonic))))



;;-----9. Mode of limited transposition-----

(om::defmethod! mode-1 (tonic)
         :icon 40051
         :initvals '("C")
         :menuins '(
                    (0 (("C" "C") ("C#" "C#")))                    
                    )
         :doc "generate a whole-tone scale (mode-1)"
            (let* (
                   (whole-tone-scale-pairs (list 
                                            (make-pair "C" '("C4" "D4" "E4" "F#4" "G#4" "A#4" "C5"))
                                            (make-pair "C#" '("C#4" "D#4" "F4" "G4" "A4" "B4" "C#5")))))
              (cond ((string-equal tonic "C") (key-list-pair-values (find-pair "C" whole-tone-scale-pairs)))
                    ((string-equal tonic "C#") (key-list-pair-values (find-pair "C#" whole-tone-scale-pairs)))
                    )))


(om::defmethod! mode-2 (tonic)
         :icon 40051
         :initvals '("C")
         :menuins '(
                    (0 (("C" "C") ("C#" "C#") ("D" "D")))                    
                    )
         :doc "generate a octatonic scale (mode-2)"
            (let* (
                   (octatonic-scale-pairs (list 
                                           (make-pair "C" '("C4" "C#4" "D#4" "E4" "F#4" "G4" "A4" "Bb4" "C5"))
                                           (make-pair "C#" '("C#4" "D4" "E4" "F4" "G4" "G#4" "A#4" "B4" "C#5"))
                                           (make-pair "D" '("D4" "Eb4" "F4" "F#4" "G#4" "A4" "B4" "C5" "D5")))))
              (cond ((string-equal tonic "C") (key-list-pair-values (find-pair "C" octatonic-scale-pairs)))
                    ((string-equal tonic "C#") (key-list-pair-values (find-pair "C#" octatonic-scale-pairs)))
                    ((string-equal tonic "D") (key-list-pair-values (find-pair "D" octatonic-scale-pairs)))
                    )))



;;---------------Rest of the other functions--------------
       
(defun scale->midic (scale)               
            "convert scales to midicent"
            (loop for a in scale
                  collect (pitch->midic (pitch-info a)))) 
;(scale->midic (major-scale-production "C" "Natural")) => (6000 6200 6400 6500 6700 6900 7100 7200)




;'(("bb" . 12) ("b" . 38) (nil . 0) ("=" . 50) ("#" . 62) ("x" . 88)) 
(defun scale->display (scale)
            "converts the scale to the abnormal midicent required for display"
            (loop for a in scale
                  collect (pitch->display (pitch-info a))))
;(pitch->display (pitch-info "Cb4")) => 6038




;; 0 = natural minor, 1 = natural major
;; 2 = harmonic minor, 3 = harmonic major

(om::defmethod! scale-production-midic (tonic mode octave-number)
         :icon 40041
         :initvals '("C4" 1 1)
         :menuins '(
                    (1 (("Natural Minor" 0) ("Natural Major" 1) ("Harmonic Minor" 2) ("Harmonic Major" 3)))                    
                    )
         :doc "generates a scale based on the specified TONIC, MODE and OCTAVES, which used to calculate"        
         (let* (
                (tonic-midic (pitch->midic (pitch-info tonic)))
                (scale-midic-1octave (cond ((= mode 0) '(200 100 200 200 100 200 200))
                                           ((= mode 1) '(200 200 100 200 200 200 100))
                                           ((= mode 2) '(200 100 200 200 100 300 100))
                                           ((= mode 3) '(200 200 100 200 100 300 100))))
                (scale-midic (om::flat (om::repeat-n scale-midic-1octave octave-number)))
                (octave (om::dx->x tonic-midic scale-midic))
                )
           octave))
           
; (scale-production-midic "C4" 2 2) => (6000 6200 6300 6500 6700 6800 7100 7200 7400 7500 7700 7900 8000 8300 8400)
; (scale-production-midic "C4" 1 1) => (6000 6200 6400 6500 6700 6900 7100 7200)

;;---------------------------------------------------------




;;--------------------------------------------------------
;;---------------------Tonality judgment------------------
;;--------------------------------------------------------


(defparameter *major-chords*
          (list (make-pair "Cb" '("Cb" "Eb" "Gb"))
                (make-pair "C" '("C" "E" "G"))
                (make-pair "C#" '("C#" "E#" "G#"))
                (make-pair "Db" '("Db" "F" "Ab"))
                (make-pair "D" '("D" "F#" "A"))
                (make-pair "Eb" '("Eb" "G" "Bb"))
                (make-pair "E" '("E" "G#" "B"))
                (make-pair "F" '("F" "A" "C"))
                (make-pair "F#" '("F#" "A#" "C#"))
                (make-pair "Gb" '("Gb" "Bb" "Db"))
                (make-pair "G" '("G" "B" "D"))
                (make-pair "Ab" '("Ab" "C" "Eb"))
                (make-pair "A" '("A" "C#" "E"))
                (make-pair "Bb" '("Bb" "D" "F"))
                (make-pair "B" '("B" "D#" "F#"))))
               
(defparameter *minor-chords*
          (list (make-pair "C" '("C" "Eb" "G"))
                (make-pair "C#" '("C#" "E" "G#"))
                (make-pair "D" '("D" "F" "A"))
                (make-pair "D#" '("D#" "F#" "A#"))
                (make-pair "Eb" '("Eb" "Gb" "Bb"))
                (make-pair "E" '("E" "G" "B"))
                (make-pair "F" '("F" "Ab" "C"))
                (make-pair "F#" '("F#" "A" "C#"))
                (make-pair "G" '("G" "Bb" "D"))
                (make-pair "G#" '("G#" "B" "D#"))
                (make-pair "Ab" '("Ab" "Cb" "Eb"))
                (make-pair "A" '("A" "C" "E"))
                (make-pair "A#" '("A#" "C#" "E#"))
                (make-pair "Bb" '("Bb" "Db" "F"))
                (make-pair "B" '("B" "D" "F#"))))



(defparameter *natural-major-scale-pairs-1*
          (list (make-pair "Cb" '("Cb" "Db" "Eb" "Fb" "Gb" "Ab" "Bb" "Cb"))
                (make-pair "C" '("C" "D" "E" "F" "G" "A" "B" "C"))
                (make-pair "C#" '("C#" "D#" "E#" "F#" "G#" "A#" "B#" "C#"))
                (make-pair "Db" '("Db" "Eb" "F" "Gb" "Ab" "Bb" "C" "Db"))
                (make-pair "D" '("D" "E" "F#" "G" "A" "B" "C#" "D")) 
                (make-pair "Eb" '("Eb" "F" "G" "Ab" "Bb" "C" "D" "Eb"))
                (make-pair "E" '("E" "F#" "G#" "A" "B" "C#" "D#" "E"))
                (make-pair "F" '("F" "G" "A" "Bb" "C" "D" "E" "F"))
                (make-pair "F#" '("F#" "G#" "A#" "B" "C#" "D#" "E#" "F#"))
                (make-pair "Gb" '("Gb" "Ab" "Bb" "Cb" "Db" "Eb" "F" "Gb"))
                (make-pair "G" '("G" "A" "B" "C" "D" "E" "F#" "G"))
                (make-pair "Ab" '("Ab" "Bb" "C" "Db" "Eb" "F" "G" "Ab"))
                (make-pair "A" '("A" "B" "C#" "D" "E" "F#" "G#" "A"))
                (make-pair "Bb" '("Bb" "C" "D" "Eb" "F" "G" "A" "Bb"))
                (make-pair "B" '("B" "C#" "D#" "E" "F#" "G#" "A#" "B"))
                ))

(defparameter *natural-minor-scale-pairs-1*
          (list (make-pair "C" '("C" "D" "Eb" "F" "G" "Ab" "Bb" "C"))
                (make-pair "C#" '("C#" "D#" "E" "F#" "G#" "A" "B" "C#"))
                (make-pair "D" '("D" "E" "F" "G" "A" "Bb" "C" "D"))
                (make-pair "D#" '("D#" "E#" "F#" "G#" "A#" "B" "C#" "D#"))
                (make-pair "Eb" '("Eb" "F" "Gb" "Ab" "Bb" "Cb" "Db" "Eb"))
                (make-pair "E" '("E" "F#" "G" "A" "B" "C" "D" "E"))
                (make-pair "F" '("F" "G" "Ab" "Bb" "C" "Db" "Eb" "F"))
                (make-pair "F#" '("F#" "G#" "A" "B" "C#" "D" "E" "F#"))
                (make-pair "G" '("G" "A" "Bb" "C" "D" "Eb" "F" "G"))
                (make-pair "G#" '("G#" "A#" "B" "C#" "D#" "E" "F#" "G#"))
                (make-pair "Ab" '("Ab" "Bb" "Cb" "Db" "Eb" "Fb" "Gb" "Ab"))
                (make-pair "A" '("A" "B" "C" "D" "E" "F" "G" "A"))
                (make-pair "A#" '("A#" "B#" "C#" "D#" "E#" "F#" "G#" "A#"))
                (make-pair "Bb" '("Bb" "C" "Db" "Eb" "F" "Gb" "Ab" "Bb"))
                (make-pair "B" '("B" "C#" "D" "E" "F#" "G" "A" "B"))                                            
                ))

(defparameter *harmonic-major-scale-pairs-1*
          (list (make-pair "Cb" '("Cb" "Db" "Eb" "Fb" "Gb" "Abb" "Bb" "Cb"))
                (make-pair "C" '("C" "D" "E" "F" "G" "Ab" "B" "C"))
                (make-pair "C#" '("C#" "D#" "E#" "F#" "G#" "A=" "B#" "C#"))
                (make-pair "Db" '("Db" "Eb" "F" "Gb" "Ab" "Bbb" "C" "Db"))
                (make-pair "D" '("D" "E" "F#" "G" "A" "Bb" "C#" "D")) 
                (make-pair "Eb" '("Eb" "F" "G" "Ab" "Bb" "Cb" "D" "Eb"))
                (make-pair "E" '("E" "F#" "G#" "A" "B" "C=" "D#" "E"))
                (make-pair "F" '("F" "G" "A" "Bb" "C" "Db" "E" "F"))
                (make-pair "F#" '("F#" "G#" "A#" "B" "C#" "D=" "E#" "F#"))
                (make-pair "Gb" '("Gb" "Ab" "Bb" "Cb" "Db" "Ebb" "F" "Gb"))
                (make-pair "G" '("G" "A" "B" "C" "D" "Eb" "F#" "G"))
                (make-pair "Ab" '("Ab" "Bb" "C" "Db" "Eb" "Fb" "G" "Ab"))
                (make-pair "A" '("A" "B" "C#" "D" "E" "F=" "G#" "A"))
                (make-pair "Bb" '("Bb" "C" "D" "Eb" "F" "Gb" "A" "Bb"))
                (make-pair "B" '("B" "C#" "D#" "E" "F#" "G=" "A#" "B"))
                ))    

(defparameter *harmonic-minor-scale-pairs-1*
          (list (make-pair "C" '("C" "D" "Eb" "F" "G" "Ab" "B=" "C"))
                (make-pair "C#" '("C#" "D#" "E" "F#" "G#" "A" "B#" "C#"))
                (make-pair "D" '("D" "E" "F" "G" "A" "Bb" "C#" "D"))
                (make-pair "D#" '("D#" "E#" "F#" "G#" "A#" "B" "Cx" "D#"))
                (make-pair "Eb" '("Eb" "F" "Gb" "Ab" "Bb" "Cb" "D=" "Eb"))
                (make-pair "E" '("E" "F#" "G" "A" "B" "C" "D#" "E"))
                (make-pair "F" '("F" "G" "Ab" "Bb" "C" "Db" "E=" "F"))
                (make-pair "F#" '("F#" "G#" "A" "B" "C#" "D" "E#" "F#"))
                (make-pair "G" '("G" "A" "Bb" "C" "D" "Eb" "F#" "G"))
                (make-pair "G#" '("G#" "A#" "B" "C#" "D#" "E" "Fx" "G#"))
                (make-pair "Ab" '("Ab" "Bb" "Cb" "Db" "Eb" "Fb" "G=" "Ab"))
                (make-pair "A" '("A" "B" "C" "D" "E" "F" "G#" "A"))
                (make-pair "A#" '("A#" "B#" "C#" "D#" "E#" "F#" "Gx" "A#"))
                (make-pair "Bb" '("Bb" "C" "Db" "Eb" "F" "Gb" "A=" "Bb"))
                (make-pair "B" '("B" "C#" "D" "E" "F#" "G" "A#" "B"))                                            
                ))      
;;-----------------------------------------------------------------------------               
                                                               

;;---------------this is auxiliary functions--------------

(defun take-off-octave-info (melody)
            "take off the octave information of each note in the melody"
            (loop for note in melody
                    collect (om::string+ (car (pitch-info note)) (cadr (pitch-info note)))))

; (take-off-octave-info '("Abb4" "Bbb3" "C#3" "Dx5")) => ("Abb" "Bbb" "C#" "Dx")



(defun note-frequencies (notes)
            "Calculate the frequency of each note in the melody."
            (let ((freq-table (make-hash-table :test 'equal)))
              (loop for note in notes
                    do (incf (gethash note freq-table 0)))
              freq-table))

; (note-frequencies '("Abb" "Bbb" "C#" "Dx" "Dx" "C#"))




(defun hash-table-keys (hash-table)
            "Return a list of all keys in the given HASH-TABLE."
            (let ((keys '()))
              (maphash (lambda (key value)
                         (push key keys))
                       hash-table)
              keys))

; (hash-table-keys (note-frequencies '("Abb" "Bbb" "C#" "Dx" "Dx" "C#"))) => ("C#" "Bbb" "Abb" "Dx")




(defun hash-table-values (hash-table)
            "Return a list of all values in the given HASH-TABLE."
            (let ((values '()))
              (maphash (lambda (key value)
                         (push value values))
                       hash-table)
              values))
; (hash-table-values (note-frequencies '("Abb" "Bbb" "C#" "Dx" "Dx" "C#"))) => (2 1 1 2)



(defun hash-table-alist (hash-table)
            "Convert a hash table to an assoc-list."
            (let ((alist '()))
              (maphash (lambda (key value)
                         (push (cons key value) alist))
                       hash-table)
              alist))

; (hash-table-alist (note-frequencies '("Abb" "Bbb" "C#" "Dx" "Dx" "C#"))) => (("C#" . 2) ("Bbb" . 1) ("Abb" . 1) ("Dx" . 2))

;;--------------------------------------------------------



;;------------this is auxiliary functions for <analyze-melody-key>------------

(defun find-tonic (notes)
            "Determine the most likely tonic of the melody based on frequency and chord notes."
            (let* (
                   (freqs (note-frequencies notes))
                   (last-note (car (last notes)))
                   (first-note (car notes))
                   (chords (append *major-chords* *minor-chords*))
                   (candidate-tonics
                    (loop for a in chords
                          if (or (member last-note (key-list-pair-values a) :test 'equal)
                                 (member first-note (key-list-pair-values a) :test 'equal))
                            collect (key-list-pair-key a) into candidates
                          finally (return (delete-duplicates candidates)))))
              (if candidate-tonics
                  (car (sort candidate-tonics
                             (lambda (a b)
                               (> (gethash a freqs 0)
                                  (gethash b freqs 0)))))
                (car (sort (hash-table-keys freqs)
                           (lambda (a b)
                             (> (gethash a freqs 0)
                                (gethash b freqs 0))))))))

; (find-tonic (take-off-octave-info '("C4" "E3" "D3" "D5" "F#3" "A3" "D3"))) => "D"




(defun scale-matching-score (notes scale)
            "Calculate how well the notes match the scale."
            (let ((score 0))
              (loop for note in notes
                    if (member note scale :test 'equal)
                      do (incf score))
              score))

; (scale-matching-score '("C" "D" "E" "F" "G" "A" "B" "C" "D" "E" "F") '("C" "D" "E" "F" "G" "A" "B")) => 11
; (scale-matching-score '("D" "E" "F#" "G" "A" "B" "C#" "D" "E" "D") '("D" "E" "F#" "G" "A" "B" "C" "D")) => 9





(defun calculate-score (notes scale-pairs freq-table tonic)
            "Calculate the comprehensive score for each scale."
            (let ((scores (make-hash-table :test 'equal)))
              (loop for a in scale-pairs
                    do (let* (
                              (match-score (scale-matching-score notes (key-list-pair-values a)))
                              (freq-score (reduce #'+ 
                                                  (mapcar (lambda (note) 
                                                            (gethash note freq-table 0)) 
                                                          (key-list-pair-values a))))
                              (tonic-score (if (member tonic (key-list-pair-values a)) 1 0))
                              (total-score (+ match-score freq-score tonic-score))
                              )
                         (setf (gethash (key-list-pair-key a) scores) total-score)))
              scores))

#| (hash-table-values (calculate-score '("C" "D" "E" "F" "G" "A" "B" "C" "D" "E" "F") *natural-major-scale-pairs-1* 
(note-frequencies '("C" "D" "E" "F" "G" "A" "B" "C" "D" "E" "F")) "C")) |#





(defun calculate-probabilities (scores)
            "Calculate the probabilities of each scale."
            (let ((total-score (reduce #'+ (hash-table-values scores))))
              (maphash (lambda (scale score)
                         (setf (gethash scale scores) (/ (float score) total-score)))
                       scores)
              scores))




(defun merge-scores (&rest score-tables)
            "Merge multiple score tables into one."
            (let ((merged-scores (make-hash-table :test 'equal)))
              (loop for table in score-tables
                    do (maphash (lambda (scale score)
                                  (incf (gethash scale merged-scores 0) score))
                                table))
              merged-scores))



(defun add-label-to-scores (scores label mode)
            "Add a label and mode to the keys in scores hash table."
            (let ((labeled-scores (make-hash-table :test 'equal)))
              (maphash (lambda (key value)
                         (setf (gethash (format nil "~a~a~a" key label mode) labeled-scores) value))
                       scores)
              labeled-scores))

;;--------------------------------------------------------
;;--------------------------------------------------------
;;--------------------------------------------------------

(defun analyze-melody-key (melody)
            "Analyze the key of the melody (Only the melodies of natural and harmonic scale can be analyzed)
             Sort in order of probable probability.
             -----------------------------------------------------
             M0 = natural Major scale, m0 = natural minor scale
             M1 = harmonic Major scale, m1 = harmonic minor scale"
            (let* (
                   (notes (take-off-octave-info melody))
                   (freq-table (note-frequencies notes))
                   (tonic (find-tonic notes))
                   (natural-major-scores (calculate-score notes *natural-major-scale-pairs-1* freq-table tonic))
                   (natural-minor-scores (calculate-score notes *natural-minor-scale-pairs-1* freq-table tonic))
                   (harmonic-major-scores (calculate-score notes *harmonic-major-scale-pairs-1* freq-table tonic))
                   (harmonic-minor-scores (calculate-score notes *harmonic-minor-scale-pairs-1* freq-table tonic))
                   (labeled-natural-major-scores (add-label-to-scores natural-major-scores "M" 0))
                   (labeled-natural-minor-scores (add-label-to-scores natural-minor-scores "m" 0))
                   (labeled-harmonic-major-scores (add-label-to-scores harmonic-major-scores "M" 1))
                   (labeled-harmonic-minor-scores (add-label-to-scores harmonic-minor-scores "m" 1))
                   (all-scores (merge-scores labeled-natural-major-scores labeled-natural-minor-scores labeled-harmonic-major-scores labeled-harmonic-minor-scores))
                   (probabilities (calculate-probabilities all-scores))
                   (sorted-probabilities (sort (hash-table-alist probabilities) #'> :key #'cdr))
                   )
              sorted-probabilities))


#|0 indicates natural mode, 1 indicates harmonic mode
  M indicates Major scale, m indicates minor scale
---------------------------------------------------------
    M0 = natural Major scale, m0 = natural minor scale
    M1 = harmonic Major scale, m1 = harmonic minor scale|#


;; (analyze-melody-key '("B4" "G#4" "F#4" "G#4" "E4" "G#4" "E4" "D#4" "C#4" "B3" "E4" "B3" "C#4" "D#4" "E4" "G#4" "F#4" "A4" "G#4" "E4" "E4" "E4"))








#|---------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-------------------------------4. Chord system-------------------------------------
-----------------------------------------------------------------------------------
---------------------------------------------------------------------------------|#



; "M" = Major triad, "m" = minor triad, "d" = diminished triad, "a" = augmented triad
; 3 = root position, 6 = first inversion, 46 = second inversion

(om::defmethod! triad-maker (root quality inversion)
         :icon 40021
         :initvals '("C4" "M" 3)
         :menuins '(                    
                    (1 (("M" "M") ("m" "m") ("d" "d") ("a" "a")))
                    (2 (("3" 3) ("6" 6) ("46" 46)))
                    )
         :doc "<input1>: root 
               <input2>: chord quality, (M = Major triad, m = minor triad, d = diminished triad, a = augmented triad)
               <input3>: inversion, (3 = root position, 6 = first inversion, 46 = second inversion)"
         (cond ((string= quality "M") (cond ((= inversion 3) (append (simple-interval-maker-up root 3 "M") (cdr (simple-interval-maker-up root 5 "p"))))
                                            ((= inversion 6) (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 3 "m")
                                                                     (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 6 "m"))))
                                            ((= inversion 46) (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 4 "p")
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 6 "M"))))))
               ((string= quality "m") (cond ((= inversion 3) (append (simple-interval-maker-up root 3 "m") (cdr (simple-interval-maker-up root 5 "p"))))
                                            ((= inversion 6) (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 3 "M")
                                                                     (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 6 "M"))))
                                            ((= inversion 46) (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 4 "p")
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 6 "m"))))))
               ((string= quality "d") (cond ((= inversion 3) (append (simple-interval-maker-up root 3 "m") (cdr (simple-interval-maker-up root 5 "d"))))
                                            ((= inversion 6) (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 3 "m")
                                                                     (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 6 "M"))))
                                            ((= inversion 46) (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "d")) 4 "a")
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "d")) 6 "M"))))))
               ((string= quality "a") (cond ((= inversion 3) (append (simple-interval-maker-up root 3 "M") (cdr (simple-interval-maker-up root 5 "a"))))
                                            ((= inversion 6) (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 3 "M")
                                                                     (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 6 "m"))))
                                            ((= inversion 46) (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "a")) 4 "d")
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "a")) 6 "m"))))))))




#| "d" = diminished seventh, "half-d" = half-diminished seventh, "m" = minor seventh, "mM" = minor Major seventh
   "v" = dominant seventh, "M" = major seventh, "a" = augmented seventh, "aM" = augmented Major seventh
 ----------------------------------------------------------------------------------------------------------- 
    7 = root position, 56 = first inversion, 34 = second inversion, 2 = third inversion   |#

(om::defmethod! seventh-maker (root quality inversion)
         :icon 40021
         :initvals '("C4" "M" 3)
         :menuins '(                    
                    (1 (("d" "d") ("half-d" "half-d") ("m" "m") ("mM" "mM") 
                        ("v" "v") ("M" "M") ("a" "a") ("aM" "aM")))
                    (2 (("7" 7) ("56" 56) ("34" 34) ("2" 2)))
                    )
         :doc "<input1>: root 
               <input2>: chord quality, (d = diminished seventh, half-d = half-diminished seventh, m = minor seventh, mM = minor Major seventh
                                         v = dominant seventh, M = major seventh, a = augmented seventh, aM = augmented Major seventh)
               <input3>:inversion, (7 = root position, 56 = first inversion, 34 = second inversion, 2 = third inversion)"
         (cond ((string= quality "d") (cond ((= inversion 7) (append (append (simple-interval-maker-up root 3 "m") (cdr (simple-interval-maker-up root 5 "d")))
                                                                     (cdr (simple-interval-maker-up root 7 "d"))))
                                            ((= inversion 56) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 3 "m")
                                                                              (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 5 "d")))
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 6 "M"))))
                                            ((= inversion 34) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "d")) 3 "m")
                                                                              (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "d")) 4 "a")))
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "d")) 6 "M"))))
                                            ((= inversion 2) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "d")) 2 "a")
                                                                             (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "d")) 4 "a")))
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "d")) 6 "M"))))))
               ((string= quality "half-d") (cond ((= inversion 7) (append (append (simple-interval-maker-up root 3 "m") (cdr (simple-interval-maker-up root 5 "d")))
                                                                          (cdr (simple-interval-maker-up root 7 "m"))))
                                                 ((= inversion 56) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 3 "m")
                                                                                   (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 5 "p")))
                                                                           (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 6 "M"))))
                                                 ((= inversion 34) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "d")) 3 "M")
                                                                                   (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "d")) 4 "a")))
                                                                           (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "d")) 6 "M"))))
                                                 ((= inversion 2) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "m")) 2 "M")
                                                                                  (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "m")) 4 "p")))
                                                                          (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "m")) 6 "m"))))))
               ((string= quality "m") (cond ((= inversion 7) (append (append (simple-interval-maker-up root 3 "m") (cdr (simple-interval-maker-up root 5 "p")))
                                                                     (cdr (simple-interval-maker-up root 7 "m"))))
                                            ((= inversion 56) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 3 "M")
                                                                              (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 5 "p")))
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 6 "M"))))
                                            ((= inversion 34) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 3 "m")
                                                                                   (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 4 "p")))
                                                                           (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 6 "m"))))
                                            ((= inversion 2) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "m")) 2 "M")
                                                                             (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "m")) 4 "p")))
                                                                     (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "m")) 6 "M"))))))
               ((string= quality "mM") (cond ((= inversion 7) (append (append (simple-interval-maker-up root 3 "m") (cdr (simple-interval-maker-up root 5 "p")))
                                                                      (cdr (simple-interval-maker-up root 7 "M"))))
                                             ((= inversion 56) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 3 "M")
                                                                               (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 5 "a")))
                                                                       (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "m")) 6 "M"))))
                                             ((= inversion 34) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 3 "M")
                                                                               (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 4 "p")))
                                                                       (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 6 "m"))))
                                             ((= inversion 2) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "M")) 2 "m")
                                                                              (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "M")) 4 "d")))
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "M")) 6 "m"))))))
               ((string= quality "v") (cond ((= inversion 7) (append (append (simple-interval-maker-up root 3 "M") (cdr (simple-interval-maker-up root 5 "p")))
                                                                     (cdr (simple-interval-maker-up root 7 "m"))))
                                            ((= inversion 56) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 3 "m")
                                                                              (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 5 "d")))
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 6 "m"))))
                                            ((= inversion 34) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 3 "m")
                                                                              (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 4 "p")))
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 6 "M"))))
                                            ((= inversion 2) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "m")) 2 "M")
                                                                             (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "m")) 4 "a")))
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "m")) 6 "M"))))))
               ((string= quality "M") (cond ((= inversion 7) (append (append (simple-interval-maker-up root 3 "M") (cdr (simple-interval-maker-up root 5 "p")))
                                                                     (cdr (simple-interval-maker-up root 7 "M"))))
                                            ((= inversion 56) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 3 "m")
                                                                              (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 5 "p")))
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 6 "m"))))
                                            ((= inversion 34) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 3 "M")
                                                                              (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 4 "p")))
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "p")) 6 "M"))))
                                            ((= inversion 2) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "M")) 2 "m")
                                                                             (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "M")) 4 "p")))
                                                                     (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "M")) 6 "m"))))))
               ((string= quality "a") (cond ((= inversion 7) (append (append (simple-interval-maker-up root 3 "M") (cdr (simple-interval-maker-up root 5 "a")))
                                                                     (cdr (simple-interval-maker-up root 7 "m"))))
                                            ((= inversion 56) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 3 "M")
                                                                              (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 5 "d")))
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 6 "m"))))
                                            ((= inversion 34) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "a")) 3 "d")
                                                                              (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "a")) 4 "d")))
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "a")) 6 "m"))))
                                            ((= inversion 2) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "m")) 2 "M")
                                                                             (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "m")) 4 "a")))
                                                                     (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "m")) 6 "a"))))))
               ((string= quality "aM") (cond ((= inversion 7) (append (append (simple-interval-maker-up root 3 "M") (cdr (simple-interval-maker-up root 5 "a")))
                                                                      (cdr (simple-interval-maker-up root 7 "M"))))
                                             ((= inversion 56) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 3 "M")
                                                                               (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 5 "p")))
                                                                       (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 3 "M")) 6 "m"))))
                                             ((= inversion 34) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "a")) 3 "m")
                                                                               (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "a")) 4 "d")))
                                                                       (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 5 "a")) 6 "m"))))
                                             ((= inversion 2) (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "M")) 2 "m")
                                                                              (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "M")) 4 "p")))
                                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 7 "M")) 6 "M"))))))))



#| "Mv9" = Major ninth chord (dominant ninth chord in natural Major), 
   "mv9" = minor ninth chord (dominant ninth chord in minor or harmonic Major) 
   "v13" = dominant thirteenth chord, "N6" = Neapolitan chord
   "b3VII6/V" = Italian augmented sixth chord,
   "b3VII56/V" = German augmented sixth chord,
   "b5V34/V" = French augmented sixth chord   |#

(om::defmethod! extended-maker (root quality)
         :icon 40021
         :initvals '("G3" "Mv9")
         :menuins '(                    
                    (1 (("Mv9" "Mv9") ("mv9" "mv9") ("v13" "v13") ("N6" "N6") 
                        ("b3VII6/V" "b3VII6/V") ("b3VII56/V" "b3VII56/V") ("b5V34/V" "b5V34/V")))                    
                    )
         :doc "<input1>: root 
               <input2>: chord quality (Mv9 = Major ninth chord, mv9 = minor ninth chord, v13 = dominant thirteenth chord
                                        N6 = Neapolitan chord, b3VII6/V = Italian augmented sixth chord,
                                        b3VII56/V = German augmented sixth chord, b5V34/V = French augmented sixth chord)"
         (cond ((string= quality "Mv9") (append (append (simple-interval-maker-up root 3 "M") (cdr (simple-interval-maker-up root 7 "m")))
                                                (cdr (compound-interval-maker-up root 9 "M"))))
               ((string= quality "mv9") (append (append (simple-interval-maker-up root 3 "M") (cdr (simple-interval-maker-up root 7 "m")))
                                                (cdr (compound-interval-maker-up root 9 "m"))))
               ((string= quality "v13") (append (append (simple-interval-maker-up root 3 "M") (cdr (simple-interval-maker-up root 7 "m")))
                                                (cdr (compound-interval-maker-up root 13 "M"))))
               ((string= quality "N6") (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 4 "p")) 3 "m")
                                               (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 4 "p")) 6 "m"))))
               ((string= quality "b3VII6/V") (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 6 "m")) 3 "M")
                                                     (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 6 "m")) 6 "a"))))
               ((string= quality "b3VII56/V") (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 6 "m")) 3 "M") 
                                                              (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 6 "m")) 5 "p")))
                                                      (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 6 "m")) 6 "a"))))
               ((string= quality "b5V34/V") (append (append (simple-interval-maker-up (cadr (simple-interval-maker-up root 6 "m")) 3 "M") 
                                                            (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 6 "m")) 4 "a")))
                                                    (cdr (simple-interval-maker-up (cadr (simple-interval-maker-up root 6 "m")) 6 "a"))))))

;;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------


(defparameter *triad-degree-3*
          (list (make-pair "I" '(0 2 4))
                (make-pair "II" '(1 3 5))
                (make-pair "III" '(2 4 6))
                (make-pair "IV" '(3 5 0))
                (make-pair "V" '(4 6 1))
                (make-pair "VI" '(5 0 2))
                (make-pair "VII" '(6 1 3))))

(defparameter *triad-degree-6*
          (list (make-pair "I" '(2 4 0))
                (make-pair "II" '(3 5 1))
                (make-pair "III" '(4 6 2))
                (make-pair "IV" '(5 0 3))
                (make-pair "V" '(6 1 4))
                (make-pair "VI" '(0 2 5))
                (make-pair "VII" '(1 3 6))))

(defparameter *triad-degree-46*
          (list (make-pair "I" '(4 0 2))
                (make-pair "II" '(5 1 3))
                (make-pair "III" '(6 2 4))
                (make-pair "IV" '(0 3 5))
                (make-pair "V" '(1 4 6))
                (make-pair "VI" '(2 5 0))
                (make-pair "VII" '(3 6 1))))

(defparameter *seventh-degree-7*
          (list (make-pair "I" '(0 2 4 6))
                (make-pair "II" '(1 3 5 0))
                (make-pair "III" '(2 4 6 1))
                (make-pair "IV" '(3 5 0 2))
                (make-pair "V" '(4 6 1 3))
                (make-pair "VI" '(5 0 2 4))
                (make-pair "VII" '(6 1 3 5))))   ; vii7, when major scale, use *harmonic-major-scale-pairs-1*

(defparameter *seventh-degree-56*
          (list (make-pair "I" '(2 4 6 0))
                (make-pair "II" '(3 5 0 1))
                (make-pair "III" '(4 6 1 2))
                (make-pair "IV" '(5 0 2 3))
                (make-pair "V" '(6 1 3 4))
                (make-pair "VI" '(0 2 4 5))
                (make-pair "VII" '(1 3 5 6))))

(defparameter *seventh-degree-34*
          (list (make-pair "I" '(4 6 0 2))
                (make-pair "II" '(5 0 1 3))
                (make-pair "III" '(6 1 2 4))
                (make-pair "IV" '(0 2 3 5))
                (make-pair "V" '(1 3 4 6))
                (make-pair "VI" '(2 4 5 0))
                (make-pair "VII" '(3 5 6 1))))   ; vii7, when major scale, use *harmonic-major-scale-pairs-1*

(defparameter *seventh-degree-2*
          (list (make-pair "I" '(6 0 2 4))
                (make-pair "II" '(0 1 3 5))
                (make-pair "III" '(1 2 4 6))
                (make-pair "IV" '(2 3 5 0))
                (make-pair "V" '(3 4 6 1))
                (make-pair "VI" '(4 5 0 2))
                (make-pair "VII" '(5 6 1 3))))   ; vii7, when major scale, use *harmonic-major-scale-pairs-1*

;;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------




;;---------------this is auxiliary functions-------------- 

(defun find-major-scale-triad (tonic degree degree-pairs-list)
  (let* (
         (degree-values (key-list-pair-values (find-pair degree degree-pairs-list)))
         (scale (key-list-pair-values (find-pair tonic *natural-major-scale-pairs-1*)))
         (chord (mapcar (lambda (index) (nth index scale)) degree-values)))
    chord))

; (find-major-scale-triad "E" "V" *triad-degree-46*)
; (find-major-scale-triad "E" "VII" *seventh-degree-7*)




(defun find-major-scale-seventh (tonic degree degree-pairs-list)
  (if (string= degree "VII") 
      (let* (
             (degree-values (key-list-pair-values (find-pair "VII" degree-pairs-list)))
             (scale (key-list-pair-values (find-pair tonic *harmonic-major-scale-pairs-1*)))
             (chord (mapcar (lambda (index) (nth index scale)) degree-values)))
        chord)
    (let* (
           (degree-values (key-list-pair-values (find-pair degree degree-pairs-list)))
           (scale (key-list-pair-values (find-pair tonic *natural-major-scale-pairs-1*)))
           (chord (mapcar (lambda (index) (nth index scale)) degree-values)))
      chord)))

; (find-major-scale-seventh "C" "IV" *seventh-degree-34*)
; (find-major-scale-seventh "C" "VII" *seventh-degree-2*)
; (find-major-scale-seventh "C" "VII" *seventh-degree-7*)




(defun find-minor-scale-chord (tonic degree degree-pairs-list)
  (let* (
         (degree-values (key-list-pair-values (find-pair degree degree-pairs-list)))
         (scale (key-list-pair-values (find-pair tonic *harmonic-minor-scale-pairs-1*)))
         (chord (mapcar (lambda (index) (nth index scale)) degree-values)))
    chord))

; (find-minor-scale-chord "F" "VII" *triad-degree-3*)
; (find-minor-scale-chord "G#" "V" *seventh-degree-7*)
; (find-minor-scale-chord "G#" "VII" *seventh-degree-2*)




(defun find-chord (tonic degree-values pairs-list)
  (let* (         
         (scale (key-list-pair-values (find-pair tonic pairs-list)))
         (chord (mapcar (lambda (index) (nth index scale)) degree-values))
         )
    chord))

; (find-chord "D" '(4 6 3 5) *natural-major-scale-pairs-1*)
; (find-chord "F" '(4 6 3 5) *harmonic-minor-scale-pairs-1*)

;;--------------------------------------------------------




(om::defmethod! chord-degree (tonic scale degree chord inversion)
         :icon 40021
         :initvals '("C" "Major" "I" "triad" 3)
         :menuins '(                    
                    (1 (("Major" "Major") ("minor" "minor")))
                    (2 (("I" "I") ("II" "II") ("III" "III") ("IV" "IV") ("V" "V") ("VI" "VI") ("VII" "VII")))
                    (3 (("triad" "triad") ("seventh" "seventh")))
                    (4 (("3" 3) ("6" 6) ("46" 46) ("7" 7) ("56" 56) ("34" 34) ("2" 2)))
                    )
         :doc "<input1>: tonic (without octave information!!)
               <input2>: Major or minor scale
               <input3>: Roman numerals (degree)
               <input4>: triad or seventh chord
               <input5>: inversion, (3/7 = root position, 6/56 = first inversion, 46/34 = second inversion, 2 = third inversion)"
         (cond ((string-equal scale "Major") 
                (cond ((string-equal chord "triad") (cond ((= inversion 3) (find-major-scale-triad tonic degree *triad-degree-3*))
                                                     ((= inversion 6) (find-major-scale-triad tonic degree *triad-degree-6*))
                                                     ((= inversion 46) (find-major-scale-triad tonic degree *triad-degree-46*))
                                                     (t nil)))
                      ((string-equal chord "seventh") (cond ((= inversion 7) (find-major-scale-seventh tonic degree *seventh-degree-7*))
                                                       ((= inversion 56) (find-major-scale-seventh tonic degree *seventh-degree-56*))
                                                       ((= inversion 34) (find-major-scale-seventh tonic degree *seventh-degree-34*))
                                                       ((= inversion 2) (find-major-scale-seventh tonic degree *seventh-degree-2*))
                                                       (t nil)))))
               ((string-equal scale "minor") 
                (cond ((string-equal chord "triad") (cond ((= inversion 3) (find-minor-scale-chord tonic degree *triad-degree-3*))
                                                     ((= inversion 6) (find-minor-scale-chord tonic degree *triad-degree-6*))
                                                     ((= inversion 46) (find-minor-scale-chord tonic degree *triad-degree-46*))
                                                     (t nil)))
                      ((string-equal chord "seventh") (cond ((= inversion 7) (find-minor-scale-chord tonic degree *seventh-degree-7*))
                                                       ((= inversion 56) (find-minor-scale-chord tonic degree *seventh-degree-56*))
                                                       ((= inversion 34) (find-minor-scale-chord tonic degree *seventh-degree-34*))
                                                       ((= inversion 2) (find-minor-scale-chord tonic degree *seventh-degree-2*))
                                                       (t nil)))))))

;; (chord-degree "F" "Major" "V" "triad" 46) => ("G" "C" "E")
;; (chord-degree "D" "Major" "III" "seventh" 2) => ("E" "F#" "A" "C#")
;; (chord-degree "A" "Major" "VII" "seventh" 7) => ("G#" "B" "D" "F=")
;; (chord-degree "C#" "MAJOR" "V" "seventh" 7) => ("G#" "B#" "D#" "F#")


                                              
;;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------

#| "N6" = Neapolitan chord
   "b3VII6/V" = Italian augmented sixth chord,
   "b3VII56/V" = German augmented sixth chord,
   "b5V34/V" = French augmented sixth chord |#

(defparameter *n6-chord-major*
          (list (make-pair "Cb" '("Fb" "Abb" "Dbb"))
                (make-pair "C" '("F" "Ab" "Db"))
                (make-pair "C#" '("F#" "A=" "D="))
                (make-pair "Db" '("Gb" "Bbb" "Ebb"))
                (make-pair "D" '("G" "Bb" "Eb")) 
                (make-pair "Eb" '("Ab" "Cb" "Fb"))
                (make-pair "E" '("A" "C=" "F="))
                (make-pair "F" '("Bb" "Db" "Gb"))
                (make-pair "F#" '("B" "D=" "G="))
                (make-pair "Gb" '("Cb" "Ebb" "Abb"))
                (make-pair "G" '("C" "Eb" "Ab"))
                (make-pair "Ab" '("Db" "Fb" "Bbb"))
                (make-pair "A" '("D" "F=" "Bb"))
                (make-pair "Bb" '("Eb" "Gb" "Cb"))
                (make-pair "B" '("E" "G=" "C="))
                ))

(defparameter *n6-chord-minor*
          (list (make-pair "C" '("F" "Ab" "Db"))
                (make-pair "C#" '("F#" "A" "D="))
                (make-pair "D" '("G" "Bb" "Eb"))
                (make-pair "D#" '("G#" "B" "E="))
                (make-pair "Eb" '("Ab" "Cb" "Fb"))
                (make-pair "E" '("A" "C" "F="))
                (make-pair "F" '("Bb" "Db" "Gb"))
                (make-pair "F#" '("B" "D" "G="))
                (make-pair "G" '("C" "Eb" "Ab"))
                (make-pair "G#" '("C#" "E" "A="))
                (make-pair "Ab" '("Db" "Fb" "Bbb"))
                (make-pair "A" '("D" "F" "Bb"))
                (make-pair "A#" '("D#" "F#" "B="))
                (make-pair "Bb" '("Eb" "Gb" "Cb"))
                (make-pair "B" '("E" "G" "C="))                                            
                ))      

(defparameter *b3vii6/v-major*
          (list (make-pair "Cb" '("Abb" "Cb" "F="))
                (make-pair "C" '("Ab" "C" "F#"))
                (make-pair "C#" '("A=" "C#" "Fx"))
                (make-pair "Db" '("Bbb" "Db" "G="))
                (make-pair "D" '("Bb" "D" "G#")) 
                (make-pair "Eb" '("Cb" "Eb" "A="))
                (make-pair "E" '("C=" "E" "A#"))
                (make-pair "F" '("Db" "F" "B="))
                (make-pair "F#" '("D=" "F#" "B#"))
                (make-pair "Gb" '("Ebb" "Gb" "C="))
                (make-pair "G" '("Eb" "G" "C#"))
                (make-pair "Ab" '("Fb" "Ab" "D="))
                (make-pair "A" '("F=" "A" "D#"))
                (make-pair "Bb" '("Gb" "Bb" "E="))
                (make-pair "B" '("G=" "B" "E#"))
                ))

(defparameter *b3vii6/v-minor*
          (list (make-pair "C" '("Ab" "C" "F#"))
                (make-pair "C#" '("A=" "C#" "Fx"))
                (make-pair "D" '("Bb" "D" "G#"))
                (make-pair "D#" '("B=" "D#" "Gx"))
                (make-pair "Eb" '("Cb" "Eb" "A="))
                (make-pair "E" '("C=" "E" "A#"))
                (make-pair "F" '("Db" "F" "B="))
                (make-pair "F#" '("D=" "F#" "B#"))
                (make-pair "G" '("Eb" "G" "C#"))
                (make-pair "G#" '("E=" "G#" "Cx"))
                (make-pair "Ab" '("Fb" "Ab" "D="))
                (make-pair "A" '("F=" "A" "D#"))
                (make-pair "A#" '("F#" "A#" "Dx"))
                (make-pair "Bb" '("Gb" "Bb" "E="))
                (make-pair "B" '("G=" "B" "E#"))                                            
                ))

(defparameter *b3vii56/v-major*
          (list (make-pair "Cb" '("Abb" "Cb" "Ebb" "F="))
                (make-pair "C" '("Ab" "C" "Eb" "F#"))
                (make-pair "C#" '("A=" "C#" "E=" "Fx"))
                (make-pair "Db" '("Bbb" "Db" "Fb" "G="))
                (make-pair "D" '("Bb" "D" "F=" "G#")) 
                (make-pair "Eb" '("Cb" "Eb" "Gb" "A="))
                (make-pair "E" '("C=" "E" "G=" "A#"))
                (make-pair "F" '("Db" "F" "Ab" "B="))
                (make-pair "F#" '("D=" "F#" "A=" "B#"))
                (make-pair "Gb" '("Ebb" "Gb" "Bbb" "C="))
                (make-pair "G" '("Eb" "G" "Bb" "C#"))
                (make-pair "Ab" '("Fb" "Ab" "Cb" "D="))
                (make-pair "A" '("F=" "A" "C=" "D#"))
                (make-pair "Bb" '("Gb" "Bb" "Db" "E="))
                (make-pair "B" '("G=" "B" "D=" "E#"))
                ))

(defparameter *b3vii56/v-minor*
          (list (make-pair "C" '("Ab" "C" "Eb" "F#"))
                (make-pair "C#" '("A=" "C#" "E=" "Fx"))
                (make-pair "D" '("Bb" "D" "F=" "G#"))
                (make-pair "D#" '("B=" "D#" "F#" "Gx"))
                (make-pair "Eb" '("Cb" "Eb" "Gb" "A="))
                (make-pair "E" '("C=" "E" "G=" "A#"))
                (make-pair "F" '("Db" "F" "Ab" "B="))
                (make-pair "F#" '("D=" "F#" "A=" "B#"))
                (make-pair "G" '("Eb" "G" "Bb" "C#"))
                (make-pair "G#" '("E=" "G#" "B=" "Cx"))
                (make-pair "Ab" '("Fb" "Ab" "Cb" "D="))
                (make-pair "A" '("F=" "A" "C=" "D#"))
                (make-pair "A#" '("F#" "A#" "C#" "Dx"))
                (make-pair "Bb" '("Gb" "Bb" "Db" "E="))
                (make-pair "B" '("G=" "B" "D=" "E#"))                                            
                ))

(defparameter *b5v34/v-major*
          (list (make-pair "Cb" '("Abb" "Cb" "Db" "F="))
                (make-pair "C" '("Ab" "C" "D" "F#"))
                (make-pair "C#" '("A=" "C#" "D#" "Fx"))
                (make-pair "Db" '("Bbb" "Db" "Eb" "G="))
                (make-pair "D" '("Bb" "D" "E" "G#")) 
                (make-pair "Eb" '("Cb" "Eb" "F" "A="))
                (make-pair "E" '("C=" "E" "F#" "A#"))
                (make-pair "F" '("Db" "F" "G" "B="))
                (make-pair "F#" '("D=" "F#" "G#" "B#"))
                (make-pair "Gb" '("Ebb" "Gb" "Ab" "C="))
                (make-pair "G" '("Eb" "G" "A" "C#"))
                (make-pair "Ab" '("Fb" "Ab" "Bb" "D="))
                (make-pair "A" '("F=" "A" "B" "D#"))
                (make-pair "Bb" '("Gb" "Bb" "C" "E="))
                (make-pair "B" '("G=" "B" "C#" "E#"))
                ))

(defparameter *b5v34/v-minor*
          (list (make-pair "C" '("Ab" "C" "D" "F#"))
                (make-pair "C#" '("A=" "C#" "D#" "Fx"))
                (make-pair "D" '("Bb" "D" "E" "G#"))
                (make-pair "D#" '("B=" "D#" "E#" "Gx"))
                (make-pair "Eb" '("Cb" "Eb" "F" "A="))
                (make-pair "E" '("C=" "E" "F#" "A#"))
                (make-pair "F" '("Db" "F" "G" "B="))
                (make-pair "F#" '("D=" "F#" "G#" "B#"))
                (make-pair "G" '("Eb" "G" "A" "C#"))
                (make-pair "G#" '("E=" "G#" "A#" "Cx"))
                (make-pair "Ab" '("Fb" "Ab" "Bb" "D="))
                (make-pair "A" '("F=" "A" "B" "D#"))
                (make-pair "A#" '("F#" "A#" "B#" "Dx"))
                (make-pair "Bb" '("Gb" "Bb" "C" "E="))
                (make-pair "B" '("G=" "B" "C#" "E#"))                                            
                ))

(defparameter *natural-major-scale-pairs-b2*
          (list (make-pair "Cb" '("Cb" "Dbb" "Eb" "Fb" "Gb" "Ab" "Bb" "Cb"))
                (make-pair "C" '("C" "Db" "E" "F" "G" "A" "B" "C"))
                (make-pair "C#" '("C#" "D=" "E#" "F#" "G#" "A#" "B#" "C#"))
                (make-pair "Db" '("Db" "Ebb" "F" "Gb" "Ab" "Bb" "C" "Db"))
                (make-pair "D" '("D" "Eb" "F#" "G" "A" "B" "C#" "D")) 
                (make-pair "Eb" '("Eb" "Fb" "G" "Ab" "Bb" "C" "D" "Eb"))
                (make-pair "E" '("E" "F=" "G#" "A" "B" "C#" "D#" "E"))
                (make-pair "F" '("F" "Gb" "A" "Bb" "C" "D" "E" "F"))
                (make-pair "F#" '("F#" "G=" "A#" "B" "C#" "D#" "E#" "F#"))
                (make-pair "Gb" '("Gb" "Abb" "Bb" "Cb" "Db" "Eb" "F" "Gb"))
                (make-pair "G" '("G" "Ab" "B" "C" "D" "E" "F#" "G"))
                (make-pair "Ab" '("Ab" "Bbb" "C" "Db" "Eb" "F" "G" "Ab"))
                (make-pair "A" '("A" "Bb" "C#" "D" "E" "F#" "G#" "A"))
                (make-pair "Bb" '("Bb" "Cb" "D" "Eb" "F" "G" "A" "Bb"))
                (make-pair "B" '("B" "C=" "D#" "E" "F#" "G#" "A#" "B"))
                ))

(defparameter *natural-major-scale-pairs-#2*
          (list (make-pair "Cb" '("Cb" "D=" "Eb" "Fb" "Gb" "Ab" "Bb" "Cb"))
                (make-pair "C" '("C" "D#" "E" "F" "G" "A" "B" "C"))
                (make-pair "C#" '("C#" "Dx" "E#" "F#" "G#" "A#" "B#" "C#"))
                (make-pair "Db" '("Db" "E=" "F" "Gb" "Ab" "Bb" "C" "Db"))
                (make-pair "D" '("D" "E#" "F#" "G" "A" "B" "C#" "D")) 
                (make-pair "Eb" '("Eb" "F#" "G" "Ab" "Bb" "C" "D" "Eb"))
                (make-pair "E" '("E" "Fx" "G#" "A" "B" "C#" "D#" "E"))
                (make-pair "F" '("F" "G#" "A" "Bb" "C" "D" "E" "F"))
                (make-pair "F#" '("F#" "Gx" "A#" "B" "C#" "D#" "E#" "F#"))
                (make-pair "Gb" '("Gb" "A=" "Bb" "Cb" "Db" "Eb" "F" "Gb"))
                (make-pair "G" '("G" "A#" "B" "C" "D" "E" "F#" "G"))
                (make-pair "Ab" '("Ab" "B=" "C" "Db" "Eb" "F" "G" "Ab"))
                (make-pair "A" '("A" "B#" "C#" "D" "E" "F#" "G#" "A"))
                (make-pair "Bb" '("Bb" "C#" "D" "Eb" "F" "G" "A" "Bb"))
                (make-pair "B" '("B" "Cx" "D#" "E" "F#" "G#" "A#" "B"))
                ))

(defparameter *harmonic-major-scale-pairs-b2*
          (list (make-pair "Cb" '("Cb" "Dbb" "Eb" "Fb" "Gb" "Abb" "Bb" "Cb"))
                (make-pair "C" '("C" "Db" "E" "F" "G" "Ab" "B" "C"))
                (make-pair "C#" '("C#" "D=" "E#" "F#" "G#" "A=" "B#" "C#"))
                (make-pair "Db" '("Db" "Ebb" "F" "Gb" "Ab" "Bbb" "C" "Db"))
                (make-pair "D" '("D" "Eb" "F#" "G" "A" "Bb" "C#" "D")) 
                (make-pair "Eb" '("Eb" "Fb" "G" "Ab" "Bb" "Cb" "D" "Eb"))
                (make-pair "E" '("E" "F=" "G#" "A" "B" "C=" "D#" "E"))
                (make-pair "F" '("F" "Gb" "A" "Bb" "C" "Db" "E" "F"))
                (make-pair "F#" '("F#" "G=" "A#" "B" "C#" "D=" "E#" "F#"))
                (make-pair "Gb" '("Gb" "Abb" "Bb" "Cb" "Db" "Ebb" "F" "Gb"))
                (make-pair "G" '("G" "Ab" "B" "C" "D" "Eb" "F#" "G"))
                (make-pair "Ab" '("Ab" "Bbb" "C" "Db" "Eb" "Fb" "G" "Ab"))
                (make-pair "A" '("A" "Bb" "C#" "D" "E" "F=" "G#" "A"))
                (make-pair "Bb" '("Bb" "Cb" "D" "Eb" "F" "Gb" "A" "Bb"))
                (make-pair "B" '("B" "C=" "D#" "E" "F#" "G=" "A#" "B"))
                ))

(defparameter *harmonic-major-scale-pairs-#2*
          (list (make-pair "Cb" '("Cb" "D=" "Eb" "Fb" "Gb" "Abb" "Bb" "Cb"))
                (make-pair "C" '("C" "D#" "E" "F" "G" "Ab" "B" "C"))
                (make-pair "C#" '("C#" "Dx" "E#" "F#" "G#" "A=" "B#" "C#"))
                (make-pair "Db" '("Db" "E=" "F" "Gb" "Ab" "Bbb" "C" "Db"))
                (make-pair "D" '("D" "E#" "F#" "G" "A" "Bb" "C#" "D")) 
                (make-pair "Eb" '("Eb" "F#" "G" "Ab" "Bb" "Cb" "D" "Eb"))
                (make-pair "E" '("E" "Fx" "G#" "A" "B" "C=" "D#" "E"))
                (make-pair "F" '("F" "G#" "A" "Bb" "C" "Db" "E" "F"))
                (make-pair "F#" '("F#" "Gx" "A#" "B" "C#" "D=" "E#" "F#"))
                (make-pair "Gb" '("Gb" "A=" "Bb" "Cb" "Db" "Ebb" "F" "Gb"))
                (make-pair "G" '("G" "A#" "B" "C" "D" "Eb" "F#" "G"))
                (make-pair "Ab" '("Ab" "B=" "C" "Db" "Eb" "Fb" "G" "Ab"))
                (make-pair "A" '("A" "B#" "C#" "D" "E" "F=" "G#" "A"))
                (make-pair "Bb" '("Bb" "C#" "D" "Eb" "F" "Gb" "A" "Bb"))
                (make-pair "B" '("B" "Cx" "D#" "E" "F#" "G=" "A#" "B"))
                ))

(defparameter *harmonic-minor-scale-pairs-b2*
          (list (make-pair "C" '("C" "Db" "Eb" "F" "G" "Ab" "B=" "C"))
                (make-pair "C#" '("C#" "D=" "E" "F#" "G#" "A" "B#" "C#"))
                (make-pair "D" '("D" "Eb" "F" "G" "A" "Bb" "C#" "D"))
                (make-pair "D#" '("D#" "E=" "F#" "G#" "A#" "B" "Cx" "D#"))
                (make-pair "Eb" '("Eb" "Fb" "Gb" "Ab" "Bb" "Cb" "D=" "Eb"))
                (make-pair "E" '("E" "F=" "G" "A" "B" "C" "D#" "E"))
                (make-pair "F" '("F" "Gb" "Ab" "Bb" "C" "Db" "E=" "F"))
                (make-pair "F#" '("F#" "G=" "A" "B" "C#" "D" "E#" "F#"))
                (make-pair "G" '("G" "Ab" "Bb" "C" "D" "Eb" "F#" "G"))
                (make-pair "G#" '("G#" "A=" "B" "C#" "D#" "E" "Fx" "G#"))
                (make-pair "Ab" '("Ab" "Bbb" "Cb" "Db" "Eb" "Fb" "G=" "Ab"))
                (make-pair "A" '("A" "Bb" "C" "D" "E" "F" "G#" "A"))
                (make-pair "A#" '("A#" "B=" "C#" "D#" "E#" "F#" "Gx" "A#"))
                (make-pair "Bb" '("Bb" "Cb" "Db" "Eb" "F" "Gb" "A=" "Bb"))
                (make-pair "B" '("B" "C=" "D" "E" "F#" "G" "A#" "B"))                                            
                ))      

(defparameter *harmonic-minor-scale-pairs-#2*
          (list (make-pair "C" '("C" "D#" "Eb" "F" "G" "Ab" "B=" "C"))
                (make-pair "C#" '("C#" "Dx" "E" "F#" "G#" "A" "B#" "C#"))
                (make-pair "D" '("D" "E#" "F" "G" "A" "Bb" "C#" "D"))
                (make-pair "D#" '("D#" "Ex" "F#" "G#" "A#" "B" "Cx" "D#"))
                (make-pair "Eb" '("Eb" "F#" "Gb" "Ab" "Bb" "Cb" "D=" "Eb"))
                (make-pair "E" '("E" "Fx" "G" "A" "B" "C" "D#" "E"))
                (make-pair "F" '("F" "G#" "Ab" "Bb" "C" "Db" "E=" "F"))
                (make-pair "F#" '("F#" "Gx" "A" "B" "C#" "D" "E#" "F#"))
                (make-pair "G" '("G" "A#" "Bb" "C" "D" "Eb" "F#" "G"))
                (make-pair "G#" '("G#" "Ax" "B" "C#" "D#" "E" "Fx" "G#"))
                (make-pair "Ab" '("Ab" "B=" "Cb" "Db" "Eb" "Fb" "G=" "Ab"))
                (make-pair "A" '("A" "B#" "C" "D" "E" "F" "G#" "A"))
                (make-pair "A#" '("A#" "Bx" "C#" "D#" "E#" "F#" "Gx" "A#"))
                (make-pair "Bb" '("Bb" "C#" "Db" "Eb" "F" "Gb" "A=" "Bb"))
                (make-pair "B" '("B" "Cx" "D" "E" "F#" "G" "A#" "B"))                                            
                ))
;;-----------------------------------------------------------------------------------------------------------------------------------------------------------------------



(om::defmethod! extended-degree (tonic scale degree)
         :icon 40021
         :initvals '("C" "Major" "N6")
         :menuins '(                    
                    (1 (("Major" "Major") ("minor" "minor")))
                    (2 (("N6" "N6") ("b3VII6/V" "b3VII6/V") ("b3VII56/V" "b3VII56/V") ("b5V34/V" "b5V34/V")))
                    )
         :doc "<input1>: tonic (without octave information!!)
               <input2>: Major or minor scale
               <input3>: the chord degree and what chord is it"
         (cond ((string-equal scale "Major") (cond ((string-equal degree "N6") (key-list-pair-values (find-pair tonic *n6-chord-major*)))
                                                   ((string-equal degree "b3VII6/V") (key-list-pair-values (find-pair tonic *b3vii6/v-major*)))
                                                   ((string-equal degree "b3VII56/V") (key-list-pair-values (find-pair tonic *b3vii56/v-major*)))
                                                   ((string-equal degree "b5V34/V") (key-list-pair-values (find-pair tonic *b5v34/v-major*)))
                                                   (t nil)))
               ((string-equal scale "minor") (cond ((string-equal degree "N6") (key-list-pair-values (find-pair tonic *n6-chord-minor*)))
                                                   ((string-equal degree "b3VII6/V") (key-list-pair-values (find-pair tonic *b3vii6/v-minor*)))
                                                   ((string-equal degree "b3VII56/V") (key-list-pair-values (find-pair tonic *b3vii56/v-minor*)))
                                                   ((string-equal degree "b5V34/V") (key-list-pair-values (find-pair tonic *b5v34/v-minor*)))
                                                   (t nil)))
               ))

;; (extended-degree "C" "Major" "N6") => ("F" "Ab" "Db")
;; (extended-degree "F#" "minor" "b5V34/V") => ("D=" "F#" "G#" "B#")
;; (extended-degree "A" "major" "b3VII56/V") => ("F=" "A" "C=" "D#")





(om::defmethod! dominant-extended-degree (tonic scale chord inversion)
         :icon 40021
         :initvals '("C" "Major" "v9" 9)
         :menuins '(                    
                    (1 (("Major" "Major") ("minor" "minor")))
                    (2 (("V9" "V9") ("V13" "V13") ("b5V7" "b5V7") ("#5V7" "#5V7") ("b3VII7" "b3VII7") ("#3VII7" "#3VII7")))
                    (3 (("7" 7) ("56" 56) ("34" 34) ("2" 2) ("9" 9) ("13" 13)))
                    )
         :doc "<input1>: tonic (without octave information!!)
               <input2>: Major or minor scale
               <input3>: the chord degree and what chord is it
               <input4>: inversion"
         (cond ((string-equal scale "Major") (cond ((string-equal chord "V9") (if (= inversion 9) 
                                                                                  (find-chord tonic '(4 6 3 5) *natural-major-scale-pairs-1*) 
                                                                                nil))
                                                   ((string-equal chord "V13") (if (= inversion 13)
                                                                                   (find-chord tonic '(4 6 2 3) *natural-major-scale-pairs-1*)
                                                                                 nil))
                                                   ((string-equal chord "b5V7") (cond ((= inversion 7) (find-chord tonic '(4 6 1 3) *natural-major-scale-pairs-b2*))
                                                                                      ((= inversion 56) (find-chord tonic '(6 1 3 4) *natural-major-scale-pairs-b2*))
                                                                                      ((= inversion 34) (find-chord tonic '(1 3 4 6) *natural-major-scale-pairs-b2*))
                                                                                      ((= inversion 2) (find-chord tonic '(3 4 6 1) *natural-major-scale-pairs-b2*))
                                                                                      (t nil)))
                                                   ((string-equal chord "#5V7") (cond ((= inversion 7) (find-chord tonic '(4 6 1 3) *natural-major-scale-pairs-#2*))
                                                                                      ((= inversion 56) (find-chord tonic '(6 1 3 4) *natural-major-scale-pairs-#2*))
                                                                                      ((= inversion 34) (find-chord tonic '(1 3 4 6) *natural-major-scale-pairs-#2*))
                                                                                      ((= inversion 2) (find-chord tonic '(3 4 6 1) *natural-major-scale-pairs-#2*))
                                                                                      (t nil)))
                                                   ((string-equal chord "b3VII7") (cond ((= inversion 7) (find-chord tonic '(6 1 3 5) *harmonic-major-scale-pairs-b2*))
                                                                                        ((= inversion 56) (find-chord tonic '(1 3 5 6) *harmonic-major-scale-pairs-b2*))
                                                                                        ((= inversion 34) (find-chord tonic '(3 5 6 1) *harmonic-major-scale-pairs-b2*))
                                                                                        ((= inversion 2) (find-chord tonic '(5 6 1 3) *harmonic-major-scale-pairs-b2*))
                                                                                        (t nil)))
                                                   ((string-equal chord "#3VII7") (cond ((= inversion 7) (find-chord tonic '(6 1 3 5) *harmonic-major-scale-pairs-#2*))
                                                                                        ((= inversion 56) (find-chord tonic '(1 3 5 6) *harmonic-major-scale-pairs-#2*))
                                                                                        ((= inversion 34) (find-chord tonic '(3 5 6 1) *harmonic-major-scale-pairs-#2*))
                                                                                        ((= inversion 2) (find-chord tonic '(5 6 1 3) *harmonic-major-scale-pairs-#2*))
                                                                                        (t nil)))
                                                   (t nil)))
               ((string-equal scale "minor") (cond ((string-equal chord "V9") (if (= inversion 9) 
                                                                                  (find-chord tonic '(4 6 3 5) *harmonic-minor-scale-pairs-1*) 
                                                                                nil))
                                                   ((string-equal chord "V13") (if (= inversion 13)
                                                                                   (find-chord tonic '(4 6 2 3) *harmonic-minor-scale-pairs-1*)
                                                                                 nil))
                                                   ((string-equal chord "b5V7") (cond ((= inversion 7) (find-chord tonic '(4 6 1 3) *harmonic-minor-scale-pairs-b2*))
                                                                                      ((= inversion 56) (find-chord tonic '(6 1 3 4) *harmonic-minor-scale-pairs-b2*))
                                                                                      ((= inversion 34) (find-chord tonic '(1 3 4 6) *harmonic-minor-scale-pairs-b2*))
                                                                                      ((= inversion 2) (find-chord tonic '(3 4 6 1) *harmonic-minor-scale-pairs-b2*))
                                                                                      (t nil)))
                                                   ((string-equal chord "#5V7") (cond ((= inversion 7) (find-chord tonic '(4 6 1 3) *harmonic-minor-scale-pairs-#2*))
                                                                                      ((= inversion 56) (find-chord tonic '(6 1 3 4) *harmonic-minor-scale-pairs-#2*))
                                                                                      ((= inversion 34) (find-chord tonic '(1 3 4 6) *harmonic-minor-scale-pairs-#2*))
                                                                                      ((= inversion 2) (find-chord tonic '(3 4 6 1) *harmonic-minor-scale-pairs-#2*))
                                                                                      (t nil)))
                                                   ((string-equal chord "b3VII7") (cond ((= inversion 7) (find-chord tonic '(6 1 3 5) *harmonic-minor-scale-pairs-b2*))
                                                                                        ((= inversion 56) (find-chord tonic '(1 3 5 6) *harmonic-minor-scale-pairs-b2*))
                                                                                        ((= inversion 34) (find-chord tonic '(3 5 6 1) *harmonic-minor-scale-pairs-b2*))
                                                                                        ((= inversion 2) (find-chord tonic '(5 6 1 3) *harmonic-minor-scale-pairs-b2*))
                                                                                        (t nil)))
                                                   ((string-equal chord "#3VII7") (cond ((= inversion 7) (find-chord tonic '(6 1 3 5) *harmonic-minor-scale-pairs-#2*))
                                                                                        ((= inversion 56) (find-chord tonic '(1 3 5 6) *harmonic-minor-scale-pairs-#2*))
                                                                                        ((= inversion 34) (find-chord tonic '(3 5 6 1) *harmonic-minor-scale-pairs-#2*))
                                                                                        ((= inversion 2) (find-chord tonic '(5 6 1 3) *harmonic-minor-scale-pairs-#2*))
                                                                                        (t nil)))
                                                   (t nil)))
               (t nil)))

;; (dominant-extended-degree "C" "major" "v9" 9)
;; (dominant-extended-degree "C" "minor" "v13" 13)





(om::defmethod! secondary-degree (tonic scale chord inversion)
         :icon 40021
         :initvals '("C" "Major" "V7/5" 7)
         :menuins '(                    
                    (1 (("Major" "Major") ("minor" "minor")))
                    (2 (("V7/II" "V7/2") ("VII7/II" "VII7/2") ("V7/III" "V7/3") ("VII7/III" "VII7/3") 
                        ("V7/IV" "V7/4") ("VII7/IV" "VII7/4") ("V7/V" "V7/5") ("VII7/V" "VII7/5")
                        ("V7/VI" "V7/6") ("VII7/VI" "VII7/6")))
                    (3 (("7" 7) ("56" 56) ("34" 34) ("2" 2)))                  
                    )
         :doc "<input1>: tonic (without octave information!!)
               <input2>: Major or minor scale
               <input3>: the chord degree and what chord is it
               <input4>: inversion"
         (let* (
                (major-key-quality '((2 . "minor") (3 . "minor") (4 . "Major") (5 . "Major") (6 . "minor")))
                (minor-key-quality '((2 . nil) (3 . "Major") (4 . "minor") (5 . "Major") (6 . "Major")))
                (degree-separator (let* (
                                         (chars (string-separator chord))
                                         (index (position "/" chars :test #'equal))
                                         (first-list (subseq chars 0 index))
                                         (last (car (subseq chars (1+ index))))
                                         )
                                    (list (reduce #'om::string+ first-list) last)))
                (scale-info (if (string-equal scale "Major") 
                                (key-list-pair-values (find-pair tonic *natural-major-scale-pairs-1*))
                              (key-list-pair-values (find-pair tonic *harmonic-minor-scale-pairs-1*))))
                (temporary-tonic (nth (1- (string-to-number (cadr degree-separator))) scale-info))
                (temporary-key-quality (if (string-equal scale "Major")
                                           (cdr (assoc (string-to-number (cadr degree-separator)) major-key-quality))
                                         (cdr (assoc (string-to-number (cadr degree-separator)) minor-key-quality))))
                (temporary-degree (if (= (length (car degree-separator)) 2) "V" "VII"))
                )


           (if (if (string-equal temporary-key-quality "Major") (find-pair temporary-tonic *natural-major-scale-pairs-1*) (find-pair temporary-tonic *harmonic-minor-scale-pairs-1*))
               (chord-degree temporary-tonic temporary-key-quality temporary-degree "seventh" inversion)
             (if (and (string-equal scale "minor") (string-equal (cadr degree-separator) "2"))
                 nil
               (let* (
                      (root (cadr (if (string-equal temporary-degree "V") 
                                      (simple-interval-maker-up (om::string+ temporary-tonic "4") 5 "p") 
                                    (simple-interval-maker-up (om::string+ temporary-tonic "4") 7 "M"))))
                      (seventh-chord (if (string-equal temporary-degree "V") 
                                         (seventh-maker root "v" inversion) 
                                       (seventh-maker root "d" inversion)))
                      )
                 (take-off-octave-info seventh-chord))))))
                            
;; (secondary-degree "D#" "minor" "V7/2" 7)
;; (secondary-degree "C" "minor" "V7/2" 7)
;; (secondary-degree "Cb" "major" "V7/4" 7)
;; (secondary-degree "C#" "major" "V7/5" 7)





;;---------------this is auxiliary functions-------------- 

(defun degree-separator (secondary-chord)
  (let* (
         (chars (string-separator secondary-chord))
         (index (position "/" chars :test #'equal))
         (first-list (subseq chars 0 index))
         (last (car (subseq chars (1+ index))))
         )
    (list (reduce #'om::string+ first-list) last)))
; (degree-separator "VII7/2")
; (degree-separator "b3VII56/5")


(defun degree-separator-pro (secondary-chord)
  (let* (
         (chars (string-separator secondary-chord))
         (index (position "/" chars :test #'equal))
         (first-list (subseq chars 0 index))
         (last-list (subseq chars (1+ index)))
         )
    (list (reduce #'om::string+ first-list) (reduce #'om::string+ last-list))))
; (degree-separator-pro "b3VII56/V")
; (degree-separator-pro "b3VII56/III")


(defun split-chord-string (chord-string)
  "Split a chord string into degree and inversion parts."
  (let* (
         (characters (string-separator chord-string))
         (inversion (loop for c in characters
                          when (find c '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9") :test #'equal)
                            collect c))
         (degree (remove-if (lambda (c) (find c '("0" "1" "2" "3" "4" "5" "6" "7" "8" "9") :test #'equal)) characters))
         )
    (list (reduce #'om::string+ degree) (reduce #'om::string+ inversion))))
; (split-chord-string "VII7")
; (split-chord-string "VII46")
; (split-chord-string "V3")
; (split-chord-string "VI56")


(defun remove-nils (lst)
  "Remove all nil elements from a nested list."
  (cond
   ((null lst) nil)
   ((listp (car lst))
    (let ((new-car (remove-nils (car lst))))
      (if new-car
          (cons new-car (remove-nils (cdr lst)))
        (remove-nils (cdr lst)))))
   ((eq (car lst) nil) (remove-nils (cdr lst)))
   (t (cons (car lst) (remove-nils (cdr lst))))))
; (remove-nils '((("A" "C#" "E" "G") ("V7/II")) (nil) (nil) (("A" "C#" "Eb" "G") ("b5V7/II"))))
; (remove-nils '((a b) nil c a))
; (remove-nils '(a b nil c a nil))


                           
(defun generate-combinations (lists)
  "Generate all possible combinations from a list of lists"
  (if (null lists)
      '(())
    (let ((combinations '()))
      (loop for element in (first lists) do
              (loop for subcombination in (generate-combinations (rest lists)) do
                      (push (cons element subcombination) combinations)))
      combinations)))
; (generate-combinations '((4) (4 3) (4 3 2) (2 3)))


(defun is-descending (lst)
  "Check if a list of numbers is in descending order"
  (loop for a on lst while (cdr a)
        always (>= (car a) (cadr a))))
; (is-descending '(4 4 2 2)) => t
; (is-descending '(4 3 4 3)) => nil


(defun interval-sep-num (interval-quality)
  (if (= (length interval-quality) 2)
      (string-to-number (cadr (string-separator interval-quality)))
    (let ((num-lst (cdr (string-separator interval-quality))))
      (string-to-number (om::string+ (first num-lst) (second num-lst))))))
; (interval-sep-num "d7")
; (interval-sep-num "p15")
; (interval-sep-num "M6")


(defun remove-one (item lst)
  "Remove one occurrence of ITEM from LST."
  (let ((result '())
        (removed nil))
    (dolist (x lst (nreverse result))
      (if (and (equal x item) (not removed))
          (setq removed t)
        (push x result)))))
; (remove-one "E" '("A" "C" "E" "E"))
; (remove-one "A" '("A" "C" "E" "E"))
; (remove-one "C" '("A" "C" "E" "E"))
; (remove-one "B" '("A" "C" "E" "E"))
; (remove-one "E" '("A" "C" "E" "E" "E"))

;;--------------------------------------------------------




(defun generate-all-chords (key scale)
            "generate all chords of the tonality"
            (let* (
                   (degree '("I" "II" "III" "IV" "V" "VI" "VII"))
                   (triad-inver '(3 6 46))
                   (seventh-inver '(7 56 34 2))
                   (extended '("N6" "b3VII6/V" "b3VII56/V" "b5V34/V"))
                   (dominant-extended '("b5V7" "#5V7" "b3VII7" "#3VII7"))
                   (secondary-degree '("V7/2" "VII7/2" "V7/3" "VII7/3" "V7/4" "VII7/4" "V7/5" "VII7/5" "V7/6" "VII7/6"))
                   (secondary-degree-alist '((2 . "II") (3 . "III") (4 . "IV") (5 . "V") (6 . "VI")))                  
              
                   (all-triad (om::flat (loop for a in degree
                                              collect
                                                (loop for b in triad-inver
                                                      collect (list (chord-degree key scale a "triad" b)
                                                                    (list (om::string+ a (write-to-string b)))))) 1))
                   (all-seventh (om::flat (loop for a in degree
                                                collect
                                                  (loop for b in seventh-inver
                                                        collect (list (chord-degree key scale a "seventh" b)
                                                                      (list (om::string+ a (write-to-string b)))))) 1))
                   (all-extended (loop for a in extended
                                       collect (list (extended-degree key scale a) (list a))))
                   (all-dominant-extended (append (list (list (dominant-extended-degree key scale "V9" 9) '("V9"))
                                                        (list (dominant-extended-degree key scale "V13" 13) '("V13"))) 
                                                  (om::flat (loop for a in dominant-extended
                                                                  collect 
                                                                    (loop for b in seventh-inver
                                                                          collect (list (dominant-extended-degree key scale a b) 
                                                                                        (list (om::string+ (reduce #'om::string+ (butlast (string-separator a))) 
                                                                                                           (write-to-string b)))))) 1)))
                   (all-secondary (remove-nils 
                                   (om::flat 
                                    (loop for a in secondary-degree
                                          collect
                                            (loop for b in seventh-inver
                                                  collect (let* (
                                                                 (correct-inver (om::string+ (reduce #'om::string+ (butlast (string-separator (car (degree-separator a)))))
                                                                                             (write-to-string b)))                                     
                                                                 (deg-roman-maker (cdr (assoc (string-to-number (cadr (degree-separator a))) 
                                                                                              secondary-degree-alist)))
                                                                 (correct-maker (om::string+ correct-inver "/" deg-roman-maker))
                                                                 )
                                                            (if (secondary-degree key scale a b)
                                                                (list (secondary-degree key scale a b) (list correct-maker))
                                                              '(nil))))) 1)))                                   
                   )
              (append all-triad (append all-seventh (append all-extended (append all-dominant-extended all-secondary))))))

; (length (generate-all-chords "F#" "Major"))
; (length (generate-all-chords "F#" "minor"))
; (length (generate-all-chords "D#" "minor"))




(defun generate-common-used-chords (key scale)
            "generate all common used chords of the tonality"
            (let* (
                   (seventh-inver '(7 56 34 2))
                   (extended '("N6" "b3VII6/V" "b3VII56/V" "b5V34/V"))
                   (dominant-extended '("b5V7" "#5V7" "b3VII7" "#3VII7"))
                   (secondary-degree '("V7/2" "VII7/2" "V7/3" "VII7/3" "V7/4" "VII7/4" "V7/5" "VII7/5" "V7/6" "VII7/6"))
                   (secondary-degree-alist '((2 . "II") (3 . "III") (4 . "IV") (5 . "V") (6 . "VI")))                  
              

                   (triad-3 (if (string-equal scale "minor")
                                (loop for a in '("I" "III" "IV" "V" "VI")
                                      collect (list (chord-degree key scale a "triad" 3)
                                                    (list (om::string+ a "3"))))
                              (loop for a in '("I" "II" "III" "IV" "V" "VI")
                                    collect (list (chord-degree key scale a "triad" 3)
                                                  (list (om::string+ a "3"))))))
                   (triad-6 (loop for a in '("I" "II" "III" "IV" "V" "VI" "VII")
                                  collect (list (chord-degree key scale a "triad" 6)
                                                (list (om::string+ a "6")))))
                   (triad-46 (loop for a in '("I" "IV" "V")
                                   collect (list (chord-degree key scale a "triad" 46)
                                                 (list (om::string+ a "46")))))
                   (all-triad (append triad-3 (append triad-6 triad-46)))
                   (all-seventh (om::flat (loop for a in '("II" "V" "VII")
                                                collect
                                                  (loop for b in seventh-inver
                                                        collect (list (chord-degree key scale a "seventh" b)
                                                                      (list (om::string+ a (write-to-string b)))))) 1))
                   (all-extended (loop for a in extended
                                       collect (list (extended-degree key scale a) (list a))))
                   (all-dominant-extended (append (list (list (dominant-extended-degree key scale "V9" 9) '("V9"))
                                                        (list (dominant-extended-degree key scale "V13" 13) '("V13"))) 
                                                  (om::flat (loop for a in dominant-extended
                                                                  collect 
                                                                    (loop for b in seventh-inver
                                                                          collect (list (dominant-extended-degree key scale a b) 
                                                                                        (list (om::string+ (reduce #'om::string+ (butlast (string-separator a))) 
                                                                                                           (write-to-string b)))))) 1)))
                   (all-secondary (remove-nils 
                                   (om::flat 
                                    (loop for a in secondary-degree
                                          collect
                                            (loop for b in seventh-inver
                                                  collect (let* (
                                                                 (correct-inver (om::string+ (reduce #'om::string+ (butlast (string-separator (car (degree-separator a)))))
                                                                                             (write-to-string b)))                                     
                                                                 (deg-roman-maker (cdr (assoc (string-to-number (cadr (degree-separator a))) 
                                                                                              secondary-degree-alist)))
                                                                 (correct-maker (om::string+ correct-inver "/" deg-roman-maker))
                                                                 )
                                                            (if (secondary-degree key scale a b)
                                                                (list (secondary-degree key scale a b) (list correct-maker))
                                                              '(nil))))) 1)))                                   
                   )
              (append all-triad (append all-seventh (append all-extended (append all-dominant-extended all-secondary))))))

; (length (generate-common-used-chords "F#" "Major"))
; (length (generate-common-used-chords "F#" "minor"))
; (length (generate-common-used-chords "D#" "minor"))




(defun search-soprano (high-note tonic scale)
            "Find all the chords in the tonality that contain high notes"
            (let (
                  (all-chords (generate-common-used-chords tonic scale))
                  (high-note2 (reduce #'om::string+ (butlast (string-separator high-note))))
                  (result '())
                  (filter-result '())
                  )

              ;; First step: find chords containing the high note
              (dolist (chord-info all-chords result)
                (let ((chord (first chord-info)))
                  (when (member high-note2 chord :test #'equal)
                    (push chord-info result))))    

              ;; Second step: filter out unusable chords
              (dolist (chord-info result filter-result)
                (let* (
                       (chord (first chord-info))
                       (degree (car (second chord-info)))
                       )
                  (cond
                   ((= (length chord) 4)
                    (unless (= (position high-note2 chord :test #'equal) 0)
                      (push chord-info filter-result)))
                   (t
                    (unless (and (or (string-equal degree "I6")
                                     (string-equal degree "IV6")
                                     (string-equal degree "V6")
                                     (string-equal degree "III3")
                                     (string-equal degree "b3VII6/V"))
                                 (= (position high-note2 chord :test #'equal) 0))
                      (push chord-info filter-result))))))))

; (search-soprano "A4" "C" "Major")
; (search-soprano "A4" "A" "minor")
; (search-soprano "A4" "E" "Major")
; (search-soprano "A4" "E" "minor")




(defun chord-degree-analyze (chord tonic scale)
            "Analyze the degree of a chord belonging to this key, and what chord is it 
             (chord can only contain a maximum of 4 notes, and it could be any arrangement!)"
            (let* (
                   (all-chords (generate-all-chords tonic scale))
                   (chord-no-octave (take-off-octave-info chord))
                   (filter-1 (lambda (chords root-note)
                             (remove-if-not (lambda (chord-info)
                                              (equal (first (first chord-info)) root-note))
                                            chords)))      ; "Filter chords to include only those with the specified root note"
                   (filter-2 (lambda (chords single-chord)
                               (remove-if-not (lambda (chord-info)
                                                (= (length (first chord-info)) (length single-chord)))
                                              chords)))   
                   (chord-root-note (first chord-no-octave))
                   (all-chords-1 (funcall filter-1 all-chords chord-root-note))
                   (all-chords-2 (funcall filter-2 all-chords-1 chord-no-octave))
                   (result '())
                   )
              (loop for chord-info in all-chords-2 do
                      (let ((generated-chord (first chord-info)))
                        (when (every (lambda (note) (member note chord-no-octave :test #'equal)) generated-chord)
              (push (car (cadr chord-info)) result))))
              (car result)))
                  
; (chord-degree-analyze '("C4" "G4" "Bb4" "E4") "C" "Major")
; (chord-degree-analyze '("F4" "Ab4" "Db4") "C" "Major")
; (chord-degree-analyze '("A4" "D4" "F4") "C" "Major")
; (chord-degree-analyze '("C4" "Ab3" "Eb4" "Gb4") "Gb" "Major")
; (chord-degree-analyze '("Db4" "Ab4" "Cb4" "F2") "Eb" "minor")




(defun chord-degree-analyze-2 (chord tonic scale)
            "Analyze the degree of a chord belonging to this key, and what chord is it 
             (chord can only contain a maximum of 4 notes, and it could be any arrangement!)"
            (let* (
                   (all-chords (generate-all-chords tonic scale))                   
                   (filter-1 (lambda (chords root-note)
                             (remove-if-not (lambda (chord-info)
                                              (equal (first (first chord-info)) root-note))
                                            chords)))      ; "Filter chords to include only those with the specified root note"
                   (filter-2 (lambda (chords single-chord)
                               (remove-if-not (lambda (chord-info)
                                                (= (length (first chord-info)) (length single-chord)))
                                              chords)))   
                   (chord-root-note (first chord))
                   (all-chords-1 (funcall filter-1 all-chords chord-root-note))
                   (all-chords-2 (funcall filter-2 all-chords-1 chord))
                   (result '())
                   )
              (loop for chord-info in all-chords-2 do
                      (let ((generated-chord (first chord-info)))
                        (when (every (lambda (note) (member note chord :test #'equal)) generated-chord)
              (push (car (cadr chord-info)) result))))
              (car result)))
          
; (chord-degree-analyze-2 '("C" "G" "Bb" "E") "C" "Major")
; (chord-degree-analyze-2 '("Ab" "C" "F#") "C" "Major")
; (chord-degree-analyze-2 '("A" "D" "F") "C" "Major")
; (chord-degree-analyze-2 '("C" "Ab" "Eb" "Gb") "Gb" "Major")
; (chord-degree-analyze-2 '("Db" "Ab" "Cb" "F") "Eb" "minor")




(defun repeated-note (triad tonic scale)
            "Confirm the repeated note of a triad
             <input1> : triad without octave information
             <input2> : tonic
             <input3> : minor/Major"
            (let* (
                   (quality (chord-degree-analyze-2 triad tonic scale))
                   (degree (if (or (string-equal quality "N6")
                                   (string-equal quality "b3VII6/V"))
                               quality
                             (car (split-chord-string quality))))
                   (inversion-num (if (or (string-equal quality "N6")
                                          (string-equal quality "b3VII6/V"))
                                      nil
                                    (string-to-number (cadr (split-chord-string quality)))))
                   )
              (cond ((string-equal degree "N6") (list (append triad (list (first triad)))))
                    ((string-equal degree "b3VII6/V") (list (append triad (list (second triad)))))
                    ((or (string-equal degree "I")
                         (string-equal degree "IV")
                         (string-equal degree "V"))
                     (cond ((= inversion-num 3) (list (append triad (list (first triad)))
                                                      (append triad (list (third triad)))))
                           ((= inversion-num 6) (list (append triad (list (third triad)))
                                                      (append triad (list (second triad)))))
                           ((= inversion-num 46) (list (append triad (list (first triad)))))))
                    ((string-equal degree "II") 
                     (cond ((= inversion-num 3) (list (append triad (list (second triad)))
                                                      (append triad (list (first triad)))
                                                      (append triad (list (third triad)))))
                           ((= inversion-num 6) (list (append triad (list (first triad)))
                                                      (append triad (list (third triad)))                
                                                      (append triad (list (second triad)))))))
                    ((string-equal degree "III")
                     (cond ((= inversion-num 3) (list (append triad (list (second triad)))))
                           ((= inversion-num 6) (list (append triad (list (first triad)))))))
                    ((string-equal degree "VI") 
                     (cond ((= inversion-num 3) (list (append triad (list (first triad)))
                                                      (append triad (list (second triad)))))
                           ((= inversion-num 6) (list (append triad (list (third triad)))
                                                      (append triad (list (first triad)))))))
                    ((string-equal degree "VII") (list (append triad (list (first triad))))))))

; (repeated-note '("D" "F" "B") "C" "Major")
; (repeated-note '("G#" "B" "E") "A" "minor")
; (repeated-note '("G#" "B" "E#") "F#" "Major")




(defun triad-arrangement (high-note chord tonic scale)
            "The triad without octave information are arranged lengthwise 
             to form a regular arrangement pattern (don't change quality!)
             <input1> : melodic note with octave information
             <input2> : triad without octave information
             <input3> : tonic
             <input4> : minor/Major"
            (let* (
                   (all-triad-with-repeated (repeated-note chord tonic scale))
                   )
              (om::flat (loop for triad in all-triad-with-repeated
                              collect (let* (
                                             (low-note (car triad))
                                             (high-note-2 (car (take-off-octave-info (list high-note))))
                                             (interior-part (remove-one high-note-2 (remove-one low-note triad)))
                                             (note-arrang-possible (list (append (append (list low-note) interior-part) (list high-note-2))
                                                                         (append (append (list low-note) (reverse interior-part)) (list high-note-2))))


                                             (soprano-oct (string-to-number (car (reverse (string-separator high-note)))))
                                             (alto-oct (list soprano-oct (1- soprano-oct)))
                                             (tenor-oct (list soprano-oct (1- soprano-oct) (- soprano-oct 2)))
                                             (bass-oct '(2 3))
                                             (satb-oct-possible (let (
                                                                      (lists (list (list soprano-oct) alto-oct tenor-oct bass-oct))
                                                                      (result '())
                                                                      )
                                                                  (loop for combination in (generate-combinations lists)
                                                                        do (when (is-descending combination)
                                                                             (push combination result)))
                                                                  result))
                                             (btas-oct-possible (mapcar #'reverse satb-oct-possible))


                                             (btas-oct-possible-1 (loop for a in btas-oct-possible
                                                                        collect (mapcar #'write-to-string a)))
                                             (all-btas-possible (om::flat (loop for a in note-arrang-possible
                                                                                collect
                                                                                  (loop for b in btas-oct-possible-1
                                                                                        collect (mapcar (lambda (x y) (concatenate 'string x y)) a b))) 1))
                                             )
                                        (remove-nils (loop for btas in all-btas-possible
                                                           collect (let (
                                                                         (bass (first btas))
                                                                         (tenor (second btas))
                                                                         (alto (third btas))
                                                                         (soprano (fourth btas))
                                                                         )
                                                                     (if (some #'null (list
                                                                                       (interval-identify-2 alto soprano) 
                                                                                       (interval-identify-2 tenor alto) 
                                                                                       (interval-identify-2 bass tenor)))
                                                                         '(nil)
                                                                       (if (and (every (lambda (x) (<= x 8)) 
                                                                                       (list 
                                                                                        (interval-sep-num (interval-identify-2 alto soprano))
                                                                                        (interval-sep-num (interval-identify-2 tenor alto))))
                                                                                (<= (interval-sep-num (interval-identify-2 bass tenor)) 15))
                                                                           btas
                                                                         '(nil)))))))) 1)))

; (triad-arrangement "E4" '("A" "C" "E") "A" "minor") 
; (triad-arrangement "G#4" '("B" "D" "G#") "A" "minor")
; (triad-arrangement "B4" '("D" "F" "B") "A" "minor")
; (triad-arrangement "A4" '("F=" "A" "D#") "A" "minor")
; (triad-arrangement "Bb4" '("D" "F=" "Bb") "A" "Major")
; (triad-arrangement "Dx5" '("F#" "A#" "Dx") "A#" "minor")




(defun seventh-arrangement (high-note chord)
            "The seventh chord without octave information are arranged lengthwise 
             to form a regular arrangement pattern (don't change quality!)
             <input1> : melodic note with octave information
             <input2> : seventh chord without octave information"
            (let* (
                   (low-note (car chord))
                   (high-note-2 (car (take-off-octave-info (list high-note))))                 
                   (interior-part (remove-one high-note-2 (remove-one low-note chord)))

                   (note-arrang-possible (list (append (append (list low-note) interior-part) (list high-note-2))
                                               (append (append (list low-note) (reverse interior-part)) (list high-note-2))))

                   (soprano-oct (string-to-number (car (reverse (string-separator high-note)))))
                   (alto-oct (list soprano-oct (1- soprano-oct)))
                   (tenor-oct (list soprano-oct (1- soprano-oct) (- soprano-oct 2)))
                   (bass-oct '(2 3))                                      
                   (satb-oct-possible (let (
                                            (lists (list (list soprano-oct) alto-oct tenor-oct bass-oct))
                                            (result '())
                                            )
                                        (loop for combination in (generate-combinations lists)
                                              do (when (is-descending combination)
                                                   (push combination result)))
                                        result))
                   (btas-oct-possible (mapcar #'reverse satb-oct-possible))


                   (btas-oct-possible-1 (loop for a in btas-oct-possible
                                              collect (mapcar #'write-to-string a)))
                   (all-btas-possible (om::flat (loop for a in note-arrang-possible
                                                      collect
                                                        (loop for b in btas-oct-possible-1
                                                              collect (mapcar (lambda (x y) (concatenate 'string x y)) a b))) 1))
                   )
              (remove-nils (loop for btas in all-btas-possible
                                 collect (let (
                                               (bass (first btas))
                                               (tenor (second btas))
                                               (alto (third btas))
                                               (soprano (fourth btas))
                                               )
                                           (if (some #'null (list
                                                             (interval-identify-2 alto soprano) 
                                                             (interval-identify-2 tenor alto) 
                                                             (interval-identify-2 bass tenor)))
                                               '(nil)
                                             (if (and (every (lambda (x) (<= x 8)) 
                                                             (list 
                                                              (interval-sep-num (interval-identify-2 alto soprano))
                                                              (interval-sep-num (interval-identify-2 tenor alto))))
                                                      (<= (interval-sep-num (interval-identify-2 bass tenor)) 15))
                                                 btas
                                               '(nil))))))))

; (seventh-arrangement "B=3" '("G" "B=" "Eb" "F"))
; (seventh-arrangement "Eb5" '("G" "B=" "Eb" "F"))
; (seventh-arrangement "B=4" '("G" "B=" "Eb" "F"))




(defun chord-arrangement (high-note chord tonic scale)
            "The chord without octave information are arranged lengthwise 
             to form a regular arrangement pattern (don't change quality!)
             <input1> : melodic note with octave information
             <input2> : chord without octave information  
             <input3> : tonic
             <input4> : Major/minor"
            (if (= (length chord) 4)
                (seventh-arrangement high-note chord)
              (triad-arrangement high-note chord tonic scale)))

; (chord-arrangement "B=4" '("D#" "F#" "B=") "A#" "minor")
; (chord-arrangement "B=4" '("G" "B=" "Eb" "F") "C" "minor")
; (chord-arrangement "Ab4" '("F" "Ab" "Cb" "Db") "Eb" "minor")
; (chord-arrangement "A4" '("A" "C" "E") "A" "minor")
; (chord-arrangement "A4" '("F" "A" "D") "A" "minor")
; (chord-arrangement "E4" '("E" "G#" "B") "A" "minor")
 


            
             




 



#|---------------------------------------------------------------------------------
-----------------------------------------------------------------------------------
-------------------------------5. Choral system------------------------------------
-----------------------------------------------------------------------------------
---------------------------------------------------------------------------------|#






;;---------------this is auxiliary functions-------------- 

(defun compound->simple (pitch-1 pitch-2)
  (let* (
         (note1-oct-str (car (reverse (string-separator pitch-1))))
         (note2-without-oct (reduce #'om::string+ (butlast (string-separator pitch-2))))
         (new-pitch-2 (if (equal (size? pitch-1 (om::string+ note2-without-oct note1-oct-str)) t)
                          (om::string+ note2-without-oct note1-oct-str)
                        (om::string+ note2-without-oct (write-to-string (+ (string-to-number note1-oct-str) 1)))))
         )
    (interval-identify-2 pitch-1 new-pitch-2)))

; (compound->simple "Bb2" "E=5")
; (compound->simple "Bb2" "Ebb8")
; (compound->simple "A#2" "Cx8")
; (compound->simple "C2" "Cb8")
; (compound->simple "C3" "C#3")
; (compound->simple "G3" "G#4")




(defun remove-oct-info (chord)
  (loop for note in chord
        collect (reduce #'om::string+ (butlast (pitch-info note)))))

; (remove-oct-info '("A3" "C4" "E4" "A4"))
; (remove-oct-info '("D3" "F3" "D4" "A4"))

;;-------------------------------------------------------- 




;;--------------------------------Chord connection rule-------------------------------------

(defun consecutive-58-judge (prior-chord latter-chord)
            "Determine whether the connection of two chords violates the consecutive fifth and octaves rule
             (output T if the rule is violated, NIL otherwise!)
             <input1> : chord-1 (The correctly arranged chord, with octave information) 
             <input2> : chord-2 (The correctly arranged chord, with octave information)"
            (let* (
                   (chord1-s-note (fourth prior-chord))
                   (chord1-a-note (third prior-chord))
                   (chord1-t-note (second prior-chord))
                   (chord1-b-note (first prior-chord))
                   (chord1-sa-quality (interval-identify-2 chord1-a-note chord1-s-note))
                   (chord1-st-quality (compound->simple chord1-t-note chord1-s-note))
                   (chord1-sb-quality (compound->simple chord1-b-note chord1-s-note))
                   (chord1-at-quality (interval-identify-2 chord1-t-note chord1-a-note))
                   (chord1-ab-quality (compound->simple chord1-b-note chord1-a-note))
                   (chord1-tb-quality (compound->simple chord1-b-note chord1-t-note))


                   (chord2-s-note (fourth latter-chord))
                   (chord2-a-note (third latter-chord))
                   (chord2-t-note (second latter-chord))
                   (chord2-b-note (first latter-chord))
                   (chord2-sa-quality (interval-identify-2 chord2-a-note chord2-s-note))
                   (chord2-st-quality (compound->simple chord2-t-note chord2-s-note))
                   (chord2-sb-quality (compound->simple chord2-b-note chord2-s-note))
                   (chord2-at-quality (interval-identify-2 chord2-t-note chord2-a-note))
                   (chord2-ab-quality (compound->simple chord2-b-note chord2-a-note))
                   (chord2-tb-quality (compound->simple chord2-b-note chord2-t-note))
                   )
              (cond ((and (and (string-equal chord1-sa-quality "p5") (string-equal chord2-sa-quality "p5"))
                          (not (and (string-equal chord1-s-note chord2-s-note) (string-equal chord1-a-note chord2-a-note))))
                     t)
                    ((and (and (string-equal chord1-sa-quality "p8") (string-equal chord2-sa-quality "p8"))
                          (not (and (string-equal chord1-s-note chord2-s-note) (string-equal chord1-a-note chord2-a-note))))
                     t)

                    ((and (and (string-equal chord1-st-quality "p5") (string-equal chord2-st-quality "p5"))
                          (not (and (string-equal chord1-s-note chord2-s-note) (string-equal chord1-t-note chord2-t-note))))
                     t)
                    ((and (and (string-equal chord1-st-quality "p8") (string-equal chord2-st-quality "p8"))
                          (not (and (string-equal chord1-s-note chord2-s-note) (string-equal chord1-t-note chord2-t-note))))
                     t)

                    ((and (and (string-equal chord1-sb-quality "p5") (string-equal chord2-sb-quality "p5"))
                          (not (and (string-equal chord1-s-note chord2-s-note) (string-equal chord1-b-note chord2-b-note))))
                     t)
                    ((and (and (string-equal chord1-sb-quality "p8") (string-equal chord2-sb-quality "p8"))
                          (not (and (string-equal chord1-s-note chord2-s-note) (string-equal chord1-b-note chord2-b-note))))
                     t)

                    ((and (and (string-equal chord1-at-quality "p5") (string-equal chord2-at-quality "p5"))
                          (not (and (string-equal chord1-a-note chord2-a-note) (string-equal chord1-t-note chord2-t-note))))
                     t)
                    ((and (and (string-equal chord1-at-quality "p8") (string-equal chord2-at-quality "p8"))
                          (not (and (string-equal chord1-a-note chord2-a-note) (string-equal chord1-t-note chord2-t-note))))
                     t)

                    ((and (and (string-equal chord1-ab-quality "p5") (string-equal chord2-ab-quality "p5"))
                          (not (and (string-equal chord1-a-note chord2-a-note) (string-equal chord1-b-note chord2-b-note))))
                     t)
                    ((and (and (string-equal chord1-ab-quality "p8") (string-equal chord2-ab-quality "p8"))
                          (not (and (string-equal chord1-a-note chord2-a-note) (string-equal chord1-b-note chord2-b-note))))
                     t)

                    ((and (and (string-equal chord1-tb-quality "p5") (string-equal chord2-tb-quality "p5"))
                          (not (and (string-equal chord1-t-note chord2-t-note) (string-equal chord1-b-note chord2-b-note))))
                     t)
                    ((and (and (string-equal chord1-tb-quality "p8") (string-equal chord2-tb-quality "p8"))
                          (not (and (string-equal chord1-t-note chord2-t-note) (string-equal chord1-b-note chord2-b-note))))
                     t)

                    (t nil))))

; (consecutive-58-judge '("A3" "C4" "E4" "A4") '("F3" "D4" "D4" "A4"))
; (consecutive-58-judge '("A2" "E3" "C4" "A4") '("F3" "D4" "D4" "A4"))
; (consecutive-58-judge '("A3" "C4" "E4" "A4") '("E3" "G#3" "B3" "E4"))
; (consecutive-58-judge '("F3" "D4" "D4" "A4") '("E3" "G#3" "E4" "B4"))
; (consecutive-58-judge '("F3" "D4" "D4" "A4") '("F3" "B3" "D4" "A4"))
; (consecutive-58-judge '("C3" "G3" "E4" "C5") '("G2" "D4" "G4" "B3"))
; (consecutive-58-judge '("C3" "C4" "G4" "E5") '("G2" "G4" "B4" "D5"))




(defun hidden-58-judge (prior-chord latter-chord)
            "Determine whether the connection of two chords violates the hidden fifth and octaves rule
             (output T if the rule is violated, NIL otherwise!)
             <input1> : chord-1 (The correctly arranged chord, with octave information) 
             <input2> : chord-2 (The correctly arranged chord, with octave information)"
            (let* (
                   (chord1-s-note (fourth prior-chord))
                   (chord1-b-note (first prior-chord))
                   (chord2-s-note (fourth latter-chord))
                   (chord2-b-note (first latter-chord))

                   (chord1-sb-quality (compound->simple chord1-b-note chord1-s-note))
                   (chord2-sb-quality (compound->simple chord2-b-note chord2-s-note))

                   (s1s2-size? (size? chord1-s-note chord2-s-note))
                   (b1b2-size? (size? chord1-b-note chord2-b-note))

                   (s1-s2-interval (if s1s2-size?
                                       (interval-identify-2 chord1-s-note chord2-s-note)
                                     (interval-identify-2 chord2-s-note chord1-s-note)))
                   )
              (and (not (string-equal chord1-sb-quality chord2-sb-quality))         
                   (or (string-equal chord2-sb-quality "p5") (string-equal chord2-sb-quality "p8"))
                   (> (string-to-number (reduce #'om::string+ (cdr (string-separator s1-s2-interval)))) 2)
                   (equal s1s2-size? b1b2-size?))))

; (hidden-58-judge '("C3" "G4" "C5" "E5") '("G3" "B4" "D5" "G5"))
; (hidden-58-judge '("G3" "D4" "G4" "B4") '("C4" "E4" "G4" "C5"))
; (hidden-58-judge '("C3" "G3" "C4" "E4") '("D3" "F3" "D4" "A4"))
; (hidden-58-judge '("C3" "C4" "E4" "G4") '("D3" "A3" "F4" "D5"))




(defun similar-motion-judge (prior-chord latter-chord)
            "Determine whether the connection of two chords violates the four-parts similar motion rule
             (output T if the rule is violated, NIL otherwise!)
             <input1> : chord-1 (The correctly arranged chord, with octave information) 
             <input2> : chord-2 (The correctly arranged chord, with octave information)"
            (let* (
                   (chord1-s-note (fourth prior-chord))
                   (chord1-a-note (third prior-chord))
                   (chord1-t-note (second prior-chord))
                   (chord1-b-note (first prior-chord))

                   (chord2-s-note (fourth latter-chord))
                   (chord2-a-note (third latter-chord))
                   (chord2-t-note (second latter-chord))
                   (chord2-b-note (first latter-chord))

                   (s1s2-size? (size? chord1-s-note chord2-s-note))
                   (a1a2-size? (size? chord1-a-note chord2-a-note))
                   (t1t2-size? (size? chord1-t-note chord2-t-note))
                   (b1b2-size? (size? chord1-b-note chord2-b-note))
                   )
              (cond ((and (equal s1s2-size? "=")
                          (equal a1a2-size? "=")
                          (equal t1t2-size? "=")
                          (equal b1b2-size? "="))
                     nil)
                    ((and (not (equal s1s2-size? "="))
                          (eq s1s2-size? a1a2-size?)
                          (eq s1s2-size? t1t2-size?)
                          (eq s1s2-size? b1b2-size?)) 
                     t)
                    (t nil))))

; (similar-motion-judge '("C3" "E3" "C4" "G4") '("G3" "B3" "D4" "G4"))
; (similar-motion-judge '("C3" "G4" "C5" "E5") '("G3" "B4" "D5" "G5"))
; (similar-motion-judge '("C3" "G3" "C4" "E4") '("D3" "F3" "D4" "A4"))
; (similar-motion-judge '("D3" "F3" "D4" "A4") '("C3" "E3" "C4" "G4"))
; (similar-motion-judge '("C3" "E3" "C4" "G4") '("C3" "E3" "C4" "G4"))
; (similar-motion-judge '("C3" "G3" "E4" "G4") '("D3" "D4" "F4" "D5"))
; (similar-motion-judge '("C3" "G3" "E4" "G4") '("D3" "A3" "F4" "F4"))




(defun voice-overlapping-judge (prior-chord latter-chord)
            "Determine whether the connection of two chords violates the voice overlapping rule
             (output T if the rule is violated, NIL otherwise!)
             <input1> : chord-1 (The correctly arranged chord, with octave information) 
             <input2> : chord-2 (The correctly arranged chord, with octave information)"
            (let (
                  (chord1-s-note (fourth prior-chord))
                  (chord1-a-note (third prior-chord))
                  (chord1-t-note (second prior-chord))
                  (chord1-b-note (first prior-chord))

                  (chord2-s-note (fourth latter-chord))
                  (chord2-a-note (third latter-chord))
                  (chord2-t-note (second latter-chord))
                  (chord2-b-note (first latter-chord))
                  )
              (cond ((eq (size? chord1-s-note chord2-a-note) t)
                     t)
                    ((eq (size? chord1-a-note chord2-s-note) nil)
                     t)
                    ((eq (size? chord1-t-note chord2-b-note) t)
                     t)
                    ((eq (size? chord1-b-note chord2-t-note) nil)
                     t)
                    (t nil))))

; (voice-overlapping-judge '("C3" "E3" "C4" "G4") '("G3" "B3" "D4" "G4"))
; (voice-overlapping-judge '("C3" "G3" "E4" "G4") '("D3" "A3" "F4" "F4"))
; (voice-overlapping-judge '("C3" "G3" "E4" "G4") '("D3" "A4" "A4" "F5"))
; (voice-overlapping-judge '("C3" "G3" "C4" "E4") '("D3" "F3" "D4" "A4"))

           


(defun augmented-interval-judge (prior-chord latter-chord)
            "Determine whether the connection of two chords appears the augmented interval, except augmented-unison
             (output T if the augmented interval appeared, NIL otherwise!)
             <input1> : chord-1 (The correctly arranged chord, with octave information) 
             <input2> : chord-2 (The correctly arranged chord, with octave information)"
            (let* (
                   (chord1-s-note (fourth prior-chord))
                   (chord1-a-note (third prior-chord))
                   (chord1-t-note (second prior-chord))
                   (chord1-b-note (first prior-chord))

                   (chord2-s-note (fourth latter-chord))
                   (chord2-a-note (third latter-chord))
                   (chord2-t-note (second latter-chord))
                   (chord2-b-note (first latter-chord))  

                   (s1s2-quality (if (size? chord1-s-note chord2-s-note)
                                     (compound->simple chord1-s-note chord2-s-note)
                                   (compound->simple chord2-s-note chord1-s-note)))
                   (a1a2-quality (if (size? chord1-a-note chord2-a-note)
                                     (compound->simple chord1-a-note chord2-a-note)
                                   (compound->simple chord2-a-note chord1-a-note)))
                   (t1t2-quality (if (size? chord1-t-note chord2-t-note)
                                     (compound->simple chord1-t-note chord2-t-note)
                                   (compound->simple chord2-t-note chord1-t-note)))
                   (b1b2-quality (if (size? chord1-b-note chord2-b-note)
                                     (compound->simple chord1-b-note chord2-b-note)
                                   (compound->simple chord2-b-note chord1-b-note)))
                   )
              (if (and (or (string= (car (string-separator s1s2-quality)) "a")
                           (string= (car (string-separator a1a2-quality)) "a")
                           (string= (car (string-separator t1t2-quality)) "a")
                           (string= (car (string-separator b1b2-quality)) "a"))
                       (not (or (string= s1s2-quality "a1")
                                (string= a1a2-quality "a1")
                                (string= t1t2-quality "a1")
                                (string= b1b2-quality "a1"))))
                  t nil)))

; (augmented-interval-judge '("F3" "C4" "F4" "A4") '("B3" "D4" "G4" "G4"))
; (augmented-interval-judge '("F3" "A3" "F4" "F4") '("G3" "D4" "G4" "B4"))
; (augmented-interval-judge '("C3" "G3" "C4" "E4") '("D3" "F3" "D4" "A4"))
; (augmented-interval-judge '("C3" "G3" "E4" "G4") '("D3" "A4" "A4" "F5"))




(defun false-relation-judge (prior-chord latter-chord)
            "Determine whether the connection of two chords violates the false relation rule
             (output T if the rule is violated, NIL otherwise!)
             <input1> : chord-1 (The correctly arranged chord, with octave information) 
             <input2> : chord-2 (The correctly arranged chord, with octave information)"
            (let* (
                   (chord1-s-note (fourth prior-chord))
                   (chord1-a-note (third prior-chord))
                   (chord1-t-note (second prior-chord))
                   (chord1-b-note (first prior-chord))
                   (s1-name (car (string-separator chord1-s-note)))
                   (a1-name (car (string-separator chord1-a-note)))
                   (t1-name (car (string-separator chord1-t-note)))
                   (b1-name (car (string-separator chord1-b-note)))
                   (satb1-name (list (list s1-name 1) (list a1-name 2) (list t1-name 3) (list b1-name 4)))

                   (chord2-s-note (fourth latter-chord))
                   (chord2-a-note (third latter-chord))
                   (chord2-t-note (second latter-chord))
                   (chord2-b-note (first latter-chord))
                   (s2-name (car (string-separator chord2-s-note)))
                   (a2-name (car (string-separator chord2-a-note)))
                   (t2-name (car (string-separator chord2-t-note)))
                   (b2-name (car (string-separator chord2-b-note)))
                   (satb2-name (list (list s2-name 1) (list a2-name 2) (list t2-name 3) (list b2-name 4)))

                 
                   (maker (loop for note1 in satb1-name
                                nconc (loop for note2 in satb2-name
                                            when (string= (car note1) (car note2))
                                              collect (list (cadr note1) (cadr note2)))))   ; Mark which two notes have the same pitch name
                   (diff-maker-idx (loop for idx from 0
                                         for pair in maker
                                         thereis (when (not (= (first pair) (second pair)))
                                                   idx)
                                         finally (return nil)))
                   )

              (if diff-maker-idx
                  (let* (
                         (diff-maker (nth diff-maker-idx maker))
                         (note1 (cond ((= (first diff-maker) 1) chord1-s-note)
                                      ((= (first diff-maker) 2) chord1-a-note)
                                      ((= (first diff-maker) 3) chord1-t-note)
                                      ((= (first diff-maker) 4) chord1-b-note)))
                         (note2 (cond ((= (second diff-maker) 1) chord2-s-note)
                                      ((= (second diff-maker) 2) chord2-a-note)
                                      ((= (second diff-maker) 3) chord2-t-note)
                                      ((= (second diff-maker) 4) chord2-b-note)))
                         )
                    (cond ((and (null (second (pitch-info note1))) (string= (second (pitch-info note2)) "="))
                           nil)
                          ((and (null (second (pitch-info note2))) (string= (second (pitch-info note1)) "="))
                           nil)
                          ((string= (second (pitch-info note1)) (second (pitch-info note2)))
                           nil)
                          (t t)))
                nil)))

; (false-relation-judge '("D3" "B3" "F#4" "D5") '("F3" "B3" "G4" "D5"))
; (false-relation-judge '("F#3" "A3" "D4" "C5") '("G3" "B3" "D4" "F4"))
; (false-relation-judge '("C3" "G3" "C4" "E4") '("D3" "F3" "D4" "A4"))
; (false-relation-judge '("F3" "A3" "F4" "F4") '("G3" "D4" "G4" "B4"))



;;--------------------------------Chord connection judge-----------------------------------



;;---------------this is auxiliary functions-------------- 

(defun smooth? (prior-part latter-part)
            "Determine whether the corresponding voice types in the preceding and following chords move smoothly
             (output T if the movement is smooth, NIL otherwise!)
             <input1> : part in the prior chord (note with octave information)
             <input2> : part in the latter chord (note with octave information)"
            (let* (
                   (n1n2-size (size? prior-part latter-part))
                   (interval (if n1n2-size 
                                 (interval-identify-2 prior-part latter-part)
                               (interval-identify-2 latter-part prior-part)))
                   (degree (reduce #'om::string+ (cdr (string-separator interval))))
                   )
              (if (null interval)
                  nil
                (if (<= (string-to-number degree) 3)
                    t nil))))

; (smooth? "B#3" "C=4")
; (smooth? "A#3" "Bb3")
; (smooth? "Bb3" "A#3")
; (smooth? "C4" "C4")
; (smooth? "Bx3" "Db4")
; (smooth? "B#3" "Db4")
; (smooth? "A4" "D5")


(defun less-octave? (prior-part latter-part)
            "Determine whether the corresponding voice types in the preceding and following chords move smoothly
             (output T if the movement is smooth, NIL otherwise!)
             <input1> : part in the prior chord (note with octave information)
             <input2> : part in the latter chord (note with octave information)"
            (let* (
                   (n1n2-size (size? prior-part latter-part))
                   (interval (if n1n2-size 
                                 (interval-identify-2 prior-part latter-part)
                               (interval-identify-2 latter-part prior-part)))
                   (degree (reduce #'om::string+ (cdr (string-separator interval))))
                   )
              (if (null interval)
                  nil
                (if (<= (string-to-number degree) 8)
                    t nil))))

; (less-octave? "B#3" "C=4")
; (less-octave? "A#3" "Bb3")
; (less-octave? "Bb3" "A#3")
; (less-octave? "C4" "C4")
; (less-octave? "Bx3" "Db4")
; (less-octave? "B#3" "Db4")
; (less-octave? "A4" "C6")
; (less-octave? "C6" "A4")


(defun same-chord? (prior-chord latter-chord)
            "Determine whether two chords are the same chord
             <input1> : prior-chord (The correctly arranged chord, with octave information) 
             <input2> : latter-chord (The correctly arranged chord, with octave information)"
            (let* (
                   (prior-notes (remove-oct-info prior-chord))
                   (latter-notes (remove-oct-info latter-chord))
                   )
              (loop for note in prior-notes
                    always (find note latter-notes :test #'equal))))

; (same-chord? '("C3" "G3" "C4" "E4") '("C3" "G3" "E4" "G4"))
; (same-chord? '("C3" "G3" "C4" "E4") '("E3" "G3" "C4" "C4"))
; (same-chord? '("C3" "G3" "C4" "E4") '("D3" "F3" "D4" "A4"))
; (same-chord? '("D3" "A3" "D4" "F4") '("F3" "A3" "D4" "D4"))
; (same-chord? '("D3" "A3" "D4" "F4") '("F3" "A3" "D4" "F4"))

;;-------------------------------------------------------- 



(defun chord-connection-judge (prior-chord latter-chord)
            "Determine if the two chords are connected correctly
             (output T if the connection is correct, NIL otherwise!)
             <input1> : prior-chord (The correctly arranged chord, with octave information) 
             <input2> : latter-chord (The correctly arranged chord, with octave information)"
            (let* (
                   (a1 (third prior-chord))
                   (t1 (second prior-chord))
                   (b1 (first prior-chord))
                   (a2 (third latter-chord))
                   (t2 (second latter-chord))
                   (b2 (first latter-chord))
                   )
              (if (same-chord? prior-chord latter-chord)
                  (and (less-octave? a1 a2)        
                       (less-octave? t1 t2)
                       (less-octave? b1 b2))
                (and (smooth? a1 a2)
                     (smooth? t1 t2)
                     (less-octave? b1 b2)))))

; (chord-connection-judge '("F3" "C4" "F4" "A4") '("B3" "D4" "G4" "G4"))
; (chord-connection-judge '("C3" "G3" "C4" "E4") '("D3" "F3" "D4" "A4"))
; (chord-connection-judge '("C3" "G3" "E4" "G4") '("D3" "A4" "A4" "F5"))
; (chord-connection-judge '("C3" "E3" "C4" "G4") '("G3" "B3" "D4" "G4"))
; (chord-connection-judge '("C3" "G3" "E4" "G4") '("D3" "A3" "F4" "F4"))
; (chord-connection-judge '("C3" "G3" "E4" "G4") '("D3" "A3" "G4" "F5"))
; (chord-connection-judge '("C3" "G3" "C4" "E4") '("D3" "F3" "D4" "A4"))




(defun valid? (prior-chord latter-chord)
             "Determine if the two chords meet all the rules and correct connection.
             (output T if the connection is valid, NIL otherwise!)
             <input1> : chord-1 (The correctly arranged chord, with octave information) 
             <input2> : chord-2 (The correctly arranged chord, with octave information)"
             (if (null prior-chord)
                 t
               (and (not (consecutive-58-judge prior-chord latter-chord))
                    (not (hidden-58-judge prior-chord latter-chord))
                    (not (similar-motion-judge prior-chord latter-chord))
                    (not (voice-overlapping-judge prior-chord latter-chord))
                    (not (augmented-interval-judge prior-chord latter-chord))
                    (not (false-relation-judge prior-chord latter-chord))
                    (chord-connection-judge prior-chord latter-chord))))




; (valid? '("F3" "C4" "F4" "A4") '("B3" "D4" "G4" "G4"))
; (valid? '("C3" "G3" "C4" "E4") '("D3" "F3" "D4" "A4"))
; (valid? '("C3" "G3" "E4" "G4") '("D3" "A4" "A4" "F5"))
; (valid? '("C3" "E3" "C4" "G4") '("G3" "B3" "D4" "G4"))
; (valid? '("C3" "G3" "E4" "G4") '("D3" "A3" "F4" "F4"))
; (valid? '("C3" "G3" "E4" "G4") '("D3" "A3" "G4" "F5"))
; (valid? '("C3" "G3" "C4" "E4") '("D3" "F3" "D4" "A4"))

; (valid? '("C3" "C4" "E4" "G4") '("F3" "A3" "D4" "F4"))

; (valid? nil '("F3" "A3" "D4" "F4"))
; (valid? '("F3" "A3" "D4" "F4") '("F3" "A3" "D4" "F4"))






;;-----------------------------------Chord library filter-----------------------------------



;;---------------this is auxiliary functions--------------
(defun secondary-str? (degree-str)
            "Determines whether a degree string is a secondary degree
             (output T if the string is a secondary degree, NIL otherwise!)"
            (if (find (car (last (string-separator degree-str))) '("2" "3" "4" "5" "6" "7" "9") :test #'equal)
                nil t))
; (secondary-str? "IV3")
; (secondary-str? "N6")
; (secondary-str? "V56/II")
; (secondary-str? "b3VII6/V")    
; (secondary-str? "V13")    


(defun function? (chord-info)
            "Analyze the function to which the raw chord belongs
             <input1> : chord-info, e.g. '((D F A) (II3)) (inside the parentheses are all string types!!)"
            (let* (
                   (chord-maker (car (cadr chord-info)))
                   (type? (secondary-str? chord-maker))
                   (degree (if type?
                               (cadr (degree-separator-pro chord-maker))
                             (car (split-chord-string chord-maker))))
                   (function (cond ((string= degree "I") 1)    ; 1 indicates tonic function (1 = T)
                                   ((or (string= degree "II") (string= degree "IV") (string= degree "N")) 2)    ; 2 indicates subdominant function (2 = S)
                                   ((or (string= degree "V") (string= degree "bV") (string= degree "#V")
                                        (string= degree "VII") (string= degree "bVII") (string= degree "#VII")) 3)    ; 3 indicates dominant function (3 = D)
                                   ((string= degree "III") 13)    ; 13 indicates that combination of tonic and dominant function (13 = T&D)
                                   ((string= degree "VI") 12)))    ; 12 indicates that combination of tonic and subdominant function (12 = T&S)
                   )
              (if type?
                  (list function degree)
                function)))
; (function? '(("D" "F" "A") ("II3")))
; (function? '(("C" "Eb" "F#" "A") ("VII34/III")))
; (function? '(("C" "E" "A") ("N6")))
; (function? '(("F#" "A#" "C#" "Dx") ("b3VII56/V")))

; (function? '(("G" "B=" "F" "Ab") ("V9")))
; (function? '(("Db" "F" "G" "B=") ("b5V34")))
; (function? '(("F" "G" "B=" "D#") ("#5V2")))
; (function? '(("B=" "Db" "F" "Ab") ("b3VII7")))
; (function? '(("Ab" "B=" "D#" "F") ("#3VII2")))


(defun func-1 (double-layer-list)
  (car (cadr double-layer-list)))
; (func-1 '(("D" "F" "A") ("II3")))
; (func-1 '(("F#" "A#" "C#" "Dx") ("b3VII56/V")))


(defun func-2 (double-layer-list)
  (let ((degree (function? double-layer-list)))
    (if (listp degree)
        (car degree)
      degree)))
; (func-2 '(("D" "F" "A") ("I3")))
; (func-2 '(("A" "C" "E") ("VI3")))
; (func-2 '(("F#" "A#" "C#" "Dx") ("b3VII56/V")))
; (func-2 '(("D#" "F#" "A" "C") ("VII7/II")))


(defun func-3 (double-layer-list)
  (car (split-chord-string (func-1 double-layer-list))))
; (func-3 '(("D" "F" "A") ("I3")))
; (func-3 '(("A" "C" "E") ("VI3")))
; (func-3 '(("Ab" "B=" "D#" "F") ("#3VII2")))
; (func-3 '(("Db" "F" "G" "B=") ("b5V34")))
; (func-3 '(("F" "G" "B=" "D#") ("#5V2")))
; (func-3 '(("B=" "Db" "F" "Ab") ("b3VII7")))

;;-------------------------------------------------------- 




(defun function-filter (1-chord-info 2-chord-library)
            "The latter chord library is filtered according to the function of the previous chord
             <input1> : chord-1 (The raw chord without octave information) 
             <input2> : chord-2-library (The raw chord library without octave information)"
            (if (null 1-chord-info)
                2-chord-library
              (let ((1-chord-func (function? 1-chord-info)))
                (if (listp 1-chord-func)
                    (let* (
                           (last-degree (cadr 1-chord-func))
                           (maybe-last-note-1 (om::string+ last-degree "3"))
                           (maybe-last-note-2 (om::string+ last-degree "6"))
                           (yn-1? (find maybe-last-note-1 2-chord-library :key #'func-1 :test #'equal))
                           (yn-2? (find maybe-last-note-2 2-chord-library :key #'func-1 :test #'equal))
                           )
                      (remove nil (list yn-1? yn-2?)))
                  (cond ((= 1-chord-func 1) 2-chord-library)
                        ((= 1-chord-func 2) 2-chord-library)
                        ((= 1-chord-func 3) (remove-if (lambda (num) 
                                                         (if (= num 2) t nil)) 2-chord-library :key #'func-2))
                        ((= 1-chord-func 13) 2-chord-library)
                        ((= 1-chord-func 12) (remove-if (lambda (num) 
                                                          (if (= num 1) t nil)) 2-chord-library :key #'func-2)))))))




(defun upbeat-filter (1-chord-info 2-chord-library)
            "Specially used on upbeats (Except for the initial pitch)!!!!
             The latter chord library is filtered according to the function of the previous chord!
             (Prevent the harmonic syncopation and secondary chord!)
             <input1> : chord-1 (The raw chord without octave information in the DOWNBEAT) 
             <input2> : chord-2-library (The raw chord library without octave information)"
            (if (null 1-chord-info)
                2-chord-library
              (let ((1-chord-func (function? 1-chord-info)))
                (if (listp 1-chord-func)
                    (let* (
                           (last-degree (cadr 1-chord-func))
                           (maybe-last-note-1 (om::string+ last-degree "3"))
                           (maybe-last-note-2 (om::string+ last-degree "6"))
                           (yn-1? (find maybe-last-note-1 2-chord-library :key #'func-1 :test #'equal))
                           (yn-2? (find maybe-last-note-2 2-chord-library :key #'func-1 :test #'equal))
                           )
                      (remove nil (list yn-1? yn-2?)))

                  (let* (
                         (1-chord-name (car (split-chord-string (func-1 1-chord-info))))
                         (new-2chord-library (remove-if (lambda (str) 
                                                          (if (string-equal str 1-chord-name) t nil)) 2-chord-library :key #'func-3))
                         )
                    (remove-if #'secondary-str? (cond ((= 1-chord-func 1) new-2chord-library)
                                                      ((= 1-chord-func 2) new-2chord-library)
                                                      ((= 1-chord-func 3) (remove-if (lambda (num) 
                                                                                       (if (= num 2) t nil)) new-2chord-library :key #'func-2))
                                                      ((= 1-chord-func 13) new-2chord-library)
                                                      ((= 1-chord-func 12) (remove-if (lambda (num) 
                                                                                        (if (= num 1) t nil)) new-2chord-library :key #'func-2))) :key #'func-1))))))




#|
; (search-soprano "C5" "C" "Major")
; (search-soprano "E5" "C" "Major")
; (search-soprano "G4" "C" "Major")

(setf 
    eg1 '((("C" "E" "G") ("I3")) (("F" "A" "C") ("IV3")) (("A" "C" "E") ("VI3")) (("E" "G" "C") ("I6")) (("A" "C" "F") ("IV6")) (("C" "E" "A") ("VI6")) (("G" "C" "E") ("I46")) (("C" "F" "A") ("IV46")) (("D" "F" "A" "C") ("II7")) (("F" "A" "C" "D") ("II56")) (("A" "C" "D" "F") ("II34")) (("Ab" "C" "F#") ("b3VII6/V")) (("Ab" "C" "Eb" "F#") ("b3VII56/V")) (("Ab" "C" "D" "F#") ("b5V34/V")) (("D#" "F#" "A" "C") ("VII7/III")) (("F#" "A" "C" "D#") ("VII56/III")) (("A" "C" "D#" "F#") ("VII34/III")) (("E" "G" "Bb" "C") ("V56/IV")) (("G" "Bb" "C" "E") ("V34/IV")) (("Bb" "C" "E" "G") ("V2/IV")) (("D" "F#" "A" "C") ("V7/V")) (("F#" "A" "C" "D") ("V56/V")) (("A" "C" "D" "F#") ("V34/V")) (("F#" "A" "C" "Eb") ("VII7/V")) (("A" "C" "Eb" "F#") ("VII56/V")) (("Eb" "F#" "A" "C") ("VII2/V")))

    eg2 '((("C" "E" "G") ("I3")) (("A" "C" "E") ("VI3")) (("G" "B" "E") ("III6")) (("C" "E" "A") ("VI6")) (("G" "C" "E") ("I46")) (("G" "B" "E" "F") ("V13")) (("A" "C#" "E" "G") ("V7/II")) (("C#" "E" "G" "A") ("V56/II")) (("G" "A" "C#" "E") ("V2/II")) (("C#" "E" "G" "Bb") ("VII7/II")) (("G" "Bb" "C#" "E") ("VII34/II")) (("Bb" "C#" "E" "G") ("VII2/II")) (("C" "E" "G" "Bb") ("V7/IV")) (("G" "Bb" "C" "E") ("V34/IV")) (("Bb" "C" "E" "G") ("V2/IV")) (("G" "Bb" "Db" "E") ("VII56/IV")) (("Bb" "Db" "E" "G") ("VII34/IV")) (("Db" "E" "G" "Bb") ("VII2/IV")) (("G#" "B" "D" "E") ("V56/VI")) (("B" "D" "E" "G#") ("V34/VI")) (("D" "E" "G#" "B") ("V2/VI")))

    eg3 '((("C" "E" "G") ("I3")) (("E" "G" "B") ("III3")) (("G" "B" "D") ("V3")) (("E" "G" "C") ("I6")) (("G" "B" "E") ("III6")) (("B" "D" "G") ("V6")) (("G" "C" "E") ("I46")) (("D" "G" "B") ("V46")) (("B" "D" "F" "G") ("V56")) (("D" "F" "G" "B") ("V34")) (("F" "G" "B" "D") ("V2")) (("B" "Db" "F" "G") ("b5V56")) (("Db" "F" "G" "B") ("b5V34")) (("F" "G" "B" "Db") ("b5V2")) (("B" "D#" "F" "G") ("#5V56")) (("D#" "F" "G" "B") ("#5V34")) (("F" "G" "B" "D#") ("#5V2")) (("A" "C#" "E" "G") ("V7/II")) (("C#" "E" "G" "A") ("V56/II")) (("E" "G" "A" "C#") ("V34/II")) (("C#" "E" "G" "Bb") ("VII7/II")) (("E" "G" "Bb" "C#") ("VII56/II")) (("Bb" "C#" "E" "G") ("VII2/II")) (("C" "E" "G" "Bb") ("V7/IV")) (("E" "G" "Bb" "C") ("V56/IV")) (("Bb" "C" "E" "G") ("V2/IV")) (("E" "G" "Bb" "Db") ("VII7/IV")) (("Bb" "Db" "E" "G") ("VII34/IV")) (("Db" "E" "G" "Bb") ("VII2/IV"))))


; (function-filter '(("A" "C" "D#" "F#") ("VII34/III")) eg2)
; (length (function-filter '(("A" "C" "E") ("VI3")) eg1)) 
; (function-filter '(("G" "B" "D") ("V3")) eg1)

; (upbeat-filter '(("C" "E" "G") ("I3")) eg2)
; (length (upbeat-filter '(("A" "C" "E") ("VI3")) eg1))
; (upbeat-filter '(("C" "E" "G") ("I3")) eg3)

|#




(defun filter-chords (chord-library prev-chord is-accent)
  (if is-accent
      (upbeat-filter chord-library prev-chord)
    (function-filter chord-library prev-chord)))

; (filter-chords '(("A" "C" "E") ("VI3")) eg1 t)
; (filter-chords '(("A" "C" "D#" "F#") ("VII34/III")) eg2 nil)
; (length (filter-chords nil eg2 1))   (length eg2)







;;------------harmonize the given melody and provide a roman-numeral analysis---------------


(om::defmethod! harmonize-melody (melody tonic mode accents)
         :icon 40041
         :menuins '((2 (("Major" "Major") ("minor" "minor"))))
         :doc "Harmonize a given melody into four-part harmony
               <input1>: melody (pitch-string-list)
               <input2>: tonic
               <input3>: Major or minor scale
               <input4>: upbeat position, e.g. (0 4 8....., Count from 0)"
         (let (
               (harmony '())
               (melody-length (length melody)) 
               (chord-cache (make-hash-table))
               )  

           (loop for note in melody
                 for idx from 0
                 do (setf (gethash idx chord-cache)
                          (search-soprano note tonic mode)))

           (let (
                 (idx 0)
                 (prev-chord nil)
                 (prev-chord-info nil)
                 (max-tries 500)
                 ) 

             (loop while (< idx melody-length)
                   do (let* (
                             (note (nth idx melody))
                             (is-accent (member idx accents))
                             (candidate-chords (gethash idx chord-cache))
                             (filtered-chords (filter-chords prev-chord-info candidate-chords is-accent))
                             (found nil)
                             (tries 0)
                             )

                        (loop for chord-info in filtered-chords
                              until found

                              do (loop for arrangement in (chord-arrangement note (first chord-info) tonic mode)
                                       until (or found (>= tries max-tries))
                                       when (valid? prev-chord arrangement)
                                         do (setq found t
                                                  prev-chord-info chord-info
                                                  prev-chord arrangement
                                                  harmony (cons arrangement harmony))
                                       do (incf tries)))

                        (unless found
                          (push prev-chord harmony))
                        (setq idx (1+ idx))))
             (reverse harmony))))




(defun roman-numeral-analyze (harmony tonic scale)
            "Provides a roman-numeral analysis
             <input1>: harmony (chord-list)
             <input2>: tonic
             <input3>: mode (Major or minor)"
            (loop for chord in harmony
                  collect (let* (
                                 (chord-2 (take-off-octave-info chord))
                                 (root-note (nth 0 chord-2))
                                 (chord-3 (remove-duplicates chord-2 :test #'equal))
                                 (idx (position root-note chord-3 :test #'equal))
                                 (original-chord (if (= idx 0)
                                                     chord-3
                                                   (cons root-note 
                                                         (append (subseq chord-3 0 idx) (subseq chord-3 (1+ idx))))))
                                 )
                            (chord-degree-analyze-2 original-chord tonic scale))))
     

;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------
;;-------------------------------copyright © 2024 Hangzhong LIAN----------------------------
;;-------------------------------------All rights reserved.---------------------------------
;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------
;;------------------------------------------------------------------------------------------

