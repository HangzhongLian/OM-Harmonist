;;;===================================================
;;;
;;; OM-Harmonist
;;; 
;;; OM-Harmonist is a library for OpenMusic
;;; It is primarily designed for automatic part-writing 
;;; and harmonic analysis, along with several auxiliary 
;;; functions.
;;;
;;; LIBRARY MAIN FILE
;;; Author: Hangzhong Lian
;;; 
;;; Contact: 
;;; hangzhonglian@gmail.com
;;; 
;;;===================================================

(defpackage :harm) 

(in-package :harm)

;--------------------------------------------------
;Loading files 
;--------------------------------------------------

(mapc 'om::compile&load 
      (list
       (make-pathname  :directory (append (pathname-directory *load-pathname*) (list "sources")) :name "harmonist" :type "lisp")
       ))



(om::fill-library 
 '(("Harmonist" 
    (("Data IO system" 
      (("data input" nil nil (pitch-receiver pitch-info pitch->midic pitch->display multiple-pitch->midic multiple-pitch->display) nil)
       ("data output" nil nil (display->pitch multiple-display->pitch multiple-chord->midic multiple-chord->display) nil))
      nil nil nil)

     ("Interval system"
      (("interval maker" nil nil (simple-interval-maker compound-interval-maker interval-maker) nil)
       ("interval identify" nil nil (simple-interval-identify simple-interval-identify-2 interval-identify interval-identify-2) nil))
      nil nil nil)

     ("Tonality system" 
      (("scale production" nil nil (major-scale-production minor-scale-production scale-production-midic mode-1 mode-2) nil)
       ("tonality judgment" nil nil (analyze-melody-key) nil)
       ("other tools" nil nil (scale->midic scale->display take-off-octave-info) nil))
      nil nil nil)

     ("Chord system" 
      (("chord maker" nil nil (triad-maker seventh-maker chord-degree extended-degree dominant-extended-degree 
                                           secondary-degree generate-all-chords generate-common-used-chords) nil)
       ("chord analyze" nil nil (search-soprano chord-degree-analyze) nil)
       ("chord voicing" nil nil (triad-arrangement seventh-arrangement chord-arrangement) nil))
      nil nil nil)

     ("Choral system"
      (("chord connection rules" nil nil (consecutive-58-judge hidden-58-judge similar-motion-judge voice-overlapping-judge augmented-interval-judge
                                                               false-relation-judge smooth? less-octave? chord-connection-judge valid?) nil)
       ("harmonization" nil nil (harmonize-melody roman-numeral-analyze) nil))
      nil nil nil))
    nil nil nil)))


(print "
;;;===================================================
;;;
;;; OM-Harmonist
;;; 
;;; OM-Harmonist is a library for OpenMusic
;;; It is primarily designed for automatic part-writing 
;;; and harmonic analysis, along with several auxiliary 
;;; functions.
;;;
;;; LIBRARY MAIN FILE
;;; Author: Hangzhong Lian
;;; 
;;; Contact: 
;;; hangzhonglian@gmail.com
;;; 
;;;===================================================
")
       






