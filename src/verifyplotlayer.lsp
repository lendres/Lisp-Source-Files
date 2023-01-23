(defun C:VERIFYPLOTLAYER ()
    (vla-runmacro (vlax-get-acad-object) "verifyplotlayer.dvb!verifyplotlayer")
    (princ)
) ;_ End defun

;|«Visual LISP© Format Options»
(72 4 40 2 T "End " 60 9 1 2 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
