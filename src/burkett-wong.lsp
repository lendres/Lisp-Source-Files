(vl-load-com)


(defun C:S1 ()
    (STRUCT-LAYER-ON "S1-*")
) ;_ End defun

(defun C:S2 ()
    (STRUCT-LAYER-ON "S2-*")
) ;_ End defun

(defun C:S3 ()
    (STRUCT-LAYER-ON "S3-*")
) ;_ End defun

(defun C:S4 ()
    (STRUCT-LAYER-ON "S4-*")
) ;_ End defun

(defun C:SM ()
    (STRUCT-LAYER-ON "S5-*")
) ;_ End defun

(defun C:S5 ()
    (STRUCT-LAYER-ON "S5-*")
) ;_ End defun

(defun C:S6 ()
    (STRUCT-LAYER-ON "S6-*")
) ;_ End defun

(defun C:SR ()
    (STRUCT-LAYER-ON "SR-*")
) ;_ End defun

(defun C:SR2 ()
    (STRUCT-LAYER-ON "SR2-*")
) ;_ End defun

(defun STRUCT-LAYER-ON (LAYER)
    (LAYERS-WC-ON-OFF :vlax-false "S1-*,S2-*,S3-*,S4-*,S5-*,S6-*,SR-*,SR2-*")
    (LAYERS-WC-ON-OFF :vlax-true LAYER)
) ;_ End defun

(defun LAYERS-WC-ON-OFF (VLAX-ON WCPATTERN / CMD item LAYERS VL-ACAD VL-ADOC)
    (setq VL-ACAD (vlax-get-acad-object)
	  VL-ADOC (vla-get-activedocument VL-ACAD)
	  CMD	  (vla-getvariable VL-ADOC "cmdecho")
	  LAYERS  (vla-get-layers VL-ADOC)
    ) ;_ End setq


    (vla-startundomark VL-ADOC)
    (vla-setvariable VL-ADOC "cmdecho" 0)

    (vlax-for item LAYERS
	(if (wcmatch (vlax-get-property item 'Name)
		     WCPATTERN
	    ) ;_ End wcmatch
	    (vla-put-layeron item VLAX-ON)
	) ;_ End if
    ) ;_ End vlax-for

					;    (mapcar (vla-put-layeron aNewLayer :vlax-false) '("S1-*" "S2-*" "S3-*" "S4-*" "S5-*" "S6-*" "SR-*" "SR2-"))

    (vla-setvariable VL-ADOC "cmdecho" CMD)
    (vla-endundomark VL-ADOC)
    (princ)
) ;_ End defun




;|«Visual LISP© Format Options»
(72 4 40 2 T "End " 60 9 0 0 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
