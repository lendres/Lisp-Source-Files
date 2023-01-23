(defun C:TEXTFIX (/	      CMD	  DIMSC	      FILTER_TYPE
		  FILTER_VALUE		  ITEM	      INTERLINESPACE
		  LMBD1	      LMBD2	  SSVL	      VL-ADOC
		  YSTARTPNT
		 )
    (setq VL-ADOC (vla-get-activedocument (vlax-get-acad-object)))
    (vla-startundomark VL-ADOC) ;_ End vla-startundomark
    (setq CMD (vla-getvariable VL-ADOC "cmdecho"))
    (mapcar '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
	    (list "cmdecho")
	    (list 1)
    ) ;_ End mapcar


;;; Get the currently seleted items.
    (setq SSVL (vla-get-pickfirstselectionset VL-ADOC))

;;; Filter out everything in the selection set except text (not mtext).
    (if	(> (vla-get-count SSVL) 0)
	(progn
	    (ssget "P" (list '(0 . "TEXT")))
	    (setq SSVL (EST-VL-SS))
	    (vla-select SSVL acselectionsetprevious)
	) ;_ End progn
    ) ;_ End if

    (if	(equal (vla-get-count SSVL) 0)
;;; Nothing was in the current selection set so get a new one.
	(progn
	    (setq SSVL (EST-VL-SS))
	    (setq FILTER_TYPE  (vlax-make-safearray
				   vlax-vbinteger
				   '(0 . 0)
			       ) ;_ End vlax-make-safearray
		  FILTER_VALUE (vlax-make-safearray
				   vlax-vbvariant
				   '(0 . 0)
			       ) ;_ End vlax-make-safearray
	    ) ;_ End setq

;;; DXF Code for ent type.
	    (vlax-safearray-fill FILTER_TYPE '(0))

;;; The filter value.
	    (vlax-safearray-fill FILTER_VALUE '("Text"))
	    (vla-selectonscreen SSVL FILTER_TYPE FILTER_VALUE)
	) ;_ End progn
    ) ;_ End if

    (if	(> (vla-get-count SSVL) 1)
	(progn
	    (setq SSVL		 (VL-VERSORTTEXT SSVL)
		  TEMP		 (vla-item SSVL 0)
		  STARTPNT	 (vlax-variant-value
				     (vla-get-insertionpoint TEMP)
				 ) ;_ End vlax-variant-value
		  YSTARTPNT	 (vlax-safearray-get-element STARTPNT 1)
		  DIMSC		 (vlax-variant-value
				     (vla-getvariable VL-ADOC "dimscale")
				 ) ;_ End vlax-variant-value
		  INTERLINESPACE (* 0.15178 DIMSC)
		  HEIGHT	 (* (/ 3.0 32.0) DIMSC)
		  CNT		 0
	    ) ;_ End setq

	    (vlax-for ITEM SSVL
		(vlax-safearray-put-element
		    STARTPNT
		    1
		    (- YSTARTPNT (* INTERLINESPACE CNT))
		) ;_ End vlax-safearray-put-element
		(vla-put-insertionpoint ITEM STARTPNT)
		(vla-put-height ITEM HEIGHT)
		(setq CNT (1+ CNT))

;;;(mapcar
;;;			    '(lambda (X)
;;;				 (if (= (logand BIT 1) 1)
;;;				     (ENT-COLOR-BYLAYER X)
;;;				 ) ;_ End if
;;;				 (if (= (logand BIT 2) 2)
;;;				     (vla-put-scalefactor X WID)
;;;				 ) ;_ End if
;;;			     ) ;_ End lambda
;;;			    (vlax-safearray->list
;;;				(vlax-variant-value
;;;				    (vla-getattributes ITEM)
;;;				) ;_ End vlax-variant-value
;;;			    ) ;_ End vlax-safearray->list
;;;			) ;_ End mapcar
	    ) ;_ End vlax-for
	) ;_ End progn
    ) ;_ End if


    (mapcar '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
	    (list "cmdecho")
	    (list CMD)
    ) ;_ End mapcar
    (vla-endundomark VL-ADOC) ;_ End vla-endundomark
    (princ)
) ;_ End defun



;;;(defun *ERROR* (MSG / ADOC LMBD1 LMBD2)
;;;    (setq ADOC (vla-get-activedocument (vlax-get-acad-object)))
;;;    (mapcar '(lambda (LMBD1 LMBD2) (vla-setvariable ADOC LMBD1 LMBD2))
;;;	    (list "osmode")
;;;	    (list OSM)
;;;    ) ;_ End mapcar
;;;
;;;    (vla-endundomark ADOC)
;;;    (princ)
;;;) ;_ End defun
;|«Visual LISP© Format Options»
(72 4 40 2 T "End " 60 9 1 2 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
