;;; color-bylayer.lsp
;;; Copyright 2000 by Lance A. Endres

(defun C:COLOR-BYLAYER (/ ACADDOC CMD ITEM LAY-STAT-LIST VL-SS1)
    (vl-load-com)
    (setq OLDERR  *ERROR*
	  *ERROR* COLOR-BYLAYERERR
	  ACADDOC (vla-get-activedocument (vlax-get-acad-object))
	  CMD	  (vla-getvariable ACADDOC "cmdecho")
	  VL-SS1  (EST-VL-SS)
    ) ;_ End setq
    (vla-startundomark ACADDOC)
    (vla-setvariable ACADDOC "cmdecho" 0)
    (princ "\nSelect objects to set color to bylayer: ")
    (ssget)
    (vla-select VL-SS1 acselectionsetprevious)
    (vlax-for ITEM VL-SS1
	(if (/= (vla-get-color ITEM) 256)
	    (progn
		(setq LAY-STAT-LIST (SET-FRZN-LOCK-LAY ITEM))
		(vla-put-color ITEM acbylayer)
		(RESET-FRZN-LOCK-LAY ITEM LAY-STAT-LIST)
	    ) ;_ End progn
	) ;_ End if
    ) ;_ End vlax-for
    (setq *ERROR* OLDERR)
    (vla-setvariable ACADDOC "cmdecho" CMD)
    (vla-endundomark ACADDOC)
    (princ)
) ;_ End defun


(defun COLOR-BYLAYERERR	(ERR)
    (if	(/= ERR "Function cancelled")
	(princ (strcat "\nError: " ERR))
    ) ;_ End if
    (setq *ERROR* OLDERR)
    (vla-setvariable ACADDOC "cmdecho" CMD)
    (vla-endundomark ACADDOC)
    (princ)
) ;_ End defun

;|«Visual LISP© Format Options»
(72 4 40 2 T "End " 60 9 1 2 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
