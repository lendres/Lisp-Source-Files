;;; dlayer.lsp
;;; Copyright 1999-2000 by Lance A. Endres

(defun C:DLAYER	(/ CMD LAYERNAME LYTEST SS PT1 PT2 ENT)
    (setq OLDERR  *ERROR*
	  *ERROR* DLAYERERR
	  CMD	  (getvar "cmdecho")
    ) ;_ End setq
    (setvar "cmdecho" 0)
    (while (= ENT NIL)
	(initget "Enter")
	(setq ENT
		 (entsel
		     "\nEnter name/<Pick an object on the layer to be deleted>: "
		 ) ;_ End entsel
	) ;_ End setq
    ) ;_ End while - ensures selected object or keyword entered
    (if	(= ENT "Enter")
	(progn
	    (while (= LYTEST NIL) ;_ Ensure layer selected is a valid layer
		(setq LAYERNAME (strcase (getstring "\nLayer name: ")))
		(setq LYTEST (tblsearch "layer" LAYERNAME))
		(if (= LYTEST NIL)
		    (princ "\nNot a valid layer name:")
		) ;_ End if
	    ) ;_ End while
	) ;_ End progn
	(setq LAYERNAME (assoc 8 (entget (car ENT))))
    ) ;_ End if
    (initget 1)
    (setq PT1 (getpoint "\nFirst corner of window: "))
    (initget 1)
    (setq PT2 (getcorner PT1 "\nSecond corner: ")
	  SS  (ssget "w" PT1 PT2 (list LAYERNAME)) ;_ Create selection set
    ) ;_ End setq
    (if	(/= SS NIL) ;_ Test for nil set, meaning nothing was selected.
	(progn
	    (princ (strcat "\n "
			   (itoa (sslength SS))
			   "entities on layer \""
			   (cdr LAYERNAME)
			   "\" found and deleted."
		   ) ;_ End strcat
	    ) ;_ End princ
	    (command "erase" SS "") ;_ Erase matching entities
	) ;_ End progn
	(princ
	    (strcat "\nNo entities on layer \"" (cdr LAYERNAME) "\" found.")
	) ;_ End princ
    ) ;_ End if
    (setq *ERROR* OLDERR)
    (setvar "cmdecho" CMD)
    (princ)
) ;_ End defun

(defun DLAYERERR (S)
    (if	(/= S "Function cancelled")
	(princ (strcat "\nError: " S))
    ) ;_ End if
    (setq *ERROR* OLDERR)
    (setvar "cmdecho" CMD)
    (princ)
) ;_ End defun

;|«Visual LISP© Format Options»
(72 4 40 2 T "End " 60 9 1 2 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
