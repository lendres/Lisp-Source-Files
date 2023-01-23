;;; acad.lsp
;;; Copyright 2000 by Lance A. Endres

(acad-push-dbmod)
(vl-load-com)
(setvar "cmdecho" 0)

;;; AutoCAD start log

(setq FILE "S:\\Standards\\CAD Standards\\acadstart.log")

(if (setq TEMP (findfile FILE))
    (setq FILE_HAND (open TEMP "a"))
    (setq FILE_HAND
	     (open FILE "w") ;_ End open
    ) ;_ End setq
) ;_ End if

(if FILE_HAND
    (progn
	(write-line
	    (strcat
		(getvar "loginname")
		" - "
		(menucmd
		    "M=$(edtime,$(getvar,date),DDDD\",\" MONTH DD YYYY HH:MM:SS)"
		) ;_ End menucmd
		" - AutoCAD Version "
		(getvar "acadver")
	    ) ;_ End strcat
	    FILE_HAND
	) ;_ End write-line

;;; Close file if open.

	(close FILE_HAND)
    ) ;_ End progn
) ;_ End if

(setvar "cmdecho" 1)
(acad-pop-dbmod)

;|«Visual LISP© Format Options»
(72 4 40 2 T "End " 60 9 1 2 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
