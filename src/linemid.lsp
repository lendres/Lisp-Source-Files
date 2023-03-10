;;; linemid.lsp
;;; Copyright 1997-2000 by Lance A. Endres

(defun c:LINEMID (/ PT1 PT2 X1 Y1)
    (setq OLDERR *ERROR*
          *ERROR* LMERROR
          OSM (getvar "osmode")
    ) 
    (setvar "osmode" 1)
    (setq PT1 (getpoint "\nSelect 1st Point: ")
          PT2 (getpoint PT1 "\nSelect 2nd Point: ")
    )
    (setvar "osmode" 0)
    (setq X1 (car PT1) X2 (car PT2)
          Y1 (cadr PT1) Y2 (cadr PT2)
          PT3 (list (/ (+ X1 X2) 2) (/ (+ Y1 Y2) 2))
    )
    (command "line" PT3 pause)
    (setq *ERROR* OLDERR)
    (setvar "osmode" OSM)
    (princ)
)

(defun LMERROR (S)
    (if (/= S "Function cancelled")
        (princ (strcat "\nError: " S))
    )
    (setvar "osmode" OSM)
    (setq *ERROR* OLDERR)
    (princ)
)