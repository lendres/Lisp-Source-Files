;;; zooms.lsp
;;; Copyright 1997-2000 by Lance A. Endres

(defun C:ZA ()
    (command "_.zoom" "all")
    (princ)
) ;_ End defun

(defun C:ZE ()
    (command "_.zoom" "extents")
    (princ)
) ;_ End defun

(defun C:ZL ()
    (command "_.zoom" (getvar "limmin") (getvar "limmax"))
    (princ)
) ;_ End defun

(defun C:ZX ()
    (command "_.zoom" (strcat (rtos (/ 1 (getvar "dimscale")) 2 8) "xp"))
    (princ)
)

(defun C:ZZ ()
    (command "_.zoom" "previous")
    (princ)
) ;_ End defun

(defun C:- ()
    (command "_.zoom" ".75x")
    (princ)
) ;_ End defun

(defun C:* ()
    (command "_.zoom" "1.3x")
    (princ)
) ;_ End defun
