;;; fastviews.lsp
;;; Copyright 1997-2000 by Lance A. Endres

(defun c:VS ()
    (command "view" "s")
    (princ)
)

(defun c:VR ()
    (command "view" "r")
    (princ)
)