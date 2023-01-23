;;; breaksnaps.lsp
;;; Copyright 1998-2000 b Lance A. Endres

(defun c:BA ()
    (command "break" pause "f" "int" pause "@")
    (princ)
)

(defun c:BQ ()
    (command "break" pause "f" "qua" pause "qua" pause)
    (princ)
)

(defun c:BZ ()
    (command "break" pause "f" "int" pause "int")
    (princ)
)