;;; fastdists.lsp
;;; Copyright 1997-2000 by Lance A. Endres

(defun c:DISTEND ()
    (command "dist" "end" pause "end")
    (princ)
)

(defun c:DISTPER ()
    (command "dist" "nea" pause "per")
    (princ)
)