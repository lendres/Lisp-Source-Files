(defun C:DRAWLOG ()
    (vla-runmacro (vlax-get-acad-object) "drawinglog.dvb!rundrawlog")
    (princ)
) ;_ End defun

(defun C:DRAWLOGSWEXPORT ()
    (vla-runmacro (vlax-get-acad-object) "drawinglog.dvb!SolidWorksExport")
    (princ)
) ;_ End defun

;;;    (vl-vbaload "c:\\custom cad files\\drawinglog.dvb")
;;;    (vl-vbarun "rundrawlog")

;|«Visual LISP© Format Options»
(72 4 40 2 T "End " 60 9 1 2 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
