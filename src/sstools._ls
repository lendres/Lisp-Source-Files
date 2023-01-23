;;; Selection Set tools for visual lisp by David M. Stein.
;;; http://members.xoom.com/_XMCM/dstein/codemine/index.html

(vl-load-com)

(defun SSETEXISTS-P (NAME)
    (not
        (vl-catch-all-error-p
            (vl-catch-all-apply
                'vla-item
                (list (vla-get-selectionsets (VLXX-ACTIVEDOCUMENT))
                      NAME
                ) ;_ End list
            ) ;_ End vl-Catch-All-Apply
        ) ;_ End vl-catch-All-Error-p
    ) ;_ End not
) ;_ End defun

(setq *VLXX-ACADOBJECT* NIL) ;_ Initialize global variable
(defun VLXX-ACADOBJECT ()
    (cond
        (*VLXX-ACADOBJECT*) ;_ Return the cached object
        (t (setq *VLXX-ACADOBJECT* (vlax-get-acad-object)))
    ) ;_ End cond
) ;_ End defun

(setq *VLXX-ACTIVEDOCUMENT* NIL) ;_ Initialize global variable
(defun VLXX-ACTIVEDOCUMENT ()
    (cond
        (*VLXX-ACTIVEDOCUMENT*) ;_ Return the cached object
        (t
         (setq *VLXX-ACTIVEDOCUMENT*
                  (vla-get-activedocument (VLXX-ACADOBJECT))
         ) ;_ End setq
        )
    ) ;_ End cond
) ;_ End defun

;; Establish a Visual LISP Selection Set.
(defun EST-VL-SS (/ ACSSTS)
    (setq ACSSTS (vla-get-selectionsets
                     (vla-get-activedocument
                         (vlax-get-acad-object)
                     ) ;_ End vla-get-activedocument
                 ) ;_ End vla-get-selectionsets
    ) ;_ End setq
    (if (SSETEXISTS-P "TEMP_SS1")
        (vla-delete (vla-item ACSSTS "TEMP_SS1"))
    ) ;_ End if
    (vla-add ACSSTS "TEMP_SS1")
) ;_ End defun
