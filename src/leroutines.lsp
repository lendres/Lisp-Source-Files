(defun c:COPY_MULT (/ SS)
    (setq OLDERR *ERROR*
          *ERROR* CMERR
          SS (ssget)
    ) ;End setq
    (command "copy" SS "" "m")
    (setq *ERROR* OLDERR)
    (princ)
)

(defun CMERR (S)
    (if (/= S "Function cancelled")
        (princ (strcat "\nError: " S))
    ) ;End if
    (setq *ERROR* OLDERR)
    (princ)
)

(defun c:CRSTRETCH ()
    (command "stretch" "c")
    (princ)
)

(defun c:CLEAN ()
    (command "_.purge" "all" "" "n")
    (princ)
)

(defun c:UCSOB ()
    (command "_.ucs" "ob")
    (princ)
)

(defun c:UPDATE ()
    (command "dim1" "update" "all" "")
    (princ)
)

(defun c:LINSB (/ PT1)
    (setq PT1 (getvar "insbase"))
    (command "line" PT1)
    (princ)
)