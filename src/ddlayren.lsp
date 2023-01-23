;;; ddlayren.lsp
;;; Copyright 1999-2000 by Lance A. Endres

(defun c:DDLAYREN (/ DCL_ID L_FILE_HAND)
    (setq OLDERR *ERROR*
          *ERROR* LAYRENAMEERR
          CMD (getvar "cmdecho")
          DCL_ID (load_dialog "DDLAYREN.DCL")
          LIST_FILE (strcat (getvar "tempprefix") "DDLAYREN.DAT")
          LIST_FILE (findfile LIST_FILE)
    ) ;End setq
    (setvar "cmdecho" 0)
    (command "_.undo" "begin")

;Load dialog file and verify loading

    (if (not (new_dialog "LAYRENAME" DCL_ID))
        (exit)
    ) ;End if test to insure dialog box loaded

;Check for ddlayren.dat and if found extract a list of data
;files from it.  Then update list box with list.

    (if (null LIST_FILE)
        (progn
            (setq LIST_FILE (strcat (getvar "tempprefix") "DDLAYREN.DAT")
                  L_FILE_HAND (open LIST_FILE "w")
            ) ;End setq
            (close L_FILE_HAND)
        ) ;End then progn
    ) ;End if to ensure that ddlayren.dat exists
    (setq L_FILE_HAND (open LIST_FILE "r")
          FILES_LIST '()
    ) ;End setq
    (while (setq LINE1 (read-line L_FILE_HAND))
        (setq FILES_LIST (append FILES_LIST (list LINE1)))
    ) ;End while for creating a list from data in ddlayren.dat
    (close L_FILE_HAND)
    (UPDATE_LIST_BOX)

;Initiate tiles

    (mode_tile "rem_file" 1)
    (action_tile "data_file" "(TOGGLE_REM)")
    (action_tile "new_file"  "(GET_NEW_FILE)")
    (action_tile "rem_file" "(REMOVE_FILE)")
    (action_tile "convert" "(DDLAYREN_OUT)")

;Run dialog session

    (start_dialog)
    (unload_dialog DCL_ID)

;Exit ddlayren

    (setq *ERROR* OLDERR)
    (if LOC
        (LAYREN LOC)
    ) ;_ End if
    (command "_.undo" "end")
    (setvar "cmdecho" CMD)
    (princ)
)

;;; --------------------------------------------------

(defun UPDATE_LIST_BOX ()
    (start_list "data_file")
    (mapcar 'add_list FILES_LIST)
    (end_list)
)

;;; --------------------------------------------------

(defun GET_NEW_FILE (/ DATA_FILE_NEW)
   (if (= DATA_FILE_DIR nil)
        (setq DATA_FILE_DIR "")
    ) ;End if
    (setq DATA_FILE_NEW (getfiled "Select a layer conversion data file" DATA_FILE_DIR "txt" 6))
    (if (and DATA_FILE_NEW (not (member DATA_FILE_NEW FILES_LIST)))
        (progn
            (setq DATA_FILE_DIR (car (fnsplitl DATA_FILE_NEW))
                  FILES_LIST (append FILES_LIST (list DATA_FILE_NEW))
            ) ;End setq
            (UPDATE_LIST_BOX)
            (set_tile "data_file" (itoa (LOC_IN_LIST)))
            (TOGGLE_REM)
            (UPDATE_DDLAYREN_DAT)
        ) ;End progn
    ) ;End if
)

;;; --------------------------------------------------

(defun LOC_IN_LIST (/ LOC)
    (setq LOC (- (length FILES_LIST) (length (member DATA_FILE_NEW FILES_LIST))))
)

;;; --------------------------------------------------

(defun TOGGLE_REM ()
    (if (read (get_tile "data_file"))
        (mode_tile "rem_file" 0)
        (mode_tile "rem_file" 1)
    ) ;End if
)

;;; --------------------------------------------------

(defun REMOVE_FILE (/ CNT LEN LOC TLIST)
    (setq LOC (atoi (get_tile "data_file"))
          LEN (length FILES_LIST)
          CNT 0
          TLIST '()
    ) ;End setq
    (while (< CNT LEN)
        (if (/= CNT LOC)
            (setq TLIST (append TLIST (list (nth CNT FILES_LIST))))
        ) ;End if
        (setq CNT (1+ CNT))
    ) ;End while
    (setq FILES_LIST TLIST)
    (UPDATE_LIST_BOX)
    (TOGGLE_REM)
    (UPDATE_DDLAYREN_DAT)
)

;;; --------------------------------------------------

(defun UPDATE_DDLAYREN_DAT (/ CNT L_FILE_HAND LEN TEMP)
    (setq CNT 0
          LEN (length FILES_LIST)
          L_FILE_HAND (open LIST_FILE "w")
    ) ;End setq
    (if (/= LEN 0)
        (progn
            (while (< CNT LEN)
                (setq TEMP (nth CNT FILES_LIST)
                      CNT (1+ CNT)
                ) ;End setq
                (write-line TEMP L_FILE_HAND)
            ) ;End while
        ) ;End progn
    ) ;End if
    (close L_FILE_HAND)
)

;;; --------------------------------------------------

(defun DDLAYREN_OUT ()
    (setq LOC (atoi (get_tile "data_file")))
    (done_dialog)
)

;;; --------------------------------------------------

(defun LAYREN (LOC / CNT CNT2 D_FILE_HAND DATA_FILE LOC)
    (setq DATA_FILE (strcase (nth LOC FILES_LIST))
          D_FILE_HAND (open DATA_FILE "r")
          CNT 0
          CNT2 0
          CNT3 0
    ) ;End setq
    (EXTRACT_LINES)
    (while LINE2
        (if (snvalid LINE2)
            (progn
                (if (tblsearch "layer" LINE1)
                    (if (not (tblsearch "layer" LINE2))
                        (command "_.rename" "layer" LINE1 LINE2)
                        (setq CNT3 (1+ CNT3))
                    ) ;End if
                    (setq CNT (1+ CNT))
                ) ;End if
            ) ;End progn
            (setq CNT2 (1+ CNT2))
        ) ;End if to check for a valid layer name
        (EXTRACT_LINES)
    ) ;End while
    (if (/= CNT 0)
        (progn
            (print CNT)
            (princ "layer(s) not found in the drawing.")
        ) ;End progn
    ) ;End if
    (if (/= CNT3 0)
        (progn
            (print CNT3)
            (princ "layer(s) already exist.")
        ) ;End progn
    ) ;End if
    (if (/= CNT2 0)
        (progn
            (print CNT2)
            (princ "invalid layer name(s) found in data file.")
        ) ;End progn
    ) ;End if
)

;;; --------------------------------------------------

(defun EXTRACT_LINES ()
    (setq LINE1 (read-line D_FILE_HAND))
    (while (= LINE1 "")
        (setq LINE1 (read-line D_FILE_HAND))
    ) ;End while
    (setq LINE2 (read-line D_FILE_HAND))
    (while (= LINE2 "")
        (setq LINE2 (read-line D_FILE_HAND))
    ) ;End while
)

;;; --------------------------------------------------

(defun LAYRENAMEERR (S)
    (if (/= S "Function cancelled")
        (princ (strcat "\nError: " S))
    )
    (setq *ERROR* OLDERR)
    (setvar "cmdecho" CMD)
    (princ)
)