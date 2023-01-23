;;; setup.lsp
;;; Copyright 1998-2006 by Lance A. Endres

;;; Revision History
;;; July 4, 2001
;;; Plotting in SI added.
;;; Selection of drawing units added.
;;; Selection of dimension precision added.
;;;
;;; March 8, 2004
;;; Updates for use with AutoCAD 2004
;;;
;;; Fixed a bug in the used of EXPERT and eliminated having to decide how to call
;;; the adding of dimstyles and linetypes depending on if they were already loaded.
;;;
;;; February 2, 2005
;;; Fixed a bug that allowed layers to be imported regardless of check box on dialog
;;; box.
;;;
;;; Added support for 12pt fonts.
;;;
;;; December 1, 2005
;;; The luprec (displayed precision) is now controled by the setting of the dimension precision.
;;; Added the variable DATAPREFIX which stores the configuration data prefix string.
;;;
;;; October 09, 2006
;;; Added an option to allow adjustments for making figures for papers.  Especially for when using
;;; LaTeX.  Adjustments made:
;;;     1) Added an option to the dialog box.
;;;     2)
;;;
;;;
;;; NOTE: When debugging this file, be sure to include a reference to the path that the source
;;;       dcl file is located.  I.e. c:\storage\programming\lisp source files\

(defun C:SETUP (/ ADJ_FOR_PAPERS      CMD	 CLAY	    DCL_ID     DWG_SCALE
		EXPRT	   GENRE01    GENRE02	 GENRE03    GENRE04
		GENRE05	   GENRE06    IMP_LYTS	 L_UNITS
		L_UNITS_HOLDER	      LMBD1	 LMBD2	    OSM
		P_SIZE	   RGMOD      SET_LAYS	 SET_LMTS   TEMP
		VL-ADOC
	       )
    (setq DCL_ID    (load_dialog "SETUP.DCL")
	  RUN_SETUP "No"
	  VL-ADOC   (vla-get-activedocument (vlax-get-acad-object))
	  CLAY	    (vla-getvariable VL-ADOC "clayer")
	  CMD	    (vla-getvariable VL-ADOC "cmdecho")
	  RGMOD	    (vla-getvariable VL-ADOC "regenmode")
	  EXPRT	    (vla-getvariable VL-ADOC "expert")
	  OSM	    (vla-getvariable VL-ADOC "osmode")
	  DATAPREFIX   "AppData/Setup_Data/"
    ) ;_ End setq
    (vl-load-com)
    (vla-startundomark VL-ADOC)
    (mapcar '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
	    (list "cmdecho" "regenmode" "expert")
	    (list 1 0 5)
    ) ;_ End mapcar

;;; Load dialog file and verify loading.

    (if	(not (new_dialog "SETUP" DCL_ID))
	(exit)
    ) ;_ End if test to insure dialog box loaded

;;; Gather data from configuration file.

;;; DWG_SCALE		Controls system variable "dimscale".
;;; L_UNITS		Controls system variable "lunits" (arch / decimal / etc.).
;;;    			Used to set the drawing units and to convert units for controlling
;;;    			the dialog box.
;;; SET_LMTS 		Determines if the user wants to set limits during the setup process.
;;; SET_LAYS 		Determines if the user wants to import layers during the setup process.
;;; IMP_LYTS 		Determines if the user wants to import layouts during the setup process.
;;; P_SIZE   		Setting for the paper sized used (i.e. D or E size).
;;; GENRE01		Import lighting layers.
;;; GENRE02 		Import power layers.
;;; GENRE03		Import HVAC layers.
;;; GENRE04		Import pluming layers.
;;; GENRE05		Import equipment layers.
;;; GENRE06		Import general layers.
;;; LAY_DATA		Path of last used layer data file.
;;; DIM_PREC		Dimension precision to use.  LUPREC (precision displayed on screen) also uses this setting.
;;; PLOT_UNTS		Plotting units, US Customary or System International (store value "uscust" / "si")
;;; ADJ_FOR_PAPERS	Adjust dimensiions, etc. for making figures for papers.

    (setq DWG_SCALE      (getcfg (strcat DATAPREFIX "Dwg_Scale"))
	  L_UNITS        (getcfg (strcat DATAPREFIX "L_Units"))
	  DIM_PREC       (getcfg (strcat DATAPREFIX "Dim_Prec"))
	  SET_LMTS       (getcfg (strcat DATAPREFIX "Set_Lmts"))
	  SET_LAYS       (getcfg (strcat DATAPREFIX "Set_Lays"))
	  IMP_LYTS       (getcfg (strcat DATAPREFIX "Imp_Lyts"))
	  P_SIZE         (getcfg (strcat DATAPREFIX "P_Size"))
	  GENRE01        (getcfg (strcat DATAPREFIX "Genre01"))
	  GENRE02        (getcfg (strcat DATAPREFIX "Genre02"))
	  GENRE03        (getcfg (strcat DATAPREFIX "Genre03"))
	  GENRE04        (getcfg (strcat DATAPREFIX "Genre04"))
	  GENRE05        (getcfg (strcat DATAPREFIX "Genre05"))
	  GENRE06        (getcfg (strcat DATAPREFIX "Genre06"))
	  LAY_DATA       (getcfg (strcat DATAPREFIX "Lay_Data"))
	  PLOT_UNTS      (getcfg (strcat DATAPREFIX "Plot_Unts"))
	  ADJ_FOR_PAPERS (getcfg (strcat DATAPREFIX "Adj_For_Papers"))
    ) ;_ End setq

;;; If data does not exist set defaults.

;;; If L_UNITS does not exist then the drawing scale did not exist.

    (if	(or (null L_UNITS) (= L_UNITS ""))
	(setq L_UNITS (getvar "lunits"))
	(setq L_UNITS (atoi L_UNITS))
    ) ;_ End if

;;; Store the value to use to initilize the tiles.
    (setq L_UNITS_HOLDER L_UNITS)

;;; Drawing scale.
;;; If it does not exist set it to 1.
;;; If it does exist the convert it to a real based on the units it was stored in
;;; then use the ai_utils to convert is to a string for the dialog box using the
;;; current drawing units.  L_UNITS then has to be set to the current drawing units
;;; so that it is compatible with the new drawing scale format.

    (if	(or (null DWG_SCALE) (= DWG_SCALE ""))
	(setq DWG_SCALE "1")
	(setq				;DWG_SCALE	(AI_RTOS (distof DWG_SCALE L_UNITS))
	    L_UNITS (getvar "lunits")
	) ;_ End setq
    ) ;_ End if

    (if	(or (null DIM_PREC) (= DIM_PREC ""))
	(if (= L_UNITS 4) ;_ If the active units are architectural.
	    (setq DIM_PREC "4") ;_ Set primary dim units to 1/16th precision.
	    (setq DIM_PREC "0") ;_ Else set primary dim units to 0 precision.
	) ;_ End if for determining which units are active.
    ) ;_ End if for primary dimension precision units.
    (if	(or (null P_SIZE) (= P_SIZE ""))
	(setq P_SIZE "ps36x24")
    ) ;_ End if
    (if	(or (null PLOT_UNTS) (= PLOT_UNTS ""))
	(setq PLOT_UNTS "uscust")
    ) ;_ End if
    (foreach TEMP '(SET_LMTS	IMP_LYTS    SET_LAYS	GENRE01
		    GENRE02	GENRE03	    GENRE04	GENRE05
		    GENRE06     ADJ_FOR_PAPERS
		   )
	(if (or (null (eval TEMP)) (= (eval TEMP) ""))
	    (set TEMP "0")
	) ;_ End if
    ) ;_ End foreach

;;; Add elements to dimension precision list

;;; Create global variables
    (setq PREC_LIST_ARCH (list "0'-0\""		  "0'-0 1/2\""
			       "0'-0 1/4\""	  "0'-0 1/8\""
			       "0'-0 1/16\""	  "0'-0 1/32\""
			       "0'-0 1/64\""	  "0'-0 1/128\""
			       "0'-0 1/256\""
			      ) ;_ End list
	  PREC_LIST_DIM	 (list "0"	    "0.0"	 "0.00"
			       "0.000"	    "0.0000"	 "0.00000"
			       "0.000000"   "0.0000000"	 "0.00000000"
			      ) ;_ End list
    ) ;_ End setq

    (start_list "dimprec")
    (mapcar 'add_list
	    (cond
		((= L_UNITS_HOLDER 4)
		 PREC_LIST_ARCH
		)
		((= L_UNITS_HOLDER 2)
		 PREC_LIST_DIM
		)
		((t)
		 PREC_LIST_ARCH
		)
	    ) ;_ End cond
    ) ;_ End mapcar
    (end_list)


;;; INITIATE TILES

;;; Initilize the plotting units (us / si)
    (set_tile PLOT_UNTS "1")

;;; Initilize the dimension precision
    (set_tile "dimprec" DIM_PREC)

;;; Initilize the proper tile for the display units (arch / decim).
    (cond
	((= L_UNITS_HOLDER 4)
	 (set_tile "arch" "1")
	)
	((= L_UNITS_HOLDER 2)
	 (set_tile "decim" "1")
	)
	((t)
	 (set_tile "arch" "1")
	) ;_ If neither has been selected set a default.
    ) ;_ End cond

    (mapcar 'set_tile
	    (list "dwg_scale"	 "set_lmts"	   "imp_lyts"
		  "set_lays"	 "genre01"	   "genre02"
		  "genre03"	 "genre04"	   "genre05"
		  "genre06"      "adj_for_papers"
		 ) ;_ End list
	    (list DWG_SCALE   SET_LMTS	  IMP_LYTS        SET_LAYS
		  GENRE01     GENRE02	  GENRE03         GENRE04
		  GENRE05     GENRE06     ADJ_FOR_PAPERS
		 ) ;_ End list
    ) ;_ End mapcar
    (mapcar 'action_tile
	    (list "dwg_scale"	 "imp_lyts"	"set_lays"
		  "lay_data"	 "brws_lay_data"
		  "accept"	 "arch"		"si"
		  "decim"
		 ) ;_ End list
	    (list
		"(DWG_SCALE_CHECK $value)"
		"(TOGGLE_PSIZES $value)"
		"(TOGGLE_LAYERS $value)"
		"(EDITLAYDATA $value)"
		"(BRWSLAYDATA)"
		"(DDSETUP_OUT)"
		"(ARCH_ACTIVATED)"
		"(SI_ACTIVATED)"
		"(DECIM_ACTIVATED)"
	       ) ;_ End list
    ) ;_ End mapcar

    (TOGGLE_LAYERS (get_tile "set_lays"))
    (TOGGLE_PSIZES (get_tile "imp_lyts"))

;;; Initialize the proper paper size tile.
    (set_tile P_SIZE "1")

;;; Initialize the proper units tile.
    (set_tile PLOT_UNTS "1")

    (if	LAY_DATA
	(set_tile "lay_data" LAY_DATA)
	(set_tile "lay_data" "")
    ) ;_ End if

;;; Run dialog session.
    (start_dialog)
    (unload_dialog DCL_ID)

;;; Test for running setup.
    (if	(= RUN_SETUP "Yes")
	(SETUP)
    ) ;_ End if

;;; Exit.
    (mapcar '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
	    (list "clayer" "regenmode" "expert" "osmode")
	    (list CLAY RGMOD EXPRT OSM)
    ) ;_ End mapcar
    
    (princ "Setup complete.")
    (vla-setvariable VL-ADOC "cmdecho" CMD)
    (vla-endundomark VL-ADOC)
    (princ)
) ;_ End defun

(defun TOGGLE_LAYERS (TEMP / TOG_LIST)
    (setq TOG_LIST (list "genre01"	 "genre02"
			 "genre03"	 "genre04"
			 "genre05"	 "genre06"
			 "lay_label"	 "lay_data"
			 "brws_lay_data"
			) ;_ End list
    ) ;_ End setq
    (if	(= TEMP "1")
	(mapcar '(lambda (TEMP) (mode_tile TEMP 0)) TOG_LIST)
	(mapcar '(lambda (TEMP) (mode_tile TEMP 1)) TOG_LIST)
    ) ;_ End if
) ;_ End defun

;;; Switch the status of the layout tiles when the import button is
;;; toggled.
(defun TOGGLE_PSIZES (TEMP)
    (if	(= TEMP "1")
	(mapcar	'(lambda (TEMP) (mode_tile TEMP 0))
		(list "ps36x24" "ps42x30")
	) ;_ End mapcar
	(mapcar	'(lambda (TEMP) (mode_tile TEMP 1))
		(list "ps36x24" "ps42x30")
	) ;_ End mapcar
    ) ;_ End if
) ;_ End defun

(defun DWG_SCALE_CHECK (VALUE / UNITS)
    (setq UNITS	(if (= (get_tile "arch") "1")
		    4
		    2
		) ;_ End if
	  VALUE	(distof VALUE UNITS)
    ) ;_ End setq
    (if	(> VALUE 0)
	(progn
	    (set_tile "dwg_scale" (LE_RTOS VALUE UNITS))
	    (if	(= (get_tile "error")
		   "Drawing scale has to be greater than zero."
		) ;_ End =
		(set_tile "error" "")
	    ) ;_ End if to clear error if it is a increment analysis error
	) ;_ End progn
	(set_tile "error"
		  "Drawing scale has to be greater than zero."
	) ;_ End set_tile
    ) ;_ End if
) ;_ End defun

(defun EDITLAYDATA (LAY_DATA_NEW)
    (if	(not (findfile LAY_DATA_NEW))
	(progn
	    (set_tile "error" "Layer data file not found.")
	    (mode_tile "lay_data" 3)
	) ;_ End prong
	(if (= (get_tile "error") "Layer data file not found.")
	    (set_tile "error" "")
	) ;_ End if to clear error if it is a material property file error.
    ) ;_ End if
) ;_ End defun

(defun BRWSLAYDATA (/ LAY_DATA_NEW)
    (if	LAY_DATA
	(setq LAY_DATA_DIR (car (fnsplitl LAY_DATA)))
    ) ;_ End if
    (if	(= LAY_DATA_DIR NIL)
	(setq LAY_DATA_DIR "")
    ) ;_ End if
    (setq LAY_DATA_NEW
	     (getfiled "Select a layer data file"
		       LAY_DATA_DIR
		       "lst"
		       6
	     ) ;_ End getfiled
    ) ;_ End setq
    (if	LAY_DATA_NEW
	(progn
	    (set_tile "lay_data" LAY_DATA_NEW)
	    (if	(= (get_tile "error") "Layer data file not found.")
		(set_tile "error" "")
	    ) ;_ End if to clear error if it is a material property file error.
	) ;_ End progn
    ) ;_ End if
) ;_ End defun

(defun DDSETUP_OUT (/ 	ADJ_FOR_PAPERS		DIM_PREC	DWG_SCALE	GENRE01
		    	GENRE02 		GENRE03 	GENRE04 	GENRE05
		    	GENRE06			IMP_LYTS	L_UNITS		LAY_DATA
		    	LAY_DATA_NEW		PLOT_UNTS   	SET_LAYS	SET_LMTS
		    	TEMP
		   )
    (setq LAY_DATA_NEW (findfile (get_tile "lay_data"))
	  SET_LAYS     (get_tile "set_lays")
    ) ;_ End set
    (if	(or (and LAY_DATA_NEW (= SET_LAYS "1"))
	    (= SET_LAYS "0")
	) ;_ End or
	(progn
	    (setq TEMP	  (get_tile "dwg_scale")
		  L_UNITS (getvar "lunits")
	    ) ;_ End setq
	    (DWG_SCALE_CHECK TEMP)
	    (if	(/= (get_tile "error")
		    "Drawing scale has to be greater than zero."
		) ;_ End /=
		(progn
		    (setq DWG_SCALE		(get_tile "dwg_scale")
			  DIM_PREC		(get_tile "dimprec")
			  PLOT_UNTS		(get_tile "uscust")
			  ADJ_FOR_PAPERS	(get_tile "adj_for_papers")
			  SET_LMTS 		(get_tile "set_lmts")
			  IMP_LYTS		(get_tile "imp_lyts")
			  SET_LAYS		(get_tile "set_lays")
			  GENRE01		(get_tile "genre01")
			  GENRE02		(get_tile "genre02")
			  GENRE03		(get_tile "genre03")
			  GENRE04		(get_tile "genre04")
			  GENRE05		(get_tile "genre05")
			  GENRE06		(get_tile "genre06")
			  RUN_SETUP 		"Yes"
		    ) ;_ End setq

		    (foreach TEMP (list "ps36x24" "ps42x30")
			(if (= (get_tile TEMP) "1")
			    (setcfg (strcat DATAPREFIX "P_Size") TEMP)
			) ;_ End if
		    ) ;_ End foreach

		    (cond
			((= (get_tile "arch") "1")
			 (setq L_UNITS 4)
			)
			((= (get_tile "decim") "1")
			 (setq L_UNITS 2)
			)
		    ) ;_ End cond

		    (foreach TEMP (list "uscust" "si")
			(if (= (get_tile TEMP) "1")
			    (setcfg (strcat DATAPREFIX "Plot_Unts") TEMP)
			) ;_ End if
		    ) ;_ End foreach

;| Alternate Method.  Used the above to allow for future additions.
		    (setcfg "AppData/Setup_Data/P_Size"
			    (if	(= (get_tile "ps36x24") "1")
				"ps36x24"
				"ps42x30"
			    ) ;_ End if
		    ) ;_ End setcfg
|;
		    (done_dialog)

		    (mapcar
			'setcfg
			(mapcar	'(lambda (TEMP)
				     (strcat DATAPREFIX TEMP)
				 ) ;_ End lambda
				(list "Dwg_Scale"    "L_Units"
				      "Dim_Prec"     "Set_Lmts"     
				      "Imp_Lyts"     "Set_Lays"
				      "Genre01"	     "Genre02"
				      "Genre03"	     "Genre04"
				      "Genre05"	     "Genre06"
				      "Adj_For_Papers"
				     ) ;_ End list
			) ;_ End mapcar
			(list DWG_SCALE	  (itoa L_UNITS) DIM_PREC
			      SET_LMTS    IMP_LYTS
			      SET_LAYS	  GENRE01     		GENRE02
			      GENRE03	  GENRE04     		GENRE05
			      GENRE06     ADJ_FOR_PAPERS
			     ) ;_ End list
		    ) ;_ End mapcar
		    (if	LAY_DATA_NEW
			(setcfg	(strcat DATAPREFIX "Lay_Data")
				LAY_DATA_NEW
			) ;_ End setcfg
		    ) ;_ End if
		) ;_ End progn
	    ) ;_ End if
	) ;_ End then progn for layer file test
	(set_tile "error" "Layer data file not found.")
    ) ;_ End if
) ;_ End defun


;;; Now it's time to get some actual work done.  Above this point are the subroutines
;;; to run the dialog box.  Below are the routines to modify the drawing.

(defun SETUP (/		    		ADJ_FOR_PAPERS	CNT1		DWG_SCALE	FILE1
	      IMP_LYTS	    		LMBD1	  	L_UNITS		L_UPREC LMBD2
	      MYLINETYPES   		PLW		PT1		PT2
	      SET_LAYS	    		SET_LMTS	VL-ADOC		VL-LAY-DISP-MARGINS
	      VL-LAY-DISP-PAPER		VL-PREF-DISPLAY
	     )
    (setq ADJ_FOR_PAPERS	(atoi (getcfg (strcat DATAPREFIX "Adj_For_Papers")))
	  DWG_SCALE		(getcfg (strcat DATAPREFIX "Dwg_Scale"))
	  L_UNITS		(atoi (getcfg (strcat DATAPREFIX "L_Units")))
	  L_UPREC		(atoi (getcfg (strcat DATAPREFIX "Dim_Prec")))
	  DWG_SCALE		(distof DWG_SCALE L_UNITS)
	  SET_LMTS		(getcfg (strcat DATAPREFIX "Set_Lmts"))
	  SET_LAYS		(getcfg (strcat DATAPREFIX "Set_Lays"))
	  IMP_LYTS		(getcfg (strcat DATAPREFIX "Imp_Lyts"))
	  FILE1			(findfile (getcfg (strcat DATAPREFIX "Lay_Data")))
	  PLW			(/ DWG_SCALE 32)
	  VL-ADOC		(vla-get-activedocument (vlax-get-acad-object))
	  PAPER_ADJ_SCALE	1.25
    ) ;_ End setq

;;; Set limits.
    (if	(= SET_LMTS "1")
	(progn
	    (setq PT1 (getpoint "\nLower left corner of Limits: ")
		  PT2 (getpoint "\nUpper right corner of Limits: ")
	    ) ;_ End setq

;;; Test to make sure that the second point is above and to the right
;;; of the first point.
	    (while
		(or (<= (car PT2) (car PT1)) (<= (cadr PT2) (cadr PT1)))
		   (princ
		       "\nSecond point must be above and to the right of first point."
		   ) ;_ End princ
		   (setq PT1 (getpoint "\nLower left corner of Limits: ")
			 PT2 (getpoint "\nUpper right corner of Limits: ")
		   ) ;_ End setq
	    ) ;_ End while
	    (setq PT1 (reverse (cdr (reverse PT1)))
		  PT2 (reverse (cdr (reverse PT2)))
		  PT1 (mapcar '(lambda (LMBD1)
				   (setq
				       LMBD1 (vlax-make-variant
						 (vlax-safearray-fill
						     (vlax-make-safearray
							 vlax-vbdouble
							 '(0 . 1)
						     ) ;_ End vlax-make-safearray
						     LMBD1
						 ) ;_ End vlax-safearray-fill
					     ) ;_ End vlax-make-variant
				   ) ;_ End setq
			       ) ;_ End lambda
			      (list PT1 PT2)
		      ) ;_ End mapcar
	    ) ;_ End setq

;;; Limits cannot be set in a layout if the margins or the paper are
;;; showing.  If a layout besides model is active, turn off the margins
;;; and paper.
	    (if	(/= (vla-get-taborder (GET-CURRENT-LAYOUT)) 0)
		(progn
		    (setq VL-PREF-DISPLAY
			     (vla-get-display
				 (vla-get-preferences
				     (vlax-get-acad-object)
				 ) ;_ End vla-get-preferences
			     ) ;_ End vla-get-display
			  VL-LAY-DISP-MARGINS
			     (vla-get-layoutdisplaymargins
				 VL-PREF-DISPLAY
			     ) ;_ End vla-get-layoutdisplaymargins
			  VL-LAY-DISP-PAPER
			     (vla-get-layoutdisplaypaper
				 VL-PREF-DISPLAY
			     ) ;_ End vla-get-layoutdisplaypaper
		    ) ;_ End setq
		    (if	(= VL-LAY-DISP-MARGINS :vlax-true)
			(vla-put-layoutdisplaymargins
			    VL-PREF-DISPLAY
			    :vlax-false
			) ;_ End vla-put-layoutdisplaymargins
		    ) ;_ End if
		    (if	(= VL-LAY-DISP-PAPER :vlax-true)
			(vla-put-layoutdisplaypaper
			    VL-PREF-DISPLAY
			    :vlax-false
			) ;_ End vla-put-layoutdisplaypaper
		    ) ;_ End if
		) ;_ End progn
	    ) ;_ End if

;;; Set the limits.
	    (mapcar '(lambda (LMBD1 LMBD2)
			 (vla-setvariable VL-ADOC LMBD1 LMBD2)
		     ) ;_ End lambda
		    (list "limmin" "limmax")
		    PT1
	    ) ;_ End mapcar

;;; Restore the margins or the paper if the layout is not model.
	    (if	(/= (vla-get-taborder (GET-CURRENT-LAYOUT)) 0)
		(progn
		    (vla-put-layoutdisplaymargins
			VL-PREF-DISPLAY
			VL-LAY-DISP-MARGINS
		    ) ;_ End vla-put-layoutdisplaymargins
		    (vla-put-layoutdisplaypaper
			VL-PREF-DISPLAY
			VL-LAY-DISP-PAPER
		    ) ;_ End vla-put-layoutdisplaypaper
		) ;_ End progn
	    ) ;_ End if
	) ;_ End progn
    ) ;_ End if test for limits

;;; Set system variables.
    (mapcar '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
	    (list "attdia"	"blipmode"    "coords"
		  "dimscale"	"lunits"      "luprec"
		  "angbase"	"angdir"      "aunits"
		  "auprec"	"mirrtext"    "plinewid"
		  "visretain"	"ucsicon"     "ltscale"
		  "limcheck"	"insunits"
		 ) ;_ End list
	    (list 0
		  0
		  2		;;; Coords.
		  DWG_SCALE     ;;; Dim scale.
		  L_UNITS       ;;; Units.
		  L_UPREC       ;;; Precision.
		  0
		  0
		  0
		  4
		  0
		  PLW
		  1
		  1
		  DWG_SCALE
		  0
		  (if (= (getcfg "AppData/Setup_Data/Plot_Unts") "si")
		      4
		      1
		  ) ;_ End if
	    ) ;_ End list
    ) ;_ End mapcar

;;; Import layouts.

    (if	(= IMP_LYTS "1")
	(progn
	    (IMPORT_LAYOUTS)
	) ;_ End progn
    ) ;_ End if

;;; View resolution.

    (command "viewres" "Y" "1000")

;;; Style settings.

    (CREATE-TEXT-STYLES)

;;; Line types.

    (foreach MYLINETYPES (list "center"	     "dashdot"
			       "hidden"	     "phantom"
			       "dashdot2"
			      ) ;_ End list
	    (command "_.-linetype" "load" MYLINETYPES "acad.lin" "")
    ) ;_ End foreach
    (foreach MYLINETYPES (list "hidden25"	 "fin_tube"
			       "dashdot_dpm"	 "dashed_dpm"
			       "hidden_dpm"	 "phantom3"
			       "fenceline_dpm"
;;;                                "vent2"
;;;                                "hot_water"
;;;                                "hot_water_supply"
;;;                                "hot_water_return"
;;;                                "cold_water"
;;;                                "gas"
;;;                                "vent"
;;;                                "sanitary"
;;;                                "non_potable_cw"
;;;                                "non_potable_hw"
;;;                                "non_potable_hwr"
			      ) ;_ End list
            (command "_.-linetype" "load" MYLINETYPES "dpm.lin" "")
    ) ;_ End foreach

;;; Layers.

;;; NOTE:  Need to varify that the heading exists within the layer standard file.

    (if (= SET_LAYS "1")
        (progn
    	    (setq CNT1 1)
    	    (while (< CNT1 7)
	        (if (= (getcfg (strcat "AppData/Setup_Data/Genre0" (itoa CNT1))
	               ) ;_ End getcfg
	               "1"
	            ) ;_ End =
	            (MAKELAYERS (strcat "**GENRE0" (itoa CNT1)) FILE1)
	        ) ;_ End if
	        (setq CNT1 (1+ CNT1))
    	    ) ;_ End while.
    	)
    )

    (if (= ADJ_FOR_PAPERS 1)
        (command "_.-layer" "m" "Text" "c" "white" "Text" "pstyle" "Text Bold" "Text" "")
	(command "_.-layer" "m" "Text" "c" "red" "Text" "pstyle" "Text" "Text" "")
    ) ;_ End if.

;;; Dimension settings.

    (if (= ADJ_FOR_PAPERS 1)
        (CREATE-DIM-STYLES VL-ADOC PAPER_ADJ_SCALE)
	(CREATE-DIM-STYLES VL-ADOC 1.0)
    ) ;_ End if.

) ;_ End defun.

(defun MAKELAYERS (HEADING   FILE1     ADJ_FOR_PAPERS	/		ACTLAYOUT FILE_HAND
		   FRZTEST   LINE1     LINE2		LINE3		LINE4
		   LINE5
		  )
    (if	(= (getvar "pstylemode") 0)
	(progn
	    (setq ACTLAYOUT (GET-CURRENT-LAYOUT))

;;; Refresh the plot information to make sure that the
;;; correct information is being used.

	    (vla-refreshplotdeviceinfo ACTLAYOUT)

	    (if	(not (member (vla-get-stylesheet ACTLAYOUT)
			     '("Color.stb"
			       "No Weight.stb"
			       "Standard - Full Size.stb"
			       "Standard - Half Size.stb"
			      )
		     ) ;_ End member.
		) ;_ End not.
		(if (= ADJ_FOR_PAPERS 1)
        	    (vla-put-stylesheet
		        ACTLAYOUT
		        "Standard - Full Size - Color.stb"
		    ) ;_ End vla-put-stylesheet.
		    (vla-put-stylesheet
		        ACTLAYOUT
		        "Standard - Full Size.stb"
		    ) ;_ End vla-put-stylesheet.
    		) ;_ End if.
	    ) ;_ End if.
	) ;_ End progn.
    ) ;_ End if.
    (setq FILE_HAND (open FILE1 "r")
	  LINE1	    (GETNEWLINE FILE_HAND)
    ) ;_ End setq.
    (while (/= (substr LINE1 1 (strlen HEADING)) HEADING)
	(setq LINE1 (GETNEWLINE FILE_HAND))
    ) ;_ End while.
    (setq LINE1	(GETNEWLINE FILE_HAND)
	  LINE2	(GETNEWLINE FILE_HAND)
	  LINE3	(GETNEWLINE FILE_HAND)
	  LINE4	(GETNEWLINE FILE_HAND)
	  LINE5	(GETNEWLINE FILE_HAND)
    ) ;_ End setq.
    (if	(and LINE1 LINE2 LINE3 LINE4 LINE5)
	(while (and (/= (substr LINE1 1 2) "**")
		    (/= (substr LINE2 1 2) "**")
		    (/= (substr LINE3 1 2) "**")
		    (/= (substr LINE4 1 2) "**")
		    (/= (substr LINE5 1 2) "**")
	       ) ;_ End and.
	    (LAYER_DATA_CHECK LINE1 LINE2 LINE3 LINE4 LINE5)
	    (setq LINE1	(GETNEWLINE FILE_HAND)
		  LINE2	(GETNEWLINE FILE_HAND)
		  LINE3	(GETNEWLINE FILE_HAND)
		  LINE4	(GETNEWLINE FILE_HAND)
		  LINE5	(GETNEWLINE FILE_HAND)
	    ) ;_ End setq.
	    (if	(or (= LINE1 NIL)
		    (= LINE2 NIL)
		    (= LINE3 NIL)
		    (= LINE4 NIL)
		    (= LINE5 NIL)
		) ;_ End or.
		(setq LINE1 "**")
	    ) ;_ End if.
	) ;_ End while.
    ) ;_ End if.
    (close FILE_HAND)
) ;_ End defun.

(defun LAYER_DATA_CHECK
       (LINE1 LINE2 LINE3 LINE4 LINE5 / PSMODE TEST TEST2)
    (setq PSMODE (getvar "pstylemode"))
    (if	LINE2
	(if (numberp (read LINE2))
	    (setq LINE2	(read LINE2)
		  TEST	1
	    ) ;_ End setq.
	    (setq TEST 2)
	) ;_ End if.
    ) ;_ End if.
    (if	(and (= TEST 1) (and (> LINE2 0) (< LINE2 250)))
	(setq TEST2 1)
    ) ;_ End if.
    (if	(and (= TEST 2)
	     (or (= (strcase LINE2) "RED")
		 (= (strcase LINE2) "YELLOW")
		 (= (strcase LINE2) "GREEN")
		 (= (strcase LINE2) "CYAN")
		 (= (strcase LINE2) "BLUE")
		 (= (strcase LINE2) "CYAN")
		 (= (strcase LINE2) "MAGENTA")
		 (= (strcase LINE2) "WHITE")
	     ) ;_ End or.
	) ;_ End and.
	(setq TEST2 1)
    ) ;_ End if.
    (cond
	((/= TEST2 1)
	 (princ "\nInvalid layer color found in layer setup file.")
	 (princ "\nCheck file and re-run setup.")
	) ;_ End cond for color check.
	((if LINE3
	     (not (tblsearch "ltype" LINE3))
	 ) ;_ End if.
	 (princ
	     "\nInvalid linetype or a linetype not loaded found in layer setup file."
	 ) ;_ End princ.
	 (princ "\nCheck file and re-run setup.")
	) ;_ End cond for linetype check.
	((if LINE1
	     (not (snvalid LINE1))
	 ) ;_ End if.
	 (princ "\nInvalid layer name found in layer setup file.")
	 (princ "\nCheck file and re-run setup.")
	) ;_ End cond for layer check.
	((and (/= (strcase LINE4) "PLOT") (/= (strcase LINE4) "NO"))
	 (princ
	     "\Invalid plot/no plot referance found in layer setup file."
	 ) ;_ End princ.
	 (princ "\nCheck file and re-run setup.")
	) ;_ End princ.

;;; Need to add a check for valid plot styles.

	((if LINE1
	     (snvalid LINE1)
	 ) ;_ End if.
	 (setq FRZTEST (tblsearch "layer" LINE1)
	       FRZTEST (cdr (assoc 70 FRZTEST))
	 ) ;_ End setq.
	 (if (or (= FRZTEST 1)
		 (= FRZTEST 3)
		 (= FRZTEST 5)
		 (= FRZTEST 7)
	     ) ;_ End or.
	     (command "_.layer" "thaw" LINE1 "")
	 ) ;_ End if.
	 (command "_.-layer"	    "m"	     LINE1    "c"      LINE2
		  LINE1	   "l"	    LINE3    LINE1    "p"      LINE4
		  LINE1
		 ) ;_ End command.
	 (if (= PSMODE 0)
	     (command "ps" LINE5 LINE1 "")
	     (command "")
	 ) ;_ End if.
	 (if (or (= FRZTEST 1)
		 (= FRZTEST 3)
		 (= FRZTEST 5)
		 (= FRZTEST 7)
	     ) ;_ End or.
	     (command "_.layer" "set" "0" "freeze" LINE1 "")
	 ) ;_ End if.
	) ;_ End cond making layer.
    ) ;_ End cond check for valid data in layer setup file.
) ;_ End defun.

(defun IMPORT_LAYOUTS (/ CLAYOUT FILE FILE_HAND LAYDISPST TEMP)
    (setq LAYDISPST (GET-LAYOUTS-DISP-STATE)
	  CLAYOUT   (GET-CURRENT-LAYOUT)
    ) ;_ End setq.

;;; Check the state of the layout tabs.
;;; If they are off - turn them on so program can function.

    (if	(= LAYDISPST :vlax-false)
	(PUT-LAYOUTS-DISP-STATE :vlax-true)
    ) ;_ End if.
    (foreach FILE (list	"Plotter Layouts.txt"
			"Scaled Layouts.txt"
			"Non-Scaled Layouts.txt"
		  ) ;_ End list.
	(setq FILE_HAND (open (findfile FILE) "r"))
	(while (setq TEMP (GETNEWLINE FILE_HAND))
	    (if	(= FILE "Plotter Layouts.txt")
		(progn
		    (MAKE_LAYOUT
			(strcat
			    TEMP
			    " "
			    (substr (getcfg "AppData/Setup_Data/P_Size")
				    3
			    ) ;_ End substr.
			    " Draft"
			) ;_ End strcat.
			FILE
		    ) ;_ End MAKE_LAYOUT.
		    (MAKE_LAYOUT
			(strcat
			    TEMP
			    " "
			    (substr (getcfg "AppData/Setup_Data/P_Size")
				    3
			    ) ;_ End substr.
			    " Final"
			) ;_ End strcat.
			FILE
		    ) ;_ End MAKE_LAYOUT.
		) ;_ End progn.
		(MAKE_LAYOUT TEMP FILE)
	    ) ;_ End if.
	) ;_ End while.
    ) ;_ End foreach.
    (PUT-CURRENT-LAYOUT CLAYOUT)
    (if	(= LAYDISPST :vlax-false)
	(PUT-LAYOUTS-DISP-STATE LAYDISPST)
    ) ;_ End if.
) ;_ End defun.

(defun MAKE_LAYOUT (LYOUT FILE / TEMPLATEFILE)
    (if	(= (getvar "pstylemode") 0)
	(setq TEMPLATEFILE (findfile "Named PS Plotting Templates.dwt"))
	(setq TEMPLATEFILE (findfile "Color PS Plotting Templates.dwt"))
    ) ;_ End if.

;;; Import System International units if they were specified.
    (if	(= (getcfg "AppData/Setup_Data/Plot_Unts") "si")
	(setq LYOUT (strcat LYOUT " - SI"))
    ) ;_ End if.

    (command "_.layout"
	     "template"
	     TEMPLATEFILE
	     LYOUT
    ) ;_ End command.

;;; Set last layout active.

    (AX:ACTIVATELASTLAYOUT)

    (command "_.zoom" (getvar "limmin") (getvar "limmax"))
    (if	(or (= FILE "Scaled Layouts.txt")
	    (= FILE "Plotter Layouts.txt")
	) ;_ End or.
	(command
	    "_.zoom"
	    (strcat (rtos (/ 1 (getvar "dimscale")) 2 8) "xp")
	) ;_ End command.
    ) ;_ End if.
    (command "_.pspace")
) ;_ End defun.

(defun ARCH_ACTIVATED (/ INDEX DWG_SCL)

;;; If S.I. units are active, change the units to U.S. Customary.  Arch units
;;; cannot be used with S.I. units.
    (if	(= (get_tile "si") "1")
	(set_tile "uscust" "1")
    ) ;_ End if.

;;; Convert the units of the drawing scale box.

    (set_tile "dwg_scale"
	      (LE_RTOS (distof (get_tile "dwg_scale") 2) 4)
    ) ;_ End set_tile.

;;; Store the value of the dimprec drop down box.

    (setq INDEX (get_tile "dimprec"))

;;; Remap the values of the drop down box to match the values currently set.

    (start_list "dimprec")
    (mapcar 'add_list PREC_LIST_ARCH)
    (end_list)

;;; Restore the value of the drop down box.

    (set_tile "dimprec" INDEX)

) ;_ End defun.

(defun DECIM_ACTIVATED (/ INDEX)

;;; Convert the units of the drawing scale box.

    (set_tile "dwg_scale"
	      (LE_RTOS (distof (get_tile "dwg_scale") 4) 2)
    ) ;_ End set_tile.

;;; Store the value of the dimprec drop down box.

    (setq INDEX (get_tile "dimprec"))

;;; Remap the values of the drop down box to match the values currently set.

    (start_list "dimprec")
    (mapcar 'add_list PREC_LIST_DIM)
    (end_list)

;;; Restore the value of the drop down box.

    (set_tile "dimprec" INDEX)

) ;_ End defun.

(defun SI_ACTIVATED (/ INDEX)
    (if	(= (get_tile "arch") "1")
	(progn
	    (alert
		"S.I. units cannot be set while units are architectural"
	    ) ;_ End alert.
	    (set_tile "uscust" "1")
	) ;_ End progn.
    ) ;_ End if.
) ;_ End defun.

(defun *ERROR* (MSG / LMBD1 LMBD2 VL-ADOC)
    (setq VL-ADOC (vla-get-activedocument (vlax-get-acad-object)))
    (mapcar '(lambda (LMBD1 LMBD2) (vla-setvariable VL-ADOC LMBD1 LMBD2))
	    (list "clayer" "regenmode" "expert" "cmdecho" "osmode")
	    (list CLAY RGMOD EXPRT CMD OSM)
    ) ;_ End mapcar.
    (vla-endundomark VL-ADOC)
    (princ)
) ;_ End defun.

;;; ==================================================

(load "ai_utils")

;|«Visual LISP© Format Options»
(72 4 40 2 T "End " 60 9 1 2 0 T T nil T)
;*** DO NOT add text below the comment! ***|;
