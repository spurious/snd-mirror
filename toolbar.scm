(provide 'snd-toolbar.scm)

(if (provided? 'snd-motif) 
    (begin
      (if (not (provided? 'snd-snd7.scm)) (load-from-path "snd7.scm"))           ; backward-mix
      (if (not (provided? 'snd-play.scm)) (load-from-path "play.scm"))           ; play-until-c-g
      (if (not (provided? 'snd-snd-motif.scm))                                   ; add-main-pane, add-tooltip (etc)
	  (load-from-path "snd-motif.scm"))))

(if (provided? 'snd-motif)
    (let* ((toolscroll (add-main-pane "toolscroll" xmScrolledWindowWidgetClass
				      (list XmNscrollingPolicy XmAUTOMATIC
					    XmNscrollBarDisplayPolicy XmSTATIC
					    XmNpaneMinimum (+ 48 26) ; leave room for scrollers
					    XmNpaneMaximum (+ 48 26)
					    XmNbackground (basic-color))))
	   (tools (XtCreateManagedWidget "tools" xmRowColumnWidgetClass toolscroll
					 (list XmNbackground (black-pixel)
					       XmNorientation XmHORIZONTAL))))
      (let* ((icon-save-as (list
			   "32 25 5 1"
			   "       c white"
			   ".      c lightslategrey"
			   "X      c black"
			   "o      c blue"
			   "O      c red"
			   "                                "
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " .......oooOOOOOOOOOOoo........X"
			   " .......oooOOOOOOOOOOoo........X"
			   " .......ooo          oo........X"
			   " .......ooo          oo........X"
			   " .......ooo          oo........X"
			   " .......ooooooooooooooo........X"
			   " .......ooooooooooooooo........X"
			   " .......ooooooooooooooo........X"
			   " .......ooo         ooo........X"
			   " .......ooo         ooo........X"
			   " .......ooo ooo     ooo........X"
			   " .......ooo ooo     ooo........X"
			   " ........oo ooo     ooo........X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	    
	    (icon-env-edit (list
			    "32 25 4 1"
			    "       c white"
			    ".      c yellow"
			    "X      c black"
			    "o      c darkgrey"
			    "                                "
			    " ..............................X"
			    " ......XXXX....................X"
			    " ......X.......................X"
			    " ......X.......................X"
			    " ......X.......................X"
			    " ......XXXX..X.XX..X.....X.....X"
			    " ......X.....X...X..X...X......X"
			    " ......X.....X...X..X...X......X"
			    " ......X.....X...X...X.X.......X"
			    " ......XXXX..X...X....X........X"
			    " ..............................X"
			    " ..............................X"
			    " ......XXXX......X..X..X.......X"
			    " ......X.........X.....X.......X"
			    " ......X......XX.X..X.XXX......X"
			    " ......X.....X..XX..X..X.......X"
			    " ......XXXX..X...X..X..X.......X"
			    " ......X.....X...X..X..X.......X"
			    " ......X.....X...X..X..X.......X"
			    " ......X.....X..XX..X..X.......X"
			    " ......XXXX...XX.X..X..XX......X"
			    " ..............................X"
			    " ..............................X"
			    " XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	    
	    (icon-no-fear (list
			   "32 25 3 1"
			   "       c white"
			   ".      c red"
			   "X      c black"
			   "                                "
			   " ..............................X"
			   " ..............................X"
			   " ..XXXX........................X"
			   " ..X...X.......................X"
			   " ..X....X......................X"
			   " ..X.....X..............XX..X..X"
			   " ..X.....X..XXXX..X.XXX..X.XXX.X"
			   " ..X.....X.X....X.XX...X....X..X"
			   " ..X....X..X....X.X....X....X..X"
			   " ..X...X...X....X.X....X....X..X"
			   " ..XXXX.....XXXX..X....X....X..X"
			   " ..............................X"
			   " ..............................X"
			   " ..XXXX........................X"
			   " ..X...X.......................X"
			   " ..X...X.......................X"
			   " ..XXXX................X.......X"
			   " ..X......XXXX..X.XXX..X..XXX..X"
			   " ..X.....X....X.XX...X.X.X.....X"
			   " ..X.....X....X.X....X.X.X.....X"
			   " ..X.....X...XX.X....X.X.X.....X"
			   " ..X......XXX.X.X....X.X..XXX..X"
			   " ..............................X"
			   " XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	    
	    (icon-regions-browser (list
				   "32 25 4 1"
				   "       c white"
				   ".      c yellow"
				   "X      c black"
				   "o      c darkgrey"
				   "                                "
				   " ..............................X"
				   " ......XXXX....................X"
				   " ......X...X...................X"
				   " ......X....X..................X"
				   " ......X...X...................X"
				   " ......XXXX.....XXXX...XXXX....X"
				   " ......X...X...X....X.X....X...X"
				   " ......X....X..XXXXX..X....X...X"
				   " ......X.....X..X.....X....X...X"
				   " ......X......X..XXXX..XXXXX...X"
				   " ..........................X...X"
				   " .......................XXX....X"
				   " ......XXXX......X..X..X.......X"
				   " ......X.........X.....X.......X"
				   " ......X......XX.X..X.XXX......X"
				   " ......X.....X..XX..X..X.......X"
				   " ......XXXX..X...X..X..X.......X"
				   " ......X.....X...X..X..X.......X"
				   " ......X.....X...X..X..X.......X"
				   " ......X.....X..XX..X..X.......X"
				   " ......XXXX...XX.X..X..XX......X"
				   " ..............................X"
				   " ..............................X"
				   " XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	    
	    (icon-mix-pane (list
			    "32 25 4 1"
			    "       c white"
			    ".      c yellow"
			    "X      c black"
			    "o      c darkgrey"
			    "                                "
			    " ..............................X"
			    " ..............................X"
			    " ......XX.....XX..X............X"
			    " ......X.X...X.X...............X"
			    " ......X..X.X..X..X..X....X....X"
			    " ......X...X...X..X...X..X.....X"
			    " ......X.......X..X....XX......X"
			    " ......X.......X..X....XX......X"
			    " ......X.......X..X...X..X.....X"
			    " ......X.......X..X..X....X....X"
			    " ..............................X"
			    " ..............................X"
			    " ......XXXX......X..X..X.......X"
			    " ......X.........X.....X.......X"
			    " ......X......XX.X..X.XXX......X"
			    " ......X.....X..XX..X..X.......X"
			    " ......XXXX..X...X..X..X.......X"
			    " ......X.....X...X..X..X.......X"
			    " ......X.....X...X..X..X.......X"
			    " ......X.....X..XX..X..X.......X"
			    " ......XXXX...XX.X..X..XX......X"
			    " ..............................X"
			    " ..............................X"
			    " XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	    
	    (icon-rec-pane (list
			    "32 25 4 1"
			    "       c white"
			    ".      c blue"
			    "X      c black"
			    "o      c red"
			    "                                "
			    " ..............................X"
			    " ..............................X"
			    " ..............................X"
			    " ..............................X"
			    " ..............................X"
			    " ..ooooo....oooooo....oooo.....X"
			    " ..oooooo...oooooo...oo..oo....X"
			    " ..oo...oo..oo......oo....oo...X"
			    " ..oo...oo..oo......oo....oo...X"
			    " ..oo...oo..oo......oo.........X"
			    " ..oo...oo..oooooo..oo.........X"
			    " ..oooooo...oooooo..oo.........X"
			    " ..oo..oo...oo......oo.........X"
			    " ..oo...oo..oo......oo.........X"
			    " ..oo...oo..oo......oo....oo...X"
			    " ..oo...oo..oooooo...oo..oo....X"
			    " ..oo...oo..oooooo....oooo.....X"
			    " ..............................X"
			    " ..............................X"
			    " ..............................X"
			    " ..............................X"
			    " ..............................X"
			    " ..............................X"
			    " XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	    
	    (icon-undo-it (list
			   "32 25 3 1"
			   "       c white"
			   ".      c antiquewhite"
			   "X      c black"
			   "                                "
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " .X....X.X....X.XXXX.....XXX...X"
			   " .X....X.XX...X.X...X...X...X..X"
			   " .X....X.X.X..X.X....X.X.....X.X"
			   " .X....X.X..X.X.X....X.X.....X.X"
			   " .X....X.X...XX.X....X.X.....X.X"
			   " .X....X.X....X.X....X.X.....X.X"
			   " .X....X.X....X.X....X.X.....X.X"
			   " ..X..X..X....X.X...X...X...X..X"
			   " ...XX...X....X.XXXX.....XXX...X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	    
	    (icon-redo-it (list
			   "32 25 3 1"
			   "       c white"
			   "X      c antiquewhite"
			   ".      c black"
			   "                                "
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " .XXXXX..XXXXXX.XXXX.....XXX...X"
			   " .X....X.X......X...X...X...X..X"
			   " .X....X.X......X....X.X.....X.X"
			   " .X....X.X......X....X.X.....X.X"
			   " .XXXXX..XXXXX..X....X.X.....X.X"
			   " .X.X....X......X....X.X.....X.X"
			   " .X..X...X......X....X.X.....X.X"
			   " .X...X..X......X...X...X...X..X"
			   " .X....X.XXXXXX.XXXX.....XXX...X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	    
	    (icon-exit-it (list
			   "32 25 4 1"
			   "       c white"
			   ".      c black"
			   "X      c grey"
			   "O      c red"
			   "                                "
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " .OOOOOO.OO.....OO.OOOO.OOOOOO.X"
			   " .OOOOOO..OO...OO...OO....OO...X"
			   " .O........OO.OO....OO....OO...X"
			   " .O.........OOO.....OO....OO...X"
			   " .OOOOO......O......OO....OO...X"
			   " .O.........OOO.....OO....OO...X"
			   " .O........OO.OO....OO....OO...X"
			   " .OOOOOO..OO...OO...OO....OO...X"
			   " .OOOOOO.OO.....OO.OOOO...OO...X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " ..............................X"
			   " XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	    
	    (icon-full-go (list
			   "32 25 4 1"
			   "X      c white"
			   "       c grey"
			   ".      c black"
			   "o      c darkgreen"
			   "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
			   "                                "
			   "                                "
			   "                                "
			   "                                "
			   "    .......................     "
			   "   .ooooooooooooooooooooooo.    "
			   "  .ooooooooooooooooooooooooo.   "
			   " .ooooooXXXXooooooXXXoooooooo.  "
			   ".ooooooXooooXooooXoooXoooooooo. "
			   ".oooooXoooooooooXoooooXooooooo. "
			   ".oooooXoooooooooXoooooXooooooo. "
			   ".oooooXoooXXXXooXoooooXooooooo. "
			   ".oooooXooooooXooXoooooXooooooo. "
			   ".oooooXooooooXooXoooooXooooooo. "
			   ".ooooooXooooXooooXoooXoooooooo. "
			   " .ooooooXXXXooooooXXXoooooooo.  "
			   "  .ooooooooooooooooooooooooo.   "
			   "   .ooooooooooooooooooooooo.    "
			   "    .......................     "
			   "                                "
			   "                                "
			   "                                "
			   "                                "
			   "................................"))
	    
	    (icon-play-direction-forward (list
					  "32 25 4 1"
					  "       c grey"
					  ".      c black"
					  "x      c white"
					  "X      c lightgrey"
					  "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
					  "                                "
					  "                                "
					  "                                "
					  "                                "
					  "        X              .        "
					  "       XX              ..       "
					  "      XXX              ...      "
					  "     XXXX              ....     "
					  "    XXXXX              .....    "
					  "   XXXXXX              ......   "
					  "  XXXXXXXXXXXXX  .............  "
					  "   XXXXXX              ......   "
					  "    XXXXX              .....    "
					  "     XXXX              ....     "
					  "      XXX              ...      "
					  "       XX              ..       "
					  "        X              .        "
					  "                                "
					  "                                "
					  "                                "
					  "                                "
					  "                                "
					  "                                "
					  "................................"))
	    
	    (icon-play-direction-backward (list
					   "32 25 4 1"
					   "       c grey"
					   ".      c black"
					   "x      c white"
					   "X      c lightgrey"
					   "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
					   "                                "
					   "                                "
					   "                                "
					   "                                "
					   "        .              X        "
					   "       ..              XX       "
					   "      ...              XXX      "
					   "     ....              XXXX     "
					   "    .....              XXXXX    "
					   "   ......              XXXXXX   "
					   "  .............  XXXXXXXXXXXXX  "
					   "   ......              XXXXXX   "
					   "    .....              XXXXX    "
					   "     ....              XXXX     "
					   "      ...              XXX      "
					   "       ..              XX       "
					   "        .              X        "
					   "                                "
					   "                                "
					   "                                "
					   "                                "
					   "                                "
					   "                                "
					   "................................"))
	    
	    (icon-loop-play (list
			     "32 25 11 1"
			     
			     ". c black"
			     "# c #00007f"
			     "a c #0000ff"
			     "b c #505850"
			     "c c #7f7f7f"
			     "d c orange"
			     "e c #dfdfdf"
			     "f c #ffff00"
			     "g c #ffffff"
			     "X c white"
			     "x c black"
			     
			     "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
			     "dddddddddddddddddddddddddddddddd"
			     "dddddddddddddddddddddddddddddddd"
			     "dddddddddddggggggggggddddddddddd"
			     "ddddddddddg..........cdddddddddd"
			     "dddddddddg.cccccccccc.cddddddddd"
			     "ddddddddg.cddddddddddg.cdddddddd"
			     "ddddddddg.cddddddddddg.cdddddddd"
			     "ddddddddg.c.dddddddddg.cdddddddd"
			     "ddddddg.c.c.ddddddddg...dddddddd"
			     "ddddddg.....ddddddddg...cddddddd"
			     "ddddddg.....dddddddg.....ddddddd"
			     "dddddddg...ddddddddg.....ddddddd"
			     "dddddddg...ddddddddg.c.c.ddddddd"
			     "ddddddddg.cddddddddddc.cdddddddd"
			     "ddddddddg.cddddddddddg.cdddddddd"
			     "ddddddddg.cddddddddddg.cdddddddd"
			     "ddddddddg.cddddddddddg.cdddddddd"
			     "dddddddddc.ddddddddgg.cddddddddd"
			     "ddddddddddc..........cdddddddddd"
			     "ddddddddddddcccccccccddddddddddd"
			     "dddddddddddddddddddddddddddddddd"
			     "dddddddddddddddddddddddddddddddd"
			     "dddddddddddddddddddddddddddddddd"
			     "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
	    
	    (icon-loop-stop (list
			     "32 25 11 1"
			     
			     ". c black"
			     "# c #00007f"
			     "a c #0000ff"
			     "b c #505850"
			     "c c #7f7f7f"
			     "d c red"
			     "e c #dfdfdf"
			     "f c #ffff00"
			     "g c #ffffff"
			     "X c white"
			     "x c black"
			     
			     "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
			     "dddddddddddddddddddddddddddddddd"
			     "dddXXddddddddddddddddddddXXddddd"
			     "ddddXXddddddddddddddddddXXdddddd"
			     "dddddXXddddggggggggggddXXddddddd"
			     "ddddddXXddg..........cXXdddddddd"
			     "dddddddXXg.ccccccccccXXddddddddd"
			     "ddddddddXXcdddddddddXX.cdddddddd"
			     "ddddddddgXXddddddddXXg.cdddddddd"
			     "ddddddddg.XXddddddXXdg.cdddddddd"
			     "ddddddg.c.cXXddddXXdg...dddddddd"
			     "ddddddg.....XXddXXddg...cddddddd"
			     "ddddddg.....dXXXXddg.....ddddddd"
			     "dddddddg...dddXXdddg.....ddddddd"
			     "dddddddg...ddXXXXddg.c.c.ddddddd"
			     "ddddddddg.cdXXddXXdddc.cdddddddd"
			     "ddddddddg.cXXddddXXddg.cdddddddd"
			     "ddddddddg.XXddddddXXdg.cdddddddd"
			     "ddddddddgXXddddddddXXg.cdddddddd"
			     "ddddddddXX.ddddddddgXXcddddddddd"
			     "dddddddXXdc..........XXddddddddd"
			     "ddddddXXddddcccccccccdXXdddddddd"
			     "dddddXXddddddddddddddddXXddddddd"
			     "ddddXXddddddddddddddddddXXdddddd"
			     "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
	    
	    (icon-full-stop (list
			     "32 25 4 1"
			     "X      c white"
			     "       c grey"
			     ".      c black"
			     "o      c red"
			     "XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"
			     "                                "
			     "                                "
			     "                                "
			     "                                "
			     "    .......................     "
			     "   .ooooooooooooooooooooooo.    "
			     "  .ooooooooooooooooooooooooo.   "
			     " .ooXXXooXXXXXooXXXoooXXXXXoo.  "
			     ".ooXoooXoooXoooXoooXooXooooXoo. "
			     ".oXooooooooXooXoooooXoXoooooXo. "
			     ".ooXoooooooXooXoooooXoXooooXoo. "
			     ".oooXXXooooXooXoooooXoXXXXXooo. "
			     ".ooooooXoooXooXoooooXoXooooooo. "
			     ".oooooooXooXooXoooooXoXooooooo. "
			     ".ooXoooXoooXoooXoooXooXooooooo. "
			     " .ooXXXooooXooooXXXoooXoooooo.  "
			     "  .ooooooooooooooooooooooooo.   "
			     "   .ooooooooooooooooooooooo.    "
			     "    .......................     "
			     "                                "
			     "                                "
			     "                                "
			     "                                "
			     "................................"))
	    
	    (icon-open-file (list
			     "32 25 4 1"
			     "x      c white"
			     "       c lightslategrey"
			     ".      c black"
			     "o      c yellow"
			     "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
			     "                                "
			     "                                "
			     "                                "
			     "                                "
			     "                                "
			     "                                "
			     "         ...........            "
			     "        .ooooooooooo.           "
			     "        .oooooooooooo.....      "
			     "        .oooooooooooooooo.      "
			     "      ....................      "
			     "      .oooooooooooooooo...      "
			     "       .oooooooooooooooo..      "
			     "       .oooooooooooooooo..      "
			     "        .oooooooooooooooo.      "
			     "         .ooooooooooooooo.      "
			     "          ................      "
			     "                                "
			     "                                "
			     "                                "
			     "                                "
			     "                                "
			     "                                "
			     "................................"))
	    
	    (icon-close-file (list
			      "32 25 4 1"
			      "       c white"
			      ".      c lightslategrey"
			      "X      c black"
			      "o      c yellow"
			      "                                "
			      " ..............................X"
			      " ..............................X"
			      " ..............................X"
			      " ..............................X"
			      " ..............................X"
			      " ..............................X"
			      " .........XXXXXX...............X"
			      " ........XooooooX..............X"
			      " .......XXXXXXXXXXXXXXXX.......X"
			      " .......XooooooooooooooX.......X"
			      " .......XooooooooooooooX.......X"
			      " .......XooooooooooooooX.......X"
			      " .......XooooooooooooooX.......X"
			      " .......XooooooooooooooX.......X"
			      " .......XooooooooooooooX.......X"
			      " .......XooooooooooooooX.......X"
			      " .......XooooooooooooooX.......X"
			      " .......XXXXXXXXXXXXXXXX.......X"
			      " ..............................X"
			      " ..............................X"
			      " ..............................X"
			      " ..............................X"
			      " ..............................X"
			      " XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	    
	    (icon-start-of-window (list
				   "32 25 3 1"
				   "       c lightblue"
				   ".      c black"
				   "x      c white"
				   "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
				   "                                "
				   "                                "
				   "                                "
				   "                                "
				   "               .        .       "
				   "              ..       ..       "
				   "             ...      ...       "
				   "            ....     ....       "
				   "           .....    .....       "
				   "         .......  .......       "
				   "       ..................       "
				   "         .......  .......       "
				   "           .....    .....       "
				   "            ....     ....       "
				   "             ...      ...       "
				   "              ..       ..       "
				   "               .        .       "
				   "                                "
				   "                                "
				   "                                "
				   "                                "
				   "                                "
				   "                                "
				   "................................"))
	    
	    (icon-mid-window (list
			      "32 25 3 1"
			      "       c lightblue"
			      ".      c black"
			      "x      c white"
			      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
			      "                                "
			      "                                "
			      "                                "
			      "                                "
			      "       .                .       "
			      "       ..              ..       "
			      "       ...            ...       "
			      "       ....          ....       "
			      "       .....        .....       "
			      "       .......    .......       "
			      "       ..................       "
			      "       .......    .......       "
			      "       .....        .....       "
			      "       ....          ....       "
			      "       ...            ...       "
			      "       ..              ..       "
			      "       .                .       "
			      "                                "
			      "                                "
			      "                                "
			      "                                "
			      "                                "
			      "                                "
			      "................................"))
	    
	    (icon-end-of-window (list
				 "32 25 3 1"
				 "       c lightblue"
				 ".      c black"
				 "x      c white"
				 "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
				 "                                "
				 "                                "
				 "                                "
				 "                                "
				 "       .        .               "
				 "       ..       ..              "
				 "       ...      ...             "
				 "       ....     ....            "
				 "       .....    .....           "
				 "       .......  .......         "
				 "       ..................       "
				 "       .......  .......         "
				 "       .....    .....           "
				 "       ....     ....            "
				 "       ...      ...             "
				 "       ..       ..              "
				 "       .        .               "
				 "                                "
				 "                                "
				 "                                "
				 "                                "
				 "                                "
				 "                                "
				 "................................"))
	    
	    (icon-start-of-file (list
				 "32 25 3 1"
				 "       c lightblue"
				 ".      c black"
				 "x      c white"
				 "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
				 "                                "
				 "                                "
				 "                                "
				 "                                "
				 "     ..        .        .       "
				 "     ..       ..       ..       "
				 "     ..      ...      ...       "
				 "     ..     ....     ....       "
				 "     ..    .....    .....       "
				 "     ..  .......  .......       "
				 "     ....................       "
				 "     ..  .......  .......       "
				 "     ..    .....    .....       "
				 "     ..     ....     ....       "
				 "     ..      ...      ...       "
				 "     ..       ..       ..       "
				 "     ..        .        .       "
				 "                                "
				 "                                "
				 "                                "
				 "                                "
				 "                                "
				 "                                "
				 "................................"))
	    
	    (icon-end-of-file (list
			       "32 25 3 1"
			       "       c lightblue"
			       ".      c black"
			       "x      c white"
			       "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
			       "                                "
			       "                                "
			       "                                "
			       "                                "
			       "       .        .        ..     "
			       "       ..       ..       ..     "
			       "       ...      ...      ..     "
			       "       ....     ....     ..     "
			       "       .....    .....    ..     "
			       "       .......  .......  ..     "
			       "       ....................     "
			       "       .......  .......  ..     "
			       "       .....    .....    ..     "
			       "       ....     ....     ..     "
			       "       ...      ...      ..     "
			       "       ..       ..       ..     "
			       "       .        .        ..     "
			       "                                "
			       "                                "
			       "                                "
			       "                                "
			       "                                "
			       "                                "
			       "................................"))
	    
	    (icon-forward-one-sample (list
				      "32 25 3 1"
				      "       c lightblue"
				      ".      c black"
				      "x      c white"
				      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
				      "                                "
				      "                                "
				      "                                "
				      "                                "
				      "       .           ..           "
				      "       ..         ...           "
				      "       ...       ....           "
				      "       ....        ..           "
				      "       .....       ..           "
				      "       .......     ..           "
				      "       .........   ..           "
				      "       .......     ..           "
				      "       .....       ..           "
				      "       ....        ..           "
				      "       ...         ..           "
				      "       ..        ......         "
				      "       .         ......         "
				      "                                "
				      "                                "
				      "                                "
				      "                                "
				      "                                "
				      "                                "
				      "................................"))
	    
	    (icon-back-one-sample (list
				   "32 25 3 1"
				   "       c lightblue"
				   ".      c black"
				   "x      c white"
				   "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
				   "                                "
				   "                                "
				   "                                "
				   "                                "
				   "            ..           .      "
				   "           ...          ..      "
				   "          ....         ...      "
				   "            ..        ....      "
				   "            ..       .....      "
				   "            ..     .......      "
				   "            ..   .........      "
				   "            ..     .......      "
				   "            ..       .....      "
				   "            ..        ....      "
				   "            ..         ...      "
				   "          ......        ..      "
				   "          ......         .      "
				   "                                "
				   "                                "
				   "                                "
				   "                                "
				   "                                "
				   "                                "
				   "................................"))
	    
	    (icon-forward-one-window (list
				      "32 25 3 1"
				      "       c lightblue"
				      ".      c black"
				      "x      c white"
				      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
				      "                                "
				      "                                "
				      "                                "
				      "                                "
				      "       .                        "
				      "       ..                       "
				      "       ...         ............ "
				      "       ....        ..         . "
				      "       .....       ..         . "
				      "       .......     ..         . "
				      "       .........   ..         . "
				      "       .......     ..         . "
				      "       .....       ..         . "
				      "       ....        ..         . "
				      "       ...         ............ "
				      "       ..                       "
				      "       .                        "
				      "                                "
				      "                                "
				      "                                "
				      "                                "
				      "                                "
				      "                                "
				      "................................"))
	    
	    (icon-back-one-window (list
				   "32 25 3 1"
				   "       c lightblue"
				   ".      c black"
				   "x      c white"
				   "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
				   "                                "
				   "                                "
				   "                                "
				   "                                "
				   "                         .      "
				   "                        ..      "
				   " .............         ...      "
				   " .          ..        ....      "
				   " .          ..       .....      "
				   " .          ..     .......      "
				   " .          ..   .........      "
				   " .          ..     .......      "
				   " .          ..       .....      "
				   " .          ..        ....      "
				   " .............         ...      "
				   "                        ..      "
				   "                         .      "
				   "                                "
				   "                                "
				   "                                "
				   "                                "
				   "                                "
				   "                                "
				   "................................"))
	    
	    (icon-next-mix-point (list
				  "32 25 4 1"
				  "       c lightblue"
				  "x      c white"
				  ".      c black"
				  "X      c black"
				  "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
				  "                                "
				  "                   XX     XX    "
				  "                   X X   X X    "
				  "                   X  X X  X    "
				  "       .           X   X   X    "
				  "       ..          X       X    "
				  "       ...         X       X    "
				  "       ....        X       X    "
				  "       .....          XXX       "
				  "       .......         X        "
				  "       .........       X        "
				  "       .......         X        "
				  "       .....           X        "
				  "       ....            X        "
				  "       ...            XXX       "
				  "       ..          XX     XX    "
				  "       .             X   X      "
				  "                      X X       "
				  "                       X        "
				  "                      X X       "
				  "                     X   X      "
				  "                   XX     XX    "
				  "                                "
				  "................................"))
	    
	    (icon-last-mix-point (list
				  "32 25 4 1"
				  "       c lightblue"
				  "x      c white"
				  ".      c black"
				  "X      c black"
				  "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
				  "                                "
				  "     XX     XX                  "
				  "     X X   X X                  "
				  "     X  X X  X                  "
				  "     X   X   X           .      "
				  "     X       X          ..      "
				  "     X       X         ...      "
				  "     X       X        ....      "
				  "        XXX          .....      "
				  "         X         .......      "
				  "         X       .........      "
				  "         X         .......      "
				  "         X           .....      "
				  "         X            ....      "
				  "        XXX            ...      "
				  "     XX     XX          ..      "
				  "       X   X             .      "
				  "        X X                     "
				  "         X                      "
				  "        X X                     "
				  "       X   X                    "
				  "     XX     XX                  "
				  "                                "
				  "................................"))
	    
	    (icon-mixer (list
			 "32 25 5 1"
			 "       c white"
			 ".      c grey"
			 "X      c black"
			 "o      c darkgrey"
			 "O      c yellow"
			 "                                "
			 " ..............................X"
			 " ..............................X"
			 " ..............................X"
			 " ...ooo...ooo...ooo......ooo...X"
			 " ...ooo...ooo...ooo......ooo...X"
			 " ...ooo...ooo...ooo......ooo...X"
			 " ...oXo...oXo.XXXXXXX....ooo...X"
			 " ...oXo...oXo.XOOOOOX..XXXXXXX.X"
			 " ...oXo...oXo.XXXXXXX..XOOOOOX.X"
			 " .XXXXXXX.oXo...oXo....XXXXXXX.X"
			 " .XOOOOOX.oXo...oXo......oXo...X"
			 " .XXXXXXX.oXo...oXo......oXo...X"
			 " ...oXo...oXo...oXo......oXo...X"
			 " ...oXo.XXXXXXX.oXo......oXo...X"
			 " ...oXo.XOOOOOX.oXo......oXo...X"
			 " ...oXo.XXXXXXX.oXo......oXo...X"
			 " ...oXo...oXo...oXo......oXo...X"
			 " ...ooo...ooo...ooo......oXo...X"
			 " ...ooo...ooo...ooo......ooo...X"
			 " ...ooo...ooo...ooo......ooo...X"
			 " ..............................X"
			 " ..............................X"
			 " ..............................X"
			 " XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX"))
	    
	    (icon-zooming-in (list
			      "32 25 4 1"
			      "       c lightblue"
			      ".      c black"
			      "x      c white"
			      "X	c black"
			      "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
			      "                                "
			      "                                "
			      "        XXXXXXXXXXX             "
			      "       X           X            "
			      "      X      X      X           "
			      "     X       X       X          "
			      "     X       X       X          "
			      "     X   XXXXXXXXX   X          "
			      "     X       X       X          "
			      "     X       X       X          "
			      "      X      X      X           "
			      "       X           X            "
			      "        XXXXXXXXXXX             "
			      "                 XX             "
			      "                  XX            "
			      "                   XX           "
			      "                    XX          "
			      "                     XX         "
			      "                      XX        "
			      "                       XX       "
			      "                        XX      "
			      "                         XX     "
			      "                                "
			      "................................"))
	    
	    (icon-zooming-out (list
			       "32 25 4 1"
			       "       c lightblue"
			       ".      c black"
			       "x      c white"
			       "X	c black"
			       "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
			       "                                "
			       "                                "
			       "        XXXXXXXXXXX             "
			       "       X           X            "
			       "      X             X           "
			       "     X               X          "
			       "     X               X          "
			       "     X   XXXXXXXXX   X          "
			       "     X               X          "
			       "     X               X          "
			       "      X             X           "
			       "       X           X            "
			       "        XXXXXXXXXXX             "
			       "                 XX             "
			       "                  XX            "
			       "                   XX           "
			       "                    XX          "
			       "                     XX         "
			       "                      XX        "
			       "                       XX       "
			       "                        XX      "
			       "                         XX     "
			       "                                "
			       "................................"))
	    
	    (icon-open-mix-file (list
				 "32 25 5 1"
				 "x      c white"
				 "       c lightslategrey"
				 ".      c black"
				 "o      c yellow"
				 "X      c darkblue"
				 "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
				 "           XX     XX            "
				 "           X X   X X            "
				 "           X  X X  X            "
				 "           X   X   X            "
				 "           X       X            "
				 "           X       X            "
				 "         ..X.......X            "
				 "        .ooooooooooo.           "
				 "        .oooooXXXoooo.....      "
				 "        .ooooooXooooooooo.      "
				 "      .........X..........      "
				 "      .ooooooooXooooooo...      "
				 "       .oooooooXoooooooo..      "
				 "       .oooooooXoooooooo..      "
				 "        .oooooXXXoooooooo.      "
				 "         .ooooooooooooooo.      "
				 "          .XX.....XX......      "
				 "             X   X              "
				 "              X X               "
				 "               X                "
				 "              X X               "
				 "             X   X              "
				 "           XX     XX            "
				 "................................"))
	    
	    (play-pixmap (make-pixmap tools icon-full-go))
	    (stop-pixmap (make-pixmap tools icon-full-stop))
	    (play-forward-pixmap (make-pixmap tools icon-play-direction-forward))
	    (play-backward-pixmap (make-pixmap tools icon-play-direction-backward))
	    (loop-pixmap (make-pixmap tools icon-loop-play))
	    (loop-stop-pixmap (make-pixmap tools icon-loop-stop)))
	    
	(for-each
	 (lambda (icon-callback-and-tooltip)
	   (let* ((icon (car icon-callback-and-tooltip))
		  (callback (cadr icon-callback-and-tooltip))
		  (tooltip (caddr icon-callback-and-tooltip))
		  (button
		   (XtCreateManagedWidget "button" xmPushButtonWidgetClass tools
					  (list XmNlabelPixmap (make-pixmap tools icon)
						XmNlabelType   XmPIXMAP
						XmNwidth       32
						XmNheight      32))))
	     (XtAddCallback button XmNactivateCallback callback)
	     (if (string? tooltip)
		 (add-tooltip button tooltip))))
	 
	 (list
	  (list icon-open-file
		(lambda (w c i) 
		  (open-file-dialog))
		"Open file")
	  
	  (list icon-close-file
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (close-sound)))
		"Close file")
	  
	  (list icon-save-as
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (save-sound-dialog)))
		"Save file")
	  
	  (list icon-open-mix-file
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (mix-file-dialog)))
		"Mix file")
	  
	  (list icon-rec-pane
		(lambda (w c i) 
		  (recorder-dialog))
		"Recorder")
	  
	  (list icon-env-edit
		(lambda (w c i) 
		  (enved-dialog))
		"Envelope editor")
	  
	  (list icon-regions-browser
		(lambda (w c i) 
		  (if (not (null? (regions))) 
		      (view-regions-dialog)))
		"Region editor")
	  
	  (list icon-mix-pane
		(lambda (w c i) 
		  (mix-dialog))
		"Mix editor")
	  
	  (list icon-undo-it
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (undo)))
		"Undo edit")
	  
	  (list icon-redo-it
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (redo)))
		"Redo edit")
	  
	  (list icon-full-go
		(let ((playing #f)
		      (already-hooked #f))
		  (lambda (w c i)
		    (if (not (null? (sounds)))
			(begin
			  (if (not already-hooked)
			      (begin
				(add-hook! stop-dac-hook ; play either ended normally or was interrupted in some way
					   (lambda ()
					     (if playing 
						 (XtVaSetValues w (list XmNlabelPixmap play-pixmap)))
					     (set! playing #f)))
				(set! already-hooked #t)))
			  (if playing
			      (stop-playing) ; hook takes care of the rest
			      (begin
				(set! playing #t)
				(play)
				(XtVaSetValues w (list XmNlabelPixmap stop-pixmap))))))))
		"Play")
	  
	  (list icon-play-direction-forward
		(lambda (w c i)
		  (if (not (null? (sounds)))
		      (begin
			(set! (speed-control) (- (speed-control)))
			(XtVaSetValues w (list XmNlabelPixmap (if (>= (speed-control) 0.0) play-forward-pixmap play-backward-pixmap))))))
		"Reverse")
	  
	  (list icon-loop-play
		(let ((looping #f)
		      (already-hooked #f))
		  (lambda (w c i)
		    (if (not (null? (sounds)))
			(begin
			  (if (not already-hooked)
			      (begin
				(add-hook! stop-dac-hook ; play either ended normally or was interrupted in some way
					   (lambda ()
					     (if looping
						 (XtVaSetValues w (list XmNlabelPixmap loop-pixmap)))
					     (set! looping #f)))
				(set! already-hooked #t)))
			  (if looping 
			      (c-g!)
			      (begin
				(set! looping #t)
				(XtVaSetValues w (list XmNlabelPixmap loop-stop-pixmap))
				(play-until-c-g)))))))
		"Loop play")
	  
	  (list icon-start-of-file
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (set! (cursor) 0))) 
		"Move to start of file")
	  
	  (list icon-start-of-window
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (set! (cursor) (left-sample)))) 
		"Move to start of window")
	  
	  (list icon-back-one-window
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (if (> (left-sample) 0) 
			  (set! (left-sample) (max 0 (- (* 2 (left-sample)) (right-sample)))))))
		"Move back one window")
	  
	  (list icon-back-one-sample
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (set! (cursor) (max 0 (1- (cursor))))))
		"Move back one sample")
	  
	  (list icon-mid-window
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (set! (cursor) (inexact->exact (round (/ (+ (left-sample) (right-sample)) 2))))))
		"Move to mid-window")
	  
	  (list icon-forward-one-sample
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (set! (cursor) (min (1- (frames)) (1+ (cursor))))))
		"Move forward one sample")
	  
	  (list icon-forward-one-window
		(lambda (w c i) 
		  (if (not (null? (sounds)))
		      (if (< (right-sample) (frames)) (set! (left-sample) (right-sample)))))
		"Move forward one window")
	  
	  (list icon-end-of-window
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (set! (cursor) (right-sample))))
		"Move to end of window")
	  
	  (list icon-end-of-file
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (set! (cursor) (1- (frames)))))
		"Move to end of file")
	  
	  (list icon-last-mix-point
		(lambda (w c i) 
		  (backward-mix)) 
		"Previous mix")
	  
	  (list icon-next-mix-point
		(lambda (w c i) 
		  (forward-mix))
		"Next mix")
	  
	  (list icon-zooming-in
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (let ((midpoint (* 0.5 (apply + (x-bounds))))
			    (range (* -0.25 (apply - (x-bounds))))
			    (dur (/ (frames) (srate))))
			(set! (x-bounds) (list (max 0.0 (- midpoint range))
					       (min dur (+ midpoint range)))))))
		"Zoom in")
	  
	  (list icon-zooming-out
		(lambda (w c i) 
		  (if (not (null? (sounds))) 
		      (let ((midpoint (* 0.5 (apply + (x-bounds))))
			    (range (abs (apply - (x-bounds))))
			    (dur (/ (frames) (srate))))
			(set! (x-bounds) (list (max 0.0 (- midpoint range))
					       (min dur (+ midpoint range)))))))
		"Zoom out")
	  
	  (list icon-exit-it
		(lambda (w c i) 
		  (exit)) 
		"Quit Snd")
	  
	  )))))


	
	
(if (provided? 'snd-gtk)
    (let* ((main-pane (caddr (main-widgets)))                       ; MAIN_PANE = top level vbox = (caddr (main-widgets))
	   (toolbar (gtk_toolbar_new))
	   (tips (gtk_tooltips_new)))
      (gtk_box_pack_start (GTK_BOX main-pane) toolbar #f #f 0)
      (gtk_box_reorder_child (GTK_BOX main-pane) toolbar 1)         ; put toolbar just under the top level menubar
      (gtk_widget_show toolbar)

      (let ((open-button (gtk_tool_button_new_from_stock GTK_STOCK_OPEN)))
	(gtk_toolbar_insert (GTK_TOOLBAR toolbar) open-button -1) ; -1 => put at end
;	(gtk_tool_item_set_is_important (GTK_TOOL_ITEM open-button) #f)
	(gtk_widget_show (GTK_WIDGET open-button))
	(gtk_tool_item_set_tooltip (GTK_TOOL_ITEM open-button) tips "open sound" #f)
	(g_signal_connect open-button "clicked" (lambda (w data) (open-file-dialog))))
	  
      (let ((close-button (gtk_tool_button_new_from_stock GTK_STOCK_CLOSE)))
	(gtk_toolbar_insert (GTK_TOOLBAR toolbar) close-button -1)
	(gtk_widget_show (GTK_WIDGET close-button))
	(gtk_tool_item_set_tooltip (GTK_TOOL_ITEM close-button) tips "close selected sound" #f)
	(g_signal_connect close-button "clicked" (lambda (w data) (if (not (null? (sounds))) (close-sound)))))
	  
      (let ((save-as-button (gtk_tool_button_new_from_stock GTK_STOCK_SAVE_AS)))
	(gtk_toolbar_insert (GTK_TOOLBAR toolbar) save-as-button -1)
	(gtk_widget_show (GTK_WIDGET save-as-button))
	(gtk_tool_item_set_tooltip (GTK_TOOL_ITEM save-as-button) tips "save selected sound in new file" #f)
	(g_signal_connect save-as-button "clicked" (lambda (w data) (if (not (null? (sounds))) (save-sound-dialog)))))

      (let ((undo-button (gtk_tool_button_new_from_stock GTK_STOCK_UNDO)))
	(gtk_toolbar_insert (GTK_TOOLBAR toolbar) undo-button -1)
	(gtk_widget_show (GTK_WIDGET undo-button))
	(gtk_tool_item_set_tooltip (GTK_TOOL_ITEM undo-button) tips "undo last edit" #f)
	(g_signal_connect undo-button "clicked" (lambda (w data) (if (not (null? (sounds))) (undo)))))
	  
      (let ((redo-button (gtk_tool_button_new_from_stock GTK_STOCK_REDO)))
	(gtk_toolbar_insert (GTK_TOOLBAR toolbar) redo-button -1)
	(gtk_widget_show (GTK_WIDGET redo-button))
	(gtk_tool_item_set_tooltip (GTK_TOOL_ITEM redo-button) tips "redo last undone edit" #f)
	(g_signal_connect redo-button "clicked" (lambda (w data) (if (not (null? (sounds))) (redo)))))

      (let ((sof-button (gtk_tool_button_new_from_stock GTK_STOCK_GOTO_FIRST)))
	(gtk_toolbar_insert (GTK_TOOLBAR toolbar) sof-button -1)
	(gtk_widget_show (GTK_WIDGET sof-button))
	(gtk_tool_item_set_tooltip (GTK_TOOL_ITEM sof-button) tips "move cursor to start of file" #f)
	(g_signal_connect sof-button "clicked" (lambda (w data) (if (not (null? (sounds))) (set! (cursor) 0)))))

      (let ((sow-button (gtk_tool_button_new_from_stock GTK_STOCK_GO_BACK)))
	(gtk_toolbar_insert (GTK_TOOLBAR toolbar) sow-button -1)
	(gtk_widget_show (GTK_WIDGET sow-button))
	(gtk_tool_item_set_tooltip (GTK_TOOL_ITEM sow-button) tips "move back one window" #f)
	(g_signal_connect sow-button "clicked" (lambda (w data) 
						 (if (not (null? (sounds))) 
						     (if (> (left-sample) 0) 
							 (set! (left-sample) (max 0 (- (* 2 (left-sample)) (right-sample)))))))))

      (let ((sof-button (gtk_tool_button_new_from_stock GTK_STOCK_GO_FORWARD)))
	(gtk_toolbar_insert (GTK_TOOLBAR toolbar) sof-button -1)
	(gtk_widget_show (GTK_WIDGET sof-button))
	(gtk_tool_item_set_tooltip (GTK_TOOL_ITEM sof-button) tips "go forward one window" #f)
	(g_signal_connect sof-button "clicked" (lambda (w data) 
						 (if (not (null? (sounds))) 
						     (if (< (right-sample) (frames)) (set! (left-sample) (right-sample)))))))
	  
      (let ((sof-button (gtk_tool_button_new_from_stock GTK_STOCK_GOTO_LAST)))
	(gtk_toolbar_insert (GTK_TOOLBAR toolbar) sof-button -1)
	(gtk_widget_show (GTK_WIDGET sof-button))
	(gtk_tool_item_set_tooltip (GTK_TOOL_ITEM sof-button) tips "move cursor to end of file" #f)
	(g_signal_connect sof-button "clicked" (lambda (w data) (if (not (null? (sounds))) (set! (cursor) (1- (frames)))))))

      (let ((sof-button (gtk_tool_button_new_from_stock GTK_STOCK_ZOOM_IN)))
	(gtk_toolbar_insert (GTK_TOOLBAR toolbar) sof-button -1)
	(gtk_widget_show (GTK_WIDGET sof-button))
	(gtk_tool_item_set_tooltip (GTK_TOOL_ITEM sof-button) tips "zoom in" #f)
	(g_signal_connect sof-button "clicked" (lambda (w data)
						 (if (not (null? (sounds))) 
						     (let ((midpoint (* 0.5 (apply + (x-bounds))))
							   (range (* -0.25 (apply - (x-bounds))))
							   (dur (/ (frames) (srate))))
						       (set! (x-bounds) (list (max 0.0 (- midpoint range))
									      (min dur (+ midpoint range)))))))))
      (let ((sof-button (gtk_tool_button_new_from_stock GTK_STOCK_ZOOM_OUT)))
	(gtk_toolbar_insert (GTK_TOOLBAR toolbar) sof-button -1)
	(gtk_widget_show (GTK_WIDGET sof-button))
	(gtk_tool_item_set_tooltip (GTK_TOOL_ITEM sof-button) tips "zoom out" #f)
	(g_signal_connect sof-button "clicked" (lambda (w data)
						 (if (not (null? (sounds))) 
						     (let ((midpoint (* 0.5 (apply + (x-bounds))))
							   (range (abs (apply - (x-bounds))))
							   (dur (/ (frames) (srate))))
						       (set! (x-bounds) (list (max 0.0 (- midpoint range))
									      (min dur (+ midpoint range)))))))))

      (let ((exit-button (gtk_tool_button_new_from_stock GTK_STOCK_QUIT)))
	(gtk_toolbar_insert (GTK_TOOLBAR toolbar) exit-button -1) ; -1 => put at end
	(gtk_widget_show (GTK_WIDGET exit-button))
	(gtk_tool_item_set_tooltip (GTK_TOOL_ITEM exit-button) tips "exit Snd" #f)
	(g_signal_connect exit-button "clicked" (lambda (w data) (exit))))

      (gtk_toolbar_set_tooltips (GTK_TOOLBAR toolbar) #t)
      (gtk_tooltips_enable (GTK_TOOLTIPS tips))
      ))
