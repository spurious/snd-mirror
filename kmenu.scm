;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; add mnemonics to the main menu (gtk only)


(use-modules (srfi srfi-1))
(use-modules (srfi srfi-13))

(let ()
  ;; returns #f if there is no label (e.g. for tearoff menu item)
  (define (get-label widget)
    (cond ((gtk_bin_get_child (GTK_BIN widget))
	   => (lambda (child) 
		(gtk_label_get_text (GTK_LABEL child))))
	  (else #f)))
  
  (define (get-mnemonic widget)
    (let ((child (gtk_bin_get_child (GTK_BIN widget))))
      (if (not child) #f
	  (let ((symbol (gtk_label_get_mnemonic_keyval (GTK_LABEL child))))
	    (if (equal? GDK_VoidSymbol symbol) #f
		symbol)))))
  
  (define (set-label widget text)
    (gtk_label_set_text_with_mnemonic 
     (GTK_LABEL (gtk_bin_get_child (GTK_BIN widget))) 
     text))
  
  ;; range(n) -> (0, 1, ..., n-1)
  (define (range n)
    (let rec ((i n) 
	      (res '()))
      (if (positive? i)
	  (rec (- i 1) (cons (- i 1) res))
	  res)))
  
  (define (g_list->list g)
    (map (lambda (i) (g_list_nth_data g i)) 
	 (range (g_list_length g))))
  
  (define (root-menu)
    (GTK_MENU (list-ref (menu-widgets) 0)))
  
  (define (menu-items menu)
    (map GTK_MENU_ITEM 
	 (g_list->list (gtk_container_get_children (GTK_CONTAINER menu)))))
  
  ;; returns #f if there is no submenu
  (define (submenu menu-item)
    (gtk_menu_item_get_submenu (GTK_MENU_ITEM menu-item)))
  
  (define (mnemonizer)
    (define used-chars '())
    (define (test-char x) 
      (let ((c (char-upcase x)))
	(if (member c used-chars) 
	    #f
	    (begin
	      (set! used-chars (cons c used-chars)) 
	      #t))))
    (define (newname s) 
      (let ((i (string-index s test-char))) 
	(if i (string-replace s "_" i i) s))) 
    newname)
  
  (define (mnemonize-menu m) 
    (define mnemonize (mnemonizer)) 
    (define (op menu-item)
      (cond ((get-label menu-item) 
	     => (lambda (s) 
		  (if (not (string-index s #\_)) ;ignore labels with underscore
		      (set-label menu-item (mnemonize s))))))
      (cond ((submenu menu-item)
	     => (lambda (m) (mnemonize-menu m)))))
    (map op (menu-items m)))
  
  
  
  ;;add mqnemonics to all menu items recursively
  (mnemonize-menu (root-menu))
    
  (let ()
    (define (add-tearoff menu)
      (let ((tearoff (gtk_tearoff_menu_item_new)))
	(gtk_menu_shell_prepend (GTK_MENU_SHELL menu) tearoff)
	(gtk_widget_show tearoff)))
    
    ;;lookup top-level menu by name
    (define (find-menu name)
      (find (lambda (x) (equal? name (get-label x))) 
	    (menu-items (root-menu))))
    
    ;;lookup menu item by path 
    ;;e.g. (find-menu-rec '("Options" "Zoom focus" "window right edge"))
    (define (find-menu-rec path)
      (let rec ((menu (root-menu)) (path path))
	(if (null? path) #f
	    (let ((item (find (lambda (x) (equal? (car path) (get-label x)))
			      (menu-items menu))))
	      (if (not item) #f 
		  (if (null? (cdr path)) 
		      item 
		      (rec (submenu item) (cdr path))))))))
    
    ;;add tearoffs to Ladspa menu submenus
    (cond ((find-menu "Ladspa")
	   => (lambda (m) 
		(add-tearoff (submenu m))
		(map add-tearoff 
		     (filter-map submenu (menu-items (submenu m)))))))
    
    ;;add tearoff to Effects menu
    (cond ((find-menu "Effects")
	   => (lambda (m) (add-tearoff (submenu m)))))
    

    (let ()
      ;; ;;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; accellerators
      
      ;;(define (mksetaccel)
      (define setaccel!
	(let setaccel ()
	  (define rootwin (GTK_WINDOW (list-ref (main-widgets) 1))) 
	  (define agroup (gtk_accel_group_new))
	  (define (set-accel widget gdk_key gdk_mod)
	    (if widget
		(begin
		  (gtk_widget_add_accelerator (GTK_WIDGET widget) "activate" agroup 
					      gdk_key gdk_mod GTK_ACCEL_VISIBLE)
		  (gtk_window_add_accel_group rootwin agroup))))
	  set-accel))

      (if (not (procedure? (key-binding #\o 4)))
	  (setaccel! (find-menu-rec '("File" "Open"))     GDK_O GDK_CONTROL_MASK))

      (if (not (procedure? (key-binding #\w 4)))
	  (setaccel! (find-menu-rec '("File" "Close"))    GDK_W GDK_CONTROL_MASK))
      (if (not (procedure? (key-binding #\s 4)))
	  (setaccel! (find-menu-rec '("File" "Save"))     GDK_S GDK_CONTROL_MASK))
      
      ;;(setaccel! (find-menu-rec '("File" "Save as"))  GDK_S (+ GDK_CONTROL_MASK GDK_SHIFT_MASK))
      (if (not (procedure? (key-binding #\n 4)))
	  (setaccel! (find-menu-rec '("File" "New"))      GDK_N GDK_CONTROL_MASK))
      
      (if (not (procedure? (key-binding #\z 4)))
	  (setaccel! (find-menu-rec '("Edit" "Undo"))       GDK_Z GDK_CONTROL_MASK))

      (if (not (procedure? (key-binding #\z 5)))
	  (setaccel! (find-menu-rec '("Edit" "Redo"))       GDK_Z (+ GDK_CONTROL_MASK GDK_SHIFT_MASK)))
      
      (if (not (procedure? (key-binding #\a 4)))
	  (setaccel! (find-menu-rec '("Edit" "Select all")) GDK_A GDK_CONTROL_MASK))
      )
    )
)











