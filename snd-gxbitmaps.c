#include "snd.h"

/* TODO: need transparent backgrounds */

#if (HAVE_XPM) || (USE_GTK)

/* -------------------------------- SOUND ICONS (lock, bomb) -------------------------------- */

static char *mini_lock_xpm[] = {
"16 14 6 1",
" 	c None s None",
".	c gray50",
"X	c black",
"o	c white",
"O	c yellow",
"-      c ivory2 s basiccolor",
"------.XXX.-----",
"-----X.ooo.X----",
"----..oXXXo..---",
"----XoX...XoX---",
"----XoX.--XoX.--",
"----XoX.--XoX.--",
"---XXXXXXXXXXX--",
"---XOOOOOOOOOX.-",
"---XO.......OX.-",
"---XOOOOOOOOOX.-",
"---XO.......OX.-",
"---XOOOOOOOOOX.-",
"---XXXXXXXXXXX.-",
"----...........-"};

char **mini_lock_bits(void) {return(mini_lock_xpm);}

static char *blank_xpm[] = {
"16 14 6 1",
" 	c None s None",
".	c gray50",
"X	c black",
"o	c white",
"O	c yellow",
"-      c ivory2 s basiccolor",
"----------------",
"----------------",
"----------------",
"----------------",
"----------------",
"----------------",
"----------------",
"----------------",
"----------------",
"----------------",
"----------------",
"----------------",
"----------------",
"----------------"};

char **blank_bits(void) {return(blank_xpm);}

static char *speed_l_xpm[] = {
"16 12 6 1",
" 	c None s None",
".	c gray50",
"X	c black",
"o	c white",
"O	c yellow",
"-      c ivory2 s basiccolor",
"-----X------------",
"----X----X--------",
"---X--------------",
"--X--------X------",
"-X----------------",
"XXXXXXXXXX-X-X----",
"-X----------------",
"--X--------X------",
"---X--------------",
"----X----X--------",
"-----X------------",
"------X-----------"};

char **speed_l_bits(void) {return(speed_l_xpm);}

static char * speed_r_xpm[] = {
"16 12 6 1",
" 	c None s None",
".	c gray50",
"X	c black",
"o	c white",
"O	c yellow",
"-      c ivory2 s basiccolor",
"--------X---------",
"----X----X--------",
"----------X-------",
"--X--------X------",
"------------X-----",
"X-X-XXXXXXXXXX----",
"------------X-----",
"--X--------X------",
"----------X-------",
"----X----X--------",
"--------X---------",
"-------X----------"};

char **speed_r_bits(void) {return(speed_r_xpm);}

/* bomb for out-of-date in-core data fuse shortens with sparks flying off; */

static char * mini_bomb0_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"-------...------","------.---.-----","-----.-----.----","----...-----.---","---.....----.---","--.X#o...----.--","-X.#X....X---.--",
"-..oX.....---O-O","-.......O.-O-OO-","-......Xo.--OOO-","-X.....X.X--O---","--.......-------","---X...X--------","----------------"};

static char * mini_bomb1_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"-------...------","------.---.-----","-----.-----.----","----...-----.---","---.....----.---","--.X#o...----.--","-X.#X....X---Y--",
"-..oX.....---YYY","-.......O.-YYOOY","-......Xo.--OOY-","-X.....X.X--Y---","--.......-------","---X...X--------","----------------"};

static char * mini_bomb2_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"-------...------","------.---.-----","-----.-----.----","----...-----.---","---.....----.---","--.X#o...----Y--","-X.#X....X---YY-",
"-..oX.....---OYO","-.......O.--O-OO","-......Xo.--Y-Y-","-X.....X.X------","--.......----Y--","---X...X--------","----------------"};

static char * mini_bomb3_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"-------...------","------.---.-----","-----.-----.----","----...-----.---","---.....----.---","--.X#o...----Y--","-X.#X....X---OO-",
"-..oX.....-YYYYO","-.......O.----O-","-......Xo.----O-","-X.....X.X----Y-","--.......-------","---X...X--------","------------YY--"};

static char * mini_bomb4_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"-------...------","------.---.-----","-----.-----.--Y-","----...-----.---","---.....----O---","--.X#o...---OO--","-X.#X....X-YOYO-",
"-..oX.....--OYY-","-.......O.------","-......Xo.-Y----","-X.....X.X------","--.......-------","---X...X--------","----------------"};

static char * mini_bomb5_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"-------...------","------.---.-----","-----.-----.----","----...-----Y---","---.....----OO--","--.X#o...--OOO--","-X.#X....X---YO-",
"-..oX.....---YY-","-.......O.-----Y","-......Xo.-----O","-X.....X.X------","--.......-------","---X...X--------","----------------"};

static char * mini_bomb6_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"-------...------","------.---.-----","-----.-----OO-O-","----...-----YO--","---.....----O---","--.X#o...-YY-OO-","-X.#X....X--Y---",
"-..oX.....------","-.......O.------","-......Xo.------","-X.....X.X------","--.......-----OO","---X...X------Y-","----------------"};

static char * mini_bomb7_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"-------...----OO","------.---O-----","-----.-----OOYY-","----...-----YOO-","---.....-YY-O---","--.X#o...-------","-X.#X....X--YO--",
"-..oX.....---Y--","-.......O.------","-......Xo.------","-X.....X.X------","--.......-------","---X...X--------","--------------YY"};

static char * mini_bomb8_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"-------..Y------","------.--OO-----","-----.----OOO-Y-","----...---OO--O-","---.....-YY----O","--.X#o...-------","-X.#X....X------",
"-..oX.....--YO--","-.......O.---O--","-......Xo.------","-X.....X.X------","--.......-------","---X...X--------","----------------"};

static char * mini_bomb9_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"-------YOY--O---","------.---YO----","-----.----OYY---","----...---------","---.....----YY--","--.X#o...-------","-X.#X....X------",
"-..oX.....------","-.......O.--YO--","-......Xo.---Y--","-X.....X.X------","--.......-------","---X...X--------","----------------"};

static char * mini_bomb10_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"-----OYYOYOO----","-----YOO--YO----","-----.-----YY---","----...---------","---.....--------","--.X#o...----OO-","-X.#X....X---Y--",
"-..oX.....------","-.......O.------","-......Xo.------","-X.....X.X------","--.......-------","---X...X--------","----------------"};

static char * mini_bomb11_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"-----OOYOO--O---","----OOY----O----","---OOOO---------","----...---------","---.....--------","--.X#o...-------","-X.#X....X------",
"-..oX.....------","-.......O.------","-......Xo.------","-X.....X.X------","--.......---OO--","---X...X--------","----------------"};

static char * mini_bomb12_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"-----OO---------","--YYOOYYY-------","YYOOOOYYYY------","--OOOOO---------","---.....--------","--.X#o...-------","-X.#X....X------",
"-..oX.....------","-.......O.------","-......Xo.------","-X.....X.X------","--.......-------","---X...X--------","------------YY--"};

static char * mini_bomb13_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"----------------","----OOY---------","--YYYYY-Y-------","--Y-OOO---------","---.YOY.--------","--.X#o...-------","-X.#X....X------",
"-..oX.....------","-.......O.------","-......Xo.------","-X.....X.X------","--.......-------","---X...X--------","----------------"};

static char * mini_bomb14_xpm[] = {
"16 14 8 1"," 	c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","-      c ivory2 s basiccolor","Y      c yellow",
"----------------","----------------","----------------","----------------","---.....--------","--.X#o...-------","-X.#X....X------",
"-..oX.....------","-.......O.------","-......Xo.------","-X.....X.X------","--.......-------","---X...X--------","----------------"};

char **mini_bomb_bits(int n)
{
  switch (n)
    {
    case 0: return(mini_bomb0_xpm); break;
    case 1: return(mini_bomb1_xpm); break;
    case 2: return(mini_bomb2_xpm); break;
    case 3: return(mini_bomb3_xpm); break;
    case 4: return(mini_bomb4_xpm); break;
    case 5: return(mini_bomb5_xpm); break;
    case 6: return(mini_bomb6_xpm); break;
    case 7: return(mini_bomb7_xpm); break;
    case 8: return(mini_bomb8_xpm); break;
    case 9: return(mini_bomb9_xpm); break;
    case 10: return(mini_bomb10_xpm); break;
    case 11: return(mini_bomb11_xpm); break;
    case 12: return(mini_bomb12_xpm); break;
    case 13: return(mini_bomb13_xpm); break;
    case 14: return(mini_bomb14_xpm); break;
    default: return(NULL); break;
    }
}


static char * mini_glass0_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.OOOOOOOOOO.c-","-.cOOOOOOOOOOc.-","--cOOOOOOOOOOc--","----cOOOOOOc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---cooooooooc---","--cooooooooooc--","-.cooooooooooc.-","-cooooooooooooc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass1_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.OOOOoOOOOO.c-","-.cOOOOOOOOOOc.-","--cOOOOOOOOOOc--","----cOOOOOOc----","-----.coOc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---coooOooooc---","--cooooooooooc--","-.cooooooooooc.-","-coooooOooooooc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass2_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.OOOOoooOOO.c-","-.cOOOOOOOOOOc.-","--cOOOOOOOOOOc--","----cOOOOOOc----","-----.cOoc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---cooooooooc---","--cooooooooooc--","-.cooooOoooooc.-","-cooooOOOoooooc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass3_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.OOOoooOOOO.c-","-.cOOOOooOOOOc.-","--cOOOOOOOOOOc--","----cOOOOOOc----","-----.coOc.-----","-------cc-------",
"-----.cOoc.-----","----cooooooc----","---cooooooooc---","--cooooooooooc--","-.coooOooooooc.-","-coooOOOOoooooc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass4_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.OOOooooOOO.c-","-.cOOOOoooOOOc.-","--cOOOOOoOOOOc--","----cOOoOOOc----","-----.cOOc.-----","-------cc-------",
"-----.coOc.-----","----cooooooc----","---cooooooooc---","--cooooooooooc--","-.coooOOOooooc.-","-coooOOOOoooooc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass5_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.OOOoooooOO.c-","-.cOOOOooooOOc.-","--cOOOOOoOOOOc--","----cOOOOOOc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---cooooooooc---","--coooOooooooc--","-.coooOOOOoooc.-","-coooOOOOOooooc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass6_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.OOOoooooOO.c-","-.cOOOOooooOOc.-","--cOOOOoooOOOc--","----cOOOOoOc----","-----.cOOc.-----","-------cc-------",
"-----.cOoc.-----","----coooOooc----","---cooooooooc---","--cooooooooooc--","-.coooOOOOoooc.-","-coooOOOOOOoooc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass7_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.OOooooooOO.c-","-.cOOOoooooOOc.-","--cOOOOoOoOOOc--","----cOOoOOOc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---cooooOoooc---","--coooOOoooooc--","-.cooOOOOOoooc.-","-cooOOOOOOOoooc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass8_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.OoooooooOO.c-","-.cOOooooooOOc.-","--cOOOooooOOOc--","----cOOOOOOc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---cooooooooc---","--coooOOOooooc--","-.cooOOOOOoooc.-","-cooOOOOOOOOOoc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass9_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.ooooooooOO.c-","-.cOoooooooOOc.-","--cOOoooooOOOc--","----cOOoOOOc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooOoooc----","---cooooooooc---","--cooOOOOOOOoc--","-.coOOOOOOOOooc.-","-coOOOOOOOOOOOoc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass10_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.oooooooooo.c-","-.cOoooooooooc.-","--cOOooooooOOc--","----cOOooOOc----","-----.coOc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---cooOOOOooc---","--cooOOOOOOooc--","-.coOOOOOOOOoc.-","-cOOOOOOOOOOOoc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass11_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.oooooooooo.c-","-.cooooooooooc.-","--cOOooooooooc--","----cOOooOOc----","-----.coOc.-----","-------cc-------",
"-----.cooc.-----","----cooOoooc----","---cooOOOOooc---","--coOOOOOOOooc--","-.cOOOOOOOOOoc.-","-cOOOOOOOOOOOoc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass12_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.oooooooooo.c-","-.cooooooooooc.-","--cooooooooooc--","----cOoooOOc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooOOooc----","---coOOOOOooc---","--cOOOOOOOOOoc--","-.cOOOOOOOOOOc.-","-cOOOOOOOOOOOOc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass13_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.oooooooooo.c-","-.cooooooooooc.-","--cooooooooooc--","----cooooooc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooOOooc----","---cOOOOOOOOc---","--cOOOOOOOOOOc--","-.cOOOOOOOOOOc.-","-cOOOOOOOOOOOOc-","-XXXXXXXXXXXXXX-"};

static char * mini_glass14_xpm[] = {"16 14 6 1",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan","-      c ivory2 s basiccolor",
"-XXXXXXXXXXXXXX-","-c.oooooooooo.c-","-.cooooooooooc.-","--cooooooooooc--","----cooooooc----","-----.cooc.-----","-------cc-------",
"-----.cOOc.-----","----cOOOOOOc----","---cOOOOOOOOc---","--cOOOOOOOOOOc--","-.cOOOOOOOOOOc.-","-cOOOOOOOOOOOOc-","-XXXXXXXXXXXXXX-"};

char **mini_glass_bits(int n)
{
  switch (n)
    {
    case 0: return(mini_glass0_xpm); break;
    case 1: return(mini_glass1_xpm); break;
    case 2: return(mini_glass2_xpm); break;
    case 3: return(mini_glass3_xpm); break;
    case 4: return(mini_glass4_xpm); break;
    case 5: return(mini_glass5_xpm); break;
    case 6: return(mini_glass6_xpm); break;
    case 7: return(mini_glass7_xpm); break;
    case 8: return(mini_glass8_xpm); break;
    case 9: return(mini_glass9_xpm); break;
    case 10: return(mini_glass10_xpm); break;
    case 11: return(mini_glass11_xpm); break;
    case 12: return(mini_glass12_xpm); break;
    case 13: return(mini_glass13_xpm); break;
    case 14: return(mini_glass14_xpm); break;
    default: return(NULL); break;
    }
}

/* -------------------------------- PROGRAM ICON -------------------------------- */

static char *snd_icon_xpm[] = {
"48 48 9 1",
". c white m white",
"B c black m black",
"l c ivory1 m white s lightestcolor",
"a c ivory2 m white s basiccolor",
"d c ivory3 m black s darkcolor",
"X c ivory4 m black s darkestcolor",
"b c lightsteelblue1 m white s textcolor",
"r c red m black s cursorcolor",
"g c lightgreen m black s mixercolor",
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
"aaXXda.........................................a",
"aaXXda..BB........................rrrrrrr......a",
"aaXXda..BB...........................r.........a",
"aaXXda..BB...........................r.........a",
"aaXXda..BB...........................r.........a",
"aaXXda..BB...........................r.........a",
"aaXXda..BB........BBB................r.........a",
"aaXXda..BB......B.....B..............r.........a",
"aadBda..BB....B........B.............r.........a",
"aalBda..BB..B...........B............r.........a",
"aalBda..BB.B.............B...........r.........a",
"aalBda..BBB..............B...........r.........a",
"aaXXda..BB...............B...........r.........a",
"aaXXda..BB................B..........r.........a",
"aaXXda..BB................B..........r.........a",
"aaXXda..BB.................B.........r.........a",
"aaXXda..BB.................B.........r........Ba",
"aaXXda..BB..................B........r.......B.a",
"aaXXda..BB...................B.......r......B..a",
"aaXXda..BB.....................B.....r.....B...a",
"aaXXda..BB......................B....r...B.....a",
"aaXXda..BB.......................B...r.B.......a",
"aaXXda..BB.........................BBr.........a",
"aaXXda..BB...........................rr........a",
"aaXXda..BB...........................rrr.......a",
"aaaaaa..BB...........................rrrr......a",
"aaaaaa..BB...........................rrr.......a",
"aa.lda..BB...........................rr........a",
"aa.lda..BB...........................r.........a",
"aa.lda..BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB.a",
"aaddda..BBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBB.a",
"aaaaaa.........................................a",
"aaaaaa.........................................a",
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
"aBbbaaaddddd....ddddddddddddddddddddddddddddddda",
"adbbaaadddddllllddddddddddddddddddddddddddddddda",
"adbbaaadddddllllddddddddddddddddddddddddddddddda",
"adbbaaaXXXXXXXXXXXXXXXXXXXXddddXXXXXXXXXXXXXXXXa",
"aaaaaaaXXXXXXXXXXXXXXXXXXXXddddXXXXXXXXXXXXXXXXa",
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaggg",
"aaaaBBBlBBBlBBBlBBBlBaaaaaaaaaaaaaaa....a....ggg",
"aaaaBlBlBlBlBlBlBlllBaaaaaaaaaaaaaaallldallldggg",
"aaaaBlBlBBBlBlBlBBllBaaaaaaaaaaaaaaallldallldaaa",
"aaaaBlBlBlBlBlBlBlllBaaaaaaaaaaaaaaaddddaddddaaa",
"aaaaBBBlBBBlBBBlBBBlBaaaaaaaaaaaaaaaaaaaaaaaaaaa",
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};

char **snd_icon_bits(void) {return(snd_icon_xpm);}


/* -------------------------------- XPM VU METER LABEL PIXMAP DATA -------------------------------- */

static char * onlabel_xpm[] = {
"240 160 11 1",
" 	c #FFFFFBFBD2D2",
".	c #FFFFFAFAC8C8",
"X	c #FFFFF8F8B9B9",
"-	c #FFFFF7F7AFAF",
"O	c #FFFF00000000",
"+	c #000000000000",
"@	c #FFFFF6F6A0A0",
"#	c #FFFFF3F38080",
"$	c #FFFFEFEF6060",
"%	c #FFFFECEC4040",
"&	c #FFFFE9E92020",
"             ..........             .................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.......                ..................             ",
"            ........................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.........................................            ",
"           ....         ...........................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX...............................        ..           ",
"          ........................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.....................               .....          ",
"                 ................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.........................................         ",
"        ........................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX....                       ..............        ",
"       ....   ..............     ..............XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX....................................            ",
"      ........................................XXX.............XXXXXXXXXX.............XXXXXXXXXXXXXXXXXXXXXXXX------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX....................................           ",
"         ....................................XXXXXXXXXXXXXXXXXXXXXXX.......XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-------------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX......................................        ",
"     ..........     ........................XXX...XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX-----------------------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX..........................             .     ",
"     ..............................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX----------------------------------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.......        ........................     ",
"     .....................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX----------------------------------------------------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX......................................     ",
"     ....      ............................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX--------------------------------------------------------------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX...........        ..................     ",
"     ..................        .........XXXXXXXXX-XXXXXXXXX--------------------------------------------------------------------------------------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX....................................     ",
"     ..................................XXXX....XX-------XXXXX--------------------------------------------------OOOO---------OOOOOO-----------------------------------------------XXXXXXXXXXXXXXXXXXXXXXX.............     .................     ",
"     ......    .......................XXXXXXXXXXXXXXXXXXXX----------------------------------------------------OO--OO--------OO------------------------------------------------------XXXXX----XXXXXXX------...................        ......     ",
"     ................................XXXXXXXXXXXX-------------------------------------------------------------OO--OO--------OO------------------------------------------------------------------XXXXXXXXX-.................................     ",
"     .............. ................XXXXXXXXXXXXX--XXXXXXXXX--------------------------------------------------OO--OO--------OO--------------------------------------------------------XXXXXXXX--------XXX-X................................     ",
"           ........................X..XXXXXXXXXXX-XXXXX-------------------------------------------------------OO--OO--------OOOOO------------------------------------------------------------------XXXXX--XX............             ......     ",
"     ......               ........XXXXXXXXXXXXXXX-------------------------------------------------------------OO--OO------------OO----------------------------------------------------------XXXXXXXX------XXX..............................     ",
"     ............................XXXXXXXXXXXXXXXXXXXXXXX------------------------------------------------------OO--OO------------OO--------------------------------------------------------XXXXXX----------XXXX.............................     ",
"        .............................XXXXXXXXXXXX-------------------------------------------------------------OO--OO---OO---OO--OO--------------------------------------------------------------XXXXXXXXX-XXXXX.........         ..........     ",
"     ..........................XXXXXXXXXXXXXXXXXXXXXX----------------------O-----------------------------------OOOO----OO---OOOOO--------------------------------------OOO--------------------------------XXXXXX...........................     ",
"     .........        ........XX....XXXXXXXXXXXXX------------------------OO-------------------------------------------------------------------------------------------OOOOO------------------------XXXXXXXXXXXX............................     ",
"     .........................XXXXXXXXXXXXXXXXXXX-XXX-------------------OO--------------------------------------------------------------------------------------------OO--OO------------------------XXXXXXXXXXXXXX.........................     ",
"           ......................XXXXXXXXXXXXXXXX-------------------OOO--OO--O---------------------------------------------------------------------------------------OO---OO------------------------------XXXXXXXXX.   ....................     ",
"            .......................XXXXXXXXXXXXXXX----------------OO--OO-OOOO-OO-------------------------------------------------------------------------------------OO---OO------------------------------XXXXXXXX.........................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX-----------------OO--OO------OO------------------------------------------------------------------------------------OO----OO-------O----------------------XXXXXX...........................     ",
"           ................XXXXX.......XXXXXXXXXXX----------------OO--OO------O-------------------------------------------------------------------------------------OO---OO---------OOO----------------XXXXXXXXXXX.............                 ",
"     ...........................XXXXXXXXXXXXXXXXX------------------O--O----OOOO-------------------------------------------------------------------------------------OO--OO-----------OOOO-----------------XXXXXXXXX........................     ",
"         ..................XXXXXXXXXXXXXXXXXXXXXXX-----OO------------O---O-OO-------------------------------------------++------------------------------------------OOOOO-------------OO------------------XXX....XXX..........       ......     ",
"     ..........................XXXXXXXXXXXXXXXXXX-----OOOOO----------OO-OO----------------------------------------------++-------------------------------------------OOO-------------OO--OOOO----------XX-XXXXXXXXXX.......................     ",
"         ..................XXXXXXXXXXXXXXXXXXXXXXX----O---OO---------O-OOO----------------------------------------------++--------------------------------------------------OO------OO---OO--O------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX-----OO---OO---------OO------------------------------------------------++-------------------------------------------------OO------OO---OO----O--------XXXXXXX.....X....................        ",
"       .......................XXXXXXXXXXXXXXXXXXXX----OO---OO---OO----------------------------@@@@@@@@@@@@@@@@+@@@@@@@@@++@@@@@@@+@@@@@@@@@@@@@@@@@@@@---------------------------OO-----OO--------------X-XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX-----OO---OO---OO---------------------------@@@@@@@+@@@@@@@@@+@@@@@@@@@++@@@@@@@+@@@@@@@@@+@@@@@@@@@@@@@@----------------------OO--------OO--------------XXXXXXXXXX........             ..     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX------OO--OO------------------------------@@@@@@@@@+@@@@@@@@@+@@@@@@@@@++@@@@@@@+@@@@@@@@@+@@@@@@@@@@@@@@@--------------------------------O-------------XXXXXXXXXXX.......................     ",
"     ........................XXXXXXXXXXXXXXXXXXXXX-----OO--OO----------------------------@+@@@@@@@@@+@@@@@@@@@+@@@@@@@@@++@@@@@@@+@@@@@@@@@+@@@@@@@@@+@@@@@@--------------------------O----OO-------------XXXXXXXXXX...     ...............     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX-------OOOO-----------------------------@+@@@@@@@@@@+@@@@@@@@+@@@@@@@@@++@@@@@@@+@@@@@@@@+@@@@@@@@@@+@@@@@@@--------------------------OO-OOO-------------XX...............................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX---------------------++-----------------@+@@@@@@@@@@+@@@@@@@@+@@@@@@@@@++@@@@@@@+@@@@@@@@+@@@@@@@@@@+@@@@@@@@-----------++--------------OO---------------XXXXXXXXXX.......................     ",
"     ..........................XXXXXXXXXXXXXXXXXX---------------------++---------+-------@@+@@@@@@@@@+@@@@@@@@++++++++++++++++++++++@@@@@@+@@@@@@@@@+@@@@@@@@@+----------++-------------------------------XXX....XXX...........        ....     ",
"     ......................XXXXXXXXXXXXXXXXXXXXX-----------------------++--------+-------@@+@@@@@@@@@+++++++++++++++++++++++++++++++++++++++@@@@@@@@+@@@@@@@@@+@--------++--------------------------------XXXXXXXXXX.......................     ",
"     .............................XXXXXXXXXXXXX------------------------++--------+-------@@+@@@@++++++++++++++@@@@@@@@@@@@@@@@@@@@@@+++++++++++++@@@+@@@@@@@@@+@@-------++--------------------------------XXXX.............................     ",
"     ..............................XXXXXXXXXXXX------------------------+++--------+------@@+++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@++++++++++@@@@@@@+@@@@-----++---------------------------------XXXXXXX..........................     ",
"     .................................XXXXXXXXX-------------------------++--------+-----+++++++++@@@@@@@@@@@@@+++++++++++++++++++++@@@@@@@@@@@@@@++++++++@@@@+@@@@-----++---------------------------------XXXXXX................................",
"     .................................XXXXXXXXX-------------------------++--@@@@@@+@@+++++++@@@@@@@@@@++++++++@@@@@@@@@@@@@@@@@@@@@++++++++@@@@@@@@@@@++++++++@@@@-----++---------------------------------XXXXXX................................",
"     .................................XXXXXXXXX--------------------------++@@@@@@@+++++++@@@@@@@@+++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+++++@@@@@@@@@+++++++@@----++----------------------------------XXXXXX................................",
"     .................................XXXXXXXXX---------------+----------++@@@@++++++@@@@@@@+++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+++++@@@@@@@@++++++@@@++---------+------------------XXXXXXXXXXXX................................",
"     .................................XXXXXXXXX----------------+---------@++++++++@@@@@@@+++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+++@@@@@@@@+++++++---------+------------------XXXXXXXXXXXXX................................",
"     .................................XXXXXXXXX----------------+--------@@+++++@@@@@@++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@++++@@@@@@@+++++--------+----------------XXXXXXXXXXX....................................",
"     ..................................XXXXXXXX-----------------+------++++++@@@@@+++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+++@@@@@@+++++-----+----------------.......XXXXX....................................",
"     .................................XXXXXXXXX-----------------+----+++++@@@@@+++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+++@@@@@@++++---+----------------XXXXXXXXXXXXXXXX......OOOO......................",
"     ...................................XXXXXXX-------+----------+-+++++@@@@@++@@@@@@@@@@@@@@@@@################################################@@@@@@@@@@@@@@@@@@++@@@@@@+++++----------+------XXXXXXXXXX..............OO......................",
"     ...................OOO...........XXXXXXXXX--------+---------+++++@@@@@++@@@@@@@@@@@@@@@@@@##################################################@@@@@@@@@@@@@@@@@@@++@@@@@@++++--------+-------XXXXXXXXXXXX...X.......OO.......................",
"     ..................OO.OO...............XXXX--------+-------+++++@@@@+++@@@@@@@@@@@@@@@@@@@####################################################@@@@@@@@@@@@@@@@@@@@+++@@@@-++++------+-------XXXXXXXXXXXXXXXX......OO........................",
"     ..................O...OO...........XXXXXXX---------+-----++++@@@@++@@@@@@@@@@@@@@@@@@@@@######################################################@@@@@@@@@@@@@@@@@@@@@@++@@@--++++---+--------XXXXXXXXXX...........OO.........................",
"     ..................OO...OO........XXXXXXXXX---------+---++++-@@@++@@@@@@@@@@@@@@@@@@@@@@########################################################@@@@@@@@@@@@@@@@@@@@@@@++@@---++++-+--------XXXXXXXXXXXXXXXX..O.O.O.........................",
"     ..................OO....OO..........XXXXXX----------+++++---@++@@@@@@@@@@@@@@@@@@@@@@@##########################################################@@@@@@@@@@@@@@@@@@@@@@@@++-----+++---------XXXXXXXXXXXXXXXX..OOOO..........................",
"     ...................OO....O..........XXXX+X---------++++----++@@@@@@@@@@@@@@@@@@@@@@@@############################################################@@@@@@@@@@@@@@@@@@@@@@@@@++-----+++-------XX+XXXXXXXXXXXXX....O...........................",
"     ....................OO..OO.........XXXXXX+--------++++----+-@@@@@@@@@@@@@@@@@@@@@@@@##############################################################@@@@@@@@@@@@@@@@@@@@@@@@--+-----+++------X+XXXXXXXXXXXXXX.....O..........................",
"     ......................OOO........XXXXXXXXX+-----++++----++--@@@@@@@@@@@@@@@@@@@@@@@################################################################@@@@@@@@@@@@@@@@@@--------++-----+++----+XXXXX.......XXX................OOO.............",
"     ...................................XXXXXXX-+---++++---++---------@@@@@@@@@@@@@@@@@##################################################################@@@@@@@@@@@@@@@@@@@@@@-----++----+++--+XXXXXXXXXXXXXXXX.........O.....OO..O............",
"     ........OOOO.....................XXXXXXXXX--+-+++----+------@@@@@@@@@@@@@@@@@@@@@####################################################################@@@@@@@@@@@@@@@@@@@@@-------+-----+++-XXXXXXXXXXXXXXXX........OOO...OO....O...........",
"     .......O...OO.....OO.............XXXXXXXXX--++++---++--------------@@@@@@@@@@@@@######################################################################@@@@@@@@@@@@@@@@@-----------++----+++XXXXXXXXXXXXXXXX.........O....OO...OO...........",
"     .......OO...OO....OO.............+XXXXXXXX-+++----+----------@@@@@@@@@@@@@@@@@@########################################################################@@@@@@@@@@@@@@@@@@@----------+-----++XXXXXXXX+XXXXXX.............OO....OO...........",
"     ........OO...OO..................X+XXXXXXX+++---++-----------------@@@@@@@@@@@##########################################################################@@@@@@@@@@@@@@@@@------------++----+++XXXXX+XXXXXXX.............O....OO............",
"     ........OO...OO..................XX+XXXX++++---+-------------@@@@@@@@@@@@@@@@############################################################################@@@@@@@@@@@@@@@@@-------------+---X+++XXX+XXXXXXXX.............O...OO.............",
"     .........OO...O..................XXX+XX+++----+-------------@@@@@@@@--@@@@@@######################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#######################@@@@@@@@@@@@@@@---------------+--XXX++X+XXXXXXXXX..............OOOO..............",
"     ..........O...O........++........XXXX++++X--++--------------@@@@@@@@@@@@@@@######################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#######################@@@@@@@@@@@@@@@---------------++XXXX++XXXXXXXXXX...++...........................",
"     ...........OOO..........++.......XXXX+++XX-+---------------------@@@@@@@@@@#####################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$######################@@@@@@@@@@@@@-------------------+XXXX+++XXXXXXXX..++............................",
"     .........................++......XXX+++XXX+-----------------@@@@@@@@@@@@@@@####################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#####################@@@@@@@@@@@@@@@-----------------X+XXXX+++XXXXXXX.++.............................",
"     ..........................++.....X+++XXXX+-----------------------@@@@@@@@@@###################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$####################@@@@@@@@@@@@@@------------------XX+XXXXX++XXXXXX++..............................",
"     ...........................++....+++XXX++X-------------------@@@@@@@@@@@@@@##################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###################@@@@@@@@@@@@@@@-----------------XXX++XXXX++XXXX++...............................",
"     ............................++..+++XXX+XXX------------------@@@@@@@@@@@@@@@#################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$##################@@@@@@@@@@@@@@@-----------------XXXXX+XXXX++XX++................................",
"     .............................+++.+XXX+XXXX----------------------@@@@@@@@@@@################$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#################@@@@@@@@@@@@@@------------------XXXXXX+XXXX++++X................................",
"     ..............................++.XXX+XXXXX------------------@@@@@@@@@@@@@@@###############$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$################@@@@@@@@@@@@@@@-----------------XXXXXXX+XXXX++XX................................",
"     ...........................XXXXXXXX+XXXXXX------------------@-@@@@@@@@@@@@@##############$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###############@@@@@@@@@@@@@@@-----------------XXXXXXXX+XXXXXXXXXXX.......................     ",
"     ........................XXXXXXXXXX+XXXXXXX-------------------@@@@@@@@@@@@@@#############$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$##############@@@@@@@@@@@@@@@-----------------XXXXXXXXX+XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXX+XXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXX+XXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX-------------------@@@@@@@@@@@@@@############$$$$$$$$$$$$$$$$$%%%%%%%%%%%%%%%%%%%%%$$$$$$$$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ...........................XXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$$$$$$$%%%%%%%%%%%%%%%%%%%%%%%$$$$$$$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$$$$$$%%%%%%%%%%%%%%%%%%%%%%%%%$$$$$$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ..........................XXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$$$$$%%%%%%%%%%%%%%%%%%%%%%%%%%%$$$$$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$$$$%%%%%%%%%%%%%%%%%%%%%%%%%%%%%$$$$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     .........................XXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$$$%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%$$$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$$%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%$$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ........................XXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%%%%%&&&&&&&&&&&&%%%%%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%%%%&&&&&&&&&&&&&&%%%%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%%%&&&&&&&&&&&&&&&&%%%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%%&&&&&&&&&&&&&&&&&&%%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%&&&&&&&&&&&&&&&&&&&&%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%&&&&&&&&&&&&&&&&&&&&%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%&&&&&&&&&&&&&&&&&&&&%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%&&&&&&&&&&&&&&&&&&&&%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%&&&&&&&&&&&&&&&&&&&&%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%&&&&&&&&&&&&&&&&&&&&%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%&&&&&&&&&&&&&&&&&&&&%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------@@@@@@@@@@@@@@@############$$$$$$$$$$%%%%%%%%&&&&&&&&&&&&&&&&&&&&%%%%%%%$$$$$$$$$$#############@@@@@@@@@@@@@@@-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++",
"++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++"};

static char * offlabel_xpm[] = {
"240 160 2 1",
" 	c #FFFFFFFFFFFF",
".	c #000000000000",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                               ....         ......                                                                                                              ",
"                                                                                                              ..  ..        ..                                                                                                                  ",
"                                                                                                              ..  ..        ..                                                                                                                  ",
"                                                                                                              ..  ..        ..                                                                                                                  ",
"                                                                                                              ..  ..        .....                                                                                                               ",
"                                                                                                              ..  ..            ..                                                                                                              ",
"                                                                                                              ..  ..            ..                                                                                                              ",
"                                                                             .                                ..  ..   ..   ..  ..                                                                                                              ",
"                                                                           ..                                  ....    ..   .....                                       ...                                                                     ",
"                                                                         ..                                                                                            .....                                                                    ",
"                                                                        ..                                                                                            ..  ..                                                                    ",
"                                                                   ...   ..  .                                                                                       ..   ..                                                                    ",
"                                                                  ..  .. .... ..                                                                                     ..   ..                                                                    ",
"                                                                  ..  ..      ..                                                                                    ..    ..       .                                                            ",
"                                                                  ..  ..      .                                                                                     ..   ..         ...                                                         ",
"                                                                      .     ....                                                                                    ..  ..           ....                                                       ",
"                                                       ..            .   . ..                                           ..                                          .....             ..                                                        ",
"                                                      .....          .. ..                                              ..                                           ...             ..  ....                                                   ",
"                                                      .  ..          . ...                                              ..                                                  ..      ..   ..  .                                                  ",
"                                                      ..  ..         ..                                                 ..                                                 ..      ..   ..    .                                                 ",
"                                                      ..  ...   ..                                            .         ..       .                                                ..    ..                                                      ",
"                                                      ..   ..   ..                                  .         .         ..       .         .                                     ..       ..                                                    ",
"                                                       ..  ..                                       .         .         ..       .         .                                               .                                                    ",
"                                                       ..  ..                             .         .         .         ..       .         .         .                                .    ..                                                   ",
"                                                        ....                              .          .        .         ..       .        .          .                                 .. ...                                                   ",
"                                                                      ..                  .          .        .         ..       .        .          .                   ..              ..                                                     ",
"                                                                      ..         .         .         .        ......................      .         .         .          ..                                                                     ",
"                                                                       ..        .         .         .......................................        .         .         ..                                                                      ",
"                                                                       ..        .         .    ..............                      .............   .         .         ..                                                                      ",
"                                                                       ...        .        ...........                                      ..........       .         ..                                                                       ",
"                                                                        ..        .     .........             .....................              ........    .         ..                                                                       ",
"                                                                        ..        .  .......          ........                     ........           ........         ..                                                                       ",
"                                                                         ..       .......        .....                                     .....         .......      ..                                                                        ",
"                                                              .          ..    ......       .....                                               .....        ......   ..         .                                                              ",
"                                                               .          ........       ...                                                         ...        .......         .                                                               ",
"                                                               .          .....      ....                                                               ....       .....        .                                                               ",
"                                                                .      ......     ...                                                                       ...      .....     .                                                                ",
"                                                                .    .....     ...                                                                             ...      ....   .                                      ....                      ",
"                                                      .          . .....     ..                                                                                   ..      .....          .                              ..                      ",
"                        ...                            .         .....     ..                                                                                       ..      ....        .                              ..                       ",
"                       .  ..                           .       .....    ...                                                                                           ...     ....      .                             ..                        ",
"                       .   ..                           .     ....    ..                                                                                                 ..     ....   .                             ..                         ",
"                       ..   ..                          .   ....    ..                                                                                                     ..     .... .                          . . .                         ",
"                       ..     .                          .....    ..                                                                                                         ..     ...                            ...                          ",
"                        ..    .              .          ....    ..                                                                                                             ..     ...         .                 .                           ",
"                         ..  ..               .        ....    .                                                                                                                 .     ...       .                   .                          ",
"                           ...                 .     ....    ..                                                                                                                   ..     ...    .                               ...             ",
"                                                .   ....   ..                                                                                                                       ..    ...  .                         .     ..  .            ",
"             ....                                . ...    .                                                                                                                           .     ...                         ...   ..    .           ",
"            .   ..     ..                        ....   ..                                                                                                                             ..    ...                         .    ..   ..           ",
"            ..   ..    ..             .         ...    .                                                                                                                                 .     ..        .                   ..   ...           ",
"             ..   ..                   .       ...   ..                                                                                                                                   ..    ...     .                    .    ..            ",
"             ...  ..                    .    ....   .                                                                                                                                       .    ...   .                     .   ..             ",
"              ..   .                     .  ...    .                                                                                                                                         .     .. .                       ....              ",
"               ..  .        ..            ....   ..                                                                                                                                           ..    ..             ..                           ",
"                 ..          ..           ...   .                                                                                                                                               .    ...          ..                            ",
"                              ..         ...   .                                                                                                                                                 .    ...        ..                             ",
"                               ..      ...    .                                                                                                                                                   .     ..      ..                              ",
"                                ..    ...   ..                                                                                                                                                     ..    ..    ..                               ",
"                                 ..  ...   .                                                                                                                                                         .    ..  ..                                ",
"                                  .....   .                                                                                                                                                           .    ....                                 ",
"                                   ..    .                                                                                                                                                             .    ..                                  ",
"                                        .                                                                                                                                                               .                                       ",
"                                       .                                                                                                                                                                 .                                      ",
"                                      .                                                                                                                                                                   .                                     ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                ",
"                                                                                                                                                                                                                                                "};
static char * cliplabel_xpm[] = {
"240 160 12 1",
" 	c #FFFF69696969",
".	c #FFFF64646464",
"X	c #FFFF5C5C5C5C",
"-	c #FFFF57575757",
"O	c #000000000000",
"+	c #FFFF50505050",
"@	c #FFFF40404040",
"#	c #FFFF30303030",
"$	c #FFFF20202020",
"%	c #FFFF10101010",
"&	c #FFFF08080808",
"*	c #FFFF00000000",
"             ........................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.........................................             ",
"            ........................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.........................................            ",
"           ........................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.........................................           ",
"          ........................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.........................................          ",
"         ........................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.........................................         ",
"        ........................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.........................................        ",
"       ........................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.........................................       ",
"      ........................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.........................................      ",
"     ........................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.........................................     ",
"     .......................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX........................................     ",
"     ......................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.......................................     ",
"     .....................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX......................................     ",
"     ....................................XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX.....................................     ",
"     ...................................XXXXXXXXX------------------------------------------------------------------------------------------------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX....................................     ",
"     ..................................XXXXXXXXXX--------------------------------------------------------------OOOO---------OOOOOO----------------------------------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX...................................     ",
"     .................................XXXXXXXXXXX-------------------------------------------------------------OO--OO--------OO----------------------------------------------------------------------------.................................     ",
"     ................................XXXXXXXXXXXX-------------------------------------------------------------OO--OO--------OO----------------------------------------------------------------------------.................................     ",
"     ...............................XXXXXXXXXXXXX-------------------------------------------------------------OO--OO--------OO----------------------------------------------------------------------------X................................     ",
"     ..............................XXXXXXXXXXXXXX-------------------------------------------------------------OO--OO--------OOOOO-------------------------------------------------------------------------XX...............................     ",
"     .............................XXXXXXXXXXXXXXX-------------------------------------------------------------OO--OO------------OO------------------------------------------------------------------------XXX..............................     ",
"     ............................XXXXXXXXXXXXXXXX-------------------------------------------------------------OO--OO------------OO------------------------------------------------------------------------XXXX.............................     ",
"     ...........................XXXXXXXXXXXXXXXXX-------------------------------------------------------------OO--OO---OO---OO--OO------------------------------------------------------------------------XXXXX............................     ",
"     ..........................XXXXXXXXXXXXXXXXXX--------------------------O-----------------------------------OOOO----OO---OOOOO--------------------------------------OOO--------------------------------XXXXXX...........................     ",
"     .........................XXXXXXXXXXXXXXXXXXX------------------------OO-------------------------------------------------------------------------------------------OOOOO-------------------------------XXXXXXX..........................     ",
"     ........................XXXXXXXXXXXXXXXXXXXX-----------------------OO--------------------------------------------------------------------------------------------OO--OO------------------------------XXXXXXXX.........................     ",
"     .......................XXXXXXXXXXXXXXXXXXXXX-------------------OO---OO------------------------------------------------------------------------------------------OO---OO------------------------------XXXXXXXXX........................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX-----------------OO--OO-OOOOOOO-------------------------------------------------------------------------------------OO---OO------------------------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX-----------------OO--OO------OO------------------------------------------------------------------------------------OO---OO--------O----------------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX-----------------O---OO------O-------------------------------------------------------------------------------------OO---OO---------OOO-------------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX---------------------O----OOOOO------------------------------------------------------------------------------------OO--OO-----------OOOO-----------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX------OO------------O---O-OO-------------------------------------------OO------------------------------------------OOOOO-------------OO------------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX-----OOOOO----------OO-OO----------------------------------------------OO-------------------------------------------OOO-------------OO--OOOO-------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX-----O---OO---------O-OOO----------------------------------------------OO--------------------------------------------------OO------OO---OO--O------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX-----OO--OO---------OO-------------------------------------------------OO-------------------------------------------------OO------OO---OO----O-----------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX-----OO---OO---OO-----------------------+++++++++++++++++++++O+++++++++OO+++++++O+++++++++++++++++++++++-------------------------OO----OO----------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX-----OO---OO---OO-----------------------+++++++++++O+++++++++O+++++++++OO+++++++O+++++++++O++++++++++++++-----------------------OO-------OO--------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX------OO--OO----------------------------+++++++++++O+++++++++O+++++++++OO+++++++O+++++++++O+++++++++++++++--------------------------------O--------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX------OO--OO----------------------------+O+++++++++O+++++++++O+++++++++OO+++++++O+++++++++O+++++++++O++++++--------------------------O----OO-------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX-------OOOO-----------------------------+O++++++++++O++++++++O+++++++++OO+++++++O++++++++O++++++++++O+++++++--------------------------OO-OOO-------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX---------------------OO-----------------+O++++++++++O++++++++O+++++++++OO+++++++O++++++++O++++++++++O++++++++-----------OO--------------OO---------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXXX---------------------OO---------O-------++O+++++++++O++++++++OOOOOOOOOOOOOOOOOOOOOO++++++O+++++++++O+++++++++O----------OO-------------------------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXXX-----------------------OO--------O-------++O+++++++++OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO++++++++O+++++++++O+--------OO--------------------------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------------OO--------O-------++O++++OOOOOOOOOOOOOO++++++++++++++++++++++OOOOOOOOOOOOO+++O+++++++++O++-------OO--------------------------------XXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------------OOO--------O------++OOOOOOOOOOO++++++++++++++++++++++++++++++++++++++OOOOOOOOOO+++++++O++++-----OO---------------------------------XXXXXXXXXX.......................     ",
"     .................................XXXXXXXXX-------------------------OO--------O-----OOOOOOOOO+++++++++++++OOOOOOOOOOOOOOOOOOOOO++++++++++++++OOOOOOOO++++O++++-----OO---------------------------------XXXXXX................................",
"     .................................XXXXXXXXX-------------------------OO--++++++O++OOOOOOO++++++++++OOOOOOOO+++++++++++++++++++++OOOOOOOO+++++++++++OOOOOOOO++++-----OO---------------------------------XXXXXX................................",
"     .................................XXXXXXXXX--------------------------OO+++++++OOOOOOO++++++++OOOOO+++++++++++++++++++++++++++++++++++++OOOOO+++++++++OOOOOOO++----OO----------------------------------XXXXXX................................",
"     .................................XXXXXXXXX---------------O----------OO++++OOOOOO+++++++OOOOO+++++++++++++++++++++++++++++++++++++++++++++++OOOOO++++++++OOOOOO+++OO---------O--------------XXXXXXXXXXXXXXXX................................",
"     .................................XXXXXXXXX----------------O---------+OOOOOOOO+++++++OOO+++++++++++++++++++++++++++++++++++++++++++++++++++++++++OOO++++++++OOOOOOO---------O---------------XXXXXXXXXXXXXXXX................................",
"     .................................XXXXXXXXX----------------O--------++OOOOO++++++OOOO+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++OOOO+++++++OOOOO--------O---------------XXXXXXXXXXXXXXXX................................",
"     .................................XXXXXXXXX-----------------O------OOOOOO+++++OOO+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++OOO++++++OOOOO-----O----------------XXXXXXXXXXXXXXXX................................",
"     .................................XXXXXXXXX-----------------O----OOOOO+++++OOO+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++OOO++++++OOOO---O----------------XXXXXXXXXXXXXXXX......OOOO......................",
"     .................................XXXXXXXXX-------O----------O-OOOOO+++++OO+++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@++++++++++++++++++OO++++++OOOOO----------O------XXXXXXXXXXXXXXXX........OO......................",
"     ....................OOO..........XXXXXXXXX--------O---------OOOOO+++++OO++++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+++++++++++++++++++OO++++++OOOO--------O-------XXXXXXXXXXXXXXXX.......OO.......................",
"     ...................O..OO.........XXXXXXXXX--------O-------OOOOO++++OOO+++++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@++++++++++++++++++++OOO++++-OOOO------O-------XXXXXXXXXXXXXXXX......OO........................",
"     ..................O....OO........XXXXXXXXX---------O-----OOOO++++OO+++++++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@++++++++++++++++++++++OO+++--OOOO---O--------XXXXXXXXXXXXXXXX.....OO.........................",
"     ..................OO....OO.......XXXXXXXXX---------O---OOOO-+++OO++++++++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+++++++++++++++++++++++OO++---OOOO-O--------XXXXXXXXXXXXXXXX..O.O.O.........................",
"     ..................OOO....O.......XXXXXXXXX----------OOOOO---+OO+++++++++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@++++++++++++++++++++++++OO-----OOO---------XXXXXXXXXXXXXXXX..OOOO..........................",
"     ...................OOO...O.......XXXXXXXOX---------OOOO----OO++++++++++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+++++++++++++++++++++++++OO-----OOO-------XXOXXXXXXXXXXXXX....O...........................",
"     .....................OO.OO.......XXXXXXXXO--------OOOO----O-++++++++++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@++++++++++++++++++++++++--O-----OOO------XOXXXXXXXXXXXXXX.....O..........................",
"     ......................OOO........XXXXXXXXXO-----OOOO----OO--+++++++++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+++++++++++++++++++++++---OO-----OOO----OXXXXXXXXXXXXXXX................OOOO............",
"     .........OOO.....................XXXXXXXXX-O---OOOO---OO----++++++++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@++++++++++++++++++++++-----OO----OOO--OXXXXXXXXXXXXXXXX.........O.....OO...O...........",
"     ........OOOO.....................XXXXXXXXX--O-OOO----O------+++++++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+++++++++++++++++++++-------O-----OOO-XXXXXXXXXXXXXXXX........OOO...OO....O...........",
"     .......O...OO.....OO.............XXXXXXXXX--OOOO---OO-------++++++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@++++++++++++++++++++--------OO----OOOXXXXXXXXXXXXXXXX.........O....OO...OO...........",
"     .......OO...OO....OO.............OXXXXXXXX-OOO----O---------+++++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+++++++++++++++++++----------O-----OOXXXXXXXXOXXXXXX.............OO....OO...........",
"     ........OO...OO..................XOXXXXXXXOOO---OO----------++++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@++++++++++++++++++-----------OO----OOOXXXXXOXXXXXXX.............O...OOO............",
"     ........OOO..OO..................XXOXXXXOOOO---O------------+++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@+++++++++++++++++-------------O---XOOOXXXOXXXXXXXX.............O..OO..............",
"     .........OO...O..................XXXOXXOOO----O-------------++++++++++++++++@@@@@@@@@@@@@@@@@@@@@@#################################@@@@@@@@@@@@@@@@@@@@@@@++++++++++++++++--------------O--XXXOOXOXXXXXXXXX..............OOO...............",
"     ..........OOO.O........OO........XXXXOOOOX--OO--------------+++++++++++++++@@@@@@@@@@@@@@@@@@@@@@###################################@@@@@@@@@@@@@@@@@@@@@@@+++++++++++++++---------------OOXXXXOOXXXXXXXXXX...OO...........................",
"     ............OO..........OO.......XXXXOOOXX-O----------------+++++++++++++++@@@@@@@@@@@@@@@@@@@@@#####################################@@@@@@@@@@@@@@@@@@@@@@+++++++++++++++-----------------OXXXXOOOXXXXXXXX..OO............................",
"     .........................OO......XXXOOOXXXO-----------------+++++++++++++++@@@@@@@@@@@@@@@@@@@@#######################################@@@@@@@@@@@@@@@@@@@@@+++++++++++++++-----------------XOXXXXOOOXXXXXXX.OO.............................",
"     ..........................OO.....XOOOXXXXO------------------+++++++++++++++@@@@@@@@@@@@@@@@@@@#########################################@@@@@@@@@@@@@@@@@@@@+++++++++++++++-----------------XXOXXXXXOOXXXXXXOO..............................",
"     ...........................OO....OOOXXXOOX------------------+++++++++++++++@@@@@@@@@@@@@@@@@@###########################################@@@@@@@@@@@@@@@@@@@+++++++++++++++-----------------XXXOOXXXXOOXXXXOO...............................",
"     ............................OO..OOOXXXOXXX------------------+++++++++++++++@@@@@@@@@@@@@@@@@#############################################@@@@@@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXOXXXXOOXXOO................................",
"     .............................OOO.OXXXOXXXX------------------+++++++++++++++@@@@@@@@@@@@@@@@###############################################@@@@@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXOXXXXOOOOX................................",
"     ..............................OO.XXXOXXXXX------------------+++++++++++++++@@@@@@@@@@@@@@@#################################################@@@@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXOXXXXOOXX................................",
"     ......................XXXXXXXXXXXXXOXXXXXX------------------+++++++++++++++@@@@@@@@@@@@@@###################################################@@@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXOXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXOXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@@#####################################################@@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXOXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXOXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@#######################################################@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXOXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@#################$$$$$$$$$$$$$$$$$$$$$#################@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@################$$$$$$$$$$$$$$$$$$$$$$$################@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@###############$$$$$$$$$$$$$$$$$$$$$$$$$###############@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##############$$$$$$$$$$$$$$$$$$$$$$$$$$$##############@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@#############$$$$$$$$$$$$$$$$$$$$$$$$$$$$$#############@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@############$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$############@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@###########$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$###########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$$$$$%%%%%%%%%%%%$$$$$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$$$$%%%%%%%%%%%%%%$$$$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$$$%%%%%%%%%%%%%%%%$$$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$$%%%%%%%%%%%%%%%%%%$$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$%%%%%%%%%%%%%%%%%%%%$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$%%%%%%%%%%%%%%%%%%%%$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$%%%%%%%&&&&&&%%%%%%%$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$%%%%%%&&&&&&&&%%%%%%$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$%%%%%&&&&&&&&&&%%%%%$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$%%%%%&&&***&&&&%%%%%$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$%%%%%&&******&&%%%%%$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"     ......................XXXXXXXXXXXXXXXXXXXX------------------+++++++++++++++@@@@@@@@@@@@##########$$$$$$$$%%%%%&&******&&%%%%%$$$$$$$##########@@@@@@@@@@@@@+++++++++++++++-----------------XXXXXXXXXXXXXXXXXXXX.......................     ",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO",
"OOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOOO"};


char **onlabel_bits(void) {return(onlabel_xpm);}
char **offlabel_bits(void) {return(offlabel_xpm);}
char **cliplabel_bits(void) {return(cliplabel_xpm);}

#endif

#if USE_MOTIF
static unsigned char snd_plain_bits[] = {
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x32, 0x00, 0x00, 0xf0, 0x01, 0x00,
   0x32, 0x00, 0x00, 0x40, 0x00, 0x00, 0x32, 0x0c, 0x00, 0x40, 0x00, 0x00,
   0x32, 0x0f, 0x10, 0x40, 0x00, 0x00, 0xb2, 0x19, 0x38, 0x40, 0x00, 0x00,
   0xb0, 0x10, 0x68, 0x40, 0x00, 0x00, 0xb0, 0x30, 0x88, 0x40, 0x00, 0x00,
   0xb0, 0x20, 0x88, 0x40, 0x00, 0x00, 0xf0, 0xe0, 0x89, 0x41, 0x00, 0x00,
   0x30, 0x00, 0x09, 0x43, 0x3c, 0x00, 0x30, 0x00, 0x0f, 0x4e, 0xe6, 0x00,
   0x30, 0x00, 0x00, 0xf8, 0x83, 0x7f, 0x30, 0x00, 0x00, 0x40, 0x00, 0xf8,
   0x30, 0x00, 0x00, 0x40, 0x00, 0x0e, 0x30, 0x00, 0x07, 0xe0, 0x01, 0x03,
   0x30, 0x80, 0x09, 0x50, 0x03, 0x01, 0x70, 0xc0, 0x08, 0x58, 0x82, 0x01,
   0xb0, 0x60, 0x18, 0x44, 0xc6, 0x00, 0xb0, 0x31, 0x10, 0x44, 0x7c, 0x00,
   0x30, 0x13, 0x10, 0x44, 0x00, 0x00, 0x30, 0x1e, 0x30, 0x44, 0x00, 0x00,
   0x30, 0x00, 0x60, 0x42, 0x00, 0x00, 0x30, 0x00, 0xc0, 0xc3, 0x00, 0x00,
   0x37, 0x00, 0x00, 0xc0, 0x01, 0x00, 0x35, 0x00, 0x00, 0xc0, 0x03, 0x00,
   0x35, 0x00, 0x00, 0xc0, 0x01, 0x00, 0x35, 0x00, 0x00, 0xc0, 0x00, 0x00,
   0xf7, 0xff, 0xff, 0xff, 0xff, 0x7f, 0xf0, 0xff, 0xff, 0xff, 0xff, 0x7f,
   0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0x70, 0x77, 0x17, 0x00, 0xde, 0x03,
   0x50, 0x55, 0x11, 0x00, 0xd6, 0x02, 0x50, 0x57, 0x13, 0x00, 0xd6, 0x02,
   0x50, 0x55, 0x11, 0x00, 0xd6, 0x02, 0x70, 0x77, 0x17, 0x00, 0xde, 0x03,
   0x00, 0x00, 0x10, 0x00, 0x00, 0x00, 0xff, 0xff, 0xff, 0xff, 0xff, 0xff,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf8, 0xff, 0xff, 0xff, 0xff, 0x77,
   0x08, 0x00, 0x80, 0x0d, 0x00, 0x54, 0xf8, 0xff, 0xff, 0xff, 0xff, 0x77,
   0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
   0xf8, 0xff, 0xff, 0xff, 0xff, 0x77, 0x88, 0x0d, 0x00, 0x00, 0x00, 0x54,
   0xf8, 0xff, 0xff, 0xff, 0xff, 0x77, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00};

unsigned char *snd_plain_icon_bits(void) {return(snd_plain_bits);}
#endif


#if USE_GTK
static char *speaker_xpm[] = {
"12 12 2 1",
"X	c black",
"-      c ivory2 s basiccolor",
"--------XXX-",
"------XX--X-",
"----XX----X-",
"-XXX------X-",
"-XX-------X-",
"-XX-------X-",
"-XX-------X-",
"-XX-------X-",
"-XXX------X-",
"----XX----X-",
"------XX--X-",
"--------XXX-"};

char **speaker_bits(void) {return(speaker_xpm);}

static char * mic_xpm[] = {
"12 12 2 1",
"X	c black",
"-      c ivory2 s basiccolor",
"----XXXX----",
"---XX-X-X---",
"---X-X-XX---",
"---XXXXXX---",
"---X----X---",
"XXXX----XXXX",
"X--X----X--X",
"X--X----X--X",
"X--XXXXXX--X",
"X---XXXX---X",
"X----XX----X",
"XXXXXXXXXXXX"};

char **mic_bits(void) {return(mic_xpm);}

static char *cd_xpm[] = {
"12 12 2 1",
"X	c black",
"-      c ivory2 s basiccolor",
"---XXXXX----",
"-XX-----XX--",
"-X--XXX--X--",
"X--X---X--X-",
"X-X-XXX-X-X-",
"X-X-X-X-X-X-",
"X-X-XXX-X-X-",
"X--X---X--X-",
"-X--XXX--X--",
"-XX-----XX--",
"---XXXXX----",
"------------"};

char **cd_bits(void) {return(cd_xpm);}

static char *line_in_xpm[] = {
"12 12 2 1",
"X	c black",
"-      c ivory2 s basiccolor",
"----------X-",
"------X--X--",
"-----X---X--",
"--X--X--X---",
"-X--X---X---",
"XXXXXXXXXXXX",
"-X--X---X---",
"--X--X--X---",
"-----X---X--",
"------X--X--",
"----------X-",
"------------"};

char **line_in_bits(void) {return(line_in_xpm);}
#endif
