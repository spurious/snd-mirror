#include "snd.h"

#if (HAVE_XPM) || (USE_GTK)

/* -------------------------------- SOUND ICONS (lock, bomb) -------------------------------- */

static const char *mini_lock_xpm[] = {
"16 14 5 1",
"-      c None s None",
".	c gray50",
"X	c black",
"o	c white",
"O	c yellow",
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

const char **mini_lock_bits(void) {return(mini_lock_xpm);}


static const char *blank_xpm[] = {
"18 18 5 1",
"-      c None s None",
".	c gray50",
"X	c black",
"o	c white",
"O	c yellow",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------",
"------------------"};

const char **blank_bits(void) {return(blank_xpm);}


static const char *speed_l_xpm[] = {
"16 12 5 1",
"-      c None s None",
".	c gray50",
"X	c black",
"o	c white",
"O	c yellow",
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

const char **speed_l_bits(void) {return(speed_l_xpm);}


static const char * speed_r_xpm[] = {
"16 12 5 1",
"-      c None s None",
".	c gray50",
"X	c black",
"o	c white",
"O	c yellow",
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

const char **speed_r_bits(void) {return(speed_r_xpm);}


/* bomb for out-of-date in-core data fuse shortens with sparks flying off; */

static const char * mini_bomb0_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"-------...------","------.---.-----","-----.-----.----","----...-----.---","---.....----.---","--.X#o...----.--","-X.#X....X---.--",
"-..oX.....---O-O","-.......O.-O-OO-","-......Xo.--OOO-","-X.....X.X--O---","--.......-------","---X...X--------","----------------"};

static const char * mini_bomb1_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"-------...------","------.---.-----","-----.-----.----","----...-----.---","---.....----.---","--.X#o...----.--","-X.#X....X---Y--",
"-..oX.....---YYY","-.......O.-YYOOY","-......Xo.--OOY-","-X.....X.X--Y---","--.......-------","---X...X--------","----------------"};

static const char * mini_bomb2_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"-------...------","------.---.-----","-----.-----.----","----...-----.---","---.....----.---","--.X#o...----Y--","-X.#X....X---YY-",
"-..oX.....---OYO","-.......O.--O-OO","-......Xo.--Y-Y-","-X.....X.X------","--.......----Y--","---X...X--------","----------------"};

static const char * mini_bomb3_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"-------...------","------.---.-----","-----.-----.----","----...-----.---","---.....----.---","--.X#o...----Y--","-X.#X....X---OO-",
"-..oX.....-YYYYO","-.......O.----O-","-......Xo.----O-","-X.....X.X----Y-","--.......-------","---X...X--------","------------YY--"};

static const char * mini_bomb4_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"-------...------","------.---.-----","-----.-----.--Y-","----...-----.---","---.....----O---","--.X#o...---OO--","-X.#X....X-YOYO-",
"-..oX.....--OYY-","-.......O.------","-......Xo.-Y----","-X.....X.X------","--.......-------","---X...X--------","----------------"};

static const char * mini_bomb5_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"-------...------","------.---.-----","-----.-----.----","----...-----Y---","---.....----OO--","--.X#o...--OOO--","-X.#X....X---YO-",
"-..oX.....---YY-","-.......O.-----Y","-......Xo.-----O","-X.....X.X------","--.......-------","---X...X--------","----------------"};

static const char * mini_bomb6_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"-------...------","------.---.-----","-----.-----OO-O-","----...-----YO--","---.....----O---","--.X#o...-YY-OO-","-X.#X....X--Y---",
"-..oX.....------","-.......O.------","-......Xo.------","-X.....X.X------","--.......-----OO","---X...X------Y-","----------------"};

static const char * mini_bomb7_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"-------...----OO","------.---O-----","-----.-----OOYY-","----...-----YOO-","---.....-YY-O---","--.X#o...-------","-X.#X....X--YO--",
"-..oX.....---Y--","-.......O.------","-......Xo.------","-X.....X.X------","--.......-------","---X...X--------","--------------YY"};

static const char * mini_bomb8_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"-------..Y------","------.--OO-----","-----.----OOO-Y-","----...---OO--O-","---.....-YY----O","--.X#o...-------","-X.#X....X------",
"-..oX.....--YO--","-.......O.---O--","-......Xo.------","-X.....X.X------","--.......-------","---X...X--------","----------------"};

static const char * mini_bomb9_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"-------YOY--O---","------.---YO----","-----.----OYY---","----...---------","---.....----YY--","--.X#o...-------","-X.#X....X------",
"-..oX.....------","-.......O.--YO--","-......Xo.---Y--","-X.....X.X------","--.......-------","---X...X--------","----------------"};

static const char * mini_bomb10_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"-----OYYOYOO----","-----YOO--YO----","-----.-----YY---","----...---------","---.....--------","--.X#o...----OO-","-X.#X....X---Y--",
"-..oX.....------","-.......O.------","-......Xo.------","-X.....X.X------","--.......-------","---X...X--------","----------------"};

static const char * mini_bomb11_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"-----OOYOO--O---","----OOY----O----","---OOOO---------","----...---------","---.....--------","--.X#o...-------","-X.#X....X------",
"-..oX.....------","-.......O.------","-......Xo.------","-X.....X.X------","--.......---OO--","---X...X--------","----------------"};

static const char * mini_bomb12_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"-----OO---------","--YYOOYYY-------","YYOOOOYYYY------","--OOOOO---------","---.....--------","--.X#o...-------","-X.#X....X------",
"-..oX.....------","-.......O.------","-......Xo.------","-X.....X.X------","--.......-------","---X...X--------","------------YY--"};

static const char * mini_bomb13_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"----------------","----OOY---------","--YYYYY-Y-------","--Y-OOO---------","---.YOY.--------","--.X#o...-------","-X.#X....X------",
"-..oX.....------","-.......O.------","-......Xo.------","-X.....X.X------","--.......-------","---X...X--------","----------------"};

static const char * mini_bomb14_xpm[] = {
"16 14 7 1","-      c None s None",".	c black","X	c gray50","o	c gray85","O	c red","#	c white","Y      c yellow",
"----------------","----------------","----------------","----------------","---.....--------","--.X#o...-------","-X.#X....X------",
"-..oX.....------","-.......O.------","-......Xo.------","-X.....X.X------","--.......-------","---X...X--------","----------------"};

const char **mini_bomb_bits(int n)
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


static const char * mini_glass0_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.OOOOOOOOOO.c-","-.cOOOOOOOOOOc.-","--cOOOOOOOOOOc--","----cOOOOOOc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---cooooooooc---","--cooooooooooc--","-.cooooooooooc.-","-cooooooooooooc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass1_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.OOOOoOOOOO.c-","-.cOOOOOOOOOOc.-","--cOOOOOOOOOOc--","----cOOOOOOc----","-----.coOc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---coooOooooc---","--cooooooooooc--","-.cooooooooooc.-","-coooooOooooooc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass2_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.OOOOoooOOO.c-","-.cOOOOOOOOOOc.-","--cOOOOOOOOOOc--","----cOOOOOOc----","-----.cOoc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---cooooooooc---","--cooooooooooc--","-.cooooOoooooc.-","-cooooOOOoooooc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass3_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.OOOoooOOOO.c-","-.cOOOOooOOOOc.-","--cOOOOOOOOOOc--","----cOOOOOOc----","-----.coOc.-----","-------cc-------",
"-----.cOoc.-----","----cooooooc----","---cooooooooc---","--cooooooooooc--","-.coooOooooooc.-","-coooOOOOoooooc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass4_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.OOOooooOOO.c-","-.cOOOOoooOOOc.-","--cOOOOOoOOOOc--","----cOOoOOOc----","-----.cOOc.-----","-------cc-------",
"-----.coOc.-----","----cooooooc----","---cooooooooc---","--cooooooooooc--","-.coooOOOooooc.-","-coooOOOOoooooc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass5_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.OOOoooooOO.c-","-.cOOOOooooOOc.-","--cOOOOOoOOOOc--","----cOOOOOOc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---cooooooooc---","--coooOooooooc--","-.coooOOOOoooc.-","-coooOOOOOooooc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass6_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.OOOoooooOO.c-","-.cOOOOooooOOc.-","--cOOOOoooOOOc--","----cOOOOoOc----","-----.cOOc.-----","-------cc-------",
"-----.cOoc.-----","----coooOooc----","---cooooooooc---","--cooooooooooc--","-.coooOOOOoooc.-","-coooOOOOOOoooc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass7_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.OOooooooOO.c-","-.cOOOoooooOOc.-","--cOOOOoOoOOOc--","----cOOoOOOc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---cooooOoooc---","--coooOOoooooc--","-.cooOOOOOoooc.-","-cooOOOOOOOoooc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass8_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.OoooooooOO.c-","-.cOOooooooOOc.-","--cOOOooooOOOc--","----cOOOOOOc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---cooooooooc---","--coooOOOooooc--","-.cooOOOOOoooc.-","-cooOOOOOOOOOoc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass9_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.ooooooooOO.c-","-.cOoooooooOOc.-","--cOOoooooOOOc--","----cOOoOOOc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooOoooc----","---cooooooooc---","--cooOOOOOOOoc--","-.coOOOOOOOOooc.-","-coOOOOOOOOOOOoc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass10_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.oooooooooo.c-","-.cOoooooooooc.-","--cOOooooooOOc--","----cOOooOOc----","-----.coOc.-----","-------cc-------",
"-----.cooc.-----","----cooooooc----","---cooOOOOooc---","--cooOOOOOOooc--","-.coOOOOOOOOoc.-","-cOOOOOOOOOOOoc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass11_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.oooooooooo.c-","-.cooooooooooc.-","--cOOooooooooc--","----cOOooOOc----","-----.coOc.-----","-------cc-------",
"-----.cooc.-----","----cooOoooc----","---cooOOOOooc---","--coOOOOOOOooc--","-.cOOOOOOOOOoc.-","-cOOOOOOOOOOOoc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass12_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.oooooooooo.c-","-.cooooooooooc.-","--cooooooooooc--","----cOoooOOc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooOOooc----","---coOOOOOooc---","--cOOOOOOOOOoc--","-.cOOOOOOOOOOc.-","-cOOOOOOOOOOOOc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass13_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.oooooooooo.c-","-.cooooooooooc.-","--cooooooooooc--","----cooooooc----","-----.cOOc.-----","-------cc-------",
"-----.cooc.-----","----cooOOooc----","---cOOOOOOOOc---","--cOOOOOOOOOOc--","-.cOOOOOOOOOOc.-","-cOOOOOOOOOOOOc-","-XXXXXXXXXXXXXX-"};

static const char * mini_glass14_xpm[] = {"16 14 6 1","-      c None s None",". 	c ivory4","c	c gray50","X	c black","o	c white","O	c tan",
"-XXXXXXXXXXXXXX-","-c.oooooooooo.c-","-.cooooooooooc.-","--cooooooooooc--","----cooooooc----","-----.cooc.-----","-------cc-------",
"-----.cOOc.-----","----cOOOOOOc----","---cOOOOOOOOc---","--cOOOOOOOOOOc--","-.cOOOOOOOOOOc.-","-cOOOOOOOOOOOOc-","-XXXXXXXXXXXXXX-"};

const char **mini_glass_bits(int n)
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

/* from HView */
static const char *stop_sign_xpm[] = {
"17 17 5 1",
" 	c None s None",
".	c red",
"X	c white",
"+      c ivory2",
"-      c ivory2",
"+++-XXXXXXXXX-+++", 
"++-X.........X-++", 
"+-X...........X-+", 
"-X.............X-", 
"X...............X", 
"X...............X", 
"X.XX.XXX.XX..XX.X", 
"X.X...X.X..X.X.XX", 
"X..X..X.X..X.XX.X", 
"X.XX..X..XX..X..X", 
"X...............X", 
"X...............X", 
"X...............X", 
"-X.............X-", 
"+-X...........X-+", 
"++-X.........X-++", 
"+++-XXXXXXXXX-+++"};

const char **stop_sign_bits(void) {return(stop_sign_xpm);}



/* -------------------------------- PROGRAM ICON -------------------------------- */

static const char *snd_icon_xpm[] = {
"48 48 5 1",
". c white m white",
"B c black m black",
"a c white m white s basiccolor",
"r c red m black s cursorcolor",
"g c lightblue m black s listenercolor",
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
"aaBBBB.........................................a",
"aaBBBB.........................................a",
"aaBBBB.........................................a",
"aaBBBB.........................................a",
"aaBBBB.........................................a",
"aaBBBB.........................................a",
"aaBBBB............BBBB.........................a",
"aaBBBB..........BB....BB.......................a",
"aaBBBB........BB.......BB......................a",
"aaBBBB......BB..........BB.....................a",
"aaBBBB.....BB............BB....................a",
"aaBBBB....BB.............BB....................a",
"aaBBBB...BB..............BB....................a",
"aaBBBB..BB................BB...................a",
"aaBBBBBB..................BB...................a",
"aaBBBB.....................BB..................a",
"aaBBBB.....................BB.................Ba",
"aaBBBB......................BB...............BBa",
"aaBBBB.......................BB.............BB.a",
"aaBBBB.........................BB..........BB..a",
"aaBBBB..........................BB.......BB....a",
"aaBBBB...........................BB....BB......a",
"aaBBBB.............................BBB.........a",
"aaBBBB.........................................a",
"aaBBBB.........................................a",
"aaBBBB.........................................a",
"aaBBBB.........................................a",
"aaBBBB.........................................a",
"aaBBBB.........................................a",
"aaBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBaa",
"aaBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBBaa",
"aaaaaa.........................................a",
"aaaaaa.........................................a",
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
"agggggggggggggggggggggggggggggggggggggggggggggga",
"agggggggggggggggggggggggggggggggggggggggggggggga",
"agggggggggggggggggggggggggggggggggggggggggggggga",
"agggggggggggggggggggggggggggggggggggggggggggggga",
"agggggggggggggggggggggggggggggggggggggggggggggga",
"aggggggggggggggggggggggggggggggggggggggggggggggg",
"aggggggggggggggggggggggggggggggggggggggggggggggg",
"aggggggggggggggggggggggggggggggggggggggggggggggg",
"agggggggggggggggggggggggggggggggggggggggggggggga",
"agggggggggggggggggggggggggggggggggggggggggggggga",
"agggggggggggggggggggggggggggggggggggggggggggggga",
"agggggggggggggggggggggggggggggggggggggggggggggga",
"aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"};

const char **snd_icon_bits(void) {return(snd_icon_xpm);}
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
static const char *speaker_xpm[] = {
"12 12 2 1",
"-      c None s None",
"X	c black",
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

const char **speaker_bits(void) {return(speaker_xpm);}


static const char *blue_speaker_xpm[] = {
"12 12 3 1",
"-      c None s None",
"o      c red",
"X	c black",
"--------XXX-",
"------XXooX-",
"----XXooooX-",
"-XXXooooooX-",
"-XXoooooooX-",
"-XXoooooooX-",
"-XXoooooooX-",
"-XXoooooooX-",
"-XXXooooooX-",
"----XXooooX-",
"------XXooX-",
"--------XXX-"};

const char **blue_speaker_bits(void) {return(blue_speaker_xpm);}
#endif


void make_icons_transparent(const char *color)
{
#if (HAVE_XPM) || (USE_GTK)
  static char *bg_line = NULL;
  int i;
  const char **tmp;
  if (bg_line) free(bg_line);
  bg_line = mus_format("-      c %s s %s", color, color); /* the background color isn't known at compile time */
  mini_lock_xpm[1] = bg_line;
  blank_xpm[1] = bg_line;
  speed_l_xpm[1] = bg_line;
  speed_r_xpm[1] = bg_line;
  for (i = 0; i < NUM_BOMBS; i++)
    {
      tmp = mini_bomb_bits(i);
      tmp[1] = bg_line;
    }
  for (i = 0; i < NUM_HOURGLASSES; i++)
    {
      tmp = mini_glass_bits(i);
      tmp[1] = bg_line;
    }
#if USE_GTK
  speaker_xpm[1] = bg_line;
  blue_speaker_xpm[1] = bg_line;
#endif
#endif 
}
