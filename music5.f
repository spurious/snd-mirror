C     typed in from old XGP output (JAM 2Aug76), 
C     file last written 19Jun75 MUSIC5[M5,GM]
C     SAIL
C        (I believe GM = George McKee, JAM = Andy Moorer)
C     
C     in gfortran, many errors: 
C     
C     Obsolete: arithmetic IF statement
C     TYPE 99 Error: Unclassifiable statement (also ACCEPT 999)
C     various labels for PRINT are there, but gfortran is unhappy
C     Incompatible types in assignment at (1), CHARACTER(1) to INTEGER(4)
C     Two main PROGRAMs (actually at least 3)
C     I think each page of this file was intended as a separate compilation (6 in all)
C       since fortran complains so much about duplicate labels
C     gfortran is unhappy with some of the DO loops and GO TO's
C     
C     The main typos are 'I' vs 1 -- the printout is so old and faded that
C       I can barely make out which is correct sometimes.  And there are space
C       characters that were dropped by the printer.  Also I haven't used
C       fortran since the early '80s -- I scarcely remember how this is supposed
C       to look.  
C
C     if split into three passes, (with various fixups for CHARACTER handling
C       and so on -- I haven't merged the changes into this file yet) music5.f compiles
C       without errors.  But I'm missing the functions putfile, ifile, finfile,
C       and fastout.  I'm guessing ifile opens a file for reading, finfile
C       closes a file, and the others open a file for writing, and fastout writes 0's.
C       So, all I need is a known-good input example.
C
C     Mus11 says ifile(n,rnam) is the same as open(n,rnam,0,'RDO',,,'UNF')
C     
C     (Bill Schottstaedt, 26-Apr-08)
C     
C     [page 1-1] -- these are the original XGP pages to help me find my place
      
      
C     PASS1 PASS 1 MAIN PROGRAM
C     PASS1   *** MUSIC V ***   THIS VERSION RUNS ON THE PDP10, JULY 14,1971
      COMMON P(100),IP(10),D(2000),IPDP
      DATA IPDP/0/
C*****PDP *****  IPDP WAS ADDED TO COMMON LIST IN PLACE OF ENTRY FEATURE.
 99   FORMAT(' TYPE FILE NAME'/)
 999  FORMAT(A5)
      TYPE 99
      ACCEPT 999,FLNM
      CALL IFILE(1,FLNM)
C*****ABOVE 5 LINES FOR PDP10 ********
      
C     INITIALIZATION
C     NOMINAL SAMPLING RATE.
      D(4) = 10000.0
C     ERROR FLAG
      IP(2)=0
      P(2)=0.0
C     C   NWRITE = 2
      NWRITE=20
C**** PDP DSK0=DEVICE 20 ******
C     C    REWIND NWRITE
C     C    CALL READ0
      CALL READ1
C********PDP ********
C     MAIN LOOP
 100  CALL READ1
      I1=P(1)
      IF (I1.GE.1.AND.I1.LE.12) GO TO 103
      IP(2)=1
C     C    WRITE (6,200)
      PRINT 200
C********PDP ********
 200  FORMAT(' NON-EXISTENT OPCODE ON DATA STATEMENT')
      GO TO 100
 103  GO TO (1,1,1,1,5,6,7,1,9,1,1,12),I1
 1    CALL WRITE1 (NWRITE)
      GO TO 100
 5    PRINT 110
C     C 5 WRITE (6, 110)
C********PDP ********
 110  FORMAT(' END OF SECTION IN PASS 1')
      GO TO 1
 6    CALL WRITE1 (NWRITE)
C     C	  WRITE (6, 111)
      PRINT 111
C********PDP ********	  
 111  FORMAT (' END OF PASS 1')
      IF(IP(2).EQ.1) CALL HARVEY
      CALL EXIT
C     SET VARIABLES IN PASS 1
 7    I2=P(3)
      I3=I2+IP(1)-4
      DO 104 I4=I2,I3
 104     D(14)=P(14-I2+4)
         GO TO 100
 9       I6=P(3)
         IF (I6.GE.1.AND.I6.LE.5) GO TO 107
         IP(2)=1
C     C    WRITE (6,201)
         
C     [page 1-2]

         PRINT 201
C********PDP ********
 201     FORMAT(' NON-EXISTENT PLF SUBROUTINE CALLED')
         GO TO 100
 12      CALL WRITE1 (NWRITE)
         GO TO 7
 107     GO TO (21,22,23,24,25),I6
 21      CALL PLF1
         GO TO 100
 22      CALL PLF2
         GO TO 100
 23      CALL PLF3
         GO TO 100
 24      CALL PLF4
         GO TO 100
 25      CALL PLF5
         GO TO 100
         END
C     WRITE1 PASS 1 DATA-WRITING ROUTINE
C     *** MUSIC V ***
      SUBROUTINE WRITE1(N)
      COMMON P(100),IP(10)
      K=IP(1)
      WRITE(N )K,(P(J),J=1,K)
      RETURN
      END
      SUBROUTINE PLF
      COMMON P(100),IP(10),D(2000)
C     C    ENTRY PLF1
C     C    ENTRY PLF2
C     C    ENTRY PLF3
C     C    ENTRY PLF4
C     C    ENTRY PLF5
      END
C     ERRO1    GENERAL ERROR ROUTINE
C     ***MUSIC V ***
      SUBROUTINE ERROR(I)
      PRINT 100,I
 100  FORMAT(13HERROR OF TYPEI5)
      RETURN
      END
      SUBROUTINE HARVEY
C     C    WRITE (6,1)
      PRINT 1
C********PDP *********
 1    FORMAT(' WHERE IS HARVEY')
      CALL EXIT
      END
      SUBROUTINE MOVR(IBCD,LA,LB)
      DIMENSION IBCD(300)
      DO 1 J=LA,LB
C     C  1 IBCD(J)=I5-(IBCD(J))/16777216
C********PDP ********
 1       IBCD(J)=IBCD(J)/536870912-48
 2       DUMMY=0
C     TO SET BREAKPOINT.
         RETURN
         END
      
      
C     [page 2-1]

C     READ1 INTERPRETATIVE READING ROUTINE
C**** MUSIC V ****
      SUBROUTINE READ1
      COMMON P(100),IP(10),D(2000),IPDP
C*****PDP ***** IPDP WAS ADDED TO COMMON LIST IN PLACE OF ENTRY FEATURE
      DIMENSION CARD(129),ICAR(128),IBCD(300),LOP(3,30)
      DIMENSION BCD(300)
      DIMENSION IBC(12),IVT(4)
      EQUIVALENCE(CARD,ICAR)
      EQUIVALENCE(BCD,IBCD)
      DATA NOPS,NBC,NC/26,3,72/
      DATA IDEC,ISTAR/'.','*'/
CCC   DATA IBC(1),IBC(2),IBC(3),IBC(4)/'=',' ',',','-'/
      DATA IBC(1),IBC(2),IBC(3),IBC(4)/';',' ',',','-'/
C********NO!!!!! THE CHARACTER = HAS BEEN SUBSTITUTED FOR
C     THE SEMICOLON AS THE END OF STATEMENT DELIMITER
      DATA IVT/'P','F','B','V'/
      DATA LOP/'N','O','T','I','N','S','G','E','N','S','V','3',
      1 'S','E','C','T','E','R','S','V','1','S','V','2','P','L','F',
      2 'P','L','S','S','I','3','S','I','A','C','O','M','E','N','D',
      3 'O','U','T','O','S','C','A','D','2','R','A','N','E','N','V',
      4 'S','T','R','A','D','3','A','D','4','M','L','T','F','L','T',
      5 'R','A','H','S','E','T',0,0,0,0,0,0,0,0,0,0,0,0/
C********LAST 12 LOCATIONS NOT YET USED. **** PDP ********
      EQUIVALENCE (JSEMI,IBC(1)), (JBLANK,IBC(2))
      
C     TO SCAN INPUT DATA TO #, ORGANIZE FIELDS AND PRINT
      IF(IPDP.EQ.0) GO TO 99
C********PDP ********
      IF (END+SNA8-1.) 10,10,90
 10   IBK=2
      END=0
      ERR=0
      NUMU=0
      ISEMI=1
      L=3
      J=0
 11   I=I+1
      IF(I.GT.NC) GO TO 15
      IF(J.EQ.299) GO TO 21
      DO 13 N=1,NBC
         IF(ICAR(I)-IBC(N)) 13,12,13
 12      GO TO (20,16,18),N
C     ; BLA ,
 13   CONTINUE
      J=J+1
      IBCD(J)=ICAR(I)
      IBK=1
      GO TO 11
 14   IBK=N
      GO TO 11
C     C   15 READ (5,1,ERR=95,END=95) (CARD(I),I=1,NC)
C********PDP ********
 15   READ (1,1,ERR=95,END=95) I, (CARD(I),I=1,NC)
C*****PDP ***** FIRST 'I' IS FOR PDP LINE NUMBERS!
 1    FORMAT(I,128A1)
C     C 1 FORMAT(128A1)
      PRINT 2,(CARD(I),I=1,NC)
 2    FORMAT(1H 128A1)
      I=0
      
C     [page 2-2]

      GO TO 11
 16   GO TO (17,11,11),IBK
 17   IBK=N
      J=J+1
      IBCD(J)=JBLANK
      GO TO (11,21),ISEMI
 18   GO TO (17,14,19),IBK
 19   J=J+1
      IBCD(J)=0
      GO TO 17
 20   ISEMI=2
      GO TO (17,21,19),IBK
 21   J=J+1
      IBCD(J)=JSEMI
C     TO SCAN FOR OP CODE
      DO 24 N=1,NOPS
         M=N
         DO 23 K=1,3
            IF (IBCD(K)-LOP(K,N)) 24,23,24
 23      CONTINUE
         GO TO 26
 24   CONTINUE
      GO TO 40
 26   NP=1
 27   L=L+1
      IF (IBCD(L)-JBLANK) 27,29,27
 29   GO TO (100,200,300,400,500,600,700,800,900,10000,1100,1200,1300,
      1217 ,201,202,203,204,205,206,207,208,209,210,211,212),M
C     OP CODE 1 TO PLAY NOTE
 100  P(1)=1.
      GO TO 30
C     OP CODE 2 TO DEFINE INSTRUMENT
 200  P(I)=2.
      IDEF=1
      N1=1
      GO TO 70
 2000 P(2)=XN
      N1=2
      GO TO 70
 2001 P(3)=XN
      IP(1)=3
      GO TO 50
C     OUT BOX
 201  P(3)=101.
      NPW=2
      IF (STER) 220,220,2011
 2011 SNA8=1.
      STER=0
      GO TO 220
C     OSCILLATOR
 202  P(3)=102.
      NPW=5
      GO TO 220
C     ADD 2
 203  P(3)=103.
      NPW=3
      GO TO 220
C     RANDOM AND INTERPOLATE
 204  P(3)=104.
      NPW=6
      
C     [page 2-3]

      GO TO 220
C     LINEAR ENVELOPE GENERATOR
 205  P(3)=105.
      NPW=7
      GO TO 220
C     STEREO OUT BOX
 206  P(3)=106.
      NPW=3
      IF(STER)220,2061,220
 2061 SNA8=1.
      STER=1.
      GO TO 220
C     THREE INPUT ADDER
 207  P(3)=107.
      NPW=4
      GO TO 220
C     FOUR INPUT ADDER
 208  P(3)=108.
      NPW=5
      GO TO 220
C     MULTIPLIER
 209  P(3)=109.
      NPW=3
      GO TO 220
C     FILTER
 210  P(3)=112.
      NPW=4
      GO TO 220
C     RANDOM AND HOLD
 211  P(3)=111.
      NPW=5
      GO TO 220
C     SET NEW FUNCTION
 212  P(3)=110.
      NPW=1
      GO TO 220
C     END OF INSTRUMENT
 217  IP(1)=2
      IDEF=0
      END=1.
      GO TO 50
C     UNNAMED UNIT - NUMERICAL NAME ASSUMED
 218  N1=8
      NUMU=1
      L=0
      GO TO 70
 219  M=XN+14.
      IF(XN.LT.11.)GO TO 29
      P(3)=XN
C     TO INTERPRET VARS IN UNIT DEFS
 220  NP=3
 221  IF(IBCD(L+1)-JSEMI) 222,240,222
 222  NP=NP+1
      L=L+1
      DO 223 N=1,4
         IF(IBCD(L)-IVT(N))223,225,223
 223  CONTINUE
 224  L=L+1
      IF(IBCD(L).EQ.JBLANK)GO TO 46
      GO TO 224
      
C     [page 2-4]

 225  GO TO (231,232,233,234),N
C     P TYPE
 231  N1=3
      GO TO 70
 2311 P(NP)=XN
      GO TO 221
C     F TYPE
 232  N1=4
      GO TO 70
 2321 P(NP)=-(XN+100.)
      GO TO 221
C     B TYPE
 233  N1=5
      GO TO 70
 2331 P(NP)=-XN
      GO TO 221
C     V TYPE
 234  N1=6
      GO TO 70
 2341 P(NP)=XN+100.
      GO TO 221
 240  IF(NUMU.EQ.1)GO TO 242
 241  IF(NPW+3-NP)42,242,42
 242  IP(1)=NP
      GO TO 50
C     OP CODE 3 - TO GENERATE FUNCTION
 300  P(1)=3.
      GO TO 30
C     OP CODE 4 -- TO SET PARAM 3RD PASS
 400  P(1)=4.
      GO TO 30
C     OP CODE 5 TO END SEC
 500  P(1)=5.
      GO TO 30
C     OP CODE 6 TO TERMINATE PIECE
 600  P(1)=6.
      GO TO 30
C     OP CODE 7 TO SET PARAM 1ST PASS
 700  P(1)=7.
      GO T0 30
C     OP CODE 8 TO SET PARAM 2ND PASS
 800  P(1)=8.
      GO TO 30
C     OP CODE 9 TO EXECUTE SUB 1ST PASS
 900  P(1)=9.
      GO TO 30
C     OP CODE 10 TO EXECUTE SUB 2ND PASS
 1000 P(1)=10.
      GO TO 30
C     OP CODE 11 TO SET INTEGER 3RD PASS
 1100 P(1)=11.
      GO TO 30
C     OP CODE 12 TO SET INTEGER ALL PASSES
 1200 P(1)=12.
      GO TO 30
C     OP CODE 13 FOR COMMENTS
 1300 IF(IBCD(L)-JSEMI)1301,10,1301
 1301 L=L+1
      GO TO 1300
C     TO STORE PFIELDS
      
C     [page 2-5]

 30   IF(IDEF)32,32,43
 32   IF(IBCD(L+1)-JSEMI)33,34,33
 33   NP=NP+1
      N1=7
      GO TO 70
 331  P(NP)=XN
      GO TO 32
 34   IP(1)=NP
      IF(NP-1)47,47,50
C     ERRORS
 40   IF(IDEF)41,41,218
 41   L=L+1
      IF(IBCD(L).NE.JSEMI)GO TO 41
      PRINT 3
 3    FORMAT(26H    OP CODE NOT UNDERSTOOD)
      GO TO 49
 42   PRINT 4
 4    FORMAT(44H    UNIT CONTAINS WRONG NUMBER OF PARAMETERS)
      GO TO 49
 43   PRINT 5
 5    FORMAT(36H    INSTRUMENT DEFINITION INCOMPLETE)
      ERR=1.
      IDEF=0
      GO TO 32
 44   PRINT 6
 6    FORMAT(25H    ERROR IN NUMERIC DATA)
      ERR=1.
      IF(NUMU.EQ.1)GO TO 45
      GO TO 30
 45   PRINT 7
 7    FORMAT(46H+                          FOR UNIT DESIGNATION)
      P(3)=0.
      GO TO 220
 46   PRINT 8
 8    FORMAT(40H    IMPROPER VARIABLE IN UNIT DEFINITION)
      ERR=1.
      GO TO 221
 47   PRINT 9
 9    FORMAT(24H    STATEMENT INCOMPLETE)
 49   IP(2)=1
      GO TO 10
 50   IF(ERR.EQ.1.) GO TO 49
      RETURN
C     CONVERSION OF NUMERIC FIELD TO FLOATING POINT
 70   SGN=1.
      IF (IBCD(L+1).NE.IBC(4)) GO TO 79
      SGN=-1.
      L=L+1
 79   L1=L+1
      LD=L1
      XN=0.
 71   L=L+1
C     *** I DON'T UNDERSTAND THIS PART OF THE SCANNER!
C     C          IF(IBCD(L).EQ.JBLANK) GO TO 77
      IF (IBCD(L)-JBLANK)72,77,72
C     THIS LOOKS FOR #S, LETTERS, BLANKS, DECI.PTS, & *S. OTHERWISE=ERROR!?
C     ******** PDP ********
 72   IF(IBCD(L).LT.10)GO TO 71
      IF(IBCD(L)-IDEC)74,71,74
 74   IF(IBCD(L)-ISTAR)76,71,76
      
C     [page 2-6]

 76   GO TO 71
C     ERROR CHECK IS REMOVED!
C**   NEXT 2 LINES BY-PASSED*** 76 L=L+1
      IF(IBCD(L).EQ.JBLANK) GO TO 44
      GO TO 76
 77   IF(IBCD(L1)-ISTAR)80,78,80
 78   XN=P(NP)
      GO TO 89
 80   DO 81 LL=L1,L
         LD=LL
         IF (IBCD(LL)-IDEC)81,82,81
 81   CONTINUE
 82   IEX=0
      LA=L1
      LB=LD-1
      IF(LD-L1)86,86,83
 83   IEX=LD-LA
 84   CALL MOVR (IBCD,LA,LB)
      DO 85 LL=LA,LB
         IEX=IEX-1
         X1=IBCD(LL)
 85      XN=XN+XI*10.**IEX
 86   IF(L-LB-2)88,88,87
 87   LA=LD+1
      LB=L-1
      GO TO 84
 88   XN=XN*SGN
 89   GO TO (2000,2001,2311,2321,2331,2341,331,219),N1
C     TO WRITE S1A8 FOR MONO STEREO CONTROL
 90   P(1)=12.
      P(3)=8.
      P(4)=STER
      IP(1)=4
      END=0.
      SNA8=0.
      GO TO 50
C     FOR PREMATURE END OF FILE ON INPUT
 95   NP=2
      IP(2)=1
      L=0
      IBCD(1)=JSEMI
      GO TO 600
C     TO INITIALIZE
C     C ENTRY READ0
C     C READ (5,1,ERR=95,END=95) (CARD(I),I=1,NC)
C********PDP ********
 99   READ (1,1,ERR=95,END=95) I,(CARD(I),I=1,NC)
C*****PDP ***** FIRST 'I' IS FOR PDP LINE NUMBERS!
C     C WRITE (6,2) (CARD(I),I=1,NC)
      PRINT 2,(CARD(I),I=1,NC)
C********PDP ********
      IPDP=1
      I=0
      IDEF=0
      IBK=2
      STER=0.
      END=0.
      SNA8=0.
      RETURN
      END
      
C     [page 3-1]

C     PASS 2 MAIN PROGRAM
C     *** MUSIC V ***
      DIMENSION G(1000),I(1000),T(1000),D(10000),P(100),IP(10)
      COMMON IP,P,G,I,T,D,IXJQ,TLAST,BLAST
C     INITIALIZING PROGRAM
C     NOMINAL SAMPLING RATE, NOTE PARAMETER LENGTH, NUMBER OF CARDS
C     NO OF OP CODES, PASS 11 REPORT PRINT PARAMETER
      G(1)=0.
      G(2)=0.
      G(4)=10000.0
      NPAR=10000
      NCAR=1000
      NOPC=12
      IXJQ=0
      IEND=0
C     C*****  NREAD=2
C     C*****  NWRITE=3
      NREAD=20
      NWRITE=21
      REWIND NREAD
      REWIND NWRITE
C     INITIALIZE SECTION
 150  ID=1
      IN=1
      TLAST=0.
      BLAST=0.
C     READ SECTION OF DATA
 106  CALL READ2 (NREAD)
      I1=IP(1)
      D(ID)=I1
      I(IN)=ID
      T(IN)=P(2)
      DO 100 I2=1,I1
         I3=ID+I2
 100     D(13)=P(I2)
      ID=ID+I1+1
      IF(ID-NPAR)102,102,101
 101  CALL ERROR(20)
      STOP
 102  IN=IN+1
      IF (IN-NCAR)103,103,101
 103  IF (P(1)-5.0)104,110,104
 104  IF (P(1)-6.0)106,105,106
 105  IEND=1
      GO TO 110
C     SORT SECTION
C***  NOT USED ****** 110 CALL SORTFL
 110  IN=IN-1
      CALL SORT(T(1),T(2),IN,I)
C     EXECUTE OP CODES M SECTION
 120  DO 1 I4=1,IN
         I5=I(I4)
         I6=D(I5+1)
         IF(I6)121,121,122
 121     CALL ERROR(21)
         GO TO 1
 122     IF (I6-NOPC)123,123,121
 123     GO TO (2,2,2,2,2,2,7,8,7,10,2,8),I6
 7       CALL ERROR(22)
         GO TO 1
         
C     [page 3-2]

 8       I7=D(I5)
         I8=I5+4
         I9=I5+I7
         I10=IFIX(D(15+3))-I8
         DO 124 I11=I8,I9
            I12=I10+I11
 124     G(I12)=D(I11)
         IF(I6-I2)1,2,1
 10      I13=D(I5+3)
         IP(2)=I5
         IF(I13)125,125,126
 125     CALL ERROR(23)
         GO TO 1
 126     IF(I13-5)127,127,125
 127     GO TO (21,22,23,24,25),I13
 21      CALL PLS1
         GO TO 1
 22      CALL PLS2
         GO TO 1
 23      CALL PLS3
         GO TO 1
 24      CALL PLS4
         GO TO 1
 25      CALL PLS5
         GO TO 1
C     WRITE OUT SECTION
 2       IP(1)=D(I5)
         I18=IP(1)
         DO 133 I19=1,I18
            I20=I19+I5
 133        P(I19)=D(I20)
         CALL WRITE2 (NWRITE)
 1       CONTINUE
C     END OF SECTION OR PASS
 140  IF(IEND)141,141,143
 141  PRINT 142
 142  FORMAT (' END OF SECTION PASS II')
      GO TO 150
 143  PRINT 144
 144  FORMAT (' END OF PASS II')
      STOP
      END
C     READ2 PASS 2 DATA INPUT ROUTINE
C     *** MUSIC V ***
      SUBROUTINE READ2(N)
      DIMENSION IP(10),P(100)
      COMMON IP,P
      READ(N)K,(P(J),J=1,K)
      IP(1)=K
      RETURN
      END
C     SORT SORTING PROGRAM
C     *** MUSIC V ***
      SUBROUTINE SORT(A,B,N,L)
      DIMENSION A(N),L(N)
C     
C     SORT SORTS THE A ARRAY INTO ASCENDING NUMERICAL ORDER, PERFORMING
C     THE SAME OPERATIONS ON ARRAY L AS ON A
C     
      N1=N-1
      
C     [page 3-3]
      
      DO 10 I=1,N1
         IN=I+1
         DO 20 J=IN,N
            IF(A(I).LE.A(J))GO TO 20
            T=A(I)
            A(I)=A(J)
            A(J)=T
            NT=L(I)
            L(I)=L(J)
            L(J)=NT
 20      CONTINUE
 10   CONTINUE
      RETURN
C     C***********  ENTRY SORTFL
C     C***********  RETURN
      END
C     WRIT2 DATA OUTPUTING ROUTINE FOR PASS 2
C     *** MUSIC V ***
      SUBROUTINE WRITE2(N)
      COMMON IP(10),P(100),G(1000),I(1000),T(1000),D(10000),IXJQ,TLAST,BLAST
      IF(G(2).EQ.0.)GO TO 150
      X=P(2)
      Y=P(4)
      ILOC=G(2)
      IF(P(1).NE.1.)GO TO 50
      P(4)=P(4)*60./CON(G,ILOC,P(2))
 50   P(2)=TLAST+(P(2)-BLAST)*60./CON(G,ILOC,P(2))
      TLAST=P(2)
      BLAST=X
 150  CALL CONVT
      K=IP(1)
      WRITE(N)K,(P(J),J=1,K)
C     *** PASS II REPORT IS OPTIONAL ***
      IF(G(1).NE.0.) RETURN
      IF(IXJQ.EQ.0) PRINT 100
      IXJQ=10
 100  FORMAT(15H1PASS II REPORT/11H0(WORD CNT))
      PRINT 101,K,(P(J),J=1,K)
      IF(G(2).NE.0.) PRINT 102,X,Y
 101  FORMAT(I8,10(F9.3))
 102  FORMAT(1H+,110X,2HB=,F7.4,2HD=,F7.4)
      RETURN
      END
C     CON2 PASS 2 FUNCTION INTERPOLATOR
C     *** MUSIC V ***
      FUNCTION CON(G,I,T)
      DIMENSION G(1)
      DO 10 J=1,1000,2
         IF (G(J)-T) 10,20,30
 30      CON = G(J-1)+((T-G(J-2))/(G(J)-G(J-2)))*(G(J+1)-G(J-1))
         RETURN
 10   CONTINUE
 20   CON = G(J+1)
      RETURN
      END
C     CONVT FOR UNIT GENERATORS CHECK
C     
C     DUMMY NO OPERATION ACTUALLY PERFORMED
C******WHEN DUMMY IS REMOVED ANOTHER CONVT MUST!!!! BE LOADED!!!*****
      
C     [page 3-4]
      
C***  SUBROUTINE CONVT
C***  COMMON IP(10),P(100),G(1000)
C***  RETURN
C***  END
C     ERRO1 GENERAL ERROR ROUTINE
C     *** MUSIC V ***
      SUBROUTINE ERROR(I)
      PRINT 100,I
 100  FORMAT(' ERROR OF TYPE',I5)
      RETURN
      END
C     C***** SUBROUTINE PLS
C     C***** ENTRY PLS1
C     C***** ENTRY PLS2
C     C***** ENTRY PLS3
C     C***** ENTRY PLS4
C     C***** ENTRY PLS5
      SUBROUTINE PLS1
      RETURN
      END
      SUBROUTINE PLS2
      RETURN
      END
      SUBROUTINE PLS3
      RETURN
      END
      SUBROUTINE PLS4
      RETURN
      END
      SUBROUTINE PLS5
      RETURN
      END
      
C     [page 4-1]
      
C     PASS3   PASS 3 MAIN PROGRAM
C     *** MUSIC V ***
C     DATA SPECIFICATION
      INTEGER PEAK
      DIMENSION T(50),TI(50),ITI(50)
      COMMON I(15000),P(100)/PARM/IP(20)/FINOUT/PEAK,NRSOR
C     C******** DATA IIIRD/Z5EECE66D/
      DATA IIIRD/976545367/
C     SET I ARRAY =0 (7/10/69)
      DATA I/15000*0/
C*****************
C     INITIALIZATION OF PIECE
C     ARBITRARY STARTING NUMBER FOR SUBROUTINE RANDU
      I(7)=IIIRD
      IP9=IP(9)
      PEAK=0
      NRSOR=0
C********NREAD=3
C********NWRITE=2
      NREAD=21
C     PDP DSK1=DEV.21
      NWRITE=1
C     PDP DSK=DEV.1
      REWIND NREAD
      REWIND NWRITE
      TYPE 10001
      ACCEPT 10002,FLNM,IDSK
C     TYPE 'PASS2' OR FILENAME + ANY POS.NUMB. TO WRITE SMPLS ON DSK.
      IF(FLNM.EQ.' '.OR.FLNM.EQ.'PASS2')FLNM='FOR21'
      CALL IFILE(21,FLNM)
      IF(IDSK.LE.0) GO TO 10003
      J='MUSAA'
      CALL PUTFILE(J)
C     IF IDSK>=1, SAMPLES WILL BE WRITTEN ON DSK (MUSAA.DMD)
      IDSK=0
      GO TO 10002
10003 IDSK=-1
10001 FORMAT(' TYPE FILE NAME'/)
10002 FORMAT(A5,I)
C**** ABOVE FOR PDP 10 ******
      SCLFT=IP(12)
      I(2)=IP(4)
      MS1=IP(7)
      MS3=MS1+(IP(8)*IP(9))-1
      MS2=IP(8)
      I(4)=IP(3)
      MOUT=IP(10)
C     INITIALIZATION OF SECTION
 5    T(1)=0.0
      DO 220 N1=MS1,MS3,MS2
 220     I(N1)=-1
      DO 221 N1=1,IP9
 221     TI(N1)=1000000.
C     MAIN CARD READING LOOP
 204  CALL DATA (NREAD)
      IF(P(2)-T(1))200,200,244
 200  IOP=P(1)
      IF(IOP)201,201,202
 201  CALL ERROR(1)
      GO TO 204
         
C     [page 4-2]
         
 202  IF(IP(1)-IOP)201,203,203
 203  GO TO (1,2,3,4,5,6,201,201,201,201,11,11),IOP
 11   IVAR=P(3)
      IVARE=IVAR+I(1)-4
      DO 297 N1=IVAR,IVARE
         IVARP=N1-IVAR+4
 297     I(N1)=P(IVARP)
      GO TO 204
 3    IGEN=P(3)
      GO TO (281,282,283,284,285),IGEN
 281  CALL GEN1
      GO TO 204
 282  CALL GEN2
      GO TO 204
 283  CALL GEN3
      GO TO 204
 284  CALL GEN4
      GO TO 204
 285  CALL GEN5
      GO TO 204
 4    IVAR=P(3)
      IVARE=IVAR+I(1)-4
      DO 296 N1=IVAR,IVARE
         IVARP=N1-IVAR+4
 296     I(N1+100)=P(IVARP)*SCLFT
      GO TO 204
 6    CALL FROUT3(IDSK)
      STOP
C     ENTER NOTE TO BE PLAYED
 1    DO 230 N1=MS1,MS3,MS2
         IF(I(N1)+1)230,231,230
 230  CONTINUE
      CALL ERROR(2)
      GO TO 204
 231  M1=N1
      M2=N1+I(1)-1
      M3=M2+1
      M4=N1+IP(8)-1
      DO 232 N1=M1,M2
         M5=N1-M1+1
 232     I(N1)=P(M5)*SCLFT
      I(M1)=P(3)
      DO 233 N1=M3,M4
 233     I(N1)=0
      DO 235 N1=1,IP9
         IF(TI(N1)-1000000.)235,234,235
 234     TI(N1)=P(2)+P(4)
         ITI(N1)=M1
         GO TO 204
 235  CONTINUE
      CALL ERROR(3)
      GO TO 204
C     DEFINE INSTRUMENT
 2    M1=I(2)
      M2=IP(5)+IFIX(P(3))
      I(M2)=M1
 218  CALL DATA (NREAD)
      IF(I(1)-2)210,210,211
 210  I(M1)=0
      I(2)=M1+1
      
C     [page 4-3]
      
      GO TO 204
 211  I(M1)=P(3)
      M3=I(1)
      I(M1+1)=M1+M3-1
      M1=M1+2
      DO 217 N1=4,M3
         M5=P(N1)
         IF(M5)212,213,213
 212     IF(M5+100)300,301,301
 300     I(M1)=-IP(2)+(M5+101)*IP(6)
         GO TO 216
 301     I(M1)=-IP(13)+(M5+1)*IP(14)
         GO TO 216
 213     IF(M5-100)214,214,215
 214     I(M1)=M5
         GO TO 216
 215     I(M1)=M5+262144
 216     M1=M1+1
 217  CONTINUE
      GO TO 218
C     PLAY TO ACTION TIME
 244  T(2)=P(2)
 250  TMIN=1000000.
      IREST=1
      DO 241 N1=1,IP9
         IF(TMIN-TI(N1))241,241,240
 240     TMIN=TI(N1)
         MNOTE=N1
 241  CONTINUE
      IF(1000000.-TMIN)251,251,243
 243  IF(TMIN-T(2))245,245,246
 245  T(3)=TMIN
      GO TO 260
 246  T(3)=T(2)
      GO TO 260
 247  IF(T(1)-T(2))249,200,200
 249  TI(MNOTE)=1000000.
      M2=ITI(MNOTE)
      I(M2)=-1
      GO TO 250
C     SETUP REST
 251  T(3)=T(2)
      IREST=2
      GO TO 260
C     PLAY
 260  ISAM=(T(3)-T(1))*FLOAT(I(4))+.5
      T(1)=T(3)
      IF(ISAM)247,247,266
 266  IF(ISAM-IP(14))262,262,263
 262  I(5)=ISAM
      ISAM=0
      GO TO 264
 263  I(5)=IP(14)
      ISAM=ISAM-IP(14)
 264  IF(I(8))290,290,291
 290  M3=MOUT+I(5)-1
      MSAMP=I(5)
      GO TO 292
 291  M3=MOUT+(2*I(5))-1
      MSAMP=2*I(5)
      
C     [page 4-4]
      
 292  DO 267 N1=MOUT,M3
 267     I(N1)=0
         GO TO (268,265),IREST
 268     DO 270 NS1=MS1,MS3,MS2
            IF(I(NS1)+1)271,270,271
C     GO THROUGH UNIT GENERATORS IN INSTRUMENT
 271        I(3)=NS1
            IGEN=IP(5)+I(NS1)
            IGEN=I(IGEN)
 272        I(6)=IGEN
CC***** IF((IGEN)-101)293,294,294
CC***** 293 CALL SAMGEN(I)
CC***** ABOVE FOR MACHINE LANG. UNIT GENERATORS ******
CC***** GO TO 295
 294        CALL FORSAM
 295        IGEN=I(IGEN+1)
            IF(I(IGEN))270,270,272
 270     CONTINUE
 265     CALL SAMOUT(IDSK,MSAMP)
      IF(ISAM)247,247,266
      END
      
      
C     [page 5-1]
      
C     FORS3        FORTRAN UNIT GENERATOR ROUTINE
C     *** MUSIC V ***
      SUBROUTINE FORSAM
      DIMENSION I(15000),P(100),IP(20),L(8),M(8)
      COMMON I,P/PARM/IP
      EQUIVALENCE (M1,M(1)),(M2,M(2)),(M3,M(3)),(M4,M(4)),(M5,M(5)),(M6,M(6)),(M7,M(7)),(M8,M(8)),(L1,L(1)),(L2,L(2)),(L3,L(3)),(L4,L(4)),(L5,L(5)),(L6,L(6)),(L7,L(7)),(L8,L(8)),(RN1,IRN1),(RN3,IRN3),(RN,IRN)
C     C***** DATA IMULT/Z5EECE66D/
      DATA IIIRD/976545367/
      SFI=1./FLOAT(IP(12))
      SFF=1./FLOAT(IP(15))
      SFID=FLOAT(IP(12))
      SFXX=FLOAT(IP(12))/FLOAT(IP(15))
      XNFUN=IP(6)-1
C     COMMON INITIALIZATION OF GENERATORS
      N1=I(6)+2
      N2=I(N1-1)-1
      DO 204 J1=N1,N2
         J2=J1-N1+1
         IF(I(J1))200,201,201
 200     L(J2)=-I(J1)
         M(J2)=1
         GO TO 204
 201     M(J2)=0
         IF(I(J1)-262144)202,202,203
C*****WHAT DOES THE BIG NUMBER DO?????
 202     L(J2)=I(J1)+I(3)-1
         GO TO 204
 203     L(J2)=I(J1)-262144
 204  CONTINUE
      NSAM=I(5)
      N3=I(N1-2)
      NGEN=N3-100
      GO TO (101,102,103,104,105,106,107,108,109,110,111,112),NGEN
 112  RETURN
C     UNIT GENERATORS
C     OUTPUT BOX
 101  IF(M1)260,260,261
 260  IN1=I(L1)
 261  CONTINUE
      DO 270 J3=1,NSAM
         IF(M1)265,265,264
 264     J4=L1+J3-1
         IN1=I(J4)
 265     J5=L2+J3-1
         I(J5)=IN1+I(J5)
 270  CONTINUE
      RETURN
C     OSCILLATOR
 102  SUM=FLOAT(I(L5))*SFI
      IF(M1)280,280,281
 280  AMP=FLOAT(I(L1))*SFI
 281  IF(M2)282,282,283
 282  FREQ=FLOAT(I(L2))*SFI
 283  CONTINUE
      DO 293 J3=1,NSAM
         J4=INT(SUM)+L4
         F=FLOAT(I(J4))
         
C     [page 5-2]
         
         IF(M2)285,285,286
 285     SUM=SUM+FREQ
         GO TO 290
 286     J4=L2+J3-1
         SUM=SUM+FLOAT(I(J4))*SFI
C     C 290 IF(SUM-XNFUN)288,287,287
 290     IF(SUM.GE.XNFUN)GO TO 287
C     C 287 SUM=SUM-XNFUN
         IF(SUM.LT.0.0)GO TO 289
 288     J5=L3+J3-1
         IF(M1)291,291,292
 291     I(J5)=IFIX(AMP*F*SFXX)
         GO TO 293
C************
 287     SUM=SUM-XNFUN
         GO TO 288
 289     SUM=SUM+XNFUN
         GO TO 288
C**********ABOVE FOR FM (NEG. FREQ. TO OSCIL)
 292     J6=L1+J3-1
         I(J5)=IFIX(FLOAT(I(J6))*F*SFF)
 293  CONTINUE
      I(L5)=IFIX(SUM*SFID)
      RETURN
C     ADD TWO BOX
 103  IF(M1)250,250,251
 250  IN1=I(L1)
 251  IF(M2)252,252,253
 252  IN2=I(L2)
 253  DO 258 J3=1,NSAM
         IF(M1)255,255,254
 254     J4=L1+J3-1
         IN1=I(J4)
 255     IF(M2)257,257,256
 256     J5=L2+J3-1
         IN2=I(J5)
 257     J6=L3+J3-1
         I(J6)=IN1+IN2
 258  CONTINUE
      RETURN
C     RANDOM INTERPOLATING GENERATOR
 104  SUM=FLOAT(I(L4))*SFI
      IF(M1)310,310,311
 310  XIN1=FLOAT(I(L1))*SFI
 311  IF(M2)312,312,313
 312  XIN2=FLOAT(I(L2))*SFI
 313  IRN1=I(L5)
      IRN3=I(L6)
      DO 340 J3=1,NSAM
         IF(M1)316,316,315
 315     J4=L1+J3-1
         XIN1=FLOAT(I(J4))*SFI
 316     IF(M2)318,318,317
 317     J5=L2+J3-1
         XIN2=FLOAT(I(J5))*SFI
 318     IF(SUM-XNFUN)320,319,319
 319     SUM=SUM-XNFUN
         I(7)=IABS(I(7)*IMULT)
         RN4=(2.*FLOAT(I(7))*SFF-1.)
         RN2=RN4-RN3
         
C     [page 5-3]
         
         RN1=RN3
         RN3=RN4
         GO TO 321
 320     RN2=RN3-RN1
 321     J7=L3+J3-1
         I(J7)=XIN1*(RN1+(RN2*SUM)/XNFUN)*SFID
         SUM=SUM+XIN2
 340  CONTINUE
      I(L4)=IFIX(SUM*SFID)
      I(L5)=IRN1
      I(L6)=IRN3
      RETURN
C     ENVELOPE GENERATOR
 105  SUM=FLOAT(I(L7))*SFI
      IF(M1)380,380,381
 380  XIN1=FLOAT(I(L1))*SFI
 381  IF(M4)382,382,383
 382  XIN4=FLOAT(I(L4))*SFI
 383  IF(M5)384,384,385
 384  XIN5=FLOAT(I(L5))*SFI
 385  IF(M6)386,386,387
 386  XIN6=FLOAT(I(L6))*SFI
 387  X1=XNFUN/4.
      X2=2.*X1
      X3=3.*X1
      DO 403 J3=1,NSAM
         J4=INT(SUM)+L2
         F=FLOAT(I(J4))
         IF(M1)405,405,404
 404     J8=L1+J3-1
         XIN1=FLOAT(I(J8))*SFI
 405     IF(SUM-XNFUN)389,388,388
 388     SUM=SUM-XNFUN
 389     IF(SUM-X1)390,390,393
 390     IF(M4)392,392,391
 391     J4=L4+J3-1
         XIN4=FLOAT(I(J4))*SFI
 392     SUM=SUM+XIN4
         GO TO 402
 393     IF(SUM-X2)394,394,397
 394     IF(M5)396,396,395
 395     J5=L5+J3-1
         XIN5=FLOAT(I(J5))*SFI
 396     SUM=SUM+XIN5
         GO TO 402
 397     IF(M6)400,400,399
 399     J6=L6+J3-1
         XIN6=FLOAT(I(J6))*SFI
 400     SUM=SUM+XIN6
 402     J7=L3+J3-1
         I(J7)=IFIX(XIN1*F*SFXX)
 403  CONTINUE
      I(L7)=IFIX(SUM*SFID)
      RETURN
C     STEREO OUTPUT BOX
 106  IF(M1)500,500,501
 500  IN1=I(L1)
 501  IF(M2)502,502,503
 502  IN2=I(L2)
 503  NSSAM=2*NSAM
      
C     [page 5-4]
      
C     6/29/70 L.C.SMITH
      ICT=0
      DO 510 J3=1,NSSAM,2
         IF(M1) 505,505,504
C     C*** 504 J4=L1+J3-1
 504     J4=L1+ICT
         IN1=I(J4)
 505     J5=L3+J3-1
         I(J5)=IN1+I(J5)
         IF(M2)507,507,506
C     C*** 506 J4=L2+J3-1
 506     J4=L2+ICT
         IN2=I(J4)
 507     J5=L3+J3
         I(J5)=IN2+I(J5)
 510  CONTINUE
      RETURN
C     ADD 3 BOX
 107  IF(M1)750,750,751
 750  IN1=I(L1)
 751  IF(M2)752,752,753
 752  IN2=I(L2)
 753  IF(M3)754,754,755
 754  IN3=I(L3)
 755  DO 780 J3=1,NSAM
         IF(M1)757,757,756
 756     J4=L1+J3-1
         IN1=I(J4)
 757     IF(M2)759,759,758
 758     J5=L2+J3-1
         IN2=I(J5)
 759     IF(M3)761,761,760
 760     J6=L3+J3-1
         IN3=I(J6)
 761     J7=L4+J3-1
         I(J7)=IN1+IN2+IN3
 780  CONTINUE
      RETURN
C     ADD 4 BOX
 108  IF(M1)850,850,851
 850  IN1=I(L1)
 851  IF(M2)852,852,853
 852  IN2=I(L2)
 853  IF(M3)854,854,855
 854  IN3=I(L3)
 855  IF(M4)856,856,857
 856  IN4=I(L4)
 857  DO 880 J3=1,NSAM
         IF(M1)859,859,858
 858     J4=L1+J3-1
         IN1=I(J4)
 859     IF(M2)861,861,860
 860     J5=L2+J3-1
         IN2=I(J5)
 861     IF(M3)863,863,862
 862     J6=L3+J3-1
         IN3=I(J6)
 863     IF(M4)865,865,864
 864     J7=L4+J3-1
         IN4=I(J7)
         
C     [page 5-5]
         
 865     J8=L5+J3-1
         I(J8)=IN1+IN2+IN3+IN4
 880  CONTINUE
      RETURN
C     MULTIPLIER
 109  IF(M1)900,900,901
 900  XIN1=FLOAT(I(L1))*SFI
 901  IF(M2)902,902,903
 902  XIN2=FLOAT(I(L2))*SFI
 903  DO 908 J=1,NSAM
         IF(M1)905,905,904
 904     J4=L1+J3-1
         XIN1=FLOAT(I(J4))*SFI
 905     IF(M2)907,907,906
 906     J5=L2+J3-1
         XIN2=FLOAT(I(J5))*SFI
 907     J6=L3+J3-1
         I(J6)=XIN1*XIN2*SFID
 908  CONTINUE
      RETURN
C     SET NEW FUNCTION IN OSC OR ENV
 110  ILOC=N1+6
      IF(I(N1+1).EQ.105) ILOC=N1+4
      IN1=I(3)+I(N1)-1
      IIN1=I(IN1)/IP(12)
      IF(IIN1)960,960,955
 955  I(ILOC)=-IP(2)-(IIN1-1)*IP(6)
 960  RETURN
C     RANDOM AND HOLD GENERATOR
 111  SUM=FLOAT(I(L4))*SFI
      IF(M1)910,910,911
 910  XIN1=FLOAT(I(L1))*SFI
 911  IF(M2)912,912,913
 912  XIN2=FLOAT(I(L2))*SFI
 913  IRN=I(L5)
      DO 940 J3=1,NSAM
         IF(M1)916,916,915
 915     J4=L1+J3-1
         XIN1=FLOAT(I(J4))*SFI
 916     IF(M2)918,918,917
 917     J5=L2+J3-1
         XIN2=FLOAT(I(J5))*SFI
 918     IF(SUM-XNFUN)920,919,919
 919     SUM=SUM-XNFUN
         I(7)=IABS(I(7)*IMULT)
         RN=(2.*FLOAT(I(7))*SFF-1.)
 920     J7=L3+J3-1
         I(J7)=XIN1*RN*SFID
         SUM=SUM+XIN2
 940  CONTINUE
      I(L4)=IFIX(SUM*SFID)
      I(L5)=IRN
      RETURN
      END
      
C     [page 6-1]
      
C     GEN1 FUNCTION GENERATOR 1
C     *** MUSIC V ***
      SUBROUTINE GEN1
      DIMENSION I(15000),P(100),IP(20)
      COMMON I,P/PARM/IP
      N1=IP(2)+(IFIX(P(4))-1)*IP(6)
      M1=7
      SCLFT=IP(15)
 102  IF(P(M1+1))103,103,100
 100  V1=P(M1-2)*SCLFT
      V2=(P(M1)-P(M1-2))/(P(M1+1)-P(M1-1))*SCLFT
      MA=N1+IFIX(P(M1-1))
      MB=N1+IFIX(P(M1+1))-1
      DO 101 J=MA,MB
         XJ=J-MA
 101     I(J)=V1+V2*XJ
      IF(IFIX(P(M1+1)).EQ.(IP(6)-1))GO TO 103
      M1=M1+2
      GO TO 102
 103  I(MB+1)=P(M1)*SCLFT
      RETURN
      END
C     GEN2 FUNCTION GENERATOR 2
c     *** MUSIC V ***
      SUBROUTINE GEN2
      DIMENSION I(15000),P(100),IP(20),A(7000)
      COMMON I,P/PARM/IP
      EQUIVALENCE(I,A)
      SCLFT=IP(15)
      N1=IP(2)+(IFIX(P(4))-1)*IP(6)
      N2=N1+IP(6)-1
      DO 101 K1=N1,N2
 101     A(K1)=0.0
      FAC=6.283185/(FLOAT(IP(6))-1.0)
      NMAX=I(1)
      N3=5+INT(ABS(P(NMAX)))-1
      IF(N3-5)104,100,100
 100  DO 103 J=5,N3
         FACK=FAC*FLOAT(J-4)
         DO 102 K=N1,N2
 102        A(K)=A(K)+SIN(FACK*FLOAT(K-N1))*P(J)
 103     CONTINUE
 104  N4=N3+1
      N5=I(1)-1
      IF(N5-N4)114,105,105
 105  DO 107 J1=N4,N5
         FACK=FAC*FLOAT(J1-N4)
         DO 106 K1=N1,N2
 106        A(K1)=A(K1)+COS(FACK*FLOAT(K1-N1))*P(J1)
 107     CONTINUE
 114  CONTINUE
      IF(P(NMAX))112,112,108
 108  FMAX=0.0
      DO 110 K2=N1,N2
         IF(ABS(A(K2))-FMAX)110,110,109
 109     FMAX=ABS(A(K2))
 110  CONTINUE
 113  DO 111 K3=N1,N2
 111     I(K3)=(A(K3)*SCLFT*.99999)/FMAX
      RETURN
                  
C     [page 6-2]
                  
 112  FMAX=.99999
      GO TO 113
      END
C     GEN3 FUNCTION GENERATOR 3
C     *** MUSIC V ***
C     ASSUMPTIONS--P(4) = THE NUMBER OF THE FUNCTION TO BE GENERATED,
C     I(1) = WORD COUNT FOR CURRENT DATA RECORD
C     P(5) = THE BEGINNING THE THE LIST OF DESCRIPTION NUMBERS
C     IP(2) = THE BEGINNING SUBSCRIPT FOR FUNCTIONS IN THE I ARRAY,
C     IP(6) = THE LENGTH OF THE FUNCTIONS
C     IP(15) = SCALE FACTOR FOR STORED FUNCTIONS
C     
      SUBROUTINE GEN3
      COMMON I(15000),P(100) /PARM/ IP(20)
      N=I(1)-5
      NL=5
      SCLFT=IP(15)
      LL=IP(6)
      RMIN=0
      RMAX=0
      NR=NL+N
      DO 10 J=NL,NR
         IF(P(J).GT.RMAX) RMAX=P(J)
 10      IF(P(J).LT.RMIN) RMIN=P(J)
      DIV=AMAX1(ABS(RMIN),ABS(RMAX))
      N1 = IP(2) + (IFIX(P(4))-1)*IP(6)
      I(N1)=(P(NL)/DIV)*SCLFT
      LAST=N1
      DO 100 J=1,N
         LL = LL-LL/(N-J+1)
         IX = N1+IP(6)-LL-1
         IX2 = NL+J
         I(IX)=(P(IX2)/DIV)*SCLFT
         DELTA=FLOAT(I(IX))-FLOAT(I(LAST))
         NR = IX-LAST-1
         SEG = NR+1
         HNCR=DELTA/SEG
         DO 50 K=1,NR
            IX2 = LAST+K
 50         I(IX2)=FLOAT(I(IX2-1))+HNCR
 100     LAST=IX
      RETURN
      END
C     DATA3 PASS 3 DATA INPUTING ROUTINE
C     *** MUSIC V ***
      SUBROUTINE DATA(N)
      COMMON I(15000),P(100)
      READ(N) K,(P(J),J=1,K)
      I(1)=K
      RETURN
      END
C     PARM CONTROL DATA SPECIFICATION FOR PASS 3
C     *** MUSIC V ***
C     
C     IP(1) = NUMBER OF OP CODES
C     IP(2) = BEGINNING SUBSCRIPT OF FIRST FUNCTION
C     IP(3) = STANDARD SAMPLING RATE
C     IP(4) = BEGINNING SUBSCRIPT OF INSTRUMENT DEFINITIONS
C     IP(5) = BEGINNING OF LOCATION TABLE FOR INSTRUMENT DEFINITIONS
C     IP(6) = LENGTH OF FUNCTIONS
      
C     [page 6-3]
      
C     IP(7) = BEGINNING OF NOTE CARD PARAMETERS
C     IP(8) = LENGTH OF NOTE CARD PARAMETER BLOCKS
C     IP(9) = NUMBER OF NOTE CARD PARAMETER BLOCKS
C     IP(10) = BEGINNING OF OUTPUT DATA BLOCK
C     IP(11) = SOUND ZERO (SILENCE VALUE)
C     IP(12) = SCALE FACTOR FOR NOTE CARD PARAMETERS
C     IP(13) = BEGINNING OF GENERATOR INPUT-OUTPUT BLOCKS
C     IP(14) = LENGTH OF GENERATOR INPUT-OUTPUT BLOCKS
C     IP(15) = SCALE FACTOR FOR FUNCTIONS
C     
      BLOCK DATA
      COMMON /PARM/IP(20)
      DATA IP/12,512,10000,14500,14400,512,13000,35,40,6657,2048,
     1     "1000000,6657,512,"377777777777,5*0/
C**** BIG NUMB. IS IBM360'S BIGGEST. 1  65536,6657,512,Z7FFFFFFF/
      END
C**** SUBROUTINE DUM
C**** ENTRY SAMGEN
C**** ENTRY GEN4
C**** ENTRY GEN5
C**** END
      SUBROUTINE SAMGEN
      RETURN
      END
      SUBROUTINE GEN4
      END
      SUBROUTINE GEN5
      END
C     **** DUMMY SUBROUTINES ****
      
      
      SUBROUTINE FROUT3(IDSK)
C     TERMINATE OUTPUT
      INTEGER PEAK
      COMMON I(15000),P(100)/PARM/IP(20)/FINOUT/PEAK,NRSOR
      K=IP(10)
      L=IP(10)+IP(14)-1
      DO 1 J=K,L
 1       I(J)=0
      CALL SAMOUT(IDSK,IP(14))
C     REWIND NWRITE
C     WRITE(6,10) PEAK,NRSOR
      TYPE 10,PEAK,NRSOR
C     CALL EXIT
      IF(IDSK.LT.0)CALL EXIT
      J=IP(10)
      L=J+1024
      DO 2 K=J,L
 2       I(K)=0
C     WILL WRITE 1024 0'S ON DSK.
      CALL FASTOUT(I(J),1024)
      CALL FINFILE
      CALL EXIT
 10   FORMAT ('0PEAK AMPLITUDE WAS',I8/'0NUMBER OF SAMPLES OUT OF RANGE WAS',I8)
      END
      
      
C     DSMOUT DEBUG SAMOUT
C     *** MUSIC V ***
      
C     [page 6-4]
      
C     DEBUG SAMOUT
      SUBROUTINE SAMOUT(IDSK,N)
      DIMENSION IDBUF(2000),MS(3)
C***  IDSK IS FLAG TO WRITE SAMPLES ON DSK -- PDP ****
C***  IDBUF WILL STORE PACKED SAMPLES. ****
      DIMENSION I(15000),T(10),P(100),IP(20)
      COMMON I,P/PARM/IP/FINOUT/PEAK,NRSOR
      INTEGER PEAK
      IF(IDSK.GE.0) GO TO 99
      N1=N
      PRINT 100,N1
 100  FORMAT(7H OUTPUTI6,8H SAMPLES)
      N2=IP(10)-1
      N3=10
      GO TO 104
 106  DO 101 L=1,10
         J=N2+L
 101     T(L)=FLOAT(I(J))/FLOAT(IP(12))
      PRINT 102, (T(K),K=1,N3)
 102  FORMAT(1H 10F11.4)
      N2=N2+10
      N1=N1-10
      IF(N1)103,103,104
 103  RETURN
 104  IF(N1-10)105,106,106
 105  N3=N1
      GO TO 106
      
 99   J=IDSK+1
      M1=IP(10)
      M2=0
      ISC=IP(12)
      IDSK=IDSK+N
C     COUNTS SAMPLES TO DATE
      DO 1 K=J,IDSK
         N1=I(M1+M2)/ISC
         IF(N1.GT.PEAK)PEAK=N1
         IDBUF(K)=N1
 1       M2=M2+1
      IF(IDSK.LT.768)RETURN
         
      KL=0
      DO 2 K=1,768,3
         KL=KL+1
         KJ=K-1
         MS(1)=IDBUF(K)
         IF(MS(1).EQ.2048) MS(1)=2047
C     A 2048 IN THE 12 LEFT HAND BITS CREATES PROBLEMS
         DO 3 L=2,3
            MS(L)=IDBUF(KJ+L)
 3          IF(MS(L).LT.0) MS(L)=4096+MS(L)
 2       IDBUF(KL)=MS(3)+MS(2)*4096+MS(1)*16777216
C     PACKS 3 SMPLS TO A 36-BIT WORD. 4096=2**12, 16---=2**24.
C     MS(1) HAS LEFT HAND 12 BITS; MS(2), MIDDLE 12 BITS; MS(3), RIGHT 12.
C     NEGATIVE NUMBERS RUN FROM 4096(I.E. -1) TO 2049(I.E. -2048).
      CALL FASTOUT(IDBUF(1),256)
      J=IDSK-768
      IF(J.LT.1) GO TO 4
      DO 5 K=1,J
 5       IDBUF(K)=IDBUF(768+K)
                  
C     [page 6-5]
                  
 4    IDSK=J
      RETURN
      END
      
C     ERROR1 GENERAL ERROR ROUTINE
C     *** MUSIC V ***
      SUBROUTINE ERROR(I)
      PRINT 100,I
 100  FORMAT(' ERROR OF TYPE',I5)
      RETURN
      END
      
      
      
      
      
