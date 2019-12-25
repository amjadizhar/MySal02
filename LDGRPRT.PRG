* Program: LDGRPRT.PRG
* All Ledger Heads Titles.
* Written on 1996
* This update: 21-Mar-2016
*********************************
*********************************
*PROCEDURE LDGRPRT
*********************************

CLEAR
CLOSE ALL

SET COLOR TO 7/1
SET CONFIRM OFF

CLEAR

USE ldgrcode ORDER ac1

IF RECCOUNT()=0
   CLEAR
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF

NO= ' '

DO WHILE .not. NO$'YN'
   SET COLOR TO +7/1
   @ 2, 2 SAY 'Ledger Heads Printing/Display :'
   @ 3, 2 SAY 'Do You Want Printing/Display With Opening Balances - Y/N ' GET NO FUNCTION '!'
   READ
   SET COLOR TO 7/1
ENDDO

STORE 0 TO TOT, TOT1, GTOT, CNT
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE, COL, W_DIFF
SUM FOR OPEN_STAT='DR' OPEN_BAL TO AMT1
SUM FOR OPEN_STAT='CR' OPEN_BAL TO AMT2

@ 7, 10 SAY 'Opening Balances Checking'
@ 10, 10 SAY 'Debit  Total :'
@ 12, 10 SAY 'Credit Total :'
@ 10, 30 SAY AMT1 PICTURE '99,999,999,999.99'
@ 12, 30 SAY AMT2 PICTURE '99,999,999,999.99'
W_DIFF= AMT1-AMT2
LINE= 13
LINE= LINE+1
@ LINE, 10 SAY 'Difference   :'
@ LINE, 30 SAY ABS(W_DIFF) PICTURE '99,999,999,999.99'
HEADING= .T.
PAGE= 1
GOTO TOP
P= ' '

DO WHILE .not. P$'PSQ'
   @ 21, 40 SAY 'Ledger Heads Printing'
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO

IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR

IF P='P'
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   STORE 0 TO SPAGE, EPAGE
   @ 10, 10 SAY 'Start At Page Number (Return For Beginning) ' GET SPAGE PICTURE '99'
   @ 12, 10 SAY 'Stop After Page Number (Return For End)     ' GET EPAGE PICTURE '99'
   READ
   IF SPAGE=0
      SPAGE= 1
   ENDIF

   DO WHILE .not. EOF()
      IF HEADING
         L= 2
         IF P='P'
            L= L+1
         ELSE
            L= L+1
         ENDIF
         L= L+2
         IF NO='Y'
            L= L+1
         ENDIF
         L= L+1
         HEADING= .F.
      ENDIF
      L= L+1
      IF C2=0.and.C3=0.and.RIGHT(STR(C1),1)='0'
         L= L+1
         L= L+1
      ENDIF
      IF C3=0.and.RIGHT(STR(C1),1)<>'0'
         L= L+1
      ENDIF
      IF NO='Y'
         IF C3<>0
            IF OPEN_STAT='DR'
               TOT= TOT+OPEN_BAL
            ELSE
               TOT1= TOT1+OPEN_BAL
            ENDIF
         ENDIF
      ENDIF
      IF PAGE>=SPAGE
         EXIT
      ENDIF
      SKIP
      IF L>56
         L= 2
         PAGE= PAGE+1
         HEADING= .T.
      ENDIF
   ENDDO
   CLEAR
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
HEADING= .T.

DO WHILE .not. EOF()
   IF HEADING
      L= 2
      IF P='P'
         DO top-prt1
         @ 2, 15 SAY CHR(18)
         @ 2, 33 SAY 'LEDGER  HEADS'
         @ 3, 32 SAY '-------------'
         @ 3, 65 SAY 'Page -'
         @ 3, 71 SAY PAGE PICTURE '99'
         L= L+1
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 2, 30 SAY 'LEDGER  HEADS'
         L= L+1
         @ L, 30 SAY '-------------  '
         CNT= 0
         MREC2= RECNO()
      ENDIF
      L= L+2
      @ L, 7 SAY 'Code'
      @ L, 28 SAY 'Title'
      IF NO='Y'
         @ L, 55 SAY 'Opening Balance'
         L= L+1
         @ L, 55 SAY 'DR.           CR.'
      ENDIF
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      HEADING= .F.
   ENDIF
   L= L+1
   IF C2=0.and.C3=0.and.RIGHT(STR(C1),1)='0'
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1
      @ L, 7 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2
      @ L, 10 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3
      @ L, 19 SAY TITLE
      MLEN= LEN(RTRIM(TITLE))
      L= L+1
      @ L, 5 SAY REPLICATE('-',15+MLEN)
   ENDIF
   IF C3=0.and.RIGHT(STR(C1),1)<>'0'
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1
      @ L, 7 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2
      @ L, 10 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3
      @ L, 19 SAY TITLE
   ELSE
      IF C3<>0
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
         @ L, 8 SAY MPC1
         @ L, 10 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
         @ L, 11 SAY MPC2
         @ L, 13 SAY '-'
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
         @ L, 14 SAY MPC3
         @ L, 21 SAY TITLE
      ENDIF
   ENDIF
   IF NO='Y'
      IF C3<>0
         IF OPEN_STAT='DR'
            @ L, 50 SAY OPEN_BAL PICTURE '999,999,999.99'
            TOT= TOT+OPEN_BAL
         ELSE
            @ L, 63 SAY OPEN_BAL PICTURE '999,999,999.99'
            TOT1= TOT1+OPEN_BAL
         ENDIF
      ENDIF
   ENDIF
   SKIP
   CNT= CNT+1
   IF P='S'.and.L>=19
      @ 23, 1 SAY REPLICATE('-',79)
      @ 24, 5 SAY 'For Next Page Press :'
      @ 24, 30 SAY 'For Previous Page :'
      @ 24, 60 SAY 'Quit :'
      SET COLOR TO /w
      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
      SET COLOR TO 7/1
      CHO= 0
      DO WHILE .T.
         @ 24, 70 GET CHO PICTURE '@z 9'
         READ
         IF READKEY()=0.or.READKEY()=4.or.READKEY()=5
            EXIT
         ENDIF
      ENDDO
      IF READKEY()=0
         CLEAR
         CLOSE ALL
         CLEAR ALL
         RETURN
      ENDIF
      IF READKEY()=4
         CLEAR
         GOTO MREC2
         SKIP  -CNT
         HEADING= .T.
      ENDIF
      IF READKEY()=5
         CLEAR
         HEADING= .T.
         PAGE= PAGE+1
      ENDIF
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
      PAGE= PAGE+1
      IF EPAGE<>0
         IF PAGE>EPAGE
            EXIT
         ENDIF
      ENDIF
   ENDIF
ENDDO
IF EOF()
   IF NO='Y'
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 5 SAY 'Total Debits  :'
      @ L, 47 SAY AMT1 PICTURE '99,999,999,999.99'
      L= L+1
      @ L, 5 SAY 'Total Credits :'
      @ L, 62 SAY AMT2 PICTURE '99,999,999,999.99'
      L= L+1
      @ L, 5 SAY 'Diffirence    :'
      @ L, 51 SAY W_DIFF
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
   ENDIF
ENDIF
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL

RETURN
