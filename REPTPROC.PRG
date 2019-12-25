* REPTPROC.PRG
* INVENTORY
* AMJAD IZHAR
* 24-NOV.2005
* ASKAR



********************
PROCEDURE REP_MEN
********************
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 6, 25 SAY '  R E P O R T S   M E N U '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 8, 25 PROMPT ' 1 - Purchase Report       '
   @ 9, 25 PROMPT ' 2 - Pending Orders Report '
   @ 10, 25 PROMPT ' 3 - Sale Report           '
   @ 11, 25 PROMPT ' 4 - Current Status Report '
   @ 12, 25 PROMPT ' 5 - Consumption Reports   '
   @ 13, 25 PROMPT ' 6 - Production Reports    '
   @ 14, 25 PROMPT ' 7 - Sale Return Report (Party Wise) '
   @ 15, 25 PROMPT ' 8 - Purchase Return Report (Party Wise) '
   @ 16, 25 PROMPT ' 9 - Stock Transfer Report (Item Wise)   '
   @ 18, 25 PROMPT '10 - Return to Main Menu   '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep_men1
      CASE MCHOICE=2
         DO rep_men2
      CASE MCHOICE=3
         DO rep_men3
      CASE MCHOICE=4
         DO rep_men4
      CASE MCHOICE=5
         DO rep_men5
      CASE MCHOICE=6
         DO rep_men6
      CASE MCHOICE=7
         DO rep_salr
      CASE MCHOICE=8
         DO rep_purr
      CASE MCHOICE=9
         DO rep_tran
      CASE MCHOICE=10
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

*******************
PROCEDURE REP_PURR
********************
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx2
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 58 DOUBLE
   @ 2, 21 SAY 'PURCHASE RETURN REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.D_D1=MD1.and.D_D2=MD2.and.D_D3=MD3.and.DOC_TYPE=2
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 50 SAY 'Printer/Screen/Quit - P/S/Q' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
IF P='P'
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, GTOT, GTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 5
      IF P='P'
         DO top_prt IN TBL_PROC.PRG
      ELSE
         DO SCREEN IN TBL_PROC.PRG
      ENDIF
      @ 3, 25 SAY 'PURCHASE RETURN REPORT (PARTY WISE)'
      @ 4, 1 SAY 'From  :'
      @ 4, 9 SAY FDATE
      @ 4, 19 SAY 'To :'
      @ 4, 25 SAY TDATE
      @ 5, 1 SAY 'Code  : '
      @ 5, 9 SAY MD1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 5, 9 SAY MP1
      @ 5, 11 SAY '-'
      @ 5, 12 SAY MD2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 5, 12 SAY MP2
      @ 5, 14 SAY '-'
      @ 5, 15 SAY MD3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 5, 15 SAY MP3
      @ 5, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'Invoice #'
      @ L, 18 SAY 'Item Code'
      @ L, 29 SAY 'Descripition'
      @ L, 47 SAY ' Qty  '
      @ L, 60 SAY 'Rate '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 2
   STORE 0 TO MTOT, MTOT1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 5
         IF P='P'
            DO top_prt IN TBL_PROC.PRG
         ELSE
            DO SCREEN IN TBL_PROC.PRG
         ENDIF
         @ 3, 25 SAY 'PURCHASE RETURN REPORT (PARTY WISE)'
         @ 4, 1 SAY 'From  :'
         @ 4, 9 SAY FDATE
         @ 4, 19 SAY 'To :'
         @ 4, 25 SAY TDATE
         @ 5, 1 SAY 'Code  : '
         @ 5, 9 SAY MD1 PICTURE '99'
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
         @ 5, 9 SAY MP1
         @ 5, 11 SAY '-'
         @ 5, 12 SAY MD2 PICTURE '99'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
         @ 5, 12 SAY MP2
         @ 5, 14 SAY '-'
         @ 5, 15 SAY MD3 PICTURE '999'
         MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
         @ 5, 15 SAY MP3
         @ 5, 24 SAY MTITLE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 10 SAY 'Invoice #'
         @ L, 18 SAY 'Item Code'
         @ L, 29 SAY 'Descripition'
         @ L, 47 SAY ' Qty '
         @ L, 60 SAY 'Rate '
         @ L, 75 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      @ L, 1 SAY DATE
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 10 SAY MDOC
      @ L, 18 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 18 SAY MP1
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 21 SAY MP2
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 24 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 29 SAY ITEM
      ENDIF
      SELECT 2
      MQTY= T_QTY
      MSVALUE= PSR_TPRICE
      @ L, 47 SAY MQTY PICTURE '9999999.9999'
      @ L, 58 SAY PSR_UPRICE PICTURE '9999999.9999'
      @ L, 68 SAY MSVALUE PICTURE '999999999.99'
      MTOT= MTOT+T_QTY
      MTOT1= MTOT1+PSR_TPRICE
      GTOT= GTOT+T_QTY
      GTOT1= GTOT1+PSR_TPRICE
      IF P='S'.and.L>=17
         @ 24, 15 SAY ''
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF P='P'.and.L>56
         L= 6
         @ 1, 1 SAY CHR(18)
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      SKIP
   ENDDO
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
   @ L, 40 SAY 'Total '
   @ L, 47 SAY MTOT PICTURE '9999999.9999'
   @ L, 68 SAY MTOT1 PICTURE '999999999.99'
   L= L+1
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
ENDDO
@ L, 40 SAY REPLICATE('-',39)
L= L+1
@ L, 40 SAY 'Grand Total '
@ L, 47 SAY GTOT PICTURE '9999999.9999'
@ L, 68 SAY GTOT1 PICTURE '999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',39)
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

*********************
PROCEDURE REP_TRAN
*********************

CLOSE ALL
CLEAR
HEADING= .T.
SELECT 1
USE item ORDER item-idx
SELECT 2
USE stock ORDER itemidx1
SELECT 3
USE loc ORDER loc-idx
ABC= 0
DO WHILE ABC=0
   SELECT 2
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MI1, MI2, MI3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'STOCK TRANSFER REPORT (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Item '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 24 SAY '  -  -    '
   @ 14, 24 GET MI1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 14, 24 SAY MPC1
   @ 14, 27 GET MI2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 14, 27 SAY MPC2
   @ 14, 30 GET MI3 PICTURE '@z 9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 14, 30 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ELSE
      MUNIT= UNIT

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 14, 46 SAY ITEM
      MITEM= ITEM

IF _DOS
      SET COLOR TO 
ENDIF

   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=8
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 50 SAY 'Printer/Screen/Quit - P/S/Q ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 2
GOTO TOP
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, MD1, MD2, MD3, FLOC, TLOC, PG
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      PG= PG+1
      IF P='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 10 SAY 'Stock TRansfer (Item Wise)'
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 3, 27 SAY 'Stock Transfer (Item Wise)'
      ENDIF
      @ 4, 1 SAY 'From  :'
      @ 4, 9 SAY FDATE
      @ 5, 1 SAY 'To    :'
      @ 5, 9 SAY TDATE
      @ 6, 1 SAY 'Code  : '
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ 6, 9 SAY MPC1
      @ 6, 11 SAY '-'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ 6, 12 SAY MPC1
      @ 6, 14 SAY '-'
      MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ 6, 15 SAY MPC1
      @ 6, 22 SAY MITEM
      @ 6, 53 SAY 'Unit : '+MUNIT
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 2 SAY 'Date'
      @ L, 11 SAY 'From Location '
      @ L, 38 SAY 'To Location '
      @ L, 65 SAY 'Qty Sold'
      L= L+1
      @ L, 1 SAY REPLICATE('=',79)
      L= L+1
      HEADING= .F.
   ENDIF
   @ L, 1 SAY DATE
   IF F_T='F'
      FLOC= LOC_CODE
      SELECT 3
      SEEK FLOC
      IF FOUND()
         @ L, 11 SAY LOC
      ELSE
         @ L, 11 SAY ' '
      ENDIF
   ELSE
      TLOC= LOC_CODE
      SELECT 3
      SEEK TLOC
      IF FOUND()
         @ L, 38 SAY LOC
      ELSE
         @ L, 38 SAY ' '
      ENDIF
   ENDIF
   SELECT 2
   MQTY= T_QTY
   @ L, 65 SAY MQTY PICTURE '9999999.9999'
   MTOT= MTOT+MQTY
   IF P='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 50 SAY REPLICATE('-',30)
L= L+1
@ L, 55 SAY 'TOTAL'
@ L, 65 SAY MTOT/2 PICTURE '9999999.9999'
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
*******

**********************
PROCEDURE REP_MEN1
************************
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 6, 25 SAY 'P U R C H A S E    R E P O R T S '

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 8, 22 PROMPT ' 1 - Purchase Report (Date Wise)          '
   @ 9, 22 PROMPT ' 2 - Purchase Report (Party Wise)         '
   @ 10, 22 PROMPT ' 3 - Purchase Report (Item Wise)          '
   @ 11, 22 PROMPT ' 4 - Purchase Report Display              '
   @ 12, 22 PROMPT ' 5 - Purchase Report (Party Wise Summary) '
   @ 13, 22 PROMPT ' 6 - Purchase Report (Item Wise Summary)  '
   @ 15, 22 PROMPT ' 9 - Return to Main Menu                  '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep_pur1
      CASE MCHOICE=2
         DO rep_pur2
      CASE MCHOICE=3
         DO rep_pur3
      CASE MCHOICE=4
         DO rep_pur4
      CASE MCHOICE=5
         DO rep_pur5
      CASE MCHOICE=6
         DO rep_pur6
      CASE MCHOICE=7
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN
********

**********************
PROCEDURE REP_MEN2
**********************
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 6, 25 SAY '   O R D E R S     R E P O R T S '

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 8, 25 SAY '        P U R C H A S E S '
   @ 9, 25 PROMPT ' 1 - Pending Orders (Date Wise) '
   @ 10, 25 PROMPT ' 2 - Pending Orders (Party Wise) '
   @ 11, 25 PROMPT ' 3 - Pending Orders (Item Wise)  '
   @ 12, 25 SAY '            S A L E S '
   @ 13, 25 PROMPT ' 4 - Pending Orders (Date Wise)  '
   @ 14, 25 PROMPT ' 5 - Pending Orders (Party Wise) '
   @ 15, 25 PROMPT ' 6 - Pending Orders (Item Wise)  '
   @ 17, 25 PROMPT ' 9 - Return to Main Menu         '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep_ord1
      CASE MCHOICE=2
         DO rep_ord2
      CASE MCHOICE=3
         DO rep_ord3
      CASE MCHOICE=4
         DO rep_ord4
      CASE MCHOICE=5
         DO rep_ord5
      CASE MCHOICE=6
         DO rep_ord6
      CASE MCHOICE=7
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN
******

********************
PROCEDURE REP_PUR1
*********************
CLEAR
CLOSE ALL
SET DATE brit
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER stk_idx
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MC1, MC2, MC3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'PURCHASE REPORT (DATE WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=1
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
WAIT
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
SELECT 2
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
L= 6
DO WHILE .not. EOF()
   IF HEADING
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(14)
      @ 3, 7 SAY 'PURCHASE REPORT (DATE WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 13 SAY 'GRN #'
      @ L, 25 SAY 'Item Code'
      @ L, 39 SAY 'Descripition'
      @ L, 75 SAY 'Unit'
      @ L, 85 SAY ' Qty '
      @ L, 108 SAY 'Rate'
      @ L, 123 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
   @ L, 13 SAY MDOC_NO
   @ L, 25 SAY '  -  -    '
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 25 SAY MP1
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 28 SAY MP2
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 31 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 1
   SEEK MI_CODE
   IF FOUND()
      @ L, 39 SAY ITEM
      @ L, 75 SAY UNIT
   ENDIF
   SELECT 2
   MQTY= T_QTY
   MSVALUE= T_AMOUNT
   @ L, 85 SAY MQTY PICTURE '9999999.9999'
   @ L, 100 SAY RATE PICTURE '9999999999.9999'
   @ L, 116 SAY MSVALUE PICTURE '9999999999.99'
   MTOT= MTOT+T_QTY
   MTOT1= MTOT1+T_AMOUNT
   IF P='P'.and.L>54
      L= 6
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      PG= PG+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 75 SAY REPLICATE('-',55)
L= L+1
@ L, 75 SAY 'TOTAL'
@ L, 116 SAY MTOT1 PICTURE '9999999999.99'
L= L+1
@ L, 75 SAY REPLICATE('-',55)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN


************************
PROCEDURE REP_PUR2
************************

CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx2
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'PURCHASE REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.C_C1=MD1.and.C_C2=MD2.and.C_C3=MD3.and.DOC_TYPE=1
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
SELECT 2
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, GTOT, GTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(14)
      @ 3, 7 SAY 'PURCHASE  REPORT  (PARTY WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      @ 7, 9 SAY MD1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 7, 9 SAY MP1
      @ 7, 11 SAY '-'
      @ 7, 12 SAY MD2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 7, 12 SAY MP2
      @ 7, 14 SAY '-'
      @ 7, 15 SAY MD3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 7, 15 SAY MP3
      @ 7, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 13 SAY 'Invoice #'
      @ L, 25 SAY 'Item Code'
      @ L, 39 SAY 'Descripition'
      @ L, 75 SAY 'Unit'
      @ L, 85 SAY 'Qty '
      @ L, 108 SAY 'Rate'
      @ L, 123 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 2
   STORE 0 TO MTOT, MTOT1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 7
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 7 SAY 'PURCHASE REPORT  (PARTY WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ 7, 1 SAY 'Code  : '
         @ 7, 9 SAY MD1 PICTURE '99'
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
         @ 7, 9 SAY MP1
         @ 7, 11 SAY '-'
         @ 7, 12 SAY MD2 PICTURE '99'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
         @ 7, 12 SAY MP2
         @ 7, 14 SAY '-'
         @ 7, 15 SAY MD3 PICTURE '999'
         MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
         @ 7, 15 SAY MP3
         @ 7, 24 SAY MTITLE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 13 SAY 'Invoice #'
         @ L, 25 SAY 'Item Code'
         @ L, 39 SAY 'Descripition'
         @ L, 75 SAY 'Unit'
         @ L, 85 SAY 'Qty '
         @ L, 108 SAY 'Rate'
         @ L, 123 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      @ L, 1 SAY DATE
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 13 SAY MDOC
      @ L, 25 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 25 SAY MP1
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 28 SAY MP2
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 31 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 39 SAY ITEM
         @ L, 75 SAY UNIT
      ENDIF
      SELECT 2
      MQTY= T_QTY
      MSVALUE= PSR_TPRICE
      @ L, 85 SAY MQTY PICTURE '9999999.9999'
      @ L, 100 SAY PSR_UPRICE PICTURE '9999999.9999'
      @ L, 115 SAY MSVALUE PICTURE '999999999.99'
      MTOT= MTOT+T_QTY
      MTOT1= MTOT1+PSR_TPRICE
      GTOT= GTOT+T_QTY
      GTOT1= GTOT1+PSR_TPRICE
      IF P='P'.and.L>56
         L= 7
         @ 1, 1 SAY CHR(18)
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      SKIP
   ENDDO
   @ L, 75 SAY REPLICATE('-',55)
   L= L+1
   @ L, 75 SAY 'Total '
   @ L, 85 SAY MTOT PICTURE '9999999.9999'
   @ L, 115 SAY MTOT1 PICTURE '999999999.99'
   L= L+1
   @ L, 75 SAY REPLICATE('-',55)
   L= L+1
ENDDO
@ L, 70 SAY REPLICATE('-',60)
L= L+1
@ L, 70 SAY 'Grand Total '
@ L, 85 SAY GTOT PICTURE '9999999.9999'
@ L, 115 SAY GTOT1 PICTURE '999999999.99'
L= L+1
@ L, 70 SAY REPLICATE('-',60)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
********


*********************
PROCEDURE REP_PUR3
*********************
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 2
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MI1, MI2, MI3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'PURCHASE REPORT (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Item '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 24 SAY '  -  -    '
   @ 14, 24 GET MI1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 14, 24 SAY MPC1
   @ 14, 27 GET MI2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 14, 27 SAY MPC2
   @ 14, 30 GET MI3 PICTURE '@z 9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 14, 30 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ELSE
      MUNIT= UNIT

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 14, 39 SAY ITEM
      MITEM= ITEM

IF _DOS
      SET COLOR TO 
ENDIF

   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=1
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
SELECT 2
HEADING= .T.
L= 7
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(14)
      @ 3, 7 SAY 'PURCHASE REPORT (ITEM WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      @ 7, 9 SAY MI1 PICTURE '99'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ 7, 9 SAY MPC1
      @ 7, 11 SAY '-'
      @ 7, 12 SAY MI2 PICTURE '99'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ 7, 12 SAY MPC1
      @ 7, 14 SAY '-'
      @ 7, 15 SAY MI3 PICTURE '9999'
      MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ 7, 15 SAY MPC1
      @ 7, 22 SAY MITEM
      @ 7, 53 SAY 'Unit : '+MUNIT
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 13 SAY 'GRN #'
      @ L, 25 SAY 'Party Code'
      @ L, 39 SAY 'Descripition'
      @ L, 85 SAY 'Qty Purchased'
      @ L, 108 SAY 'Rate'
      @ L, 123 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MC1= C_C1
   MC2= C_C2
   MC3= C_C3
   @ L, 1 SAY DATE
   MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
   @ L, 13 SAY MDOC_NO
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   @ L, 25 SAY MPC1
   @ L, 27 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ L, 28 SAY MPC2
   @ L, 30 SAY '-'
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ L, 31 SAY MPC3
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 39 SAY TITLE
   ENDIF
   SELECT 2
   MQTY= T_QTY
   MSVALUE= T_AMOUNT
   @ L, 85 SAY MQTY PICTURE '9999999.9999'
   @ L, 100 SAY RATE PICTURE '999999999.9999'
   @ L, 116 SAY MSVALUE PICTURE '999999999.99'
   MTOT= MTOT+MQTY
   MTOT1= MTOT1+MSVALUE
   IF P='P'.and.L>56
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      PG= PG+1
      L= 7
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 75 SAY REPLICATE('-',58)
L= L+1
@ L, 75 SAY 'TOTAL'
@ L, 85 SAY MTOT PICTURE '999999999.9999'
@ L, 116 SAY MTOT1 PICTURE '999999999.99'
L= L+1
@ L, 75 SAY REPLICATE('-',58)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
*******

*******************
PROCEDURE REP_PUR4
*******************
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx2
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'PURCHASE  REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.C_C1=MD1.and.C_C2=MD2.and.C_C3=MD3.and.DOC_TYPE=1
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'SQ'
   @ 23, 50 SAY 'Screen/Quit  -  S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, GTOT, GTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 5
      DO SCREEN IN TBL_PROC.PRG
      @ 3, 28 SAY 'PURCHASE REPORT  (PARTY WISE)'
      @ 4, 1 SAY 'From  :'
      @ 4, 9 SAY FDATE
      @ 4, 19 SAY 'To :'
      @ 4, 25 SAY TDATE
      @ 5, 1 SAY 'Code  : '
      @ 5, 9 SAY MD1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 5, 9 SAY MP1
      @ 5, 11 SAY '-'
      @ 5, 12 SAY MD2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 5, 12 SAY MP2
      @ 5, 14 SAY '-'
      @ 5, 15 SAY MD3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 5, 15 SAY MP3
      @ 5, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'Invoice #'
      @ L, 18 SAY 'Item Code'
      @ L, 29 SAY 'Descripition'
      @ L, 47 SAY 'Qty '
      @ L, 60 SAY 'Rate '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 2
   STORE 0 TO MTOT, MTOT1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 5
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 28 SAY 'PURCHASE REPORT  (PARTY WISE)'
         @ 4, 1 SAY 'From  :'
         @ 4, 9 SAY FDATE
         @ 4, 19 SAY 'To :'
         @ 4, 25 SAY TDATE
         @ 5, 1 SAY 'Code  : '
         @ 5, 9 SAY MD1 PICTURE '99'
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
         @ 5, 9 SAY MP1
         @ 5, 11 SAY '-'
         @ 5, 12 SAY MD2 PICTURE '99'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
         @ 5, 12 SAY MP2
         @ 5, 14 SAY '-'
         @ 5, 15 SAY MD3 PICTURE '999'
         MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
         @ 5, 15 SAY MP3
         @ 5, 24 SAY MTITLE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 10 SAY 'Invoice #'
         @ L, 18 SAY 'Item Code'
         @ L, 29 SAY 'Descripition'
         @ L, 47 SAY 'Qty '
         @ L, 60 SAY 'Rate '
         @ L, 75 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      @ L, 1 SAY DATE
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 10 SAY MDOC
      @ L, 18 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 18 SAY MP1
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 21 SAY MP2
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 24 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 29 SAY ITEM
      ENDIF
      SELECT 2
      MQTY= T_QTY
      MSVALUE= PSR_TPRICE
      @ L, 47 SAY MQTY PICTURE '9999999.9999'
      @ L, 58 SAY PSR_UPRICE PICTURE '9999999.9999'
      @ L, 68 SAY MSVALUE PICTURE '999999999.99'
      MTOT= MTOT+T_QTY
      MTOT1= MTOT1+PSR_TPRICE
      GTOT= GTOT+T_QTY
      GTOT1= GTOT1+PSR_TPRICE
      IF L>=17
         @ 24, 15 SAY ''
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      SKIP
   ENDDO
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
   @ L, 40 SAY 'Total '
   @ L, 47 SAY MTOT PICTURE '9999999.9999'
   @ L, 68 SAY MTOT1 PICTURE '999999999.99'
   L= L+1
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
ENDDO
@ L, 40 SAY REPLICATE('-',39)
L= L+1
@ L, 40 SAY 'Grand Total '
@ L, 47 SAY GTOT PICTURE '9999999.9999'
@ L, 68 SAY GTOT1 PICTURE '999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',39)
@ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
WAIT ''
CLEAR
CLOSE ALL
RETURN
*******

**********************
PROCEDURE REP_PUR5
**********************
CLEAR
CLOSE ALL
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MC1, MC2, MC3, MTP, MTOT, MTQTY
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'PURCHASE REPORT (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MC1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MC2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MC3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 36 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.C_C1=MC1.and.C_C2=MC2.and.C_C3=MC3.and.DOC_TYPE=1
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      IF P='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 3 SAY 'PURCHASE REPORT (PARTY WISE SUMMARY)'
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 24 SAY 'PURCHASE REPORT (PARTY WISE SUMMARY)'
      ENDIF
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      @ 7, 9 SAY MC1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ 7, 9 SAY MP1
      @ 7, 11 SAY '-'
      @ 7, 12 SAY MC2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ 7, 12 SAY MP2
      @ 7, 14 SAY '-'
      @ 7, 15 SAY MC3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ 7, 15 SAY MP3
      @ 7, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Purchased'
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+T_AMOUNT
      MTOT= MTOT+T_AMOUNT
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 1
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 2
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   @ L, 65 SAY MAMT PICTURE '9999999999.99'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF P='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
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
********

*******************
PROCEDURE REP_PUR6
*******************
CLEAR
CLOSE ALL
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MC1, MC2, MC3, MTP, MTOT, MTQTY
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'PURCHASE REPORT (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=1
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      IF P='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 3 SAY 'PURCHASE REPORT (ITEM WISE SUMMARY)'
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 24 SAY 'PURCHASE REPORT (ITEM WISE SUMMARY)'
      ENDIF
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Purchased'
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+T_AMOUNT
      MTOT= MTOT+T_AMOUNT
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 1
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 2
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   @ L, 66 SAY MAMT PICTURE '9999999999.99'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF P='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
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
********

********************
PROCEDURE REP_ORD1
********************
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 2
USE item ORDER item_idx
SELECT 1
USE p-order ORDER prod_idx
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   MC1= 0
   STORE 0 TO MC2, MC3
   STORE DATE() TO TDATE, FDATE
   @ 1, 18 TO 3, 56 DOUBLE
   @ 2, 21 SAY 'PENDING ORDERS REPORT (DATE WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions Exist for This Party - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
L= 6
STORE 0 TO MQTY, MQTYD, MQTYB, MTOT
DO WHILE .not. EOF()
   IF HEADING
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(14)
      @ 3, 13 SAY 'ORDERS TO SUPPLIERS '
      @ 4, 23 SAY 'PENDING ORDERS REPORT (DATE WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Order Date '
      @ L, 15 SAY 'Order # '
      @ L, 25 SAY 'Item '
      @ L, 60 SAY 'Unit '
      @ L, 70 SAY 'Qty Ordered '
      @ L, 90 SAY 'Rate '
      @ L, 100 SAY 'Qty Delivered '
      @ L, 120 SAY 'Qty Balance  '
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MTOT= 0
   MTOT= QTY_ORD-QTY_DEL
   IF MTOT=0
      SKIP
      LOOP
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   @ L, 15 SAY ORDER_NO PICTURE '999999'
   SELECT 2
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 25 SAY ITEM
      @ L, 60 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 70 SAY QTY_ORD
   @ L, 85 SAY RATE
   @ L, 100 SAY QTY_DEL
   @ L, 115 SAY MTOT
   MQTY= MQTY+QTY_ORD
   MQTYD= MQTYD+QTY_DEL
   MQTYB= MQTYB+MTOT
   IF L>=56
      @ 0, 0 SAY CHR(18)
      HEADING= .T.
      L= 6
      PG= PG+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 65 SAY REPLICATE('-',67)
L= L+1
@ L, 68 SAY 'TOTAL '
@ L, 70 SAY MQTY PICTURE '9999999.9999'
@ L, 100 SAY MQTYD PICTURE '9999999.9999'
@ L, 115 SAY MQTYB PICTURE '9999999.9999'
L= L+1
@ L, 65 SAY REPLICATE('-',67)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
*******

*******************
PROCEDURE REP_ORD2
*******************
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 2
USE item ORDER item_idx
SELECT 1
USE p-order ORDER prod_idx
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   MC1= 0
   STORE 0 TO MC2, MC3
   STORE DATE() TO MDATE
   @ 1, 18 TO 3, 56 DOUBLE
   @ 2, 21 SAY 'PENDING ORDERS REPORT (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 12, 3 SAY 'Party '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 13 SAY '  -  -   '
   @ 12, 13 GET MC1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 16 GET MC2 PICTURE '@z 99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 13 SAY MPC1
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ 12, 16 SAY MPC2
   @ 12, 19 GET MC3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 12, 19 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   LOCATE FOR C1=MC1.and.C2=MC2.and.C3=MC3
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 23 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO C1=MC1.and.C2=MC2.and.C3=MC3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions Exist for This Party - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
STORE 0 TO MQTY, MQTYD, MQTYB, MTOT
DO WHILE .not. EOF()
   IF HEADING
      DO top_prt IN TBL_PROC.PRG 
      @ 3, 1 SAY CHR(14)
      @ 3, 13 SAY 'ORDERS TO SUPPLIERS '
      @ 4, 23 SAY 'PENDING ORDERS REPORT (PARTY WISE)'
      @ 5, 1 SAY 'As On : '
      @ 5, 9 SAY MDATE
      @ 6, 1 SAY 'Party : '+MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Order Date '
      @ L, 15 SAY 'Order # '
      @ L, 25 SAY 'Item '
      @ L, 60 SAY 'Unit'
      @ L, 70 SAY 'Qty Ordered '
      @ L, 90 SAY 'Rate '
      @ L, 100 SAY 'Qty Delivered '
      @ L, 120 SAY 'Qty Balance  '
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MTOT= 0
   MTOT= QTY_ORD-QTY_DEL
   IF MTOT=0
      SKIP
      LOOP
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   @ L, 15 SAY ORDER_NO PICTURE '999999'
   SELECT 2
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 25 SAY ITEM
      @ L, 60 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 70 SAY QTY_ORD
   @ L, 85 SAY RATE
   @ L, 100 SAY QTY_DEL
   MTOT= QTY_ORD-QTY_DEL
   @ L, 115 SAY MTOT
   MQTY= MQTY+QTY_ORD
   MQTYD= MQTYD+QTY_DEL
   MQTYB= MQTYB+MTOT
   IF L>=56
      @ 0, 0 SAY CHR(18)
      HEADING= .T.
      L= 1
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 65 SAY REPLICATE('-',67)
L= L+1
@ L, 68 SAY 'TOTAL '
@ L, 70 SAY MQTY PICTURE '9999999.9999'
@ L, 100 SAY MQTYD PICTURE '9999999.9999'
@ L, 115 SAY MQTYB PICTURE '9999999.9999'
L= L+1
@ L, 65 SAY REPLICATE('-',67)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
*******

********************
PROCEDURE REP_ORD3
*********************
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 2
USE item ORDER item_idx
SELECT 1
USE p-order ORDER pord_idx
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 2
   STORE 0 TO MITEM_CODE, MI1, MI2, MI3
   STORE DATE() TO MDATE
   @ 1, 18 TO 3, 55 DOUBLE
   @ 2, 21 SAY 'PENDING ORDERS REPORT (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 25 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 15 SAY 'Item  '
   @ 12, 25 GET MI1 PICTURE '99'
   READ
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 25 SAY MPC1
   @ 12, 28 GET MI2 PICTURE '99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 28 SAY MPC2
   @ 12, 31 GET MI3 PICTURE '9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 12, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   SELECT 2
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ELSE
      MUNIT= UNIT

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 12, 36 SAY ITEM
      MITEM= ITEM

IF _DOS
      SET COLOR TO 
ENDIF

   ENDIF
   SELECT 1
   SET FILTER TO I1=MI1.and.I2=MI2.and.I3=MI3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions Exist for This Party - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
STORE 0 TO MQTY, MQTYD, MQTYB, MTOT
DO WHILE .not. EOF()
   IF HEADING
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(14)
      @ 3, 13 SAY 'ORDERS TO SUPPLIERS '
      @ 4, 23 SAY 'PENDING ORDERS REPORT (ITEM WISE)'
      @ 5, 1 SAY 'As On : '
      @ 5, 9 SAY MDATE
      @ 6, 1 SAY 'Item  : '+MITEM
      @ 6, 50 SAY 'Unit : '+MUNIT
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Order Date '
      @ L, 15 SAY 'Order # '
      @ L, 25 SAY 'Party '
      @ L, 70 SAY 'Qty Ordered '
      @ L, 92 SAY 'Rate'
      @ L, 100 SAY 'Qty Delivered '
      @ L, 120 SAY 'Qty Balance  '
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MTOT= 0
   MTOT= QTY_ORD-QTY_DEL
   IF MTOT=0
      SKIP
      LOOP
   ENDIF
   MC1= C1
   MC2= C2
   MC3= C3
   @ L, 1 SAY DATE
   @ L, 15 SAY ORDER_NO PICTURE '99999'
   SELECT 3
   LOCATE FOR C1= A->C1.and.C2= A->C2.and.C3= A->C3
   IF FOUND()
      @ L, 25 SAY TITLE
   ENDIF
   SELECT 1
   @ L, 70 SAY QTY_ORD
   @ L, 85 SAY RATE
   @ L, 100 SAY QTY_DEL
   MTOT= QTY_ORD-QTY_DEL
   @ L, 115 SAY MTOT
   MQTY= MQTY+QTY_ORD
   MQTYD= MQTYD+QTY_DEL
   MQTYB= MQTYB+MTOT
   IF L>=56
      @ 0, 0 SAY CHR(18)
      HEADING= .T.
      L= 1
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 60 SAY REPLICATE('-',70)
L= L+1
@ L, 65 SAY 'TOTAL '
@ L, 70 SAY MQTY PICTURE '9999999.9999'
@ L, 100 SAY MQTYD PICTURE '9999999.9999'
@ L, 115 SAY MQTYB PICTURE '9999999.9999'
L= L+1
@ L, 60 SAY REPLICATE('-',70)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
********

************************
PROCEDURE REP_ORD4
************************
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 2
USE item ORDER item_idx
SELECT 1
USE s-order ORDER sord_idx
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   MC1= 0
   STORE 0 TO MC2, MC3
   STORE DATE() TO TDATE, FDATE
   @ 1, 18 TO 3, 56 DOUBLE
   @ 2, 21 SAY 'PENDING ORDERS REPORT (DATE WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions Exist for This Party - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
L= 6
STORE 0 TO MQTY, MQTYD, MQTYB, MTOT
DO WHILE .not. EOF()
   IF HEADING
      DO top_prt
      @ 3, 1 SAY CHR(14)
      @ 3, 13 SAY 'ORDERS FROM DEALERS '
      @ 4, 23 SAY 'PENDING ORDERS REPORT (DATE WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Order Date '
      @ L, 15 SAY 'Order # '
      @ L, 25 SAY 'Item '
      @ L, 60 SAY 'Unit '
      @ L, 70 SAY 'Qty Ordered '
      @ L, 90 SAY 'Rate '
      @ L, 100 SAY 'Qty Delivered '
      @ L, 120 SAY 'Qty Balance  '
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MTOT= 0
   MTOT= QTY_ORD-QTY_DEL
   IF MTOT=0
      SKIP
      LOOP
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   @ L, 15 SAY ORDER_NO PICTURE '999999'
   SELECT 2
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 25 SAY ITEM
      @ L, 60 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 70 SAY QTY_ORD
   @ L, 85 SAY RATE
   @ L, 100 SAY QTY_DEL
   @ L, 115 SAY MTOT
   MQTY= MQTY+QTY_ORD
   MQTYD= MQTYD+QTY_DEL
   MQTYB= MQTYB+MTOT
   IF L>=56
      @ 0, 0 SAY CHR(18)
      HEADING= .T.
      L= 6
      PG= PG+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 65 SAY REPLICATE('-',67)
L= L+1
@ L, 68 SAY 'TOTAL '
@ L, 70 SAY MQTY PICTURE '9999999.9999'
@ L, 100 SAY MQTYD PICTURE '9999999.9999'
@ L, 115 SAY MQTYB PICTURE '9999999.9999'
L= L+1
@ L, 65 SAY REPLICATE('-',67)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
*********

********************
PROCEDURE REP_ORD5
********************
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 2
USE item ORDER item_idx
SELECT 1
USE s-order ORDER sord_idx
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   MC1= 0
   STORE 0 TO MC2, MC3
   STORE DATE() TO MDATE
   @ 1, 18 TO 3, 56 DOUBLE
   @ 2, 21 SAY 'PENDING ORDERS REPORT (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 12, 3 SAY 'Party '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 13 SAY '  -  -   '
   @ 12, 13 GET MC1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 16 GET MC2 PICTURE '@z 99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 13 SAY MPC1
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ 12, 16 SAY MPC2
   @ 12, 19 GET MC3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 12, 19 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   LOCATE FOR C1=MC1.and.C2=MC2.and.C3=MC3
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 23 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO C1=MC1.and.C2=MC2.and.C3=MC3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions Exist for This Party - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
STORE 0 TO MQTY, MQTYD, MQTYB, MTOT
DO WHILE .not. EOF()
   IF HEADING
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(14)
      @ 3, 13 SAY 'ORDERS FROM DEALERS '
      @ 4, 23 SAY 'PENDING ORDERS REPORT (PARTY WISE)'
      @ 5, 1 SAY 'As On : '
      @ 5, 9 SAY MDATE
      @ 6, 1 SAY 'Party : '+MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Order Date '
      @ L, 15 SAY 'Order # '
      @ L, 25 SAY 'Item '
      @ L, 60 SAY 'Unit'
      @ L, 70 SAY 'Qty Ordered '
      @ L, 90 SAY 'Rate '
      @ L, 100 SAY 'Qty Delivered '
      @ L, 120 SAY 'Qty Balance  '
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MTOT= 0
   MTOT= QTY_ORD-QTY_DEL
   IF MTOT=0
      SKIP
      LOOP
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   @ L, 15 SAY ORDER_NO PICTURE '999999'
   SELECT 2
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 25 SAY ITEM
      @ L, 60 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 70 SAY QTY_ORD
   @ L, 85 SAY RATE
   @ L, 100 SAY QTY_DEL
   MTOT= QTY_ORD-QTY_DEL
   @ L, 115 SAY MTOT
   MQTY= MQTY+QTY_ORD
   MQTYD= MQTYD+QTY_DEL
   MQTYB= MQTYB+MTOT
   IF L>=56
      @ 0, 0 SAY CHR(18)
      HEADING= .T.
      L= 1
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 65 SAY REPLICATE('-',67)
L= L+1
@ L, 68 SAY 'TOTAL '
@ L, 70 SAY MQTY PICTURE '9999999.9999'
@ L, 100 SAY MQTYD PICTURE '9999999.9999'
@ L, 115 SAY MQTYB PICTURE '9999999.9999'
L= L+1
@ L, 65 SAY REPLICATE('-',67)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
*********


**************************
PROCEDURE REP_ORD6
**************************
CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 2
USE item ORDER item_idx
SELECT 1
USE s-order ORDER sord_idx
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 2
   STORE 0 TO MITEM_CODE, MI1, MI2, MI3
   STORE DATE() TO MDATE
   @ 1, 18 TO 3, 55 DOUBLE
   @ 2, 21 SAY 'PENDING ORDERS REPORT (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 25 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 15 SAY 'Item  '
   @ 12, 25 GET MI1 PICTURE '99'
   READ
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 25 SAY MPC1
   @ 12, 28 GET MI2 PICTURE '99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 28 SAY MPC2
   @ 12, 31 GET MI3 PICTURE '9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 12, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   SELECT 2
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ELSE
      MUNIT= UNIT

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 12, 36 SAY ITEM
      MITEM= ITEM

IF _DOS
      SET COLOR TO 
ENDIF

   ENDIF
   SELECT 1
   SET FILTER TO I1=MI1.and.I2=MI2.and.I3=MI3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions Exist for This Party - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
STORE 0 TO MQTY, MQTYD, MQTYB, MTOT
DO WHILE .not. EOF()
   IF HEADING
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(14)
      @ 3, 13 SAY 'ORDERS FROM DEALERS '
      @ 4, 23 SAY 'PENDING ORDERS REPORT (ITEM WISE)'
      @ 5, 1 SAY 'As On : '
      @ 5, 9 SAY MDATE
      @ 6, 1 SAY 'Item  : '+MITEM
      @ 6, 50 SAY 'Unit : '+MUNIT
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Order Date '
      @ L, 15 SAY 'Order # '
      @ L, 25 SAY 'Party '
      @ L, 70 SAY 'Qty Ordered '
      @ L, 92 SAY 'Rate'
      @ L, 100 SAY 'Qty Delivered '
      @ L, 120 SAY 'Qty Balance  '
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MTOT= 0
   MTOT= QTY_ORD-QTY_DEL
   IF MTOT=0
      SKIP
      LOOP
   ENDIF
   MC1= C1
   MC2= C2
   MC3= C3
   @ L, 1 SAY DATE
   @ L, 15 SAY ORDER_NO PICTURE '99999'
   SELECT 3
   LOCATE FOR C1= A->C1.and.C2= A->C2.and.C3= A->C3
   IF FOUND()
      @ L, 25 SAY TITLE
   ENDIF
   SELECT 1
   @ L, 70 SAY QTY_ORD
   @ L, 85 SAY RATE
   @ L, 100 SAY QTY_DEL
   MTOT= QTY_ORD-QTY_DEL
   @ L, 115 SAY MTOT
   MQTY= MQTY+QTY_ORD
   MQTYD= MQTYD+QTY_DEL
   MQTYB= MQTYB+MTOT
   IF L>=56
      @ 0, 0 SAY CHR(18)
      HEADING= .T.
      L= 1
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 60 SAY REPLICATE('-',70)
L= L+1
@ L, 65 SAY 'TOTAL '
@ L, 70 SAY MQTY PICTURE '9999999.9999'
@ L, 100 SAY MQTYD PICTURE '9999999.9999'
@ L, 115 SAY MQTYB PICTURE '9999999.9999'
L= L+1
@ L, 60 SAY REPLICATE('-',70)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
********

**********************
PROCEDURE REP_MEN3
**********************
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY '    S A L E S    R E P O R T S '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 7, 25 SAY '       Printing/Display'
   @ 8, 25 PROMPT ' 1 - Sale  Report   (Date Wise)  '
   @ 9, 25 PROMPT ' 2 - Sale  Report   (Party Wise) '
   @ 10, 25 PROMPT ' 3 - Sale  Report   (Item Wise)  '
   @ 11, 25 PROMPT ' 4 - Sale  Report (Party Wise Summary) '
   @ 12, 25 PROMPT ' 5 - Sale  Report (Item Wise Summary)  '
   @ 13, 25 PROMPT ' 6 - Sale  Display  (Date Wise)  '
   @ 14, 25 PROMPT ' 7 - Sale  Display  (Party Wise) '
   @ 15, 25 PROMPT ' 8 - Sale  Display  (Item Wise)  '
   @ 16, 25 PROMPT ' 9 - Sale  Display  (City Wise)  '
   @ 17, 25 PROMPT '10 - Sale  Display  (Zone Wise)  '
   @ 19, 25 PROMPT '11 - Return to Main Menu         '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep_sal1
      CASE MCHOICE=2
         DO rep_sal2
      CASE MCHOICE=3
         DO rep_sal3
      CASE MCHOICE=4
         DO rep_sal7
      CASE MCHOICE=5
         DO rep_sal8
      CASE MCHOICE=6
         DO rep_sal4
      CASE MCHOICE=7
         DO rep_sal5
      CASE MCHOICE=8
         DO rep_sal6
      CASE MCHOICE=9
         DO rep_city
      CASE MCHOICE=10
         DO rep_zone
      CASE MCHOICE=11
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN


***********************
PROCEDURE REP_SALR
***********************
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx2
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  RETURN REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.C_C1=MD1.and.C_C2=MD2.and.C_C3=MD3.and.DOC_TYPE=7
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 50 SAY 'Printer/Screen/Quit - P/S/Q' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
IF P='P'
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, GTOT, GTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 5
      IF P='P'
         DO top_prt IN TBL_PROC.PRG
      ELSE
         DO SCREEN IN TBL_PROC.PRG
      ENDIF
      @ 3, 25 SAY 'SALE RETURN REPORT (PARTY WISE)'
      @ 4, 1 SAY 'From  :'
      @ 4, 9 SAY FDATE
      @ 4, 19 SAY 'To :'
      @ 4, 25 SAY TDATE
      @ 5, 1 SAY 'Code  : '
      @ 5, 9 SAY MD1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 5, 9 SAY MP1
      @ 5, 11 SAY '-'
      @ 5, 12 SAY MD2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 5, 12 SAY MP2
      @ 5, 14 SAY '-'
      @ 5, 15 SAY MD3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 5, 15 SAY MP3
      @ 5, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'Invoice #'
      @ L, 18 SAY 'Item Code'
      @ L, 29 SAY 'Descripition'
      @ L, 47 SAY 'Qty Sold'
      @ L, 60 SAY 'Rate '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 2
   STORE 0 TO MTOT, MTOT1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 5
         IF P='P'
            DO top_prt IN TBL_PROC.PRG
         ELSE
            DO SCREEN IN TBL_PROC.PRG
         ENDIF
         @ 3, 25 SAY 'SALE RETURN REPORT (PARTY WISE)'
         @ 4, 1 SAY 'From  :'
         @ 4, 9 SAY FDATE
         @ 4, 19 SAY 'To :'
         @ 4, 25 SAY TDATE
         @ 5, 1 SAY 'Code  : '
         @ 5, 9 SAY MD1 PICTURE '99'
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
         @ 5, 9 SAY MP1
         @ 5, 11 SAY '-'
         @ 5, 12 SAY MD2 PICTURE '99'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
         @ 5, 12 SAY MP2
         @ 5, 14 SAY '-'
         @ 5, 15 SAY MD3 PICTURE '999'
         MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
         @ 5, 15 SAY MP3
         @ 5, 24 SAY MTITLE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 10 SAY 'Invoice #'
         @ L, 18 SAY 'Item Code'
         @ L, 29 SAY 'Descripition'
         @ L, 47 SAY 'Qty Sold'
         @ L, 60 SAY 'Rate '
         @ L, 75 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      @ L, 1 SAY DATE
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 10 SAY MDOC
      @ L, 18 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 18 SAY MP1
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 21 SAY MP2
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 24 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 29 SAY ITEM
      ENDIF
      SELECT 2
      MQTY= T_QTY
      MSVALUE= PSR_TPRICE
      @ L, 47 SAY MQTY PICTURE '9999999.9999'
      @ L, 58 SAY PSR_UPRICE PICTURE '9999999.9999'
      @ L, 68 SAY MSVALUE PICTURE '999999999.99'
      MTOT= MTOT+T_QTY
      MTOT1= MTOT1+PSR_TPRICE
      GTOT= GTOT+T_QTY
      GTOT1= GTOT1+PSR_TPRICE
      IF P='S'.and.L>=17
         @ 24, 15 SAY ''
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF P='P'.and.L>56
         L= 6
         @ 1, 1 SAY CHR(18)
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      SKIP
   ENDDO
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
   @ L, 40 SAY 'Total '
   @ L, 47 SAY MTOT PICTURE '9999999.9999'
   @ L, 68 SAY MTOT1 PICTURE '999999999.99'
   L= L+1
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
ENDDO
@ L, 40 SAY REPLICATE('-',39)
L= L+1
@ L, 40 SAY 'Grand Total '
@ L, 47 SAY GTOT PICTURE '9999999.9999'
@ L, 68 SAY GTOT1 PICTURE '999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',39)
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
*******


************************
PROCEDURE REP_SAL1
************************
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx1
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (DATE WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
SELECT 2
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(14)
      @ 3, 7 SAY 'SALE  REPORT  (DATE WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 13 SAY 'Invoice #'
      @ L, 25 SAY 'Item Code'
      @ L, 39 SAY 'Descripition'
      @ L, 75 SAY 'Unit'
      @ L, 89 SAY 'Qty Sold'
      @ L, 108 SAY 'Rate'
      @ L, 123 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
   @ L, 13 SAY MDOC_NO
   @ L, 25 SAY '  -  -    '
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 25 SAY MP1
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 28 SAY MP2
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 31 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 1
   SEEK MI_CODE
   IF FOUND()
      @ L, 39 SAY ITEM
      @ L, 75 SAY UNIT
   ENDIF
   SELECT 2
   MQTY= T_QTY
   MSVALUE= PSR_TPRICE
   @ L, 85 SAY MQTY PICTURE '9999999.9999'
   @ L, 100 SAY PSR_UPRICE PICTURE '9999999.9999'
   @ L, 115 SAY MSVALUE PICTURE '999999999.99'
   MTOT= MTOT+T_QTY
   MTOT1= MTOT1+PSR_TPRICE
   IF P='P'.and.L>56
      L= 6
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      PG= PG+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 75 SAY REPLICATE('-',55)
L= L+1
@ L, 75 SAY 'TOTAL'
@ L, 85 SAY MTOT PICTURE '9999999.9999'
@ L, 115 SAY MTOT1 PICTURE '999999999.99'
L= L+1
@ L, 75 SAY REPLICATE('-',55)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
********

***********************
PROCEDURE REP_SAL2
***********************
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx2
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.D_D1=MD1.and.D_D2=MD2.and.D_D3=MD3.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
SELECT 2
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, GTOT, GTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(14)
      @ 3, 7 SAY 'SALE  REPORT  (PARTY WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      @ 7, 9 SAY MD1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 7, 9 SAY MP1
      @ 7, 11 SAY '-'
      @ 7, 12 SAY MD2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 7, 12 SAY MP2
      @ 7, 14 SAY '-'
      @ 7, 15 SAY MD3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 7, 15 SAY MP3
      @ 7, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 13 SAY 'Invoice #'
      @ L, 25 SAY 'Item Code'
      @ L, 39 SAY 'Descripition'
      @ L, 75 SAY 'Unit'
      @ L, 85 SAY 'Qty Sold'
      @ L, 108 SAY 'Rate'
      @ L, 123 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 2
   STORE 0 TO MTOT, MTOT1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 7
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 7 SAY 'SALE  REPORT  (PARTY WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ 7, 1 SAY 'Code  : '
         @ 7, 9 SAY MD1 PICTURE '99'
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
         @ 7, 9 SAY MP1
         @ 7, 11 SAY '-'
         @ 7, 12 SAY MD2 PICTURE '99'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
         @ 7, 12 SAY MP2
         @ 7, 14 SAY '-'
         @ 7, 15 SAY MD3 PICTURE '999'
         MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
         @ 7, 15 SAY MP3
         @ 7, 24 SAY MTITLE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 13 SAY 'Invoice #'
         @ L, 25 SAY 'Item Code'
         @ L, 39 SAY 'Descripition'
         @ L, 75 SAY 'Unit'
         @ L, 85 SAY 'Qty Sold'
         @ L, 108 SAY 'Rate'
         @ L, 123 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      @ L, 1 SAY DATE
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 13 SAY MDOC
      @ L, 25 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 25 SAY MP1
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 28 SAY MP2
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 31 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 39 SAY ITEM
         @ L, 75 SAY UNIT
      ENDIF
      SELECT 2
      MQTY= T_QTY
      MSVALUE= PSR_TPRICE
      @ L, 85 SAY MQTY PICTURE '9999999.9999'
      @ L, 100 SAY PSR_UPRICE PICTURE '9999999.9999'
      @ L, 115 SAY MSVALUE PICTURE '999999999.99'
      MTOT= MTOT+T_QTY
      MTOT1= MTOT1+PSR_TPRICE
      GTOT= GTOT+T_QTY
      GTOT1= GTOT1+PSR_TPRICE
      IF P='P'.and.L>56
         L= 7
         @ 1, 1 SAY CHR(18)
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      SKIP
   ENDDO
   @ L, 75 SAY REPLICATE('-',55)
   L= L+1
   @ L, 75 SAY 'Total '
   @ L, 85 SAY MTOT PICTURE '9999999.9999'
   @ L, 115 SAY MTOT1 PICTURE '999999999.99'
   L= L+1
   @ L, 75 SAY REPLICATE('-',55)
   L= L+1
ENDDO
@ L, 70 SAY REPLICATE('-',60)
L= L+1
@ L, 70 SAY 'Grand Total '
@ L, 85 SAY GTOT PICTURE '9999999.9999'
@ L, 115 SAY GTOT1 PICTURE '999999999.99'
L= L+1
@ L, 70 SAY REPLICATE('-',60)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN


********************
PROCEDURE REP_SAL3
********************
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx1
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 2
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MI1, MI2, MI3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Item '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 24 SAY '  -  -    '
   @ 14, 24 GET MI1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 14, 24 SAY MPC1
   @ 14, 27 GET MI2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 14, 27 SAY MPC2
   @ 14, 30 GET MI3 PICTURE '@z 9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 14, 30 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ELSE
      MUNIT= UNIT

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 14, 46 SAY ITEM
      MITEM= ITEM

IF _DOS
      SET COLOR TO 
ENDIF

   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
@ 11, 15 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
GOTO TOP
SELECT 2
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, MD1, MD2, MD3
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(14)
      @ 3, 7 SAY 'SALE  REPORT  (ITEM WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ 7, 9 SAY MPC1
      @ 7, 11 SAY '-'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ 7, 12 SAY MPC1
      @ 7, 14 SAY '-'
      MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ 7, 15 SAY MPC1
      @ 7, 22 SAY MITEM
      @ 7, 53 SAY 'Unit : '+MUNIT
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 13 SAY 'Invoice #'
      @ L, 25 SAY 'Party Code'
      @ L, 39 SAY 'Descripition'
      @ L, 85 SAY 'Qty Sold'
      @ L, 108 SAY 'Rate'
      @ L, 123 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MD1= D_D1
   MD2= D_D2
   MD3= D_D3
   @ L, 1 SAY DATE
   MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
   @ L, 13 SAY MDOC_NO
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ L, 25 SAY MPC1
   @ L, 27 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ L, 28 SAY MPC2
   @ L, 30 SAY '-'
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ L, 31 SAY MPC3
   STORE STR(MD1,2)+STR(MD2,2)+STR(MD3,3) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 39 SAY TITLE
   ENDIF
   SELECT 2
   MQTY= T_QTY
   MSVALUE= PSR_TPRICE
   @ L, 85 SAY MQTY PICTURE '9999999.9999'
   @ L, 100 SAY PSR_UPRICE PICTURE '9999999.9999'
   @ L, 115 SAY MSVALUE PICTURE '999999999.99'
   MTOT= MTOT+MQTY
   MTOT1= MTOT1+PSR_TPRICE
   IF L>56
      L= 7
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      PG= PG+1
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 75 SAY REPLICATE('-',58)
L= L+1
@ L, 75 SAY 'TOTAL'
@ L, 85 SAY MTOT PICTURE '9999999.9999'
@ L, 115 SAY MTOT1 PICTURE '999999999.99'
L= L+1
@ L, 75 SAY REPLICATE('-',58)
EJECT
@ 1, 1 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
**********

************************
PROCEDURE REP_SAL4
************************
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx1
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (DATE WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'SQ'
   @ 5, 4 SAY 'Screen/Quit  -  S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
GOTO TOP
SELECT 2
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      DO SCREEN IN TBL_PROC.PRG
      L= 6
      @ 3, 28 SAY 'SALE  REPORT  (DATE WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'Invoice #'
      @ L, 18 SAY 'Item Code'
      @ L, 29 SAY 'Descripition'
      @ L, 47 SAY 'Qty Sold'
      @ L, 60 SAY 'Rate '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   @ L, 1 SAY DATE
   MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
   @ L, 10 SAY MDOC_NO
   @ L, 18 SAY '  -  -    '
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 18 SAY MP1
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 21 SAY MP2
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 24 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 1
   SEEK MI_CODE
   IF FOUND()
      @ L, 29 SAY LEFT(ITEM,18)
   ENDIF
   SELECT 2
   MQTY= T_QTY
   MRATE= PSR_UPRICE
   MSVALUE= PSR_TPRICE
   @ L, 47 SAY MQTY PICTURE '99999.9999'
   @ L, 58 SAY MRATE PICTURE '9999999.99'
   @ L, 68 SAY MSVALUE PICTURE '999999999.99'
   MTOT= MTOT+T_QTY
   MTOT1= MTOT1+PSR_TPRICE
   IF L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      PG= PG+1
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL'
@ L, 45 SAY MTOT PICTURE '9999999.9999'
@ L, 68 SAY MTOT1 PICTURE '999999999.99'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
SET FILTER TO
@ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
WAIT ''
CLEAR
CLOSE ALL
RETURN
**********


****************************
PROCEDURE REP_SAL5
****************************

CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx2
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.D_D1=MD1.and.D_D2=MD2.and.D_D3=MD3.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'SQ'
   @ 23, 50 SAY 'Screen/Quit  -  S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, GTOT, GTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 5
      DO SCREEN IN TBL_PROC.PRG
      @ 3, 28 SAY 'SALE  REPORT  (PARTY WISE)'
      @ 4, 1 SAY 'From  :'
      @ 4, 9 SAY FDATE
      @ 4, 19 SAY 'To :'
      @ 4, 25 SAY TDATE
      @ 5, 1 SAY 'Code  : '
      @ 5, 9 SAY MD1 PICTURE '99'
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 5, 9 SAY MP1
      @ 5, 11 SAY '-'
      @ 5, 12 SAY MD2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 5, 12 SAY MP2
      @ 5, 14 SAY '-'
      @ 5, 15 SAY MD3 PICTURE '999'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 5, 15 SAY MP3
      @ 5, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'Invoice #'
      @ L, 18 SAY 'Item Code'
      @ L, 29 SAY 'Descripition'
      @ L, 47 SAY 'Qty Sold'
      @ L, 60 SAY 'Rate '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 2
   STORE 0 TO MTOT, MTOT1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 5
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 28 SAY 'SALE  REPORT  (PARTY WISE)'
         @ 4, 1 SAY 'From  :'
         @ 4, 9 SAY FDATE
         @ 4, 19 SAY 'To :'
         @ 4, 25 SAY TDATE
         @ 5, 1 SAY 'Code  : '
         @ 5, 9 SAY MD1 PICTURE '99'
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
         @ 5, 9 SAY MP1
         @ 5, 11 SAY '-'
         @ 5, 12 SAY MD2 PICTURE '99'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
         @ 5, 12 SAY MP2
         @ 5, 14 SAY '-'
         @ 5, 15 SAY MD3 PICTURE '999'
         MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
         @ 5, 15 SAY MP3
         @ 5, 24 SAY MTITLE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 10 SAY 'Invoice #'
         @ L, 18 SAY 'Item Code'
         @ L, 29 SAY 'Descripition'
         @ L, 47 SAY 'Qty Sold'
         @ L, 60 SAY 'Rate '
         @ L, 75 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      @ L, 1 SAY DATE
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 10 SAY MDOC
      @ L, 18 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 18 SAY MP1
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 21 SAY MP2
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 24 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 29 SAY ITEM
      ENDIF
      SELECT 2
      MQTY= T_QTY
      MSVALUE= PSR_TPRICE
      @ L, 47 SAY MQTY PICTURE '9999999.9999'
      @ L, 58 SAY PSR_UPRICE PICTURE '9999999.9999'
      @ L, 68 SAY MSVALUE PICTURE '999999999.99'
      MTOT= MTOT+T_QTY
      MTOT1= MTOT1+PSR_TPRICE
      GTOT= GTOT+T_QTY
      GTOT1= GTOT1+PSR_TPRICE
      IF L>=17
         @ 24, 15 SAY ''
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      SKIP
   ENDDO
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
   @ L, 40 SAY 'Total '
   @ L, 47 SAY MTOT PICTURE '9999999.9999'
   @ L, 68 SAY MTOT1 PICTURE '999999999.99'
   L= L+1
   @ L, 40 SAY REPLICATE('-',39)
   L= L+1
ENDDO
@ L, 40 SAY REPLICATE('-',39)
L= L+1
@ L, 40 SAY 'Grand Total '
@ L, 47 SAY GTOT PICTURE '9999999.9999'
@ L, 68 SAY GTOT1 PICTURE '999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',39)
@ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
WAIT ''
CLEAR
CLOSE ALL
RETURN
********


**********************
PROCEDURE REP_SAL7
**********************
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   STORE 0 TO MC1, MC2, MC3, MTP, MTOT, MTQTY
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (PARTY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Party '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MD1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MD2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MD3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 3
   SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY TITLE
   MTITLE= TITLE

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.D_D1=MD1.and.D_D2=MD2.and.D_D3=MD3.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      IF P='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 5 SAY 'SALE REPORT (PARTY WISE SUMMARY)'
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 24 SAY 'SALE REPORT (PARTY WISE SUMMARY)'
      ENDIF
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
      @ 7, 9 SAY MP1
      @ 7, 11 SAY '-'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
      @ 7, 12 SAY MP2
      @ 7, 14 SAY '-'
      MP3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
      @ 7, 15 SAY MP3
      @ 7, 24 SAY MTITLE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Sold '
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+PSR_TPRICE
      MTOT= MTOT+PSR_TPRICE
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 1
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 2
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   @ L, 65 SAY MAMT PICTURE '9999999999.99'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF P='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      PG= PG+1
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
      PG= PG+1
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
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
********

***************************
PROCEDURE REP_SAL8
***************************
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER std_idx
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MI1, MI2, MI3, MTP
   STORE 0 TO MC1, MC2, MC3, MTP, MTOT, MTQTY
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Item   '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 25 SAY '  -  -   '
   @ 14, 25 GET MI1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 14, 25 SAY MPC1
   @ 14, 28 GET MI2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 14, 28 SAY MPC2
   @ 14, 31 GET MI3 PICTURE '@z 9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 14, 31 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 46 SAY ITEM
   MITEM= ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      IF P='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 5 SAY 'SALE REPORT (PARTY WISE SUMMARY)'
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 24 SAY 'SALE REPORT (PARTY WISE SUMMARY)'
      ENDIF
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      @ 7, 9 SAY MPC1
      @ 7, 11 SAY '-'
      @ 7, 12 SAY MPC2
      @ 7, 14 SAY '-'
      @ 7, 15 SAY MPC3
      @ 7, 24 SAY MITEM
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Sold '
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MC1= D_D1
   MC2= D_D2
   MC3= D_D3
   STORE 0 TO MQTY, MAMT
   DO WHILE D_D1=MC1.and.D_D2=MC2.and.D_D3=MC3
      IF D_D1<>MC1.and.D_D2<>MC2.and.D_D3<>MC3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+PSR_TPRICE
      MTOT= MTOT+PSR_TPRICE
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ L, 7 SAY MP3
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
   SELECT 3
   SEEK MCODE
   IF FOUND()
      @ L, 12 SAY TITLE
   ELSE
      @ L, 12 SAY '  '
   ENDIF
   SELECT 2
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   @ L, 65 SAY MAMT PICTURE '9999999999.99'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF P='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      PG= PG+1
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
      PG= PG+1
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
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
PROCEDURE REP-SAL6
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER itemidx1
SELECT 3
USE ldgrcode ORDER ac1
ABC= 0
DO WHILE ABC=0
   SELECT 2
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MI1, MI2, MI3, MTP
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   @ 14, 15 SAY 'Item '

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 14, 24 SAY '  -  -    '
   @ 14, 24 GET MI1 PICTURE '@z 99'

IF _DOS
   SET COLOR TO 
ENDIF


IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 14, 24 SAY MPC1
   @ 14, 27 GET MI2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 14, 27 SAY MPC2
   @ 14, 30 GET MI3 PICTURE '@z 9999'
   READ
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 14, 30 SAY MPC3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF .not. FOUND()
      @ 23, 8 SAY 'This Code Does Not Exists  - Press any key'
      WAIT ''
      @ 23, 8 CLEAR TO 23, 63
      LOOP
   ELSE
      MUNIT= UNIT

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 14, 46 SAY ITEM
      MITEM= ITEM

IF _DOS
      SET COLOR TO 
ENDIF

   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
P= ' '
DO WHILE .not. P$'SQ'
   @ 5, 4 SAY 'Screen/Quit  -  S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
CLEAR
GOTO TOP
SELECT 2
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1, MD1, MD2, MD3
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      DO SCREEN IN TBL_PROC.PRG
      @ 3, 30 SAY 'SALE REPORT (ITEM WISE)'
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ 7, 1 SAY 'Code  : '
      @ 7, 9 SAY MI1 PICTURE '99'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ 7, 9 SAY MPC1
      @ 7, 11 SAY '-'
      @ 7, 12 SAY MI2 PICTURE '99'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ 7, 12 SAY MPC1
      @ 7, 14 SAY '-'
      @ 7, 15 SAY MI3 PICTURE '9999'
      MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ 7, 15 SAY MPC1
      @ 7, 22 SAY MITEM
      @ 7, 53 SAY 'Unit : '+MUNIT
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'Invoice #'
      @ L, 18 SAY 'Party Code'
      @ L, 29 SAY 'Descripition'
      @ L, 58 SAY 'Qty Sold'
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MD1= D_D1
   MD2= D_D2
   MD3= D_D3
   @ L, 1 SAY DATE
   MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
   @ L, 10 SAY MDOC_NO
   @ L, 18 SAY '  -  -   '
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MD1))))+LTRIM(STR(MD1))
   @ L, 18 SAY MPC1
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MD2))))+LTRIM(STR(MD2))
   @ L, 21 SAY MPC2
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MD3))))+LTRIM(STR(MD3))
   @ L, 24 SAY MPC3
   STORE STR(MD1,2)+STR(MD2,2)+STR(MD3,3) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 29 SAY TITLE
   ENDIF
   SELECT 2
   MQTY= T_QTY
   MSVALUE= PSR_TPRICE
   @ L, 54 SAY MQTY PICTURE '9999999.9999'
   @ L, 68 SAY MSVALUE PICTURE '999999999.99'
   MTOT= MTOT+MQTY
   MTOT1= MTOT1+MSVALUE
   IF L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      PG= PG+1
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 40 SAY 'TOTAL'
@ L, 54 SAY MTOT PICTURE '9999999.9999'
@ L, 68 SAY MTOT1 PICTURE '999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
SET FILTER TO
@ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
WAIT ''
CLEAR
CLOSE ALL
RETURN
*********


*********************
PROCEDURE REP_CITY
*********************
CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER cstck
SELECT 3
USE city ORDER city-idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   STORE 0 TO MC1, MC2, MC3, MTP, MTOT, MTQTY
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (CITY WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      IF P='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 5 SAY 'SALE REPORT (CITY WISE SUMMARY)'
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 24 SAY 'SALE REPORT (CITY WISE SUMMARY)'
      ENDIF
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Sold '
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MCITY_CODE= CITY_CODE
   SELECT 3
   SEEK MCITY_CODE
   IF FOUND()
      MLOC= CITY
   ELSE
      MLOC= 'Invalid'
   ENDIF
   @ L, 2 SAY 'City : '+STR(MCITY_CODE,3)+'  '+MLOC
   L= L+1
   MLEN= LEN(RTRIM(MLOC))
   @ L, 2 SAY REPLICATE('-',15+MLEN)
   L= L+1
   SELECT 2
   DO WHILE CITY_CODE=MCITY_CODE
      IF HEADING
         L= 6
         IF P='P'
            DO top_prt IN TBL_PROC.PRG
            @ 3, 1 SAY CHR(14)
            @ 3, 5 SAY 'SALE REPORT (CITY WISE SUMMARY)'
         ELSE
            DO SCREEN IN TBL_PROC.PRG
            @ 3, 24 SAY 'SALE REPORT (CITY WISE SUMMARY)'
         ENDIF
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 12 SAY 'Descripition'
         @ L, 42 SAY 'Unit'
         @ L, 52 SAY 'Qty Sold '
         @ L, 70 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF CITY_CODE<>MCITY_CODE
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      STORE 0 TO MQTY, MAMT
      DO WHILE MCITY_CODE=CITY_CODE.and.MI1=I1.and.MI2=I2.and.MI3=I3
         IF MCITY_CODE<>CITY_CODE.and.MI1<>I1.and.MI2<>I2.and.MI3<>I3
            EXIT
         ENDIF
         MQTY= MQTY+T_QTY
         MAMT= MAMT+PSR_TPRICE
         MTOT= MTOT+PSR_TPRICE
         MTQTY= MTQTY+T_QTY
         IF .not. EOF()
            SKIP
         ENDIF
      ENDDO
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 1 SAY MP1
      @ L, 3 SAY '-'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 4 SAY MP2
      @ L, 6 SAY '-'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 7 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 12 SAY ITEM
         @ L, 42 SAY UNIT
      ENDIF
      SELECT 2
      @ L, 52 SAY MQTY PICTURE '9999999.9999'
      @ L, 65 SAY MAMT PICTURE '9999999999.99'
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      IF P='S'.and.L>21
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF P='P'.and.L>56
         HEADING= .T.
         PG= PG+1
      ENDIF
   ENDDO
   L= L+1
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
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
**********************


***********************
PROCEDURE REP_ZONE
***********************

CLOSE ALL
CLEAR
HEADING= .T.
PG= 1
SELECT 1
USE item ORDER item_idx
SELECT 2
USE stock ORDER zstck
SELECT 3
USE zone
INDEX ON ZONE_CODE TAG zone_idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MD1, MD2, MD3, MTP
   STORE 0 TO MC1, MC2, MC3, MTP, MTOT, MTQTY
   @ 1, 18 TO 3, 53 DOUBLE
   @ 2, 21 SAY 'SALE  REPORT  (ZONE WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 70
      LOOP
   ENDIF
   SELECT 2
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=6
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 2
GOTO TOP
HEADING= .T.
STORE 0 TO MQTY, MSVALUE, MTOT, MTOT1
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      IF P='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 5 SAY 'SALE REPORT (ZONE WISE SUMMARY)'
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 24 SAY 'SALE REPORT (ZONE WISE SUMMARY)'
      ENDIF
      @ 5, 1 SAY 'From  :'
      @ 5, 9 SAY FDATE
      @ 6, 1 SAY 'To    :'
      @ 6, 9 SAY TDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY PG PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Sold '
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MZONE_CODE= ZONE_CODE
   SELECT 3
   SEEK MZONE_CODE
   IF FOUND()
      MLOC= ZONE
   ELSE
      MLOC= 'Invalid'
   ENDIF
   @ L, 2 SAY 'Zone : '+STR(MZONE_CODE,2)+'  '+MLOC
   L= L+1
   MLEN= LEN(RTRIM(MLOC))
   @ L, 2 SAY REPLICATE('-',15+MLEN)
   L= L+1
   SELECT 2
   DO WHILE ZONE_CODE=MZONE_CODE
      IF HEADING
         L= 6
         IF P='P'
            DO top_prt IN TBL_PROC.PRG
            @ 3, 1 SAY CHR(14)
            @ 3, 5 SAY 'SALE REPORT (ZONE WISE SUMMARY)'
         ELSE
            DO SCREEN IN TBL_PROC.PRG
            @ 3, 24 SAY 'SALE REPORT (ZONE WISE SUMMARY)'
         ENDIF
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ L, 66 SAY 'Page No - '
         @ L, 77 SAY PG PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 12 SAY 'Descripition'
         @ L, 42 SAY 'Unit'
         @ L, 52 SAY 'Qty Sold '
         @ L, 70 SAY 'Value'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF ZONE_CODE<>MZONE_CODE
         EXIT
      ENDIF
      MI1= I1
      MI2= I2
      MI3= I3
      STORE 0 TO MQTY, MAMT
      DO WHILE MZONE_CODE=ZONE_CODE.and.MI1=I1.and.MI2=I2.and.MI3=I3
         IF MZONE_CODE<>ZONE_CODE.and.MI1<>I1.and.MI2<>I2.and.MI3<>I3
            EXIT
         ENDIF
         MQTY= MQTY+T_QTY
         MAMT= MAMT+PSR_TPRICE
         MTOT= MTOT+PSR_TPRICE
         MTQTY= MTQTY+T_QTY
         IF .not. EOF()
            SKIP
         ENDIF
      ENDDO
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ L, 1 SAY MP1
      @ L, 3 SAY '-'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ L, 4 SAY MP2
      @ L, 6 SAY '-'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ L, 7 SAY MP3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SELECT 1
      SEEK MI_CODE
      IF FOUND()
         @ L, 12 SAY ITEM
         @ L, 42 SAY UNIT
      ENDIF
      SELECT 2
      @ L, 52 SAY MQTY PICTURE '9999999.9999'
      @ L, 65 SAY MAMT PICTURE '9999999999.99'
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      IF P='S'.and.L>21
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         HEADING= .T.
         PG= PG+1
      ENDIF
      IF P='P'.and.L>56
         HEADING= .T.
         PG= PG+1
      ENDIF
   ENDDO
   L= L+1
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
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
*********

**************************
PROCEDURE REP_MEN4
**************************

DO WHILE .T.
   CLEAR

IF _DOS
   SET COLOR TO 7/1
ENDIF

   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 26 SAY ' Current Status Reports  '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 8, 22 PROMPT ' 1 - Present Stock Status Report          '
   @ 9, 22 PROMPT ' 2 - Present Stock Status (Location Wise) '
   @ 10, 22 PROMPT ' 3 - Present Stock Status (Item Wise)     '
   @ 11, 22 PROMPT ' 4 - Present Stock Status (Location & Item Wise) '
   @ 12, 22 PROMPT ' 5 - Stock Ledger Display                 '
   @ 13, 22 PROMPT ' 6 - Stock Ledger Printing                '
   @ 14, 22 PROMPT ' 7 - Stock Below Minimum Level            '
   @ 15, 22 PROMPT ' 8 - Stock Below Reording Level           '
   @ 16, 22 PROMPT ' 9 - Stock Above Maximum Level            '
   @ 18, 22 PROMPT '10 - Return to Main Menu                  '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep_stat
      CASE MCHOICE=2
         DO stat_loc
      CASE MCHOICE=3
         DO stat_itm
      CASE MCHOICE=4
         DO statitem
      CASE MCHOICE=5
         DO rep_ledg
      CASE MCHOICE=6
         DO repledg
      CASE MCHOICE=7
         DO rep_min
      CASE MCHOICE=8
         DO re_ord
      CASE MCHOICE=9
         DO rep_max
      CASE MCHOICE=10
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN
**********

***********************
PROCEDURE REP_STAT
***********************

CLEAR
CLOSE DATA
HEADING= .T.
P= 1
SELECT 1
USE item-bal ORDER itm_bal
SELECT 2
USE item ORDER item_idx
SELECT 3
USE loc ORDER loc_idx
CLEAR
NO= ' '
DO WHILE .not. NO$'YN'

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 2, 2 SAY 'Present Stock Status Printing/Display :'
   @ 3, 2 SAY 'Do You Want Printing/Display With Average Price - Y/N ' GET NO FUNCTION '!'
   READ

IF _DOS
   SET COLOR TO 7/1
ENDIF

ENDDO
ABC= 0
DO WHILE ABC=0
   SELECT 1
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO FITEM_CODE, EITEM_CODE, MLOC_CODE, MAMT, MI1, MI2, MI3, MFI1, MFI2, MFI3
   STORE DATE() TO MDATE
   @ 10, 15 SAY 'Location Code ' GET MLOC_CODE PICTURE '9999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 77
   SELECT 3
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 40 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   @ 12, 15 SAY 'From Item No '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 29 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 29 GET MFI1 PICTURE '99'
   READ
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 29 SAY MP1
   @ 12, 31 SAY '-'
   @ 12, 32 GET MFI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 32 SAY MP2
   @ 12, 34 SAY '-'
   @ 12, 35 GET MFI3 PICTURE '9999'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 35 SAY MP3
   READ

IF _DOS
   SET COLOR TO 
ENDIF

   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO FITEM_CODE
   SELECT 2
   SEEK FITEM_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

* updated on 12-12-2019

   @ 12, 40 SAY ITEM SIZE 1,30

IF _DOS
   SET COLOR TO 
ENDIF

   MI1= MFI1
   MI2= MFI2
   SELECT 1
   @ 14, 15 SAY 'To Item No '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 29 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 14, 29 SAY MI1 PICTURE '99'
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 29 SAY MP1
   @ 14, 31 SAY '-'
   @ 14, 32 SAY MI2 PICTURE '99'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 32 SAY MP2
   @ 14, 34 SAY '-'
   @ 14, 35 GET MI3 PICTURE '9999'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 35 SAY MP3
   READ

IF _DOS
   SET COLOR TO 
ENDIF

   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO EITEM_CODE
   SELECT 2
   SEEK EITEM_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

* updated on 12-12-2019

   @ 14, 40 SAY ITEM SIZE 1,30

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   COPY TO abc FOR LOC_CODE=MLOC_CODE.and.I1=MFI1.and.I2=MFI2.and.I3>=MFI3.and.I3<=MI3
   SELECT 1
   USE abc
   COUNT TO RCON
   IF RCON=0
      @ 23, 15 SAY 'No Codes exist in this range - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='S'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
L= 5
SELECT 1
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY ' PRESENT STOCK STATUS REPORT  '
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 3, 28 SAY 'PRESENT STOCK STATUS REPORT  '
      ENDIF
      @ L, 1 SAY 'Location : '+MLOC
      L= L+1
      @ L, 1 SAY 'As On    : '
      @ L, 12 SAY MDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      @ L, 1 SAY 'Code'
      @ L, 12 SAY 'Item'
      @ L, 40 SAY 'Unit'
      IF NO='Y'
         @ L, 47 SAY 'P.Qty'
         @ L, 57 SAY 'Avg Price '
         @ L, 75 SAY 'Value'
      ELSE
         @ L, 60 SAY 'P.Qty'
      ENDIF
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1 PICTURE '99'
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2 PICTURE '99'
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 2
   SEEK MI_CODE

   IF FOUND()
* updated on 12-12-2019
      @ L, 12 SAY ITEM SIZE 1,30
      @ L, 40 SAY UNIT
   ENDIF

   SELECT 1

* Updated on 12-12-2019

   IF NO='Y'
      @ L, 46 SAY QTY PICTURE '9999999.99'
      @ L, 59 SAY AVG_PRICE PICTURE '99999999'
      MTOT= 0
      MTOT= QTY*AVG_PRICE
      @ L, 68 SAY MTOT PICTURE '999999999.99'
      MAMT= MAMT+MTOT
   ELSE
      @ L, 58 SAY QTY PICTURE '9999999.99'
   ENDIF
   IF ANS='P'.and.L>=56
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   IF ANS='S'.and.L>=22
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   SELECT 1
   L= L+1
   SKIP
ENDDO
IF NO='Y'
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   @ L, 50 SAY 'T O T A L '
   @ L, 65 SAY MAMT PICTURE '999999999999.99'
ENDIF
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RUN del abc.dbf
CLOSE ALL
RETURN
********


**********************
PROCEDURE STAT_LOC
**********************

CLEAR
CLOSE ALL
HEADING= .T.
P= 1
SELECT 1
USE item-bal ORDER itm_bal
SELECT 2
USE item ORDER item_idx
SELECT 3
USE loc ORDER loc_idx
CLEAR
ABC= 0
DO WHILE ABC=0
   SELECT 1
   NO= ' '
   DO WHILE .not. NO$'YN'

IF _DOS
      SET COLOR TO +7/1
ENDIF

      @ 2, 2 SAY 'Present Stock Status Printing/Display :'
      @ 3, 2 SAY 'Do You Want Printing/Display With Average Price - Y/N ' GET NO FUNCTION '!'
      READ

IF _DOS
      SET COLOR TO 7/1
ENDIF

   ENDDO
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO MLOC_CODE, MAMT
   STORE DATE() TO MDATE
   @ 10, 15 SAY 'Location Code ' GET MLOC_CODE PICTURE '9999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 77
   SELECT 3
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 40 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   COPY TO abc FOR LOC_CODE=MLOC_CODE
   SELECT 1
   USE abc
   COUNT TO RCON
   IF RCON=0
      @ 23, 15 SAY 'No Codes exist in this Location - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='S'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
L= 5
SELECT 1
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY ' PRESENT STOCK STATUS REPORT  '
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 3, 28 SAY 'PRESENT STOCK STATUS REPORT  '
      ENDIF
      @ L, 1 SAY 'Location : '+MLOC
      L= L+1
      @ L, 1 SAY 'As On    : '
      @ L, 12 SAY MDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      @ L, 1 SAY 'Code'
      @ L, 12 SAY 'Item'
      @ L, 40 SAY 'Unit'
      @ L, 47 SAY 'P.Qty'
      IF NO='Y'
         @ L, 57 SAY 'Avg Price '
         @ L, 75 SAY 'Value'
      ENDIF
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1 PICTURE '99'
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2 PICTURE '99'
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 2
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 40 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 46 SAY QTY PICTURE '9999999.9999'
   IF NO='Y'
      @ L, 59 SAY AVG_PRICE PICTURE '999999.99'
      MTOT= 0
      MTOT= QTY*AVG_PRICE
      @ L, 68 SAY MTOT PICTURE '999999999.99'
      MAMT= MAMT+MTOT
   ENDIF
   IF ANS='P'.and.L>=56
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   IF ANS='S'.and.L>=22
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   SELECT 1
   L= L+1
   SKIP
ENDDO
IF NO='Y'
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   @ L, 50 SAY 'T O T A L '
   @ L, 65 SAY MAMT PICTURE '999999999999.99'
ENDIF
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RUN del abc.dbf
CLOSE ALL
RETURN
**************


**********************
PROCEDURE STAT_ITM
**********************
CLEAR
CLOSE ALL
HEADING= .T.
P= 1
SELECT 1
USE item-bal ORDER itm_bal
SELECT 2
USE item ORDER item_idx
SELECT 3
USE loc ORDER loc_idx
ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 5, 15 SAY ' PRESENT STOCK STATUS REPORT (ITEM WISE)'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO MI1, MI2, MI3, TOT, MAMT
   @ 12, 15 SAY 'From Item No '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 29 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 29 GET MI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 29 SAY MP1
   @ 12, 31 SAY '-'
   @ 12, 32 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 32 SAY MP2
   @ 12, 34 SAY '-'
   @ 12, 35 GET MI3 PICTURE '9999'
   READ
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 35 SAY MP3

IF _DOS
   SET COLOR TO 
ENDIF

   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO FITEM_CODE
   SELECT 2
   SEEK FITEM_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 40 SAY ITEM
   MITEM= ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO I1=MI1.and.I2=MI2.and.I3=MI3
   COUNT TO RCON
   IF RCON=0
      @ 23, 15 SAY 'No Codes exist in this range - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='S'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 1
DO WHILE .not. EOF()
   IF HEADING
      L= 5
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 3 SAY 'PRESENT STOCK STATUS REPORT (ITEM WISE)'
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 3, 25 SAY 'PRESENT STOCK STATUS REPORT (ITEM WISE) '
      ENDIF
      @ L, 2 SAY 'Item : '
      @ L, 9 SAY MP1 PICTURE '99'
      @ L, 11 SAY '-'
      @ L, 12 SAY MP2 PICTURE '99'
      @ L, 14 SAY '-'
      @ L, 15 SAY MP3 PICTURE '9999'
      @ L, 20 SAY MITEM
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      @ L, 1 SAY 'Code'
      @ L, 9 SAY 'Location '
      @ L, 40 SAY 'P.Qty'
      @ L, 57 SAY 'Avg Price '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MLOC_CODE= LOC_CODE
   MLC= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
   @ L, 1 SAY MLC PICTURE '9999'
   SELECT 3
   SEEK MLOC_CODE
   IF FOUND()
      MLOC= LOC
   ELSE
      MLOC= ' '
   ENDIF
   SELECT 1
   @ L, 9 SAY MLOC
   @ L, 40 SAY QTY PICTURE '9999999.9999'
   @ L, 57 SAY AVG_PRICE PICTURE '9999.9999'
   MTOT= 0
   MTOT= QTY*AVG_PRICE
   @ L, 68 SAY MTOT PICTURE '999999999.99'
   MAMT= MAMT+MTOT
   IF ANS='P'.and.L>=56
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   IF ANS='S'.and.L>=22
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   SELECT 1
   L= L+1
   SKIP
ENDDO
@ L, 50 SAY REPLICATE('-',30)
L= L+1
@ L, 50 SAY 'T O T A L '
@ L, 65 SAY MAMT PICTURE '999999999999.99'
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
CLOSE ALL
RETURN
***************


**********************
PROCEDURE STATITEM
***********************

CLEAR
CLOSE ALL
HEADING= .T.
P= 1
SELECT 1
USE item-bal ORDER itm_bal
SELECT 2
USE item ORDER item_idx
SELECT 3
USE loc ORDER loc_idx
CLEAR
ABC= 0
DO WHILE ABC=0
   SELECT 1
   @ 1, 25 TO 3, 56 DOUBLE
   @ 2, 27 SAY ' PRESENT STOCK STATUS REPORT'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO FITEM_CODE, EITEM_CODE, MLOC_CODE, MAMT, MI1, MI2, MI3, MFI1, MFI2, MFI3
   STORE DATE() TO MDATE
   @ 10, 15 SAY 'Location Code ' GET MLOC_CODE PICTURE '9999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 77
   SELECT 3
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 40 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 
ENDIF

   ABC= 1
ENDDO
@ 12, 15 SAY 'Item Code '

IF _DOS
SET COLOR TO /w
ENDIF

@ 12, 30 SAY '  -  '

IF _DOS
SET COLOR TO 7/1 
ENDIF

@ 12, 30 GET MI1 PICTURE '99'
READ
IF READKEY()=12
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
@ 23, 45 CLEAR TO 23, 78
MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
SET COLOR TO /w
ENDIF

@ 12, 30 SAY MP1
@ 12, 32 SAY '-'
@ 12, 33 GET MI2 PICTURE '99'
READ
MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
SET COLOR TO /w
ENDIF

@ 12, 33 SAY MP2

IF _DOS
SET COLOR TO 
ENDIF

SELECT 1
SET FILTER TO LOC_CODE=MLOC_CODE.and.I1=MI1.and.I2=MI2
COUNT TO RCON
IF RCON=0
   CLEAR
   @ 12, 12 SAY 'No Codes in Code File  -  Press any key to retu to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
SELECT 1
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='S'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
L= 5
SELECT 1
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY ' PRESENT STOCK STATUS REPORT  '
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 3, 28 SAY 'PRESENT STOCK STATUS REPORT  '
      ENDIF
      @ L, 1 SAY 'Location : '+MLOC
      L= L+1
      @ L, 1 SAY 'As On    : '
      @ L, 12 SAY MDATE
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      @ L, 1 SAY 'Code'
      @ L, 12 SAY 'Item'
      @ L, 40 SAY 'Unit'
      @ L, 47 SAY 'P.Qty'
      @ L, 57 SAY 'Avg Price '
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 0 SAY REPLICATE('-',80)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1 PICTURE '99'
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2 PICTURE '99'
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 2
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 40 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 46 SAY QTY PICTURE '9999999.9999'
   @ L, 59 SAY AVG_PRICE PICTURE '9999.9999'
   MTOT= 0
   MTOT= QTY*AVG_PRICE
   @ L, 68 SAY MTOT PICTURE '999999999.99'
   MAMT= MAMT+MTOT
   IF ANS='P'.and.L>=56
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   IF ANS='S'.and.L>=22
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      L= 5
      P= P+1
   ENDIF
   SELECT 1
   L= L+1
   SKIP
ENDDO
@ L, 50 SAY REPLICATE('-',30)
L= L+1
@ L, 50 SAY 'T O T A L '
@ L, 65 SAY MAMT PICTURE '999999999999.99'
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RUN del abc.dbf
CLOSE ALL
RETURN
*******

***************************
PROCEDURE REP_LEDG
**********************
CLEAR
CLEAR ALL
SET DATE brit
SELECT 1
USE stock
SELECT 2
USE ldgrcode ORDER ac1
SELECT 3
USE dept EXCL
INDEX ON dept_code TAG dept_idx
SET ORDER TO dept_idx
SELECT 4
USE item ORDER item_idx
SELECT 5
USE loc ORDER loc_idx
DO WHILE .T.
   SELECT 1
   CLEAR
   @ 2, 20 SAY '     STOCK  LEGDER  REPORT '
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MC1, MC2, MC3, MD1, MD2, MD3, MI1, MI2, MI3, MLOC_CODE
   @ 8, 20 SAY 'From Date    (dd/mm/yy) :' GET FDATE
   @ 10, 20 SAY 'To   Date    (dd/mm/yy) :' GET TDATE
   READ
   IF TDATE<FDATE
      @ 23, 25 SAY 'To Date Should be greater than From Date'
      WAIT ''
      @ 23, 25 SAY '                                        '
      LOOP
   ENDIF
   @ 23, 55 SAY 'Press Ctrl Q To Quit '
   @ 12, 20 SAY 'Item  '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 28 SAY '  -  -    '
   @ 12, 28 GET MI1 PICTURE '99'
   READ
   MCP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 12, 28 SAY MCP1
   @ 12, 30 SAY '-'
   @ 12, 31 GET MI2 PICTURE '99'
   READ
   MCP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 12, 31 SAY MCP2
   @ 12, 33 SAY '-'
   @ 12, 34 GET MI3 PICTURE '9999'
   READ
   MCP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 12, 34 SAY MCP3

IF _DOS
   SET COLOR TO 
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   @ 23, 50 CLEAR TO 23, 78
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 4
   SEEK MI_CODE
   IF .not. FOUND()
      @ 23, 25 SAY 'Item Code Does not Exist --- Re-try '
      WAIT ''
      @ 23, 23 CLEAR TO 23, 78
      LOOP
   ENDIF
   MITEM= ITEM

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 40 SAY ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   MT= 0
   DO WHILE MT=0
      @ 14, 20 SAY 'Location ' GET MLOC_CODE PICTURE '9999'
      READ
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         @ 23, 25 SAY 'Location Code Does not Exist --- Re-try '
         WAIT ''
         @ 23, 23 CLEAR TO 23, 78
         LOOP
      ENDIF
      MLOC= LOC

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 14, 40 SAY MLOC

IF _DOS
      SET COLOR TO 
ENDIF

      MT= 1
   ENDDO
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.LOC_CODE=MLOC_CODE
   GOTO TOP
   P= ' '
   DO WHILE .not. P$'S/P/Q'
      @ 23, 40 SAY 'Screen/Printing/Quit  -  S/P/Q  ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   IF P='P'
      @ 11, 20 SAY 'Please wait ... Printing in progress '
      SET PRINTER ON
      SET DEVICE TO PRINTER
   ENDIF
   CLEAR
   L= 3
   HEADING= .T.
   PAGE= 0
   STORE 0 TO MTOT, MTOT1, MBAL
   SELECT 1
   DO WHILE .not. EOF()
      IF HEADING
         PAGE= PAGE+1
         IF P='P'
            DO top_prt IN TBL_PROC.PRG
         ELSE
            DO screen IN TBL_PROC.PRG
         ENDIF
         @ 3, 25 SAY '  STOCK  LEDGER  DISPLAY'
         @ 4, 1 SAY 'From Date : '+DTOC(FDATE)
         @ 5, 1 SAY 'To   Date : '+DTOC(TDATE)
         @ 6, 1 SAY 'Item      : '
         MCP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ 6, 13 SAY MCP1
         @ 6, 15 SAY '-'
         MCP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ 6, 16 SAY MCP2
         @ 6, 18 SAY '-'
         MCP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ 6, 19 SAY MCP3
         @ 6, 26 SAY MITEM
         @ 7, 1 SAY 'Location  : '+STR(MLOC_CODE,4)
         @ 7, 26 SAY MLOC
         @ 7, 68 SAY 'Page : '+STR(PAGE,2)
         @ 8, 1 SAY REPLICATE('-',79)
         @ 9, 1 SAY 'Date'
         @ 9, 10 SAY 'Doc #'
         @ 9, 20 SAY 'Description'
         @ 9, 38 SAY 'Inward'
         @ 9, 49 SAY 'Outward'
         @ 9, 61 SAY 'Balance'
         @ 9, 73 SAY 'Average'
         @ 10, 1 SAY REPLICATE('-',79)
         L= 11
         HEADING= .F.
      ENDIF
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      MD1= D_D1
      MD2= D_D2
      MD3= D_D3
      MDEPT_CODE= DEPT_CODE
      @ L, 1 SAY DATE
      T= '    '
      IF DOC_TYPE=1
         T= 'GRN-'
      ENDIF
      IF DOC_TYPE=2
         T= 'PRN-'
      ENDIF
      IF DOC_TYPE=3
         T= 'IPN-'
      ENDIF
      IF DOC_TYPE=4
         T= 'IPR-'
      ENDIF
      IF DOC_TYPE=5
         T= 'PRD-'
      ENDIF
      IF DOC_TYPE=6
         T= 'INV-'
      ENDIF
      IF DOC_TYPE=7
         T= 'SRN-'
      ENDIF
      IF DOC_TYPE=8
         T= 'TRN-'
      ENDIF
      IF SR_NO=1
         @ L, 22 SAY '*** Opening Balance ***'
      ENDIF
      IF SR_NO<>1
         MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
         @ L, 10 SAY RTRIM(T)+MDOC_NO
         IF DOC_TYPE=8
            MF_T= F_T
            IF MF_T='F'
               MDES= 'From Location '
            ELSE
               MDES= 'To Location   '
            ENDIF
            @ L, 22 SAY MDES
         ENDIF
         IF DOC_TYPE=1.or.DOC_TYPE=7
            SELECT 2
            SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
            IF FOUND()
               MTITLE= LEFT(TITLE,15)
               @ L, 22 SAY MTITLE
            ENDIF
         ENDIF
         SELECT 1
         IF DOC_TYPE=2.or.DOC_TYPE=6
            SELECT 2
            SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
            IF FOUND()
               MTITLE= LEFT(TITLE,15)
               @ L, 22 SAY MTITLE
            ENDIF
         ENDIF
         SELECT 1
         IF DOC_TYPE=3.or.DOC_TYPE=4.or.DOC_TYPE=5
            SELECT 3
            SEEK MDEPT_CODE
            IF FOUND()
               MDEPT= LEFT(DEPT,15)
               @ L, 22 SAY MDEPT
            ENDIF
         ENDIF
         SELECT 1
         IF DOC_TYPE=8
            IF MF_T='T'
               @ L, 36 SAY T_QTY
               MTOT= MTOT+T_QTY
            ELSE
               @ L, 44 SAY T_QTY
               MTOT1= MTOT1+T_QTY
            ENDIF
         ENDIF
         IF DOC_TYPE=1.or.DOC_TYPE=4.or.DOC_TYPE=5.or.DOC_TYPE=7
            @ L, 36 SAY T_QTY
            MTOT= MTOT+T_QTY
         ENDIF
         IF DOC_TYPE=2.or.DOC_TYPE=3.or.DOC_TYPE=6
            @ L, 44 SAY T_QTY
            MTOT1= MTOT1+T_QTY
         ENDIF
      ENDIF
      @ L, 58 SAY QTY_BAL PICTURE '9999999.99'
      @ L, 70 SAY AVG_PRICE PICTURE '9999999.99'
      L= L+1
      SKIP
      IF P='P'.and.L>56
         HEADING= .T.
      ENDIF
      IF P='S'.and.L>21
         @ 24, 15 SAY ''
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         L= 3
         HEADING= .T.
      ENDIF
      IF EOF()
         EXIT
      ENDIF
   ENDDO
   @ L, 30 SAY REPLICATE('-',40)
   L= L+1
   @ L, 36 SAY MTOT PICTURE '99999999'
   @ L, 44 SAY MTOT1 PICTURE '99999999'
   IF P='S'
      @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
      WAIT ''
   ENDIF
   IF P='P'
      EJECT
      SET PRINTER OFF
      SET DEVICE TO SCREEN
   ENDIF
   SELECT 1
   SET FILTER TO
ENDDO
CLEAR
CLOSE ALL
RETURN
********************

**********************
PROCEDURE REPLEDG
**********************

CLEAR
CLEAR ALL
SET DATE brit
SELECT 1
USE stock
SELECT 2
USE ldgrcode ORDER ac1
SELECT 3
USE dept ORDER dept_idx
SELECT 4
USE item ORDER item_idx
SELECT 5
USE loc ORDER loc_idx
DO WHILE .T.
   CLEAR
   SELECT 1
   @ 2, 20 SAY ' COMPLETE STOCK LEGDER REPORT '
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MC1, MC2, MC3, MD1, MD2, MD3, MI1, MI2, MI3, MLOC_CODE
   @ 8, 20 SAY 'From Date    (dd/mm/yy) :' GET FDATE
   @ 10, 20 SAY 'To   Date    (dd/mm/yy) :' GET TDATE
   READ
   IF TDATE<FDATE
      @ 23, 25 SAY 'To Date Should be greater than From Date'
      WAIT ''
      @ 23, 25 SAY '                                        '
      LOOP
   ENDIF
   @ 23, 55 SAY 'Press Ctrl Q To Quit '
   @ 12, 20 SAY 'Item  '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 28 SAY '  -  -    '
   @ 12, 28 GET MI1 PICTURE '99'
   READ
   MCP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ 12, 28 SAY MCP1
   @ 12, 30 SAY '-'
   @ 12, 31 GET MI2 PICTURE '99'
   READ
   MCP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ 12, 31 SAY MCP2
   @ 12, 33 SAY '-'
   @ 12, 34 GET MI3 PICTURE '9999'
   READ
   MCP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 12, 34 SAY MCP3

IF _DOS
   SET COLOR TO 
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   @ 23, 50 CLEAR TO 23, 78
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 4
   SEEK MI_CODE
   IF .not. FOUND()
      @ 23, 25 SAY 'Item Code Does not Exist --- Re-try '
      WAIT ''
      @ 23, 23 CLEAR TO 23, 78
      LOOP
   ENDIF
   MITEM= ITEM

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 40 SAY ITEM
   MUNIT= UNIT

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   MT= 0
   DO WHILE MT=0
      @ 14, 20 SAY 'Location ' GET MLOC_CODE PICTURE '9999'
      READ
      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         @ 23, 25 SAY 'Location Code Does not Exist --- Re-try '
         WAIT ''
         @ 23, 23 CLEAR TO 23, 78
         LOOP
      ENDIF
      MLOC= LOC

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 14, 40 SAY MLOC

IF _DOS
      SET COLOR TO 
ENDIF

      MT= 1
   ENDDO
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.LOC_CODE=MLOC_CODE
   GOTO TOP
   P= ' '
   DO WHILE .not. P$'PQ'
      @ 23, 40 SAY 'Printing/Quit  -  P/Q  ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   IF P='P'
      @ 11, 20 SAY 'Please wait ... Printing in progress '
      SET PRINTER ON
      SET DEVICE TO PRINTER
   ENDIF
   CLEAR
   L= 3
   HEADING= .T.
   PAGE= 0
   STORE 0 TO MTOT, MTOT1, MBAL
   SELECT 1
   DO WHILE .not. EOF()
      IF HEADING
         PAGE= PAGE+1
         DO top_prt IN TBL_PROC.PRG
         @ 3, 25 SAY '  STOCK  LEDGER  DISPLAY'
         @ 4, 1 SAY 'From Date : '+DTOC(FDATE)
         @ 5, 1 SAY 'To   Date : '+DTOC(TDATE)
         @ 6, 1 SAY 'Item      : '
         MCP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ 6, 13 SAY MCP1
         @ 6, 15 SAY '-'
         MCP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ 6, 16 SAY MCP2
         @ 6, 18 SAY '-'
         MCP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ 6, 19 SAY MCP3
         @ 6, 26 SAY MITEM
         @ 6, 60 SAY 'Unit : '
         @ 6, 67 SAY MUNIT
         @ 7, 1 SAY 'Location  : '+STR(MLOC_CODE,4)
         @ 7, 26 SAY MLOC
         @ 7, 68 SAY 'Page : '+STR(PAGE,2)
         L= 8
         @ L, 0 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 1 SAY 'Date'
         @ L, 9 SAY 'Doc #'
         @ L, 22 SAY 'Description'
         @ L, 65 SAY 'Inward'
         @ L, 75 SAY 'Outward'
         @ L, 90 SAY 'Qty Bal'
         @ L, 104 SAY 'Average'
         @ L, 117 SAY 'Value (Rs.)'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      MD1= D_D1
      MD2= D_D2
      MD3= D_D3
      MDEPT_CODE= DEPT_CODE
      @ L, 1 SAY DATE
      T= '    '
      IF DOC_TYPE=1
         T= 'GRN-'
      ENDIF
      IF DOC_TYPE=2
         T= 'PRN-'
      ENDIF
      IF DOC_TYPE=3
         T= 'IPN-'
      ENDIF
      IF DOC_TYPE=4
         T= 'IPR-'
      ENDIF
      IF DOC_TYPE=5
         T= 'PRD-'
      ENDIF
      IF DOC_TYPE=6
         T= 'INV-'
      ENDIF
      IF DOC_TYPE=7
         T= 'SRN-'
      ENDIF
      IF DOC_TYPE=8
         T= 'TRN-'
      ENDIF
      IF SR_NO=1
         @ L, 22 SAY '*** Opening Balance ***'
      ENDIF
      IF SR_NO<>1
         MDOC_NO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
         @ L, 10 SAY RTRIM(T)+MDOC_NO
         IF DOC_TYPE=8
            MF_T= F_T
            IF MF_T='F'
               MDES= 'From Location '
            ELSE
               MDES= 'To Location   '
            ENDIF
            @ L, 22 SAY MDES
         ENDIF
         IF DOC_TYPE=1.or.DOC_TYPE=7
            SELECT 2
            SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
            IF FOUND()
               MTITLE= LEFT(TITLE,35)
               @ L, 22 SAY MTITLE
            ENDIF
         ENDIF
         SELECT 1
         IF DOC_TYPE=2.or.DOC_TYPE=6
            SELECT 2
            SEEK STR(MD1,2)+STR(MD2,2)+STR(MD3,3)
            IF FOUND()
               MTITLE= LEFT(TITLE,35)
               @ L, 22 SAY MTITLE
            ENDIF
         ENDIF
         SELECT 1
         IF DOC_TYPE=3.or.DOC_TYPE=4.or.DOC_TYPE=5
            SELECT 3
            SEEK MDEPT_CODE
            IF FOUND()
               MDEPT= LEFT(DEPT,15)
               @ L, 22 SAY MDEPT
            ENDIF
         ENDIF
         SELECT 1
         IF DOC_TYPE=8
            IF MF_T='T'
               @ L, 62 SAY T_QTY
               MTOT= MTOT+T_QTY
            ELSE
               @ L, 72 SAY T_QTY
               MTOT1= MTOT1+T_QTY
            ENDIF
         ENDIF
         IF DOC_TYPE=1.or.DOC_TYPE=4.or.DOC_TYPE=5.or.DOC_TYPE=7
            @ L, 62 SAY T_QTY
            MTOT= MTOT+T_QTY
         ENDIF
         IF DOC_TYPE=2.or.DOC_TYPE=3.or.DOC_TYPE=6
            @ L, 72 SAY T_QTY
            MTOT1= MTOT1+T_QTY
         ENDIF
      ENDIF
      @ L, 90 SAY QTY_BAL PICTURE '9999999.99'
      @ L, 105 SAY AVG_PRICE PICTURE '9999999.99'
      MVAL= 0
      MVAL= QTY_BAL*AVG_PRICE
      @ L, 119 SAY MVAL PICTURE '9999999.99'
      L= L+1
      SKIP
      IF P='P'.and.L>56
         @ 0, 0 SAY CHR(18)
         HEADING= .T.
      ENDIF
      IF EOF()
         EXIT
      ENDIF
   ENDDO
   @ L, 60 SAY REPLICATE('-',30)
   L= L+1
   @ L, 63 SAY MTOT PICTURE '99999999'
   @ L, 73 SAY MTOT1 PICTURE '99999999'
   L= L+1
   @ L, 60 SAY REPLICATE('-',30)
   EJECT
   @ 0, 0 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
   SELECT 1
   SET FILTER TO
ENDDO
CLEAR
CLOSE ALL
RETURN
**********


**********************
PROCEDURE REP_MIN
**********************

CLEAR
HEADING= .T.
P= 1
SELECT 1
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 2
USE item-bal
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4)+STR(LOC_CODE,4) TAG open_idx
SELECT 3
USE loc
INDEX ON LOC_CODE TAG loc_idx
CLEAR
ABC= 0
DO WHILE ABC=0
   @ 1, 25 TO 3, 55 DOUBLE
   @ 2, 27 SAY ' STOCK BELOW MINIMUM LEVEL'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO FITEM_CODE, EITEM_CODE, MLOC_CODE, MI1, MI2, MI3
   STORE DATE() TO MDATE
   @ 10, 15 SAY 'Location Code ' GET MLOC_CODE PICTURE '9999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 77
   SELECT 3
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 40 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 15 SAY "Enter Today's Date      " GET MDATE
   READ
   SELECT 2
   SET FILTER TO LOC_CODE=MLOC_CODE
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist For This Location - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
DO WHILE .not. EOF()
   IF HEADING
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(14)
      @ 3, 10 SAY 'STOCK BELOW MINIMUM LEVEL'
      @ 5, 1 SAY 'Location : '+MLOC
      @ L, 0 SAY CHR(15)
      @ L, 3 SAY 'As On           : '
      @ L, 22 SAY MDATE
      @ L, 120 SAY 'Page No -'
      @ L, 130 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Code'
      @ L, 13 SAY 'Item'
      @ L, 53 SAY 'Unit'
      @ L, 61 SAY 'Min'
      @ L, 71 SAY 'Max'
      @ L, 84 SAY 'Present'
      @ L, 95 SAY 'Reorder'
      @ L, 105 SAY 'Reorder'
      @ L, 117 SAY 'Last'
      L= L+1
      @ L, 61 SAY 'Level'
      @ L, 71 SAY 'Level'
      @ L, 84 SAY 'Qty'
      @ L, 95 SAY 'Level'
      @ L, 105 SAY 'Qty'
      @ L, 117 SAY 'Purchase Price'
      L= L+1
      @ L, 0 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MQTY= QTY
   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF FOUND()
      IF MQTY<MIN_LEV
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ L, 1 SAY MI1 PICTURE '99'
         @ L, 3 SAY '-'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ L, 4 SAY MI2 PICTURE '99'
         @ L, 6 SAY '-'
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ L, 7 SAY MI3 PICTURE '9999'
         @ L, 13 SAY ITEM
         @ L, 53 SAY UNIT
         @ L, 61 SAY MIN_LEV PICTURE '99999'
         @ L, 71 SAY MAX_LEV PICTURE '99999'
         @ L, 82 SAY MQTY PICTURE '999999.99'
         @ L, 96 SAY REORD_LEV PICTURE '99999'
         @ L, 106 SAY REORD_QTY PICTURE '99999'
         @ L, 122 SAY LAST_PRICE PICTURE '9999.99'
      ELSE
         SELECT 2
         IF .not. EOF()
            SKIP
         ENDIF
         LOOP
      ENDIF
   ENDIF
   IF L>=56
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      P= P+1
      L= 1
   ENDIF
   SELECT 2
   L= L+1
   IF .not. EOF()
      SKIP
   ENDIF
ENDDO
EJECT
@ 0, 0 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
*******


*********************
PROCEDURE RE_ORD
*********************
CLEAR
HEADING= .T.
P= 1
SELECT 1
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 2
USE item-bal
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4)+STR(LOC_CODE,4) TAG open_idx
SELECT 3
USE loc
INDEX ON LOC_CODE TAG loc_idx
CLEAR
ABC= 0
DO WHILE ABC=0
   @ 1, 25 TO 3, 55 DOUBLE
   @ 2, 27 SAY ' STOCK BELOW REORDING LEVEL'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO FITEM_CODE, EITEM_CODE, MLOC_CODE, MI1, MI2, MI3
   STORE DATE() TO MDATE
   @ 10, 15 SAY 'Location Code ' GET MLOC_CODE PICTURE '9999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 77
   SELECT 3
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 40 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 15 SAY "Enter Today's Date      " GET MDATE
   READ
   SELECT 2
   SET FILTER TO LOC_CODE=MLOC_CODE
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist For This Location - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
DO WHILE .not. EOF()
   IF HEADING
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(14)
      @ 3, 10 SAY 'STOCK BELOW REORDING LEVEL'
      @ 5, 1 SAY 'Location : '+MLOC
      @ L, 0 SAY CHR(15)
      @ L, 3 SAY 'As On           : '
      @ L, 22 SAY MDATE
      @ L, 120 SAY 'Page No -'
      @ L, 130 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Code '
      @ L, 13 SAY 'Item '
      @ L, 53 SAY 'Unit'
      @ L, 61 SAY 'Min  '
      @ L, 71 SAY 'Max '
      @ L, 84 SAY 'Present  '
      @ L, 95 SAY 'Reorder  '
      @ L, 105 SAY 'Reorder  '
      @ L, 117 SAY 'Last '
      L= L+1
      @ L, 61 SAY 'Level '
      @ L, 71 SAY 'Level'
      @ L, 84 SAY 'Qty '
      @ L, 95 SAY 'Level '
      @ L, 105 SAY 'Qty'
      @ L, 117 SAY 'Purchase Price'
      L= L+1
      @ L, 0 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MQTY= QTY
   SELECT 1
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF FOUND()
      IF MQTY<REORD_LEV
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ L, 1 SAY MI1 PICTURE '99'
         @ L, 3 SAY '-'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ L, 4 SAY MI2 PICTURE '99'
         @ L, 6 SAY '-'
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ L, 7 SAY MI3 PICTURE '9999'
         @ L, 13 SAY ITEM
         @ L, 53 SAY UNIT
         @ L, 61 SAY MIN_LEV PICTURE '99999'
         @ L, 71 SAY MAX_LEV PICTURE '99999'
         @ L, 82 SAY MQTY PICTURE '999999.99'
         @ L, 96 SAY REORD_LEV PICTURE '99999'
         @ L, 106 SAY REORD_QTY PICTURE '99999'
         @ L, 122 SAY LAST_PRICE PICTURE '9999.99'
      ELSE
         SELECT 2
         IF .not. EOF()
            SKIP
         ENDIF
         LOOP
      ENDIF
   ENDIF
   IF L>=56
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      P= P+1
      L= 1
   ENDIF
   SELECT 2
   L= L+1
   IF .not. EOF()
      SKIP
   ENDIF
ENDDO
EJECT
@ 0, 0 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
***************


***********************
PROCEDURE REP_MAX
***********************

CLEAR
HEADING= .T.
P= 1
SELECT 1
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 2
USE item-bal
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4)+STR(LOC_CODE,4) TAG open_idx
SELECT 3
USE loc
INDEX ON LOC_CODE TAG loc_idx
CLEAR
ABC= 0
DO WHILE ABC=0
   @ 1, 25 TO 3, 55 DOUBLE
   @ 2, 27 SAY ' STOCK ABOVE MAXIMUM LEVEL'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO FITEM_CODE, EITEM_CODE, MLOC_CODE, MI1, MI2, MI3
   STORE DATE() TO MDATE
   @ 10, 15 SAY 'Location Code ' GET MLOC_CODE PICTURE '9999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 77
   SELECT 3
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 40 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 
ENDIF

   @ 12, 15 SAY "Enter Today's Date      " GET MDATE
   READ
   SELECT 2
   SET FILTER TO LOC_CODE=MLOC_CODE
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist For This Location - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
SET PRINTER ON
SET DEVICE TO PRINTER
L= 6
DO WHILE .not. EOF()
   IF HEADING
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(14)
      @ 3, 10 SAY 'STOCK ABOVE MAXIMUM LEVEL'
      @ 5, 1 SAY 'Location : '+MLOC
      @ L, 0 SAY CHR(15)
      @ L, 3 SAY 'As On           : '
      @ L, 22 SAY MDATE
      @ L, 120 SAY 'Page No -'
      @ L, 130 SAY P PICTURE '99'
      L= L+1
      @ L, 0 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Item'
      @ L, 13 SAY 'Descripition'
      @ L, 53 SAY 'Unit'
      @ L, 61 SAY 'Min'
      @ L, 71 SAY 'Max'
      @ L, 84 SAY 'Present'
      @ L, 95 SAY 'Reorder'
      @ L, 105 SAY 'Reorder'
      @ L, 117 SAY 'Last'
      L= L+1
      @ L, 61 SAY 'Level'
      @ L, 71 SAY 'Level'
      @ L, 84 SAY 'Qty'
      @ L, 95 SAY 'Level'
      @ L, 105 SAY 'Qty'
      @ L, 117 SAY 'Purchase Price'
      L= L+1
      @ L, 0 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   STORE STR(I1,2)+STR(I2,2)+STR(I3,4)+STR(MLOC_CODE,4) TO XYZ
   SELECT 2
   SEEK XYZ
   MQTY= QTY
   STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO XYZ
   SELECT 1
   SEEK XYZ
   IF FOUND()
      IF MQTY>MAX_LEV
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ L, 1 SAY MI1 PICTURE '99'
         @ L, 3 SAY '-'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ L, 4 SAY MI2 PICTURE '99'
         @ L, 6 SAY '-'
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ L, 7 SAY MI3 PICTURE '9999'
         @ L, 13 SAY ITEM
         @ L, 53 SAY UNIT
         @ L, 61 SAY MIN_LEV PICTURE '99999'
         @ L, 71 SAY MAX_LEV PICTURE '99999'
         @ L, 82 SAY MQTY PICTURE '999999.99'
         @ L, 96 SAY REORD_LEV PICTURE '99999'
         @ L, 106 SAY REORD_QTY PICTURE '99999'
         @ L, 122 SAY LAST_PRICE PICTURE '9999.99'
      ENDIF
   ENDIF
   IF L>=56
      @ 1, 1 SAY CHR(18)
      HEADING= .T.
      P= P+1
      L= 1
   ENDIF
   SELECT 2
   L= L+1
   SKIP
ENDDO
EJECT
@ 0, 0 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
*********

************************
PROCEDURE REP_MEN5
************************

DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 23 SAY 'C O N S U M P T I O N   R E P O R T S '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 8, 20 PROMPT ' 1 - Consumption Report (Item Wise) '
   @ 9, 20 PROMPT ' 2 - Consumption Report (Department Wise) '
   @ 10, 20 PROMPT ' 3 - Consumption Report (Date Wise) '
   @ 11, 20 PROMPT ' 4 - Consumption Report (Department Wise)'
   @ 12, 20 SAY '     (Summary) '
   @ 13, 20 PROMPT ' 5 - Consumption Report (Item Wise Summary)'
   @ 15, 20 PROMPT ' 9 - Return to Main Menu'
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep_con
      CASE MCHOICE=2
         DO rep_con1
      CASE MCHOICE=3
         DO rep_con2
      CASE MCHOICE=4
         DO rep_con3
      CASE MCHOICE=5
         DO rep_con4
      CASE MCHOICE=6
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN
***************

************************
PROCEDURE REP_CON
*************************

CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 2
USE dept
INDEX ON DEPT_CODE TAG dept_idx
SELECT 1
USE stock
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MQTY, MTP, MI1, MI2, MI3
   MITEM= SPACE(30)
   @ 1, 22 TO 3, 59 DOUBLE
   @ 2, 24 SAY 'C O N S U M P T I O N   R E P O R T'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 1
   @ 14, 15 SAY 'Item Code '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 29 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 14, 29 GET MI1 PICTURE '99'
   READ
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 29 SAY MPC1
   @ 14, 31 SAY '-'
   @ 14, 32 GET MI2 PICTURE '99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 32 SAY MPC2
   @ 14, 34 SAY '-'
   @ 14, 35 GET MI3 PICTURE '9999'
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 35 SAY MPC3
   READ

IF _DOS
   SET COLOR TO 
ENDIF

   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO 
ENDIF

   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 40 SAY ITEM
   MUNIT= UNIT
   MITEM= ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 5, 5 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='S'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
MTOT= 0
L= 7
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY 'CONSUMPTION REPORT (ITEM WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ 7, 1 SAY 'Item  :'
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ 7, 9 SAY MPC1 PICTURE '99'
         @ 7, 11 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ 7, 12 SAY MPC2 PICTURE '99'
         @ 7, 14 SAY '-'
         MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ 7, 15 SAY MPC3 PICTURE '9999'
         @ 7, 22 SAY MITEM
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 8 SAY 'CONSUMPTION REPORT (ITEM WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ 7, 1 SAY 'Code  :'
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ 7, 9 SAY MPC1 PICTURE '99'
         @ 7, 11 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ 7, 12 SAY MPC2 PICTURE '99'
         @ 7, 14 SAY '-'
         MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ 7, 15 SAY MPC3 PICTURE '9999'
         @ 7, 22 SAY MITEM
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Date'
      @ L, 11 SAY 'Department'
      @ L, 35 SAY 'Unit'
      @ L, 44 SAY 'Qty Issued'
      @ L, 62 SAY 'Rate'
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   @ L, 1 SAY DATE
   SELECT 2
   LOCATE FOR DEPT_CODE= A->DEPT_CODE
   IF FOUND()
      @ L, 11 SAY DEPT
   ENDIF
   @ L, 35 SAY MUNIT
   SELECT 1
   @ L, 42 SAY T_QTY PICTURE '9999999.9999'
   @ L, 55 SAY RATE PICTURE '9999999.9999'
   MTOT= T_QTY*RATE
   @ L, 68 SAY MTOT PICTURE '999999999.99'
   MQTY= MQTY+T_QTY
   MTP= MTP+MTOT
   IF ANS='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      L= 7
      HEADING= .T.
   ENDIF
   IF L>=56
      HEADING= .T.
      L= 7
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL :'
@ L, 42 SAY MQTY PICTURE '9999999.9999'
@ L, 68 SAY MTP PICTURE '999999999.99'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
*********


***********************
PROCEDURE REP_CON1
***********************

CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 2
USE dept
INDEX ON DEPT_CODE TAG dept_idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE, MI1, MI2, MI3
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 21 SAY 'C O N S U M P T I O N    R E P O R T '
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 2
   MD_CODE= 0
   @ 14, 15 SAY 'Department ' GET MD_CODE PICTURE '99'
   READ
   IF MD_CODE=0
      LOOP
   ENDIF
   SEEK MD_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ----  Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 33 SAY DEPT
   MDEPT= DEPT

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DEPT_CODE=MD_CODE.and.DOC_TYPE=3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 5, 5 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
GOTO TOP
IF ANS='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT
L= 7
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 4 SAY 'CONSUMPTION REPORT (DEPARTMENT WISE)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 3, 22 SAY 'CONSUMPTION REPORT (DEPARTMENT WISE)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item'
      @ L, 12 SAY 'Descripition'
      @ L, 35 SAY 'Unit'
      @ L, 44 SAY 'Qty Issued'
      @ L, 63 SAY 'Rate'
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MPC1 PICTURE '99'
   @ L, 3 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MPC2 PICTURE '99'
   @ L, 6 SAY '-'
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MPC3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 35 SAY UNIT
   ENDIF
   SELECT 1
   MSVALUE= T_QTY*RATE
   @ L, 42 SAY T_QTY PICTURE '9999999.9999'
   @ L, 55 SAY RATE PICTURE '9999999.9999'
   @ L, 68 SAY MSVALUE PICTURE '999999999.99'
   QTY= QTY+T_QTY
   MTOT= MTOT+MSVALUE
   IF ANS='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      L= 7
      P= P+1
   ENDIF
   IF L>=56
      HEADING= .T.
      L= 7
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
L= L+1
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL : '
@ L, 42 SAY QTY PICTURE '9999999.9999'
@ L, 68 SAY MTOT PICTURE '999999999.99'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   @ 0, 0 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
********


**********************
PROCEDURE REP_CON2
**********************

CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 21 SAY 'C O N S U M P T I O N    R E P O R T '
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 5, 5 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='P'
   SET PRINTER ON
   SET DEVICE TO PRINTER
ELSE
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT
L= 6
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY 'CONSUMPTION REPORT (DATE WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 3, 24 SAY 'CONSUMPTION REPORT (DATE WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item'
      @ L, 12 SAY 'Descripition'
      @ L, 35 SAY 'Unit'
      @ L, 44 SAY 'Qty Issued'
      @ L, 63 SAY 'Rate'
      @ L, 75 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MPC1 PICTURE '99'
   @ L, 3 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MPC2 PICTURE '99'
   @ L, 6 SAY '-'
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MPC3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 2
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 35 SAY UNIT
   ENDIF
   SELECT 1
   MSVALUE= T_QTY*RATE
   @ L, 42 SAY T_QTY PICTURE '9999999.9999'
   @ L, 55 SAY RATE PICTURE '9999999.9999'
   @ L, 68 SAY MSVALUE PICTURE '999999999.99'
   QTY= QTY+T_QTY
   MTOT= MTOT+MSVALUE
   IF ANS='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      L= 6
      HEADING= .T.
   ENDIF
   IF L>=56
      HEADING= .T.
      L= 6
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
L= L+1
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL : '
@ L, 42 SAY QTY PICTURE '9999999.9999'
@ L, 68 SAY MTOT PICTURE '999999999.99'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   @ 3, 1 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
*********



**********************
PROCEDURE REP_CON3
**********************

CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 2
USE dept
INDEX ON DEPT_CODE TAG dept_idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE, MI1, MI2, MI3
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 21 SAY 'C O N S U M P T I O N    R E P O R T '
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 2
   MD_CODE= 0
   @ 14, 15 SAY 'Department ' GET MD_CODE PICTURE '99'
   READ
   IF MD_CODE=0
      LOOP
   ENDIF
   SEEK MD_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ----  Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 33 SAY DEPT
   MDEPT= DEPT

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DEPT_CODE=MD_CODE.and.DOC_TYPE=3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
GOTO TOP
IF ANS='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT, MTQTY
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 2 SAY 'CONSUMPTION REPORT (DEPT WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 3, 19 SAY 'CONSUMPTION REPORT (DEPARTMENT WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Issurd '
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+T_AMOUNT
      MTOT= MTOT+T_AMOUNT
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   @ L, 66 SAY MAMT PICTURE '9999999999.99'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF ANS='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF ANS='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN
*****************

********************
PROCEDURE REP_CON4
********************

CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE, MI1, MI2, MI3
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 21 SAY 'C O N S U M P T I O N    R E P O R T '
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=3
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
GOTO TOP
IF ANS='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT, MTQTY
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 2 SAY 'CONSUMPTION REPORT (ITEM WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 3, 19 SAY 'CONSUMPTION REPORT (ITEM WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Issurd '
      @ L, 70 SAY 'Value'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+T_AMOUNT
      MTOT= MTOT+T_AMOUNT
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   @ L, 65 SAY MAMT PICTURE '9999999999.99'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF ANS='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF ANS='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
@ L, 66 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN
*************

***********************
PROCEDURE REP_MEN6
***********************
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1   
ENDIF

   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 23 SAY '      PRODUCTION REPORTS  '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 8, 20 PROMPT ' 1 - Production Report (Item Wise) '
   @ 9, 20 PROMPT ' 2 - Production Report (Department Wise) '
   @ 10, 20 PROMPT ' 3 - Production Report (Date Wise) '
   @ 11, 20 PROMPT ' 4 - Production Report (Department Wise)'
   @ 12, 20 SAY '     (Summary) '
   @ 13, 20 PROMPT ' 5 - Production Report (Item Wise Summary)'
   @ 15, 20 PROMPT ' 9 - Return to Main Menu'
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO rep_pro
      CASE MCHOICE=2
         DO rep_pro1
      CASE MCHOICE=3
         DO rep_pro2
      CASE MCHOICE=4
         DO rep_pro3
      CASE MCHOICE=5
         DO rep_pro4
      CASE MCHOICE=6
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN
*****************

*********************
PROCEDURE REP_PRO
*********************

CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 2
USE dept
INDEX ON DEPT_CODE TAG dept_idx
SELECT 1
USE stock
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MQTY, MTP, MI1, MI2, MI3
   MITEM= SPACE(30)
   @ 1, 22 TO 3, 59 DOUBLE
   @ 2, 24 SAY ' P R O D U C T I O N    R E P O R T'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 1
   @ 14, 15 SAY 'Item Code '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 29 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 14, 29 GET MI1 PICTURE '99'
   READ
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 29 SAY MPC1
   @ 14, 31 SAY '-'
   @ 14, 32 GET MI2 PICTURE '99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 32 SAY MPC2
   @ 14, 34 SAY '-'
   @ 14, 35 GET MI3 PICTURE '9999'
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 35 SAY MPC3
   READ

IF _DOS
   SET COLOR TO 
ENDIF

   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO 
ENDIF

   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ---- Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 40 SAY ITEM
   MUNIT= UNIT
   MITEM= ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.I1=MI1.and.I2=MI2.and.I3=MI3.and.DOC_TYPE=5
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 5, 5 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='S'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
MTOT= 0
L= 7
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY 'PRODUCTION REPORT (ITEM WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ 7, 1 SAY 'Item  :'
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ 7, 9 SAY MPC1 PICTURE '99'
         @ 7, 11 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ 7, 12 SAY MPC2 PICTURE '99'
         @ 7, 14 SAY '-'
         MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ 7, 15 SAY MPC3 PICTURE '9999'
         @ 7, 22 SAY MITEM
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 8 SAY 'PRODUCTION REPORT (ITEM WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
         @ 7, 1 SAY 'Code  :'
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ 7, 9 SAY MPC1 PICTURE '99'
         @ 7, 11 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ 7, 12 SAY MPC2 PICTURE '99'
         @ 7, 14 SAY '-'
         MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ 7, 15 SAY MPC3 PICTURE '9999'
         @ 7, 22 SAY MITEM
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Date'
      @ L, 11 SAY 'Department'
      @ L, 35 SAY 'Unit'
      @ L, 44 SAY 'Qty Issued'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   @ L, 1 SAY DATE
   SELECT 2
   LOCATE FOR DEPT_CODE= A->DEPT_CODE
   IF FOUND()
      @ L, 11 SAY DEPT
   ENDIF
   @ L, 35 SAY MUNIT
   SELECT 1
   @ L, 42 SAY T_QTY PICTURE '9999999.9999'
   MQTY= MQTY+T_QTY
   IF ANS='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      L= 7
      HEADING= .T.
   ENDIF
   IF L>=56
      HEADING= .T.
      L= 7
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL :'
@ L, 42 SAY MQTY PICTURE '9999999.9999'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
***********

**********************
PROCEDURE REP_PRO1
**********************

CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 2
USE dept
INDEX ON DEPT_CODE TAG dept_idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE, MI1, MI2, MI3
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 24 SAY ' P R O D U C T I O N    R E P O R T'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 2
   MD_CODE= 0
   @ 14, 15 SAY 'Department ' GET MD_CODE PICTURE '99'
   READ
   IF MD_CODE=0
      LOOP
   ENDIF
   SEEK MD_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ----  Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 33 SAY DEPT
   MDEPT= DEPT

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DEPT_CODE=MD_CODE.and.DOC_TYPE=5
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 5, 5 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
GOTO TOP
IF ANS='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT
L= 7
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 4 SAY 'PRODUCTION REPORT (DEPARTMENT WISE)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 3, 22 SAY 'PRODUCTION REPORT (DEPARTMENT WISE)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item'
      @ L, 12 SAY 'Descripition'
      @ L, 35 SAY 'Unit'
      @ L, 44 SAY 'Qty Issued'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MPC1 PICTURE '99'
   @ L, 3 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MPC2 PICTURE '99'
   @ L, 6 SAY '-'
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MPC3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 35 SAY UNIT
   ENDIF
   SELECT 1
   MSVALUE= T_QTY*RATE
   @ L, 42 SAY T_QTY PICTURE '9999999.9999'
   QTY= QTY+T_QTY
   IF ANS='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
      L= 7
      P= P+1
   ENDIF
   IF L>=56
      HEADING= .T.
      L= 7
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
L= L+1
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL : '
@ L, 42 SAY QTY PICTURE '9999999.9999'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   @ 0, 0 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
********

**********************
PROCEDURE REP_PRO2
**********************

CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 24 SAY ' P R O D U C T I O N    R E P O R T'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=5
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
@ 24, 50 SAY 'Press any key to Continue ... '
WAIT ''
CLEAR
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 5, 5 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
CLEAR
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='P'
   SET PRINTER ON
   SET DEVICE TO PRINTER
ELSE
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT
L= 6
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 8 SAY 'PRODUCTION REPORT (DATE WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 3, 24 SAY 'PRODUCTION REPORT (DATE WISE)'
         @ 5, 1 SAY 'From  :'
         @ 5, 9 SAY FDATE
         @ 6, 1 SAY 'To    :'
         @ 6, 9 SAY TDATE
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item'
      @ L, 12 SAY 'Descripition'
      @ L, 35 SAY 'Unit'
      @ L, 44 SAY 'Qty Issued'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MPC1 PICTURE '99'
   @ L, 3 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MPC2 PICTURE '99'
   @ L, 6 SAY '-'
   MPC3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MPC3 PICTURE '9999'
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 2
   LOCATE FOR I1=MI1.and.I2=MI2.and.I3=MI3
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 35 SAY UNIT
   ENDIF
   SELECT 1
   MSVALUE= T_QTY*RATE
   @ L, 42 SAY T_QTY PICTURE '9999999.9999'
   QTY= QTY+T_QTY
   MTOT= MTOT+MSVALUE
   IF ANS='S'.and.L>21
      @ 24, 15 SAY ''
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      L= 6
      HEADING= .T.
   ENDIF
   IF L>=56
      HEADING= .T.
      L= 6
      P= P+1
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   SKIP
ENDDO
L= L+1
@ L, 35 SAY REPLICATE('-',45)
L= L+1
@ L, 35 SAY 'TOTAL : '
@ L, 42 SAY QTY PICTURE '9999999.9999'
L= L+1
@ L, 35 SAY REPLICATE('-',45)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   @ 3, 1 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SET PRINTER OFF
SET DEVICE TO SCREEN
SET FILTER TO
CLEAR
CLOSE ALL
RETURN
*************

**********************
PROCEDURE REP_PRO3
**********************

CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 2
USE dept
INDEX ON DEPT_CODE TAG dept_idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE, MI1, MI2, MI3
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 24 SAY ' P R O D U C T I O N    R E P O R T'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 2
   MD_CODE= 0
   @ 14, 15 SAY 'Department ' GET MD_CODE PICTURE '99'
   READ
   IF MD_CODE=0
      LOOP
   ENDIF
   SEEK MD_CODE
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist  ----  Please Retry'
      WAIT ''
      @ 23, 9 SAY SPACE(45)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 14, 33 SAY DEPT
   MDEPT= DEPT

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DEPT_CODE=MD_CODE.and.DOC_TYPE=5
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
GOTO TOP
IF ANS='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT, MTQTY
DO WHILE .not. EOF()
   IF HEADING
      L= 7
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 2 SAY 'PRODUCTION REPORT (DEPT WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 3, 19 SAY 'PRODUCTION REPORT (DEPARTMENT WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
         @ 7, 1 SAY 'Department :'
         @ 7, 15 SAY MD_CODE PICTURE '99'
         @ 7, 20 SAY MDEPT
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Issurd '
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+T_AMOUNT
      MTOT= MTOT+T_AMOUNT
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF ANS='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF ANS='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN
*************



************************
PROCEDURE REP_PRO4
************************

CLOSE ALL
CLEAR
HEADING= .T.
P= 1
SELECT 3
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 1
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_idx
ABC= 0
DO WHILE ABC=0
   SELECT 1
   STORE DATE() TO FDATE, TDATE, MI1, MI2, MI3
   STORE 0 TO MTP
   @ 1, 18 TO 3, 59 DOUBLE
   @ 2, 24 SAY ' P R O D U C T I O N    R E P O R T'
   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   @ 10, 15 SAY 'Enter Date From (dd-mm-yy)    ' GET FDATE
   @ 12, 15 SAY '           To   (dd-mm-yy)    ' GET TDATE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 49 CLEAR TO 23, 77
   IF TDATE<FDATE
      @ 23, 15 SAY 'From date should be greater than to date'
      WAIT ''
      @ 23, 15 CLEAR TO 23, 65
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.DOC_TYPE=5
   COUNT TO RCON
   IF RCON=0
      @ 23, 5 SAY 'No Transactions exist between these two dates - Please Re-try '
      WAIT ''
      @ 23, 4 CLEAR TO 23, 75
      LOOP
   ENDIF
   ABC= 1
ENDDO
ANS= ' '
DO WHILE .not. ANS$'SPQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  - P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
GOTO TOP
IF ANS='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
STORE 0 TO QTY, MSVALUE, MTOT, MTQTY
DO WHILE .not. EOF()
   IF HEADING
      L= 6
      IF ANS='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(14)
         @ 3, 2 SAY 'PRODUCTION REPORT (ITEM WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
      ELSE
         DO screen IN TBL_PROC.PRG
         @ 3, 19 SAY 'PRODUCTION REPORT (ITEM WISE SUMMARY)'
         @ 5, 1 SAY 'From       :'
         @ 5, 15 SAY FDATE
         @ 6, 1 SAY 'To         :'
         @ 6, 15 SAY TDATE
      ENDIF
      @ L, 66 SAY 'Page No - '
      @ L, 77 SAY P PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 12 SAY 'Descripition'
      @ L, 42 SAY 'Unit'
      @ L, 52 SAY 'Qty Issurd '
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   STORE 0 TO MQTY, MAMT
   DO WHILE MI1=I1.and.MI2=I2.and.MI3=I3
      IF MI1<>I1.and.MI2<>I2.and.MI3<>I3
         EXIT
      ENDIF
      MQTY= MQTY+T_QTY
      MAMT= MAMT+T_AMOUNT
      MTOT= MTOT+T_AMOUNT
      MTQTY= MTQTY+T_QTY
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 7 SAY MP3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SELECT 3
   SEEK MI_CODE
   IF FOUND()
      @ L, 12 SAY ITEM
      @ L, 42 SAY UNIT
   ENDIF
   SELECT 1
   @ L, 52 SAY MQTY PICTURE '9999999.9999'
   IF EOF()
      EXIT
   ENDIF
   L= L+1
   IF ANS='S'.and.L>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEADING= .T.
   ENDIF
   IF ANS='P'.and.L>56
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 41 SAY 'Total'
@ L, 52 SAY MTQTY PICTURE '9999999.9999'
L= L+1
@ L, 40 SAY REPLICATE('-',40)
IF ANS='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF ANS='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN
***************

************************
PROCEDURE DENIED
************************

CLEAR
ABC= 0
DO WHILE ABC<=3
   ?? CHR(7)

IF _DOS
   SET COLOR TO *w
ENDIF

   @ 12, 25 SAY 'Access Denied ! '
   ABC= ABC+1
ENDDO

IF _DOS
SET COLOR TO 
ENDIF


CLEAR
RETURN


**************
PROCEDURE CHK
***************
SET DATE brit
SET CENT ON
IF MDATE>CTOD('07/07/2099')
   SET ESCAPE OFF
   CLOSE ALL
   CLEAR
   QUIT
ENDIF
RETURN
 
******************
******************
==================
* AMJAD IZHAR
