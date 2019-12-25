**********************************************
* PROCEDURE : PLB_PROC.PRG					 *
* Author: Amjad Izhar 						 *
* Profit & Loss Reports 					 *
* and Balance Sheet Reports  				 *
* Written : 21-09-96 						 *
* Revised Again On : 19-01-2001 7:15PM
**********************************************


*********************
PROCEDURE C3_PL
*********************

CLEAR
CLOSE ALL

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
CLEAR PROGRAM
ZEROBAL= ' '
DO WHILE .not. ZEROBAL$'YN'
   @ 4, 2 SAY 'Do You Want To Include Ledger Heads Having Zero Balance - Y/N ' GET ZEROBAL FUNCTION '!'
   READ
ENDDO
@ 4, 2 SAY SPACE(70)
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE, TOTCR7, TOTDR7, TOTCR8, TOTDR8, TOT7, TOT8, SWCH
CLEAR
SELECT 1
USE master ORDER mastdate

GOTO BOTTOM
STORE DATE TO MTDATE
CLEAR
@ 2, 5 SAY 'Profit /(Loss) (Or Income) Statement'
@ 5, 5 SAY 'Your last posted voucher is dated '
@ 5, 42 SAY MTDATE
@ 6, 5 SAY 'Do you want to dispaly same date on your Report '
@ 7, 5 SAY 'Or enter the date you want to dispaly  ' GET MTDATE
READ
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='P'
   do PRNCHK in tbl_proc
   CLEAR
   @ 8, 17 SAY 'Profit / Loss (Or Income) Statement'
   @ 10, 17 SAY 'Please use 80 column (10 inches wide) continous'
   @ 11, 17 SAY 'printer paper and set Top Of Form'
   @ 12, 17 SAY 'Press ENTER when ready...........'
   WAIT ''
   @ 14, 17 SAY 'Printing In Progress'
ENDIF
IF P='P'
   CLEAR
   PANS= ' '
   DO prnchk IN tbl_proc.prg
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing In Progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ELSE
   CLEAR
   @ 10, 18 SAY 'Please Wait ... Calculations In Progress '
   SET DEVICE TO SCREEN
ENDIF
STORE SPACE(30) TO WTITLE, WHTITLE
STORE 0 TO AMT6, AMT7, AMT8, AMT9, GPFT, GPFT2
SELECT 2
USE chart
COPY TO abc FOR CHART_NO>5.and.CHART_NO<10
USE abc
INDEX ON CHART_NO TAG cht_idx ADDITIVE
GOTO TOP
M1= FR
M2= TO
SELECT 1
USE pl-work2
ZAP
APPEND FROM ldgrcode FOR C1>=M1.and.C1<=M2
REPLACE FOR CHART_NO=0 CHART_NO WITH 6
SELECT 2
DO WHILE .not. EOF()
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   M3= FR
   M4= TO
   SELECT 1
   IF M3<>0.and.M4<>0
      APPEND FROM ldgrcode FOR C1>=M3.and.C1<=M4
      REPLACE FOR CHART_NO=0 CHART_NO WITH 7
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   M5= FR
   M6= TO
   SELECT 1
   IF M5<>0.and.M6<>0
      APPEND FROM ldgrcode FOR C1>=M5.and.C1<=M6
      REPLACE FOR CHART_NO=0 CHART_NO WITH 8
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   M7= FR
   M8= TO
   SELECT 1
   IF M7<>0.and.M8<>0
      APPEND FROM ldgrcode FOR C1>=M7.and.C1<=M8
      REPLACE FOR CHART_NO=0 CHART_NO WITH 9
   ENDIF
   EXIT
ENDDO
SELECT 3
USE ldgrcode ORDER ac1

SELECT 1
USE pl-work2 ORDER bl_c1

SELECT 2
GOTO TOP
SELECT 1
MCODE= 0
HEADING= .T.
PAGE= 1
STORE 0 TO WRECNO, MSRNO, WCODE
WGTITLE= SPACE(30)
STORE 0 TO DRTOT, CRTOT, TCR6, TDR6, GTDR, GTCR, LINE, TOTDR, TOTCR
STORE 0 TO TCR7, TDR7, TCR8, TDR8, TCR9, TDR9
MC1= C1
MC2= C2
MC3= C3
CLEAR
DO WHILE .not. EOF()
   IF ZEROBAL='N'
      IF LDGR_BAL=0
         SKIP
         LOOP
      ENDIF
   ENDIF
   MC1= C1
   MCHART= CHART_NO
   CHD= 0
   DO WHILE CHART_NO=MCHART
      IF EOF()
         EXIT
      ENDIF
      IF HEADING
         DO PRNCHK IN tbl_proc.prg
         DO top_prt1 IN tbl_proc.prg
         @ 2, 4 SAY 'PROFIT / LOSS STATEMENT FOR THE PERIOD ENDING ON:'
         @ 2, 53 SAY MTDATE
         @ 2, 72 SAY 'PAGE: '
         @ 2, 78 SAY PAGE PICTURE '99'
         @ 3, 1 SAY REPLICATE('-',79)
         @ 4, 1 SAY 'CODE-----'
         @ 4, 11 SAY '   ---- ACCOUNT  TITLE ----'
         @ 4, 62 SAY 'AMOUNT'
         @ 5, 1 SAY REPLICATE('-',79)
         LINE= 6
         HEADING= .F.
      ENDIF
      IF ZEROBAL='N'
         IF LDGR_BAL=0
            SKIP
            LOOP
         ENDIF
      ENDIF
      IF C3=0
         SKIP
         LOOP
      ENDIF
      LINE= LINE+1
      STORE SPACE(30) TO WTITLE
      STORE TITLE TO WTITLE
      IF CHD=0
         STORE CHART_NO*10 TO MC1
         STORE 0 TO MC2, MC3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MLCODE
         SELECT 3
         SEEK MLCODE
         IF .not. FOUND()
            SELECT 2
            HD= AT('Head',CHART)
            @ LINE, 5 SAY LEFT(CHART,HD-1)
            LINE= LINE+1
            @ LINE, 5 SAY REPLICATE('-',LEN(RTRIM(LEFT(CHART,HD-1))))
            LINE= LINE+1
         ELSE
            SELECT 3
            @ LINE, 5 SAY TITLE
            LINE= LINE+1
            @ LINE, 5 SAY REPLICATE('-',LEN(RTRIM(TITLE)))
            LINE= LINE+1
         ENDIF
         SELECT 1
         CHD= 1
      ENDIF
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ LINE, 1 SAY MPC1
      @ LINE, 3 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ LINE, 4 SAY MPC2
      @ LINE, 6 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ LINE, 7 SAY MPC3
      @ LINE, 14 SAY WTITLE
      IF MCHART=6
         IF STATUS='DR'
            @ LINE, 56 SAY LDGR_BAL* -1 PICTURE '@( 99,999,999,999.99'
            TDR6= TDR6+LDGR_BAL
         ELSE
            @ LINE, 56 SAY LDGR_BAL PICTURE '@( 9,9999,999,999.99'
            TCR6= TCR6+LDGR_BAL
         ENDIF
      ENDIF
      IF MCHART>6
         IF STATUS='DR'
            @ LINE, 56 SAY LDGR_BAL PICTURE '@( 99,999,999,999.99'
         ELSE
            @ LINE, 56 SAY LDGR_BAL* -1 PICTURE '@( 99,999,999,999.99'
         ENDIF
      ENDIF
      IF MCHART=7
         IF STATUS='DR'
            TDR7= TDR7+LDGR_BAL
         ELSE
            TCR7= TCR7+LDGR_BAL
         ENDIF
      ENDIF
      IF MCHART=8
         IF STATUS='DR'
            TDR8= TDR8+LDGR_BAL
         ELSE
            TCR8= TCR8+LDGR_BAL
         ENDIF
      ENDIF
      IF MCHART=9
         IF STATUS='DR'
            TDR9= TDR9+LDGR_BAL
         ELSE
            TCR9= TCR9+LDGR_BAL
         ENDIF
      ENDIF
      IF P='S'.and.LINE>=18
         ANS= ' '
         DO WHILE .not. ANS$'CQ'

IF _DOS
            SET COLOR TO w+
ENDIF

            SET CONFIRM OFF
            @ 24, 20 SAY 'Please Enter <C> To Continue <Q> To Quit' GET ANS PICTURE '!'
            READ

IF _DOS
            SET COLOR TO 
ENDIF

         ENDDO
         IF ANS='C'
            CLEAR
            PAGE= PAGE+1
            HEADING= .T.
            LINE= 6
         ELSE
            CLOSE DATA
            CLEAR
            RETURN
         ENDIF
      ENDIF
      IF P='P'.and.LINE>=56
         PAGE= PAGE+1
         HEADING= .T.
         EJECT
      ENDIF
      SKIP
   ENDDO
   IF HEADING
      DO PRNCHK IN tbl_proc.prg
      DO top_prt1 IN tbl_proc.prg
      @ 2, 4 SAY 'PROFIT / LOSS STATEMENT FOR THE PERIOD ENDING ON:'
      @ 2, 53 SAY MTDATE
      @ 2, 72 SAY 'PAGE: '
      @ 2, 78 SAY PAGE PICTURE '99'
      @ 3, 1 SAY REPLICATE('-',79)
      @ 4, 1 SAY 'CODE-----'
      @ 4, 11 SAY '   ---- ACCOUNT  TITLE ----'
      @ 4, 62 SAY 'AMOUNT'
      @ 5, 1 SAY REPLICATE('-',79)
      LINE= 6
      HEADING= .F.
   ENDIF
   LINE= LINE+1
   IF MCHART=6
      AMT6= TCR6-TDR6
      @ LINE, 55 SAY REPLICATE('-',20)
      LINE= LINE+1
      @ LINE, 10 SAY 'Total '
      @ LINE, 56 SAY AMT6 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
   ENDIF
   IF MCHART=7
      AMT7= TDR7-TCR7
      @ LINE, 55 SAY REPLICATE('-',20)
      LINE= LINE+1
      @ LINE, 10 SAY 'Total '
      @ LINE, 56 SAY AMT7 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
   ENDIF
   NGPFT= AMT6-AMT7
   IF AMT7<>0.and.AMT8=0.and.GPFT=0
      LINE= LINE+1
      @ LINE, 10 SAY 'Gross Profit/(Loss)'
      @ LINE, 56 SAY NGPFT PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
      GPFT= 1
   ENDIF
   IF MCHART=8
      AMT8= TDR8-TCR8
      @ LINE, 55 SAY REPLICATE('-',20)
      LINE= LINE+1
      @ LINE, 10 SAY 'Total '
      @ LINE, 56 SAY AMT8 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
   ENDIF
   NPFT= NGPFT-AMT8
   IF NGPFT<>0.and.AMT8<>0.and.GPFT2=0
      @ LINE, 10 SAY 'Net Profit /(Loss)'
      @ LINE, 56 SAY NPFT PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
      GPFT2= 1
   ENDIF
   IF MCHART=9
      AMT9= TDR9-TCR9
      @ LINE, 55 SAY REPLICATE('-',20)
      LINE= LINE+1
      @ LINE, 10 SAY 'Total '
      @ LINE, 56 SAY AMT9 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
   ENDIF
   IF AMT9<>0
      @ LINE, 10 SAY 'Net Profit /(Loss) After Appropriations '
      @ LINE, 56 SAY NPFT-AMT9 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
   ENDIF
   LINE= LINE+1
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   SELECT 1
ENDDO
IF P='P'
   EJECT
   LINE= 2
   @ LINE, 4 SAY 'PROFIT / LOSS STATEMENT '
   @ LINE, 28 SAY 'SUMMARY'
   @ LINE, 50 SAY 'Up TO'
   @ LINE, 56 SAY MTDATE
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Sales'
   @ LINE, 56 SAY AMT6 PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Cost Of Sales'
   @ LINE, 56 SAY AMT7 PICTURE '@( 99,999,999,999.99'
   NGPFT= AMT6-AMT7
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',15)
   LINE= LINE+1
   @ LINE, 10 SAY 'Gross Profit/(Loss)'
   @ LINE, 56 SAY NGPFT PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',15)
   LINE= LINE+1
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Expenses'
   @ LINE, 56 SAY AMT8 PICTURE '@( 99,999,999,999.99'
   NPFT= NGPFT-AMT8
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',15)
   LINE= LINE+1
   @ LINE, 10 SAY 'Net Profit /(Loss)'
   @ LINE, 56 SAY NPFT PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',15)
   LINE= LINE+1
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Appropriations'
   @ LINE, 56 SAY AMT9 PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',15)
   LINE= LINE+1
   @ LINE, 10 SAY 'Net Profit /(Loss) After Appropriations '
   @ LINE, 56 SAY NPFT-AMT9 PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   @ 23, 1 SAY ''
   WAIT SPACE(20)+'Press Any Key For Summary'
   CLEAR
   LINE= 2
   @ LINE, 4 SAY 'PROFIT / LOSS STATEMENT '
   @ LINE, 28 SAY 'SUMMARY'
   @ LINE, 50 SAY 'Up To'
   @ LINE, 56 SAY MTDATE
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Sales'
   @ LINE, 56 SAY AMT6 PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Cost Of Sales'
   @ LINE, 56 SAY AMT7 PICTURE '@( 99,999,999,999.99'
   NGPFT= AMT6-AMT7
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   @ LINE, 10 SAY 'Gross Profit/(Loss)'
   @ LINE, 56 SAY NGPFT PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Expenses'
   @ LINE, 56 SAY AMT8 PICTURE '@( 99,999,999,999.99'
   NPFT= NGPFT-AMT8
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   @ LINE, 10 SAY 'Net Profit /(Loss)'
   @ LINE, 56 SAY NPFT PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Appropriations'
   @ LINE, 56 SAY AMT9 PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   @ LINE, 10 SAY 'Net Profit /(Loss) After Appropriations '
   @ LINE, 56 SAY NPFT-AMT9 PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   @ 23, 1 SAY ''
   WAIT SPACE(10)+'End Of Screen Display  - Press Any Key To Return'
ENDIF
USE pl-work2
ZAP
USE work
ZAP
RUN del abc.dbf
CLOSE DATA
CLEAR

RETURN
***************************

*****************************
PROCEDURE C2_PL
*****************************

CLEAR
CLOSE ALL

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR PROGRAM
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE, TOTCR7, TOTDR7, TOTCR8, TOTDR8, TOT7, TOT8, SWCH
CLEAR
SELECT 1
USE master ORDER mastdate

GOTO BOTTOM
STORE DATE TO MTDATE
CLEAR
@ 2, 5 SAY 'Profit /(Loss) (Or Income) Statement'
@ 5, 5 SAY 'Your last posted voucher is dated '
@ 5, 42 SAY MTDATE
@ 6, 5 SAY 'Do you want to dispaly same date on your Report '
@ 7, 5 SAY 'Or enter the date you want to dispaly  ' GET MTDATE
READ
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='P'
   do PRNCHK in tbl_proc
   CLEAR
   @ 8, 17 SAY 'Profit / Loss (Or Income) Statement'
   @ 10, 17 SAY 'Please use 80 column (10 inches wide) continous'
   @ 11, 17 SAY 'printer paper and set Top Of Form'
   @ 12, 17 SAY 'Press ENTER when ready...........'
   WAIT ''
   @ 14, 17 SAY 'Printing In Progress'
ENDIF
IF P='P'
   CLEAR
   PANS= ' '
   DO prnchk IN tbl_proc.prg
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing In Progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ELSE
   CLEAR
   @ 10, 18 SAY 'Please Wait ... Calculations In Progress '
   SET DEVICE TO SCREEN
ENDIF
STORE SPACE(30) TO WTITLE, WHTITLE
STORE 0 TO AMT6, AMT7, AMT8, AMT9, GPFT, GPFT2
SELECT 2
USE chart
COPY TO abc FOR CHART_NO>5.and.CHART_NO<10
USE abc
INDEX ON CHART_NO TAG cht_idx ADDITIVE
GOTO TOP
M1= FR
M2= TO
SELECT 1
USE pl-work2
ZAP
APPEND FROM ldgrcode FOR C1>=M1.and.C1<=M2
REPLACE FOR CHART_NO=0 CHART_NO WITH 6
SELECT 2
DO WHILE .not. EOF()
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   M3= FR
   M4= TO
   SELECT 1
   IF M3<>0.and.M4<>0
      APPEND FROM ldgrcode FOR C1>=M3.and.C1<=M4
      REPLACE FOR CHART_NO=0 CHART_NO WITH 7
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   M5= FR
   M6= TO
   SELECT 1
   IF M5<>0.and.M6<>0
      APPEND FROM ldgrcode FOR C1>=M5.and.C1<=M6
      REPLACE FOR CHART_NO=0 CHART_NO WITH 8
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   M7= FR
   M8= TO
   SELECT 1
   IF M7<>0.and.M8<>0
      APPEND FROM ldgrcode FOR C1>=M7.and.C1<=M8
      REPLACE FOR CHART_NO=0 CHART_NO WITH 9
   ENDIF
   EXIT
ENDDO
SELECT 3
USE ldgrcode ORDER ac1

SELECT 1
USE pl-work2 ORDER bl_c1

SELECT 2
GOTO TOP
SELECT 1
GOTO TOP
MCODE= 0
HEADING= .T.
PAGE= 1
STORE 0 TO WRECNO, MSRNO, WCODE
WGTITLE= SPACE(30)
STORE 0 TO DRTOT, CRTOT, TCR6, TDR6, GTDR, GTCR, LINE, TOTDR, TOTCR
STORE 0 TO TCR7, TDR7, TCR8, TDR8, TCR9, TDR9
MC1= C1
MC2= C2
MC3= C3
CLEAR
DO WHILE .not. EOF()
   MCHART= CHART_NO
   STORE 0 TO MTOTDR, MTOTCR
   CHD= 0
   DO WHILE CHART_NO=MCHART
      MCC1= C1
      MCC2= C2
      STORE 0 TO TDR, TCR
      IF EOF()
         EXIT
      ENDIF
      IF HEADING
         IF P='P'
			do PRNCHK in tbl_proc
            DO top_prt1 IN tbl_proc.prg
         ELSE
            DO screen IN tbl_proc.prg
         ENDIF
         @ 2, 4 SAY 'PROFIT / LOSS STATEMENT FOR THE PERIOD ENDING ON:'
         @ 2, 53 SAY MTDATE
         @ 2, 72 SAY 'PAGE: '
         @ 2, 78 SAY PAGE PICTURE '99'
         @ 3, 1 SAY REPLICATE('-',79)
         @ 4, 1 SAY 'CODE-----'
         @ 4, 11 SAY '   ---- ACCOUNT  TITLE ----'
         @ 4, 62 SAY 'AMOUNT'
         @ 5, 1 SAY REPLICATE('-',79)
         LINE= 6
         HEADING= .F.
      ENDIF
      IF C3=0
         SKIP
         LOOP
      ENDIF
      LINE= LINE+1
      STORE SPACE(30) TO WTITLE
      STORE TITLE TO WTITLE
      IF CHD=0
         STORE CHART_NO*10 TO MC1
         STORE 0 TO MD2, MD3
         STORE STR(MC1,2)+STR(MD2,2)+STR(MD3,3) TO MLCODE
         SELECT 3
         SEEK MLCODE
         IF .not. FOUND()
            SELECT 2
            HD= AT('Head',CHART)
            @ LINE, 5 SAY LEFT(CHART,HD-1)
            LINE= LINE+1
            @ LINE, 5 SAY REPLICATE('-',LEN(RTRIM(LEFT(CHART,HD-1))))
            LINE= LINE+1
         ELSE
            SELECT 3
            @ LINE, 5 SAY TITLE
            LINE= LINE+1
            @ LINE, 5 SAY REPLICATE('-',LEN(RTRIM(TITLE)))
            LINE= LINE+1
         ENDIF
         SELECT 1
         CHD= 1
      ENDIF
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MCC1))))+LTRIM(STR(MCC1))
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MCC2))))+LTRIM(STR(MCC2))
      SELECT 1
      DO WHILE C1=MCC1.and.C2=MCC2
         IF STATUS='DR'
            TDR= TDR+LDGR_BAL
            MTOTDR= MTOTDR+LDGR_BAL
         ELSE
            TCR= TCR+LDGR_BAL
            MTOTCR= MTOTCR+LDGR_BAL
         ENDIF
         MSTAT= STATUS
         SKIP
      ENDDO
      @ LINE, 1 SAY MPC1 PICTURE '99'
      @ LINE, 3 SAY '-'
      @ LINE, 4 SAY MPC2 PICTURE '99'
      SELECT 3
      MDC3= 0
      SEEK STR(MCC1,2)+STR(MCC2,2)+STR(MDC3,3)
      IF FOUND()
         @ LINE, 14 SAY TITLE
      ELSE
         @ LINE, 14 SAY 'Group Total'
      ENDIF
      SELECT 1
      IF MCHART=6
         IF TCR>=TDR
            @ LINE, 56 SAY TCR-TDR PICTURE '@( 99,999,999,999.99'
         ELSE
            @ LINE, 56 SAY TDR* -1-TCR PICTURE '@( 99,999,999,999.99'
         ENDIF
      ENDIF
      IF MCHART>6
         IF TDR>=TCR
            @ LINE, 56 SAY TDR-TCR PICTURE '@( 99,999,999,999.99'
         ELSE
            @ LINE, 56 SAY TCR* -1-TDR PICTURE '@( 99,999,999,999.99'
         ENDIF
      ENDIF
      IF P='S'.and.LINE>=16
         ANS= ' '
         DO WHILE .not. ANS$'CQ'

IF _DOS
            SET COLOR TO w+
ENDIF

            SET CONFIRM OFF
            @ 24, 20 SAY 'Please Enter <C> To Continue <Q> To Quit' GET ANS PICTURE '!'
            READ

IF _DOS
            SET COLOR TO 
ENDIF

         ENDDO
         IF ANS='C'
            CLEAR
            PAGE= PAGE+1
            HEADING= .T.
            LINE= 6
         ELSE
            CLOSE DATA
            CLEAR
            RETURN
         ENDIF
      ENDIF
      IF P='P'.and.LINE>=56
         PAGE= PAGE+1
         HEADING= .T.
      ENDIF
   ENDDO
   LINE= LINE+1
   IF MCHART=6
      AMT6= MTOTCR-MTOTDR
      @ LINE, 55 SAY REPLICATE('-',20)
      LINE= LINE+1
      @ LINE, 10 SAY 'Total '
      @ LINE, 56 SAY AMT6 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
   ENDIF
   IF MCHART=7
      AMT7= MTOTDR-MTOTCR
      @ LINE, 55 SAY REPLICATE('-',20)
      LINE= LINE+1
      @ LINE, 10 SAY 'Total '
      @ LINE, 56 SAY AMT7 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
   ENDIF
   NGPFT= AMT6-AMT7
   IF AMT7<>0.and.AMT8=0.and.GPFT=0
      LINE= LINE+1
      @ LINE, 10 SAY 'Gross Profit/(Loss)'
      @ LINE, 56 SAY NGPFT PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
      GPFT= 1
   ENDIF
   IF P='S'.and.LINE>=16
      ANS= ' '
      DO WHILE .not. ANS$'CQ'

IF _DOS
         SET COLOR TO w+
ENDIF

         SET CONFIRM OFF
         @ 24, 20 SAY 'Please Enter <C> To Continue <Q> To Quit' GET ANS PICTURE '!'
         READ

IF _DOS
         SET COLOR TO 
ENDIF

      ENDDO
      IF ANS='C'
         CLEAR
         PAGE= PAGE+1
         HEADING= .T.
         LINE= 6
      ELSE
         CLOSE DATA
         CLEAR
         RETURN
      ENDIF
   ENDIF
   IF P='P'.and.LINE>=56
      PAGE= PAGE+1
      HEADING= .T.
   ENDIF
   IF MCHART=8
      AMT8= MTOTDR-MTOTCR
      @ LINE, 55 SAY REPLICATE('-',20)
      LINE= LINE+1
      @ LINE, 10 SAY 'Total '
      @ LINE, 56 SAY AMT8 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
   ENDIF
   NPFT= NGPFT-AMT8
   IF NGPFT<>0.and.AMT8<>0.and.GPFT2=0
      @ LINE, 10 SAY 'Net Profit /(Loss)'
      @ LINE, 56 SAY NPFT PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
      GPFT2= 1
   ENDIF
   IF MCHART=9
      AMT9= MTOTDR-MTOTCR
      @ LINE, 55 SAY REPLICATE('-',20)
      LINE= LINE+1
      @ LINE, 10 SAY 'Total '
      @ LINE, 56 SAY AMT9 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
   ENDIF
   IF AMT9<>0
      @ LINE, 10 SAY 'Net Profit /(Loss) After Appropriations '
      @ LINE, 56 SAY NPFT-AMT9 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   SELECT 1
ENDDO
IF P='P'
   EJECT
   LINE= 2
   @ LINE, 4 SAY 'PROFIT / LOSS STATEMENT '
   @ LINE, 28 SAY 'SUMMARY'
   @ LINE, 50 SAY 'Up TO'
   @ LINE, 56 SAY MTDATE
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Sales'
   @ LINE, 56 SAY AMT6 PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Cost Of Sales'
   @ LINE, 56 SAY AMT7 PICTURE '@( 99,999,999,999.99'
   NGPFT= AMT6-AMT7
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   @ LINE, 10 SAY 'Gross Profit/(Loss)'
   @ LINE, 56 SAY NGPFT PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Expenses'
   @ LINE, 56 SAY AMT8 PICTURE '@( 99,999,999,999.99'
   NPFT= NGPFT-AMT8
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   @ LINE, 10 SAY 'Net Profit /(Loss)'
   @ LINE, 56 SAY NPFT PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Appropriations'
   @ LINE, 56 SAY AMT9 PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   @ LINE, 10 SAY 'Net Profit /(Loss) After Appropriations '
   @ LINE, 56 SAY NPFT-AMT9 PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   @ 23, 1 SAY ''
   WAIT SPACE(20)+'Press Any Key For Summary'
   CLEAR
   LINE= 2
   @ LINE, 4 SAY 'PROFIT / LOSS STATEMENT '
   @ LINE, 28 SAY 'SUMMARY'
   @ LINE, 50 SAY 'Up To'
   @ LINE, 56 SAY MTDATE
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Sales'
   @ LINE, 56 SAY AMT6 PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Cost Of Sales'
   @ LINE, 56 SAY AMT7 PICTURE '@( 99,999,999,999.99'
   NGPFT= AMT6-AMT7
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   @ LINE, 10 SAY 'Gross Profit/(Loss)'
   @ LINE, 56 SAY NGPFT PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Expenses'
   @ LINE, 56 SAY AMT8 PICTURE '@( 99,999,999,999.99'
   NPFT= NGPFT-AMT8
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   @ LINE, 10 SAY 'Net Profit /(Loss)'
   @ LINE, 56 SAY NPFT PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   LINE= LINE+1
   @ LINE, 10 SAY 'Total Appropriations'
   @ LINE, 56 SAY AMT9 PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 56 SAY REPLICATE('-',18)
   LINE= LINE+1
   @ LINE, 10 SAY 'Net Profit /(Loss) After Appropriations '
   @ LINE, 56 SAY NPFT-AMT9 PICTURE '@( 99,999,999,999.99'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   @ 23, 1 SAY ''
   WAIT SPACE(10)+'End Of Screen Display  - Press Any Key To Return'
ENDIF
CLOSE DATA
RUN del abc.dbf
CLEAR

RETURN
*************************

***************************
PROCEDURE REP_PL2
***************************

CLEAR
CLOSE ALL

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
STORE 0 TO TCR, TDR, LINE, TOTCR7, TOTDR7, TOTCR8, TOTDR8, TOT7, TOT8
STORE 0 TO DRTOT, CRTOT, SWCH, M1, M2, M3, MTOT, TOTL, TOTG, PTOT, AMT
HEADING= .T.
PAGE= 1
DO WHILE .T.
   USE master
   @ 2, 2 SAY 'Profit / Loss (Or Income) Statement'
   CLEAR
   @ 5, 25 SAY 'Profit / Loss Between Two Dates '
   STORE DATE() TO DD1, DD2
   @ 10, 25 SAY 'From Date ' GET DD1
   @ 12, 25 SAY 'To   Date ' GET DD2
   READ
   IF READKEY()=12
      CLOSE DATA
      CLEAR
      RETURN
   ENDIF
   LOCATE FOR DATE>=DD1.and.DATE<=DD2
   IF .not. FOUND()

IF _DOS
      SET COLOR TO w+
ENDIF

      @ 16, 20 SAY 'No Transactions Exist Between These Dates'

IF _DOS
      SET COLOR TO 
ENDIF

      WAIT ''
      @ 16, 20 SAY '                                          '
      LOOP
   ENDIF
   EXIT
ENDDO
P= ' '
DO WHILE .not. P$'SPQ'
   @ 23, 45 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='P'
   do PRNCHK in tbl_proc
   CLEAR
   @ 8, 17 SAY 'Profit / Loss (Or Income) Statement'
   @ 10, 17 SAY 'Please use 80 column (10 inches wide) continous'
   @ 11, 17 SAY 'printer paper and set Top Of Form'
   @ 12, 17 SAY 'Press ENTER when ready...........'
   WAIT ''
ENDIF
IF P='P'
   CLEAR
   PANS= ' '
   DO prnchk IN tbl_proc.prg
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing In Progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ELSE
   CLEAR
   SET DEVICE TO SCREEN
   @ 10, 18 SAY 'Please Wait ... Calculation In Progress '
ENDIF
MCODE= 0
HEADING= .T.
PAGE= 1
STORE 0 TO WRECNO, MSRNO, WCODE, AMT6, AMT7, AMT8, AMT9
MM= SPACE(30)
SELECT 2
USE chart
COPY TO abc FOR CHART_NO>5.and.CHART_NO<10
USE abc
INDEX ON CHART_NO TAG cht_idx ADDITIVE
GOTO TOP
M1= FR
M2= TO
SELECT 3
USE ldgrcode ORDER ac1
SELECT 1
USE work
ZAP
APPEND FROM master FOR DATE>=DD1.and.DATE<=DD2.and.C1>=M1.and.C1<=M2
REPLACE FOR CHART_NO=0 CHART_NO WITH 6
SELECT 2
DO WHILE .not. EOF()
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   M3= FR
   M4= TO
   SELECT 1
   IF M3<>0.and.M4<>0
      APPEND FROM master FOR DATE>=DD1.and.DATE<=DD2.and.C1>=M3.and.C1<=M4
      REPLACE FOR CHART_NO=0 CHART_NO WITH 7
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   M5= FR
   M6= TO
   SELECT 1
   IF M5<>0.and.M6<>0
      APPEND FROM master FOR DATE>=DD1.and.DATE<=DD2.and.C1>=M5.and.C1<=M6
      REPLACE FOR CHART_NO=0 CHART_NO WITH 8
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   M7= FR
   M8= TO
   SELECT 1
   M7= 0
   IF M7<>0.and.M8<>0
      APPEND FROM master FOR DATE>=DD1.and.DATE<=DD2.and.C1>=M7.and.C1<=M8
      REPLACE FOR CHART_NO=0 CHART_NO WITH 9
   ENDIF
   EXIT
ENDDO
SELECT 1
USE work
INDEX ON STR(CHART_NO,2)+STR(C1,2)+STR(C2,2)+STR(C3,3) TAG acwork ADDITIVE
CLEAR
STORE SPACE(30) TO WTITLE, WHTITLE, MRT
SELECT 1
SELECT 2
GOTO TOP
SELECT 1
DO WHILE .not. EOF()
   SELECT 1
   STORE 0 TO CRTOT, DRTOT, CHD
   STORE CHART_NO TO MCHART
   DO WHILE CHART_NO=MCHART
      IF DR_AMOUNT=0.and.CR_AMOUNT=0.and.BL_AMOUNT=0
         IF .not. EOF()
            SKIP
         ENDIF
         LOOP
      ENDIF
      STORE 0 TO DRTOT, CRTOT
      MC1= C1
      MC2= C2
      MC3= C3
      STORE STR(C1,2)+STR(C2,2)+STR(C3,3) TO MLCODE
      DO WHILE C1=MC1.and.C2=MC2.and.C3=MC3
         IF CR_AMOUNT=0
            DRTOT= DRTOT+DR_AMOUNT
         ENDIF
         IF DR_AMOUNT=0
            CRTOT= CRTOT+CR_AMOUNT
         ENDIF
         IF .not. EOF()
            SKIP
         ENDIF
      ENDDO
      SELECT 3
      SEEK MLCODE
      IF FOUND()
         STORE TITLE TO MTITLE
      ELSE
         STORE 'Code Does Not Exists in Ledger' TO MTITLE
      ENDIF
      SELECT 1
      IF HEADING
         CLEAR
         IF P='P'
			do PRNCHK in tbl_proc
            DO top_prt1 IN tbl_proc.prg
         ELSE
            DO screen IN tbl_proc.prg
         ENDIF
         @ 2, 4 SAY 'PROFIT / LOSS STATEMENT   From '
         @ 2, 36 SAY DD1
         @ 2, 46 SAY 'To'
         @ 2, 49 SAY DD2
         @ 2, 72 SAY 'PAGE: '
         @ 2, 78 SAY PAGE PICTURE '99'
         @ 3, 1 SAY REPLICATE('-',79)
         @ 4, 1 SAY 'CODE-----'
         @ 4, 11 SAY '   ---- ACCOUNT  TITLE ----'
         @ 4, 50 SAY '   AMOUNT  '
         @ 5, 1 SAY REPLICATE('-',79)
         LINE= 6
         HEADING= .F.
      ENDIF
      IF CHD=0
         STORE CHART_NO*10 TO MD1
         STORE 0 TO MD2, MD3
         STORE STR(MD1,2)+STR(MD2,2)+STR(MD3,3) TO MLCODE
         SELECT 3
         SEEK MLCODE
         IF .not. FOUND()
            SELECT 2
            HD= AT('Head',CHART)
            @ LINE, 5 SAY LEFT(CHART,HD-1)
            LINE= LINE+1
            @ LINE, 5 SAY REPLICATE('-',LEN(RTRIM(LEFT(CHART,HD-1))))
            LINE= LINE+1
         ELSE
            SELECT 3
            @ LINE, 5 SAY TITLE
            LINE= LINE+1
            @ LINE, 5 SAY REPLICATE('-',LEN(RTRIM(TITLE)))
            LINE= LINE+1
         ENDIF
         SELECT 1
         CHD= 1
      ENDIF
      AMT= 0
      IF MCHART=6
         AMT= CRTOT-DRTOT
      ELSE
         AMT= DRTOT-CRTOT
      ENDIF
      IF AMT=0
         SKIP
         LOOP
      ENDIF
      LINE= LINE+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ LINE, 1 SAY MPC1
      @ LINE, 3 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ LINE, 4 SAY MPC2
      @ LINE, 6 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ LINE, 7 SAY MPC3
      @ LINE, 14 SAY MTITLE
      IF MCHART=6
         @ LINE, 55 SAY AMT PICTURE '@( 999,999,999,999.99'
         AMT6= AMT+AMT6
      ENDIF
      IF MCHART>6
         @ LINE, 55 SAY AMT PICTURE '@( 999,999,999,999.99'
         IF MCHART=7
            AMT7= AMT+AMT7
         ENDIF
         IF MCHART=8
            AMT8= AMT+AMT8
         ENDIF
         IF MCHART=9
            AMT9= AMT+AMT9
         ENDIF
      ENDIF
      IF P='S'.and.LINE>=16
         ANS= ' '
         DO WHILE .not. ANS$'CQ'

IF _DOS
            SET COLOR TO w+
ENDIF

            SET CONFIRM OFF
            @ 24, 20 SAY 'Please Enter <C> To Continue <Q> To Quit' GET ANS PICTURE '!'
            READ

IF _DOS
            SET COLOR TO 
ENDIF

         ENDDO
         IF ANS='C'
            LINE= 6
            PAGE= PAGE+1
            HEADING= .T.
         ELSE
            CLOSE DATA
            CLEAR
            RETURN
         ENDIF
      ENDIF
      IF P='P'.and.LINE>=56
         PAGE= PAGE+1
         HEADING= .T.
         EJECT
      ENDIF
   ENDDO
   IF HEADING
      CLEAR
      IF P='P'
		 do PRNCHK in tbl_proc
         DO top_prt1 IN tbl_proc.prg
      ELSE
         DO screen IN tbl_proc.prg
      ENDIF
      @ 2, 4 SAY 'PROFIT / LOSS STATEMENT   From '
      @ 2, 36 SAY DD1
      @ 2, 46 SAY 'To'
      @ 2, 49 SAY DD2
      @ 2, 72 SAY 'PAGE: '
      @ 2, 78 SAY PAGE PICTURE '99'
      @ 3, 1 SAY REPLICATE('-',79)
      @ 4, 1 SAY 'CODE-----'
      @ 4, 11 SAY '   ---- ACCOUNT  TITLE ----'
      @ 4, 50 SAY '   AMOUNT  '
      @ 5, 1 SAY REPLICATE('-',79)
      LINE= 6
      HEADING= .F.
   ENDIF
   LINE= LINE+1
   IF MCHART=6
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
      @ LINE, 10 SAY 'Total Sales'
      @ LINE, 56 SAY AMT6 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
   ENDIF
   IF MCHART=7
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
      @ LINE, 10 SAY 'Total Cost Of Sales'
      @ LINE, 56 SAY AMT7 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
   ENDIF
   NGPFT= AMT6-AMT7
   IF AMT7<>0.and.AMT8=0
      LINE= LINE+1
      @ LINE, 10 SAY 'Gross Profit'
      @ LINE, 56 SAY NGPFT PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
   ENDIF
   IF MCHART=8
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
      @ LINE, 10 SAY 'Total Expenses'
      @ LINE, 56 SAY AMT8 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
   ENDIF
   NPFT= NGPFT-AMT8
   IF NGPFT<>0.and.AMT8<>0.and.AMT9=0
      @ LINE, 10 SAY 'Net Profit /(Loss)'
      @ LINE, 56 SAY NPFT PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
   ENDIF
   IF MCHART=9
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
      @ LINE, 10 SAY 'total Appropriation'
      @ LINE, 56 SAY AMT9 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
   ENDIF
   IF AMT9<>0
      @ LINE, 10 SAY 'Net Profit /(Loss) After Appropriation '
      @ LINE, 56 SAY NPFT-AMT9 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
   ENDIF
   LINE= LINE+1
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   SELECT 1
ENDDO
IF P='P'
   EJECT
   LINE= 2
   @ LINE, 4 SAY 'PROFIT / LOSS STATEMENT '
   @ LINE, 28 SAY 'SUMMARY'
   @ LINE, 50 SAY 'From'
   @ LINE, 56 SAY DD1
   @ LINE, 66 SAY 'To'
   @ LINE, 69 SAY DD2
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   IF AMT6<>0
      LINE= LINE+1
      @ LINE, 10 SAY 'Total Sales'
      @ LINE, 56 SAY AMT6 PICTURE '@( 99,999,999,999.99'
   ENDIF
   IF AMT7<>0
      LINE= LINE+1
      @ LINE, 10 SAY 'Total Cost Of Sales'
      @ LINE, 56 SAY AMT7 PICTURE '@( 99,999,999,999.99'
   ENDIF
   NGPFT= AMT6-AMT7
   IF AMT7<>0
      LINE= LINE+1
      @ LINE, 56 SAY REPLICATE('-',18)
      LINE= LINE+1
      @ LINE, 10 SAY 'Gross Profit'
      @ LINE, 56 SAY NGPFT PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 56 SAY REPLICATE('-',18)
      LINE= LINE+1
   ENDIF
   IF AMT8<>0
      LINE= LINE+1
      @ LINE, 10 SAY 'Total Expenses'
      @ LINE, 56 SAY AMT8 PICTURE '@( 99,999,999,999.99'
   ENDIF
   NPFT= NGPFT-AMT8
   IF NGPFT<>0.and.AMT8<>0
      LINE= LINE+1
      @ LINE, 56 SAY REPLICATE('-',18)
      LINE= LINE+1
      @ LINE, 10 SAY 'Net Profit /(Loss)'
      @ LINE, 56 SAY NPFT PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 56 SAY REPLICATE('-',18)
      LINE= LINE+1
   ENDIF
   IF AMT9<>0
      LINE= LINE+1
      @ LINE, 10 SAY 'Total Appropriations'
      @ LINE, 56 SAY AMT9 PICTURE '@( 99,999,999,999.99'
   ENDIF
   IF AMT9<>0
      LINE= LINE+1
      @ LINE, 56 SAY REPLICATE('-',18)
      LINE= LINE+1
      @ LINE, 10 SAY 'Net Profit /(Loss) After Appropriation '
      @ LINE, 56 SAY NPFT-AMT9 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
   ENDIF
   LINE= LINE+1
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   @ 23, 1 SAY ''
   WAIT SPACE(20)+'Press Any Key For Summary'
   CLEAR
   LINE= 2
   @ LINE, 4 SAY 'PROFIT / LOSS STATEMENT '
   @ LINE, 28 SAY 'SUMMARY'
   @ LINE, 50 SAY 'From'
   @ LINE, 56 SAY DD1
   @ LINE, 66 SAY 'To'
   @ LINE, 69 SAY DD2
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   IF AMT6<>0
      LINE= LINE+1
      @ LINE, 10 SAY 'Total Sales'
      @ LINE, 56 SAY AMT6 PICTURE '@( 99,999,999,999.99'
   ENDIF
   IF AMT7<>0
      LINE= LINE+1
      @ LINE, 10 SAY 'Total Cost Of Sales'
      @ LINE, 56 SAY AMT7 PICTURE '@( 99,999,999,999.99'
   ENDIF
   NGPFT= AMT6-AMT7
   IF AMT7<>0
      LINE= LINE+1
      @ LINE, 56 SAY REPLICATE('-',18)
      LINE= LINE+1
      @ LINE, 10 SAY 'Gross Profit/(Loss)'
      @ LINE, 56 SAY NGPFT PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 56 SAY REPLICATE('-',18)
      LINE= LINE+1
   ENDIF
   IF AMT8<>0
      LINE= LINE+1
      @ LINE, 10 SAY 'Total Expenses'
      @ LINE, 56 SAY AMT8 PICTURE '@( 99,999,999,999.99'
   ENDIF
   NPFT= NGPFT-AMT8
   IF NGPFT<>0.and.AMT8<>0
      LINE= LINE+1
      @ LINE, 56 SAY REPLICATE('-',18)
      LINE= LINE+1
      @ LINE, 10 SAY 'Net Profit /(Loss)'
      @ LINE, 56 SAY NPFT PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 56 SAY REPLICATE('-',18)
      LINE= LINE+1
   ENDIF
   IF AMT9<>0
      LINE= LINE+1
      @ LINE, 10 SAY 'Total Appropriations'
      @ LINE, 56 SAY AMT9 PICTURE '@( 99,999,999,999.99'
   ENDIF
   IF AMT9<>0
      LINE= LINE+1
      @ LINE, 56 SAY REPLICATE('-',18)
      LINE= LINE+1
      @ LINE, 10 SAY 'Net Profit /(Loss) After Appropriation '
      @ LINE, 56 SAY NPFT-AMT9 PICTURE '@( 99,999,999,999.99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
   ENDIF
   @ 23, 1 SAY ''
   WAIT SPACE(10)+'End Of Screen Display  - Press Any Key To Return'
   LINE= LINE+1
ENDIF
CLOSE DATA
CLEAR

RETURN
************************

