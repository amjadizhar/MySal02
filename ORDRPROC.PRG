* INVENTORY
* PROGRAM: ORDRPROC.PRG
* WRITTEN BY : AMJAD iZHAR
* THIS REVISION: 17-05-2005
*---------------------------------------

**********************
PROCEDURE DORD_ENT
**********************


CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE s-order
INDEX ON ORDER_NO TAG sordidx
COPY TO ord-work STRUCTURE
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG ac1
SELECT 5
USE ord-work
DO WHILE .T.
   CLEAR
   R= 3
   @ 1, 25 SAY ' ORDER FROM DEALERS/DEBTORS  ENTRY'
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MORDER_NO, MC1, MC2, MC3
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 23, 45 CLEAR TO 23, 78
      IF MORDER_NO=0
         LOOP
      ENDIF
      MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
      SET COLOR TO /g
ENDIF

      @ R, 15 SAY MVCH_NO

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 1
      SEEK MORDER_NO
      IF FOUND()
         @ 23, 1 SAY SPACE(78)
         @ 23, 15 SAY 'This Order No Already Exists  -- Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 
ENDIF

         @ 23, 10 SAY SPACE(75)
         LOOP
      ENDIF
      SELECT 5
      SET EXCLUSIVE ON
      ZAP
      @ R, 28 SAY 'Date ' GET MDATE
      READ
      DO chk IN TBL_PROC.PRG
      @ R, 45 SAY 'Delivery Deadline ' GET MDEADLINE
      READ
      IF MDEADLINE<MDATE
         LOOP
      ENDIF
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Party Code'

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 15 SAY '  -  -   '

IF _DOS
         SET COLOR TO 
ENDIF

         @ R, 15 GET MC1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO n/g
ENDIF

         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ R, 15 SAY MPC1
         @ R, 17 SAY '-'
         @ R, 18 GET MC2 PICTURE '99'
         READ
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ R, 18 SAY MPC2
         @ R, 20 SAY '-'
         @ R, 21 GET MC3 PICTURE '999'
         READ
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ R, 21 SAY MPC3

IF _DOS
         SET COLOR TO 
ENDIF

         SELECT 3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SEEK MCODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 28 SAY TITLE
         MTITLE= TITLE

IF _DOS
         SET COLOR TO 
ENDIF

         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 3 SAY 'Item Code'
      @ R, 15 SAY 'Item '
      @ R, 47 SAY 'Qty Ordered'
      @ R, 62 SAY 'Rate '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0
            @ 23, 50 SAY 'Press Ctrl Q To Quit '

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 3 SAY '  -  -    '

IF _DOS
            SET COLOR TO 
ENDIF

            @ R, 3 GET MI1 PICTURE '99'
            READ
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            @ 23, 45 CLEAR TO 23, 78

IF _DOS
            SET COLOR TO n/g
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 3 SAY MP1
            @ R, 5 SAY '-'
            @ R, 6 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 6 SAY MP2
            @ R, 8 SAY '-'
            @ R, 9 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 9 SAY MP3

IF _DOS
            SET COLOR TO 
ENDIF

            @ 23, 50 CLEAR TO 23, 78
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            IF MI1=0.and.MI2=0.and.MI3=0
               LOOP
            ENDIF
            SELECT 2
            STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
            SEEK MI_CODE
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 15 SAY ITEM

IF _DOS
            SET COLOR TO 
ENDIF

            ABC= 1
         ENDDO
         DO WHILE .T.
            @ R, 47 GET MQTY_ORD PICTURE '9999999.9999'
            READ
            IF MQTY_ORD=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 61 GET MRATE PICTURE '999999999.9999'
         READ
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'Y'
            @ R, 79 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 5
         APPEND BLANK
         REPLACE ORDER_NO WITH MORDER_NO, DATE WITH MDATE, DEADLINE WITH MDEADLINE
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3, QTY_ORD WITH MQTY_ORD
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE
         IF ANS='Y'
            STORE 0 TO MI1, MI2, MI3, MRATE, MQTY_ORD
            R= R+1
            LOOP
         ENDIF
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
            READ
         ENDDO
         @ 24, 20 CLEAR TO 24, 78
         IF ANS='Y'
            DO ord-cor
         ENDIF
         SELECT 5
         USE
         SELECT 1
         APPEND FROM ord-work
         SELECT 5
         USE ord-work
         ZAP
         EXIT
      ENDDO
      ANS= ' '
      DO WHILE .not. ANS$'YN'
         @ 24, 25 SAY 'More Data Entry - Y/N' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
***********

**************************
PROCEDURE DORD_COR
*****************************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE s-order EXCLUSIVE
INDEX ON ORDER_NO TAG ordidx
COPY TO ord-work STRUCTURE
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG ac1
SELECT 5
USE ord-work
DO WHILE .T.
   CLEAR
   R= 3
   @ 1, 25 SAY ' ORDER FROM DEALERS/DEBTORS CORRECTION'
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MORDER_NO, MC1, MC2, MC3
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MORDER_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
   SET COLOR TO /g
ENDIF

   @ R, 15 SAY MVCH_NO

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK MORDER_NO
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Order No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   SELECT 1
   USE
   SELECT 5
   ZAP
   APPEND FROM s-order FOR ORDER_NO=MORDER_NO
   SELECT 1
   USE s-order ORDER ordidx
   SELECT 5
   GOTO TOP
   MDATE= DATE
   MDEADLINE= DEADLINE
   @ R, 28 SAY 'Date ' GET MDATE
   @ R, 45 SAY 'Delivery Deadline ' GET MDEADLINE
   READ
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 5
      MC1= C1
      MC2= C2
      MC3= C3

IF _DOS
      SET COLOR TO 
ENDIF

      @ R, 2 SAY 'Party Code'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 15 SAY '  -  -   '
      @ R, 15 GET MC1 PICTURE '99'
      READ

IF _DOS
      SET COLOR TO n/g
ENDIF

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 17 SAY '-'
      @ R, 18 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2
      @ R, 20 SAY '-'
      @ R, 21 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 21 SAY MPC3

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 3
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 28 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 3 SAY 'Item Code'
   @ R, 15 SAY 'Item '
   @ R, 47 SAY 'Qty Ordered'
   @ R, 62 SAY 'Rate '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   DO ord_cor
   SELECT 5
   GOTO TOP
   REPLACE ALL DATE WITH MDATE, DEADLINE WITH MDEADLINE, C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
   USE
   SELECT 1
   DELETE ALL FOR ORDER_NO=MORDER_NO
   PACK
   APPEND FROM ord-work
   SELECT 5
   USE ord-work
   ZAP
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'More Data Correction - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
*******
**********************
PROCEDURE DORD_DEL
***********************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE s-order
INDEX ON ORDER_NO TAG ordidx
COPY TO ord-work STRUCTURE
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG ac1
SELECT 5
USE ord-work
DO WHILE .T.
   CLEAR
   R= 3
   @ 1, 25 SAY ' ORDER FROM DEALERS/DEBTORS DELETION '
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MORDER_NO, MC1, MC2, MC3
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MORDER_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
   SET COLOR TO /g
ENDIF

   @ R, 15 SAY MVCH_NO

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK MORDER_NO
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Order No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   SELECT 1
   USE
   SELECT 5
   ZAP
   APPEND FROM s-order FOR ORDER_NO=MORDER_NO
   SELECT 1
   USE s-order ORDER ordidx
   SELECT 5
   GOTO TOP
   MDATE= DATE
   MDEADLINE= DEADLINE
   @ R, 28 SAY 'Date ' GET MDATE
   @ R, 45 SAY 'Delivery Deadline ' GET MDEADLINE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 5
      MC1= C1
      MC2= C2
      MC3= C3
      @ R, 2 SAY 'Party Code'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 15 SAY '  -  -   '
      @ R, 15 SAY MC1 PICTURE '99'

IF _DOS
      SET COLOR TO n/g
ENDIF

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 17 SAY '-'
      @ R, 18 SAY MC2 PICTURE '99'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2
      @ R, 20 SAY '-'
      @ R, 21 SAY MC3 PICTURE '999'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 21 SAY MPC3

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 3
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 28 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 3 SAY 'Item Code'
   @ R, 15 SAY 'Item '
   @ R, 47 SAY 'Qty Ordered'
   @ R, 62 SAY 'Rate '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   CLEAR GETS
   DO ord_dsp
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   IF ANS='Y'
      SELECT 1
      DELETE ALL FOR ORDER_NO=MORDER_NO
      PACK
      SELECT 5
      ZAP
   ENDIF
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Any More Deletion   - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
***********

**********************
PROCEDURE DORD_PRT
**********************
CLEAR
CLOSE ALL
SELECT 1
USE s-order
INDEX ON ORDER_NO TAG ordidx
COPY TO ord-work STRUCTURE
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG ac1
SELECT 5
USE ord-work
STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO
ABC= 0
DO WHILE ABC=0
   @ 3, 30 SAY 'ORDER FROM DEALERS/DEBTORS '
   @ 4, 30 SAY '-------------------------- '
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ 10, 20 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MORDER_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 10, 33 SAY MVCH_NO

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK MORDER_NO
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Order No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 1
   USE
   SELECT 5
   ZAP
   APPEND FROM s-order FOR ORDER_NO=MORDER_NO
   SELECT 1
   USE p-order ORDER ord_idx
   SELECT 5
   GOTO TOP
   IF RECCOUNT()=0
      CLEAR
      @ 12, 12 SAY 'No Order Exist in Data File  -  Press any key to return to menu .... '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   ABC= 1
ENDDO
SELECT 5
GOTO TOP
MDATE= DATE
MDEADLINE= DEADLINE
MC1= C1
MC2= C2
MC3= C3
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
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
L= 3
HEADING= .T.
CNT= 0
DO WHILE .not. EOF()
   SELECT 5
   IF HEADING
      IF P='P'
         DO top-prt
         @ 3, 28 SAY 'ORDER FROM DEALERS/DEBTORS '
         L= L+1
         @ L, 28 SAY '----------------------------'
         L= L+1
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 25 SAY 'ORDER FROM DEALERS/DEBTORS '
         L= L+1
         @ L, 25 SAY '----------------------------'
         L= L+1
      ENDIF
      MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
      @ L, 1 SAY 'Order No   : '
      @ L, 14 SAY MVCH_NO
      @ L, 51 SAY 'Date              : '+DTOC(DATE)
      L= L+1
      @ L, 1 SAY 'Party Code : '
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ L, 14 SAY MPC1
      @ L, 16 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ L, 17 SAY MPC2
      @ L, 19 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ L, 20 SAY MPC3
      SELECT 3
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF FOUND()
         @ L, 24 SAY TITLE
      ENDIF
      SELECT 5
      @ L, 51 SAY 'Delivery Deadline : '+DTOC(DEADLINE)
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 13 SAY 'Item '
      @ L, 38 SAY 'Unit'
      @ L, 43 SAY 'Qty Ordered'
      @ L, 62 SAY 'Rate '
      @ L, 73 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 5
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
   @ L, 7 SAY MP3
   STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
   SELECT 2
   SEEK MI_CODE
   IF FOUND()
      MUNIT= UNIT
      @ L, 13 SAY ITEM
   ENDIF
   SELECT 5
   MQTY= QTY_ORD
   MRATE= RATE
   MAMT= MQTY*MRATE
   @ L, 38 SAY MUNIT
   @ L, 41 SAY MQTY PICTURE '9999999.9999'
   @ L, 54 SAY MRATE PICTURE '99999999.9999'
   @ L, 67 SAY MAMT PICTURE '999999999.99'
   MTOT= MTOT+MAMT
   L= L+1
   SKIP
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
   IF P='P'.and.L>56
      L= 3
      HEADING= .T.
   ENDIF
ENDDO
@ L, 50 SAY REPLICATE('-',30)
L= L+1
@ L, 54 SAY 'Total : '
@ L, 67 SAY MTOT PICTURE '999999999.99'
L= L+1
@ L, 50 SAY REPLICATE('-',30)
L= L+1
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
***********

********************
PROCEDURE ORD_COR
**********************
SELECT 5
GOTO TOP
DO ord_dsp
SELECT 5
GOTO TOP
R= 7
CK= 0
DO WHILE .T.
   R= R+1
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 5
      DELETE
      PACK
      IF EOF()
         EXIT
      ENDIF
   ENDIF
   IF MQES='N'
      SELECT 5
      IF .not. EOF()
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 5
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE 0 TO MI1, MI2, MI3, MQTY, MRATE, MAMOUNT, MQTY_ORD
         DO WHILE .T.

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 3 SAY '  -  -    '

IF _DOS
            SET COLOR TO 
ENDIF

            @ R, 3 GET MI1 PICTURE '99'
            READ

IF _DOS
            SET COLOR TO n/g
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 3 SAY MP1
            @ R, 5 SAY '-'
            @ R, 6 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 6 SAY MP2
            @ R, 8 SAY '-'
            @ R, 9 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 9 SAY MP3

IF _DOS
            SET COLOR TO 
ENDIF

            @ 23, 50 CLEAR TO 23, 78
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            IF MI1=0.and.MI2=0.and.MI3=0
               LOOP
            ENDIF
            SELECT 2
            STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
            SEEK MI_CODE
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 15 SAY ITEM

IF _DOS
            SET COLOR TO 
ENDIF

            DO WHILE .T.
               @ R, 47 GET MQTY_ORD PICTURE '9999999.9999'
               READ
               IF MQTY_ORD=0
                  LOOP
               ENDIF
               EXIT
            ENDDO
            @ R, 61 GET MRATE PICTURE '999999999.9999'
            READ
            EXIT
         ENDDO
         SELECT 5
         APPEND BLANK
         REPLACE ORDER_NO WITH MORDER_NO, DATE WITH MDATE, DEADLINE WITH MDEADLINE
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3, QTY_ORD WITH MQTY_ORD
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 5
         MI1= I1
         MI2= I2
         MI3= I3
         MORDER_NO= ORDER_NO
         MQTY_ORD= QTY_ORD
         MRATE= RATE

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 3 SAY '  -  -    '

IF _DOS
         SET COLOR TO 
ENDIF

         @ R, 3 GET MI1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO n/g
ENDIF

         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ R, 3 SAY MP1
         @ R, 5 SAY '-'
         @ R, 6 GET MI2 PICTURE '99'
         READ
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ R, 6 SAY MP2
         @ R, 8 SAY '-'
         @ R, 9 GET MI3 PICTURE '9999'
         READ
         IF MI3=0
            LOOP
         ENDIF
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ R, 9 SAY MP3

IF _DOS
         SET COLOR TO 
ENDIF

         @ 23, 50 CLEAR TO 23, 78
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         IF MI1=0.and.MI2=0.and.MI3=0
            LOOP
         ENDIF
         SELECT 2
         STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
         SEEK MI_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 15 SAY ITEM

IF _DOS
         SET COLOR TO 
ENDIF

         DO WHILE .T.
            @ R, 47 GET MQTY_ORD PICTURE '9999999.9999'
            READ
            IF MQTY_ORD=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 61 GET MRATE PICTURE '999999999.9999'
         READ
         EXIT
      ENDDO
      SELECT 5
      REPLACE ORDER_NO WITH MORDER_NO, DATE WITH MDATE, DEADLINE WITH MDEADLINE
      REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3, QTY_ORD WITH MQTY_ORD
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE
   ENDIF
   SELECT 5
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      @ R, 79 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      SKIP
   ENDIF
   IF MANS='N'
      RETURN
   ENDIF
ENDDO
RETURN
*******

**********************
PROCEDURE ORD_DSP
**********************
R= 8
@ R, 0 CLEAR TO 24, 79
SELECT 5
GOTO TOP
DO WHILE .not. EOF()
   SELECT 5
   MI1= I1
   MI2= I2
   MI3= I3
   MQTY_ORD= QTY_ORD
   MRATE= RATE

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 3 SAY '  -  -    '
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ R, 3 SAY MP1
   @ R, 5 SAY '-'
   @ R, 6 SAY MI2 PICTURE '99'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ R, 6 SAY MP2
   @ R, 8 SAY '-'
   @ R, 9 SAY MI3 PICTURE '9999'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ R, 9 SAY MP3

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 2
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SEEK MI_CODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 15 SAY ITEM
   SELECT 5
   @ R, 47 SAY MQTY_ORD PICTURE '9999999.9999'
   @ R, 61 SAY MRATE PICTURE '999999999.9999'

IF _DOS
   SET COLOR TO 
ENDIF

   R= R+1
   SKIP
ENDDO
RETURN
**************

***************
PROCEDURE POST
****************
CLEAR
CLOSE ALL
SET DATE brit

IF _DOS
SET COLOR TO 7/1
ENDIF

SELECT 1
CLEAR
USE st-tran
SORT ON DOC_TYPE, DOC_NO, SR_NO TO abc
USE
SELECT 1
USE abc
COPY TO st-tran
RUN del abc.dbf
SELECT 1
USE st-tran
INDEX ON DATE TAG stdate
SELECT 2
USE p-order
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG ordidx
SELECT 3
USE item-bal
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_bal
SELECT 4
USE stock
SELECT 5
USE s-order
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG sordidx
SELECT 1
GOTO BOTTOM
STORE DATE TO MDATE, MPDATE
@ 1, 2 SAY 'Post Transactions To Ledger:'
@ 3, 2 SAY 'This option will post all un-posted transactions to their respective'
@ 4, 2 SAY 'ledger heads Upto Given Date....'
@ 13, 14 SAY 'Transactions upto            exist in file '

IF _DOS
SET COLOR TO *+g
ENDIF

@ 13, 32 SAY MDATE

IF _DOS
SET COLOR TO 7/1     
ENDIF

@ 14, 14 SAY 'Date upto which you want transactions to be posted ' GET MPDATE
READ
P= ' '
DO WHILE .not. P$'CQ'
   @ 23, 50 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
@ 12, 23 SAY 'Press any key to start posting'
WAIT ''
CLEAR
@ 13, 23 SAY 'Please wait data is being posted....'
SELECT 1
CNT= 0
GOTO TOP
SET FILTER TO FLAG=' '.and.DATE<=MPDATE
GOTO TOP
MSR_NO= 0
DO WHILE .not. EOF()
   MDATE= DATE
   MDOC_NO= DOC_NO
   MDOC_TYPE= DOC_TYPE
   MLOC_CODE= LOC_CODE
   MORDER_NO= ORDER_NO
   MDEPT_CODE= DEPT_CODE
   MCITY_CODE= CITY_CODE
   MZONE_CODE= ZONE_CODE
   MI1= I1
   MI2= I2
   MI3= I3
   MC1= C_C1
   MC2= C_C2
   MC3= C_C3
   MD1= D_D1
   MD2= D_D2
   MD3= D_D3
   MQTY= T_QTY
   MRATE= RATE
   MAMOUNT= T_AMOUNT
   MPSR_UPRIC= PSR_UPRICE
   MPSR_TPRIC= PSR_TPRICE
   MF_T= F_T
   IF MDOC_TYPE=1
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         VAL_TOT= 0
         VAL_TOT= QTY*AVG_PRICE
         MTOT_QTY= 0
         MTOT_QTY= QTY+MQTY
         MVAL_TOT= 0
         MVAL_TOT= VAL_TOT+MAMOUNT
         MAVG= 0
         IF MTOT_QTY<>0
            MAVG= MVAL_TOT/MTOT_QTY
         ENDIF
         REPLACE QTY WITH MTOT_QTY
         REPLACE AVG_PRICE WITH MAVG
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 1, DATE WITH MDATE, LOC_CODE WITH MLOC_CODE
      REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE, T_AMOUNT WITH MAMOUNT
      REPLACE AVG_PRICE WITH MAVG, SR_NO WITH MSR_NO, QTY_BAL WITH MTOT_QTY
      REPLACE PSR_UPRICE WITH MPSR_UPRIC, PSR_TPRICE WITH MPSR_TPRIC
      SELECT 2
      STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
      SEEK ORD
      IF FOUND()
         MORD_DEL= 0
         MORD_DEL= QTY_DEL+MQTY
         MORDBAL= 0
         MORDBAL= QTY_ORD-MORD_DEL
         REPLACE QTY_BAL WITH MORDBAL, QTY_DEL WITH MORD_DEL
      ENDIF
   ENDIF
   IF MDOC_TYPE=2
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         MTOT_QTY= 0
         MTOT_QTY= QTY-MQTY
         MAVG= AVG_PRICE
         REPLACE QTY WITH MTOT_QTY
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 2, DATE WITH MDATE, LOC_CODE WITH MLOC_CODE
      REPLACE D_D1 WITH MD1, D_D2 WITH MD2, D_D3 WITH MD3, T_QTY WITH MQTY
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MAVG, T_AMOUNT WITH MQTY*MAVG
      REPLACE AVG_PRICE WITH MAVG, SR_NO WITH MSR_NO, QTY_BAL WITH MTOT_QTY
      REPLACE PSR_UPRICE WITH MPSR_UPRIC, PSR_TPRICE WITH MPSR_TPRIC
      SELECT 2
      STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
      SEEK ORD
      IF FOUND()
         MORD_DEL= 0
         MORD_DEL= QTY_DEL+MQTY
         MORDBAL= 0
         MORDBAL= QTY_ORD-MORD_DEL
         REPLACE QTY_BAL WITH MORDBAL, QTY_DEL WITH MORD_DEL
      ENDIF
   ENDIF
   IF MDOC_TYPE=3
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         MTOT_QTY= 0
         MTOT_QTY= QTY-MQTY
         MAVG= AVG_PRICE
         REPLACE QTY WITH MTOT_QTY
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 3, DATE WITH MDATE
      REPLACE LOC_CODE WITH MLOC_CODE, T_QTY WITH MQTY
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, SR_NO WITH MSR_NO
      REPLACE DEPT_CODE WITH MDEPT_CODE, RATE WITH MAVG, T_AMOUNT WITH MQTY*MAVG
      REPLACE AVG_PRICE WITH MAVG, QTY_BAL WITH MTOT_QTY
   ENDIF
   IF MDOC_TYPE=4
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         MTOT_QTY= 0
         MTOT_QTY= QTY+MQTY
         MAVG= AVG_PRICE
         REPLACE QTY WITH MTOT_QTY
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 4, DATE WITH MDATE
      REPLACE LOC_CODE WITH MLOC_CODE, T_QTY WITH MQTY
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, SR_NO WITH MSR_NO
      REPLACE DEPT_CODE WITH MDEPT_CODE, RATE WITH MAVG, T_AMOUNT WITH MQTY*MAVG
      REPLACE AVG_PRICE WITH MAVG, QTY_BAL WITH MTOT_QTY
   ENDIF
   IF MDOC_TYPE=5
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         MTOT_QTY= 0
         MTOT_QTY= QTY+MQTY
         MAVG= AVG_PRICE
         REPLACE QTY WITH MTOT_QTY
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 5, DATE WITH MDATE
      REPLACE LOC_CODE WITH MLOC_CODE, T_QTY WITH MQTY, SR_NO WITH MSR_NO
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, DEPT_CODE WITH MDEPT_CODE
      REPLACE AVG_PRICE WITH MAVG, QTY_BAL WITH MTOT_QTY
   ENDIF
   IF MDOC_TYPE=6
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         MTOT_QTY= 0
         MTOT_QTY= QTY-MQTY
         MAVG= AVG_PRICE
         REPLACE QTY WITH MTOT_QTY
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 6, DATE WITH MDATE, LOC_CODE WITH MLOC_CODE
      REPLACE D_D1 WITH MD1, D_D2 WITH MD2, D_D3 WITH MD3, T_QTY WITH MQTY
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MAVG, T_AMOUNT WITH MQTY*MAVG
      REPLACE AVG_PRICE WITH MAVG, SR_NO WITH MSR_NO, QTY_BAL WITH MTOT_QTY
      REPLACE PSR_UPRICE WITH MPSR_UPRIC, PSR_TPRICE WITH MPSR_TPRIC
      REPLACE CITY_CODE WITH MCITY_CODE, ZONE_CODE WITH MZONE_CODE
      SELECT 5
      STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
      SEEK ORD
      IF FOUND()
         MORD_DEL= 0
         MORD_DEL= QTY_DEL+MQTY
         MORDBAL= 0
         MORDBAL= QTY_ORD-MORD_DEL
         REPLACE QTY_BAL WITH MORDBAL, QTY_DEL WITH MORD_DEL
      ENDIF
   ENDIF
   IF MDOC_TYPE=7
      STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
      SELECT 3
      SEEK XYZ
      IF FOUND()
         MTOT_QTY= 0
         MTOT_QTY= QTY+MQTY
         MAVG= AVG_PRICE
         REPLACE QTY WITH MTOT_QTY
         MSR_NO= SR_NO+1
         REPLACE SR_NO WITH MSR_NO
      ENDIF
      SELECT 4
      APPEND BLANK
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 7, DATE WITH MDATE, LOC_CODE WITH MLOC_CODE
      REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MAVG, T_AMOUNT WITH MQTY*MAVG
      REPLACE AVG_PRICE WITH MAVG, SR_NO WITH MSR_NO, QTY_BAL WITH MTOT_QTY
      REPLACE PSR_UPRICE WITH MPSR_UPRIC, PSR_TPRICE WITH MPSR_TPRIC
      SELECT 5
      STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
      SEEK ORD
      IF FOUND()
         MORD_DEL= 0
         MORD_DEL= QTY_DEL+MQTY
         MORDBAL= 0
         MORDBAL= QTY_ORD+MORD_DEL
         REPLACE QTY_BAL WITH MORDBAL, QTY_DEL WITH MORD_DEL
      ENDIF
   ENDIF
   IF MDOC_TYPE=8
      IF MF_T='F'
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 3
         SEEK XYZ
         IF FOUND()
            MTOT_QTY= 0
            MTOT_QTY= QTY-MQTY
            MAVG= AVG_PRICE
            REPLACE QTY WITH MTOT_QTY
            MSR_NO= SR_NO+1
            REPLACE SR_NO WITH MSR_NO
            SELECT 4
            APPEND BLANK
            REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 8, DATE WITH MDATE
            REPLACE LOC_CODE WITH MLOC_CODE, T_QTY WITH MQTY, SR_NO WITH MSR_NO
            REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, F_T WITH 'F'
            REPLACE QTY_BAL WITH MTOT_QTY, AVG_PRICE WITH MAVG
         ENDIF
      ENDIF
      IF MF_T='T'
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 3
         SEEK XYZ
         IF FOUND()
            MTOT_QTY= 0
            MTOT_QTY= QTY+MQTY
            MAVG= AVG_PRICE
            REPLACE QTY WITH MTOT_QTY
            MSR_NO= SR_NO+1
            REPLACE SR_NO WITH MSR_NO
            SELECT 4
            APPEND BLANK
            REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 8, DATE WITH MDATE
            REPLACE LOC_CODE WITH MLOC_CODE, T_QTY WITH MQTY, SR_NO WITH MSR_NO
            REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, F_T WITH 'T'
            REPLACE QTY_BAL WITH MTOT_QTY, AVG_PRICE WITH MAVG
         ENDIF
      ENDIF
   ENDIF
   SELECT 1
   REPLACE FLAG WITH 'Y'
   IF .not. EOF()
      SKIP
   ENDIF
ENDDO
CLEAR
CLOSE ALL
USE stock
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itemidx
INDEX ON DTOC(DATE)+STR(DOC_NO,7) TAG itemidx1
INDEX ON DOC_NO TAG itemidx2
INDEX ON DATE TAG stk-idx
CLOSE ALL
RETURN
***********************


************************
PROCEDURE CORD_ENT
************************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE p-order
INDEX ON ORDER_NO TAG ord_idx
COPY TO ord-work STRUCTURE
SELECT 2
USE item ORDER item_idx
SELECT 3
USE ldgrcode ORDER ac1
SELECT 5
USE ord-work
DO WHILE .T.
   CLEAR
   R= 3
   @ 1, 25 SAY 'ORDER TO SUPPLIERS / CREDITORS  ENTRY'
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MORDER_NO, MC1, MC2, MC3
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 23, 45 CLEAR TO 23, 78
      IF MORDER_NO=0
         LOOP
      ENDIF
      MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
      SET COLOR TO /g
ENDIF

      @ R, 15 SAY MVCH_NO

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 1
      SEEK MORDER_NO
      IF FOUND()
         @ 23, 1 SAY SPACE(78)
         @ 23, 15 SAY 'This Order No Already Exists  -- Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 
ENDIF

         @ 23, 10 SAY SPACE(75)
         LOOP
      ENDIF
      SELECT 5
      ZAP
      @ R, 28 SAY 'Date ' GET MDATE
      READ
      DO chk IN TBL_PROC.PRG
      @ R, 45 SAY 'Delivery Deadline ' GET MDEADLINE
      READ
      IF MDEADLINE<MDATE
         LOOP
      ENDIF
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Party Code'

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 15 SAY '  -  -   '

IF _DOS
         SET COLOR TO 
ENDIF

         @ R, 15 GET MC1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO n/g
ENDIF

         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ R, 15 SAY MPC1
         @ R, 17 SAY '-'
         @ R, 18 GET MC2 PICTURE '99'
         READ
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ R, 18 SAY MPC2
         @ R, 20 SAY '-'
         @ R, 21 GET MC3 PICTURE '999'
         READ
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ R, 21 SAY MPC3

IF _DOS
         SET COLOR TO 
ENDIF

         SELECT 3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SEEK MCODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 28 SAY TITLE
         MTITLE= TITLE

IF _DOS
         SET COLOR TO 
ENDIF

         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 3 SAY 'Item Code'
      @ R, 15 SAY 'Item '
      @ R, 47 SAY 'Qty Ordered'
      @ R, 62 SAY 'Rate '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0
            @ 23, 50 SAY 'Press Ctrl Q To Quit '

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 3 SAY '  -  -    '

IF _DOS
            SET COLOR TO 
ENDIF

            @ R, 3 GET MI1 PICTURE '99'
            READ
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            @ 23, 45 CLEAR TO 23, 78

IF _DOS
            SET COLOR TO n/g
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 3 SAY MP1
            @ R, 5 SAY '-'
            @ R, 6 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 6 SAY MP2
            @ R, 8 SAY '-'
            @ R, 9 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 9 SAY MP3

IF _DOS
            SET COLOR TO 
ENDIF

            @ 23, 50 CLEAR TO 23, 78
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            IF MI1=0.and.MI2=0.and.MI3=0
               LOOP
            ENDIF
            SELECT 2
            STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
            SEEK MI_CODE
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 15 SAY ITEM

IF _DOS
            SET COLOR TO 
ENDIF

            ABC= 1
         ENDDO
         DO WHILE .T.
            @ R, 47 GET MQTY_ORD PICTURE '9999999.9999'
            READ
            IF MQTY_ORD=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 61 GET MRATE PICTURE '999999999.9999'
         READ
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'Y'
            @ R, 79 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 5
         APPEND BLANK
         REPLACE ORDER_NO WITH MORDER_NO, DATE WITH MDATE, DEADLINE WITH MDEADLINE
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3, QTY_ORD WITH MQTY_ORD
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE
         IF ANS='Y'
            STORE 0 TO MI1, MI2, MI3, MRATE, MQTY_ORD
            R= R+1
            LOOP
         ENDIF
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
            READ
         ENDDO
         @ 24, 20 CLEAR TO 24, 78
         IF ANS='Y'
            DO ord-cor
         ENDIF
         SELECT 5
         USE
         SELECT 1
         APPEND FROM ord-work
         SELECT 5
         USE ord-work
         ZAP
         EXIT
      ENDDO
      ANS= ' '
      DO WHILE .not. ANS$'YN'
         @ 24, 25 SAY 'More Data Entry - Y/N' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
*******

*********************
PROCEDURE CORD_COR
*********************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE p-order
INDEX ON ORDER_NO TAG ord_idx
COPY TO ord-work STRUCTURE
SELECT 2
USE item ORDER item_idx
SELECT 3
USE ldgrcode ORDER ac1
SELECT 5
USE ord-work
DO WHILE .T.
   CLEAR
   R= 3
   @ 1, 23 SAY 'ORDER TO SUPPLIERS / CREDITORS  CORRECTION'
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MORDER_NO, MC1, MC2, MC3
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MORDER_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
   SET COLOR TO /g
ENDIF

   @ R, 15 SAY MVCH_NO

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK MORDER_NO
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Order No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   SELECT 1
   USE
   SELECT 5
   ZAP
   APPEND FROM p-order FOR ORDER_NO=MORDER_NO
   SELECT 1
   USE p-order ORDER ord_idx
   SELECT 5
   GOTO TOP
   MDATE= DATE
   MDEADLINE= DEADLINE
   @ R, 28 SAY 'Date ' GET MDATE
   @ R, 45 SAY 'Delivery Deadline ' GET MDEADLINE
   READ
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 5
      MC1= C1
      MC2= C2
      MC3= C3

IF _DOS
      SET COLOR TO 
ENDIF

      @ R, 2 SAY 'Party Code'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 15 SAY '  -  -   '
      @ R, 15 GET MC1 PICTURE '99'
      READ

IF _DOS
      SET COLOR TO n/g
ENDIF

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 17 SAY '-'
      @ R, 18 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2
      @ R, 20 SAY '-'
      @ R, 21 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 21 SAY MPC3

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 3
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 28 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 3 SAY 'Item Code'
   @ R, 15 SAY 'Item '
   @ R, 47 SAY 'Qty Ordered'
   @ R, 62 SAY 'Rate '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   DO ord_cor
   SELECT 5
   GOTO TOP
   REPLACE ALL DATE WITH MDATE, DEADLINE WITH MDEADLINE, C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
   USE
   SELECT 1
   DELETE ALL FOR ORDER_NO=MORDER_NO
   PACK
   APPEND FROM ord-work
   SELECT 5
   USE ord-work
   ZAP
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'More Data Correction - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
**************

********************
PROCEDURE CORD_DEL
**********************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE p-order
INDEX ON ORDER_NO TO ord_idx
COPY TO ord-work STRUCTURE
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG ac1
SELECT 5
USE ord-work
DO WHILE .T.
   CLEAR
   R= 3
   @ 1, 23 SAY 'ORDER TO SUPPLIERS / CREDITORS  DELETION '
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MORDER_NO, MC1, MC2, MC3
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MORDER_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
   SET COLOR TO /g
ENDIF

   @ R, 15 SAY MVCH_NO

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK MORDER_NO
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Order No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   SELECT 1
   USE
   SELECT 5
   ZAP
   APPEND FROM p-order FOR ORDER_NO=MORDER_NO
   SELECT 1
   USE p-order ORDER ord_idx
   SELECT 5
   GOTO TOP
   MDATE= DATE
   MDEADLINE= DEADLINE
   @ R, 28 SAY 'Date ' GET MDATE
   @ R, 45 SAY 'Delivery Deadline ' GET MDEADLINE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 5
      MC1= C1
      MC2= C2
      MC3= C3
      @ R, 2 SAY 'Party Code'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 15 SAY '  -  -   '
      @ R, 15 SAY MC1 PICTURE '99'

IF _DOS
      SET COLOR TO n/g
ENDIF

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 17 SAY '-'
      @ R, 18 SAY MC2 PICTURE '99'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2
      @ R, 20 SAY '-'
      @ R, 21 SAY MC3 PICTURE '999'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 21 SAY MPC3

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 3
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 28 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 3 SAY 'Item Code'
   @ R, 15 SAY 'Item '
   @ R, 47 SAY 'Qty Ordered'
   @ R, 62 SAY 'Rate '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   CLEAR GETS
   DO ord_dsp
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   IF ANS='Y'
      SELECT 1
      DELETE ALL FOR ORDER_NO=MORDER_NO
      PACK
      SELECT 5
      ZAP
   ENDIF
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Any More Deletion   - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
**********

*********************
PROCEDURE CORD_PRT
**********************
CLEAR
CLOSE ALL
SELECT 1
USE p-order EXCLUSIVE
INDEX ON ORDER_NO TAG ord_idx
COPY TO ord-work STRUCTURE
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG ac1
SELECT 5
USE ord-work
STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO
ABC= 0
DO WHILE ABC=0
   @ 3, 30 SAY 'ORDER TO SUPPLIERS/CREDITORS'
   @ 4, 30 SAY '----------------------------'
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ 10, 20 SAY 'Order No    ' GET MORDER_NO PICTURE '99999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MORDER_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
   SET COLOR TO /g
ENDIF

   @ 10, 33 SAY MVCH_NO

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   SEEK MORDER_NO
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Order No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 1
   USE
   SELECT 5
   ZAP
   APPEND FROM p-order FOR ORDER_NO=MORDER_NO
   SELECT 1
   USE p-order ORDER ord_idx
   SELECT 5
   GOTO TOP
   IF RECCOUNT()=0
      CLEAR
      @ 12, 12 SAY 'No Order Exist in Data File  -  Press any key to return to menu .... '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   ABC= 1
ENDDO
SELECT 5
GOTO TOP
MDATE= DATE
MDEADLINE= DEADLINE
MC1= C1
MC2= C2
MC3= C3
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
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
L= 3
HEADING= .T.
CNT= 0
DO WHILE .not. EOF()
   SELECT 5
   IF HEADING
      IF P='P'
         DO top-prt
         @ 3, 28 SAY 'ORDER TO SUPPLIERS/CREDITORS'
         L= L+1
         @ L, 28 SAY '----------------------------'
         L= L+1
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 25 SAY 'ORDER TO SUPPLIERS/CREDITORS'
         L= L+1
         @ L, 25 SAY '----------------------------'
         L= L+1
      ENDIF
      MVCH_NO= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))
      @ L, 1 SAY 'Order No   : '
      @ L, 14 SAY MVCH_NO
      @ L, 51 SAY 'Date              : '+DTOC(DATE)
      L= L+1
      @ L, 1 SAY 'Party Code : '
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ L, 14 SAY MPC1
      @ L, 16 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ L, 17 SAY MPC2
      @ L, 19 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ L, 20 SAY MPC3
      SELECT 3
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF FOUND()
         @ L, 24 SAY TITLE
      ENDIF
      SELECT 5
      @ L, 51 SAY 'Delivery Deadline : '+DTOC(DEADLINE)
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 13 SAY 'Item '
      @ L, 38 SAY 'Unit'
      @ L, 43 SAY 'Qty Ordered'
      @ L, 62 SAY 'Rate '
      @ L, 73 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 5
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
   @ L, 7 SAY MP3
   STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
   SELECT 2
   SEEK MI_CODE
   IF FOUND()
      MUNIT= UNIT
      @ L, 13 SAY ITEM
   ENDIF
   SELECT 5
   MQTY= QTY_ORD
   MRATE= RATE
   MAMT= MQTY*MRATE
   @ L, 38 SAY MUNIT
   @ L, 41 SAY MQTY PICTURE '9999999.9999'
   @ L, 54 SAY MRATE PICTURE '99999999.9999'
   @ L, 67 SAY MAMT PICTURE '999999999.99'
   MTOT= MTOT+MAMT
   L= L+1
   SKIP
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
   IF P='P'.and.L>56
      L= 3
      HEADING= .T.
   ENDIF
ENDDO
@ L, 50 SAY REPLICATE('-',30)
L= L+1
@ L, 54 SAY 'Total : '
@ L, 67 SAY MTOT PICTURE '999999999.99'
L= L+1
@ L, 50 SAY REPLICATE('-',30)
L= L+1
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