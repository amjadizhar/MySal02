* INVENTORY
* PROGRAM: PURRPROC.PRG
* WRITTEN BY : AMJAD IZHAR
* THIS REVISION: 19-MAY-2005
*******************
PROCEDURE PUR_ENT
*******************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE item ORDER item_idx
SELECT 4
USE ldgrcode ORDER ac1
SELECT 5
USE loc ORDER loc_idx
SELECT 6
USE p-order ORDER pord_idx
SELECT 7
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 1
   MVCH_TYPE= 0
   T= '  '
   MVCH1= '  '
   MVCH2= '     '

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 1, 30 SAY 'PURCHASE ENTRY '

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MVCH_NO1, MVCH_N2
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MTOT1, MSR, MSR_NO
   DO WHILE .T.
      @ 24, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'GRN No      ' GET MDOC_NO PICTURE '9999999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MDOC_NO=0
         LOOP
      ENDIF
      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 15 SAY MDOC

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      SELECT 2
      USE st-key ORDER st_idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This GRN No Already Exists  -- Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      SELECT 2
      USE
      ABC= 0
      DO WHILE ABC=0
         IF INT=1
            @ 21, 1 CLEAR TO 24, 79
            @ 21, 1 SAY 'Voucher Types:'
            @ 21, 17 SAY '1 - '
            @ 21, 21 SAY 'Cash Receipt - '

IF _DOS
            SET COLOR TO /w 
ENDIF

            @ 21, 36 SAY 'CR'

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            @ 22, 17 SAY '2 - '
            @ 22, 21 SAY 'Cash Payment - '

IF _DOS
            SET COLOR TO /w 
ENDIF

            @ 22, 36 SAY 'CP'

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            @ 21, 58 SAY '3 - '
            @ 21, 62 SAY 'Bank Receipt - '

IF _DOS
            SET COLOR TO /w 
ENDIF

            @ 21, 77 SAY 'BR'

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            @ 22, 58 SAY '4 - '
            @ 22, 62 SAY 'Bank Payment - '

IF _DOS
            SET COLOR TO /w 
ENDIF

            @ 22, 77 SAY 'BP'

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            @ 23, 17 SAY '5 - '
            @ 23, 21 SAY 'Journal      - '

IF _DOS
            SET COLOR TO /w 
ENDIF

            @ 23, 36 SAY 'JV'

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            @ 23, 40 SAY '6 - '
            @ 23, 44 SAY 'Sales - '

IF _DOS
            SET COLOR TO /w 
ENDIF

            @ 23, 52 SAY 'SV'

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            @ 23, 58 SAY '7 - '
            @ 23, 62 SAY 'Purchase     - '

IF _DOS
            SET COLOR TO /w 
ENDIF

            @ 23, 77 SAY 'PV'

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            @ R, 28 SAY 'Voucher Type ' GET MVCH_TYPE PICTURE '9'
            READ
            IF MVCH_TYPE=0
               LOOP
            ENDIF
            IF MVCH_TYPE=1
               T= 'CR-'
            ENDIF
            IF MVCH_TYPE=2
               T= 'CP-'
            ENDIF
            IF MVCH_TYPE=3
               T= 'BR-'
            ENDIF
            IF MVCH_TYPE=4
               T= 'BP-'
            ENDIF
            IF MVCH_TYPE=5
               T= 'JV-'
            ENDIF
            IF MVCH_TYPE=6
               T= 'SV-'
            ENDIF
            IF MVCH_TYPE=7
               T= 'PV-'
            ENDIF

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 45 SAY T
            @ R, 48 SAY '  -     '

IF _DOS
            SET COLOR TO 7/1
ENDIF

            @ R, 48 GET MVCH_NO1 PICTURE '@z 99'
            READ
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            IF MVCH_NO1=0
               LOOP
            ENDIF
            MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 48 SAY MVCH1

IF _DOS
            SET COLOR TO 7/1   
ENDIF

            SELECT 1
            USE tran-key ORDER KEY
            SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1
            GOTO BOTTOM
            MVCH_NO2= VCH_NO2+1
            @ R, 51 GET MVCH_NO2 PICTURE '@z 99999'
            READ
            IF MVCH_NO2=0
               LOOP
            ENDIF
            MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 51 SAY MVCH2

IF _DOS
            SET COLOR TO 7/1
ENDIF

            SELECT 1
            SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
            IF FOUND()
               ?? CHR(7)
               @ 24, 1 SAY SPACE(78)
               @ 24, 15 SAY 'This Voucher Already Exists  -- Press any key'
               WAIT ''
               @ 24, 10 SAY SPACE(75)
               LOOP
            ELSE
               APPEND BLANK
               IF .not. RLOCK()
                  rlock()
               ENDIF
               REPLACE VCH_TYPE WITH MVCH_TYPE
               REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
               UNLOCK
            ENDIF
            @ 21, 1 CLEAR TO 24, 79
            SELECT 1
            USE
         ENDIF
         ABC= 1
      ENDDO
      @ R, 60 SAY 'Date ' GET MDATE
      READ
      DO chk IN TBL_PROC.PRG
      
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Credit / Party Code'

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 23 SAY '  -  -   '

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         @ R, 23 GET MC1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO /w
ENDIF

         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ R, 23 SAY MPC1
         @ R, 25 SAY '-'
         @ R, 26 GET MC2 PICTURE '99'
         READ
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ R, 26 SAY MPC2
         @ R, 28 SAY '-'
         @ R, 29 GET MC3 PICTURE '999'
         READ
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ R, 29 SAY MPC3

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         SELECT 4
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

         @ R, 33 SAY TITLE
         MTITLE= TITLE

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         ABC= 1
      ENDDO
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
         READ
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 23, 45 CLEAR TO 23, 78
         IF MLOC_CODE=0
            LOOP
         ENDIF
         SELECT 5
         SEEK MLOC_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         @ R, 33 SAY LOC
         MLOC= LOC
         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 2 SAY 'Item Code'
      @ R, 13 SAY 'Item '
      @ R, 30 SAY 'Order #'
      @ R, 43 SAY 'Qty '
      @ R, 53 SAY 'Rate '
      @ R, 65 SAY 'Amount '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 2 SAY '  -  -    '

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            @ R, 2 GET MI1 PICTURE '99'
            READ

IF _DOS
            SET COLOR TO /w
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 2 SAY MP1
            @ R, 4 SAY '-'
            @ R, 5 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 5 SAY MP2
            @ R, 7 SAY '-'
            @ R, 8 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 8 SAY MP3

IF _DOS
            SET COLOR TO 7/1 
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
            SELECT 3
            SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF
            @ R, 13 SAY ITEM
            MDC1= DC1
            MDC2= DC2
            MDC3= DC3
            IF INT=1
               IF MDC1=0.and.MDC2=0.and.MDC3=0
                  @ 22, 14 SAY 'Debit Code (For Purchase Voucher) does not exist.'
                  @ 23, 14 SAY 'Please feed Debit Code using Item Code Correction Option '
                  WAIT ''
                  CLEAR
                  CLOSE ALL
                  RETURN
               ENDIF
               SELECT 4
               SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)
               IF FOUND()
                  MDTITLE= TITLE
               ENDIF
            ENDIF
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            ABC= 1
         ENDDO
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 30 SAY MORD

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            SELECT 6
            STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '999999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 51 GET MRATE PICTURE '99999999.9999'
         READ
         MTOT= 0
         MTOT= MQTY*MRATE
         @ R, 65 GET MTOT PICTURE '9999999999.99'
         READ
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'Y'
            @ R, 79 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 1
         USE st-work
         MSR= MSR+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         IF INT=1
            REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2, VCH_TYPE WITH MVCH_TYPE
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 1, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE, T_AMOUNT WITH MTOT
         REPLACE PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MTOT, SR_NO WITH MSR
         UNLOCK
         USE
         MTOT1= MTOT1+MTOT
         IF ANS='Y'
            IF R<=21
               STORE 0 TO MI1, MI2, MI3, MRATE, MQTY, MORDER_NO
               R= R+1
               LOOP
            ELSE
               STORE 0 TO MI1, MI2, MI3, MRATE, MQTY, MORDER_NO
               @ 9, 0 CLEAR TO 24, 79
               R= 9
               LOOP
            ENDIF
         ENDIF
         @ 23, 50 SAY 'Total '
         @ 23, 63 SAY MTOT1 PICTURE '9999999999.99'
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
            READ
         ENDDO
         @ 24, 20 CLEAR TO 24, 78
         IF ANS='Y'
            SELECT 1
            USE st-work
            SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
            DO p-cor
         ENDIF
         IF INT=1
            SELECT 2
            USE tran ORDER tranent
            SELECT 1
            USE st-work
            GOTO TOP
            SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
            GOTO TOP
            MC1= C_C1
            MC2= C_C2
            MC3= C_C3
            STORE 0 TO MSR_NO, MTOT1
            DO WHILE .not. EOF()
               MI1= I1
               MI2= I2
               MI3= I3
               MTOT= T_AMOUNT
               SELECT 3
               SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
               IF FOUND()
                  MDC1= DC1
                  MDC2= DC2
                  MDC3= DC3
               ENDIF
               SELECT 4
               SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)
               IF FOUND()
                  MDTITLE= TITLE
               ENDIF
               SELECT 2
               MSR_NO= MSR_NO+1
               APPEND BLANK
               IF .not. RLOCK()
                  rlock()
               ENDIF
               REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE
               REPLACE C1 WITH MDC1, C2 WITH MDC2, C3 WITH MDC3, DATE WITH MDATE
               REPLACE DR_CR WITH 'DR', AMOUNT WITH MTOT, SR_NO WITH MSR_NO
               REPLACE DESCRIP WITH MTITLE, VCH_NO2 WITH MVCH_NO2
               UNLOCK
               MTOT1= MTOT1+MTOT
               SELECT 1
               SKIP
            ENDDO
            SELECT 1
            USE
            SELECT 2
            MSR_NO= MSR_NO+1
            APPEND BLANK
            IF .not. RLOCK()
               rlock()
            ENDIF
            REPLACE VCH_TYPE WITH MVCH_TYPE, DATE WITH MDATE
            REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
            REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
            REPLACE DR_CR WITH 'CR', SR_NO WITH MSR_NO
            REPLACE DESCRIP WITH 'Purchase ', AMOUNT WITH MTOT1
            UNLOCK
            USE
            SELECT 1
            USE tran-key ORDER key
            SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
            IF FOUND()
               REPLACE FLAG WITH 'Y'
            ENDIF
            USE
         ENDIF
SELECT 2
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX1
         
         
         *USE st-tran ORDER stidx
         *, stidx1
         
         
         APPEND FROM st-work FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE
         SELECT 2
         USE st-key ORDER st_idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
         ENDIF
         USE
         SELECT 1
         USE st-work
         DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE
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
******


**********************
PROCEDURE PUR_COR
***********************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON

SELECT 2
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX1



*SELECT 2
*USE st-tran ORDER stidx
*USE st-tran ORDER stidx1
*USE st-tran ORDER stidx
*, stidx1


SET FILTER TO DOC_TYPE=1
GOTO TOP
SELECT 3
USE item ORDER item_idx
SELECT 4
USE ldgrcode ORDER ac1
SELECT 5
USE loc ORDER loc_idx
SELECT 6
USE p-order ORDER pord_idx
SELECT 7
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   R= 3
   T= '   '
   MVCH1= '  '
   MVCH2= '     '
   MDOC_TYPE= 1
   MVCH_TYPE= 7

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 1, 30 SAY 'PURCHASE CORRECTION '

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'GRN No      ' GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 15 SAY MDOC

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   SELECT 2
   SET ORDER TO STIDX
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This GRN No Does Not Exists ... Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Change/Correct it. If you still want to  '
      @ 14, 15 SAY 'Change/Correct it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   MVCH_TYPE= VCH_TYPE
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   SET ORDER TO 2
   IF INT=1
      @ 21, 1 CLEAR TO 24, 79
      @ 21, 1 SAY 'Voucher Types:'
      @ 21, 17 SAY '1 - '
      @ 21, 21 SAY 'Cash Receipt - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 21, 36 SAY 'CR'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 22, 17 SAY '2 - '
      @ 22, 21 SAY 'Cash Payment - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 22, 36 SAY 'CP'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 21, 58 SAY '3 - '
      @ 21, 62 SAY 'Bank Receipt - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 21, 77 SAY 'BR'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 22, 58 SAY '4 - '
      @ 22, 62 SAY 'Bank Payment - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 22, 77 SAY 'BP'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 17 SAY '5 - '
      @ 23, 21 SAY 'Journal      - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 23, 36 SAY 'JV'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 40 SAY '6 - '
      @ 23, 44 SAY 'Sales - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 23, 52 SAY 'SV'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 58 SAY '7 - '
      @ 23, 62 SAY 'Purchase     - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 23, 77 SAY 'PV'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ R, 28 SAY 'Voucher Type ' GET MVCH_TYPE PICTURE '9'
      READ
      IF MVCH_TYPE=0
         LOOP
      ENDIF
      IF MVCH_TYPE=1
         T= 'CR-'
      ENDIF
      IF MVCH_TYPE=2
         T= 'CP-'
      ENDIF
      IF MVCH_TYPE=3
         T= 'BR-'
      ENDIF
      IF MVCH_TYPE=4
         T= 'BP-'
      ENDIF
      IF MVCH_TYPE=5
         T= 'JV-'
      ENDIF
      IF MVCH_TYPE=6
         T= 'SV-'
      ENDIF
      IF MVCH_TYPE=7
         T= 'PV-'
      ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 45 SAY T
      @ R, 48 SAY '  -     '

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ R, 48 GET MVCH_NO1 PICTURE '@z 99'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MVCH_NO1=0
         LOOP
      ENDIF
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 48 SAY MVCH1

IF _DOS
      SET COLOR TO 7/1   
ENDIF

      @ R, 51 GET MVCH_NO2 PICTURE '@z 99999'
      READ
      IF MVCH_NO2=0
         LOOP
      ENDIF
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 51 SAY MVCH2

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 2
      SET ORDER TO STIDX1
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)+STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
      IF .not. FOUND()
         @ 23, 1 SAY SPACE(78)
         @ 23, 13 SAY 'This GRN No & Voucher No Does Not Exists ... Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         @ 23, 10 SAY SPACE(80)
         LOOP
      ENDIF
      USE
      SELECT 2
      USE tran ORDER tranent
      SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
      IF .not. FOUND()
         @ 23, 13 SAY 'This Voucher No Does Not Exists ... Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         @ 23, 10 SAY SPACE(75)
         MV_NO= 0
         LOOP
      ENDIF
      IF FLAG='Y'
         CLEAR
         @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
         @ 13, 15 SAY 'you cannot Change/Correct it. If you still want to  '
         @ 14, 15 SAY 'Change/Correct it, please use UN-POST OPTION first. '
         @ 16, 15 SAY 'Press Any Key to Return ............ '
         WAIT ''
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ENDIF
   @ 21, 1 CLEAR TO 24, 79
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   DELETE ALL
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=1
   SELECT 1
   MDATE= DATE
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      MLOC_CODE= LOC_CODE

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ R, 2 SAY 'Credit / Party Code'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 23 SAY '  -  -   '

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ R, 23 GET MC1 PICTURE '99'
      READ
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      SELECT 4
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

      @ R, 33 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ R, 15 GET MLOC_CODE PICTURE '9999'
      READ

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 33 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   DO p_cor
   SELECT 1
   GOTO TOP
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 DATE WITH MDATE
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 C_C1 WITH MC1
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 C_C2 WITH MC2
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 C_C3 WITH MC3
   USE
   IF INT=1
      SELECT 2
      USE tran ORDER tranent
      DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SELECT 1
      USE st-cor
      GOTO TOP
      SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SUM FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 PSR_TPRICE TO MTOT1
      GOTO TOP
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      STORE 0 TO MSR_NO, MTOT1
      DO WHILE .not. EOF()
         MI1= I1
         MI2= I2
         MI3= I3
         MTOT= T_AMOUNT
         SELECT 3
         SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
         IF FOUND()
            MDC1= DC1
            MDC2= DC2
            MDC3= DC3
         ENDIF
         SELECT 4
         SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)
         IF FOUND()
            MDTITLE= TITLE
         ENDIF
         SELECT 2
         MSR_NO= MSR_NO+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE
         REPLACE C1 WITH MDC1, C2 WITH MDC2, C3 WITH MDC3, DATE WITH MDATE
         REPLACE DR_CR WITH 'DR', AMOUNT WITH MTOT, SR_NO WITH MSR_NO
         REPLACE DESCRIP WITH MTITLE, VCH_NO2 WITH MVCH_NO2
         UNLOCK
         MTOT1= MTOT1+MTOT
         SELECT 1
         SKIP
      ENDDO
      SELECT 2
      MSR_NO= MSR_NO+1
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE VCH_TYPE WITH MVCH_TYPE, DATE WITH MDATE
      REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
      REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
      REPLACE DR_CR WITH 'CR', SR_NO WITH MSR_NO
      REPLACE DESCRIP WITH 'Purchase ', AMOUNT WITH MTOT1
      UNLOCK
      USE
      SELECT 1
      USE
   ENDIF

SELECT 2
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX1



*   SELECT 2
*   USE st-tran ORDER stidx
   *, stidx1


   DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   APPEND FROM st-cor FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   SET FILTER TO DOC_TYPE=1
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   USE
   @ 23, 50 SAY 'Total '
   @ 23, 63 SAY MTOT1 PICTURE '9999999999.99'
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
******

**********************
PROCEDURE PUR_DEL
**********************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON



SELECT 2
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX1


*SELECT 2
*USE st-tran ORDER stidx
*USE st-tran ORDER stidx1
*USE st-tran ORDER stidx



SET FILTER TO DOC_TYPE=1
GOTO TOP
SELECT 3
USE item ORDER item_idx
SELECT 4
USE ldgrcode ORDER ac1
SELECT 5
USE loc ORDER loc_idx
SELECT 6
USE p-order ORDER pord_idx
SELECT 7
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   R= 3
   T= '   '
   MVCH1= '  '
   MVCH2= '     '
   MDOC_TYPE= 1
   MVCH_TYPE= 7

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 1, 30 SAY 'PURCHASE DELETION '

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'GRN No      ' GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 15 SAY MDOC

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   SELECT 2
   SET ORDER TO STIDX
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This GRN No Does Not Exists ... Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Delete it. If you still want to Delete   '
      @ 14, 15 SAY 'it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   MVCH_TYPE= VCH_TYPE
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   SET ORDER TO 2
   IF INT=1
      @ 21, 1 CLEAR TO 24, 79
      @ 21, 1 SAY 'Voucher Types:'
      @ 21, 17 SAY '1 - '
      @ 21, 21 SAY 'Cash Receipt - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 21, 36 SAY 'CR'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 22, 17 SAY '2 - '
      @ 22, 21 SAY 'Cash Payment - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 22, 36 SAY 'CP'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 21, 58 SAY '3 - '
      @ 21, 62 SAY 'Bank Receipt - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 21, 77 SAY 'BR'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 22, 58 SAY '4 - '
      @ 22, 62 SAY 'Bank Payment - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 22, 77 SAY 'BP'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 17 SAY '5 - '
      @ 23, 21 SAY 'Journal      - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 23, 36 SAY 'JV'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 40 SAY '6 - '
      @ 23, 44 SAY 'Sales - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 23, 52 SAY 'SV'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 58 SAY '7 - '
      @ 23, 62 SAY 'Purchase     - '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 23, 77 SAY 'PV'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ R, 28 SAY 'Voucher Type ' GET MVCH_TYPE PICTURE '9'
      READ
      IF MVCH_TYPE=0
         LOOP
      ENDIF
      IF MVCH_TYPE=1
         T= 'CR-'
      ENDIF
      IF MVCH_TYPE=2
         T= 'CP-'
      ENDIF
      IF MVCH_TYPE=3
         T= 'BR-'
      ENDIF
      IF MVCH_TYPE=4
         T= 'BP-'
      ENDIF
      IF MVCH_TYPE=5
         T= 'JV-'
      ENDIF
      IF MVCH_TYPE=6
         T= 'SV-'
      ENDIF
      IF MVCH_TYPE=7
         T= 'PV-'
      ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 45 SAY T
      @ R, 48 SAY '  -     '

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ R, 48 GET MVCH_NO1 PICTURE '@z 99'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MVCH_NO1=0
         LOOP
      ENDIF
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 48 SAY MVCH1

IF _DOS
      SET COLOR TO 7/1   
ENDIF

      @ R, 51 GET MVCH_NO2 PICTURE '@z 99999'
      READ
      IF MVCH_NO2=0
         LOOP
      ENDIF
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 51 SAY MVCH2

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 2
      SET ORDER TO STIDX1
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)+STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
      IF .not. FOUND()
         @ 23, 1 SAY SPACE(78)
         @ 23, 13 SAY 'This GRN No & Voucher No Does Not Exists ... Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         @ 23, 10 SAY SPACE(80)
         LOOP
      ENDIF
      USE
      SELECT 2
      USE tran ORDER tranent
      SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
      IF .not. FOUND()
         @ 23, 13 SAY 'This Voucher No Does Not Exists ... Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         @ 23, 10 SAY SPACE(75)
         MV_NO= 0
         LOOP
      ENDIF
      IF FLAG='Y'
         CLEAR
         @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
         @ 13, 15 SAY 'you cannot Delete it. If you still want to Delete   '
         @ 14, 15 SAY 'it, please use UN-POST OPTION first. '
         @ 16, 15 SAY 'Press Any Key to Return ............ '
         WAIT ''
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ENDIF
   @ 21, 1 CLEAR TO 24, 79
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   DELETE ALL
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=1
   SELECT 1
   MDATE= DATE
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      MLOC_CODE= LOC_CODE

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ R, 2 SAY 'Credit / Party Code'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 23 SAY '  -  -   '

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ R, 23 GET MC1 PICTURE '99'
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      SELECT 4
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

      @ R, 33 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ R, 15 GET MLOC_CODE PICTURE '9999'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 33 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   CLEAR GETS
   SELECT 1
   GOTO TOP
   SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   GOTO TOP
   DO WHILE .not. EOF()
      SELECT 1
      MI1= I1
      MI2= I2
      MI3= I3
      MORDER_NO= ORDER_NO
      MQTY= T_QTY
      MRATE= RATE
      MAMOUNT= T_AMOUNT

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 2 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ R, 2 SAY MP1
      @ R, 4 SAY '-'
      @ R, 5 SAY MI2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ R, 5 SAY MP2
      @ R, 7 SAY '-'
      @ R, 8 SAY MI3 PICTURE '9999'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ R, 8 SAY MP3

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      SELECT 3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SEEK MI_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 13 SAY ITEM
      SELECT 1

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 30 SAY MORDER_NO PICTURE '99999'
      @ R, 36 SAY MQTY PICTURE '999999999.9999'
      @ R, 51 SAY MRATE PICTURE '99999999.9999'
      @ R, 65 SAY MAMOUNT PICTURE '9999999999.99'

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      R= R+1
      IF R>=23
         WAIT
         @ 9, 0 CLEAR TO 24, 78
         R= 9
      ENDIF
      SKIP
   ENDDO
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='Y'
      IF INT=1
         SELECT 2
         USE tran ORDER tranent
         DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE
         SELECT 2
         USE tran-key ORDER key
         DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE
      ENDIF
      SELECT 2
      USE st-key ORDER st_idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         DELETE
      ENDIF
      USE


SELECT 2
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX1


*      SELECT 2
*      USE st-tran ORDER stidx, stidx1


      DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SET FILTER TO DOC_TYPE=1
      GOTO TOP
   ENDIF
   SELECT 1
   USE st-cor
   DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   USE
   @ 24, 20 CLEAR TO 24, 78
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


********************
PROCEDURE P_COR
***********************
SELECT 1
GOTO TOP
MREC2= RECNO()
DO pur_dsp
GOTO TOP
MREC2= RECNO()
R= 8
CK= 0
DO WHILE .T.
   R= R+1
   IF R>=23
      @ 9, 0 CLEAR TO 24, 79
      R= 9
      IF .not. EOF()
         MREC2= RECNO()
         DO pur-dsp
         GOTO MREC2
         R= 9
      ENDIF
   ENDIF
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 1
      DELETE
   ENDIF
   IF MQES='N'
      SELECT 1
      IF .not. EOF()
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 1
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE 0 TO MI1, MI2, MI3, MQTY, MORDER_NO, MRATE, MAMOUNT, MSR
         DO WHILE .T.

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 2 SAY '  -  -    '

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            @ R, 2 GET MI1 PICTURE '99'
            READ

IF _DOS
            SET COLOR TO /w
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 2 SAY MP1
            @ R, 4 SAY '-'
            @ R, 5 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 5 SAY MP2
            @ R, 7 SAY '-'
            @ R, 8 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 8 SAY MP3

IF _DOS
            SET COLOR TO 7/1 
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
            SELECT 3
            STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
            SEEK MI_CODE
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF
            @ R, 13 SAY ITEM
            MDC1= DC1
            MDC2= DC2
            MDC3= DC3
            IF INT=1
               IF MDC1=0.and.MDC2=0.and.MDC3=0
                  @ 22, 14 SAY 'Debit Code (For Purchase Voucher) does not exist.'
                  @ 23, 14 SAY 'Please feed Debit Code using Item Code Correction Option '
                  WAIT ''
                  CLEAR
                  CLOSE ALL
                  RETURN
               ENDIF
               SELECT 4
               SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)
               IF FOUND()
                  MDTITLE= TITLE
               ENDIF
            ENDIF
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            MT= 0
            DO WHILE MT=0
               @ R, 30 GET MORDER_NO PICTURE '99999'
               READ
               IF MORDER_NO=0
                  EXIT
               ENDIF
               MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
               SET COLOR TO /w
ENDIF

               @ R, 30 SAY MORD

IF _DOS
               SET COLOR TO 7/1 
ENDIF

               SELECT 6
               STORE  +STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
               SEEK ORD
               IF .not. FOUND()
                  ?? CHR(7)
                  @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
                  WAIT ''
                  @ 23, 10 SAY SPACE(58)
                  LOOP
               ENDIF
               MT= 1
            ENDDO
            DO WHILE .T.
               @ R, 36 GET MQTY PICTURE '999999999.9999'
               READ
               IF MQTY=0
                  LOOP
               ENDIF
               EXIT
            ENDDO
            @ R, 51 GET MRATE PICTURE '99999999.9999'
            READ
            MAMOUNT= 0
            MAMOUNT= MQTY*MRATE
            @ R, 65 GET MAMOUNT PICTURE '9999999999.99'
            READ
            EXIT
         ENDDO
         SELECT 1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         IF INT=1
            REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2, VCH_TYPE WITH MVCH_TYPE
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 1, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE, T_AMOUNT WITH MAMOUNT
         REPLACE PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
         UNLOCK
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 1
         MI1= I1
         MI2= I2
         MI3= I3
         MQTY= T_QTY
         MRATE= RATE
         MAMOUNT= T_AMOUNT

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 2 SAY '  -  -    '

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         @ R, 2 GET MI1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO /w
ENDIF

         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ R, 2 SAY MP1
         @ R, 4 SAY '-'
         @ R, 5 GET MI2 PICTURE '99'
         READ
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ R, 5 SAY MP2
         @ R, 7 SAY '-'
         @ R, 8 GET MI3 PICTURE '9999'
         READ
         IF MI3=0
            LOOP
         ENDIF
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ R, 8 SAY MP3

IF _DOS
         SET COLOR TO 7/1 
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
         SELECT 3
         STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
         SEEK MI_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         @ R, 13 SAY ITEM
         MDC1= DC1
         MDC2= DC2
         MDC3= DC3
         IF INT=1
            IF MDC1=0.and.MDC2=0.and.MDC3=0
               @ 22, 14 SAY 'Debit Code (For Purchase Voucher) does not exist.'
               @ 23, 14 SAY 'Please feed Debit Code using Item Code Correction Option '
               WAIT ''
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            SELECT 4
            SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)
            IF FOUND()
               MDTITLE= TITLE
            ENDIF
         ENDIF
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 7
         SEEK XYZ
         IF .not. FOUND()
            @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
            WAIT ''
            @ 21, 1 SAY SPACE(79)
            LOOP
         ENDIF
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 30 SAY MORD

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            SELECT 6
            STORE  +STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '999999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 51 GET MRATE PICTURE '99999999.9999'
         READ
         MAMOUNT= 0
         MAMOUNT= MQTY*MRATE
         @ R, 65 GET MAMOUNT PICTURE '9999999999.99'
         READ
         EXIT
      ENDDO
      SELECT 1
      IF .not. RLOCK()
         rlock()
      ENDIF
      IF INT=1
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2, VCH_TYPE WITH MVCH_TYPE
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 1, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
      REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, RATE WITH MRATE, T_AMOUNT WITH MAMOUNT
      REPLACE PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
      UNLOCK
   ENDIF
   SELECT 1
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      @ R, 79 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      IF R<22
         SELECT 1
         IF .not. EOF()
            SKIP
         ENDIF
         SELECT 1
      ELSE
         @ 9, 1 CLEAR TO 23, 79
         IF .not. EOF()
            SKIP
         ENDIF
      ENDIF
   ENDIF
   IF MANS='N'
      SELECT 1
      GOTO TOP
      SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      GOTO TOP
      MSR= 0
      DO WHILE .not. EOF()
         MSR= MSR+1
         REPLACE SR_NO WITH MSR
         SKIP
      ENDDO
      RETURN
   ENDIF
ENDDO
RETURN




********************
PROCEDURE PUR_DSP
******************
R= 9
@ R, 0 CLEAR TO 24, 79
SELECT 1
GOTO MREC2
DO WHILE .not. EOF()
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   MORDER_NO= ORDER_NO
   MQTY= T_QTY
   MRATE= RATE
   MAMOUNT= T_AMOUNT

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 2 SAY '  -  -    '
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ R, 2 SAY MP1
   @ R, 4 SAY '-'
   @ R, 5 SAY MI2 PICTURE '99'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ R, 5 SAY MP2
   @ R, 7 SAY '-'
   @ R, 8 SAY MI3 PICTURE '9999'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ R, 8 SAY MP3

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   SELECT 3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SEEK MI_CODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   @ R, 13 SAY ITEM
   SELECT 1

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 30 SAY MORDER_NO PICTURE '99999'
   @ R, 36 SAY MQTY PICTURE '999999999.9999'
   @ R, 51 SAY MRATE PICTURE '99999999.9999'
   @ R, 65 SAY MAMOUNT PICTURE '9999999999.99'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   R= R+1
   IF R>=23
      RETURN
   ENDIF
   SKIP
ENDDO
RETURN
********

********************
PROCEDURE PUR_PRT
********************

CLEAR
CLOSE ALL

SELECT 3
USE item ORDER item_idx

SELECT 4
USE ldgrcode ORDER ac1

SELECT 5
USE loc ORDER loc_idx

SELECT 6
USE p-order ORDER pord_idx

SELECT 7
USE item-bal ORDER itm_bal

MUNIT= '    '

IF _DOS
SET COLOR TO 7/1 
ENDIF

IF _WINDOWS
SET COLOR TO
ENDIF

ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 3, 33 SAY 'Goods Receipt Note '
   @ 4, 33 SAY '-------------------'
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 1
   MVCH1= '  '
   MVCH2= '     '
   STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO, MDOC_NO1, CNT
   @ 10, 20 SAY 'From GRN No : '
   @ 10, 35 GET MDOC_NO PICTURE '9999999'
   READ

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF

   MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ENDIF

IF _WINDOWS
SET COLOR TO
ENDIF


   @ 10, 35 SAY MVCH_NO

IF _DOS
   SET COLOR TO 7/1 
ENDIF

IF _WINDOWS
SET COLOR TO
ENDIF


SELECT 2
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX



*   SELECT 2
*   USE st-tran ORDER stidx

   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)

   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This GRN No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF


IF _WINDOWS
SET COLOR TO
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF

   @ 12, 20 SAY 'To   GRN No : '
   @ 12, 35 GET MDOC_NO1 PICTURE '9999999'

   READ

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO1=0
      LOOP
   ENDIF

   MVCH_NO1= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO1))))+LTRIM(STR(MDOC_NO1))

IF _DOS
   SET COLOR TO /w
ENDIF

IF _WINDOWS
SET COLOR TO
ENDIF


   @ 12, 35 SAY MVCH_NO1

IF _DOS
   SET COLOR TO 7/1 
ENDIF

IF _WINDOWS
SET COLOR TO
ENDIF


   SELECT 2
   SET ORDER TO STIDX
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)

   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This GRN No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF

IF _WINDOWS
SET COLOR TO
ENDIF


      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF


   SELECT 2

   COPY TO st-rep FOR DOC_NO>=MDOC_NO.and.DOC_NO<=MDOC_NO1.and.DOC_TYPE=1

   SELECT 2
   USE

   SELECT 1

   USE st-rep

   GOTO TOP

   IF RECCOUNT()=0
      CLEAR
      @ 12, 12 SAY 'No GRN Exist in Data File  -  Press any key to return to menu .... '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

   CLEAR
   ABC= 1

ENDDO

SELECT 1

GOTO TOP

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

HEAD= .T.
CLEAR

DO WHILE .not. EOF()
   MDOC_NO= DOC_NO
   STORE 0 TO MTOT

   DO WHILE DOC_NO=MDOC_NO
      IF HEAD
         L= 3
         IF P='P'
            DO top_prt IN TBL_PROC.PRG
            @ 3, 30 SAY 'Goods Receipt Note '
            L= L+1
            @ L, 30 SAY '------------------ '
            L= L+1
         ELSE
            DO screen IN TBL_PROC.PRG
            @ 3, 30 SAY 'Goods Receipt Note '
            L= L+1
            @ L, 30 SAY '------------------ '
            L= L+1
            CNT= 0
            MREC2= RECNO()
         ENDIF

         MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
         MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
         MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
         @ L, 1 SAY 'GRN No       : '
         @ L, 18 SAY MVCH_NO
         @ L, 30 SAY 'Voucher No : '
         @ L, 43 SAY 'PV-'
         @ L, 46 SAY MVCH1
         @ L, 48 SAY '-'
         @ L, 49 SAY MVCH2
         @ L, 65 SAY 'Date : '+DTOC(DATE)
         L= L+1

         MC1= C_C1
         MC2= C_C2
         MC3= C_C3
         MLOC_CODE= LOC_CODE
         @ L, 1 SAY 'Credit/Party : '
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ L, 18 SAY MPC1
         @ L, 20 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ L, 21 SAY MPC2
         @ L, 23 SAY '-'
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ L, 24 SAY MPC3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE

         SELECT 4
         SEEK MCODE

         IF FOUND()
            @ L, 30 SAY TITLE
         ENDIF

         L= L+1
         SELECT 1
         @ L, 1 SAY 'Location     : '
         MLOC_CODE= REPLICATE('0',4-LEN(LTRIM(STR(LOC_CODE))))+LTRIM(STR(LOC_CODE))
         @ L, 18 SAY MLOC_CODE
         SELECT 5
         SEEK MLOC_CODE
         IF FOUND()
            @ L, 30 SAY LOC
         ENDIF
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 12 SAY 'Item '
         @ L, 31 SAY 'Unit'
         @ L, 37 SAY 'Order #'
         @ L, 51 SAY 'Qty '
         @ L, 59 SAY 'Rate '
         @ L, 74 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEAD= .F.
      ENDIF
      SELECT 1
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 1 SAY MP1
      @ L, 3 SAY '-'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 4 SAY MP2
      @ L, 6 SAY '-'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 7 SAY MP3
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      SELECT 3
      SEEK MI_CODE
      IF FOUND()
         MUNIT= UNIT
         @ L, 12 SAY ITEM
      ENDIF
      SELECT 1
      MQTY= T_QTY
      MRATE= RATE
      MAMT= MQTY*MRATE
      @ L, 33 SAY MUNIT
      @ L, 37 SAY ORDER_NO
      @ L, 41 SAY MQTY PICTURE '9999999.9999'
      @ L, 53 SAY MRATE PICTURE '9999999.9999'
      @ L, 66 SAY MAMT PICTURE '999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      CNT= CNT+1
      IF P='S'.and.L>20
         @ 23, 1 SAY REPLICATE('-',79)
         @ 24, 5 SAY 'For Next Page Press :'
         @ 24, 30 SAY 'For Previous Page :'
         @ 24, 60 SAY 'Quit :'

IF _DOS
         SET COLOR TO /w
ENDIF

         @ 24, 27 SAY CHR(25)
         @ 24, 50 SAY CHR(24)
         @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)

IF _DOS
         SET COLOR TO 7/1
ENDIF


IF _WINDOWS
SET COLOR TO
ENDIF

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

*            RUN del st-rep.dbf

            CLOSE ALL
            RETURN
         ENDIF
         IF READKEY()=4
            CLEAR
            GOTO MREC2
            SKIP  -CNT
            HEAD= .T.
         ENDIF
         IF READKEY()=5
            CLEAR
            HEAD= .T.
         ENDIF
      ENDIF
      IF P='P'.and.L>56
         HEAD= .T.
      ENDIF
   ENDDO
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   @ L, 54 SAY 'Total : '
   @ L, 65 SAY MTOT PICTURE '999999999.99'
   L= L+1
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   IF P='S'
      @ 23, 1 SAY REPLICATE('-',79)
      @ 24, 5 SAY 'For Next Page Press :'
      @ 24, 30 SAY 'For Previous Page :'
      @ 24, 60 SAY 'Quit :'

IF _DOS
      SET COLOR TO /w
ENDIF


IF _WINDOWS
SET COLOR TO
ENDIF

      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)

IF _DOS
      SET COLOR TO 7/1
ENDIF


IF _WINDOWS
SET COLOR TO
ENDIF

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
*         RUN del st-rep.dbf
         CLOSE ALL
         RETURN
      ENDIF
      IF READKEY()=4
         CLEAR
         GOTO MREC2
         SKIP  -CNT
         HEAD= .T.
      ENDIF
      IF READKEY()=5
         CLEAR
         HEAD= .T.
      ENDIF
   ENDIF
   IF P='P'
      HEAD= .T.
   ENDIF
ENDDO
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
*RUN del st-rep.dbf
CLEAR
CLOSE ALL
RETURN
*******

*********************
PROCEDURE PUR_VCH
*********************
CLEAR
CLOSE ALL

SELECT 1
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX


*SELECT 1
*USE st-tran ORDER stidx


SELECT 2
USE item ORDER item_idx
SELECT 3
USE ldgrcode ORDER ac1
STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO
STORE DATE() TO MFDAT, MTDAT
ABC= 0
DO WHILE ABC=0

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   @ 3, 25 SAY 'VERIFICATION VOUCHER  REPORT'
   @ 4, 25 SAY '----------------------------'
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 1
   @ 10, 30 SAY 'From Date : ' GET MFDAT
   @ 12, 30 SAY 'To   Date : ' GET MTDAT
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MTDAT<MFDAT
      @ 23, 10 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 10 SAY '                                        '
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=MFDAT.and.DATE<=MTDAT.and.DOC_TYPE=1
   IF RECCOUNT()=0
      CLEAR
      @ 23, 10 SAY 'No Transaction Exist in These Two Dates -Press any key to return to menu ....'
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   ABC= 1
ENDDO
SELECT 1
GOTO TOP
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
IF P='P'
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
HEADING= .T.
CNT= 0
PG= 0
DO WHILE .not. EOF()
   SELECT 1
   IF HEADING
      L= 3
      DO top-prt
      @ 3, 1 SAY CHR(18)
      @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
      L= L+1
      @ L, 27 SAY '---------------------------'
      L= L+1
      PG= PG+1
      @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
      L= L+1
      @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
      @ L, 67 SAY 'Page : '+STR(PG,3)
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Date'
      @ L, 10 SAY 'Vch # '
      @ L, 20 SAY 'GRN # '
      @ L, 30 SAY 'Party'
      @ L, 59 SAY 'Item'
      @ L, 88 SAY 'Order #'
      @ L, 103 SAY 'Qty'
      @ L, 113 SAY 'Rate'
      @ L, 126 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         DO top-prt
         @ 3, 1 SAY CHR(18)
         @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
         L= L+1
         @ L, 27 SAY '---------------------------'
         L= L+1
         PG= PG+1
         @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
         L= L+1
         @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
         @ L, 67 SAY 'Page : '+STR(PG,3)
         L= L+1
         @ L, 1 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 1 SAY 'Date'
         @ L, 10 SAY 'Vch # '
         @ L, 23 SAY 'GRN # '
         @ L, 30 SAY 'Party'
         @ L, 60 SAY 'Item'
         @ L, 88 SAY 'Order #'
         @ L, 103 SAY 'Qty'
         @ L, 113 SAY 'Rate'
         @ L, 126 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      SELECT 1
      @ L, 1 SAY DATE
      MVCH1= '  '
      MVCH2= '     '
      MDOCNO= '       '
      MDOCNO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
      @ L, 10 SAY 'PV-'
      @ L, 13 SAY MVCH1
      @ L, 15 SAY '-'
      @ L, 16 SAY MVCH2
      @ L, 22 SAY MDOCNO
      MC_C1= C_C1
      MC_C2= C_C2
      MC_C3= C_C3
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      STORE STR(MC_C1,2)+STR(MC_C2,2)+STR(MC_C3,3) TO MP_CODE
      SELECT 3
      SEEK MP_CODE
      IF FOUND()
         @ L, 30 SAY TITLE
      ENDIF
      SELECT 2
      SEEK MI_CODE
      IF FOUND()
         @ L, 60 SAY ITEM
      ENDIF
      SELECT 1
      MQTY= T_QTY
      MRATE= RATE
      MAMT= MQTY*MRATE
      @ L, 87 SAY ORDER_NO
      @ L, 93 SAY MQTY PICTURE '9999999.9999'
      @ L, 106 SAY MRATE PICTURE '99999999.9999'
      @ L, 120 SAY MAMT PICTURE '9999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      IF P='P'.and.L>56
         @ 1, 1 SAY CHR(18)
         L= 3
         HEADING= .T.
      ENDIF
   ENDDO
   L= L+1
ENDDO
@ L, 102 SAY REPLICATE('-',30)
L= L+1
@ L, 105 SAY 'Total : '
@ L, 117 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 102 SAY REPLICATE('-',30)
L= L+1
IF P='P'
   EJECT
   @ 1, 1 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN
******


******************
PROCEDURE PURR_ENT
******************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE item ORDER item_idx
SELECT 4
USE ldgrcode ORDER ac1
SELECT 5
USE loc ORDER loc_idx
SELECT 6
USE p-order ORDER pord_idx
SELECT 7
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 2

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 1, 28 SAY 'PURCHASE RETURN ENTRY '

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MSR_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MAMT
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Purchase Return No  ' GET MDOC_NO PICTURE '9999999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MDOC_NO=0
         LOOP
      ENDIF
      MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 23 SAY MVCH_NO

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      SELECT 2
      USE st-key ORDER st_idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This P.R. No Already Exists  -- Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      SELECT 2
      USE
      @ R, 57 SAY 'Date ' GET MDATE
      READ
      DO chk IN TBL_PROC.PRG
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Debit / Party Code'

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 23 SAY '  -  -   '

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         @ R, 23 GET MC1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO /w
ENDIF

         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ R, 23 SAY MPC1
         @ R, 25 SAY '-'
         @ R, 26 GET MC2 PICTURE '99'
         READ
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ R, 26 SAY MPC2
         @ R, 28 SAY '-'
         @ R, 29 GET MC3 PICTURE '999'
         READ
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ R, 29 SAY MPC3

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         SELECT 4
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

         @ R, 33 SAY TITLE
         MTITLE= TITLE

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         ABC= 1
      ENDDO
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
         READ
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 23, 45 CLEAR TO 23, 78
         IF MLOC_CODE=0
            LOOP
         ENDIF
         SELECT 5
         SEEK MLOC_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         @ R, 33 SAY LOC
         MLOC= LOC
         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 2 SAY 'Item Code'
      @ R, 13 SAY 'Item '
      @ R, 30 SAY 'Order #'
      @ R, 43 SAY 'Qty '
      @ R, 53 SAY 'Rate '
      @ R, 65 SAY 'Amount '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 2 SAY '  -  -    '

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            @ R, 2 GET MI1 PICTURE '99'
            READ

IF _DOS
            SET COLOR TO /w
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 2 SAY MP1
            @ R, 4 SAY '-'
            @ R, 5 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 5 SAY MP2
            @ R, 7 SAY '-'
            @ R, 8 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 8 SAY MP3

IF _DOS
            SET COLOR TO 7/1 
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
            SELECT 3
            SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF
            @ R, 13 SAY ITEM
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            ABC= 1
         ENDDO
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 30 SAY MORD

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            SELECT 6
            STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 49 GET MRATE PICTURE '99999999.9999'
         READ
         MTOT= 0
         MTOT= MQTY*MRATE
         @ R, 63 GET MTOT PICTURE '9999999999.99'
         READ
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'Y'
            @ R, 79 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 1
         USE st-work
         MSR_NO= MSR_NO+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE D_D1 WITH MC1, D_D2 WITH MC2, D_D3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MTOT
         REPLACE SR_NO WITH MSR_NO
         UNLOCK
         USE
         MAMT= MAMT+MTOT
         IF ANS='Y'
            STORE 0 TO MI1, MI2, MI3, MRATE, MQTY, MORDER_NO
            R= R+1
            LOOP
         ENDIF
         @ 23, 50 SAY 'Total '
         @ 23, 63 SAY MAMT PICTURE '9999999999.99'
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
            READ
         ENDDO
         @ 24, 20 CLEAR TO 24, 78
         IF ANS='Y'
            SELECT 1
            USE st-work
            SET FILTER TO DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
            DO pr-cor
         ENDIF
         SELECT 1
         USE

SELECT 2
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX


*         SELECT 2
*         USE st-tran ORDER stidx


         APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         SELECT 2
         USE st-key ORDER st_idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
         ENDIF
         USE
         SELECT 1
         USE st-work
         DELETE ALL FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
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


********************
PROCEDURE PURR_COR
********************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON


SELECT 2
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX


*SELECT 2
*USE st-tran ORDER stidx


SET FILTER TO DOC_TYPE=2
GOTO TOP
SELECT 3
USE item ORDER item_idx
SELECT 4
USE ldgrcode ORDER ac1
SELECT 5
USE loc ORDER loc_idx
SELECT 6
USE p-order ORDER pord_idx
SELECT 7
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 2

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 1, 28 SAY 'PURCHASE RETURN CORRECTION'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'P.R. No     ' GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 15 SAY MDOC

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   SELECT 2
   SET ORDER TO STIDX
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This P.R. No Does Not Exists ... Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Change/Correct it. If you still want to  '
      @ 14, 15 SAY 'Change/Correct it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SELECT 1
   MDATE= DATE
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= D_D1
      MC2= D_D2
      MC3= D_D3
      MLOC_CODE= LOC_CODE

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ R, 2 SAY 'Debit / Party Code'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 23 SAY '  -  -   '

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ R, 23 GET MC1 PICTURE '99'
      READ
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      SELECT 4
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

      @ R, 33 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ R, 15 GET MLOC_CODE PICTURE '9999'
      READ

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 33 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   DO pr_cor
   SELECT 1
   GOTO TOP
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE DATE WITH MDATE
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE D_D1 WITH MC1
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE D_D2 WITH MC2
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE D_D3 WITH MC3
   USE

SELECT 2
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX



*   SELECT 2
*   USE st-tran ORDER stidx


   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   APPEND FROM st-cor FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SET FILTER TO DOC_TYPE=2
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   USE
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

********************
PROCEDURE PURR_DEL
********************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON

SELECT 2
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX


*SELECT 2
*USE st-tran ORDER stidx


SET FILTER TO DOC_TYPE=2
GOTO TOP
SELECT 3
USE item ORDER item_idx
SELECT 4
USE ldgrcode ORDER ac1
SELECT 5
USE loc ORDER loc_idx
SELECT 6
USE p-order ORDER pord_idx
SELECT 7
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 2

IF _DOS
   SET COLOR TO +7/1 
ENDIF

   @ 1, 28 SAY 'PURCHASE RETURN DELETION'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'P.R. No     ' GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 15 SAY MDOC

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   SELECT 2
   SET ORDER TO STIDX
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This P.R. No Does Not Exists ... Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Change/Correct it. If you still want to  '
      @ 14, 15 SAY 'Change/Correct it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SELECT 1
   MDATE= DATE
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= D_D1
      MC2= D_D2
      MC3= D_D3
      MLOC_CODE= LOC_CODE

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ R, 2 SAY 'Debit / Party Code'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 23 SAY '  -  -   '

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ R, 23 GET MC1 PICTURE '99'
      READ
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      SELECT 4
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

      @ R, 33 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    '

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ R, 15 GET MLOC_CODE PICTURE '9999'
      READ

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 33 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   SELECT 1
   SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   GOTO TOP
   MREC2= RECNO()
   DO pr_dsp
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78


SELECT 2
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX


*   SELECT 2
*   USE st-tran ORDER stidx


   IF ANS='Y'
      DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      SELECT 1
      USE st-key ORDER st_idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         DELETE
      ENDIF
      USE
   ENDIF
   SELECT 2
   SET FILTER TO DOC_TYPE=2
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   USE
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Any More Deletion - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******

******************
PROCEDURE PR_COR
******************
SELECT 1
GOTO TOP
MREC2= RECNO()
DO pr_dsp
GOTO TOP
MREC2= RECNO()
R= 8
CK= 0
DO WHILE .T.
   R= R+1
   IF R>=23
      @ 9, 0 CLEAR TO 24, 79
      R= 9
      IF .not. EOF()
         MREC2= RECNO()
         DO sal-dsp
         GOTO MREC2
         R= 9
      ENDIF
   ENDIF
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 1
      DELETE
   ENDIF
   IF MQES='N'
      SELECT 1
      IF .not. EOF()
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 1
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE 0 TO MI1, MI2, MI3, MQTY, MORDER_NO, MRATE, MAMOUNT
         DO WHILE .T.

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 2 SAY '  -  -    '

IF _DOS
            SET COLOR TO 7/1
ENDIF

            @ R, 2 GET MI1 PICTURE '99'
            READ

IF _DOS
            SET COLOR TO /w
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 2 SAY MP1
            @ R, 4 SAY '-'
            @ R, 5 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 5 SAY MP2
            @ R, 7 SAY '-'
            @ R, 8 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 8 SAY MP3

IF _DOS
            SET COLOR TO 7/1
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
            SELECT 3
            STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
            SEEK MI_CODE
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF
            @ R, 13 SAY ITEM
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            MT= 0
            DO WHILE MT=0
               @ R, 30 GET MORDER_NO PICTURE '99999'
               READ
               IF MORDER_NO=0
                  EXIT
               ENDIF
               MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
               SET COLOR TO /w
ENDIF

               @ R, 30 SAY MORD

IF _DOS
               SET COLOR TO 7/1 
ENDIF

               SELECT 6
               STORE  +STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
               SEEK ORD
               IF .not. FOUND()
                  ?? CHR(7)
                  @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
                  WAIT ''
                  @ 23, 10 SAY SPACE(58)
                  LOOP
               ENDIF
               MT= 1
            ENDDO
            DO WHILE .T.
               @ R, 36 GET MQTY PICTURE '9999999.9999'
               READ
               IF MQTY=0
                  LOOP
               ENDIF
               EXIT
            ENDDO
            @ R, 49 GET MRATE PICTURE '99999999.9999'
            READ
            MAMOUNT= 0
            MAMOUNT= MQTY*MRATE
            @ R, 63 GET MAMOUNT PICTURE '9999999999.99'
            READ
            EXIT
         ENDDO
         SELECT 1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE D_D1 WITH MC1, D_D2 WITH MC2, D_D3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
         UNLOCK
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 1
         MI1= I1
         MI2= I2
         MI3= I3
         MQTY= T_QTY
         MRATE= PSR_UPRICE
         MAMOUNT= PSR_TPRICE

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 2 SAY '  -  -    '

IF _DOS
         SET COLOR TO 7/1
ENDIF

         @ R, 2 GET MI1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO /w
ENDIF

         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ R, 2 SAY MP1
         @ R, 4 SAY '-'
         @ R, 5 GET MI2 PICTURE '99'
         READ
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ R, 5 SAY MP2
         @ R, 7 SAY '-'
         @ R, 8 GET MI3 PICTURE '9999'
         READ
         IF MI3=0
            LOOP
         ENDIF
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ R, 8 SAY MP3

IF _DOS
         SET COLOR TO 7/1
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
         SELECT 3
         STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
         SEEK MI_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         @ R, 13 SAY ITEM
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 7
         SEEK XYZ
         IF .not. FOUND()
            @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
            WAIT ''
            @ 21, 1 SAY SPACE(79)
            LOOP
         ENDIF
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 30 SAY MORD

IF _DOS
            SET COLOR TO 7/1
ENDIF

            SELECT 6
            STORE  +STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 49 GET MRATE PICTURE '99999999.9999'
         READ
         MAMOUNT= 0
         MAMOUNT= MQTY*MRATE
         @ R, 63 GET MAMOUNT PICTURE '9999999999.99'
         READ
         EXIT
      ENDDO
      SELECT 1
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
      REPLACE D_D1 WITH MC1, D_D2 WITH MC2, D_D3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
      UNLOCK
   ENDIF
   SELECT 1
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      @ R, 79 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      IF R<22
         SELECT 1
         IF .not. EOF()
            SKIP
         ENDIF
         SELECT 1
      ELSE
         @ 9, 1 CLEAR TO 23, 79
         IF .not. EOF()
            SKIP
         ENDIF
      ENDIF
   ENDIF
   IF MANS='N'
      SELECT 1
      GOTO TOP
      SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      GOTO TOP
      MSR= 0
      DO WHILE .not. EOF()
         MSR= MSR+1
         REPLACE SR_NO WITH MSR
         SKIP
      ENDDO
      RETURN
   ENDIF
ENDDO
RETURN
******


*****************
PROCEDURE PR_DSP
*****************
R= 9
@ R, 0 CLEAR TO 24, 79
SELECT 1
GOTO MREC2
DO WHILE .not. EOF()
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   MORDER_NO= ORDER_NO
   MQTY= T_QTY
   MRATE= PSR_UPRICE
   MAMOUNT= PSR_TPRICE

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 2 SAY '  -  -    '
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ R, 2 SAY MP1
   @ R, 4 SAY '-'
   @ R, 5 SAY MI2 PICTURE '99'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ R, 5 SAY MP2
   @ R, 7 SAY '-'
   @ R, 8 SAY MI3 PICTURE '9999'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ R, 8 SAY MP3

IF _DOS
   SET COLOR TO 7/1
ENDIF

   SELECT 3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SEEK MI_CODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   @ R, 13 SAY ITEM
   SELECT 1

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 30 SAY MORDER_NO PICTURE '99999'
   @ R, 36 SAY MQTY PICTURE '9999999.9999'
   @ R, 49 SAY MRATE PICTURE '99999999.9999'
   @ R, 63 SAY MAMOUNT PICTURE '9999999999.99'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   R= R+1
   IF R>=23
      RETURN
   ENDIF
   SKIP
ENDDO
RETURN
******

*****************
PROCEDURE PURRPRT
*****************
CLEAR
CLOSE ALL
SELECT 3
USE item ORDER item_idx
SELECT 4
USE ldgrcode ORDER ac1
SELECT 5
USE loc ORDER loc_idx
SELECT 6
USE p-order ORDER pord_idx
SELECT 7
USE item-bal ORDER itm_bal
MUNIT= '    '

IF _DOS
SET COLOR TO 7/1 
ENDIF

ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 3, 33 SAY 'Purchase Return Note '
   @ 4, 33 SAY '-------------------- '
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 2
   STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO, MDOC_NO1, CNT
   @ 10, 20 SAY 'From P.R.#  : '
   @ 10, 35 GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 10, 35 SAY MVCH_NO

IF _DOS
   SET COLOR TO 7/1 
ENDIF

SELECT 2
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX


*   SELECT 2
*   USE st-tran ORDER stidx


   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This P.R. No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   @ 12, 20 SAY 'To   P.R.#  : '
   @ 12, 35 GET MDOC_NO1 PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO1=0
      LOOP
   ENDIF
   MVCH_NO1= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO1))))+LTRIM(STR(MDOC_NO1))

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 35 SAY MVCH_NO1

IF _DOS
   SET COLOR TO 7/1 
ENDIF

   SELECT 2
   SET ORDER TO STIDX
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This P.R. No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 2
   COPY TO st-rep FOR DOC_NO>=MDOC_NO.and.DOC_NO<=MDOC_NO1.and.DOC_TYPE=MDOC_TYPE
   SELECT 2
   USE
   SELECT 1
   USE st-rep
   GOTO TOP
   IF RECCOUNT()=0
      CLEAR
      @ 12, 12 SAY 'No GRN Exist in Data File  -  Press any key to return to menu .... '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   ABC= 1
ENDDO
SELECT 1
GOTO TOP
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
HEAD= .T.
CLEAR
DO WHILE .not. EOF()
   MDOC_NO= DOC_NO
   STORE 0 TO MTOT
   DO WHILE DOC_NO=MDOC_NO
      IF HEAD
         L= 3
         IF P='P'
            DO top-prt
            @ 3, 30 SAY 'Purchase Return Note '
            L= L+1
            @ L, 30 SAY '-------------------- '
            L= L+1
         ELSE
            DO screen IN TBL_PROC.PRG
            @ 3, 30 SAY 'Purchase Return Note '
            L= L+1
            @ L, 30 SAY '-------------------- '
            L= L+1
            CNT= 0
            MREC2= RECNO()
         ENDIF
         MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
         @ L, 1 SAY 'P.R. No       : '
         @ L, 18 SAY MVCH_NO
         @ L, 65 SAY 'Date : '+DTOC(DATE)
         L= L+1
         MC1= D_D1
         MC2= D_D2
         MC3= D_D3
         MLOC_CODE= LOC_CODE
         @ L, 1 SAY 'Debit/Party : '
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ L, 18 SAY MPC1
         @ L, 20 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ L, 21 SAY MPC2
         @ L, 23 SAY '-'
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ L, 24 SAY MPC3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SELECT 4
         SEEK MCODE
         IF FOUND()
            @ L, 30 SAY TITLE
         ENDIF
         L= L+1
         SELECT 1
         @ L, 1 SAY 'Location     : '
         MLOC_CODE= REPLICATE('0',4-LEN(LTRIM(STR(LOC_CODE))))+LTRIM(STR(LOC_CODE))
         @ L, 18 SAY MLOC_CODE
         SELECT 5
         SEEK MLOC_CODE
         IF FOUND()
            @ L, 30 SAY LOC
         ENDIF
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 12 SAY 'Item '
         @ L, 31 SAY 'Unit'
         @ L, 37 SAY 'Order #'
         @ L, 51 SAY 'Qty '
         @ L, 59 SAY 'Rate '
         @ L, 74 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEAD= .F.
      ENDIF
      SELECT 1
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 1 SAY MP1
      @ L, 3 SAY '-'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 4 SAY MP2
      @ L, 6 SAY '-'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 7 SAY MP3
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      SELECT 3
      SEEK MI_CODE
      IF FOUND()
         MUNIT= UNIT
         @ L, 12 SAY ITEM
      ENDIF
      SELECT 1
      MQTY= T_QTY
      MRATE= PSR_UPRICE
      MAMT= MQTY*MRATE
      @ L, 33 SAY MUNIT
      @ L, 37 SAY ORDER_NO
      @ L, 41 SAY MQTY PICTURE '9999999.9999'
      @ L, 53 SAY MRATE PICTURE '9999999.9999'
      @ L, 66 SAY MAMT PICTURE '999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      CNT= CNT+1
      IF P='S'.and.L>20
         @ 23, 1 SAY REPLICATE('-',79)
         @ 24, 5 SAY 'For Next Page Press :'
         @ 24, 30 SAY 'For Previous Page :'
         @ 24, 60 SAY 'Quit :'

IF _DOS
         SET COLOR TO /w
ENDIF

         @ 24, 27 SAY CHR(25)
         @ 24, 50 SAY CHR(24)
         @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)

IF _DOS
         SET COLOR TO 7/1
ENDIF

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
            RUN del st-rep.dbf
            CLOSE ALL
            RETURN
         ENDIF
         IF READKEY()=4
            CLEAR
            GOTO MREC2
            SKIP  -CNT
            HEAD= .T.
         ENDIF
         IF READKEY()=5
            CLEAR
            HEAD= .T.
         ENDIF
      ENDIF
      IF P='P'.and.L>56
         HEAD= .T.
      ENDIF
   ENDDO
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   @ L, 54 SAY 'Total : '
   @ L, 65 SAY MTOT PICTURE '999999999.99'
   L= L+1
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   IF P='S'
      @ 23, 1 SAY REPLICATE('-',79)
      @ 24, 5 SAY 'For Next Page Press :'
      @ 24, 30 SAY 'For Previous Page :'
      @ 24, 60 SAY 'Quit :'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)

IF _DOS
      SET COLOR TO 7/1
ENDIF

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
         RUN del st-rep.dbf
         CLOSE ALL
         RETURN
      ENDIF
      IF READKEY()=4
         CLEAR
         GOTO MREC2
         SKIP  -CNT
         HEAD= .T.
      ENDIF
      IF READKEY()=5
         CLEAR
         HEAD= .T.
      ENDIF
   ENDIF
   IF P='P'
      HEAD= .T.
   ENDIF
ENDDO
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
RUN del st-rep.dbf
CLEAR
CLOSE ALL
RETURN
*******


******************
PROCEDURE PURRVCH
******************
CLEAR
CLOSE ALL

SELECT 1
USE
USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX


*SELECT 1
*USE st-tran ORDER stidx


SELECT 2
USE item ORDER item_idx
SELECT 3
USE ldgrcode ORDER ac1
STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO
STORE DATE() TO MFDAT, MTDAT
ABC= 0
DO WHILE ABC=0

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   @ 3, 25 SAY 'VERIFICATION VOUCHER  REPORT'
   @ 4, 25 SAY '----------------------------'
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 1
   @ 10, 30 SAY 'From Date : ' GET MFDAT
   @ 12, 30 SAY 'To   Date : ' GET MTDAT
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MTDAT<MFDAT
      @ 23, 10 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 10 SAY '                                        '
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=MFDAT.and.DATE<=MTDAT.and.DOC_TYPE=2
   IF RECCOUNT()=0
      CLEAR
      @ 23, 10 SAY 'No Transaction Exist in These Two Dates -Press any key to return to menu ....'
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   ABC= 1
ENDDO
SELECT 1
GOTO TOP
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
IF P='P'
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
HEADING= .T.
CNT= 0
PG= 0
DO WHILE .not. EOF()
   SELECT 1
   IF HEADING
      L= 3
      DO top-prt
      @ 3, 1 SAY CHR(18)
      @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
      L= L+1
      @ L, 27 SAY '---------------------------'
      L= L+1
      PG= PG+1
      @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
      L= L+1
      @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
      @ L, 67 SAY 'Page : '+STR(PG,3)
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 10 SAY 'PR. # '
      @ L, 26 SAY 'Party'
      @ L, 57 SAY 'Item'
      @ L, 88 SAY 'Order #'
      @ L, 103 SAY 'Qty'
      @ L, 113 SAY 'Rate'
      @ L, 126 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         L= 3
         DO top-prt
         @ 3, 1 SAY CHR(18)
         @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
         L= L+1
         @ L, 27 SAY '---------------------------'
         L= L+1
         PG= PG+1
         @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
         L= L+1
         @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
         @ L, 67 SAY 'Page : '+STR(PG,3)
         L= L+1
         @ L, 1 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 10 SAY 'PR. # '
         @ L, 26 SAY 'Party'
         @ L, 57 SAY 'Item'
         @ L, 88 SAY 'Order #'
         @ L, 103 SAY 'Qty'
         @ L, 113 SAY 'Rate'
         @ L, 126 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      SELECT 1
      @ L, 1 SAY DATE
      MDOCNO= REPLICATE('0',5-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 13 SAY 'PR-'
      @ L, 16 SAY MDOCNO
      MD_D1= D_D1
      MD_D2= D_D2
      MD_D3= D_D3
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      STORE STR(MD_D1,2)+STR(MD_D2,2)+STR(MD_D3,3) TO MP_CODE
      SELECT 3
      SEEK MP_CODE
      IF FOUND()
         @ L, 24 SAY TITLE
      ENDIF
      SELECT 2
      SEEK MI_CODE
      IF FOUND()
         @ L, 57 SAY ITEM
      ENDIF
      SELECT 1
      MQTY= T_QTY
      MRATE= PSR_UPRICE
      MAMT= MQTY*MRATE
      @ L, 87 SAY ORDER_NO
      @ L, 93 SAY MQTY PICTURE '9999999.9999'
      @ L, 106 SAY MRATE PICTURE '99999999.9999'
      @ L, 119 SAY MAMT PICTURE '9999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      IF P='P'.and.L>56
         @ 1, 1 SAY CHR(18)
         L= 3
         HEADING= .T.
      ENDIF
   ENDDO
   L= L+1
ENDDO
@ L, 102 SAY REPLICATE('-',30)
L= L+1
@ L, 105 SAY 'Total : '
@ L, 117 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 102 SAY REPLICATE('-',30)
L= L+1
IF P='P'
   EJECT
   @ 1, 1 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN
*******


******************
PROCEDURE P_COR1
******************
SELECT 1
GOTO TOP
MREC2= RECNO()
DO pur-dsp1
GOTO TOP
MREC2= RECNO()
R= 8
CK= 0
DO WHILE .T.
   R= R+1
   IF R>=23
      @ 9, 0 CLEAR TO 24, 79
      R= 9
      IF .not. EOF()
         MREC2= RECNO()
         DO pur-dsp1
         GOTO MREC2
         R= 9
      ENDIF
   ENDIF
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 1
      DELETE
      IF EOF()
         EXIT
      ENDIF
   ENDIF
   IF MQES='N'
      SELECT 1
      IF .not. EOF()
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 1
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE 0 TO MI1, MI2, MI3, MQTY, MORDER_NO, MRATE, MAMOUNT
         DO WHILE .T.

IF _DOS
            SET COLOR TO /w
ENDIF

            @ R, 2 SAY '  -  -    '

IF _DOS
            SET COLOR TO 7/1
ENDIF

            @ R, 2 GET MI1 PICTURE '99'
            READ

IF _DOS
            SET COLOR TO /w
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 2 SAY MP1
            @ R, 4 SAY '-'
            @ R, 5 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 5 SAY MP2
            @ R, 7 SAY '-'
            @ R, 8 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 8 SAY MP3

IF _DOS
            SET COLOR TO 7/1
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
            SELECT 3
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

            @ R, 13 SAY ITEM

IF _DOS
            SET COLOR TO 7/1
ENDIF

            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            DO WHILE .T.
               @ R, 45 GET MQTY PICTURE '9999999.9999'
               READ
               IF MQTY=0
                  LOOP
               ENDIF
               EXIT
            ENDDO
            EXIT
         ENDDO
         SELECT 1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, DATE WITH MDATE
         REPLACE DEPT_CODE WITH MDEPT_CODE, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
         UNLOCK
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 1
         MI1= I1
         MI2= I2
         MI3= I3
         MQTY= T_QTY

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 2 SAY '  -  -    '

IF _DOS
         SET COLOR TO 7/1
ENDIF

         @ R, 2 GET MI1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO /w
ENDIF

         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ R, 2 SAY MP1
         @ R, 4 SAY '-'
         @ R, 5 GET MI2 PICTURE '99'
         READ
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ R, 5 SAY MP2
         @ R, 7 SAY '-'
         @ R, 8 GET MI3 PICTURE '9999'
         READ
         IF MI3=0
            LOOP
         ENDIF
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ R, 8 SAY MP3

IF _DOS
         SET COLOR TO 7/1
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
         SELECT 3
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

         @ R, 13 SAY ITEM

IF _DOS
         SET COLOR TO 7/1
ENDIF

         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 7
         SEEK XYZ
         IF .not. FOUND()
            @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
            WAIT ''
            @ 21, 1 SAY SPACE(79)
            LOOP
         ENDIF
         DO WHILE .T.
            @ R, 45 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         EXIT
      ENDDO
      SELECT 1
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, DATE WITH MDATE
      REPLACE DEPT_CODE WITH MDEPT_CODE, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
      UNLOCK
   ENDIF
   SELECT 1
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      @ R, 79 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      SKIP
   ENDIF
   IF MANS='N'
      SELECT 1
      GOTO TOP
      SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      GOTO TOP
      MSR= 0
      DO WHILE .not. EOF()
         MSR= MSR+1
         REPLACE SR_NO WITH MSR
         SKIP
      ENDDO
      RETURN
   ENDIF
ENDDO
RETURN
******

*******************
PROCEDURE PUR_DSP1
*******************
R= 9
@ R, 0 CLEAR TO 24, 79
SELECT 1
GOTO MREC2
DO WHILE .not. EOF()
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   MQTY= T_QTY

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 2 SAY '  -  -    '
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ R, 2 SAY MP1
   @ R, 4 SAY '-'
   @ R, 5 SAY MI2 PICTURE '99'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ R, 5 SAY MP2
   @ R, 7 SAY '-'
   @ R, 8 SAY MI3 PICTURE '9999'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ R, 8 SAY MP3

IF _DOS
   SET COLOR TO 7/1
ENDIF

   SELECT 3
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

   @ R, 13 SAY ITEM

IF _DOS
   SET COLOR TO 7/1
ENDIF

   SELECT 1

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 45 SAY MQTY PICTURE '9999999.9999'

IF _DOS
   SET COLOR TO 7/1
ENDIF


   R= R+1
   IF R>=23
      RETURN
   ENDIF
   SKIP
ENDDO
RETURN
********

