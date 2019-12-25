* PROGRAM: PRODPROC.PRG
* WRITTEN BY : AMJAD IZHAR
* WRITTEN ON : 31-MAY-2005
* THIS REVISION: 31-MAY-2005 3:25 PM
******************************************


**********************
PROCEDURE PROD_MEN
**********************

DO WHILE .T.

   SET DELETED ON

   USE st-key ORDER st_idx

   DELETE ALL FOR FLAG=' '

   CLEAR
   CLOSE ALL

   L= 8
   M= 23


IF _DOS
   SET COLOR TO 7/1
ENDIF


   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG


IF _DOS
   SET COLOR TO +7/1
ENDIF

IF _WINDOWS
SET COLOR TO
ENDIF


   @ 6, 28 SAY '  Production  Receipts  '

IF _DOS
   SET COLOR TO 7/1
ENDIF

IF _WINDOWS
SET COLOR TO
ENDIF

   @ L+0, M PROMPT ' 1 - Production Receipts .. Entry      '
   @ L+1, M PROMPT ' 2 - Production Receipts .. Correction '
   @ L+2, M PROMPT ' 3 - Production Receipts .. Deletion   '
   @ L+3, M PROMPT ' 4 - Production Receipts .. Printing   '
   @ L+4, M PROMPT ' 5 - Production Receipts &             '
   @ L+5, M SAY '      Issue to Production with Formula  '
   @ L+7, M PROMPT ' 9 - Return to Previous Menu           '

   MCHOICE= 0

   MENU TO MCHOICE

   DO CASE
      CASE MCHOICE=6
         CLEAR
         CLOSE DATABASE
         RETURN
      CASE MCHOICE=5
         CLEAR
         DO prodent
      CASE MCHOICE=4
         CLEAR
         DO prod_prt
      CASE MCHOICE=3
         CLEAR
         DO prod_del
      CASE MCHOICE=2
         CLEAR
         DO prod_cor
      CASE MCHOICE=1
         CLEAR
         DO prod_ent
   ENDCASE
ENDDO
RETURN
**********************

**************************
PROCEDURE PROD_ENT
**************************

CLEAR
CLOSE ALL


PUBLIC MDOC_NO
MDOC_NO=0

SELECT 1
USE st-tran EXCLUSIVE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG stidx
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG stidx1
USE

SELECT 1
USE st-key EXCLUSIVE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG st_idx
USE


SELECT 1
USE st-tran ALIAS st_tran EXCLUSIVE
INDEX ON STR(doc_type,1)+STR(doc_no,7) TAG docno
SET ORDER TO docno
SET FILTER TO doc_type = 5
GOTO BOTTOM
MDOC_NO = doc_no + 1
USE


SET DATE brit
SET DELETED ON

SELECT 3
USE item ORDER item_idx

SELECT 4
USE dept ORDER dept_idx

SELECT 5
USE loc ORDER loc_idx

SELECT 7

USE item-bal ORDER itm_bal

DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 5

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 1, 20 SAY 'INVENTORY : PRODUCTION RECEIPTS ENTRY  ( Factory - IN )'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MSR_NO

   DO WHILE .T.

      @ 23, 50 SAY 'Press Ctrl Q To Quit '

      @ R, 2 SAY 'Production Note No  ' GET MDOC_NO PICTURE '9999999'
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
      FLUSH
      SELECT 2
      USE
      @ R, 57 SAY 'Date ' GET MDATE
      READ
      DO chk IN TBL_PROC.PRG
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Department Code '
         @ R, 23 GET MDEPT_CODE PICTURE '999'
         READ

IF _DOS
         SET COLOR TO /w
ENDIF

         MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
         @ R, 23 SAY MPC1

IF _DOS
         SET COLOR TO 7/1
ENDIF

         SELECT 4
         SEEK MDEPT_CODE
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

         @ R, 33 SAY DEPT
         MDEPT= DEPT

IF _DOS
         SET COLOR TO 7/1
ENDIF

         ABC= 1
      ENDDO
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Location            ' GET MLOC_CODE PICTURE '9999'
         READ
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

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 33 SAY LOC
         MLOC= LOC

IF _DOS
         SET COLOR TO 7/1
ENDIF

         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 2 SAY 'Item Code'
      @ R, 13 SAY 'Item '
      @ R, 45 SAY 'Qty '
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
               
               *LOOP
            
            ENDIF
            
            ABC= 1
         
         ENDDO
         
         
         DO WHILE .T.
            @ R, 45 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'Y'
            @ R, 78 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 1
         USE st-work
         MSR_NO= MSR_NO+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 5, DATE WITH MDATE
         REPLACE DEPT_CODE WITH MDEPT_CODE, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, SR_NO WITH MSR_NO
         UNLOCK
         FLUSH
         USE
         IF ANS='Y'
            STORE 0 TO MI1, MI2, MI3, MRATE, MQTY, MORDER_NO
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
            SELECT 1
            USE st-work
            SET FILTER TO DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
            DO p_cor1 IN PRODPROC
         ENDIF
         SELECT 1
         USE
         SELECT 2
         USE st-tran ORDER stidx
         APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         SELECT 2
         USE st-key ORDER st_idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
            FLUSH
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
***************

**********************
PROCEDURE PROD_COR
**********************

CLEAR
CLOSE ALL

SET DATE brit
SET DELETED ON

SELECT 1
USE st-tran EXCLUSIVE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG stidx
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG stidx1
USE

SELECT 1
USE st-key EXCLUSIVE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG st_idx
USE



SELECT 2

USE st-tran ORDER stidx
SET FILTER TO DOC_TYPE=5

GOTO TOP

SELECT 3
USE item ORDER item_idx
SELECT 4
USE dept ORDER dept_idx
SELECT 5
USE loc ORDER loc_idx
SELECT 7
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 5

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 1, 26 SAY 'PRODUCTION RECEIPTS CORRECTION '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MNAVG
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Production Note No  ' GET MDOC_NO PICTURE '9999999'
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

   @ R, 23 SAY MVCH_NO

IF _DOS
   SET COLOR TO 7/1
ENDIF

   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 10 SAY 'This P.R. No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 23, 9 SAY SPACE(75)
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
   MDEPT_CODE= DEPT_CODE
   MLOC_CODE= LOC_CODE
   @ R, 57 SAY 'Date ' GET MDATE
   R= R+1
   DO chk IN TBL_PROC.PRG
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Department Code '
      @ R, 23 GET MDEPT_CODE PICTURE '999'
      READ

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ R, 23 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 4
      SEEK MDEPT_CODE
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

      @ R, 33 SAY DEPT
      MDEPT= DEPT

IF _DOS
      SET COLOR TO 7/1
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location            ' GET MLOC_CODE PICTURE '9999'
      READ
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

IF _DOS
      SET COLOR TO /W
ENDIF

      @ R, 33 SAY LOC
      MLOC= LOC

IF _DOS
      SET COLOR TO 7/1
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 45 SAY 'Qty '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   SELECT 1
   DO p_cor1
   SELECT 1
   GOTO TOP
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE DATE WITH MDATE
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE DEPT_CODE WITH MDEPT_CODE
   FLUSH
   USE
   SELECT 2
   USE st-tran ORDER stidx
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   APPEND FROM st-cor FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SET FILTER TO DOC_TYPE=5
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
***************

************************
PROCEDURE PROD_DEL
************************
CLEAR
CLOSE ALL

SELECT 1
USE st-tran EXCLUSIVE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG stidx
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG stidx1
USE

SELECT 1
USE st-key EXCLUSIVE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG st_idx
USE



SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran ORDER stidx
SET FILTER TO DOC_TYPE=5
GOTO TOP
SELECT 3
USE item ORDER item_idx
SELECT 4
USE dept ORDER dept_idx
SELECT 5
USE loc ORDER loc_idx
SELECT 7
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 5

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 1, 28 SAY 'PRODUCTION RECEIPTS DELETION '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'Production Note No  ' GET MDOC_NO PICTURE '9999999'
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

   @ R, 23 SAY MVCH_NO

IF _DOS
   SET COLOR TO 7/1
ENDIF

   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 10 SAY 'This P.R. No Does Not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 23, 9 SAY SPACE(75)
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
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SELECT 1
   MDATE= DATE
   MDEPT_CODE= DEPT_CODE
   MLOC_CODE= LOC_CODE
   @ R, 57 SAY 'Date ' GET MDATE
   DO chk IN TBL_PROC.PRG
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Department Code '
      @ R, 23 GET MDEPT_CODE PICTURE '999'

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ R, 23 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 4
      SEEK MDEPT_CODE
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

      @ R, 33 SAY DEPT
      MDEPT= DEPT

IF _DOS
      SET COLOR TO 7/1
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location            ' GET MLOC_CODE PICTURE '9999'
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

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 33 SAY LOC
      MLOC= LOC

IF _DOS
      SET COLOR TO 7/1
ENDIF

      ABC= 1
   ENDDO
   SELECT 1
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 45 SAY 'Qty '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   CLEAR GETS
   SELECT 1
   GOTO TOP
   SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   GOTO TOP
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
   @ 24, 20 CLEAR TO 24, 78
   SELECT 2
   USE st-tran ORDER stidx
   IF ANS='Y'
      SELECT 2
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
   SET FILTER TO DOC_TYPE=5
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   USE
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
******************


**********************
PROCEDURE PROD_PRT
**********************
CLEAR
CLOSE ALL

SELECT 1
USE st-tran EXCLUSIVE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG stidx
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG stidx1
USE

SELECT 1
USE st-key EXCLUSIVE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG st_idx
USE



SELECT 3
USE item ORDER item_idx
SELECT 4
USE dept ORDER dept_idx
SELECT 5
USE loc ORDER loc_idx
SELECT 7
USE item-bal ORDER itm_bal
MUNIT= '    '

IF _DOS
SET COLOR TO 7/1 
ENDIF

ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 3, 33 SAY 'Production Receipt Note '
   @ 4, 33 SAY '----------------------- '
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 5
   MVCH1= '  '
   MVCH2= '     '
   STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO, MDOC_NO1, CNT
   @ 10, 20 SAY 'From P.R. Note : '
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
   USE st-tran ORDER stidx
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This P.R. Note No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   @ 12, 20 SAY 'To   P.R. Note : '
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
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This P.R. Note No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 2
   COPY TO st-rep FOR DOC_NO>=MDOC_NO.and.DOC_NO<=MDOC_NO1.and.DOC_TYPE=5
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
            @ 3, 30 SAY 'Production Receipt '
            L= L+1
            @ L, 30 SAY '------------------ '
            L= L+1
         ELSE
            DO screen IN TBL_PROC.PRG
            @ 3, 30 SAY 'Production Receipt '
            L= L+1
            @ L, 30 SAY '------------------ '
            L= L+1
            CNT= 0
            MREC2= RECNO()
         ENDIF
         MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
         @ L, 1 SAY 'P.R. Note No  : '
         @ L, 18 SAY MVCH_NO
         @ L, 65 SAY 'Date : '+DTOC(DATE)
         L= L+1
         MDEPT_CODE= DEPT_CODE
         MLOC_CODE= LOC_CODE
         @ L, 1 SAY 'Department   : '
         MDEPT_CODE= REPLICATE('0',2-LEN(LTRIM(STR(DEPT_CODE))))+LTRIM(STR(DEPT_CODE))
         @ L, 18 SAY MDEPT_CODE
         SELECT 4
         SEEK MDEPT_CODE
         IF FOUND()
            @ L, 30 SAY DEPT
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
         @ L, 45 SAY 'Unit'
         @ L, 58 SAY 'Qty '
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
      @ L, 45 SAY MUNIT
      @ L, 55 SAY MQTY PICTURE '9999999.9999'
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
******************

***************************
PROCEDURE PRODENT
***************************
CLEAR
CLOSE ALL
SET DELETED ON
SELECT 3
USE item ORDER item_idx
SELECT 4
USE dept ORDER dept_idx
SELECT 5
USE loc ORDER loc_idx
SELECT 7
USE item-bal ORDER itm_bal
HEAD= .T.
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 5

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 1, 28 SAY 'PRODUCTION RECEIPTS ENTRY '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MDEPT_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   STORE 0 TO MFI1, MFI2, MFI3, FMQTY, Q, MDOC_NO1
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Production Note No  ' GET MDOC_NO PICTURE '9999999'
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
      FLUSH
      SELECT 2
      USE
      @ R, 53 SAY 'Date ' GET MDATE
      READ
      DO chk IN TBL_PROC.PRG
      R= R+2
      MT= 0
      DO WHILE MT=0
         @ R, 2 SAY 'Issue To Production ' GET MDOC_NO1 PICTURE '9999999'
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

         @ R, 23 SAY MVCH_NO1

IF _DOS
         SET COLOR TO 7/1
ENDIF

         MDOC_TYPE1= 3
         SELECT 2
         USE st-key ORDER st_idx
         SEEK STR(MDOC_NO1,7)+STR(MDOC_TYPE1,1)
         IF FOUND()
            @ 24, 1 SAY SPACE(78)
            @ 24, 15 SAY 'This I.P. No Already Exists  -- Press any key'
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
         REPLACE DOC_NO WITH MDOC_NO1, DOC_TYPE WITH MDOC_TYPE1
         UNLOCK
         FLUSH
         SELECT 2
         USE
         MT= 1
      ENDDO
      MDOC_TYPE= 5
      R= R+2
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Department Code '
         @ R, 23 GET MDEPT_CODE PICTURE '999'
         READ

IF _DOS
         SET COLOR TO /w
ENDIF

         MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
         @ R, 23 SAY MPC1

IF _DOS
         SET COLOR TO 7/1
ENDIF

         SELECT 4
         SEEK MDEPT_CODE
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

         @ R, 33 SAY DEPT
         MDEPT= DEPT

IF _DOS
         SET COLOR TO 7/1
ENDIF

         ABC= 1
      ENDDO
      R= R+2
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Location            ' GET MLOC_CODE PICTURE '9999'
         READ
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

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 33 SAY LOC
         MLOC= LOC

IF _DOS
         SET COLOR TO 7/1
ENDIF

         ABC= 1
      ENDDO
      R= R+2
      MT= 0
      DO WHILE MT=0
         @ 23, 50 SAY 'Press Ctrl Q To Quit '
         @ R, 2 SAY 'Finished Item No ..'

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 23 SAY '  -  -    '

IF _DOS
         SET COLOR TO 7/1
ENDIF

         @ R, 23 GET MFI1 PICTURE '99'
         READ
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 23, 45 CLEAR TO 23, 78

IF _DOS
         SET COLOR TO /w
ENDIF

         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))
         @ R, 23 SAY MP1
         @ R, 25 SAY '-'
         @ R, 26 GET MFI2 PICTURE '99'
         READ
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))
         @ R, 26 SAY MP2
         @ R, 28 SAY '-'
         @ R, 29 GET MFI3 PICTURE '9999'
         READ
         IF MFI3=0

IF _DOS
            SET COLOR TO 7/1
ENDIF

            LOOP
         ENDIF
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
         @ R, 29 SAY MP3

IF _DOS
         SET COLOR TO 7/1
ENDIF

         @ 23, 50 CLEAR TO 23, 78
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         IF MFI1=0.and.MFI2=0.and.MFI3=0
            LOOP
         ENDIF
         SELECT 3
         STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
         SEEK MFICODE
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

         @ R, 38 SAY ITEM

IF _DOS
         SET COLOR TO 7/1
ENDIF

         MUNIT= UNIT
         SELECT 8
         USE formula ORDER for_idx1
         STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
         SEEK MFICODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 7 SAY 'This Item Code Does Not Exist In Your Formula File ... Please Re-try'
            WAIT ''
            @ 23, 5 SAY SPACE(70)
            LOOP
         ENDIF
         MT= 1
      ENDDO
      SELECT 8
      USE
      R= R+2
      MT= 0
      DO WHILE MT=0
         @ R, 38 SAY 'Unit '

IF _DOS
         SET COLOR TO /w
ENDIF

         @ R, 44 SAY MUNIT

IF _DOS
         SET COLOR TO 7/1
ENDIF

         @ R, 2 SAY 'Qty '
         @ R, 23 GET FMQTY PICTURE '9999999.9999'
         READ
         IF FMQTY=0
            LOOP
         ENDIF
         MT= 1
      ENDDO
      ANS= ' '
      DO WHILE .not. ANS$'YN'
         @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
         READ
      ENDDO
      @ 24, 20 CLEAR TO 24, 78
      IF ANS='Y'
         LOOP
      ENDIF
      SELECT 6
      USE for-work
      DELETE ALL
      APPEND FROM formula FOR FI1=MFI1.and.FI2=MFI2.and.FI3=MFI3
      GOTO TOP
      STORE 0 TO MSR_NO
      DO WHILE .not. EOF()
         MI1= RI1
         MI2= RI2
         MI3= RI3
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 7
         SEEK XYZ

         IF .not. FOUND()

            IF HEAD
               CLEAR
               R= 4
               @ 2, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'

IF _DOS
               SET COLOR TO +7/1
ENDIF

               @ R, 10 SAY 'Location Code'
               @ R, 30 SAY 'Item Code'

IF _DOS
               SET COLOR TO 7/1
ENDIF

               R= R+2
               HEAD= .F.
            ENDIF
            MLOC= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
            @ R, 10 SAY MLOC PICTURE '9999'
            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 30 SAY MP1 PICTURE '99'
            @ R, 32 SAY '-'
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 33 SAY MP2 PICTURE '99'
            @ R, 35 SAY '-'
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 36 SAY MP3 PICTURE '9999'
            Q= 21
         ENDIF

         IF R>=21
            WAIT
            CLEAR
            R= 2
            HEAD= .T.
         ENDIF
         R= R+1
         SELECT 6
         SKIP
      ENDDO

      IF EOF().and.Q=21
         @ 24, 5 SAY 'Vouchers Not Posted --- Press Any Key To Return to Menu '
         WAIT ''
         CLEAR
         CLEAR ALL
         RETURN
      ENDIF

      SELECT 1
      USE st-work
      MSR_NO= 1

      APPEND BLANK

      IF .not. RLOCK()
         rlock()
      ENDIF

      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 5, DATE WITH MDATE
      REPLACE DEPT_CODE WITH MDEPT_CODE, LOC_CODE WITH MLOC_CODE
      REPLACE I1 WITH MFI1, I2 WITH MFI2, I3 WITH MFI3
      REPLACE T_QTY WITH FMQTY, SR_NO WITH MSR_NO
      UNLOCK
      FLUSH
      USE

      SELECT 6

      GOTO TOP


      DO WHILE .not. EOF()
         MI1= RI1
         MI2= RI2
         MI3= RI3
         MQTY= QTY
         SELECT 1
         USE st-work
         MSR_NO= MSR_NO+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO1, DOC_TYPE WITH 3, DATE WITH MDATE
         REPLACE DEPT_CODE WITH MDEPT_CODE, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
         REPLACE T_QTY WITH MQTY*FMQTY, SR_NO WITH MSR_NO
         UNLOCK
         FLUSH
         USE
         SELECT 6
         SKIP
      ENDDO
      SELECT 2
      USE st-tran ORDER stidx
      APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
      APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE1.and.DOC_NO=MDOC_NO
      USE
      SELECT 2
      USE st-key ORDER st_idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         REPLACE FLAG WITH 'Y'
         FLUSH
      ENDIF
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE1,1)
      IF FOUND()
         REPLACE FLAG WITH 'Y'
         FLUSH
      ENDIF
      USE
      SELECT 1
      USE st-work
      DELETE ALL FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
      DELETE ALL FOR DOC_TYPE=MDOC_TYPE1.and.DOC_NO=MDOC_NO
      USE
      SELECT 6
      DELETE ALL
      USE
      ANS= ' '
      DO WHILE .not. ANS$'YN'
         @ 24, 25 SAY 'More Data Entry - Y/N' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='N'
         CLEAR
         CLOSE ALL
         RUN del for-work.dbf
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
****************


**********************
PROCEDURE PTAG_MEN
**********************
DO WHILE .T.
   L= 8
   M= 25

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 24 SAY ' Production Targets & Budgeting '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' 1 - Formula File            '
   @ L+2, M PROMPT ' 2 - Production Target       '
   @ L+4, M PROMPT ' 9 - Return to Previous Menu '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=3
         CLEAR
         CLOSE DATABASE
         RETURN
      CASE MCHOICE=2
         CLEAR
         DO targ_men
      CASE MCHOICE=1
         CLEAR
         DO formumen
   ENDCASE
ENDDO
RETURN


*********************
PROCEDURE FORMUMEN
*********************
DO WHILE .T.
   L= 8
   M= 23

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY ' Formula File Maintenance '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' 1 -  Formula .... Entry      '
   @ L+1, M PROMPT ' 2 -  Formula .... Correction '
   @ L+2, M PROMPT ' 3 -  Formula .... Deletion   '
   @ L+3, M PROMPT ' 4 -  Formula .... Printing   '
   @ L+4, M PROMPT ' 5 -  Formula Printing (Item Wise)'
   @ L+6, M PROMPT ' 9 -  Return to Previous Menu '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO formuent
      CASE MCHOICE=2
         DO formucor
      CASE MCHOICE=3
         DO formudel
      CASE MCHOICE=4
         DO formuprt
      CASE MCHOICE=5
         DO form-itm
      CASE MCHOICE=6
         CLEAR ALL
         CLEAR
         RETURN
   ENDCASE
ENDDO
RETURN
******************

***************************
PROCEDURE FORMUENT
***************************
CLEAR
CLEAR ALL
SET DELETED ON
SELECT 1
USE formula ORDER for_idx
SELECT 2
USE item ORDER item_idx
SELECT 3
USE item-bal ORDER itm_bal
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   C= 5
   C2= 35
   R= 7

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 3, 25 SAY ' Item Formula Entry '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 50 SAY ' Press Esc To Return '
   STORE SPACE(25) TO MNAME
   STORE 0 TO MFI1, MFI2, MFI3, MRI1, MRI2, MRI3, MQTY, MRATE
   @ R, C SAY 'Finished Item No ..'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 27 SAY '  -  -    '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ R, 27 GET MFI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

IF _DOS
   SET COLOR TO n/g
ENDIF

   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))
   @ R, 27 SAY MP1
   @ R, 29 SAY '-'
   @ R, 30 GET MFI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))
   @ R, 30 SAY MP2
   @ R, 32 SAY '-'
   @ R, 33 GET MFI3 PICTURE '9999'
   READ
   IF MFI3=0

IF _DOS
      SET COLOR TO 7/1
ENDIF

      LOOP
   ENDIF
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
   @ R, 33 SAY MP3

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MFI1=0.and.MFI2=0.and.MFI3=0
      LOOP
   ENDIF
   SELECT 2
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
   SEEK MFICODE
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

   @ R, 38 SAY ITEM

IF _DOS
   SET COLOR TO 7/1
ENDIF

   R= R+2
   @ R, 5 SAY REPLICATE('-',64)
   R= R+1
   M= R
   DO WHILE .T.
      @ R, C SAY 'Raw Item No .......'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 27 SAY '  -  -    '

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ R, 27 GET MRI1 PICTURE '99'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF

IF _DOS
      SET COLOR TO n/g
ENDIF

      MPR1= REPLICATE('0',2-LEN(LTRIM(STR(MRI1))))+LTRIM(STR(MRI1))
      @ R, 27 SAY MPR1
      @ R, 29 SAY '-'
      @ R, 30 GET MRI2 PICTURE '99'
      READ
      MPR2= REPLICATE('0',2-LEN(LTRIM(STR(MRI2))))+LTRIM(STR(MRI2))
      @ R, 30 SAY MPR2
      @ R, 32 SAY '-'
      @ R, 33 GET MRI3 PICTURE '9999'
      READ
      IF MRI3=0

IF _DOS
         SET COLOR TO 7/1
ENDIF

         LOOP
      ENDIF
      MPR3= REPLICATE('0',4-LEN(LTRIM(STR(MRI3))))+LTRIM(STR(MRI3))
      @ R, 33 SAY MPR3

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 23, 50 CLEAR TO 23, 78
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MRI1=0.and.MRI2=0.and.MRI3=0
         LOOP
      ENDIF
      SELECT 1
      STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4)+STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MR
      SEEK MR
      IF FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'The Formula For This Item Already Exsist - Retry '
         WAIT ''
         @ 23, 10 SAY SPACE(60)
         LOOP
      ENDIF
      SELECT 2
      STORE STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MRICODE
      SEEK MRICODE
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

      @ R, 38 SAY ITEM

IF _DOS
      SET COLOR TO 7/1
ENDIF

      SELECT 3
      STORE STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MRICODE
      SEEK MRICODE
      IF FOUND()
         MRATE= AVG_PRICE
      ENDIF
      SELECT 1
      R= R+2
      MT= 0
      DO WHILE MT=0
         @ R, C SAY 'Units Required ...... ' GET MQTY PICTURE '9999.9999'
         R= R+2
         @ R, C SAY 'Unit Rate ........... ' GET MRATE PICTURE '9999.9999'
         READ
         IF MQTY=0.or.MRATE=0
            R= R-2
            LOOP
         ENDIF
         MT= 1
      ENDDO
      COR= ' '
      DO WHILE .not. COR$'NY'
         @ 23, 25 SAY ' Any Correction - Y/N ' GET COR PICTURE '!'
         READ
      ENDDO
      @ 23, 19 CLEAR TO 23, 77
      IF COR='Y'
         R= R-4
         LOOP
      ELSE
         SELECT 1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE FI1 WITH MFI1, FI2 WITH MFI2, FI3 WITH MFI3
         REPLACE RI1 WITH MRI1, RI2 WITH MRI2, RI3 WITH MRI3
         REPLACE QTY WITH MQTY
         REPLACE RATE WITH MRATE
         UNLOCK
         FLUSH
      ENDIF
      ANS= ' '
      DO WHILE .not. ANS$'NY'
         @ 23, 20 SAY 'More Data For This Finished Item - Y/N ' GET ANS PICTURE '!'
         READ
      ENDDO
      @ 23, 10 CLEAR TO 23, 77
      IF ANS='Y'
         R= R-4
         STORE 0 TO MRI1, MRI2, MRI3, MQTY, MRATE
         @ M, C CLEAR TO 17, 71
         LOOP
      ELSE
         EXIT
      ENDIF
   ENDDO
   ANS= ' '
   DO WHILE .not. ANS$'NY'
      @ 23, 20 SAY ' More Data Entry - Y/N ' GET ANS PICTURE '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR ALL
      EXIT
   ENDIF
ENDDO
CLOSE ALL
CLEAR
RETURN
******************

**************************
PROCEDURE FORMUCOR
**************************
CLEAR
CLEAR ALL
SELECT 2
USE item ORDER item_idx
SELECT 1
USE formula ORDER for_idx
DO WHILE .T.
   CLEAR
   C= 5
   C2= 35
   R= 7

IF _DOS
   SET COLOR TO u
ENDIF

   @ 3, 25 SAY ' Item Formula Correction '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 23, 50 SAY ' Press Esc To Return '
   STORE SPACE(25) TO MNAME
   STORE 0 TO MFI1, MFI2, MFI3, MRI1, MRI2, MRI3, MQTY, MRATE
   @ R, C SAY 'Finished Item No ..'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 27 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ R, 27 GET MFI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

IF _DOS
   SET COLOR TO n/g
ENDIF

   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))
   @ R, 27 SAY MP1
   @ R, 29 SAY '-'
   @ R, 30 GET MFI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))
   @ R, 30 SAY MP2
   @ R, 32 SAY '-'
   @ R, 33 GET MFI3 PICTURE '9999'
   READ
   IF MFI3=0

IF _DOS
      SET COLOR TO 
ENDIF

      LOOP
   ENDIF
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
   @ R, 33 SAY MP3

IF _DOS
   SET COLOR TO 
ENDIF

   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MFI1=0.and.MFI2=0.and.MFI3=0
      LOOP
   ENDIF
   SELECT 2
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
   SEEK MFICODE
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

   @ R, 38 SAY ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   R= R+2
   @ R, C SAY 'Raw Item No .......'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 27 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ R, 27 GET MRI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

IF _DOS
   SET COLOR TO n/g
ENDIF

   MPR1= REPLICATE('0',2-LEN(LTRIM(STR(MRI1))))+LTRIM(STR(MRI1))
   @ R, 27 SAY MPR1
   @ R, 29 SAY '-'
   @ R, 30 GET MRI2 PICTURE '99'
   READ
   MPR2= REPLICATE('0',2-LEN(LTRIM(STR(MRI2))))+LTRIM(STR(MRI2))
   @ R, 30 SAY MPR2
   @ R, 32 SAY '-'
   @ R, 33 GET MRI3 PICTURE '9999'
   READ
   IF MRI3=0

IF _DOS
      SET COLOR TO 
ENDIF

      LOOP
   ENDIF
   MPR3= REPLICATE('0',4-LEN(LTRIM(STR(MRI3))))+LTRIM(STR(MRI3))
   @ R, 33 SAY MPR3

IF _DOS
   SET COLOR TO 
ENDIF

   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MRI1=0.and.MRI2=0.and.MRI3=0
      LOOP
   ENDIF
   SELECT 1
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4)+STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MR
   SEEK MR
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'The Formula For This Item Does Not Exsist - Retry '
      WAIT ''
      @ 23, 10 SAY SPACE(60)
      LOOP
   ENDIF
   SELECT 2
   STORE STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MRICODE
   SEEK MRICODE
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

   @ R, 38 SAY ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   R= R+2
   SELECT 1
   MQTY= QTY
   MRATE= RATE
   MT= 0
   DO WHILE MT=0
      @ R, C SAY 'Units Required ...... ' GET MQTY PICTURE '9999.9999'
      R= R+2
      @ R, C SAY 'Unit Rate ........... ' GET MRATE PICTURE '9999.9999'
      READ
      IF MQTY=0.or.MRATE=0
         R= R-2
         LOOP
      ENDIF
      MT= 1
   ENDDO
   SELECT 1
   REPLACE QTY WITH MQTY
   REPLACE RATE WITH MRATE
   FLUSH
   ANS= ' '
   DO WHILE .not. ANS$'NY'
      @ 23, 20 SAY ' More Data Correction - Y/N ' GET ANS PICTURE '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR ALL
      EXIT
   ENDIF
ENDDO
CLOSE ALL
CLEAR
RETURN
*****************


***********************
PROCEDURE FORMUDEL
***********************
CLEAR
CLEAR ALL
SET DELETED ON
SELECT 2
USE item ORDER item_idx
SELECT 1
USE formula ORDER for_idx
DO WHILE .T.
   CLEAR
   C= 5
   C2= 35
   R= 7

IF _DOS
   SET COLOR TO u
ENDIF

   @ 3, 25 SAY ' Item Formula Deletion '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 23, 50 SAY ' Press Esc To Return '
   STORE SPACE(25) TO MNAME
   STORE 0 TO MFI1, MFI2, MFI3, MRI1, MRI2, MRI3, MQTY, MRATE
   @ R, C SAY 'Finished Item No ..'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 27 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ R, 27 GET MFI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

IF _DOS
   SET COLOR TO n/g
ENDIF

   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))
   @ R, 27 SAY MP1
   @ R, 29 SAY '-'
   @ R, 30 GET MFI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))
   @ R, 30 SAY MP2
   @ R, 32 SAY '-'
   @ R, 33 GET MFI3 PICTURE '9999'
   READ
   IF MFI3=0

IF _DOS
      SET COLOR TO 
ENDIF

      LOOP
   ENDIF
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
   @ R, 33 SAY MP3

IF _DOS
   SET COLOR TO 
ENDIF

   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MFI1=0.and.MFI2=0.and.MFI3=0
      LOOP
   ENDIF
   SELECT 2
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
   SEEK MFICODE
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

   @ R, 38 SAY ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   R= R+2
   @ R, C SAY 'Raw Item No .......'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 27 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ R, 27 GET MRI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

IF _DOS
   SET COLOR TO n/g
ENDIF

   MPR1= REPLICATE('0',2-LEN(LTRIM(STR(MRI1))))+LTRIM(STR(MRI1))
   @ R, 27 SAY MPR1
   @ R, 29 SAY '-'
   @ R, 30 GET MRI2 PICTURE '99'
   READ
   MPR2= REPLICATE('0',2-LEN(LTRIM(STR(MRI2))))+LTRIM(STR(MRI2))
   @ R, 30 SAY MPR2
   @ R, 32 SAY '-'
   @ R, 33 GET MRI3 PICTURE '9999'
   READ
   IF MRI3=0

IF _DOS
      SET COLOR TO 
ENDIF

      LOOP
   ENDIF
   MPR3= REPLICATE('0',4-LEN(LTRIM(STR(MRI3))))+LTRIM(STR(MRI3))
   @ R, 33 SAY MPR3

IF _DOS
   SET COLOR TO 
ENDIF

   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MRI1=0.and.MRI2=0.and.MRI3=0
      LOOP
   ENDIF
   SELECT 1
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4)+STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MR
   SEEK MR
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'The Formula For This Item Does Not Exsist - Retry '
      WAIT ''
      @ 23, 10 SAY SPACE(60)
      LOOP
   ENDIF
   SELECT 2
   STORE STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MRICODE
   SEEK MRICODE
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

   @ R, 38 SAY ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   R= R+2
   SELECT 1
   MQTY= QTY
   MRATE= RATE
   @ R, C SAY 'Units Required ...... ' GET MQTY PICTURE '9999.9999'
   R= R+2
   @ R, C SAY 'Unit Rate ........... ' GET MRATE PICTURE '9999.9999'
   CLEAR GETS
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   IF ANS='Y'
      SELECT 1
      DELETE
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
***************

*********************
PROCEDURE FORMUPRT
*********************
CLEAR
CLOSE ALL
SELECT 1
USE item ORDER item_idx
SELECT 2
USE formula ORDER for_idx
CLEAR
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Code Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
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
HEADING= .T.
STORE 0 TO PAGE, CNT, MTOT, MGT
SELECT 2
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      PAGE= PAGE+1
      IF P='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'FORMULA FILE PRINTING '
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 30 SAY 'FORMULA FILE PRINTING '
      ENDIF
      L= L+1
      @ L, 65 SAY 'Page -'
      @ L, 71 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 2 SAY 'Item Code'
      @ L, 14 SAY 'Item'
      @ L, 43 SAY 'Unit '
      @ L, 50 SAY 'Qty '
      @ L, 60 SAY 'Rate'
      @ L, 72 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MFI1= FI1
   MFI2= FI2
   MFI3= FI3
   MF1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))
   MF2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))
   MF3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFITEM
   STORE QTY TO MQTY
   SELECT 1
   SEEK MFITEM
   IF FOUND()
      MNAME= ITEM
   ELSE
      MNAME= 'Invalid'
   ENDIF
   SELECT 2
   @ L, 1 SAY MF1 PICTURE '99'
   @ L, 3 SAY '-'
   @ L, 4 SAY MF2 PICTURE '99'
   @ L, 6 SAY '-'
   @ L, 7 SAY MF3 PICTURE '9999'
   @ L, 14 SAY MNAME
   L= L+1
   MLEN= LEN(RTRIM(MNAME))
   @ L, 1 SAY REPLICATE('-',17+MLEN)
   L= L+1
   SELECT 2
   MGT= 0
   DO WHILE FI1=MFI1.and.FI2=MFI2.and.FI3=MFI3
      IF FI1<>MFI1.and.FI2<>MFI2.and.FI3<>MFI3
         EXIT
      ENDIF
      IF HEADING
         L= 3
         PAGE= PAGE+1
         IF P='P'
            DO top_prt IN TBL_PROC.PRG
            @ 3, 15 SAY CHR(18)
            @ 3, 33 SAY 'FORMULA FILE PRINTING '
         ELSE
            DO SCREEN IN TBL_PROC.PRG
            @ 3, 30 SAY 'FORMULA FILE PRINTING '
         ENDIF
         L= L+1
         @ L, 65 SAY 'Page -'
         @ L, 71 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 2 SAY 'Item Code'
         @ L, 14 SAY 'Item'
         @ L, 43 SAY 'Unit '
         @ L, 50 SAY 'Qty '
         @ L, 60 SAY 'Rate'
         @ L, 72 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      SELECT 2
      MRI1= RI1
      MRI2= RI2
      MRI3= RI3
      STORE STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MRITEM
      SELECT 1
      SEEK MRITEM
      IF FOUND()
         STORE ITEM TO MRNAME
         MUNIT= UNIT
      ELSE
         MRNAME= 'Invalid'
         MUNIT= '      '
      ENDIF
      SELECT 2
      MR1= REPLICATE('0',2-LEN(LTRIM(STR(MRI1))))+LTRIM(STR(MRI1))
      MR2= REPLICATE('0',2-LEN(LTRIM(STR(MRI2))))+LTRIM(STR(MRI2))
      MR3= REPLICATE('0',4-LEN(LTRIM(STR(MRI3))))+LTRIM(STR(MRI3))
      @ L, 1 SAY MR1 PICTURE '99'
      @ L, 3 SAY '-'
      @ L, 4 SAY MR2 PICTURE '99'
      @ L, 6 SAY '-'
      @ L, 7 SAY MR3 PICTURE '9999'
      @ L, 14 SAY MRNAME
      @ L, 43 SAY MUNIT
      @ L, 47 SAY QTY PICTURE '9999.9999'
      @ L, 60 SAY RATE PICTURE '9999.9999'
      MTOT= 0
      MTOT= QTY*RATE
      @ L, 70 SAY MTOT PICTURE '999999.99'
      MGT= MGT+MTOT
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
      IF L>53
         HEADING= .T.
      ENDIF
      IF .not. EOF()
         SKIP
         L= L+1
      ENDIF
   ENDDO
   @ L, 60 SAY REPLICATE('-',20)
   L= L+1
   @ L, 60 SAY 'Total'
   @ L, 70 SAY MGT PICTURE '999999.99'
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
*****************

***************************
PROCEDURE FORM_ITM
***************************
CLEAR
CLOSE ALL
SELECT 1
USE item ORDER item_idx
SELECT 2
USE formula ORDER for_idx1
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   R= 9
   C= 10

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 25 SAY ' Formula Printing (Item Wise) '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 50 SAY ' Press Esc To Return '
   STORE 0 TO MFI1, MFI2, MFI3
   @ R, C SAY 'Finished Item No ..'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 27 SAY '  -  -    '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ R, 27 GET MFI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MFI1))))+LTRIM(STR(MFI1))
   @ R, 27 SAY MP1
   @ R, 29 SAY '-'
   @ R, 30 GET MFI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MFI2))))+LTRIM(STR(MFI2))
   @ R, 30 SAY MP2
   @ R, 32 SAY '-'
   @ R, 33 GET MFI3 PICTURE '9999'
   READ
   IF MFI3=0

IF _DOS
      SET COLOR TO 7/1
ENDIF

      LOOP
   ENDIF
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MFI3))))+LTRIM(STR(MFI3))
   @ R, 33 SAY MP3

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 50 CLEAR TO 23, 78
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MFI1=0.and.MFI2=0.and.MFI3=0
      LOOP
   ENDIF
   SELECT 2
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
   SEEK MFICODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 5 SAY 'This Finished Item Code Does Not Exist in Formula File ... Please Re-try'
      WAIT ''
      @ 23, 1 SAY SPACE(70)
      LOOP
   ENDIF
   SELECT 1
   STORE STR(MFI1,2)+STR(MFI2,2)+STR(MFI3,4) TO MFICODE
   SEEK MFICODE
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

   @ R, 38 SAY ITEM
   MITEM= ITEM

IF _DOS
   SET COLOR TO 7/1
ENDIF

   EXIT
ENDDO
SELECT 2
SET FILTER TO FI1=MFI1.and.FI2=MFI2.and.FI3=MFI3
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
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
HEADING= .T.
STORE 0 TO PAGE, CNT, MTOT, MGT
SELECT 2
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 4
      PAGE= PAGE+1
      IF P='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 15 SAY CHR(18)
         @ 3, 25 SAY 'FORMULA PRINTING (ITEM WISE)'
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 25 SAY 'FORMULA PRINTING (ITEM WISE)'
      ENDIF
      L= L+1
      @ L, 1 SAY 'Finished Item : '
      @ L, 17 SAY MP1 PICTURE '99'
      @ L, 19 SAY '-'
      @ L, 20 SAY MP2 PICTURE '99'
      @ L, 22 SAY '-'
      @ L, 23 SAY MP3 PICTURE '9999'
      @ L, 28 SAY MITEM
      @ L, 70 SAY 'Page -'
      @ L, 78 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 2 SAY 'Item Code'
      @ L, 14 SAY 'Item'
      @ L, 43 SAY 'Unit '
      @ L, 50 SAY 'Qty '
      @ L, 60 SAY 'Rate'
      @ L, 72 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MRI1= RI1
   MRI2= RI2
   MRI3= RI3
   SELECT 1
   STORE STR(MRI1,2)+STR(MRI2,2)+STR(MRI3,4) TO MRCODE
   SEEK MRCODE
   IF .not. FOUND()
      MNAME= 'Invalid'
   ELSE
      MNAME= ITEM
      MUNIT= UNIT
   ENDIF
   SELECT 2
   MR1= REPLICATE('0',2-LEN(LTRIM(STR(MRI1))))+LTRIM(STR(MRI1))
   MR2= REPLICATE('0',2-LEN(LTRIM(STR(MRI2))))+LTRIM(STR(MRI2))
   MR3= REPLICATE('0',4-LEN(LTRIM(STR(MRI3))))+LTRIM(STR(MRI3))
   @ L, 1 SAY MR1 PICTURE '99'
   @ L, 3 SAY '-'
   @ L, 4 SAY MR2 PICTURE '99'
   @ L, 6 SAY '-'
   @ L, 7 SAY MR3 PICTURE '9999'
   @ L, 14 SAY MNAME
   @ L, 43 SAY MUNIT
   @ L, 47 SAY QTY PICTURE '9999.9999'
   @ L, 60 SAY RATE PICTURE '9999.9999'
   MTOT= 0
   MTOT= QTY*RATE
   @ L, 70 SAY MTOT PICTURE '999999.99'
   MGT= MGT+MTOT
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
   IF L>53
      HEADING= .T.
   ENDIF
   IF .not. EOF()
      SKIP
      L= L+1
   ENDIF
ENDDO
@ L, 60 SAY REPLICATE('-',20)
L= L+1
@ L, 60 SAY 'Total'
@ L, 70 SAY MGT PICTURE '999999.99'
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

PROCEDURE TARG_MEN
DO WHILE .T.
   L= 8
   M= 23
   CLEAR

IF _DOS
   SET COLOR TO 7/1
ENDIF

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

IF _DOS
   SET COLOR TO +7/1  
ENDIF

   @ 6, 25 SAY 'Target  File  Maintenance'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' 1 - Target .... Entry       '
   @ L+2, M PROMPT ' 2 - Target .... Correction  '
   @ L+4, M PROMPT ' 3 - Target .... Deletion    '
   @ L+6, M PROMPT ' 4 - Target .... Printing    '
   @ L+8, M PROMPT ' 9 - Return to Previous Menu '
   DO bot
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=1
         DO targ-ent
      CASE MCHOICE=2
         DO targ-cor
      CASE MCHOICE=3
         DO targ-del
      CASE MCHOICE=4
         DO targ-rep
      CASE MCHOICE=5
         CLEAR ALL
         CLEAR
         RETURN
   ENDCASE
ENDDO
RETURN
*************


*********************
PROCEDURE TARG_ENT
**********************
CLEAR
CLEAR ALL
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 1
USE target
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG targ_idx
DO WHILE .T.
   CLEAR
   STORE 0 TO MI1, MI2, MI3, MQTY

IF _DOS
   SET COLOR TO u
ENDIF

   @ 5, 30 SAY ' Item Target Entry '

IF _DOS
   SET COLOR TO 
ENDIF

   DO WHILE .T.
      R= 8

IF _DOS
      SET COLOR TO 
ENDIF

      @ 23, 50 SAY ' Press Esc To Return '
      @ R, 10 SAY 'Item Code '

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 22 SAY '  -  -    '

IF _DOS
      SET COLOR TO 
ENDIF

      @ R, 22 GET MI1 PICTURE '99'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF

IF _DOS
      SET COLOR TO n/g
ENDIF

      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ R, 22 SAY MP1
      @ R, 24 SAY '-'
      @ R, 25 GET MI2 PICTURE '99'
      READ
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ R, 25 SAY MP2
      @ R, 27 SAY '-'
      @ R, 28 GET MI3 PICTURE '9999'
      READ
      IF MI3=0
         LOOP
      ENDIF
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ R, 28 SAY MP3

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
      SELECT 1
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SEEK MI_CODE
      IF FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'The Target Of This Item Already Exsist - Retry '
         WAIT ''
         @ 23, 10 SAY SPACE(55)
         LOOP
      ENDIF
      SELECT 2
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

      @ R, 33 SAY ITEM

IF _DOS
      SET COLOR TO 
ENDIF

      R= R+2
      @ R, 10 SAY 'Target Quantity ..... ' GET MQTY PICTURE '99999999'
      READ
      ANS= ' '
      DO WHILE .not. ANS$'YN'
         @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
         READ
      ENDDO
      @ 24, 20 CLEAR TO 24, 78
      IF ANS='Y'
         LOOP
      ENDIF
      SELECT 1
      APPEND BLANK
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
      REPLACE QUANTITY WITH MQTY
      FLUSH
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
***************


**********************
PROCEDURE TARG_COR
**********************
CLEAR
CLEAR ALL
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 1
USE target
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG targ_idx
DO WHILE .T.
   CLEAR
   R= 8
   STORE 0 TO MI1, MI2, MI3, MQTY

IF _DOS
   SET COLOR TO u
ENDIF

   @ 5, 27 SAY ' Item Target Correction '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 23, 50 SAY ' Press Esc To Return '
   @ R, 10 SAY 'Item Code '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 22 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ R, 22 GET MI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

IF _DOS
   SET COLOR TO n/g
ENDIF

   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ R, 22 SAY MP1
   @ R, 24 SAY '-'
   @ R, 25 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ R, 25 SAY MP2
   @ R, 27 SAY '-'
   @ R, 28 GET MI3 PICTURE '9999'
   READ
   IF MI3=0

IF _DOS
      SET COLOR TO 
ENDIF

      LOOP
   ENDIF
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ R, 28 SAY MP3

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
   SELECT 1
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SEEK MI_CODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'The Target Of This Item Does Not Exsist - Retry '
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   SELECT 2
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

   @ R, 33 SAY ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   R= R+2
   @ R, 10 SAY 'Target Quantity ..... ' GET QUANTITY PICTURE '99999999'
   READ
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
*****************

************************
PROCEDURE TARG_DEL
************************
CLEAR
CLEAR ALL
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 1
USE target
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG targ_idx
DO WHILE .T.
   CLEAR
   R= 8
   STORE 0 TO MI1, MI2, MI3, MQTY

IF _DOS
   SET COLOR TO u
ENDIF

   @ 5, 27 SAY ' Item Target Deletion '

IF _DOS
   SET COLOR TO 
ENDIF

   @ 23, 50 SAY ' Press Esc To Return '
   @ R, 10 SAY 'Item Code '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 22 SAY '  -  -    '

IF _DOS
   SET COLOR TO 
ENDIF

   @ R, 22 GET MI1 PICTURE '99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

IF _DOS
   SET COLOR TO n/g
ENDIF

   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ R, 22 SAY MP1
   @ R, 24 SAY '-'
   @ R, 25 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ R, 25 SAY MP2
   @ R, 27 SAY '-'
   @ R, 28 GET MI3 PICTURE '9999'
   READ
   IF MI3=0

IF _DOS
      SET COLOR TO 
ENDIF

      LOOP
   ENDIF
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ R, 28 SAY MP3

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
   SELECT 1
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SEEK MI_CODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'The Target Of This Item Does Not Exsist - Retry '
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   SELECT 2
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

   @ R, 33 SAY ITEM

IF _DOS
   SET COLOR TO 
ENDIF

   SELECT 1
   R= R+2
   @ R, 10 SAY 'Target Quantity ..... ' GET QUANTITY PICTURE '99999999'
   CLEAR GETS
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   IF ANS='Y'
      SELECT 1
      DELETE
      PACK
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
**********************

***********************
PROCEDURE TARG_REP
***********************
CLEAR
CLEAR ALL
SELECT 2
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
SELECT 1
USE target
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG targ_idx
SELECT 1
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Code Exist in Data File  -  Press any key to retu to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
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
PAGE= 0
DO WHILE .not. EOF()
   IF HEADING
      PAGE= PAGE+1
      IF P='P'
         DO top_prt1 IN TBL_PROC.PRG
         @ 3, 15 SAY CHR(18)
         @ 3, 30 SAY ' Bughet Target Report '
         @ 4, 30 SAY ' -------------------- '
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 30 SAY ' Bughet Target Report '
         L= L+1
         @ L, 30 SAY ' -------------------- '
         @ L, 62 SAY 'Page - '
         @ L, 70 SAY PAGE PICTURE '999'
      ENDIF
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 5 SAY 'Item '
      @ L, 19 SAY 'Description'
      @ L, 65 SAY 'Target'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      HEADING= .F.
      L= L+1
   ENDIF
   MI1= I1
   MI2= I2
   MI3= I3
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 5 SAY MP1 PICTURE '99'
   @ L, 7 SAY '-'
   @ L, 8 SAY MP2 PICTURE '99'
   @ L, 10 SAY '-'
   @ L, 11 SAY MP3 PICTURE '9999'
   SELECT 2
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SEEK MI_CODE
   IF .not. FOUND()
      STORE 'Invalid Name ' TO MNAME
   ELSE
      STORE ITEM TO MNAME
   ENDIF
   SELECT 1
   @ L, 19 SAY MNAME
   @ L, 60 SAY QUANTITY
   SKIP
   L= L+1
   IF P='S'
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
         L= 3
         HEADING= .T.
      ENDIF
   ENDIF
   IF P='P'
      IF L>56
         L= 3
         HEADING= .T.
      ENDIF
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to retu to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
SET CONFIRM ON
CLOSE ALL
RETURN
***************

***********************
PROCEDURE MAINTMEN
************************
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

   @ 6, 27 SAY 'System Maintenance Menu'

IF _DOS
   SET COLOR TO 7/1  
ENDIF

   @ 9, 21 PROMPT ' 1 - Backup Data Files To Diskette (Copy)  '
   @ 11, 21 PROMPT ' 2 - Backup Data Files To Diskette (Backup)'
   @ 13, 21 PROMPT ' 3 - Re-Index Data Files     '
   @ 15, 21 PROMPT ' 4 - Sort Vouchers Date Wise '
   @ 17, 21 PROMPT ' 9 - Return to Previous Menu '
   CANS= 0
   MENU TO CANS
   DO CASE
      CASE CANS=1
         DO back-up1
      CASE CANS=2
         DO back-up2
      CASE CANS=3
         DO reindex
      CASE CANS=4
         DO dsort
      CASE CANS=5
         CLEAR
         EXIT
   ENDCASE
ENDDO
CLEAR
RETURN
***************

**************************
PROCEDURE BACK_UP1
***************************
CLEAR
CLEAR ALL
ANS= ' '
DO WHILE .not. ANS$'YN'
   @ 8, 15 SAY 'Please Insert Diskette in Drive A: To Backup Data Files '
   @ 10, 15 SAY 'Do You Want To Make Backup - Y/N ' GET ANS PICTURE '!'
   READ
   IF ANS='Y'
      CLEAR
      RUN copy *.dbf a:
   ELSE
      EXIT
   ENDIF
ENDDO
CLEAR
RETURN
**************

**********************
PROCEDURE BACK_UP2
**********************
CLEAR
CLEAR ALL
ANS= ' '
DO WHILE .not. ANS$'YN'
   @ 8, 15 SAY 'Please Insert Diskette in Drive A: To Backup Data Files '
   @ 10, 15 SAY 'Do You Want To Make Backup - Y/N ' GET ANS PICTURE '!'
   READ
   IF ANS='Y'
      CLEAR
      RUN backup  *.dbf a:
   ELSE
      EXIT
   ENDIF
ENDDO
CLEAR
RETURN
***************

**************************
PROCEDURE SECURMEN
************************
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

   @ 6, 27 SAY ' System Security Menu'

IF _DOS
   SET COLOR TO 7/1  
ENDIF

   @ 8, 25 PROMPT ' 1 - Change Passwords        '
   @ 10, 25 PROMPT ' 2 - Initialize Passwords    '
   @ 12, 25 PROMPT ' 3 - Unpost Vouchers From Ledger'
   @ 14, 25 PROMPT ' 9 - Return to Previous Menu '
   CANS= 0
   MENU TO CANS
   DO CASE
      CASE CANS=1
         DO password
      CASE CANS=2
         DO init-pw
      CASE CANS=3
         DO un-post
      CASE CANS=4
         CLEAR
         EXIT
   ENDCASE
ENDDO
CLEAR
RETURN
****************

***********************
PROCEDURE PASSWORD
************************
CLEAR
CLOSE ALL
USE invt-pw
GOTO TOP
STORE SPACE(6) TO MPW1, MPW2, MPW3, MPW4, MPW5, MPW6, MPW7, MPW8
R= 5
DO WHILE .T.
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 1'
   @ R, 46 GET MM FUNCTION '!'
   READ
   MCODE= MM
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   R= R+2
   SKIP
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm1     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 2'
   @ R, 46 GET MM1 FUNCTION '!'
   READ
   MCODE= MM1
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   R= R+2
   SKIP
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm2     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 3'
   @ R, 46 GET MM2 FUNCTION '!'
   READ
   MCODE= MM2
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   R= R+2
   SKIP
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm3     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 4'
   @ R, 46 GET MM3 FUNCTION '!'
   READ
   MCODE= MM3
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   R= R+2
   SKIP
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm4     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 5'
   @ R, 46 GET MM4 FUNCTION '!'
   READ
   MCODE= MM4
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   R= R+2
   SKIP
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm5     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 6'
   @ R, 46 GET MM5 FUNCTION '!'
   READ
   MCODE= MM5
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   R= R+2
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm6     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 7'
   @ R, 46 GET MM6 FUNCTION '!'
   READ
   MCODE= MM6
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   R= R+2
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm7     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 8'
   @ R, 46 GET MM7 FUNCTION '!'
   READ
   MCODE= MM7
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 23, 20 SAY 'Any Change In These Passwords - Y/N ' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 23, 19 CLEAR TO 23, 66
   IF ANS='Y'
      R= 5
      GOTO TOP
      LOOP
   ENDIF
   EXIT
ENDDO
CLEAR
CLOSE ALL
RETURN
****************

**********************
PROCEDURE INIT_PW
***************************
CLEAR
CLOSE ALL
@ 10, 15 SAY 'Running This Program Will Initialize All Passwords '
P= ' '
DO WHILE .not. P$'YN'
   @ 12, 15 SAY 'Want to proceed ......Y/N' GET P FUNCTION '!'
   READ
ENDDO
IF P='N'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
USE invt-pw
GOTO TOP
STORE SPACE(6) TO MPW1, MPW2, MPW3, MPW4, MPW5, MPW6, MPW7, MPW8
R= 5
DO WHILE .T.
   MCODE= MPW1
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW2
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW3
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW4
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW5
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW6
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW7
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   SKIP
   MCODE= MPW8
   MCODE1= SUBSTR(MCODE,1,1)
   MCODE2= SUBSTR(MCODE,2,1)
   MCODE3= SUBSTR(MCODE,3,1)
   MCODE4= SUBSTR(MCODE,4,1)
   MCODE5= SUBSTR(MCODE,5,1)
   MCODE6= SUBSTR(MCODE,6,1)
   FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
   IF RLOCK()
      REPLACE PW WITH FMCODE
      FLUSH
   ENDIF
   EXIT
ENDDO
CLEAR
CLOSE ALL
RETURN
***************

*********************
PROCEDURE UN_POST
*********************
CLEAR
CLEAR ALL
SET EXCLUSIVE ON
CLOSE ALL
ANS= ' '
DO WHILE .not. ANS$'YN'
   @ 10, 15 SAY ' This Option will UN-POST all Posted Vouchers'
   @ 11, 15 SAY ' from the Ledger.'
   @ 13, 15 SAY ' Do you want to continue - Y/N ' GET ANS PICTURE '!'
   READ
ENDDO
IF ANS='N'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
USE item-bal
DO WHILE .not. EOF()
   IF I3=0
      SKIP
      LOOP
   ENDIF
   REPLACE QTY WITH OPEN_BAL, AVG_PRICE WITH OPEN_AVG, SR_NO WITH 1
   FLUSH
   SKIP
   IF EOF()
      EXIT
   ENDIF
ENDDO
USE stock
DELETE ALL FOR SR_NO<>1
PACK
USE st-tran
REPLACE FOR FLAG='Y' FLAG WITH ' '
FLUSH
USE p-order
REPLACE ALL QTY_DEL WITH 0, QTY_BAL WITH 0
FLUSH
USE s-order
REPLACE ALL QTY_DEL WITH 0, QTY_BAL WITH 0
FLUSH
CLOSE ALL
SET EXCLUSIVE OFF
CLEAR
CLOSE ALL
RETURN
*****************

**********************
PROCEDURE REINDEX
********************
CLEAR
CLOSE ALL
SET SAFETY OFF
SET DELETED OFF
SET EXCLUSIVE OFF

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
@ 2, 2 SAY 'Reindex/Re-organize Data Files'
P= ' '
DO WHILE .not. P$'CQ'
   @ 3, 2 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
@ 2, 20 SAY 'Re-indexing Data Files  --- Please wait'
CLOSE ALL
SET EXCLUSIVE ON
CLOSE ALL
USE item
PACK
USE ldgrcode
PACK
USE item-bal
PACK
USE stock
PACK
USE loc
PACK
USE dept
PACK
USE mach
PACK
USE zone
PACK
USE city
PACK
USE st-tran
PACK
USE st-work
ZAP
USE st-cor
ZAP
USE st-key
PACK
USE p-order
PACK
USE s-order
PACK
USE formula
PACK
COPY TO for-work STRUCTURE
USE for-work
ZAP
CLOSE ALL
SET EXCLUSIVE OFF
CLOSE ALL
USE item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_idx
INDEX ON ITEM TAG itm1_idx
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG ac1
USE item-bal
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_bal
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itmd_idx
USE stock
INDEX ON STR(D_D1,2)+STR(D_D2,2)+STR(D_D3,3) TAG std_idx
INDEX ON STR(CITY_CODE,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG cstck
INDEX ON STR(ZONE_CODE,2)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG zstck
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG stck_idx
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itemidx
INDEX ON DTOC(DATE)+STR(DOC_NO,7) TAG itemidx1
INDEX ON DOC_NO TAG itemidx2
INDEX ON DATE TAG stk_idx
USE loc
INDEX ON LOC_CODE TAG loc_idx
USE dept
INDEX ON DEPT_CODE TAG dept_idx
USE mach
INDEX ON STR(DEPT_CODE,3)+STR(MACH_CODE,4) TAG mach_idx
USE zone
INDEX ON ZONE_CODE TAG zone_idx
USE city
INDEX ON CITY_CODE TAG city_idx
USE st-tran
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG stidx
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG stidx1
USE st-key
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG st_idx
USE p-order
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG pord_idx
USE s-order
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG sord_idx
USE formula
INDEX ON STR(FI1,2)+STR(FI2,2)+STR(FI3,4)+STR(RI1,2)+STR(RI2,2)+STR(RI3,4) TAG for_idx
INDEX ON STR(FI1,2)+STR(FI2,2)+STR(FI3,4) TAG for_idx1
CLEAR
CLOSE ALL
SET EXCLUSIVE OFF
SET DELETED ON
CLOSE ALL
RETURN
*****************

*********************
PROCEDURE DSORT
*********************
CLEAR
CLOSE ALL
SET EXCLUSIVE ON
CLOSE ALL
SELECT 1
USE st-tran
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG stidx
SELECT 2
USE st-key
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG st_idx
ZAP
SELECT 1
DO WHILE .not. EOF()
   MDOC_TYPE= DOC_TYPE
   MDOC_NO= DOC_NO
   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      APPEND BLANK
      REPLACE DOC_TYPE WITH MDOC_TYPE
      REPLACE DOC_NO WITH MDOC_NO
      FLUSH
   ENDIF
   SELECT 1
   SKIP
ENDDO
SELECT 2
GOTO TOP
REPLACE ALL FLAG WITH 'Y'
FLUSH
CLOSE ALL
CLEAR
USE st-key
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG st_idx
SET EXCLUSIVE OFF
SET DELETED ON
CLOSE ALL
RETURN
*************

***********************
PROCEDURE DENIED
***************************
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


********************
PROCEDURE P_COR1
********************

SELECT 1
GOTO TOP
MREC2= RECNO()
DO pur_dsp1 IN PURRPROC.PRG
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
         DO pur_dsp1 IN PURRPROC.PRG
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
               *LOOP
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
         FLUSH
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
            *LOOP
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
      FLUSH
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
         FLUSH
         SKIP
      ENDDO
      RETURN
   ENDIF
ENDDO
RETURN
