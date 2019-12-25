* inventory
* PROGRAM: OPENPROC.PRG
* WRITTEN BY  : AMJAD iZHAR
* THIS REVISION: 19-12-2019
*------------------------------------------------

**********************
PROCEDURE OPEN_ENT
**********************

SET DATE TO BRIT
SET CENT ON

CLEAR
CLOSE ALL
SELECT 1
USE item ORDER item_idx
SELECT 2
USE loc ORDER loc_idx
SELECT 3
USE item-bal ORDER itm_bal
SELECT 4
USE stock
STORE 0 TO MI1, MI2, MI3


***************************************
* Main Loop
***************************************

DO WHILE MI3 <= 9999

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   CLEAR

IF _DOS
   SET COLOR TO +7/1 
ELSE
	SET COLOR TO
ENDIF


   @ 4, 27 SAY 'Opening Balances Entry '

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   STORE 0 TO MITEM_CODE, MLOC_CODE, MQTY_BAL, MAVG_PRICE, MQTY, CNT
   STORE DATE() TO MDATE

   DO WHILE .T.
      @ 21, 50 SAY 'Press Ctrl Q to Return'

IF _DOS
      SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


      @ 8, 23 SAY '  -  -    '

IF _DOS
      SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


      @ 8, 10 SAY 'Item        '
      @ 8, 23 GET MI1 PICTURE '99'
      READ

      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
      SET COLOR TO /w 
ELSE
	SET COLOR TO
ENDIF


      @ 8, 23 SAY MP1 PICTURE '99'
      @ 8, 25 SAY '-'
      @ 8, 26 GET MI2 PICTURE '99'
      READ

      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
      SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


      @ 8, 26 SAY MP2 PICTURE '99'
      @ 8, 28 SAY '-'
      @ 8, 29 GET MI3 PICTURE '9999'
      READ

      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ 8, 29 SAY MP3 PICTURE '9999'

IF _DOS
      SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF

      @ 21, 49 CLEAR TO 21, 73

      IF MI1=0.and.MI2=0.and.MI3=0
         LOOP
      ENDIF

      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MITEM_CODE

      SELECT 1  && item.dbf
      SEEK MITEM_CODE

      IF .not. FOUND()
         @ 21, 20 SAY 'This Item Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 21, 19 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


      @ 8, 35 SAY ITEM
	  @ 8, 72 GET UNIT
	  READ
	  
IF _DOS
      SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


      IF MI3=0
         @ 12, 8 SAY ' Note:-   Four zeros at the end of the Item code   '
         @ 13, 8 SAY '          will make it a consolidation head i.e you'
         @ 14, 8 SAY '          will  not be  able  to Post  Transactions'
         @ 15, 8 SAY '          directly into this head.'
         @ 16, 8 SAY '          Hit any key to continue ..........'
         WAIT ''
         @ 12, 7 CLEAR TO 16, 63
         LOOP
      ENDIF
      MLOC_CODE = 1
      @ 10, 10 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
      READ
      IF MLOC_CODE=0
         LOOP
      ENDIF
      SELECT 2
      SEEK MLOC_CODE
      IF .not. FOUND()
         @ 21, 20 SAY 'This Location Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 21, 19 SAY SPACE(52)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


      @ 10, 35 SAY LOC

IF _DOS
      SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


      IF CNT=0
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 3
         SEEK XYZ
         IF FOUND()
            @ 21, 2 SAY 'Opening Balance For This Item At This Location Already Exists ...Please Re-try'
            WAIT ''
            @ 21, 1 SAY SPACE(79)
            LOOP
         ENDIF
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, LOC_CODE WITH MLOC_CODE
         UNLOCK
         SELECT 4
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, LOC_CODE WITH MLOC_CODE
         UNLOCK
         CNT= 1
      ENDIF
      @ 12, 10 SAY 'Opening Balance Qty     ' GET MQTY PICTURE '99999999999999'
      READ
      MT= 0
      DO WHILE MT=0
         @ 14, 10 SAY 'Average Price           ' GET MAVG_PRICE PICTURE '99999999999999'
         READ
         IF MAVG_PRICE<0
            LOOP
         ENDIF
         MT= 1
      ENDDO

IF _DOS
      SET COLOR TO *+7/1 
ELSE
	SET COLOR TO
ENDIF


      @ 16, 10 SAY 'Opening Balance Date '

IF _DOS
      SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


      SET DATE TO BRIT
      SET CENT ON
      MDATE = {01/07/2015}
      @ 16, 35 GET MDATE
      READ
      DO CHK IN TBL_PROC.PRG

* Updated on 12-12-2019

      P= "N"

      DO WHILE .not. P$'YN'
         @ 23, 30 SAY ' Any Corrections - Y/N ' GET P FUNCTION '!'
         READ
      ENDDO

      @ 23, 25 CLEAR TO 23, 72
      IF P='Y'
         LOOP
      ENDIF
      IF MQTY=0
         MAVG_PRICE= 0
      ENDIF
      SELECT 3
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE SR_NO WITH 1, QTY WITH MQTY, AVG_PRICE WITH MAVG_PRICE
      REPLACE OPEN_BAL WITH MQTY, OPEN_AVG WITH MAVG_PRICE
      UNLOCK
      SELECT 4
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE SR_NO WITH 1, DATE WITH MDATE
      REPLACE QTY_BAL WITH MQTY, AVG_PRICE WITH MAVG_PRICE
      UNLOCK

* Updated on 12-12-2019

      P= "Y"

      DO WHILE .not. P$'YN'
         @ 23, 30 SAY ' More Data Entry - Y/N ' GET P FUNCTION '!'
         READ
      ENDDO
      IF P='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
   MI3= MI3+1
ENDDO
*************************
* End of Main Loop
*************************

RETURN
*******

***********************
PROCEDURE OPEN_COR
***********************
CLEAR
CLOSE DATA
SELECT 1
USE item ORDER item_idx
SELECT 2
USE loc ORDER loc_idx
SELECT 3
USE item-bal ORDER itm_bal
SELECT 4
USE stock ORDER stck_idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   CLEAR

IF _DOS
   SET COLOR TO +7/1 
ELSE
	SET COLOR TO
ENDIF


   @ 4, 27 SAY 'Opening Balances Correction'

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   STORE 0 TO MITEM_CODE, MLOC_CODE, MQTY_BAL, MAVG_PRICE, MI1, MI2, MI3, MQTY
   STORE DATE() TO MDATE
   @ 21, 50 SAY 'Press Ctrl Q to Return'

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 8, 23 SAY '  -  -    '

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   @ 8, 10 SAY 'Item        '
   @ 8, 23 GET MI1 PICTURE '99'
   READ
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w 
ELSE
	SET COLOR TO
ENDIF


   @ 8, 23 SAY MP1 PICTURE '99'
   @ 8, 25 SAY '-'
   @ 8, 26 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 8, 26 SAY MP2 PICTURE '99'
   @ 8, 28 SAY '-'
   @ 8, 29 GET MI3 PICTURE '9999'
   READ
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 8, 29 SAY MP3 PICTURE '9999'

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 21, 49 CLEAR TO 21, 73
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MITEM_CODE
   SELECT 1
   SEEK MITEM_CODE
   IF .not. FOUND()
      @ 21, 20 SAY 'This Item Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 21, 19 SAY SPACE(50)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 8, 35 SAY ITEM SIZE 1,30
   @ 8, 72 GET UNIT
   READ

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   IF MI3=0
      @ 12, 8 SAY ' Note:-   Four zeros at the end of the Item code   '
      @ 13, 8 SAY '          will make it a consolidation head i.e you'
      @ 14, 8 SAY '          will  not be  able  to Post  Transactions'
      @ 15, 8 SAY '          directly into this head.'
      @ 16, 8 SAY '          Hit any key to continue ..........'
      WAIT ''
      @ 12, 7 CLEAR TO 16, 63
      LOOP
   ENDIF
   @ 10, 10 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
   READ
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SELECT 2
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 21, 20 SAY 'This Location Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 21, 19 SAY SPACE(52)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 10, 35 SAY LOC

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
   SELECT 3
   SEEK XYZ
   IF FOUND()
      MSR_NO= SR_NO
      IF MSR_NO<>1
         @ 14, 20 SAY 'Transactions  already  exist  for  this Item  at  '
         @ 15, 20 SAY 'this location.  Therefore  you  cannot  make any  '
         @ 16, 20 SAY 'change in its " Opening Balance ".  If you still  '
         @ 17, 20 SAY 'want to change the  Opening Balance of this item  '
         @ 18, 20 SAY 'please use UN-POST Option first.  '
         @ 20, 20 SAY 'Press any key to return ..... '
         WAIT ''
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ELSE
      @ 21, 9 SAY 'Opening Balance For This Item At This Location Does Not Exist.'
      @ 22, 9 SAY 'Please use " Opening Balance - Entry "  Option .......'
      WAIT ''
      @ 14, 20 CLEAR TO 18, 63
      @ 21, 1 CLEAR TO 22, 79
      LOOP
   ENDIF
   MQTY= QTY
   @ 12, 10 SAY 'Opening Balance Qty     ' GET MQTY PICTURE '9999999.99'
   READ
   MAVG_PRICE= AVG_PRICE
   @ 14, 10 SAY 'Average Price           ' GET MAVG_PRICE PICTURE '99999999.99'
   READ
   STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ABC
   SELECT 4
   SEEK ABC
   IF FOUND()

IF _DOS
      SET COLOR TO *+7/1 
ELSE
	SET COLOR TO
ENDIF


      @ 16, 10 SAY 'Opening Balance Date '

IF _DOS
      SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


      SELECT 4
      MDATE= DATE
      @ 16, 35 GET MDATE
      READ
   ENDIF
   DO CHK IN TBL_PROC.PRG
   IF MQTY=0
      MAVG_PRICE= 0
   ENDIF
   SELECT 3
   IF .not. RLOCK()
      rlock()
   ENDIF
   REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, LOC_CODE WITH MLOC_CODE
   REPLACE SR_NO WITH 1, QTY WITH MQTY, AVG_PRICE WITH MAVG_PRICE
   REPLACE OPEN_BAL WITH MQTY, OPEN_AVG WITH MAVG_PRICE
   UNLOCK
   SELECT 4
   IF .not. RLOCK()
      rlock()
   ENDIF
   REPLACE SR_NO WITH 1, DATE WITH MDATE
   REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, LOC_CODE WITH MLOC_CODE
   REPLACE QTY_BAL WITH MQTY, AVG_PRICE WITH MAVG_PRICE
   UNLOCK
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 23 SAY ' More Data Correction - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******

*********************
PROCEDURE OPEN_DEL
*********************
CLEAR
CLOSE ALL
SET DELETED ON
SELECT 1
USE item ORDER item_idx
SELECT 2
USE loc ORDER loc_idx
SELECT 3
USE item-bal ORDER itm_bal
SELECT 4
USE stock ORDER stck_idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   CLEAR

IF _DOS
   SET COLOR TO +7/1 
ELSE
	SET COLOR TO
ENDIF


   @ 4, 27 SAY 'Opening Balances Deletion'

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   STORE 0 TO MITEM_CODE, MLOC_CODE, MQTY_BAL, MAVG_PRICE, MI1, MI2, MI3, MQTY
   STORE DATE() TO MDATE
   @ 21, 50 SAY 'Press Ctrl Q to Return'

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 8, 23 SAY '  -  -    '

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   @ 8, 10 SAY 'Item        '
   @ 8, 23 GET MI1 PICTURE '99'
   READ
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))

IF _DOS
   SET COLOR TO /w 
ELSE
	SET COLOR TO
ENDIF


   @ 8, 23 SAY MP1 PICTURE '99'
   @ 8, 25 SAY '-'
   @ 8, 26 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 8, 26 SAY MP2 PICTURE '99'
   @ 8, 28 SAY '-'
   @ 8, 29 GET MI3 PICTURE '9999'
   READ
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ 8, 29 SAY MP3 PICTURE '9999'

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 21, 49 CLEAR TO 21, 73
   IF MI1=0.and.MI2=0.and.MI3=0
      LOOP
   ENDIF
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MITEM_CODE
   SELECT 1
   SEEK MITEM_CODE
   IF .not. FOUND()
      @ 21, 20 SAY 'This Item Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 21, 19 SAY SPACE(50)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 8, 35 SAY ITEM
   @ 8, 68 SAY UNIT

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   IF MI3=0
      @ 12, 8 SAY ' Note:-   Four zeros at the end of the Item code   '
      @ 13, 8 SAY '          will make it a consolidation head i.e you'
      @ 14, 8 SAY '          will  not be  able  to Post  Transactions'
      @ 15, 8 SAY '          directly into this head.'
      @ 16, 8 SAY '          Hit any key to continue ..........'
      WAIT ''
      @ 12, 7 CLEAR TO 16, 63
      LOOP
   ENDIF
   @ 10, 10 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
   READ
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SELECT 2
   SEEK MLOC_CODE
   IF .not. FOUND()
      @ 21, 20 SAY 'This Location Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 21, 19 SAY SPACE(52)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 10, 35 SAY LOC

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
   SELECT 3
   SEEK XYZ
   IF FOUND()
      MSR_NO= SR_NO
      IF MSR_NO<>1
         @ 14, 20 SAY 'Transactions already exist  for  this Item at  '
         @ 15, 20 SAY 'this  location.  Therefore, you cannot Delete  '
         @ 16, 20 SAY 'its " Opening Balance " for this location. If  '
         @ 17, 20 SAY 'you still want to delete the  Opening Balance  '
         @ 18, 20 SAY 'of this item please use UN-POST Option first.  '
         @ 20, 20 SAY 'Press any key to return ..... '
         WAIT ''
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ELSE
      @ 21, 9 SAY 'Opening Balance For This Item At This Location Does Not Exist.'
      @ 22, 9 SAY 'Please use " Opening Balance - Entry "  Option .......'
      WAIT ''
      @ 14, 20 CLEAR TO 18, 63
      @ 21, 1 CLEAR TO 22, 79
      LOOP
   ENDIF
   @ 12, 10 SAY 'Opening Balance Qty     '
   @ 14, 10 SAY 'Average Price           '

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 12, 35 SAY QTY PICTURE '9999999.9999'
   @ 14, 35 SAY AVG_PRICE PICTURE '9999.9999'

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ABC
   SELECT 4
   SEEK ABC
   IF FOUND()

IF _DOS
      SET COLOR TO *+7/1 
ELSE
	SET COLOR TO
ENDIF


      @ 16, 10 SAY 'Opening Balance Date '

IF _DOS
      SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF



IF _DOS
      SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


      @ 16, 35 SAY DATE

IF _DOS
      SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   ENDIF
   DO CHK IN TBL_PROC.PRG
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 30 SAY ' Are You Sure - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   @ 23, 29 CLEAR TO 23, 66
   IF P='Y'
      SELECT 4
      DELETE
      SELECT 3
      DELETE
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 30 SAY ' More Data Deletion - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
*******


**************************
PROCEDURE OPEN_PRT
**************************

IF _DOS
SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


CLEAR
CLOSE DATA
SELECT 3
USE loc ORDER loc_idx
SELECT 2
USE item ORDER item_idx
SELECT 1
USE item-bal ORDER itm_bal
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Code Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
NO= ' '
DO WHILE .not. NO$'YN'

IF _DOS
   SET COLOR TO +7/1
ELSE
	SET COLOR TO
ENDIF


   @ 2, 2 SAY 'Opening Balance Printing/Display :'
   @ 3, 2 SAY 'Do You Want Printing/Display With Average Price - Y/N ' GET NO FUNCTION '!'
   READ

IF _DOS
   SET COLOR TO 7/1
ELSE
	SET COLOR TO
ENDIF


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
STORE 0 TO PAGE, CNT, MTOT
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      PAGE= PAGE+1
      IF P='P'
         DO top_prt IN TBL_PROC.PRG
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'OPENING BALANCE REPORT'
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 30 SAY 'OPENING BALANCE REPORT'
      ENDIF
      L= L+1
      @ L, 65 SAY 'Page -'
      @ L, 71 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 2 SAY 'Item Code'
      @ L, 14 SAY 'Item'
      IF NO='Y'
         @ L, 45 SAY 'Opening Qty'
         @ L, 60 SAY 'Avg Price'
         @ L, 74 SAY 'Amount'
      ELSE
         @ L, 55 SAY 'Opening Qty'
      ENDIF
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   MLOC_CODE= LOC_CODE
   SELECT 3
   SEEK MLOC_CODE
   IF FOUND()
      MLOC= LOC
   ELSE
      MLOC= 'Invalid'
   ENDIF
   @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
   L= L+1
   MLEN= LEN(RTRIM(MLOC))
   @ L, 2 SAY REPLICATE('-',17+MLEN)
   L= L+1
   SELECT 1
   DO WHILE LOC_CODE=MLOC_CODE
      IF HEADING
         PAGE= PAGE+1
         IF P='P'
            DO top_prt IN TBL_PROC.PRG
            @ 3, 15 SAY CHR(18)
            @ 3, 33 SAY 'OPENING BALANCE REPORT'
         ELSE
            DO SCREEN IN TBL_PROC.PRG
            @ 3, 30 SAY 'OPENING BALANCE REPORT'
         ENDIF
         L= L+1
         @ L, 65 SAY 'Page -'
         @ L, 71 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 2 SAY 'Item Code'
         @ L, 14 SAY 'Item'
         IF NO='Y'
            @ L, 45 SAY 'Opening Qty'
            @ L, 60 SAY 'Avg Price'
            @ L, 74 SAY 'Amount'
         ELSE
            @ L, 55 SAY 'Opening Qty'
         ENDIF
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF LOC_CODE<>MLOC_CODE
         EXIT
      ENDIF
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 2 SAY MPC1 PICTURE '99'
      @ L, 4 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 5 SAY MPC2 PICTURE '99'
      @ L, 7 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 8 SAY MPC3 PICTURE '9999'
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      SELECT 2
      SEEK MI_CODE
      
      IF FOUND()
         MITEM= ITEM
         MUNIT= UNIT
      ENDIF
      
      SELECT 1
      MTOT= OPEN_BAL*OPEN_AVG
      @ L, 14 SAY LEFT(MITEM,20)

* updated 19-12-2019
      
      IF NO='Y'
	     @ L, 36 SAY MUNIT
         @ L, 42 SAY OPEN_BAL PICTURE '9999999'
         @ L, 57 SAY OPEN_AVG PICTURE '9999999'
         @ L, 67 SAY MTOT PICTURE '9999999999'
      ELSE
         @ L, 52 SAY OPEN_BAL PICTURE '9999999'
      ENDIF
      
      L= L+1
      SKIP
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
         L= 3
         HEADING= .T.
      ENDIF
      IF P='P'.and.L>56
         L= 3
         HEADING= .T.
      ENDIF
   ENDDO
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
      L= 2
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      L= 2
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
******

************************
PROCEDURE OPENPRT
************************

IF _DOS
SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


CLEAR
CLOSE ALL
SELECT 3
USE loc ORDER loc_idx
SELECT 2
USE item ORDER item_idx
SELECT 1
USE item-bal ORDER itm_bal
MT= 0
DO WHILE MT=0

IF _DOS
   SET COLOR TO +7/1 
ELSE
	SET COLOR TO
ENDIF


   @ 5, 20 SAY 'Opening Balance Printing '

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   @ 23, 50 SAY 'Press Ctrl Q to Return'
   STORE 0 TO MLOC_CODE, MNET, CNT, PGTOT
   @ 9, 20 SAY 'Location Code  ' GET MLOC_CODE PICTURE '9999'
   READ

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
   @ 9, 36 SAY MPC1

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SELECT 3
   SEEK MLOC_CODE
   @ 23, 40 CLEAR TO 23, 77
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exist ... Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(40)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 11, 20 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   MT= 1
ENDDO
SELECT 1
SET FILTER TO LOC_CODE=MLOC_CODE
COUNT TO RCON
IF RCON=0
   CLEAR
   @ 12, 12 SAY 'No Codes in Code File  -  Press any key to retu to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
GOTO TOP
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
L= 3
HEADING= .T.
STORE 0 TO PAGE, CNT, MTOT, MAMT
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 4
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'OPENING BALANCE REPORT'
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 30 SAY 'OPENING BALANCE REPORT'
      ENDIF
      L= L+1
      @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
      @ L, 65 SAY 'Page -'
      @ L, 71 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 13 SAY 'Item'
      @ L, 45 SAY 'Opening Qty'
      @ L, 58 SAY 'Avg Price'
      @ L, 70 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      IF CNT=1
         L= L+1
         @ L, 35 SAY 'Total Frm Page '
         @ L, 52 SAY PAGE PICTURE '999'
         @ L, 57 SAY 'B/F '
         @ L, 64 SAY MNET PICTURE '999999999999.99'
         L= L+1
      ENDIF
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   STORE 0 TO MTOT
   DO WHILE I1=MI1.and.I2=MI2
      IF HEADING
         L= 4
         PAGE= PAGE+1
         IF P='P'
            DO top-prt
            @ 3, 15 SAY CHR(18)
            @ 3, 33 SAY 'OPENING BALANCE REPORT'
         ELSE
            DO SCREEN
            @ 3, 30 SAY 'OPENING BALANCE REPORT'
         ENDIF
         L= L+1
         @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
         @ L, 65 SAY 'Page -'
         @ L, 71 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 13 SAY 'Item'
         @ L, 45 SAY 'Opening Qty'
         @ L, 58 SAY 'Avg Price'
         @ L, 70 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         IF CNT=1
            L= L+1
            @ L, 45 SAY 'Total Frm Page '
            @ L, 52 SAY PAGE PICTURE '999'
            @ L, 57 SAY 'B/F '
            @ L, 64 SAY MNET PICTURE '999999999999.99'
            L= L+1
         ENDIF
         HEADING= .F.
      ENDIF
      IF I1<>MI1.and.I2<>MI2
         EXIT
      ENDIF
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 1 SAY MPC1 PICTURE '99'
      @ L, 3 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 4 SAY MPC2 PICTURE '99'
      @ L, 6 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 7 SAY MPC3 PICTURE '9999'
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      SELECT 2
      SEEK MI_CODE
      IF FOUND()
         MITEM= ITEM
      ENDIF
      SELECT 1
      MAMT= OPEN_BAL*OPEN_AVG
      MTM= LEFT(MITEM,20)
      @ L, 13 SAY MTM
      @ L, 45 SAY OPEN_BAL PICTURE '9999999.9999'
      @ L, 58 SAY OPEN_AVG PICTURE '9999.99'
      @ L, 67 SAY MAMT PICTURE '999999999.99'
      MTOT= MTOT+MAMT
      MNET= MNET+MAMT
      PGTOT= PGTOT+MAMT
      SKIP
      IF P='S'.and.L>14
         EXIT
      ENDIF
      IF P='P'.and.L>50
         EXIT
      ENDIF
   ENDDO
   L= L+1
   @ L, 45 SAY REPLICATE('-',35)
   L= L+1
   @ L, 45 SAY 'Total Amount '
   @ L, 64 SAY MTOT PICTURE '999999999999.99'
   L= L+1
   @ L, 45 SAY REPLICATE('-',35)
   IF P='S'.and.L>14
      L= L+1
      @ L, 40 SAY REPLICATE('-',40)
      L= L+1
      @ L, 45 SAY 'Total (This Page) '
      @ L, 64 SAY PGTOT PICTURE '999999999999.99'
      L= L+1
      @ L, 35 SAY 'Total (Frm Page 1) B/F'
      @ L, 64 SAY MNET PICTURE '999999999999.99'
      PGTOT= 0
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 1 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      CNT= 1
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>50
      L= L+1
      @ L, 40 SAY REPLICATE('-',40)
      L= L+1
      @ L, 45 SAY 'Total (This Page) '
      @ L, 64 SAY PGTOT PICTURE '999999999999.99'
      L= L+1
      @ L, 45 SAY 'Total (Frm Page 1) C/F'
      @ L, 64 SAY MNET PICTURE '999999999999.99'
      PGTOT= 0
      CNT= 1
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 45 SAY 'Total (This Page) '
@ L, 64 SAY PGTOT PICTURE '999999999999.99'
L= L+1
@ L, 45 SAY 'Net Total '
@ L, 64 SAY MNET PICTURE '999999999999.99'
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
PROCEDURE OPENPRT1
*************************

IF _DOS
SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


CLEAR
CLOSE ALL
SELECT 3
USE loc ORDER loc_idx
SELECT 2
USE item ORDER item_idx
SELECT 1
USE item-bal ORDER itm_bal
MT= 0
DO WHILE MT=0

IF _DOS
   SET COLOR TO +7/1 
ELSE
	SET COLOR TO
ENDIF


   @ 5, 20 SAY 'Opening Balance Printing '

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   @ 23, 50 SAY 'Press Ctrl Q to Return'
   STORE 0 TO MLOC_CODE, MNET, CNT, PGTOT
   @ 9, 20 SAY 'Location Code  ' GET MLOC_CODE PICTURE '9999'
   READ

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
   @ 9, 36 SAY MPC1

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SELECT 3
   SEEK MLOC_CODE
   @ 23, 40 CLEAR TO 23, 77
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exist ... Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(40)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 11, 20 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   MT= 1
ENDDO
SELECT 1
SET FILTER TO LOC_CODE=MLOC_CODE
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
USE
SELECT 1
USE alpha EXCLUSIVE
ZAP
APPEND FROM item-bal FOR LOC_CODE=MLOC_CODE
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   MI1= I1
   MI2= I2
   MI3= I3
   STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
   SELECT 2
   SEEK MI_CODE
   IF FOUND()
      MITEM= ITEM
   ENDIF
   SELECT 1
   REPLACE ITEM WITH MITEM
   SKIP
ENDDO
SELECT 1
INDEX ON ITEM TAG itemidx
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
L= 3
HEADING= .T.
STORE 0 TO PAGE, CNT, MTOT, MAMT
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 4
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'OPENING BALANCE REPORT'
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 30 SAY 'OPENING BALANCE REPORT'
      ENDIF
      L= L+1
      @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
      @ L, 65 SAY 'Page -'
      @ L, 71 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 13 SAY 'Item'
      @ L, 37 SAY 'Opening Qty'
      @ L, 50 SAY 'Avg Price'
      @ L, 62 SAY 'Amount'
      @ L, 72 SAY 'T.Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      IF CNT=1
         L= L+1
         @ L, 45 SAY 'Total Frm Page '
         @ L, 52 SAY PAGE PICTURE '999'
         @ L, 57 SAY 'B/F '
         @ L, 63 SAY MNET PICTURE '9999999999999.99'
         L= L+1
      ENDIF
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   STORE 0 TO MTOT
   DO WHILE I1=MI1.and.I2=MI2
      IF HEADING
         L= 4
         PAGE= PAGE+1
         IF P='P'
            DO top-prt
            @ 3, 15 SAY CHR(18)
            @ 3, 33 SAY 'OPENING BALANCE REPORT'
         ELSE
            DO SCREEN
            @ 3, 30 SAY 'OPENING BALANCE REPORT'
         ENDIF
         L= L+1
         @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
         @ L, 65 SAY 'Page -'
         @ L, 71 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 13 SAY 'Item'
         @ L, 37 SAY 'Opening Qty'
         @ L, 50 SAY 'Avg Price'
         @ L, 62 SAY 'Amount'
         @ L, 72 SAY 'T.Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         IF CNT=1
            L= L+1
            @ L, 45 SAY 'Total Frm Page '
            @ L, 52 SAY PAGE PICTURE '999'
            @ L, 57 SAY 'B/F '
            @ L, 68 SAY MNET PICTURE '99999999.99'
            L= L+1
         ENDIF
         HEADING= .F.
      ENDIF
      IF I1<>MI1.and.I2<>MI2
         EXIT
      ENDIF
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 1 SAY MPC1 PICTURE '99'
      @ L, 3 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 4 SAY MPC2 PICTURE '99'
      @ L, 6 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 7 SAY MPC3 PICTURE '9999'
      SELECT 1
      MAMT= OPEN_BAL*OPEN_AVG
      MTM= LEFT(ITEM,20)
      @ L, 13 SAY MTM
      @ L, 35 SAY OPEN_BAL PICTURE '9999999.9999'
      @ L, 49 SAY OPEN_AVG PICTURE '9999.99'
      @ L, 57 SAY MAMT PICTURE '99999999.99'
      MTOT= MTOT+MAMT
      MNET= MNET+MAMT
      PGTOT= PGTOT+MAMT
      SKIP
      IF P='S'.and.L>19
         EXIT
      ENDIF
      IF P='P'.and.L>56
         EXIT
      ENDIF
   ENDDO
   @ L, 68 SAY MTOT PICTURE '99999999.99'
   IF P='S'.and.L>19
      L= L+1
      @ L, 40 SAY REPLICATE('-',40)
      L= L+1
      @ L, 45 SAY 'Total (This Page) '
      @ L, 70 SAY PGTOT PICTURE '999999.99'
      L= L+1
      @ L, 35 SAY 'Total (Frm Page 1) B/F'
      @ L, 63 SAY MNET PICTURE '9999999999999.99'
      PGTOT= 0
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
      CNT= 1
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>56
      L= L+1
      @ L, 40 SAY REPLICATE('-',40)
      L= L+1
      @ L, 45 SAY 'Total (This Page) '
      @ L, 68 SAY PGTOT PICTURE '99999999.99'
      L= L+1
      @ L, 45 SAY 'Total (Frm Page 1) C/F'
      @ L, 68 SAY MNET PICTURE '99999999.99'
      PGTOT= 0
      CNT= 1
      HEADING= .T.
   ENDIF
ENDDO
IF P='S'.and.L>19
   L= 1
ENDIF
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 45 SAY 'Total (This Page) '
@ L, 68 SAY PGTOT PICTURE '99999999.99'
L= L+1
@ L, 45 SAY 'Net Total '
@ L, 68 SAY MNET PICTURE '99999999.99'
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SELECT 1
ZAP
CLEAR
CLOSE ALL
RETURN
********

**********************
PROCEDURE OPENPRT2
**********************

IF _DOS
SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


CLEAR
CLOSE ALL
HEADING= .T.
P= 1
SELECT 1
USE item-bal ORDER itmd_idx
SELECT 2
USE item ORDER item_idx
SELECT 3
USE loc ORDER loc_idx
ABC= 0
DO WHILE ABC=0
   CLEAR

IF _DOS
   SET COLOR TO +7/1 
ELSE
	SET COLOR TO
ENDIF


   @ 2, 27 SAY ' OPENING BALANCE (ITEM WISE)'

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   @ 23, 50 SAY 'Press Ctrl Q to Quit '
   STORE 0 TO MI1, MI2, MI3, TOT
   @ 12, 15 SAY 'Two Level Item '

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 12, 30 SAY '  -  '

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
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
ELSE
	SET COLOR TO
ENDIF


   @ 12, 30 SAY MP1
   @ 12, 32 SAY '-'
   @ 12, 33 GET MI2 PICTURE '99'
   READ
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 12, 33 SAY MP2

IF _DOS
   SET COLOR TO 7/1
ELSE
	SET COLOR TO
ENDIF


   SELECT 1
   SET FILTER TO I1=MI1.and.I2=MI2
   COUNT TO RCON
   IF RCON=0
      @ 23, 15 SAY 'No Codes exist in this range - Please Re-try '
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
L= 3
HEADING= .T.
STORE 0 TO PAGE
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 4
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'OPENING BALANCE REPORT (ITEM WISE)'
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         @ 3, 25 SAY 'OPENfING BALANCE REPORT (ITEM WISE)'
      ENDIF
      @ L, 69 SAY 'Page -'
      @ L, 77 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Code '
      @ L, 9 SAY 'Location '
      @ L, 38 SAY 'Opening Qty'
      @ L, 53 SAY 'Avg Price'
      @ L, 67 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   SELECT 2
   SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
   IF FOUND()
      MITEM= ITEM
   ELSE
      MITEM= ' '
   ENDIF
   SELECT 1
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ L, 1 SAY MP1
   @ L, 3 SAY '-'
   @ L, 4 SAY MP2
   @ L, 6 SAY '-'
   @ L, 7 SAY MP3
   @ L, 12 SAY MITEM
   MLEN= LEN(RTRIM(MITEM))
   L= L+1
   @ L, 1 SAY REPLICATE('-',20+MLEN)
   L= L+1
   DO WHILE I1=MI1.and.I2=MI2.and.I3=MI3
      IF HEADING
         L= 4
         PAGE= PAGE+1
         IF P='P'
            DO top-prt
            @ 3, 15 SAY CHR(18)
            @ 3, 33 SAY 'OPENING BALANCE REPORT (ITEM WISE)'
         ELSE
            DO SCREEN IN TBL_PROC.PRG
            @ 3, 25 SAY 'OPENfING BALANCE REPORT (ITEM WISE)'
         ENDIF
         @ L, 69 SAY 'Page -'
         @ L, 77 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Code '
         @ L, 9 SAY 'Location '
         @ L, 38 SAY 'Opening Qty'
         @ L, 53 SAY 'Avg Price'
         @ L, 67 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEADING= .F.
      ENDIF
      IF I1<>MI1.and.I2<>MI2.and.I3<>MI3
         EXIT
      ENDIF
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
      @ L, 38 SAY OPEN_BAL PICTURE '9999999.9999'
      @ L, 53 SAY OPEN_AVG PICTURE '9999.99'
      MTOT= 0
      MTOT= OPEN_BAL*OPEN_AVG
      @ L, 67 SAY MTOT PICTURE '99999999.99'
      TOT= TOT+MTOT
      SKIP
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
ENDDO
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 45 SAY 'Net Total '
@ L, 67 SAY TOT PICTURE '99999999.99'
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
*************

*********************
PROCEDURE OPENPRT3
**********************

IF _DOS
SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


CLEAR
CLOSE ALL
SELECT 3
USE loc ORDER loc_idx
SELECT 2
USE item ORDER item_idx
SELECT 1
USE item-bal ORDER itm_bal
MT= 0
DO WHILE MT=0

IF _DOS
   SET COLOR TO +7/1 
ELSE
	SET COLOR TO
ENDIF


   @ 5, 20 SAY 'Opening Balance Printing '

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   @ 23, 50 SAY 'Press Ctrl Q to Return'
   STORE 0 TO MLOC_CODE, MNET, CNT, PGTOT, MI1, MI2
   @ 9, 20 SAY 'Location Code  ' GET MLOC_CODE PICTURE '9999'
   READ

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
   @ 9, 36 SAY MPC1

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SELECT 3
   SEEK MLOC_CODE
   @ 23, 40 CLEAR TO 23, 77
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exist ... Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(40)
      LOOP
   ENDIF

IF _DOS
   SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


   @ 11, 20 SAY LOC
   MLOC= LOC

IF _DOS
   SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


   MT= 1
ENDDO
@ 13, 20 SAY 'Item Code '

IF _DOS
SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


@ 13, 36 SAY '  -  '

IF _DOS
SET COLOR TO 7/1 
ELSE
	SET COLOR TO
ENDIF


@ 13, 36 GET MI1 PICTURE '99'
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
ELSE
	SET COLOR TO
ENDIF


@ 13, 36 SAY MP1
@ 13, 38 SAY '-'
@ 13, 39 GET MI2 PICTURE '99'
READ
MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))

IF _DOS
SET COLOR TO /w
ELSE
	SET COLOR TO
ENDIF


@ 13, 39 SAY MP2

IF _DOS
SET COLOR TO 
ELSE
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
GOTO TOP
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
L= 3
HEADING= .T.
STORE 0 TO PAGE, CNT, MTOT, MAMT
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   IF HEADING
      L= 4
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'OPENING BALANCE REPORT'
      ELSE
         
         DO SCREEN IN TBL_PROC.PRG
         
         @ 3, 30 SAY 'OPENING BALANCE REPORT'
      ENDIF
      L= L+1
      @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
      @ L, 65 SAY 'Page -'
      @ L, 71 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY 'Item Code'
      @ L, 13 SAY 'Item'
      @ L, 45 SAY 'Opening Qty'
      @ L, 58 SAY 'Avg Price'
      @ L, 70 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      IF CNT=1
         L= L+1
         @ L, 35 SAY 'Total Frm Page '
         @ L, 52 SAY PAGE PICTURE '999'
         @ L, 57 SAY 'B/F '
         @ L, 64 SAY MNET PICTURE '999999999999.99'
      ENDIF
      HEADING= .F.
   ENDIF
   SELECT 1
   MI1= I1
   MI2= I2
   STORE 0 TO MTOT
   DO WHILE I1=MI1.and.I2=MI2
      IF HEADING
         L= 4
         PAGE= PAGE+1
         IF P='P'
            DO top-prt
            @ 3, 15 SAY CHR(18)
            @ 3, 33 SAY 'OPENING BALANCE REPORT'
         ELSE
            DO SCREEN IN TBL_PROC.PRG
            @ 3, 30 SAY 'OPENING BALANCE REPORT'
         ENDIF
         L= L+1
         @ L, 2 SAY 'Location : '+STR(MLOC_CODE,4)+'  '+MLOC
         @ L, 65 SAY 'Page -'
         @ L, 71 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 13 SAY 'Item'
         @ L, 45 SAY 'Opening Qty'
         @ L, 58 SAY 'Avg Price'
         @ L, 70 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         IF CNT=1
            L= L+1
            @ L, 45 SAY 'Total Frm Page '
            @ L, 52 SAY PAGE PICTURE '999'
            @ L, 57 SAY 'B/F '
            @ L, 64 SAY MNET PICTURE '999999999999.99'
         ENDIF
         HEADING= .F.
      ENDIF
      IF I1<>MI1.and.I2<>MI2
         EXIT
      ENDIF
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 1 SAY MPC1 PICTURE '99'
      @ L, 3 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 4 SAY MPC2 PICTURE '99'
      @ L, 6 SAY '-'
      MPC3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 7 SAY MPC3 PICTURE '9999'
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      SELECT 2
      SEEK MI_CODE
      IF FOUND()
         MITEM= ITEM
      ENDIF
      SELECT 1
      MAMT= OPEN_BAL*OPEN_AVG
      MTM= LEFT(MITEM,20)
      @ L, 13 SAY MTM
      @ L, 45 SAY OPEN_BAL PICTURE '9999999.9999'
      @ L, 58 SAY OPEN_AVG PICTURE '9999.99'
      @ L, 67 SAY MAMT PICTURE '999999999.99'
      MTOT= MTOT+MAMT
      MNET= MNET+MAMT
      PGTOT= PGTOT+MAMT
      SKIP
      IF P='S'.and.L>14
         EXIT
      ENDIF
      IF P='P'.and.L>50
         EXIT
      ENDIF
   ENDDO
   L= L+1
   @ L, 45 SAY REPLICATE('-',35)
   L= L+1
   @ L, 45 SAY 'Total Amount '
   @ L, 64 SAY MTOT PICTURE '999999999999.99'
   L= L+1
   @ L, 45 SAY REPLICATE('-',35)
   IF P='S'.and.L>14
      L= L+1
      @ L, 40 SAY REPLICATE('-',40)
      L= L+1
      @ L, 45 SAY 'Total (This Page) '
      @ L, 64 SAY PGTOT PICTURE '999999999999.99'
      L= L+1
      @ L, 35 SAY 'Total (Frm Page 1) B/F'
      @ L, 64 SAY MNET PICTURE '999999999999.99'
      PGTOT= 0
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 24, 1 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      CNT= 1
      HEADING= .T.
   ENDIF
   IF P='P'.and.L>50
      L= L+1
      @ L, 40 SAY REPLICATE('-',40)
      L= L+1
      @ L, 45 SAY 'Total (This Page) '
      @ L, 64 SAY PGTOT PICTURE '999999999999.99'
      L= L+1
      @ L, 45 SAY 'Total (Frm Page 1) C/F'
      @ L, 64 SAY MNET PICTURE '999999999999.99'
      PGTOT= 0
      CNT= 1
      HEADING= .T.
   ENDIF
ENDDO
L= L+1
@ L, 40 SAY REPLICATE('-',40)
L= L+1
@ L, 45 SAY 'Total (This Page) '
@ L, 64 SAY PGTOT PICTURE '999999999999.99'
L= L+1
@ L, 45 SAY 'Net Total '
@ L, 64 SAY MNET PICTURE '999999999999.99'
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
****************