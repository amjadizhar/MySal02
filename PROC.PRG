
******************************************
PROCEDURE TOP
*******************************************
SET DATE TO BRIT
SET CENTURY ON
SET STEP OFF
SET ECHO OFF
SET DOHISTORY OFF
@ 1, 1 SAY 'Designed & Developed by Amjad Izhar: '
@ 2, 0 TO 2, 79
@ 4, 0 TO 4, 79
@ 3, 18 SAY '         A R S L A N   A U T O S '

RETURN
**************************************************************

***************************************************
PROCEDURE BOT
***************************************************
@ 22, 3 SAY 'Designed & Developed by:'
@ 23, 0 TO 23, 79 DOUBLE
@ 24, 3 SAY 'Amjad Izhar :  CosmoSoft Systems Lahore. '
RETURN
****************************************************

********************************************************
PROCEDURE LOGO
********************************************************

SET TALK OFF
SET SAFETY OFF
SET STATUS OFF
SET ESCAPE OFF
*SET COLOR TO 7/1
CLEAR
TEXT
CLEAR
STORE DATE() TO MDATE
TEXT

    лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл
				Amjad Izhar
				A R S L A N   A U T O S

    лллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллл


               Program :  CosmoSoft Accounting System
               Version :  5
               Revision:  1
               Dated   :  Sunday 30th January, 2005
               
               This System Is Licensed Exclusively For The
               Use Of Messrs.


               A R S L A N   A U T O S  
               Liecence # LSL-AUT-005


ENDTEXT

@ 22, 35 SAY "Enter Today's Date: " GET MDATE

READ

MDT= LTRIM(STR(MONTH(MDATE),2))+'-'+LTRIM(STR(DAY(MDATE),2)+'-'+STR(YEAR(MDATE),4))

!date &mdt

CLEAR

RETURN
************************************************************************

****************************************************
PROCEDURE SCREEN
****************************************************
SET STEP OFF
SET ECHO OFF
SET DOHISTORY OFF
CLEAR
@ 0, 15 SAY ' A R S L A N   A U T O S '
RETURN
******************************************************

**********************************************************
PROCEDURE TOPPRT1
**********************************************************
SET STEP OFF
SET ECHO OFF
SET DOHISTORY OFF
@ 0, 0 SAY CHR(18)
@ 0, 0 SAY CHR(14)
@ 0, 1 SAY ' A R S L A N    A U T O S'
RETURN
************************************************************

*******************************************
PROCEDURE TOPPRT2
*********************************************
SET STEP OFF
SET ECHO OFF
SET DOHISTORY OFF
@ 0, 0 SAY CHR(18)
@ 0, 0 SAY CHR(14)
@ 0, 1 SAY 'A R S L A N   A U T O S'
CLEAR
RETURN
**********************************************

****************************************
PROCEDURE DISPRT
****************************************
SET STEP OFF
SET ECHO OFF
SET DOHISTORY OFF
CLEAR
@ 10, 15 SAY 'This Report requires 80 Column   (10 Inch Wide) '
@ 11, 15 SAY 'continues Printer Paper.  Please load the Paper '
@ 12, 15 SAY 'in the Printer, set it to TOF (Top of Form) and '
@ 13, 15 SAY 'Strike any key when ready ......'
WAIT ''
CLEAR
PRT= ' '
DO WHILE .not. PRT$'CQ'
   @ 5, 5 SAY 'Continue/Quit - C/Q ' GET PRT FUNCTION '!'
   READ
ENDDO
IF PRT='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
RETURN
**************************************************************

************************
PROCEDURE CHK
*************************
SET STEP OFF
SET ECHO OFF
SET DOHISTORY OFF
SET DATE brit
SET CENTURY ON

IF MDATE>CTOD('31/12/2006')
   SET ESCAPE OFF
   CLOSE ALL
   CLEAR
   CLOSE ALL
   QUIT
ENDIF
RETURN

PROCEDURE CHK2
SET STEP OFF
SET ECHO OFF
SET DOHISTORY OFF
SET DATE brit
IF MDATE>CTOD('01/07/92').or.MDATE<CTOD('01/07/91')
   SET ESCAPE OFF
   CLOSE ALL
   CLEAR
   CLOSE ALL
   QUIT
ENDIF
RETURN

PROCEDURE CODEMEN
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 26 SAY ' Ledger Heads Maintenance'
   SET COLOR TO 7/1
   @ 8, 20 PROMPT '1 - Legder Heads ........ Creation   '
   @ 9, 20 PROMPT '2 - Ledger Heads ........ Correction '
   @ 10, 20 PROMPT '3 - Ledger Heads ........ Deletion   '
   SET COLOR TO +7/1
   @ 12, 20 SAY '  -------- Printing/Display --------  '
   SET COLOR TO 7/1
   @ 14, 20 PROMPT '4 - Ledger Heads                           '
   @ 15, 20 PROMPT '5 - Debtors ...... (With Address)          '
   @ 16, 20 PROMPT '6 - Debtors ...... (Alphabetically Sorted) '
   @ 17, 20 PROMPT '7 - Creditors .... (With Address)          '
   @ 18, 20 PROMPT '8 - Creditors .... (Alphabetically Sorted) '
   @ 20, 20 PROMPT '9 - Return to Previous Menu                '
   CHOICE= 0
   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         DO ldgrcre
      CASE CHOICE=2
         DO ldgrcor
      CASE CHOICE=3
         DO ldgrdel
      CASE CHOICE=4
         DO ldgrprt
      CASE CHOICE=5
         DO ldgrprt1
      CASE CHOICE=6
         DO ldgrprt2
      CASE CHOICE=7
         DO ldgrprt3
      CASE CHOICE=8
         DO ldgrprt4
      CASE CHOICE=9
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE LDGRCRE
CLEAR
CLOSE ALL
SET EXCLUSIVE ON
SET DELETED ON
SET COLOR TO 7/1
SELECT 1
USE ldgrcode INDEX ac1
SELECT 2
USE master INDEX ac2
SELECT 3
USE chart INDEX crt
CITYZONE= 0
IF CITYZONE=1
   STORE 0 TO MCITY_CODE, MZONE_CODE
   SELECT 6
   USE city
   INDEX ON STR(CITY_CODE,2)+STR(ZONE_CODE,2) TO city-idx
   SELECT 5
   USE zone
   INDEX ON ZONE_CODE TO zone-idx
ENDIF
STORE 0 TO MC1, MC2, MC3, MLDGR_BAL, CNT
STORE CTOD('  /  /  ') TO MDATE
DO WHILE .T.
   CLEAR
   DO top
   SET COLOR TO +7/1
   @ 6, 28 SAY 'Ledger Heads Creations '
   SET COLOR TO 7/1
   @ 23, 1 TO 23, 79
   STORE 0 TO MLDGR_BAL, MCITY_CODE, CNT
   STORE SPACE(30) TO MTITLE
   DO WHILE .T.
      SELECT 1
      SET COLOR TO 7/1
      @ 24, 51 SAY 'Press Ctrl Q To Quit '
      @ 8, 8 SAY 'Ledger Head Code '
      SET COLOR TO /w
      @ 8, 30 SAY '  -  -   '
      @ 8, 30 GET MC1 PICTURE '@z 99'
      SET COLOR TO 7/1
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         SET CONFIRM OFF
         RETURN
      ENDIF
      @ 24, 50 CLEAR TO 24, 78
      IF MC1=0
         LOOP
      ENDIF
      SELECT 3
      SET FILTER TO MC1>=FR.and.MC1<=TO
      GOTO TOP
      IF MC1>=FR.and.MC1<=TO
         SEEK STR(FR,2)+STR(TO,2)
         IF FOUND()
            MCH= CHART
         ENDIF
      ENDIF
      SET FILTER TO
      SELECT 1
      IF MC1<10
         @ 24, 1 SAY 'Code C1 less than 10 not allowed.'
         WAIT ''
         @ 24, 1
         LOOP
      ENDIF
      SET COLOR TO *+7/1
      @ 8, 50 SAY MCH
      SET COLOR TO 7/1
      IF MC1>99.and.MC1<100
         SET COLOR TO *+7/1
         @ 8, 50 SAY 'Not Defined Head      '
         SET COLOR TO 7/1
      ENDIF
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      SET COLOR TO /w
      @ 8, 30 SAY MPC1
      @ 8, 33 GET MC2 PICTURE '@z 99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ 8, 33 SAY MPC2
      @ 8, 36 GET MC3 PICTURE '@z 999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ 8, 36 SAY MPC3
      SET COLOR TO 7/1
      IF READKEY()=12
         CLEAR ALL
         CLEAR
         RETURN
      ENDIF
      SELECT 1
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MLCODE
      SEEK MLCODE
      IF FOUND()
         @ 24, 15 SAY 'This Code Already Exist  -- Please Re-try '
         WAIT ''
         @ 24, 8 CLEAR TO 24, 63
         LOOP
      ENDIF
      IF MC3=0
         @ 12, 1 CLEAR TO 22, 79
         @ 12, 8 SAY ' Note:-   Three zeros at the end of the ledger code'
         @ 13, 8 SAY '          will make it a consolidation head i.e you'
         @ 14, 8 SAY '          will  not be  able  to Post  Transactions'
         @ 15, 8 SAY '          directly into this head.'
         @ 16, 8 SAY '          Hit any key to continue ..........'
         WAIT ''
         @ 12, 7 CLEAR TO 16, 63
      ENDIF
      @ 10, 8 SAY 'Title of Ledger Head ' GET MTITLE
      READ
      IF MTITLE=SPACE(30)
         LOOP
      ENDIF
      IF MC3<>0
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'N'
            @ 24, 8 SAY 'In Case OF Debtor Or Creditor, do you want to feed address - Y/N ' GET ANS PICTURE '!'
            READ
         ENDDO
         @ 24, 5 CLEAR TO 24, 78
         @ 12, 1 CLEAR TO 16, 79
         IF ANS='Y'
            STORE SPACE(25) TO MADD1, MADD2
            @ 12, 8 SAY 'Address              ' GET MADD1
            @ 14, 8 SAY '                     ' GET MADD2
            READ
            IF CITYZONE=1
               @ 16, 8 SAY 'City & Zone          ' GET MCITY_CODE PICTURE '99'
               @ 16, 33 GET MZONE_CODE PICTURE '99'
               READ
               STORE STR(MCITY_CODE,2)+STR(MZONE_CODE,2) TO MCODE
               SELECT 6
               SEEK MCODE
               IF FOUND()
                  SET COLOR TO /w 
                  @ 16, 36 SAY CITY
                  SET COLOR TO 7/1
               ELSE
                  @ 24, 15 SAY 'This Code Does Not Exist -- Press any key'
                  WAIT ''
                  @ 24, 14 CLEAR TO 24, 79
                  SELECT 1
                  LOOP
               ENDIF
            ENDIF
         ENDIF
         @ 18, 8 SAY 'Opening Balance  Rs. ' GET MLDGR_BAL PICTURE '999999999.99'
         READ
         MSTATUS= '  '
         IF MLDGR_BAL>0
            DO WHILE .not. MSTATUS$'CRDR'
               @ 20, 8 SAY 'Balance Status CR/DR ' GET MSTATUS FUNCTION '!' VALID MSTATUS='CR'.or.MSTATUS='DR'
               READ
            ENDDO
         ELSE
            @ 20, 1 CLEAR TO 20, 79
         ENDIF
         DO WHILE .T.
            SET COLOR TO +7/1
            @ 22, 42 SAY 'A/c Head Creation Date:' GET MDATE
            @ 21, 5 SAY 'N.B. Entering Vouchers Dated Earlier '
            @ 22, 5 SAY '     Than This Date May Cause Errors '
            READ
            SET COLOR TO 7/1 
            @ 21, 1 CLEAR TO 22, 40
            IF MDATE=CTOD('  /  /  ')
               LOOP
            ELSE
               EXIT
            ENDIF
         ENDDO
      ENDIF
      P= ' '
      DO WHILE .not. P$'YN'
         @ 24, 25 SAY 'Any Correction - Y/N ' GET P FUNCTION '!'
         READ
      ENDDO
      @ 24, 24 CLEAR TO 24, 63
      IF P='Y'
         LOOP
      ENDIF
      SELECT 1
      APPEND BLANK
      REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
      REPLACE TITLE WITH MTITLE
      FLUSH
      IF MC3<>0
         REPLACE SR_NO WITH 1, TITLE WITH MTITLE
         REPLACE LDGR_BAL WITH MLDGR_BAL, STATUS WITH MSTATUS
         REPLACE OPEN_BAL WITH MLDGR_BAL, OPEN_STAT WITH MSTATUS
         IF ANS='Y'
            REPLACE ADDRESS1 WITH MADD1, ADDRESS2 WITH MADD2
            IF CITYZONE=1
               REPLACE CITY_CODE WITH MCITY_CODE
               REPLACE ZONE_CODE WITH MZONE_CODE
            ENDIF
         ENDIF
      ENDIF
      SELECT 2
      IF MC3<>0
         APPEND BLANK
         REPLACE DATE WITH MDATE
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
         REPLACE DESC WITH '** Opening Balance **'
         REPLACE BL_AMOUNT WITH MLDGR_BAL, STATUS WITH MSTATUS
         REPLACE SR_NO WITH 1
         FLUSH
      ENDIF
      Q= ' '
      DO WHILE .not. Q$'YN'
         @ 24, 25 SAY 'More Data Entry - Y/N' GET Q FUNCTION '!'
         READ
      ENDDO
      IF Q='N'
         CLEAR
         CLOSE ALL
         SET CONFIRM OFF
         RETURN
      ENDIF
      EXIT
   ENDDO
   MC3= MC3+1
ENDDO
RETURN

PROCEDURE LDGRCOR
CLEAR
CLOSE ALL
SET CONFIRM OFF
SET EXCLUSIVE ON
SET DELETED ON
SET COLOR TO 7/1
SELECT 1
USE ldgrcode INDEX ac1
SELECT 2
USE master INDEX ac2
SELECT 3
USE chart INDEX crt
CITYZONE= 0
IF CITYZONE=1
   STORE 0 TO MCITY_CODE, MZONE_CODE
   SELECT 6
   USE city
   INDEX ON STR(CITY_CODE,2)+STR(ZONE_CODE,2) TO city-idx
   SELECT 5
   USE zone
   INDEX ON ZONE_CODE TO zone-idx
ENDIF
DO WHILE .T.
   CLEAR
   DO top
   SET COLOR TO +7/1
   @ 6, 28 SAY 'Ledger Heads Correction '
   SET COLOR TO 7/1
   @ 23, 1 TO 23, 79
   STORE 0 TO MLDGR_BAL
   STORE SPACE(30) TO MTITLE
   STORE 0 TO MC1, MC2, MC3, MLDGR_BAL, MCITY_CODE
   ANS= ' '
   SELECT 1
   SET COLOR TO 7/1
   @ 24, 51 SAY 'Press Ctrl Q To Quit '
   @ 8, 8 SAY 'Ledger Head Code '
   SET COLOR TO /w
   @ 8, 30 SAY '  -  -   '
   @ 8, 30 GET MC1 PICTURE '@z 99'
   SET COLOR TO 7/1
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      SET CONFIRM OFF
      RETURN
   ENDIF
   @ 24, 50 CLEAR TO 24, 78
   IF MC1=0
      LOOP
   ENDIF
   SELECT 3
   SET FILTER TO MC1>=FR.and.MC1<=TO
   GOTO TOP
   IF MC1>=FR.and.MC1<=TO
      SEEK STR(FR,2)+STR(TO,2)
      IF FOUND()
         MCH= CHART
      ENDIF
   ENDIF
   SET FILTER TO
   SELECT 1
   IF MC1<10
      @ 24, 1 SAY 'Code C1 less than 10 not allowed.'
      WAIT ''
      @ 24, 1
      LOOP
   ENDIF
   SET COLOR TO *+7/1
   @ 8, 50 SAY MCH
   SET COLOR TO 7/1
   IF MC1>99.and.MC1<100
      SET COLOR TO *+7/1
      @ 8, 50 SAY 'Not Defined Head      '
      SET COLOR TO 7/1
   ENDIF
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   SET COLOR TO /w
   @ 8, 30 SAY MPC1
   @ 8, 33 GET MC2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ 8, 33 SAY MPC2
   @ 8, 36 GET MC3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 8, 36 SAY MPC3
   SET COLOR TO 7/1
   IF READKEY()=12
      CLEAR ALL
      CLEAR
      RETURN
   ENDIF
   SELECT 1
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MLCODE
   SEEK MLCODE
   IF .not. FOUND()
      @ 24, 15 SAY 'This Code Does Not Exist -- Please Re-try '
      WAIT ''
      @ 24, 8 CLEAR TO 24, 63
      LOOP
   ENDIF
   SELECT 1
   @ 10, 8 SAY 'Title of Ledger Head ' GET TITLE
   READ
   IF SR_NO>1
      @ 14, 17 SAY 'Transactions already  exist for this  '
      @ 15, 17 SAY 'Ledger Head  therefore  you can only  '
      @ 16, 17 SAY 'make changes in the Title and cannot  '
      @ 17, 17 SAY 'make changes in the Openning Balance  '
      WAIT ''
      @ 14, 15 CLEAR TO 17, 77
      LOOP
   ENDIF
   IF SR_NO=1.and.C3<>0
      IF ADDRESS1<>SPACE(25)
         @ 12, 8 SAY 'Address              ' GET ADDRESS1
         @ 14, 8 SAY '                     ' GET ADDRESS2
         READ
         IF CITYZONE=1
            @ 16, 8 SAY 'City & Zone          ' GET CITY_CODE PICTURE '99'
            @ 16, 33 GET ZONE_CODE PICTURE '99'
            READ
            STORE STR(CITY_CODE,2)+STR(ZONE_CODE,2) TO MCODE
            SELECT 6
            SEEK MCODE
            IF FOUND()
               SET COLOR TO /w 
               @ 16, 36 SAY CITY
               SET COLOR TO 7/1
            ELSE
               @ 24, 15 SAY 'This Code Does Not Exist -- Press any key'
               WAIT ''
               @ 24, 14 CLEAR TO 24, 79
               SELECT 1
               LOOP
            ENDIF
         ENDIF
      ELSE
         SELECT 1
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'N'
            @ 24, 8 SAY 'In Case OF Debtor Or Creditor, do you want to feed address - Y/N ' GET ANS PICTURE '!'
            READ
         ENDDO
         @ 24, 5 CLEAR TO 24, 78
         @ 12, 1 CLEAR TO 16, 79
         IF ANS='Y'
            STORE SPACE(25) TO MADD1, MADD2
            @ 12, 8 SAY 'Address              ' GET MADD1
            @ 14, 8 SAY '                     ' GET MADD2
            READ
            IF CITYZONE=1
               @ 16, 8 SAY 'City & Zone          ' GET MCITY_CODE PICTURE '99'
               @ 16, 33 GET MZONE_CODE PICTURE '99'
               READ
               STORE STR(MCITY_CODE,2)+STR(MZONE_CODE,2) TO MCODE
               SELECT 6
               SEEK MCODE
               IF FOUND()
                  SET COLOR TO /w 
                  @ 16, 36 SAY CITY
                  SET COLOR TO 7/1
               ELSE
                  @ 24, 15 SAY 'This Code Does Not Exist -- Press any key'
                  WAIT ''
                  @ 24, 14 CLEAR TO 24, 79
                  SELECT 1
                  LOOP
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      SELECT 1
      MLDGR_BAL= LDGR_BAL
      @ 18, 8 SAY 'Opening Balance  Rs. ' GET MLDGR_BAL PICTURE '999999999.99'
      READ
      MSTATUS= '  '
      IF MLDGR_BAL>0
         DO WHILE .not. MSTATUS$'CRDR'
            MSTATUS= STATUS
            @ 20, 8 SAY 'Balance Status CR/DR ' GET MSTATUS FUNCTION '!' VALID MSTATUS='CR'.or.MSTATUS='DR'
            READ
         ENDDO
      ELSE
         @ 20, 1 CLEAR TO 20, 79
      ENDIF
      SELECT 2
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MLCODE
      SEEK MLCODE
      IF FOUND()
         MDATE= DATE
      ENDIF
      DO WHILE .T.
         SET COLOR TO +7/1
         @ 22, 42 SAY 'A/c Head Creation Date:' GET MDATE
         @ 21, 5 SAY 'N.B. Entering Vouchers Dated Earlier '
         @ 22, 5 SAY '     Than This Date May Cause Errors '
         READ
         SET COLOR TO 7/1 
         @ 21, 1 CLEAR TO 22, 40
         IF MDATE=CTOD('  /  /  ')
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
   ENDIF
   SELECT 1
   IF MC3<>0
      REPLACE SR_NO WITH 1
      REPLACE LDGR_BAL WITH MLDGR_BAL, STATUS WITH MSTATUS
      REPLACE OPEN_BAL WITH MLDGR_BAL, OPEN_STAT WITH MSTATUS
      IF ANS='Y'
         REPLACE ADDRESS1 WITH MADD1, ADDRESS2 WITH MADD2
         IF CITYZONE=1
            REPLACE CITY_CODE WITH MCITY_CODE
            REPLACE ZONE_CODE WITH MZONE_CODE
         ENDIF
      ENDIF
   ENDIF
   FLUSH
   SELECT 2
   IF MC3<>0
      REPLACE DATE WITH MDATE
      REPLACE DESC WITH '** Opening Balance **'
      REPLACE BL_AMOUNT WITH MLDGR_BAL, STATUS WITH MSTATUS
      REPLACE SR_NO WITH 1
   ENDIF
   FLUSH
   Q= ' '
   DO WHILE .not. Q$'YN'
      @ 24, 25 SAY 'More Data Correction - Y/N' GET Q FUNCTION '!'
      READ
   ENDDO
   IF Q='N'
      CLEAR
      CLOSE ALL
      SET CONFIRM OFF
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE LDGRDEL
CLEAR
CLOSE ALL
SET CONFIRM OFF
SET COLOR TO 7/1
SET DELETED ON
SELECT 1
USE ldgrcode INDEX ac1
SELECT 2
USE master INDEX ac2
SELECT 3
USE chart INDEX crt
DO WHILE .T.
   CLEAR
   R= 8
   DO top
   SET COLOR TO +7/1
   @ 6, 28 SAY 'Ledger Heads Deletion '
   SET COLOR TO 7/1
   @ 23, 1 TO 23, 79
   STORE 0 TO MC1, MC2, MC3, MLDGR_BAL
   @ 24, 51 SAY 'Press Ctrl Q To Quit '
   @ R, 8 SAY 'Ledger Head Code '
   SET COLOR TO /w
   @ R, 30 SAY '  -  -   '
   @ R, 30 GET MC1 PICTURE '@z 99'
   SET COLOR TO 7/1
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      SET CONFIRM OFF
      RETURN
   ENDIF
   @ 24, 50 CLEAR TO 24, 78
   IF MC1=0
      LOOP
   ENDIF
   SELECT 3
   SET FILTER TO MC1>=FR.and.MC1<=TO
   GOTO TOP
   IF MC1>=FR.and.MC1<=TO
      SEEK STR(FR,2)+STR(TO,2)
      IF FOUND()
         MCH= CHART
      ENDIF
   ENDIF
   SET FILTER TO
   SELECT 1
   IF MC1<10
      SET COLOR TO 7/1
      @ 23, 1 SAY 'First Level Cannot be Less than 10 '
      WAIT ''
      @ 23, 1
      @ 23, 1 CLEAR TO 23, 40
      LOOP
   ENDIF
   SET COLOR TO *+7/1
   @ R, 50 SAY MCH
   SET COLOR TO 7/1
   IF MC1>99.and.MC1<100
      SET COLOR TO *+7/1
      @ R, 50 SAY 'Not Defined Head   '
      SET COLOR TO 7/1
   ENDIF
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   SET COLOR TO /w
   @ R, 30 SAY MPC1
   @ R, 33 GET MC2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ R, 33 SAY MPC2
   @ R, 36 GET MC3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ R, 36 SAY MPC3
   SET COLOR TO 7/1
   IF READKEY()=12
      CLOSE ALL
      CLEAR
      SET CONFIRM OFF
      RETURN
   ENDIF
   SELECT 1
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MLCODE
   SEEK MLCODE
   IF .not. FOUND()
      @ 24, 15 SAY 'This Code Does Not Exist -- Please Re-try '
      WAIT ''
      @ 24, 8 CLEAR TO 24, 63
      LOOP
   ENDIF
   SELECT 1
   R= R+2
   @ R, 8 SAY 'Title of Ledger Head '
   SET COLOR TO /w
   @ R, 30 SAY TITLE
   SET COLOR TO 7/1
   IF SR_NO>1
      @ 14, 17 SAY 'Transactions exist for this Ledger  '
      @ 15, 17 SAY 'Head, therefore you  cannot delete  '
      @ 16, 17 SAY 'this Legder Head   '
      WAIT ''
      LOOP
   ENDIF
   R= R+2
   IF ADDRESS1<>SPACE(25)
      @ R, 8 SAY 'Address              '
      SET COLOR TO /w
      @ R, 30 SAY ADDRESS1
      SET COLOR TO 7/1
      R= R+2
      @ R, 8 SAY '                     '
      SET COLOR TO /w
      @ R, 30 SAY ADDRESS2
      SET COLOR TO 7/1
      R= R+2
   ENDIF
   IF SR_NO=1.and.C3<>0
      @ R, 8 SAY 'Opening Balance  Rs. '
      SET COLOR TO /w
      @ R, 30 SAY LDGR_BAL
      SET COLOR TO 7/1
      R= R+2
      @ R, 8 SAY 'Balance Status CR/DR '
      SET COLOR TO /w
      @ R, 30 SAY STATUS
      SET COLOR TO 7/1  
      SELECT 2
      SEEK MLCODE
      IF FOUND()
         @ R, 50 SAY 'Date'
         SET COLOR TO /w 
         @ R, 56 SAY DATE
         SET COLOR TO 7/1
      ENDIF
   ENDIF
   R= R+2
   P= ' '
   DO WHILE .not. P$'YN'
      @ 24, 25 SAY 'Are you Sure - Y/N' GET P FUNCTION '!'
      READ
   ENDDO
   @ 24, 1 CLEAR TO 24, 64
   IF P='Y'
      SELECT 1
      IF SR_NO=1.and.C3<>0
         DELETE FOR C1=MC1.and.C2=MC2.and.C3=MC3
         SELECT 2
         DELETE FOR C1=MC1.and.C2=MC2.and.C3=MC3
      ENDIF
      SELECT 1
      IF C3=0
         DELETE FOR C1=MC1.and.C2=MC2.and.C3=MC3
      ENDIF
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 24, 24 SAY 'More Data Deletion - Y/N' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE LDGRPRT
CLEAR
CLOSE ALL
SET COLOR TO 7/1
SET CONFIRM OFF
CLEAR
USE ldgrcode INDEX ac1
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
         DO screen
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

PROCEDURE LDGRPRT1
CLEAR
CLOSE ALL
SET CONFIRM OFF
SET COLOR TO 7/1
CLEAR
USE ldgrcode INDEX ac1
IF RECCOUNT()=0
   CLEAR
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
MC1= 0
DO WHILE .T.
   SET COLOR TO +7/1
   @ 10, 14 SAY 'Please give "First Level Code" of Debtors'
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.'
   @ 11, 66 GET MC1 PICTURE '99'
   @ 11, 69 SAY '-xx-xxx'
   SET COLOR TO 7/1
   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SET FILTER TO C1=MC1
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
CNT= 0
PAGE= 1
HEADING= .T.
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      IF P='P'
         DO top-prt1
         @ 2, 15 SAY CHR(18)
         @ 2, 32 SAY 'LIST OF DEBTORS'
         @ 3, 32 SAY '---------------'
         @ 4, 32 SAY '(With Address)'
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
      ELSE
         DO screen
         @ 3, 32 SAY 'LIST OF DEBTORS'
         L= L+1
         @ L, 32 SAY '---------------'
         L= L+1
         @ L, 32 SAY '(With Address)                   Page - '
         @ L, 70 SAY PAGE PICTURE '999'
         CNT= 0
         MREC2= RECNO()
      ENDIF
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 8 SAY 'Code'
      @ L, 25 SAY 'Title/Address'
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      HEADING= .F.
      L= L+1
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
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
      @ L, 20 SAY TITLE
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
      @ L, 20 SAY TITLE
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
         @ L, 20 SAY TITLE
      ENDIF
   ENDIF
   IF C3<>0
      L= L+1
      @ L, 20 SAY ADDRESS1
      L= L+1
      @ L, 20 SAY ADDRESS2
   ENDIF
   SKIP
   CNT= CNT+1
   L= L+2
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
         PAGE= PAGE-1
         IF PAGE<=1
            PAGE= 1
         ENDIF
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

PROCEDURE LDGRPRT2
CLEAR
CLOSE ALL
SET CONFIRM OFF
SET COLOR TO 7/1
CLEAR
USE ldgrcode INDEX tit-idx
IF RECCOUNT()=0
   CLEAR
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
MC1= 0
DO WHILE .T.
   SET COLOR TO +7/1
   @ 10, 14 SAY 'Please give "First Level Code" of Debtors'
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.'
   SET COLOR TO 7/1
   @ 11, 66 GET MC1 PICTURE '99'
   @ 11, 69 SAY '-xx-xxx'
   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SET FILTER TO C1=MC1
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
CNT= 0
PAGE= 1
HEADING= .T.
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      IF P='P'
         DO top-prt1
         @ 2, 15 SAY CHR(18)
         @ 2, 36 SAY 'LIST OF DEBTORS'
         @ 3, 36 SAY '---------------'
         @ 4, 32 SAY '(Alphabetically Sorted)'
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
      ELSE
         DO SCREEN
         @ 3, 36 SAY 'LIST OF DEBTORS'
         L= L+1
         @ L, 36 SAY '---------------'
         L= L+1
         @ L, 32 SAY '(Alphabetically Sorted)          Page - '
         @ L, 70 SAY PAGE PICTURE '999'
         CNT= 0
         MREC2= RECNO()
      ENDIF
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 8 SAY 'Code'
      @ L, 25 SAY 'Title'
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      HEADING= .F.
      L= L+1
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
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
      @ L, 20 SAY TITLE
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
      @ L, 20 SAY TITLE
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
         @ L, 20 SAY TITLE
      ENDIF
   ENDIF
   SKIP
   CNT= CNT+1
   L= L+2
   IF P='S'.and.L>=21
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
         PAGE= PAGE-1
         IF PAGE<=1
            PAGE= 1
         ENDIF
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

PROCEDURE LDGRPRT3
CLEAR
CLOSE ALL
SET CONFIRM OFF
SET COLOR TO 7/1
CLEAR
USE ldgrcode INDEX ac1
IF RECCOUNT()=0
   CLEAR
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
MC1= 0
DO WHILE .T.
   SET COLOR TO +7/1
   @ 10, 14 SAY 'Please give "First Level Code" of Creditors'
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.'
   @ 11, 66 GET MC1 PICTURE '99'
   @ 11, 69 SAY '-xx-xxx'
   SET COLOR TO 7/1
   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SET FILTER TO C1=MC1
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
CNT= 0
PAGE= 1
HEADING= .T.
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      IF P='P'
         DO top-prt1
         @ 2, 15 SAY CHR(18)
         @ 2, 32 SAY 'LIST OF CREDITORS'
         @ 3, 32 SAY '-----------------'
         @ 4, 32 SAY '(With Address)'
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
      ELSE
         DO screen
         @ 3, 32 SAY 'LIST OF CREDITORS'
         L= L+1
         @ L, 32 SAY '-----------------'
         L= L+1
         @ L, 32 SAY '(With Address)                   Page - '
         @ L, 70 SAY PAGE PICTURE '999'
         CNT= 0
         MREC2= RECNO()
      ENDIF
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 8 SAY 'Code'
      @ L, 25 SAY 'Title/Address'
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      HEADING= .F.
      L= L+1
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
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
      @ L, 20 SAY TITLE
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
      @ L, 20 SAY TITLE
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
         @ L, 20 SAY TITLE
      ENDIF
   ENDIF
   IF C3<>0
      L= L+1
      @ L, 20 SAY ADDRESS1
      L= L+1
      @ L, 20 SAY ADDRESS2
   ENDIF
   SKIP
   CNT= CNT+1
   L= L+2
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
         PAGE= PAGE-1
         IF PAGE<=1
            PAGE= 1
         ENDIF
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

PROCEDURE LDGRPRT4
CLEAR
CLOSE ALL
SET CONFIRM OFF
SET COLOR TO 7/1
CLEAR
USE ldgrcode INDEX tit-idx
IF RECCOUNT()=0
   CLEAR
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
MC1= 0
DO WHILE .T.
   SET COLOR TO +7/1
   @ 10, 14 SAY 'Please give "First Level Code" of Creditors'
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.'
   SET COLOR TO 7/1
   @ 11, 66 GET MC1 PICTURE '99'
   @ 11, 69 SAY '-xx-xxx'
   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SET FILTER TO C1=MC1
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
CNT= 0
PAGE= 1
HEADING= .T.
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      IF P='P'
         DO top-prt1
         @ 2, 15 SAY CHR(18)
         @ 2, 36 SAY 'LIST OF CREDITORS'
         @ 3, 36 SAY '-----------------'
         @ 4, 32 SAY '(Alphabetically Sorted)'
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
      ELSE
         DO SCREEN
         @ 3, 36 SAY 'LIST OF CREDITORS'
         L= L+1
         @ L, 36 SAY '-----------------'
         L= L+1
         @ L, 32 SAY '(Alphabetically Sorted)          Page - '
         @ L, 70 SAY PAGE PICTURE '999'
         CNT= 0
         MREC2= RECNO()
      ENDIF
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 8 SAY 'Code'
      @ L, 25 SAY 'Title'
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      HEADING= .F.
      L= L+1
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
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
      @ L, 20 SAY TITLE
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
      @ L, 20 SAY TITLE
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
         @ L, 20 SAY TITLE
      ENDIF
   ENDIF
   SKIP
   CNT= CNT+1
   L= L+2
   IF P='S'.and.L>=21
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
         PAGE= PAGE-1
         IF PAGE<=1
            PAGE= 1
         ENDIF
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

PROCEDURE BUDG-MEN
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 26 SAY ' Budget Heads Maintenance'
   SET COLOR TO 7/1
   @ 8, 23 PROMPT '1 - Budget Heads ..... Creation '
   @ 10, 23 PROMPT '2 - Budget Heads ..... Printing/Display'
   @ 12, 23 PROMPT '3 - Return to Previous Menu     '
   CHOICE= 0
   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         DO budg-ent
      CASE CHOICE=2
         DO budg-prt
      CASE CHOICE=3
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE BUDGENT
CLEAR
CLOSE ALL
SET CONFIRM OFF
SET COLOR TO 7/1
SELECT 1
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 3
USE chart
INDEX ON STR(FR,2)+STR(TO,2) TO crt
STORE 0 TO MC1, MC2, MC3
DO WHILE .T.
   CLEAR
   DO top
   SET COLOR TO +7/1
   @ 6, 25 SAY 'Budget Maintenance Entry '
   SET COLOR TO 7/1 
   @ 23, 1 TO 23, 79
   SELECT 1
   SET COLOR TO 7/1
   @ 24, 51 SAY 'Press Ctrl Q To Quit '
   @ 8, 8 SAY 'Ledger Head Code '
   SET COLOR TO /w
   @ 8, 30 SAY '  -  -   '
   @ 8, 30 GET MC1 PICTURE '@z 99'
   SET COLOR TO 7/1
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      SET CONFIRM OFF
      RETURN
   ENDIF
   @ 24, 50 CLEAR TO 24, 78
   IF MC1=0
      LOOP
   ENDIF
   SELECT 3
   SET FILTER TO MC1>=FR.and.MC1<=TO
   GOTO TOP
   IF MC1>=FR.and.MC1<=TO
      SEEK STR(FR,2)+STR(TO,2)
      IF FOUND()
         MCH= CHART
      ENDIF
   ENDIF
   SET FILTER TO
   SELECT 1
   IF MC1<10
      @ 24, 1 SAY 'Code C1 less than 10 not allowed.'
      WAIT ''
      @ 24, 1
      LOOP
   ENDIF
   SET COLOR TO *+7/1
   @ 8, 50 SAY MCH
   SET COLOR TO 7/1
   IF MC1>99.and.MC1<100
      SET COLOR TO *+7/1
      @ 8, 50 SAY 'Not Defined Head      '
      SET COLOR TO 7/1
   ENDIF
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   SET COLOR TO /w
   @ 8, 30 SAY MPC1
   @ 8, 33 GET MC2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ 8, 33 SAY MPC2
   @ 8, 36 GET MC3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 8, 36 SAY MPC3
   SET COLOR TO 7/1
   IF READKEY()=12
      CLEAR ALL
      CLEAR
      RETURN
   ENDIF
   SELECT 1
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MLCODE
   SEEK MLCODE
   IF .not. FOUND()
      @ 24, 15 SAY 'This Code Does Not Exist  -- Please Re-try '
      WAIT ''
      @ 24, 8 CLEAR TO 24, 63
      LOOP
   ENDIF
   IF MC3=0
      @ 12, 1 CLEAR TO 22, 79
      @ 12, 8 SAY ' Note:-   Three zeros at the end of the ledger code'
      @ 13, 8 SAY '          will make it a consolidation head i.e you'
      @ 14, 8 SAY '          will  not be  able  to Post  Transactions'
      @ 15, 8 SAY '          directly into this head.'
      @ 16, 8 SAY '          Hit any key to continue ..........'
      WAIT ''
      CLEAR
      LOOP
   ENDIF
   @ 10, 8 SAY 'Title of Ledger Head ' GET TITLE
   CLEAR GETS
   IF SR_NO>1
      @ 14, 17 SAY 'Transactions already  exist for this  '
      @ 15, 17 SAY 'Ledger Head  therefore  you can only  '
      @ 16, 17 SAY 'make changes in the Title and cannot  '
      @ 17, 17 SAY 'make changes in the Budget Amount   '
      WAIT ''
      CLEAR
      LOOP
   ENDIF
   IF SR_NO=1.and.C3<>0
      @ 12, 8 SAY 'Budget Amount  Rs.   ' GET BUDGET PICTURE '999999999.99'
      READ
   ENDIF
   Q= ' '
   DO WHILE .not. Q$'YN'
      @ 24, 25 SAY 'More Data Entry - Y/N' GET Q FUNCTION '!'
      READ
   ENDDO
   IF Q='N'
      CLEAR
      CLOSE ALL
      SET CONFIRM OFF
      RETURN
   ENDIF
   MC3= MC3+1
ENDDO
RETURN

PROCEDURE BUDGPRT
CLEAR
CLOSE ALL
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
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
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      PAGE= PAGE+1
      IF P='P'
         DO top-prt1
         @ 3, 15 SAY CHR(18)
         @ 3, 33 SAY 'BUDGET  HEADS'
         @ 4, 32 SAY '--------------'
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
      ELSE
         DO screen
         @ 3, 30 SAY 'BUDGET  HEADS'
         L= L+1
         @ L, 30 SAY '--------------                   Page - '
         @ L, 70 SAY PAGE PICTURE '999'
      ENDIF
      L= L+2
      @ L, 7 SAY 'Code'
      @ L, 28 SAY 'Title'
      @ L, 52 SAY 'Budget'
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      HEADING= .F.
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
   IF BUDGET=0
      SKIP
      LOOP
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
      @ L, 20 SAY TITLE
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
      @ L, 20 SAY TITLE
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
         @ L, 20 SAY TITLE
      ENDIF
   ENDIF
   @ L, 45 SAY BUDGET PICTURE '999,999,999.99'
   SKIP
   IF EOF()
      IF P='P'
         EJECT
         SET PRINTER OFF
         SET DEVICE TO SCREEN
      ENDIF
   ENDIF
   IF P='S'
      IF L>21
         @ 24, 15 SAY ''
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            SET CONFIRM OFF
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
         EJECT
         L= 3
         HEADING= .T.
      ENDIF
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ELSE
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLOSE ALL
RETURN

PROCEDURE VCHMEN
DO WHILE .T.
   CLEAR
   CLOSE ALL
   SET EXCLUSIVE ON
   SET DELETED ON
   USE tran-key INDEX key-idx
   DELETE ALL FOR FLAG=' '
   CLEAR
   CLOSE ALL
   SET COLOR TO 7/1
   USE voucher
   ZAP
   USE vch-cor
   ZAP
   CLEAR
   CLOSE ALL
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 25 SAY '   Daily Transactions'
   SET COLOR TO 7/1
   @ 8, 23 PROMPT '1 - Vouchers ....... Entry      '
   @ 9, 23 PROMPT '2 - Vouchers ....... Entry - 2  '
   @ 10, 23 PROMPT '3 - Vouchers ....... Correction '
   @ 11, 23 PROMPT '4 - Vouchers ....... Deletion   '
   SET COLOR TO +7/1
   @ 12, 23 SAY '------------- Printing --------- '
   SET COLOR TO 7/1
   @ 13, 23 PROMPT '5 - Vouchers ....... Date Wise     '
   @ 14, 23 PROMPT '6 - Vouchers ....... Type Wise     '
   @ 15, 23 PROMPT '7 - Complete Voucher Printing - I  '
   @ 16, 23 PROMPT '8 - Complete Voucher Printing - II '
   SET COLOR TO +7/1
   @ 17, 23 SAY '---------- View On Screen ------ '
   SET COLOR TO 7/1
   @ 18, 23 PROMPT ' 9 - Vouchers ....... Type Wise   '
   @ 19, 23 PROMPT '10 - Vouchers ....... Date Wise   '
   @ 20, 23 PROMPT '11 - Return to Previous Menu     '
   CHOICE= 0
   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         DO vchent
      CASE CHOICE=2
         DO vch2ent
      CASE CHOICE=3
         DO vchcor
      CASE CHOICE=4
         DO vchdel
      CASE CHOICE=5
         DO vchdprt
      CASE CHOICE=6
         DO vchtprt
      CASE CHOICE=7
         DO vchprt1
      CASE CHOICE=8
         DO vchprt2
      CASE CHOICE=9
         DO vchtvew
      CASE CHOICE=10
         DO vchdvew
      CASE CHOICE=11
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE VCHENT
CLEAR
CLOSE ALL
SET DELETED ON
SET EXCLUSIVE ON
SET DATE brit
SET COLOR TO 7/1
CLEAR
@ 2, 20 SAY 'Re-indexing Data Files  ....  Please wait'
SELECT 3
USE ldgrcode INDEX ac1
SELECT 5
USE master INDEX ac2
DIMENSION MAVT( 7)
MAVT( 1)= 'CR'
MAVT( 2)= 'CP'
MAVT( 3)= 'BR'
MAVT( 4)= 'BP'
MAVT( 5)= 'JV'
MAVT( 6)= 'SV'
MAVT( 7)= 'PV'
STORE DATE() TO MDATE
STORE SPACE(1) TO CANS, MQES
DO WHILE .T.
   IF CANS<>'Y'
      PAGE= 1
      CLEAR
      MVCH1= '  '
      MVCH2= '    '
      STORE 0 TO MCODE, MSRNO, MAMOUNT, AMT1, AMT2
      STORE 0 TO MC1, MC2, MC3, MVCH_NO1, MVCH_NO2
      STORE SPACE(30) TO MTITLE, MDESCRIP, MDESCRIP1
      M= 6
      L= 16
      STORE SPACE(1) TO Y, MDR_CR, MANS
      STORE 0 TO MVCH_TYPE, AMT1, AMT2
   ENDIF
   R= 1
   @ 2, 1 TO 2, 79 DOUBLE
   @ 21, 1 TO 21, 79 DOUBLE
   @ 1, 30 SAY 'VOUCHER  ENTRY'
   @ 1, 60 SAY 'PAGE #'
   @ 1, 67 SAY PAGE PICTURE '99'
   @ 22, 50 SAY 'Press Esc to Return'
   R= 3
   @ R, 5 SAY 'Date       ' GET MDATE
   READ
   IF READKEY()=12
      CLEAR
      CLEAR ALL
      CLOSE ALL
      RETURN
   ENDIF
   DO chk
   @ 22, 45 CLEAR TO 22, 79
   @ R, 32 SAY 'Voucher Type  ' GET MVCH_TYPE PICTURE '9'
   @ 22, 1 SAY 'Voucher Types:'
   @ 22, 17 SAY '1 - '
   @ 23, 17 SAY '2 - '
   @ 22, 58 SAY '3 - '
   @ 23, 58 SAY '4 - '
   @ 24, 17 SAY '5 - '
   @ 24, 40 SAY '6 - '
   @ 24, 58 SAY '7 - '
   @ 22, 21 SAY 'Cash Receipt - '
   @ 23, 21 SAY 'Cash Payment - '
   @ 22, 62 SAY 'Bank Receipt - '
   @ 23, 62 SAY 'Bank Payment - '
   @ 24, 21 SAY 'Journal      - '
   @ 24, 44 SAY 'Sales - '
   @ 24, 62 SAY 'Purchase     - '
   SET COLOR TO /w 
   @ 22, 36 SAY 'CR'
   @ 23, 36 SAY 'CP'
   @ 22, 77 SAY 'BR'
   @ 23, 77 SAY 'BP'
   @ 24, 36 SAY 'JV'
   @ 24, 52 SAY 'SV'
   @ 24, 77 SAY 'PV'
   SET COLOR TO 7/1
   READ
   IF MVCH_TYPE=0
      LOOP
   ENDIF
   IF MVCH_TYPE>=8
      @ R, 55 SAY '  '
      @ R, 30 SAY 'No such Voucher type exist -- Press any key'
      WAIT ''
      @ R, 30 SAY SPACE(55)
      LOOP
   ENDIF
   @ 21, 1 CLEAR TO 24, 79
   SET COLOR TO /w
   @ R, 50 SAY MAVT(MVCH_TYPE)
   SET COLOR TO 7/1 
   ABC= 0
   DO WHILE ABC=0
      @ 23, 50 SAY 'Press Esc to Return'
      @ 5, 22 SAY '          '
      @ 5, 5 SAY 'Voucher No '
      SET COLOR TO /w
      @ 5, 17 SAY MAVT(MVCH_TYPE)
      @ 5, 20 SAY '  -     '
      SET COLOR TO 7/1
      @ 5, 20 GET MVCH_NO1 PICTURE '@z 99'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 23, 50 SAY SPACE(26)
      IF MVCH_NO1=0
         LOOP
      ENDIF
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
      SET COLOR TO /w
      @ 5, 20 SAY MVCH1
      SET COLOR TO 7/1   
      SELECT 4
      USE tran-key INDEX key-idx
      SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1
      GOTO BOTTOM
      MVCH_NO2= VCH_NO2+1
      @ 5, 23 GET MVCH_NO2 PICTURE '@z 99999'
      READ
      IF MVCH_NO2=0
         LOOP
      ENDIF
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
      SET COLOR TO /w
      @ 5, 23 SAY MVCH2
      SET COLOR TO 7/1
      SELECT 4
      SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
      IF FOUND()
         ?? CHR(7)
         @ 23, 1 SAY SPACE(78)
         @ 23, 15 SAY 'This Voucher Already Exists  -- Press any key'
         WAIT ''
         @ 23, 10 SAY SPACE(75)
         LOOP
      ELSE
         APPEND BLANK
         REPLACE VCH_TYPE WITH MVCH_TYPE
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
         FLUSH
      ENDIF
      ABC= 1
   ENDDO
   SELECT 4
   USE
   R= 5
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Sr  Acct-No.  Title                        Dr/Cr   Dr-Amt       Cr-Amt   More'
   R= R+1
   @ R, 1 TO R, 79 DOUBLE
   R= 8
   PAGE= 1
   MANS= 'Y'
   DO WHILE .T.
      IF MANS='Y'.or.MQES='N'
         MQES= ' '
         R= R+1
         IF R>19
            PAGE= PAGE+1
            @ 1, 67 SAY PAGE PICTURE '99'
            @ 9, 0 CLEAR TO 19, 79
            R= 9
         ENDIF
         STORE 0 TO MCODE, MAMOUNT
         STORE 0 TO MC1, MC2, MC3
         STORE SPACE(30) TO MTITLE, MDESCRIP, MDESCRIP1
         STORE SPACE(1) TO MDR_CR, MANS
      ENDIF
      MSRNO= 1+MSRNO
      @ 23, 50 SAY 'Press Esc to Return'
      @ R, 1 SAY MSRNO PICTURE '999'
      ABC= 0
      DO WHILE ABC=0
         @ R, 5 GET MC1 PICTURE '99'
         SET COLOR TO /w
         @ R, 7 SAY '-'
         @ R, 8 GET MC2 PICTURE '99'
         @ R, 10 SAY '-'
         @ R, 11 GET MC3 PICTURE '999'
         READ
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         SET COLOR TO /w
         @ R, 5 SAY MPC1 PICTURE '99'
         @ R, 8 SAY MPC2 PICTURE '99'
         @ R, 11 SAY MPC3 PICTURE '999'
         SET COLOR TO 7/1
         IF READKEY()=12
            SET COLOR TO 7/1
            CLEAR ALL
            CLEAR
            RETURN
         ENDIF
         SET COLOR TO 7/1
         @ 23, 50 SAY SPACE(25)
         SET COLOR TO /w
         IF MC3=0
            LOOP
         ENDIF
         SELECT 5
         SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
         IF FOUND()
            IF MDATE<DATE
               ?? CHR(7)
               SET COLOR TO +7/1
               @ 13, 20 SAY 'This Voucher Dated earlier than the Head'
               @ 14, 20 SAY 'creation Date of this Head Please Correct.'
               @ 15, 20 SAY 'Head Creation Date before Entering this Voucher.'
               SET COLOR TO 7/1
               @ 17, 25 SAY 'Press Any Key To Continue...'
               A= INKEY(10000000)
               @ 13, 20 CLEAR TO 18, 69
               EXIT
               LOOP
            ENDIF
         ENDIF
         SELECT 3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SEEK MCODE
         IF FOUND()
            @ R, 15 SAY TITLE
            STORE TITLE TO MTITLE
         ELSE
            SET COLOR TO 7/1 
            ?? CHR(7)
            @ 23, 20 SAY 'This Code Does Not Exist ... Please Re-try '
            WAIT ''
            SET COLOR TO 7/1
            @ 23, 10 SAY SPACE(70)
            SELECT 2
            LOOP
         ENDIF
         IF LOCK='Y'
            ?? CHR(7)
            @ 23, 13 SAY 'This Code Has Been Defined As Lock ... Please Re-try '
            WAIT ''
            SET COLOR TO 7/1
            @ 23, 10 SAY SPACE(79)
            SELECT 2
            LOOP
         ENDIF
         ABC= 1
      ENDDO
      SET COLOR TO 7/1
      @ 4, 32 SAY 'Description   ' GET MDESCRIP
      @ 5, 32 SAY '              ' GET MDESCRIP1
      READ
      SET COLOR TO /w
      MDR_CR= ' '
      DO WHILE .not. MDR_CR$'DC'
         @ R, 46 GET MDR_CR PICTURE '@!'
         READ
      ENDDO
      IF MDR_CR='D'
         STORE 'DR' TO MDR_CR
      ELSE
         STORE 'CR' TO MDR_CR
      ENDIF
      @ R, 46 SAY MDR_CR PICTURE '!!'
      COL= IIF(MDR_CR='DR',50,62)
      DO WHILE .T.
         @ R, COL GET MAMOUNT PICTURE '999999,999.99'
         READ
         IF MAMOUNT<=0
            LOOP
         ENDIF
         EXIT
      ENDDO
      SET COLOR TO 7/1 
      IF MDR_CR='DR'
         AMT1= AMT1+MAMOUNT
         @ 21, 48 SAY AMT1
      ELSE
         AMT2= AMT2+MAMOUNT
         @ 21, 64 SAY AMT2
      ENDIF
      IF MSRNO<>1
         MANS= ' '
         DO WHILE .not. MANS$'YN'
            MANS= 'Y'
            @ R, 78 GET MANS PICTURE '!'
            READ
         ENDDO
         SET COLOR TO 7/1
      ELSE
         MANS= 'Y'
      ENDIF
      SELECT 1
      USE voucher
      IF MANS='Y'
         APPEND BLANK
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
         REPLACE DATE WITH MDATE, VCH_TYPE WITH MVCH_TYPE
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
         REPLACE DESCRIP WITH MDESCRIP, SR_NO WITH MSRNO
         REPLACE AMOUNT WITH MAMOUNT, DR_CR WITH MDR_CR
         REPLACE DESCRIP1 WITH MDESCRIP1
         LOOP
      ELSE
         APPEND BLANK
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
         REPLACE DATE WITH MDATE, VCH_TYPE WITH MVCH_TYPE
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
         REPLACE DESCRIP WITH MDESCRIP, SR_NO WITH MSRNO
         REPLACE AMOUNT WITH MAMOUNT, DR_CR WITH MDR_CR
         REPLACE DESCRIP1 WITH MDESCRIP1
      ENDIF
      SUM FOR DR_CR='DR'.and.VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 AMOUNT TO AMT1
      SUM FOR DR_CR='CR'.and.VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 AMOUNT TO AMT2
      @ 21, 35 CLEAR TO 21, 79
      @ 21, 49 SAY AMT1 PICTURE '9999999,999.99'
      @ 21, 64 SAY AMT2 PICTURE '9999999,999.99'
      IF AMT1<>AMT2
         ?? CHR(7)
         @ 23, 15 SAY 'Unbalanced Voucher - Please Correct '
         WAIT ''
         @ 23, 15 SAY SPACE(50)
         MANS= ' '
         @ 10, 0 CLEAR TO 22, 79
         DO v-cor
         IF READKEY()=12
            SET COLOR TO 7/1 
            CLEAR ALL
            CLEAR
            RETURN
         ENDIF
         IF MANS='Y'
            LOOP
         ENDIF
         IF MQES='N'
            LOOP
         ENDIF
      ENDIF
      IF AMT1=AMT2
         COR= ' '
         DO WHILE .not. COR$'YN'
            @ 23, 0 SAY SPACE(79)
            @ 23, 30 SAY 'Any Correction Y/N  ' GET COR PICTURE '!'
            READ
         ENDDO
         @ 23, 30 SAY SPACE(49)
         IF COR='Y'
            @ 10, 0 CLEAR TO 22, 79
            DO v-cor
            IF READKEY()=12
               SET COLOR TO 7/1 
               CLEAR ALL
               RETURN
            ENDIF
            IF MANS='Y'
               LOOP
            ENDIF
            IF MQES='N'
               LOOP
            ENDIF
         ELSE
            @ 23, 30 SAY SPACE(49)
         ENDIF
         SELECT 1
         USE
         SELECT 2
         USE tran INDEX tranent
         APPEND FROM voucher FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         FLUSH
         USE
         SELECT 4
         USE tran-key INDEX key-idx
         SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
         ENDIF
         USE
         SELECT 1
         USE voucher
         ZAP
         USE
         EXIT
      ENDIF
   ENDDO
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 23, 0 SAY SPACE(79)
      @ 23, 27 SAY 'More Vouchers To Enter - Y/N  ' GET ANS PICTURE '!'
      READ
   ENDDO
   IF ANS='Y'
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
CLEAR
CLOSE ALL
RETURN

PROCEDURE VCH2ENT
CLEAR
CLOSE ALL
SET EXCLUSIVE ON
SET DATE brit
SET COLOR TO 7/1
SET DELETED ON
CLEAR
@ 2, 20 SAY 'Re-indexing Data Files  ....  Please wait'
SELECT 3
USE ldgrcode INDEX ac1
SELECT 4
USE tran-key INDEX key-idx
SELECT 5
USE master INDEX ac2
DIMENSION MAVT( 7)
MAVT( 1)= 'CR'
MAVT( 2)= 'CP'
MAVT( 3)= 'BR'
MAVT( 4)= 'BP'
MAVT( 5)= 'JV'
MAVT( 6)= 'SV'
MAVT( 7)= 'PV'
STORE DATE() TO MDATE
STORE SPACE(1) TO COR, CANS, MQES
STORE 0 TO MC1, MC2, MC3, M1, M2, M3, MV_NO
DO WHILE .T.
   IF COR<>'Y'
      PAGE= 1
      CLEAR
      COR= ' '
      MVCH1= '  '
      MVCH2= '    '
      STORE 0 TO MCODE, MSRNO, MAMOUNT, AMT1, AMT2
      STORE 0 TO MC1, MC2, MC3, MVCH_NO1, MVCH_NO2
      STORE SPACE(30) TO MTITLE, MDESCRIP, MDESCRIP1
      M= 6
      L= 16
      STORE SPACE(2) TO MDR_CR
      STORE 0 TO MVCH_TYPE, AMT1, AMT2
   ENDIF
   R= 1
   @ 2, 1 TO 2, 79 DOUBLE
   @ 21, 1 TO 21, 79 DOUBLE
   @ 1, 30 SAY 'VOUCHER  ENTRY'
   @ 1, 60 SAY 'PAGE #'
   @ 1, 67 SAY PAGE PICTURE '99'
   @ 23, 50 SAY 'Press Esc to Return'
   R= 3
   @ R, 1 SAY 'A/c Code .. '
   SET COLOR TO /w
   @ R, 15 SAY '  -  -   '
   SET COLOR TO 7/1
   @ R, 15 GET MC1 PICTURE '99'
   @ R, 18 GET MC2 PICTURE '99'
   @ R, 21 GET MC3 PICTURE '999'
   READ
   IF READKEY()=12
      CLEAR
      CLEAR ALL
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 40 CLEAR TO 23, 79
   IF MC3=0
      LOOP
   ENDIF
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   SET COLOR TO /w
   @ R, 15 SAY MPC1 PICTURE '99'
   @ R, 18 SAY MPC2 PICTURE '99'
   @ R, 21 SAY MPC3 PICTURE '999'
   SET COLOR TO 7/1
   SELECT 3
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
   SEEK MCODE
   IF FOUND()
      CTITLE= TITLE
      SET COLOR TO /w
      @ R, 31 SAY CTITLE
      SET COLOR TO 7/1
   ELSE
      SET COLOR TO 7/1
      @ 23, 20 SAY 'This Code Does Not Exist ... Please Re-try '
      SET COLOR TO 7/1
      ? CHR(7)
      A= INKEY(200000)
      @ 23, 10 CLEAR TO 23, 79
      LOOP
   ENDIF
   SELECT 1
   DO WHILE .not. MDR_CR$'DRCR'
      @ R, 65 SAY 'Dr/Cr  ' GET MDR_CR FUNCTION '!'
      READ
   ENDDO
   DO chk
   @ R+2, 1 SAY 'Voucher Type' GET MVCH_TYPE PICTURE '9'
   @ 22, 45 CLEAR TO 22, 79
   @ 22, 1 SAY 'Voucher Types:'
   @ 22, 17 SAY '1 - '
   @ 23, 17 SAY '2 - '
   @ 22, 58 SAY '3 - '
   @ 23, 58 SAY '4 - '
   @ 24, 17 SAY '5 - '
   @ 24, 40 SAY '6 - '
   @ 24, 58 SAY '7 - '
   @ 22, 21 SAY 'Cash Receipt - '
   @ 23, 21 SAY 'Cash Payment - '
   @ 22, 62 SAY 'Bank Receipt - '
   @ 23, 62 SAY 'Bank Payment - '
   @ 24, 21 SAY 'Journal      - '
   @ 24, 44 SAY 'Sales - '
   @ 24, 62 SAY 'Purchase     - '
   SET COLOR TO /w 
   @ 22, 36 SAY 'CR'
   @ 23, 36 SAY 'CP'
   @ 22, 77 SAY 'BR'
   @ 23, 77 SAY 'BP'
   @ 24, 36 SAY 'JV'
   @ 24, 52 SAY 'SV'
   @ 24, 77 SAY 'PV'
   SET COLOR TO 7/1
   READ
   IF MVCH_TYPE=0
      LOOP
   ENDIF
   IF MVCH_TYPE>=8
      @ R, 30 SAY SPACE(75)
      @ R, 55 SAY '  '
      @ R, 30 SAY 'No such Voucher type exist -- Press any key'
      WAIT ''
      @ R, 30 SAY SPACE(75)
      LOOP
   ENDIF
   SET COLOR TO /w
   @ R+2, 16 SAY MAVT(MVCH_TYPE)
   SET COLOR TO 7/1
   @ R+3, 1 TO R+3, 79 DOUBLE
   @ 21, 1 CLEAR TO 24, 79
   COR= ' '
   DO WHILE .not. COR$'YN'
      COR= 'N'
      @ 23, 30 SAY 'Any Correction Y/N  ' GET COR PICTURE '!'
      READ
   ENDDO
   @ 23, 30 SAY SPACE(49)
   IF COR='Y'
      @ 9, 0 CLEAR TO 22, 77
      LOOP
      IF READKEY()=12
         SET COLOR TO 7/1
         CLEAR ALL
         RETURN
      ENDIF
   ENDIF
   R= 7
   IF MDR_CR='CR'
      @ R, 1 SAY 'Sr  Vch-No      Date    Debit Code   Description                        Amount'
   ELSE
      @ R, 1 SAY 'Sr  Vch-No      Date    Credit Code  Description                       Amount'
   ENDIF
   R= R+1
   PAGE= 1
   MANS= 'Y'
   MSRNO= 1
   MS= 1
   DO WHILE .T.
      IF R>19
         PAGE= PAGE+1
         @ 1, 67 SAY PAGE PICTURE '99'
         @ 8, 0 CLEAR TO 19, 79
         R= 8
      ENDIF
      IF COR<>'Y'
         STORE 0 TO MCODE, MAMOUNT, CNT
         STORE SPACE(30) TO DTITLE
         STORE SPACE(1) TO MANS
      ENDIF
      SET COLOR TO 7/1
      @ R, 0 SAY MS PICTURE '999'
      ABC= 0
      DO WHILE ABC=0
         @ 23, 50 SAY 'Press Esc to Return'
         SET COLOR TO /w
         @ R, 5 SAY MAVT(MVCH_TYPE)
         @ R, 7 SAY '-  -     '
         SET COLOR TO 7/1        
         @ R, 8 GET MVCH_NO1 PICTURE '@z 99'
         READ
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 23, 50 SAY SPACE(26)
         IF MVCH_NO1=0
            LOOP
         ENDIF
         MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
         SET COLOR TO /w
         @ R, 8 SAY MVCH1
         SET COLOR TO 7/1   
         SELECT 4
         USE tran-key INDEX key-idx
         SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1
         GOTO BOTTOM
         IF CNT=0
            MVCH_NO2= VCH_NO2+1
         ENDIF
         @ R, 11 GET MVCH_NO2 PICTURE '@z 99999'
         READ
         IF MVCH_NO2=0
            LOOP
         ENDIF
         MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
         SET COLOR TO /w
         @ R, 11 SAY MVCH2
         SET COLOR TO 7/1
         SELECT 4
         SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
         IF CNT=0
            IF FOUND()
               ?? CHR(7)
               @ 23, 1 SAY SPACE(78)
               @ 23, 15 SAY 'This Voucher Already Exists  -- Press any key'
               WAIT ''
               @ 23, 10 SAY SPACE(75)
               LOOP
            ELSE
               APPEND BLANK
               REPLACE VCH_TYPE WITH MVCH_TYPE
               REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
               FLUSH
               CNT= 1
            ENDIF
         ENDIF
         ABC= 1
      ENDDO
      SELECT 4
      USE
      @ R, 17 GET MDATE
      ABC= 0
      DO WHILE ABC=0
         SET COLOR TO /w
         @ R, 26 SAY '  -  -   '
         SET COLOR TO 7/1
         @ R, 26 GET M1 PICTURE '99'
         @ R, 29 GET M2 PICTURE '99'
         @ R, 32 GET M3 PICTURE '999'
         READ
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(M1))))+LTRIM(STR(M1))
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(M2))))+LTRIM(STR(M2))
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(M3))))+LTRIM(STR(M3))
         SET COLOR TO /w
         @ R, 26 SAY MPC1 PICTURE '99'
         @ R, 29 SAY MPC2 PICTURE '99'
         @ R, 32 SAY MPC3 PICTURE '999'
         SET COLOR TO 7/1
         SELECT 5
         SEEK STR(C1,2)+STR(C2,2)+STR(C3,3)
         IF FOUND()
            IF MDATE<DATE
               SAVE SCREEN TO AA
               SET COLOR TO +7/1
               @ 14, 18 SAY '                                                     '
               @ 15, 18 SAY '   This Voucher Dated earlier than the Head          '
               @ 16, 18 SAY '   creation Date of this Head Please Correct.        '
               @ 17, 18 SAY '   Head Creation Date before Entering this Voucher.  '
               @ 18, 18 SAY '                                                     '
               SET COLOR TO 7/1
               @ 19, 25 SAY 'Press Any Key To Continue...'
               A= INKEY(10000000)
               RESTORE SCREEN FROM AA
               LOOP
            ENDIF
         ENDIF
         SET COLOR TO /w
         IF READKEY()=12
            SET COLOR TO 7/1
            CLEAR ALL
            CLEAR
            RETURN
         ENDIF
         IF M3=0
            LOOP
         ENDIF
         SELECT 3
         STORE STR(M1,2)+STR(M2,2)+STR(M3,3) TO MCODE
         SEEK MCODE
         IF FOUND()
            @ R, 36 SAY LEFT(TITLE,26)
            STORE TITLE TO DTITLE
         ELSE
            ?? CHR(7)
            SET COLOR TO 7/1
            @ 23, 20 SAY 'This Code Does Not Exist ... Please Re-try '
            WAIT ''
            SET COLOR TO 7/1
            @ 23, 11 SAY SPACE(70)
            SELECT 2
            LOOP
         ENDIF
         IF LOCK='Y'
            ?? CHR(7)
            @ 23, 13 SAY 'This Code Has Been Defined As Lock ... Please Re-try '
            WAIT ''
            SET COLOR TO 7/1
            @ 23, 10 SAY SPACE(79)
            SELECT 2
            LOOP
         ENDIF
         ABC= 1
      ENDDO
      SELECT 1
      SET COLOR TO 7/1
      @ 4, 15 SAY 'Description '
      @ 4, 31 GET MDESCRIP
      @ 5, 31 GET MDESCRIP1
      READ
      DO WHILE .T.
         @ R, 65 GET MAMOUNT PICTURE '999,999,999.99'
         READ
         IF MAMOUNT<=0
            LOOP
         ENDIF
         EXIT
      ENDDO
      COR= 'N'
      @ 23, 30 SAY 'Any Correction Y/N  ' GET COR PICTURE 'Y'
      READ
      IF UPPER(COR)='Y'
         @ 23, 30 SAY SPACE(49)
         LOOP
      ENDIF
      IF UPPER(COR)='N'
         SELECT 1
         USE voucher
         IF MDR_CR='CR'
            APPEND BLANK
            REPLACE DATE WITH MDATE, VCH_TYPE WITH MVCH_TYPE
            REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
            REPLACE C1 WITH M1, C2 WITH M2, C3 WITH M3
            REPLACE DESCRIP WITH MDESCRIP, SR_NO WITH 1
            REPLACE AMOUNT WITH MAMOUNT, DR_CR WITH 'DR'
            REPLACE DESCRIP1 WITH MDESCRIP1
            APPEND BLANK
            REPLACE DATE WITH MDATE, VCH_TYPE WITH MVCH_TYPE
            REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
            REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
            REPLACE DESCRIP WITH MDESCRIP, SR_NO WITH 2
            REPLACE AMOUNT WITH MAMOUNT, DR_CR WITH 'CR'
            REPLACE DESCRIP1 WITH MDESCRIP1
         ENDIF
         IF MDR_CR='DR'
            APPEND BLANK
            REPLACE DATE WITH MDATE, VCH_TYPE WITH MVCH_TYPE
            REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
            REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
            REPLACE DESCRIP WITH MDESCRIP, SR_NO WITH 1
            REPLACE AMOUNT WITH MAMOUNT, DR_CR WITH 'DR'
            REPLACE DESCRIP1 WITH MDESCRIP1
            APPEND BLANK
            REPLACE DATE WITH MDATE, VCH_TYPE WITH MVCH_TYPE
            REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
            REPLACE C1 WITH M1, C2 WITH M2, C3 WITH M3
            REPLACE DESCRIP WITH MDESCRIP, SR_NO WITH 2
            REPLACE AMOUNT WITH MAMOUNT, DR_CR WITH 'CR'
            REPLACE DESCRIP1 WITH MDESCRIP1
         ENDIF
         MSRNO= MSRNO+1
      ENDIF
      SELECT 1
      USE
      SELECT 2
      USE tran INDEX tranent
      APPEND FROM voucher FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      FLUSH
      USE
      SELECT 4
      USE tran-key INDEX key-idx
      SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
      IF FOUND()
         REPLACE FLAG WITH 'Y'
      ENDIF
      USE
      SELECT 1
      USE voucher
      ZAP
      IF UPPER(COR)='N'
         AN= 'Y'
         @ 23, 25 SAY 'More Data For This Credit Code Y/N  ' GET AN PICTURE 'Y'
         READ
         @ 23, 25 SAY SPACE(40)
         IF UPPER(AN)='Y'
            R= R+1
            MS= MS+1
            MSRNO= MSRNO+1
            STORE 0 TO MAMOUNT, M3
            STORE SPACE(30) TO MTITLE, MDESCRIP, MDESCRIP1
            @ 23, 25 SAY SPACE(40)
            LOOP
         ENDIF
      ENDIF
      EXIT
   ENDDO
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 23, 30 SAY 'Any More Data Y/N  ' GET ANS PICTURE '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
CLEAR
CLEAR ALL
RETURN

PROCEDURE VCOR
SELECT 1
GOTO TOP
MREC2= RECNO()
DO vdisp
GOTO TOP
MREC2= RECNO()
R= 8
DO WHILE .T.
   R= R+1
   IF R>19
      PAGE= PAGE+1
      @ 2, 67 SAY PAGE PICTURE '99'
      @ 9, 0 CLEAR TO 19, 79
      R= 9
      IF .not. EOF()
         MREC2= RECNO()
         DO vdisp
         GOTO MREC2
         R= 9
      ENDIF
   ENDIF
   SELECT 1
   SET COLOR TO 7/1 
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   SET COLOR TO 7/1 
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 1
      DELETE
   ENDIF
   IF MQES='N'
      SELECT 1
      IF .not. EOF()
         MSR_NO= SR_NO
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 1
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE SPACE(30) TO MTITLE, MDESCRIP, MDESCRIP1
         STORE 0 TO MC1, MC2, MC3, MAMOUNT
         GOTO BOTTOM
         MSR_NO= SR_NO+1
         DO WHILE .T.
            @ R, 1 SAY MSR_NO PICTURE '999'
            @ R, 5 GET MC1 PICTURE '99'
            SET COLOR TO /w
            @ R, 7 SAY '-'
            @ R, 8 GET MC2 PICTURE '99'
            @ R, 10 SAY '-'
            @ R, 11 GET MC3 PICTURE '999'
            READ
            MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
            MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
            MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
            SET COLOR TO /w
            @ R, 5 SAY MPC1 PICTURE '99'
            @ R, 8 SAY MPC2 PICTURE '99'
            @ R, 11 SAY MPC3 PICTURE '999'
            SET COLOR TO 7/1
            @ 23, 50 SAY SPACE(35)
            IF READKEY()=12
               SET COLOR TO 7/1
               SELECT 1
               ZAP
               CLEAR
               RETURN
            ENDIF
            IF MC3=0
               ?? CHR(7)
               SET COLOR TO 7/1
               @ 23, 0 CLEAR TO 23, 77
               @ 23, 17 SAY 'You Can Not Post in Consolided Head - Retry'
               WAIT ''
               SET COLOR TO 7/1
               @ 23, 10 SAY SPACE(60)
               LOOP
            ENDIF
            SELECT 3
            STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
            SEEK MCODE
            IF FOUND()
               SET COLOR TO /w
               @ R, 15 SAY TITLE
               STORE TITLE TO MTITLE
               SET COLOR TO 7/1 
            ELSE
               ?? CHR(7)
               SET COLOR TO 7/1
               @ 23, 0 CLEAR TO 23, 77
               @ 23, 17 SAY 'Code Does Not Exist In File - Retry'
               WAIT ''
               SET COLOR TO 7/1
               @ 23, 10 SAY SPACE(60)
               LOOP
            ENDIF
            IF LOCK='Y'
               ?? CHR(7)
               @ 23, 13 SAY 'This Code Has Been Defined As Lock ... Please Re-try '
               WAIT ''
               SET COLOR TO 7/1
               @ 23, 10 SAY SPACE(79)
               LOOP
            ENDIF
            SET COLOR TO 7/1
            @ 5, 32 SAY 'Description   ' GET MDESCRIP
            @ 5, 32 SAY 'Description   ' GET MDESCRIP1
            READ
            SET COLOR TO /w
            MDR_CR= ' '
            DO WHILE .not. MDR_CR$'DC'
               @ R, 46 GET MDR_CR PICTURE '!!'
               READ
            ENDDO
            IF MDR_CR='D'
               STORE 'DR' TO MDR_CR
            ELSE
               STORE 'CR' TO MDR_CR
            ENDIF
            @ R, 46 SAY MDR_CR PICTURE '!!'
            COL= IIF(MDR_CR='DR',50,62)
            MT= 0
            DO WHILE MT=0
               @ R, COL GET MAMOUNT PICTURE '999999,999.99'
               READ
               IF MAMOUNT<=0
                  LOOP
               ENDIF
               MT= 1
            ENDDO
            EXIT
         ENDDO
         SELECT 1
         APPEND BLANK
         REPLACE DATE WITH MDATE, VCH_TYPE WITH MVCH_TYPE
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
         REPLACE DESCRIP WITH MDESCRIP, SR_NO WITH MSR_NO
         REPLACE AMOUNT WITH MAMOUNT, DR_CR WITH MDR_CR
         REPLACE DESCRIP1 WITH MDESCRIP1
         FLUSH
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 1
         MC1= C1
         MC2= C2
         MC3= C3
         MDESCRIP= DESCRIP
         MDESCRIP1= DESCRIP1
         MAMOUNT= AMOUNT
         MDR_CR= DR_CR
         SET COLOR TO 7/1
         @ R, 1 SAY SR_NO PICTURE '999'
         @ R, 5 GET MC1 PICTURE '99'
         SET COLOR TO /w
         @ R, 7 SAY '-'
         @ R, 8 GET MC2 PICTURE '99'
         @ R, 10 SAY '-'
         @ R, 11 GET MC3 PICTURE '999'
         READ
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         SET COLOR TO /w
         @ R, 5 SAY MPC1 PICTURE '99'
         @ R, 8 SAY MPC2 PICTURE '99'
         @ R, 11 SAY MPC3 PICTURE '999'
         SET COLOR TO 7/1 
         @ 23, 50 SAY SPACE(35)
         IF READKEY()=12
            SET COLOR TO 7/1
            SELECT 1
            ZAP
            CLEAR
            RETURN
         ENDIF
         IF MC3=0
            ?? CHR(7)
            @ 23, 0 CLEAR TO 23, 77
            @ 23, 17 SAY 'You Can Not Post in Consolided Head - Retry'
            WAIT ''
            SET COLOR TO 7/1
            @ 23, 10 SAY SPACE(60)
            LOOP
         ENDIF
         SELECT 3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SEEK MCODE
         IF FOUND()
            SET COLOR TO /w
            @ R, 15 SAY TITLE
            STORE TITLE TO MTITLE
            SET COLOR TO 7/1 
         ELSE
            ?? CHR(7)
            SET COLOR TO 7/1
            @ 23, 0 CLEAR TO 23, 77
            @ 23, 17 SAY 'Code Does Not Exist In File - Retry'
            WAIT ''
            SET COLOR TO 7/1
            @ 23, 10 SAY SPACE(60)
            LOOP
         ENDIF
         IF LOCK='Y'
            ?? CHR(7)
            @ 23, 13 SAY 'This Code Has Been Defined As Lock ... Please Re-try '
            WAIT ''
            SET COLOR TO 7/1
            @ 23, 10 SAY SPACE(79)
            LOOP
         ENDIF
         SET COLOR TO 7/1
         @ 5, 32 SAY 'Description   ' GET MDESCRIP
         READ
         @ 5, 32 SAY 'Description   ' GET MDESCRIP1
         READ
         SET COLOR TO /w
         DO WHILE .not. MDR_CR$'DC'
            @ R, 46 GET MDR_CR PICTURE '!!'
            READ
            EXIT
         ENDDO
         IF MDR_CR='D'
            STORE 'DR' TO MDR_CR
         ELSE
            STORE 'CR' TO MDR_CR
         ENDIF
         COL= IIF(MDR_CR='DR',50,62)
         DO WHILE .T.
            SET COLOR TO 7/1
            @ R, 50 SAY SPACE(28)
            SET COLOR TO /w 
            @ R, COL GET MAMOUNT PICTURE '999999,999.99'
            READ
            IF MAMOUNT<=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         EXIT
      ENDDO
      SELECT 1
      REPLACE DATE WITH MDATE, VCH_TYPE WITH MVCH_TYPE
      REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
      REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3, DR_CR WITH MDR_CR
      REPLACE DESCRIP WITH MDESCRIP, AMOUNT WITH MAMOUNT
      REPLACE DESCRIP1 WITH MDESCRIP1
      FLUSH
   ENDIF
   SELECT 1
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      SET COLOR TO 7/1 
      @ R, 78 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      IF .not. EOF()
         SKIP
      ENDIF
   ENDIF
   IF MANS='N'
      SET COLOR TO 7/1
      SUM FOR DR_CR='DR'.and.VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 AMOUNT TO AMT1
      SUM FOR DR_CR='CR'.and.VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 AMOUNT TO AMT2
      @ 21, 49 SAY AMT1 PICTURE '9999999,999.99'
      @ 21, 64 SAY AMT2 PICTURE '9999999,999.99'
      SELECT 1
      IF AMT1<>AMT2
         ?? CHR(7)
         SET COLOR TO 7/1
         @ 23, 0 CLEAR TO 23, 77
         @ 23, 15 SAY 'Unbalanced Voucher - Please Correct '
         WAIT ''
         @ 23, 15
         @ 10, 0 CLEAR TO 23, 79
         SELECT 1
         GOTO TOP
         MREC2= RECNO()
         DO vdisp
         SELECT 1
         GOTO TOP
         MREC2= RECNO()
         R= 8
         LOOP
      ENDIF
      SELECT 1
      GOTO MREC2
      MSRNO= 0
      DO WHILE .not. EOF()
         MSRNO= MSRNO+1
         REPLACE SR_NO WITH MSRNO
         SKIP
      ENDDO
      MANS= ' '
      MQES= ' '
      EXIT
   ENDIF
ENDDO
RETURN

PROCEDURE VDISP
SELECT 1
USE voucher
GOTO MREC2
SELECT 3
USE ldgrcode INDEX ac1
SELECT 1
R= 9
DO WHILE .not. EOF()
   @ R, 1 SAY SR_NO PICTURE '999'
   SET COLOR TO /w
   @ R, 5 SAY C1 PICTURE '99'
   @ R, 7 SAY '-'
   @ R, 8 SAY C2 PICTURE '99'
   @ R, 10 SAY '-'
   @ R, 11 SAY C3 PICTURE '999'
   SET COLOR TO /w
   STORE C1 TO MC1
   STORE C2 TO MC2
   STORE C3 TO MC3
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   SET COLOR TO /w
   @ R, 5 SAY MPC1 PICTURE '99'
   @ R, 8 SAY MPC2 PICTURE '99'
   @ R, 11 SAY MPC3 PICTURE '999'
   SET COLOR TO 7/1
   SELECT 3
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
   SEEK MCODE
   IF FOUND()
      @ R, 15 SAY TITLE
   ENDIF
   SELECT 1
   SET COLOR TO 7/1
   @ 5, 32 SAY 'Description   '
   SET COLOR TO /w
   @ 5, 45 SAY DESCRIP
   @ R, 46 SAY DR_CR PICTURE '@!'
   COL= IIF(DR_CR='DR',50,62)
   @ R, COL SAY AMOUNT PICTURE '999999,999.99'
   SKIP
   R= R+1
   IF R>19
      SET COLOR TO 7/1
      RETURN
   ENDIF
ENDDO
SET COLOR TO 7/1
RETURN

PROCEDURE VCHCOR
CLEAR
CLOSE ALL
SET DELETED ON
SET EXCLUSIVE ON
SET COLOR TO 7/1
@ 2, 20 SAY 'Re-indexing Data Files  ....  Please wait'
SELECT 2
USE tran INDEX tranent
SELECT 3
USE ldgrcode INDEX ac1
SELECT 4
USE tran-key INDEX key-idx
SELECT 5
USE master INDEX ac2
DIMENSION MAVT( 7)
MAVT( 1)= 'CR'
MAVT( 2)= 'CP'
MAVT( 3)= 'BR'
MAVT( 4)= 'BP'
MAVT( 5)= 'JV'
MAVT( 6)= 'SV'
MAVT( 7)= 'PV'
CANS= ' '
DO WHILE .T.
   IF CANS<>'Y'
      PAGE= 1
      CLEAR
      MVCH1= '  '
      MVCH2= '    '
      STORE 0 TO MCODE, MSRNO, MAMOUNT, AMT1, AMT2
      STORE 0 TO MC1, MC2, MC3, MVCH_NO1, MVCH_NO2
      STORE SPACE(30) TO MTITLE, MDESCRIP
      M= 6
      L= 16
      STORE SPACE(2) TO MDR_CR
      STORE 0 TO MVCH_TYPE, AMT1, AMT2
   ENDIF
   R= 1
   @ 2, 1 TO 2, 79 DOUBLE
   @ 21, 1 TO 21, 79 DOUBLE
   @ 1, 30 SAY 'VOUCHER  CORRECTION '
   @ 1, 60 SAY 'PAGE #'
   @ 1, 67 SAY PAGE PICTURE '99'
   @ 22, 50 SAY 'Press Esc to Return'
   R= 3
   @ 22, 45 CLEAR TO 22, 79
   @ R, 32 SAY 'Voucher Type  ' GET MVCH_TYPE PICTURE '9'
   @ 22, 1 SAY 'Voucher Types:'
   @ 22, 17 SAY '1 - '
   @ 23, 17 SAY '2 - '
   @ 22, 58 SAY '3 - '
   @ 23, 58 SAY '4 - '
   @ 24, 17 SAY '5 - '
   @ 24, 40 SAY '6 - '
   @ 24, 58 SAY '7 - '
   @ 22, 21 SAY 'Cash Receipt - '
   @ 23, 21 SAY 'Cash Payment - '
   @ 22, 62 SAY 'Bank Receipt - '
   @ 23, 62 SAY 'Bank Payment - '
   @ 24, 21 SAY 'Journal      - '
   @ 24, 44 SAY 'Sales - '
   @ 24, 62 SAY 'Purchase     - '
   SET COLOR TO /w 
   @ 22, 36 SAY 'CR'
   @ 23, 36 SAY 'CP'
   @ 22, 77 SAY 'BR'
   @ 23, 77 SAY 'BP'
   @ 24, 36 SAY 'JV'
   @ 24, 52 SAY 'SV'
   @ 24, 77 SAY 'PV'
   SET COLOR TO 7/1
   READ
   IF MVCH_TYPE=0
      LOOP
   ENDIF
   IF MVCH_TYPE>=8
      @ R, 55 SAY '  '
      @ R, 30 SAY 'No such Voucher type exist -- Press any key'
      WAIT ''
      @ R, 30 SAY SPACE(55)
      LOOP
   ENDIF
   @ 21, 1 CLEAR TO 24, 79
   SET COLOR TO /w
   @ R, 52 SAY MAVT(MVCH_TYPE)
   SET COLOR TO 7/1
   @ 23, 50 SAY 'Press Esc to Return'
   @ 5, 22 SAY '          '
   @ 5, 5 SAY 'Voucher No '
   SET COLOR TO /w
   @ 5, 17 SAY MAVT(MVCH_TYPE)
   @ 5, 20 SAY '  -     '
   SET COLOR TO 7/1
   @ 5, 20 GET MVCH_NO1 PICTURE '@z 99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 50 SAY SPACE(26)
   IF MVCH_NO1=0
      LOOP
   ENDIF
   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
   SET COLOR TO /w
   @ 5, 20 SAY MVCH1
   SET COLOR TO 7/1   
   @ 5, 23 GET MVCH_NO2 PICTURE '@z 99999'
   READ
   IF MVCH_NO2=0
      LOOP
   ENDIF
   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
   SET COLOR TO /w
   @ 5, 23 SAY MVCH2
   SET COLOR TO 7/1
   SELECT 2
   SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Voucher No Does Not Exists  -- Press any key'
      WAIT ''
      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to  Ledger,  therefore '
      @ 13, 15 SAY 'you cannot Change/Correct it.  If you still want to '
      @ 14, 15 SAY 'Change/Correct it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ELSE
      SELECT 2
      USE
      SELECT 1
      USE vch-cor
      MREC2= RECNO()
      APPEND FROM tran FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SELECT 1
      USE vch-cor
      GOTO MREC2
   ENDIF
   SELECT 1
   @ 3, 5 SAY 'Date       ' GET DATE
   READ
   MDATE= DATE
   R= 5
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Sr  Acct-No.  Title                        Dr/Cr   Dr-Amt       Cr-Amt   More'
   R= R+1
   @ R, 1 TO R, 79 DOUBLE
   DO v-cor1
   SELECT 1
   SUM FOR DR_CR='DR'.and.VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 AMOUNT TO AMT1
   SUM FOR DR_CR='CR'.and.VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 AMOUNT TO AMT2
   IF AMT1=AMT2
      IF READKEY()=12
         SET COLOR TO 7/1 
         CLEAR ALL
         RETURN
      ENDIF
      SELECT 1
      GOTO MREC2
      REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 DATE WITH MDATE
      USE
      SELECT 2
      USE tran INDEX tranent
      DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      APPEND FROM vch-cor FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      FLUSH
      SELECT 1
      USE vch-cor
      ZAP
      USE
   ENDIF
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 23, 0 CLEAR TO 23, 79
      @ 23, 30 SAY 'Any More Correction Y/N  ' GET ANS PICTURE '!'
      READ
   ENDDO
   IF ANS='Y'
      LOOP
   ELSE
      EXIT
   ENDIF
ENDDO
CLEAR
CLEAR ALL
RETURN

PROCEDURE V-COR1
SELECT 1
GOTO TOP
MREC2= RECNO()
DO vdisp1
GOTO TOP
MREC2= RECNO()
R= 8
DO WHILE .T.
   R= R+1
   IF R>19
      PAGE= PAGE+1
      @ 2, 67 SAY PAGE PICTURE '99'
      @ 9, 0 CLEAR TO 19, 79
      R= 9
      IF .not. EOF()
         MREC2= RECNO()
         DO vdisp1
         GOTO MREC2
         R= 9
      ENDIF
   ENDIF
   SELECT 1
   SET COLOR TO 7/1 
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   SET COLOR TO 7/1 
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 1
      DELETE
   ENDIF
   IF MQES='N'
      SELECT 1
      IF .not. EOF()
         MSR_NO= SR_NO
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 1
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE 0 TO MC1, MC2, MC3, MAMOUNT
         STORE SPACE(30) TO MTITLE, MDESCRIP, MDESCRIP1
         GOTO BOTTOM
         MSR_NO= SR_NO+1
         DO WHILE .T.
            @ R, 1 SAY MSR_NO PICTURE '999'
            @ R, 5 GET MC1 PICTURE '99'
            SET COLOR TO /w
            @ R, 7 SAY '-'
            @ R, 8 GET MC2 PICTURE '99'
            @ R, 10 SAY '-'
            @ R, 11 GET MC3 PICTURE '999'
            READ
            MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
            MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
            MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
            SET COLOR TO /w
            @ R, 5 SAY MPC1 PICTURE '99'
            @ R, 8 SAY MPC2 PICTURE '99'
            @ R, 11 SAY MPC3 PICTURE '999'
            SET COLOR TO 7/1
            @ 23, 50 SAY SPACE(35)
            IF READKEY()=12
               SET COLOR TO 7/1
               SELECT 1
               ZAP
               CLEAR
               RETURN
            ENDIF
            IF MC3=0
               ?? CHR(7)
               SET COLOR TO 7/1
               @ 23, 0 CLEAR TO 23, 77
               @ 23, 17 SAY 'You Can Not Post in Consolided Head - Retry'
               WAIT ''
               SET COLOR TO 7/1
               @ 23, 10 SAY SPACE(60)
               LOOP
            ENDIF
            SELECT 3
            STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
            SEEK MCODE
            IF FOUND()
               SET COLOR TO /w
               @ R, 15 SAY TITLE
               STORE TITLE TO MTITLE
               SET COLOR TO 7/1 
            ELSE
               ?? CHR(7)
               SET COLOR TO 7/1
               @ 23, 0 CLEAR TO 23, 77
               @ 23, 17 SAY 'Code Does Not Exist In File - Retry'
               WAIT ''
               SET COLOR TO 7/1
               @ 23, 10 SAY SPACE(60)
               LOOP
            ENDIF
            IF LOCK='Y'
               ?? CHR(7)
               @ 23, 13 SAY 'This Code Has Been Defined As Lock ... Please Re-try '
               WAIT ''
               SET COLOR TO 7/1
               @ 23, 10 SAY SPACE(79)
               LOOP
            ENDIF
            SET COLOR TO 7/1
            @ 5, 32 SAY 'Description   ' GET MDESCRIP
            READ
            @ 5, 32 SAY 'Description   ' GET MDESCRIP1
            READ
            SET COLOR TO /w
            MDR_CR= ' '
            DO WHILE .not. MDR_CR$'DC'
               @ R, 46 GET MDR_CR PICTURE '!!'
               READ
            ENDDO
            IF MDR_CR='D'
               STORE 'DR' TO MDR_CR
            ELSE
               STORE 'CR' TO MDR_CR
            ENDIF
            @ R, 46 SAY MDR_CR PICTURE '!!'
            COL= IIF(MDR_CR='DR',50,62)
            MT= 0
            DO WHILE MT=0
               @ R, COL GET MAMOUNT PICTURE '999999,999.99'
               READ
               IF MAMOUNT<=0
                  LOOP
               ENDIF
               MT= 1
            ENDDO
            EXIT
         ENDDO
         SELECT 1
         APPEND BLANK
         REPLACE DATE WITH MDATE, VCH_TYPE WITH MVCH_TYPE
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
         REPLACE DESCRIP WITH MDESCRIP, SR_NO WITH MSR_NO
         REPLACE AMOUNT WITH MAMOUNT, DR_CR WITH MDR_CR
         REPLACE DESCRIP1 WITH MDESCRIP1
         FLUSH
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 1
         MC1= C1
         MC2= C2
         MC3= C3
         MDESCRIP= DESCRIP
         MDESCRIP1= DESCRIP1
         MAMOUNT= AMOUNT
         MDR_CR= DR_CR
         SET COLOR TO 7/1
         @ R, 1 SAY SR_NO PICTURE '999'
         @ R, 5 GET MC1 PICTURE '99'
         SET COLOR TO /w
         @ R, 7 SAY '-'
         @ R, 8 GET MC2 PICTURE '99'
         @ R, 10 SAY '-'
         @ R, 11 GET MC3 PICTURE '999'
         READ
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         SET COLOR TO /w
         @ R, 5 SAY MPC1 PICTURE '99'
         @ R, 8 SAY MPC2 PICTURE '99'
         @ R, 11 SAY MPC3 PICTURE '999'
         SET COLOR TO 7/1 
         @ 23, 50 SAY SPACE(35)
         IF READKEY()=12
            SET COLOR TO 7/1
            SELECT 1
            ZAP
            CLEAR
            RETURN
         ENDIF
         IF MC3=0
            ?? CHR(7)
            @ 23, 0 CLEAR TO 23, 77
            @ 23, 17 SAY 'You Can Not Post in Consolided Head - Retry'
            WAIT ''
            SET COLOR TO 7/1
            @ 23, 10 SAY SPACE(60)
            LOOP
         ENDIF
         SELECT 3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SEEK MCODE
         IF FOUND()
            SET COLOR TO /w
            @ R, 15 SAY TITLE
            STORE TITLE TO MTITLE
            SET COLOR TO 7/1 
         ELSE
            ?? CHR(7)
            SET COLOR TO 7/1
            @ 23, 0 CLEAR TO 23, 77
            @ 23, 17 SAY 'Code Does Not Exist In File - Retry'
            WAIT ''
            SET COLOR TO 7/1
            @ 23, 10 SAY SPACE(60)
            LOOP
         ENDIF
         IF LOCK='Y'
            ?? CHR(7)
            @ 23, 13 SAY 'This Code Has Been Defined As Lock ... Please Re-try '
            WAIT ''
            SET COLOR TO 7/1
            @ 23, 10 SAY SPACE(79)
            LOOP
         ENDIF
         SET COLOR TO 7/1
         @ 5, 32 SAY 'Description   ' GET MDESCRIP
         READ
         @ 5, 32 SAY 'Description   ' GET MDESCRIP1
         READ
         SET COLOR TO /w
         DO WHILE .not. MDR_CR$'DC'
            @ R, 46 GET MDR_CR PICTURE '!!'
            READ
            EXIT
         ENDDO
         IF MDR_CR='D'
            STORE 'DR' TO MDR_CR
         ELSE
            STORE 'CR' TO MDR_CR
         ENDIF
         COL= IIF(MDR_CR='DR',50,62)
         DO WHILE .T.
            SET COLOR TO 7/1
            @ R, 50 SAY SPACE(28)
            SET COLOR TO /w 
            @ R, COL GET MAMOUNT PICTURE '999999,999.99'
            READ
            IF MAMOUNT<=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         EXIT
      ENDDO
      SELECT 1
      REPLACE DATE WITH MDATE, VCH_TYPE WITH MVCH_TYPE
      REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
      REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3, DR_CR WITH MDR_CR
      REPLACE DESCRIP WITH MDESCRIP, AMOUNT WITH MAMOUNT
      REPLACE DESCRIP1 WITH MDESCRIP1
      FLUSH
   ENDIF
   SELECT 1
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      SET COLOR TO 7/1 
      @ R, 78 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      IF .not. EOF()
         SKIP
      ENDIF
   ENDIF
   IF MANS='N'
      SET COLOR TO 7/1
      SUM FOR DR_CR='DR'.and.VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 AMOUNT TO AMT1
      SUM FOR DR_CR='CR'.and.VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 AMOUNT TO AMT2
      @ 21, 49 SAY AMT1 PICTURE '9999999,999.99'
      @ 21, 64 SAY AMT2 PICTURE '9999999,999.99'
      SELECT 1
      IF AMT1<>AMT2
         ?? CHR(7)
         SET COLOR TO 7/1
         @ 23, 0 CLEAR TO 23, 77
         @ 23, 15 SAY 'Unbalanced Voucher - Please Correct '
         WAIT ''
         @ 23, 15
         @ 10, 0 CLEAR TO 23, 79
         SELECT 1
         GOTO TOP
         MREC2= RECNO()
         DO vdisp1
         SELECT 1
         GOTO TOP
         MREC2= RECNO()
         R= 8
         LOOP
      ENDIF
      SELECT 1
      GOTO MREC2
      MSRNO= 0
      DO WHILE .not. EOF()
         MSRNO= MSRNO+1
         REPLACE SR_NO WITH MSRNO
         SKIP
      ENDDO
      MANS= ' '
      MQES= ' '
      EXIT
   ENDIF
ENDDO
RETURN

PROCEDURE VDISP1
SELECT 1
USE vch-cor
GOTO MREC2
R= 9
DO WHILE .not. EOF()
   @ R, 1 SAY SR_NO PICTURE '999'
   SET COLOR TO /w
   @ R, 5 SAY C1 PICTURE '99'
   @ R, 7 SAY '-'
   @ R, 8 SAY C2 PICTURE '99'
   @ R, 10 SAY '-'
   @ R, 11 SAY C3 PICTURE '999'
   SET COLOR TO /w
   STORE C1 TO MC1
   STORE C2 TO MC2
   STORE C3 TO MC3
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   SET COLOR TO /w
   @ R, 5 SAY MPC1 PICTURE '99'
   @ R, 8 SAY MPC2 PICTURE '99'
   @ R, 11 SAY MPC3 PICTURE '999'
   SET COLOR TO 7/1
   SELECT 3
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
   SEEK MCODE
   IF FOUND()
      @ R, 15 SAY TITLE
   ENDIF
   SELECT 1
   SET COLOR TO 7/1
   @ 5, 32 SAY 'Description   '
   SET COLOR TO /w
   @ 5, 45 SAY DESCRIP
   @ R, 46 SAY DR_CR PICTURE '@!'
   COL= IIF(DR_CR='DR',50,62)
   @ R, COL SAY AMOUNT PICTURE '999999,999.99'
   SKIP
   R= R+1
   IF R>19
      SET COLOR TO 7/1
      RETURN
   ENDIF
ENDDO
SET COLOR TO 7/1
RETURN

PROCEDURE VCH-DEL
CLEAR
CLOSE ALL
SET DELETED ON
SET COLOR TO 7/1
@ 2, 20 SAY 'Re-indexing Data Files .... Please wait'
SELECT 1
USE tran INDEX tranent
SELECT 2
USE ldgrcode INDEX ac1
SELECT 1
DIMENSION MAVT( 7)
MAVT( 1)= 'CR'
MAVT( 2)= 'CP'
MAVT( 3)= 'BR'
MAVT( 4)= 'BP'
MAVT( 5)= 'JV'
MAVT( 6)= 'SV'
MAVT( 7)= 'PV'
DO WHILE .T.
   SET COLOR TO 7/1
   SELECT 1
   CLEAR
   PAGE= 1
   MVCH1= '  '
   MVCH2= '    '
   STORE 0 TO MCODE, MSRNO, MAMOUNT, AMT1, AMT2
   STORE 0 TO MVCH_TYPE, MVCH_NO1, MVCH_NO2
   @ 2, 1 TO 2, 79 DOUBLE
   @ 1, 30 SAY 'VOUCHER  DELETION '
   @ 1, 60 SAY 'PAGE #'
   @ 1, 67 SAY PAGE PICTURE '99'
   @ 21, 1 TO 21, 79 DOUBLE
   @ 23, 50 SAY 'Press Esc to Return'
   R= 3
   @ 22, 45 CLEAR TO 22, 79
   @ R, 32 SAY 'Voucher Type  ' GET MVCH_TYPE PICTURE '9'
   @ 22, 1 SAY 'Voucher Types:'
   @ 22, 17 SAY '1 - '
   @ 23, 17 SAY '2 - '
   @ 22, 58 SAY '3 - '
   @ 23, 58 SAY '4 - '
   @ 24, 17 SAY '5 - '
   @ 24, 40 SAY '6 - '
   @ 24, 58 SAY '7 - '
   @ 22, 21 SAY 'Cash Receipt - '
   @ 23, 21 SAY 'Cash Payment - '
   @ 22, 62 SAY 'Bank Receipt - '
   @ 23, 62 SAY 'Bank Payment - '
   @ 24, 21 SAY 'Journal      - '
   @ 24, 44 SAY 'Sales - '
   @ 24, 62 SAY 'Purchase     - '
   SET COLOR TO /w 
   @ 22, 36 SAY 'CR'
   @ 23, 36 SAY 'CP'
   @ 22, 77 SAY 'BR'
   @ 23, 77 SAY 'BP'
   @ 24, 36 SAY 'JV'
   @ 24, 52 SAY 'SV'
   @ 24, 77 SAY 'PV'
   SET COLOR TO 7/1
   READ
   IF MVCH_TYPE=0
      LOOP
   ENDIF
   IF MVCH_TYPE>=8
      @ R, 55 SAY '  '
      @ R, 30 SAY 'No such Voucher type exist -- Press any key'
      WAIT ''
      @ R, 30 SAY SPACE(55)
      LOOP
   ENDIF
   @ 21, 1 CLEAR TO 24, 79
   SET COLOR TO /w
   @ R, 52 SAY MAVT(MVCH_TYPE)
   SET COLOR TO 7/1 
   @ 23, 50 SAY 'Press Esc to Return'
   @ 5, 22 SAY '          '
   @ 5, 5 SAY 'Voucher No '
   SET COLOR TO /w
   @ 5, 17 SAY MAVT(MVCH_TYPE)
   @ 5, 20 SAY '  -     '
   SET COLOR TO 7/1
   @ 5, 20 GET MVCH_NO1 PICTURE '@z 99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 50 SAY SPACE(26)
   IF MVCH_NO1=0
      LOOP
   ENDIF
   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
   SET COLOR TO /w
   @ 5, 20 SAY MVCH1
   SET COLOR TO 7/1   
   @ 5, 23 GET MVCH_NO2 PICTURE '@z 99999'
   READ
   IF MVCH_NO2=0
      LOOP
   ENDIF
   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
   SET COLOR TO /w
   @ 5, 23 SAY MVCH2
   SET COLOR TO 7/1
   SELECT 1
   SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Voucher No Does Not Exists  -- Press any key'
      WAIT ''
      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore  '
      @ 13, 15 SAY 'you cannot Delete it. If you still want to Delete  '
      @ 14, 15 SAY 'it, please use UN-POST OPTION first.  '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SELECT 1
   SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   GOTO TOP
   @ 3, 5 SAY 'Date       ' GET DATE
   CLEAR GETS
   SET COLOR TO 7/1
   @ 7, 1 SAY REPLICATE('-',78)
   @ 8, 2 SAY 'SR   ACCT-NO.  T I T L E                    DR/CR     DR-AMT.       CR-AMT.  '
   @ 9, 1 TO 9, 79 DOUBLE
   L= 10
   Y= '  '
   DO WHILE .not. EOF()
      MC1= C1
      MC2= C2
      MC3= C3
      @ L, 3 SAY SR_NO PICTURE '99'
      SET COLOR TO /w
      @ L, 6 SAY '  -  -   '
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ L, 6 SAY MPC1 PICTURE '99'
      @ L, 9 SAY MPC2 PICTURE '99'
      @ L, 12 SAY MPC3 PICTURE '999'
      SET COLOR TO 7/1
      SELECT 2
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF FOUND()
         SET COLOR TO /w
         @ L, 17 SAY LEFT(TITLE,28)
         SET COLOR TO 7/1 
      ELSE
         @ L, 17 SAY ' '
      ENDIF
      SELECT 1
      @ L, 47 SAY DR_CR
      IF DR_CR='DR'
         SET COLOR TO /w
         @ L, 50 SAY AMOUNT PICTURE '999999999.99'
         SET COLOR TO 7/1
      ELSE
         SET COLOR TO /w
         @ L, 64 SAY AMOUNT PICTURE '999999999.99'
         SET COLOR TO 7/1 
      ENDIF
      IF L>=21
         ANS= ' '
         DO WHILE .not. ANS$'CQ'
            SET CONFIRM OFF
            @ 24, 57 SAY 'Continue/Quit -  C/Q ' GET ANS FUNCTION '!'
            READ
         ENDDO
         IF ANS='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 9, 1 CLEAR TO 24, 79
         L= 9
         PAGE= PAGE+1
         @ 2, 67 SAY PAGE PICTURE '99'
      ENDIF
      IF .not. EOF()
         SKIP
      ENDIF
      L= L+1
      Y= '  '
   ENDDO
   IF EOF()
      SUM FOR DR_CR='DR' AMOUNT TO AMT1
      SUM FOR DR_CR='CR' AMOUNT TO AMT2
      @ 21, 49 SAY AMT1 PICTURE '9,999,999,999.99'
      @ 21, 64 SAY AMT2 PICTURE '9,999,999,999.99'
   ENDIF
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Are You Sure - Y/N ' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 19 CLEAR TO 24, 77
   IF ANS='Y'
      SELECT 1
      DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SELECT 3
      USE tran-key INDEX key-idx
      DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      USE
   ENDIF
   SELECT 1
   SET FILTER TO
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 24 SAY 'More Data Deletion - Y/N ' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE VCH-DPRT
CLEAR
CLOSE ALL
SET DATE brit
SELECT 1
USE tran
INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TO tranent
SELECT 2
USE ldgrcode INDEX ac1
STORE 0 TO DRTOT, CRTOT, WVCH_TYPE, MTOT, MTOT1
STORE DATE() TO MDAT1, MDAT2
@ 4, 20 SAY 'Vouchers Verification Report (Date Wise) '
@ 5, 20 SAY '----------------------------------------'
@ 8, 20 SAY 'From Date         : ' GET MDAT1
@ 10, 20 SAY 'To   Date         : ' GET MDAT2
READ
@ 23, 0 CLEAR TO 23, 77
@ 14, 20 SAY 'Y O U R   P A P E R   W I D T H'
@ 16, 20 SAY '      A -  10 INCHES'
@ 17, 20 SAY '      B -  15 INCHES'
@ 18, 20 SAY '      Q -  Quit     '
OPTN= ' '
DO WHILE .not. OPTN$'ABQ'
   @ 20, 25 SAY 'Your Choice (A/B/Q)' GET OPTN FUNCTION '!'
   READ
ENDDO
IF OPTN='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
SELECT 1
SET FILTER TO DATE>=MDAT1.and.DATE<=MDAT2
GOTO TOP
CLEAR
@ 11, 20 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   MVCH_TYPE= VCH_TYPE
   DO WHILE VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      IF HEADING
         PAGE= PAGE+1
         IF OPTN='A'
            DO top-prt1
         ELSE
            DO top-prt2
         ENDIF
         IF OPTN='A'
            @ PROW(), PCOL() SAY CHR(15)
         ELSE
            @ PROW(), PCOL() SAY CHR(18)
         ENDIF
         @ 3, 18 SAY "  Daily Vouchers' Printing              Page : "
         @ 3, 65 SAY PAGE PICTURE '99'
         @ 4, 1 SAY REPLICATE('-',135)
         @ 5, 1 SAY '  DATE   VOUCHER-NO CODE--------TITLE ------------------------ DESCRIPTION ------------------  -----------DR  -----------CR  REMARKS'
         @ 6, 1 SAY REPLICATE('-',135)
         L= 7
         HEADING= .F.
      ENDIF
      T= '   '
      DO CASE
         CASE VCH_TYPE=1
            T= 'CR-'
         CASE VCH_TYPE=2
            T= 'CP-'
         CASE VCH_TYPE=3
            T= 'BR-'
         CASE VCH_TYPE=4
            T= 'BP-'
         CASE VCH_TYPE=5
            T= 'JV-'
         CASE VCH_TYPE=6
            T= 'SV-'
         CASE VCH_TYPE=7
            T= 'PV-'
      ENDCASE
      SELECT 1
      MC1= C1
      MC2= C2
      MC3= C3
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ L, 0 SAY DATE
      @ L, 10 SAY T
      @ L, 13 SAY MVCH1
      @ L, 15 SAY '-'
      @ L, 16 SAY MVCH2
      @ L, 22 SAY MPC1
      @ L, 24 SAY '-'
      @ L, 25 SAY MPC2
      @ L, 27 SAY '-'
      @ L, 28 SAY MPC3
      SELECT 2
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF FOUND()
         @ L, 32 SAY TITLE
      ELSE
         @ L, 32 SAY ' '
      ENDIF
      SELECT 1
      @ L, 62 SAY DESCRIP
      COL= IIF(DR_CR='DR',95,110)
      @ L, COL SAY AMOUNT PICTURE '999,999,999.99'
      IF COL=95
         DRTOT= DRTOT+AMOUNT
         MTOT= MTOT+AMOUNT
      ELSE
         CRTOT= CRTOT+AMOUNT
         MTOT1= MTOT1+AMOUNT
      ENDIF
      L= L+1
      @ L, 62 SAY DESCRIP1
      L= L+1
      SKIP
      IF VCH_TYPE<>MVCH_TYPE.and.VCH_NO1<>MVCH_NO1.and.VCH_NO2<>MVCH_NO2
         EXIT
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      IF L>56
         HEADING= .T.
      ENDIF
   ENDDO
   L= L+1
   IF DRTOT<>CRTOT
      @ L, 75 SAY 'Voucher Totals:'
      @ L, 94 SAY DRTOT PICTURE '99,999,999,999.99'
      @ L, 109 SAY CRTOT PICTURE '99,999,999,999.99'
      @ L, 124 SAY 'Unbalance ***'
      L= L+1
   ENDIF
   DRTOT= 0
   CRTOT= 0
   IF L>56
      EJECT
      HEADING= .T.
   ENDIF
ENDDO
@ L, 70 SAY REPLICATE('-',60)
L= L+1
@ L, 75 SAY 'Voucher Totals:'
@ L, 94 SAY MTOT PICTURE '99,999,999,999.99'
@ L, 109 SAY MTOT1 PICTURE '99,999,999,999.99'
EJECT
SET PRINTER OFF
SET DEVICE TO SCREEN
CLOSE ALL
CLEAR
RETURN

PROCEDURE VCH-TPRT
CLEAR
CLOSE ALL
SELECT 1
USE tran
INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TO tranent
SELECT 2
USE ldgrcode INDEX ac1
SET DATE brit
SET COLOR TO 7/1
CLEAR
WVCH_NO= '     '
STORE 0 TO DRTOT, CRTOT, WVCH_TYPE, MTOT, MTOT1, MVCH_NO1, MVCH_NO2
STORE DATE() TO MDAT1, MDAT2
@ 3, 20 SAY 'Vouchers Verification Report (Type Wise) '
@ 4, 20 SAY '----------------------------------------'
MVCH_TYPE= 0
SET COLOR TO 7/1
@ 21, 1 SAY 'Voucher Types:'
@ 21, 17 SAY '1 - '
@ 21, 21 SAY 'Cash Receipt - '
SET COLOR TO /w 
@ 21, 36 SAY 'CR'
SET COLOR TO 7/1
@ 22, 17 SAY '2 - '
@ 22, 21 SAY 'Cash Payment - '
SET COLOR TO /w 
@ 22, 36 SAY 'CP'
SET COLOR TO 7/1
@ 21, 58 SAY '3 - '
@ 21, 62 SAY 'Bank Receipt - '
SET COLOR TO /w 
@ 21, 77 SAY 'BR'
SET COLOR TO 7/1
@ 22, 58 SAY '4 - '
@ 22, 62 SAY 'Bank Payment - '
SET COLOR TO /w 
@ 22, 77 SAY 'BP'
SET COLOR TO 7/1
@ 23, 17 SAY '5 - '
@ 23, 21 SAY 'Journal      - '
SET COLOR TO /w 
@ 23, 36 SAY 'JV'
SET COLOR TO 7/1
@ 23, 40 SAY '6 - '
@ 23, 44 SAY 'Sales - '
SET COLOR TO /w 
@ 23, 52 SAY 'SV'
SET COLOR TO 7/1
@ 23, 58 SAY '7 - '
@ 23, 62 SAY 'Purchase     - '
SET COLOR TO /w 
@ 23, 77 SAY 'PV'
SET COLOR TO 7/1
DO WHILE .T.
   @ 6, 20 SAY 'Enter Voucher Type:' GET MVCH_TYPE PICTURE '9'
   READ
   IF MVCH_TYPE=0
      LOOP
   ENDIF
   IF MVCH_TYPE=9
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MVCH_TYPE>7
      LOOP
   ENDIF
   EXIT
ENDDO
@ 21, 1 CLEAR TO 24, 79
@ 8, 20 SAY 'From Date         : ' GET MDAT1
@ 10, 20 SAY 'To   Date         : ' GET MDAT2
READ
@ 23, 0 CLEAR TO 23, 77
@ 14, 20 SAY 'Y O U R   P A P E R   W I D T H'
@ 16, 20 SAY '      A -  10 INCHES'
@ 17, 20 SAY '      B -  15 INCHES'
@ 18, 20 SAY '      Q -  Quit     '
OPTN= ' '
DO WHILE .not. OPTN$'ABQ'
   @ 20, 25 SAY 'Your Choice (A/B/Q)' GET OPTN FUNCTION '!'
   READ
ENDDO
IF OPTN='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
SELECT 1
SET FILTER TO VCH_TYPE=MVCH_TYPE.and.DATE>=MDAT1.and.DATE<=MDAT2
GOTO TOP
CLEAR
@ 11, 20 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   DO WHILE VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      IF HEADING
         PAGE= PAGE+1
         IF OPTN='A'
            DO top-prt1
         ELSE
            DO top-prt2
         ENDIF
         IF OPTN='A'
            @ PROW(), PCOL() SAY CHR(15)
         ELSE
            @ PROW(), PCOL() SAY CHR(18)
         ENDIF
         @ 3, 18 SAY "  Daily Vouchers' Printing              Page : "
         @ 3, 65 SAY PAGE PICTURE '99'
         @ 4, 1 SAY REPLICATE('-',135)
         @ 5, 1 SAY '  DATE   VOUCHER-NO CODE--------TITLE ------------------------ DESCRIPTION ------------------  -----------DR  -----------CR  REMARKS'
         @ 6, 1 SAY REPLICATE('-',135)
         L= 7
         HEADING= .F.
      ENDIF
      T= '   '
      DO CASE
         CASE VCH_TYPE=1
            T= 'CR-'
         CASE VCH_TYPE=2
            T= 'CP-'
         CASE VCH_TYPE=3
            T= 'BR-'
         CASE VCH_TYPE=4
            T= 'BP-'
         CASE VCH_TYPE=5
            T= 'JV-'
         CASE VCH_TYPE=6
            T= 'SV-'
         CASE VCH_TYPE=7
            T= 'PV-'
      ENDCASE
      SELECT 1
      MC1= C1
      MC2= C2
      MC3= C3
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ L, 0 SAY DATE
      @ L, 10 SAY T
      @ L, 13 SAY MVCH1
      @ L, 15 SAY '-'
      @ L, 16 SAY MVCH2
      @ L, 22 SAY MPC1
      @ L, 24 SAY '-'
      @ L, 25 SAY MPC2
      @ L, 27 SAY '-'
      @ L, 28 SAY MPC3
      SELECT 2
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF FOUND()
         @ L, 32 SAY TITLE
      ELSE
         @ L, 32 SAY ' '
      ENDIF
      SELECT 1
      @ L, 62 SAY DESCRIP
      COL= IIF(DR_CR='DR',95,110)
      @ L, COL SAY AMOUNT PICTURE '999,999,999.99'
      IF COL=95
         DRTOT= DRTOT+AMOUNT
         MTOT= MTOT+AMOUNT
      ELSE
         CRTOT= CRTOT+AMOUNT
         MTOT1= MTOT1+AMOUNT
      ENDIF
      L= L+1
      @ L, 62 SAY DESCRIP1
      L= L+1
      SKIP
      IF VCH_NO1<>MVCH_NO1.and.VCH_NO2<>MVCH_NO2
         EXIT
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      IF L>56
         HEADING= .T.
      ENDIF
   ENDDO
   L= L+1
   IF DRTOT<>CRTOT
      @ L, 75 SAY 'Voucher Totals:'
      @ L, 94 SAY DRTOT PICTURE '99,999,999,999.99'
      @ L, 109 SAY CRTOT PICTURE '99,999,999,999.99'
      @ L, 124 SAY 'Unbalance ***'
      L= L+1
   ENDIF
   DRTOT= 0
   CRTOT= 0
   IF L>56
      EJECT
      HEADING= .T.
   ENDIF
ENDDO
@ L, 70 SAY REPLICATE('-',60)
L= L+1
@ L, 75 SAY 'Voucher Totals:'
@ L, 94 SAY MTOT PICTURE '99,999,999,999.99'
@ L, 109 SAY MTOT1 PICTURE '99,999,999,999.99'
EJECT
SET PRINTER OFF
SET DEVICE TO SCREEN
CLOSE ALL
CLEAR
RETURN

PROCEDURE VCH-PRT1
CLEAR
CLOSE ALL
SET DATE brit
SET CENTURY OFF
SELECT 1
USE tran
INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TO tranent
SELECT 2
USE ldgrcode INDEX ac1
STORE 0 TO DRTOT, CRTOT, MVCH_NO1, MVCH_NO2, MVCH_TYPE
T= '   '
MT= 0
DO WHILE MT=0
   @ 6, 20 SAY 'V o u c h e r   P r i n t i n g '
   @ 21, 1 TO 21, 79
   @ 22, 1 SAY 'Voucher Types:'
   @ 22, 17 SAY '1 - '
   @ 23, 17 SAY '2 - '
   @ 22, 58 SAY '3 - '
   @ 23, 58 SAY '4 - '
   @ 24, 17 SAY '5 - '
   @ 24, 40 SAY '6 - '
   @ 24, 58 SAY '7 - '
   @ 22, 21 SAY 'Cash Receipt - '
   @ 23, 21 SAY 'Cash Payment - '
   @ 22, 62 SAY 'Bank Receipt - '
   @ 23, 62 SAY 'Bank Payment - '
   @ 24, 21 SAY 'Journal      - '
   @ 24, 44 SAY 'Sales - '
   @ 24, 62 SAY 'Purchase     - '
   SET COLOR TO /w 
   @ 22, 36 SAY 'CR'
   @ 23, 36 SAY 'CP'
   @ 22, 77 SAY 'BR'
   @ 23, 77 SAY 'BP'
   @ 24, 36 SAY 'JV'
   @ 24, 52 SAY 'SV'
   @ 24, 77 SAY 'PV'
   SET COLOR TO 7/1
   @ 8, 5 SAY 'Voucher TYpe   ' GET MVCH_TYPE PICTURE '9'
   READ
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
   @ 22, 1 CLEAR TO 24, 79
   @ 23, 50 SAY 'Press Esc to Return'
   @ 10, 22 SAY '          '
   @ 10, 5 SAY 'Voucher No '
   SET COLOR TO /w
   @ 10, 20 SAY '  -     '
   SET COLOR TO 7/1
   @ 10, 20 GET MVCH_NO1 PICTURE '@z 99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 50 SAY SPACE(26)
   IF MVCH_NO1=0
      LOOP
   ENDIF
   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
   SET COLOR TO /w
   @ 10, 20 SAY MVCH1
   SET COLOR TO 7/1   
   @ 10, 23 GET MVCH_NO2 PICTURE '@z 99999'
   READ
   IF MVCH_NO2=0
      LOOP
   ENDIF
   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
   SET COLOR TO /w
   @ 10, 23 SAY MVCH2
   SET COLOR TO 7/1
   SELECT 1
   SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Voucher No Does Not Exists  -- Press any key'
      WAIT ''
      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   MT= 1
ENDDO
SELECT 1
SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'PQ'
   @ 23, 50 SAY 'Printing/Quit ... P/Q ' GET ANS PICTURE '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='P'
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
HEAD= .T.
STORE 0 TO PAGE, CNT
SELECT 1
CLEAR
L= 2
DO WHILE .not. EOF()
   IF HEAD
      IF CNT=0
         @ 0, 0 SAY CHR(18)
         DO top-prt1
      ENDIF
      @ L, 1 SAY 'Voucher No. ___________'
      @ L, 13 SAY T+MVCH1+'-'+MVCH2
      @ L, 65 SAY 'Date : ________'
      @ L, 72 SAY DATE
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY ':'
      @ L, 2 SAY ' CODE      : P A R T I C U L A R S           :     DEBIT     :     CREDIT    :'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEAD= .F.
   ENDIF
   MC1= C1
   MC2= C2
   MC3= C3
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   SELECT 2
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF FOUND()
      MTITLE= TITLE
   ELSE
      MTITLE= '  '
   ENDIF
   SELECT 1
   @ L, 1 SAY ':'
   @ L, 3 SAY MPC1 PICTURE '99'
   @ L, 5 SAY '-'
   @ L, 6 SAY MPC2 PICTURE '99'
   @ L, 8 SAY '-'
   @ L, 9 SAY MPC3 PICTURE '999'
   @ L, 13 SAY ':'
   @ L, 15 SAY MTITLE
   @ L, 47 SAY ':'
   IF DR_CR='DR'
      @ L, 50 SAY AMOUNT
      DRTOT= DRTOT+AMOUNT
      @ L, 63 SAY ':'
   ELSE
      @ L, 63 SAY ':'
      @ L, 66 SAY AMOUNT
      CRTOT= CRTOT+AMOUNT
   ENDIF
   @ L, 79 SAY ':'
   L= L+1
   @ L, 1 SAY ':'
   @ L, 13 SAY ':'
   @ L, 15 SAY DESCRIP
   @ L, 47 SAY ':'
   @ L, 63 SAY ':'
   @ L, 79 SAY ':'
   L= L+1
   @ L, 1 SAY ':'
   @ L, 13 SAY ':'
   @ L, 15 SAY DESCRIP1
   @ L, 47 SAY ':'
   @ L, 63 SAY ':'
   @ L, 79 SAY ':'
   L= L+1
   SKIP
   IF CNT=0
      IF L>=19.or.L>=50
         @ L, 1 SAY REPLICATE('-',79)
         L= L+2
         @ L, 55 SAY 'Continued Next Page '
         CNT= 1
         HEAD= .T.
         L= L+11
      ENDIF
   ENDIF
ENDDO
DO WHILE L<19
   @ L, 1 SAY ':'
   @ L, 13 SAY ':'
   @ L, 47 SAY ':'
   @ L, 63 SAY ':'
   @ L, 79 SAY ':'
   L= L+1
   @ L, 1 SAY ':'
   @ L, 13 SAY ':'
   @ L, 47 SAY ':'
   @ L, 63 SAY ':'
   @ L, 79 SAY ':'
   L= L+1
ENDDO
IF L>=19.or.L>=50
   @ L, 1 SAY REPLICATE('-',79)
   L= L+1
   @ L, 1 SAY ':'
   @ L, 35 SAY 'Total Rs: '
   @ L, 47 SAY ':'
   @ L, 63 SAY ':'
   @ L, 48 SAY DRTOT PICTURE '99999999999.99'
   @ L, 64 SAY CRTOT PICTURE '99999999999.99'
   @ L, 79 SAY ':'
   L= L+1
   @ L, 1 SAY REPLICATE('-',79)
   L= L+1
   L= L+2
   @ L, 10 SAY 'Prepared By'
   @ L, 35 SAY 'Accountant '
   @ L, 64 SAY 'Director '
   L= L+2
   EJECT
ENDIF
IF ANS='P'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
RETURN

PROCEDURE VCH-PRT2
CLEAR
CLOSE ALL
SET DATE brit
SET CENTURY OFF
CLEAR
CLOSE ALL
SET DATE brit
SET CENTURY OFF
SELECT 1
USE tran
INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TO tranent
SELECT 2
USE ldgrcode INDEX ac1
SELECT 1
STORE 0 TO DRTOT, CRTOT, MVCH_NO1, MVCH_NO2, MVCH_TYPE
T= '   '
MT= 0
DO WHILE MT=0
   @ 6, 20 SAY 'V o u c h e r   P r i n t i n g '
   @ 21, 1 TO 21, 79
   @ 22, 1 SAY 'Voucher Types:'
   @ 22, 17 SAY '1 - '
   @ 23, 17 SAY '2 - '
   @ 22, 58 SAY '3 - '
   @ 23, 58 SAY '4 - '
   @ 24, 17 SAY '5 - '
   @ 24, 40 SAY '6 - '
   @ 24, 58 SAY '7 - '
   @ 22, 21 SAY 'Cash Receipt - '
   @ 23, 21 SAY 'Cash Payment - '
   @ 22, 62 SAY 'Bank Receipt - '
   @ 23, 62 SAY 'Bank Payment - '
   @ 24, 21 SAY 'Journal      - '
   @ 24, 44 SAY 'Sales - '
   @ 24, 62 SAY 'Purchase     - '
   SET COLOR TO /w 
   @ 22, 36 SAY 'CR'
   @ 23, 36 SAY 'CP'
   @ 22, 77 SAY 'BR'
   @ 23, 77 SAY 'BP'
   @ 24, 36 SAY 'JV'
   @ 24, 52 SAY 'SV'
   @ 24, 77 SAY 'PV'
   SET COLOR TO 7/1
   @ 8, 5 SAY 'Voucher TYpe   ' GET MVCH_TYPE PICTURE '9'
   READ
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
   @ 22, 1 CLEAR TO 24, 79
   @ 23, 50 SAY 'Press Esc to Return'
   @ 10, 22 SAY '          '
   @ 10, 5 SAY 'Voucher No '
   SET COLOR TO /w
   @ 10, 20 SAY '  -     '
   SET COLOR TO 7/1
   @ 10, 20 GET MVCH_NO1 PICTURE '@z 99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 50 SAY SPACE(26)
   IF MVCH_NO1=0
      LOOP
   ENDIF
   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
   SET COLOR TO /w
   @ 10, 20 SAY MVCH1
   SET COLOR TO 7/1   
   @ 10, 23 GET MVCH_NO2 PICTURE '@z 99999'
   READ
   IF MVCH_NO2=0
      LOOP
   ENDIF
   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
   SET COLOR TO /w
   @ 10, 23 SAY MVCH2
   SET COLOR TO 7/1
   SELECT 1
   SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Voucher No Does Not Exists  -- Press any key'
      WAIT ''
      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   MT= 1
ENDDO
SELECT 1
SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'PQ'
   @ 23, 50 SAY 'Printing/Quit ... P/Q ' GET ANS PICTURE '!'
   READ
ENDDO
IF ANS='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF ANS='P'
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
HEAD= .T.
STORE 0 TO PAGE, CNT
SELECT 1
CLEAR
L= 2
DO WHILE .not. EOF()
   IF HEAD
      L= 2
      @ 0, 0 SAY CHR(18)
      DO top-prt1
      @ L, 1 SAY 'Voucher No. ___________'
      @ L, 13 SAY T+MVCH1+'-'+MVCH2
      @ L, 65 SAY 'Date : ________'
      @ L, 72 SAY DATE
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 1 SAY ':'
      @ L, 2 SAY ' CODE      : P A R T I C U L A R S           :     DEBIT     :     CREDIT    :'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      HEAD= .F.
   ENDIF
   MC1= C1
   MC2= C2
   MC3= C3
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   SELECT 2
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF FOUND()
      MTITLE= TITLE
   ELSE
      MTITLE= '  '
   ENDIF
   SELECT 1
   @ L, 1 SAY ':'
   @ L, 3 SAY MPC1 PICTURE '99'
   @ L, 5 SAY '-'
   @ L, 6 SAY MPC2 PICTURE '99'
   @ L, 8 SAY '-'
   @ L, 9 SAY MPC3 PICTURE '999'
   @ L, 13 SAY ':'
   @ L, 15 SAY MTITLE
   @ L, 47 SAY ':'
   IF DR_CR='DR'
      @ L, 50 SAY AMOUNT
      DRTOT= DRTOT+AMOUNT
      @ L, 63 SAY ':'
   ELSE
      @ L, 63 SAY ':'
      @ L, 66 SAY AMOUNT
      CRTOT= CRTOT+AMOUNT
   ENDIF
   @ L, 79 SAY ':'
   L= L+1
   @ L, 1 SAY ':'
   @ L, 13 SAY ':'
   @ L, 15 SAY DESCRIP
   @ L, 47 SAY ':'
   @ L, 63 SAY ':'
   @ L, 79 SAY ':'
   L= L+1
   @ L, 1 SAY ':'
   @ L, 13 SAY ':'
   @ L, 15 SAY DESCRIP1
   @ L, 47 SAY ':'
   @ L, 63 SAY ':'
   @ L, 79 SAY ':'
   @ L, 1 SAY REPLICATE('_',79)
   L= L+1
   SKIP
   IF CNT=0
      IF L>=54
         @ L, 1 SAY REPLICATE('-',79)
         L= L+2
         @ L, 55 SAY 'Continued Next Page '
         CNT= 1
         HEAD= .T.
      ENDIF
   ENDIF
ENDDO
DO WHILE L<48
   @ L, 1 SAY ':'
   @ L, 13 SAY ':'
   @ L, 47 SAY ':'
   @ L, 63 SAY ':'
   @ L, 79 SAY ':'
   L= L+1
   @ L, 1 SAY ':'
   @ L, 13 SAY ':'
   @ L, 47 SAY ':'
   @ L, 63 SAY ':'
   @ L, 79 SAY ':'
   L= L+1
ENDDO
IF L>=48
   @ L, 1 SAY REPLICATE('-',79)
   L= L+1
   @ L, 1 SAY ':'
   @ L, 35 SAY 'Total Rs: '
   @ L, 47 SAY ':'
   @ L, 63 SAY ':'
   @ L, 48 SAY DRTOT PICTURE '99999999999.99'
   @ L, 64 SAY CRTOT PICTURE '99999999999.99'
   @ L, 79 SAY ':'
   L= L+1
   @ L, 1 SAY REPLICATE('-',79)
   L= L+1
   @ L, 1 SAY 'Received the sum of Rupees '
   @ L, 29 SAY REPLICATE('_',51)
   L= L+1
   @ L, 1 SAY '______________________________________________ Signature ______________________'
   L= L+2
   @ L, 1 SAY REPLICATE('=',79)
   L= L+2
   @ L, 10 SAY 'Prepared By'
   @ L, 35 SAY 'Accountant '
   @ L, 64 SAY 'Director '
   L= L+2
   EJECT
ENDIF
IF ANS='P'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
RETURN

PROCEDURE BACH-BAL
DO WHILE .T.
   SELECT 1
   SKIP  -MSNO+1
   L= L-MSNO+1
   PSNO= MSNO
   MSNO= 1
   MCREDIT= 0
   MDEBIT= 0
   TDR= 0
   TCR= 0
   DO WHILE .T.
      DO WHILE .T.
         @ L, 3 SAY MSNO PICTURE '9'
         SET COLOR TO /W
         @ L, 5 SAY '  -  -   '
         @ L, 5 GET C1 PICTURE '@Z 99'
         READ
         MC1= C1
         IF C1=0
            LOOP
         ELSE
            MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
            @ L, 5 SAY MPC1
            EXIT
         ENDIF
      ENDDO
      DO WHILE .T.
         @ L, 8 GET C2 PICTURE '@Z 99'
         READ
         MC2= C2
         IF C2=0
            LOOP
         ELSE
            MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
            @ L, 8 SAY MPC2
            EXIT
         ENDIF
      ENDDO
      DO WHILE .T.
         @ L, 11 GET C3 PICTURE '@Z 999'
         READ
         MC3= C3
         IF C3=0
            LOOP
         ENDIF
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ L, 11 SAY MPC3
         EXIT
      ENDDO
      SET COLOR TO 
      SELECT 2
      LOCATE FOR  B->C1= A->C1.and. B->C2= A->C2.and. B->C3= A->C3
      IF .not. FOUND()
         @ 23, 8 SAY 'This Code Does not Exist  --Press any key'
         WAIT ''
         @ 23, 8 CLEAR TO 23, 63
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ L, 16 SAY  B->TITLE
      SET COLOR TO 7/1
      SELECT 1
      @ 9, 40 SAY 'DESCR'
      @ 9, 47 GET DESCRIP
      READ
      @ L, 47 SAY DR_CR
      Y= DR_CR
      DO WHILE .T.
         SET CONFIRM ON
         @ L, 47 GET Y FUNCTION '!'
         READ
         IF Y='CR'.or.Y='DR'
            EXIT
         ELSE
            LOOP
         ENDIF
      ENDDO
      REPLACE DR_CR WITH Y
      IF Y='DR'
         DO WHILE .T.
            SET COLOR TO /W
            @ L, 64 SAY MCREDIT PICTURE '999999999.99'
            SET COLOR TO 
            @ L, 50 GET AMOUNT PICTURE '999999999.99'
            READ
            IF AMOUNT=0
               LOOP
            ENDIF
            TDR= TDR+AMOUNT
            EXIT
         ENDDO
      ENDIF
      IF Y='CR'
         DO WHILE .T.
            SET COLOR TO /W
            @ L, 50 SAY MDEBIT PICTURE '999999999.99'
            SET COLOR TO 
            @ L, 64 GET AMOUNT PICTURE '999999999.99'
            READ
            IF AMOUNT=0
               LOOP
            ENDIF
            TCR= TCR+AMOUNT
            EXIT
         ENDDO
      ENDIF
      X= ' '
      DO WHILE .not. X$'YN'
         @ L, 77 GET X FUNCTION '!'
         READ
      ENDDO
      IF X='N'
         IF Y='DR'
            TDR= TDR-AMOUNT
         ELSE
            TCR= TCR-AMOUNT
         ENDIF
         LOOP
      ENDIF
      IF MSNO=PSNO
         IF TDR<>TCR
            @ 23, 1 SAY ' VOUCHER IS UN-BALANCED PL. CORRECT'
            @ 23, 42 SAY 'TOTALS:'
            @ 23, 50 SAY TDR PICTURE '999999999.99'
            @ 23, 64 SAY TCR PICTURE '999999999.99'
            WAIT ' '
            @ 23, 1
            IND= 1
            EXIT
         ELSE
            IND= 2
            EXIT
         ENDIF
      ENDIF
      SELECT 1
      SKIP
      L= L+1
      MSNO= MSNO+1
      MCREDIT= 0
      MDEBIT= 0
      Y= '  '
      STORE SPACE(30) TO MDESCRIP
      LOOP
   ENDDO
   IF IND=2
      RETURN
   ELSE
      LOOP
   ENDIF
ENDDO
RETURN

PROCEDURE VCH-TVEW
CLEAR
CLOSE ALL
SELECT 1
USE tran
INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TO tranent
SELECT 2
USE ldgrcode INDEX ac1
SELECT 1
STORE DATE() TO MDATE
STORE 0 TO MVCH_NO1, MVCH_NO2
STORE 0 TO MC1, MC2, MC3
STORE 0 TO MCODE, MUPDATE, MSTATUS, MSNO, MVCH_TYPE
STORE SPACE(2) TO T, MVCH1
MVCH2= '     '
PAGE= 1
Y= '  '
DO WHILE .T.
   @ 2, 1 TO 2, 79 DOUBLE
   @ 1, 30 SAY 'VOUCHER  DISPLAY '
   @ 1, 60 SAY 'PAGE #'
   @ 1, 67 SAY PAGE PICTURE '999'
   @ 21, 1 TO 21, 79 DOUBLE
   @ 22, 17 SAY '1 - '
   @ 23, 17 SAY '2 - '
   @ 22, 58 SAY '3 - '
   @ 23, 58 SAY '4 - '
   @ 24, 17 SAY '5 - '
   @ 24, 40 SAY '6 - '
   @ 24, 58 SAY '7 - '
   @ 22, 21 SAY 'Cash Receipt - '
   @ 23, 21 SAY 'Cash Payment - '
   @ 22, 62 SAY 'Bank Receipt - '
   @ 23, 62 SAY 'Bank Payment - '
   @ 24, 21 SAY 'Journal      - '
   @ 24, 44 SAY 'Sales - '
   @ 24, 62 SAY 'Purchase     - '
   SET COLOR TO /w 
   @ 22, 36 SAY 'CR'
   @ 23, 36 SAY 'CP'
   @ 22, 77 SAY 'BR'
   @ 23, 77 SAY 'BP'
   @ 24, 36 SAY 'JV'
   @ 24, 52 SAY 'SV'
   @ 24, 77 SAY 'PV'
   SET COLOR TO 7/1
   @ 3, 40 SAY 'Voucher Type  ' GET MVCH_TYPE PICTURE '9'
   READ
   IF READKEY()=12
      CLEAR
      CLEAR ALL
      CLOSE ALL
      RETURN
   ENDIF
   @ 21, 1 CLEAR TO 24, 79
   IF MVCH_TYPE=0
      LOOP
   ENDIF
   IF MVCH_TYPE=9
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   IF MVCH_TYPE>=8
      @ 23, 20 SAY 'No such Voucher type exist --Press anykey'
      WAIT ''
      @ 23, 19 CLEAR TO 23, 77
      LOOP
   ENDIF
   SET COLOR TO /w
   SELECT 1
   SET FILTER TO VCH_TYPE=MVCH_TYPE
   GOTO TOP
   IF MVCH_TYPE=1
      @ 3, 58 SAY 'CR'
      T= 'CR-'
   ENDIF
   IF MVCH_TYPE=2
      @ 3, 58 SAY 'CP'
      T= 'CP-'
   ENDIF
   IF MVCH_TYPE=3
      @ 3, 58 SAY 'BR'
      T= 'BR-'
   ENDIF
   IF MVCH_TYPE=4
      @ 3, 58 SAY 'BP'
      T= 'BP-'
   ENDIF
   IF MVCH_TYPE=5
      @ 3, 58 SAY 'JV'
      T= 'JV-'
   ENDIF
   IF MVCH_TYPE=6
      @ 3, 58 SAY 'SV'
      T= 'SV-'
   ENDIF
   IF MVCH_TYPE=7
      @ 3, 58 SAY 'PV'
      T= 'PV-'
   ENDIF
   SET COLOR TO 7/1 
   ABC= 0
   DO WHILE ABC=0
      @ 23, 50 SAY 'Press Esc to Return'
      @ 5, 5 SAY 'Voucher No '
      SET COLOR TO /w
      @ 5, 17 SAY T
      @ 5, 20 SAY '  -     '
      SET COLOR TO 7/1
      @ 5, 20 GET MVCH_NO1 PICTURE '@z 99'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 23, 50 SAY SPACE(26)
      IF MVCH_NO1=0
         LOOP
      ENDIF
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
      SET COLOR TO /w
      @ 5, 20 SAY MVCH1
      SET COLOR TO 7/1   
      @ 5, 23 GET MVCH_NO2 PICTURE '@z 99999'
      READ
      IF MVCH_NO2=0
         LOOP
      ENDIF
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
      SET COLOR TO /w
      @ 5, 23 SAY MVCH2
      SET COLOR TO 7/1
      SELECT 1
      SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 1 SAY SPACE(78)
         @ 23, 15 SAY 'This Voucher No Does Not Exists  -- Press any key'
         WAIT ''
         @ 23, 10 SAY SPACE(75)
         LOOP
      ENDIF
      ABC= 1
   ENDDO
   EXIT
ENDDO
MSNO= 1
PAGE= 0
HEAD= .T.
DO WHILE .not. EOF()
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   DO WHILE VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      IF HEAD
         CLEAR
         IF FLAG='Y'
            SET COLOR TO */w
            @ 3, 5 SAY 'Posted Voucher '
            SET COLOR TO 7/1
         ELSE
            SET COLOR TO */w
            @ 3, 5 SAY 'Not Yet Posted '
            SET COLOR TO 7/1
         ENDIF
         PAGE= PAGE+1
         @ 2, 1 TO 2, 79 DOUBLE
         @ 1, 30 SAY 'VOUCHER  DISPLAY '
         @ 1, 60 SAY 'PAGE #'
         @ 1, 67 SAY PAGE PICTURE '999'
         SELECT 1
         @ 5, 40 SAY 'Date:          '
         SET COLOR TO /w
         @ 5, 50 SAY DATE
         SET COLOR TO 7/1 
         @ 3, 40 SAY 'Voucher Type  '
         MSNO= 1
         SET COLOR TO /w 
         @ 3, 54 SAY MVCH_TYPE PICTURE '9'
         IF MVCH_TYPE=1
            @ 3, 58 SAY 'CR'
            T= 'CR-'
         ENDIF
         IF MVCH_TYPE=2
            @ 3, 58 SAY 'CP'
            T= 'CP-'
         ENDIF
         IF MVCH_TYPE=3
            @ 3, 58 SAY 'BR'
            T= 'BR-'
         ENDIF
         IF MVCH_TYPE=4
            @ 3, 58 SAY 'BP'
            T= 'BP-'
         ENDIF
         IF MVCH_TYPE=5
            @ 3, 58 SAY 'JV'
            T= 'JV-'
         ENDIF
         IF MVCH_TYPE=6
            @ 3, 58 SAY 'SV'
            T= 'SV-'
         ENDIF
         IF MVCH_TYPE=7
            @ 3, 58 SAY 'PV'
            T= 'PV-'
         ENDIF
         MVCH_NO1= VCH_NO1
         MVCH_NO2= VCH_NO2
         MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
         MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
         SET COLOR TO 7/1
         @ 5, 5 SAY 'Voucher No '
         SET COLOR TO /w
         @ 5, 17 SAY T
         @ 5, 20 SAY '  -     '
         @ 5, 20 SAY MVCH1
         @ 5, 23 SAY MVCH2
         SET COLOR TO 7/1
         @ 6, 1 SAY REPLICATE('-',78)
         @ 7, 2 SAY 'SR   ACCT-NO.  T I T L E                    DR/CR     DR-AMT.       CR-AMT.  '
         L= 8
         HEAD= .F.
      ENDIF
      MC1= C1
      MC2= C2
      MC3= C3
      SET COLOR TO /w
      @ L, 3 SAY SR_NO PICTURE '99'
      @ L, 6 SAY '  -  -   '
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 6 SAY MPC1
      @ L, 9 SAY MPC2
      @ L, 12 SAY MPC3
      SET COLOR TO 7/1
      SELECT 2
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF FOUND()
         @ L, 17 SAY TITLE
      ELSE
         @ L, 17 SAY ' '
      ENDIF
      SELECT 1
      SET COLOR TO 7/1
      SET COLOR TO /W
      @ L, 47 SAY DR_CR
      IF DR_CR='DR'
         @ L, 50 SAY AMOUNT PICTURE '999,999,999.99'
         SET COLOR TO 7/1
      ELSE
         @ L, 64 SAY AMOUNT PICTURE '999,999,999.99'
         SET COLOR TO 7/1
      ENDIF
      SET COLOR TO 7/1
      IF L>20
         IF .not. EOF()
            SKIP
         ENDIF
         EXIT
      ENDIF
      IF VCH_NO1<>MVCH_NO1.and.VCH_NO2<>MVCH_NO2
         EXIT
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
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
      HEAD= .T.
      CNT= 0
      DO WHILE .T.
         SELECT 1
         IF BOF()
            RETURN
         ENDIF
         SKIP  -1
         IF SR_NO=1
            CNT= CNT+1
            IF CNT=2
               EXIT
            ELSE
               LOOP
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   IF READKEY()=5
      HEAD= .T.
   ENDIF
ENDDO
CLEAR
CLOSE ALL
RETURN

PROCEDURE VCH-DVEW
CLEAR
CLOSE ALL
SELECT 1
USE tran
INDEX ON DATE TO trandate
SELECT 2
USE ldgrcode INDEX ac1
DO WHILE .T.
   CLEAR
   @ 6, 20 SAY 'VOUCHER DISPLAY DATE-WISE'
   @ 21, 50 SAY 'Press Ctrl Q to Return'
   STORE DATE() TO MDATE, LDATE
   @ 10, 20 SAY 'Starting Date: ' GET MDATE
   @ 12, 20 SAY 'Ending   Date: ' GET LDATE
   READ
   IF READKEY()=12
      CLEAR
      CLEAR ALL
      CLOSE ALL
      RETURN
   ENDIF
   SELECT 1
   LOCATE FOR DATE>=MDATE.and.DATE<=LDATE
   IF .not. FOUND()
      @ 23, 10 SAY 'No Voucher is Present For this Date ...'
      WAIT ''
      @ 23, 10
      STORE DATE() TO MDATE
      LOOP
   ENDIF
   CLEAR
   EXIT
ENDDO
SELECT 1
GOTO TOP
SET FILTER TO DATE>=MDATE.and.DATE<=LDATE
GOTO TOP
MSNO= 1
HEAD= .T.
PAGE= 0
DO WHILE .not. EOF()
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   MVCH_TYPE= VCH_TYPE
   DO WHILE VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      IF HEAD
         CLEAR
         IF FLAG='Y'
            SET COLOR TO */w
            @ 3, 5 SAY 'Posted Voucher '
            SET COLOR TO 7/1
         ELSE
            SET COLOR TO */w
            @ 3, 5 SAY 'Not Yet Posted '
            SET COLOR TO 7/1
         ENDIF
         PAGE= PAGE+1
         @ 2, 1 TO 2, 79 DOUBLE
         @ 1, 30 SAY 'VOUCHER  DISPLAY '
         @ 1, 60 SAY 'PAGE #'
         @ 1, 67 SAY PAGE PICTURE '999'
         SELECT 1
         @ 5, 40 SAY 'Date:          '
         SET COLOR TO /w
         @ 5, 50 SAY DATE
         SET COLOR TO 7/1 
         @ 3, 40 SAY 'Voucher Type  '
         MSNO= 1
         SET COLOR TO /w 
         @ 3, 54 SAY MVCH_TYPE PICTURE '9'
         IF MVCH_TYPE=1
            @ 3, 58 SAY 'CR'
            T= 'CR-'
         ENDIF
         IF MVCH_TYPE=2
            @ 3, 58 SAY 'CP'
            T= 'CP-'
         ENDIF
         IF MVCH_TYPE=3
            @ 3, 58 SAY 'BR'
            T= 'BR-'
         ENDIF
         IF MVCH_TYPE=4
            @ 3, 58 SAY 'BP'
            T= 'BP-'
         ENDIF
         IF MVCH_TYPE=5
            @ 3, 58 SAY 'JV'
            T= 'JV-'
         ENDIF
         IF MVCH_TYPE=6
            @ 3, 58 SAY 'SV'
            T= 'SV-'
         ENDIF
         IF MVCH_TYPE=7
            @ 3, 58 SAY 'PV'
            T= 'PV-'
         ENDIF
         MVCH_NO1= VCH_NO1
         MVCH_NO2= VCH_NO2
         MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
         MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
         SET COLOR TO 7/1
         @ 5, 5 SAY 'Voucher No '
         SET COLOR TO /w
         @ 5, 17 SAY T
         @ 5, 20 SAY '  -     '
         @ 5, 20 SAY MVCH1
         @ 5, 23 SAY MVCH2
         SET COLOR TO 7/1
         @ 6, 1 SAY REPLICATE('-',78)
         @ 7, 2 SAY 'SR   ACCT-NO.  T I T L E                    DR/CR     DR-AMT.       CR-AMT.  '
         L= 8
         HEAD= .F.
      ENDIF
      MC1= C1
      MC2= C2
      MC3= C3
      SET COLOR TO /w
      @ L, 3 SAY SR_NO PICTURE '99'
      @ L, 6 SAY '  -  -   '
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 6 SAY MPC1
      @ L, 9 SAY MPC2
      @ L, 12 SAY MPC3
      SET COLOR TO 7/1
      SELECT 2
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF FOUND()
         @ L, 17 SAY TITLE
      ELSE
         @ L, 17 SAY ' '
      ENDIF
      SELECT 1
      SET COLOR TO 7/1
      SET COLOR TO /W
      @ L, 47 SAY DR_CR
      IF DR_CR='DR'
         @ L, 50 SAY AMOUNT PICTURE '999,999,999.99'
         SET COLOR TO 7/1
      ELSE
         @ L, 64 SAY AMOUNT PICTURE '999,999,999.99'
         SET COLOR TO 7/1
      ENDIF
      SET COLOR TO 7/1
      IF L>20
         IF .not. EOF()
            SKIP
         ENDIF
         EXIT
      ENDIF
      IF VCH_TYPE<>MVCH_TYPE.and.VCH_NO1<>MVCH_NO1.and.VCH_NO2<>MVCH_NO2
         EXIT
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      L= L+1
      IF .not. EOF()
         SKIP
      ENDIF
   ENDDO
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
      HEAD= .T.
      CNT= 0
      DO WHILE .T.
         SELECT 1
         IF BOF()
            RETURN
         ENDIF
         SKIP  -1
         IF SR_NO=1
            CNT= CNT+1
            IF CNT=2
               EXIT
            ELSE
               LOOP
            ENDIF
         ENDIF
      ENDDO
   ENDIF
   IF READKEY()=5
      HEAD= .T.
   ENDIF
ENDDO
CLEAR
CLOSE ALL
RETURN

PROCEDURE VEW-BACK
DO WHILE .T.
   SELECT 1
   MC1= C1
   MC2= C2
   MC3= C3
   @ 6, 1 SAY REPLICATE('-',78)
   @ 7, 2 SAY 'SR   ACCT-NO.  T I T L E                    DR/CR     DR-AMT.       CR-AMT.  '
   SET COLOR TO /w
   @ L, 3 SAY SR_NO PICTURE '99'
   @ L, 6 SAY '  -  -   '
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
   @ L, 6 SAY MPC1
   @ L, 9 SAY MPC2
   @ L, 12 SAY MPC3
   SET COLOR TO 7/1
   SELECT 2
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF FOUND()
      @ L, 17 SAY TITLE
   ELSE
      @ L, 17 SAY ' '
   ENDIF
   SELECT 1
   SET COLOR TO /w
   @ L, 47 SAY DR_CR
   IF DR_CR='DR'
      @ L, 50 SAY AMOUNT PICTURE '999,999,999.99'
      SET COLOR TO 
   ELSE
      @ L, 64 SAY AMOUNT PICTURE '999,999,999.99'
      SET COLOR TO 
   ENDIF
   SET COLOR TO 7/1
   @ 22, 1 SAY VCH_NO1
   @ 22, 45 SAY VCH_NO2
   @ 23, 1 SAY MVCH_NO1
   @ 23, 45 SAY MVCH_NO2
   SKIP
   IF VCH_NO1<>MVCH_NO1.and.VCH_NO2<>MVCH_NO2
      @ 23, 1 SAY VCH_NO1
      @ 23, 45 SAY VCH_NO2
      WAIT
      RETURN
   ENDIF
   IF EOF()
      RETURN
   ENDIF
   L= L+1
   IF L>21
      L= 9
      @ 24, 30 SAY 'Press Any Key To Continue '
      WAIT ''
      @ 12, 2 CLEAR TO 23, 77
      @ 24, 30 SAY SPACE(50)
   ENDIF
ENDDO
CLEAR
RETURN

PROCEDURE VCH-BOOK
CLEAR
CLOSE ALL
SET DATE brit
SET CENTURY OFF
SELECT 1
USE master
SELECT 2
USE ldgrcode INDEX ac1
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   STORE 0 TO MC1, MC2, MC3, TDR, TCR, MBL_AMOUNT
   STORE ' ' TO MSTATUS
   PG_HDNG= .T.
   PAGE= 1
   LINE= 8
   WTITLE= SPACE(30)
   GOTO TOP
   DO WHILE .T.
      MCNT= 0
      STORE DATE() TO SDATE, LDATE
      @ 4, 25 SAY 'Day Book Printing '
      @ 23, 1 SAY 'Press Esc To Return '
      @ 10, 60 PROMPT 'Cash Book '
      @ 11, 60 PROMPT 'Bank Book '
      @ 12, 60 PROMPT 'Day Book '
      OPT= 0
      MENU TO OPT
      DO CASE
         CASE OPT=1
            MBOOK= 'Cash Book '
         CASE OPT=2
            MBOOK= 'Bank Book '
         CASE OPT=3
            MBOOK= 'Day Book '
      ENDCASE
      @ 10, 10 SAY 'Enter  Code  for  Display:     -  -'
      @ 10, 40 GET MC1 PICTURE '99'
      @ 10, 43 GET MC2 PICTURE '99'
      @ 10, 46 GET MC3 PICTURE '999'
      READ
      IF READKEY()=12
         CLOSE ALL
         CLEAR
         RETURN
      ENDIF
      @ 23, 1
      SELECT 2
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF .not. FOUND()
         @ 22, 10 SAY 'code is not in code file'
         WAIT ''
         @ 22, 10 SAY '                           '
         LOOP
      ENDIF
      WTITLE=  B->TITLE
      @ 12, 10 SAY 'TITLE:'
      SET COLOR TO /w
      @ 12, 19 SAY WTITLE
      MREM= REMARKS
      SET COLOR TO 7/1
      @ 14, 10 SAY 'From Date ' GET SDATE
      @ 16, 10 SAY 'To.. Date ' GET LDATE
      READ
      EXIT
   ENDDO
   SELECT 1
   LINE= 8
   SET FILTER TO DATE>=SDATE.and.DATE<=LDATE.and.C1=MC1.and.C2=MC2.and.C3=MC3
   COUNT TO RCON
   IF RCON=0
      CLEAR
      @ 10, 10 SAY 'There is No Transaction Between These Dates'
      @ 24, 25 SAY 'Press Any Key To Return'
      WAIT ''
      @ 10, 10 SAY SPACE(60)
      CLEAR ALL
      EXIT
   ENDIF
   P= ' '
   DO WHILE .not. P$'PSQ'
      @ 23, 50 SAY 'Printer/Screen/Quit - P/S/Q' GET P FUNCTION '!'
      READ
   ENDDO
   CLEAR
   IF P='Q'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF P='P'
      CLEAR
      @ 11, 20 SAY 'Pleas Wait -  Prepairing Sub-Ledger '
      SET PRINTER ON
      SET DEVICE TO PRINTER
   ENDIF
   CNT= 0
   RMK= '             '
   CLEAR
   SELECT 1
   GOTO TOP
   WC1= MC1
   WC2= MC2
   WC3= MC3
   DO WHILE .not. EOF()
      IF PG_HDNG
         IF P='P'
            DO top-prt1
         ENDIF
         @ 2, 20 SAY MBOOK
         @ 2, 30 SAY 'From : '
         @ 2, 37 SAY SDATE
         @ 2, 47 SAY 'To'
         @ 2, 51 SAY LDATE
         @ 2, 70 SAY 'page: '
         @ 2, 76 SAY PAGE PICTURE '99'
         @ 3, 1 SAY REPLICATE('-',79)
         @ 4, 1 SAY 'ACCT.CODE:'
         WPC1= REPLICATE('0',2-LEN(LTRIM(STR(WC1))))+LTRIM(STR(WC1))
         @ 4, 12 SAY WPC1
         @ 4, 14 SAY '-'
         WPC2= REPLICATE('0',2-LEN(LTRIM(STR(WC2))))+LTRIM(STR(WC2))
         @ 4, 15 SAY WPC2
         @ 4, 17 SAY '-'
         WPC3= REPLICATE('0',3-LEN(LTRIM(STR(WC3))))+LTRIM(STR(WC3))
         @ 4, 18 SAY WPC3
         @ 4, 24 SAY 'TITLE OF ACCOUNT:- '
         @ 4, 46 SAY WTITLE PICTURE 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
         @ 5, 1 SAY REPLICATE('-',79)
         @ 6, 1 SAY 'Date'
         @ 6, 10 SAY 'Voucher #   '
         @ 6, 22 SAY 'Description '
         @ 6, 55 SAY ' Receipts    '
         @ 6, 67 SAY '  Payments   '
         @ 7, 1 SAY REPLICATE('-',79)
         PG_HDNG= .F.
      ENDIF
      IF BL_AMOUNT=0.and.SR_NO<>1.and.DR_AMOUNT=0.and.CR_AMOUNT=0
         SKIP
         LOOP
      ENDIF
      STORE 0 TO TDR, TCR, MBL_AMOUNT
      MDATE= DATE
      SELECT 1
      DO WHILE DATE=MDATE
         IF PG_HDNG
            IF P='P'
               DO top-prt1
            ENDIF
            @ 2, 20 SAY MBOOK
            @ 2, 30 SAY 'From : '
            @ 2, 37 SAY SDATE
            @ 2, 47 SAY 'To'
            @ 2, 51 SAY LDATE
            @ 2, 70 SAY 'page: '
            @ 2, 76 SAY PAGE PICTURE '99'
            @ 3, 1 SAY REPLICATE('-',79)
            @ 4, 1 SAY 'ACCT.CODE:'
            WPC1= REPLICATE('0',2-LEN(LTRIM(STR(WC1))))+LTRIM(STR(WC1))
            @ 4, 12 SAY WPC1
            @ 4, 14 SAY '-'
            WPC2= REPLICATE('0',2-LEN(LTRIM(STR(WC2))))+LTRIM(STR(WC2))
            @ 4, 15 SAY WPC2
            @ 4, 17 SAY '-'
            WPC3= REPLICATE('0',3-LEN(LTRIM(STR(WC3))))+LTRIM(STR(WC3))
            @ 4, 18 SAY WPC3
            @ 4, 24 SAY 'TITLE OF ACCOUNT:- '
            @ 4, 46 SAY WTITLE PICTURE 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
            @ 5, 1 SAY REPLICATE('-',79)
            @ 6, 1 SAY 'Date'
            @ 6, 10 SAY 'Voucher #   '
            @ 6, 22 SAY 'Description '
            @ 6, 55 SAY ' Receipts    '
            @ 6, 67 SAY '  Payments   '
            @ 7, 1 SAY REPLICATE('-',79)
            PG_HDNG= .F.
         ENDIF
         IF DATE<>MDATE
            EXIT
         ENDIF
         IF EOF()
            EXIT
         ENDIF
         IF C3=0
            SKIP
            LOOP
         ENDIF
         IF SR_NO=1
            @ LINE, 1 SAY DATE
            WDESC= LTRIM(DESC)
            @ LINE, 10 SAY WDESC PICTURE 'XXXXXXXXXXXXXXXXXXX'
            @ LINE, 48 SAY BL_AMOUNT PICTURE '99,999,999,999.99'
            RMK= 'Opn.Bal'
            @ LINE, 72 SAY RMK
            MBL_AMOUNT= BL_AMOUNT
            MOPEN= BL_AMOUNT
            MSTATUS= STATUS
            SKIP
            IF C1<>MC1.or.C2<>MC2.or.C3<>MC3
               EXIT
            ENDIF
            MCNT= 1
         ELSE
            OPBAL= 0
            IF MCNT=0
               IF DR_AMOUNT>0
                  IF STATUS='DR'
                     IF DR_AMOUNT<=BL_AMOUNT
                        OPBAL= BL_AMOUNT-DR_AMOUNT
                        OPST= 'DR'
                     ELSE
                        OPBAL= DR_AMOUNT-BL_AMOUNT
                        OPST= 'CR'
                     ENDIF
                  ELSE
                     OPBAL= BL_AMOUNT+DR_AMOUNT
                     OPST= 'CR'
                  ENDIF
               ENDIF
               IF CR_AMOUNT>0
                  IF STATUS='CR'
                     IF CR_AMOUNT<=BL_AMOUNT
                        OPBAL= BL_AMOUNT-CR_AMOUNT
                        OPST= 'CR'
                     ELSE
                        OPBAL= CR_AMOUNT-BL_AMOUNT
                        OPST= 'DR'
                     ENDIF
                  ELSE
                     OPBAL= BL_AMOUNT+CR_AMOUNT
                     OPST= 'DR'
                  ENDIF
               ENDIF
               @ LINE, 10 SAY 'Opening Balance'
               @ LINE, 48 SAY OPBAL PICTURE '99,999,999,999.99'
               MOPEN= OPBAL
               LINE= LINE+1
            ENDIF
         ENDIF
         MCNT= 1
         LINE= LINE+1
         CNT= CNT+1
         @ LINE, 1 SAY DATE
         T= '  '
         DO CASE
            CASE VCH_TYPE=1
               T= 'CR-'
            CASE VCH_TYPE=2
               T= 'CP-'
            CASE VCH_TYPE=3
               T= 'BR-'
            CASE VCH_TYPE=4
               T= 'BP-'
            CASE VCH_TYPE=5
               T= 'JV-'
            CASE VCH_TYPE=6
               T= 'SV-'
            CASE VCH_TYPE=7
               T= 'PV-'
         ENDCASE
         MVCH1= '  '
         MVCH2= '    '
         MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
         MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
         @ LINE, 10 SAY T
         @ LINE, 13 SAY MVCH1
         @ LINE, 15 SAY '-'
         @ LINE, 16 SAY MVCH2
         WDESC= LTRIM(DESC)
         WDESC1= LTRIM(DESC1)
         @ LINE, 22 SAY WDESC PICTURE 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
         IF STATUS='DR'
            TDR= TDR+MOPEN
         ELSE
            TCR= TCR+MOPEN
         ENDIF
         IF DR_AMOUNT>0
            @ LINE, 51 SAY DR_AMOUNT PICTURE '999,999,999.99'
            TDR= TDR+DR_AMOUNT
         ENDIF
         IF CR_AMOUNT>0
            @ LINE, 66 SAY CR_AMOUNT PICTURE '999,999,999.99'
            TCR= TCR+CR_AMOUNT
         ENDIF
         LINE= LINE+1
         @ LINE, 22 SAY WDESC1 PICTURE 'XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX'
         STORE BL_AMOUNT TO MBL_AMOUNT
         STORE STATUS TO MSTATUS
         IF P='S'.and.LINE>=15
            XYZ= ' '
            DO WHILE .not. XYZ$'CQ'
               SET CONFIRM OFF
               @ 24, 50 SAY 'Continue/Quit  -  C/Q  ' GET XYZ FUNCTION '!'
               READ
            ENDDO
            IF XYZ='Q'
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            @ 8, 0 CLEAR
            PAGE= PAGE+1
            PG_HDNG= .T.
            LINE= 8
            CLEAR
         ENDIF
         IF P='P'.and.LINE>52
            PAGE= PAGE+1
            PG_HDNG= .T.
            LINE= 8
         ENDIF
         SELECT 1
         MOPEN= 0
         SKIP
      ENDDO
      IF P='S'.and.LINE>=18
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            SET CONFIRM OFF
            @ 24, 50 SAY 'Continue/Quit  -  C/Q  ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 8, 0 CLEAR
         PAGE= PAGE+1
         PG_HDNG= .T.
         LINE= 8
      ENDIF
      IF P='P'.and.LINE>56
         PAGE= PAGE+1
         PG_HDNG= .T.
         LINE= 8
      ENDIF
      LINE= LINE+1
      @ LINE, 50 SAY REPLICATE('-',30)
      LINE= LINE+1
      @ LINE, 30 SAY 'T o t a l s'
      @ LINE, 51 SAY TDR PICTURE '9999999,999.99'
      @ LINE, 66 SAY TCR PICTURE '9999999,999.99'
      LINE= LINE+1
      @ LINE, 50 SAY REPLICATE('=',30)
      LINE= LINE+1
      @ LINE, 48 SAY MBL_AMOUNT PICTURE '99,999,999,999.99'
      MOPEN= 0
      MOPEN= MBL_AMOUNT
      RMK= 'Cls.Bal.'
      @ LINE, 72 SAY RMK
      LINE= LINE+1
   ENDDO
   SET FILTER TO
   IF P='S'
      @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
      WAIT ''
   ENDIF
   IF P='P'
      EJECT
      SET PRINTER OFF
      SET DEVICE TO SCREEN
   ENDIF
ENDDO
CLEAR
CLEAR ALL
CLOSE ALL
RETURN

PROCEDURE VCH-CONF
CLEAR
CLOSE ALL
SET DELETED ON
SET COLOR TO 7/1
@ 2, 20 SAY 'Re-indexing Data Files .... Please wait'
SELECT 1
USE tran INDEX tranent
SELECT 2
USE ldgrcode INDEX ac1
SELECT 1
DIMENSION MAVT( 7)
MAVT( 1)= 'CR'
MAVT( 2)= 'CP'
MAVT( 3)= 'BR'
MAVT( 4)= 'BP'
MAVT( 5)= 'JV'
MAVT( 6)= 'SV'
MAVT( 7)= 'PV'
DO WHILE .T.
   SET COLOR TO 7/1
   SELECT 1
   CLEAR
   PAGE= 1
   MVCH1= '  '
   MVCH2= '    '
   STORE 0 TO MCODE, MSRNO, MAMOUNT, AMT1, AMT2
   STORE 0 TO MVCH_TYPE, MVCH_NO1, MVCH_NO2
   @ 2, 1 TO 2, 79 DOUBLE
   @ 1, 30 SAY 'VOUCHER  CONFIRMATION '
   @ 1, 60 SAY 'PAGE #'
   @ 1, 67 SAY PAGE PICTURE '99'
   @ 21, 1 TO 21, 79 DOUBLE
   @ 23, 50 SAY 'Press Esc to Return'
   R= 3
   @ 22, 45 CLEAR TO 22, 79
   @ R, 32 SAY 'Voucher Type  ' GET MVCH_TYPE PICTURE '9'
   @ 22, 1 SAY 'Voucher Types:'
   @ 22, 17 SAY '1 - '
   @ 23, 17 SAY '2 - '
   @ 22, 58 SAY '3 - '
   @ 23, 58 SAY '4 - '
   @ 24, 17 SAY '5 - '
   @ 24, 40 SAY '6 - '
   @ 24, 58 SAY '7 - '
   @ 22, 21 SAY 'Cash Receipt - '
   @ 23, 21 SAY 'Cash Payment - '
   @ 22, 62 SAY 'Bank Receipt - '
   @ 23, 62 SAY 'Bank Payment - '
   @ 24, 21 SAY 'Journal      - '
   @ 24, 44 SAY 'Sales - '
   @ 24, 62 SAY 'Purchase     - '
   SET COLOR TO /w 
   @ 22, 36 SAY 'CR'
   @ 23, 36 SAY 'CP'
   @ 22, 77 SAY 'BR'
   @ 23, 77 SAY 'BP'
   @ 24, 36 SAY 'JV'
   @ 24, 52 SAY 'SV'
   @ 24, 77 SAY 'PV'
   SET COLOR TO 7/1
   READ
   IF MVCH_TYPE=0
      LOOP
   ENDIF
   IF MVCH_TYPE>=8
      @ R, 55 SAY '  '
      @ R, 30 SAY 'No such Voucher type exist -- Press any key'
      WAIT ''
      @ R, 30 SAY SPACE(55)
      LOOP
   ENDIF
   @ 21, 1 CLEAR TO 24, 79
   SET COLOR TO /w
   @ R, 52 SAY MAVT(MVCH_TYPE)
   SET COLOR TO 7/1 
   @ 23, 50 SAY 'Press Esc to Return'
   @ 5, 22 SAY '          '
   @ 5, 5 SAY 'Voucher No '
   SET COLOR TO /w
   @ 5, 17 SAY MAVT(MVCH_TYPE)
   @ 5, 20 SAY '  -     '
   SET COLOR TO 7/1
   @ 5, 20 GET MVCH_NO1 PICTURE '@z 99'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 50 SAY SPACE(26)
   IF MVCH_NO1=0
      LOOP
   ENDIF
   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
   SET COLOR TO /w
   @ 5, 20 SAY MVCH1
   SET COLOR TO 7/1   
   @ 5, 23 GET MVCH_NO2 PICTURE '@z 99999'
   READ
   IF MVCH_NO2=0
      LOOP
   ENDIF
   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
   SET COLOR TO /w
   @ 5, 23 SAY MVCH2
   SET COLOR TO 7/1
   SELECT 1
   SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This Voucher No Does Not Exists  -- Press any key'
      WAIT ''
      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore  '
      @ 13, 15 SAY 'you cannot Delete it. If you still want to Delete  '
      @ 14, 15 SAY 'it, please use UN-POST OPTION first.  '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SELECT 1
   SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   GOTO TOP
   @ 3, 5 SAY 'Date       ' GET DATE
   CLEAR GETS
   SET COLOR TO 7/1
   @ 7, 1 SAY REPLICATE('-',78)
   @ 8, 2 SAY 'SR   ACCT-NO.  T I T L E                    DR/CR     DR-AMT.       CR-AMT.  '
   @ 9, 1 TO 9, 79 DOUBLE
   L= 10
   Y= '  '
   DO WHILE .not. EOF()
      MC1= C1
      MC2= C2
      MC3= C3
      @ L, 3 SAY SR_NO PICTURE '99'
      SET COLOR TO /w
      @ L, 6 SAY '  -  -   '
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ L, 6 SAY MPC1 PICTURE '99'
      @ L, 9 SAY MPC2 PICTURE '99'
      @ L, 12 SAY MPC3 PICTURE '999'
      SET COLOR TO 7/1
      SELECT 2
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF FOUND()
         SET COLOR TO /w
         @ L, 17 SAY LEFT(TITLE,28)
         SET COLOR TO 7/1 
      ELSE
         @ L, 17 SAY ' '
      ENDIF
      SELECT 1
      @ L, 47 SAY DR_CR
      IF DR_CR='DR'
         SET COLOR TO /w
         @ L, 50 SAY AMOUNT PICTURE '999999999.99'
         SET COLOR TO 7/1
      ELSE
         SET COLOR TO /w
         @ L, 64 SAY AMOUNT PICTURE '999999999.99'
         SET COLOR TO 7/1 
      ENDIF
      IF L>=21
         ANS= ' '
         DO WHILE .not. ANS$'CQ'
            SET CONFIRM OFF
            @ 24, 57 SAY 'Continue/Quit -  C/Q ' GET ANS FUNCTION '!'
            READ
         ENDDO
         IF ANS='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 9, 1 CLEAR TO 24, 79
         L= 9
         PAGE= PAGE+1
         @ 2, 67 SAY PAGE PICTURE '99'
      ENDIF
      IF .not. EOF()
         SKIP
      ENDIF
      L= L+1
      Y= '  '
   ENDDO
   IF EOF()
      SUM FOR DR_CR='DR' AMOUNT TO AMT1
      SUM FOR DR_CR='CR' AMOUNT TO AMT2
      @ 21, 49 SAY AMT1 PICTURE '9999999,999.99'
      @ 21, 64 SAY AMT2 PICTURE '9999999,999.99'
   ENDIF
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Voucher - Y/N ' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 19 CLEAR TO 24, 77
   IF ANS='Y'
      STORE 0 TO MDB_CODE, MCR_CODE, WSRNO, WRECNO, MSRNO, WAMOUNT, MAMOUNT, Q
      MVOUCH_NO= SPACE(8)
      MDISC= SPACE(30)
      MDATE= DATE()
      STORE '  ' TO ST, RST
      SELECT 1
      GOTO TOP
      DO WHILE VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         SELECT 1
         DO chk
         IF FLAG='Y'
            SKIP
            LOOP
         ENDIF
         IF DR_CR='DR'
            MDR_AMOUNT= AMOUNT
            MCR_AMOUNT= 0
         ELSE
            MCR_AMOUNT= AMOUNT
            MDR_AMOUNT= 0
         ENDIF
         MDATE= DATE
         MC1= C1
         MC2= C2
         MC3= C3
         MVCH_TYPE= VCH_TYPE
         MVCH_NO1= VCH_NO1
         MVCH_NO2= VCH_NO2
         MDISC= DESCRIP
         MDISC1= DESCRIP1
         SELECT 2
         SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
         IF FOUND()
            WSRNO= SR_NO+1
            IF  B->STATUS='  '
               IF MDR_AMOUNT>0
                  WAMOUNT= MDR_AMOUNT
                  ST= 'DR'
               ELSE
                  WAMOUNT= MCR_AMOUNT
                  ST= 'CR'
               ENDIF
            ENDIF
            IF  B->STATUS='DR'
               IF MDR_AMOUNT>0
                  WAMOUNT= LDGR_BAL+MDR_AMOUNT
                  ST= 'DR'
               ELSE
                  WAMOUNT= LDGR_BAL-MCR_AMOUNT
                  IF WAMOUNT>0
                     ST= 'DR'
                  ELSE
                     ST= 'CR'
                  ENDIF
               ENDIF
            ENDIF
            IF  B->STATUS='CR'
               IF MCR_AMOUNT>0
                  WAMOUNT= LDGR_BAL+MCR_AMOUNT
                  ST= 'CR'
               ELSE
                  WAMOUNT= LDGR_BAL-MDR_AMOUNT
                  IF WAMOUNT>0
                     ST= 'CR'
                  ELSE
                     ST= 'DR'
                  ENDIF
               ENDIF
            ENDIF
            REPLACE LDGR_BAL WITH ABS(WAMOUNT)
            REPLACE SR_NO WITH WSRNO
            REPLACE B->STATUS WITH ST
         ENDIF
         SELECT 3
         USE master INDEX ac2
         APPEND BLANK
         REPLACE DATE WITH MDATE
         REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
         REPLACE BL_AMOUNT WITH ABS(WAMOUNT)
         IF MDR_AMOUNT>0
            REPLACE DR_AMOUNT WITH MDR_AMOUNT
         ELSE
            REPLACE CR_AMOUNT WITH MCR_AMOUNT
         ENDIF
         REPLACE DESC WITH MDISC
         REPLACE DESC1 WITH MDISC1
         REPLACE VCH_TYPE WITH MVCH_TYPE
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
         REPLACE SR_NO WITH WSRNO
         REPLACE C->STATUS WITH ST
         FLUSH
         SELECT 3
         USE
         WSRNO= 0
         SELECT 1
         REPLACE FLAG WITH 'Y'
         REPLACE CONFIRM WITH 'C'
         SKIP
      ENDDO
   ENDIF
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 24 SAY 'More Voucher For Confirmation - Y/N ' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE POST-MEN
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 25 SAY '       P O S T I N G '
   SET COLOR TO 7/1
   @ 8, 24 PROMPT '1 - Vouchers Confirmation/Posting '
   @ 10, 24 PROMPT '2 - Post Transactions To Ledgers  '
   @ 12, 24 PROMPT '9 - Return to Previous Menu       '
   CHOICE= 0
   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         DO vch-conf
      CASE CHOICE=2
         DO ldgrpost
      CASE CHOICE=3
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE LDGRPOST
CLEAR
SET COLOR TO 7/1
CLEAR
CLOSE ALL
SET DATE brit
HEADING= .T.
STORE DATE() TO MPDATE
SELECT 1
USE tran
SORT ON VCH_TYPE, VCH_NO1, VCH_NO2, SR_NO TO smq
USE
SELECT 1
USE smq
COPY TO tran
RUN del smq.dbf
SELECT 1
USE tran
INDEX ON DATE TO trandate
GOTO BOTTOM
CLEAR
STORE DATE TO MDATE, MPDATE
SET COLOR TO +7/1
@ 1, 2 SAY 'Post Transactions To Ledger:'
@ 3, 2 SAY 'This option will post all un-posted vouchers to their respective'
@ 4, 2 SAY 'ledger heads Upto Given Date....'
SET COLOR TO 7/1     
@ 13, 14 SAY 'Vouchers upto            exist in file '
SET COLOR TO *+/W
@ 13, 29 SAY MDATE
SET COLOR TO 7/1     
@ 14, 14 SAY 'Date upto which you want vouchers to be posted ' GET MPDATE
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
SELECT 2
USE ldgrcode INDEX ac1
SELECT 3
USE master
STORE 0 TO MDB_CODE, MCR_CODE, WSRNO, WRECNO, MSRNO, WAMOUNT, MAMOUNT, Q, MVCH_NO1, MVCH_NO2
MVOUCH_NO= SPACE(8)
MDISC= SPACE(30)
MDISC1= SPACE(30)
MDATE= DATE()
STORE '  ' TO ST, RST
SELECT 1
SET FILTER TO FLAG=' '
GOTO TOP
R= 2
CLEAR
DO WHILE .not. EOF()
   SELECT 1
   STORE STR(C1,2)+STR(C2,2)+STR(C3,3) TO MCODE
   SELECT 2
   SEEK MCODE
   IF .not. FOUND()
      IF HEADING
         @ R, 10 SAY 'The Following Account Codes Do Not Exist In Code File'
         R= R+1
         @ R, 10 SAY 'These Account Codes Were Probably Deleted Before Data Posting'
         R= R+1
         @ R, 10 SAY 'Please Correct Your Vouchers Or Create Missing Ledger Heads'
         R= R+1
         @ R, 10 SAY 'Before Data Posting'
         R= R+1
         @ R, 1 TO R, 78
         R= R+1
         @ R, 1 SAY 'Voucher No'
         @ R, 13 SAY 'Voucher Type'
         @ R, 28 SAY 'Account Code'
         R= R+1
         @ R, 1 TO R, 78
         HEADING= .F.
      ENDIF
      SELECT 1
      R= R+1
      @ R, 1 SAY VCH_NO1 PICTURE '99'
      @ R, 3 SAY '-'
      @ R, 4 SAY VCH_NO2 PICTURE '99999'
      @ R, 18 SAY VCH_TYPE
      @ R, 28 SAY C1
      @ R, 30 SAY '-'
      @ R, 31 SAY C2
      @ R, 33 SAY '-'
      @ R, 34 SAY C3
      Q= 21
   ENDIF
   IF R>=21
      WAIT
      CLEAR
      R= 2
      HEADING= .T.
   ENDIF
   SELECT 1
   SKIP
ENDDO
IF EOF().and.Q=21
   @ 24, 5 SAY 'Vouchers Not Posted --- Press Any Key To Return to Menu '
   WAIT ''
   CLEAR
   CLEAR ALL
   RETURN
ENDIF
CLEAR
@ 13, 23 SAY 'Please wait data is being posted....'
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   DO chk
   IF FLAG='Y'.or.DATE>MPDATE
      SKIP
      LOOP
   ENDIF
   IF DR_CR='DR'
      MDR_AMOUNT= AMOUNT
      MCR_AMOUNT= 0
   ELSE
      MCR_AMOUNT= AMOUNT
      MDR_AMOUNT= 0
   ENDIF
   MDATE= DATE
   MC1= C1
   MC2= C2
   MC3= C3
   MVCH_TYPE= VCH_TYPE
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   MDISC= DESCRIP
   MDISC1= DESCRIP1
   SELECT 2
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF FOUND()
      WSRNO= SR_NO+1
      IF  B->STATUS='  '
         IF MDR_AMOUNT>0
            WAMOUNT= MDR_AMOUNT
            ST= 'DR'
         ELSE
            WAMOUNT= MCR_AMOUNT
            ST= 'CR'
         ENDIF
      ENDIF
      IF  B->STATUS='DR'
         IF MDR_AMOUNT>0
            WAMOUNT= LDGR_BAL+MDR_AMOUNT
            ST= 'DR'
         ELSE
            WAMOUNT= LDGR_BAL-MCR_AMOUNT
            IF WAMOUNT>0
               ST= 'DR'
            ELSE
               ST= 'CR'
            ENDIF
         ENDIF
      ENDIF
      IF  B->STATUS='CR'
         IF MCR_AMOUNT>0
            WAMOUNT= LDGR_BAL+MCR_AMOUNT
            ST= 'CR'
         ELSE
            WAMOUNT= LDGR_BAL-MDR_AMOUNT
            IF WAMOUNT>0
               ST= 'CR'
            ELSE
               ST= 'DR'
            ENDIF
         ENDIF
      ENDIF
      REPLACE LDGR_BAL WITH ABS(WAMOUNT)
      REPLACE SR_NO WITH WSRNO
      REPLACE B->STATUS WITH ST
   ENDIF
   SELECT 3
   APPEND BLANK
   REPLACE DATE WITH MDATE
   REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
   REPLACE BL_AMOUNT WITH ABS(WAMOUNT)
   IF MDR_AMOUNT>0
      REPLACE DR_AMOUNT WITH MDR_AMOUNT
   ELSE
      REPLACE CR_AMOUNT WITH MCR_AMOUNT
   ENDIF
   REPLACE DESC WITH MDISC
   REPLACE DESC1 WITH MDISC1
   REPLACE VCH_TYPE WITH MVCH_TYPE
   REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
   REPLACE SR_NO WITH WSRNO
   REPLACE C->STATUS WITH ST
   WSRNO= 0
   SELECT 1
   REPLACE FLAG WITH 'Y'
   SKIP
ENDDO
CLOSE ALL
USE master
INDEX ON DATE TO mastdate
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac2
CLEAR
CLOSE ALL
RETURN

PROCEDURE REP-MEN1
CLEAR
STORE DATE() TO DATE
SET DATE brit
MDATE= DATE()
DO chk
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 25 SAY ' R E P O R T S   M E N U - 1'
   SET COLOR TO 7/1
   @ 8, 24 PROMPT ' 1 - Ledger Printing                  '
   @ 9, 24 PROMPT ' 2 - Accounts Receivable Report       '
   @ 10, 24 PROMPT ' 3 - .........  With Address          '
   @ 11, 24 PROMPT ' 4 - Accounts Payable Report          '
   @ 12, 24 PROMPT ' 5 - ......... With Address           '
   @ 13, 24 PROMPT ' 6 - Accounts Payable/Receivable (Between Dates) '
   @ 14, 24 PROMPT ' 7 - Journal Report                   '
   @ 15, 24 PROMPT ' 8 - Journal Report With Description  '
   @ 16, 24 PROMPT ' 9 - Reminder Letter (Aging From Due Date)   '
   @ 17, 24 PROMPT '10 - Reminder Letter (Aging From Issue Date) '
   @ 18, 24 PROMPT '11 - Aging Report Summary             '
   @ 19, 24 PROMPT '12 - Reminders (Without Aging)        '
   @ 20, 24 PROMPT '13 - Transactions Summary             '
   @ 21, 24 PROMPT '14 - Return to Previous Menu          '
   CHOICE= 0
   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         DO ldgr-men
      CASE CHOICE=2
         DO rep-rec
      CASE CHOICE=3
         DO rep-rec1
      CASE CHOICE=4
         DO rep-pay
      CASE CHOICE=5
         DO rep-pay1
      CASE CHOICE=6
         DO reprpay
      CASE CHOICE=7
         DO jou-rep
      CASE CHOICE=8
         DO jou1-rep
      CASE CHOICE=9
         DO rec-age
      CASE CHOICE=10
         DO rec-age1
      CASE CHOICE=11
         DO rec-age2
      CASE CHOICE=12
         DO rec-age3
      CASE CHOICE=13
         DO tr-rep
      CASE CHOICE=14
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE LDGR-MEN
STORE DATE() TO DATE
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 25 SAY '  LEDGER  PRINTING  REPORTS'
   SET COLOR TO 7/1
   @ 8, 22 PROMPT ' 1 - Selective Ledger (Display)        '
   @ 9, 22 PROMPT ' 2 - Selective Ledger (Display - II)   '
   @ 10, 22 PROMPT ' 3 - Selective Ledger (Printing)       '
   @ 11, 22 PROMPT ' 4 - Selective Ledger (Printing - II)  '
   @ 12, 22 PROMPT ' 5 - Ledger Within Range (Printing)    '
   @ 13, 22 PROMPT ' 6 - Selective Ledger (Printing With Address)    '
   @ 14, 22 PROMPT ' 7 - Ledger Within Range (Printing With Address) '
   @ 15, 22 PROMPT ' 8 - Selective Ledger (Balance Display)   '
   @ 16, 22 PROMPT ' 9 - Day Book (Printing/Display)          '
   @ 17, 22 PROMPT '10 - Ledger Within Range (Printing - II)  '
   @ 19, 22 PROMPT '11 - Return to Previous Menu              '
   CHOICE= 0
   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         DO ldgr-sd
      CASE CHOICE=2
         DO ldgr-sd2
      CASE CHOICE=3
         DO ldgr-sp
      CASE CHOICE=4
         DO ldgr-sp2
      CASE CHOICE=5
         DO ldgr-fp
      CASE CHOICE=6
         DO ldgrasp
      CASE CHOICE=7
         DO ldgrafp
      CASE CHOICE=8
         DO ldgr-sdb
      CASE CHOICE=9
         DO vch-book
      CASE CHOICE=10
         DO ldgr-fp2
      CASE CHOICE=11
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE LDGR-SD
SET DATE brit
SET CENTURY OFF
SELECT 1
USE master
SELECT 2
USE ldgrcode INDEX ac1
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   STORE 0 TO MC1, MC2, MC3, TDR, TCR, MBL_AMOUNT
   STORE ' ' TO MSTATUS
   PG_HDNG= .T.
   PAGE= 1
   LINE= 8
   WTITLE= SPACE(30)
   GOTO TOP
   DO WHILE .T.
      MCNT= 0
      STORE DATE() TO SDATE, LDATE
      @ 1, 10 SAY 'For Accurate Result Please Sort Ledger Date Wise Option 6.5'
      @ 23, 1 SAY 'Press Esc To Return '
      @ 10, 10 SAY 'Enter  Code  for  Display:     -  -'
      @ 10, 40 GET MC1 PICTURE '99'
      @ 10, 43 GET MC2 PICTURE '99'
      @ 10, 46 GET MC3 PICTURE '999'
      READ
      IF READKEY()=12
         CLOSE ALL
         CLEAR
         RETURN
      ENDIF
      @ 23, 1
      SELECT 2
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF .not. FOUND()
         @ 22, 10 SAY 'code is not in code file'
         WAIT ''
         @ 22, 10 SAY '                           '
         LOOP
      ENDIF
      WTITLE=  B->TITLE
      @ 12, 10 SAY 'TITLE:'
      SET COLOR TO /w
      @ 12, 19 SAY WTITLE
      MREM= REMARKS
      SET COLOR TO 7/1
      @ 14, 10 SAY 'From Date ' GET SDATE
      @ 16, 10 SAY 'To.. Date ' GET LDATE
      READ
      EXIT
   ENDDO
   P= ' '
   DO WHILE .not. P$'CQ'
      @ 19, 10 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   CNT= 0
   RMK= '             '
   CLEAR
   SELECT 1
   LINE= 8
   @ 11, 20 SAY 'Pleas Wait -  Prepairing Sub-Ledger '
   SET FILTER TO DATE>=SDATE.and.DATE<=LDATE.and.C1=MC1.and.C2=MC2.and.C3=MC3
   CLEAR
   IF RECCOUNT()=0
      @ 10, 10 SAY 'There is No Transaction Between These Dates'
      @ 24, 25 SAY 'Press Any Key To Return'
      WAIT ''
      @ 10, 10 SAY SPACE(60)
      CLEAR ALL
      EXIT
   ENDIF
   GOTO TOP
   DO WHILE .not. EOF()
      IF BL_AMOUNT=0.and.SR_NO<>1.and.DR_AMOUNT=0.and.CR_AMOUNT=0
         SKIP
         LOOP
      ENDIF
      WC1= MC1
      WC2= MC2
      WC3= MC3
      IF PG_HDNG
         LINE= 8
         @ 2, 20 SAY 'LEDGER DISPLAY FROM: '
         @ 2, 42 SAY SDATE
         @ 2, 52 SAY 'To'
         @ 2, 56 SAY LDATE
         @ 2, 70 SAY 'page: '
         @ 2, 76 SAY PAGE PICTURE '99'
         @ 3, 1 SAY REPLICATE('-',79)
         @ 4, 1 SAY 'ACCT.CODE:'
         WPC1= REPLICATE('0',2-LEN(LTRIM(STR(WC1))))+LTRIM(STR(WC1))
         @ 4, 12 SAY WPC1
         @ 4, 14 SAY '-'
         WPC2= REPLICATE('0',2-LEN(LTRIM(STR(WC2))))+LTRIM(STR(WC2))
         @ 4, 15 SAY WPC2
         @ 4, 17 SAY '-'
         WPC3= REPLICATE('0',3-LEN(LTRIM(STR(WC3))))+LTRIM(STR(WC3))
         @ 4, 18 SAY WPC3
         @ 4, 24 SAY 'TITLE OF ACCOUNT:- '
         @ 4, 46 SAY WTITLE PICTURE 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
         @ 5, 1 SAY REPLICATE('-',79)
         @ 6, 1 SAY ' DATE     DESCRIPTION'
         @ 6, 26 SAY '         DEBIT      CREDIT        BALANCE    REMARKS'
         @ 7, 1 SAY REPLICATE('-',79)
      ENDIF
      SELECT 1
      IF EOF()
         EXIT
      ENDIF
      IF C3=0
         SKIP
         LOOP
      ENDIF
      IF SR_NO=1
         @ LINE, 1 SAY DATE
         WDESC= LTRIM(DESC)
         @ LINE, 10 SAY WDESC PICTURE 'XXXXXXXXXXXXXXXXXXX'
         @ LINE, 53 SAY BL_AMOUNT PICTURE '99,999,999,999.99'
         @ LINE, 69 SAY STATUS
         RMK= 'Opn.Bal'
         @ LINE, 72 SAY RMK
         MBL_AMOUNT= BL_AMOUNT
         MSTATUS= STATUS
         SKIP
         IF C1<>MC1.or.C2<>MC2.or.C3<>MC3
            EXIT
         ENDIF
         MCNT= 1
      ELSE
         OPBAL= 0
         IF MCNT=0
            IF DR_AMOUNT>0
               IF STATUS='DR'
                  IF DR_AMOUNT<=BL_AMOUNT
                     OPBAL= BL_AMOUNT-DR_AMOUNT
                     OPST= 'DR'
                  ELSE
                     OPBAL= DR_AMOUNT-BL_AMOUNT
                     OPST= 'CR'
                  ENDIF
               ELSE
                  OPBAL= BL_AMOUNT+DR_AMOUNT
                  OPST= 'CR'
               ENDIF
            ENDIF
            IF CR_AMOUNT>0
               IF STATUS='CR'
                  IF CR_AMOUNT<=BL_AMOUNT
                     OPBAL= BL_AMOUNT-CR_AMOUNT
                     OPST= 'CR'
                  ELSE
                     OPBAL= CR_AMOUNT-BL_AMOUNT
                     OPST= 'DR'
                  ENDIF
               ELSE
                  OPBAL= BL_AMOUNT+CR_AMOUNT
                  OPST= 'DR'
               ENDIF
            ENDIF
            @ LINE, 10 SAY 'Opening Balance'
            @ LINE, 53 SAY OPBAL PICTURE '99999999,999.99'
            @ LINE, 69 SAY OPST
            LINE= LINE+1
         ENDIF
      ENDIF
      MCNT= 1
      LINE= LINE+1
      CNT= CNT+1
      @ LINE, 1 SAY DATE
      WDESC= LTRIM(DESC)
      @ LINE, 10 SAY WDESC PICTURE 'XXXXXXXXXXXXXXXXXXX'
      IF DR_AMOUNT>0
         @ LINE, 30 SAY DR_AMOUNT PICTURE '999,999,999.99'
         TDR= TDR+DR_AMOUNT
      ENDIF
      IF CR_AMOUNT>0
         @ LINE, 35 SAY CR_AMOUNT PICTURE '999,999,999.99'
         TCR= TCR+CR_AMOUNT
      ENDIF
      @ LINE, 50 SAY BL_AMOUNT PICTURE '99999999,999.99'
      @ LINE, 66 SAY STATUS
      T= '  '
      DO CASE
         CASE VCH_TYPE=1
            T= 'CR-'
         CASE VCH_TYPE=2
            T= 'CP-'
         CASE VCH_TYPE=3
            T= 'BR-'
         CASE VCH_TYPE=4
            T= 'BP-'
         CASE VCH_TYPE=5
            T= 'JV-'
         CASE VCH_TYPE=6
            T= 'SV-'
         CASE VCH_TYPE=7
            T= 'PV-'
      ENDCASE
      MVCH1= '  '
      MVCH2= '    '
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
      @ LINE, 69 SAY T
      @ LINE, 72 SAY MVCH1
      @ LINE, 74 SAY '-'
      @ LINE, 75 SAY MVCH2
      STORE BL_AMOUNT TO MBL_AMOUNT
      STORE STATUS TO MSTATUS
      IF LINE>18
         PAGE= PAGE+1
         LINE= 8
         PG_HDNG= .T.
         P= ' '
         DO WHILE .not. P$'CQ'
            SET CONFIRM OFF
            @ 24, 50 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
            READ
         ENDDO
         IF P='Q'
            CLEAR
            EXIT
         ENDIF
         @ 8, 0 CLEAR
      ELSE
         PG_HDNG= .F.
      ENDIF
      SKIP
   ENDDO
   LINE= LINE+2
   @ LINE, 1 SAY 'Total Transactions = '
   @ LINE, 20 SAY CNT PICTURE '99999'
   @ LINE, 25 SAY TDR PICTURE '999999,999.99'
   @ LINE, 35 SAY TCR PICTURE '999999,999.99'
   @ LINE, 50 SAY MBL_AMOUNT PICTURE '99999999,999.99'
   @ LINE, 66 SAY MSTATUS
   RMK= 'Cls.Bal.'
   @ LINE, 72 SAY RMK
   LINE= LINE+2
   @ LINE, 5 SAY 'R e m a r k s     -  '
   @ LINE, 25 SAY MREM
   @ 24, 22 SAY 'Press Any Key To Return '
   WAIT ''
   SET FILTER TO
ENDDO
CLEAR
CLEAR ALL
CLOSE ALL
RETURN

PROCEDURE LDGR-SD2
SET DATE brit
SET CENTURY OFF
SELECT 1
USE master
SELECT 2
USE ldgrcode INDEX ac1
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   STORE 0 TO MC1, MC2, MC3, TDR, TCR, MBL_AMOUNT
   STORE ' ' TO MSTATUS
   PG_HDNG= .T.
   PAGE= 1
   LINE= 8
   WTITLE= SPACE(30)
   GOTO TOP
   DO WHILE .T.
      MCNT= 0
      STORE DATE() TO SDATE, LDATE
      @ 1, 10 SAY 'For Accurate Result Please Sort Ledger Date Wise Option 6.5'
      @ 23, 1 SAY 'Press Esc To Return '
      @ 10, 10 SAY 'Enter  Code  for  Display:     -  -'
      @ 10, 40 GET MC1 PICTURE '99'
      @ 10, 43 GET MC2 PICTURE '99'
      @ 10, 46 GET MC3 PICTURE '999'
      READ
      IF READKEY()=12
         CLOSE ALL
         CLEAR
         RETURN
      ENDIF
      @ 23, 1
      SELECT 2
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF .not. FOUND()
         @ 22, 10 SAY 'code is not in code file'
         WAIT ''
         @ 22, 10 SAY '                           '
         LOOP
      ENDIF
      WTITLE=  B->TITLE
      @ 12, 10 SAY 'TITLE:'
      SET COLOR TO /w
      @ 12, 19 SAY WTITLE
      MREM= REMARKS
      SET COLOR TO 7/1
      @ 14, 10 SAY 'From Date ' GET SDATE
      @ 16, 10 SAY 'To.. Date ' GET LDATE
      READ
      EXIT
   ENDDO
   P= ' '
   DO WHILE .not. P$'CQ'
      @ 19, 10 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   CNT= 0
   RMK= '             '
   CLEAR
   SELECT 1
   LINE= 8
   @ 11, 20 SAY 'Pleas Wait -  Prepairing Sub-Ledger '
   SET FILTER TO DATE>=SDATE.and.DATE<=LDATE.and.C1=MC1.and.C2=MC2.and.C3=MC3
   CLEAR
   IF RECCOUNT()=0
      @ 10, 10 SAY 'There is No Transaction Between These Dates'
      @ 24, 25 SAY 'Press Any Key To Return'
      WAIT ''
      @ 10, 10 SAY SPACE(60)
      CLEAR ALL
      EXIT
   ENDIF
   GOTO TOP
   DO WHILE .not. EOF()
      IF BL_AMOUNT=0.and.SR_NO<>1.and.DR_AMOUNT=0.and.CR_AMOUNT=0
         SKIP
         LOOP
      ENDIF
      WC1= MC1
      WC2= MC2
      WC3= MC3
      IF PG_HDNG
         LINE= 8
         @ 2, 20 SAY 'LEDGER DISPLAY FROM: '
         @ 2, 42 SAY SDATE
         @ 2, 52 SAY 'To'
         @ 2, 56 SAY LDATE
         @ 2, 70 SAY 'page: '
         @ 2, 76 SAY PAGE PICTURE '99'
         @ 3, 1 SAY REPLICATE('-',79)
         @ 4, 1 SAY 'ACCT.CODE:'
         WPC1= REPLICATE('0',2-LEN(LTRIM(STR(WC1))))+LTRIM(STR(WC1))
         @ 4, 12 SAY WPC1
         @ 4, 14 SAY '-'
         WPC2= REPLICATE('0',2-LEN(LTRIM(STR(WC2))))+LTRIM(STR(WC2))
         @ 4, 15 SAY WPC2
         @ 4, 17 SAY '-'
         WPC3= REPLICATE('0',3-LEN(LTRIM(STR(WC3))))+LTRIM(STR(WC3))
         @ 4, 18 SAY WPC3
         @ 4, 24 SAY 'TITLE OF ACCOUNT:- '
         @ 4, 46 SAY WTITLE PICTURE 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
         @ 5, 1 SAY REPLICATE('-',79)
         @ 6, 1 SAY ' DATE     DESCRIPTION'
         @ 6, 26 SAY '         DEBIT      CREDIT        BALANCE    REMARKS'
         @ 7, 1 SAY REPLICATE('-',79)
      ENDIF
      SELECT 1
      IF EOF()
         EXIT
      ENDIF
      IF C3=0
         SKIP
         LOOP
      ENDIF
      IF SR_NO=1
         @ LINE, 1 SAY DATE
         WDESC= LTRIM(DESC)
         @ LINE, 10 SAY WDESC PICTURE 'XXXXXXXXXXXXXXXXXXX'
         @ LINE, 53 SAY BL_AMOUNT PICTURE '99,999,999,999.99'
         @ LINE, 69 SAY STATUS
         RMK= 'Opn.Bal'
         @ LINE, 72 SAY RMK
         MBL_AMOUNT= BL_AMOUNT
         MSTATUS= STATUS
         SKIP
         IF C1<>MC1.or.C2<>MC2.or.C3<>MC3
            EXIT
         ENDIF
         MCNT= 1
      ELSE
         OPBAL= 0
         IF MCNT=0
            IF DR_AMOUNT>0
               IF STATUS='DR'
                  IF DR_AMOUNT<=BL_AMOUNT
                     OPBAL= BL_AMOUNT-DR_AMOUNT
                     OPST= 'DR'
                  ELSE
                     OPBAL= DR_AMOUNT-BL_AMOUNT
                     OPST= 'CR'
                  ENDIF
               ELSE
                  OPBAL= BL_AMOUNT+DR_AMOUNT
                  OPST= 'CR'
               ENDIF
            ENDIF
            IF CR_AMOUNT>0
               IF STATUS='CR'
                  IF CR_AMOUNT<=BL_AMOUNT
                     OPBAL= BL_AMOUNT-CR_AMOUNT
                     OPST= 'CR'
                  ELSE
                     OPBAL= CR_AMOUNT-BL_AMOUNT
                     OPST= 'DR'
                  ENDIF
               ELSE
                  OPBAL= BL_AMOUNT+CR_AMOUNT
                  OPST= 'DR'
               ENDIF
            ENDIF
            @ LINE, 10 SAY 'Opening Balance'
            @ LINE, 53 SAY OPBAL PICTURE '99999999,999.99'
            @ LINE, 69 SAY OPST
            LINE= LINE+1
         ENDIF
      ENDIF
      MCNT= 1
      LINE= LINE+1
      CNT= CNT+1
      @ LINE, 1 SAY DATE
      WDESC= LTRIM(DESC)
      @ LINE, 10 SAY WDESC PICTURE 'XXXXXXXXXXXXXXXXXXX'
      IF DR_AMOUNT>0
         @ LINE, 30 SAY DR_AMOUNT PICTURE '999,999,999.99'
         TDR= TDR+DR_AMOUNT
      ENDIF
      IF CR_AMOUNT>0
         @ LINE, 35 SAY CR_AMOUNT PICTURE '999,999,999.99'
         TCR= TCR+CR_AMOUNT
      ENDIF
      @ LINE, 50 SAY BL_AMOUNT PICTURE '99999999,999.99'
      @ LINE, 66 SAY STATUS
      T= '  '
      DO CASE
         CASE VCH_TYPE=1
            T= 'CR-'
         CASE VCH_TYPE=2
            T= 'CP-'
         CASE VCH_TYPE=3
            T= 'BR-'
         CASE VCH_TYPE=4
            T= 'BP-'
         CASE VCH_TYPE=5
            T= 'JV-'
         CASE VCH_TYPE=6
            T= 'SV-'
         CASE VCH_TYPE=7
            T= 'PV-'
      ENDCASE
      MVCH1= '  '
      MVCH2= '    '
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
      @ LINE, 69 SAY T
      @ LINE, 72 SAY MVCH1
      @ LINE, 74 SAY '-'
      @ LINE, 75 SAY MVCH2
      STORE BL_AMOUNT TO MBL_AMOUNT
      STORE STATUS TO MSTATUS
      LINE= LINE+1
      @ LINE, 10 SAY DESC1 PICTURE 'XXXXXXXXXXXXXXXXXXX'
      IF LINE>18
         PAGE= PAGE+1
         LINE= 8
         PG_HDNG= .T.
         P= ' '
         DO WHILE .not. P$'CQ'
            SET CONFIRM OFF
            @ 24, 50 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
            READ
         ENDDO
         IF P='Q'
            CLEAR
            EXIT
         ENDIF
         @ 8, 0 CLEAR
      ELSE
         PG_HDNG= .F.
      ENDIF
      SKIP
   ENDDO
   LINE= LINE+2
   @ LINE, 1 SAY 'Total Transactions = '
   @ LINE, 20 SAY CNT PICTURE '99999'
   @ LINE, 25 SAY TDR PICTURE '999999,999.99'
   @ LINE, 35 SAY TCR PICTURE '999999,999.99'
   @ LINE, 50 SAY MBL_AMOUNT PICTURE '99999999,999.99'
   @ LINE, 66 SAY MSTATUS
   RMK= 'Cls.Bal.'
   @ LINE, 72 SAY RMK
   LINE= LINE+2
   @ LINE, 5 SAY 'R e m a r k s     -  '
   @ LINE, 25 SAY MREM
   @ 24, 22 SAY 'Press Any Key To Return '
   WAIT ''
   SET FILTER TO
ENDDO
CLEAR
CLEAR ALL
CLOSE ALL
RETURN

PROCEDURE LDGR-SP
CLEAR
CLOSE ALL
SET DATE brit
SET CENTURY OFF
SELECT 1
USE master
SELECT 2
USE ldgrcode INDEX ac1
STORE SPACE(65) TO MREM1, MREM2
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   STORE 0 TO TCR, TDR, MBLAMT, MCSTAT
   @ 2, 2 SAY 'Selective Ledger Printing '
   STORE DATE() TO SDATE, LDATE
   @ 23, 50 SAY 'Press Esc to Return '
   STORE 0 TO MC1, MC2, MC3, TDR, TCR, MNO
   @ 6, 10 SAY 'Enter  Code  for  Printing:'
   SET COLOR TO /w
   @ 6, 40 SAY '  -  -'
   SET COLOR TO 7/1
   @ 6, 40 GET MC1 PICTURE '99'
   @ 6, 43 GET MC2 PICTURE '99'
   @ 6, 46 GET MC3 PICTURE '999'
   READ
   IF READKEY()=12
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 23, 1 CLEAR TO 23, 79
   SELECT 2
   GOTO TOP
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist ... Please Re-try  '
      WAIT ''
      @ 23, 10 SAY SPACE(25)
      LOOP
   ELSE
      STORE TITLE TO WTITLE
   ENDIF
   @ 8, 10 SAY 'TITLE:'
   SET COLOR TO /w
   @ 8, 19 SAY WTITLE
   MREM= REMARKS
   SET COLOR TO 7/1
   @ 10, 10 SAY 'From Date ' GET SDATE
   @ 12, 10 SAY 'To.. Date ' GET LDATE
   READ
   @ 13, 10 SAY 'R e m a r k s   '
   @ 14, 10 GET MREM1
   @ 15, 10 GET MREM2
   READ
   OPTN= ' '
   @ 16, 1 TO 16, 79
   @ 17, 25 SAY 'Y O U R   P A P E R   W I D T H'
   @ 19, 25 SAY '      A -  10 Inches  '
   @ 20, 25 SAY '      B -  15 Inches  '
   @ 21, 25 SAY '      Q -  Quit       '
   DO WHILE .not. OPTN$'ABQ'
      @ 23, 31 SAY 'Your Choice  (A/B/Q)' GET OPTN FUNCTION '!'
      READ
   ENDDO
   IF OPTN='Q'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   PANS= ' '
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing In Progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
   HEAD= .T.
   PAGE= 1
   SELECT 1
   STORE 0 TO CNT, MCONT, MBLAMT, MCSTAT
   SET FILTER TO DATE>=SDATE.and.DATE<=LDATE.and.C1=MC1.and.C2=MC2.and.C3=MC3
   IF RECCOUNT()=0
      @ 10, 10 SAY 'There is No Transaction Between These Dates'
      @ 24, 25 SAY 'Press Any Key To Return'
      WAIT ''
      @ 10, 10 SAY SPACE(60)
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      CLEAR ALL
      EXIT
   ENDIF
   GOTO TOP
   LINE= 10
   MCNT= 0
   DO WHILE .not. EOF()
      IF HEAD
         LINE= 10
         IF OPTN='A'
            DO top-prt1
         ELSE
            DO top-prt2
         ENDIF
         IF OPTN='A'
            @ 1, 1 SAY CHR(15)
         ELSE
            @ 1, 1 SAY CHR(18)
         ENDIF
         @ 2, 40 SAY '        LEDGER FROM : '
         @ 2, 65 SAY SDATE
         @ 2, 75 SAY 'To'
         @ 2, 79 SAY LDATE
         @ 2, 115 SAY 'page: '
         @ 2, 122 SAY PAGE PICTURE '99'
         @ 3, 1 SAY REPLICATE('-',129)
         @ 4, 10 SAY 'ACCOUNT CODE: '
         WPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ 4, 24 SAY WPC1
         @ 4, 26 SAY '-'
         WPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ 4, 27 SAY WPC2
         @ 4, 29 SAY '-'
         WPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ 4, 30 SAY WPC3
         @ 4, 45 SAY 'TITLE OF ACCOUNT:- '
         @ 4, 65 SAY WTITLE
         @ 5, 1 SAY REPLICATE('-',129)
         @ 6, 1 SAY '<----- VOUCHER ----->'
         @ 6, 72 SAY '<----- TRANSACTIONS ----->'
         @ 7, 1 SAY '  DATE        NO.               DESCRIPTION'
         @ 7, 75 SAY '   DEBIT         CREDIT          BALANCE      REMARKS'
         @ 8, 1 SAY REPLICATE('=',129)
         HEAD= .F.
      ENDIF
      SELECT 1
      IF SR_NO=1
         @ LINE, 1 SAY DATE
         @ LINE, 22 SAY DESC
         @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
         @ LINE, 117 SAY STATUS
         RMK= ' Opn. Bal.'
         @ LINE, 119 SAY RMK
         LINE= LINE+1
         MBLAMT= BL_AMOUNT
         MCSTAT= STATUS
         SKIP
         IF C1<>MC1.or.C2<>MC2.or.C3<>MC3
            EXIT
         ENDIF
      ELSE
         OPBAL= 0
         IF MCNT=0
            IF DR_AMOUNT>0
               IF STATUS='DR'
                  IF DR_AMOUNT<=BL_AMOUNT
                     OPBAL= BL_AMOUNT-DR_AMOUNT
                     OPST= 'DR'
                  ELSE
                     OPBAL= DR_AMOUNT-BL_AMOUNT
                     OPST= 'CR'
                  ENDIF
               ELSE
                  OPBAL= BL_AMOUNT+DR_AMOUNT
                  OPST= 'CR'
               ENDIF
            ENDIF
            IF CR_AMOUNT>0
               IF STATUS='CR'
                  IF CR_AMOUNT<=BL_AMOUNT
                     OPBAL= BL_AMOUNT-CR_AMOUNT
                     OPST= 'CR'
                  ELSE
                     OPBAL= CR_AMOUNT-BL_AMOUNT
                     OPST= 'DR'
                  ENDIF
               ELSE
                  OPBAL= BL_AMOUNT+CR_AMOUNT
                  OPST= 'DR'
               ENDIF
            ENDIF
            @ LINE, 25 SAY 'Opening Balance'
            @ LINE, 100 SAY OPBAL PICTURE '9,999,999,999.99'
            @ LINE, 117 SAY OPST
            @ LINE, 120 SAY ' Opn. Bal.'
            LINE= LINE+1
         ENDIF
      ENDIF
      MCNT= 1
      T= '   '
      DO CASE
         CASE VCH_TYPE=1
            T= 'CR-'
         CASE VCH_TYPE=2
            T= 'CP-'
         CASE VCH_TYPE=3
            T= 'BR-'
         CASE VCH_TYPE=4
            T= 'BP-'
         CASE VCH_TYPE=5
            T= 'JV-'
         CASE VCH_TYPE=6
            T= 'SV-'
         CASE VCH_TYPE=7
            T= 'PV-'
      ENDCASE
      @ LINE, 1 SAY DATE
      MVCH1= '  '
      MVCH2= '    '
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
      @ LINE, 12 SAY T
      @ LINE, 15 SAY MVCH1
      @ LINE, 17 SAY '-'
      @ LINE, 18 SAY MVCH2
      @ LINE, 25 SAY DESC
      IF DR_AMOUNT<>0
         @ LINE, 70 SAY DR_AMOUNT PICTURE '9,999,999,999.99'
         TDR= TDR+DR_AMOUNT
      ENDIF
      IF CR_AMOUNT<>0
         @ LINE, 84 SAY CR_AMOUNT PICTURE '9,999,999,999.99'
         TCR= TCR+CR_AMOUNT
      ENDIF
      @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
      @ LINE, 117 SAY STATUS
      CNT= CNT+1
      MCONT= MCONT+1
      LINE= LINE+1
      IF LINE>55
         PAGE= PAGE+1
         HEAD= .T.
      ENDIF
      MCSTAT= STATUS
      MBLAMT= BL_AMOUNT
      SKIP
   ENDDO
   LINE= LINE+2
   @ LINE, 5 SAY 'Total Transactions = '
   @ LINE, 25 SAY CNT PICTURE '99999'
   @ LINE, 70 SAY TDR PICTURE '9,999,999,999.99'
   @ LINE, 84 SAY TCR PICTURE '9,999,999,999.99'
   @ LINE, 100 SAY MBLAMT PICTURE '9,999,999,999.99'
   @ LINE, 117 SAY MCSTAT
   RMK= ' Cls. Bal.'
   @ LINE, 119 SAY RMK
   LINE= LINE+2
   @ LINE, 5 SAY 'R e m a r k s     -  '
   @ LINE, 25 SAY MREM1
   LINE= LINE+1
   @ LINE, 25 SAY MREM2
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLEAR
CLEAR ALL
RETURN

PROCEDURE LDGR-SP2
CLEAR
CLOSE ALL
SET DATE brit
SET CENTURY OFF
SELECT 1
USE master
SELECT 2
USE ldgrcode INDEX ac1
STORE SPACE(65) TO MREM1, MREM2
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   STORE 0 TO TCR, TDR, MBLAMT, MCSTAT
   @ 2, 2 SAY 'Selective Ledger Printing '
   STORE DATE() TO SDATE, LDATE
   @ 23, 50 SAY 'Press Esc to Return '
   STORE 0 TO MC1, MC2, MC3, TDR, TCR, MNO
   @ 6, 10 SAY 'Enter  Code  for  Printing:'
   SET COLOR TO /w
   @ 6, 40 SAY '  -  -'
   SET COLOR TO 7/1
   @ 6, 40 GET MC1 PICTURE '99'
   @ 6, 43 GET MC2 PICTURE '99'
   @ 6, 46 GET MC3 PICTURE '999'
   READ
   IF READKEY()=12
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 23, 1 CLEAR TO 23, 79
   SELECT 2
   GOTO TOP
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist ... Please Re-try  '
      WAIT ''
      @ 23, 10 SAY SPACE(25)
      LOOP
   ELSE
      STORE TITLE TO WTITLE
   ENDIF
   @ 8, 10 SAY 'TITLE:'
   SET COLOR TO /w
   @ 8, 19 SAY WTITLE
   MREM= REMARKS
   SET COLOR TO 7/1
   @ 10, 10 SAY 'From Date ' GET SDATE
   @ 12, 10 SAY 'To.. Date ' GET LDATE
   READ
   @ 13, 10 SAY 'R e m a r k s   '
   @ 14, 10 GET MREM1
   @ 15, 10 GET MREM2
   READ
   OPTN= ' '
   @ 16, 1 TO 16, 79
   @ 17, 25 SAY 'Y O U R   P A P E R   W I D T H'
   @ 19, 25 SAY '      A -  10 Inches  '
   @ 20, 25 SAY '      B -  15 Inches  '
   @ 21, 25 SAY '      Q -  Quit       '
   DO WHILE .not. OPTN$'ABQ'
      @ 23, 31 SAY 'Your Choice  (A/B/Q)' GET OPTN FUNCTION '!'
      READ
   ENDDO
   IF OPTN='Q'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   PANS= ' '
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing In Progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
   HEAD= .T.
   PAGE= 1
   SELECT 1
   STORE 0 TO CNT, MCONT, MBLAMT, MCSTAT
   SET FILTER TO DATE>=SDATE.and.DATE<=LDATE.and.C1=MC1.and.C2=MC2.and.C3=MC3
   IF RECCOUNT()=0
      @ 10, 10 SAY 'There is No Transaction Between These Dates'
      @ 24, 25 SAY 'Press Any Key To Return'
      WAIT ''
      @ 10, 10 SAY SPACE(60)
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      CLEAR ALL
      EXIT
   ENDIF
   GOTO TOP
   LINE= 10
   MCNT= 0
   DO WHILE .not. EOF()
      IF HEAD
         LINE= 10
         IF OPTN='A'
            DO top-prt1
         ELSE
            DO top-prt2
         ENDIF
         IF OPTN='A'
            @ 1, 1 SAY CHR(15)
         ELSE
            @ 1, 1 SAY CHR(18)
         ENDIF
         @ 2, 40 SAY '        LEDGER FROM : '
         @ 2, 65 SAY SDATE
         @ 2, 75 SAY 'To'
         @ 2, 79 SAY LDATE
         @ 2, 115 SAY 'page: '
         @ 2, 122 SAY PAGE PICTURE '99'
         @ 3, 1 SAY REPLICATE('-',129)
         @ 4, 10 SAY 'ACCOUNT CODE: '
         WPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ 4, 24 SAY WPC1
         @ 4, 26 SAY '-'
         WPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ 4, 27 SAY WPC2
         @ 4, 29 SAY '-'
         WPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ 4, 30 SAY WPC3
         @ 4, 45 SAY 'TITLE OF ACCOUNT:- '
         @ 4, 65 SAY WTITLE
         @ 5, 1 SAY REPLICATE('-',129)
         @ 6, 1 SAY '<----- VOUCHER ----->'
         @ 6, 72 SAY '<----- TRANSACTIONS ----->'
         @ 7, 1 SAY '  DATE        NO.               DESCRIPTION'
         @ 7, 75 SAY '   DEBIT         CREDIT          BALANCE      REMARKS'
         @ 8, 1 SAY REPLICATE('=',129)
         HEAD= .F.
      ENDIF
      SELECT 1
      IF SR_NO=1
         @ LINE, 1 SAY DATE
         @ LINE, 22 SAY DESC
         @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
         @ LINE, 117 SAY STATUS
         RMK= ' Opn. Bal.'
         @ LINE, 119 SAY RMK
         LINE= LINE+1
         MBLAMT= BL_AMOUNT
         MCSTAT= STATUS
         SKIP
         IF C1<>MC1.or.C2<>MC2.or.C3<>MC3
            EXIT
         ENDIF
      ELSE
         OPBAL= 0
         IF MCNT=0
            IF DR_AMOUNT>0
               IF STATUS='DR'
                  IF DR_AMOUNT<=BL_AMOUNT
                     OPBAL= BL_AMOUNT-DR_AMOUNT
                     OPST= 'DR'
                  ELSE
                     OPBAL= DR_AMOUNT-BL_AMOUNT
                     OPST= 'CR'
                  ENDIF
               ELSE
                  OPBAL= BL_AMOUNT+DR_AMOUNT
                  OPST= 'CR'
               ENDIF
            ENDIF
            IF CR_AMOUNT>0
               IF STATUS='CR'
                  IF CR_AMOUNT<=BL_AMOUNT
                     OPBAL= BL_AMOUNT-CR_AMOUNT
                     OPST= 'CR'
                  ELSE
                     OPBAL= CR_AMOUNT-BL_AMOUNT
                     OPST= 'DR'
                  ENDIF
               ELSE
                  OPBAL= BL_AMOUNT+CR_AMOUNT
                  OPST= 'DR'
               ENDIF
            ENDIF
            @ LINE, 25 SAY 'Opening Balance'
            @ LINE, 100 SAY OPBAL PICTURE '9,999,999,999.99'
            @ LINE, 117 SAY OPST
            @ LINE, 120 SAY ' Opn. Bal.'
            LINE= LINE+1
         ENDIF
      ENDIF
      MCNT= 1
      T= '   '
      DO CASE
         CASE VCH_TYPE=1
            T= 'CR-'
         CASE VCH_TYPE=2
            T= 'CP-'
         CASE VCH_TYPE=3
            T= 'BR-'
         CASE VCH_TYPE=4
            T= 'BP-'
         CASE VCH_TYPE=5
            T= 'JV-'
         CASE VCH_TYPE=6
            T= 'SV-'
         CASE VCH_TYPE=7
            T= 'PV-'
      ENDCASE
      @ LINE, 1 SAY DATE
      MVCH1= '  '
      MVCH2= '    '
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
      @ LINE, 12 SAY T
      @ LINE, 15 SAY MVCH1
      @ LINE, 17 SAY '-'
      @ LINE, 18 SAY MVCH2
      @ LINE, 25 SAY DESC
      IF DR_AMOUNT<>0
         @ LINE, 70 SAY DR_AMOUNT PICTURE '9,999,999,999.99'
         TDR= TDR+DR_AMOUNT
      ENDIF
      IF CR_AMOUNT<>0
         @ LINE, 84 SAY CR_AMOUNT PICTURE '9,999,999,999.99'
         TCR= TCR+CR_AMOUNT
      ENDIF
      @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
      @ LINE, 117 SAY STATUS
      LINE= LINE+1
      @ LINE, 25 SAY DESC1
      CNT= CNT+1
      MCONT= MCONT+1
      LINE= LINE+1
      IF LINE>55
         PAGE= PAGE+1
         HEAD= .T.
      ENDIF
      MCSTAT= STATUS
      MBLAMT= BL_AMOUNT
      SKIP
   ENDDO
   LINE= LINE+2
   @ LINE, 5 SAY 'Total Transactions = '
   @ LINE, 25 SAY CNT PICTURE '99999'
   @ LINE, 70 SAY TDR PICTURE '9,999,999,999.99'
   @ LINE, 84 SAY TCR PICTURE '9,999,999,999.99'
   @ LINE, 100 SAY MBLAMT PICTURE '9,999,999,999.99'
   @ LINE, 117 SAY MCSTAT
   RMK= ' Cls. Bal.'
   @ LINE, 119 SAY RMK
   LINE= LINE+2
   @ LINE, 5 SAY 'R e m a r k s     -  '
   @ LINE, 25 SAY MREM1
   LINE= LINE+1
   @ LINE, 25 SAY MREM2
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLEAR
CLEAR ALL
RETURN

PROCEDURE LDGR-FP
CLEAR
CLOSE ALL
SET DATE brit
SELECT 3
USE master INDEX ac2
SELECT 2
USE ldgrcode INDEX ac1
PG_HDNG= .T.
PAGE= 1
LINE= 8
GOTO TOP
SELECT 1
RMK= '             '
STORE 0 TO MC1, MC2, MC3, OPBAL, MCNT
STORE 0 TO MCC1, MCC2, MCC3, TDR, TCR
OPST= '  '
@ 5, 30 SAY 'Ledger Printing '
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   STORE DATE() TO SDATE, LDATE
   @ 23, 55 SAY 'Press Ctrl Q To Quit '
   @ 10, 10 SAY 'From Ledger Head    -  -'
   @ 10, 28 GET MC1 PICTURE '99'
   @ 10, 31 GET MC2 PICTURE '99'
   @ 10, 34 GET MC3 PICTURE '999'
   READ
   IF READKEY()=12
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 23, 1
   MCC1= MC1
   MCC2= MC2
   @ 12, 10 SAY 'To                  -  -'
   SET COLOR TO /w
   @ 12, 28 SAY MCC1 PICTURE '99'
   @ 12, 31 SAY MCC2 PICTURE '99'
   SET COLOR TO 7/1 
   @ 12, 34 GET MCC3 PICTURE '999'
   READ
   IF MCC3<MC3
      LOOP
   ENDIF
   @ 14, 10 SAY 'From Date        ' GET SDATE
   @ 16, 10 SAY 'To.. Date        ' GET LDATE
   READ
   EXIT
ENDDO
SELECT 3
SET FILTER TO C1=MC1.and.C2=MC2
COPY TO ldgr FOR DATE>=SDATE.and.DATE<=LDATE.and.C1=MC1.and.C2=MC2.and.C3>=MC3.and.C3<=MCC3
SELECT 1
USE ldgr
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ldgr
GOTO TOP
P= ' '
DO WHILE .not. P$'CQ'
   @ 23, 55 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
OPTN= ' '
CLEAR
@ 11, 25 SAY 'Y O U R   P A P E R   W I D T H'
@ 14, 25 SAY '      A -  10 Inches  '
@ 15, 25 SAY '      B -  15 Inches'
DO WHILE .not. OPTN$'AB'
   @ 17, 35 SAY 'Your Choice  (A/B)' GET OPTN FUNCTION '!'
   READ
ENDDO
CLEAR
PANS= ' '
DO prnchk
IF PANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
@ 11, 20 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
STORE SPACE(50) TO WTITLE
DO WHILE .not. EOF()
   SELECT 1
   IF BL_AMOUNT=0.and.DR_AMOUNT=0.and.CR_AMOUNT=0
      SKIP
      LOOP
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
   WC1= C1
   WC2= C2
   WC3= C3
   SELECT 2
   SEEK STR(WC1,2)+STR(WC2,2)+STR(WC3,3)
   WTITLE=  B->TITLE
   IF PG_HDNG.or.WC1<>MC1.or.WC2<>MC2.or.WC3<>MC3
      LINE= 10
      IF OPTN='A'
         DO top-prt1
      ELSE
         DO top-prt2
      ENDIF
      IF OPTN='A'
         @ PROW(), PCOL() SAY CHR(15)
      ELSE
         @ PROW(), PCOL() SAY CHR(18)
      ENDIF
      @ 2, 40 SAY '        LEDGER FROM:'
      @ 2, 65 SAY SDATE
      @ 2, 75 SAY 'To'
      @ 2, 79 SAY LDATE
      @ 2, 115 SAY 'page: '
      @ 2, 122 SAY PAGE PICTURE '99'
      @ 3, 1 SAY REPLICATE('-',129)
      @ 4, 10 SAY 'ACCOUNT CODE: '
      WPC1= REPLICATE('0',2-LEN(LTRIM(STR(WC1))))+LTRIM(STR(WC1))
      @ 4, 24 SAY WPC1
      @ 4, 26 SAY '-'
      WPC2= REPLICATE('0',2-LEN(LTRIM(STR(WC2))))+LTRIM(STR(WC2))
      @ 4, 27 SAY WPC2
      @ 4, 29 SAY '-'
      WPC3= REPLICATE('0',4-LEN(LTRIM(STR(WC3))))+LTRIM(STR(WC3))
      @ 4, 30 SAY WPC3
      @ 4, 45 SAY 'TITLE OF ACCOUNT:- '
      @ 4, 65 SAY WTITLE
      MREM= REMARKS
      @ 5, 1 SAY REPLICATE('-',129)
      @ 6, 1 SAY '<----- VOUCHER ----->'
      @ 6, 72 SAY '<----- TRANSACTIONS ----->'
      @ 7, 1 SAY '  DATE        NO.               DESCRIPTION'
      @ 7, 65 SAY ' DEBIT             CREDIT                 BALANCE      REMARKS'
      @ 8, 1 SAY REPLICATE('=',129)
   ENDIF
   SELECT 1
   IF SR_NO=1
      @ LINE, 1 SAY DATE
      @ LINE, 22 SAY DESC
      @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
      @ LINE, 117 SAY STATUS
      RMK= ' Opn. Bal.'
      @ LINE, 119 SAY RMK
      LINE= LINE+1
      MBLAMT= BL_AMOUNT
      MCSTAT= STATUS
      SKIP
      IF C1<>WC1.or.C2<>WC2.or.C3<>WC3
         EXIT
      ENDIF
   ELSE
      OPBAL= 0
      IF MCNT=0
         IF DR_AMOUNT>0
            IF STATUS='DR'
               IF DR_AMOUNT<=BL_AMOUNT
                  OPBAL= BL_AMOUNT-DR_AMOUNT
                  OPST= 'DR'
               ELSE
                  OPBAL= DR_AMOUNT-BL_AMOUNT
                  OPST= 'CR'
               ENDIF
            ELSE
               OPBAL= BL_AMOUNT+DR_AMOUNT
               OPST= 'CR'
            ENDIF
         ENDIF
         IF CR_AMOUNT>0
            IF STATUS='CR'
               IF CR_AMOUNT<=BL_AMOUNT
                  OPBAL= BL_AMOUNT-CR_AMOUNT
                  OPST= 'CR'
               ELSE
                  OPBAL= CR_AMOUNT-BL_AMOUNT
                  OPST= 'DR'
               ENDIF
            ELSE
               OPBAL= BL_AMOUNT+CR_AMOUNT
               OPST= 'DR'
            ENDIF
         ENDIF
         @ LINE, 25 SAY 'Opening Balance'
         @ LINE, 100 SAY OPBAL PICTURE '9,999,999,999.99'
         @ LINE, 117 SAY OPST
         @ LINE, 120 SAY ' Opn. Bal.'
         LINE= LINE+1
      ENDIF
   ENDIF
   MCNT= 1
   T= '   '
   DO CASE
      CASE VCH_TYPE=1
         T= 'CR-'
      CASE VCH_TYPE=2
         T= 'CP-'
      CASE VCH_TYPE=3
         T= 'BR-'
      CASE VCH_TYPE=4
         T= 'BP-'
      CASE VCH_TYPE=5
         T= 'JV-'
      CASE VCH_TYPE=6
         T= 'SV-'
      CASE VCH_TYPE=7
         T= 'PV-'
   ENDCASE
   @ LINE, 1 SAY DATE
   MVCH1= '  '
   MVCH2= '    '
   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
   @ LINE, 12 SAY T
   @ LINE, 15 SAY MVCH1
   @ LINE, 17 SAY '-'
   @ LINE, 18 SAY MVCH2
   @ LINE, 25 SAY DESC
   IF DR_AMOUNT>0
      @ LINE, 58 SAY DR_AMOUNT PICTURE '9,999,999,999.99'
      TDR= TDR+DR_AMOUNT
   ENDIF
   IF CR_AMOUNT>0
      @ LINE, 78 SAY CR_AMOUNT PICTURE '9,999,999,999.99'
      TCR= TCR+CR_AMOUNT
   ENDIF
   @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
   @ LINE, 117 SAY STATUS
   MCNT= MCNT+1
   LINE= LINE+1
   MC1= WC1
   MC2= WC2
   MC3= WC3
   IF LINE>56
      PAGE= PAGE+1
      LINE= 8
      PG_HDNG= .T.
   ELSE
      PG_HDNG= .F.
   ENDIF
   SKIP
   IF C1<>WC1.or.C2<>WC2.or.C3<>WC3
      LINE= LINE+2
      SKIP  -1
      @ LINE, 5 SAY 'Total Transactions = '
      @ LINE, 25 SAY MCNT PICTURE '999999'
      @ LINE, 58 SAY TDR PICTURE '9,999,999,999.99'
      @ LINE, 78 SAY TCR PICTURE '9,999,999,999.99'
      @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
      @ LINE, 117 SAY STATUS
      RMK= ' Cls. Bal.'
      @ LINE, 119 SAY RMK
      LINE= LINE+2
      @ LINE, 5 SAY 'R e m a r k s     -  '
      @ LINE, 25 SAY MREM
      EJECT
      STORE 0 TO TDR, TCR
      MCNT= 0
      SKIP
   ENDIF
ENDDO
RUN del ldgr.*
SET PRINTER OFF
SET DEVICE TO SCREEN
CLEAR
CLEAR ALL
RETURN

PROCEDURE LDGR-FP2
CLEAR
CLOSE ALL
SET DATE brit
SELECT 3
USE master INDEX ac2
SELECT 2
USE ldgrcode INDEX ac1
PG_HDNG= .T.
PAGE= 1
LINE= 8
GOTO TOP
SELECT 1
RMK= '             '
STORE 0 TO MC1, MC2, MC3, OPBAL, MCNT
STORE 0 TO MCC1, MCC2, MCC3, TDR, TCR
OPST= '  '
@ 5, 30 SAY 'Ledger Printing '
DO WHILE .T.
   SET COLOR TO 7/1 
   CLEAR
   STORE DATE() TO SDATE, LDATE
   @ 23, 55 SAY 'Press Ctrl Q To Quit '
   @ 10, 10 SAY 'From Ledger Head    -  -'
   @ 10, 28 GET MC1 PICTURE '99'
   @ 10, 31 GET MC2 PICTURE '99'
   @ 10, 34 GET MC3 PICTURE '999'
   READ
   IF READKEY()=12
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 23, 1
   MCC1= MC1
   MCC2= MC2
   @ 12, 10 SAY 'To                  -  -'
   SET COLOR TO /w
   @ 12, 28 SAY MCC1 PICTURE '99'
   @ 12, 31 SAY MCC2 PICTURE '99'
   SET COLOR TO 7/1 
   @ 12, 34 GET MCC3 PICTURE '999'
   READ
   IF MCC3<MC3
      LOOP
   ENDIF
   @ 14, 10 SAY 'From Date        ' GET SDATE
   @ 16, 10 SAY 'To.. Date        ' GET LDATE
   READ
   EXIT
ENDDO
SELECT 3
SET FILTER TO C1=MC1.and.C2=MC2
COPY TO ldgr FOR DATE>=SDATE.and.DATE<=LDATE.and.C1=MC1.and.C2=MC2.and.C3>=MC3.and.C3<=MCC3
SELECT 1
USE ldgr
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ldgr
GOTO TOP
P= ' '
DO WHILE .not. P$'CQ'
   @ 23, 55 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
OPTN= ' '
CLEAR
@ 11, 25 SAY 'Y O U R   P A P E R   W I D T H'
@ 14, 25 SAY '      A -  10 Inches  '
@ 15, 25 SAY '      B -  15 Inches'
DO WHILE .not. OPTN$'AB'
   @ 17, 35 SAY 'Your Choice  (A/B)' GET OPTN FUNCTION '!'
   READ
ENDDO
CLEAR
PANS= ' '
DO prnchk
IF PANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
@ 11, 20 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
STORE SPACE(50) TO WTITLE
DO WHILE .not. EOF()
   SELECT 1
   IF BL_AMOUNT=0.and.DR_AMOUNT=0.and.CR_AMOUNT=0
      SKIP
      LOOP
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
   WC1= C1
   WC2= C2
   WC3= C3
   SELECT 2
   SEEK STR(WC1,2)+STR(WC2,2)+STR(WC3,3)
   WTITLE=  B->TITLE
   IF PG_HDNG.or.WC1<>MC1.or.WC2<>MC2.or.WC3<>MC3
      LINE= 10
      IF OPTN='A'
         DO top-prt1
      ELSE
         DO top-prt2
      ENDIF
      IF OPTN='A'
         @ PROW(), PCOL() SAY CHR(15)
      ELSE
         @ PROW(), PCOL() SAY CHR(18)
      ENDIF
      @ 2, 40 SAY '        LEDGER FROM:'
      @ 2, 65 SAY SDATE
      @ 2, 75 SAY 'To'
      @ 2, 79 SAY LDATE
      @ 2, 115 SAY 'page: '
      @ 2, 122 SAY PAGE PICTURE '99'
      @ 3, 1 SAY REPLICATE('-',129)
      @ 4, 10 SAY 'ACCOUNT CODE: '
      WPC1= REPLICATE('0',2-LEN(LTRIM(STR(WC1))))+LTRIM(STR(WC1))
      @ 4, 24 SAY WPC1
      @ 4, 26 SAY '-'
      WPC2= REPLICATE('0',2-LEN(LTRIM(STR(WC2))))+LTRIM(STR(WC2))
      @ 4, 27 SAY WPC2
      @ 4, 29 SAY '-'
      WPC3= REPLICATE('0',4-LEN(LTRIM(STR(WC3))))+LTRIM(STR(WC3))
      @ 4, 30 SAY WPC3
      @ 4, 45 SAY 'TITLE OF ACCOUNT:- '
      @ 4, 65 SAY WTITLE
      MREM= REMARKS
      @ 5, 1 SAY REPLICATE('-',129)
      @ 6, 1 SAY '<----- VOUCHER ----->'
      @ 6, 72 SAY '<----- TRANSACTIONS ----->'
      @ 7, 1 SAY '  DATE        NO.               DESCRIPTION'
      @ 7, 65 SAY ' DEBIT             CREDIT                 BALANCE      REMARKS'
      @ 8, 1 SAY REPLICATE('=',129)
   ENDIF
   SELECT 1
   IF SR_NO=1
      @ LINE, 1 SAY DATE
      @ LINE, 22 SAY DESC
      @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
      @ LINE, 117 SAY STATUS
      RMK= ' Opn. Bal.'
      @ LINE, 119 SAY RMK
      LINE= LINE+1
      MBLAMT= BL_AMOUNT
      MCSTAT= STATUS
      SKIP
      IF C1<>WC1.or.C2<>WC2.or.C3<>WC3
         EXIT
      ENDIF
   ELSE
      OPBAL= 0
      IF MCNT=0
         IF DR_AMOUNT>0
            IF STATUS='DR'
               IF DR_AMOUNT<=BL_AMOUNT
                  OPBAL= BL_AMOUNT-DR_AMOUNT
                  OPST= 'DR'
               ELSE
                  OPBAL= DR_AMOUNT-BL_AMOUNT
                  OPST= 'CR'
               ENDIF
            ELSE
               OPBAL= BL_AMOUNT+DR_AMOUNT
               OPST= 'CR'
            ENDIF
         ENDIF
         IF CR_AMOUNT>0
            IF STATUS='CR'
               IF CR_AMOUNT<=BL_AMOUNT
                  OPBAL= BL_AMOUNT-CR_AMOUNT
                  OPST= 'CR'
               ELSE
                  OPBAL= CR_AMOUNT-BL_AMOUNT
                  OPST= 'DR'
               ENDIF
            ELSE
               OPBAL= BL_AMOUNT+CR_AMOUNT
               OPST= 'DR'
            ENDIF
         ENDIF
         @ LINE, 25 SAY 'Opening Balance'
         @ LINE, 100 SAY OPBAL PICTURE '9,999,999,999.99'
         @ LINE, 117 SAY OPST
         @ LINE, 120 SAY ' Opn. Bal.'
         LINE= LINE+1
      ENDIF
   ENDIF
   MCNT= 1
   T= '   '
   DO CASE
      CASE VCH_TYPE=1
         T= 'CR-'
      CASE VCH_TYPE=2
         T= 'CP-'
      CASE VCH_TYPE=3
         T= 'BR-'
      CASE VCH_TYPE=4
         T= 'BP-'
      CASE VCH_TYPE=5
         T= 'JV-'
      CASE VCH_TYPE=6
         T= 'SV-'
      CASE VCH_TYPE=7
         T= 'PV-'
   ENDCASE
   @ LINE, 1 SAY DATE
   MVCH1= '  '
   MVCH2= '    '
   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
   @ LINE, 12 SAY T
   @ LINE, 15 SAY MVCH1
   @ LINE, 17 SAY '-'
   @ LINE, 18 SAY MVCH2
   @ LINE, 25 SAY DESC
   IF DR_AMOUNT>0
      @ LINE, 58 SAY DR_AMOUNT PICTURE '9,999,999,999.99'
      TDR= TDR+DR_AMOUNT
   ENDIF
   IF CR_AMOUNT>0
      @ LINE, 78 SAY CR_AMOUNT PICTURE '9,999,999,999.99'
      TCR= TCR+CR_AMOUNT
   ENDIF
   @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
   @ LINE, 117 SAY STATUS
   MCNT= MCNT+1
   LINE= LINE+1
   @ LINE, 25 SAY DESC1
   LINE= LINE+1
   MC1= WC1
   MC2= WC2
   MC3= WC3
   IF LINE>56
      PAGE= PAGE+1
      LINE= 8
      PG_HDNG= .T.
   ELSE
      PG_HDNG= .F.
   ENDIF
   SKIP
   IF C1<>WC1.or.C2<>WC2.or.C3<>WC3
      LINE= LINE+2
      SKIP  -1
      @ LINE, 5 SAY 'Total Transactions = '
      @ LINE, 25 SAY MCNT PICTURE '999999'
      @ LINE, 58 SAY TDR PICTURE '9,999,999,999.99'
      @ LINE, 78 SAY TCR PICTURE '9,999,999,999.99'
      @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
      @ LINE, 117 SAY STATUS
      RMK= ' Cls. Bal.'
      @ LINE, 119 SAY RMK
      LINE= LINE+2
      @ LINE, 5 SAY 'R e m a r k s     -  '
      @ LINE, 25 SAY MREM
      EJECT
      STORE 0 TO TDR, TCR
      MCNT= 0
      SKIP
   ENDIF
ENDDO
RUN del ldgr.*
SET PRINTER OFF
SET DEVICE TO SCREEN
CLEAR
CLEAR ALL
RETURN

PROCEDURE LDGR-SDB
CLEAR
SET CENTURY OFF
SELECT 1
USE master INDEX ac2
SELECT 2
USE ldgrcode INDEX ac1
STORE 0 TO MC1, MC2, MC3
PG_HDNG= .T.
PAGE= 1
LINE= 8
WTITLE= SPACE(30)
GOTO TOP
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   STORE DATE() TO SDATE, LDATE
   @ 23, 1 SAY 'Press Esc To Return '
   @ 10, 10 SAY 'Enter  Code  for  Display:     -  -'
   @ 10, 40 GET MC1 PICTURE '99'
   @ 10, 43 GET MC2 PICTURE '99'
   @ 10, 46 GET MC3 PICTURE '999'
   READ
   IF READKEY()=12
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   SELECT 2
   GOTO TOP
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 22, 10 SAY 'code is not in code file'
      WAIT ''
      @ 22, 10 SAY '                           '
      LOOP
   ENDIF
   WTITLE=  B->TITLE
   @ 12, 10 SAY 'TITLE:'
   SET COLOR TO /w
   @ 12, 19 SAY WTITLE
   SET COLOR TO 7/1
   @ 14, 10 SAY 'Date  ' GET SDATE
   READ
   EXIT
ENDDO
P= ' '
DO WHILE .not. P$'CQ'
   @ 19, 10 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CNT= 0
RMK= '             '
CLEAR
SELECT 1
SET FILTER TO DATE<=SDATE.and.C1=MC1.and.C2=MC2.and.C3=MC3
GOTO TOP
DO WHILE .not. EOF()
   IF BL_AMOUNT=0
      SKIP
      LOOP
   ENDIF
   CNT= CNT+1
   WC1= MC1
   WC2= MC2
   WC3= MC3
   IF PG_HDNG
      LINE= 8
      DO screen
   ENDIF
   SELECT 1
   IF EOF()
      EXIT
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
   IF CNT=1
      WDESC= LTRIM(DESC)
      RMK= ' Opn. Bal.'
      SKIP
      IF C1<>WC1.or.C2<>WC2.or.C3<>WC3
         LINE= LINE+2
         SKIP  -1
         RMK= ' Cls. Bal.'
         EXIT
      ENDIF
   ENDIF
   WDESC= LTRIM(DESC)
   IF DR_AMOUNT>0
   ENDIF
   IF CR_AMOUNT>0
   ENDIF
   T= '  '
   DO CASE
      CASE VCH_TYPE=1
         T= 'CR-'
      CASE VCH_TYPE=2
         T= 'CP-'
      CASE VCH_TYPE=3
         T= 'BR-'
      CASE VCH_TYPE=4
         T= 'BP-'
      CASE VCH_TYPE=5
         T= 'JV-'
      CASE VCH_TYPE=6
         T= 'SV-'
      CASE VCH_TYPE=7
         T= 'PV-'
   ENDCASE
   LINE= LINE+1
   IF LINE>23
      PAGE= PAGE+1
      LINE= 8
      PG_HDNG= .T.
      P= ' '
      IF P='Q'
         CLOSE ALL
         CLEAR
         RETURN
      ENDIF
   ELSE
      PG_HDNG= .F.
   ENDIF
   SKIP
   IF C1<>WC1.or.C2<>WC2.or.C3<>WC3
      LINE= 5
      SKIP  -1
      @ LINE, 10 SAY 'Balance Upto '
      @ LINE, 40 SAY SDATE
      LINE= LINE+2
      @ LINE, 10 SAY WTITLE
      @ LINE, 55 SAY BL_AMOUNT PICTURE '999999999.99'
      @ LINE, 68 SAY STATUS
      RMK= ' Cls. Bal.'
      @ LINE, 70 SAY RMK
      EXIT
   ENDIF
ENDDO
IF EOF()
   LINE= LINE+2
   SKIP  -1
   RMK= ' Cls. Bal.'
ENDIF
@ 24, 50 SAY 'Press Any key To Return..'
WAIT ''
CLEAR
CLOSE ALL
RETURN

PROCEDURE LDGRASP
CLEAR
CLOSE ALL
SET DATE brit
SET CENTURY OFF
SELECT 1
USE master
SELECT 2
USE ldgrcode INDEX ac1
STORE SPACE(65) TO MREM1, MREM2
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   STORE 0 TO TCR, TDR, MBLAMT, MCSTAT
   @ 2, 2 SAY 'Selective Ledger (With Address) Printing '
   STORE DATE() TO SDATE, LDATE
   @ 23, 50 SAY 'Press Esc to Return '
   STORE 0 TO MC1, MC2, MC3, TDR, TCR, MNO
   @ 6, 10 SAY 'Enter  Code  for  Printing:'
   SET COLOR TO /w
   @ 6, 40 SAY '  -  -'
   SET COLOR TO 7/1
   @ 6, 40 GET MC1 PICTURE '99'
   @ 6, 43 GET MC2 PICTURE '99'
   @ 6, 46 GET MC3 PICTURE '999'
   READ
   IF READKEY()=12
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 23, 1 CLEAR TO 23, 79
   SELECT 2
   GOTO TOP
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exist ... Please Re-try  '
      WAIT ''
      @ 23, 10 SAY SPACE(25)
      LOOP
   ELSE
      STORE TITLE TO WTITLE
      MADD1= ADDRESS1
      MADD2= ADDRESS2
   ENDIF
   @ 8, 10 SAY 'TITLE:'
   SET COLOR TO /w
   @ 8, 19 SAY WTITLE
   SET COLOR TO 7/1
   @ 10, 10 SAY 'From Date ' GET SDATE
   @ 12, 10 SAY 'To.. Date ' GET LDATE
   READ
   @ 13, 10 SAY 'R e m a r k s   '
   @ 14, 10 GET MREM1
   @ 15, 10 GET MREM2
   READ
   OPTN= ' '
   @ 16, 1 TO 16, 79
   @ 17, 25 SAY 'Y O U R   P A P E R   W I D T H'
   @ 19, 25 SAY '      A -  10 Inches  '
   @ 20, 25 SAY '      B -  15 Inches  '
   @ 21, 25 SAY '      Q -  Quit       '
   DO WHILE .not. OPTN$'ABQ'
      @ 23, 31 SAY 'Your Choice  (A/B/Q)' GET OPTN FUNCTION '!'
      READ
   ENDDO
   IF OPTN='Q'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   PANS= ' '
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing In Progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
   HEAD= .T.
   PAGE= 1
   SELECT 1
   STORE 0 TO CNT, MCONT, MBLAMT, MCSTAT
   SET FILTER TO DATE>=SDATE.and.DATE<=LDATE.and.C1=MC1.and.C2=MC2.and.C3=MC3
   IF RECCOUNT()=0
      @ 10, 10 SAY 'There is No Transaction Between These Dates'
      @ 24, 25 SAY 'Press Any Key To Return'
      WAIT ''
      @ 10, 10 SAY SPACE(60)
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      CLEAR ALL
      EXIT
   ENDIF
   GOTO TOP
   MCNT= 0
   DO WHILE .not. EOF()
      IF HEAD
         LINE= 11
         IF OPTN='A'
            DO top-prt1
         ELSE
            DO top-prt2
         ENDIF
         IF OPTN='A'
            @ 1, 1 SAY CHR(15)
         ELSE
            @ 1, 1 SAY CHR(18)
         ENDIF
         @ 2, 40 SAY '        LEDGER FROM : '
         @ 2, 65 SAY SDATE
         @ 2, 75 SAY 'To'
         @ 2, 79 SAY LDATE
         @ 2, 115 SAY 'page: '
         @ 2, 122 SAY PAGE PICTURE '99'
         @ 3, 1 SAY REPLICATE('-',129)
         @ 4, 10 SAY 'ACCOUNT CODE: '
         WPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ 4, 24 SAY WPC1
         @ 4, 26 SAY '-'
         WPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ 4, 27 SAY WPC2
         @ 4, 29 SAY '-'
         WPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ 4, 30 SAY WPC3
         @ 4, 45 SAY 'TITLE OF ACCOUNT:- '
         @ 4, 65 SAY WTITLE
         @ 5, 65 SAY MADD1
         @ 6, 65 SAY MADD2
         @ 7, 1 SAY REPLICATE('-',129)
         @ 8, 1 SAY '<----- VOUCHER ----->'
         @ 8, 72 SAY '<----- TRANSACTIONS ----->'
         @ 9, 1 SAY '  DATE        NO.               DESCRIPTION'
         @ 9, 75 SAY '   DEBIT         CREDIT          BALANCE      REMARKS'
         @ 10, 1 SAY REPLICATE('=',129)
         HEAD= .F.
      ENDIF
      SELECT 1
      IF SR_NO=1
         @ LINE, 1 SAY DATE
         @ LINE, 22 SAY DESC
         @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
         @ LINE, 117 SAY STATUS
         RMK= ' Opn. Bal.'
         @ LINE, 119 SAY RMK
         LINE= LINE+1
         MBLAMT= BL_AMOUNT
         MCSTAT= STATUS
         SKIP
         IF C1<>MC1.or.C2<>MC2.or.C3<>MC3
            EXIT
         ENDIF
      ELSE
         OPBAL= 0
         IF MCNT=0
            IF DR_AMOUNT>0
               IF STATUS='DR'
                  IF DR_AMOUNT<=BL_AMOUNT
                     OPBAL= BL_AMOUNT-DR_AMOUNT
                     OPST= 'DR'
                  ELSE
                     OPBAL= DR_AMOUNT-BL_AMOUNT
                     OPST= 'CR'
                  ENDIF
               ELSE
                  OPBAL= BL_AMOUNT+DR_AMOUNT
                  OPST= 'CR'
               ENDIF
            ENDIF
            IF CR_AMOUNT>0
               IF STATUS='CR'
                  IF CR_AMOUNT<=BL_AMOUNT
                     OPBAL= BL_AMOUNT-CR_AMOUNT
                     OPST= 'CR'
                  ELSE
                     OPBAL= CR_AMOUNT-BL_AMOUNT
                     OPST= 'DR'
                  ENDIF
               ELSE
                  OPBAL= BL_AMOUNT+CR_AMOUNT
                  OPST= 'DR'
               ENDIF
            ENDIF
            @ LINE, 25 SAY 'Opening Balance'
            @ LINE, 100 SAY OPBAL PICTURE '9,999,999,999.99'
            @ LINE, 117 SAY OPST
            @ LINE, 120 SAY ' Opn. Bal.'
            LINE= LINE+1
         ENDIF
      ENDIF
      MCNT= 1
      T= '   '
      DO CASE
         CASE VCH_TYPE=1
            T= 'CR-'
         CASE VCH_TYPE=2
            T= 'CP-'
         CASE VCH_TYPE=3
            T= 'BR-'
         CASE VCH_TYPE=4
            T= 'BP-'
         CASE VCH_TYPE=5
            T= 'JV-'
         CASE VCH_TYPE=6
            T= 'SV-'
         CASE VCH_TYPE=7
            T= 'PV-'
      ENDCASE
      @ LINE, 1 SAY DATE
      MVCH1= '  '
      MVCH2= '    '
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
      @ LINE, 12 SAY T
      @ LINE, 15 SAY MVCH1
      @ LINE, 17 SAY '-'
      @ LINE, 18 SAY MVCH2
      @ LINE, 25 SAY DESC
      IF DR_AMOUNT<>0
         @ LINE, 70 SAY DR_AMOUNT PICTURE '9,999,999,999.99'
         TDR= TDR+DR_AMOUNT
      ENDIF
      IF CR_AMOUNT<>0
         @ LINE, 84 SAY CR_AMOUNT PICTURE '9,999,999,999.99'
         TCR= TCR+CR_AMOUNT
      ENDIF
      @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
      @ LINE, 117 SAY STATUS
      CNT= CNT+1
      MCONT= MCONT+1
      LINE= LINE+1
      IF LINE>55
         PAGE= PAGE+1
         HEAD= .T.
      ENDIF
      MCSTAT= STATUS
      MBLAMT= BL_AMOUNT
      SKIP
   ENDDO
   LINE= LINE+2
   @ LINE, 5 SAY 'Total Transactions = '
   @ LINE, 25 SAY CNT PICTURE '99999'
   @ LINE, 70 SAY TDR PICTURE '9,999,999,999.99'
   @ LINE, 84 SAY TCR PICTURE '9,999,999,999.99'
   @ LINE, 100 SAY MBLAMT PICTURE '9,999,999,999.99'
   @ LINE, 117 SAY MCSTAT
   RMK= ' Cls. Bal.'
   @ LINE, 119 SAY RMK
   LINE= LINE+2
   @ LINE, 5 SAY 'R e m a r k s     -  '
   @ LINE, 25 SAY MREM1
   LINE= LINE+1
   @ LINE, 25 SAY MREM2
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDDO
CLEAR
CLEAR ALL
RETURN

PROCEDURE LDGRAFP
CLEAR
CLOSE ALL
SET DATE brit
SELECT 3
USE master INDEX ac2
SELECT 2
USE ldgrcode INDEX ac1
PG_HDNG= .T.
PAGE= 1
LINE= 8
GOTO TOP
SELECT 1
RMK= '             '
STORE 0 TO MC1, MC2, MC3, OPBAL, MCNT
STORE 0 TO MCC1, MCC2, MCC3, TDR, TCR
OPST= '  '
@ 5, 30 SAY 'Ledger Printing '
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   STORE DATE() TO SDATE, LDATE
   @ 23, 55 SAY 'Press Ctrl Q To Quit '
   @ 10, 10 SAY 'From Ledger Head    -  -'
   @ 10, 28 GET MC1 PICTURE '99'
   @ 10, 31 GET MC2 PICTURE '99'
   @ 10, 34 GET MC3 PICTURE '999'
   READ
   IF READKEY()=12
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 23, 1
   MCC1= MC1
   MCC2= MC2
   @ 12, 10 SAY 'To                  -  -'
   SET COLOR TO /w
   @ 12, 28 SAY MCC1 PICTURE '99'
   @ 12, 31 SAY MCC2 PICTURE '99'
   SET COLOR TO 7/1
   @ 12, 34 GET MCC3 PICTURE '999'
   READ
   IF MCC3<MC3
      LOOP
   ENDIF
   @ 14, 10 SAY 'From Date        ' GET SDATE
   @ 16, 10 SAY 'To.. Date        ' GET LDATE
   READ
   EXIT
ENDDO
SELECT 3
SET FILTER TO C1=MC1.and.C2=MC2
COPY TO ldgr FOR DATE>=SDATE.and.DATE<=LDATE.and.C1=MC1.and.C2=MC2.and.C3>=MC3.and.C3<=MCC3
SELECT 1
USE ldgr
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ldgr
GOTO TOP
P= ' '
DO WHILE .not. P$'CQ'
   @ 23, 55 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
OPTN= ' '
CLEAR
@ 11, 25 SAY 'Y O U R   P A P E R   W I D T H'
@ 14, 25 SAY '      A -  10 Inches  '
@ 15, 25 SAY '      B -  15 Inches'
DO WHILE .not. OPTN$'AB'
   @ 17, 35 SAY 'Your Choice  (A/B)' GET OPTN FUNCTION '!'
   READ
ENDDO
CLEAR
PANS= ' '
DO prnchk
IF PANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
@ 11, 20 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
STORE SPACE(50) TO WTITLE
DO WHILE .not. EOF()
   SELECT 1
   IF BL_AMOUNT=0.and.DR_AMOUNT=0.and.CR_AMOUNT=0
      SKIP
      LOOP
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
   WC1= C1
   WC2= C2
   WC3= C3
   SELECT 2
   SEEK STR(WC1,2)+STR(WC2,2)+STR(WC3,3)
   WTITLE=  B->TITLE
   MADD1=  B->ADDRESS1
   MADD2=  B->ADDRESS2
   IF PG_HDNG.or.WC1<>MC1.or.WC2<>MC2.or.WC3<>MC3
      LINE= 11
      IF OPTN='A'
         DO top-prt1
      ELSE
         DO top-prt2
      ENDIF
      IF OPTN='A'
         @ PROW(), PCOL() SAY CHR(15)
      ELSE
         @ PROW(), PCOL() SAY CHR(18)
      ENDIF
      @ 2, 40 SAY 'GENERAL LEDGER FROM:'
      @ 2, 65 SAY SDATE
      @ 2, 75 SAY 'To'
      @ 2, 79 SAY LDATE
      @ 2, 115 SAY 'page: '
      @ 2, 122 SAY PAGE PICTURE '99'
      @ 3, 1 SAY REPLICATE('-',129)
      @ 4, 10 SAY 'ACCOUNT CODE: '
      WPC1= REPLICATE('0',2-LEN(LTRIM(STR(WC1))))+LTRIM(STR(WC1))
      @ 4, 24 SAY WPC1
      @ 4, 26 SAY '-'
      WPC2= REPLICATE('0',2-LEN(LTRIM(STR(WC2))))+LTRIM(STR(WC2))
      @ 4, 27 SAY WPC2
      @ 4, 29 SAY '-'
      WPC3= REPLICATE('0',4-LEN(LTRIM(STR(WC3))))+LTRIM(STR(WC3))
      @ 4, 30 SAY WPC3
      @ 4, 45 SAY 'TITLE OF ACCOUNT:- '
      @ 4, 65 SAY WTITLE
      @ 5, 65 SAY MADD1
      @ 6, 65 SAY MADD2
      MREM= REMARKS
      @ 7, 1 SAY REPLICATE('-',129)
      @ 8, 1 SAY '<----- VOUCHER ----->'
      @ 8, 72 SAY '<----- TRANSACTIONS ----->'
      @ 9, 1 SAY '  DATE        NO.               DESCRIPTION'
      @ 9, 65 SAY ' DEBIT             CREDIT                 BALANCE      REMARKS'
      @ 10, 1 SAY REPLICATE('=',129)
   ENDIF
   SELECT 1
   OPBAL= 0
   IF MCNT=0
      IF DR_AMOUNT>0
         IF STATUS='DR'
            IF DR_AMOUNT<=BL_AMOUNT
               OPBAL= BL_AMOUNT-DR_AMOUNT
               OPST= 'DR'
            ELSE
               OPBAL= DR_AMOUNT-BL_AMOUNT
               OPST= 'CR'
            ENDIF
         ELSE
            OPBAL= BL_AMOUNT+DR_AMOUNT
            OPST= 'CR'
         ENDIF
      ENDIF
      IF CR_AMOUNT>0
         IF STATUS='CR'
            IF CR_AMOUNT<=BL_AMOUNT
               OPBAL= BL_AMOUNT-CR_AMOUNT
               OPST= 'CR'
            ELSE
               OPBAL= CR_AMOUNT-BL_AMOUNT
               OPST= 'DR'
            ENDIF
         ELSE
            OPBAL= BL_AMOUNT+CR_AMOUNT
            OPST= 'DR'
         ENDIF
      ENDIF
      @ LINE, 25 SAY 'Opening Balance'
      @ LINE, 100 SAY OPBAL PICTURE '9,999,999,999.99'
      @ LINE, 117 SAY OPST
      @ LINE, 120 SAY ' Opn. Bal.'
      LINE= LINE+1
   ENDIF
   T= '   '
   DO CASE
      CASE VCH_TYPE=1
         T= 'CR-'
      CASE VCH_TYPE=2
         T= 'CP-'
      CASE VCH_TYPE=3
         T= 'BR-'
      CASE VCH_TYPE=4
         T= 'BP-'
      CASE VCH_TYPE=5
         T= 'JV-'
      CASE VCH_TYPE=6
         T= 'SV-'
      CASE VCH_TYPE=7
         T= 'PV-'
   ENDCASE
   @ LINE, 1 SAY DATE
   MVCH1= '  '
   MVCH2= '    '
   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
   @ LINE, 12 SAY T
   @ LINE, 15 SAY MVCH1
   @ LINE, 17 SAY '-'
   @ LINE, 18 SAY MVCH2
   @ LINE, 25 SAY DESC
   IF DR_AMOUNT>0
      @ LINE, 58 SAY DR_AMOUNT PICTURE '9,999,999,999.99'
      TDR= TDR+DR_AMOUNT
   ENDIF
   IF CR_AMOUNT>0
      @ LINE, 78 SAY CR_AMOUNT PICTURE '9,999,999,999.99'
      TCR= TCR+CR_AMOUNT
   ENDIF
   @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
   @ LINE, 117 SAY STATUS
   MCNT= MCNT+1
   LINE= LINE+1
   MC1= WC1
   MC2= WC2
   MC3= WC3
   IF LINE>56
      PAGE= PAGE+1
      LINE= 8
      PG_HDNG= .T.
   ELSE
      PG_HDNG= .F.
   ENDIF
   SKIP
   IF C1<>WC1.or.C2<>WC2.or.C3<>WC3
      LINE= LINE+2
      SKIP  -1
      @ LINE, 5 SAY 'Total Transactions = '
      @ LINE, 25 SAY MCNT PICTURE '999999'
      @ LINE, 58 SAY TDR PICTURE '9,999,999,999.99'
      @ LINE, 78 SAY TCR PICTURE '9,999,999,999.99'
      @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999.99'
      @ LINE, 117 SAY STATUS
      RMK= ' Cls. Bal.'
      @ LINE, 119 SAY RMK
      LINE= LINE+2
      @ LINE, 5 SAY 'R e m a r k s     -  '
      @ LINE, 25 SAY MREM
      EJECT
      STORE 0 TO TDR, TCR
      MCNT= 0
      SKIP
   ENDIF
ENDDO
RUN del ldgr.*
SET PRINTER OFF
SET DEVICE TO SCREEN
CLEAR
CLEAR ALL
RETURN

PROCEDURE REP-REC
SET CONFIRM OFF
HEADING= .T.
PAGE= 1
L= 2
MTOT= 0
SELECT 2
USE ldgrcode INDEX ac1
STORE 0 TO MC1, MC2, MC3, TDR, TCR, MBL_AMOUNT
STORE 0 TO MDRTOT, MCRTOT
STORE 0 TO MNO
STORE ' ' TO MSTATUS
WTITLE= SPACE(30)
CNT= 0
RMK= '             '
SET COLOR TO 7/1
CLEAR
ZEROBAL= ' '
DO WHILE .not. ZEROBAL$'YN'
   @ 4, 2 SAY 'Do You Want To Include Ledger Heads Having Zero Balance - Y/N ' GET ZEROBAL FUNCTION '!'
   READ
ENDDO
CLEAR
ANS= ' '
SELECT 2
DO WHILE .T.
   SET COLOR TO +7/1
   @ 8, 14 SAY 'Accounts Receivable Report '
   SET COLOR TO 7/1
   @ 10, 14 SAY 'Please give "First Two Level Code" of Debtors '
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts....'
   SET COLOR TO /w
   @ 11, 67 SAY '-'
   SET COLOR TO 7/1
   @ 11, 65 GET MC1 PICTURE '99'
   @ 11, 68 GET MC2 PICTURE '99'
   @ 11, 70 SAY '-xxx'
   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SELECT 1
USE master INDEX mastdate
GOTO BOTTOM
STORE DATE TO MTDATE
CLEAR
@ 2, 5 SAY 'Accounts Receivable Report '
@ 5, 5 SAY 'Your last posted voucher is dated '
@ 5, 42 SAY MTDATE
@ 6, 5 SAY 'Do you want to dispaly same date on your Report '
@ 7, 5 SAY 'Or enter the date you want to dispaly  ' GET MTDATE
READ
SELECT 2
DO WHILE .not. ANS$'PSQ'
   @ 23, 40 SAY 'Printer/Screen/Quit -  P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
IF ANS='P'
   CLEAR
   @ 2, 5 SAY 'No Of Copies       ' GET MNO PICTURE '99'
   READ
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
IF ANS='S'
   MNO= 1
ENDIF
ABC= 1
DO WHILE ABC<=MNO
   HEADING= .T.
   PAGE= 1
   L= 2
   STORE 0 TO MDRTOT, MCRTOT, MTOT
   SELECT 2
   SET FILTER TO C1=MC1.and.C2=MC2
   GOTO TOP
   DO WHILE .not. EOF()
      IF ZEROBAL='N'
         IF LDGR_BAL=0
            SKIP
            LOOP
         ENDIF
      ENDIF
      IF HEADING
         IF ANS='S'
            DO screen
         ENDIF
         IF ANS='P'
            DO top-prt1
         ENDIF
         L= L+1
         @ L, 28 SAY 'RECEIVABLE  REPORT'
         L= L+1
         @ L, 1 SAY 'As On : '
         @ L, 10 SAY MTDATE
         @ L, 70 SAY 'Page: '
         @ L, 76 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Acc Code    Description'
         @ L, 57 SAY 'Dr'
         @ L, 72 SAY 'Cr'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         HEADING= .F.
      ENDIF
      IF C3=0
         SKIP
         LOOP
      ENDIF
      WTITLE= TITLE
      MBL_AMOUNT= LDGR_BAL
      MSTAT= STATUS
      WC1= C1
      WC2= C2
      WC3= C3
      L= L+1
      WPC1= REPLICATE('0',2-LEN(LTRIM(STR(WC1))))+LTRIM(STR(WC1))
      @ L, 1 SAY WPC1
      @ L, 3 SAY '-'
      WPC2= REPLICATE('0',2-LEN(LTRIM(STR(WC2))))+LTRIM(STR(WC2))
      @ L, 4 SAY WPC2
      @ L, 6 SAY '-'
      WPC3= REPLICATE('0',3-LEN(LTRIM(STR(WC3))))+LTRIM(STR(WC3))
      @ L, 7 SAY WPC3
      @ L, 13 SAY WTITLE PICTURE 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      IF MSTAT='DR'
         @ L, 50 SAY ABS(MBL_AMOUNT) PICTURE '99,999,999,999.99'
         MDRTOT= MDRTOT+MBL_AMOUNT
      ELSE
         @ L, 63 SAY ABS(MBL_AMOUNT) PICTURE '99,999,999,999.99'
         MCRTOT= MCRTOT+MBL_AMOUNT
      ENDIF
      IF ANS='P'
         @ L, 1 SAY REPLICATE('_',79)
      ENDIF
      IF .not. EOF()
         SKIP
      ENDIF
      IF ANS='S'.and.L>20
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            SET CONFIRM OFF
            @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         PAGE= PAGE+1
         L= 2
         HEADING= .T.
      ENDIF
      IF ANS='P'.and.L>=55
         L= 2
         PAGE= PAGE+1
         HEADING= .T.
      ENDIF
   ENDDO
   L= L+1
   @ L, 40 SAY REPLICATE('-',40)
   L= L+1
   @ L, 40 SAY 'Total'
   @ L, 50 SAY MDRTOT PICTURE '99999999,999.99'
   @ L, 65 SAY MCRTOT PICTURE '99999999,999.99'
   L= L+1
   @ L, 40 SAY REPLICATE('-',40)
   IF ANS='S'
      @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
      WAIT ''
   ENDIF
   IF ANS='P'
      EJECT
   ENDIF
   ABC= ABC+1
ENDDO
IF ANS='P'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
SET FILTER TO
CLOSE ALL
RETURN

PROCEDURE REP-REC1
SET CONFIRM OFF
HEADING= .T.
PAGE= 1
L= 2
MTOT= 0
SELECT 2
USE ldgrcode INDEX ac1
STORE 0 TO MC1, MC2, MC3, TDR, TCR, MBL_AMOUNT
STORE 0 TO MDRTOT, MCRTOT
STORE 0 TO MNO
STORE ' ' TO MSTATUS
WTITLE= SPACE(30)
CNT= 0
RMK= '             '
SET COLOR TO 7/1
CLEAR
ZEROBAL= ' '
DO WHILE .not. ZEROBAL$'YN'
   @ 4, 2 SAY 'Do You Want To Include Ledger Heads Having Zero Balance - Y/N ' GET ZEROBAL FUNCTION '!'
   READ
ENDDO
CLEAR
ANS= ' '
SELECT 2
DO WHILE .T.
   SET COLOR TO +7/1
   @ 8, 14 SAY 'Accounts Receivable Report (With Address)'
   SET COLOR TO 7/1
   @ 10, 14 SAY 'Please give "First Two Level Code" of Debtors '
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts....'
   SET COLOR TO /w
   @ 11, 67 SAY '-'
   SET COLOR TO 7/1
   @ 11, 65 GET MC1 PICTURE '99'
   @ 11, 68 GET MC2 PICTURE '99'
   @ 11, 70 SAY '-xxx'
   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SELECT 1
USE master INDEX mastdate
GOTO BOTTOM
STORE DATE TO MTDATE
CLEAR
@ 2, 5 SAY 'Accounts Receivable Report (With Address) '
@ 5, 5 SAY 'Your last posted voucher is dated '
@ 5, 42 SAY MTDATE
@ 6, 5 SAY 'Do you want to dispaly same date on your Report '
@ 7, 5 SAY 'Or enter the date you want to dispaly  ' GET MTDATE
READ
SELECT 2
DO WHILE .not. ANS$'PSQ'
   @ 23, 40 SAY 'Printer/Screen/Quit -  P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
IF ANS='P'
   CLEAR
   @ 2, 5 SAY 'No Of Copies       ' GET MNO PICTURE '99'
   READ
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
IF ANS='S'
   MNO= 1
ENDIF
ABC= 1
DO WHILE ABC<=MNO
   HEADING= .T.
   PAGE= 1
   L= 2
   STORE 0 TO MDRTOT, MCRTOT, MTOT
   SELECT 2
   SET FILTER TO C1=MC1.and.C2=MC2
   GOTO TOP
   DO WHILE .not. EOF()
      IF ZEROBAL='N'
         IF LDGR_BAL=0
            SKIP
            LOOP
         ENDIF
      ENDIF
      IF HEADING
         IF ANS='S'
            DO screen
         ENDIF
         IF ANS='P'
            DO top-prt1
         ENDIF
         L= L+1
         @ L, 25 SAY 'RECEIVABLE REPORT (With Address) '
         L= L+1
         @ L, 1 SAY 'As On : '
         @ L, 10 SAY MTDATE
         @ L, 70 SAY 'Page: '
         @ L, 76 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Acc Code    Description / Address '
         @ L, 57 SAY 'Dr'
         @ L, 72 SAY 'Cr'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         HEADING= .F.
      ENDIF
      IF C3=0
         SKIP
         LOOP
      ENDIF
      WTITLE= TITLE
      MBL_AMOUNT= LDGR_BAL
      MSTAT= STATUS
      WC1= C1
      WC2= C2
      WC3= C3
      L= L+1
      WPC1= REPLICATE('0',2-LEN(LTRIM(STR(WC1))))+LTRIM(STR(WC1))
      @ L, 1 SAY WPC1
      @ L, 3 SAY '-'
      WPC2= REPLICATE('0',2-LEN(LTRIM(STR(WC2))))+LTRIM(STR(WC2))
      @ L, 4 SAY WPC2
      @ L, 6 SAY '-'
      WPC3= REPLICATE('0',3-LEN(LTRIM(STR(WC3))))+LTRIM(STR(WC3))
      @ L, 7 SAY WPC3
      @ L, 13 SAY WTITLE PICTURE 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      IF MSTAT='DR'
         @ L, 50 SAY ABS(MBL_AMOUNT) PICTURE '99,999,999,999.99'
         MDRTOT= MDRTOT+MBL_AMOUNT
      ELSE
         @ L, 63 SAY ABS(MBL_AMOUNT) PICTURE '99,999,999,999.99'
         MCRTOT= MCRTOT+MBL_AMOUNT
      ENDIF
      L= L+1
      @ L, 13 SAY ADDRESS1
      L= L+1
      @ L, 13 SAY ADDRESS2
      L= L+1
      IF .not. EOF()
         SKIP
      ENDIF
      IF ANS='S'.and.L>20
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            SET CONFIRM OFF
            @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         PAGE= PAGE+1
         L= 2
         HEADING= .T.
      ENDIF
      IF ANS='P'.and.L>=55
         L= 2
         PAGE= PAGE+1
         HEADING= .T.
      ENDIF
   ENDDO
   L= L+1
   @ L, 40 SAY REPLICATE('-',40)
   L= L+1
   @ L, 40 SAY 'Total'
   @ L, 50 SAY MDRTOT PICTURE '99999999,999.99'
   @ L, 65 SAY MCRTOT PICTURE '99999999,999.99'
   L= L+1
   @ L, 40 SAY REPLICATE('-',40)
   IF ANS='S'
      @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
      WAIT ''
   ENDIF
   IF ANS='P'
      EJECT
   ENDIF
   ABC= ABC+1
ENDDO
IF ANS='P'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
SET FILTER TO
CLOSE ALL
RETURN

PROCEDURE REP-PAY
SET CONFIRM OFF
HEADING= .T.
PAGE= 1
L= 2
MTOT= 0
SELECT 2
USE ldgrcode INDEX ac1
STORE 0 TO MC1, MC2, MC3, TDR, TCR, MBL_AMOUNT, MDRTOT, MCRTOT
STORE 0 TO MNO
STORE ' ' TO MSTATUS
WTITLE= SPACE(30)
CNT= 0
RMK= '             '
SET COLOR TO 7/1 
CLEAR
ZEROBAL= ' '
DO WHILE .not. ZEROBAL$'YN'
   @ 4, 2 SAY 'Do You Want To Include Ledger Heads Having Zero Balance - Y/N ' GET ZEROBAL FUNCTION '!'
   READ
ENDDO
CLEAR
ANS= ' '
SELECT 2
DO WHILE .T.
   SET COLOR TO +7/1
   @ 8, 14 SAY 'Accounts Payable Report '
   SET COLOR TO 7/1
   @ 10, 14 SAY 'Please give "First Two Level Code" of Creditors '
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts....'
   SET COLOR TO /w
   @ 11, 67 SAY '-'
   SET COLOR TO 7/1
   @ 11, 65 GET MC1 PICTURE '99'
   @ 11, 68 GET MC2 PICTURE '99'
   @ 11, 70 SAY '-xxx'
   SET COLOR TO 7/1
   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SELECT 1
USE master INDEX mastdate
GOTO BOTTOM
STORE DATE TO MTDATE
CLEAR
@ 2, 5 SAY 'Accounts Payable Report '
@ 5, 5 SAY 'Your last posted voucher is dated '
@ 5, 42 SAY MTDATE
@ 6, 5 SAY 'Do you want to dispaly same date on your Report '
@ 7, 5 SAY 'Or enter the date you want to dispaly  ' GET MTDATE
READ
SELECT 2
DO WHILE .not. ANS$'PSQ'
   @ 23, 40 SAY 'Printer/Screen/Quit -  P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
IF ANS='P'
   CLEAR
   @ 2, 5 SAY 'No Of Copies       ' GET MNO PICTURE '99'
   READ
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
IF ANS='S'
   MNO= 1
ENDIF
ABC= 1
DO WHILE ABC<=MNO
   HEADING= .T.
   PAGE= 1
   L= 2
   STORE 0 TO MDRTOT, MCRTOT, MTOT
   SELECT 2
   SET FILTER TO C1=MC1.and.C2=MC2
   GOTO TOP
   STORE C2 TO MWC2
   STORE 0 TO MSCRTOT, MSDRTOT, NXT
   DO WHILE .not. EOF()
      IF ZEROBAL='N'
         IF LDGR_BAL=0
            SKIP
            LOOP
         ENDIF
      ENDIF
      IF HEADING
         IF ANS='S'
            DO screen
         ENDIF
         IF ANS='P'
            DO top-prt1
         ENDIF
         L= L+1
         @ L, 30 SAY 'PAYABLE  REPORT'
         L= L+1
         @ L, 1 SAY 'As On : '
         @ L, 10 SAY MTDATE
         @ L, 70 SAY 'Page: '
         @ L, 76 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Acc Code    Description'
         @ L, 57 SAY 'Dr'
         @ L, 72 SAY 'Cr'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         HEADING= .F.
      ENDIF
      IF C3=0
         SKIP
         LOOP
      ENDIF
      WTITLE= TITLE
      MBL_AMOUNT= LDGR_BAL
      MSTAT= STATUS
      WC1= C1
      WC2= C2
      WC3= C3
      L= L+1
      WPC1= REPLICATE('0',2-LEN(LTRIM(STR(WC1))))+LTRIM(STR(WC1))
      @ L, 1 SAY WPC1
      @ L, 3 SAY '-'
      WPC2= REPLICATE('0',2-LEN(LTRIM(STR(WC2))))+LTRIM(STR(WC2))
      @ L, 4 SAY WPC2
      @ L, 6 SAY '-'
      WPC3= REPLICATE('0',3-LEN(LTRIM(STR(WC3))))+LTRIM(STR(WC3))
      @ L, 7 SAY WPC3
      @ L, 13 SAY WTITLE PICTURE 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      IF MSTAT='DR'
         @ L, 50 SAY ABS(MBL_AMOUNT) PICTURE '99,999,999,999.99'
         MDRTOT= MDRTOT+MBL_AMOUNT
         MSDRTOT= MSDRTOT+MBL_AMOUNT
      ELSE
         @ L, 63 SAY ABS(MBL_AMOUNT) PICTURE '99,999,999,999.99'
         MCRTOT= MCRTOT+MBL_AMOUNT
         MSCRTOT= MSCRTOT+MBL_AMOUNT
      ENDIF
      IF ANS='P'
         @ L, 1 SAY REPLICATE('_',79)
      ENDIF
      IF .not. EOF()
         SKIP
      ENDIF
      IF ANS='S'.and.L>20
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            SET CONFIRM OFF
            @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         PAGE= PAGE+1
         L= 2
         HEADING= .T.
      ENDIF
      IF ANS='P'.and.L>55
         PAGE= PAGE+1
         HEADING= .T.
         L= 2
      ENDIF
   ENDDO
   L= L+1
   @ L, 40 SAY REPLICATE('-',40)
   L= L+1
   @ L, 40 SAY 'Total'
   @ L, 50 SAY MDRTOT PICTURE '99999999,999.99'
   @ L, 65 SAY MCRTOT PICTURE '99999999,999.99'
   L= L+1
   @ L, 40 SAY REPLICATE('-',40)
   IF ANS='S'
      @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
      WAIT ''
   ENDIF
   IF ANS='P'
      EJECT
   ENDIF
   ABC= ABC+1
ENDDO
IF ANS='P'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
SET FILTER TO
CLOSE ALL
RETURN

PROCEDURE REP-PAY1
SET CONFIRM OFF
HEADING= .T.
PAGE= 1
L= 2
MTOT= 0
SELECT 2
USE ldgrcode INDEX ac1
STORE 0 TO MC1, MC2, MC3, TDR, TCR, MBL_AMOUNT, MDRTOT, MCRTOT
STORE 0 TO MNO
STORE ' ' TO MSTATUS
WTITLE= SPACE(30)
CNT= 0
RMK= '             '
SET COLOR TO 7/1 
CLEAR
ZEROBAL= ' '
DO WHILE .not. ZEROBAL$'YN'
   @ 4, 2 SAY 'Do You Want To Include Ledger Heads Having Zero Balance - Y/N ' GET ZEROBAL FUNCTION '!'
   READ
ENDDO
CLEAR
ANS= ' '
SELECT 2
DO WHILE .T.
   SET COLOR TO +7/1
   @ 8, 14 SAY 'Accounts Payable Report (With Address) '
   SET COLOR TO 7/1
   @ 10, 14 SAY 'Please give "First Two Level Code" of Creditors '
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts....'
   SET COLOR TO /w
   @ 11, 67 SAY '-'
   SET COLOR TO 7/1
   @ 11, 65 GET MC1 PICTURE '99'
   @ 11, 68 GET MC2 PICTURE '99'
   @ 11, 70 SAY '-xxx'
   SET COLOR TO 7/1
   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SELECT 1
USE master INDEX mastdate
GOTO BOTTOM
STORE DATE TO MTDATE
CLEAR
@ 2, 5 SAY 'Accounts Payable Report (With Address) '
@ 5, 5 SAY 'Your last posted voucher is dated '
@ 5, 42 SAY MTDATE
@ 6, 5 SAY 'Do you want to dispaly same date on your Report '
@ 7, 5 SAY 'Or enter the date you want to dispaly  ' GET MTDATE
READ
SELECT 2
DO WHILE .not. ANS$'PSQ'
   @ 23, 40 SAY 'Printer/Screen/Quit -  P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
IF ANS='P'
   CLEAR
   @ 2, 5 SAY 'No Of Copies       ' GET MNO PICTURE '99'
   READ
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
IF ANS='S'
   MNO= 1
ENDIF
ABC= 1
DO WHILE ABC<=MNO
   HEADING= .T.
   PAGE= 1
   L= 2
   STORE 0 TO MDRTOT, MCRTOT, MTOT
   SELECT 2
   SET FILTER TO C1=MC1.and.C2=MC2
   GOTO TOP
   STORE C2 TO MWC2
   STORE 0 TO MSCRTOT, MSDRTOT, NXT
   DO WHILE .not. EOF()
      IF ZEROBAL='N'
         IF LDGR_BAL=0
            SKIP
            LOOP
         ENDIF
      ENDIF
      IF HEADING
         IF ANS='S'
            DO screen
         ENDIF
         IF ANS='P'
            DO top-prt1
         ENDIF
         L= L+1
         @ L, 25 SAY 'PAYABLE REPORT (With Address) '
         L= L+1
         @ L, 1 SAY 'As On : '
         @ L, 10 SAY MTDATE
         @ L, 70 SAY 'Page: '
         @ L, 76 SAY PAGE PICTURE '99'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Acc Code    Description / Address '
         @ L, 57 SAY 'Dr'
         @ L, 72 SAY 'Cr'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         HEADING= .F.
      ENDIF
      IF C3=0
         SKIP
         LOOP
      ENDIF
      WTITLE= TITLE
      MBL_AMOUNT= LDGR_BAL
      MSTAT= STATUS
      WC1= C1
      WC2= C2
      WC3= C3
      L= L+1
      WPC1= REPLICATE('0',2-LEN(LTRIM(STR(WC1))))+LTRIM(STR(WC1))
      @ L, 1 SAY WPC1
      @ L, 3 SAY '-'
      WPC2= REPLICATE('0',2-LEN(LTRIM(STR(WC2))))+LTRIM(STR(WC2))
      @ L, 4 SAY WPC2
      @ L, 6 SAY '-'
      WPC3= REPLICATE('0',3-LEN(LTRIM(STR(WC3))))+LTRIM(STR(WC3))
      @ L, 7 SAY WPC3
      @ L, 13 SAY WTITLE PICTURE 'xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx'
      IF MSTAT='DR'
         @ L, 50 SAY ABS(MBL_AMOUNT) PICTURE '99,999,999,999.99'
         MDRTOT= MDRTOT+MBL_AMOUNT
         MSDRTOT= MSDRTOT+MBL_AMOUNT
      ELSE
         @ L, 63 SAY ABS(MBL_AMOUNT) PICTURE '99,999,999,999.99'
         MCRTOT= MCRTOT+MBL_AMOUNT
         MSCRTOT= MSCRTOT+MBL_AMOUNT
      ENDIF
      L= L+1
      @ L, 13 SAY ADDRESS1
      L= L+1
      @ L, 13 SAY ADDRESS2
      L= L+1
      IF .not. EOF()
         SKIP
      ENDIF
      IF ANS='S'.and.L>20
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            SET CONFIRM OFF
            @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         PAGE= PAGE+1
         L= 2
         HEADING= .T.
      ENDIF
      IF ANS='P'.and.L>55
         PAGE= PAGE+1
         HEADING= .T.
         L= 2
      ENDIF
   ENDDO
   L= L+1
   @ L, 40 SAY REPLICATE('-',40)
   L= L+1
   @ L, 40 SAY 'Total'
   @ L, 50 SAY MDRTOT PICTURE '99999999,999.99'
   @ L, 65 SAY MCRTOT PICTURE '99999999,999.99'
   L= L+1
   @ L, 40 SAY REPLICATE('-',40)
   IF ANS='S'
      @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
      WAIT ''
   ENDIF
   IF ANS='P'
      EJECT
   ENDIF
   ABC= ABC+1
ENDDO
IF ANS='P'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
SET FILTER TO
CLOSE ALL
RETURN

PROCEDURE REPRPAY
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
SELECT 1
USE master INDEX ac2
SELECT 2
USE ldgrcode INDEX ac1
SET DATE brit
DO WHILE .T.
   CLEAR
   STORE 0 TO MC1, MC2
   STORE 0 TO T1, T2, T3, T4, T5, T6
   STORE DATE() TO FDATE, TDATE
   SET COLOR TO +7/1
   @ 6, 14 SAY 'Accounts Payable/Receivable Report  '
   SET COLOR TO 7/1
   @ 8, 14 SAY 'From Date ................. ' GET FDATE
   @ 9, 14 SAY 'To   Date ................. ' GET TDATE
   @ 11, 14 SAY 'Please give "First Two Level Code"  '
   @ 12, 14 SAY 'that you have Defined in your Chart of Accounts....'
   SET COLOR TO /w
   @ 12, 67 SAY '-'
   SET COLOR TO 7/1
   @ 12, 65 GET MC1 PICTURE '99'
   @ 12, 68 GET MC2 PICTURE '99'
   @ 12, 70 SAY '-xxx'
   SET COLOR TO 7/1
   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=FDATE.and.DATE<=TDATE.and.C1=MC1.and.C2=MC2
   COUNT TO RCON
   IF RCON=0
      CLEAR
      @ 10, 10 SAY 'There is No Transaction Between These Dates'
      @ 24, 25 SAY 'Press Any Key To Return'
      WAIT ''
      CLEAR
      LOOP
   ENDIF
   EXIT
ENDDO
GOTO TOP
ANS= ' '
DO WHILE .not. ANS$'PQ'
   @ 23, 50 SAY 'Printer/Quit -  P/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
@ 11, 20 SAY 'Please wait ... Printing in progress '
SET PRINTER ON
SET DEVICE TO PRINTER
HEAD= .T.
PG= 0
LINE= 2
DO WHILE .not. EOF()
   IF HEAD
      DO top-prt1
      LINE= 2
      @ LINE, 23 SAY 'PAYABLE/RECEIVABLE REPORT (BETWEEN DATES)'
      LINE= LINE+1
      @ LINE, 1 SAY CHR(15)
      @ LINE, 1 SAY REPLICATE('=',132)
      LINE= LINE+1
      @ LINE, 1 SAY 'Code'
      @ LINE, 12 SAY 'Title'
      @ LINE, 43 SAY '|'
      @ LINE, 45 SAY 'Opening Balance '
      @ LINE, 74 SAY '|'
      @ LINE, 76 SAY 'Transactions '
      @ LINE, 106 SAY '|'
      @ LINE, 108 SAY 'Closing Balance '
      LINE= LINE+1
      @ LINE, 43 SAY '|'
      @ LINE, 45 SAY 'On : '
      @ LINE, 50 SAY FDATE
      @ LINE, 74 SAY '|'
      @ LINE, 76 SAY 'Within The Period'
      @ LINE, 106 SAY '|'
      @ LINE, 108 SAY 'On : '
      @ LINE, 113 SAY TDATE
      LINE= LINE+1
      @ LINE, 43 SAY '|'
      @ LINE, 45 SAY REPLICATE('-',87)
      LINE= LINE+1
      @ LINE, 43 SAY '|'
      @ LINE, 45 SAY 'Dr'
      @ LINE, 61 SAY 'Cr'
      @ LINE, 74 SAY '|'
      @ LINE, 76 SAY 'Dr'
      @ LINE, 92 SAY 'Cr'
      @ LINE, 106 SAY '|'
      @ LINE, 108 SAY 'Dr'
      @ LINE, 124 SAY 'Cr'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('=',132)
      LINE= LINE+1
      HEAD= .F.
   ENDIF
   SELECT 1
   MOPEN_BAL= 0
   MSTAT= '  '
   MC1= C1
   MC2= C2
   MC3= C3
   MSTAT= STATUS
   IF SR_NO=1
      MOPEN_BAL= BL_AMOUNT
   ELSE
      IF MSTAT='DR'
         IF DR_AMOUNT<>0
            MOPEN_BAL= BL_AMOUNT-DR_AMOUNT
         ENDIF
         IF CR_AMOUNT<>0
            MOPEN_BAL= BL_AMOUNT+CR_AMOUNT
         ENDIF
      ENDIF
      IF MSTAT='CR'
         IF DR_AMOUNT<>0
            MOPEN_BAL= BL_AMOUNT+DR_AMOUNT
         ENDIF
         IF CR_AMOUNT<>0
            MOPEN_BAL= BL_AMOUNT-CR_AMOUNT
         ENDIF
      ENDIF
   ENDIF
   STORE 0 TO MCLOSE_BAL, MDRTOT, MCRTOT
   MCLOSE= '  '
   DO WHILE C1=MC1.and.C2=MC2.and.C3=MC3
      IF C1<>MC1.and.C2<>MC2.and.C3<>MC3
         EXIT
      ENDIF
      MDRTOT= MDRTOT+DR_AMOUNT
      MCRTOT= MCRTOT+CR_AMOUNT
      MCLOSE_BAL= BL_AMOUNT
      MCLOSE= STATUS
      SKIP
   ENDDO
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ LINE, 1 SAY MPC1
   @ LINE, 3 SAY '-'
   @ LINE, 4 SAY MPC2
   @ LINE, 6 SAY '-'
   @ LINE, 7 SAY MPC3
   SELECT 2
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF FOUND()
      @ LINE, 12 SAY TITLE
   ELSE
      @ LINE, 12 SAY '  '
   ENDIF
   SELECT 1
   @ LINE, 43 SAY '|'
   IF MSTAT='DR'
      @ LINE, 45 SAY MOPEN_BAL PICTURE '999999999.99'
      T1= T1+MOPEN_BAL
      @ LINE, 59 SAY '|'
   ELSE
      @ LINE, 59 SAY '|'
      @ LINE, 61 SAY MOPEN_BAL PICTURE '999999999.99'
      T2= T2+MOPEN_BAL
   ENDIF
   @ LINE, 74 SAY '|'
   @ LINE, 76 SAY MDRTOT PICTURE '9999999999.99'
   T3= T3+MDRTOT
   @ LINE, 90 SAY '|'
   @ LINE, 92 SAY MCRTOT PICTURE '9999999999.99'
   T4= T4+MCRTOT
   @ LINE, 106 SAY '|'
   IF MCLOSE='DR'
      @ LINE, 108 SAY MCLOSE_BAL PICTURE '999999999.99'
      T5= T5+MCLOSE_BAL
      @ LINE, 122 SAY '|'
   ELSE
      @ LINE, 122 SAY '|'
      @ LINE, 124 SAY MCLOSE_BAL PICTURE '999999999.99'
      T6= T6+MCLOSE_BAL
   ENDIF
   LINE= LINE+1
   IF LINE>=56
      @ 0, 0 SAY CHR(18)
      HEAD= .T.
   ENDIF
ENDDO
@ LINE, 1 SAY REPLICATE('-',132)
LINE= LINE+1
@ LINE, 1 SAY 'T o t a l s '
@ LINE, 43 SAY '|'
@ LINE, 45 SAY T1 PICTURE '999999999.99'
@ LINE, 59 SAY '|'
@ LINE, 61 SAY T2 PICTURE '999999999.99'
@ LINE, 74 SAY '|'
@ LINE, 76 SAY T3 PICTURE '9999999999.99'
@ LINE, 90 SAY '|'
@ LINE, 92 SAY T4 PICTURE '9999999999.99'
@ LINE, 106 SAY '|'
@ LINE, 108 SAY T5 PICTURE '999999999.99'
@ LINE, 122 SAY '|'
@ LINE, 124 SAY T6 PICTURE '999999999.99'
EJECT
@ 0, 0 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
CLEAR
CLOSE ALL
RETURN

PROCEDURE JOU-REP
CLEAR
CLOSE ALL
SET DELETED ON
SELECT 1
USE tran INDEX tranent
SELECT 2
USE ldgrcode INDEX ac1
SELECT 3
USE tran-rep INDEX rep-idx
DELETE ALL
DO WHILE .T.
   CLEAR
   R= 6
   MVCH1= '  '
   MVCH2= '     '
   STORE SPACE(30) TO MTITLE, WTITLE
   @ 4, 25 SAY 'Journal Report '
   STORE 0 TO MC1, MC2, MC3, TDR, TCR, TDR1, TCR1
   STORE 0 TO MVCH_TYPE, MVCH_NO1, MVCH_NO2
   STORE DATE() TO SDATE, LDATE
   @ 23, 1 SAY 'Press Ctrl Q To Quit '
   @ R, 10 SAY 'Enter Code for Reports '
   SET COLOR TO /w
   @ R, 35 SAY '  -  -   '
   SET COLOR TO 7/1
   @ R, 35 GET MC1 PICTURE '99'
   @ R, 38 GET MC2 PICTURE '99'
   @ R, 41 GET MC3 PICTURE '999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 1 CLEAR TO 23, 45
   SELECT 2
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 23, 15 SAY 'This Code Does Not Exist .. Please Re-try  '
      WAIT ''
      @ 23, 10 CLEAR TO 23, 79
      LOOP
   ENDIF
   MTITLE= TITLE
   R= R+2
   @ R, 10 SAY 'Title '
   SET COLOR TO /w
   @ R, 21 SAY MTITLE
   SET COLOR TO 7/1
   R= R+2
   @ R, 10 SAY 'From Date ' GET SDATE
   R= R+2
   @ R, 10 SAY 'To.. Date ' GET LDATE
   READ
   SELECT 1
   SET FILTER TO DATE>=SDATE.and.DATE<=LDATE
   COUNT TO RCON
   IF RCON=0
      CLEAR
      @ 10, 10 SAY 'There is No Transaction Between These Dates'
      @ 24, 25 SAY 'Press Any Key To Return'
      WAIT ''
      CLEAR
      LOOP
   ENDIF
   EXIT
ENDDO
P= ' '
DO WHILE .not. P$'PSQ'
   @ 23, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
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
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   MVCH_TYPE= VCH_TYPE
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   SELECT 3
   SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF FOUND()
      SELECT 1
      IF .not. EOF()
         SKIP
      ENDIF
      LOOP
   ENDIF
   SELECT 1
   IF C1=MC1.and.C2=MC2.and.C3=MC3
      SELECT 3
      APPEND BLANK
      REPLACE DATE WITH  A->DATE, VCH_NO1 WITH  A->VCH_NO1, VCH_NO2 WITH  A->VCH_NO2
      REPLACE C1 WITH  A->C1, VCH_TYPE WITH  A->VCH_TYPE, DR_CR WITH  A->DR_CR
      REPLACE C2 WITH  A->C2, AMOUNT WITH  A->AMOUNT
      REPLACE C3 WITH  A->C3, SR_NO WITH  A->SR_NO
   ENDIF
   SELECT 1
   IF .not. EOF()
      SKIP
   ENDIF
ENDDO
HEAD= .T.
SELECT 3
GOTO TOP
PAGE= 0
DO WHILE .not. EOF()
   IF HEAD
      LINE= 2
      IF P='S'
         DO screen
         @ LINE, 29 SAY 'J o u r n a l '
      ENDIF
      IF P='P'
         DO top-prt1
         @ LINE, 0 SAY CHR(14)
         @ LINE, 17 SAY 'Journal '
      ENDIF
      LINE= LINE+1
      @ LINE, 1 SAY 'From : '
      @ LINE, 9 SAY SDATE
      @ LINE, 25 SAY 'To : '
      @ LINE, 30 SAY LDATE
      LINE= LINE+1
      @ LINE, 1 SAY MTITLE
      PAGE= PAGE+1
      @ LINE, 65 SAY 'Page # '
      @ LINE, 73 SAY PAGE PICTURE '99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',79)
      LINE= LINE+1
      @ LINE, 1 SAY 'P A R T I C U L A R S '
      @ LINE, 30 SAY ':'
      @ LINE, 31 SAY '   DEBIT  '
      @ LINE, 43 SAY ':'
      @ LINE, 44 SAY '   CREDIT '
      @ LINE, 56 SAY ':'
      @ LINE, 57 SAY '   DEBIT '
      @ LINE, 69 SAY ':'
      @ LINE, 70 SAY '   CREDIT '
      LINE= LINE+1
      @ LINE, 56 SAY ':'
      @ LINE, 57 SAY LEFT(MTITLE,23)
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('=',79)
      LINE= LINE+1
      HEAD= .F.
   ENDIF
   SELECT 3
   MVCH_TYPE= VCH_TYPE
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   MDATE= DATE
   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
   T= '  '
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
   SELECT 1
   SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF FOUND()
      @ LINE, 1 SAY T
      @ LINE, 4 SAY MVCH1
      @ LINE, 6 SAY '-'
      @ LINE, 7 SAY MVCH2
      @ LINE, 14 SAY 'Dt: '
      @ LINE, 18 SAY MDATE
      LINE= LINE+1
      DO WHILE VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         IF HEAD
            LINE= 2
            IF P='S'
               DO screen
               @ LINE, 29 SAY 'J o u r n a l '
            ENDIF
            IF P='P'
               DO top-prt1
               @ LINE, 0 SAY CHR(14)
               @ LINE, 17 SAY 'Journal '
            ENDIF
            LINE= LINE+1
            @ LINE, 1 SAY 'From : '
            @ LINE, 9 SAY SDATE
            @ LINE, 25 SAY 'To : '
            @ LINE, 30 SAY LDATE
            LINE= LINE+1
            @ LINE, 1 SAY MTITLE
            PAGE= PAGE+1
            @ LINE, 65 SAY 'Page # '
            @ LINE, 73 SAY PAGE PICTURE '99'
            LINE= LINE+1
            @ LINE, 1 SAY REPLICATE('-',79)
            LINE= LINE+1
            @ LINE, 1 SAY 'P A R T I C U L A R S '
            @ LINE, 30 SAY ':'
            @ LINE, 31 SAY '   DEBIT  '
            @ LINE, 43 SAY ':'
            @ LINE, 44 SAY '   CREDIT '
            @ LINE, 56 SAY ':'
            @ LINE, 57 SAY '   DEBIT '
            @ LINE, 69 SAY ':'
            @ LINE, 70 SAY '   CREDIT '
            LINE= LINE+1
            @ LINE, 56 SAY ':'
            @ LINE, 57 SAY LEFT(MTITLE,23)
            LINE= LINE+1
            @ LINE, 1 SAY REPLICATE('=',79)
            LINE= LINE+1
            HEAD= .F.
         ENDIF
         IF VCH_TYPE<>MVCH_TYPE.and.VCH_NO1<>MVCH_NO1.and.VCH_NO2<>MVCH_NO2
            EXIT
         ENDIF
         WC1= C1
         WC2= C2
         WC3= C3
         SELECT 2
         SEEK STR(WC1,2)+STR(WC2,2)+STR(WC3,3)
         IF FOUND()
            WTITLE= TITLE
         ENDIF
         SELECT 1
         IF MC1=WC1.and.MC2=WC2.and.MC3=WC3
            IF DR_CR='DR'
               @ LINE, 1 SAY WTITLE
               @ LINE, 57 SAY AMOUNT PICTURE '999,999,999.99'
               TDR1= TDR1+AMOUNT
            ELSE
               @ LINE, 1 SAY 'To '
               @ LINE, 4 SAY WTITLE
               @ LINE, 66 SAY AMOUNT PICTURE '999,999,999.99'
               TCR1= TCR1+AMOUNT
            ENDIF
         ELSE
            IF DR_CR='DR'
               @ LINE, 1 SAY WTITLE
               @ LINE, 30 SAY AMOUNT PICTURE '999,999,999.99'
               TDR= TDR+AMOUNT
            ELSE
               @ LINE, 1 SAY 'To '
               @ LINE, 4 SAY WTITLE
               @ LINE, 43 SAY AMOUNT PICTURE '999,999,999.99'
               TCR= TCR+AMOUNT
            ENDIF
         ENDIF
         LINE= LINE+1
         SKIP
         IF P='S'.and.LINE>20
            XYZ= ' '
            DO WHILE .not. XYZ$'CQ'
               @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
               READ
            ENDDO
            IF XYZ='Q'
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            CLEAR
            HEAD= .T.
         ENDIF
         IF P='P'.and.LINE>=56
            HEAD= .T.
         ENDIF
      ENDDO
   ENDIF
   LINE= LINE-1
   IF P='P'
      @ LINE, 1 SAY REPLICATE('_',79)
   ENDIF
   LINE= LINE+2
   SELECT 3
   SKIP
   IF P='S'.and.LINE>20
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      HEAD= .T.
   ENDIF
   IF P='P'.and.LINE>=56
      HEAD= .T.
   ENDIF
ENDDO
LINE= LINE-1
@ LINE, 1 SAY REPLICATE('=',79)
LINE= LINE+1
@ LINE, 1 SAY 'T o t a l s '
@ LINE, 32 SAY TDR PICTURE '99999999.99'
@ LINE, 58 SAY TDR1 PICTURE '99999999.99'
LINE= LINE+1
@ LINE, 45 SAY TCR PICTURE '99999999.99'
@ LINE, 69 SAY TCR1 PICTURE '99999999.99'
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
SELECT 3
DELETE ALL
CLEAR
CLOSE ALL
RETURN

PROCEDURE JOU1-REP
CLEAR
CLOSE ALL
SET DELETED ON
SELECT 1
USE tran INDEX tranent
SELECT 2
USE ldgrcode INDEX ac1
SELECT 3
USE tran-rep INDEX rep-idx
DELETE ALL
DO WHILE .T.
   CLEAR
   R= 6
   MVCH1= '  '
   MVCH2= '     '
   STORE SPACE(30) TO MTITLE, WTITLE
   @ 4, 25 SAY 'Journal Report '
   STORE 0 TO MC1, MC2, MC3, TDR, TCR, TDR1, TCR1
   STORE 0 TO MVCH_TYPE, MVCH_NO1, MVCH_NO2
   STORE DATE() TO SDATE, LDATE
   @ 23, 1 SAY 'Press Ctrl Q To Quit '
   @ R, 10 SAY 'Enter Code for Reports '
   SET COLOR TO /w
   @ R, 35 SAY '  -  -   '
   SET COLOR TO 7/1
   @ R, 35 GET MC1 PICTURE '99'
   @ R, 38 GET MC2 PICTURE '99'
   @ R, 41 GET MC3 PICTURE '999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 1 CLEAR TO 23, 45
   SELECT 2
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 23, 15 SAY 'This Code Does Not Exist .. Please Re-try  '
      WAIT ''
      @ 23, 10 CLEAR TO 23, 79
      LOOP
   ENDIF
   MTITLE= TITLE
   R= R+2
   @ R, 10 SAY 'Title '
   SET COLOR TO /w
   @ R, 21 SAY MTITLE
   SET COLOR TO 7/1
   R= R+2
   @ R, 10 SAY 'From Date ' GET SDATE
   R= R+2
   @ R, 10 SAY 'To.. Date ' GET LDATE
   READ
   SELECT 1
   SET FILTER TO DATE>=SDATE.and.DATE<=LDATE
   COUNT TO RCON
   IF RCON=0
      CLEAR
      @ 10, 10 SAY 'There is No Transaction Between These Dates'
      @ 24, 25 SAY 'Press Any Key To Return'
      WAIT ''
      CLEAR
      LOOP
   ENDIF
   EXIT
ENDDO
P= ' '
DO WHILE .not. P$'PQ'
   @ 23, 45 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
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
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   MVCH_TYPE= VCH_TYPE
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   SELECT 3
   SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF FOUND()
      SELECT 1
      IF .not. EOF()
         SKIP
      ENDIF
      LOOP
   ENDIF
   SELECT 1
   IF C1=MC1.and.C2=MC2.and.C3=MC3
      SELECT 3
      APPEND BLANK
      REPLACE DATE WITH  A->DATE, VCH_NO1 WITH  A->VCH_NO1, VCH_NO2 WITH  A->VCH_NO2
      REPLACE C1 WITH  A->C1, VCH_TYPE WITH  A->VCH_TYPE, DR_CR WITH  A->DR_CR
      REPLACE C2 WITH  A->C2, AMOUNT WITH  A->AMOUNT
      REPLACE C3 WITH  A->C3, SR_NO WITH  A->SR_NO
   ENDIF
   SELECT 1
   IF .not. EOF()
      SKIP
   ENDIF
ENDDO
HEAD= .T.
SELECT 3
GOTO TOP
PAGE= 0
DO WHILE .not. EOF()
   IF HEAD
      LINE= 2
      IF P='S'
         DO screen
         @ LINE, 29 SAY 'J o u r n a l '
      ENDIF
      IF P='P'
         DO top-prt1
         @ LINE, 0 SAY CHR(14)
         @ LINE, 17 SAY 'Journal '
      ENDIF
      LINE= LINE+1
      @ LINE, 1 SAY 'From : '
      @ LINE, 9 SAY SDATE
      @ LINE, 25 SAY 'To : '
      @ LINE, 30 SAY LDATE
      LINE= LINE+1
      @ LINE, 1 SAY MTITLE
      PAGE= PAGE+1
      @ LINE, 65 SAY 'Page # '
      @ LINE, 73 SAY PAGE PICTURE '99'
      LINE= LINE+1
      @ LINE, 1 SAY CHR(15)
      @ LINE, 1 SAY REPLICATE('-',132)
      LINE= LINE+1
      @ LINE, 1 SAY 'P A R T I C U L A R S '
      @ LINE, 30 SAY ':'
      @ LINE, 31 SAY 'D e s c r i p t i o n  '
      @ LINE, 92 SAY ':'
      @ LINE, 93 SAY '   DEBIT  '
      @ LINE, 103 SAY ':'
      @ LINE, 104 SAY '   CREDIT '
      @ LINE, 114 SAY ':'
      @ LINE, 115 SAY '   DEBIT '
      @ LINE, 125 SAY ':'
      @ LINE, 126 SAY '   CREDIT '
      LINE= LINE+1
      @ LINE, 114 SAY ':'
      @ LINE, 115 SAY LEFT(MTITLE,23)
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('=',132)
      LINE= LINE+1
      HEAD= .F.
   ENDIF
   SELECT 3
   MVCH_TYPE= VCH_TYPE
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   MDATE= DATE
   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))
   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))
   T= '  '
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
   SELECT 1
   SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF FOUND()
      @ LINE, 1 SAY T
      @ LINE, 4 SAY MVCH1
      @ LINE, 6 SAY '-'
      @ LINE, 7 SAY MVCH2
      @ LINE, 14 SAY 'Dt: '
      @ LINE, 18 SAY MDATE
      LINE= LINE+1
      DO WHILE VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         IF HEAD
            LINE= 2
            IF P='S'
               DO screen
               @ LINE, 29 SAY 'J o u r n a l '
            ENDIF
            IF P='P'
               DO top-prt1
               @ LINE, 0 SAY CHR(14)
               @ LINE, 17 SAY 'Journal '
            ENDIF
            LINE= LINE+1
            @ LINE, 1 SAY 'From : '
            @ LINE, 9 SAY SDATE
            @ LINE, 25 SAY 'To : '
            @ LINE, 30 SAY LDATE
            LINE= LINE+1
            @ LINE, 1 SAY MTITLE
            PAGE= PAGE+1
            @ LINE, 65 SAY 'Page # '
            @ LINE, 73 SAY PAGE PICTURE '99'
            LINE= LINE+1
            @ LINE, 1 SAY CHR(15)
            @ LINE, 1 SAY REPLICATE('-',132)
            LINE= LINE+1
            @ LINE, 1 SAY 'P A R T I C U L A R S '
            @ LINE, 30 SAY ':'
            @ LINE, 31 SAY 'D e s c r i p t i o n  '
            @ LINE, 92 SAY ':'
            @ LINE, 93 SAY '   DEBIT  '
            @ LINE, 103 SAY ':'
            @ LINE, 104 SAY '   CREDIT '
            @ LINE, 114 SAY ':'
            @ LINE, 115 SAY '   DEBIT '
            @ LINE, 125 SAY ':'
            @ LINE, 126 SAY '   CREDIT '
            LINE= LINE+1
            @ LINE, 114 SAY ':'
            @ LINE, 115 SAY LEFT(MTITLE,23)
            LINE= LINE+1
            @ LINE, 1 SAY REPLICATE('=',132)
            LINE= LINE+1
            HEAD= .F.
         ENDIF
         IF VCH_TYPE<>MVCH_TYPE.and.VCH_NO1<>MVCH_NO1.and.VCH_NO2<>MVCH_NO2
            EXIT
         ENDIF
         WC1= C1
         WC2= C2
         WC3= C3
         SELECT 2
         SEEK STR(WC1,2)+STR(WC2,2)+STR(WC3,3)
         IF FOUND()
            WTITLE= TITLE
         ENDIF
         SELECT 1
         IF MC1=WC1.and.MC2=WC2.and.MC3=WC3
            IF DR_CR='DR'
               @ LINE, 1 SAY WTITLE
               @ LINE, 31 SAY DESCRIP
               @ LINE, 62 SAY DESCRIP1
               @ LINE, 111 SAY AMOUNT PICTURE '999,999,999.99'
               TDR1= TDR1+AMOUNT
            ELSE
               @ LINE, 1 SAY 'To '
               @ LINE, 4 SAY WTITLE
               @ LINE, 31 SAY DESCRIP
               @ LINE, 62 SAY DESCRIP1
               @ LINE, 120 SAY AMOUNT PICTURE '999,999,999.99'
               TCR1= TCR1+AMOUNT
            ENDIF
         ELSE
            IF DR_CR='DR'
               @ LINE, 1 SAY WTITLE
               @ LINE, 31 SAY DESCRIP
               @ LINE, 62 SAY DESCRIP1
               @ LINE, 91 SAY AMOUNT PICTURE '999,999,999.99'
               TDR= TDR+AMOUNT
            ELSE
               @ LINE, 1 SAY 'To '
               @ LINE, 4 SAY WTITLE
               @ LINE, 31 SAY DESCRIP
               @ LINE, 62 SAY DESCRIP1
               @ LINE, 101 SAY AMOUNT PICTURE '999,999,999.99'
               TCR= TCR+AMOUNT
            ENDIF
         ENDIF
         LINE= LINE+1
         SKIP
         IF P='S'.and.LINE>20
            XYZ= ' '
            DO WHILE .not. XYZ$'CQ'
               @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
               READ
            ENDDO
            IF XYZ='Q'
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            CLEAR
            HEAD= .T.
         ENDIF
         IF P='P'.and.LINE>=56
            HEAD= .T.
         ENDIF
      ENDDO
   ENDIF
   LINE= LINE-1
   IF P='P'
      @ LINE, 1 SAY REPLICATE('_',132)
   ENDIF
   LINE= LINE+2
   SELECT 3
   SKIP
   IF P='P'.and.LINE>=56
      @ 0, 0 SAY CHR(18)
      HEAD= .T.
   ENDIF
ENDDO
LINE= LINE-1
@ LINE, 1 SAY REPLICATE('=',132)
LINE= LINE+1
@ LINE, 1 SAY 'T o t a l s '
@ LINE, 90 SAY TDR PICTURE '99,999,999.99'
@ LINE, 113 SAY TDR1 PICTURE '99,999,999.99'
LINE= LINE+1
@ LINE, 104 SAY TCR PICTURE '99,999,999.99'
@ LINE, 120 SAY TCR1 PICTURE '99,999,999.99'
EJECT
@ 0, 0 SAY CHR(18)
SET PRINTER OFF
SET DEVICE TO SCREEN
SELECT 3
DELETE ALL
CLEAR
CLOSE ALL
RETURN

PROCEDURE REC-AGE
CLEAR
CLEAR ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE ldgrcode INDEX ac1
DO WHILE .T.
   CLEAR
   T= '   '
   STORE DATE() TO SDATE, MDATE
   STORE 0 TO MC1, MC2, MC3, MBAL, MBAL1, MTOT, MNET, XYZ
   @ 3, 10 SAY 'Aging Of Accounts Receivble'
   ABC= 0
   DO WHILE ABC=0
      R= 5
      @ 24, 45 SAY 'Press Ctrl Q To Quit '
      @ R, 15 SAY 'Party Code '
      R= R+2
      SET COLOR TO /g
      @ R, 15 SAY '  -  -   '
      SET COLOR TO 
      @ R, 15 GET MC1 PICTURE '@z 99'
      READ
      IF READKEY()=12
         CLOSE ALL
         CLEAR
         RETURN
      ENDIF
      @ 24, 40 CLEAR TO 24, 79
      SET COLOR TO /w
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 18 GET MC2 PICTURE '@z 99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2
      SET COLOR TO 
      @ R, 21 GET MC3 PICTURE '@z 999'
      READ
      IF MC3=0
         LOOP
      ENDIF
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      SET COLOR TO /w
      @ R, 21 SAY MPC3
      SET COLOR TO 
      SELECT 3
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF .not. FOUND()
         @ 23, 20 SAY 'This Code Does Not Exists ... Please Re-try'
         WAIT ''
         @ 23, 19 SAY SPACE(55)
         LOOP
      ENDIF
      SET COLOR TO /w
      @ R, 30 SAY TITLE
      MTITLE= TITLE
      MBAL= LDGR_BAL
      MBAL1= LDGR_BAL
      MADD1= ADDRESS1
      MADD2= ADDRESS2
      MTOT= LDGR_BAL
      SET COLOR TO 
      ABC= 1
   ENDDO
   @ 9, 15 SAY 'As On ....... ' GET MDATE
   READ
   IF READKEY()=12
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 15 SAY 'Due Date .... ' GET SDATE
   READ
   P= ' '
   DO WHILE .not. P$'CQ'
      @ 24, 55 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='Q'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   SELECT 2
   USE master
   SUM FOR DATE>SDATE.and.DATE<=MDATE.and.C1=MC1.and.C2=MC2.and.C3=MC3 DR_AMOUNT TO XYZ
   USE
   SELECT 2
   USE mast-ref
   ZAP
   APPEND FROM master FOR C1=MC1.and.C2=MC2.and.C3=MC3.and.DATE<=SDATE
   FLUSH
   SORT ON DATE TO mastref
   DELETE ALL
   USE
   SELECT 1
   USE mastref
   GOTO BOTTOM
   MBAL= MBAL-XYZ
   DO WHILE .not. BOF()
      MBAL= MBAL-DR_AMOUNT
      REPLACE BAL WITH MBAL
      REPLACE FLAG WITH 'Y'
      IF MBAL<=0
         EXIT
      ENDIF
      SKIP  -1
   ENDDO
   STORE 0 TO PAGE, CNT
   HEAD= .T.
   SELECT 1
   GOTO TOP
   SET FILTER TO FLAG='Y'
   GOTO TOP
   MDR= 0
   MDR= DR_AMOUNT+BAL
   REPLACE DR_AMOUNT WITH MDR
   SET PRINTER ON
   LINE= 2
   SET DEVICE TO PRINTER
   DO WHILE .not. EOF()
      IF HEAD
         DO top-prt1
         LINE= 2
         @ LINE, 60 SAY 'Date : '
         @ LINE, 67 SAY DATE()
         LINE= LINE+2
         @ LINE, 1 SAY CHR(14)
         @ LINE, 12 SAY 'R E M I N D E R '
         LINE= LINE+1
         @ LINE, 1 SAY CHR(14)
         @ LINE, 12 SAY '--------------- '
         LINE= LINE+2
         @ LINE, 2 SAY 'Messrs '
         LINE= LINE+1
         @ LINE, 2 SAY MTITLE
         LINE= LINE+1
         @ LINE, 2 SAY MADD1
         LINE= LINE+1
         @ LINE, 2 SAY MADD2
         LINE= LINE+2
         @ LINE, 2 SAY 'Dear Sirs,'
         LINE= LINE+2
         @ LINE, 2 SAY 'We inform you that a sum of Rs. '
         @ LINE, 34 SAY MBAL1 PICTURE '999,999,999.99'
         @ LINE, 34 SAY MBAL1 PICTURE '999,999,999.99'
         @ LINE, 34 SAY '______________'
         @ LINE, 34 SAY '______________'
         @ LINE, 49 SAY 'is outstanding against you '
         LINE= LINE+1
         @ LINE, 2 SAY 'as at '
         @ LINE, 10 SAY MDATE
         @ LINE, 10 SAY MDATE
         @ LINE, 20 SAY 'as per our books of  accounts.   Out of said amount the '
         LINE= LINE+1
         @ LINE, 2 SAY 'following bills are due for payment.  Kindly  arrange  payment  of due at '
         LINE= LINE+1
         @ LINE, 2 SAY 'earliest, as we need funds urgently. '
         LINE= LINE+2
         @ LINE, 1 SAY REPLICATE('=',79)
         LINE= LINE+1
         @ LINE, 5 SAY 'Bill No '
         @ LINE, 20 SAY '|'
         @ LINE, 30 SAY 'Date  '
         @ LINE, 45 SAY '|'
         @ LINE, 50 SAY 'Amount (Rs) '
         LINE= LINE+1
         @ LINE, 1 SAY REPLICATE('-',79)
         LINE= LINE+1
         HEAD= .F.
      ENDIF
      IF DR_AMOUNT=0
         SKIP
         LOOP
      ENDIF
      IF VCH_TYPE=1
         T= 'CR-'
      ENDIF
      IF VCH_TYPE=2
         T= 'CP-'
      ENDIF
      IF VCH_TYPE=3
         T= 'BR-'
      ENDIF
      IF VCH_TYPE=4
         T= 'CP-'
      ENDIF
      IF VCH_TYPE=5
         T= 'JV-'
      ENDIF
      IF VCH_TYPE=6
         T= 'SV-'
      ENDIF
      IF VCH_TYPE=7
         T= 'PV-'
      ENDIF
      MVCH1= '  '
      MVCH2= '     '
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
      IF CNT=0
         @ LINE, 2 SAY 'On Act. Bal '
         CNT= 1
      ELSE
         @ LINE, 2 SAY T
         @ LINE, 5 SAY MVCH1
         @ LINE, 7 SAY '-'
         @ LINE, 8 SAY MVCH2
      ENDIF
      @ LINE, 20 SAY '|'
      @ LINE, 30 SAY DATE
      @ LINE, 45 SAY '|'
      @ LINE, 50 SAY DR_AMOUNT PICTURE '999,999,999.99'
      MNET= MNET+DR_AMOUNT
      LINE= LINE+1
      SKIP
   ENDDO
   @ LINE, 35 SAY REPLICATE('-',45)
   LINE= LINE+1
   @ LINE, 35 SAY 'Total (Rs) '
   @ LINE, 50 SAY MNET PICTURE '999,999,999.99'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('=',79)
   LINE= LINE+1
   @ LINE, 30 SAY 'Age Analysis '
   LINE= LINE+2
   SELECT 1
   GOTO TOP
   STORE 0 TO DRAMT15, DRAMT30, DRAMT60, DRAMT90, DRAMTAB
   STORE 0 TO CRAMT15, CRAMT30, CRAMT60, CRAMT90, CRAMTAB
   STORE 0 TO DRAMOUNT, CRAMOUNT, TAMOUNT
   DO WHILE .not. EOF()
      DRAMOUNT= DRAMOUNT+DR_AMOUNT
      IF DATE<=SDATE.and.DATE>=SDATE-15
         DRAMT15= DR_AMOUNT+DRAMT15
      ENDIF
      IF DATE<SDATE-15.and.DATE>=SDATE-30
         DRAMT30= DR_AMOUNT+DRAMT30
      ENDIF
      IF DATE<SDATE-30.and.DATE>=SDATE-60
         DRAMT60= DR_AMOUNT+DRAMT60
      ENDIF
      IF DATE<SDATE-60.and.DATE>=SDATE-90
         DRAMT90= DR_AMOUNT+DRAMT90
      ENDIF
      IF DATE<SDATE-90
         DRAMTAB= DR_AMOUNT+DRAMTAB
      ENDIF
      SKIP
   ENDDO
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   @ LINE, 5 SAY '15-Days '
   @ LINE, 16 SAY '|'
   @ LINE, 21 SAY '30-Days '
   @ LINE, 31 SAY '|'
   @ LINE, 37 SAY '60-Days '
   @ LINE, 48 SAY '|'
   @ LINE, 53 SAY '90-Days '
   @ LINE, 63 SAY '|'
   @ LINE, 65 SAY 'Above 90 Days'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   DO WHILE .T.
      IF DRAMT15<=MTOT
         @ LINE, 1 SAY DRAMT15 PICTURE '999,999,999.99'
         MTOT= MTOT-DRAMT15
      ELSE
         @ LINE, 1 SAY MTOT PICTURE '999,999,999.99'
         EXIT
      ENDIF
      IF DRAMT30<=MTOT
         @ LINE, 16 SAY DRAMT30 PICTURE '999,999,999.99'
         MTOT= MTOT-DRAMT30
      ELSE
         @ LINE, 16 SAY MTOT PICTURE '999,999,999.99'
         EXIT
      ENDIF
      IF DRAMT60<=MTOT
         @ LINE, 33 SAY DRAMT60 PICTURE '999,999,999.99'
         MTOT= MTOT-DRAMT60
      ELSE
         @ LINE, 33 SAY MTOT PICTURE '999,999,999.99'
         EXIT
      ENDIF
      IF DRAMT90<=MTOT
         @ LINE, 49 SAY DRAMT90 PICTURE '999,999,999.99'
         MTOT= MTOT-DRAMT90
      ELSE
         @ LINE, 49 SAY MTOT PICTURE '999,999,999.99'
         EXIT
      ENDIF
      IF DRAMTAB<=MTOT
         @ LINE, 65 SAY DRAMTAB PICTURE '999,999,999.99'
         MTOT= MTOT-DRAMTAB
      ELSE
         @ LINE, 65 SAY MTOT PICTURE '999,999,999.99'
         EXIT
      ENDIF
      EXIT
   ENDDO
   LINE= 56
   LINE= LINE+1
   @ LINE, 1 SAY CHR(15)
   @ LINE, 1 SAY 'Gold Coin Plus Accounting.'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',132)
   LINE= LINE+1
   @ LINE, 1 SAY 'Designed & Developed by: Logical Systems (Pvt) Limited, 82, 3rd Floor, Hafeez Centre Gulberg III, Lahore, Pakistan. Tel: (042) 5762391'
   EJECT
   @ 0, 0 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
   SELECT 1
   USE
   RUN del mastref.dbf 
ENDDO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REC-AGE1
CLEAR
CLEAR ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE ldgrcode INDEX ac1
DO WHILE .T.
   CLEAR
   T= '   '
   STORE DATE() TO SDATE, MDATE
   STORE 0 TO MC1, MC2, MC3, MBAL, MBAL1, MTOT, MNET, XYZ
   @ 3, 10 SAY 'Aging Of Accounts Receivble'
   ABC= 0
   DO WHILE ABC=0
      R= 5
      @ 24, 45 SAY 'Press Ctrl Q To Quit '
      @ R, 15 SAY 'Party Code '
      R= R+2
      SET COLOR TO /g
      @ R, 15 SAY '  -  -   '
      SET COLOR TO 
      @ R, 15 GET MC1 PICTURE '@z 99'
      READ
      IF READKEY()=12
         CLOSE ALL
         CLEAR
         RETURN
      ENDIF
      @ 24, 40 CLEAR TO 24, 79
      SET COLOR TO /w
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 18 GET MC2 PICTURE '@z 99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2
      SET COLOR TO 
      @ R, 21 GET MC3 PICTURE '@z 999'
      READ
      IF MC3=0
         LOOP
      ENDIF
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      SET COLOR TO /w
      @ R, 21 SAY MPC3
      SET COLOR TO 
      SELECT 3
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF .not. FOUND()
         @ 23, 20 SAY 'This Code Does Not Exists ... Please Re-try'
         WAIT ''
         @ 23, 19 SAY SPACE(55)
         LOOP
      ENDIF
      SET COLOR TO /w
      @ R, 30 SAY TITLE
      MTITLE= TITLE
      MBAL= LDGR_BAL
      MBAL1= LDGR_BAL
      MADD1= ADDRESS1
      MADD2= ADDRESS2
      MTOT= LDGR_BAL
      SET COLOR TO 
      ABC= 1
   ENDDO
   @ 9, 15 SAY 'As On ....... ' GET MDATE
   READ
   IF READKEY()=12
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 15 SAY 'Due Date .... ' GET SDATE
   READ
   P= ' '
   DO WHILE .not. P$'CQ'
      @ 24, 55 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='Q'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   SELECT 2
   USE master
   SUM FOR DATE>SDATE.and.DATE<=MDATE.and.C1=MC1.and.C2=MC2.and.C3=MC3 DR_AMOUNT TO XYZ
   USE
   SELECT 2
   USE mast-ref
   ZAP
   APPEND FROM master FOR C1=MC1.and.C2=MC2.and.C3=MC3.and.DATE<=SDATE
   FLUSH
   SORT ON DATE TO mastref
   DELETE ALL
   USE
   SELECT 1
   USE mastref
   GOTO BOTTOM
   MBAL= MBAL-XYZ
   DO WHILE .not. BOF()
      MBAL= MBAL-DR_AMOUNT
      REPLACE BAL WITH MBAL
      REPLACE FLAG WITH 'Y'
      IF MBAL<=0
         EXIT
      ENDIF
      SKIP  -1
   ENDDO
   STORE 0 TO PAGE, CNT
   HEAD= .T.
   SELECT 1
   GOTO TOP
   SET FILTER TO FLAG='Y'
   GOTO TOP
   MDR= 0
   MDR= DR_AMOUNT+BAL
   REPLACE DR_AMOUNT WITH MDR
   SET PRINTER ON
   LINE= 0
   SET DEVICE TO PRINTER
   DO WHILE .not. EOF()
      IF HEAD
         DO top-prt1
         LINE= 2
         @ LINE, 60 SAY 'Date : '
         @ LINE, 67 SAY DATE()
         LINE= LINE+1
         @ LINE, 1 SAY CHR(14)
         @ LINE, 12 SAY 'R E M I N D E R '
         LINE= LINE+1
         @ LINE, 1 SAY CHR(14)
         @ LINE, 12 SAY '--------------- '
         LINE= LINE+2
         @ LINE, 2 SAY 'Messrs '
         LINE= LINE+1
         @ LINE, 2 SAY MTITLE
         LINE= LINE+1
         @ LINE, 2 SAY MADD1
         LINE= LINE+1
         @ LINE, 2 SAY MADD2
         LINE= LINE+2
         @ LINE, 2 SAY 'Dear Sirs,'
         LINE= LINE+2
         @ LINE, 2 SAY 'We inform you that a sum of Rs. '
         @ LINE, 34 SAY MBAL1 PICTURE '999,999,999.99'
         @ LINE, 34 SAY MBAL1 PICTURE '999,999,999.99'
         @ LINE, 34 SAY '______________'
         @ LINE, 34 SAY '______________'
         @ LINE, 49 SAY 'is outstanding against you '
         LINE= LINE+1
         @ LINE, 2 SAY 'as at '
         @ LINE, 10 SAY MDATE
         @ LINE, 10 SAY MDATE
         @ LINE, 20 SAY 'as per our books of  accounts.   Out of said amount the '
         LINE= LINE+1
         @ LINE, 2 SAY 'following bills are due for payment.  Kindly  arrange  payment  of due at '
         LINE= LINE+1
         @ LINE, 2 SAY 'earliest, as we need funds urgently. '
         LINE= LINE+2
         @ LINE, 1 SAY REPLICATE('=',79)
         LINE= LINE+1
         @ LINE, 5 SAY 'Bill No '
         @ LINE, 20 SAY '|'
         @ LINE, 30 SAY 'Date  '
         @ LINE, 45 SAY '|'
         @ LINE, 50 SAY 'Amount (Rs) '
         LINE= LINE+1
         @ LINE, 1 SAY REPLICATE('-',79)
         LINE= LINE+1
         HEAD= .F.
      ENDIF
      IF DR_AMOUNT=0
         SKIP
         LOOP
      ENDIF
      IF VCH_TYPE=1
         T= 'CR-'
      ENDIF
      IF VCH_TYPE=2
         T= 'CP-'
      ENDIF
      IF VCH_TYPE=3
         T= 'BR-'
      ENDIF
      IF VCH_TYPE=4
         T= 'CP-'
      ENDIF
      IF VCH_TYPE=5
         T= 'JV-'
      ENDIF
      IF VCH_TYPE=6
         T= 'SV-'
      ENDIF
      IF VCH_TYPE=7
         T= 'PV-'
      ENDIF
      MVCH1= '  '
      MVCH2= '     '
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
      IF CNT=0
         @ LINE, 2 SAY 'On Act. Bal '
         CNT= 1
      ELSE
         @ LINE, 2 SAY T
         @ LINE, 5 SAY MVCH1
         @ LINE, 7 SAY '-'
         @ LINE, 8 SAY MVCH2
      ENDIF
      @ LINE, 20 SAY '|'
      @ LINE, 30 SAY DATE
      @ LINE, 45 SAY '|'
      @ LINE, 50 SAY DR_AMOUNT PICTURE '999,999,999.99'
      MNET= MNET+DR_AMOUNT
      LINE= LINE+1
      SKIP
   ENDDO
   @ LINE, 35 SAY REPLICATE('-',45)
   LINE= LINE+1
   @ LINE, 35 SAY 'Total (Rs) '
   @ LINE, 50 SAY MNET PICTURE '999,999,999.99'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('=',79)
   LINE= LINE+1
   @ LINE, 30 SAY 'Age Analysis '
   LINE= LINE+2
   SELECT 1
   GOTO TOP
   STORE 0 TO DRAMT15, DRAMT30, DRAMT60, DRAMT90, DRAMTAB
   STORE 0 TO CRAMT15, CRAMT30, CRAMT60, CRAMT90, CRAMTAB
   STORE 0 TO DRAMOUNT, CRAMOUNT, TAMOUNT
   DO WHILE .not. EOF()
      DRAMOUNT= DRAMOUNT+DR_AMOUNT
      IF DATE<=MDATE.and.DATE>=MDATE-30
         DRAMT15= DR_AMOUNT+DRAMT15
      ENDIF
      IF DATE<MDATE-30.and.DATE>=MDATE-45
         DRAMT30= DR_AMOUNT+DRAMT30
      ENDIF
      IF DATE<MDATE-45.and.DATE>=MDATE-60
         DRAMT60= DR_AMOUNT+DRAMT60
      ENDIF
      IF DATE<MDATE-60.and.DATE>=MDATE-90
         DRAMT90= DR_AMOUNT+DRAMT90
      ENDIF
      IF DATE<MDATE-90
         DRAMTAB= DR_AMOUNT+DRAMTAB
      ENDIF
      SKIP
   ENDDO
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   @ LINE, 5 SAY '30-Days '
   @ LINE, 16 SAY '|'
   @ LINE, 21 SAY '45-Days '
   @ LINE, 31 SAY '|'
   @ LINE, 37 SAY '60-Days '
   @ LINE, 48 SAY '|'
   @ LINE, 53 SAY '90-Days '
   @ LINE, 63 SAY '|'
   @ LINE, 65 SAY 'Above 90 Days'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   DO WHILE .T.
      IF DRAMT15<=MTOT
         @ LINE, 1 SAY DRAMT15 PICTURE '999,999,999.99'
         MTOT= MTOT-DRAMT15
      ELSE
         @ LINE, 1 SAY MTOT PICTURE '999,999,999.99'
         EXIT
      ENDIF
      IF DRAMT30<=MTOT
         @ LINE, 16 SAY DRAMT30 PICTURE '999,999,999.99'
         MTOT= MTOT-DRAMT30
      ELSE
         @ LINE, 16 SAY MTOT PICTURE '999,999,999.99'
         EXIT
      ENDIF
      IF DRAMT60<=MTOT
         @ LINE, 33 SAY DRAMT60 PICTURE '999,999,999.99'
         MTOT= MTOT-DRAMT60
      ELSE
         @ LINE, 33 SAY MTOT PICTURE '999,999,999.99'
         EXIT
      ENDIF
      IF DRAMT90<=MTOT
         @ LINE, 49 SAY DRAMT90 PICTURE '999,999,999.99'
         MTOT= MTOT-DRAMT90
      ELSE
         @ LINE, 49 SAY MTOT PICTURE '999,999,999.99'
         EXIT
      ENDIF
      IF DRAMTAB<=MTOT
         @ LINE, 65 SAY DRAMTAB PICTURE '999,999,999.99'
         MTOT= MTOT-DRAMTAB
      ELSE
         @ LINE, 65 SAY MTOT PICTURE '999,999,999.99'
         EXIT
      ENDIF
      EXIT
   ENDDO
   LINE= 56
   LINE= LINE+1
   @ LINE, 1 SAY CHR(15)
   @ LINE, 1 SAY 'Gold Coin Plus Accounting.'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',132)
   LINE= LINE+1
   @ LINE, 1 SAY 'Designed & Developed by: Logical Systems (Pvt) Limited, 82, 3rd Floor, Hafeez Centre Gulberg III, Lahore, Pakistan. Tel: (042) 5762391'
   EJECT
   @ 0, 0 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
   SELECT 1
   USE
   RUN del mastref.dbf 
ENDDO
CLEAR
CLOSE ALL
RETURN

PROCEDURE REC-AGE2
CLEAR
CLEAR ALL
SET DATE brit
SET DELETED ON
SELECT 1
USE ldgrcode INDEX ac1
CLEAR
T= '   '
STORE DATE() TO SDATE, MDATE
STORE 0 TO TOT1, TOT2, TOT3, TOT4, TOT5, PAGE
STORE 0 TO MC1, MC2, MC3, MBAL, MBAL1, MTOT, MNET, XYZ
@ 3, 10 SAY 'Aging Of Accounts Receivble'
ABC= 0
DO WHILE ABC=0
   R= 5
   @ 24, 45 SAY 'Press Ctrl Q To Quit '
   @ R, 15 SAY 'Party Code '
   R= R+2
   SET COLOR TO /g
   @ R, 15 SAY '  -  -'
   SET COLOR TO 7/1
   @ R, 15 GET MC1 PICTURE '@z 99'
   READ
   IF READKEY()=12
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 24, 40 CLEAR TO 24, 79
   SET COLOR TO /w
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   @ R, 15 SAY MPC1
   @ R, 18 GET MC2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ R, 18 SAY MPC2
   SET COLOR TO 7/1
   SELECT 1
   COPY TO mldgr FOR C1=MC1.and.C2=MC2
   SELECT 3
   USE mldgr
   ABC= 1
ENDDO
@ 9, 15 SAY 'As On ....... ' GET MDATE
READ
IF READKEY()=12
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
@ 11, 15 SAY 'Due Date .... ' GET SDATE
READ
P= ' '
DO WHILE .not. P$'PQ'
   @ 23, 50 SAY 'Printer/Quit - P/Q ' GET P FUNCTION '!'
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
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
HEAD= .T.
SELECT 3
GOTO TOP
DO WHILE .not. EOF()
   IF HEAD
      IF P='P'
         DO top-prt1
      ELSE
         DO screen
      ENDIF
      LINE= 2
      PAGE= PAGE+1
      @ LINE, 30 SAY 'Age Analysis '
      LINE= LINE+1
      @ LINE, 1 SAY CHR(15)
      @ LINE, 1 SAY 'Date : '
      @ LINE, 10 SAY DATE()
      @ LINE, 100 SAY 'Page # '
      @ LINE, 108 SAY PAGE PICTURE '99'
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('=',130)
      LINE= LINE+1
      @ LINE, 1 SAY 'Code  '
      @ LINE, 12 SAY 'Party '
      @ LINE, 45 SAY '  15-Days '
      @ LINE, 59 SAY '|'
      @ LINE, 60 SAY '  30-Days '
      @ LINE, 74 SAY '|'
      @ LINE, 75 SAY '  45-Days '
      @ LINE, 89 SAY '|'
      @ LINE, 90 SAY '  60-Days '
      @ LINE, 104 SAY '|'
      @ LINE, 105 SAY '   Total '
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',130)
      LINE= LINE+1
      HEAD= .F.
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
   IF LDGR_BAL=0
      SKIP
      LOOP
   ENDIF
   IF STATUS='CR'
      SKIP
      LOOP
   ENDIF
   MC1= C1
   MC2= C2
   MC3= C3
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ LINE, 1 SAY MPC1 PICTURE '99'
   @ LINE, 3 SAY '-'
   @ LINE, 4 SAY MPC2 PICTURE '99'
   @ LINE, 6 SAY '-'
   @ LINE, 7 SAY MPC3 PICTURE '999'
   @ LINE, 12 SAY LEFT(TITLE,25)
   MBAL= LDGR_BAL
   MBAL1= LDGR_BAL
   MADD1= ADDRESS1
   MADD2= ADDRESS2
   MTOT= LDGR_BAL
   SET COLOR TO 7/1
   SELECT 2
   USE master
   SUM FOR DATE>SDATE.and.DATE<=MDATE.and.C1=MC1.and.C2=MC2.and.C3=MC3 DR_AMOUNT TO XYZ
   USE
   SELECT 2
   USE mast-ref
   ZAP
   APPEND FROM master FOR C1=MC1.and.C2=MC2.and.C3=MC3.and.DATE<=SDATE
   FLUSH
   SORT ON DATE TO mastref
   ZAP
   USE
   SELECT 1
   USE mastref
   GOTO BOTTOM
   MBAL= MBAL-XYZ
   DO WHILE .not. BOF()
      MBAL= MBAL-DR_AMOUNT
      REPLACE BAL WITH MBAL
      REPLACE FLAG WITH 'Y'
      IF MBAL<=0
         EXIT
      ENDIF
      SKIP  -1
   ENDDO
   STORE 0 TO PAGE, CNT
   SELECT 1
   GOTO TOP
   SET FILTER TO FLAG='Y'
   GOTO TOP
   MDR= 0
   MDR= DR_AMOUNT+BAL
   REPLACE DR_AMOUNT WITH MDR
   STORE 0 TO DRAMT15, DRAMT30, DRAMT60, DRAMT45, DRAMTAB, DRAMT90
   STORE 0 TO CRAMT15, CRAMT30, CRAMT60, CRAMT45, CRAMTAB, CRAMT90
   STORE 0 TO DRAMOUNT, CRAMOUNT, TAMOUNT
   DO WHILE .not. EOF()
      DRAMOUNT= DRAMOUNT+DR_AMOUNT
      IF DATE<=SDATE.and.DATE>=SDATE-15
         DRAMT15= DR_AMOUNT+DRAMT15
      ENDIF
      IF DATE<SDATE-15.and.DATE>=SDATE-30
         DRAMT30= DR_AMOUNT+DRAMT30
      ENDIF
      IF DATE<SDATE-30.and.DATE>=SDATE-45
         DRAMT45= DR_AMOUNT+DRAMT45
      ENDIF
      IF DATE<SDATE-45
         DRAMT60= DR_AMOUNT+DRAMT60
      ENDIF
      SKIP
   ENDDO
   DO WHILE .T.
      IF DRAMT15<=MTOT
         @ LINE, 45 SAY DRAMT15 PICTURE '999999,999.99'
         MTOT= MTOT-DRAMT15
         TOT1= TOT1+DRAMT15
      ELSE
         @ LINE, 45 SAY MTOT PICTURE '999999,999.99'
         TOT1= TOT1+MTOT
         EXIT
      ENDIF
      @ LINE, 59 SAY '|'
      IF DRAMT30<=MTOT
         @ LINE, 60 SAY DRAMT30 PICTURE '999999,999.99'
         MTOT= MTOT-DRAMT30
         TOT2= TOT2+DRAMT30
      ELSE
         @ LINE, 60 SAY MTOT PICTURE '999999,999.99'
         TOT2= TOT2+MTOT
         EXIT
      ENDIF
      @ LINE, 74 SAY '|'
      IF DRAMT45<=MTOT
         @ LINE, 75 SAY DRAMT45 PICTURE '999999,999.99'
         MTOT= MTOT-DRAMT45
         TOT3= TOT3+DRAMT45
      ELSE
         @ LINE, 75 SAY MTOT PICTURE '999999,999.99'
         TOT3= TOT3+MTOT
         EXIT
      ENDIF
      @ LINE, 89 SAY '|'
      IF DRAMT60<=MTOT
         @ LINE, 90 SAY DRAMT60 PICTURE '999999,999.99'
         MTOT= MTOT-DRAMT60
         TOT4= TOT4+DRAMT60
      ELSE
         @ LINE, 90 SAY MTOT PICTURE '999999,999.99'
         TOT4= TOT4+MTOT
         EXIT
      ENDIF
      @ LINE, 104 SAY '|'
      MTOTAL= 0
      MTOTAL= DRAMT15+DRAMT30+DRAMT45+DRAMT60
      @ LINE, 105 SAY MTOTAL PICTURE '999999,999.99'
      TOT5= TOT5+MTOTAL
      EXIT
   ENDDO
   SELECT 1
   USE
   SELECT 3
   SKIP
   LINE= LINE+1
   IF P='P'.and.LINE>56
      HEAD= .T.
   ENDIF
ENDDO
LINE= LINE+1
@ LINE, 1 SAY REPLICATE('=',130)
LINE= LINE+1
@ LINE, 1 SAY 'Totals '
@ LINE, 45 SAY TOT1 PICTURE '999999,999.99'
@ LINE, 59 SAY '|'
@ LINE, 60 SAY TOT2 PICTURE '999999,999.99'
@ LINE, 74 SAY '|'
@ LINE, 75 SAY TOT3 PICTURE '999999,999.99'
@ LINE, 89 SAY '|'
@ LINE, 90 SAY TOT4 PICTURE '999999,999.99'
@ LINE, 104 SAY '|'
@ LINE, 105 SAY TOT5 PICTURE '999999,999.99'
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
RUN del mldgr.dbf 
CLEAR
CLOSE ALL
RETURN

PROCEDURE REC-AGE3
CLEAR
CLEAR ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE ldgrcode INDEX ac1
DO WHILE .T.
   CLEAR
   T= '   '
   STORE DATE() TO SDATE, MDATE
   STORE 0 TO MC1, MC2, MC3, MBAL, MBAL1, MTOT, MNET, XYZ
   @ 3, 10 SAY 'Aging Of Accounts Receivble'
   ABC= 0
   DO WHILE ABC=0
      R= 5
      @ 24, 45 SAY 'Press Ctrl Q To Quit '
      @ R, 15 SAY 'Party Code '
      R= R+2
      SET COLOR TO /g
      @ R, 15 SAY '  -  -   '
      SET COLOR TO 
      @ R, 15 GET MC1 PICTURE '@z 99'
      READ
      IF READKEY()=12
         CLOSE ALL
         CLEAR
         RETURN
      ENDIF
      @ 24, 40 CLEAR TO 24, 79
      SET COLOR TO /w
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 18 GET MC2 PICTURE '@z 99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2
      SET COLOR TO 
      @ R, 21 GET MC3 PICTURE '@z 999'
      READ
      IF MC3=0
         LOOP
      ENDIF
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      SET COLOR TO /w
      @ R, 21 SAY MPC3
      SET COLOR TO 
      SELECT 3
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF .not. FOUND()
         @ 23, 20 SAY 'This Code Does Not Exists ... Please Re-try'
         WAIT ''
         @ 23, 19 SAY SPACE(55)
         LOOP
      ENDIF
      SET COLOR TO /w
      @ R, 30 SAY TITLE
      MTITLE= TITLE
      MBAL= LDGR_BAL
      MBAL1= LDGR_BAL
      MADD1= ADDRESS1
      MADD2= ADDRESS2
      MTOT= LDGR_BAL
      SET COLOR TO 
      ABC= 1
   ENDDO
   @ 9, 15 SAY 'As On ....... ' GET MDATE
   READ
   IF READKEY()=12
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 15 SAY 'Due Date .... ' GET SDATE
   READ
   P= ' '
   DO WHILE .not. P$'PSQ'
      @ 24, 50 SAY 'Printer/Screen/Quit - P/SQ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='Q'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   SELECT 2
   USE master
   SUM FOR DATE>SDATE.and.DATE<=MDATE.and.C1=MC1.and.C2=MC2.and.C3=MC3 DR_AMOUNT TO XYZ
   USE
   SELECT 2
   USE mast-ref
   ZAP
   APPEND FROM master FOR C1=MC1.and.C2=MC2.and.C3=MC3.and.DATE<=SDATE
   FLUSH
   SORT ON DATE TO mastref
   DELETE ALL
   USE
   SELECT 1
   USE mastref
   GOTO BOTTOM
   MBAL= MBAL-XYZ
   DO WHILE .not. BOF()
      MBAL= MBAL-DR_AMOUNT
      REPLACE BAL WITH MBAL
      REPLACE FLAG WITH 'Y'
      IF MBAL<=0
         EXIT
      ENDIF
      SKIP  -1
   ENDDO
   STORE 0 TO PAGE, CNT
   HEAD= .T.
   SELECT 1
   GOTO TOP
   SET FILTER TO FLAG='Y'
   GOTO TOP
   MDR= 0
   MDR= DR_AMOUNT+BAL
   REPLACE DR_AMOUNT WITH MDR
   IF P='P'
      SET PRINTER ON
      SET DEVICE TO PRINTER
   ENDIF
   LINE= 2
   DO WHILE .not. EOF()
      IF HEAD
         IF P='Q'
            DO top-prt1
         ELSE
            DO SCREEN
         ENDIF
         LINE= 2
         @ LINE, 35 SAY 'R E M I N D E R '
         LINE= LINE+1
         @ LINE, 35 SAY '--------------- '
         LINE= LINE+1
         @ LINE, 2 SAY 'Messrs '
         LINE= LINE+1
         @ LINE, 2 SAY MTITLE
         @ LINE, 60 SAY 'Date : '
         @ LINE, 67 SAY DATE()
         LINE= LINE+1
         @ LINE, 2 SAY MADD1
         LINE= LINE+1
         @ LINE, 2 SAY MADD2
         LINE= LINE+1
         @ LINE, 1 SAY REPLICATE('=',79)
         LINE= LINE+1
         @ LINE, 5 SAY 'Bill No '
         @ LINE, 20 SAY '|'
         @ LINE, 30 SAY 'Date  '
         @ LINE, 45 SAY '|'
         @ LINE, 50 SAY 'Amount (Rs) '
         LINE= LINE+1
         @ LINE, 1 SAY REPLICATE('-',79)
         LINE= LINE+1
         HEAD= .F.
      ENDIF
      IF DR_AMOUNT=0
         SKIP
         LOOP
      ENDIF
      IF VCH_TYPE=1
         T= 'CR-'
      ENDIF
      IF VCH_TYPE=2
         T= 'CP-'
      ENDIF
      IF VCH_TYPE=3
         T= 'BR-'
      ENDIF
      IF VCH_TYPE=4
         T= 'CP-'
      ENDIF
      IF VCH_TYPE=5
         T= 'JV-'
      ENDIF
      IF VCH_TYPE=6
         T= 'SV-'
      ENDIF
      IF VCH_TYPE=7
         T= 'PV-'
      ENDIF
      MVCH1= '  '
      MVCH2= '     '
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
      IF CNT=0
         @ LINE, 2 SAY 'On Act. Bal '
         CNT= 1
      ELSE
         @ LINE, 2 SAY T
         @ LINE, 5 SAY MVCH1
         @ LINE, 7 SAY '-'
         @ LINE, 8 SAY MVCH2
      ENDIF
      @ LINE, 20 SAY '|'
      @ LINE, 30 SAY DATE
      @ LINE, 45 SAY '|'
      @ LINE, 50 SAY DR_AMOUNT PICTURE '999,999,999.99'
      MNET= MNET+DR_AMOUNT
      LINE= LINE+1
      IF P='S'.and.LINE>=19
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
         HEAD= .T.
      ENDIF
      SKIP
   ENDDO
   @ LINE, 35 SAY REPLICATE('-',45)
   LINE= LINE+1
   @ LINE, 35 SAY 'Total (Rs) '
   @ LINE, 50 SAY MNET PICTURE '999,999,999.99'
   IF P='S'
      @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
      WAIT ''
   ENDIF
   IF P='P'
      EJECT
      @ 0, 0 SAY CHR(18)
      SET PRINTER OFF
      SET DEVICE TO SCREEN
   ENDIF
   SELECT 1
   USE
   RUN del mastref.dbf 
ENDDO
CLEAR
CLOSE ALL
RETURN

PROCEDURE TR-REP
CLEAR
CLOSE ALL
SELECT 1
USE ldgrcode INDEX ac1
SELECT 2
USE tran INDEX tridx
DO WHILE .T.
   CLEAR
   MC1= 0
   STORE DATE() TO SDATE, LDATE
   @ 3, 10 SAY ' Transaction Summary Report '
   @ 23, 1 SAY 'Press Esc To Return '
   @ 10, 10 SAY 'Enter First Level Code '
   @ 10, 35 GET MC1 PICTURE '99'
   READ
   IF READKEY()=12
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   IF MC1=0
      LOOP
   ENDIF
   @ 23, 1 CLEAR TO 23, 79
   @ 12, 10 SAY 'From Date ' GET SDATE
   @ 14, 10 SAY 'To.. Date ' GET LDATE
   READ
   SELECT 2
   SET FILTER TO C1=MC1.and.DATE>=SDATE.and.DATE<=LDATE
   COUNT TO COTN
   IF COTN=0
      CLEAR
      @ 10, 25 SAY 'No Report For Given Dates'
      @ 24, 25 SAY 'Press Any Key For Menu '
      WAIT ''
      LOOP
   ENDIF
   P= ' '
   DO WHILE .not. P$'SPQ'
      @ 24, 50 SAY 'Printer/Screen/Quit - P/S/Q ' GET P FUNCTION '!'
      READ
   ENDDO
   CLEAR
   IF P='Q'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF P='P'
      CLEAR
      SET PRINTER ON
      SET DEVICE TO PRINTER
   ENDIF
   SELECT 2
   GOTO TOP
   HEAD= .T.
   PAGE= 0
   DO WHILE .not. EOF()
      IF HEAD
         IF P='S'
            DO screen
         ENDIF
         IF P='P'
            DO top-prt1
         ENDIF
         ROW= 2
         @ ROW, 28 SAY 'Transactions Summary '
         ROW= ROW+2
         PAGE= PAGE+1
         @ ROW, 1 SAY 'From : '
         @ ROW, 9 SAY SDATE
         @ ROW, 20 SAY 'To : '
         @ ROW, 25 SAY LDATE
         @ ROW, 67 SAY 'Page # '
         @ ROW, 75 SAY PAGE PICTURE '99'
         ROW= ROW+1
         @ ROW, 1 SAY REPLICATE('-',79)
         ROW= ROW+1
         @ ROW, 1 SAY 'Code     '
         @ ROW, 12 SAY 'Title    '
         @ ROW, 41 SAY ' Debits  '
         @ ROW, 55 SAY ' Credits '
         @ ROW, 69 SAY 'Net Amount '
         ROW= ROW+1
         @ ROW, 1 SAY REPLICATE('=',79)
         HEAD= .F.
         ROW= ROW+1
      ENDIF
      MC1= C1
      MC2= C2
      MC3= C3
      STORE 0 TO MDR, MCR
      DO WHILE C1=MC1.and.C2=MC2.and.C3=MC3
         IF C1<>MC1.and.C2<>MC2.and.C3<>MC3
            EXIT
         ENDIF
         IF DR_CR='DR'
            MDR= MDR+AMOUNT
         ELSE
            MCR= MCR+AMOUNT
         ENDIF
         SKIP
      ENDDO
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ ROW, 1 SAY MPC1 PICTURE '99'
      @ ROW, 3 SAY '-'
      @ ROW, 4 SAY MPC2 PICTURE '99'
      @ ROW, 6 SAY '-'
      @ ROW, 7 SAY MPC3 PICTURE '999'
      SELECT 1
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF FOUND()
         @ ROW, 12 SAY LEFT(TITLE,25)
      ELSE
         @ ROW, 12 SAY ' '
      ENDIF
      @ ROW, 39 SAY MDR PICTURE '99,999,999.99'
      @ ROW, 53 SAY MCR PICTURE '99,999,999.99'
      NET= 0
      NET= MDR-MCR
      @ ROW, 68 SAY NET PICTURE '9999,999.99'
      SELECT 2
      ROW= ROW+1
      IF P='S'.and.ROW>21
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
         HEAD= .T.
      ENDIF
      IF P='P'.and.ROW>56
         HEAD= .T.
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
ENDDO
RETURN

PROCEDURE REP-MEN2
CLEAR
STORE DATE() TO DATE
SET DATE brit
MDATE= DATE()
DO chk
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 25 SAY ' R E P O R T S   M E N U - 2'
   SET COLOR TO 7/1
   @ 8, 25 PROMPT ' 1 - Trial Balance Reports     '
   @ 9, 25 PROMPT ' 2 - Profit & Loss Reports     '
   @ 10, 25 PROMPT ' 3 - Balance Sheet             '
   @ 11, 25 PROMPT ' 4 - Budget Versus Actual Performance '
   @ 12, 25 TO 12, 63
   SET COLOR TO +7/1
   @ 13, 25 SAY '    Convert To Lotus Format '
   SET COLOR TO 7/1
   @ 14, 25 TO 14, 63
   @ 15, 25 PROMPT ' 5 - Trial Balance -- Detailed '
   @ 16, 25 PROMPT ' 6 - Trial Balance -- Summary  '
   @ 17, 25 PROMPT ' 7 - Profit & Loss -- Detailed '
   @ 18, 25 PROMPT ' 8 - Profit & Loss -- Summary  '
   @ 19, 25 PROMPT ' 9 - Balance Sheet -- Detailed '
   @ 21, 25 PROMPT '10 - Return to Previous Menu   '
   CHOICE= 0
   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         DO trl-men
      CASE CHOICE=2
         DO pl-men
      CASE CHOICE=3
         DO bs-c1
      CASE CHOICE=4
         DO bud-comp
      CASE CHOICE=5
         DO trl-dtl
      CASE CHOICE=6
         DO trl-sum
      CASE CHOICE=7
         DO pl-dtl
      CASE CHOICE=8
         DO pl-sum
      CASE CHOICE=9
         DO bs-sum
      CASE CHOICE=10
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE TRL-MEN
CLEAR
STORE DATE() TO DATE
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 27 SAY ' TRIAL  BALANCE  REPORTS'
   SET COLOR TO 7/1
   @ 8, 25 PROMPT '1 - Trial Balance -- Detailed'
   @ 9, 25 PROMPT '2 - Trial Balance -- Summary '
   @ 10, 25 PROMPT '3 - Trial Balance -- Detailed With Group Totals'
   @ 11, 25 PROMPT '4 - Trial Balance -- Previous Date '
   @ 12, 25 PROMPT '5 - Trial Balance -- Between Two Dates'
   @ 13, 25 PROMPT '6 - User Definable Trial Balance   '
   @ 15, 25 PROMPT '9 - Return to Previous Menu '
   CHOICE= 0
   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         DO c3-trl
      CASE CHOICE=2
         DO c2-trl
      CASE CHOICE=3
         DO c3-trl1
      CASE CHOICE=4
         DO prv-trl
      CASE CHOICE=5
         DO date-trl
      CASE CHOICE=6
         DO utrl-men
      CASE CHOICE=7
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE C3-TRL
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE
@ 2, 2 SAY 'Subsideries Trial Balance '
ZEROBAL= ' '
DO WHILE .not. ZEROBAL$'YN'
   @ 4, 2 SAY 'Do You Want To Include Ledger Heads Having Zero Balance - Y/N ' GET ZEROBAL FUNCTION '!'
   READ
ENDDO
@ 4, 2 SAY SPACE(70)
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 4, 2 SAY 'Printer/Screen/Quit -  P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
USE master
INDEX ON DATE TO mastdate
GOTO BOTTOM
STORE DATE TO MTDATE
CLEAR
@ 3, 5 SAY 'Your last posted voucher is dated '
@ 3, 42 SAY MTDATE
@ 4, 5 SAY 'Do you want to dispaly same date on your Trial Balance'
@ 5, 5 SAY 'Or enter the date you want to dispaly  ' GET MTDATE
READ
GOTO TOP
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
STORE 0 TO MCODE, WRECNO, MSRNO, WCODE, MNO
CLEAR
IF ZEROBAL='N'
   TIT= ' '
   DO WHILE .not. TIT$'YN'
      @ 4, 2 SAY 'Report With Control Heads - Y/N ' GET TIT FUNCTION '!'
      READ
   ENDDO
   @ 4, 2 SAY SPACE(70)
ENDIF
IF ANS='P'
   CLEAR
   @ 2, 5 SAY 'No Of Copies       ' GET MNO PICTURE '99'
   READ
   CLEAR
   @ 8, 17 SAY 'Subsideries Trial Balance Printing'
   @ 10, 17 SAY 'Please use 80 column (10 inches wide) continous'
   @ 11, 17 SAY 'printer paper and set Top Of Form'
   @ 12, 17 SAY 'Press ENTER when ready...........'
   WAIT ''
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
IF ANS='S'
   MNO= 1
ENDIF
ABC= 1
DO WHILE ABC<=MNO
   PAGE= 1
   HEADING= .T.
   GOTO TOP
   STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE
   CLEAR
   DO WHILE .not. EOF()
      IF HEADING
         IF ANS='S'
            DO screen
            @ 2, 20 SAY 'TRIAL  BALANCE  AS  ON: '
         ENDIF
         IF ANS='P'
            DO top-prt1
            @ 2, 23 SAY 'TRIAL BALANCE AS ON: '
         ENDIF
         @ 2, 44 SAY MTDATE
         @ 2, 72 SAY 'PAGE: '
         @ 2, 78 SAY PAGE PICTURE '99'
         @ 3, 1 SAY REPLICATE('-',79)
         @ 4, 1 SAY 'CODE-----'
         @ 4, 11 SAY '   ---- ACCOUNT  TITLE ----'
         @ 4, 50 SAY 'D E B I T'
         @ 4, 69 SAY 'C R E D I T'
         @ 5, 1 SAY REPLICATE('-',79)
         LINE= 6
         HEADING= .F.
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      STORE SPACE(30) TO WTITLE
      STORE TITLE TO WTITLE
      IF ZEROBAL='N'.and.TIT='N'
         IF LDGR_BAL=0
            SKIP
            LOOP
         ENDIF
      ENDIF
      IF C3<>0
         IF ZEROBAL='N'.and.TIT='Y'
            IF LDGR_BAL=0
               SKIP
               LOOP
            ENDIF
         ENDIF
      ENDIF
      IF C3=0
         LINE= LINE+1
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
         @ LINE, 1 SAY MPC1
         @ LINE, 3 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
         @ LINE, 4 SAY MPC2
         @ LINE, 6 SAY '-'
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
         @ LINE, 7 SAY MPC3
         @ LINE, 11 SAY TITLE
         MLEN= LEN(RTRIM(TITLE))
         LINE= LINE+1
         @ LINE, 1 SAY REPLICATE('-',15+MLEN)
         LINE= LINE+1
         SKIP
         IF ANS='S'.and.LINE>21
            IF EOF()
               EXIT
            ENDIF
            XYZ= ' '
            DO WHILE .not. XYZ$'CQ'
               SET CONFIRM OFF
               @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
               READ
            ENDDO
            IF XYZ='Q'
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            CLEAR
            PAGE= PAGE+1
            HEADING= .T.
         ENDIF
         IF ANS='P'.and.LINE>=56
            PAGE= PAGE+1
            HEADING= .T.
         ENDIF
         LOOP
      ENDIF
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ LINE, 1 SAY MPC1
      @ LINE, 3 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ LINE, 4 SAY MPC2
      @ LINE, 6 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ LINE, 7 SAY MPC3
      @ LINE, 11 SAY WTITLE
      COL= IIF(STATUS='DR',45,63)
      @ LINE, COL SAY ABS(LDGR_BAL) PICTURE '9,999,999,999.99'
      IF COL=45
         DRTOT= DRTOT+LDGR_BAL
      ELSE
         CRTOT= CRTOT+LDGR_BAL
      ENDIF
      LINE= LINE+1
      IF .not. EOF()
         SKIP
      ENDIF
      IF ANS='S'.and.LINE>21
         IF EOF()
            EXIT
         ENDIF
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            SET CONFIRM OFF
            @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         PAGE= PAGE+1
         HEADING= .T.
      ENDIF
      IF ANS='P'.and.LINE>=56
         PAGE= PAGE+1
         HEADING= .T.
      ENDIF
   ENDDO
   IF ANS='S'.and.LINE>21
      LINE= LINE-1
   ENDIF
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   @ LINE, 10 SAY '        T O T A L S :'
   @ LINE, 47 SAY DRTOT PICTURE '9999999,999.99'
   @ LINE, 65 SAY CRTOT PICTURE '9999999,999.99'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   IF ANS='S'
      @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
      WAIT ''
   ENDIF
   IF ANS='P'
      EJECT
   ENDIF
   ABC= ABC+1
ENDDO
IF ANS='P'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
SET FILTER TO
CLOSE ALL
RETURN

PROCEDURE C2-TRL
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
ON ERROR  to
SET TALK OFF
USE master
INDEX ON DATE TO mastdate
GOTO BOTTOM
STORE DATE TO MTDATE
CLEAR
@ 2, 5 SAY 'Level-2  Trial Balance '
@ 5, 5 SAY 'Your last posted voucher is dated '
@ 5, 42 SAY MTDATE
@ 6, 5 SAY 'Do you want to dispaly same date on your Trial Balance'
@ 7, 5 SAY 'Or enter the date you want to dispaly  ' GET MTDATE
READ
STORE 0 TO TDR, TCR, LINE, MC1, MC2, MC3, GTOTDR, GTOTCR
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 23, 47 SAY 'Printer/Screen/Quit -  P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
SELECT 1
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
MCODE= 0
HEADING= .T.
PAGE= 1
IF ANS='P'
   CLEAR
   @ 8, 17 SAY 'Level-2  Trial Balance Printing'
   @ 10, 17 SAY 'Please use 80 column (10 inches wide) continous'
   @ 11, 17 SAY 'printer paper and set Top Of Form'
   @ 12, 17 SAY 'Press ENTER when ready...........'
   WAIT ''
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 1
GOTO TOP
MC1= C1
STORE SPACE(25) TO WTITLE
STORE SPACE(25) TO WGTITLE
C= 45
DO WHILE .not. EOF()
   SELECT 1
   IF LDGR_BAL=0.and.C3<>0
      SKIP
      LOOP
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   IF HEADING
      IF ANS='S'
         DO screen
      ENDIF
      IF ANS='P'
         DO top-prt1
      ENDIF
      @ 2, 14 SAY 'LEVEL-2  TRIAL BALANCE PRINTING AS ON: '
      @ 2, 52 SAY MTDATE
      @ 2, 72 SAY 'PAGE: '
      @ 2, 78 SAY PAGE PICTURE '99'
      @ 3, 1 SAY REPLICATE('-',79)
      @ 4, 1 SAY 'CODE-----'
      @ 4, 14 SAY 'T I T L E -----------------'
      @ 4, 43 SAY '-------D E B I T'
      @ 4, 62 SAY '-------C R E D I T'
      @ 5, 1 SAY REPLICATE('-',79)
      LINE= 6
      HEADING= .F.
   ENDIF
   STORE SPACE(30) TO WGTITLE
   IF C3=0
      STORE TITLE TO WGTITLE
   ENDIF
   STORE TITLE TO WTITLE
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
   MC1= C1
   MC2= C2
   TAMT= 0
   STORE 0 TO TDR, TCR
   DO WHILE C1=MC1.and.C2=MC2
      IF STATUS='DR'
         TDR= TDR+LDGR_BAL
      ELSE
         TCR= TCR+LDGR_BAL
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      SKIP
   ENDDO
   IF TDR>TCR
      TAMT= TDR-TCR
      GTOTDR= GTOTDR+TAMT
      C= 45
   ENDIF
   IF TCR>TDR
      TAMT= TCR-TDR
      GTOTCR= GTOTCR+TAMT
      C= 60
   ENDIF
   IF TAMT=0
      LOOP
   ENDIF
   @ LINE, 1 SAY MPC1
   @ LINE, 3 SAY '-'
   @ LINE, 4 SAY MPC2
   IF WGTITLE<>SPACE(30)
      @ LINE, 14 SAY WGTITLE
   ELSE
      @ LINE, 14 SAY 'Group Total'
   ENDIF
   @ LINE, C SAY TAMT PICTURE '99,999,999,999.99'
   LINE= LINE+1
   IF ANS='S'.and.LINE>20
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         SET CONFIRM OFF
         @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
         READ
         CLEAR
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      PAGE= PAGE+1
      HEADING= .T.
   ENDIF
   IF ANS='P'.and.LINE>55
      PAGE= PAGE+1
      HEADING= .T.
   ENDIF
ENDDO
LINE= LINE+1
@ LINE, 1 SAY REPLICATE('-',79)
LINE= LINE+1
@ LINE, 20 SAY 'TOTALS:'
@ LINE, 45 SAY GTOTDR PICTURE '99999999,999.99'
@ LINE, 65 SAY GTOTCR PICTURE '99999999,999.99'
LINE= LINE+1
@ LINE, 1 SAY REPLICATE('-',79)
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
SET FILTER TO
CLOSE ALL
RETURN

PROCEDURE C3-TRL1
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE
@ 2, 2 SAY 'Subsideries Trial Balance With Group Totals '
ZEROBAL= ' '
DO WHILE .not. ZEROBAL$'YN'
   @ 4, 2 SAY 'Do You Want To Include Ledger Heads Having Zero Balance - Y/N ' GET ZEROBAL FUNCTION '!'
   READ
ENDDO
@ 4, 2 SAY SPACE(70)
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 4, 2 SAY 'Printer/Screen/Quit -  P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
USE master
INDEX ON DATE TO mastdate
GOTO BOTTOM
STORE DATE TO MTDATE
CLEAR
@ 3, 5 SAY 'Your last posted voucher is dated '
@ 3, 42 SAY MTDATE
@ 4, 5 SAY 'Do you want to dispaly same date on your Trial Balance'
@ 5, 5 SAY 'Or enter the date you want to dispaly  ' GET MTDATE
READ
GOTO TOP
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
STORE 0 TO MCODE, WRECNO, MSRNO, WCODE, MNO
CLEAR
IF ANS='P'
   CLEAR
   @ 2, 5 SAY 'No Of Copies       ' GET MNO PICTURE '99'
   READ
   CLEAR
   @ 8, 17 SAY 'Subsideries Trial Balance Printing'
   @ 10, 17 SAY 'Please use 80 column (10 inches wide) continous'
   @ 11, 17 SAY 'printer paper and set Top Of Form'
   @ 12, 17 SAY 'Press ENTER when ready...........'
   WAIT ''
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
IF ANS='S'
   MNO= 1
ENDIF
ABC= 1
DO WHILE ABC<=MNO
   PAGE= 1
   HEADING= .T.
   GOTO TOP
   STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE
   CLEAR
   DO WHILE .not. EOF()
      IF HEADING
         IF ANS='S'
            DO screen
            @ 2, 20 SAY 'TRIAL  BALANCE  AS  ON: '
         ENDIF
         IF ANS='P'
            DO top-prt1
            @ 2, 23 SAY 'TRIAL BALANCE AS ON: '
         ENDIF
         @ 2, 44 SAY MTDATE
         @ 2, 72 SAY 'PAGE: '
         @ 2, 78 SAY PAGE PICTURE '99'
         @ 3, 1 SAY REPLICATE('-',79)
         @ 4, 1 SAY 'CODE-----'
         @ 4, 11 SAY '   ---- ACCOUNT  TITLE ----'
         @ 4, 50 SAY 'D E B I T'
         @ 4, 69 SAY 'C R E D I T'
         @ 5, 1 SAY REPLICATE('-',79)
         LINE= 6
         HEADING= .F.
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      STORE SPACE(30) TO WTITLE
      STORE TITLE TO WTITLE
      MC1= C1
      MC2= C2
      STORE 0 TO MDR, MCR
      DO WHILE C1=MC1.and.C2=MC2
         IF HEADING
            IF ANS='S'
               DO screen
               @ 2, 20 SAY 'TRIAL  BALANCE  AS  ON: '
            ENDIF
            IF ANS='P'
               DO top-prt1
               @ 2, 23 SAY 'TRIAL BALANCE AS ON: '
            ENDIF
            @ 2, 44 SAY MTDATE
            @ 2, 72 SAY 'PAGE: '
            @ 2, 78 SAY PAGE PICTURE '99'
            @ 3, 1 SAY REPLICATE('-',79)
            @ 4, 1 SAY 'CODE-----'
            @ 4, 11 SAY '   ---- ACCOUNT  TITLE ----'
            @ 4, 50 SAY 'D E B I T'
            @ 4, 69 SAY 'C R E D I T'
            @ 5, 1 SAY REPLICATE('-',79)
            LINE= 6
            HEADING= .F.
         ENDIF
         IF C1<>MC1.and.C2<>MC2
            EXIT
         ENDIF
         STORE SPACE(30) TO WTITLE
         STORE TITLE TO WTITLE
         IF C3<>0
            IF ZEROBAL='N'
               IF LDGR_BAL=0
                  SKIP
                  LOOP
               ENDIF
            ENDIF
         ENDIF
         IF C3=0
            LINE= LINE+1
            MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
            @ LINE, 1 SAY MPC1
            @ LINE, 3 SAY '-'
            MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
            @ LINE, 4 SAY MPC2
            @ LINE, 6 SAY '-'
            MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
            @ LINE, 7 SAY MPC3
            @ LINE, 11 SAY TITLE
            MLEN= LEN(RTRIM(TITLE))
            LINE= LINE+1
            @ LINE, 1 SAY REPLICATE('-',15+MLEN)
            LINE= LINE+1
            SKIP
            IF ANS='S'.and.LINE>19
               IF EOF()
                  EXIT
               ENDIF
               XYZ= ' '
               DO WHILE .not. XYZ$'CQ'
                  SET CONFIRM OFF
                  @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
                  READ
               ENDDO
               IF XYZ='Q'
                  CLEAR
                  CLOSE ALL
                  RETURN
               ENDIF
               CLEAR
               PAGE= PAGE+1
               HEADING= .T.
            ENDIF
            IF ANS='P'.and.LINE>=56
               PAGE= PAGE+1
               HEADING= .T.
            ENDIF
            LOOP
         ENDIF
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
         @ LINE, 1 SAY MPC1
         @ LINE, 3 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
         @ LINE, 4 SAY MPC2
         @ LINE, 6 SAY '-'
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
         @ LINE, 7 SAY MPC3
         @ LINE, 11 SAY WTITLE
         COL= IIF(STATUS='DR',45,63)
         @ LINE, COL SAY ABS(LDGR_BAL) PICTURE '9,999,999,999.99'
         IF COL=45
            DRTOT= DRTOT+LDGR_BAL
            MDR= MDR+LDGR_BAL
         ELSE
            CRTOT= CRTOT+LDGR_BAL
            MCR= MCR+LDGR_BAL
         ENDIF
         LINE= LINE+1
         IF .not. EOF()
            SKIP
         ENDIF
         IF ANS='S'.and.LINE>19
            IF EOF()
               EXIT
            ENDIF
            XYZ= ' '
            DO WHILE .not. XYZ$'CQ'
               SET CONFIRM OFF
               @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
               READ
            ENDDO
            IF XYZ='Q'
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            CLEAR
            PAGE= PAGE+1
            HEADING= .T.
         ENDIF
         IF ANS='P'.and.LINE>=56
            PAGE= PAGE+1
            HEADING= .T.
         ENDIF
      ENDDO
      IF MDR<>0.or.MCR<>0
         @ LINE, 50 SAY REPLICATE('-',30)
         LINE= LINE+1
         @ LINE, 47 SAY MDR PICTURE '9999999,999.99'
         @ LINE, 65 SAY MCR PICTURE '9999999,999.99'
         LINE= LINE+1
         @ LINE, 50 SAY REPLICATE('-',30)
      ENDIF
   ENDDO
   IF ANS='S'.and.LINE>20
      LINE= LINE-1
   ENDIF
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   @ LINE, 10 SAY '        T O T A L S :'
   @ LINE, 47 SAY DRTOT PICTURE '9999999,999.99'
   @ LINE, 65 SAY CRTOT PICTURE '9999999,999.99'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   IF ANS='S'
      @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
      WAIT ''
   ENDIF
   IF ANS='P'
      EJECT
   ENDIF
   ABC= ABC+1
ENDDO
IF ANS='P'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
SET FILTER TO
CLOSE ALL
RETURN

PROCEDURE C1-TRL2
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
STORE 0 TO DRTOT, CRTOT, TCR, TDR, GTDR, GTCR, LINE
@ 2, 2 SAY 'Level-1  Trial Balance '
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 3, 2 SAY 'Printer/Screen/Quit -  P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
MCODE= 0
STORE 0 TO TOTDR, TOTCR, TDR, TCR
WGTITLE= SPACE(30)
HEADING= .T.
PAGE= 1
MSRNO= 0
WCODE= 0
IF ANS='P'
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
GOTO TOP
MC1= C1
C= 45
DO WHILE .not. EOF()
   IF LDGR_BAL=0
      SKIP
      LOOP
   ENDIF
   MC1= C1
   STORE 0 TO TDR, TCR, TAMT
   DO WHILE C1=MC1
      IF EOF()
         EXIT
      ENDIF
      IF HEADING
         IF ANS='S'
            DO screen
         ENDIF
         IF ANS='P'
            DO top-prt1
         ENDIF
         @ 2, 18 SAY 'LEVEL-1  TRIAL  BALANCE  AS  ON: '
         @ 2, 52 SAY DATE()
         @ 2, 72 SAY 'PAGE: '
         @ 2, 78 SAY PAGE PICTURE '99'
         @ 3, 1 SAY REPLICATE('-',79)
         @ 4, 1 SAY 'CODE-----'
         @ 4, 14 SAY 'T I T L E -----------------'
         @ 4, 43 SAY '-------D E B I T'
         @ 4, 62 SAY '-------C R E D I T'
         @ 5, 1 SAY REPLICATE('-',79)
         LINE= 6
         HEADING= .F.
      ENDIF
      IF C3=0.and.C2=0
         STORE TITLE TO WGTITLE
      ENDIF
      STORE TITLE TO WTITLE
      IF STATUS='DR'
         TDR= TDR+LDGR_BAL
      ELSE
         TCR= TCR+LDGR_BAL
      ENDIF
      IF EOF()
         EXIT
      ENDIF
      SKIP
   ENDDO
   IF TDR>TCR
      TAMT= TDR-TCR
      TOTDR= TOTDR+TAMT
      C= 45
   ENDIF
   IF TCR>TDR
      TAMT= TCR-TDR
      TOTCR= TOTCR+TAMT
      C= 60
   ENDIF
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   @ LINE, 1 SAY MPC1
   @ LINE, C SAY TAMT PICTURE '99,999,999,999.99'
   IF ANS='S'.and.LINE>17
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         SET CONFIRM OFF
         @ 24, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
         READ
      ENDDO
      CLEAR
      IF XYZ='Q'
         CLOSE ALL
         RETURN
      ENDIF
      PAGE= PAGE+1
      HEADING= .T.
   ENDIF
   IF ANS='P'.and.LINE>53
      PAGE= PAGE+1
      HEADING= .T.
   ENDIF
   STORE 0 TO TDR, TCR
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   IF WGTITLE<>SPACE(30)
      @ LINE, 1 SAY 'TOTAL '
      @ LINE, 10 SAY WGTITLE
   ELSE
      @ LINE, 1 SAY 'GROUP TOTAL:'
   ENDIF
   WGTITLE= SPACE(30)
   @ LINE, C SAY TAMT PICTURE '99999999,999.99'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',79)
   LINE= LINE+1
   STORE 0 TO GTDR, GTCR
   MC1= C1
   LINE= LINE+1
ENDDO
LINE= LINE+1
IF ANS='S'.and.LINE>20
   XYZ= ' '
   DO WHILE .not. XYZ$'CQ'
      SET CONFIRM OFF
      @ 24, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
      READ
   ENDDO
   CLEAR
   IF XYZ='Q'
      CLOSE ALL
      RETURN
   ENDIF
   PAGE= PAGE+1
   HEADING= .T.
ENDIF
IF ANS='P'.and.LINE>53
   PAGE= PAGE+1
   HEADING= .T.
ENDIF
@ LINE, 1 SAY REPLICATE('-',79)
LINE= LINE+1
@ LINE, 10 SAY 'G R A N D    T O T A L S :'
@ LINE, 45 SAY TOTDR PICTURE '99999999,999.99'
@ LINE, 60 SAY TOTCR PICTURE '99999999,999.99'
LINE= LINE+1
@ LINE, 1 SAY REPLICATE('-',79)
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
SET FILTER TO
CLOSE ALL
RETURN

PROCEDURE PRV-TRL
CLEAR
CLOSE ALL
SET COLOR TO 7/1
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE
STORE DATE() TO MTDATE
CLEAR
@ 8, 15 SAY 'Enter the date for Previous Trial Balance' GET MTDATE
READ
IF READKEY()=12
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
@ 13, 23 SAY 'Please wait - Calculations in progress ...'
SELECT 2
USE ldgrcode
COPY TO mldgr
USE mldgr
REPLACE ALL LDGR_BAL WITH 0, STATUS WITH '  '
REPLACE ALL LDGR_BAL WITH OPEN_BAL, STATUS WITH OPEN_STAT
GOTO TOP
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO mac2
SELECT 1
USE tran
COPY TO mtran FOR DATE<=MTDATE
USE mtran
REPLACE ALL FLAG WITH ' '
CLOSE ALL
SET DATE brit
HEADING= .T.
STORE DATE() TO MPDATE
SELECT 1
USE mtran
INDEX ON DATE TO trandate
GOTO BOTTOM
SELECT 2
USE mldgr INDEX mac2
SELECT 3
USE master
COPY TO mmast STRUCTURE
USE mmast
STORE 0 TO MDB_CODE, MCR_CODE, WSRNO, WRECNO, MSRNO, WAMOUNT, MAMOUNT, Q
MVOUCH_NO= SPACE(8)
MDISC= SPACE(30)
MDATE= DATE()
STORE '  ' TO ST, RST
SELECT 1
SET FILTER TO FLAG=' '
GOTO TOP
R= 2
DO WHILE .not. EOF()
   SELECT 1
   STORE STR(C1,2)+STR(C2,2)+STR(C3,3) TO MCODE
   SELECT 2
   SEEK MCODE
   IF .not. FOUND()
      IF HEADING
         @ R, 10 SAY 'The Following Account Codes Do Not Exist In Code File'
         R= R+1
         @ R, 10 SAY 'These Account Codes Were Probably Deleted Before Data Posting'
         R= R+1
         @ R, 10 SAY 'Please Correct Your Vouchers Or Create Missing Ledger Heads'
         R= R+1
         @ R, 10 SAY 'Before Data Posting'
         R= R+1
         @ R, 1 TO R, 78
         R= R+1
         @ R, 1 SAY 'Voucher No'
         @ R, 13 SAY 'Voucher Type'
         @ R, 28 SAY 'Account Code'
         R= R+1
         @ R, 1 TO R, 78
         HEADING= .F.
      ENDIF
      SELECT 1
      R= R+1
      @ R, 1 SAY VCH_NO
      @ R, 18 SAY VCH_TYPE
      @ R, 28 SAY C1
      @ R, 30 SAY '-'
      @ R, 31 SAY C2
      @ R, 33 SAY '-'
      @ R, 34 SAY C3
      Q= 21
   ENDIF
   IF R>=21
      WAIT
      CLEAR
      R= 2
      HEADING= .T.
   ENDIF
   SELECT 1
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
GOTO TOP
DO WHILE .not. EOF()
   DO chk
   IF FLAG='Y'
      SKIP
      LOOP
   ENDIF
   IF DR_CR='DR'
      MDR_AMOUNT= AMOUNT
      MCR_AMOUNT= 0
   ELSE
      MCR_AMOUNT= AMOUNT
      MDR_AMOUNT= 0
   ENDIF
   MDATE= DATE
   MC1= C1
   MC2= C2
   MC3= C3
   MVCH_TYPE= VCH_TYPE
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   MDISC= DESCRIP
   SELECT 2
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF FOUND()
      WSRNO= SR_NO+1
      IF  B->STATUS='  '
         IF MDR_AMOUNT>0
            WAMOUNT= MDR_AMOUNT
            ST= 'DR'
         ELSE
            WAMOUNT= MCR_AMOUNT
            ST= 'CR'
         ENDIF
      ENDIF
      IF  B->STATUS='DR'
         IF MDR_AMOUNT>0
            WAMOUNT= LDGR_BAL+MDR_AMOUNT
            ST= 'DR'
         ELSE
            WAMOUNT= LDGR_BAL-MCR_AMOUNT
            IF WAMOUNT>0
               ST= 'DR'
            ELSE
               ST= 'CR'
            ENDIF
         ENDIF
      ENDIF
      IF  B->STATUS='CR'
         IF MCR_AMOUNT>0
            WAMOUNT= LDGR_BAL+MCR_AMOUNT
            ST= 'CR'
         ELSE
            WAMOUNT= LDGR_BAL-MDR_AMOUNT
            IF WAMOUNT>0
               ST= 'CR'
            ELSE
               ST= 'DR'
            ENDIF
         ENDIF
      ENDIF
      REPLACE LDGR_BAL WITH ABS(WAMOUNT)
      REPLACE SR_NO WITH WSRNO
      REPLACE B->STATUS WITH ST
   ENDIF
   SELECT 3
   APPEND BLANK
   REPLACE DATE WITH MDATE
   REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
   REPLACE BL_AMOUNT WITH ABS(WAMOUNT)
   IF MDR_AMOUNT>0
      REPLACE DR_AMOUNT WITH MDR_AMOUNT
   ELSE
      REPLACE CR_AMOUNT WITH MCR_AMOUNT
   ENDIF
   REPLACE DESC WITH MDISC
   REPLACE VCH_TYPE WITH MVCH_TYPE
   REPLACE VCH_NO1 WITH MVCH_NO1, SR_NO WITH WSRNO
   REPLACE C->STATUS WITH ST, VCH_NO2 WITH MVCH_NO2
   WSRNO= 0
   SELECT 1
   REPLACE FLAG WITH 'Y'
   SKIP
ENDDO
SELECT 3
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
CLOSE ALL
CLEAR
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE
CLEAR
@ 2, 2 SAY 'Subsideries Trial Balance '
ZEROBAL= ' '
DO WHILE .not. ZEROBAL$'YN'
   @ 4, 2 SAY 'Do You Want To Include Ledger Heads Having Zero Balance - Y/N ' GET ZEROBAL FUNCTION '!'
   READ
ENDDO
@ 4, 2 SAY SPACE(70)
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 4, 2 SAY 'Printer/Screen/Quit -  P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
USE mldgr
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO mac2
STORE 0 TO MCODE, WRECNO, MSRNO, WCODE
PAGE= 1
HEADING= .T.
IF ANS='P'
   CLEAR
   @ 8, 17 SAY 'Previous Trial Balance Printing'
   @ 10, 17 SAY 'Please use 80 column (10 inches wide) continous'
   @ 11, 17 SAY 'printer paper and set Top Of Form'
   @ 12, 17 SAY 'Press ENTER when ready...........'
   WAIT ''
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
GOTO TOP
WCODE= 0
CLEAR
DO WHILE .not. EOF()
   IF ZEROBAL='N'
      IF LDGR_BAL=0
         SKIP
         LOOP
      ENDIF
   ENDIF
   IF HEADING
      IF ANS='S'
         DO screen
         @ 2, 20 SAY 'TRIAL  BALANCE  AS  ON: '
      ENDIF
      IF ANS='P'
         DO top-prt1
         @ 2, 23 SAY 'TRIAL BALANCE AS ON: '
      ENDIF
      @ 2, 44 SAY MTDATE
      @ 2, 72 SAY 'PAGE: '
      @ 2, 78 SAY PAGE PICTURE '99'
      @ 3, 1 SAY REPLICATE('-',79)
      @ 4, 1 SAY 'CODE-----'
      @ 4, 11 SAY '   ---- ACCOUNT  TITLE ----'
      @ 4, 57 SAY 'D E B I T'
      @ 4, 69 SAY 'C R E D I T'
      @ 5, 1 SAY REPLICATE('-',79)
      LINE= 6
      HEADING= .F.
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   STORE SPACE(30) TO WTITLE
   STORE TITLE TO WTITLE
   IF C3=0
      IF ZEROBAL='N'
         IF LDGR_BAL=0
            SKIP
            LOOP
         ENDIF
      ENDIF
      LINE= LINE+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ LINE, 1 SAY MPC1
      @ LINE, 3 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ LINE, 4 SAY MPC2
      @ LINE, 6 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ LINE, 7 SAY MPC3
      @ LINE, 14 SAY TITLE
      MLEN= LEN(RTRIM(TITLE))
      LINE= LINE+1
      @ LINE, 1 SAY REPLICATE('-',15+MLEN)
      LINE= LINE+1
      SKIP
      IF ANS='S'.and.LINE>21
         IF EOF()
            EXIT
         ENDIF
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            SET CONFIRM OFF
            @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         PAGE= PAGE+1
         HEADING= .T.
      ENDIF
      IF ANS='P'.and.LINE>=56
         PAGE= PAGE+1
         HEADING= .T.
      ENDIF
      LOOP
   ENDIF
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
   @ LINE, 3 SAY MPC1
   @ LINE, 5 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
   @ LINE, 6 SAY MPC2
   @ LINE, 8 SAY '-'
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
   @ LINE, 9 SAY MPC3
   @ LINE, 16 SAY WTITLE
   COL= IIF(STATUS='DR',54,63)
   @ LINE, COL SAY ABS(LDGR_BAL) PICTURE '9,999,999,999.99'
   IF COL=54
      DRTOT= DRTOT+LDGR_BAL
   ELSE
      CRTOT= CRTOT+LDGR_BAL
   ENDIF
   LINE= LINE+1
   IF .not. EOF()
      SKIP
   ENDIF
   IF ANS='S'.and.LINE>21
      IF EOF()
         EXIT
      ENDIF
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         SET CONFIRM OFF
         @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      PAGE= PAGE+1
      HEADING= .T.
   ENDIF
   IF ANS='P'.and.LINE>=56
      PAGE= PAGE+1
      HEADING= .T.
   ENDIF
ENDDO
IF ANS='S'.and.LINE>21
   LINE= LINE-1
ENDIF
LINE= LINE+1
@ LINE, 1 SAY REPLICATE('-',79)
LINE= LINE+1
@ LINE, 10 SAY '        T O T A L S :'
@ LINE, 51 SAY DRTOT PICTURE '9999999,999.99'
@ LINE, 65 SAY CRTOT PICTURE '9999999,999.99'
LINE= LINE+1
@ LINE, 1 SAY REPLICATE('-',79)
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
SET FILTER TO
CLOSE ALL
RUN del mldgr.dbf
RUN del mtran.dbf
RUN del mmast.dbf
RETURN

PROCEDURE PRV-TRL2
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE
P= ' '
DO WHILE .not. P$'PSQ'
   @ 3, 2 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
STORE DATE() TO MTDATE
CLEAR
@ 8, 15 SAY 'Enter the date of Previous Trail ' GET MTDATE
READ
IF P='P'
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ELSE
   SET DEVICE TO SCREEN
ENDIF
SELECT 2
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 1
USE master
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac2
SET FILTER TO DATE<=MTDATE
GOTO TOP
MCODE= 0
HEADING= .T.
PAGE= 1
STORE 0 TO WRECNO, MSRNO, WCODE, AMT1, AMT2
GOTO TOP
WCODE= 0
CLEAR
DO WHILE .not. EOF()
   IF DR_AMOUNT=0.and.CR_AMOUNT=0.and.BL_AMOUNT=0
      SKIP
      LOOP
   ENDIF
   IF HEADING
      IF P='P'
         DO top-prt1
      ELSE
         DO screen
      ENDIF
      @ 2, 20 SAY 'TRIAL  BALANCE  AS  ON: '
      @ 2, 44 SAY MTDATE
      @ 2, 72 SAY 'PAGE: '
      @ 2, 78 SAY PAGE PICTURE '99'
      @ 3, 1 SAY REPLICATE('-',79)
      @ 4, 1 SAY 'CODE-----'
      @ 4, 11 SAY '   ---- ACCOUNT  TITLE ----'
      @ 4, 57 SAY 'D E B I T'
      @ 4, 69 SAY 'C R E D I T'
      @ 5, 1 SAY REPLICATE('-',79)
      LINE= 6
      HEADING= .F.
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
   STORE STR(C1,2)+STR(C2,2)+STR(C3,3) TO MCODE
   STORE C1 TO MC1
   STORE C2 TO MC2
   STORE C3 TO MC3
   STORE 0 TO DRTOT, CRTOT, AMT
   DO WHILE C1=MC1.and.C2=MC2.and.C3=MC3
      IF SR_NO=1.and.STATUS='DR'
         DRTOT= DRTOT+BL_AMOUNT
         SKIP
         LOOP
      ENDIF
      IF SR_NO=1.and.STATUS='CR'
         CRTOT= CRTOT+BL_AMOUNT
         SKIP
         LOOP
      ENDIF
      IF CR_AMOUNT=0
         DRTOT= DRTOT+DR_AMOUNT
      ENDIF
      IF DR_AMOUNT=0
         CRTOT= CRTOT+CR_AMOUNT
      ENDIF
      SKIP
   ENDDO
   AMT= DRTOT-CRTOT
   SELECT 2
   SEEK MCODE
   IF FOUND()
      STORE SPACE(30) TO WTITLE
      STORE TITLE TO WTITLE
   ENDIF
   SELECT 1
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   @ LINE, 1 SAY MPC1
   @ LINE, 3 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ LINE, 4 SAY MPC2
   @ LINE, 6 SAY '-'
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ LINE, 7 SAY MPC3
   @ LINE, 14 SAY WTITLE
   IF DRTOT>CRTOT
      @ LINE, 53 SAY ABS(AMT)
      AMT1= AMT1+ABS(AMT)
   ENDIF
   IF CRTOT>DRTOT
      @ LINE, 67 SAY ABS(AMT)
      AMT2= AMT2+ABS(AMT)
   ENDIF
   LINE= LINE+1
   IF P='P'
      IF LINE>54
         PAGE= PAGE+1
         HEADING= .T.
         EJECT
      ENDIF
   ELSE
      IF LINE>20
         PAGE= PAGE+1
         HEADING= .T.
         ANS= ' '
         DO WHILE .not. ANS$'CQ'
            SET CONFIRM OFF
            @ 23, 50 SAY 'Continue/Quit  -  C/Q  ' GET ANS FUNCTION '!'
            READ
         ENDDO
         CLEAR
         IF ANS='Q'
            CLOSE ALL
            CLEAR
            RETURN
         ENDIF
      ENDIF
   ENDIF
ENDDO
LINE= LINE+1
@ LINE, 1 SAY REPLICATE('-',79)
LINE= LINE+1
@ LINE, 10 SAY '        T O T A L S :'
@ LINE, 51 SAY AMT1 PICTURE '9999999,999.99'
@ LINE, 66 SAY AMT2 PICTURE '9999999,999.99'
LINE= LINE+1
@ LINE, 1 SAY REPLICATE('-',79)
IF P='P'
   SET DEVICE TO SCREEN
   EJECT
ELSE
   @ 23, 1 SAY ''
   WAIT '       End Of Screen Display     Press Any Key To Go Back ... '
ENDIF
CLOSE ALL
RETURN

PROCEDURE DATE-TRL
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE
STORE 0 TO AMT1, AMT2
@ 5, 25 SAY 'Trial Balance Between Two Dates '
STORE DATE() TO DD1, DD2
@ 10, 25 SAY 'From Date ' GET DD1
@ 12, 25 SAY 'To   Date ' GET DD2
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
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ELSE
   SET DEVICE TO SCREEN
ENDIF
SELECT 2
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 1
USE master
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac2
COPY TO trl-work FOR DATE>=DD1.and.DATE<=DD2
MCODE= 0
HEADING= .T.
PAGE= 1
WRECNO= 0
MSRNO= 0
WCODE= 0
GOTO TOP
WCODE= 0
CLEAR
SELECT 1
USE trl-work
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO trl-idx
DO WHILE .not. EOF()
   IF DR_AMOUNT=0.and.CR_AMOUNT=0.and.BL_AMOUNT=0
      SKIP
      LOOP
   ENDIF
   IF HEADING
      IF P='P'
         DO top-prt1
      ELSE
         DO screen
      ENDIF
      @ 2, 20 SAY 'TRIAL  BALANCE  BETWEEN  TWO  DATES '
      @ 3, 20 SAY 'From :'
      @ 3, 28 SAY DD1
      @ 3, 40 SAY 'To :'
      @ 3, 45 SAY DD2
      @ 3, 70 SAY 'PAGE : '
      @ 3, 76 SAY PAGE PICTURE '99'
      @ 4, 1 SAY REPLICATE('-',79)
      @ 5, 1 SAY 'CODE-----'
      @ 5, 11 SAY '   ---- ACCOUNT  TITLE ----'
      @ 5, 57 SAY 'D E B I T'
      @ 5, 69 SAY 'C R E D I T'
      @ 6, 1 SAY REPLICATE('-',79)
      LINE= 7
      HEADING= .F.
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
   STORE STR(C1,2)+STR(C2,2)+STR(C3,3) TO MCODE
   STORE C1 TO MC1
   STORE C2 TO MC2
   STORE C3 TO MC3
   STORE 0 TO DRTOT, CRTOT
   DO WHILE C1=MC1.and.C2=MC2.and.C3=MC3
      IF CR_AMOUNT=0
         DRTOT= DRTOT+DR_AMOUNT
      ENDIF
      IF DR_AMOUNT=0
         CRTOT= CRTOT+CR_AMOUNT
      ENDIF
      SKIP
   ENDDO
   SELECT 2
   SEEK MCODE
   IF FOUND()
      STORE TITLE TO WTITLE
   ENDIF
   SELECT 1
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   @ LINE, 1 SAY MPC1
   @ LINE, 3 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ LINE, 4 SAY MPC2
   @ LINE, 6 SAY '-'
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ LINE, 7 SAY MPC3
   @ LINE, 14 SAY WTITLE
   AMT= 0
   AMT= DRTOT-CRTOT
   IF DRTOT>CRTOT
      @ LINE, 54 SAY ABS(AMT) PICTURE '9,999,999,999.99'
      AMT1= AMT1+ABS(AMT)
   ENDIF
   IF CRTOT>DRTOT
      @ LINE, 62 SAY ABS(AMT) PICTURE '9,999,999,999.99'
      AMT2= AMT2+ABS(AMT)
   ENDIF
   LINE= LINE+1
   IF P='P'
      IF LINE>54
         PAGE= PAGE+1
         HEADING= .T.
         EJECT
      ENDIF
   ELSE
      IF LINE>20
         PAGE= PAGE+1
         HEADING= .T.
         ANS= ' '
         DO WHILE .not. ANS$'CQ'
            SET CONFIRM OFF
            @ 23, 50 SAY 'Continue/Quit  -  C/Q  ' GET ANS FUNCTION '!'
            READ
         ENDDO
         CLEAR
         IF ANS='Q'
            CLOSE ALL
            CLEAR
            RETURN
         ENDIF
      ENDIF
   ENDIF
ENDDO
LINE= LINE+1
@ LINE, 1 SAY REPLICATE('-',79)
LINE= LINE+1
@ LINE, 10 SAY '        T O T A L S :'
@ LINE, 53 SAY AMT1 PICTURE '9999999999.99'
@ LINE, 67 SAY AMT2 PICTURE '9999999999.99'
LINE= LINE+1
@ LINE, 1 SAY REPLICATE('-',79)
IF P='P'
   SET PRINTER OFF
   SET DEVICE TO SCREEN
   EJECT
ELSE
   @ 23, 1 SAY ''
   WAIT '       End Of Screen Display     Press Any Key To Go Back ... '
ENDIF
CLOSE ALL
RUN del trl-work.dbf
RETURN

PROCEDURE UTRL-MEN
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 25 SAY 'User Definable Trial Balance Reports'
   SET COLOR TO 7/1
   @ 8, 25 PROMPT ' 1 - Add Ledgerheads To This Report  '
   @ 10, 25 PROMPT ' 2 - Delete Ledgerheads              '
   @ 12, 25 PROMPT ' 3 - Uptodate Trial Balance Report   '
   @ 14, 25 PROMPT ' 9 - Return to Previous Menu         '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=4
         CLEAR
         CLOSE DATABASE
         RETURN
      CASE MCHOICE=3
         CLEAR
         DO utrl-prt
      CASE MCHOICE=2
         CLEAR
         DO utrl-del
      CASE MCHOICE=1
         CLEAR
         DO utrl-ent
   ENDCASE
ENDDO
RETURN

PROCEDURE UTRL-ENT
CLEAR
CLOSE ALL
SELECT 1
USE ldgrcode INDEX ac1
SELECT 2
USE trl-ref INDEX trl-idx
DO WHILE .T.
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ 2, 10 SAY 'Adding Ledgerheads To This Report  '
   STORE SPACE(30) TO MTITLE
   STORE 0 TO MC1, MC2, MC3
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ 10, 10 SAY 'T i t l e '
   @ 12, 10 GET MTITLE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ 14, 10 SAY 'Ledger Heads Codes '
      SET COLOR TO /w
      @ 16, 10 SAY '  -  -   '
      SET COLOR TO 
      @ 16, 10 GET MC1 PICTURE '99'
      @ 16, 13 GET MC2 PICTURE '99'
      @ 16, 16 GET MC3 PICTURE '999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      @ 23, 45 CLEAR TO 23, 78
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      SET COLOR TO /w
      @ 16, 10 SAY MPC1 PICTURE '99'
      @ 16, 13 SAY MPC2 PICTURE '99'
      @ 16, 16 SAY MPC3 PICTURE '999'
      SET COLOR TO 
      SELECT 1
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF .not. FOUND()
         @ 23, 15 SAY 'This Code Does Not Exists .. Please Re-try '
         WAIT ''
         @ 22, 10 CLEAR TO 23, 79
         LOOP
      ENDIF
      SELECT 2
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF FOUND()
         @ 23, 10 SAY 'This Code Already Exists In Reference File .. Please Re-try '
         WAIT ''
         @ 22, 9 CLEAR TO 23, 79
         LOOP
      ENDIF
      SELECT 1
      SET COLOR TO /w
      @ 16, 20 SAY TITLE
      SET COLOR TO 
      ANS= ' '
      DO WHILE .not. ANS$'YN'
         @ 23, 25 SAY 'Any Correction - Y/N ' GET ANS FUNCTION '!'
         READ
      ENDDO
      @ 23, 1 CLEAR TO 23, 79
      IF ANS='Y'
         LOOP
      ENDIF
      SELECT 2
      APPEND BLANK
      REPLACE C1 WITH MC1
      REPLACE C2 WITH MC2
      REPLACE C3 WITH MC3, TITLE WITH MTITLE
      ANS= ' '
      DO WHILE .not. ANS$'YN'
         @ 23, 22 SAY 'More Ledgerheads For This Title - Y/N ' GET ANS FUNCTION '!'
         READ
      ENDDO
      @ 23, 1 CLEAR TO 23, 79
      IF ANS='N'
         EXIT
      ENDIF
      @ 14, 1 CLEAR TO 24, 79
      STORE 0 TO MC1, MC2, MC3
   ENDDO
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 23, 25 SAY 'More Ledgerheads - Y/N ' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 23, 1 CLEAR TO 23, 79
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
ENDDO
RETURN

PROCEDURE UTRL-DEL
CLEAR
CLOSE ALL
SET DELETED ON
SELECT 1
USE ldgrcode INDEX ac1
SELECT 2
USE trl-ref INDEX trl-idx
DO WHILE .T.
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ 2, 10 SAY 'Delete Ledgerheads To This Report  '
   STORE SPACE(30) TO MTITLE
   STORE 0 TO MC1, MC2, MC3
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ 10, 10 SAY 'T i t l e '
   @ 12, 10 GET MTITLE
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ 14, 10 SAY 'Ledger Heads Codes '
   SET COLOR TO /w
   @ 16, 10 SAY '  -  -   '
   SET COLOR TO 
   @ 16, 10 GET MC1 PICTURE '99'
   @ 16, 13 GET MC2 PICTURE '99'
   @ 16, 16 GET MC3 PICTURE '999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   SET COLOR TO /w
   @ 16, 10 SAY MPC1 PICTURE '99'
   @ 16, 13 SAY MPC2 PICTURE '99'
   @ 16, 16 SAY MPC3 PICTURE '999'
   SET COLOR TO 
   SELECT 1
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 23, 15 SAY 'This Code Does Not Exists .. Please Re-try '
      WAIT ''
      @ 22, 10 CLEAR TO 23, 79
      LOOP
   ENDIF
   SELECT 2
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF .not. FOUND()
      @ 23, 10 SAY 'This Code Does Not Exists In Reference File .. Please Re-try '
      WAIT ''
      @ 22, 9 CLEAR TO 23, 79
      LOOP
   ENDIF
   SELECT 1
   SET COLOR TO /w
   @ 16, 20 SAY TITLE
   SET COLOR TO 
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 23, 25 SAY 'Are You Sure - Y/N ' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 23, 1 CLEAR TO 23, 79
   IF ANS='Y'
      SELECT 2
      DELETE
   ENDIF
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 23, 25 SAY 'More Data Deletion - Y/N ' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 23, 1 CLEAR TO 23, 79
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
ENDDO
RETURN

PROCEDURE UTRL-PRT
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
USE master
INDEX ON DATE TO mastdate
GOTO BOTTOM
STORE DATE TO MTDATE
CLEAR
@ 3, 5 SAY 'Your last posted voucher is dated '
@ 3, 42 SAY MTDATE
@ 4, 5 SAY 'Do you want to dispaly same date on your Trial Balance'
@ 5, 5 SAY 'Or enter the date you want to dispaly  ' GET MTDATE
READ
CLEAR
CLOSE ALL
SELECT 1
USE ldgrcode INDEX ac1
SELECT 2
USE trl-ref INDEX trl-idx
ANS= ' '
DO WHILE .not. ANS$'PSQ'
   @ 4, 2 SAY 'Printer/Screen/Quit -  P/S/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE
STORE 0 TO MCODE, WRECNO, MSRNO, WCODE, MNO
IF ANS='P'
   CLEAR
   @ 8, 17 SAY 'Subsideries Trial Balance Printing'
   @ 10, 17 SAY 'Please use 80 column (10 inches wide) continous'
   @ 11, 17 SAY 'printer paper and set Top Of Form'
   @ 12, 17 SAY 'Press ENTER when ready...........'
   WAIT ''
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
PAGE= 1
HEADING= .T.
WCODE= 0
CLEAR
SELECT 2
DO WHILE .not. EOF()
   IF HEADING
      IF ANS='S'
         DO screen
         @ 2, 20 SAY 'TRIAL  BALANCE  AS  ON: '
      ENDIF
      IF ANS='P'
         DO top-prt1
         @ 2, 23 SAY 'TRIAL BALANCE AS ON: '
      ENDIF
      @ 2, 44 SAY MTDATE
      @ 2, 72 SAY 'PAGE: '
      @ 2, 78 SAY PAGE PICTURE '99'
      @ 3, 1 SAY REPLICATE('-',79)
      @ 4, 1 SAY '   ---- ACCOUNT  TITLE ----'
      @ 4, 55 SAY 'D E B I T'
      @ 4, 69 SAY 'C R E D I T'
      @ 5, 1 SAY REPLICATE('-',79)
      LINE= 6
      HEADING= .F.
   ENDIF
   STORE 0 TO MDR, MCR
   MTITLE= TITLE
   DO WHILE TITLE=MTITLE
      IF TITLE<>MTITLE
         EXIT
      ENDIF
      MC1= C1
      MC2= C2
      MC3= C3
      SELECT 1
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF FOUND()
         IF STATUS='DR'
            MDR= MDR+LDGR_BAL
            DRTOT= DRTOT+LDGR_BAL
         ELSE
            MCR= MCR+LDGR_BAL
            CRTOT= CRTOT+LDGR_BAL
         ENDIF
      ENDIF
      SELECT 2
      SKIP
   ENDDO
   @ LINE, 5 SAY MTITLE
   IF MDR<>0
      @ LINE, 48 SAY MDR PICTURE '9,999,999,999.99'
   ENDIF
   IF MCR<>0
      @ LINE, 63 SAY MCR PICTURE '9,999,999,999.99'
   ENDIF
   LINE= LINE+1
   IF ANS='S'.and.LINE>21
      XYZ= ' '
      DO WHILE .not. XYZ$'CQ'
         @ 23, 57 SAY 'Continue/Quit - C/Q' GET XYZ FUNCTION '!'
         READ
      ENDDO
      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      CLEAR
      PAGE= PAGE+1
      HEADING= .T.
   ENDIF
   IF ANS='P'.and.LINE>=56
      PAGE= PAGE+1
      HEADING= .T.
   ENDIF
ENDDO
@ LINE, 1 SAY REPLICATE('-',79)
LINE= LINE+1
@ LINE, 10 SAY '        T O T A L S :'
@ LINE, 50 SAY DRTOT PICTURE '9999999,999.99'
@ LINE, 66 SAY CRTOT PICTURE '9999999,999.99'
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
SET FILTER TO
CLOSE ALL
RETURN

PROCEDURE PL-MEN
CLEAR
CLOSE ALL
STORE DATE() TO DATE
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 27 SAY ' PROFIT & LOSS REPORTS'
   @ 8, 21 SAY '---------  P & L  (Uptodate) -------- '
   @ 13, 21 SAY '-----  P & L (Between Two Dates) ---- '
   SET COLOR TO 7/1
   @ 10, 21 PROMPT '1 - Profit & Loss Statement (Level 3) '
   @ 11, 21 PROMPT '2 - Profit & Loss Statement (Level 2) '
   @ 15, 21 PROMPT '3 - Profit & Loss Statement (Level 3) '
   @ 17, 21 PROMPT '4 - Return to Previous Menu '
   CHOICE= 0
   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         DO c3-pl
      CASE CHOICE=2
         DO c2-pl
      CASE CHOICE=3
         DO rep-pl2
      CASE CHOICE=4
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE C3-PL
CLEAR
CLOSE ALL
SET COLOR TO 7/1
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
USE master
INDEX ON DATE TO mastdate
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
   DO prnchk
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
INDEX ON CHART_NO TO cht-idx
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
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 1
USE pl-work2
INDEX ON STR(CHART_NO,2)+STR(C1,2)+STR(C2,2)+STR(C3,3) TO pl-work2
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
         DO top-prt1
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
            SET COLOR TO w+
            SET CONFIRM OFF
            @ 24, 20 SAY 'Please Enter <C> To Continue <Q> To Quit' GET ANS PICTURE '!'
            READ
            SET COLOR TO 
         ENDDO
         IF ANS='C'
            CLEAR
            PAGE= PAGE+1
            HEADING= .T.
            LINE= 6
         ELSE
            CLEAR ALL
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
      DO top-prt1
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
CLEAR ALL
CLEAR
RETURN

PROCEDURE C2-PL
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR PROGRAM
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE, TOTCR7, TOTDR7, TOTCR8, TOTDR8, TOT7, TOT8, SWCH
CLEAR
SELECT 1
USE master
INDEX ON DATE TO mastdate
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
   DO prnchk
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
INDEX ON CHART_NO TO cht-idx
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
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 1
USE pl-work2
INDEX ON STR(CHART_NO,2)+STR(C1,2)+STR(C2,2)+STR(C3,3) TO pl-work2
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
            DO top-prt1
         ELSE
            DO screen
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
            SET COLOR TO w+
            SET CONFIRM OFF
            @ 24, 20 SAY 'Please Enter <C> To Continue <Q> To Quit' GET ANS PICTURE '!'
            READ
            SET COLOR TO 
         ENDDO
         IF ANS='C'
            CLEAR
            PAGE= PAGE+1
            HEADING= .T.
            LINE= 6
         ELSE
            CLEAR ALL
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
         SET COLOR TO w+
         SET CONFIRM OFF
         @ 24, 20 SAY 'Please Enter <C> To Continue <Q> To Quit' GET ANS PICTURE '!'
         READ
         SET COLOR TO 
      ENDDO
      IF ANS='C'
         CLEAR
         PAGE= PAGE+1
         HEADING= .T.
         LINE= 6
      ELSE
         CLEAR ALL
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
CLEAR ALL
RUN del abc.dbf
CLEAR
RETURN

PROCEDURE REP-PL2
CLEAR
CLOSE ALL
SET COLOR TO 7/1
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
      CLEAR ALL
      CLEAR
      RETURN
   ENDIF
   LOCATE FOR DATE>=DD1.and.DATE<=DD2
   IF .not. FOUND()
      SET COLOR TO w+
      @ 16, 20 SAY 'No Transactions Exist Between These Dates'
      SET COLOR TO 
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
   DO prnchk
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
INDEX ON CHART_NO TO cht-idx
GOTO TOP
M1= FR
M2= TO
SELECT 3
USE ldgrcode INDEX ac1
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
INDEX ON STR(CHART_NO,2)+STR(C1,2)+STR(C2,2)+STR(C3,3) TO acwork
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
            DO top-prt1
         ELSE
            DO screen
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
            SET COLOR TO w+
            SET CONFIRM OFF
            @ 24, 20 SAY 'Please Enter <C> To Continue <Q> To Quit' GET ANS PICTURE '!'
            READ
            SET COLOR TO 
         ENDDO
         IF ANS='C'
            LINE= 6
            PAGE= PAGE+1
            HEADING= .T.
         ELSE
            CLEAR ALL
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
         DO top-prt1
      ELSE
         DO screen
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
CLEAR ALL
CLEAR
RETURN

PROCEDURE BS-C1
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
SELECT 1
USE master
INDEX ON DATE TO mastdate
GOTO BOTTOM
STORE DATE TO MTDATE
CLEAR
@ 2, 5 SAY 'Balance Sheet Report '
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
   CLEAR
   @ 8, 17 SAY 'Balance Sheet Report '
   @ 10, 17 SAY 'Please use 80 column (10 inches wide) continous'
   @ 11, 17 SAY 'printer paper and set Top Of Form'
   @ 12, 17 SAY 'Press ENTER when ready...........'
   WAIT ''
   @ 14, 17 SAY 'Printing In Progress'
ENDIF
IF P='P'
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ELSE
   CLEAR
   @ 10, 17 SAY 'Please Wait Balance Sheet Calculations In Progress'
   SET DEVICE TO SCREEN
ENDIF
STORE SPACE(30) TO WTITLE, WHTITLE
STORE 0 TO AMT6, AMT7, AMT8, AMT9, GPFT, GPFT2
SELECT 2
USE chart
INDEX ON CHART_NO TO cht-idx
GOTO TOP
MF1= FR
MT1= TO
SELECT 1
USE pl-work2
ZAP
APPEND FROM ldgrcode FOR C1>=MF1.and.C1<=MT1
REPLACE FOR CHART_NO=0 CHART_NO WITH 1
SELECT 2
DO WHILE .not. EOF()
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF2= FR
   MT2= TO
   SELECT 1
   IF MF2<>0.and.MT2<>0
      APPEND FROM ldgrcode FOR C1>=MF2.and.C1<=MT2
      REPLACE FOR CHART_NO=0 CHART_NO WITH 2
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF3= FR
   MT3= TO
   SELECT 1
   IF MF3<>0.and.MT3<>0
      APPEND FROM ldgrcode FOR C1>=MF3.and.C1<=MT3
      REPLACE FOR CHART_NO=0 CHART_NO WITH 3
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF4= FR
   MT4= TO
   SELECT 1
   IF MF4<>0.and.MT4<>0
      APPEND FROM ldgrcode FOR C1>=MF4.and.C1<=MT4
      REPLACE FOR CHART_NO=0 CHART_NO WITH 4
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF5= FR
   MT5= TO
   SELECT 1
   IF MF5<>0.and.MT5<>0
      APPEND FROM ldgrcode FOR C1>=MF5.and.C1<=MT5
      REPLACE FOR CHART_NO=0 CHART_NO WITH 5
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF6= FR
   MT6= TO
   SELECT 1
   IF MF6<>0.and.MT6<>0
      APPEND FROM ldgrcode FOR C1>=MF6.and.C1<=MT6
      REPLACE FOR CHART_NO=0 CHART_NO WITH 6
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF7= FR
   MT7= TO
   SELECT 1
   IF MF7<>0.and.MT7<>0
      APPEND FROM ldgrcode FOR C1>=MF7.and.C1<=MT7
      REPLACE FOR CHART_NO=0 CHART_NO WITH 7
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF8= FR
   MT8= TO
   SELECT 1
   IF MF8<>0.and.MT8<>0
      APPEND FROM ldgrcode FOR C1>=MF8.and.C1<=MT8
      REPLACE FOR CHART_NO=0 CHART_NO WITH 8
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF9= FR
   MT9= TO
   SELECT 1
   IF MF9<>0.and.MT9<>0
      APPEND FROM ldgrcode FOR C1>=MF9.and.C1<=MT9
      REPLACE FOR CHART_NO=0 CHART_NO WITH 9
   ENDIF
ENDDO
SELECT 1
USE pl-work2
INDEX ON CHART_NO TO bl-c1
GOTO TOP
MCODE= 0
HEADING= .T.
PAGE= 1
STORE 0 TO WRECNO, MSRNO, WCODE
WGTITLE= SPACE(30)
STORE 0 TO DRTOT, CRTOT, GTDR, GTCR, LINE, TOTDR, TOTCR
STORE 0 TO TCR1, TDR1, TCR2, TDR2, TCR3, TDR3
STORE 0 TO TCR4, TDR4, TCR5, TDR5, TCR6, TDR6
STORE 0 TO TCR7, TDR7, TCR8, TDR8, TCR9, TDR9
MC1= C1
MC2= C2
MC3= C3
CLEAR
DO WHILE .not. EOF()
   IF EOF()
      EXIT
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
   IF CHART_NO=1
      IF STATUS='DR'
         TDR1= TDR1+LDGR_BAL
      ELSE
         TCR1= TCR1+LDGR_BAL
      ENDIF
   ENDIF
   IF CHART_NO=2
      IF STATUS='DR'
         TDR2= TDR2+LDGR_BAL
      ELSE
         TCR2= TCR2+LDGR_BAL
      ENDIF
   ENDIF
   IF CHART_NO=3
      IF STATUS='DR'
         TDR3= TDR3+LDGR_BAL
      ELSE
         TCR3= TCR3+LDGR_BAL
      ENDIF
   ENDIF
   IF CHART_NO=4
      IF STATUS='DR'
         TDR4= TDR4+LDGR_BAL
      ELSE
         TCR4= TCR4+LDGR_BAL
      ENDIF
   ENDIF
   IF CHART_NO=5
      IF STATUS='DR'
         TDR5= TDR5+LDGR_BAL
      ELSE
         TCR5= TCR5+LDGR_BAL
      ENDIF
   ENDIF
   IF CHART_NO=6
      IF STATUS='DR'
         TDR6= TDR6+LDGR_BAL
      ELSE
         TCR6= TCR6+LDGR_BAL
      ENDIF
   ENDIF
   IF CHART_NO>6
      IF STATUS='DR'
         TDR7= TDR7+LDGR_BAL
      ELSE
         TCR7= TCR7+LDGR_BAL
      ENDIF
   ENDIF
   SKIP
ENDDO
AMT1= TCR1-TDR1
AMT2= TCR2-TDR2
AMT3= TCR3-TDR3
AMT4= TDR4-TCR4
AMT5= TDR5-TCR5
AMT6= TCR6-TDR6
AMT7= TDR7-TCR7
PLS= AMT6-AMT7
HEADING= .T.
SELECT 1
IF HEADING
   IF P='S'
      DO screen
   ELSE
      DO top-prt1
   ENDIF
   @ 2, 10 SAY 'B A L A N C E    S H E E T     A S   O N :'
   @ 2, 55 SAY MTDATE
   @ 3, 1 SAY REPLICATE('-',79)
   @ 4, 3 SAY '     L I A B I L I T I E S'
   @ 4, 55 SAY 'A S S E T S'
   @ 5, 1 SAY REPLICATE('-',79)
   @ 5, 40 SAY '|'
   L= 6
   @ 6, 40 SAY '|'
   HEADING= .F.
ENDIF
@ L, 1 SAY '  Capital / Equity '
@ L, 20 SAY AMT1 PICTURE '@( 999,999,999,999.99'
@ L, 40 SAY '|'
@ L, 42 SAY ' Long Term Assets     '
@ L, 61 SAY AMT4 PICTURE '@( 999,999,999,999.99'
L= L+1
@ L, 1 SAY '  Long Term Liabs  '
@ L, 20 SAY AMT2 PICTURE '@( 999,999,999,999.99'
@ L, 40 SAY '|'
@ L, 42 SAY ' Current Assets       '
@ L, 61 SAY AMT5 PICTURE '@( 999,999,999,999.99'
L= L+1
@ L, 1 SAY '  Short Term Liabs    '
@ L, 20 SAY AMT3 PICTURE '@( 999,999,999,999.99'
@ L, 40 SAY '|'
L= L+1
@ L, 40 SAY '|'
L= L+1
@ L, 40 SAY '|'
L= L+1
@ L, 40 SAY '|'
L= L+1
@ L, 1 SAY '  Profit/(Loss)  '
@ L, 20 SAY PLS PICTURE '@( 999,999,999,999.99'
@ L, 40 SAY '|'
L= L+1
@ L, 40 SAY '|'
L= L+1
@ L, 1 SAY REPLICATE('-',79)
@ L, 40 SAY '|'
L= L+1
LTOT= AMT1+AMT2+AMT3+PLS
@ L, 20 SAY LTOT PICTURE '@( 999,999,999,999.99'
@ L, 40 SAY '|'
RTOT= AMT4+AMT5
@ L, 61 SAY RTOT PICTURE '@( 999,999,999,999.99'
L= L+1
@ L, 1 SAY REPLICATE('-',79)
IF P='P'
   EJECT
ELSE
   @ 23, 1 SAY ''
   WAIT SPACE(15)+'Press Any Key To Continue For Balance Sheet Details '
ENDIF
DO bs-c2

PROCEDURE BS-C2
CLEAR
IF P='S'
   @ 10, 20 SAY 'Please Wait - For Balance Sheet Level 2'
ENDIF
SELECT 2
USE chart
INDEX ON CHART_NO TO cht-idx
GOTO TOP
SELECT 3
USE ldgrcode INDEX ac1
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 1
USE pl-work2
INDEX ON STR(CHART_NO,2)+STR(C1,2)+STR(C2,2)+STR(C3,3) TO bl-c1
GOTO TOP
STORE 0 TO LTAMT, DTAMT, MCODE, PCON
HEADING= .T.
PAGE= 1
STORE 0 TO WRECNO, MSRNO, WCODE
WGTITLE= SPACE(30)
STORE 0 TO DRTOT, CRTOT, GTDR, GTCR, LINE, TOTDR, TOTCR, NTOT
STORE 0 TO TCR1, TDR1, TCR2, TDR2, TCR3, TDR3
STORE 0 TO TCR4, TDR4, TCR5, TDR5
MC3= 0
R= 5
CLEAR
GOTO TOP
CLEAR
DO WHILE .not. EOF()
   IF EOF()
      EXIT
   ENDIF
   IF CHART_NO>5
      IF .not. EOF()
         SKIP
      ENDIF
      LOOP
   ENDIF
   TAMT= 0
   IF CHART_NO<=3
      STORE CHART_NO TO MCHART
      DO WHILE CHART_NO=MCHART
         STORE 0 TO TDR, TCR, AMT, MC3
         STORE C1 TO MC1
         STORE C2 TO MC2
         DO WHILE C1=MC1.and.C2=MC2
            IF STATUS='DR'
               TDR= TDR+LDGR_BAL
            ELSE
               TCR= TCR+LDGR_BAL
            ENDIF
            IF .not. EOF()
               SKIP
            ENDIF
         ENDDO
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SELECT 3
         SEEK MCODE
         IF FOUND()
            STORE TITLE TO MTITLE
         ELSE
            STORE 'Group Total ' TO MTITLE
         ENDIF
         SELECT 1
         IF MCHART<=3
            AMT= TCR-TDR
         ELSE
            AMT= TDR-TCR
         ENDIF
         IF HEADING
            IF P='S'
               DO screen
            ELSE
               DO top-prt1
            ENDIF
            @ 2, 10 SAY 'B A L A N C E   S H E E T - 2   A S   O N :'
            @ 2, 54 SAY MTDATE
            @ 3, 1 SAY REPLICATE('-',79)
            @ 4, 1 SAY '(1).   L I A B I L I T I E S'
            R= 6
            HEADING= .F.
         ENDIF
         IF MC2<>0.and.AMT=0
            IF .not. EOF()
               SKIP
            ENDIF
            LOOP
         ENDIF
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ R, 1 SAY MPC1 PICTURE '99'
         @ R, 3 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ R, 4 SAY MPC2 PICTURE '99'
         IF MC2=0
            @ R, 12 SAY MTITLE
         ELSE
            @ R, 15 SAY MTITLE
            @ R, 60 SAY AMT PICTURE '@( 9,999,999,999.99'
            TAMT= AMT+TAMT
         ENDIF
         R= R+1
         IF P='S'.and.R>=16
            ANS= ' '
            DO WHILE .not. ANS$'CQ'
               SET COLOR TO +7/1
               SET CONFIRM OFF
               @ 24, 20 SAY 'Please Enter <C> To Continue <Q> To Quit' GET ANS PICTURE '!'
               READ
               SET COLOR TO 7/1
            ENDDO
            IF ANS='C'
               CLEAR
               R= 6
               PAGE= PAGE+1
               HEADING= .T.
            ELSE
               CLEAR ALL
               CLEAR
               RETURN
            ENDIF
         ENDIF
         IF P='P'.and.R>=56
            PAGE= PAGE+1
            HEADING= .T.
            EJECT
         ENDIF
      ENDDO
      IF P='S'.and.R>=13
         ANS= ' '
         DO WHILE .not. ANS$'CQ'
            SET COLOR TO +7/1
            SET CONFIRM OFF
            @ 24, 20 SAY 'Please Enter <C> To Continue <Q> To Quit' GET ANS PICTURE '!'
            READ
            SET COLOR TO 7/1
         ENDDO
         IF ANS='C'
            CLEAR
            R= 6
            PAGE= PAGE+1
            HEADING= .T.
         ELSE
            CLEAR ALL
            CLEAR
            RETURN
         ENDIF
      ENDIF
      SELECT 2
      SEEK MCHART
      R= R+1
      IF FOUND()
      ENDIF
      SELECT 1
      @ R, 60 SAY '-----------------'
      R= R+1
      @ R, 40 SAY 'Total '
      @ R, 60 SAY TAMT PICTURE '@( 9,999,999,999.99'
      R= R+1
      @ R, 60 SAY '-----------------'
      R= R+1
      LTAMT= LTAMT+TAMT
   ELSE
      IF PCON=0
         R= R+1
         @ R, 5 SAY 'Profit / (Loss)'
         @ R, 60 SAY PLS PICTURE '@( 9,999,999,999.99'
         R= R+2
         LTAMT= LTAMT+PLS
         R= R+2
         @ R, 60 SAY '-----------------'
         R= R+1
         @ R, 40 SAY 'Total'
         @ R, 60 SAY LTAMT PICTURE '@( 9,999,999,999.99'
         R= R+1
         @ R, 60 SAY '================='
         PCON= 2
         HEADING= .T.
         IF P='S'
            ANS= ' '
            DO WHILE .not. ANS$'CQ'
               SET COLOR TO +7/1
               SET CONFIRM OFF
               @ 24, 20 SAY 'Please Enter <C> To Continue <Q> To Quit' GET ANS PICTURE '!'
               READ
               SET COLOR TO 7/1
            ENDDO
            IF ANS='C'
               CLEAR
               R= 6
               PAGE= PAGE+1
               HEADING= .T.
            ELSE
               CLEAR ALL
               CLEAR
               RETURN
            ENDIF
         ENDIF
         IF P='P'
            PAGE= PAGE+1
            HEADING= .T.
            EJECT
         ENDIF
         endi &p4 
         STORE CHART_NO TO MCHART
         DO WHILE CHART_NO=MCHART
            STORE 0 TO TDR, TCR, AMT, MC3
            STORE C1 TO MC1
            STORE C2 TO MC2
            DO WHILE C1=MC1.and.C2=MC2
               IF STATUS='DR'
                  TDR= TDR+LDGR_BAL
               ELSE
                  TCR= TCR+LDGR_BAL
               ENDIF
               IF .not. EOF()
                  SKIP
               ENDIF
            ENDDO
            STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
            SELECT 3
            SEEK MCODE
            IF FOUND()
               STORE TITLE TO MTITLE
            ELSE
               STORE 'Group Total ' TO MTITLE
            ENDIF
            SELECT 1
            IF MCHART<=3
               AMT= TCR-TDR
            ELSE
               AMT= TDR-TCR
            ENDIF
            IF HEADING
               IF P='S'
                  DO screen
               ELSE
                  DO top-prt1
               ENDIF
               @ 2, 10 SAY 'B A L A N C E   S H E E T - 2   A S   O N :'
               @ 2, 54 SAY MTDATE
               @ 3, 1 SAY REPLICATE('-',79)
               @ 5, 1 SAY '(2).  A S S E T S :-'
               R= 7
               HEADING= .F.
            ENDIF
            IF MC2<>0.and.AMT=0
               IF .not. EOF()
                  SKIP
               ENDIF
               LOOP
            ENDIF
            MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
            @ R, 1 SAY MPC1 PICTURE '99'
            @ R, 3 SAY '-'
            MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
            @ R, 4 SAY MPC2 PICTURE '99'
            IF MC2=0
               @ R, 12 SAY MTITLE
            ELSE
               @ R, 15 SAY MTITLE
               @ R, 60 SAY AMT PICTURE '@( 9,999,999,999.99'
               TAMT= AMT+TAMT
            ENDIF
            R= R+1
            IF P='S'.and.R>=16
               ANS= ' '
               DO WHILE .not. ANS$'CQ'
                  SET COLOR TO +7/1
                  SET CONFIRM OFF
                  @ 24, 20 SAY 'Please Enter <C> To Continue <Q> To Quit' GET ANS PICTURE '!'
                  READ
                  SET COLOR TO 7/1
               ENDDO
               IF ANS='C'
                  CLEAR
                  PAGE= PAGE+1
                  HEADING= .T.
               ELSE
                  CLEAR ALL
                  CLEAR
                  RETURN
               ENDIF
            ENDIF
            IF P='P'.and.R>=56
               PAGE= PAGE+1
               HEADING= .T.
               EJECT
            ENDIF
         ENDDO
         SELECT 2
         SEEK MCHART
         R= R+1
         IF FOUND()
         ENDIF
         SELECT 1
         @ R, 60 SAY '-----------------'
         R= R+1
         @ R, 40 SAY 'Total '
         @ R, 60 SAY TAMT PICTURE '@( 9,999,999,999.99'
         R= R+1
         @ R, 60 SAY '-----------------'
         R= R+1
         DTAMT= DTAMT+TAMT
      ENDIF
   ENDDO
   R= R+2
   @ R, 60 SAY '-----------------'
   R= R+1
   @ R, 40 SAY 'Total'
   @ R, 60 SAY DTAMT PICTURE '@( 9,999,999,999.99'
   R= R+1
   @ R, 60 SAY '=================='
   IF P='P'
      EJECT
      SET DEVICE TO SCREEN
      SET PRINTER OFF
   ELSE
      @ 23, 1 SAY ''
      WAIT SPACE(25)+'Press Any Key To Return '
   ENDIF
   CLOSE ALL
   USE pl-work2
   ZAP
   CLOSE ALL
   RETURN

PROCEDURE BUD-COMP
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
SELECT 2
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 1
USE master
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac2
R2= 'r=r+1'
CL= 'set colo to n/w'
CL2= 'set colo to 7/1'
SET DATE brit
STORE 0 TO MC1, MC2, MC3
STORE 0 TO NC1, NC2, NC3
STORE DATE() TO FDATE, SDATE
HEAD= .T.
DO WHILE .T.
   CLEAR
   R= 4
   @ 2, 20 SAY 'Budget Variance Report'
   @ R, 20 SAY 'Budget From Head Code '
   &r2
   &r2
   @ R, 20 GET MC1 PICTURE '99' RANGE 60, 89
   @ R, 40 SAY 'Range For level From 60 to 89 '
   &cl
   @ R, 22 SAY '-'
   @ R, 23 SAY 'xx' PICTURE '99'
   &cl
   @ R, 25 SAY '-'
   @ R, 26 SAY 'xxx' PICTURE '999'
   &cl2
   READ
   IF READKEY()=12
      CLEAR ALL
      CLEAR
      RETURN
   ENDIF
   &r2
   &r2
   @ R, 20 SAY ' " " "  To Head Code No'
   &r2
   &r2
   @ R, 20 GET NC1 PICTURE '99' RANGE 60, 89
   &cl
   @ R, 22 SAY '-'
   @ R, 23 SAY 'xx' PICTURE '99'
   &cl
   @ R, 25 SAY '-'
   @ R, 26 SAY 'xxx' PICTURE '999'
   &cl2
   READ
   IF READKEY()=12
      CLEAR ALL
      CLEAR
      RETURN
   ENDIF
   &r2
   &r2
   @ R, 20 SAY 'From   Date ' GET FDATE
   &r2
   &r2
   @ R, 20 SAY 'To     Date ' GET SDATE
   READ
   IF READKEY()=12
      EXIT
   ENDIF
   IF FDATE>SDATE
      @ 14, 15 SAY '" From Date " Cannot Be Greater Than " To Date " - Retry'
      WAIT ''
      LOOP
   ENDIF
   EXIT
ENDDO
CLEAR
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 1
COPY TO mfil FOR C1>=MC1.and.C1<=NC1.and.DATE>=FDATE.and.DATE<=SDATE.and.C3<>0
USE mfil
IF RECCOUNT()=0
   CLEAR
   @ 12, 8 SAY 'No Transaction Exist in Data File  -  Press any key to return to menu '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO mfil
TDAY= SDATE-FDATE+1
CLEAR
DO WHILE .not. EOF()
   IF EOF()
      CLEAR ALL
      CLEAR
      RETURN
   ENDIF
   STORE STR(C1,2)+STR(C2,2)+STR(C3,3) TO MCODE
   STORE 0 TO TAMT, TCRAMT, TDRAMT
   STORE C1 TO MC1
   STORE C2 TO MC2
   STORE C3 TO MC3
   SELECT 2
   SEEK MCODE
   IF FOUND()
      IF BUDGET=0
         SKIP
         LOOP
      ENDIF
      STORE BUDGET TO MTBUJET
      STORE BUDGET/365 TO MBUJET
      STORE TITLE TO MTITLE
   ELSE
      STORE 'Invalid Code' TO MTITLE
   ENDIF
   SELECT 1
   DO WHILE STR(C1,2)+STR(C2,2)+STR(C3,3)=MCODE
      TDRAMT= DR_AMOUNT+TDRAMT
      TCRAMT= CR_AMOUNT+TCRAMT
      IF EOF()
         EXIT
      ENDIF
      SKIP
   ENDDO
   IF MC1>60.and.MC1<70
      TAMT= TCRAMT-TDRAMT+TAMT
   ENDIF
   IF MC1>70
      TAMT= TDRAMT-TCRAMT+TAMT
   ENDIF
   IF TAMT=0
      SKIP
      LOOP
   ENDIF
   IF HEAD
      IF P='P'
         DO top-prt1
      ENDIF
      IF P='S'
         DO screen
      ENDIF
      R= 3
      @ R, 25 SAY 'Revenve Budgeting Variance Report'
      &r2
      @ R, 25 SAY '---------------------------------'
      &r2
      &r2
      @ R, 5 SAY 'From'
      @ R, 11 SAY FDATE
      @ R, 24 SAY 'To'
      @ R, 28 SAY SDATE
      @ R, 60 SAY 'Dated'
      @ R, 70 SAY DATE()
      &r2
      &r2
      @ R, 5 SAY 'Code'
      @ R, 22 SAY 'Title'
      @ R, 38 SAY 'Annual'
      @ R, 50 SAY 'For This Period'
      @ R, 70 SAY 'Variance'
      &r2
      @ R, 38 SAY 'Budget'
      @ R, 49 SAY '| Budget |'
      @ R, 60 SAY 'Actual|'
      @ R, 75 SAY '%'
      &r2
      @ R, 1 SAY REPLICATE('-',79)
      &r2
      HEAD= .F.
   ENDIF
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   @ R, 1 SAY MPC1 PICTURE '99'
   @ R, 3 SAY '-'
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ R, 4 SAY MPC2 PICTURE '99'
   @ R, 6 SAY '-'
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ R, 7 SAY MPC3 PICTURE '999'
   @ R, 12 SAY MTITLE
   @ R, 35 SAY MTBUJET PICTURE '9999999999.99'
   ACT= TDAY*MBUJET
   @ R, 48 SAY ACT PICTURE '9999999999.99'
   @ R, 58 SAY TAMT PICTURE '99999999999.99'
   IF MTBUJET<>0
      @ R, 70 SAY 100-TAMT/ACT*100 PICTURE '9999999999.99'
   ENDIF
   &r2
   IF P='S'
      IF R>21
         @ 24, 15 SAY ''
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            SET CONFIRM OFF
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            SET DEVICE TO SCREEN
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         R= 3
         HEAD= .T.
      ENDIF
   ENDIF
   IF P='P'
      IF R>56
         EJECT
         HEAD= .T.
      ENDIF
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   SET DEVICE TO SCREEN
   SET PRINTER OFF
   EJECT
ENDIF
RUN del mfil.dbf
CLOSE ALL
RETURN

PROCEDURE BUD-COMP2
SELECT 2
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 1
USE master
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac2
R2= 'r=r+1'
CL= 'set colo to n/w'
CL2= 'set colo to '
SET DATE brit
STORE 0 TO MC1, MC2, MC3
STORE 0 TO NC1, NC2, NC3
STORE DATE() TO FDATE, SDATE
HEAD= .T.
DO WHILE .T.
   CLEAR
   R= 4
   @ 2, 20 SAY 'Budget Variance Report'
   @ R, 20 SAY 'Budget From Head Code '
   &r2
   &r2
   @ R, 20 GET MC1 PICTURE '99' RANGE 70, 89
   &cl
   @ R, 22 SAY '-'
   &cl2
   @ R, 23 GET MC2 PICTURE '99'
   &cl
   @ R, 25 SAY '-'
   &cl2
   @ R, 26 GET MC3 PICTURE '999'
   READ
   IF READKEY()=12
      CLEAR ALL
      CLEAR
      RETURN
   ENDIF
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
   SELECT 2
   SEEK MCODE
   IF .not. FOUND()
      &r2
      @ R, 20 SAY 'This Code does not Exist - Retry '
      WAIT ''
      LOOP
   ELSE
      &cl
      @ R, 32 SAY TITLE
      &cl2
   ENDIF
   &r2
   &r2
   @ R, 20 SAY ' " " "  To Head Code No'
   &r2
   &r2
   @ R, 20 GET NC1 PICTURE '99' RANGE 70, 89
   &cl
   @ R, 22 SAY '-'
   &cl2
   @ R, 23 GET NC2 PICTURE '99'
   &cl
   @ R, 25 SAY '-'
   &cl2
   @ R, 26 GET NC3 PICTURE '999'
   READ
   IF READKEY()=12
      CLEAR ALL
      CLEAR
      RETURN
   ENDIF
   STORE STR(NC1,2)+STR(NC2,2)+STR(NC3,3) TO MCODE
   SELECT 2
   SEEK MCODE
   IF .not. FOUND()
      &r2
      @ R, 20 SAY 'This Code does not Exist - Retry '
      WAIT ''
      LOOP
   ELSE
      &cl
      @ R, 32 SAY TITLE
      &cl2
   ENDIF
   &r2
   &r2
   @ R, 20 SAY 'From   Date ' GET FDATE
   &r2
   &r2
   @ R, 20 SAY 'To     Date ' GET SDATE
   READ
   IF READKEY()=12
      EXIT
   ENDIF
   IF FDATE>SDATE
      @ 14, 20 SAY 'Second Connot Be Greater Than First Date - Retry'
      WAIT ''
      LOOP
   ENDIF
   EXIT
ENDDO
CLEAR
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
SELECT 1
COPY TO mfil FOR C1>=MC1.and.C1<=NC1.and.DATE>=FDATE.and.DATE<=SDATE.and.C3<>0
USE mfil
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Transaction Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
CLEAR
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO mfil
TDAY= SDATE-FDATE+1
CLEAR
DO WHILE .not. EOF()
   STORE STR(C1,2)+STR(C2,2)+STR(C3,3) TO MCODE
   STORE 0 TO TAMT, TCRAMT, TDRAMT
   STORE C1 TO MC1
   STORE C2 TO MC2
   STORE C3 TO MC3
   SELECT 2
   SEEK MCODE
   IF FOUND()
      STORE BUDGET TO MTBUJET
      STORE BUDGET/365 TO MBUJET
      STORE TITLE TO MTITLE
   ELSE
      STORE 'Invalid Code' TO MTITLE
   ENDIF
   SELECT 1
   DO WHILE STR(C1,2)+STR(C2,2)+STR(C3,3)=MCODE
      TDRAMT= DR_AMOUNT+TDRAMT
      TCRAMT= CR_AMOUNT+TCRAMT
      IF EOF()
         EXIT
      ENDIF
      SKIP
   ENDDO
   IF MC1>70.and.MC1<80
      TAMT= TCRAMT-TDRAMT+TAMT
   ENDIF
   IF MC1>80
      TAMT= TDRAMT-TCRAMT+TAMT
   ENDIF
   IF HEAD
      IF P='P'
         DO top-prt1
      ENDIF
      IF P='S'
         DO screen
      ENDIF
      R= 3
      @ R, 25 SAY 'Revenve Budgeting Variance Report'
      &r2
      @ R, 25 SAY '---------------------------------'
      &r2
      &r2
      @ R, 5 SAY 'From'
      @ R, 11 SAY FDATE
      @ R, 24 SAY 'To'
      @ R, 28 SAY SDATE
      @ R, 60 SAY 'Dated'
      @ R, 70 SAY DATE()
      &r2
      &r2
      @ R, 5 SAY 'Code'
      @ R, 22 SAY 'Title'
      @ R, 38 SAY 'Annual'
      @ R, 50 SAY 'For This Period'
      @ R, 70 SAY 'Variance'
      &r2
      @ R, 38 SAY 'Budget'
      @ R, 49 SAY '| Budget |'
      @ R, 60 SAY 'Actual|'
      @ R, 75 SAY '%'
      &r2
      @ R, 1 SAY REPLICATE('-',79)
      &r2
      HEAD= .F.
   ENDIF
   @ R, 1 SAY MC1 PICTURE '99'
   @ R, 3 SAY '-'
   @ R, 4 SAY MC2 PICTURE '99'
   @ R, 6 SAY '-'
   @ R, 7 SAY MC3 PICTURE '999'
   @ R, 12 SAY MTITLE
   @ R, 35 SAY MTBUJET PICTURE '9999999.99'
   ACT= TDAY*MBUJET
   @ R, 48 SAY ACT PICTURE '999999.99'
   @ R, 58 SAY TAMT PICTURE '9999999.99'
   IF MTBUJET<>0
      @ R, 70 SAY TAMT/ACT*100 PICTURE '9999.99'
   ENDIF
   &r2
   IF P='S'
      IF R>21
         @ 24, 15 SAY ''
         XYZ= ' '
         DO WHILE .not. XYZ$'CQ'
            SET CONFIRM OFF
            @ 24, 55 SAY 'Continue/Quit - C/Q ' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            SET DEVICE TO SCREEN
            CLOSE ALL
            RETURN
         ENDIF
         CLEAR
         R= 3
         HEAD= .T.
      ENDIF
   ENDIF
   IF P='P'
      IF R>56
         EJECT
         HEAD= .T.
      ENDIF
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   SET DEVICE TO SCREEN
   SET PRINTER OFF
   EJECT
ENDIF
CLOSE ALL
RUN del mfil.dbf
RETURN

PROCEDURE TRL-DTL
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE
@ 2, 2 SAY 'Convert To Lotus File Format'
@ 4, 2 SAY 'Subsideries (Detailed) Trial Balance '
@ 8, 2 SAY 'This option will create :'
@ 10, 2 SAY '" Subsideries (Detailed) Trial Balance " Report output file'
@ 11, 2 SAY '  named  " TRL-DTL.DBF ". This Foxbase/dBase file can be   '
@ 12, 2 SAY '  converted to a LOTUS worksheet file using LOTUS Translate Option.'
ANS= ' '
DO WHILE .not. ANS$'CQ'
   @ 24, 55 SAY 'Continue/Quit - C/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
ZEROBAL= ' '
CLEAR
DO WHILE .not. ZEROBAL$'YN'
   @ 4, 2 SAY 'Do You Want To Include Ledger Heads Having Zero Balance - Y/N ' GET ZEROBAL FUNCTION '!'
   READ
ENDDO
@ 4, 2 SAY SPACE(70)
ANS= ' '
DO WHILE .not. ANS$'CQ'
   @ 4, 2 SAY 'Continue/Quit - C/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
SELECT 1
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 2
USE trl-dtl
ZAP
STORE 0 TO MCODE, WRECNO, MSRNO, WCODE, MNO
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE
SELECT 1
GOTO TOP
CLEAR
DO WHILE .not. EOF()
   IF EOF()
      EXIT
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
   MC1= C1
   MC2= C2
   MC3= C3
   MC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   MC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   MC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   MSTAT= STATUS
   SELECT 2
   APPEND BLANK
   REPLACE CODE WITH MC1+'-'+MC2+'-'+MC3
   REPLACE TITLE WITH  A->TITLE
   IF MSTAT='DR'
      REPLACE DR_AMOUNT WITH  A->LDGR_BAL
   ELSE
      REPLACE CR_AMOUNT WITH  A->LDGR_BAL
   ENDIF
   SELECT 1
   SKIP
ENDDO
CLEAR
CLOSE ALL
RETURN

PROCEDURE TRL-SUM
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
@ 2, 2 SAY 'Convert To Lotus File Format'
@ 4, 2 SAY 'Subsideries (Summary) Trial Balance '
@ 8, 2 SAY 'This option will create :'
@ 10, 2 SAY '" Subsideries (Summary) Trial Balance " Report output file'
@ 11, 2 SAY '  named  " TRL-SUM.DBF ". This Foxbase/dBase file can be   '
@ 12, 2 SAY '  converted to a LOTUS worksheet file using LOTUS Translate Option.'
ANS= ' '
DO WHILE .not. ANS$'CQ'
   @ 24, 55 SAY 'Continue/Quit - C/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
SELECT 1
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 2
USE trl-sum
ZAP
SELECT 1
GOTO TOP
DO WHILE .not. EOF()
   SELECT 1
   IF LDGR_BAL=0.and.C3<>0
      SKIP
      LOOP
   ENDIF
   IF EOF()
      EXIT
   ENDIF
   MC1= C1
   MC2= C2
   MTITLE= TITLE
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   STORE 0 TO TDR, TCR, TAMT
   DO WHILE C1=MC1.and.C2=MC2
      IF C1<>MC1.and.C2<>MC2
         EXIT
      ENDIF
      IF STATUS='DR'
         TDR= TDR+LDGR_BAL
      ELSE
         TCR= TCR+LDGR_BAL
      ENDIF
      SKIP
   ENDDO
   IF TDR>TCR
      TAMT= TDR-TCR
   ENDIF
   IF TCR>TDR
      TAMT= TCR-TDR
   ENDIF
   IF TAMT=0
      LOOP
   ENDIF
   SELECT 2
   APPEND BLANK
   REPLACE CODE WITH MPC1+'-'+MPC2
   REPLACE TITLE WITH MTITLE
   IF TDR>TCR
      REPLACE DR_AMOUNT WITH TAMT
   ELSE
      REPLACE CR_AMOUNT WITH TAMT
   ENDIF
   SELECT 1
ENDDO
CLEAR
CLOSE ALL
RETURN

PROCEDURE PL-DTL
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
CLEAR PROGRAM
@ 2, 2 SAY 'Convert To Lotus File Format'
@ 4, 2 SAY '(Detailed) Profit & Loss '
@ 8, 2 SAY 'This option will create :'
@ 10, 2 SAY '" (Detailed) Profit & Loss " Report output file'
@ 11, 2 SAY '  named  " PL-DTL.DBF ". This Foxbase/dBase file can be   '
@ 12, 2 SAY '  converted to a LOTUS worksheet file using LOTUS Translate Option.'
ANS= ' '
DO WHILE .not. ANS$'CQ'
   @ 24, 55 SAY 'Continue/Quit - C/Q ' GET ANS FUNCTION '!'
   READ
ENDDO
IF ANS='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
CLEAR
ZEROBAL= ' '
DO WHILE .not. ZEROBAL$'YN'
   @ 4, 2 SAY 'Do You Want To Include Ledger Heads Having Zero Balance - Y/N ' GET ZEROBAL FUNCTION '!'
   READ
ENDDO
P= ' '
DO WHILE .not. P$'CQ'
   @ 23, 50 SAY 'Continue/Quit - C/Q ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
STORE SPACE(30) TO WTITLE, WHTITLE
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE, TOTCR7, TOTDR7, TOTCR8, TOTDR8, TOT7, TOT8, SWCH
STORE 0 TO AMT6, AMT7, AMT8, AMT9, GPFT, GPFT2
SELECT 2
USE chart
COPY TO abc FOR CHART_NO>5.and.CHART_NO<10
USE abc
INDEX ON CHART_NO TO cht-idx
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
SELECT 4
USE pl-dtl
ZAP
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 1
USE pl-work2
INDEX ON STR(CHART_NO,2)+STR(C1,2)+STR(C2,2)+STR(C3,3) TO pl-work2
SELECT 2
GOTO TOP
SELECT 1
WGTITLE= SPACE(30)
STORE 0 TO WRECNO, MSRNO, WCODE, MCODE
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
      STORE SPACE(30) TO WTITLE
      STORE TITLE TO WTITLE
      MSTAT= STATUS
      IF CHD=0
         STORE CHART_NO*10 TO MC1
         STORE 0 TO MC2, MC3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MLCODE
         SELECT 3
         SEEK MLCODE
         IF .not. FOUND()
            SELECT 2
            HD= AT('Head',CHART)
         ELSE
            SELECT 3
         ENDIF
         SELECT 1
         CHD= 1
      ENDIF
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      SELECT 4
      APPEND BLANK
      REPLACE CODE WITH MPC1+'-'+MPC2+'-'+MPC3
      REPLACE TITLE WITH WTITLE
      IF MCHART=6
         IF MSTAT='DR'
            REPLACE AMOUNT WITH  A->LDGR_BAL* -1
         ELSE
            REPLACE AMOUNT WITH  A->LDGR_BAL
         ENDIF
      ENDIF
      IF MCHART>6
         IF MSTAT='DR'
            REPLACE AMOUNT WITH  A->LDGR_BAL
         ELSE
            REPLACE AMOUNT WITH  A->LDGR_BAL* -1
         ENDIF
      ENDIF
      SELECT 1
      SKIP
   ENDDO
ENDDO
CLEAR
CLOSE ALL
USE pl-work2
ZAP
USE work
ZAP
RUN del abc.dbf
CLOSE ALL
RETURN

PROCEDURE PL-SUM
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR PROGRAM
CLEAR
@ 2, 2 SAY 'Convert To Lotus File Format'
@ 4, 2 SAY '(Summary) Profit & Loss '
@ 8, 2 SAY 'This option will create :'
@ 10, 2 SAY '" (Summary) Profit & Loss " Report output file'
@ 11, 2 SAY '  named  " PL-SUM.DBF ". This Foxbase/dBase file can be   '
@ 12, 2 SAY '  converted to a LOTUS worksheet file using LOTUS Translate Option.'
P= ' '
DO WHILE .not. P$'CQ'
   @ 23, 50 SAY 'Continue/Quit - C/Q ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
STORE SPACE(30) TO WTITLE, WHTITLE
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE, TOTCR7, TOTDR7, TOTCR8, TOTDR8
STORE 0 TO AMT6, AMT7, AMT8, AMT9, GPFT, GPFT2, TOT7, TOT8, SWCH
SELECT 2
USE chart
COPY TO abc FOR CHART_NO>5.and.CHART_NO<10
USE abc
INDEX ON CHART_NO TO cht-idx
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
SELECT 4
USE pl-sum
ZAP
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 1
USE pl-work2
INDEX ON STR(CHART_NO,2)+STR(C1,2)+STR(C2,2)+STR(C3,3) TO pl-work2
SELECT 2
GOTO TOP
SELECT 1
GOTO TOP
WGTITLE= SPACE(30)
STORE 0 TO WRECNO, MSRNO, WCODE, MCODE
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
      IF C3=0
         SKIP
         LOOP
      ENDIF
      LINE= LINE+1
      IF CHD=0
         STORE CHART_NO*10 TO MC1
         STORE 0 TO MD2, MD3
         STORE STR(MC1,2)+STR(MD2,2)+STR(MD3,3) TO MLCODE
         SELECT 3
         SEEK MLCODE
         IF .not. FOUND()
            SELECT 2
            HD= AT('Head',CHART)
         ELSE
            SELECT 3
         ENDIF
         SELECT 1
         CHD= 1
      ENDIF
      MCC3= 0
      STORE SPACE(30) TO WTITLE
      STORE STR(MCC1,2)+STR(MCC2,2)+STR(MCC3,3) TO MLCODE
      SELECT 3
      SEEK MLCODE
      IF FOUND()
         STORE TITLE TO WTITLE
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
      SELECT 4
      APPEND BLANK
      REPLACE CODE WITH MPC1+'-'+MPC2
      REPLACE TITLE WITH WTITLE
      IF MCHART=6
         IF TCR>=TDR
            REPLACE AMOUNT WITH TCR-TDR
         ELSE
            REPLACE AMOUNT WITH TDR* -1-TCR
         ENDIF
      ENDIF
      IF MCHART>6
         IF TDR>=TCR
            REPLACE AMOUNT WITH TDR-TCR
         ELSE
            REPLACE AMOUNT WITH TCR* -1-TDR
         ENDIF
      ENDIF
      SELECT 1
   ENDDO
ENDDO
CLEAR
CLOSE ALL
USE pl-work2
ZAP
USE work
ZAP
RUN del abc.dbf
CLOSE ALL
RETURN

PROCEDURE BS-SUM
CLEAR
CLOSE ALL
@ 2, 2 SAY 'Convert To Lotus File Format'
@ 4, 2 SAY '(Summary) Balance Sheet '
@ 8, 2 SAY 'This option will create :'
@ 10, 2 SAY '" (Summary) Balance Sheet " Report output file'
@ 11, 2 SAY '  named  " BS-SUM.DBF ". This Foxbase/dBase file can be   '
@ 12, 2 SAY '  converted to a LOTUS worksheet file using LOTUS Translate Option.'
P= ' '
DO WHILE .not. P$'CQ'
   @ 23, 50 SAY 'Continue/Quit - C/Q ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
STORE SPACE(30) TO WTITLE, WHTITLE
STORE 0 TO AMT6, AMT7, AMT8, AMT9, GPFT, GPFT2
SELECT 2
USE chart
INDEX ON CHART_NO TO cht-idx
GOTO TOP
MF1= FR
MT1= TO
SELECT 1
USE pl-work2
ZAP
APPEND FROM ldgrcode FOR C1>=MF1.and.C1<=MT1
REPLACE FOR CHART_NO=0 CHART_NO WITH 1
SELECT 2
DO WHILE .not. EOF()
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF2= FR
   MT2= TO
   SELECT 1
   IF MF2<>0.and.MT2<>0
      APPEND FROM ldgrcode FOR C1>=MF2.and.C1<=MT2
      REPLACE FOR CHART_NO=0 CHART_NO WITH 2
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF3= FR
   MT3= TO
   SELECT 1
   IF MF3<>0.and.MT3<>0
      APPEND FROM ldgrcode FOR C1>=MF3.and.C1<=MT3
      REPLACE FOR CHART_NO=0 CHART_NO WITH 3
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF4= FR
   MT4= TO
   SELECT 1
   IF MF4<>0.and.MT4<>0
      APPEND FROM ldgrcode FOR C1>=MF4.and.C1<=MT4
      REPLACE FOR CHART_NO=0 CHART_NO WITH 4
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF5= FR
   MT5= TO
   SELECT 1
   IF MF5<>0.and.MT5<>0
      APPEND FROM ldgrcode FOR C1>=MF5.and.C1<=MT5
      REPLACE FOR CHART_NO=0 CHART_NO WITH 5
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF6= FR
   MT6= TO
   SELECT 1
   IF MF6<>0.and.MT6<>0
      APPEND FROM ldgrcode FOR C1>=MF6.and.C1<=MT6
      REPLACE FOR CHART_NO=0 CHART_NO WITH 6
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF7= FR
   MT7= TO
   SELECT 1
   IF MF7<>0.and.MT7<>0
      APPEND FROM ldgrcode FOR C1>=MF7.and.C1<=MT7
      REPLACE FOR CHART_NO=0 CHART_NO WITH 7
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF8= FR
   MT8= TO
   SELECT 1
   IF MF8<>0.and.MT8<>0
      APPEND FROM ldgrcode FOR C1>=MF8.and.C1<=MT8
      REPLACE FOR CHART_NO=0 CHART_NO WITH 8
   ENDIF
   SELECT 2
   IF .not. EOF()
      SKIP
   ENDIF
   MF9= FR
   MT9= TO
   SELECT 1
   IF MF9<>0.and.MT9<>0
      APPEND FROM ldgrcode FOR C1>=MF9.and.C1<=MT9
      REPLACE FOR CHART_NO=0 CHART_NO WITH 9
   ENDIF
ENDDO
CLEAR
CLOSE ALL
SELECT 4
USE BS-sum
ZAP
SELECT 3
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
SELECT 2
USE chart
INDEX ON CHART_NO TO cht-idx
GOTO TOP
SELECT 1
USE pl-work2
INDEX ON STR(CHART_NO,2)+STR(C1,2)+STR(C2,2)+STR(C3,3) TO bl-c1
WGTITLE= SPACE(30)
STORE 0 TO LTAMT, DTAMT, MCODE, PCON
STORE 0 TO WRECNO, MSRNO, WCODE
STORE 0 TO DRTOT, CRTOT, GTDR, GTCR, LINE, TOTDR, TOTCR, NTOT
STORE 0 TO TCR1, TDR1, TCR2, TDR2, TCR3, TDR3
STORE 0 TO TCR4, TDR4, TCR5, TDR5, MC3
CLEAR
GOTO TOP
DO WHILE .not. EOF()
   IF EOF()
      EXIT
   ENDIF
   IF CHART_NO>5
      SKIP
      LOOP
   ENDIF
   TAMT= 0
   IF CHART_NO<=3
      STORE CHART_NO TO MCHART
      DO WHILE CHART_NO=MCHART
         STORE 0 TO TDR, TCR, AMT, MC3
         STORE C1 TO MC1
         STORE C2 TO MC2
         DO WHILE C1=MC1.and.C2=MC2
            IF STATUS='DR'
               TDR= TDR+LDGR_BAL
            ELSE
               TCR= TCR+LDGR_BAL
            ENDIF
            SKIP
         ENDDO
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SELECT 3
         SEEK MCODE
         IF FOUND()
            STORE TITLE TO MTITLE
         ELSE
            STORE 'Group Total ' TO MTITLE
         ENDIF
         SELECT 1
         IF MCHART<=3
            AMT= TCR-TDR
         ELSE
            AMT= TDR-TCR
         ENDIF
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         SELECT 4
         APPEND BLANK
         REPLACE CODE WITH MPC1+'-'+MPC2
         REPLACE TITLE WITH MTITLE
         IF MC2<>0
            REPLACE AMOUNT WITH AMT
            TAMT= AMT+TAMT
         ENDIF
         SELECT 1
      ENDDO
      SELECT 2
      SEEK MCHART
      IF FOUND()
      ENDIF
      SELECT 1
      LTAMT= LTAMT+TAMT
   ELSE
      IF PCON=0
         PCON= 2
         HEADING= .T.
      ENDIF
      STORE CHART_NO TO MCHART
      DO WHILE CHART_NO=MCHART
         STORE 0 TO TDR, TCR, AMT, MC3
         STORE C1 TO MC1
         STORE C2 TO MC2
         DO WHILE C1=MC1.and.C2=MC2
            IF STATUS='DR'
               TDR= TDR+LDGR_BAL
            ELSE
               TCR= TCR+LDGR_BAL
            ENDIF
            SKIP
         ENDDO
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SELECT 3
         SEEK MCODE
         IF FOUND()
            STORE TITLE TO MTITLE
         ELSE
            STORE 'Group Total ' TO MTITLE
         ENDIF
         SELECT 1
         IF MCHART<=3
            AMT= TCR-TDR
         ELSE
            AMT= TDR-TCR
         ENDIF
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         SELECT 4
         APPEND BLANK
         REPLACE CODE WITH MPC1+'-'+MPC2
         REPLACE TITLE WITH MTITLE
         IF MC2<>0
            REPLACE AMOUNT WITH AMT
            TAMT= AMT+TAMT
         ENDIF
         SELECT 1
      ENDDO
      SELECT 2
      SEEK MCHART
      IF FOUND()
      ENDIF
      SELECT 1
      DTAMT= DTAMT+TAMT
   ENDIF
ENDDO
CLEAR
CLOSE ALL
RETURN

PROCEDURE MAINTMEN
DO WHILE .T.
   SET EXCLUSIVE ON
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   COL= 25
   SET COLOR TO +7/1
   @ 6, 27 SAY 'System Maintenance Menu'
   SET COLOR TO 7/1
   @ 8, COL PROMPT '1 - Backup Data (Via Dos 4 & Above) '
   @ 9, COL PROMPT '2 - Backup Data (Via Dos 3.3      ) '
   @ 10, COL PROMPT '3 - Re-index Data Files '
   @ 11, COL PROMPT '4 - Sort Ledgers Date Wise '
   @ 13, COL PROMPT '5 - Return to Previous Menu '
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
         DO sortdate
      CASE CANS=5
         CLEAR
         EXIT
   ENDCASE
ENDDO
CLEAR
RETURN

PROCEDURE BACK-UP1
CLEAR
CLEAR ALL
SET COLOR TO 7/1
CLEAR
ANS= ' '
DO WHILE .not. ANS$'YN'
   ANS2= ' '
   DO WHILE .not. ANS2$'AB'
      @ 5, 15 SAY 'Enter Drive On Which You Want To Make Backup - A/B ' GET ANS2 PICTURE '!'
      READ
   ENDDO
   @ 8, 15 SAY 'Please Insert Diskette in Drive To Backup Data Files '
   @ 10, 15 SAY 'Do You Want To Continue  - Y/N ' GET ANS PICTURE '!'
   READ
   IF ANS='Y'
      CLEAR
      IF ANS2='A'
         RUN backup *.dbf a:
      ELSE
         RUN backup *.dbf b:
      ENDIF
   ELSE
      EXIT
   ENDIF
ENDDO
CLEAR
RETURN

PROCEDURE BACK-UP2
CLEAR
CLEAR ALL
SET COLOR TO 7/1
CLEAR
ANS= ' '
DO WHILE .not. ANS$'YN'
   ANS2= ' '
   DO WHILE .not. ANS2$'AB'
      @ 5, 15 SAY 'Enter Drive On Which You Want To Make Backup - A/B ' GET ANS2 PICTURE '!'
      READ
   ENDDO
   @ 8, 15 SAY 'Please Insert Diskette in Drive To Backup Data Files '
   @ 10, 15 SAY 'Do You Want To Continue  - Y/N ' GET ANS PICTURE '!'
   READ
   IF ANS='Y'
      CLEAR
      IF ANS2='A'
         RUN backup c:*.dbf a:
      ELSE
         RUN backup c:*.dbf b:
      ENDIF
   ELSE
      EXIT
   ENDIF
ENDDO
CLEAR
RETURN

PROCEDURE REINDEX
CLEAR
CLOSE ALL
SET EXCLUSIVE ON
SET DELETED OFF
SET COLOR TO 7/1
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
SET EXCLUSIVE ON
USE ldgrcode
PACK
USE tran-key
PACK
USE master
PACK
USE voucher
ZAP
USE vch-cor
ZAP
USE tran-rep
ZAP
USE trl-ref
PACK
USE pl-work2
ZAP
USE work
ZAP
USE tran
PACK
CLOSE ALL
SELECT 1
USE tran
INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TO tranent
SELECT 2
USE tran-key
INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TO key-idx
ZAP
SELECT 1
DO WHILE .not. EOF()
   MVCH_TYPE= VCH_TYPE
   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2
   SELECT 2
   SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   IF .not. FOUND()
      APPEND BLANK
      REPLACE VCH_TYPE WITH MVCH_TYPE
      REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
   ENDIF
   SELECT 1
   SKIP
ENDDO
SELECT 2
GOTO TOP
REPLACE ALL FLAG WITH 'Y'
CLOSE ALL
CLEAR
SET EXCLUSIVE ON
CLOSE ALL
USE ldgrcode
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac1
INDEX ON TITLE TO tit-idx
USE tran
INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TO tranent
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO tridx
INDEX ON DATE TO trandate
USE tran-key
INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TO key-idx
USE master
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO ac2
INDEX ON DATE TO mastdate
USE chart
INDEX ON STR(FR,2)+STR(TO,2) TO crt
USE tran-rep
INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TO rep-idx
USE trl-ref
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO trl-idx
CLEAR
CLOSE ALL
SET EXCLUSIVE ON
SET DELETED ON
CLOSE ALL
RETURN

PROCEDURE SORTDATE
CLEAR PROGRAM
CLEAR ALL
CLEAR
SET COLOR TO 7/1
CLEAR
SET DATE brit
@ 10, 18 SAY 'Ledgers Being Sorted Date Wise - Please Wait '
USE master
SORT ON DATE, SR_NO TO mastsort
USE mastsort
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TO mastsort
HEAD= .T.
Q= 10
DO WHILE .not. EOF()
   STORE STR(C1,2)+STR(C2,2)+STR(C3,3) TO MCODE
   IF SR_NO<>1
      IF Q=10
         CLEAR
         @ 14, 26 SAY 'Please Switch Your Printer On '
         WAIT
      ENDIF
      CLEAR
      PANS= ' '
      DO prnchk
      IF PANS='Q'
         CLOSE ALL
         CLEAR
         RETURN
      ENDIF
      @ 11, 20 SAY 'Please wait ... Printing In Progress '
      SET PRINTER ON
      SET DEVICE TO PRINTER
      Q= 21
      IF HEAD
         R= 5
         @ R, 1 SAY 'The Following Vouchers Are Dated Before The Opening Balance Date '
         R= R+1
         @ R, 1 SAY "Please Correct Voucher's Date Before Using This Option "
         R= R+2
         @ R, 1 SAY 'Date'
         @ R, 10 SAY 'Vch-Type'
         @ R, 20 SAY 'Vch-No'
         @ R, 35 SAY 'C o d e'
         @ R, 47 SAY 'Description'
         R= R+1
         @ R, 1 SAY REPLICATE('-',79)
         HEAD= .F.
         R= R+2
      ENDIF
      @ R, 1 SAY DATE
      @ R, 10 SAY VCH_TYPE
      @ R, 20 SAY VCH_NO
      @ R, 30 SAY C1
      @ R, 33 SAY C2
      @ R, 37 SAY C3
      @ R, 45 SAY DESC
      R= R+1
      IF R>54
         EJECT
         HEAD= .F.
      ENDIF
   ENDIF
   DO WHILE STR(C1,2)+STR(C2,2)+STR(C3,3)=MCODE
      SKIP
   ENDDO
ENDDO
IF Q=21
   SET DEVICE TO SCREEN
   EJECT
   CLEAR ALL
   RETURN
ENDIF
REPLACE FOR SR_NO<>1 BL_AMOUNT WITH 0
USE mastsort INDEX mastsort
DO WHILE .not. EOF()
   MSRNO= 0
   STORE STR(C1,2)+STR(C2,2)+STR(C3,3) TO MCODE
   IF SR_NO<>1
      @ 16, 25 SAY 'The Voucher No'
      @ 16, 45 SAY VCH_NO
      @ 17, 25 SAY 'Is Dated Before The Opening Balance Date '
      @ 18, 25 SAY 'Please Correct Data/Voucher Date '
      WAIT ''
      CLEAR ALL
      RETURN
   ELSE
      IF BL_AMOUNT=0
         MSRNO= MSRNO+1
         REPLACE SR_NO WITH MSRNO
         SKIP
      ENDIF
   ENDIF
   MAMT= BL_AMOUNT
   MST= STATUS
   DO WHILE STR(C1,2)+STR(C2,2)+STR(C3,3)=MCODE
      MSRNO= MSRNO+1
      REPLACE SR_NO WITH MSRNO
      IF MST='DR'
         IF DR_AMOUNT>0
            REPLACE BL_AMOUNT WITH MAMT+DR_AMOUNT
            REPLACE STATUS WITH 'DR'
         ENDIF
         IF CR_AMOUNT>0
            IF CR_AMOUNT>MAMT
               REPLACE BL_AMOUNT WITH CR_AMOUNT-MAMT
               REPLACE STATUS WITH 'CR'
            ENDIF
            IF CR_AMOUNT<MAMT
               REPLACE BL_AMOUNT WITH MAMT-CR_AMOUNT
               REPLACE STATUS WITH 'DR'
            ENDIF
            IF CR_AMOUNT<>0
               IF CR_AMOUNT=MAMT
                  REPLACE BL_AMOUNT WITH MAMT-CR_AMOUNT
                  REPLACE STATUS WITH 'DR'
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      IF MST='CR'
         IF CR_AMOUNT>0
            REPLACE BL_AMOUNT WITH MAMT+CR_AMOUNT
            REPLACE STATUS WITH 'CR'
         ENDIF
         IF DR_AMOUNT>0
            IF DR_AMOUNT>MAMT
               REPLACE BL_AMOUNT WITH DR_AMOUNT-MAMT
               REPLACE STATUS WITH 'DR'
            ENDIF
            IF DR_AMOUNT<MAMT
               REPLACE BL_AMOUNT WITH MAMT-DR_AMOUNT
               REPLACE STATUS WITH 'CR'
            ENDIF
            IF DR_AMOUNT<>0
               IF DR_AMOUNT=MAMT
                  REPLACE BL_AMOUNT WITH MAMT-DR_AMOUNT
                  REPLACE STATUS WITH 'DR'
               ENDIF
            ENDIF
         ENDIF
      ENDIF
      MAMT= BL_AMOUNT
      MST= STATUS
      SKIP
   ENDDO
ENDDO
CLEAR ALL
USE mastsort
COPY TO master
RUN del mastsort.dbf
RETURN

PROCEDURE SECURMEN
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 27 SAY ' System Security Menu'
   SET COLOR TO 7/1
   @ 8, 25 PROMPT '1 - Change Passwords '
   @ 9, 25 PROMPT '2 - Unpost Vouchers From Ledger'
   @ 10, 25 PROMPT '3 - Year End Procedures   '
   @ 11, 25 PROMPT '4 - Initialize Data File  '
   @ 12, 25 PROMPT '5 - Initialize Password   '
   @ 13, 25 PROMPT '6 - Party Locking System  '
   @ 15, 25 PROMPT '9 - Return to Previous Menu '
   CANS= 0
   MENU TO CANS
   DO CASE
      CASE CANS=1
         DO password
      CASE CANS=2
         DO unpost
      CASE CANS=3
         CLEAR
         SET COLOR TO +7/1
         @ 10, 25 SAY ' Please  Refer  to .... '
         @ 12, 25 SAY ' LOGICAL SYSTEMS (PVT) LIMITED'
         SET COLOR TO 7/1
         @ 14, 25 SAY ' Press any key to Return to Menu'
         WAIT ''
         SET COLOR TO 7/1       
      CASE CANS=4
         DO initial
      CASE CANS=5
         DO init-pw
      CASE CANS=6
         DO lock-men
      CASE CANS=7
         CLEAR
         EXIT
   ENDCASE
ENDDO
CLEAR
RETURN

PROCEDURE PASSWORD
CLEAR
CLOSE ALL
USE ac-pw
GOTO TOP
STORE SPACE(6) TO MPW1, MPW2, MPW3, MPW4, MPW5, MPW6, MPW7, MPW8
R= 5
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm1    = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 1'
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
   @ R, 24 SAY 'Password For Menu 2'
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
   @ R, 24 SAY 'Password For Menu 3'
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
   @ R, 24 SAY 'Password For Menu 4'
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
   @ R, 24 SAY 'Password For Menu 5'
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
   R= R+2
   SKIP
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm6     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 6'
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
   @ R, 24 SAY 'Password For Menu 7'
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
   SKIP
   R= R+2
   NCODE1= SUBSTR(PW,1,1)
   NCODE2= SUBSTR(PW,2,1)
   NCODE3= SUBSTR(PW,3,1)
   NCODE4= SUBSTR(PW,4,1)
   NCODE5= SUBSTR(PW,5,1)
   NCODE6= SUBSTR(PW,6,1)
   mm8     = chr(asc("&ncode1")+30)+chr(asc("&ncode2")+28)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)
   @ R, 24 SAY 'Password For Menu 8'
   @ R, 46 GET MM8 FUNCTION '!'
   READ
   MCODE= MM8
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

PROCEDURE UNPOST
SET EXCLUSIVE ON
CLOSE ALL
CLEAR
ANS= ' '
DO WHILE .not. ANS$'YN'
   SET COLOR TO 7/1
   CLEAR
   @ 10, 15 SAY ' This option will un-post all posted vouchers'
   @ 11, 15 SAY ' from the ledger.'
   @ 13, 15 SAY ' Do you want to continue...... Y/N ' GET ANS PICTURE '!'
   READ
   IF ANS='Y'
      EXIT
   ELSE
      CLEAR
      RETURN
   ENDIF
ENDDO
CLEAR
USE ldgrcode
DO WHILE .not. EOF()
   IF C3=0
      SKIP
      LOOP
   ENDIF
   REPLACE LDGR_BAL WITH OPEN_BAL, STATUS WITH OPEN_STAT, SR_NO WITH 1
   SKIP
   IF EOF()
      EXIT
   ENDIF
ENDDO
USE master
DELETE ALL FOR SR_NO<>1
PACK
USE TRAN
REPLACE FOR FLAG='Y' FLAG WITH ' '
SET EXCLUSIVE ON
CLOSE ALL
CLEAR
RETURN

PROCEDURE INITIAL
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
@ 10, 15 SAY 'Running this program will erase all data from'
@ 11, 15 SAY 'your data files'
P= ' '
DO WHILE .not. P$'YN'
   @ 12, 15 SAY 'Want to proceed ......Y/N' GET P FUNCTION '!'
   READ
   IF P='N'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   IF P='Y'
      CLEAR
      SET COLOR TO +7/1
      @ 10, 25 SAY ' Please  Refer  to .... '
      @ 12, 25 SAY ' LOGICAL SYSTEMS (PVT) LIMITED'
      SET COLOR TO 7/1
      @ 14, 25 SAY ' Press any key to Return to Menu'
      WAIT ''
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE INIT-PW
CLEAR
CLOSE ALL
SET COLOR TO 7/1
CLEAR
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
USE ac-pw
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

PROCEDURE LOCK-MEN
DO WHILE .T.
   SET COLOR TO 7/1
   CLEAR
   DO top
   DO bot
   SET COLOR TO +7/1
   @ 6, 26 SAY '      Party Locking '
   SET COLOR TO 7/1
   @ 8, 24 PROMPT '1 - Party Locking ...... Entry    '
   @ 10, 24 PROMPT '2 - Party Locking ...... Deletion '
   @ 12, 24 PROMPT '3 - Party Locking ...... Printing '
   @ 14, 24 PROMPT '4 - Party Locking (Alphabetically Sorted) '
   @ 16, 24 PROMPT '9 - Return to Previous Menu       '
   CHOICE= 0
   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         DO lock-ent
      CASE CHOICE=2
         DO lock-del
      CASE CHOICE=3
         DO lock-prt
      CASE CHOICE=4
         DO lockprt
      CASE CHOICE=5
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN

PROCEDURE LOCK-ENT
CLEAR
CLOSE ALL
SET CONFIRM OFF
SET COLOR TO 7/1
SET DELETED ON
SELECT 1
USE ldgrcode INDEX ac1
DO WHILE .T.
   CLEAR
   R= 8
   DO top
   SET COLOR TO +7/1
   @ 6, 28 SAY 'Party Locks .. Entry  '
   SET COLOR TO 7/1
   @ 23, 1 TO 23, 79
   STORE 0 TO MC1, MC2, MC3, MLDGR_BAL
   @ 24, 51 SAY 'Press Ctrl Q To Quit '
   @ R, 8 SAY 'Ledger Head Code '
   SET COLOR TO /w
   @ R, 30 SAY '  -  -   '
   @ R, 30 GET MC1 PICTURE '@z 99'
   SET COLOR TO 7/1
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      SET CONFIRM OFF
      RETURN
   ENDIF
   @ 24, 50 CLEAR TO 24, 78
   IF MC1=0
      LOOP
   ENDIF
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   SET COLOR TO /w
   @ R, 30 SAY MPC1
   SET COLOR TO 7/1
   @ R, 33 GET MC2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   SET COLOR TO /w 
   @ R, 33 SAY MPC2
   SET COLOR TO 7/1
   @ R, 36 GET MC3 PICTURE '@z 999'
   READ
   IF MC3=0
      LOOP
   ENDIF
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   SET COLOR TO /w 
   @ R, 36 SAY MPC3
   SET COLOR TO 7/1
   IF READKEY()=12
      CLOSE ALL
      CLEAR
      SET CONFIRM OFF
      RETURN
   ENDIF
   SELECT 1
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MLCODE
   SEEK MLCODE
   IF .not. FOUND()
      @ 24, 15 SAY 'This Code Does Not Exist -- Please Re-try '
      WAIT ''
      @ 24, 8 CLEAR TO 24, 63
      LOOP
   ENDIF
   IF LOCK='Y'
      @ 24, 12 SAY 'This Party Code Is Already Defined As Lock -- Please Re-try '
      WAIT ''
      @ 24, 8 CLEAR TO 24, 63
      LOOP
   ENDIF
   SELECT 1
   R= R+2
   @ R, 8 SAY 'Title of Ledger Head '
   SET COLOR TO /w
   @ R, 30 SAY TITLE
   SET COLOR TO 7/1
   R= R+2
   IF ADDRESS1<>SPACE(25)
      @ R, 8 SAY 'Address              '
      SET COLOR TO /w
      @ R, 30 SAY ADDRESS1
      SET COLOR TO 7/1
      R= R+2
      @ R, 8 SAY '                     '
      SET COLOR TO /w
      @ R, 30 SAY ADDRESS2
      SET COLOR TO 7/1
      R= R+2
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 24, 25 SAY 'Are you Sure - Y/N' GET P FUNCTION '!'
      READ
   ENDDO
   @ 24, 1 CLEAR TO 24, 64
   IF P='Y'
      SELECT 1
      REPLACE LOCK WITH 'Y'
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 24, 24 SAY 'More Data Entry - Y/N' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE LOCK-DEL
CLEAR
CLOSE ALL
SET CONFIRM OFF
SET COLOR TO 7/1
SET DELETED ON
SELECT 1
USE ldgrcode INDEX ac1
DO WHILE .T.
   CLEAR
   R= 8
   DO top
   SET COLOR TO +7/1
   @ 6, 28 SAY 'Party Locks .. Deletion '
   SET COLOR TO 7/1
   @ 23, 1 TO 23, 79
   STORE 0 TO MC1, MC2, MC3, MLDGR_BAL
   @ 24, 51 SAY 'Press Ctrl Q To Quit '
   @ R, 8 SAY 'Ledger Head Code '
   SET COLOR TO /w
   @ R, 30 SAY '  -  -   '
   @ R, 30 GET MC1 PICTURE '@z 99'
   SET COLOR TO 7/1
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      SET CONFIRM OFF
      RETURN
   ENDIF
   @ 24, 50 CLEAR TO 24, 78
   IF MC1=0
      LOOP
   ENDIF
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   SET COLOR TO /w
   @ R, 30 SAY MPC1
   SET COLOR TO 7/1
   @ R, 33 GET MC2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   SET COLOR TO /w 
   @ R, 33 SAY MPC2
   SET COLOR TO 7/1
   @ R, 36 GET MC3 PICTURE '@z 999'
   READ
   IF MC3=0
      LOOP
   ENDIF
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   SET COLOR TO /w 
   @ R, 36 SAY MPC3
   SET COLOR TO 7/1
   IF READKEY()=12
      CLOSE ALL
      CLEAR
      SET CONFIRM OFF
      RETURN
   ENDIF
   SELECT 1
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MLCODE
   SEEK MLCODE
   IF .not. FOUND()
      @ 24, 15 SAY 'This Code Does Not Exist -- Please Re-try '
      WAIT ''
      @ 24, 8 CLEAR TO 24, 63
      LOOP
   ENDIF
   IF LOCK=' '
      @ 24, 12 SAY 'This Party Code Has Not Been Defined As Lock -- Please Re-try '
      WAIT ''
      @ 24, 8 CLEAR TO 24, 63
      LOOP
   ENDIF
   SELECT 1
   R= R+2
   @ R, 8 SAY 'Title of Ledger Head '
   SET COLOR TO /w
   @ R, 30 SAY TITLE
   SET COLOR TO 7/1
   R= R+2
   IF ADDRESS1<>SPACE(25)
      @ R, 8 SAY 'Address              '
      SET COLOR TO /w
      @ R, 30 SAY ADDRESS1
      SET COLOR TO 7/1
      R= R+2
      @ R, 8 SAY '                     '
      SET COLOR TO /w
      @ R, 30 SAY ADDRESS2
      SET COLOR TO 7/1
      R= R+2
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 24, 25 SAY 'Are you Sure - Y/N' GET P FUNCTION '!'
      READ
   ENDDO
   @ 24, 1 CLEAR TO 24, 64
   IF P='Y'
      SELECT 1
      REPLACE LOCK WITH ' '
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 24, 24 SAY 'More Data Entry - Y/N' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

PROCEDURE LOCK-PRT
CLEAR
CLOSE ALL
SET CONFIRM OFF
SET COLOR TO 7/1
CLEAR
USE ldgrcode INDEX ac1
IF RECCOUNT()=0
   CLEAR
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
MC1= 0
DO WHILE .T.
   SET COLOR TO +7/1
   @ 10, 14 SAY 'Please give "First Level Code" '
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.'
   @ 11, 66 GET MC1 PICTURE '99'
   @ 11, 69 SAY '-xx-xxx'
   SET COLOR TO 7/1
   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SET FILTER TO C1=MC1.and.LOCK='Y'
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
CNT= 0
PAGE= 1
HEADING= .T.
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      IF P='P'
         DO top-prt1
         @ 2, 15 SAY CHR(18)
         @ 2, 32 SAY 'LIST OF CODES '
         @ 3, 32 SAY '--------------'
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
      ELSE
         DO screen
         @ 3, 32 SAY 'LIST OF CODES '
         L= L+1
         @ L, 32 SAY '------------- '
         L= L+1
         @ L, 65 SAY 'Page - '
         @ L, 70 SAY PAGE PICTURE '999'
         CNT= 0
         MREC2= RECNO()
      ENDIF
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 8 SAY 'Code'
      @ L, 25 SAY 'Title '
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      HEADING= .F.
      L= L+1
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
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
      @ L, 20 SAY TITLE
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
      @ L, 20 SAY TITLE
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
         @ L, 20 SAY TITLE
      ENDIF
   ENDIF
   SKIP
   CNT= CNT+1
   L= L+2
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
         PAGE= PAGE-1
         IF PAGE<=1
            PAGE= 1
         ENDIF
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

PROCEDURE LOCKPRT
CLEAR
CLOSE ALL
SET CONFIRM OFF
SET COLOR TO 7/1
CLEAR
USE ldgrcode INDEX tit-idx
IF RECCOUNT()=0
   CLEAR
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
MC1= 0
DO WHILE .T.
   SET COLOR TO +7/1
   @ 10, 14 SAY 'Please give "First Level Code" '
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.'
   SET COLOR TO 7/1
   @ 11, 66 GET MC1 PICTURE '99'
   @ 11, 69 SAY '-xx-xxx'
   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SET FILTER TO C1=MC1.and.LOCK='Y'
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   PANS= ' '
   DO prnchk
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 11, 20 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
CNT= 0
PAGE= 1
HEADING= .T.
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      IF P='P'
         DO top-prt1
         @ 2, 15 SAY CHR(18)
         @ 2, 36 SAY 'LIST OF CODES '
         @ 3, 36 SAY '------------- '
         @ 4, 32 SAY '(Alphabetically Sorted)'
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
      ELSE
         DO SCREEN
         @ 3, 36 SAY 'LIST OF CODES '
         L= L+1
         @ L, 36 SAY '------------- '
         L= L+1
         @ L, 32 SAY '(Alphabetically Sorted)          Page - '
         @ L, 70 SAY PAGE PICTURE '999'
         CNT= 0
         MREC2= RECNO()
      ENDIF
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 8 SAY 'Code'
      @ L, 25 SAY 'Title'
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      HEADING= .F.
      L= L+1
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
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
      @ L, 20 SAY TITLE
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
      @ L, 20 SAY TITLE
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
         @ L, 20 SAY TITLE
      ENDIF
   ENDIF
   SKIP
   CNT= CNT+1
   L= L+2
   IF P='S'.and.L>=21
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
         PAGE= PAGE-1
         IF PAGE<=1
            PAGE= 1
         ENDIF
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

PROCEDURE DENIED
CLEAR
ABC= 0
DO WHILE ABC<=3
   ?? CHR(7)
   SET COLOR TO *+7/1
   @ 12, 25 SAY 'Access Denied ! '
   ABC= ABC+1
ENDDO
SET COLOR TO 7/1
CLEAR
RETURN

PROCEDURE PRNCHK
IF SYS(13)='OFFLINE'
   PANS= ' '
   DO WHILE .not. PANS$'CQ'
      CLEAR
      SET COLOR TO 7/1
      CLEAR
      @ 10, 10 SAY 'You Printer is Not Connected - Please Check'
      @ 11, 10 SAY 'Continue/Quit - C/Q ' GET PANS PICTURE '!'
      READ
   ENDDO
   IF PANS='C'
      CLEAR
      RETRY
   ENDIF
   IF PANS='Q'
      CLEAR
      RETURN
   ELSE
      RETURN
   ENDIF
ENDIF
 
