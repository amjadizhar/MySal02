********************************
* PROGRAM: PST_PROC.PRG
* Author : Amjad Izhar
* Posting Vouchers to Ledgers
* Written: 15-09-96
* This Revision On : 10-Feb-2017
* at DHA Home
********************************


**********************
PROCEDURE VCH_CONF
**********************

CLEAR
CLOSE ALL
SET DELETED ON

SET COLOR TO 7/1

@ 2, 20 SAY 'Re-indexing Data Files .... Please wait'

SELECT 1
USE tran ORDER tranent

SELECT 2
USE ldgrcode ORDER ac1

SELECT 1

DECLARE MAVT( 7)

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
         DO chk IN tbl_proc
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
         USE master ORDER ac2

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
********************



**********************
PROCEDURE LDGRPOST
***********************

CLEAR
CLOSE DATA

SET DATE brit
HEADING= .T.

STORE DATE() TO MPDATE

SELECT 1
USE tran EXCLUSIVE
SORT ON VCH_TYPE, VCH_NO1, VCH_NO2, SR_NO TO smq
ZAP
APPEND FROM smq
USE

*RUN del smq.dbf

SELECT 1
USE tran ORDER trandate


GOTO BOTTOM
CLEAR
STORE DATE TO MDATE, MPDATE


@ 1, 2 SAY 'Post Transactions To Ledger:' 
@ 3, 2 SAY 'This option will post all un-posted vouchers to their respective' 
@ 4, 2 SAY 'ledger heads Upto Given Date....' 


@ 13, 14 SAY 'Vouchers upto                 exist in file ' 


@ 13, 29 SAY MDATE 


@ 14, 14 SAY 'Date upto which you want vouchers to be posted ' GET MPDATE 
READ

P= ' '



DO WHILE .not. P$'CQ'
   @ 22, 5 SAY 'Continue/Quit  -  C/Q  '
   @ 22, 30 GET P FUNCTION '!'
   READ
ENDDO

IF P='Q'
	CLEAR
	*CLEAR EVENTS
   CLOSE DATA
   RETURN
ENDIF


CLEAR
@ 12, 23 SAY 'Press any key to start posting' 
WAIT ''
CLEAR
@ 13, 23 SAY 'Please wait data is being posted....' 
SELECT 2
USE ldgrcode ORDER ac1
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
   CLOSE DATA
   RETURN
ENDIF

CLEAR
@ 13, 23 SAY 'Please wait data is being posted....'

SELECT 1
GOTO TOP

DO WHILE .not. EOF()
   DO chk IN tbl_proc.prg
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

IF FLOCK()
   REPLACE DATE WITH MDATE
   REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
   REPLACE BL_AMOUNT WITH ABS(WAMOUNT)
	UNLOCK
ELSE

ENDIF

   IF MDR_AMOUNT>0
      
   	IF FLOCK()   
      REPLACE DR_AMOUNT WITH MDR_AMOUNT
   		UNLOCK
   ELSE
   
   ENDIF
   
   ELSE
      IF FLOCK()
      REPLACE CR_AMOUNT WITH MCR_AMOUNT
      UNLOCK
      ELSE
      ENDIF
   ENDIF
	IF FLOCK()
   REPLACE DESC WITH MDISC
   REPLACE DESC1 WITH MDISC1
   REPLACE VCH_TYPE WITH MVCH_TYPE
   REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
   REPLACE SR_NO WITH WSRNO
   REPLACE C->STATUS WITH ST
   	UNLOCK
   	ELSE
   	
   	ENDIF
   
   WSRNO= 0
   SELECT 1
   IF FLOCK()
   REPLACE FLAG WITH 'Y'
   UNLOCK
   ELSE
   
   ENDIF
   SKIP
ENDDO

CLOSE DATA

*USE master EXCLUSIVE
*REINDEX

SELECT * ;
FROM TRAN ;
ORDER BY DATE,VCH_TYPE,VCH_NO1,VCH_NO2,SR_NO ;
INTO DBF TRANSORT.DBF

USE TRANSORT EXCLUSIVE
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG CODE

SELE TRAN
USE
USE TRAN EXCLUSIVE
ZAP
APPEND FROM TRANSORT


CLEAR
CLOSE DATA


RETURN
******************************

