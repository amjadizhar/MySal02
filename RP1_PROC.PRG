*******************************
* PROGRAM: RP1_PROC.PRG 	  *
* Author : Amjad Izhar 		  *
* Report - 1 Procedures	      *
* Written : 15-09-96 		  *
* Revised Again On : 19-01-2001 7:15 PM
*******************************

*************************************
* Accounts Receivable Report 		*
*************************************
PROCEDURE REP_REC 
***********************

SET CONFIRM OFF

HEADING= .T.
PAGE= 1
L= 2
MTOT= 0

SELECT 2
USE ldgrcode ORDER ac1

STORE 0 TO MC1, MC2, MC3, TDR, TCR, MBL_AMOUNT
STORE 0 TO MDRTOT, MCRTOT
STORE 0 TO MNO && No Of Copies
STORE ' ' TO MSTATUS
WTITLE= SPACE(30)
CNT= 0
RMK= '             '


IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR

ZEROBAL= ' '

DO WHILE .not. ZEROBAL$'YN'
   @ 4, 2 SAY 'Do You Want To Include Ledger Heads Having Zero Balance - Y/N ' GET ZEROBAL FUNCTION '!'
   READ
ENDDO

CLEAR
ANS= ' '

SELECT 2  && ldgrcode ORDER ac1

DO WHILE .T.


IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 8, 14 SAY 'Accounts Receivable Report '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 10, 14 SAY 'Please give "First Two Level Code" of Debtors '
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts....'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 67 SAY '-'

IF _DOS
   SET COLOR TO 7/1
ENDIF

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
USE master ALIAS master EXCLUSIVE
INDEX ON date TAG mastdate 
SET ORDER TO mastdate


GOTO BOTTOM

STORE DATE TO MTDATE

CLEAR

@ 2, 5 SAY 'Accounts Receivable Report '
@ 5, 5 SAY 'Your last posted voucher is dated '
@ 5, 42 SAY MTDATE
@ 6, 5 SAY 'Do you want to dispaly same date on your Report '
@ 7, 5 SAY 'Or enter the date you want to dispaly  ' GET MTDATE
READ

SELECT 2 && ldgrcode ORDER ac1

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
   do PRNCHK in tbl_proc
   CLEAR
   @ 2, 5 SAY 'No Of Copies       ' GET MNO PICTURE '99'
   READ

   CLEAR

   PANS= ' ' && Printer Answer from PRNCHK procedure

   DO prnchk IN tbl_proc.prg

   IF PANS='Q' && Printer Answer from PRNCHK procedure
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

   IF ZEROBAL='N'
	   SET FILTER TO C1=MC1.and.C2=MC2.and.ldgr_bal<>0
   ELSE
   	   SET FILTER TO C1=MC1.and.C2=MC2
   ENDIF

   GOTO TOP
   DO WHILE .not. EOF()
      IF HEADING
         IF ANS='S'
            DO screen IN tbl_proc.prg
         ENDIF
         IF ANS='P'
			DO PRNCHK IN tbl_proc.prg
            DO top_prt1 IN tbl_proc.prg
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
         DO WHILE .not. XYZ$'CPQ'
            SET CONFIRM OFF
            @ 23, 45 SAY 'Continue/Previous/Quit - C/P/Q' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF

		 IF XYZ='P' && Previous Page
		 	IF NOT BOF()

		 		SKIP -28

		 	ENDIF
		 ENDIF
		 
         CLEAR && Clear for Next Page

		IF XYZ = 'C'		
         	PAGE= PAGE+1
		ENDIF

		IF XYZ = 'P'		
			IF PAGE <> 1
         		PAGE= PAGE-1
			ENDIF
		ENDIF

		
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
      @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to first page.... '
      WAIT ''

		IF mDrTot+mCrTot <> 0

			LOOP

		ENDIF

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
**********************

*************************************************
* Accounts Receivable Report (With Address)		*
*************************************************
PROCEDURE REP_REC1
**********************

SET CONFIRM OFF

HEADING= .T.
PAGE= 1
L= 2
MTOT= 0

SELECT 2
USE ldgrcode ORDER ac1

STORE 0 TO MC1, MC2, MC3, TDR, TCR, MBL_AMOUNT
STORE 0 TO MDRTOT, MCRTOT
STORE 0 TO MNO
STORE ' ' TO MSTATUS
WTITLE= SPACE(30)
CNT= 0
RMK= '             '


IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR

ZEROBAL= ' '

DO WHILE .not. ZEROBAL$'YN'
   @ 4, 2 SAY 'Do You Want To Include Ledger Heads Having Zero Balance - Y/N ' GET ZEROBAL FUNCTION '!'
   READ
ENDDO

CLEAR
ANS= ' '

SELECT 2 && ldgrcode ORDER ac1

DO WHILE .T.

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 8, 14 SAY 'Accounts Receivable Report (With Address)'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 10, 14 SAY 'Please give "First Two Level Code" of Debtors '
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts....'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 67 SAY '-'

IF _DOS
   SET COLOR TO 7/1
ENDIF

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
USE master ORDER mastdate
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
   do PRNCHK in tbl_proc
   CLEAR
   @ 2, 5 SAY 'No Of Copies       ' GET MNO PICTURE '99'
   READ
   CLEAR
   PANS= ' '
   DO prnchk IN tbl_proc.prg
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
   
   IF ZEROBAL='N'
	   SET FILTER TO C1=MC1.and.C2=MC2.and.ldgr_bal<>0
   ELSE
	   SET FILTER TO C1=MC1.and.C2=MC2
   ENDIF
   
   GOTO TOP
   DO WHILE .not. EOF()


      IF HEADING
         IF ANS='S'
            DO screen IN tbl_proc.prg
         ENDIF

         IF ANS='P'
			DO PRNCHK IN tbl_proc.prg
            DO top_prt1 IN tbl_proc.prg
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

         DO WHILE .not. XYZ$'CPQ'
            SET CONFIRM OFF
            @ 23, 45 SAY 'Continue/Previous/Quit - C/P/Q' GET XYZ FUNCTION '!'
            READ
         ENDDO

         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF

		 IF XYZ='P' && Previous Page
		 	IF NOT BOF()

		 		SKIP -8

		 	ENDIF
		 ENDIF


         CLEAR

		IF XYZ = 'C'
	         PAGE= PAGE+1
		ENDIF
		
		IF XYZ = 'P'		
			IF PAGE <> 1
         		PAGE= PAGE-1
			ENDIF
		ENDIF

         L= 2
         HEADING= .T.
      ENDIF

      IF ANS='P'.and.L>=55 && Setting for Printing Next Page
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
      @ 24, 5 SAY 'End of Screen Printing  -  Press any key to return to first page.... '
      WAIT ''

		IF mDrTot+mCrTot <> 0
			LOOP
		ENDIF

   ENDIF

   IF ANS='P' && Eject for next page
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
*************************


*********************************
* Accounts Payable Report		*
*********************************
PROCEDURE REP_PAY
*********************


SET CONFIRM OFF
HEADING= .T.
PAGE= 1
L= 2
MTOT= 0
SELECT 2
USE ldgrcode ORDER ac1
STORE 0 TO MC1, MC2, MC3, TDR, TCR, MBL_AMOUNT, MDRTOT, MCRTOT
STORE 0 TO MNO
STORE ' ' TO MSTATUS
WTITLE= SPACE(30)
CNT= 0
RMK= '             '

IF _DOS
SET COLOR TO 7/1 
ENDIF

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

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 8, 14 SAY 'Accounts Payable Report '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 10, 14 SAY 'Please give "First Two Level Code" of Creditors '
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts....'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 67 SAY '-'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 11, 65 GET MC1 PICTURE '99'
   @ 11, 68 GET MC2 PICTURE '99'
   @ 11, 70 SAY '-xxx'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SELECT 1
USE master ORDER mastdate
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
   do PRNCHK in tbl_proc
   CLEAR
   @ 2, 5 SAY 'No Of Copies       ' GET MNO PICTURE '99'
   READ
   CLEAR
   PANS= ' '
   DO prnchk IN tbl_proc.prg
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

   IF ZEROBAL='N'
	   SET FILTER TO C1=MC1.and.C2=MC2.and.LDGR_BAL <> 0
   ELSE
	   SET FILTER TO C1=MC1.and.C2=MC2
   ENDIF

   GOTO TOP
   STORE C2 TO MWC2
   STORE 0 TO MSCRTOT, MSDRTOT, NXT
   DO WHILE .not. EOF()

      IF HEADING
         IF ANS='S'
            DO screen IN tbl_proc.prg
         ENDIF
         IF ANS='P'
			DO PRNCHK IN tbl_proc.prg
            DO top_prt1 IN tbl_proc.prg
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
         DO WHILE .not. XYZ$'CPQ'
            SET CONFIRM OFF
            @ 23, 45 SAY 'Continue/Previous/Quit - C/P/Q' GET XYZ FUNCTION '!'
            READ
         ENDDO
         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF

		 IF XYZ='P' && Previous Page
		 	IF NOT BOF()

		 		SKIP -28

		 	ENDIF
		 ENDIF



        CLEAR
		
		IF XYZ = 'C'
	         PAGE= PAGE+1
		ENDIF

		IF XYZ = 'P'		
			IF PAGE <> 1
         		PAGE= PAGE-1
			ENDIF
		ENDIF
		
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
      @ 24, 5 SAY 'End of Screen Printing  -  Press any key to return to page 1.... '
      WAIT ''
		
		IF mDrTot+mCrTot <> 0
			LOOP
		ENDIF
		
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
***************************


*********************************************
* Accounts Payable Report (With Address) 	*
*********************************************
PROCEDURE REP_PAY1
***************************

SET CONFIRM OFF

HEADING = .T.
PAGE= 1
L= 2
MTOT= 0

SELECT 2
USE ldgrcode ORDER ac1

STORE 0 TO MC1, MC2, MC3, TDR, TCR, MBL_AMOUNT, MDRTOT, MCRTOT
STORE 0 TO MNO
STORE ' ' TO MSTATUS
WTITLE= SPACE(30)
CNT= 0
RMK= '             '


IF _DOS
SET COLOR TO 7/1 
ENDIF


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

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 8, 14 SAY 'Accounts Payable Report (With Address) '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 10, 14 SAY 'Please give "First Two Level Code" of Creditors '
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts....'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 67 SAY '-'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 11, 65 GET MC1 PICTURE '99'
   @ 11, 68 GET MC2 PICTURE '99'
   @ 11, 70 SAY '-xxx'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO

SELECT 1
USE master ORDER mastdate

GOTO BOTTOM
STORE DATE TO MTDATE

CLEAR
@ 2, 5 SAY 'Accounts Payable Report (With Address) '
@ 5, 5 SAY 'Your last posted voucher is dated '
@ 5, 42 SAY MTDATE
@ 6, 5 SAY 'Do you want to dispaly same date on your Report '
@ 7, 5 SAY 'Or enter the date you want to dispaly  ' GET MTDATE
READ

SELECT 2 && ldgrcode order ac1

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
   do PRNCHK in tbl_proc
   CLEAR
   @ 2, 5 SAY 'No Of Copies       ' GET MNO PICTURE '99'
   READ
   CLEAR
   PANS= ' '
   DO prnchk IN tbl_proc.prg
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
   
   IF ZEROBAL='N'
	   SET FILTER TO C1=MC1.and.C2=MC2.and.ldgr_bal<>0
   ELSE
   	   SET FILTER TO C1=MC1.and.C2=MC2
   ENDIF
   
   GOTO TOP
   STORE C2 TO MWC2
   STORE 0 TO MSCRTOT, MSDRTOT, NXT
   DO WHILE .not. EOF()
   
   
      IF HEADING
         IF ANS='S'
            DO screen IN tbl_proc.prg
         ENDIF
         IF ANS='P'
			DO PRNCHK IN tbl_proc.prg
            DO top_prt1 IN tbl_proc.prg
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

         DO WHILE .not. XYZ$'CPQ'
            SET CONFIRM OFF
            @ 23, 45 SAY 'Continue/Previous/Quit - C/P/Q' GET XYZ FUNCTION '!'
            READ
         ENDDO

         IF XYZ='Q'
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF

		 IF XYZ='P' && Previous Page
		 	IF NOT BOF()

		 		SKIP -8

		 	ENDIF
		 ENDIF

         CLEAR

		IF XYZ = 'C'
	         PAGE= PAGE+1
		ENDIF

		IF XYZ = 'P'		
			IF PAGE <> 1
         		PAGE= PAGE-1
			ENDIF
		ENDIF


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
      @ 24, 5 SAY 'End of Screen Printing  -  Press any key to return to first page .... '
      WAIT ''
		
		IF mDrTot+mCrTot <> 0
			LOOP
		ENDIF
		
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
**************************


******************************************************
*Accounts Payable/Receivable Report (Between Dates)  *
******************************************************
PROCEDURE REPRPAY
***************************

CLEAR
CLOSE ALL


IF _DOS
SET COLOR TO 7/1
ENDIF


CLEAR

SELECT 1

USE master ORDER ac2

SELECT 2
USE ldgrcode ORDER ac1

SET DATE brit

DO WHILE .T.
   CLEAR

   STORE 0 TO MC1, MC2
   STORE 0 TO T1, T2, T3, T4, T5, T6
   STORE DATE() TO FDATE, TDATE


IF _DOS
   SET COLOR TO +7/1
ENDIF


   @ 6, 14 SAY 'Accounts Payable/Receivable Report  '


IF _DOS
   SET COLOR TO 7/1
ENDIF


   @ 8, 14 SAY 'From Date ................. ' GET FDATE
   @ 9, 14 SAY 'To   Date ................. ' GET TDATE
   @ 11, 14 SAY 'Please give "First Two Level Code"  '
   @ 12, 14 SAY 'that you have Defined in your Chart of Accounts....'


IF _DOS
   SET COLOR TO /w
ENDIF


   @ 12, 67 SAY '-'


IF _DOS
   SET COLOR TO 7/1
ENDIF


   @ 12, 65 GET MC1 PICTURE '99'
   @ 12, 68 GET MC2 PICTURE '99'
   @ 12, 70 SAY '-xxx'


IF _DOS
   SET COLOR TO 7/1
ENDIF

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
	  DO PRNCHK IN tbl_proc.prg
      DO top_prt1 IN tbl_proc.prg
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
******************************

* Journal Report
************************
PROCEDURE JOU_REP
************************

CLEAR
CLOSE ALL
SET DELETED ON
SELECT 1
USE tran ORDER tranent
SELECT 2
USE ldgrcode ORDER ac1
SELECT 3
USE tran-rep ORDER rep
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
   STORE SPACE(1) TO XYZ
   @ 23, 1 SAY 'Press Ctrl Q To Quit '
   @ R, 10 SAY 'Enter Code for Reports '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 35 SAY '  -  -   '

IF _DOS
   SET COLOR TO 7/1
ENDIF

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

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 21 SAY MTITLE

IF _DOS
   SET COLOR TO 7/1
ENDIF

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
   do PRNCHK in tbl_proc
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
PAGE= 1

PUBLIC line
line = 2
DO WHILE .not. EOF()
   IF HEAD
      LINE= 2
      IF P='S'
         DO screen IN tbl_proc.prg
         @ LINE, 29 SAY 'J o u r n a l '
      ENDIF
      IF P='P'
		 do PRNCHK in tbl_proc
         DO top_prt1 IN tbl_proc.prg
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


		IF XYZ = 'C'
	      PAGE= PAGE+1
		ENDIF

		IF XYZ = 'P'
			IF NOT BOF()
		      PAGE= PAGE-1
			ENDIF
		ENDIF


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
               DO screen IN tbl_proc.prg
               @ LINE, 29 SAY 'J o u r n a l '
            ENDIF
            IF P='P'
			   do PRNCHK in tbl_proc
               DO top_prt1 IN tbl_proc
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

      		IF XYZ = 'C'
            	PAGE= PAGE+1
            ENDIF
            
			IF XYZ = 'P'
				IF NOT BOF()
			      PAGE= PAGE-1
				ENDIF
			ENDIF


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

            DO WHILE .not. XYZ$'CPQ'
               @ 23, 45 SAY 'Continue/Previous/Quit - C/P/Q' GET XYZ FUNCTION '!'
               READ
            ENDDO

            IF XYZ='Q'
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
*amend
			IF XYZ = 'P'
				SKIP -6
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

      DO WHILE .not. XYZ$'CPQ'
         @ 23, 45 SAY 'Continue/Previous/Quit - C/P/Q' GET XYZ FUNCTION '!'
         READ
      ENDDO

      IF XYZ='Q'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF

			IF XYZ = 'P'
				SKIP -6
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
   @ 24, 5 SAY 'End of Screen Printing  -  Press any key to return to first page.... '
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
*************************


**************************************
* Journal Report					 *
**************************************
PROCEDURE JOU1_REP
***************************

CLEAR
CLOSE ALL
SET DELETED ON
SELECT 1
USE tran ORDER tranent
SELECT 2
USE ldgrcode ORDER ac1
SELECT 3
USE tran-rep ORDER rep
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

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 35 SAY '  -  -   '

IF _DOS
   SET COLOR TO 7/1
ENDIF

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

IF _DOS
   SET COLOR TO /w
ENDIF

   @ R, 21 SAY MTITLE

IF _DOS
   SET COLOR TO 7/1
ENDIF

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
         DO screen IN tbl_proc.prg
         @ LINE, 29 SAY 'J o u r n a l '
      ENDIF
      IF P='P'
		 do PRNCHK in tbl_proc
         DO top_prt1 IN tbl_proc.prg
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
               DO screen IN tbl_proc.prg
               @ LINE, 29 SAY 'J o u r n a l '
            ENDIF
            IF P='P'
			   do PRNCHK in tbl_proc
               DO top_prt1 IN tbl_proc.prg
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
************************



* Aging Of Accounts Receivble
**********************
PROCEDURE REC_AGE
**********************

CLEAR
CLOSE DATA
SET DATE brit
SET DELETED ON
SELECT 3
USE ldgrcode ORDER ac1
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

IF _DOS
      SET COLOR TO /g
ENDIF

      @ R, 15 SAY '  -  -   '

IF _DOS
      SET COLOR TO 
ENDIF

      @ R, 15 GET MC1 PICTURE '@z 99'
      READ
      IF READKEY()=12
         CLOSE ALL
         CLEAR
         RETURN
      ENDIF
      @ 24, 40 CLEAR TO 24, 79

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 18 GET MC2 PICTURE '@z 99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2

IF _DOS
      SET COLOR TO 
ENDIF

      @ R, 21 GET MC3 PICTURE '@z 999'
      READ
      IF MC3=0
         LOOP
      ENDIF
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 21 SAY MPC3

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 3
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF .not. FOUND()
         @ 23, 20 SAY 'This Code Does Not Exists ... Please Re-try'
         WAIT ''
         @ 23, 19 SAY SPACE(55)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 30 SAY TITLE
      MTITLE= TITLE
      MBAL= LDGR_BAL
      MBAL1= LDGR_BAL
      MADD1= ADDRESS1
      MADD2= ADDRESS2
      MTOT= LDGR_BAL

IF _DOS
      SET COLOR TO 
ENDIF

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
         DO PRNCHK IN tbl_proc.prg
         DO top_prt1 IN tbl_proc.prg
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
   @ LINE, 1 SAY 'CosmoSoft Accounting.'
   LINE= LINE+1
   @ LINE, 1 SAY REPLICATE('-',132)
   LINE= LINE+1
   @ LINE, 1 SAY 'Designed & Developed by: CosmoSoft Systems ,  Lahore, Pakistan. Tel: (0300) 4406589  E-Mail:  amjad18@yahoo.com'
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
*************************


* Aging Of Accounts Receivble
**************************
PROCEDURE REC_AGE1
**************************
CLEAR
CLOSE DATA
SET DATE brit
SET DELETED ON
SELECT 3
USE ldgrcode ORDER ac1
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

IF _DOS
      SET COLOR TO /g
ENDIF

      @ R, 15 SAY '  -  -   '

IF _DOS
      SET COLOR TO 
ENDIF

      @ R, 15 GET MC1 PICTURE '@z 99'
      READ
      IF READKEY()=12
         CLOSE ALL
         CLEAR
         RETURN
      ENDIF
      @ 24, 40 CLEAR TO 24, 79

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 18 GET MC2 PICTURE '@z 99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2

IF _DOS
      SET COLOR TO 
ENDIF

      @ R, 21 GET MC3 PICTURE '@z 999'
      READ
      IF MC3=0
         LOOP
      ENDIF
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 21 SAY MPC3

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 3
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF .not. FOUND()
         @ 23, 20 SAY 'This Code Does Not Exists ... Please Re-try'
         WAIT ''
         @ 23, 19 SAY SPACE(55)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 30 SAY TITLE
      MTITLE= TITLE
      MBAL= LDGR_BAL
      MBAL1= LDGR_BAL
      MADD1= ADDRESS1
      MADD2= ADDRESS2
      MTOT= LDGR_BAL

IF _DOS
      SET COLOR TO 
ENDIF

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
         DO PRNCHK IN tbl_proc.prg
         DO top_prt1 IN tbl_proc.prg
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
   @ LINE, 1 SAY 'CosmoSoft Accounting.'
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
***********************


* Aging Of Accounts Receivble
***************************
PROCEDURE REC_AGE2
***************************

CLEAR
CLOSE DATA
SET DATE brit
SET DELETED ON
SELECT 1
USE ldgrcode ORDER ac1
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

IF _DOS
   SET COLOR TO /g
ENDIF

   @ R, 15 SAY '  -  -'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ R, 15 GET MC1 PICTURE '@z 99'
   READ
   IF READKEY()=12
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF
   @ 24, 40 CLEAR TO 24, 79

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
   @ R, 15 SAY MPC1
   @ R, 18 GET MC2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ R, 18 SAY MPC2

IF _DOS
   SET COLOR TO 7/1
ENDIF

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
		 do PRNCHK in tbl_proc
         DO top_prt1 IN tbl_proc.prg
      ELSE
         DO screen IN tbl_proc.prg
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

IF _DOS
   SET COLOR TO 7/1
ENDIF

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
**************************



* Aging Of Accounts Receivble
****************************
PROCEDURE REC_AGE3
****************************

CLEAR
CLOSE DATA
SET DATE brit
SET DELETED ON
SELECT 3
USE ldgrcode ORDER ac1
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

IF _DOS
      SET COLOR TO /g
ENDIF

      @ R, 15 SAY '  -  -   '

IF _DOS
      SET COLOR TO 
ENDIF

      @ R, 15 GET MC1 PICTURE '@z 99'
      READ
      IF READKEY()=12
         CLOSE ALL
         CLEAR
         RETURN
      ENDIF
      @ 24, 40 CLEAR TO 24, 79

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
      @ R, 15 SAY MPC1
      @ R, 18 GET MC2 PICTURE '@z 99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 18 SAY MPC2

IF _DOS
      SET COLOR TO 
ENDIF

      @ R, 21 GET MC3 PICTURE '@z 999'
      READ
      IF MC3=0
         LOOP
      ENDIF
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 21 SAY MPC3

IF _DOS
      SET COLOR TO 
ENDIF

      SELECT 3
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
      IF .not. FOUND()
         @ 23, 20 SAY 'This Code Does Not Exists ... Please Re-try'
         WAIT ''
         @ 23, 19 SAY SPACE(55)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF

      @ R, 30 SAY TITLE
      MTITLE= TITLE
      MBAL= LDGR_BAL
      MBAL1= LDGR_BAL
      MADD1= ADDRESS1
      MADD2= ADDRESS2
      MTOT= LDGR_BAL

IF _DOS
      SET COLOR TO 
ENDIF

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
	  do PRNCHK in tbl_proc
      SET PRINTER ON
      SET DEVICE TO PRINTER
   ENDIF
   LINE= 2
   DO WHILE .not. EOF()
      IF HEAD
         IF P='Q'
            DO PRNCHK IN tbl_proc.prg
            DO top_prt1 IN tbl_proc.prg
         ELSE
            DO SCREEN IN tbl_proc.prg
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
************************


* Transaction Summary Report
*************************
PROCEDURE TR_REP
*************************
CLEAR
CLOSE ALL
SELECT 1
USE ldgrcode ORDER ac1
SELECT 2
USE tran ORDER tr
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
	  do PRNCHK in tbl_proc
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
            DO screen IN tbl_proc.prg
         ENDIF
         IF P='P'
			do PRNCHK in tbl_proc
            DO top_prt1 IN tbl_proc.prg
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
*******************************************
