*************************************
* Programmer: Amjad Izhar
* Revised again on 18-01-2001
* PROGRAM: LPR_PROC.PRG
* Author : Amjad Izhar
* Ledger Printing Menu Procedures
* Written : 17-09-96
* Revised again on 19-01-2001 7:15 PM

**************************************


*Day Book Printing
***********************
PROCEDURE VCH_BOOK
***********************

CLEAR
CLOSE ALL
SET DATE brit
SET CENTURY ON
SELECT 1
USE master
SELECT 2
USE ldgrcode ORDER ac1
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

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

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 12, 19 SAY WTITLE
      MREM= REMARKS

IF _DOS
      SET COLOR TO 7/1
ENDIF

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
      CLOSE DATA
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
	  do PRNCHK in tbl_proc
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
		    do PRNCHK in tbl_proc
            DO top_prt1 IN tbl_proc.prg
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
			   do PRNCHK in tbl_proc
               DO top_prt1 IN tbl_proc.prg
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
CLOSE DATA
CLOSE ALL


RETURN
*********************





* Ledger Display
**************************
PROCEDURE LDGR_SD
**************************

SET DATE brit
SET CENTURY ON


SELECT 1
USE master ORDER ac2

SELECT 2
USE ldgrcode ORDER ac1

P= ' '

DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

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

		SELECT 1 && master.dbf order ac2
		SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
		STORE date TO sdate
		
      IF READKEY()=12
         CLOSE ALL
         CLEAR
         RETURN
      ENDIF

      @ 23, 1
      SELECT 2 && ldgrcode ORDER ac1
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)

      IF .not. FOUND()
         @ 22, 10 SAY 'code is not in code file'
         WAIT ''
         @ 22, 10 SAY '                           '
         LOOP
      ENDIF

      WTITLE=  B->TITLE
      @ 12, 10 SAY 'TITLE:'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 12, 19 SAY WTITLE
      MREM= REMARKS

IF _DOS
      SET COLOR TO 7/1
ENDIF


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

   SELECT 1 && master
   LINE= 8
   @ 11, 20 SAY 'Please Wait Prepaing Ledger ... '

SELECT SUM(dr_amount) AS "mTdr",;
	   SUM(cr_amount) AS "mTcr";
FROM master;
WHERE date>=sdate.and.date<=ldate.and.c1=mc1.and.c2=mc2.and.c3=mc3;
INTO CURSOR sumdrcr

STORE mTdr TO TDR
STORE mTcr TO TCR

USE

SELECT *;
FROM master;
WHERE date>=sdate.and.date<=ldate.and.c1=mc1.and.c2=mc2.and.c3=mc3;
INTO CURSOR master1



SELECT master1


*amend

   COUNT TO cnt
	CNT = CNT-1
	
	GOTO BOTTOM
	STORE BL_AMOUNT TO MBL_AMOUNT
	STORE STATUS TO MSTATUS


   CLEAR

   IF CNT+1=0
      @ 10, 10 SAY 'There is No Transaction Between These Dates'
      @ 24, 25 SAY 'Press Any Key To Return'
      WAIT ''
      @ 10, 10 SAY SPACE(60)
      CLOSE DATA
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

	   SELECT master1

      IF EOF()
         EXIT
      ENDIF

      IF C3=0
         SKIP
         LOOP
      ENDIF


		
      IF SR_NO=1
			DELE FILE OP.MEM
			OPSR = 0
			OPDATE = {  /  /  }
			OPWDESC = 'XXXXXXXXXXXXXXXXXXX'
			OPBAL = 0
			OPST = SPACE(2)
			OPRMK = SPACE(7)			

			STORE SR_NO TO OPSR
         @ LINE, 1 SAY DATE
			STORE DATE TO OPDATE
         WDESC= LTRIM(DESC)
			STORE WDESC TO OPWDESC
         @ LINE, 10 SAY WDESC PICTURE 'XXXXXXXXXXXXXXXXXXX'
         @ LINE, 53 SAY BL_AMOUNT PICTURE '99,999,999,999.99'
			STORE BL_AMOUNT TO OPBAL
         @ LINE, 69 SAY STATUS
			STORE STATUS TO OPST
         RMK= 'Opn.Bal'
			STORE RMK TO OPRMK
         @ LINE, 72 SAY RMK

		SAVE TO OP ALL LIKE OP*

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
                     OPSR = 0
                     OPBAL= BL_AMOUNT-DR_AMOUNT
                     OPST= 'DR'
						DELE FILE OP.MEM
						SAVE TO OP ALL LIKE OP*		
                  ELSE
                     OPSR = 0
                     OPBAL= DR_AMOUNT-BL_AMOUNT
                     OPST= 'CR'
						DELE FILE OP.MEM
						SAVE TO OP ALL LIKE OP*		
                  ENDIF
               ELSE
                  OPSR = 0
                  OPBAL= BL_AMOUNT+DR_AMOUNT
                  OPST= 'CR'
					DELE FILE OP.MEM
					SAVE TO OP ALL LIKE OP*		

               ENDIF
            ENDIF

            IF CR_AMOUNT>0
               IF STATUS='CR'
                  IF CR_AMOUNT<=BL_AMOUNT
                     OPSR = 0
                     OPBAL= BL_AMOUNT-CR_AMOUNT
                     OPST= 'CR'
							DELE FILE OP.MEM
							SAVE TO OP ALL LIKE OP*		
                  ELSE
                     OPSR = 0
                     OPBAL= CR_AMOUNT-BL_AMOUNT
                     OPST= 'DR'
						DELE FILE OP.MEM
						SAVE TO OP ALL LIKE OP*		
                  ENDIF
               ELSE
                  OPSR = 0
                  OPBAL= BL_AMOUNT+CR_AMOUNT
                  OPST= 'DR'
					DELE FILE OP.MEM
					SAVE TO OP ALL LIKE OP*		
               ENDIF
            ENDIF

            @ LINE, 10 SAY 'Opening Balance'
            @ LINE, 53 SAY OPBAL PICTURE '99999999,999.99'
            @ LINE, 69 SAY OPST


         ENDIF
      ENDIF


      MCNT= 1
      LINE= LINE+1



      @ LINE, 1 SAY DATE
      WDESC= LTRIM(DESC)
      @ LINE, 10 SAY WDESC PICTURE 'XXXXXXXXXXXXXXXXXXX'

      IF DR_AMOUNT>0
         @ LINE, 30 SAY DR_AMOUNT PICTURE '999,999,999.99'
      ENDIF


      IF CR_AMOUNT>0
         @ LINE, 35 SAY CR_AMOUNT PICTURE '999,999,999.99'
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

*amend



      IF LINE>18

         LINE= 8
         PG_HDNG= .T.
         P= ' '

		 IF PAGE <> 1
         DO WHILE .not. P$'CPQ'
            SET CONFIRM OFF
            @ 24, 40 SAY 'Continue/Previous/Quit  -  C/P/Q  ' GET P FUNCTION '!'
            READ
         ENDDO
		 ENDIF

		 IF PAGE = 1
         DO WHILE .not. P$'CQ'
            SET CONFIRM OFF
            @ 24, 40 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
            READ
         ENDDO
		 ENDIF


		IF P='C'

	         PAGE= PAGE+1
			 
		ENDIF
		

		IF P='P'
			IF PAGE <> 1
	         	PAGE= PAGE-1
			ENDIF 
		ENDIF


         IF P='Q'
            CLEAR
            EXIT

         ENDIF

*amend
		IF P='P'
			IF NOT BOF()
				SKIP -22
			ENDIF
		ENDIF


         @ 8, 0 CLEAR
*op
		IF PAGE = 1
			RESTORE FROM OP ADDITIVE

			IF OPSR <> 1            
            @ 8, 10 SAY 'Opening Balance'
            @ 8, 53 SAY OPBAL PICTURE '99999999,999.99'
            @ 8, 69 SAY OPST
			ENDIF

			IF OPSR = 1
				@ 8, 1 SAY OPDATE			
		        @ 8, 10 SAY OPWDESC PICTURE 'XXXXXXXXXXXXXXXXXXX'
        		@ 8, 53 SAY OPBAL PICTURE '99,999,999,999.99'
				@ 8, 69 SAY OPST			
         		@ 8, 72 SAY OPRMK				
			
			ENDIF


		ENDIF

      ELSE
         PG_HDNG= .F.
      ENDIF

      SKIP

   IF EOF()

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

		PAGE = 1
		LINE = 8
		@ 8,0 CLEAR
         @ 2, 70 SAY 'page: '
         @ 2, 76 SAY PAGE PICTURE '99'

*		GOTO TOP

* HERE IT HELDS IN ON PAGE 19-01-2001 AMENDMENT 8:15 PM I STARED GOTO TOP

   ENDIF


   ENDDO		

   LINE = 12
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

   IF READKEY()=12
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF



ENDDO

CLEAR
CLOSE DATA
CLOSE ALL

RETURN
******************************


* Ledger Display - 2
******************************
PROCEDURE LDGR_SD2
******************************

SET DATE brit
SET CENTURY ON

SELECT 1
USE master ORDER ac2

SELECT 2
USE ldgrcode ORDER ac1

P= ' '

DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

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
      STORE DATE() TO LDATE

      @ 1, 10 SAY 'For Accurate Result Please Sort Ledger Date Wise Option 6.5'
      @ 23, 1 SAY 'Press Esc To Return '
      @ 10, 10 SAY 'Enter  Code  for  Display:     -  -'
      @ 10, 40 GET MC1 PICTURE '99'
      @ 10, 43 GET MC2 PICTURE '99'
      @ 10, 46 GET MC3 PICTURE '999'
      READ
			SELECT 1 && master.dbf order ac2
		    SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
			STORE date TO sdate

      IF READKEY()=12
         CLOSE ALL
         CLEAR
         RETURN
      ENDIF
      @ 23, 1

      SELECT 2 && ldgrcode order ac1
      SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)

      IF .not. FOUND()
         @ 22, 10 SAY 'code is not in code file'
         WAIT ''
         @ 22, 10 SAY '                           '
         LOOP
      ENDIF

      WTITLE=  B->TITLE
      @ 12, 10 SAY 'TITLE:'

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 12, 19 SAY WTITLE
      MREM= REMARKS

IF _DOS
      SET COLOR TO 7/1
ENDIF


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

SELECT SUM(dr_amount) AS "mTdr",;
	   SUM(cr_amount) AS "mTcr";
FROM master;
WHERE date>=sdate.and.date<=ldate.and.c1=mc1.and.c2=mc2.and.c3=mc3;
INTO CURSOR sumdrcr

STORE mTdr TO TDR
STORE mTcr TO TCR

USE

SELECT *;
FROM master;
WHERE date>=sdate.and.date<=ldate.and.c1=mc1.and.c2=mc2.and.c3=mc3;
INTO CURSOR master1


SELECT master1


*amend

   COUNT TO cnt
	CNT = CNT-1
	
	GOTO BOTTOM
	STORE BL_AMOUNT TO MBL_AMOUNT
	STORE STATUS TO MSTATUS


   CLEAR

   IF CNT+1=0
      @ 10, 10 SAY 'There is No Transaction Between These Dates'
      @ 24, 25 SAY 'Press Any Key To Return'
      WAIT ''
      @ 10, 10 SAY SPACE(60)
      CLOSE DATA
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

      SELECT master1

      IF EOF()
         EXIT
      ENDIF

      IF C3=0
         SKIP
         LOOP
      ENDIF



      IF SR_NO=1
			DELE FILE OP.MEM
			OPSR = 0
			OPDATE = {  /  /  }
			OPWDESC = 'XXXXXXXXXXXXXXXXXXX'
			OPBAL = 0
			OPST = SPACE(2)
			OPRMK = SPACE(7)			

			STORE SR_NO TO OPSR
         @ LINE, 1 SAY DATE
			STORE DATE TO OPDATE
         WDESC= LTRIM(DESC)
			STORE WDESC TO OPWDESC
         @ LINE, 10 SAY WDESC PICTURE 'XXXXXXXXXXXXXXXXXXX'
         @ LINE, 53 SAY BL_AMOUNT PICTURE '99,999,999,999.99'
			STORE BL_AMOUNT TO OPBAL
         @ LINE, 69 SAY STATUS
			STORE STATUS TO OPST
         RMK= 'Opn.Bal'
			STORE RMK TO OPRMK
         @ LINE, 72 SAY RMK

		SAVE TO OP ALL LIKE OP*

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
                     OPSR = 0
                     OPBAL= BL_AMOUNT-DR_AMOUNT
                     OPST= 'DR'
						DELE FILE OP.MEM
						SAVE TO OP ALL LIKE OP*		
                  ELSE
                     OPSR = 0
                     OPBAL= DR_AMOUNT-BL_AMOUNT
                     OPST= 'CR'
						DELE FILE OP.MEM
						SAVE TO OP ALL LIKE OP*		
                  ENDIF
               ELSE
                  OPSR = 0
                  OPBAL= BL_AMOUNT+DR_AMOUNT
                  OPST= 'CR'
					DELE FILE OP.MEM
					SAVE TO OP ALL LIKE OP*		

               ENDIF
            ENDIF

            IF CR_AMOUNT>0
               IF STATUS='CR'
                  IF CR_AMOUNT<=BL_AMOUNT
                     OPSR = 0
                     OPBAL= BL_AMOUNT-CR_AMOUNT
                     OPST= 'CR'
							DELE FILE OP.MEM
							SAVE TO OP ALL LIKE OP*		
                  ELSE
                     OPSR = 0
                     OPBAL= CR_AMOUNT-BL_AMOUNT
                     OPST= 'DR'
						DELE FILE OP.MEM
						SAVE TO OP ALL LIKE OP*		
                  ENDIF
               ELSE
                  OPSR = 0
                  OPBAL= BL_AMOUNT+CR_AMOUNT
                  OPST= 'DR'
					DELE FILE OP.MEM
					SAVE TO OP ALL LIKE OP*		
               ENDIF
            ENDIF

            @ LINE, 10 SAY 'Opening Balance'
            @ LINE, 53 SAY OPBAL PICTURE '99999999,999.99'
            @ LINE, 69 SAY OPST

*            LINE= LINE+1
         ENDIF
      ENDIF
 
 
      MCNT= 1
      LINE= LINE+1



      @ LINE, 1 SAY DATE
      WDESC= LTRIM(DESC)
      @ LINE, 10 SAY WDESC PICTURE 'XXXXXXXXXXXXXXXXXXX'

      IF DR_AMOUNT>0
         @ LINE, 30 SAY DR_AMOUNT PICTURE '999,999,999.99'
      ENDIF


      IF CR_AMOUNT>0
         @ LINE, 35 SAY CR_AMOUNT PICTURE '999,999,999.99'
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

*amend
      LINE= LINE+1

      @ LINE, 10 SAY DESC1 PICTURE 'XXXXXXXXXXXXXXXXXXX'

      IF LINE>18

         LINE= 8
         PG_HDNG= .T.
         P= ' '

		 IF PAGE <> 1
         DO WHILE .not. P$'CPQ'
            SET CONFIRM OFF
            @ 24, 40 SAY 'Continue/Previous/Quit  -  C/P/Q  ' GET P FUNCTION '!'
            READ
         ENDDO
		 ENDIF

		 IF PAGE = 1
         DO WHILE .not. P$'CQ'
            SET CONFIRM OFF
            @ 24, 45 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
            READ
         ENDDO
		 ENDIF


		IF P='C'

	         PAGE= PAGE+1
			 
		ENDIF
		

		IF P='P'
			IF PAGE <> 1
	         	PAGE= PAGE-1
			ENDIF 
		ENDIF


         IF P='Q'
            CLEAR
            EXIT

         ENDIF

*amend2
		IF P='P'
			IF NOT BOF()
				SKIP -12
			ENDIF
		ENDIF


         @ 8, 0 CLEAR
*op
		IF PAGE = 1
			RESTORE FROM OP ADDITIVE

			IF OPSR <> 1            
            @ 8, 10 SAY 'Opening Balance'
            @ 8, 53 SAY OPBAL PICTURE '99999999,999.99'
            @ 8, 69 SAY OPST
			ENDIF

			IF OPSR = 1
				@ 8, 1 SAY OPDATE			
		        @ 8, 10 SAY OPWDESC PICTURE 'XXXXXXXXXXXXXXXXXXX'
        		@ 8, 53 SAY OPBAL PICTURE '99,999,999,999.99'
				@ 8, 69 SAY OPST			
         		@ 8, 72 SAY OPRMK				
			
			ENDIF


		ENDIF

      ELSE
         PG_HDNG= .F.
      ENDIF

      SKIP

IF EOF()

   LINE= LINE+2

   @ LINE, 1 SAY 'Total Transactions = '
   @ LINE, 20 SAY CNT PICTURE '99999'
   @ LINE, 25 SAY TDR PICTURE '999999,999.99'
   @ LINE, 35 SAY TCR PICTURE '999999,999.99'
   @ LINE, 50 SAY MBL_AMOUNT PICTURE '99999999,999.99'
   @ LINE, 66 SAY MSTATUS

* Amendment on 19-01-2001 for only one page display quiting

		 IF LINE < 18 AND PAGE = 1
         DO WHILE .not. P$'CQ'
            SET CONFIRM OFF
            @ 24, 40 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
            READ
         ENDDO
		 ENDIF

         IF P='Q'
            CLEAR
            EXIT

         ENDIF


* Amendment on 19-01-2001 -- ends here--



   RMK= 'Cls.Bal.'
   @ LINE, 72 SAY RMK
   LINE= LINE+2
   @ LINE, 5 SAY 'R e m a r k s     -  '
   @ LINE, 25 SAY MREM
   @ 24, 22 SAY 'Press Any Key To Return '
   WAIT ''
		PAGE = 1
		LINE = 8
		@ 8,0 CLEAR
		@ 2,70 SAY 'page: '
        @ 2, 76 SAY PAGE PICTURE '99'
		GOTO TOP
	
	ENDIF
	
	
	ENDDO

   LINE = 12
   @ LINE, 1 SAY 'Total Transactions = '
   @ LINE, 20 SAY CNT PICTURE '99999'
   @ LINE, 25 SAY TDR PICTURE '999999,999.99'
   @ LINE, 35 SAY TCR PICTURE '999999,999.99'
   @ LINE, 50 SAY MBL_AMOUNT PICTURE '99999999,999.99'
   @ LINE, 66 SAY MSTATUS
* Amendment on 19-01-2001 for only one page display quiting

		 IF LINE < 18 AND PAGE = 1
         DO WHILE .not. P$'CQ'
            SET CONFIRM OFF
            @ 24, 40 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
            READ
         ENDDO
		 ENDIF

         IF P='Q'
            CLEAR
            EXIT

         ENDIF


* Amendment on 19-01-2001 -- ends here--



   RMK= 'Cls.Bal.'
   @ LINE, 72 SAY RMK
   LINE= LINE+2
   @ LINE, 5 SAY 'R e m a r k s     -  '
   @ LINE, 25 SAY MREM
* Amendment on 19-01-2001 for only one page display quiting

		 IF LINE < 18 AND PAGE = 1
         DO WHILE .not. P$'CQ'
            SET CONFIRM OFF
            @ 24, 40 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
            READ
         ENDDO
		 ENDIF

         IF P='Q'
            CLEAR
            EXIT

         ENDIF


* Amendment on 19-01-2001 -- ends here--


   @ 24, 22 SAY 'Press Any Key To Return '
   WAIT ''



ENDDO

CLEAR
CLOSE DATA
CLOSE ALL

RETURN
******************************


* FOR WINDOWS AMENDMENTS 
* ON MONDAY, 20TH JUNE,2005
* This revision : 20th June,2005 at 5:50 pm
******************************
PROCEDURE LDGR_SP
******************************
* Ledger Selective Printing

CLEAR
CLOSE ALL
SET DATE brit
SET CENTURY on

SET DEVICE TO SCREEN

SELECT 1
USE master

SELECT 2
USE ldgrcode ORDER ac1

STORE SPACE(65) TO MREM1, MREM2


DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

SET DEVICE TO SCREEN
   
   CLEAR
	
	SET DEVICE TO SCREEN
   
   STORE 0 TO TCR, TDR, MBLAMT, MCSTAT
   
   @ 2, 2 SAY 'Selective Ledger Printing '
   
   STORE DATE() TO SDATE, LDATE
   
   @ 23, 50 SAY 'Press Esc to Return '
   
   STORE 0 TO MC1, MC2, MC3, TDR, TCR, MNO
   @ 6, 10 SAY 'Enter  Code  for  Printing:'

IF _DOS
   SET COLOR TO /w
ENDIF

IF _DOS
   @ 6, 40 SAY '  -  -'
ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF


IF _DOS
   @ 6, 40 GET MC1 PICTURE '99'
   @ 6, 43 GET MC2 PICTURE '99'
   @ 6, 46 GET MC3 PICTURE '999'
   READ
ENDIF


IF _WINDOWS
   @ 6, 40 GET MC1 PICTURE '99' FONT "Arial",11 SIZE 1,3
	@ 6,46 SAY '-'
   @ 6, 48 GET MC2 PICTURE '99' FONT "Arial",11 SIZE 1,3
    @ 6,52 SAY '-'
   @ 6, 55 GET MC3 PICTURE '999' FONT "Arial",11 SIZE 1,4
   READ
ENDIF


   IF READKEY()=12
      SET PRINTER OFF
      SET DEVICE TO SCREEN
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF

	SET DEVICE TO SCREEN
   
   IF _DOS
   @ 23, 1 CLEAR TO 23, 79
   ENDIF
   
   IF _WINDOWS
   @ 23, 1 CLEAR TO 23, 120
   ENDIF

   
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

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 8, 19 SAY WTITLE
   MREM= REMARKS

IF _DOS
   SET COLOR TO 7/1
ENDIF

   
IF _DOS
   @ 10, 10 SAY 'From Date ' GET SDATE
   @ 12, 10 SAY 'To.. Date ' GET LDATE
   READ
ENDIF

IF _WINDOWS
   @ 10, 10 SAY 'From Date ' GET SDATE ;
   	FONT "Arial",11 SIZE 1,12
   	
   @ 12, 10 SAY 'To.. Date ' GET LDATE ;
   	FONT "Arial",11 SIZE 1,12
   
   READ
ENDIF


IF _DOS
   @ 13, 10 SAY 'R e m a r k s   '
   @ 14, 10 GET MREM1
   @ 15, 10 GET MREM2
   READ
ENDIF

IF _WINDOWS
   @ 13, 10 SAY 'R e m a r k s   ' ;
   	FONT "Times New Roman",10 
   @ 14, 10 GET MREM1 ;
   	FONT "Times New Roman",10
   @ 15, 10 GET MREM2 ;
   	FONT "Times New Roman",10
   READ
ENDIF


   OPTN= ' '

	SET DEVICE TO SCREEN

IF _DOS	
   @ 16, 1 TO 16, 79
ENDIF

IF _WINDOWS	
   @ 16, 1 TO 16, 129
ENDIF


IF _DOS
   @ 17, 25 SAY 'Y O U R   P A P E R   W I D T H'
   @ 19, 25 SAY '      A -  10 Inches  '
   @ 20, 25 SAY '      B -  15 Inches  '
   @ 21, 25 SAY '      Q -  Quit       '
ENDIF


IF _WINDOWS

   @ 17, 25 SAY 'Y O U R   P A P E R   W I D T H' ;
   	FONT "Times New Roman",10
   
   @ 19, 25 SAY '      A -  10 Inches  ' ;
   	FONT "Times New Roman",10
   
   @ 20, 25 SAY '      B -  15 Inches  ' ;
   	FONT "Times New Roman",10
   
   @ 21, 25 SAY '      Q -  Quit       ' ;
   	FONT "Times New Roman",10

ENDIF



IF _DOS
   DO WHILE .not. OPTN$'ABQ'
      @ 23, 31 SAY 'Your Choice  (A/B/Q)' GET OPTN FUNCTION '!'
      READ
   ENDDO
ENDIF

IF _WINDOWS
   DO WHILE .not. OPTN$'ABQ'
      @ 23, 31 SAY 'Your Choice  (A/B/Q)' GET OPTN FUNCTION '!';
      	FONT "Times New Roman",10
      READ
   ENDDO
ENDIF


   IF OPTN='Q'
    	SET PRINTER OFF
    	SET DEVICE TO SCREEN
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

	SET PRINTER OFF
	SET DEVICE TO SCREEN
   

IF _DOS
   @ 11, 20 SAY 'Please wait ... Printing In Progress '
ENDIF


IF _WINDOWS
   @ 11, 20 SAY 'Please wait ... Printing In Progress ' ;
   FONT "Times New Roman",14;
   STYLE "BI"
ENDIF


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
   
      CLOSE DATA
      EXIT
   
   ENDIF

   GOTO TOP

   LINE= 10

   MCNT= 0


IF _DOS
   do PRNCHK in tbl_proc
ENDIF
		   	SET PRINTER ON
   			SET DEVICE TO PRINTER


   DO WHILE .not. EOF()



      IF HEAD
   
         LINE= 10
   
         IF OPTN='A'
			
			IF _DOS
			do PRNCHK in tbl_proc
			ENDIF
		   	
		   	SET PRINTER ON
   			SET DEVICE TO PRINTER

            DO top_prt1 IN tbl_proc.prg
         
         ELSE
            DO top_prt2 IN tbl_proc.prg
         
         ENDIF
         
         
         IF _DOS
         	IF OPTN='A'
            	@ 1, 1 SAY CHR(15)
         	ELSE
            	@ 1, 1 SAY CHR(18)
         	ENDIF
         ENDIF

	
		IF _DOS
         @ 2, 40 SAY '        LEDGER FROM : '
         @ 2, 65 SAY SDATE
         @ 2, 75 SAY 'To'
         @ 2, 79 SAY LDATE
         @ 2, 115 SAY 'page: '
         @ 2, 122 SAY PAGE PICTURE '99'
         @ 3, 1 SAY REPLICATE('-',129)
         @ 4, 10 SAY 'ACCOUNT CODE: '
		ENDIF


		IF _WINDOWS
         @ 2, 5 SAY 'LEDGER FROM : ' ;
         	FONT "QuickType Condensed",10
         
         @ 2, 18 SAY SDATE ;
         FONT "QuickType Condensed",10
         
         @ 2, 28 SAY 'To' ;
         	FONT "QuickType Condensed",10
         
         @ 2, 33 SAY LDATE ;
         	FONT "QuickType Condensed",10
         
         @ 2, 58 SAY 'P a g e : ' ;
         	FONT "QuickType Condensed",10
         
         @ 2, 70 SAY PAGE PICTURE '99' ;
         	FONT "QuickType Condensed",10 SIZE 1,3
         
         @ 3, 1 SAY REPLICATE('-',130) ;
         	FONT "QuickType Condensed",10
         
         @ 4, 5 SAY 'ACCOUNT CODE: ' ;
         	FONT "QuickType Condensed",10
		
		ENDIF

		
		
		IF _DOS
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
		ENDIF


		
		
		IF _WINDOWS
        
         WPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         
         @ 4, 15 SAY WPC1 ;
         	FONT "QuickType Condensed",10 ;
         	SIZE 1,3
         
         @ 4, 19 SAY '-' ;
         	FONT "QuickType Condensed",10
         
         WPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         
         @ 4, 21 SAY WPC2 ;
         	FONT "QuickType Condensed",10 SIZE 1,3
         
         @ 4, 24 SAY '-' ;
         	FONT "QuickType Condensed",10
         
         WPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         
         @ 4, 26 SAY WPC3 ;
         	FONT "QuickType Condensed",10 ;
         	SIZE 1,3
         
         @ 4, 32 SAY 'TITLE OF ACCOUNT:- ' ;
         	FONT "QuickType Condensed",10 
         
         @ 4, 52 SAY WTITLE ;
         	FONT "QuickType Condensed",10 
         
         @ 5, 1 SAY REPLICATE('-',130) ;
         	FONT "QuickType Condensed",10
         
         @ 6, 1 SAY '<----- VOUCHER ----->' ;
         	FONT "QuickType Condensed",10 
         
         @ 6, 40 SAY '<----- TRANSACTIONS ----->' ;
         	FONT "QuickType Condensed",10
         
         @ 7, 1 SAY '  DATE            NO.         DESCRIPTION' ;
         	FONT "QuickType Condensed",10
         
         @ 7, 35 SAY 'DEBIT      CREDIT       BALANCE   REMARKS' ;
         	FONT "QuickType Condensed",10
         
         @ 8, 1 SAY REPLICATE('=',75) ;
         	FONT "QuickType Condensed",10
		
		ENDIF

        
        
         HEAD= .F.
      
      
      ENDIF


      SELECT 1

      IF SR_NO=1

		IF _DOS
         @ LINE, 1 SAY DATE
         @ LINE, 22 SAY DESC
         @ LINE, 100 SAY BL_AMOUNT PICTURE '9,999,999,999'
         @ LINE, 117 SAY STATUS
         RMK= ' Opn. Bal.'
         @ LINE, 119 SAY RMK
		ENDIF
		

		IF _WINDOWS
         
         @ LINE, 1 SAY DATE ;
         	FONT "QuickType Condensed",10;
         	SIZE 1,10
         
         *@ LINE, 22 SAY DESC FONT "QuickType Condensed",10
         
         @ LINE, 50 SAY BL_AMOUNT PICTURE '9,999,999,999' ;
         	FONT "QuickType Condensed",10 ;
         	SIZE 1,17
         
         @ LINE, 65 SAY STATUS ;
         FONT "QuickType Condensed",10;
         SIZE 1,3
         
         RMK= 'Opn. Bal.'
         
         @ LINE, 70 SAY RMK ;
         	FONT "QuickType Condensed",10
		
		ENDIF

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

			IF _DOS
            @ LINE, 25 SAY 'Opening Balance'
            @ LINE, 100 SAY OPBAL PICTURE '9,999,999,999.99'
            @ LINE, 117 SAY OPST
            @ LINE, 120 SAY ' Opn. Bal.'
			ENDIF


		IF _WINDOWS
            
            @ LINE, 15 SAY 'Opening Balance' ;
            	FONT "QuickType Condensed",10
            
            @ LINE, 45 SAY OPBAL PICTURE '9,999,999,999' ;
            	FONT "QuickType Condensed",10 SIZE 1,16
            
            @ LINE, 55 SAY OPST ;
            	FONT "QuickType Condensed",10
            
            @ LINE, 70 SAY ' Opn. Bal.' ;
            	FONT "QuickType Condensed",10
			
		ENDIF


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

IF _DOS
      @ LINE, 1 SAY DATE
ENDIF

IF _WINDOWS
      @ LINE, 1 SAY DATE ;
      FONT "QuickType Condensed",10;
      SIZE 1,10
ENDIF

      MVCH1= '  '
      MVCH2= '    '
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))

	IF _DOS
      @ LINE, 12 SAY T
      @ LINE, 15 SAY MVCH1
      @ LINE, 17 SAY '-'
      @ LINE, 18 SAY MVCH2
      @ LINE, 25 SAY DESC
	ENDIF


	IF _WINDOWS
    
      @ LINE, 11 SAY T ;
      	FONT "QuickType Condensed",10
      
      @ LINE, 14 SAY MVCH1 ;
      	FONT "QuickType Condensed",10 SIZE 1,3
      
      @ LINE, 16 SAY '-' ;
      	FONT "QuickType Condensed",10
      
      @ LINE, 18 SAY MVCH2 ;
      	FONT "QuickType Condensed",10 SIZE 1,6
      
      *@ LINE, 22 SAY DESC FONT "QuickType Condensed",10
	
	ENDIF


	IF _DOS
      
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
	
	ENDIF


	IF _WINDOWS
    
      IF DR_AMOUNT<>0
         
         @ LINE, 40 SAY DR_AMOUNT PICTURE '9,999,999,999' ;
         	FONT "QuickType Condensed",10 ;
         	SIZE 1,14
         	
         TDR= TDR+DR_AMOUNT
      
      ENDIF
    
      IF CR_AMOUNT<>0
      
         @ LINE, 55 SAY CR_AMOUNT PICTURE '9,999,999,999' ;
         	FONT "QuickType Condensed",10 ;
         	SIZE 1,14
         	
         TCR= TCR+CR_AMOUNT
      
      ENDIF

      @ LINE, 70 SAY BL_AMOUNT PICTURE '9,999,999,999' ;
      FONT "QuickType Condensed",10 ;
      SIZE 1,14
      
      @ LINE, 75 SAY STATUS FONT "QuickType Condensed",10 
	
	ENDIF

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

IF _DOS
   @ LINE, 5 SAY 'Total Transactions = '
   @ LINE, 25 SAY CNT PICTURE '99999'
   @ LINE, 70 SAY TDR PICTURE '9,999,999,999.99'
   @ LINE, 84 SAY TCR PICTURE '9,999,999,999.99'
   @ LINE, 100 SAY MBLAMT PICTURE '9,999,999,999.99'
   @ LINE, 117 SAY MCSTAT
ENDIF


IF _WINDOWS

   @ LINE, 1 SAY 'Total Transactions = ' ;
   	FONT "QuickType Condensed",11

   @ LINE, 15 SAY CNT PICTURE '99999' ;
   	FONT "QuickType Condensed",11 ;
   	SIZE 1,6

   @ LINE, 40 SAY TDR PICTURE '9,999,999,999' ;
   	FONT "QuickType Condensed",11 ;
   	SIZE 1,14

   @ LINE, 55 SAY TCR PICTURE '9,999,999,999' ;
   FONT "QuickType Condensed",11 ;
   SIZE 1,14
   
   @ LINE, 68 SAY MBLAMT PICTURE '9,999,999,999' ;
   FONT "QuickType Condensed",11 ;
   SIZE 1,14
   
   @ LINE, 80 SAY MCSTAT ;
   FONT "QuickType Condensed",11

ENDIF


* Amendment on 19-01-2001 for only one page display quiting

		 *IF LINE < 18 AND PAGE = 1
	         *DO WHILE .not. P$'CQ'
    	     *   SET CONFIRM OFF
        	 *   @ 24, 40 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
            *	READ
         	*ENDDO
		 *ENDIF

         *IF P='Q'
          *  CLEAR
           * EXIT

         *ENDIF


* Amendment on 19-01-2001 -- ends here--




IF _DOS
   RMK= ' Cls. Bal.'
   @ LINE, 119 SAY RMK
   LINE= LINE+2
   @ LINE, 5 SAY 'R e m a r k s     -  '
   @ LINE, 25 SAY MREM1
   LINE= LINE+1
   @ LINE, 25 SAY MREM2
ENDIF
   

IF _WINDOWS

   RMK= ' Cls. Bal.' 

   @ LINE, 68 SAY RMK ;
	   FONT "QuickType Condensed",10
   
   LINE= LINE+2
   
   @ LINE, 5 SAY 'R e m a r k s     -  ' ;
   	FONT "QuickType Condensed",10
   
   @ LINE, 25 SAY MREM1 ;
   	FONT "QuickType Condensed",10
   
   LINE= LINE+1
   
   @ LINE, 25 SAY MREM2 ;
   	FONT "QuickType Condensed",10

ENDIF


IF _DOS   
   EJECT
ENDIF
   
   SET PRINTER OFF
   SET DEVICE TO SCREEN

ENDDO

   SET PRINTER OFF
   SET DEVICE TO SCREEN


CLEAR
CLOSE DATA

RETURN
*****************************



******************************
PROCEDURE LDGR_SP2
******************************

CLEAR
CLOSE ALL
SET DATE brit
SET CENTURY ON
SELECT 1
USE master
SELECT 2
USE ldgrcode ORDER ac1
STORE SPACE(65) TO MREM1, MREM2
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO TCR, TDR, MBLAMT, MCSTAT
   @ 2, 2 SAY 'Selective Ledger Printing '
   STORE DATE() TO SDATE, LDATE
   @ 23, 50 SAY 'Press Esc to Return '
   STORE 0 TO MC1, MC2, MC3, TDR, TCR, MNO
   @ 6, 10 SAY 'Enter  Code  for  Printing:'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 6, 40 SAY '  -  -'

IF _DOS
   SET COLOR TO 7/1
ENDIF

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

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 8, 19 SAY WTITLE
   MREM= REMARKS

IF _DOS
   SET COLOR TO 7/1
ENDIF

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
      CLOSE DATA
      EXIT
   ENDIF
   GOTO TOP
   LINE= 10
   MCNT= 0

   do PRNCHK in tbl_proc
   DO WHILE .not. EOF()
      IF HEAD
         LINE= 10
         IF OPTN='A'
			do PRNCHK in tbl_proc
            DO top_prt1 IN tbl_proc.prg
         ELSE
            DO top_prt2 IN tbl_proc.prg
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
* Amendment on 19-01-2001 for only one page display quiting

		 IF LINE < 18 AND PAGE = 1
         DO WHILE .not. P$'CQ'
            SET CONFIRM OFF
            @ 24, 40 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
            READ
         ENDDO
		 ENDIF

         IF P='Q'
            CLEAR
            EXIT

         ENDIF


* Amendment on 19-01-2001 -- ends here--


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
CLOSE DATA

RETURN
********************************




********************************
PROCEDURE LDGR_FP
********************************

CLEAR
CLOSE ALL
SET DATE brit
SELECT 3
USE master ORDER ac2
SELECT 2
USE ldgrcode ORDER ac1
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

IF _DOS
   SET COLOR TO 7/1 
ENDIF

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

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 28 SAY MCC1 PICTURE '99'
   @ 12, 31 SAY MCC2 PICTURE '99'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

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
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG ldgr ADDITIVE
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
DO prnchk IN tbl_proc.prg
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
         DO PRNCHK IN tbl_proc.prg
         DO top_prt1 IN tbl_proc.prg
      ELSE
         DO PRNCHK IN tbl_proc.prg
         DO top_prt2 IN tbl_proc.prg
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

* Amendment on 19-01-2001 for only one page display quiting

		 IF LINE < 18 AND PAGE = 1
         DO WHILE .not. P$'CQ'
            SET CONFIRM OFF
            @ 24, 40 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
            READ
         ENDDO
		 ENDIF

         IF P='Q'
            CLEAR
            EXIT

         ENDIF


* Amendment on 19-01-2001 -- ends here--


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
CLOSE DATA

RETURN
***************************


***************************
PROCEDURE LDGR_FP2
***************************

CLEAR
CLOSE ALL
SET DATE brit
SELECT 3
USE master ORDER ac2
SELECT 2
USE ldgrcode ORDER ac1
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

IF _DOS
   SET COLOR TO 7/1 
ENDIF

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

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 28 SAY MCC1 PICTURE '99'
   @ 12, 31 SAY MCC2 PICTURE '99'

IF _DOS
   SET COLOR TO 7/1 
ENDIF

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
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG ldgr ADDITIVE
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
DO prnchk IN tbl_proc.prg
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
         DO PRNCHK IN tbl_proc.prg
         DO top_prt1 IN tbl_proc.prg
      ELSE
         DO PRNCHK IN tbl_proc.prg
         DO top_prt2 IN tbl_proc.prg
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
* Amendment on 19-01-2001 for only one page display quiting

		 IF LINE < 18 AND PAGE = 1
         DO WHILE .not. P$'CQ'
            SET CONFIRM OFF
            @ 24, 40 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
            READ
         ENDDO
		 ENDIF

         IF P='Q'
            CLEAR
            EXIT

         ENDIF


* Amendment on 19-01-2001 -- ends here--


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
CLOSE DATA

RETURN
*****************************

************************
PROCEDURE LDGR_SDB
*************************

CLEAR
SET CENTURY ON
SELECT 1
USE master ORDER ac2
SELECT 2
USE ldgrcode ORDER ac1
STORE 0 TO MC1, MC2, MC3
PG_HDNG= .T.
PAGE= 1
LINE= 8
WTITLE= SPACE(30)
GOTO TOP
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

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

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 19 SAY WTITLE

IF _DOS
   SET COLOR TO 7/1
ENDIF

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
      DO screen IN tbl_proc.prg
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
*************


******************
PROCEDURE LDGRASP
******************

CLEAR
CLOSE ALL
SET DATE brit
SET CENTURY ON
SELECT 1
USE master
SELECT 2
USE ldgrcode ORDER ac1
STORE SPACE(65) TO MREM1, MREM2
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO TCR, TDR, MBLAMT, MCSTAT
   @ 2, 2 SAY 'Selective Ledger (With Address) Printing '
   STORE DATE() TO SDATE, LDATE
   @ 23, 50 SAY 'Press Esc to Return '
   STORE 0 TO MC1, MC2, MC3, TDR, TCR, MNO
   @ 6, 10 SAY 'Enter  Code  for  Printing:'

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 6, 40 SAY '  -  -'

IF _DOS
   SET COLOR TO 7/1
ENDIF

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

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 8, 19 SAY WTITLE

IF _DOS
   SET COLOR TO 7/1
ENDIF

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
      CLOSE DATA
      EXIT
   ENDIF
   GOTO TOP
   MCNT= 0
   do PRNCHK in tbl_proc
   DO WHILE .not. EOF()
      IF HEAD
         LINE= 11
         IF OPTN='A'
			do PRNCHK in tbl_proc
            DO top_prt1 IN tbl_proc.prg
         ELSE
            DO PRNCHK IN tbl_proc.prg
            DO top_prt2 IN tbl_proc.prg
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

* Amendment on 19-01-2001 for only one page display quiting

		 IF LINE < 18 AND PAGE = 1
         DO WHILE .not. P$'CQ'
            SET CONFIRM OFF
            @ 24, 40 SAY 'Continue/Quit  -  C/Q  ' GET P FUNCTION '!'
            READ
         ENDDO
		 ENDIF

         IF P='Q'
            CLEAR
            EXIT

         ENDIF


* Amendment on 19-01-2001 -- ends here--


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
CLOSE DATA

RETURN
**************

********************
PROCEDURE LDGRAFP
********************

CLEAR
CLOSE ALL
SET DATE brit
SELECT 3
USE master ORDER ac2
SELECT 2
USE ldgrcode ORDER ac1
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

IF _DOS
   SET COLOR TO 7/1
ENDIF

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

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 12, 28 SAY MCC1 PICTURE '99'
   @ 12, 31 SAY MCC2 PICTURE '99'

IF _DOS
   SET COLOR TO 7/1
ENDIF

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
INDEX ON STR(C1,2)+STR(C2,2)+STR(C3,3) TAG ldgr ADDITIVE
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
DO prnchk IN tbl_proc.prg
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
         DO PRNCHK IN tbl_proc.prg
         DO top_prt1 IN tbl_proc.prg
      ELSE
         DO PRNCHK IN tbl_proc.prg
         DO top_prt2 IN tbl_proc.prg
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
CLOSE DATA

RETURN
*********************


