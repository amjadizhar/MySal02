* Programmer : Amjad Izhar
* Revised Again on 19-01-2001 7:15 PM



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

   RMK= 'Cls.Bal.'
   @ LINE, 72 SAY RMK
   LINE= LINE+2
   @ LINE, 5 SAY 'R e m a r k s     -  '
   @ LINE, 25 SAY MREM
   @ 24, 22 SAY 'Press Any Key To Return '
   WAIT ''



ENDDO

CLEAR
CLOSE DATA
CLOSE ALL

RETURN
******************************
