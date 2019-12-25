* Home. : 21-11-2019
* PROGRAM : MNU_PROC.PRG
* Author: Amjad Izhar
* Menus Procedures


*********************
PROCEDURE code_men
*********************
* 	Ledger Heads Maintenance

DO WHILE .T.

IF _WINDOWS
 SET COLOR TO
ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

   DO top IN tbl_proc
   DO bot IN tbl_proc

IF _DOS
   SET COLOR TO +7/1
ENDIF


	IF _DOS
   @ 6, 26 SAY ' Ledger Heads Maintenance'
	ENDIF



IF _DOS
   SET COLOR TO 7/1
ENDIF


IF _DOS
   @ 8, 20 PROMPT 'A - Opening New Ledger Head         '
   @ 9, 20 PROMPT 'B - Correction                      '
   @ 10, 20 PROMPT 'C - Deletion                        '
ENDIF


R = 13
C = 62


	IF _WINDOWS
   		@ R-2, 63 SAY ' Ledger Heads Maintenance' FONT 'FoxFont',11
	ENDIF


IF _WINDOWS
   @ R, C PROMPT 'A - Opening New Ledger Head         ' + SPACE(10)
   @ R+1.5, C PROMPT 'B - Correction                      '+ SPACE(26)
   @ R+3, C PROMPT 'C - Deletion                        '+ SPACE(28)
ENDIF

IF _DOS
   SET COLOR TO +7/1
ENDIF

IF _DOS
   @ 12, 20 SAY '  -------- Printing/Display --------  '
ENDIF



IF _WINDOWS
   @ R+4.7, C SAY '   Printing/Display   ' FONT 'FoxFont',11
ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF


IF _DOS
   @ 13, 20 PROMPT 'D - All Ledger Heads Titles                '
   @ 14, 20 PROMPT 'E - Debtors ...... (With Address)          '
   @ 15, 20 PROMPT 'F - Debtors ...... (Alphabetically Sorted) '
   @ 16, 20 PROMPT 'G - Creditors .... (With Address)          '
   @ 17, 20 PROMPT 'H - Creditors .... (Alphabetically Sorted) '
   @ 18, 20 PROMPT 'M - Return to Previous Menu                '
ENDIF




IF _WINDOWS
		*R = 8
		*C = 20
   @ R+6, C PROMPT 'D - All Ledger Heads Titles                ' + SPACE(11)
   @ R+7.5, C PROMPT 'E - Debtors ...... (With Address)          '+ SPACE(10)
   @ R+9, C PROMPT 'F - Debtors ...... (Alphabetically Sorted) '+ SPACE(5)
   @ R+10.5, C PROMPT 'G - Creditors .... (With Address)          '+ SPACE(9)
   @ R+12, C PROMPT 'H - Creditors .... (Alphabetically Sorted) '+ SPACE(4)
   @ R+13.5, C PROMPT 'M - Return to Previous Menu                '+ SPACE(7)
ENDIF

   CHOICE= 0


		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF


		IF _WINDOWS
			@ 37,141 SAY "A c c o u n t i n g" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF

   MENU TO CHOICE

   SET CLOCK OFF	
   
   DO CASE
      CASE CHOICE=1
         DO ldgr_cre IN lhm_proc.prg
      CASE CHOICE=2
         DO ldgr_cor IN lhm_proc.prg
      CASE CHOICE=3
         DO ldgr_del IN lhm_proc.prg
      CASE CHOICE=4
	 DO ldgrprt.PRG
      CASE CHOICE=5
         DO ldgrprt1 IN lhm_proc.prg
      CASE CHOICE=6
         DO ldgrprt2 IN lhm_proc.prg
      CASE CHOICE=7
         DO ldgrprt3 IN lhm_proc.prg
      CASE CHOICE=8
         DO ldgrprt4 IN lhm_proc.prg
      CASE CHOICE=9
         CLEAR
         IF _WINDOWS
         	CLEAR EVENTS
         ENDIF
         CLOSE DATA
         RETURN
   ENDCASE

ENDDO
RETURN
*************************


**********************
PROCEDURE budg_men
**********************

DO WHILE .T.

IF _WINDOWS
SET COLOR TO
ENDIF


IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top IN tbl_proc
   DO bot IN tbl_proc

IF _DOS
   SET COLOR TO +7/1
ENDIF


	IF _DOS
   @ 6, 26 SAY ' Budget Heads Creation / Printing'
	ENDIF




IF _DOS
   SET COLOR TO 7/1
ENDIF


IF _DOS
   @ 8, 23 PROMPT 'A - Creating New Budget Head           '
   @ 10, 23 PROMPT 'B - Budget Heads ..... Printing/Display'
   @ 12, 23 PROMPT 'M - Return to Previous Menu     '
ENDIF

R = 15
C = 61

	IF _WINDOWS
   		@ R-2, 57 SAY ' Budget Heads Creation / Printing' FONT 'FoxFont',11
	ENDIF

IF _WINDOWS
   @ R, C PROMPT 'A - Creating New Budget Head           ' + SPACE(10)
   @ R+2.5, C PROMPT 'B - Budget Heads ..... Printing/Display' + SPACE(9)
   @ R+5, C PROMPT 'M - Return to Previous Menu     ' + SPACE(19)
ENDIF


   CHOICE= 0

		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF


		IF _WINDOWS
			@ 37,141 SAY "A c c o u n t i n g" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF

   MENU TO CHOICE


   SET CLOCK OFF
	
	
   DO CASE
      CASE CHOICE=1
         DO budg_ent IN bhm_proc.prg
      CASE CHOICE=2
         DO budg_prt IN bhm_proc.prg
      CASE CHOICE=3
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO



RETURN
*********************


* Daily Vouchers
***********************
PROCEDURE VCH_MEN
***********************

DO WHILE .T.


	IF _WINDOWS
		SET COLOR TO
	ENDIF


   CLEAR
   CLOSE ALL
   SET EXCLUSIVE ON
   SET DELETED ON
   USE tran-key ORDER key
   DELETE ALL FOR FLAG=' '
   CLEAR
   CLOSE ALL

	IF _DOS
   		SET COLOR TO 7/1
	ELSE
		SET COLOR TO
	ENDIF

   USE voucher
   ZAP
   USE vch-cor
   ZAP
   CLEAR
   CLOSE ALL
   DO top IN tbl_proc.prg
   DO bot IN tbl_proc.prg

	IF _DOS
   		SET COLOR TO +7/1
	ELSE
		SET COLOR TO
	ENDIF






IF _DOS
   SET COLOR TO 7/1
ELSE
	SET COLOR TO
ENDIF


R = 10
C = 60

IF _DOS
   @ 6, 25 SAY '   Daily Vouchers'
ELSE
   @ R-2, 72 SAY 'Daily Vouchers' FONT 'FoxFont',11
ENDIF


IF _DOS
   @ 8, 23 PROMPT ' A - Entry New Voucher                  '
   @ 9, 23 PROMPT ' B - Entry New Voucher - Second Method  '
   @ 10, 23 PROMPT ' C - Vouchers Correction                '
   @ 11, 23 PROMPT ' D - Vouchers Deletion                  '
ELSE
   @ R, C PROMPT ' A - Entry New Voucher                  ' + SPACE(20)
   @ R+1.5, C PROMPT ' B - Entry New Voucher - Second Method  ' + SPACE(5)
   @ R+3, C PROMPT ' C - Vouchers Correction                ' + SPACE(20)
   @ R+4.5, C PROMPT ' D - Vouchers Deletion                  ' + SPACE(22)
ENDIF


IF _DOS
   SET COLOR TO +7/1
ELSE
	SET COLOR TO
ENDIF


IF _DOS
   @ 12, 23 SAY '------------- Printing --------- '
ELSE
   @ R+6.500, C+10 SAY '  Printing  ' FONT 'FoxFont',11
ENDIF


IF _DOS
   SET COLOR TO 7/1
ELSE
	SET COLOR TO
ENDIF


*R = 6
*C = 23


IF _DOS
   @ 13, 23 PROMPT ' E - Date Wise                      '
   @ 14, 23 PROMPT ' F - Type Wise                      '
   @ 15, 23 PROMPT ' G - Complete Voucher Printing - I  '
   @ 16, 23 PROMPT ' H - Complete Voucher Printing - II '
ELSE
   @ R+8, C PROMPT ' E - Date Wise                      ' + SPACE(33)
   @ R+9.5, C PROMPT ' F - Type Wise                      ' + SPACE(32)
   @ R+11, C PROMPT ' G - Complete Voucher Printing - I  ' + SPACE(17)
   @ R+12.5, C PROMPT ' H - Complete Voucher Printing - II ' + SPACE(17)
ENDIF


IF _DOS
   SET COLOR TO +7/1
ELSE
	SET COLOR TO
ENDIF


IF _DOS
   @ 17, 23 SAY '---------- View On Screen ------ '
ELSE
   @ R+14.8, C+8 SAY ' View On Screen  ' FONT 'FoxFont',11
ENDIF


IF _DOS
   SET COLOR TO 7/1
ELSE
	SET COLOR TO
ENDIF


*R = 6
*C = 23


IF _DOS
   @ 18, 23 PROMPT ' I - Type Wise                     '
   @ 19, 23 PROMPT ' J - Date Wise                     '
   @ 20, 23 PROMPT ' M - Return to Previous Menu       '
ELSE
   @ R+16.5, C PROMPT ' I - Type Wise                     ' + SPACE(35)
   @ R+18, C PROMPT ' J - Date Wise                     ' + SPACE(35)
   @ R+19.5, C PROMPT ' M - Return to Previous Menu       ' + SPACE(20)
ENDIF



   CHOICE= 0

		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ELSE
			@ 37,141 SAY "A c c o u n t i n g" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF

   MENU TO CHOICE

	IF _DOS
   		SET CLOCK OFF
	ELSE
		SET CLOCK OFF
	ENDIF

   DO CASE
      CASE CHOICE=1
		DO vch_ent IN dtn_proc.prg
      CASE CHOICE=2
         DO vch2_ent IN dtn_proc.prg
      CASE CHOICE=3
         DO vch_cor IN dtn_proc.prg
      CASE CHOICE=4
         DO vch_del IN dtn_proc.prg
      CASE CHOICE=5
         DO vch_dprt IN dtn_proc.prg
      CASE CHOICE=6
         DO vch_tprt IN dtn_proc.prg
      CASE CHOICE=7
         DO vch_prt1 IN dtn_proc.prg
      CASE CHOICE=8
         DO vch_prt2 IN dtn_proc.prg
      CASE CHOICE=9
		 DO vch_tvew IN dtn_proc.prg
      CASE CHOICE=10
         DO vch_dvew IN dtn_proc.prg
      CASE CHOICE=11
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO

RETURN
*****************


******************
PROCEDURE DENIED
******************

IF _WINDOWS
SET COLOR TO
ENDIF


CLEAR

		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF


		IF _WINDOWS
			@ 28,65.200 SAY "A c c o u n t i n g" ;
			FONT "Times New Roman", 14 ;
			STYLE "BIT"
		ENDIF


ABC= 0
DO WHILE ABC<=3
   ?? CHR(7)

IF _DOS
   SET COLOR TO *+7/1
ENDIF

   @ 12, 25 SAY 'Access Denied ! '
   ABC= ABC+1
ENDDO

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR

RETURN
************



********************
PROCEDURE POST_MEN
********************

DO WHILE .T.

IF _WINDOWS
	SET COLOR TO
ENDIF

IF _DOS
   SET COLOR TO 7/1
ELSE
	SET COLOR TO
ENDIF

   CLEAR
   DO top IN tbl_proc.prg
   DO bot IN tbl_proc.prg

IF _DOS
   SET COLOR TO +7/1
ENDIF


	IF _DOS
   @ 6, 25 SAY '       P O S T I N G '
	ENDIF




IF _DOS
   SET COLOR TO 7/1
ENDIF

IF _DOS
   @ 8, 24 PROMPT 'A - Vouchers Confirmation / Posting '
   @ 10, 24 PROMPT 'B - Post Vouchers To Ledgers  '
   @ 12, 24 PROMPT 'M - Return to Previous Menu       '
ENDIF

R = 17
C = 63

	IF _WINDOWS
   		@ R-2, 72 SAY 'P O S T I N G' FONT 'FoxFont',11
	ENDIF


IF _WINDOWS
   @ R, C PROMPT 'A - Vouchers Confirmation / Posting ' + SPACE(6)
   @ R+2, C PROMPT 'B - Post Vouchers To Ledgers  ' + SPACE(18)
   @ R+4, C PROMPT 'M - Return to Previous Menu       ' + SPACE(13)
ENDIF


   CHOICE= 0

		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF


		IF _WINDOWS
			@ 37,141 SAY "A c c o u n t i n g" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF

   MENU TO CHOICE
   SET CLOCK OFF

   DO CASE
      CASE CHOICE=1
         DO vch_conf IN pst_proc.prg
      CASE CHOICE=2
         DO ldgrpost IN pst_proc.prg
      CASE CHOICE=3
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO

RETURN
*********************


********************
PROCEDURE REP_MEN1
********************

CLEAR
* STORE DATE() TO DATE
SET DATE TO BRIT

DO chk IN TBL_PROC.PRG


DO WHILE .T.

IF _WINDOWS
SET COLOR TO
ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top IN tbl_proc.prg
   DO bot IN tbl_proc.prg

IF _DOS
   SET COLOR TO +7/1
ENDIF


IF _DOS
   @ 6, 25 SAY ' R E P O R T S  - 1        '
ENDIF


IF _DOS
   SET COLOR TO 7/1
ENDIF


IF _DOS
   @ 8, 24 PROMPT ' A - Ledger Printing                  '
   @ 9, 24 PROMPT ' B - Accounts Receivable Report       '
   @ 10, 24 PROMPT ' C - .........  With Address          '
   @ 11, 24 PROMPT ' D - Accounts Payable Report          '
   @ 12, 24 PROMPT ' E - ......... With Address           '
   @ 13, 24 PROMPT ' F - Accounts Payable/Receivable (Between Dates) '
   @ 14, 24 PROMPT ' G - Journal Report                   '
   @ 15, 24 PROMPT ' H - Journal Report With Description  '
   @ 16, 24 PROMPT ' I - Reminder Letter (Aging From Due Date)   '
   @ 17, 24 PROMPT ' J - Reminder Letter (Aging From Issue Date) '
   @ 18, 24 PROMPT ' K - Aging Report Summary             '
   @ 19, 24 PROMPT ' L - Reminders (Without Aging)        '
   @ 20, 24 PROMPT ' N - Transactions Summary             '
   @ 21, 24 PROMPT ' M - Return to Previous Menu          '
ENDIF

R = 10
C = 58

IF _WINDOWS
   @ R-2, 69 SAY 'R E P O R T S  - 1' FONT "FoxFont",11
ENDIF


IF _WINDOWS
   @ R, C PROMPT ' A - Ledger Printing                  ' + SPACE(41)
   @ R+1.5, C PROMPT ' B - Accounts Receivable Report       ' + SPACE(30)
   @ R+3, C PROMPT ' C - .........  With Address          ' + SPACE(41)
   @ R+4.5, C PROMPT ' D - Accounts Payable Report          ' + SPACE(32)
   @ R+6, C PROMPT ' E - ......... With Address           ' + SPACE(42)
   @ R+7.5, C PROMPT ' F - Accounts Payable/Receivable (Between Dates) ' + SPACE(4)
   @ R+9, C PROMPT ' G - Journal Report                   ' + SPACE(41)
   @ R+10.5, C PROMPT ' H - Journal Report With Description  ' + SPACE(27)
   @ R+12, C PROMPT ' I - Reminder Letter (Aging From Due Date)   ' + SPACE(14)
   @ R+13.5, C PROMPT ' J - Reminder Letter (Aging From Issue Date) ' + SPACE(14)
   @ R+15, C PROMPT ' K - Aging Report Summary             ' + SPACE(31)
   @ R+16.5, C PROMPT ' L - Reminders (Without Aging)        ' + SPACE(29)
   @ R+18, C PROMPT ' N - Transactions Summary             ' + SPACE(31)
   @ R+19.5, C PROMPT ' M - Return to Previous Menu          ' + SPACE(30)
ENDIF



   CHOICE= 0

		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF


		IF _WINDOWS
			@ 37,141 SAY "A c c o u n t i n g" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF

   MENU TO CHOICE
   SET CLOCK OFF

   DO CASE
      CASE CHOICE=1
         DO ldgr_men
      CASE CHOICE=2
         DO rep_rec IN rp1_proc.prg
      CASE CHOICE=3
         DO rep_rec1 IN rp1_proc.prg
      CASE CHOICE=4
         DO rep_pay IN rp1_proc.prg
      CASE CHOICE=5
         DO rep_pay1 IN rp1_proc.prg
      CASE CHOICE=6
         DO reprpay IN rp1_proc.prg
      CASE CHOICE=7
         DO jou_rep IN rp1_proc.prg
      CASE CHOICE=8
         DO jou1_rep IN rp1_proc.prg
      CASE CHOICE=9
         DO rec_age IN rp1_proc.prg
      CASE CHOICE=10
         DO rec_age1 IN rp1_proc.prg
      CASE CHOICE=11
         DO rec_age2 IN rp1_proc.prg
      CASE CHOICE=12
         DO rec_age3 IN rp1_proc.prg
      CASE CHOICE=13
         DO tr_rep IN rp1_proc.prg
      CASE CHOICE=14
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO

RETURN
***********************


********************
PROCEDURE REP_MEN2
*********************

CLEAR
* STORE DATE() TO DATE
SET DATE brit

DO chk IN TBL_PROC.PRG


DO WHILE .T.

IF _WINDOWS
SET COLOR TO
ENDIF


IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top IN tbl_proc.prg
   DO bot IN tbl_proc.prg

IF _DOS
   SET COLOR TO +7/1
ENDIF


IF _DOS
   @ 6, 25 SAY ' R E P O R T S  - 2            '
ENDIF




IF _DOS
   SET COLOR TO 7/1
ENDIF


IF _DOS
   @ 8, 25 PROMPT ' A - Trial Balance Reports     '
   @ 9, 25 PROMPT ' B - Profit & Loss Reports     '
   @ 10, 25 PROMPT ' C - Balance Sheet             '
   @ 11, 25 PROMPT ' D - Budget Versus Actual Performance '
   @ 12, 25 TO 12, 63
ENDIF

R = 10
C = 61


IF _WINDOWS
   @ R-2, 67 SAY ' R E P O R T S  - 2' FONT 'FoxFont',11
ENDIF


IF _WINDOWS
   @ R, C PROMPT ' A - Trial Balance Reports     ' + SPACE(25)
   @ R+1.5, C PROMPT ' B - Profit & Loss Reports     ' + SPACE(25)
   @ R+3, C PROMPT ' C - Balance Sheet             ' + SPACE(29)
   @ R+4.5, C PROMPT ' D - Budget Versus Actual Performance ' + SPACE(4)
   
ENDIF



IF _DOS
   SET COLOR TO +7/1
ENDIF


IF _DOS
   @ 13, 25 SAY '    Convert To Excel Format '
ENDIF


IF _WINDOWS
   @ R+6.5, C+2 SAY 'Convert To Excel Format' FONT 'FoxFont',11
ENDIF



IF _DOS
   SET COLOR TO 7/1
ENDIF


IF _DOS
   @ 14, 25 TO 14, 63
   @ 15, 25 PROMPT ' E - Trial Balance -- Detailed '
   @ 16, 25 PROMPT ' F - Trial Balance -- Summary  '
   @ 17, 25 PROMPT ' G - Profit & Loss -- Detailed '
   @ 18, 25 PROMPT ' H - Profit & Loss -- Summary  '
   @ 19, 25 PROMPT ' I - Balance Sheet -- Detailed '
   @ 21, 25 PROMPT ' M - Return to Previous Menu   '
ENDIF


*R = 6
*C = 25


IF _WINDOWS

   @ R+8, C PROMPT ' E - Trial Balance -- Detailed ' + SPACE(24)
   @ R+9.5, C PROMPT ' F - Trial Balance -- Summary  ' + SPACE(20)
   @ R+11, C PROMPT ' G - Profit & Loss -- Detailed ' + SPACE(23)
   @ R+12.5, C PROMPT ' H - Profit & Loss -- Summary  ' + SPACE(19)
   @ R+14, C PROMPT ' I - Balance Sheet -- Detailed ' + SPACE(23)
   @ R+15.5, C PROMPT ' M - Return to Previous Menu   ' + SPACE(19)
ENDIF




   CHOICE= 0


		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF


		IF _WINDOWS
			@ 37,141 SAY "A c c o u n t i n g" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF

   MENU TO CHOICE
   SET CLOCK OFF
   DO CASE
      CASE CHOICE=1
         DO trl_men
      CASE CHOICE=2
         DO pl_men
      CASE CHOICE=3
         DO bs_c1 IN trl_proc.prg
      CASE CHOICE=4
         DO bud_comp IN trl_proc.prg
      CASE CHOICE=5
         DO trl_dtl IN trl_proc.prg
      CASE CHOICE=6
         DO trl_sum IN trl_proc.prg
      CASE CHOICE=7
         DO pl_dtl IN trl_proc.prg
      CASE CHOICE=8
         DO pl_sum IN trl_proc.prg
      CASE CHOICE=9
         DO bs_sum IN trl_proc.prg
      CASE CHOICE=10
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO
RETURN
*********************


********************
PROCEDURE MAINTMEN
********************

DO WHILE .T.
   SET EXCLUSIVE ON


IF _WINDOWS
SET COLOR TO
ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top IN tbl_proc.prg
   DO bot IN tbl_proc.prg


IF _DOS
   SET COLOR TO +7/1
ENDIF


IF _DOS
   @ 6, 27 SAY 'System Maintenance  '
ENDIF




IF _DOS
   SET COLOR TO 7/1
ENDIF


IF _DOS
COL = 8
   @ 8, COL PROMPT 'A - Backup Data (Via Dos - 5)               '
   @ 9, COL PROMPT 'B - Backup Data (Via Compression Programme) '
   @ 10, COL PROMPT 'C - Re-index Data Files                     '
   @ 11, COL PROMPT 'D - Sort Ledgers Date Wise                  '
   @ 13, COL PROMPT 'M - Return to Previous Menu                 '
ENDIF


R = 14
C = 58

IF _WINDOWS
   @ R-2, 68 SAY 'System Maintenance' FONT 'FoxFont',11
ENDIF


IF _WINDOWS
   @ R, C PROMPT 'A - Backup Data (Via Dos - 5)               ' + SPACE(20)
   @ R+1.5, C PROMPT 'B - Backup Data (Via Compression Programme) ' + SPACE(1)
   @ R+3, C PROMPT 'C - Re-index Data Files                     ' + SPACE(25)
   @ R+4.5, C PROMPT 'D - Sort Ledgers Date Wise                  ' + SPACE(21)
   @ R+6, C PROMPT 'M - Return to Previous Menu                 ' + SPACE(17)
ENDIF


   CANS= 0


		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF


		IF _WINDOWS
			@ 37,141 SAY "A c c o u n t i n g" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF

   MENU TO CANS
   DO CASE
      CASE CANS=1
         DO back_up1 IN smn_proc.prg
      CASE CANS=2
         DO back_up2 IN smn_proc.prg
      CASE CANS=3
         DO reindex IN smn_proc.prg
      CASE CANS=4
         DO sortdate IN smn_proc.prg
      CASE CANS=5
         CLEAR
         EXIT
   ENDCASE
ENDDO
CLEAR

RETURN
*******************



***********************
PROCEDURE SECURMEN
***********************
* Security Menu

DO WHILE .T.

IF _WINDOWS
SET COLOR TO
ENDIF


IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top IN tbl_proc.prg
   DO bot IN tbl_proc.prg

IF _DOS
   SET COLOR TO +7/1
ENDIF


IF _DOS
   @ 6, 27 SAY ' System Security     '
ENDIF




IF _DOS
   SET COLOR TO 7/1
ENDIF


IF _DOS
   @ 8, 25 PROMPT 'A - Change Passwords '
   @ 9, 25 PROMPT 'B - Unpost Vouchers From Ledger'
   @ 10, 25 PROMPT 'C - Year End Procedures   '
   @ 11, 25 PROMPT 'D - Initialize Data File  '
   @ 12, 25 PROMPT 'E - Initialize Password   '
   @ 13, 25 PROMPT 'F - Party Locking System  '
   @ 15, 25 PROMPT 'M - Return to Previous Menu '
ENDIF

R = 13
C = 64


IF _WINDOWS
   @ R-2, 70 SAY 'System Security' FONT 'FoxFont',11
ENDIF


IF _WINDOWS
   @ R, C PROMPT 'A - Change Passwords ' + SPACE(25)
   @ R+1.5, C PROMPT 'B - Unpost Vouchers From Ledger' + SPACE(6)
   @ R+3, C PROMPT 'C - Year End Procedures   ' + SPACE(20)
   @ R+4.5, C PROMPT 'D - Initialize Data File  ' + SPACE(26)
   @ R+6, C PROMPT 'E - Initialize Password   ' + SPACE(24)
   @ R+7.5, C PROMPT 'F - Party Locking System  ' + SPACE(20)
   @ R+9, C PROMPT 'M - Return to Previous Menu ' + SPACE(13)
ENDIF


   CANS= 0


		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF

		IF _WINDOWS
			@ 37,141 SAY "A c c o u n t i n g" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF

   MENU TO CANS
   DO CASE
      CASE CANS=1
         DO password IN smn_proc.prg
      CASE CANS=2
         DO unpost IN smn_proc.prg
      CASE CANS=3
         CLEAR

IF _DOS
         SET COLOR TO +7/1
ENDIF

         @ 10, 25 SAY ' Please  Refer  to .... '
         @ 12, 25 SAY ' Amjad Izhar '
         @ 14, 25 SAY ' E-Mail:  amjad18@yahoo.com '

IF _DOS
         SET COLOR TO 7/1
ENDIF

         @ 14, 25 SAY ' Press any key to Return to Menu'
         WAIT ''

IF _DOS
         SET COLOR TO 7/1       
ENDIF

      CASE CANS=4
         DO initial IN smn_proc.prg
      CASE CANS=5
         DO init_pw IN smn_proc.prg
      CASE CANS=6
         DO lock_men
      CASE CANS=7
         CLEAR
         EXIT
   ENDCASE
ENDDO
CLEAR

RETURN
**********************


***********************
PROCEDURE LDGR_MEN
***********************

STORE DATE() TO DATE

DO WHILE .T.

IF _WINDOWS
SET COLOR TO
ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top IN tbl_proc.prg
   DO bot IN tbl_proc.prg

IF _DOS
   SET COLOR TO +7/1
ENDIF


IF _DOS
   @ 6, 25 SAY '  Ledger  Printing / Display  Reports'
ENDIF


IF _WINDOWS
   @ 4.500, 25 SAY '  Ledger  Printing / Display  Reports'
ENDIF


IF _DOS
   SET COLOR TO 7/1
ENDIF


IF _DOS
   @ 8, 22 PROMPT ' A - Selective Ledger (Display)        '
   @ 9, 22 PROMPT ' B - Selective Ledger (Display - II)   '
   @ 10, 22 PROMPT ' C - Selective Ledger (Printing)       '
   @ 11, 22 PROMPT ' D - Selective Ledger (Printing - II)  '
   @ 12, 22 PROMPT ' E - Ledger Within Range (Printing)    '
   @ 13, 22 PROMPT ' F - Selective Ledger (Printing With Address)    '
   @ 14, 22 PROMPT ' G - Ledger Within Range (Printing With Address) '
   @ 15, 22 PROMPT ' H - Selective Ledger (Balance Display)   '
   @ 16, 22 PROMPT ' I - Day Book (Printing/Display)          '
   @ 17, 22 PROMPT ' J - Ledger Within Range (Printing - II)  '
   @ 19, 22 PROMPT ' M - Return to Previous Menu              '
ENDIF


IF _WINDOWS
   @ 6, 22 PROMPT ' A - Selective Ledger (Display)        '
   @ 7.500, 22 PROMPT ' B - Selective Ledger (Display - II)   '
   @ 9, 22 PROMPT ' C - Selective Ledger (Printing)       '
   @ 10.500, 22 PROMPT ' D - Selective Ledger (Printing - II)  '
   @ 12, 22 PROMPT ' E - Ledger Within Range (Printing)    '
   @ 13.500, 22 PROMPT ' F - Selective Ledger (Printing With Address)    '
   @ 15, 22 PROMPT ' G - Ledger Within Range (Printing With Address) '
   @ 16.500, 22 PROMPT ' H - Selective Ledger (Balance Display)   '
   @ 18, 22 PROMPT ' I - Day Book (Printing/Display)          '
   @ 19.500, 22 PROMPT ' J - Ledger Within Range (Printing - II)  '
   @ 21, 22 PROMPT ' M - Return to Previous Menu              '
ENDIF


   CHOICE= 0


		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF

		IF _WINDOWS
			@ 35.077,65.200 SAY "A c c o u n t i n g" ;
			FONT "Times New Roman", 24 ;
			STYLE "BIT"
		ENDIF

   MENU TO CHOICE

   SET CLOCK OFF

   DO CASE
      CASE CHOICE=1
         DO ldgr_sd IN lpr_proc.prg
      CASE CHOICE=2
         DO ldgr_sd2 IN lpr_proc.prg
      CASE CHOICE=3
         DO ldgr_sp IN lpr_proc.prg
      CASE CHOICE=4
         DO ldgr_sp2 IN lpr_proc.prg
      CASE CHOICE=5
         DO ldgr_fp IN lpr_proc.prg
      CASE CHOICE=6
         DO ldgrasp IN lpr_proc.prg
      CASE CHOICE=7
         DO ldgrafp IN lpr_proc.prg
      CASE CHOICE=8
         DO ldgr_sdb IN lpr_proc.prg
      CASE CHOICE=9
         DO vch_book IN lpr_proc.prg
      CASE CHOICE=10
         DO ldgr_fp2 IN lpr_proc.prg
      CASE CHOICE=11
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO

RETURN
*******************



************************
PROCEDURE TRL_MEN
************************

CLEAR
STORE DATE() TO DATE
DO WHILE .T.

IF _WINDOWS
SET COLOR TO
ENDIF


IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top IN tbl_proc.prg
   DO bot IN tbl_proc.prg

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 27 SAY ' Trial  Balance  Reports'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 8, 25 PROMPT 'A - Trial Balance -- Detailed'
   @ 9, 25 PROMPT 'B - Trial Balance -- Summary '
   @ 10, 25 PROMPT 'C - Trial Balance -- Detailed With Group Totals'
   @ 11, 25 PROMPT 'D - Trial Balance -- Previous Date '
   @ 12, 25 PROMPT 'E - Trial Balance -- Between Two Dates'
   @ 13, 25 PROMPT 'F - User Definable Trial Balance   '
   @ 15, 25 PROMPT 'M - Return to Previous Menu '
   CHOICE= 0

		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF


		IF _WINDOWS
			@ 35.077,65.200 SAY "A c c o u n t i n g" ;
			FONT "Times New Roman", 24 ;
			STYLE "BIT"
		ENDIF

   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         DO c3_trl IN trl_proc.prg
      CASE CHOICE=2
         DO c2_trl IN trl_proc.prg
      CASE CHOICE=3
         DO c3_trl1 IN trl_proc.prg
      CASE CHOICE=4
         DO prv_trl IN trl_proc.prg
      CASE CHOICE=5
         DO date_trl IN trl_proc.prg
      CASE CHOICE=6
         DO utrl_men
      CASE CHOICE=7
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO

RETURN
************************



*********************
PROCEDURE PL_MEN
*********************

CLEAR
CLOSE ALL
STORE DATE() TO DATE
DO WHILE .T.

IF _WINDOWS
SET COLOR TO
ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top IN tbl_proc.prg
   DO bot IN tbl_proc.prg

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 27 SAY ' PROFIT & LOSS REPORTS'
   @ 8, 21 SAY '---------  P & L  (Uptodate) -------- '
   @ 13, 21 SAY '-----  P & L (Between Two Dates) ---- '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 10, 21 PROMPT 'A - Profit & Loss Statement (Level 3) '
   @ 11, 21 PROMPT 'B - Profit & Loss Statement (Level 2) '
   @ 15, 21 PROMPT 'C - Profit & Loss Statement (Level 3) '
   @ 17, 21 PROMPT 'M - Return to Previous Menu '
   CHOICE= 0

		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF


		IF _WINDOWS
			@ 35.077,65.200 SAY "A c c o u n t i n g" ;
			FONT "Times New Roman", 24 ;
			STYLE "BIT"
		ENDIF

   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         DO c3_pl IN plb_proc.prg
      CASE CHOICE=2
         DO c2_pl IN plb_proc.prg
      CASE CHOICE=3
         DO rep_pl2 IN plb_proc.prg
      CASE CHOICE=4
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO

RETURN
************************



******************************
PROCEDURE UTRL_MEN
******************************


DO WHILE .T.

IF _WINDOWS
SET COLOR TO
ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY 'User Definable Trial Balance Reports'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 8, 25 PROMPT ' 1 - Add Ledgerheads To This Report  '
   @ 10, 25 PROMPT ' 2 - Delete Ledgerheads              '
   @ 12, 25 PROMPT ' 3 - Uptodate Trial Balance Report   '
   @ 14, 25 PROMPT ' 9 - Return to Previous Menu         '
   MCHOICE= 0


		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF

		IF _WINDOWS
			@ 35.077,65.200 SAY "A c c o u n t i n g" ;
			FONT "Times New Roman", 24 ;
			STYLE "BIT"
		ENDIF

   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=4
         CLEAR
         CLOSE DATABASE
         RETURN
      CASE MCHOICE=3
         CLEAR
         DO utrl_prt IN trl_proc.prg
      CASE MCHOICE=2
         CLEAR
         DO utrl_del IN trl_proc.prg
      CASE MCHOICE=1
         CLEAR
         DO utrl_ent IN trl_proc.prg
   ENDCASE
ENDDO

RETURN
*************************


******************************
PROCEDURE LOCK_MEN
*******************************

DO WHILE .T.

IF _WINDOWS
SET COLOR TO
ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top IN tbl_proc.prg
   DO bot IN tbl_proc.prg

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 26 SAY '      Party Locking '

IF _DOS
   SET COLOR TO 7/1
ENDIF


   @ 8, 24 PROMPT '1 - Party Locking ...... Entry    '
   @ 10, 24 PROMPT '2 - Party Locking ...... Deletion '
   @ 12, 24 PROMPT '3 - Party Locking ...... Printing '
   @ 14, 24 PROMPT '4 - Party Locking (Alphabetically Sorted) '
   @ 16, 24 PROMPT '9 - Return to Previous Menu       '
   CHOICE= 0


		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ENDIF

		IF _WINDOWS
			@ 35.077,65.200 SAY "A c c o u n t i n g" ;
			FONT "Times New Roman", 24 ;
			STYLE "BIT"
		ENDIF

   MENU TO CHOICE
   DO CASE
      CASE CHOICE=1
         DO lock_ent IN smn_proc.prg
      CASE CHOICE=2
         DO lock_del IN smn_proc.prg
      CASE CHOICE=3
         DO lock_prt IN smn_proc.prg
      CASE CHOICE=4
         DO lockprt IN smn_proc.prg
      CASE CHOICE=5
         CLEAR
         CLOSE ALL
         RETURN
   ENDCASE
ENDDO

RETURN
************************************


*****************
PROCEDURE CHK
*****************

IF _WINDOWS
SET COLOR TO
ENDIF


SET DATE brit
SET CENT ON



IF mDATE > {30/06/2097}
QUIT

ENDIF

RETURN
*******************
