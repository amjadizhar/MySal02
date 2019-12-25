* PROGRAM : MAIN.PRG
* ACCOUNTING & INVENTORY SYSTEM
* WRITTEN BY : AMJAD IZHAR
* THIS UPDATE: Tue. 03-Oct-2015 09:39 PM
* MENU 1-7 PASSWORD = AMJ005
* MENU 8 PASSWORD = IZHAR9
* Dada Industries 2015-16
* Directory: DI16
*********************************

CLEAR

IF _WINDOWS
	CLEAR EVENTS
ENDIF

CLOSE ALL
CLEAR ALL

#DEFINE Programmer "Amjad Izhar"
#DEFINE ContactEmail "amjad18@yahoo.com"
#DEFINE ThisUpdate "03-Oct-2015"
#DEFINE BottomLine "Integrated : Accounting + Inventory + Payroll    &    Others"

#DEFINE DesignBy "Designed & Developed by Amjad Izhar :"
#DEFINE MyCompany "CosmoSoft Systems , Lahore"
#DEFINE CompanyName "CosmoSoft Trading Systems"


**********13-11-2015************************************************************
*PUBLIC CompanyName, Programmer, ContactEmail, ThisUpdate, BottomLine
*USE COMPANYINFO IN 0 SHARED
*LOCATE FOR COMPID = 2
*CompanyName = COMPANYINFO.COMPNAME
*Programmer = COMPANYINFO.PROGRAMMER
*ContactEmail = "amjad18@yahoo.com"
*ThisUpdate = "11-Nov-2015"
*BottomLine = "Integrated : Accounting + Inventory + Payroll    &    Others"
********************************************************************************

IF _WINDOWS
	_screen.caption = CompanyName
ENDIF

SET EXCLUSIVE OFF
SET SAFE OFF
SET DATE TO BRITISH
SET CENT ON


on key label F1 ?? CHR(7)
on key label F10 ?? CHR(7)



*LDGRCODE STRUCTURE
*C1				N	2			01
*C2				N	2			02
*C3				N	3			03
*ACCTCODE		C	10			04
*TITLE			C	30			05
*ADDRESS1		C	35			06
*ADDRESS2		C	35			07
*CITY			C	35			08
*SR_NO			N	5			09
*BUDGET			N	12	2		10
*LDGR_BAL		N	12	2		11
*STATUS			C	2			12
*OPEN_BAL		N	12	2		13
*OPEN_STAT		C	2			14
*COST_C			N	1			15
*REMARKS		C	45			16
*LOCK			C	1			17
*RCVABLE		L				18
*PAYABLE		L				19
*DI14			L				20
*ASSETS			L				21
*CHART			C	30			22
*NZ				L				23




USE LDGRCODE SHARED
IF FLOCK()
	*Receivables
	REPL ALL RCVABLE WITH .T. FOR C1=53
	*Payables
	REPL ALL PAYABLE WITH .T. FOR C1=34
	REPL ALL PAYABLE WITH .T. FOR C1=35
	REPL ALL PAYABLE WITH .T. FOR C1=36
	REPL ALL PAYABLE WITH .T. FOR C1=37
	REPL ALL PAYABLE WITH .T. FOR C1=38
	REPL ALL PAYABLE WITH .T. FOR C1=39
	*Dada Industries 2014
	REPL ALL DI14 WITH .T. FOR C1=11 .AND. C2=10
	*Assets
	REPL ALL ASSETS WITH .T. FOR C1=41
	REPL ALL ASSETS WITH .T. FOR C1=42
	REPL ALL ASSETS WITH .T. FOR C1=43
	REPL ALL ASSETS WITH .T. FOR C1=44
	REPL ALL ASSETS WITH .T. FOR C1=45
	REPL ALL ASSETS WITH .T. FOR C1=46
	REPL ALL ASSETS WITH .T. FOR C1=47
	REPL ALL ASSETS WITH .T. FOR C1=48
	REPL ALL ASSETS WITH .T. FOR C1=49
	USE
	UNLOCK
ELSE
	WAIT "File is in use by another user. Please wait ... " WINDOW NOWAIT
ENDIF

USE MASTER SHARED
IF FLOCK()
	REPL ALL MASTER.SERIAL WITH RECNO()
	USE
	UNLOCK
ELSE
	WAIT "File is in use by another user. Please wait ... " WINDOW NOWAIT
ENDIF


* Prepare files for Admin Separately in Shared Environment
* to be used by Admin Exclusively.

* MASTER01
	SELECT * ;
	FROM MASTER;
	ORDER BY C1,C2,C3,DATE,SERIAL;
	INTO DBF MASTER01.DBF
	SELECT MASTER
	USE
	SELECT MASTER01
	USE
	
* LDGRCODE01

IF _WINDOWS
	SELECT * ;
	FROM LDGRCODE;
	ORDER BY C1,C2,C3;
	INTO DBF LDGRCODE01.DBF
	SELECT LDGRCODE
	USE
	SELECT LDGRCODE01
	USE

* TRAN_KEY01
	SELECT * ;
	FROM TRAN-KEY;
	ORDER BY VCH_TYPE,VCH_NO1,VCH_NO2;
	INTO DBF TRAN_KEY01.DBF
	SELECT TRAN_KEY
	USE
	SELECT TRAN_KEY01
	USE


* TRAN01
	SELECT * ;
	FROM TRAN;
	ORDER BY DATE,VCH_TYPE,VCH_NO1,VCH_NO2,SR_NO;
	INTO DBF TRAN01.DBF
	SELECT TRAN
	USE
	SELECT TRAN01
	USE
ENDIF


SET TALK OFF
SET SAFE OFF
SET EXCLUSIVE OFF


*DO IndexInvt IN INDXPROC.PRG
*DO DbfsPack IN INDXPROC.PRG


CLEAR

********************************************
* the following procedure UpdClosBal
* will update Closing Balance for all
* inventory items in item.dbf
* taking last balance from item-bal.dbf

*DO UpdClosBal IN invtlib1
*********************************************

********************************************
* the following procedure UpdItmName
* will update Item Name for all
* inventory items in item-bal.dbf
* taking Item Name from item.dbf

*DO UpdItmName IN invtlib1
*********************************************



PUBLIC mTopTitle,mRootPrg
mTopTitle = SPACE(25)


		IF _DOS
 			SET COLOR TO 7/1
		ELSE
			SET COLOR TO
		ENDIF

		CLEAR


IF _DOS
 SET SAFETY OFF
 SET TALK OFF
 SET STATUS OFF
 SET EXCLUSIVE ON
 SET DATE BRIT
 SET CENT ON
 SET AUTOSAVE ON
ENDIF

IF _WINDOWS
 SET SYSMENU OFF
 SET SAFETY OFF
 SET TALK OFF
 SET STATUS OFF
 SET EXCLUSIVE OFF
 SET DATE BRIT
 SET CENT ON
 SET AUTOSAVE ON
ENDIF





DO WHILE .T.

mTopTitle = SPACE(25)


	IF _DOS
 			SET COLOR TO 7/1    
		ELSE
			SET COLOR TO
	ENDIF

   USE master SHARED
   FYDATE= DATE
   USE

   CLEAR

IF _WINDOWS
   L= 15
   M= 65
ENDIF

IF _DOS
   L= 7
   M= 8
ENDIF


   DO top IN TBL_PROC.prg
   DO bot IN TBL_PROC.prg

		IF _DOS   
 			SET COLOR TO +7/1
		ENDIF

		IF _WINDOWS
			SET COLOR TO
		ENDIF




IF _WINDOWS
	@ 5,125 SAY "Programs Updated On: " FONT 'FoxFont',11
	@ 5,153 SAY ThisUpdate FONT 'FoxFont',11
	@ 36,102 SAY CompanyName FONT 'Times New Roman',14
ENDIF

		IF _DOS
   			@ 6, 28 SAY ' M A I N    M E N U'
		ENDIF

*  	FONT "FoxFont",11

IF _WINDOWS
   @ L-2, 70 SAY 'M A I N    M E N U' FONT "FoxFont",11
ENDIF

IF _DOS
    SET COLOR TO 7/1    
ENDIF


IF _WINDOWS
SET COLOR TO
ENDIF



IF _DOS
   @ L+0, M PROMPT ' A - A c c o u n t i n g  '
   @ L+2, M PROMPT ' I - I n v e n t o r y   '
   @ L+4, M PROMPT ' P - P a y r o l l        '
   @ L+6, M PROMPT ' Q - Q u i t              '
ENDIF


IF _WINDOWS
   @ L+0, M PROMPT ' A - A c c o u n t i n g  ' + SPACE(28)
   @ L+2, M PROMPT ' I - I n v e n t o r y   ' + SPACE(32)
   @ L+4, M PROMPT ' P - P a y r o l l        ' + SPACE(33)
   @ L+6, M PROMPT ' Q - Q u i t              ' + SPACE(34)
ENDIF



		IF _DOS
		@ 22,25 SAY BottomLine
		ENDIF

		IF _WINDOWS
			@ 38,102 SAY BottomLine FONT "Times New Roman", 12 ;
			STYLE "BIT"
		ENDIF

   MCHOICE= 0

   MENU TO MCHOICE

   DO CASE

      CASE MCHOICE=1
         CLEAR
         mTopTitle = 'Accounting'

		IF _WINDOWS
		SET COLOR TO
		ENDIF

         DO mainac

      CASE MCHOICE=2
         CLEAR
		 mTopTitle = 'Inventory'

		IF _WINDOWS
		SET COLOR TO
		ENDIF

         DO MainInvt

      CASE MCHOICE=3

         CLEAR
		 mTopTitle = 'Payroll'

		IF _WINDOWS
		SET COLOR TO
		ENDIF

         DO mmpayrol


	CASE MCHOICE=4

         CLEAR
         CLOSE ALL
         CANCEL



		IF _WINDOWS
		SET COLOR TO
		ENDIF



   ENDCASE

ENDDO


