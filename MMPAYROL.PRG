********************************
* MMPAYROL.PRG
* Main Menu Payroll Program
********************************
********************************
* Programmer Amjad Izhar
* Last Revised On ; 21-Dec-2019
********************************

*DataBase Tables List
* 1- Saldtail.dbf
* 2- Employee.dbf
* 3- Salary.dbf
* 4- Attend.dbf
* 5- Ovtime.dbf
* 6- Artime.dbf
* 7- Dptime.dbf
* 8- Deduct.dbf
* 9- Attendat.dbf
* 10- Ovtimdat.dbf
* 11- Artimdat.dbf
* 12- Dptimdat.dbf
* 13- Slrydat.dbf
* 14- Advance.dbf
* 15- Contrib.dbf
* 16- Instded.dbf
* 17- Leaves.dbf
* 18- usr.dbf
* 19- month.dbf
* 20- holiday.dbf
* 21- deptt.dbf
* 22- ysetting.dbf
* 23- Instded.dbf
* 24- SalSheet.dbf
* 25- EmpData.dbf
* 26- AdvData.dbf
* 27- InstData.dbf


* PRG files
* 1-mmpayrol.PRG
* 2-AskPw.PRG
* 3-BackGrnd.PRG
* 4-dlyatnd.PRG

SET TALK OFF
SET CONFIRM OFF
SET SAFE OFF
SET DATE TO BRITISH
SET CENT ON

 PUBLIC mCdir,mDrive
 STORE sys(5) TO mDrive
 STORE curdir() TO mCdir

 PUBLIC mPCUser
 STORE SYS(0) TO mPCUser


* UpdEmpSr.PRG written on 20-Jul-2017 by Amjad Izhar
*PROCEDURE UPDEMPSR is in this PRG file MMPAYROL.PRG
DO UPDEMPSR

DO UpdEmp.PRG



USE salsheet EXCLUSIVE
INDEX ON STR(mcode,2)+STR(year,4) TAG mcode
USE

USE attend EXCLUSIVE
INDEX ON STR(mcode,2)+STR(year,4) TAG mcode
USE

PUBLIC backgrnd

on key label F1 ?? CHR(7)
on key label F10 ?? CHR(7)

on key label rightarrow ?? CHR(7)
on key label LEFTarrow ?? CHR(7)

SET SYSMENU OFF
SET TALK OFF
SET STATUS OFF
SET SAFETY OFF
SET DATE TO BRIT
SET CENT ON


IF _DOS
 SET CLOCK TO 0,69
 SET CLOCK ON
ENDIF

SET TALK OFF
SET ESCAPE OFF

CLEAR MACROS

CLOSE ALL
CLEAR

 DO BackGrnd

 SAVE SCREEN TO backgrnd

R = 1
R2 = 19
C = 1
C2 = 30

DEFINE POPUP mainmenu FROM R,C TO R2,C2
DEFINE BAR 1 OF mainmenu PROMPT "²²²²²²²  Main Menu   ²²²²²²²" SKIP
DEFINE BAR 2 OF mainmenu PROMPT "\-"
DEFINE BAR 3 OF mainmenu PROMPT " Data \<Files"
DEFINE BAR 4 OF mainmenu PROMPT "\-"
DEFINE BAR 5 OF mainmenu PROMPT " \<Daily Attendence"
DEFINE BAR 6 OF mainmenu PROMPT " Attendence Register \<View"
DEFINE BAR 7 OF mainmenu PROMPT "\-"
DEFINE BAR 8 OF mainmenu PROMPT " Ded\<uctions"
DEFINE BAR 9 OF mainmenu PROMPT "\-"
DEFINE BAR 10 OF mainmenu PROMPT " \<System Maintenance"
DEFINE BAR 11 OF mainmenu PROMPT " Sys\<tem Security"
DEFINE BAR 12 OF mainmenu PROMPT "\-"
DEFINE BAR 13 OF mainmenu PROMPT " \<Open Month"
DEFINE BAR 14 OF mainmenu PROMPT "\-"
DEFINE BAR 15 OF mainmenu PROMPT " \<Reports"
DEFINE BAR 16 OF mainmenu PROMPT "\-"
DEFINE BAR 17 OF mainmenu PROMPT " \<Quit"
ON SELECTION POPUP mainmenu DO mmchoice

DEFINE POPUP empMenu FROM R+3,C+5 TO R2+3,C2+5
DEFINE BAR 1 OF empMenu PROMPT "°°°°°° Data Files °°°°°°" SKIP
DEFINE BAR 2 OF empMenu PROMPT "\-"
DEFINE BAR 3 OF empMenu PROMPT " \<A- Department Codes"
DEFINE BAR 4 OF empMenu PROMPT " \<B- Employees Data"
DEFINE BAR 5 OF empMenu PROMPT " \<C- "
DEFINE BAR 6 OF empMenu PROMPT " \<D- "
DEFINE BAR 7 OF empMenu PROMPT " \<E- "
DEFINE BAR 10 OF empMenu PROMPT "\-"
DEFINE BAR 11 OF empMenu PROMPT " \<Main Menu"
ON SELECTION POPUP empMenu DO empmnu


DEFINE POPUP empdata FROM 8,12 TO 19,62
DEFINE BAR 1 OF empdata PROMPT "°°°°°° Employees Data °°°°°°" SKIP
DEFINE BAR 2 OF empdata PROMPT "\-"
DEFINE BAR 3 OF empdata PROMPT " \<A- View List of All Employees"
DEFINE BAR 4 OF empdata PROMPT " \<B- Add New Employee"
DEFINE BAR 5 OF empdata PROMPT " \<C- Edit Employee"
DEFINE BAR 6 OF empdata PROMPT " \<D- Confirmation of Employee"
DEFINE BAR 7 OF empdata PROMPT " \<E- Remove Employee from Attendence Register"
DEFINE BAR 10 OF empdata PROMPT "\-"
DEFINE BAR 11 OF empdata PROMPT " Data Files \<Menu"
ON SELECTION POPUP empdata DO empchoice

DEFINE POPUP AtRegVew1 FROM 10,6 TO 19,65
DEFINE BAR 1 OF AtRegVew1 PROMPT "°°°°°° Register View °°°°°°" SKIP
DEFINE BAR 2 OF AtRegVew1 PROMPT "\-"
DEFINE BAR 3 OF AtRegVew1 PROMPT " \<A- Attendence View"
DEFINE BAR 4 OF AtRegVew1 PROMPT " \<B- Over Time View"
DEFINE BAR 5 OF AtRegVew1 PROMPT " \<C- Department Wise Over Time Total"
DEFINE BAR 6 OF AtRegVew1 PROMPT " \<D- Department Wise Over Time Entry"
DEFINE BAR 10 OF AtRegVew1 PROMPT "\-"
DEFINE BAR 11 OF AtRegVew1 PROMPT " \<Main Menu"
ON SELECTION POPUP AtRegVew1 DO AtRegVew


DEFINE POPUP rptchoice FROM 6,6 to 20,45
DEFINE BAR 1 OF rptchoice PROMPT "°°°°°° Reports °°°°°°" SKIP
DEFINE BAR 2 OF rptchoice PROMPT "\-"
DEFINE BAR 3 OF rptchoice PROMPT " \<A- Reports - 1"
DEFINE BAR 4 OF rptchoice PROMPT " \<B- Reports - 2"
DEFINE BAR 5 OF rptchoice PROMPT " \<C- Reports - 3"
DEFINE BAR 6 OF rptchoice PROMPT " \<D- Reports - 4"
DEFINE BAR 7 OF rptchoice PROMPT " \<E- Reports - 5"
DEFINE BAR 8 OF rptchoice PROMPT " \<F- Reports - 6"
DEFINE BAR 15 OF rptchoice PROMPT "\-"
DEFINE BAR 16 OF rptchoice PROMPT " \<Main Menu"
ON SELECTION POPUP rptchoice DO rptch

DEFINE POPUP rpt1menu FROM 8,10 TO 22,60
DEFINE BAR 1 OF rpt1menu PROMPT "°°°°°° Reports Menu - 1 °°°°°°" SKIP
DEFINE BAR 2 OF rpt1menu PROMPT "\-"
DEFINE BAR 3 OF rpt1menu PROMPT " \<A- Salary Sheets"
DEFINE BAR 4 OF rpt1menu PROMPT " \<B- Wages Sheets"
DEFINE BAR 5 OF rpt1menu PROMPT " \<C- Over Time Sheets"
DEFINE BAR 15 OF rpt1menu PROMPT "\-"
DEFINE BAR 16 OF rpt1menu PROMPT " Reports \<Menu"
ON SELECTION POPUP rpt1menu DO rpt1mnu

DEFINE POPUP rpt2menu FROM 8,10 TO 22,60
DEFINE BAR 1 OF rpt2menu PROMPT "°°°°°° Reports Menu - 2 °°°°°°" SKIP
DEFINE BAR 2 OF rpt2menu PROMPT "\-"
DEFINE BAR 3 OF rpt2menu PROMPT " \<A- Employee Leaves Record"
DEFINE BAR 4 OF rpt2menu PROMPT " \<B- Employee Detailed Attendence Sheet"
DEFINE BAR 5 OF rpt2menu PROMPT " \<C- "
DEFINE BAR 15 OF rpt2menu PROMPT "\-"
DEFINE BAR 16 OF rpt2menu PROMPT " Reports \<Menu"
ON SELECTION POPUP rpt2menu DO rpt2mnu

DEFINE POPUP rpt3menu FROM 9,10 TO 23,65
DEFINE BAR 1 OF rpt3menu PROMPT "°°°°°° Reports Menu - 3 °°°°°°" SKIP
DEFINE BAR 2 OF rpt3menu PROMPT "\-"
DEFINE BAR 3 OF rpt3menu PROMPT " \<A- Over Time Tally Sheet 1-10"
DEFINE BAR 4 OF rpt3menu PROMPT " \<B- Over Time Tally Sheet 11-20"
DEFINE BAR 5 OF rpt3menu PROMPT " \<C- Over Time Tally Sheet 21-31"
DEFINE BAR 6 OF rpt3menu PROMPT " \<D- Week Wise Over Time Tally Sheet 1-31"
DEFINE BAR 15 OF rpt3menu PROMPT "\-"
DEFINE BAR 16 OF rpt3menu PROMPT " Reports \<Menu"
ON SELECTION POPUP rpt3menu DO rpt3mnu

DEFINE POPUP SysSec1 FROM 8,10 TO 22,60
DEFINE BAR 1 OF SysSec1 PROMPT "°°°°°° System Security °°°°°°" SKIP
DEFINE BAR 2 OF SysSec1 PROMPT "\-"
DEFINE BAR 3 OF SysSec1 PROMPT " \<A- Change Passwords"
DEFINE BAR 4 OF SysSec1 PROMPT " \<B- "
DEFINE BAR 5 OF SysSec1 PROMPT " \<C- "
DEFINE BAR 15 OF SysSec1 PROMPT "\-"
DEFINE BAR 16 OF SysSec1 PROMPT " \<Main Menu"
ON SELECTION POPUP SysSec1 DO SysSec

DEFINE POPUP SysMain1 FROM 7,10 TO 21,55
DEFINE BAR 1 OF SysMain1 PROMPT "°°°°°° System Maintenance °°°°°°" SKIP
DEFINE BAR 2 OF SysMain1 PROMPT "\-"
DEFINE BAR 3 OF SysMain1 PROMPT " \<A- Re-Index Data Files"
DEFINE BAR 4 OF SysMain1 PROMPT " \<B- Posting Leaves Data"
DEFINE BAR 5 OF SysMain1 PROMPT " \<C- "
DEFINE BAR 15 OF SysMain1 PROMPT "\-"
DEFINE BAR 16 OF SysMain1 PROMPT " \<Main Menu"
ON SELECTION POPUP SysMain1 DO SysMain

ACTIVATE POPUP mainmenu

*****************************
PROCEDURE mmchoice
*****************************
on key label rightarrow ?? CHR(7)
on key label rightarrow ?? CHR(7)

DO CASE
	
	*" Data \<Files"
	CASE BAR() = 3
		
		ACTIVATE POPUP empMenu

	*" \<Daily Attendence"
	CASE BAR() = 5
		

        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
		
		
		PUBLIC mMenu
		mMenu = 2

		DO AskPw

		IF Permit = "OK"
			DO dlyatnd
		ENDIF
		
		
		RESTORE SCREEN FROM backgrnd	
		

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)
		SHOW POPUP mainmenu

	*" Attendence Register \<View"
	CASE BAR() = 6
	
		ACTIVATE POPUP AtRegVew1


	*" Ded\<uctions"
	CASE BAR() = 8



        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
		
		PUBLIC mMenu
		mMenu = 4
		DO AskPw

		IF Permit = "OK"
		DO emdt1  && Deductions
		ENDIF
		
*		RESTORE SCREEN FROM backgrnd

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)
		SHOW POPUP mainmenu

	*" \<System Maintenance"
	CASE BAR() = 10
		
		ACTIVATE POPUP SysMain1



	*" Sys\<tem Security"
	CASE BAR() = 11
		
		ACTIVATE POPUP SysSec1


	*" \<Open Month"
	CASE BAR() = 13

        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL

		PUBLIC mMenu
		mMenu = 8
		DO AskPw

		IF Permit = "OK"
		DO opmonth
		ENDIF


IF _DOS
		RESTORE SCREEN FROM backgrnd		
ENDIF



        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)
		SHOW POPUP mainmenu

	*" \<Reports"
	CASE BAR() = 15

		ACTIVATE POPUP rptchoice


	*" \<Quit"
	CASE BAR() = 17


        on key label rightarrow 
        on key label LEFTarrow 

		*SET RESOURCE OFF
		*SET RESOURCE TO foxusr1

		CLEAR
		
		*return to master
		CANCEL
		
	OTHERWISE 

		?? CHR(7)

ENDCASE

RETURN
*****************




************************************
PROCEDURE empmnu
************************************

DO CASE

	CASE BAR() = 4

		ACTIVATE POPUP empdata

	CASE BAR() = 11

		DEACTIVATE POPUP empMenu

	OTHERWISE 

		?? CHR(7)
ENDCASE


RETURN
******************			


**************************************
PROCEDURE empchoice
**************************************

DO CASE

	CASE BAR() = 4


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 1
		DO AskPw

		IF Permit = "OK"
		DO NewEmp.prg
		ENDIF
		
		IF _DOS
		RESTORE SCREEN FROM backgrnd
		ENDIF
		

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)
		SHOW POPUP mainmenu
		SHOW POPUP empMenu
		SHOW POPUP empdata


	CASE BAR() = 5


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 1
		DO AskPw

		IF Permit = "OK"
		DO EditEmp.prg
		ENDIF
		
		IF _DOS
		RESTORE SCREEN FROM backgrnd
		ENDIF


        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)

		SHOW POPUP mainmenu
		SHOW POPUP empMenu
		SHOW POPUP empdata


	CASE BAR() = 6


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 1
		DO AskPw

		IF Permit = "OK"
		DO CnfEmp.prg		
		ENDIF

*		RESTORE SCREEN FROM backgrnd

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)
		SHOW POPUP mainmenu
		SHOW POPUP empMenu
		SHOW POPUP empdata
		
	CASE BAR() = 7


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 1
		DO AskPw

		IF Permit = "OK"
		DO EndEmp.prg
		ENDIF
		
*		RESTORE SCREEN FROM backgrnd

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)
		SHOW POPUP mainmenu
		SHOW POPUP empMenu
		SHOW POPUP empdata


	CASE BAR() = 11

		DEACTIVATE POPUP empdata

	OTHERWISE 

		?? CHR(7)

ENDCASE

RETURN
**********************************


**************************************
PROCEDURE AtRegVew
**************************************

DO CASE

	CASE BAR() = 3


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL

		PUBLIC mMenu
		mMenu = 3
		DO AskPw

		IF Permit = "OK"
			DO atndv2.prg
		ENDIF
		
		IF _DOS
		RESTORE SCREEN FROM backgrnd
		ENDIF



        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)
		SHOW POPUP mainmenu
		SHOW POPUP AtRegVew1


	CASE BAR() = 4


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL

		PUBLIC mMenu
		mMenu = 3
		DO AskPw

		IF Permit = "OK"
			DO OvtmV2.prg
		ENDIF
		
*		RESTORE SCREEN FROM backgrnd

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)
		SHOW POPUP mainmenu
		SHOW POPUP AtRegVew1

	CASE BAR() = 5


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL

		PUBLIC mMenu
		mMenu = 3
		DO AskPw

		IF Permit = "OK"
			DO DwOtV2.prg
		ENDIF
		
*		RESTORE SCREEN FROM backgrnd

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)
		SHOW POPUP mainmenu
		SHOW POPUP AtRegVew1


	CASE BAR() = 6


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL

		PUBLIC mMenu
		mMenu = 3
		DO AskPw

		IF Permit = "OK"
			DO tattend1.prg
		ENDIF
		
		*RESTORE SCREEN FROM backgrnd

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)
		SHOW POPUP mainmenu
		SHOW POPUP AtRegVew1



	CASE BAR() = 11

		DEACTIVATE POPUP AtRegVew1

	OTHERWISE 

		?? CHR(7)

ENDCASE

RETURN
***********************


**************************************
PROCEDURE rptch
**************************************

DO CASE

	CASE BAR() = 3

		ACTIVATE POPUP rpt1menu

	CASE BAR() = 4

		ACTIVATE POPUP rpt2menu

	CASE BAR() = 5

		ACTIVATE POPUP rpt3menu


	CASE BAR() = 16

		DEACTIVATE POPUP rptchoice

	OTHERWISE 

		?? CHR(7)

ENDCASE

RETURN
***********************




**************************************
PROCEDURE rpt1mnu
**************************************

DO CASE

	CASE BAR() = 3


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 9
		DO AskPw

		IF Permit = "OK"
		DO SheetSal
		ENDIF
		

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)

		RESTORE SCREEN FROM backgrnd
		SHOW POPUP mainmenu
		SHOW POPUP rptchoice
		SHOW POPUP rpt1menu		

	CASE BAR() = 4


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 9
		DO AskPw

		IF Permit = "OK"
		DO SheetWag
		ENDIF
		

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)

		RESTORE SCREEN FROM backgrnd
		SHOW POPUP mainmenu
		SHOW POPUP rptchoice
		SHOW POPUP rpt1menu		
	
	CASE BAR() = 5
	

        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 9
		DO AskPw

		IF Permit = "OK"
		DO SheetOt
		ENDIF
				

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)

		RESTORE SCREEN FROM backgrnd
		SHOW POPUP mainmenu
		SHOW POPUP rptchoice
		SHOW POPUP rpt1menu		



	CASE BAR() = 16

		DEACTIVATE POPUP rpt1menu

	OTHERWISE 

		?? CHR(7)

ENDCASE

RETURN
*********************

**************************************
PROCEDURE rpt2mnu
**************************************

DO CASE

	CASE BAR() = 3


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 9
		DO AskPw

		IF Permit = "OK"
		DO LvRpt1.prg
		ENDIF
		

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)

		RESTORE SCREEN FROM backgrnd
		SHOW POPUP mainmenu
		SHOW POPUP rptchoice
		SHOW POPUP rpt2menu		

	CASE BAR() = 4


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 9
		DO AskPw

		IF Permit = "OK"
		DO AtDetSht.prg
		ENDIF
		

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)

		RESTORE SCREEN FROM backgrnd
		SHOW POPUP mainmenu
		SHOW POPUP rptchoice
		SHOW POPUP rpt2menu		



	CASE BAR() = 16

		DEACTIVATE POPUP rpt2menu

	OTHERWISE 

		?? CHR(7)

ENDCASE

RETURN
**************


**************************************
PROCEDURE rpt3mnu
**************************************

DO CASE

	CASE BAR() = 3


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 9
		DO AskPw

		IF Permit = "OK"
		DO AtSheet.prg
		ENDIF
		

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)

		RESTORE SCREEN FROM backgrnd
		SHOW POPUP mainmenu
		SHOW POPUP rptchoice
		SHOW POPUP rpt3menu		

	CASE BAR() = 4


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 9
		DO AskPw

		IF Permit = "OK"
		DO AtSheet2.prg
		ENDIF
		

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)

		RESTORE SCREEN FROM backgrnd
		SHOW POPUP mainmenu
		SHOW POPUP rptchoice
		SHOW POPUP rpt3menu		


	CASE BAR() = 5


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 9
		DO AskPw

		IF Permit = "OK"
		DO AtSheet3.prg
		ENDIF
		

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)

		RESTORE SCREEN FROM backgrnd
		SHOW POPUP mainmenu
		SHOW POPUP rptchoice
		SHOW POPUP rpt3menu		


	CASE BAR() = 6


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 9
		DO AskPw

		IF Permit = "OK"
		DO AtSheet4.prg
		ENDIF
		

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)

		RESTORE SCREEN FROM backgrnd
		SHOW POPUP mainmenu
		SHOW POPUP rptchoice
		SHOW POPUP rpt3menu		



	CASE BAR() = 16

		DEACTIVATE POPUP rpt3menu

	OTHERWISE 

		?? CHR(7)

ENDCASE

RETURN
**************






********************
PROCEDURE SysSec
********************

DO CASE

	CASE BAR() = 3


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 7
		DO AskPw

		IF Permit = "OK"
		DO Encr.prg
		ENDIF
		

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)

		RESTORE SCREEN FROM backgrnd
		SHOW POPUP mainmenu
		SHOW POPUP SysSec1

	CASE BAR() = 16

		DEACTIVATE POPUP SysSec1

	OTHERWISE 

		?? CHR(7)

ENDCASE

RETURN
***************************




********************
PROCEDURE SysMain
********************

DO CASE

	CASE BAR() = 3


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 5
		DO AskPw

		IF Permit = "OK"
		DO Ridx-All.prg
		ENDIF
		

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)



		IF _DOS
		RESTORE SCREEN FROM backgrnd
		ENDIF

		SHOW POPUP mainmenu
		SHOW POPUP SysMain1

	CASE BAR() = 4


        on key label rightarrow 
        on key label LEFTarrow 
		HIDE POPUP ALL
				
		PUBLIC mMenu
		mMenu = 5
		DO AskPw

		IF Permit = "OK"
		DO PstLeavs.prg
		ENDIF
		

        on key label rightarrow ?? CHR(7)
        on key label LEFTarrow ?? CHR(7)

		RESTORE SCREEN FROM backgrnd
		SHOW POPUP mainmenu
		SHOW POPUP SysMain1



	CASE BAR() = 16

		DEACTIVATE POPUP SysMain1

	OTHERWISE 

		?? CHR(7)

ENDCASE

RETURN
***************************








********************
PROCEDURE BackGrnd
*******************

**********************************************************************

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º 11/18/95            BACKGRND.SPR               17:42:11 º
*       º                                                         º
*       ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*       º                                                         º
*       º Author's Name                                           º
*       º                                                         º
*       º Copyright (c) 1995 Company Name                         º
*       º Address                                                 º
*       º City,     Zip                                           º
*       º                                                         º
*       º Description:                                            º
*       º This program was automatically generated by GENSCRN.    º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½


#REGION 0
REGIONAL m.currarea, m.talkstat, m.compstat

IF SET("TALK") = "ON"
	SET TALK OFF
	m.talkstat = "ON"
ELSE
	m.talkstat = "OFF"
ENDIF
m.compstat = SET("COMPATIBLE")
SET COMPATIBLE FOXPLUS

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º                MS-DOS Window definitions                º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*


*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º              BACKGRND/MS-DOS Screen Layout              º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*

#REGION 1
@ 24,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 1,0 SAY "°°" ;
	SIZE 1,2, 0
@ 1,2 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,75, 0
@ 2,0 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,58, 0
@ 3,0 SAY "°°" ;
	SIZE 1,2, 0
@ 3,2 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,75, 0
@ 4,0 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,58, 0
@ 5,0 SAY "°°" ;
	SIZE 1,2, 0
@ 5,2 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,75, 0
@ 6,0 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,58, 0
@ 7,0 SAY "°°" ;
	SIZE 1,2, 0
@ 7,2 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,75, 0
@ 8,0 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,58, 0
@ 9,0 SAY "°°" ;
	SIZE 1,2, 0
@ 9,2 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,75, 0
@ 10,0 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,58, 0
@ 11,0 SAY "°°" ;
	SIZE 1,2, 0
@ 11,2 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,75, 0
@ 12,0 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,58, 0
@ 13,0 SAY "°°" ;
	SIZE 1,2, 0
@ 13,2 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,75, 0
@ 14,0 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,58, 0
@ 15,0 SAY "°°" ;
	SIZE 1,2, 0
@ 15,2 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,75, 0
@ 16,0 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,58, 0
@ 17,0 SAY "°°" ;
	SIZE 1,2, 0
@ 17,2 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,75, 0
@ 18,0 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,58, 0
@ 19,0 SAY "°°" ;
	SIZE 1,2, 0
@ 19,2 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,75, 0
@ 0,58 SAY "°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,19, 0
@ 2,58 SAY "°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,19, 0
@ 4,58 SAY "°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,19, 0
@ 6,58 SAY "°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,19, 0
@ 8,58 SAY "°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,19, 0
@ 10,58 SAY "°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,19, 0
@ 12,58 SAY "°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,19, 0
@ 14,58 SAY "°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,19, 0
@ 16,58 SAY "°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,19, 0
@ 18,58 SAY "°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,19, 0
@ 20,0 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,77, 0
@ 21,0 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,77, 0
@ 22,0 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,77, 0
@ 23,1 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,77, 0
@ 0,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 1,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 2,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 3,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 4,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 5,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 6,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 7,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 8,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 9,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 10,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 11,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 12,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 13,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 14,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 15,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 16,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 17,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 18,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 19,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 20,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 21,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 22,77 SAY "°°°" ;
	SIZE 1,3, 0
@ 23,77 SAY "°°°" ;
	SIZE 1,3, 0

*updated 21-12-2019

 PUBLIC mCdir,mDrive
 STORE sys(5) TO mDrive
 STORE curdir() TO mCdir

@ 22,22 SAY ALLTRIM(mDrive)+ALLTRIM(mCdir)+" - "+"General 2019" ;
	SIZE 1,35, 0


@ 0,4 SAY "°°°°°°°°°°°° P°°° A°°° Y°°° R°°° O°°° L°°° L°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,72, 0
@ 0,0 SAY "°°" ;
	SIZE 1,2, 0
@ 0,2 SAY "°°" ;
	SIZE 1,2, 0
@ 24,0 SAY "°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°°" ;
	SIZE 1,77, 0
@ 23,0 SAY "°" ;
	SIZE 1,1, 0






#REGION 0
IF m.talkstat = "ON"
	SET TALK ON
ENDIF
IF m.compstat = "ON"
	SET COMPATIBLE ON
ENDIF


RETURN
**************************




********************************
* Prorgram: UPDEMPSR.PRG
* To Update empsrl Field in all tables
* To Update name Field in all tables
* THIS UPDATE: 11-JUL-2017
*******************************
******************
PROCEDURE UPDEMPSR
******************

CLEAR
CLEAR ALL
CLOSE ALL

PUBLIC mMcode, mYear
mMcode = 0
mYear = 0

USE employee EXCL
REPLACE ALL empsrl WITH RECNO()
USE

USE employee
GOTO TOP

SCAN
	repliYear=REPLICATE('0',4-LEN(LTRIM(STR(year))))+LTRIM(STR(year))
	repliMcode = REPLICATE('0',2-LEN(LTRIM(STR(mcode))))+LTRIM(STR(mcode))
	repliDeptcode=REPLICATE('0',2-LEN(LTRIM(STR(deptcode))))+LTRIM(STR(deptcode))
	repliEmpNo = REPLICATE('0',3-LEN(LTRIM(STR(empno))))+LTRIM(STR(empno))
	repliEmpSrl=REPLICATE('0',12-LEN(LTRIM(STR(empsrl))))+LTRIM(STR(empsrl))
	mEmpID = repliYear + repliMcode + repliDeptcode + repliEmpNo + repliEmpSrl
	REPLACE EmpID WITH mEmpID
ENDSCAN
USE

***********************************************************

SELECT 1
USE employee EXCL
INDEX ON STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) TAG empno
SET ORDER TO empno

SELECT 2
USE salary EXCL
INDEX ON STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) TAG empno
SET ORDER TO empno

SELECT salary
SET RELATION TO STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) INTO employee ADDITIVE
REPLACE ALL salary.empsrl WITH employee.empsrl
REPLACE ALL salary.name WITH employee.name
REPLACE ALL salary.empid WITH employee.empid


SELECT 3
USE attend EXCL
INDEX ON STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) TAG empno
SET ORDER TO empno

mMcode = attend.mcode
mYear = attend.year


SELECT attend
SET RELATION TO STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) INTO employee ADDITIVE
REPLACE ALL attend.empsrl WITH employee.empsrl
REPLACE ALL attend.name WITH employee.name
REPLACE ALL attend.empid WITH employee.empid

SELECT 4
USE artime EXCL
INDEX ON STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) TAG empno
SET ORDER TO empno

SELECT artime
SET RELATION TO STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) INTO employee ADDITIVE
REPLACE ALL artime.empsrl WITH employee.empsrl
REPLACE ALL artime.name WITH employee.name
REPLACE ALL artime.empid WITH employee.empid

SELECT 5
USE ovtime EXCL
INDEX ON STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) TAG empno
SET ORDER TO empno

SELECT ovtime
SET RELATION TO STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) INTO employee ADDITIVE
REPLACE ALL ovtime.empsrl WITH employee.empsrl
REPLACE ALL ovtime.name WITH employee.name
REPLACE ALL ovtime.empid WITH employee.empid

SELECT 6
USE dptime EXCL
INDEX ON STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) TAG empno
SET ORDER TO empno

SELECT dptime
SET RELATION TO STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) INTO employee ADDITIVE
REPLACE ALL dptime.empsrl WITH employee.empsrl
REPLACE ALL dptime.name WITH employee.name
REPLACE ALL dptime.empid WITH employee.empid

SELECT 7
USE deduct EXCL
INDEX ON STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) TAG empno
SET ORDER TO empno

SELECT deduct
SET FILTER TO
SET FILTER TO mcode = mMcode AND year = mYear
SET RELATION TO STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) INTO employee ADDITIVE
REPLACE ALL deduct.empsrl WITH employee.empsrl
REPLACE ALL deduct.name WITH employee.name
REPLACE ALL deduct.empid WITH employee.empid

SELECT 8
USE saldtail EXCL
INDEX ON STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) TAG empno
SET ORDER TO empno

SELECT saldtail
SET FILTER TO
SET FILTER TO mcode = mMcode AND year = mYear
SET RELATION TO STR(DEPTCODE)+STR(EMPNO)+STR(MCODE)+STR(YEAR) INTO employee ADDITIVE
REPLACE ALL saldtail.empsrl WITH employee.empsrl
REPLACE ALL saldtail.name WITH employee.name
REPLACE ALL saldtail.empid WITH employee.empid

CLEAR
CLEA ALL
CLOSE ALL
RETURN
**********************************