* PROGRAM : OPMONTH.PRG
* Programmer : Amjad Izhar
* This program will update Attendat.dbf for
* Opening New Month into current editing database
* and moving the previous data into attendat.dbf
* This Update: 27-Sep-2017

DO MainProg


***********************
PROCEDURE MainProg
***********************
HIDE POPUP mainmenu
CLEAR
DO iniset


DO IniSetVars

DO WHILE Change = .F.
DO GetChoice
ENDDO

IF .NOT. Quiting
CLEAR

DO iniset
@ 10,30 SAY "Please Wait ..."

DO AtnData
DO ArtData
DO DptData
DO OvtData
DO SlryData

ENDIF



IF MopenNew = "N"
	DO DelCurData
	DO CrFlsNMnth
ENDIF

IF MopenNew = "P"
	DO DelCurData
	DO CrFlsPMnth
ENDIF


DO EndProg
RETURN
************************
****************************
PROCEDURE IniSetVars
****************************


PUBLIC MOpenNew,Quiting,DexistAt,DexistAr,DexistDp,DexistOv,CursorOn,;
		Change,mNewMnth,mNewYear,LastSheet,SltdMcode,SltdMonth,SltdYear
		
Change = .F.

RETURN
*****************************



***************************************************************
PROCEDURE GetChoice  && get choice whether to open new month or
					 && to open previous month
***************************************************************

					 
PUBLIC MOpenNew,Quiting,change
MOpenNew = "P"
change = .F.


DEFINE WINDOW full1 FROM 0,0 TO 24,79 COLOR W/B
ACTIVATE WINDOW full1

@ 6,2,10,45 BOX

@ 7,4 SAY "Select whether to open a New Month or "
@ 8,4 SAY "You want to open a previous month."
@ 9,4 SAY "(N)ew,(P)revious or (Q)uit (N/P/Q)"

@ 15,4 SAY "Do you want to open a New Month?  (N/P/Q) ";
	GET MOpenNew PICT "@!" VALID INLIST(MOpenNew,'N','P','Q');
	ERROR "(N)ew,(P)revious,(Q)uit (N/P/Q)"

READ

LastKey = LASTKEY()

IF LastKey = 27
		CLEAR
		Change = .T.
		Quiting = .T.
		MOpenNew = "Q"
		DO EndProg
ENDIF


DO CASE
	CASE UPPER(MOpenNew) = "N"
		CLEAR
		Quiting = .F.
		DO iniset
		DO AskNew

	CASE UPPER(MOpenNew) = "P"	
		CLEAR
		Quiting = .F.
		DO iniset
		DO AskMonth && Ask about the previous month

	CASE UPPER(MOpenNew) = "Q"	
		CLEAR
		Change = .T.
		Quiting = .T.
		DO EndProg

ENDCASE

RETURN


*****************
PROCEDURE AtnData
*****************

DO iniset
DO chkatnd
DO UpdAtnDat

RETURN
*****************

*****************
PROCEDURE ArtData
*****************

DO iniset
DO ChkArtime
DO UpdArtDat

RETURN
*****************

*****************
PROCEDURE DptData
*****************

DO iniset
DO ChkDptime
DO UpdDptDat

RETURN
*****************

*****************
PROCEDURE OvtData
*****************

DO iniset
DO ChkOvtime
DO UpdOvtDat

RETURN
******************


*****************
PROCEDURE SlryData
*****************

DO iniset
DO ChkSlryDat
DO UpdSlryDat

RETURN
******************




***********************************************************************



***********************************************************************
PROCEDURE iniset							&& Initial Settings

***********************************************************************



CLOSE DATABASES

CLEAR MACROS
ON KEY LABEL F1 ?? CHR(7)
SET FUNCTION F2 TO
SET FUNCTION F3 TO
SET FUNCTION F4 TO
SET FUNCTION F5 TO
SET FUNCTION F6 TO
SET FUNCTION F7 TO
SET FUNCTION F8 TO
SET FUNCTION F9 TO
ON KEY LABEL F10 ?? CHR(7)
SET FUNCTION F11 TO
SET FUNCTION F12 TO


SET SYSMENU OFF
SET TALK OFF
SET SAFETY OFF
SET EXCLUSIVE ON
RETURN

**********************************
PROCEDURE chkatnd  && Check if data is in attendat.dbf file
**********************************


PUBLIC DexistAt

SET EXCLUSIVE ON

SELECT 1
USE attend
SET ORDER TO empno

SELECT 2
USE attendat
SET ORDER TO empno

SELECT 1
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO attendat ADDITIVE

IF mcode = attendat.mcode .AND. year = attendat.year
DExistAt = .T.


ELSE
DexistAt = .F.
ENDIF


RETURN
************************************************




************************************************
PROCEDURE UpdAtnDat  && Update Attendat.dbf file
************************************************





DO CASE

CASE DExistAt

	select attend
	DatMonth = str(mcode)+str(year)
	select attendat
	delete for str(mcode)+str(year) = DatMonth
	PACK
	APPEND FROM attend

CASE .NOT. DExistAt
	
	
	CLOSE DATABASES
	USE attendat
	APPEND FROM attend

ENDCASE
RETURN
**************************************************************

**********************************
PROCEDURE ChkArtime  && Check if data is in attendat.dbf file
**********************************


PUBLIC DexistAr

SET EXCLUSIVE ON

SELECT 1
USE artime
SET ORDER TO TAG empno OF artime.cdx

SELECT 2
USE artimdat
SET ORDER TO TAG empno OF artimdat.cdx

SELECT 1
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO artimdat ADDITIVE

IF mcode = artimdat.mcode .AND. year = artimdat.year
DExistAr = .T.


ELSE
DexistAr = .F.


ENDIF


RETURN
************************************************




************************************************
PROCEDURE UpdArtDat  && Update Attendat.dbf file
************************************************





DO CASE

CASE DExistAr

	select artime
	DatMonth = str(mcode)+str(year)
	select artimdat
	delete for str(mcode)+str(year) = DatMonth
	PACK
	APPEND FROM artime

CASE .NOT. DExistAr
	
	
	CLOSE DATABASES
	USE artimdat
	APPEND FROM artime

ENDCASE
RETURN
**************************************************************


**********************************
PROCEDURE ChkDptime  && Check if data is in dptimdat.dbf file
**********************************


PUBLIC DexistDp

SET EXCLUSIVE ON

SELECT 1
USE dptime
SET ORDER TO TAG empno OF dptime.cdx

SELECT 2
USE dptimdat
SET ORDER TO TAG empno OF dptimdat.cdx

SELECT 1
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO dptimdat ADDITIVE

IF mcode = dptimdat.mcode .AND. year = dptimdat.year
DExistDp = .T.


ELSE
DexistDp = .F.


ENDIF


RETURN
************************************************




************************************************
PROCEDURE UpdDptDat  && Update dptimdat.dbf file
************************************************





DO CASE

CASE DExistDp

	select dptime
	DatMonth = str(mcode)+str(year)
	select dptimdat
	delete for str(mcode)+str(year) = DatMonth
	PACK
	APPEND FROM dptime

CASE .NOT. DExistDp
	
	
	CLOSE DATABASES
	USE dptimdat
	APPEND FROM dptime

ENDCASE
RETURN
**************************************************************


**********************************
PROCEDURE ChkOvtime  && Check if data is in ovtimdat.dbf file
**********************************


PUBLIC DexistOv

SET EXCLUSIVE ON

SELECT 1
USE ovtime
SET ORDER TO TAG empno OF ovtime.cdx

SELECT 2
USE ovtimdat
SET ORDER TO TAG empno OF ovtimdat.cdx

SELECT 1
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO ovtimdat ADDITIVE


IF mcode = ovtimdat.mcode .AND. year = ovtimdat.year
DExistOv = .T.


ELSE
DexistOv = .F.


ENDIF


RETURN
************************************************




************************************************
PROCEDURE UpdOvtDat  && Update dptimdat.dbf file
************************************************




DO CASE

CASE DExistOv

	select ovtime
	DatMonth = str(mcode)+str(year)
	select ovtimdat
	delete for str(mcode)+str(year) = DatMonth
	PACK
	APPEND FROM ovtime

CASE .NOT. DExistOv
	
	
	CLOSE DATABASES
	USE ovtimdat
	APPEND FROM ovtime

ENDCASE
RETURN
**************************************************************


**********************************
PROCEDURE ChkSlryDat  && Check if data is in Slrydat.dbf file
**********************************


PUBLIC DexistSldt

SET EXCLUSIVE ON

SELECT 1
USE salary
SET ORDER TO TAG empno OF salary.cdx

SELECT 2
USE Slrydat
SET ORDER TO TAG empno OF Slrydat.cdx

SELECT 1
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO Slrydat ADDITIVE


IF mcode = Slrydat.mcode .AND. year = Slrydat.year
DexistSldt = .T.


ELSE
DexistSldt = .F.


ENDIF


RETURN
************************************************



************************************************
PROCEDURE UpdSlryDat  && Update dptimdat.dbf file
************************************************

DO CASE

CASE DexistSldt

	select Salary
	DatMonth = str(mcode)+str(year)
	select SlryDat
	delete for str(mcode)+str(year) = DatMonth
	PACK
	APPEND FROM Salary

CASE .NOT. DExistOv
	
	CLOSE DATABASES
	USE SlryDat
	APPEND FROM Salary

ENDCASE
RETURN
**************************************************************




********************************
PROCEDURE AskMonth
********************************
PUBLIC CursorOn
Change = .F.

DEFINE WINDOW full1 FROM 0,0 TO 24,79 COLOR W/B
ACTIVATE WINDOW full1

@ 5,2,10,40 BOX
@ 6,4 SAY "You can move in the list"
@ 7,4 SAY "with Up and Down Arrow keys."
@ 8,4 SAY "Press <F10> to open the month."


SET EXCLUSIVE ON


SELECT 1
USE salsheet && ORDER mcode

SELECT 2
USE month ORDER mcode

SELECT 3
USE attend ORDER mcode
STORE str(mcode)+str(year) TO CursorOn

SELECT 1
SET RELATION TO mcode INTO month ADDITIVE
SET RELATION TO mcode INTO attend ADDITIVE

LOCATE FOR str(mcode)+str(year) = CursorOn

DEFINE WINDOW BrSalSht FROM 6,45 TO 20,70 COLOR N/G


ON KEY LABEL F10 DO SltdMonth

BROW FIELDS;
month.month,;
year;
FREEZE month.month;
WINDOW BrSalSht;
WHEN SelInf();
TITLE "Month to Open";
NOEDIT NOAPPEND NODELETE

clear
DEACTIVATE WINDOW full1

RETURN


***************************
PROCEDURE SltdMonth  && F10 key process
***************************
Change = .T.

PUBLIC SltdMcode,SltdMonth,SltdYear

STORE mcode TO SltdMcode
STORE month.month TO SltdMonth
STORE year TO SltdYear

IF mcode = attend.mcode .AND. year = attend.year
?? CHR(7)
@ 18,3 SAY space(42)
@ 18,3 SAY "Current Month Already Opened !!!" COLOR N/R
change = .F.
RETURN
ENDIF

close all
clear
DEACTIVATE WINDOW full1
Change = .T.

RETURN

*******************************
PROCEDURE SelInf
*******************************
Change = .F.
@ 18,3 SAY space(42)
@ 15,3 SAY space(42)
@ 16,3 SAY space(42)
@ 15,3 SAY "You have selected "

@ 16,3 SAY ALLTRIM(month.month)+","+ALLTRIM(STR(YEAR)) COLOR GR+/B

IF mcode = attend.mcode .AND. year = attend.year
?? CHR(7)
@ 18,3 SAY space(42)
@ 18,3 SAY "Current Month Opened"
ENDIF


RETURN .T.



********************************

PROCEDURE AskNew  && Ask If to Open new month
*********************************************
PUBLIC CursorOn,Change,mNewMnth,mNewYear
Change = .F.


DEFINE WINDOW full1 FROM 0,0 TO 24,79 COLOR W/B
ACTIVATE WINDOW full1

DEFINE WINDOW outer FROM 2,39 TO 13,71;
	TITLE "This Month already Exists";
	FOOTER "Press <Esc> to exit" COLOR N/R
	
DEFINE WINDOW BrMonth FROM 3,40 TO 12,70 COLOR N/R

SET EXCLUSIVE ON


SELECT 1
USE salsheet ORDER mcode
SCATTER MEMVAR BLANK

SELECT 2
USE month ORDER mcode

SELECT 3
USE attend ORDER mcode
STORE str(mcode)+str(year) TO CursorOn

SELECT 1
SET RELATION TO mcode INTO month ADDITIVE
SET RELATION TO mcode INTO attend ADDITIVE

mobject = _curobj

@ 2,3 SAY "Attendence New Month Opening"
@ 6,2 SAY "Enter new month to open (1-12): "
@ 6,37 GET m.mcode;
	RANGE 1,12;
	PICT "99"


@ 8,2 SAY "Enter Year : "
@ 8,35 GET m.year;
	PICT "9999";
	VALID ChkNewMnth()

READ	
LastKey = LASTKEY()

IF LastKey = 27
Change = .F.
RETURN
ENDIF

STORE m.mcode TO mNewMnth	
STORE m.year TO mNewYear

RETURN

***************************************************	
PROCEDURE ChkNewMnth && Valid in GET m.mcode+m.year
***************************************************
Change = .T.

DO CASE
CASE m.mcode = 0
	Change = .F.
CASE m.year = 0
	Change = .F.
CASE m.mcode = 0 .AND. m.year = 0
	Change = .F.
ENDCASE




SELECT 1

LOCATE FOR m.mcode = mcode .AND. m.year = year

IF FOUND()

	ACTIVATE WINDOW outer
	
	BROWSE FIELDS;
	mcode,;
	month.month,;
	year;
	WINDOW BrMonth;
	TITLE "in file. ";
	NOEDIT NOAPPEND NODELETE

	SCATTER MEMVAR BLANK
	_curobj = 1
	DEACTIVATE WINDOW outer


	RETURN 0
ENDIF

RETURN .T.
*****************	

************************************
PROCEDURE DelCurData
************************************



CLOSE DATABASES

SET EXCLUSIVE ON
USE attend
ZAP
USE artime
ZAP
USE dptime
ZAP
USE ovtime
ZAP
CLOSE DATABASES

RETURN
************************************



**************************************
PROCEDURE CrFlsNMnth && Create Files for New Month
**************************************

PUBLIC LastSheet
CLOSE DATABASES

USE empdata EXCL
APPEND FROM employee
USE
USE employee EXCL
REPL ALL mcode WITH mNewMnth
REPL ALL year WITH mNewYear
USE


USE Salsheet ORDER Sheetno
GOTO BOTTOM
STORE sheetno TO LastSheet
APPEND BLANK
REPLACE sheetno WITH LastSheet+1
REPLACE mcode WITH mNewMnth
REPLACE year WITH mNewYear

CLOSE DATABASES
SET EXCLUSIVE ON

SELECT 1
USE attend ORDER empno2

SELECT 2
USE employee ORDER empno

SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE

APPEND FROM employee FOR employee.working
REPLACE ALL mcode WITH mNewMnth
REPLACE ALL year WITH mNewYear

DO Friday
CLOSE DATABASES

USE artime
APPEND FROM attend

USE dptime
APPEND FROM attend

USE ovtime
APPEND FROM attend

USE deduct
APPEND FROM attend
REPLACE ALL sheetno WITH LastSheet+1 FOR mcode = mNewMnth AND year = mNewYear

USE saldtail
APPEND FROM attend
REPLACE ALL sheetno WITH LastSheet+1 FOR mcode = mNewMnth AND year = mNewYear

CLOSE DATABASES


USE salary
REPLACE ALL mcode WITH mNewMnth
REPLACE ALL year WITH mNewYear


USE EmpData EXCL
APPEND FROM employee
USE

USE employee EXCL
REPL ALL mcode WITH mNewMnth
REPL ALL year WITH mNewYear
USE




RETURN
**************************************

******************************
PROCEDURE CrFlsPMnth
******************************
CLOSE DATABASES


SELECT 1
USE attend
APPEND FROM attendat FOR mcode = SltdMcode .AND. year = SltdYear
STORE mcode TO mMcode
STORE year TO mYear


SELECT 2
USE month ORDER mcode

SELECT 3
USE employee ORDER empno

SELECT 4
USE salary ORDER empno2


SELECT 1
SET RELATION TO mcode INTO month ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO salary ADDITIVE


SELECT 1

SCAN
Pal = d1+d2+d3+d4+d5+d6+d7+d8+d9+d10+d11+d12+d13+d14+d15+;
	  d16+d17+d18+d19+d20+d21+d22+d23+d24+d25+d26+d27+d28+;
	  d29+d30+d31

REPLACE present WITH OCCURS("P",Pal)
REPLACE absent WITH OCCURS("A",Pal)
REPLACE leave WITH OCCURS("L",Pal)
REPLACE late WITH OCCURS("T",Pal)

PUBLIC mPblDysS,mPblDysW,mPblDysP
mPblDysS = 0
mPblDysW = 0
mPblDysP = 0

IF present <> 0
mPblDysS = month.mdays-absent
mPblDysW = present+late
mPblDysP = month.mdays-absent
ENDIF

DO CASE
CASE salary.mpay AND NOT employee.probation
	
	REPLACE pbldays WITH mPblDysS

CASE salary.mpay AND employee.probation
	
	REPLACE pbldays WITH mPblDysP

CASE salary.mpay = .f. ;
	AND salary.wrdaily = .f. AND NOT employee.probation;
	AND NOT EMPTY(salary.dwratem)
	
	REPLACE pbldays WITH mPblDysS

CASE salary.mpay = .f. ;
	AND salary.wrdaily = .f. AND employee.probation;
	AND NOT EMPTY(salary.dwratem)

	REPLACE pbldays WITH mPblDysP

OTHERWISE

	REPLACE pbldays WITH mPblDysW

ENDCASE


ENDSCAN


CLOSE DATABASES
SET EXCLUSIVE ON

USE artime
APPEND FROM artimdat FOR mcode = SltdMcode .AND. year = SltdYear

USE dptime
APPEND FROM dptimdat FOR mcode = SltdMcode .AND. year = SltdYear

USE ovtime
APPEND FROM ovtimdat FOR mcode = SltdMcode .AND. year = SltdYear

USE salary
ZAP
APPEND FROM SlryDat FOR mcode = SltdMcode .AND. year = SltdYear


CLOSE DATABASES
RETURN
*******************************




**************************************
PROCEDURE EndProg
**************************************

CLEAR
CLEAR WINDOWS
CLOSE DATABASES

SHOW POPUP mainmenu

IF _DOS
RESTORE SCREEN FROM backgrnd
ENDIF

SET EXCLUSIVE OFF
RETURN
**************************************

