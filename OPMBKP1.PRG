* PROGRAM : OPMONTH.PRG
* Programmer : Amjad Izhar
* This program will update Attendat.dbf for
* Opening New Month into current editing database
* and moving the previous data into attendat.dbf


DO MainProg


***********************
PROCEDURE MainProg
***********************

CLEAR
DO iniset
@ 10,30 SAY "Please Wait ..."

DO AtnData
DO ArtData
DO DptData
DO OvtData

CLEAR
DO iniset
DO AskMonth


RETURN
************************


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
RETURN

**********************************
PROCEDURE chkatnd  && Check if data is in attendat.dbf file
**********************************


PUBLIC DexistAt

SELECT 1
USE attend
SET ORDER TO TAG empno OF attend.cdx

SELECT 2
USE attendat
SET ORDER TO TAG empno OF attendat.cdx

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


********************************
PROCEDURE AskMonth
********************************
PUBLIC CursorOn


DEFINE WINDOW full1 FROM 0,0 TO 24,79 COLOR W/B
ACTIVATE WINDOW full1

@ 5,2,10,40 BOX
@ 6,4 SAY "You can move in the list"
@ 7,4 SAY "with Up and Down Arrow keys."
@ 8,4 SAY "Press <F10> to open the month."



SELECT 1
USE salsheet ORDER mcode

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
TITLE "Month to Open"

clear
DEACTIVATE WINDOW full1

RETURN


***************************
PROCEDURE SltdMonth  && F10 key process
***************************

PUBLIC SltdMcode,SltdMonth,SltdYear
STORE mcode TO SltdMcode
STORE month.month TO SltdMonth
STORE year TO SltdYear

IF mcode = attend.mcode .AND. year = attend.year
?? CHR(7)
@ 18,3 SAY space(42)
@ 18,3 SAY "Current Month Already Opened !!!" COLOR N/R
RETURN
ENDIF

close all
clear
DEACTIVATE WINDOW full1


RETURN

*******************************
PROCEDURE SelInf
*******************************
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
