******************************
* PROGRAM: SETUP.PRG
* Program to setup initial Databases and indexes
* also this program searches if any file is missing
* PROGRAMMER : AMJAD IZHAR
*******************************
DO iniset
DO srchfls
DO endprog


PROCEDURE iniset
CLEAR
CLEAR ALL

SET FUNCTION F1 TO
SET FUNCTION F2 TO
SET FUNCTION F3 TO
SET FUNCTION F4 TO
SET FUNCTION F5 TO
SET FUNCTION F6 TO
SET FUNCTION F7 TO
SET FUNCTION F8 TO
SET FUNCTION F9 TO
SET FUNCTION F10 TO
SET FUNCTION F11 TO
SET FUNCTION F12 TO

SET SYSMENU OFF
SET TALK OFF
SET SAFETY OFF

RETURN

***********************************
PROCEDURE srchfls
***********************************
DEFINE WINDOW full FROM 0,0 TO 24,79 DOUBLE
ACTIVATE WINDOW full

@ 0,3 SAY "This Software requires following files existing in current directory"
@ 1,1,1,78 BOX

row1=2
col1=2
col2=16
col3=30
col4=44

@ row1-1,col1+3 SAY "Database Files"
@ row1-1,col3+3 SAY "Index Files"

@ row1-1,col1-1,22,col3-4 BOX
@ row1-1,col3-1,22,col4+10 BOX

@ row1,col1 SAY "ATTEND.DBF"
IF FILE("ATTEND.DBF") = .T.
	@ row1,col2 SAY "Found - Ok"
ENDIF
IF FILE("ATTEND.DBF") = .F.
	@ row1,col2 SAY "Not Found!"
ENDIF


@ row1,col3 SAY "ATTEND.CDX"
IF FILE("ATTEND.CDX") = .T.
	@ row1,col4 SAY "Found - Ok"
ENDIF
IF FILE("ATTEND.CDX") = .F.
	@ row1,col4 SAY "Not Found!"
ENDIF

@ row1+1,col1 SAY "EMPLOYEE.DBF"
IF FILE("EMPLOYEE.DBF") = .T.
	@ row1+1,col2 SAY "Found - Ok"
ENDIF
IF FILE("EMPLOYEE.DBF") = .F.
	@ row1+1,col2 SAY "Not Found!"
ENDIF


@ row1+1,col3 SAY "EMPLOYEE.CDX"
IF FILE("EMPLOYEE.CDX") = .T.
	@ row1+1,col4 SAY "Found - Ok"
ENDIF
IF FILE("EMPLOYEE.CDX") = .F.
	@ row1+1,col4 SAY "Not Found!"
ENDIF

RETURN

PROCEDURE endprog

@ 18,60 GET moption ;
	PICTURE "@*HN \!Ok" ;
	SIZE 1,4,1 ;
	DEFAULT 1
READ

CLEAR WINDOWS
CLEAR ALL
CLEAR
CANCEL
RETURN
