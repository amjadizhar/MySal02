*************************
* PROGRAM: OVTMV2.PRG
* Program to paint a brow for attendence register
* Overtime View
* with complete 31 days
* Programmer: Amjad Izhar
**************************

DO mainprog

**************************
PROCEDURE mainprog
DO iniset
DO SetVars
DO painbrow
DO endprog
RETURN
**************************


*************************
PROCEDURE iniset
*************************
CLEAR
HIDE POPUP mainmenu
SET TALK OFF

ON KEY LABEL F1 DO Ord1
ON KEY LABEL F2 DO Ord2
ON KEY LABEL F3 DO Ord3

SET FUNCTION F4 TO
SET FUNCTION F5 TO
SET FUNCTION F6 TO
SET FUNCTION F7 TO
SET FUNCTION F8 TO
SET FUNCTION F9 TO
SET FUNCTION F10 TO
SET FUNCTION F11 TO
SET FUNCTION F12 TO

*SET RESOURCE OFF
*SET RESOURCE TO pr_rsrc1

PUBLIC mMcode,mYear
mMcode = 0
mYear = 0

SELECT 1
USE ovtime

SELECT 2
USE attend ORDER empno2
STORE mcode TO mMcode
STORE year TO mYear

SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO attend ADDITIVE


REPLACE ALL atord WITH attend.atord

CLOSE DATA


SELECT 1
USE ovtime.dbf
SET ORDER TO TAG empno3 OF ovtime.cdx

SELECT 2
USE employee.dbf
SET ORDER TO TAG empno OF employee.cdx

SELECT 3
USE month
SET ORDER TO TAG mcode OF month.cdx

SELECT 4
USE attend
SET ORDER TO TAG empno2 OF attend.cdx

SELECT 5
USE deptt
SET ORDER TO TAG deptcode OF deptt.cdx

SELECT 6
USE salary
SET ORDER TO empno2

SELECT 7
USE saldtail ORDER empno
SET FILTER TO
SET FILTER TO mcode = mMcode AND year = mYear


SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
SET RELATION TO mcode INTO month ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO attend ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO salary ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year);
				INTO saldtail ADDITIVE
SET RELATION TO deptcode INTO deptt ADDITIVE
goto top

RETURN
*************************


*************************
PROCEDURE SetVars
*************************
PUBLIC CurFld,Brfields1,Brfields2,Brfields3,Brfields4,Brfields5,mRecord
CurFld = space(3)
mRecord = 0

Brfields1 = "D1ot:H='1':P='@Z' :V=U(),;
	D2ot:H='2':P='@Z':V=U(),D3ot:H='3':P='@Z':V=U(),;
	D4ot:H='4':P='@Z':V=U(),D5ot:H='5':P='@Z':V=U(),;
	D6ot:H='6':P='@Z':V=U(),D7ot:H='7':P='@Z':V=U()"

Brfields2 = "D8ot:H='8':P='@Z':V=U(),;
	D9ot:H='9':P='@Z':V=U(),D10ot:H='10':P='@Z':V=U(),;
	D11ot:H='11':P='@Z':V=U(),D12ot:H='12':P='@Z':V=U(),;
	D13ot:H='13':P='@Z':V=U(),D14ot:H='14':P='@Z':V=U()"

		
Brfields3 = "D15ot:H=' 15':P='@Z':V=U(),;
	D16ot:H=' 16':P='@Z':V=U(),D17ot:H=' 17':P='@Z':V=U(),;
	D18ot:H=' 18':P='@Z':V=U(),D19ot:H=' 19':P='@Z':V=U(),;
	D20ot:H=' 20':P='@Z':V=U(),D21ot:H=' 21':P='@Z':V=U()"

Brfields4 = "D22ot:H=' 22':P='@Z':V=U(),;
	D23ot:H=' 23':P='@Z':V=U(),D24ot:H=' 24':P='@Z':V=U(),;
	D25ot:H=' 25':P='@Z':V=U(),D26ot:H=' 26':P='@Z':V=U(),;
	D27ot:H=' 27':P='@Z':V=U(),D28ot:H=' 28':P='@Z':V=U()"

Brfields5 = "D29ot:H='29':P='@Z' :V=U(),;
	D30ot:H='30':P='@Z':V=U(),D31ot:H='31':P='@Z':V=U()"



RETURN
*************************


******************************************
PROCEDURE painbrow
******************************************
PUBLIC Pal


@ 0,0,21,79 BOX
DEFINE WINDOW b FROM 1,1 TO 20,78
mrecord = RECNO()

GOTO mrecord


BROWS FIELDS;
	Eno = str(deptcode,2)+"-"+;
		str(empno,3)+"³";
		:R :H = "Code" :W=.F.,;
	employee.name:R :W=.F.,;
	L1 = "³" :1 :R :H = "" :W=.F.,;
	&Brfields1;
WINDOW b;
WHEN bl();
TITLE "Daily Over Time Register";
FOR salary.otrated <> 0 AND employee.working


BROWS FIELDS;
	Eno = str(deptcode,2)+"-"+;
		str(empno,3)+"³";
		:R :H = "Code" :W=.F.,;
	employee.name:R :W=.F.,;
	L1 = "³" :1 :R :H = "" :W=.F.,;
	&Brfields2;
WINDOW b;
WHEN bl();
TITLE "Daily Over Time Register";
FOR salary.otrated <> 0 AND employee.working


BROWS FIELDS;
	Eno = str(deptcode,2)+"-"+;
		str(empno,3)+"³";
		:R :H = "Code" :W=.F.,;
	employee.name:R :W=.F.,;
	L1 = "³" :1 :R :H = "" :W=.F.,;
	&Brfields3;
WINDOW b;
WHEN bl();
TITLE "Daily Over Time Register";
FOR salary.otrated <> 0 AND employee.working


BROWS FIELDS;
	Eno = str(deptcode,2)+"-"+;
		str(empno,3)+"³";
		:R :H = "Code" :W=.F.,;
	employee.name:R :W=.F.,;
	L1 = "³" :1 :R :H = "" :W=.F.,;
	&Brfields4;
WINDOW b;
WHEN bl();
TITLE "Daily Over Time Register";
FOR salary.otrated <> 0 AND employee.working


BROWS FIELDS;
	Eno = str(deptcode,2)+"-"+;
		str(empno,3)+"³";
		:R :H = "Code" :W=.F.,;
	employee.name:R :W=.F.,;
	L1 = "³" :1 :R :H = "" :W=.F.,;
	&Brfields5;
WINDOW b;
WHEN bl();
TITLE "Daily Over Time Register";
FOR salary.otrated <> 0 AND employee.working


ON KEY
SET INDEX TO ovtime.cdx
SET ORDER TO empno3

RETURN
***************************************
***********************
PROCEDURE bl			&& Browse When clause
***********************

@ 22,2 SAY deptcode
@ 22,4 SAY "-"
@ 22,5 SAY empno PICT "@L"

@ 22,9 SAY employee.salutation+"."
@ 22,12 SAY employee.name
@ 22,35 SAY deptt.deptname

@ 23,9 SAY "P="
@ 23,11 SAY attend.present
@ 23,15 SAY "A="
@ 23,17 SAY attend.absent
@ 23,21 SAY "L="
@ 23,23 SAY attend.leave
@ 23,27 SAY "T="
@ 23,29 SAY attend.late

IF ovtime.totalot <> 0
	@ 23,35 SAY "Tot.OT="
	@ 23,42 SAY ovtime.totalot
ELSE
	@ 23,32 TO 23,48 CLEAR

ENDIF

@ 22,60 SAY ALLTRIM(month.month)+","+ALLTRIM(STR(attend.year));
	COLOR GR+/B

@ 24,9 SAY "F1=Attendence Wise        F2=Emp.No Wise           F3=Name Wise"

RETURN .T.
******************************


****************************
PROCEDURE U
****************************

STORE VARREAD() TO CurFld

movtotal = ovtime.d1ot+ovtime.d2ot+ovtime.d3ot+ovtime.d4ot+ovtime.d5ot+;
		   ovtime.d6ot+ovtime.d7ot+ovtime.d8ot+ovtime.d9ot+ovtime.d10ot+;
		   ovtime.d11ot+ovtime.d12ot+ovtime.d13ot+ovtime.d14ot+;
		   ovtime.d15ot+ovtime.d16ot+ovtime.d17ot+ovtime.d18ot+;
		   ovtime.d19ot+ovtime.d20ot+ovtime.d21ot+ovtime.d22ot+;
		   ovtime.d23ot+ovtime.d24ot+ovtime.d25ot+ovtime.d26ot+;
		   ovtime.d27ot+ovtime.d28ot+ovtime.d29ot+ovtime.d30ot+;
		   ovtime.d31ot
		   
	REPLACE ovtime.totalot WITH movtotal	


REPLACE saldtail.othrs WITH totalot


RETURN .T.
***************************

**************
PROCEDURE Ord1
**************

STORE RECNO() TO mRecord
WAIT WINDOW "Changing Order to Attendence Wise" TIMEOUT 1
SET INDEX TO ovtime.cdx
SET ORDER TO empno3
KEYBOARD '{PGUP}'
KEYBOARD '{PGUP}'
GOTO mRecord

RETURN
********

**************
PROCEDURE Ord2
**************

STORE RECNO() TO mRecord
WAIT WINDOW "Changing Order to Emp. No. Wise" TIMEOUT 1
SET INDEX TO ovtime.cdx
SET ORDER TO empno2
KEYBOARD '{PGUP}'
KEYBOARD '{PGUP}'
GOTO mRecord

RETURN
***********


**************
PROCEDURE Ord3
**************

STORE RECNO() TO mRecord
WAIT WINDOW "Changing Order to Name Wise" TIMEOUT 1

INDEX ON employee.name TO ovName.idx
SET INDEX TO ovName.idx

KEYBOARD '{PGUP}'
KEYBOARD '{PGUP}'
GOTO mRecord

RETURN
***********





************************
PROCEDURE endprog
************************
CLEAR
CLOSE DATABASES
CLEAR WINDOWS
SHOW POPUP mainmenu
*RESTORE SCREEN FROM backgrnd
RETURN
************************