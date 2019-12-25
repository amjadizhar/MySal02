* Program SALVIEW.PRG
* To see in cursor the Salary View

DO IniSet


***********************************************************************
PROCEDURE iniset							&& Initial Settings

***********************************************************************

CLEAR


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

SELECT 1
USE attend
SET ORDER TO TAG empno OF attend.cdx

SELECT 2
USE employee
SET ORDER TO TAG empno OF employee.cdx

SELECT 3
USE deptt
SET ORDER TO TAG deptcode OF deptt.cdx

SELECT 4
USE artime
SET ORDER TO TAG empno OF artime.cdx

SELECT 5
USE ovtime
SET ORDER TO TAG empno OF ovtime.cdx

SELECT 6
USE month
SET ORDER TO TAG mcode OF month.cdx

SELECT 7
USE salary
SET ORDER TO TAG empno OF salary.cdx

SELECT 8
USE dptime
SET ORDER TO TAG empno OF dptime

SELECT 9
USE saldtail ORDER empno

SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
SET RELATION TO deptcode INTO deptt ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO artime ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO ovtime ADDITIVE
SET RELATION TO mcode INTO month ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO salary ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO dptime ADDITIVE
SET RELATION TO str(deptcode)+str(empno)+str(mcode)+str(year) INTO saldtail ADDITIVE

RETURN