* General Maintenance Procedure in DBF files
* Programmer: Amjad Izhar
* This revision 04-03-2017
********************************************************

**********************************
PROCEDURE EmpName
**********************************
*Payroll Maintainance of DBF files.

SET TALK OFF
SET SAFE OFF
SET CONFIRM OFF

SELECT 1
USE saldtail EXCL
INDEX ON STR(deptcode)+STR(empno)+STR(mcode)+STR(year) TAG empno
SET ORDER TO empno

SELECT 2
USE employee EXCL
INDEX ON STR(deptcode)+STR(empno)+STR(mcode)+STR(year) TAG empno
SET ORDER TO empno

SELECT 1
SET RELATION TO STR(deptcode)+STR(empno)+STR(mcode)+STR(year) INTO employee ADDITIVE

REPL ALL saldtail.name with employee.name

BROW

CLEAR
CLOSE DATA
**********
SELECT 1
USE salary EXCL
INDEX ON STR(deptcode)+STR(empno)+STR(mcode)+STR(year) TAG empno
SET ORDER TO empno

SELECT 2
USE employee EXCL
INDEX ON STR(deptcode)+STR(empno)+STR(mcode)+STR(year) TAG empno
SET ORDER TO empno

SELECT 1
SET RELATION TO STR(deptcode)+STR(empno)+STR(mcode)+STR(year) INTO employee ADDITIVE

REPL ALL salary.name with employee.name

BROW

CLEAR
CLOSE DATA
**********


RETURN
*******************************************