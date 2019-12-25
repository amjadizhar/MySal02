SET TALK OFF
SET SAFETY OFF

CLEAR
CLOSE DATA

@ 1,25 SAY "Report: Employee Leaves Record"
@ 2,25 SAY "------------------------------"

PUBLIC mMnth,mMcode,mYear,Vp
mMnth = space(10)
mMcode = 0
mYear = 0
Vp = space(1)

SELECT 1
USE leaves

SELECT 2
USE attendat ORDER empsrl

SELECT 3
USE attend ORDER empsrl
STORE mcode TO mMcode
STORE year TO mYear


SELECT 4
USE month ORDER mcode

SELECT 5
USE deptt ORDER deptcode

SELECT 3
SET RELATION TO mcode INTO month
STORE month.month TO mMnth


SELECT 1
SET RELATION TO empsrl INTO attendat ADDITIVE
SET RELATION TO empsrl INTO attend ADDITIVE
SET RELATION TO deptcode INTO deptt ADDITIVE


SET SKIP TO attendat

DO AskEmpno

@ 12,5 SAY "Report to Printer or View (P/V) ? "
@ ROW(),COL() GET Vp PICT "@!" VALID INLIST(Vp,'P','V');
	   ERROR "(P)rinter,(V)iew "
READ

DO CASE

	CASE Vp = "P"
		report form lvrpt1 TO PRINTER NOEJECT;
		 for deptcode = mDptCode and empno = mEmpNo	

	CASE Vp = "V"
		report form lvrpt1 preview;
		 for deptcode = mDptCode and empno = mEmpNo

ENDCASE

CLOSE DATA



*******************
PROCEDURE AskEmpno
*******************
PUBLIC mDptCode,mEmpNo

mDptCode = 0
mEmpNo = 0


@ 5,5 SAY "Enter Empno for which you want Leave Record ? "
@ ROW(),COL() GET mDptCode PICT "99" VALID VmDptCode() 
@ ROW(),COL() SAY "-"
@ ROW(),COL() GET mEmpNo PICT "999" VALID VmEmpNo()
READ

RETURN
*************************

********************
PROCEDURE VmDptCode
********************

SELECT deptt
LOCATE FOR deptcode = mDptCode
IF FOUND()
	@ 8,0 TO 8,79 CLEAR
	@ 8,5 SAY "Department Name: "
	@ ROW(),COL() SAY deptname
	SELECT 1
	RETURN .T.
ELSE
	@ 8,0 TO 8,79 CLEAR
	WAIT WINDOW "Deptt.Code Not in File! Try Again" TIMEOUT 1
	SELECT 1
	RETURN .F.
ENDIF
SELECT 1
RETURN .T.
*********************


******************
PROCEDURE VmEmpNo
******************

SELECT 1
LOCATE FOR deptcode = mDptCode AND empno = mEmpNo

DO CASE

	CASE FOUND()
		
		@ 10,0 TO 10,79 CLEAR
		@ 10,5 SAY "Employee Name: "
		@ ROW(),COL() SAY name

	CASE NOT FOUND()
		@ 10,0 TO 10,79 CLEAR
		WAIT WINDOW "Employee Code Not in File! Try Again" TIMEOUT 1
		SELECT 1
		_CUROBJ = 1
		RETURN .T.
ENDCASE

SELECT 1

RETURN .T.
*************
