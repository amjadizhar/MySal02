* This revision : 27-Oct-2018
* Entering new employee

	DO IniSet1
	DO SetVars
	DO OpenDbfs
	DO AskDeptt


IF mGoAhead = .T.
	DO Confirm1
ENDIF

IF mGoAhead = .T.
	DO OpenNemp
	DO NewEmp1
	DO NewEmp2
ENDIF

CLEAR
CLOSE DATA

RETURN
****************
*****************
PROCEDURE IniSet1
*****************

CLEA
CLOSE DATA

SET EXCLUSIVE ON
SET TALK OFF
SET SAFETY OFF

SET FUNCTION F1 TO
SET FUNCTION F2 TO
SET FUNCTION F3 TO
SET FUNCTION F4 TO

SET FUNCTION F5 TO

RETURN
*************

*********************
PROCEDURE SetVars
**********************
PUBLIC mmcode,myear,mNewDpt,mNewEmpNo
mmcode = 0
myear = 0
mNewDpt = 0
mNewEmpNo = 0

RETURN

**********************
PROCEDURE OpenDbfs
**********************
USE attend
STORE mcode TO mmcode
STORE year TO myear
USE

SELECT 1
USE employee ORDER empno

SELECT 2
USE attend ORDER empsrl

SELECT 3
USE ovtime ORDER empsrl

SELECT 4
USE artime ORDER empsrl

SELECT 5
USE dptime ORDER empsrl

SELECT 6
USE deduct ORDER empsrl
SET FILTER TO
SET FILTER TO mcode = mmcode AND year = myear

SELECT 7
USE salary ORDER empsrl

SELECT 8
USE salcode ORDER salcode

SELECT 9
USE deptt ORDER deptcode

SELECT 10
USE advance EXCL

SELECT 11
USE instded EXCL



SELECT salary
SET RELATION TO salcode INTO salcode ADDITIVE

SELECT employee
SET RELATION TO deptcode INTO deptt ADDITIVE
SET RELATION TO empsrl INTO attend ADDITIVE
SET RELATION TO empsrl INTO ovtime ADDITIVE
SET RELATION TO empsrl INTO artime ADDITIVE
SET RELATION TO empsrl INTO dptime ADDITIVE
SET RELATION TO empsrl INTO deduct ADDITIVE
SET RELATION TO empsrl INTO salary ADDITIVE

RETURN


******************
PROCEDURE AskDeptt
******************
PUBLIC mDptCode,mEmpNo,mGoAhead

CLEAR
mDptCode = 0
mEmpNo = 0

ON KEY LABEL F5 DO ShowDpt

@ 0,35 SAY "ADD NEW EMPLOYEE"
@ 1,35 SAY "----------------"
@ 24,20 SAY "Press F5 to View Department List"


@ 3,5 SAY "Enter Department No. "
@ 4,5 SAY "in Which You want to Open a New Employee ? "
@ ROW(),COL()+1 GET mDptCode PICT "99" VALID Vdept()
READ


ON KEY LABEL F5

RETURN
********


****************
PROCEDURE Vdept
****************
SELECT deptt
LOCATE FOR deptcode = mDptCode
IF FOUND()
	@ 24,0 TO 24,79 CLEAR
	@ 8,0 TO 8,45 CLEAR
	@ 8,5 SAY "Department Name: "
	@ ROW(),COL() SAY deptt.deptname
	SELECT employee
	SET FILTER TO deptcode = mDptCode
	GOTO BOTTOM
	STORE empno+1 TO mEmpNo
	@ 10,5 SAY "New Emp.No in This Department Will be : "
	@ ROW(),COL()+1 SAY mEmpNo PICT "@L 999"
			mGoAhead = .T.
	RETURN .T.
ENDIF

IF NOT FOUND()
	@ 8,0 TO 8,45 CLEAR
	WAIT WINDOW "Department Code Not in File. Try Again!" TIMEOUT 1

	mDptCode = 0
	CLEAR GETS
	DO ShowDpt
	@ 24,0 TO 24,79 CLEAR
	@ 8,0 TO 8,45 CLEAR
	@ 8,5 SAY "Department Name: "
	@ ROW(),COL() SAY deptt.deptname
	SELECT employee
	SET FILTER TO deptcode = mDptCode
	GOTO BOTTOM
	STORE empno+1 TO mEmpNo
	@ 10,5 SAY "New Emp.No in This Department Will be : "
	@ ROW(),COL()+1 SAY mEmpNo PICT "@L 999"
			mGoAhead = .T.
	RETURN .T.

ENDIF

SELECT 1

RETURN .T.
***************

*******************
PROCEDURE ShowDpt
*******************
PUSH KEY

DEFINE WINDOW brShDpt FROM 5,5 TO 21,45 DOUBLE SHADOW;
		Title "Department List"

ACTIVATE WINDOW brShDpt

SELECT deptt
GOTO TOP

ON KEY LABEL F5

ON KEY LABEL ENTER DO Pick

LOCATE FOR deptcode = mDptCode

IF NOT FOUND()
	GOTO TOP
ENDIF


BROW FIELDS;
	deptcode :2 :H=" ",;
	deptname :H="Department Name";
IN WINDOW brShDpt;
NOEDIT NODELETE NOAPPEND

RELEASE WINDOW brShDpt

POP KEY


SELECT 1
RETURN
*********************


*****************
PROCEDURE Pick
*****************

mDptCode = 0

PUSH KEY
CLEA GETS
mDptCode = deptcode

RELEASE WINDOW brShDpt

POP KEY
ON KEY LABEL ENTER
SHOW GETS

RETURN .T.
*****************


******************
PROCEDURE Confirm1
******************
PUBLIC mConf1
mConf1 = space(1)


@ 12,5 SAY "Do you Confirm to Open the New Employee No. "
@ ROW(),COL()+1 SAY mDptCode PICT "@L 99"
@ ROW(),COL() SAY "-"
@ ROW(),COL() SAY mEmpNo PICT "@L 999"
@ ROW(),COL()+1 SAY "?"
@ ROW(),COL()+2 GET mConf1 PICT "@!" VALID INLIST(mConf1,'Y','N');
			    ERROR "(Y)es,(N)o"
READ



DO CASE
	CASE mConf1 = "Y"
		mGoAhead = .T.
	CASE mConf1 = "N"
		mGoAhead = .F.
ENDCASE


RETURN
**********


*******************
PROCEDURE OpenNemp
*******************
PUBLIC mEmpSrl
mEmpSrl = 0



CLEAR
@ 10,20 SAY "Opening New Employee. Code: "
@ ROW(),COL() SAY mDptCode PICT "@L 99"
@ ROW(),COL() SAY "-"
@ ROW(),COL() SAY mEmpNo PICT "@L 99"
@ 14,20 SAY "Please Wait ... "



SELECT employee
APPEND BLANK
REPLACE EmpSrl WITH RECNO()
STORE EmpSrl TO mEmpSrl
REPLACE deptcode WITH mDptCode
REPLACE EmpNo WITH mEmpNo
REPLACE probation WITH .F.
REPLACE working WITH .T.
REPLACE mCode WITH mMCode
REPLACE year WITH mYear



SELECT attend
APPEND BLANK
REPLACE EmpSrl WITH mEmpSrl
REPLACE deptcode WITH mDptCode
REPLACE EmpNo WITH mEmpNo
REPLACE mCode WITH mMCode
REPLACE year WITH mYear

SELECT ovtime
APPEND BLANK
REPLACE EmpSrl WITH mEmpSrl
REPLACE deptcode WITH mDptCode
REPLACE EmpNo WITH mEmpNo
REPLACE mCode WITH mMCode
REPLACE year WITH mYear


SELECT artime
APPEND BLANK
REPLACE EmpSrl WITH mEmpSrl
REPLACE deptcode WITH mDptCode
REPLACE EmpNo WITH mEmpNo
REPLACE mCode WITH mMCode
REPLACE year WITH mYear

SELECT dptime
APPEND BLANK
REPLACE EmpSrl WITH mEmpSrl
REPLACE deptcode WITH mDptCode
REPLACE EmpNo WITH mEmpNo
REPLACE mCode WITH mMCode
REPLACE year WITH mYear


SELECT deduct
LOCATE FOR mcode = mMcode AND year = mYear
IF FOUND()
	APPEND BLANK
	REPLACE EmpSrl WITH mEmpSrl
	REPLACE deptcode WITH mDptCode
	REPLACE EmpNo WITH mEmpNo
	REPLACE mCode WITH mMCode
	REPLACE year WITH mYear
ENDIF
GOTO TOP

SELECT salary
APPEND BLANK
REPLACE EmpSrl WITH mEmpSrl
REPLACE deptcode WITH mDptCode
REPLACE EmpNo WITH mEmpNo
REPLACE mCode WITH mMCode
REPLACE year WITH mYear

SELECT advance
APPEND BLANK
REPLACE EmpSrl WITH mEmpSrl
REPLACE deptcode WITH mDptCode
REPLACE EmpNo WITH mEmpNo
REPLACE mCode WITH mMCode
REPLACE year WITH mYear
SET DATE TO BRITISH
SET CENT ON
REPLACE date WITH {01/07/2015}
REPLACE advamt WITH 99999999


SELECT instded
APPEND BLANK
REPLACE EmpSrl WITH mEmpSrl
REPLACE deptcode WITH mDptCode
REPLACE EmpNo WITH mEmpNo
REPLACE mCode WITH mMCode
REPLACE year WITH mYear
REPLACE inst WITH 9999


SELECT employee



RETURN
**************
