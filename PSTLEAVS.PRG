SET TALK OFF
SET SAFETY OFF

CLEAR
@ 10,25 SAY "Posting Leaves Data. Please Wait ... "

PUBLIC mMcode,mYear,CmExist
mMcode = 0
mYear = 0

CLOSE DATA
SELECT 1
USE leaves EXCLUSIVE
ZAP

SELECT 2
USE salary ORDER empsrl

SELECT 3
USE employee ORDER empsrl

SELECT 1
SET RELATION TO empsrl INTO salary ADDITIVE
SET RELATION TO empsrl INTO employee ADDITIVE

APPEND FROM employee FOR salary.mpay = .T.
REPLACE ALL allowed WITH employee.LvsAllow FOR NOT employee.probation

CLOSE DATA
select 1
use leaves
REPLACE ALL availed WITH 0
REPLACE ALL bal WITH 0

SELECT 2
USE attend ORDER empsrl
STORE mcode TO mMcode
STORE year TO mYear

select 3
use attendat order empsrl
LOCATE FOR mcode = mMcode AND year = mYear

IF FOUND()
	CmExist = .T.
ENDIF	



SELECT 4
USE employee ORDER empsrl


select 1
set relation to empsrl into attend ADDITIVE
set relation to empsrl into attendat ADDITIVE
set relation to empsrl into employee ADDITIVE

IF CmExist = .F.
set skip to attend
SCAN
replace availed with attend.leave+availed
ENDSCAN
ENDIF


set skip to attendat
SCAN
replace availed with attendat.leave+availed
ENDSCAN


CLOSE DATA

USE leaves

REPLACE ALL bal WITH allowed - availed
REPLACE ALL bal WITH 0 FOR NOT working

CLOSE DATA