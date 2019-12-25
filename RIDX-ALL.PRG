CLEAR
SET TALK OFF
SET SAFETY OFF

SET EXCLUSIVE ON


@ 10,5 SAY "Re-Indexing All Data Files. Please Wait ... "

USE saldtail EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON str(deptcode)+str(empno) TAG empno2 ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE

USE employee EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON name TAG name ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE


USE salary EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON str(deptcode)+str(empno) TAG empno2 ADDITIVE
	INDEX ON salcode TAG salcode ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE
		

USE attend EXCLUSIVE
	INDEX ON mcode TAG mcode ADDITIVE
	INDEX ON str(mcode)+str(year) TAG ymcode ADDITIVE
	INDEX ON str(deptcode)+str(atord)+;
			 str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE
	INDEX ON str(deptcode)+str(empno) TAG empno2 ADDITIVE


USE ovtime EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON str(deptcode)+str(empno) TAG empno2 ADDITIVE
	INDEX ON str(deptcode)+str(atord)+str(empno) TAG empno3 ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE	
	
	

USE artime EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON str(deptcode)+str(empno) TAG empno2 ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE	


USE dptime EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON str(deptcode)+str(empno) TAG empno2 ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE	

USE deduct EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON str(deptcode)+str(empno) TAG empno2 ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE	



USE attendat EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON str(deptcode)+str(empno) TAG empno2 ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE	


USE ovtimdat EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON str(deptcode)+str(empno) TAG empno2 ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE	


USE artimdat EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON str(deptcode)+str(empno) TAG empno2 ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE	


USE dptimdat EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON str(deptcode)+str(empno) TAG empno2 ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE	

USE Slrydat
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON str(deptcode)+str(empno) TAG empno2 ADDITIVE
	INDEX ON salcode TAG salcode ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE


USE advance EXCLUSIVE
	INDEX ON str(deptcode)+str(empno) TAG empno ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE

USE contrib EXCLUSIVE
	INDEX ON str(deptcode)+str(empno)+str(mcode)+str(year) TAG empno ADDITIVE
	INDEX ON str(deptcode)+str(empno) TAG empno2 ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE	

USE instded EXCLUSIVE
	INDEX ON str(deptcode)+str(empno) TAG empno ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE

USE leaves EXCLUSIVE
	INDEX ON str(deptcode)+str(empno) TAG empno ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE


USE usr EXCLUSIVE
	INDEX ON Usrcode TAG code ADDITIVE


USE month
	INDEX ON mcode TAG mcode ADDITIVE

USE holiday
	INDEX ON mcode TAG mcode ADDITIVE

USE deptt
	INDEX ON deptcode TAG deptcode ADDITIVE
	
USE ysetting
	INDEX ON year TAG year ADDITIVE

USE Instded
	INDEX ON str(deptcode)+str(empno) TAG empno ADDITIVE
	INDEX ON empsrl TAG empsrl ADDITIVE


CLEAR
@ 10,5 SAY "Updating Employee Serial Numbers..."

USE employee ORDER empno
REPLACE ALL EMPSRL WITH RECNO()
CLOSE DATA


SELECT 1
USE attend ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
CLOSE DATA


SELECT 1
USE attendat ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
CLOSE DATA


SELECT 1
USE artime ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
CLOSE DATA


SELECT 1
USE artimdat ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
CLOSE DATA


SELECT 1
USE ovtime ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
CLOSE DATA

SELECT 1
USE ovtimdat ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
CLOSE DATA


SELECT 1
USE dptime ORDER empno2
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
CLOSE DATA


SELECT 1
USE salary ORDER empno
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
CLOSE DATA

SELECT 1
USE deduct ORDER empno
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
CLOSE DATA

SELECT 1
USE saldtail ORDER empno
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
CLOSE DATA


SELECT 1
USE leaves ORDER EMPNO
SELECT 2
USE employee ORDER EMPNO
SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
REPLACE ALL empsrl WITH employee.empsrl
CLOSE DATA

CLEAR
CLOSE DATA

RETURN
************************

