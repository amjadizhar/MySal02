
CLEAR
CLOSE DATABASES

SELECT 1
USE salary ORDER empno

SELECT 2
USE employee ORDER empno

SELECT 3
USE deduct ORDER empno

SELECT 4
USE salcode ORDER salcode

SELECT 5
USE month ORDER mcode

SELECT 6
USE salsheet ORDER sheetno

SELECT 7
USE saldtail ORDER empno

SELECT 1
SET RELATION TO salcode INTO salsheet ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO deduct ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE

SELECT 7
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
SET RELATION TO mcode INTO month ADDITIVE
SET RELATION TO sheetno INTO salsheet ADDITIVE

PUBLIC Rmonth,Rsalcode
Rmonth = 0
Rsalcode = 0

clear
@ 6,10 SAY "Enter the Salary Sheet Code  : "
@ 7,12 GET Rsalcode PICT "99"
READ

SELECT salcode
SEEK(Rsalcode)
@ 7,25 SAY salcode.descrip
PUBLIC title
title = salcode.descrip

@ 10,10 SAY "Enter the Month No. You want to get Salary Sheet"
@ 12,10 GET Rmonth PICT "99"
READ

SELECT saldtail
set filter to 
set filter to mcode = Rmonth

SELECT deduct
set filter to
set filter to mcode = Rmonth


SELECT month
SEEK(Rmonth)
@ 14,10 SAY month.month
PUBLIC mdescrip1
mdescrip1 = ALLTRIM(month.month)

SELECT salsheet
SET ORDER TO mcode
SEEK(Rmonth)
@ 14,22 SAY salsheet.year
PUBLIC mdescrip
mdescrip = mdescrip1 + "," + ALLTRIM(str(salsheet.year))
SET ORDER TO sheetno
wait

SELECT salary

SET FILTER TO
SET FILTER TO SALCODE = Rsalcode



