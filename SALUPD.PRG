* Salary update entry

****************
PROCEDURE upd1
***************

SELECT 1
USE salary ORDER empno

SELECT 2
USE employee ORDER empno

SELECT 3
USE attend ORDER empno

SELECT 4
USE artime ORDER empno

SELECT 5
USE ovtime ORDER empno

SELECT 6
USE dptime ORDER empno

SELECT 7
USE deduct ORDER empno

SELECT 8
USE salcode ORDER salcode

SELECT 1
SET RELATION TO str(deptcode)+str(empno) INTO employee ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO attend ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO artime ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO ovtime ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO dptime ADDITIVE
SET RELATION TO str(deptcode)+str(empno) INTO deduct ADDITIVE
SET RELATION TO salcode INTO salcode ADDITIVE

BROW FIELDS;
deptcode,empno,employee.name,;
salcode,salcode.descrip,basic,hrent,convy,medical,cma,pma,other,special,;
otrated,dwratem