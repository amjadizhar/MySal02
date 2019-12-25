* PROGRAM: PW99.PRG
* TO GET PARTY WISE SALE FOR THE YEAR 1999
* DIR. C:\ACCOUNTS\AS\AS99
* Written on: 01-02-00.

PUBLIC mTitle

SELECT *;
FROM tran;
WHERE vch_type = 6;
INTO DBF prtyws.dbf

CLEAR
CLEAR ALL
CLOSE ALL

SELECT 1
USE prtyws.dbf
SELECT 2
USE ldgrcode.dbf
SET ORDER TO ac1
SELECT 1
SET RELATION TO str(c1,2)+str(c2,2)+str(c3,3) INTO ldgrcode ADDITIVE

REPLACE ALL descrip1 WITH ldgrcode.title FOR C1=53
GOTO TOP

mTitle=space(30)

scan
if c1 = 53
store descrip1 to mtitle
endif

skip

do while c1 = 61 or c1=69
replace descrip1 with mtitle
skip
enddo

endscan


CLEAR ALL
CLOSE ALL

SELECT Descrip1 AS "Title", ;
	   SPACE(30) AS "Address1",;
	   SPACE(30) AS "Address2",;
	   SUM(amount);
FROM prtyws.dbf;
WHERE C1=61;
GROUP BY descrip1;
ORDER BY descrip1;
INTO DBF prtyws61.dbf

CLEAR ALL
CLOSE ALL
SELECT 1
USE prtyws61.dbf
SELECT 2
USE ldgrcode.dbf
INDEX ON title TAG title ADDITIVE
SELECT 1
SET RELATION TO title INTO ldgrcode ADDITIVE
REPL ALL address1 WITH ldgrcode.address1
REPL ALL address2 WITH ldgrcode.address2




CLEAR ALL
CLOSE ALL

SELECT Descrip1 AS "Title", ;
	   SPACE(30) AS "Address1",;
	   SPACE(30) AS "Address2",;
	   SUM(amount);
FROM prtyws.dbf;
WHERE C1=69;
GROUP BY descrip1;
ORDER BY descrip1;
INTO DBF prtyws69.dbf

CLEAR ALL
CLOSE ALL
SELECT 1
USE prtyws69.dbf
SELECT 2
USE ldgrcode.dbf
INDEX ON title TAG title ADDITIVE
SELECT 1
SET RELATION TO title INTO ldgrcode ADDITIVE
REPL ALL address1 WITH ldgrcode.address1
REPL ALL address2 WITH ldgrcode.address2
