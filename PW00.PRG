* PROGRAM: PW00.PRG
* TO GET PARTY WISE SALE FOR THE YEAR 2000
* DIR. C:\AMJAD\OLDACC\AS\AS00
* Written on: 01-02-2000.
* Last Revised On: 19-01-2001 7:15 PM



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

SELECT 	00 AS "C1",00 AS "C2",000 AS "C3",;
		Descrip1 AS "Title", ;
	   	SPACE(30) AS "Address1",;
	   	SPACE(30) AS "Address2",;
	   	SUM(amount) AS "JUN30_2000",;
	   	0000000000000.00 AS "Jun3099",;
	   	SPACE(9) AS "Code";
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
REPL ALL c1 WITH ldgrcode.c1
REPL ALL c2 WITH ldgrcode.c2
REPL ALL c3 WITH ldgrcode.c3
REPL ALL code WITH STR(c1,2)+"-"+STR(c2,2)+"-"+STR(c3,3)

COPY TO pwfin00.dbf

use pwfin00
INDEX ON title TAG title ADDITIVE
INDEX ON code TAG code ADDITIVE


CLEAR ALL
CLOSE ALL

!copy ..\as99\prtyws61.dbf pw063099.dbf
USE pw063099
INDEX ON title TAG title ADDITIVE
INDEX ON code TAG code ADDITIVE
CLOSE ALL

SELECT * FROM pw063099;
WHERE title NOT IN (SELECT title FROM pwfin00);
INTO DBF prtnot00.dbf

CLEAR
CLEA ALL
CLOSE ALL

USE pwfin00
append from prtnot00

clear
clear all
close all

SELECT 1
USE pwfin00
SELECT 2
USE pw063099
SET ORDER TO title
SELECT 1
SET RELATION TO title INTO pw063099
REPL ALL jun3099 WITH pw063099.jun3099
close all

USE pwfin00
INDEX ON title TAG title ADDITIVE
INDEX ON code TAG code ADDITIVE
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
