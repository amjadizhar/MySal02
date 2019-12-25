clear all
CLOSE ALL

mTitle=space(30)

select 1
use prtyws
select 2
use ldgrcode
index on str(c1,2)+str(c2,2)+str(c3,3) tag acc additive
select 1
set relation to str(c1,2)+str(c2,2)+str(c3,3) into ldgrcode

scan
if c1 = 53
store descrip1 to mtitle
endif

skip

do while c1 = 61
replace descrip1 with mtitle
skip
enddo

endscan

dele all for empty(descrip1)
pack

do prtysum
do prcodsum

PROCEDURE PrtySum
CLEAR ALL
CLOSE ALL

SELECT Descrip1,SUM(amount);
FROM prtyws.dbf;
GROUP BY descrip1;
ORDER BY descrip1;
INTO DBF prtyws1.dbf

RETURN

PROCEDURE PrCodSum
CLEAR ALL
CLOSE ALL

SELECT c1,c2,c3,Descrip1,SUM(amount);
FROM prtyws.dbf;
GROUP BY c1,c2,c3,descrip1;
ORDER BY descrip1;
INTO DBF prtyws2.dbf

SELECT prtyws2
INDEX ON str(c1,2)+str(c2,2)+str(c3,3) TAG acc ADDITIVE

DELE ALL FOR C1<>61
PACK

RETURN


**********************
PROCEDURE SortPrty
***********************

SELECT *;
FROM prtyws.dbf;
ORDER BY descrip1,date;
INTO DBF prtysort.dbf

RETURN
**********************
