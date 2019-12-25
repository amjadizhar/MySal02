PROCEDURE Multi1
clear
clear all
set sysmenu off
set talk off

SET VIEW TO attend
DEFINE WINDOW win1 FROM 0,0 TO 10,35 CLOSE

SELECT 1
BROW fields;
deptcode,empno,employee.name;
PREFERENCE attend;
WINDOW win1


SELECT 3
DEFINE WINDOW win2 FROM 12,0 TO 18,35
BROW fields;
deptcode,deptname;
PREFERENCE deptt;
WINDOW win2

SELECT 1
BROW LAST
RETURN

******************************************************************
Date: 1-1-96
******************************************************************

PROCEDURE Multi2

SELECT 1
USE employee ORDER name

IF NOT WVISIBLE("Emp. List")
	SELECT employee
	DEFINE WINDOW Ebr FROM 0,0 TO 12,35
	
	BROWSE PREFERENCE Ebr;
	NODELETE NOAPPEND NOEDIT;
	WINDOW Ebr TITLE "Emp. List";
	FIELDS deptcode,empno,name,doa

	
	RELEASE WINDOW Ebr
ENDIF

IF NOT WVISIBLE("Employees Working")
	SELECT employee
	DEFINE WINDOW Ebr2 FROM 0,40 TO 12,78
	
	BROWSE PREFERENCE Ebr2;
	NODELETE NOAPPEND NOEDIT;
	WINDOW Ebr2 TITLE "Employees Working";
	FIELDS deptcode,empno,name,working
	
	

ENDIF


RETURN
**********************