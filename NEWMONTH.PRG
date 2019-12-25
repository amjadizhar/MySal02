*****************************************
* PROGRAMM NEWMONTH.PRG
* A programm to Enter Data for new month 
* Information in Screen and Brows Windows
*****************************************
SET CONFIRM ON
SET SYSMENU OFF
SET TALK OFF
SET SAFETY OFF

CLEAR

*@ 0,0 SAY "Data Entry for Days,Overtime and Advances"


@ 0,0,11,79 BOX
DEFINE WINDOW mainbrows FROM 1,1 TO 10,78


SET VIEW TO newmonth

BROWS FIELDS;
	Eno = RIGHT(str(temp.deptcode),2)+"-"+;
		RIGHT(str(temp.empno),3)+"ณ";
		:R :H = "Code",;
	Ename = employee.name :17 :H = "Employee Name",;
	L1 = "ณ" :1 :R :H = "",;
	temp.days,;
	temp.othrs,temp.advance,temp.midadv,temp.other,temp.pfund,temp.bonday;
	WINDOW mainbrows;
	Title " ";
	FREEZE temp.days;
	NOAPPEND NODELETE NOMENU;
	WHEN empinf()

	
@ 1,0 TO 15,33 CLEAR
CANCEL



FUNCTION empinf
SET TALK OFF
SET SAFETY OFF
@ 12,0 TO 24,30 CLEAR

@ 12,0 SAY "Department    :"
@ 12,16 SAY deptt.deptname

IF salary.mpay = .T.

@ 13,0 SAY "Basic         :"
@ 13,16 SAY salary.basic
@ 14,0 SAY "House Rent    :"
@ 14,18 SAY salary.hrent

	IF salary.convy > 0
		@ 15,0 SAY "Conveyance    :"
		@ 15,18 SAY salary.convy
	ELSE
		@ 15,0 TO 15,30 CLEAR
	ENDIF

	IF salary.medical > 0
		@ 16,0 SAY "Medical       :"
		@ 16,18 SAY salary.medical
	ELSE
		@ 16,0 TO 16,30 CLEAR
	ENDIF

	IF salary.cma > 0
		@ 17,0 SAY "CMA           :"
		@ 17,18 SAY salary.cma
	ELSE
		@ 17,0 TO 17,30 CLEAR
	ENDIF

	IF salary.pma > 0
		@ 18,0 SAY "PMA           :"
		@ 18,18 SAY salary.cma
	ELSE
		@ 18,0 TO 18,30 CLEAR
	ENDIF

	IF salary.other > 0
		@ 19,0 SAY "Other         :"
		@ 19,18 SAY salary.other
	ELSE
		@ 19,0 TO 19,30 CLEAR
	ENDIF

	IF salary.special > 0
		@ 20,0 SAY "Special       :"
		@ 20,18 SAY salary.special
	ELSE
		@ 20,0 TO 20,30 CLEAR
	ENDIF

@ 21,0 SAY "อออออออออออออออออออออออ"

@ 22,15 SAY salary.basic+salary.hrent+salary.convy+salary.medical+;
			salary.cma+salary.pma+salary.other+salary.special;
			PICT "999,999"
@ 22,0 SAY "Total:"
@ 23,0 SAY "อออออออออออออออออออออออ"
ELSE

@ 13,0 SAY "D.Wages Rate  :"
@ 13,18 SAY salary.dwrate

ENDIF



RETURN .T.