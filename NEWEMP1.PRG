*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º 08/19/96             NEWEMP1.SPR               12:33:11 º
*       º This Revision: 11-May-2017                              º
*       ÇÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ¶
*       º                                                         º
*       º Author's Name  Amjad Izhar                              º
*       º                                                         º
*       º Copyright (c) 1996 CosmoSoft Systems                    º
*       º Address   54-Zeenat Block Allama Iqbal Town Lahore      º
*       º City,     Lahore Zip 54570                              º
*       º                                                         º
*       º Description:                                            º
*       º Continuation from PRG NewEmp.PRG                        º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½


#REGION 0
REGIONAL m.currarea, m.talkstat, m.compstat

IF SET("TALK") = "ON"
	SET TALK OFF
	m.talkstat = "ON"
ELSE
	m.talkstat = "OFF"
ENDIF
m.compstat = SET("COMPATIBLE")
SET COMPATIBLE FOXPLUS
SET DATE TO BRITISH
SET CENT ON


*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º                MS-DOS Window definitions                º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*

IF NOT WEXIST("newemp1") ;
	OR UPPER(WTITLE("NEWEMP1")) == "NEWEMP1.PJX" ;
	OR UPPER(WTITLE("NEWEMP1")) == "NEWEMP1.SCX" ;
	OR UPPER(WTITLE("NEWEMP1")) == "NEWEMP1.MNX" ;
	OR UPPER(WTITLE("NEWEMP1")) == "NEWEMP1.PRG" ;
	OR UPPER(WTITLE("NEWEMP1")) == "NEWEMP1.FRX" ;
	OR UPPER(WTITLE("NEWEMP1")) == "NEWEMP1.QPR"
	DEFINE WINDOW newemp1 ;
		FROM INT((SROW()-24)/2),INT((SCOL()-79)/2) ;
		TO INT((SROW()-24)/2)+23,INT((SCOL()-79)/2)+78 ;
		TITLE "New Employee" ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		COLOR SCHEME 1
ENDIF


*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º          NEWEMP1/MS-DOS Setup Code - SECTION 2          º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*

#REGION 1



*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º              NEWEMP1/MS-DOS Screen Layout               º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*

#REGION 1
IF WVISIBLE("newemp1")
	ACTIVATE WINDOW newemp1 SAME
ELSE
	ACTIVATE WINDOW newemp1 NOSHOW
ENDIF

@ 1,2 SAY "Employee Code:" ;
	SIZE 1,14, 0

@ 1,17 GET employee.deptcode ;
	SIZE 1,2 ;
	DEFAULT 0 ;
	PICTURE "@L" ;
	DISABLE

@ 1,19 SAY "-" ;
	SIZE 1,1, 0

@ 1,20 GET employee.empno ;
	SIZE 1,3 ;
	DEFAULT 0 ;
	PICTURE "@L" ;
	DISABLE

@ 1,25 SAY "Department:" ;
	SIZE 1,11, 0

@ 1,37 GET deptt.deptname ;
	SIZE 1,20 ;
	DEFAULT " " ;
	DISABLE

@ 3,2 SAY "Employee Name:" ;
	SIZE 1,14, 0

@ 3,17 GET employee.salutation ;
	SIZE 1,2 ;
	DEFAULT " " ;
	PICTURE "!x" ;
	VALID _rd40qwn35() ;
	MESSAGE "Enter Mr or Ms"

@ 3,20 GET employee.name ;
	SIZE 1,20 ;
	DEFAULT " " ;
	VALID _rd40qwnbo() ;
	MESSAGE "Enter Employee Full Name in any Case (Lower/Upper)"

@ 3,42 SAY "Father Name:" ;
	SIZE 1,12, 0

@ 3,55 GET employee.father ;
	SIZE 1,20 ;
	DEFAULT " " ;
	VALID _rd40qwni9() ;
	MESSAGE "Enter Employee's Father Full Name"

@ 7,2 SAY "Address:" ;
	SIZE 1,8, 0

@ 11,5 SAY "City:" ;
	SIZE 1,5, 0

@ 7,41 SAY "Phone Numbers:" ;
	SIZE 1,14, 0

@ 8,41 SAY "Phone No.1" ;
	SIZE 1,10, 0

@ 9,41 SAY "Phone No.2" ;
	SIZE 1,10, 0

@ 10,41 SAY "Phone PP." ;
	SIZE 1,9, 0

@ 5,2 SAY "N.I.C. Number:" ;
	SIZE 1,14, 0

@ 5,17 GET employee.nicno ;
	SIZE 1,15 ;
	DEFAULT " " ;
	PICTURE "99999-9999999-9" ;
	VALID _rd40qwnns() ;
	MESSAGE "Enter Employee's National Identity Card Number"

@ 8,2 GET employee.addr1 ;
	SIZE 1,25 ;
	DEFAULT " " ;
	VALID _rd40qwnqv() ;
	MESSAGE "Enter employee's Permanent Address Line Number 1"

@ 9,2 GET employee.addr2 ;
	SIZE 1,25 ;
	DEFAULT " " ;
	VALID _rd40qwnvz() ;
	MESSAGE "Enter Employee's Permanent Address Line Number 2"

@ 10,2 GET employee.addr3 ;
	SIZE 1,25 ;
	DEFAULT " " ;
	VALID _rd40qwo0z() ;
	MESSAGE "Enter employee's Permanent Address Line Number 3"

@ 11,12 GET employee.city ;
	SIZE 1,15 ;
	DEFAULT " " ;
	VALID _rd40qwo54() ;
	MESSAGE "Enter Employee's City of above address"

@ 8,53 GET employee.phone1 ;
	SIZE 1,15 ;
	DEFAULT " " ;
	PICTURE "###############" ;
	MESSAGE "Enter First Contact Phone No. Like 042-6661431"

@ 9,53 GET employee.phone2 ;
	SIZE 1,15 ;
	DEFAULT " " ;
	PICTURE "###############" ;
	MESSAGE "Enter Second Contact Phone No. Like 042-6661431"

@ 10,53 GET employee.phonepp ;
	SIZE 1,15 ;
	DEFAULT " " ;
	PICTURE "###############" ;
	MESSAGE "Enter Employee's nearby phone number. In case of failure on above phones"

@ 14,24 GET employee.doa ;
	SIZE 1,10 ;
	DEFAULT {  /  /    } ;
	PICTURE "@E" ;
	MESSAGE "Date of appointment from where you want to start this employee attendence"

@ 14,2 SAY "Date of Appointment:" ;
	SIZE 1,20, 0

@ 16,2 SAY "Designation:" ;
	SIZE 1,12, 0

@ 16,15 GET employee.designatn ;
	SIZE 1,25 ;
	DEFAULT " " ;
	VALID _rd40qwocg() ;
	MESSAGE "Enter Employee Designation Like Manager Production etc."

@ 18,2 SAY "Reference 1:" ;
	SIZE 1,12, 0

@ 18,15 GET employee.ref1 ;
	SIZE 1,30 ;
	DEFAULT " " ;
	VALID _rd40qwohp() ;
	MESSAGE "Employee's acquaintances or anyone who has refered him/her"

@ 20,2 SAY "Reference 2:" ;
	SIZE 1,12, 0

@ 20,15 GET employee.ref2 ;
	SIZE 1,30 ;
	DEFAULT " " ;
	VALID _rd40qwokq() ;
	MESSAGE "Employee's acquaintances or anyone who has refered him/her"

@ 20,48 SAY "Notes:" ;
	SIZE 1,6, 0

@ 20,55 GET notes ;
	SIZE 1,5 ;
	DEFAULT " " ;
	PICTURE "@! xxxxx" ;
	WHEN _rd40qwoo7() ;
	MESSAGE "Any Other Notes Like Employee's Hobbies, Interests, Researches Etc."

IF NOT WVISIBLE("newemp1")
	ACTIVATE WINDOW newemp1
ENDIF

READ MODAL ;
	VALID _rd40qwow9()

RELEASE WINDOW newemp1

#REGION 0
IF m.talkstat = "ON"
	SET TALK ON
ENDIF
IF m.compstat = "ON"
	SET COMPATIBLE ON
ENDIF


*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RD40QWN35           employee.salutation VALID          º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         NEWEMP1,     Record Number:    9   º
*       º Variable:            employee.salutation                º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      1                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rd40qwn35     &&  employee.salutation VALID
#REGION 1
IF INLIST(salutation,'Mr','Ms') = .F.
WAIT WINDOW "Mr or Ms" TIMEOUT 1
RETURN .F.
ENDIF
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RD40QWNBO           employee.name VALID                º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         NEWEMP1,     Record Number:   10   º
*       º Variable:            employee.name                      º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      2                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rd40qwnbo     &&  employee.name VALID
#REGION 1
REPLACE name WITH PROPER(name)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RD40QWNI9           employee.father VALID              º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         NEWEMP1,     Record Number:   12   º
*       º Variable:            employee.father                    º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      3                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rd40qwni9     &&  employee.father VALID
#REGION 1
REPLACE father WITH PROPER(father)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RD40QWNNS           employee.nicno VALID               º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         NEWEMP1,     Record Number:   20   º
*       º Variable:            employee.nicno                     º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      4                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rd40qwnns     &&  employee.nicno VALID
#REGION 1
 *IF OCCURS(" ",nicno) > 0
   *WAIT WINDOW "Some Digits are Missing. Please Complete!" TIMEOUT 1
   *RETURN .F.
 *ENDIF
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RD40QWNQV           employee.addr1 VALID               º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         NEWEMP1,     Record Number:   21   º
*       º Variable:            employee.addr1                     º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      5                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rd40qwnqv     &&  employee.addr1 VALID
#REGION 1
REPLACE addr1 WITH PROPER(addr1)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RD40QWNVZ           employee.addr2 VALID               º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         NEWEMP1,     Record Number:   22   º
*       º Variable:            employee.addr2                     º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      6                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rd40qwnvz     &&  employee.addr2 VALID
#REGION 1
REPLACE addr2 WITH PROPER(addr2)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RD40QWO0Z           employee.addr3 VALID               º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         NEWEMP1,     Record Number:   23   º
*       º Variable:            employee.addr3                     º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      7                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rd40qwo0z     &&  employee.addr3 VALID
#REGION 1
REPLACE addr3 WITH PROPER(addr3)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RD40QWO54           employee.city VALID                º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         NEWEMP1,     Record Number:   24   º
*       º Variable:            employee.city                      º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      8                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rd40qwo54     &&  employee.city VALID
#REGION 1
REPLACE city WITH PROPER(city)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RD40QWOCG           employee.designatn VALID           º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         NEWEMP1,     Record Number:   31   º
*       º Variable:            employee.designatn                 º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      9                                  º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rd40qwocg     &&  employee.designatn VALID
#REGION 1
REPLACE designatn WITH PROPER(designatn)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RD40QWOHP           employee.ref1 VALID                º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         NEWEMP1,     Record Number:   33   º
*       º Variable:            employee.ref1                      º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      10                                 º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rd40qwohp     &&  employee.ref1 VALID
#REGION 1
REPLACE ref1 WITH PROPER(ref1)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RD40QWOKQ           employee.ref2 VALID                º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         NEWEMP1,     Record Number:   35   º
*       º Variable:            employee.ref2                      º
*       º Called By:           VALID Clause                       º
*       º Object Type:         Field                              º
*       º Snippet Number:      11                                 º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rd40qwokq     &&  employee.ref2 VALID
#REGION 1
REPLACE ref2 WITH PROPER(ref2)
RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RD40QWOO7           notes WHEN                         º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         NEWEMP1,     Record Number:   37   º
*       º Variable:            notes                              º
*       º Called By:           WHEN Clause                        º
*       º Object Type:         Field                              º
*       º Snippet Number:      12                                 º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rd40qwoo7     &&  notes WHEN
#REGION 1
DEFINE WINDOW NtsWin1 FROM 5,31 TO 19,74;
 TITLE "Other Notes for "+ALLTRIM(salutation)+"."+ALLTRIM(name);
 FOOTER "Press Esc to Exit";
 CLOSE FLOAT GROW SHADOW ZOOM

PUSH KEY
ON KEY LABEL ESC CLOSE MEMO notes
MODI MEMO notes WINDOW NtsWin1
ON KEY LABEL ESC
POP KEY

RETURN .T.

*       ÖÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ·
*       º                                                         º
*       º _RD40QWOW9           Read Level Valid                   º
*       º                                                         º
*       º Function Origin:                                        º
*       º                                                         º
*       º                                                         º
*       º From Platform:       MS-DOS                             º
*       º From Screen:         NEWEMP1                            º
*       º Called By:           READ Statement                     º
*       º Snippet Number:      13                                 º
*       º                                                         º
*       ÓÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ½
*
FUNCTION _rd40qwow9     && Read Level Valid
*
* Valid Code from screen: NEWEMP1
*
#REGION 1
IF EMPTY(name)
	WAIT WINDOW "Please Enter Employee Name" TIMEOUT 1
	_curobj = 5
	RETURN .F.
ENDIF

IF EMPTY(doa)
	WAIT WINDOW "Please Enter date of appointment" TIMEOUT 1
	_curobj = 15
	RETURN .F.
ENDIF
RETURN .T.