*       ���������������������������������������������������������ķ
*       �                                                         �
*       � 08/19/96             NEWEMP1.SPR               12:33:11 �
*       � This Revision: 11-May-2017                              �
*       ���������������������������������������������������������Ķ
*       �                                                         �
*       � Author's Name  Amjad Izhar                              �
*       �                                                         �
*       � Copyright (c) 1996 CosmoSoft Systems                    �
*       � Address   54-Zeenat Block Allama Iqbal Town Lahore      �
*       � City,     Lahore Zip 54570                              �
*       �                                                         �
*       � Description:                                            �
*       � Continuation from PRG NewEmp.PRG                        �
*       �                                                         �
*       ���������������������������������������������������������Ľ


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


*       ���������������������������������������������������������ķ
*       �                                                         �
*       �                MS-DOS Window definitions                �
*       �                                                         �
*       ���������������������������������������������������������Ľ
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


*       ���������������������������������������������������������ķ
*       �                                                         �
*       �          NEWEMP1/MS-DOS Setup Code - SECTION 2          �
*       �                                                         �
*       ���������������������������������������������������������Ľ
*

#REGION 1



*       ���������������������������������������������������������ķ
*       �                                                         �
*       �              NEWEMP1/MS-DOS Screen Layout               �
*       �                                                         �
*       ���������������������������������������������������������Ľ
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


*       ���������������������������������������������������������ķ
*       �                                                         �
*       � _RD40QWN35           employee.salutation VALID          �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         NEWEMP1,     Record Number:    9   �
*       � Variable:            employee.salutation                �
*       � Called By:           VALID Clause                       �
*       � Object Type:         Field                              �
*       � Snippet Number:      1                                  �
*       �                                                         �
*       ���������������������������������������������������������Ľ
*
FUNCTION _rd40qwn35     &&  employee.salutation VALID
#REGION 1
IF INLIST(salutation,'Mr','Ms') = .F.
WAIT WINDOW "Mr or Ms" TIMEOUT 1
RETURN .F.
ENDIF
RETURN .T.

*       ���������������������������������������������������������ķ
*       �                                                         �
*       � _RD40QWNBO           employee.name VALID                �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         NEWEMP1,     Record Number:   10   �
*       � Variable:            employee.name                      �
*       � Called By:           VALID Clause                       �
*       � Object Type:         Field                              �
*       � Snippet Number:      2                                  �
*       �                                                         �
*       ���������������������������������������������������������Ľ
*
FUNCTION _rd40qwnbo     &&  employee.name VALID
#REGION 1
REPLACE name WITH PROPER(name)
RETURN .T.

*       ���������������������������������������������������������ķ
*       �                                                         �
*       � _RD40QWNI9           employee.father VALID              �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         NEWEMP1,     Record Number:   12   �
*       � Variable:            employee.father                    �
*       � Called By:           VALID Clause                       �
*       � Object Type:         Field                              �
*       � Snippet Number:      3                                  �
*       �                                                         �
*       ���������������������������������������������������������Ľ
*
FUNCTION _rd40qwni9     &&  employee.father VALID
#REGION 1
REPLACE father WITH PROPER(father)
RETURN .T.

*       ���������������������������������������������������������ķ
*       �                                                         �
*       � _RD40QWNNS           employee.nicno VALID               �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         NEWEMP1,     Record Number:   20   �
*       � Variable:            employee.nicno                     �
*       � Called By:           VALID Clause                       �
*       � Object Type:         Field                              �
*       � Snippet Number:      4                                  �
*       �                                                         �
*       ���������������������������������������������������������Ľ
*
FUNCTION _rd40qwnns     &&  employee.nicno VALID
#REGION 1
 *IF OCCURS(" ",nicno) > 0
   *WAIT WINDOW "Some Digits are Missing. Please Complete!" TIMEOUT 1
   *RETURN .F.
 *ENDIF
RETURN .T.

*       ���������������������������������������������������������ķ
*       �                                                         �
*       � _RD40QWNQV           employee.addr1 VALID               �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         NEWEMP1,     Record Number:   21   �
*       � Variable:            employee.addr1                     �
*       � Called By:           VALID Clause                       �
*       � Object Type:         Field                              �
*       � Snippet Number:      5                                  �
*       �                                                         �
*       ���������������������������������������������������������Ľ
*
FUNCTION _rd40qwnqv     &&  employee.addr1 VALID
#REGION 1
REPLACE addr1 WITH PROPER(addr1)
RETURN .T.

*       ���������������������������������������������������������ķ
*       �                                                         �
*       � _RD40QWNVZ           employee.addr2 VALID               �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         NEWEMP1,     Record Number:   22   �
*       � Variable:            employee.addr2                     �
*       � Called By:           VALID Clause                       �
*       � Object Type:         Field                              �
*       � Snippet Number:      6                                  �
*       �                                                         �
*       ���������������������������������������������������������Ľ
*
FUNCTION _rd40qwnvz     &&  employee.addr2 VALID
#REGION 1
REPLACE addr2 WITH PROPER(addr2)
RETURN .T.

*       ���������������������������������������������������������ķ
*       �                                                         �
*       � _RD40QWO0Z           employee.addr3 VALID               �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         NEWEMP1,     Record Number:   23   �
*       � Variable:            employee.addr3                     �
*       � Called By:           VALID Clause                       �
*       � Object Type:         Field                              �
*       � Snippet Number:      7                                  �
*       �                                                         �
*       ���������������������������������������������������������Ľ
*
FUNCTION _rd40qwo0z     &&  employee.addr3 VALID
#REGION 1
REPLACE addr3 WITH PROPER(addr3)
RETURN .T.

*       ���������������������������������������������������������ķ
*       �                                                         �
*       � _RD40QWO54           employee.city VALID                �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         NEWEMP1,     Record Number:   24   �
*       � Variable:            employee.city                      �
*       � Called By:           VALID Clause                       �
*       � Object Type:         Field                              �
*       � Snippet Number:      8                                  �
*       �                                                         �
*       ���������������������������������������������������������Ľ
*
FUNCTION _rd40qwo54     &&  employee.city VALID
#REGION 1
REPLACE city WITH PROPER(city)
RETURN .T.

*       ���������������������������������������������������������ķ
*       �                                                         �
*       � _RD40QWOCG           employee.designatn VALID           �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         NEWEMP1,     Record Number:   31   �
*       � Variable:            employee.designatn                 �
*       � Called By:           VALID Clause                       �
*       � Object Type:         Field                              �
*       � Snippet Number:      9                                  �
*       �                                                         �
*       ���������������������������������������������������������Ľ
*
FUNCTION _rd40qwocg     &&  employee.designatn VALID
#REGION 1
REPLACE designatn WITH PROPER(designatn)
RETURN .T.

*       ���������������������������������������������������������ķ
*       �                                                         �
*       � _RD40QWOHP           employee.ref1 VALID                �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         NEWEMP1,     Record Number:   33   �
*       � Variable:            employee.ref1                      �
*       � Called By:           VALID Clause                       �
*       � Object Type:         Field                              �
*       � Snippet Number:      10                                 �
*       �                                                         �
*       ���������������������������������������������������������Ľ
*
FUNCTION _rd40qwohp     &&  employee.ref1 VALID
#REGION 1
REPLACE ref1 WITH PROPER(ref1)
RETURN .T.

*       ���������������������������������������������������������ķ
*       �                                                         �
*       � _RD40QWOKQ           employee.ref2 VALID                �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         NEWEMP1,     Record Number:   35   �
*       � Variable:            employee.ref2                      �
*       � Called By:           VALID Clause                       �
*       � Object Type:         Field                              �
*       � Snippet Number:      11                                 �
*       �                                                         �
*       ���������������������������������������������������������Ľ
*
FUNCTION _rd40qwokq     &&  employee.ref2 VALID
#REGION 1
REPLACE ref2 WITH PROPER(ref2)
RETURN .T.

*       ���������������������������������������������������������ķ
*       �                                                         �
*       � _RD40QWOO7           notes WHEN                         �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         NEWEMP1,     Record Number:   37   �
*       � Variable:            notes                              �
*       � Called By:           WHEN Clause                        �
*       � Object Type:         Field                              �
*       � Snippet Number:      12                                 �
*       �                                                         �
*       ���������������������������������������������������������Ľ
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

*       ���������������������������������������������������������ķ
*       �                                                         �
*       � _RD40QWOW9           Read Level Valid                   �
*       �                                                         �
*       � Function Origin:                                        �
*       �                                                         �
*       �                                                         �
*       � From Platform:       MS-DOS                             �
*       � From Screen:         NEWEMP1                            �
*       � Called By:           READ Statement                     �
*       � Snippet Number:      13                                 �
*       �                                                         �
*       ���������������������������������������������������������Ľ
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