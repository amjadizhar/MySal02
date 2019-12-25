* P.FXP 
* ----- 
*
PARAMETER _DRIVE
EXTERNAL PROC MKDIR
EXTERNAL PROC CLEANPATH
EXTERNAL PROC JUSTDRIVE
EXTERNAL PROC DRIVETYPE
EXTERNAL PROC MSGBOX
PRIVATE CURPROGRAM, LCONVERT, LDBFCREATE, CRICHIE, NCALENDAR
NCALENDAR = 0
LCONVERT = .F.
LDBFCREATE = .F.
LCONNUNIV = .F.
CURPROGRAM = SYS(16,1)
FLLPATH = LEFT(CURPROGRAM,AT('\',CURPROGRAM,OCCURS('\',CURPROGRAM)))
CRICHIE = .F.
CLOSE ALL
CLEAR
SET BELL OFF
SET CONFIRM ON
SET DATE british
SET DELETED ON
SET ESCAPE OFF
SET EXCLUSIVE OFF
SET HEADING OFF
SET REPROCESS TO AUTOMATIC
SET SAFETY OFF
SET SCOREBOARD OFF
SET TALK OFF
SET NOTIFY OFF
SET SYSMENU TO
SET KEYCOMP TO WINDOW
SET DEBUG OFF
SET PROCEDURE TO p00proc
SET MEMOWIDTH TO 255
SET READBORDER ON
SET STATUS BAR OFF
SET RESOURCE OFF

CLEAR MACROS

SET MACKEY TO

ON KEY LABEL ALT+F4 Do WinQuit
ON KEY LABEL ALT+F10 Do NullRet

JCSYSDIR = ''
JCFOXTOOLS = JCSYSDIR+'foxtools.fll'

SET LIBRARY TO (JCFOXTOOLS) ADDITIVE

PUBLIC UDF_BROWSE, UDF_DELETE, UDF_SAVE, UDF_EXIT, UDF_PAY
PUBLIC PLNAME, MCDEFFRMEX, MCPRDTNAME, MCICONNAME, MCKEYCODE
PUBLIC MCINVFRM, MCCHKFRM, MCSTAFRM
PUBLIC PNDEFAULTG
PUBLIC GLNODOCGEN, PRGVAR
PUBLIC MCFILETOMO, MLPRNFLAG, MLCREATE, MLLIGHTVER, SAMPLE

MCMESSAGE = 'Set Message to " "'

MLCREATE = .F.
MLPRNFLAG = .F.
MCFILETOMO = ''
MLLIGHTVER = .F.
GLNODOCGEN = .T.
SAMPLE = .T.
MCICONNAME = ''
PNDEFAULTG = 7
PRGVAR = 0
_WHICH_WIN = 99
MPROGRAM = 'A'

ON ERROR do error_msg with error(),message(),program(),line()

PUBLIC STATEPIC, ZIPPIC, PHONEPIC

SENDPRN = 'SCREEN'
COSTING = 'A'
COUNTRY = 1
STATEPIC = '!!'
ZIPPIC = 'XXXXX-XXXX'
PHONEPIC = '(999)999-9999'

MODIFY WINDOW SCREEN FONT 'FoxFont', 7

DO PSELICO

IF  .NOT. FILE('PA7.DBF')
   MCTRY = 1
   OLD = ''
   MDRIVE = ''
   PATH = ''
   IF  .NOT. FILE('ACCT.DBF')
      IF MLLIGHTVER=.F. .OR. FILE('DLXACC.ICO') .OR. FILE('EZAACC.ICO')
         IF FILE('EZAACC.ICO')
            IF  .NOT. FILE('DISCLAIM.DBF')
               DO DISCLAIM
            ENDIF
            SELECT 0
            USE NOUPDATE disclaim
            DO disclaim.spr
         ENDIF
         DO P1ECRE.SPR
      ELSE
         DATA_PATH = LEFT(CURDIR(),LEN(CURDIR())-1)
         MDRIVE = DATA_PATH
         DO CREATEFI
         DO UPD_ACC
         IF  .NOT. LCONVERT
            NEWCOMPANY = SPACE(30)
            = UPDDOCGEN()
         ENDIF
         DO P19GEN.SPR
      ENDIF
   ELSE
      LCONVERT = .T.
      MLCREATE = .T.
      CVER3CO = SPACE(30)
      CVER3REG = 0
      DATA_PATH = FLLPATH
      DO CONVERT
      IF FILE(DATA_PATH+'\SERVICE.DBF')
         IF  .NOT. USED('SERVICE')
            USE IN 0 (DATA_PATH+'\SERVICE.DBF')
         ENDIF
         SELECT SERVICE
         IF  .NOT. EOF() .AND.  .NOT. BOF()
            DO Invent.SPR
            DO SetupMsg.SPR
            = FIXCOGS()
         ENDIF
         IF USED('SERVICE')
            SELECT SERVICE
            USE
         ENDIF
      ENDIF
   ENDIF
   IF TYPE('cVer3Co')='C'
      IF  .NOT. USED('PA7')
         USE IN 0 ('PA7.DBF')
      ENDIF
      SELECT PA7
      APPEND BLANK
      REPLACE COMPANY WITH CVER3CO
      CCURDIR = LEFT(CURDIR(),LEN(CURDIR())-1)
      REPLACE PATH WITH SYS(5)+CCURDIR
      RELEASE CVER3CO, CCURDIR, CVER3REG
   ENDIF
   RELEASE STATEPIC, ZIPPIC, PHONEPIC
   CLOSE DATABASES
ELSE
   USE IN 0 LASTUSED
   SELECT LASTUSED
   DATA_PATH = IIF(EMPTY(LASTUSED.COMPPATH),LEFT(CURDIR(),LEN(CURDIR())-1),ALLTRIM(LASTUSED.COMPPATH))
   = CHKDIR()
   = CHKDATA()
ENDIF
DO INITVARS
SET READBORDER ON
TOP_COL = 0
TOP_ROW = 0
BOT_ROW = 0
BOT_COL = 0
DBF_CNT = 45
CLOSACC = .F.
CLOSSAL = .F.
PRGVAR = 0
MACLOAD = .F.
PRG_TYPE = 3
PUBLIC SHAREWARE, _VERSION, PASS_LEVEL, PIN_ARRAY[ 4], DBFS[ DBF_CNT]
PUBLIC _START, PIN_TOT
_VERSION = '7.0'
DO CASE
   CASE PRG_TYPE=2
      SHAREWARE = .F.
      SET EXCLUSIVE ON
      _VERSION = _VERSION+'R'
   CASE PRG_TYPE=3
      SHAREWARE = .F.
      SET EXCLUSIVE OFF
      _VERSION = _VERSION+'N'
ENDCASE
IF FILE('PA7.DBF')
   IF  .NOT. USED('PA7')
      USE IN 0 PA7
   ENDIF
   SELECT PA7
   MDRIVE = ALLTRIM(PATH)
   PCOMPANY = COMPANY
ENDIF
IF FILE('LASTUSED.DBF')
   IF  .NOT. USED('LASTUSED')
      USE IN 0 LASTUSED
   ENDIF
   SELECT LASTUSED
   IF  .NOT. EMPTY(COMPPATH)
      MDRIVE = ALLTRIM(COMPPATH)
      PCOMPANY = COMPNAME
   ENDIF
ENDIF
IF EMPTY(MDRIVE)
   IF MLLIGHTVER=.T.
      MDRIVE = LEFT(CURDIR(),LEN(CURDIR())-1)
   ENDIF
ENDIF
IF  .NOT. FILE(MDRIVE+'\DOCGEN.DBF')
   DATA_PATH = MDRIVE
   PATH = DATA_PATH
   IF MLLIGHTVER=.F.
      DO P1ECRE.SPR
   ELSE
      DATA_PATH = LEFT(CURDIR(),LEN(CURDIR())-1)
      MDRIVE = DATA_PATH
      DO CREATEFI
      DO UPD_ACC
      DO P19GEN.SPR
   ENDIF
   IF GLNODOCGEN=.F.
      QUIT
   ENDIF
   USE PA7.dbf
   CLOSE DATABASES
ENDIF
RELEASE CRICHIE
IF EMPTY(PCOMPANY)
   use &mdrive\docgen
   PCOMPANY = COMPANY
ENDIF
PCOMPANY = ALLTRIM(PCOMPANY)
SET SAFETY OFF
= UP_FILES()
SET SAFETY ON
use &mdrive\docgen
CCOMPANY = COMPANY
CNAME = NAME
CADDRESS1 = ADDRESS1
CADDRESS2 = ADDRESS2
CCITY = CITY
CSTATE = STATE
CZIP = ZIP
CPHONE1 = PHONE1
MPASS0 = LEFT(LINE1,25)
MPASS1 = LEFT(LINE2,25)
MPASS2 = LEFT(LINE3,25)
MPASS3 = LEFT(LINE4,25)
MPASS4 = LEFT(LINE5,25)
MPASS5 = SUBSTR(LINE1,26,25)
MPASS6 = SUBSTR(LINE2,26,25)
MPASS7 = SUBSTR(LINE3,26,25)
MPASS8 = SUBSTR(LINE4,26,25)
MPASS9 = SUBSTR(LINE5,26,25)
IF  .NOT. PASSOK()
   = PA7GETOUT()
ENDIF
I0 = 11
J0 = 40
PROSIZE = ':'+STR(I0*2,2,0)
PROPIC = ":P='"+REPLICATE('!',I0)+"'"
DOTS = 'Description..................................'
PRNDEF = SUBSTR(GENERAL,5,1)
DO CASE
   CASE PRNDEF='P'
      SENDPRN = 'Printer'
   CASE PRNDEF='F'
      SENDPRN = 'File'
   OTHERWISE
      SENDPRN = 'Screen'
ENDCASE
SCRLINES = VAL(SUBSTR(LINE15,17,2))
IF SCRLINES<>25 .AND. SCRLINES<>43 .AND. SCRLINES<>50
   SCRLINES = 25
ENDIF
MSIGN = SUBSTR(LINE10,21,5)
MDOLL = SUBSTR(LINE10,26,20)
INTERFACE = IIF(SUBSTR(LINE15,16,1)='Y',.T.,.F.)
M.INSHIP = IIF(SUBSTR(LINE15,17,1)='Y',.T.,.F.)
JOBCOST = IIF(SUBSTR(GENERAL,6,1)='Y',.T.,.F.)
HOLDINV = IIF(SUBSTR(GENERAL,7,1)='Y',.T.,.F.)
NEGUNITS = IIF(SUBSTR(GENERAL,8,1)='Y',.T.,.F.)
WRITEOFF = IIF(SUBSTR(GENERAL,9,1)='Y',.T.,.F.)
SHIPTO = IIF(SUBSTR(GENERAL,10,1)='Y',.T.,.F.)
EXPLOWIN = IIF(SUBSTR(GENERAL,11,1)='Y',.T.,.F.)
CALCCOMM = IIF(SUBSTR(GENERAL,12,1)='Y',.T.,.F.)
HOLDREV = IIF(SUBSTR(GENERAL,13,1)='Y',.T.,.F.)
GSTTAX = IIF(SUBSTR(GENERAL,14,1)='Y',.T.,.F.)
VATTAX = IIF(SUBSTR(GENERAL,19,1)='Y',.T.,.F.)
COSTING = SUBSTR(GENERAL,18,1)
COSTING = IIF(COSTING=' ','S',COSTING)
BROWTYPE = SUBSTR(GENERAL,20,1)
BROWTYPE = IIF(BROWTYPE=' ','N',BROWTYPE)
IS_EXPLODE = IIF(EXPLOWIN=.T.,'EXPLODE','  ')
MPRODUCTS = PRODUCTS
MPAY = PAYABLE
MCHECKING = CHECKING
MFRGHTE = FREIGHTEXP
MSALES = SALES
MRECEIV = RECEIVABLE
MCOGS = COGS
MFRGHTI = FREIGHTINC
MCOGSLIAB = SALESTAX
MDISCEXP = TAXEXP
MCINVADJ = PADJ
NCALENDAR = INT(VAL(ALLTRIM(PORT)))
IF SUBSTR(GENERAL,15,1)=' '
   ILEFT = 8
ELSE
   ILEFT = 8
ENDIF
IF SUBSTR(GENERAL,16,1)=' '
   IRIGHT = 2
ELSE
   IRIGHT = 2
ENDIF
IF ILEFT+IRIGHT=3
   CQTY = 'O'
ELSE
   CQTY = LEFT('Ordr.......',(IRIGHT+ILEFT)-1)
ENDIF
IF IRIGHT>0
   QTYPIC = ":P='"+REPLICATE('9',ILEFT)+'.'+REPLICATE('9',IRIGHT)+"'"
ELSE
   QTYPIC = ":P='"+REPLICATE('9',ILEFT)+"'"
ENDIF
MRATE0 = 0
MRATE1 = RATE1/100
MRATE2 = RATE2/100
MRATE3 = RATE3/100
MRATE4 = RATE4/100
MRATE5 = RATE5/100
MRATE6 = RATE6/100
MRATE7 = GSTRATE/100
MHEADB = COMPANY
COLHEAD = (80-LEN(RTRIM(COMPANY)))/2
COUNTRY = VAL(SUBSTR(LINE15,1,1))
DATEF = VAL(SUBSTR(LINE15,2,1))
IF COUNTRY=0
   COUNTRY = 1
ENDIF
DO CASE
   CASE COUNTRY=2
      STATEPIC = '!!!!'
      ZIPPIC = 'XXX-XXX'
      PHONEPIC = '(999)999-9999'
   CASE COUNTRY=3
      STATEPIC = '!!!'
      ZIPPIC = 'XXXX'
      PHONEPIC = '9999999999999'
   OTHERWISE
      STATEPIC = '!!'
      ZIPPIC = 'XXXXX-XXXX'
      PHONEPIC = '(999)999-9999'
ENDCASE
DO CASE
   CASE DATEF=2
      SET DATE ansi
   CASE DATEF=3
      SET DATE british
   CASE DATEF=4
      SET DATE german
   CASE DATEF=5
      SET DATE italian
   CASE DATEF=6
      SET DATE french
   OTHERWISE
      SET DATE american
ENDCASE
IF EMPTY(LPT)
   PRN_PORT = 'LPT1'
ELSE
   PRN_PORT = 'LPT'+ALLTRIM(STR(LPT))
ENDIF
PDRIVE = ALLTRIM(LEFT(LINE6,30))
IF INTERFACE
   IF FILE(PDRIVE+'\PA_INTER.DBF')
      use &pdrive\pa_inter
      IF RECCOUNT()<>0
         DO p00payup.prg
         use &mdrive\docgen
      ENDIF
   ENDIF
ENDIF
use &mdrive\docgen
PRNUM = VAL(SUBSTR(LINE15,14,2))
IF PRNUM=0
   PRNUM = 19
ENDIF
USE
IF FILE('PA7PRINT.DBF')
   USE PA7print
   GOTO PRNUM
   SN1 = SUBSTR(NORMAL,1,3)
   SN2 = SUBSTR(NORMAL,5,3)
   SN3 = SUBSTR(NORMAL,9,3)
   SN4 = SUBSTR(NORMAL,13,3)
   SN5 = SUBSTR(NORMAL,17,3)
   SN6 = SUBSTR(NORMAL,21,3)
   SC1 = SUBSTR(CONDENSE,1,3)
   SC2 = SUBSTR(CONDENSE,5,3)
   SC3 = SUBSTR(CONDENSE,9,3)
   SC4 = SUBSTR(CONDENSE,13,3)
   SC5 = SUBSTR(CONDENSE,17,3)
   SC6 = SUBSTR(CONDENSE,21,3)
   USE
ENDIF
DIMENSION DBFS[ DBF_CNT]
DBFS[ 1] = 'ACCOUNT'
DBFS[ 2] = 'CLIENT'
DBFS[ 3] = 'VENDOR'
DBFS[ 4] = 'SALESMAN'
DBFS[ 5] = 'SERVICE'
DBFS[ 6] = 'JOB'
DBFS[ 7] = 'ACCTYPE'
DBFS[ 8] = 'INVOICE'
DBFS[ 9] = 'INVLINE'
DBFS[ 10] = 'AROPEN'
DBFS[ 11] = 'RECEIV'
DBFS[ 12] = 'RECLINE'
DBFS[ 13] = 'GLTRAN'
DBFS[ 14] = 'GLREO'
DBFS[ 15] = 'ARREO'
DBFS[ 16] = 'ARLINREO'
DBFS[ 17] = 'BIREO'
DBFS[ 18] = 'BILINREO'
DBFS[ 19] = 'DIRCHK'
DBFS[ 20] = 'CHECKS'
DBFS[ 21] = 'PURCHASE'
DBFS[ 22] = 'PURLINE'
DBFS[ 23] = 'APOPEN'
DBFS[ 24] = 'PAYABLE'
DBFS[ 25] = 'PAYLINE'
DBFS[ 26] = 'INV-ADJ'
DBFS[ 27] = 'INVYTD'
DBFS[ 28] = 'CK-YTD'
DBFS[ 29] = 'GL-YTD'
DBFS[ 30] = 'APREO'
DBFS[ 31] = 'APLINREO'
DBFS[ 32] = 'BUILD'
DBFS[ 33] = 'JOBLOG'
DBFS[ 34] = 'LEVELS'
DBFS[ 35] = 'LIFOFIFO'
DBFS[ 36] = 'PURYTD'
DBFS[ 37] = 'PURLIYTD'
DBFS[ 38] = 'INVLIYTD'
DBFS[ 39] = 'ORDER'
DBFS[ 40] = 'ORDLINE'
DBFS[ 41] = 'RUNIT'
DBFS[ 42] = 'DOCGEN'
DBFS[ 43] = 'CLINOTES'
DBFS[ 44] = 'CASH'
DBFS[ 45] = 'ASSETS'

DO INITVARS

IF FILE('LASTUSED.DBF')
   IF  .NOT. USED('LASTUSED')
      USE IN 0 Lastused
   ENDIF
   SELECT LASTUSED
   IF TYPE('REGISKNT')='U'
      COPY TO Richie.Txt TYPE SDF
      CREATE DBF ('LastUsed.DBF') ( COMPNAME C ( 40 ), COMPPATH C ( 30 ), REGISKNT N ( 2 ), REGNO C ( 15 ) )
      APPEND FROM Richie.Txt TYPE SDF
      REPLACE REGISKNT WITH 25
      ERASE Richie.Txt
   ENDIF
   IF RECCOUNT()=0
      APPEND BLANK
      REPLACE REGISKNT WITH 25
   ENDIF
   IF REGISKNT<>-1
      DO NewReg.Spr
   ENDIF
   IF MCICONNAME='DIYACC.ICO' .AND. REGISKNT>0 .AND. LCONNUNIV=.F.
      DO GiveAway.Spr
   ENDIF
ENDIF

IF FILE('EZAACC.ICO')
   DO ezsplash.spr
ELSE
   DO p00about.spr
ENDIF

MODIFY WINDOW SCREEN TITLE plname+ pcompany ICON FILE &mcIconName
_INIT_LOAD = .T.
_IS_LOGO = .F.
PASS_LEVEL = 0
IF FILE('PA7P.DBF')
   USE PA7p
   SET FILTER TO PRG='P'
   GOTO TOP
   IF  .NOT. PASSOK()
      = OKMSG('Invalid Password!')
      SET COLOR TO
      CLEAR ALL
      QUIT
   ENDIF
ENDIF

DIMENSION PIN_ARRAY[ 4]
PIN_ARRAY[ 1] = '|'
PIN_ARRAY[ 2] = '/'
PIN_ARRAY[ 3] = '-'
PIN_ARRAY[ 4] = '\'
PIN_COUNTE = 1
PIN_TOT = 1

ON KEY LABEL ctrl+w *
ON KEY LABEL ctrl+q *
ON KEY LABEL alt+j do CleLastKey
ON KEY LABEL f1 do help_call
IF MLLIGHTVER=.T.
   DO LiteMenu.Mpr
   IF INLIST(MCICONNAME,'SWIACC.ICO','DLXACC.ICO','EZAACC.ICO')
      DEFINE BAR 14 OF FILE PROMPT '\<New Company' BEFORE 1
      DEFINE BAR 15 OF FILE PROMPT '\<Open Company' BEFORE 1
      ON SELECTION BAR 14 OF FILE DO P1ECRE.SPR
      ON SELECTION BAR 15 OF FILE DO P1DOPN.SPR
   ENDIF
   IF INLIST(MCICONNAME,'DLXACC.ICO','EZAACC.ICO')
      DEFINE BAR 16 OF FILE PROMPT '\<Jobs' AFTER 9
      ON SELECTION BAR 16 OF FILE do p18job.spr
      DEFINE PAD _RJ712RGQH OF _MSYSMENU PROMPT 'Purcha\<se' AFTER FILE SKIP FOR PRGVAR<>0 KEY ALT+S, 'ALT+S' COLOR SCHEME 3
      ON PAD _RJ712RGQH OF _MSYSMENU PAD POPUP PURCHASE
      DEFINE POPUP PURCHASE SHADOW MARGIN RELATIVE COLOR SCHEME 4
      DEFINE BAR 1 OF PURCHASE PROMPT '\<Purchase Orders'
      DEFINE BAR 2 OF PURCHASE PROMPT 'Pri\<nt Purchase Orders'
      DEFINE BAR 3 OF PURCHASE PROMPT "\<Receive PO's"
      DEFINE BAR 4 OF PURCHASE PROMPT 'Prin\<t Received PO'
      DEFINE BAR 5 OF PURCHASE PROMPT "P\<ost Received PO's"
      ON SELECTION BAR 1 OF PURCHASE do p21po.spr
      ON SELECTION BAR 2 OF PURCHASE DO _rvr0s2kos  IN LOCFILE("PROG\P8K" ,"MPX;MPR|FXP;PRG" ,"Where is P8K?")
      ON SELECTION BAR 3 OF PURCHASE do p22mr.spr
      ON SELECTION BAR 4 OF PURCHASE DO _rvr0s2kp3  IN LOCFILE("PROG\P8K" ,"MPX;MPR|FXP;PRG" ,"Where is P8K?")
      ON SELECTION BAR 5 OF PURCHASE do p27popst.spr
      DEFINE BAR 11 OF RECEIVABLE PROMPT 'A/R \<Transactions' AFTER 5
      DEFINE BAR 12 OF RECEIVABLE PROMPT '\<Finance Charges' AFTER 8
      DEFINE BAR 13 OF RECEIVABLE PROMPT '\<Sales Tax Report' AFTER 10
      DEFINE BAR 14 OF RECEIVABLE PROMPT 'Sales \<Analysis Report'
      DEFINE BAR 15 OF RECEIVABLE PROMPT '\<Estimates' BEFORE 1
      DEFINE BAR 16 OF RECEIVABLE PROMPT '\<Print Estimates' AFTER 15
      ON SELECTION BAR 11 OF RECEIVABLE DO P41AR.SPR		
      ON SELECTION BAR 12 OF RECEIVABLE DO P45FINCH.SPR
      ON SELECTION BAR 13 OF RECEIVABLE do p65taxpr
      ON SELECTION BAR 14 OF RECEIVABLE do _rvr0s2kso  IN LOCFILE("PROG\P8K" ,"MPX;MPR|FXP;PRG" ,"Where is P8K?")
      ON SELECTION BAR 15 OF RECEIVABLE do p31est.spr
      ON SELECTION BAR 16 OF RECEIVABLE DO _rvr0s2kqa  IN LOCFILE("PROG\P8K" ,"MPX;MPR|FXP;PRG" ,"Where is P8K?")
      IF MCICONNAME='DLXACC.ICO'
         DEFINE BAR 17 OF RECEIVABLE PROMPT 'A/R Rec\<urring Entries' AFTER 11
         ON BAR 17 OF RECEIVABLE PAD POPUP ARRECURRIN
         DEFINE POPUP ARRECURRIN SHADOW MARGIN RELATIVE COLOR SCHEME 4
         DEFINE BAR 1 OF ARRECURRIN PROMPT '\<Enter Recurring Entries'
         DEFINE BAR 2 OF ARRECURRIN PROMPT '\<Print List of Recurring Entries'
         DEFINE BAR 3 OF ARRECURRIN PROMPT '\<Generate Recurring Entries'
         ON SELECTION BAR 1 OF ARRECURRIN DO P461ARRE.SPR
         ON SELECTION BAR 2 OF ARRECURRIN DO P462ARPR
         ON SELECTION BAR 3 OF ARRECURRIN DO P463ARGN.SPR
         DEFINE BAR 9 OF LEDGER PROMPT 'G/L \<Recurring Entries' AFTER 1
         ON BAR 9 OF LEDGER PAD POPUP GLRECURRIN
         DEFINE POPUP GLRECURRIN SHADOW MARGIN RELATIVE COLOR SCHEME 4
         DEFINE BAR 1 OF GLRECURRIN PROMPT '\<Enter Recurring Entries'
         DEFINE BAR 2 OF GLRECURRIN PROMPT '\<Print List of Recurring Entries'
         DEFINE BAR 3 OF GLRECURRIN PROMPT '\<Generate Recurring Entries'
         ON SELECTION BAR 1 OF GLRECURRIN DO P671GLRE.SPR
         ON SELECTION BAR 2 OF GLRECURRIN DO P672GLPR
         ON SELECTION BAR 3 OF GLRECURRIN DO P673GLGN.SPR
      ENDIF
   ENDIF
   DEFINE BAR 7 OF _QXD12ALL8 PROMPT '\<Notepad'
   DEFINE BAR 8 OF _QXD12ALL8 PROMPT '\<File Manager'
   DEFINE BAR 9 OF _QXD12ALL8 PROMPT '\<System Editor'
   ON SELECTION BAR 7 OF _QXD12ALL8 run /n notepad
   ON SELECTION BAR 8 OF _QXD12ALL8 run /n winfile
   ON SELECTION BAR 9 OF _QXD12ALL8 run /n sysedit
ELSE
   DO p8k.mpr
ENDIF
CLEAR
_IS_LOGO = .T.
_INIT_LOAD = .F.
XPASS_LEVE = PASS_LEVEL
DO INITCO
PASS_LEVEL = XPASS_LEVE
DO CUSTOM_SET
DATA_PATH = MDRIVE
XLCREATE = MLCREATE
M.INCLUDE = .T.
MLCREATE = .T.
LDBFCREATE = .F.
DO CREATEFI
MLCREATE = XLCREATE
RELEASE M.INCLUDE, XLCREATE
IF MLCREATE=.F.
   = CHKFILES()
ENDIF
IF DAY(DATE())<=7 .AND. CDOW(DATE())='Monday'
   PRGVAR = 1
   DO Reminder.Spr
   PRGVAR = 0
ENDIF
IF INLIST(MONTH(DATE()),1,7) .AND. CDOW(DATE())='Monday' .AND. DAY(DATE())<=7
   DO QtrUpdt.Spr
ENDIF
ON SHUTDOWN =PA7GETOUT()	
_GOODBYE = .F.
DO WHILE _GOODBYE=.F.
   DO p00tbar.prg
   CLEAR GETS
ENDDO

PROCEDURE CANCEL_IT
SET SYSMENU TO DEFAULT
CANCEL
SET SYSMENU TO DEFAULT
ON ERROR 
SET NOTIFY ON
SET TALK WINDOW
SET SYSMENU TO
SET KEYCOMP TO WINDOW
SET DEBUG OFF
SET PROCEDURE TO
SET BELL ON
ON KEY LABEL * 

PROCEDURE PA7SHUTDOW
RETURN

PROCEDURE CLELASTKEY
IF LASTKEY()<>27 .AND. LASTKEY()<>-3 .AND. LASTKEY()<>24
   KEYBOARD CHR(32) CLEAR
   = INKEY(0)
ENDIF
RETURN

PROCEDURE CLRMSG
JCOLDWIN = WONTOP()
IF EMPTY(WONTOP())
   ACTIVATE SCREEN
   @ 15, 0 CLEAR
ENDIF
IF  .NOT. EMPTY(JCOLDWIN)
   IF OCCURS('Calendar',JCOLDWIN)>0
      ACTIVATE WINDOW CALENDAR
   ELSE
      IF TYPE('jcOldWin')='C'
         ACTIVATE WINDOW (JCOLDWIN)
      ENDIF
   ENDIF
ENDIF
IF TYPE('_goodbye')<>'U' .AND. FILE('SWIACC.ICO')
ENDIF
RETURN

PROCEDURE DISCLAIM
CREATE DBF disclaim ( DISCLAIM M )
APPEND BLANK
REPLACE DISCLAIM WITH 'This software is designed to provide accurate and authoritative information '+'in regard to the subject matter covered. It is sold with the understanding that the '+'publisher, author, distributor and retailer are not engaged in rendering legal, '+'accounting or other professional services. If legal advice or other expert assistance '+'is required, the services of a competent professional should be sought. From a '+'Declaration of Principles jointly adopted by a committee of the American Bar Association '+'and a committee of publishers.'+CHR(13)+CHR(13)
REPLACE DISCLAIM WITH 'This self-help legal product is intended to be used by the customer for '+'his/her own benefit. It may not be reproduced in whole or in part, resold or used for '+'commercial purposes without the written permission from the publisher. In addition to '+'copyright violations, the unauthorized reproduction and use of this product to benefit a '+'second party may be considered the unauthorized practice of law.'+CHR(13)+CHR(13)+'This product is designed to provide authoritative and accurate information '+'in regard to the subject matter covered. However, the accuracy of the information is '+'not guaranteed, as laws, regulations and accepted accounting principles may change or '+'be subject to differing interpretations. Consequently, you may be responsible for '+'following alternative procedures, or using material or forms different from those '+'supplied with this product.'+CHR(13)+CHR(13) ADDITIVE
REPLACE DISCLAIM WITH 'As with any accounting matter, common sense should determine whether you '+'need the assistance of a qualified accountant. We urge you to seek relevant expert '+'advice, whenever substantial sums of money are involved, you doubt the suitability of '+'the product you have purchased, or if there is anything about the product that you do '+'not understand. Even if you are completely satisfied with this product, we encourage '+'you to have your accountant review it.'+CHR(13)+CHR(13) ADDITIVE
REPLACE DISCLAIM WITH 'It is understood that by using this product, you are acting as your own '+'accountant. Neither the author, publisher, distributor nor retailer are engaged in '+'rendering legal, accounting or other professional services. Accordingly, the publisher, '+'author, distributor and retailer shall have neither liability nor responsibility to '+'any party for any loss or damage caused or alleged to be caused by the use of this '+'product.'+CHR(13)+CHR(13) ADDITIVE
REPLACE DISCLAIM WITH 'E-Z Legal Software warrants that for 90 days from the date of purchase the '+'disk(s) on which this program is recorded will satisfactorily perform with the '+'documentation furnished.  Return the disk(s) along with a written report of any defects '+'to E-Z Legal Software, and we will replace the disk(s) free of charge as long as they '+'are returned within 90 days from date of purchase. In no event shall our liability or '+'the liability of any retailer exceed the purchase price of the product. Use of this '+'product constitutes acceptance of these terms.'+CHR(13)+CHR(13) ADDITIVE
USE
