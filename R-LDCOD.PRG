*-----------------------------------*
*Program to Report Party Codes		*
*Author		:	Muhammad Usman		*
*Date		:	29-01-2000			*
*Prog		:	R-Ldcod				*
*Last Modify:	10-02-2000			*
*-----------------------------------*
* Select Files
* 
* 
* R-LDCOD.FXP 
* ----------- 
*
CLOSE DATABASES
SELECT 1
USE SHARED Ldgrcod ALIAS LDGRCOD
SELECT 2
USE SHARED City ALIAS CITY
SET ORDER TO C_Code
MC1 = SPACE(2)
MC2 = SPACE(2)
MC3 = SPACE(3)
MTITL = SPACE(40)
MCN = SPACE(1)
MPS = SPACE(1)
MPAGE = 1
MLINE = 0
MCQ = SPACE(1)
MSR_NO = 0
DEFINE WINDOW RLDCOD FROM INT((SROWS()-17)/1), INT((SCOLS()-68)/1) TO INT((SROWS()-17)/1)+9, INT((SCOLS()-68)/1)+50 NOFLOAT NOCLOSE SHADOW NOMINIMIZE PANEL COLOR R/G 
ACTIVATE WINDOW SAME RLDCOD
SELECT LDGRCOD
CLEAR
@ 4, 5 SAY 'Party Code Wise / Party Name Wise' COLOR W/N 
@ 4, 40 GET MCN PICTURE '@!' VALID MCN='C' .OR. MCN='N' ERROR 'Code / Name (C/N)' COLOR W/N 
READ
CLEAR
@ 3, 13 SAY 'Report in Processing'
@ 5, 13 SAY "Please Don't Distrub"
IF MCN='C'
   SET ORDER TO C123
   MTITLE = 'Code Wise List Of All Parties'
ELSE
   SET ORDER TO Title
   MTITLE = 'Name Wise List Of All Parties'
ENDIF
GOTO TOP
CLEAR
@ 4, 5 SAY 'Report To Printer Or Screen (P/S)' COLOR W/N 
@ 4, 40 GET MPS PICTURE '@!' VALID MPS='P' .OR. MPS='S' ERROR 'Printer Or Screen (P/S)' COLOR W/N 
READ
SET RELATION TO C_CODE INTO CITY ADDITIVE
IF MPS='P'
   DO REPPRTPRO
ELSE
   DO REPSCRPRO
ENDIF
RELEASE WINDOW RLDCOD
SET PRINTER OFF
SET DEVICE TO SCREEN
CLOSE DATABASES
RETURN

PROCEDURE REPPRTPRO
SET PRINTER ON
SET DEVICE TO PRINTER
MLINE = 6
@ 1, 1 SAY MCOMP
@ 1, 67 SAY 'Page: '+STR(MPAGE,2)
@ 2, 1 SAY MTITLE
@ 2, 62 SAY 'Date: '+DTOC(DATE())
@ 3, 0 SAY REPLICATE('-',80)
@ 4, 0 SAY 'SERIAL    CODE          NAME                          CURRENT     CITY'
@ 5, 0 SAY 'NUMBER                                                BALANCE'
@ 6, 0 SAY REPLICATE('-',80)
DO WHILE  .NOT. EOF()
   MLINE = MLINE+1
   MSR_NO = MSR_NO+1
   IF MCN='C'
      IF STR(C1,2)<>MC1
         MLINE = MLINE+1
      ENDIF
   ENDIF
   MC1 = STR(C1,2)
   MC2 = STR(C2,2)
   MC3 = STR(C3,3)
   DO LEDCODPRO IN Proc1
   @ MLINE, 0 SAY STR(MSR_NO,4)+' '+MC1+'-'+MC2+'-'+MC3+' '+TITLE
   @ MLINE, 47 SAY LDGR_BAL PICTURE '999,999,999.99'
   @ MLINE, 62 SAY STATUS
   @ MLINE, 65 SAY C_CODE+' '+LEFT(CITY.NAME,8)
   SKIP
   IF MLINE>58
      EJECT
      MPAGE = MPAGE+1
      MLINE = 6
      @ 1, 1 SAY MCOMP
      @ 1, 67 SAY 'Page: '+STR(MPAGE,2)
      @ 2, 1 SAY MTITLE
      @ 2, 62 SAY 'Date: '+DTOC(DATE())
      @ 3, 0 SAY REPLICATE('-',80)
      @ 4, 0 SAY 'SERIAL    CODE          NAME                          CURRENT     CITY'
      @ 5, 0 SAY 'NUMBER                                                BALANCE'
      @ 6, 0 SAY REPLICATE('-',80)
   ENDIF
ENDDO
MLINE = MLINE+1
@ MLINE, 0 SAY REPLICATE('-',77)
MLINE = MLINE+1
@ MLINE, 65 SAY 'End Of Report'
EJECT
RETURN

PROCEDURE REPSCRPRO
DO DSPSCRPRO IN Proc1
MLINE = 6
@ 1, 1 SAY MCOMP
@ 2, 1 SAY MTITLE
@ 2, 62 SAY 'Date: '+DTOC(DATE())
@ 3, 0 SAY REPLICATE('-',78)
@ 4, 0 SAY 'SERIAL    CODE          NAME                          CURRENT     CITY'
@ 5, 0 SAY 'NUMBER                                                BALANCE'
@ 6, 0 SAY REPLICATE('-',78)
DO WHILE  .NOT. EOF()
   MLINE = MLINE+1
   MSR_NO = MSR_NO+1
   IF MCN='C'
      IF STR(C1,2)<>MC1
         MLINE = MLINE+1
      ENDIF
   ENDIF
   MC1 = STR(C1,2)
   MC2 = STR(C2,2)
   MC3 = STR(C3,3)
   DO LEDCODPRO IN Proc1
   @ MLINE, 0 SAY STR(MSR_NO,4)+' '+MC1+'-'+MC2+'-'+MC3+' '+TITLE
   @ MLINE, 47 SAY LDGR_BAL PICTURE '999,999,999.99'
   @ MLINE, 62 SAY STATUS
   @ MLINE, 65 SAY C_CODE+' '+LEFT(CITY.NAME,8)
   SKIP
   IF MLINE>18
      @ 21, 28 SAY 'Continue Or Quit C/Q'
      @ 21, 50 GET MCQ PICTURE '@!' VALID MCQ='C' .OR. MCQ='Q' ERROR 'Continue Or Quit (C/Q)'
      READ
      IF MCQ='Q'
         RELEASE WINDOW SCR
         RETURN
      ENDIF
      @ 7, 0 CLEAR TO 21, 78
      MLINE = 6
   ENDIF
ENDDO
WAIT WINDOW 'End of Report.....'
RELEASE WINDOW SCR
RETURN
