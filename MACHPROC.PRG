* MACHPROC.PRG
* AMJAD IZHAR
* 24-NOV.2005
* FOR ASKAR 



*******************
PROCEDURE MACH_MEN
*******************
DO WHILE .T.
   L= 8
   M= 23

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   DO top
   DO bot

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 6, 25 SAY ' Machine Code File Maintenance'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ L+0, M PROMPT ' 1 - Machine Code .... Entry      '
   @ L+2, M PROMPT ' 2 - Machine Code .... Correction '
   @ L+4, M PROMPT ' 3 - Machine Code .... Deletion   '
   @ L+6, M PROMPT ' 4 - Machine Code .... Printing   '
   @ L+8, M PROMPT ' 9 - Return to Previous Menu      '
   MCHOICE= 0
   MENU TO MCHOICE
   DO CASE
      CASE MCHOICE=5
         CLEAR
         CLOSE ALL
         RETURN
      CASE MCHOICE=4
         CLEAR
         DO mach_prt
      CASE MCHOICE=3
         CLEAR
         DO mach_del
      CASE MCHOICE=2
         CLEAR
         DO mach_cor
      CASE MCHOICE=1
         CLEAR
         DO mach_ent
   ENDCASE
ENDDO
RETURN
*******

******************
PROCEDURE MACH_ENT
******************
CLEAR
CLOSE ALL
SELECT 1
USE mach ORDER mach_idx
SELECT 2
USE dept ORDER dept_idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO MMACH_CODE, MDEPT_CODE
   STORE SPACE(25) TO MMACH

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 26 SAY 'Machine Codes Entry'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   DO WHILE .T.
      @ 23, 42 SAY 'Press Ctrl Q to Return'

IF _DOS
      SET COLOR TO /w 
ENDIF

      @ 9, 34 SAY '   -    '

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 9, 20 SAY 'Machine Code '
      @ 9, 34 GET MDEPT_CODE PICTURE '999'
      READ

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
      @ 9, 34 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MDEPT_CODE=0
         LOOP
      ENDIF
      @ 23, 40 CLEAR TO 23, 73
      SELECT 2
      SEEK MDEPT_CODE
      IF .not. FOUND()
         @ 23, 20 SAY 'This Department Code Does Not Exist ---Please Retry'
         WAIT ''
         @ 23, 19 SAY SPACE(60)
         LOOP
      ENDIF
      @ 11, 20 SAY 'Department  '

IF _DOS
      SET COLOR TO /w
ENDIF

      @ 11, 34 SAY DEPT

IF _DOS
      SET COLOR TO 7/1
ENDIF

      @ 9, 38 GET MMACH_CODE PICTURE '9999'
      READ

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MMACH_CODE))))+LTRIM(STR(MMACH_CODE))
      @ 9, 38 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      IF MMACH_CODE=0
         LOOP
      ENDIF
      SELECT 1
      STORE STR(MDEPT_CODE,3)+STR(MMACH_CODE,4) TO XYZ
      SEEK XYZ
      IF FOUND()
         @ 23, 20 SAY 'This Machine Code Already Exist ---Please Retry'
         WAIT ''
         @ 23, 19 SAY SPACE(60)
         LOOP
      ENDIF
      @ 13, 20 SAY 'Machine      '
      @ 13, 34 GET MMACH
      READ
      MYES= ' '
      DO WHILE .not. MYES$'YN'
         @ 23, 28 SAY ' Any Correction - Y/N ' GET MYES FUNCTION '!'
         READ
      ENDDO
      @ 23, 19 CLEAR TO 23, 73
      IF MYES='Y'
         LOOP
      ENDIF
      SELECT 1
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE MACH_CODE WITH MMACH_CODE, MACH WITH MMACH, DEPT_CODE WITH MDEPT_CODE
      UNLOCK
      P= ' '
      DO WHILE .not. P$'YN'
         @ 23, 28 SAY ' More Data Entry - Y/N ' GET P FUNCTION '!'
         READ
      ENDDO
      IF P='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
*******

******************
PROCEDURE MACH_COR
******************
CLEAR
CLOSE ALL
SELECT 1
USE mach ORDER mach_idx
SELECT 2
USE dept ORDER dept_idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO MMACH_CODE, MDEPT_CODE

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 26 SAY 'Machine Codes Correction'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 42 SAY 'Press Ctrl Q to Return'

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 9, 34 SAY '   -    '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 9, 20 SAY 'Machine Code '
   @ 9, 34 GET MDEPT_CODE PICTURE '999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
   @ 9, 34 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MDEPT_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 73
   SELECT 2
   SEEK MDEPT_CODE
   IF .not. FOUND()
      @ 23, 20 SAY 'This Department Code Does Not Exist ---Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(60)
      LOOP
   ENDIF
   @ 11, 20 SAY 'Department  '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 34 SAY DEPT

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 9, 38 GET MMACH_CODE PICTURE '9999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MMACH_CODE))))+LTRIM(STR(MMACH_CODE))
   @ 9, 38 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF MMACH_CODE=0
      LOOP
   ENDIF
   SELECT 1
   STORE STR(MDEPT_CODE,3)+STR(MMACH_CODE,4) TO XYZ
   SEEK XYZ
   IF .not. FOUND()
      @ 23, 20 SAY 'This Machine Code Does Not Exist ---Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(60)
      LOOP
   ENDIF
   @ 13, 20 SAY 'Machine      ' GET MACH
   READ
   MYES= ' '
   DO WHILE .not. MYES$'YN'
      @ 23, 28 SAY ' More Data Correction - Y/N ' GET MYES FUNCTION '!'
      READ
   ENDDO
   IF MYES='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
*******

******************
PROCEDURE MACH_DEL
******************
CLEAR
CLOSE ALL
SET DELETED ON
SELECT 1
USE mach ORDER mach_idx
SELECT 2
USE dept ORDER dept_idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR
   STORE 0 TO MMACH_CODE, MDEPT_CODE

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 26 SAY 'Machine Codes Deletion'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 23, 42 SAY 'Press Ctrl Q to Return'

IF _DOS
   SET COLOR TO /w 
ENDIF

   @ 9, 34 SAY '   -    '

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 9, 20 SAY 'Machine Code '
   @ 9, 34 GET MDEPT_CODE PICTURE '999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MDEPT_CODE))))+LTRIM(STR(MDEPT_CODE))
   @ 9, 34 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MDEPT_CODE=0
      LOOP
   ENDIF
   @ 23, 40 CLEAR TO 23, 73
   SELECT 2
   SEEK MDEPT_CODE
   IF .not. FOUND()
      @ 23, 20 SAY 'This Department Code Does Not Exist ---Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(60)
      LOOP
   ENDIF
   @ 11, 20 SAY 'Department  '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 34 SAY DEPT

IF _DOS
   SET COLOR TO 7/1
ENDIF

   @ 9, 38 GET MMACH_CODE PICTURE '9999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MMACH_CODE))))+LTRIM(STR(MMACH_CODE))
   @ 9, 38 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF MMACH_CODE=0
      LOOP
   ENDIF
   SELECT 1
   STORE STR(MDEPT_CODE,3)+STR(MMACH_CODE,4) TO XYZ
   SEEK XYZ
   IF .not. FOUND()
      @ 23, 20 SAY 'This Machine Code Does Not Exist ---Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(60)
      LOOP
   ENDIF
   @ 13, 20 SAY 'Machine      '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 13, 34 SAY MACH

IF _DOS
   SET COLOR TO 7/1
ENDIF

   MYES= ' '
   DO WHILE .not. MYES$'YN'
      @ 23, 28 SAY ' Are You Sure - Y/N ' GET MYES FUNCTION '!'
      READ
   ENDDO
   @ 23, 27 CLEAR TO 23, 54
   IF MYES='Y'
      SELECT 1
      DELETE
   ENDIF
   MYES= ' '
   DO WHILE .not. MYES$'YN'
      @ 23, 28 SAY ' More Data Deletion - Y/N ' GET MYES FUNCTION '!'
      READ
   ENDDO
   IF MYES='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******

******************
PROCEDURE MACH_PRT
******************

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
CLOSE ALL
SELECT 1
USE mach ORDER mach_idx
SELECT 2
USE dept ORDER dept_idx
SELECT 1
IF RECCOUNT()=0
   CLEAR
   @ 12, 12 SAY 'No Codes in Code File  -  Press any key to return to menu .... '
   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit - P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
HEADING= .T.
PAGE= 0
DO WHILE .not. EOF()
   IF HEADING
      PAGE= PAGE+1
      L= 1
      IF P='P'
         DO top-prt
         @ 3, 10 SAY CHR(14)
         @ 3, 17 SAY 'MACHINE    CODES'
         L= L+2
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         L= L+2
         @ L, 22 SAY 'M A C H I N E     C O D E S'
      ENDIF
      L= L+1
      @ L, 66 SAY 'Page No -'
      @ L, 77 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 5 SAY '   Code '
      @ L, 22 SAY 'Department'
      @ L, 54 SAY 'Machine'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      HEADING= .F.
   ENDIF
   L= L+1
   @ L, 8 SAY DEPT_CODE PICTURE '999'
   @ L, 11 SAY '-'
   MPC1= REPLICATE('0',3-LEN(LTRIM(STR(DEPT_CODE))))+LTRIM(STR(DEPT_CODE))
   @ L, 8 SAY MPC1
   SELECT 1
   @ L, 12 SAY MACH_CODE PICTURE '9999'
   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MACH_CODE))))+LTRIM(STR(MACH_CODE))
   @ L, 12 SAY MPC1
   SELECT 2
   LOCATE FOR DEPT_CODE= A->DEPT_CODE
   IF FOUND()
      @ L, 22 SAY DEPT
   ENDIF
   SELECT 1
   @ L, 54 SAY MACH
   SKIP
   IF P='S'.and.L>20
      ANS= ' '
      DO WHILE .not. ANS$'CQ'
         @ 24, 58 SAY 'Continue/Quit C/Q' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='C'
         CLEAR
         HEADING= .T.
      ELSE
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
   ENDIF
   IF P='P'.and.L>=56
      HEADING= .T.
   ENDIF
ENDDO
IF P='S'
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLOSE ALL
CLEAR
RETURN
*******
