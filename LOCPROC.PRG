* PROGRAM : LOCPROC.PRG
* WRITTEN BY : AMJAD IZHAR
* THIS REVISION: 14-05-2005
********************************

*****************
PROCEDURE LOC_ENT
*****************
CLEAR
CLOSE ALL
USE loc ORDER loc_idx
DO WHILE .T.



IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 30 SAY 'Location Codes Entry'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   STORE 0 TO MLOC_CODE, CNT
   STORE SPACE(25) TO MLOC
   DO WHILE .T.
      @ 23, 42 SAY 'Press Ctrl Q to Return'
      @ 9, 20 SAY 'Location Code  ' GET MLOC_CODE PICTURE '9999'
      READ

IF _DOS
      SET COLOR TO /w
ENDIF

      MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
      @ 9, 36 SAY MPC1

IF _DOS
      SET COLOR TO 7/1
ENDIF

      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MLOC_CODE=0
         LOOP
      ENDIF
      SEEK MLOC_CODE
      @ 23, 40 CLEAR TO 23, 63
      IF CNT=0
         IF FOUND()
            @ 23, 20 SAY 'This Code Already Exist ... Please Retry'
            WAIT ''
            @ 23, 19 SAY SPACE(40)
            LOOP
         ENDIF
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE LOC_CODE WITH MLOC_CODE
         UNLOCK
         CNT= 1
      ENDIF
      @ 11, 20 SAY 'Location       ' GET MLOC
      READ
      MYES= ' '
      DO WHILE .not. MYES$'YN'
         @ 23, 28 SAY 'Any Correction - Y/N ' GET MYES FUNCTION '!'
         READ
      ENDDO
      @ 23, 19 CLEAR TO 23, 61
      IF MYES='Y'
         LOOP
      ENDIF
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE LOC WITH MLOC
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
******
*****************
PROCEDURE LOC_COR
******************
CLEAR
CLOSE ALL
USE loc INDEX loc_idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 25 SAY 'Location Codes Correction'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   MLOC_CODE= 0
   @ 23, 42 SAY 'Press Ctrl Q to Return'
   @ 9, 20 SAY 'Location Code  ' GET MLOC_CODE PICTURE '9999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
   @ 9, 36 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SEEK MLOC_CODE
   @ 23, 40 CLEAR TO 23, 63
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exist ... Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(40)
      LOOP
   ENDIF
   @ 11, 20 SAY 'Location       ' GET LOC
   READ
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 28 SAY ' More Data Correction - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
*******
*****************
PROCEDURE LOC_DEL
*****************
CLEAR
CLOSE ALL
SET DELETED ON
USE loc ORDER loc_idx
DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1
ENDIF

   CLEAR

IF _DOS
   SET COLOR TO +7/1
ENDIF

   @ 5, 25 SAY 'Location Codes Deletion'

IF _DOS
   SET COLOR TO 7/1
ENDIF

   MLOC_CODE= 0
   @ 23, 42 SAY 'Press Ctrl Q to Return'
   @ 9, 20 SAY 'Location Code  ' GET MLOC_CODE PICTURE '9999'
   READ

IF _DOS
   SET COLOR TO /w
ENDIF

   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(MLOC_CODE))))+LTRIM(STR(MLOC_CODE))
   @ 9, 36 SAY MPC1

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   IF MLOC_CODE=0
      LOOP
   ENDIF
   SEEK MLOC_CODE
   @ 23, 40 CLEAR TO 23, 63
   IF .not. FOUND()
      @ 23, 20 SAY 'This Code Does Not Exist ... Please Retry'
      WAIT ''
      @ 23, 19 SAY SPACE(40)
      LOOP
   ENDIF
   @ 11, 20 SAY 'Location     '

IF _DOS
   SET COLOR TO /w
ENDIF

   @ 11, 36 SAY LOC

IF _DOS
   SET COLOR TO 7/1
ENDIF

   MYES= ' '
   DO WHILE .not. MYES$'YN'
      @ 23, 30 SAY ' Are You Sure Y/N ' GET MYES FUNCTION '!'
      READ
   ENDDO
   @ 23, 20 CLEAR TO 23, 78
   IF MYES='Y'
      DELETE
   ENDIF
   P= ' '
   DO WHILE .not. P$'YN'
      @ 23, 28 SAY ' More Data Deletion - Y/N ' GET P FUNCTION '!'
      READ
   ENDDO
   IF P='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
*************

******************
PROCEDURE LOC_PRT
******************
CLEAR
CLOSE ALL

IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
USE loc ORDER loc_idx
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
      L= 1
      PAGE= PAGE+1
      IF P='P'
         DO top-prt
         @ 3, 10 SAY CHR(14)
         @ 3, 17 SAY ' LOCATION      CODES'
         L= L+2
      ELSE
         DO SCREEN IN TBL_PROC.PRG
         L= L+2
         @ L, 22 SAY 'L O C A T I O N       C O D E S'
      ENDIF
      L= L+1
      @ L, 66 SAY 'Page No -'
      @ L, 77 SAY PAGE PICTURE '99'
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      L= L+1
      @ L, 25 SAY 'Code '
      @ L, 40 SAY 'Location '
      L= L+1
      @ L, 1 SAY REPLICATE('-',79)
      HEADING= .F.
   ENDIF
   L= L+1
   @ L, 25 SAY LOC_CODE
   MPC1= REPLICATE('0',4-LEN(LTRIM(STR(LOC_CODE))))+LTRIM(STR(LOC_CODE))
   @ L, 25 SAY MPC1
   @ L, 40 SAY LOC
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
CLEAR
CLOSE ALL
RETURN
****************


