* PROGRAM: MAININVT.PRG
* WRITTEN BY : AMJAD IZHAR
* THIS REVISION: 19-12-2019
**********************************************

CLEAR
CLOSE DATA

 SET DATE TO BRIT
 SET CENTURY ON
 SET SCORE OFF
 SET BELL OFF
 SET SAFETY OFF
 SET TALK OFF
 SET CONFIRM OFF
 SET STATUS OFF
 SET STEP OFF
 SET ECHO OFF
 SET DOHISTORY OFF

 STORE DATE() TO DATE

DO WHILE .T.

	SET COLOR TO 7/1 

   STORE SPACE(6) TO MPW
   CLEAR

   DO top IN TBL_PROC.PRG
   DO bot IN TBL_PROC.PRG

   			SET COLOR TO +7/1

   		@ 6, 28 SAY 'INVENTORY MAIN MENU'

   R=8
   C=24
   @ R, C PROMPT '  1  -  Code Files         '
   @ R+1, C PROMPT '  2  -  Opening Balance    '
   @ R+2, C PROMPT '  3  -  Orders             '
   @ R+3, C PROMPT '  4  -  Daily Transactions '
   @ R+4, C PROMPT '  5  -  Posting            '
   @ R+5, C PROMPT '  6  -  Reports            '
   @ R+6, C PROMPT '  7  -  System Maintenance '
   @ R+7, C PROMPT '  8  -  System Security    '
   @ R+9, C PROMPT '  9  -  Exit to System     '


			@ 22,65 SAY "Inventory"

   CHOICE= 0

   MENU TO CHOICE

	DO CASE

	CASE CHOICE=1

         CLEAR
         SET COLOR TO W/N,N/N
         @ 1, 5 SAY 'Password :' GET MPW FUNCTION '!'
         READ

         USE invt-pw
         LOCATE FOR PW_CODE=1

         SET COLOR TO W/N,N/W

		IF FOUND()
    
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)
            FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)

            IF PW=FMCODE
               DO code_men IN INVT1MNU.PRG
			ELSE
               DO denied
            ENDIF

		ENDIF


	CASE CHOICE=2

         CLEAR


		IF _DOS
         SET COLOR TO W/N,N/N
		ENDIF

		IF _WINDOWS
		SET COLOR TO
		ENDIF

	IF _DOS
         @ 1, 5 SAY 'Password :' GET MPW FUNCTION '!'
         READ
	ENDIF

	IF _WINDOWS
         @ 1, 5 SAY 'Password :' FONT 'FoxFont',11
         @ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b FONT 'FoxFont',9 SIZE 1,6
         READ
	ENDIF

         USE invt-pw
         LOCATE FOR PW_CODE=2

		
         SET COLOR TO W/N,N/W
		

         IF FOUND()
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)
            FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
            IF PW=FMCODE

               DO open_men IN INVT1MNU.PRG

            ELSE


               DO denied
            ENDIF
         ENDIF



      CASE CHOICE=3

         CLEAR
         SET COLOR TO W/N,N/N

         @ 1, 5 SAY 'Password :' GET MPW FUNCTION '!'
         READ

         USE invt-pw
         LOCATE FOR PW_CODE=3

         SET COLOR TO W/N,N/W

         IF FOUND()
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)
            FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)

            IF PW=FMCODE
               DO ord_men IN INVT1MNU.PRG
            ELSE

               DO denied
            ENDIF
         ENDIF

      CASE CHOICE=4

         CLEAR
         	SET COLOR TO W/N,N/N
	
         @ 1, 5 SAY 'Password :' GET MPW FUNCTION '!'
         READ

         USE invt-pw
         LOCATE FOR PW_CODE=4
		
         SET COLOR TO W/N,N/W

         IF FOUND()
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)
            FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
            IF PW=FMCODE

               DO tran_men IN INVT1MNU.PRG

            ELSE

               DO denied
            ENDIF
         ENDIF

      CASE CHOICE=5

         CLEAR
         SET COLOR TO W/N,N/N
	
         @ 1, 5 SAY 'Password :' GET MPW FUNCTION '!'
         READ

         USE invt-pw
         LOCATE FOR PW_CODE=5
		
         SET COLOR TO W/N,N/W

         IF FOUND()
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)
            FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
            IF PW=FMCODE

               DO post IN POSTPROC.PRG

            ELSE

               DO denied
            ENDIF
         ENDIF



      CASE CHOICE=6

         CLEAR
         SET COLOR TO W/N,N/N
	
         @ 1, 5 SAY 'Password :' GET MPW FUNCTION '!'
         READ

         USE invt-pw
         LOCATE FOR PW_CODE=6
		
         SET COLOR TO W/N,N/W

         IF FOUND()
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)
            FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
            IF PW=FMCODE

               DO rep_men IN INVT1MNU.PRG

            ELSE

               DO denied
            ENDIF
         ENDIF

      CASE CHOICE=7

         CLEAR
         SET COLOR TO W/N,N/N
	
         @ 1, 5 SAY 'Password :' GET MPW FUNCTION '!'
         READ

         USE invt-pw
         LOCATE FOR PW_CODE=7
		
         SET COLOR TO W/N,N/W

         IF FOUND()
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)
            FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)
            IF PW=FMCODE

               DO maintmen IN INVT1MNU.PRG

            ELSE

               DO denied
            ENDIF
         ENDIF


      CASE CHOICE=8

         CLEAR
         SET COLOR TO W/N,N/N
	
         @ 1, 5 SAY 'Password :' GET MPW FUNCTION '!'
         READ

         USE invt-pw
         LOCATE FOR PW_CODE=8
		
         SET COLOR TO W/N,N/W

         IF FOUND()
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)
            FMCODE= CHR(ASC(MCODE1)-30)+CHR(ASC(MCODE2)-28)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)

            IF PW=FMCODE
               DO securmen IN INVT1MNU.PRG
            ELSE
               DO denied
            ENDIF

         ENDIF

      CASE CHOICE=9

         CLOSE ALL
         CLEAR
         RETURN

      OTHERWISE

         ABC= 0
         DO WHILE ABC<5
            ? CHR(7)
            @ 18, 17 CLEAR TO 18, 55
            @ 18, 23 SAY 'Please select correct choice:-'
            ABC= ABC+1
         ENDDO
         @ 19, 23 SAY SPACE(40)
   ENDCASE
ENDDO
 


******************
PROCEDURE DENIED
******************


	CLEAR
			@ 22,59 SAY "A c c o u n t i n g" 

ABC= 0

DO WHILE ABC<=3
   ?? CHR(7)
   SET COLOR TO *+7/1
   @ 12, 25 SAY 'Access Denied ! ' FONT 'FoxFont',11
   ABC= ABC+1
ENDDO

SET COLOR TO 7/1
CLEAR

RETURN
************
