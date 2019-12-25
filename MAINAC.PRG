* Home : 16-Dec-2019 .
*
**************************************
* PROGRAM : MAINAC.PRG
* Author : Amjad Izhar
* Accounts Main Menu routine.
* Written Date: 17-09-96 
* At Sh.Ghulam Ali & Sons, 
* and Asia Snacks. 
* With Shahid Niaz.
* 14-Lawerance Road, Lahore.
* THIS Revision on : 16-Dec-2019 
* for General One Million Accounts
* MENU 1-7 PASSWORD = AMJ005
* MENU 8 PASSWORD = IZHAR9
**************************************

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
 SET ESCAPE OFF
 SET STEP OFF
 SET ECHO OFF
 SET DOHISTORY OFF
 SET EXCLUSIVE ON

 PUBLIC mDate,date

 STORE DATE() TO DATE
 STORE DATE() TO mDate

		IF _DOS
 			DO logo IN tbl_proc
		ENDIF

DO WHILE .T.

   INT = 1

		IF _WINDOWS
			SET COLOR TO
		ELSE
   			SET COLOR TO 7/1
		ENDIF

	IF _DOS
   		CLEAR
   		DO top IN tbl_proc.PRG
   		DO bot IN tbl_proc.PRG
	ENDIF

	IF _WINDOWS
   		CLEAR
   		DO top IN tbl_proc.PRG
   		DO bot IN tbl_proc.PRG
	ENDIF

   	STORE SPACE(6) TO MPW

		IF _DOS
   			SET COLOR TO +7/1
		ELSE
			SET COLOR TO
		ENDIF

		IF _DOS
   			@ 7,17,21,62 BOX
   			@ 10,18,10,61 BOX
   			@ 13,18,13,61 BOX
   			@ 16,18,16,61 BOX
   			@ 19,18,19,61 BOX
		ENDIF

			IF _DOS
   				SET COLOR TO 7/1    
			ELSE
				SET COLOR TO
			ENDIF

	IF _WINDOWS
 		R = 13
 		C = 63
	ENDIF

	IF _DOS
   		@ 6, 20 SAY 'ACCOUNTING: MAIN MENU'
	ELSE
   		@ R-2, 68 SAY 'M A I N   M E N U' FONT 'FoxFont',11
	ENDIF

	
	IF _DOS
   		@ 8, 23 PROMPT '1 - Ledger Heads Maintenance         '
   		@ 9, 23 PROMPT '2 - Budget Heads Creation / Printing '
   		@ 11, 23 PROMPT '3 - Daily Vouchers                   ' 
   		@ 12, 23 PROMPT '4 - P o s t i n g                    '
   		@ 14, 23 PROMPT '5 - Reports - 1                      '
   		@ 15, 23 PROMPT '6 - Reports - 2                      '
   		@ 17, 23 PROMPT '7 - System Maintenance               '
   		@ 18, 23 PROMPT '8 - System Security                  '
   		@ 20, 23 PROMPT '9 - Exit to Main System Menu         '
	ELSE
   		@ R, C PROMPT 'A - Ledger Heads Maintenance         ' + SPACE(5)
   		@ R+1.5, C PROMPT 'B - Budget Heads Creation / Printing ' + SPACE(3)
   		@ R+4, C PROMPT 'C - Daily Vouchers                   ' + SPACE(17)
   		@ R+5.500, C PROMPT 'D - P o s t i n g                    ' + SPACE(23)
   		@ R+7, C PROMPT 'E - Reports - 1                      ' + SPACE(22)
   		@ R+8.5, C PROMPT 'F - Reports - 2                      ' + SPACE(22)
   		@ R+10, C PROMPT 'G - System Maintenance               ' + SPACE(11)
   		@ R+11.5, C PROMPT 'H - System Security                  ' + SPACE(17)
   		@ R+13, C PROMPT 'M - Exit to Main System Menu         ' + SPACE(7)
	ENDIF

   	CHOICE= 0


		IF _DOS
			@ 22,59 SAY "A c c o u n t i n g" 
		ELSE
			@ 37,141 SAY "A c c o u n t i n g" ;
			FONT "Times New Roman", 14 ;
			STYLE "BIT"
		ENDIF

   MENU TO CHOICE

   DO CASE

		***********************		
      	CASE CHOICE=1
      	***********************
		*Ledger Heads Maintenance
        ***********************
         CLEAR

		IF _DOS
         @ 1, 5 SAY 'Password :'
		ELSE
         @ 1, 5 SAY 'Password :' FONT "FoxFont",11
		ENDIF


   		IF INT=0

					IF _DOS
						SET COLOR TO 1,1/1,1
					ELSE
						SET COLOR TO
					ENDIF

   		ELSE

					IF _DOS
            			SET COLOR TO W/N,N/N
        			ELSE
        				SET COLOR TO
        			ENDIF

		ENDIF


	IF _DOS
         @ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b
         READ
	ENDIF


	IF _WINDOWS
         @ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b FONT 'FoxFont',9 SIZE 1,6
         READ
	ENDIF

         USE pw SHARED
         LOCATE FOR PW_CODE=1
         
 			IF _DOS
 				SET COLOR TO 
           		SET COLOR TO 7/1
			ELSE
				SET COLOR TO
			ENDIF
 
        IF FOUND()
           MCODE= MPW
           MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)

			* -26, -30, -29, -21, -19, -13
            FMCODE= CHR(ASC(MCODE1)-26)+CHR(ASC(MCODE2)-30)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)

            IF PW=FMCODE
               DO code_men IN mnu_proc.prg
               * Ledger Heads Maintenance
            ELSE
               DO denied IN mnu_proc.prg
            ENDIF
         ENDIF
		
		
		
		
		***********************		
      	CASE CHOICE=2
      	***********************		
      	*Budget Heads Creation / Printing
		***********************		
		
		
         CLEAR

			IF _DOS
         		@ 1, 5 SAY 'Password :'
			ELSE
         		@ 1, 5 SAY 'Password :' FONT "FoxFont",11
			ENDIF
        

			IF _DOS
         		IF INT=0
            		SET COLOR TO 1,1/1,1
         		ELSE
            		SET COLOR TO W/N,N/N
         		ENDIF
			ELSE
			*Windows
         		IF INT=0
            		SET COLOR TO 
         		ELSE
            		SET COLOR TO 
         		ENDIF
	
			ENDIF


	IF _DOS
         	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b
         	READ
	ENDIF


	IF _WINDOWS
         	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b FONT 'FoxFont',9 SIZE 1,6
         	READ
	ENDIF
         	USE pw SHARED
         	LOCATE FOR PW_CODE=2

				IF _DOS
	         		SET COLOR TO 
    	     		SET COLOR TO 7/1
				ELSE
					SET COLOR TO
				ENDIF
		
       	IF FOUND()
            	MCODE= MPW
            	MCODE1= SUBSTR(MCODE,1,1)
            	MCODE2= SUBSTR(MCODE,2,1)
            	MCODE3= SUBSTR(MCODE,3,1)
            	MCODE4= SUBSTR(MCODE,4,1)
            	MCODE5= SUBSTR(MCODE,5,1)
            	MCODE6= SUBSTR(MCODE,6,1)

				* -26, -30, -29, -21, -19, -13
            	FMCODE= CHR(ASC(MCODE1)-26)+CHR(ASC(MCODE2)-30)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)

            IF PW=FMCODE

               DO budg_men IN mnu_proc.prg

            ELSE
               DO denied IN mnu_proc.prg
            ENDIF
         ENDIF


      	***********************		
      	CASE CHOICE=3
		***********************		
		* Daily Vouchers
		***********************		
		
		
         CLEAR


		IF _DOS
         @ 1, 5 SAY 'Password :'
		ELSE
         @ 1, 5 SAY 'Password :' FONT "FoxFont",11
		ENDIF


         IF _DOS

	         IF INT=0
    	        SET COLOR TO 1,1/1,1
        	 ELSE
            	SET COLOR TO W/N,N/N
         	ENDIF

         ELSE

				SET COLOR TO


         ENDIF
         


	IF _DOS
       	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b 
        READ
	ENDIF


	IF _WINDOWS
       	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b FONT 'FoxFont',9 SIZE 1,6
        READ
	ENDIF

         USE pw SHARED
         LOCATE FOR PW_CODE=3
    
    	IF _DOS
         SET COLOR TO 
         SET COLOR TO 7/1
    	ELSE
    		SET COLOR TO
    	ENDIF
    
         IF FOUND()
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)

			* -26, -30, -29, -21, -19, -13
            FMCODE= CHR(ASC(MCODE1)-26)+CHR(ASC(MCODE2)-30)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)

            IF PW=FMCODE


               DO vch_men IN mnu_proc.prg


            ELSE
               DO denied IN mnu_proc.prg
            ENDIF
         ENDIF



      CASE CHOICE=4
         CLEAR

		IF _DOS
         @ 1, 5 SAY 'Password :'
		ELSE
         @ 1, 5 SAY 'Password :' FONT "FoxFont",11
		ENDIF


	IF _DOS
         IF INT=0
            SET COLOR TO 1,1/1,1
         ELSE
            SET COLOR TO W/N,N/N
         ENDIF
     ELSE
     *Windows
         IF INT=0
            SET COLOR TO
         ELSE
            SET COLOR TO
         ENDIF
      ENDIF


		IF _DOS
         	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b
         	READ
		ENDIF


		IF _WINDOWS
         	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b FONT 'FoxFont',9 SIZE 1,6
         	READ
		ENDIF

         USE pw SHARED
         LOCATE FOR PW_CODE=4
         
         IF _DOS
         	SET COLOR TO 
         	SET COLOR TO 7/1
         ELSE
         	SET COLOR TO
         ENDIF
         
         IF FOUND()
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)

			* -26, -30, -29, -21, -19, -13
            FMCODE= CHR(ASC(MCODE1)-26)+CHR(ASC(MCODE2)-30)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)


            IF PW=FMCODE
 
               DO post_men IN mnu_proc.prg

            ELSE
               DO denied IN mnu_proc.prg
            ENDIF
         ENDIF



      CASE CHOICE=5
         CLEAR


		IF _DOS
        	@ 1, 5 SAY 'Password :'
		ELSE
        	@ 1, 5 SAY 'Password :' FONT "FoxFont",11
		ENDIF

		IF _DOS
         	IF INT=0
            	SET COLOR TO 1,1/1,1
         	ELSE
            	SET COLOR TO W/N,N/N
         	ENDIF
        ELSE
        *Windows
         	IF INT=0
            	SET COLOR TO
         	ELSE
            	SET COLOR TO
         	ENDIF
        ENDIF 
         

		IF _DOS
         	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b
         	READ
		ENDIF

		IF _WINDOWS
         	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b FONT 'FoxFont',9 SIZE 1,6
         	READ
		ENDIF

         USE pw SHARED
         LOCATE FOR PW_CODE=5
         
         IF _DOS
         	SET COLOR TO 
         	SET COLOR TO 7/1
         ELSE
         	SET COLOR TO
         ENDIF
         
         IF FOUND()
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)

			* -26, -30, -29, -21, -19, -13
            FMCODE= CHR(ASC(MCODE1)-26)+CHR(ASC(MCODE2)-30)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)


            IF PW=FMCODE


               DO rep_men1 IN mnu_proc.prg


            ELSE
               DO denied IN mnu_proc.prg
            ENDIF
         ENDIF



      CASE CHOICE=6
         CLEAR


		IF _DOS
         @ 1, 5 SAY 'Password :'
		ENDIF


		IF _WINDOWS
         @ 1, 5 SAY 'Password :' FONT "FoxFont",11
		ENDIF

	IF _DOS
         IF INT=0
            SET COLOR TO 1,1/1,1
         ELSE
            SET COLOR TO W/N,N/N
         ENDIF
	ELSE
	*Windows
         IF INT=0
            SET COLOR TO
         ELSE
            SET COLOR TO
         ENDIF
	
	ENDIF



		IF _DOS
         	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b
         	READ
		ENDIF


		IF _WINDOWS
         	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b FONT 'FoxFont',9 SIZE 1,6
         	READ
		ENDIF

         USE pw SHARED
         LOCATE FOR PW_CODE=6
         
         IF _DOS
         	SET COLOR TO 
         	SET COLOR TO 7/1
         ELSE
         	SET COLOR TO
         ENDIF
         
         IF FOUND()
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)

			* -26, -30, -29, -21, -19, -13
            FMCODE= CHR(ASC(MCODE1)-26)+CHR(ASC(MCODE2)-30)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)


            IF PW=FMCODE


               DO rep_men2 IN mnu_proc.prg


            ELSE
               DO denied IN mnu_proc.prg
            ENDIF
         ENDIF


      CASE CHOICE=7
         CLEAR


		IF _DOS
         @ 1, 5 SAY 'Password :'
		ELSE
         @ 1, 5 SAY 'Password :' FONT "FoxFont",11
		ENDIF

	IF _DOS
         IF INT=0
            SET COLOR TO 1,1/1,1
         ELSE
            SET COLOR TO W/N,N/N
         ENDIF
	ELSE
	*Windows
         IF INT=0
            SET COLOR TO
         ELSE
            SET COLOR TO
         ENDIF
	
	ENDIF


		IF _DOS
         	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b
         	READ
		ENDIF

		IF _WINDOWS
         	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b FONT 'FoxFont',9 SIZE 1,6
         	READ
		ENDIF


         USE pw SHARED
         LOCATE FOR PW_CODE=7
         
         IF _DOS
         	SET COLOR TO 
         	SET COLOR TO 7/1
         ELSE
         	SET COLOR TO
         ENDIF
         
         IF FOUND()
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)

			* -26, -30, -29, -21, -19, -13
            FMCODE= CHR(ASC(MCODE1)-26)+CHR(ASC(MCODE2)-30)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)


            IF PW=FMCODE


               DO maintmen IN mnu_proc.prg


            ELSE
               DO denied IN mnu_proc.prg
            ENDIF
         ENDIF


      CASE CHOICE=8
         CLEAR

		IF _DOS
         @ 1, 5 SAY 'Password :'
		ENDIF

		IF _WINDOWS
         @ 1, 5 SAY 'Password :' FONT "FoxFont",11
		ENDIF

	IF _DOS
         IF INT=0
            SET COLOR TO 1,1/1,1
         ELSE
            SET COLOR TO W/N,N/N
         ENDIF
	ELSE
	*Windows
         IF INT=0
            SET COLOR TO
         ELSE
            SET COLOR TO
         ENDIF
	
	
	ENDIF


		IF _DOS
         	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b
         	READ
		ENDIF


		IF _WINDOWS
         	@ 1, 20 GET MPW FUNCTION '!' COLOR b/b, b/b FONT 'FoxFont',9 SIZE 1,6
         	READ
		ENDIF

         USE pw SHARED
         LOCATE FOR PW_CODE=8
         

         IF _DOS
         	SET COLOR TO 
         	SET COLOR TO 7/1
         ELSE
         	SET COLOR TO
         ENDIF
         
         IF FOUND()
            MCODE= MPW
            MCODE1= SUBSTR(MCODE,1,1)
            MCODE2= SUBSTR(MCODE,2,1)
            MCODE3= SUBSTR(MCODE,3,1)
            MCODE4= SUBSTR(MCODE,4,1)
            MCODE5= SUBSTR(MCODE,5,1)
            MCODE6= SUBSTR(MCODE,6,1)

			* -26, -30, -29, -21, -19, -13
            FMCODE= CHR(ASC(MCODE1)-26)+CHR(ASC(MCODE2)-30)+CHR(ASC(MCODE3)-29)+CHR(ASC(MCODE4)-21)+CHR(ASC(MCODE5)-19)+CHR(ASC(MCODE6)-13)


            IF PW=FMCODE



               DO securmen IN mnu_proc.prg


            ELSE
               DO denied IN mnu_proc.prg
            ENDIF
         ENDIF



      CASE CHOICE=9
		SET COLOR TO
         return to master
   ENDCASE


ENDDO

