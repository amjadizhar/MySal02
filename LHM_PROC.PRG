* Programmer : Amjad Izhar
* Revised again On : 16-12-2019
* PROGRAM LHM_PROC.PRG
*******************************************
*
* Ledger Heads Maintenance Procedures
*******************************************

* 1- Ledger Heads ... Creation - 			ldgr_cre
* 2- Ledger Heads ... Correction -  		ldgr_cor
* 3- Ledger Heads ... Deletion - 	 		ldgr_del

* 		Printing / Display

* 4- Chart of Accounts All Ledger Heads		ldgr_prt
* 5- Debtors ... (With Address)				ldgrprt1
* 6- Debtors ... (Alphabetically Sorted)	ldgrprt2
* 7- Creditors ... (With Address)			ldgrprt3
* 8- Creditors ... (Alphabetically Sorted)	ldgrprt4




**********************************************************
* 1- Ledger Heads ... Creation - 			ldgr_cre	 *
**********************************************************
PROCEDURE ldgr_cre
***********************

CLEAR
CLOSE DATA

SET EXCLUSIVE ON
SET DELETED ON

IF _DOS
 SET CLOCK ON
 SET CLOCK TO 24,68
ENDIF


IF _WINDOWS
SET COLOR TO
ENDIF

IF _DOS
 	SET COLOR TO 7/1
ENDIF

					*USE MASTER SHARED
					*IF FLOCK()
					*
					*ELSE
					*	WAIT "File is in use by another user. Please wait ... " WINDOW NOWAIT
					*ENDIF


SELECT 1
USE ldgrcode EXCL
INDEX ON STR(c1,2)+STR(c2,2)+STR(c3,3) TAG ac1
SET ORDER TO ac1

IF FLOCK()
	USE ldgrcode ALIAS ldgrcode SHARED
	*INDEX ON STR(c1,2)+STR(c2,2)+STR(c3,3) TAG ac1
	SET ORDER TO ac1
	FLUSH
ELSE
	WAIT "File is in use by another user. Please wait ... " WINDOW NOWAIT
ENDIF

SELECT 2
USE master EXCL
INDEX ON STR(c1,2)+STR(c2,2)+STR(c3,3) TAG ac2
SET ORDER TO ac2

IF FLOCK()
	USE master ALIAS master SHARED
	*INDEX ON STR(c1,2)+STR(c2,2)+STR(c3,3) TAG ac2
	SET ORDER TO ac2
	FLUSH
ELSE
	WAIT "File is in use by another user. Please wait ... " WINDOW NOWAIT
ENDIF


SELECT 3
USE chart EXCL
SET ORDER TO crt

CITYZONE= 0

IF CITYZONE=1

   STORE 0 TO MCITY_CODE, MZONE_CODE
   SELECT 6

   IF FILE("city.dbf")
   		USE city EXCL
   		IF FLOCK()
   			USE city EXCL
   			INDEX ON STR(CITY_CODE,2)+STR(ZONE_CODE,2) TAG city_idx
   			SET ORDER TO city_idx
   			FLUSH
   		ELSE
   			WAIT "File is in use by another user. Please wait ... " WINDOW NOWAIT
		ENDIF
   ENDIF

   SELECT 5
   IF FILE("zone.dbf")
   	
   	USE zone EXCL
   	SET ORDER TO zone_idx
   	
   	IF FLOCK()
   		USE zone EXCL
   		INDEX ON ZONE_CODE TAG zone_idx 
   		SET ORDER TO zone_idx
   		FLUSH
	ELSE
		WAIT "File is in use by another user. Please wait ... " WINDOW NOWAIT
	ENDIF   
	
   ENDIF

ENDIF

STORE 0 TO MC1, MC2, MC3, MLDGR_BAL, CNT
STORE CTOD('  /  /  ') TO MDATE


*************************************
* Main Loop
*************************************
DO WHILE MC3 <= 999
   CLEAR
   
   	DO top IN tbl_proc.prg
	DO bot IN tbl_proc.prg

IF _DOS
   SET COLOR TO +7/1
ENDIF


IF _DOS
   @ 6, 28 SAY 'Ledger Heads Creation '
ENDIF


IF _WINDOWS
   @ 6, 66 SAY 'Ledger Heads Creation ' FONT "FoxFont",11
ENDIF



IF _DOS
   SET COLOR TO 7/1
ENDIF

IF _DOS
   @ 23, 1 TO 23, 79
ENDIF


*IF _WINDOWS
*   @ 23, 1 TO 23, 130
*ENDIF

   STORE 0 TO MLDGR_BAL, MCITY_CODE, CNT

   STORE SPACE(30) TO MTITLE

   ***************************
   * Sub Loop 
   ***************************
   DO WHILE .T.
						*SELECT 1
						*USE ldgrcode ALIAS ldgrcode SHARED
						*SET ORDER TO ac1

      SELECT 1 && ldgrcode ORDER ac1
      SET ORDER TO ac1

					IF _DOS
					      SET COLOR TO 7/1
					ENDIF


					IF _DOS
				      @ 24, 51 SAY 'Press Ctrl Q To Quit '
				      @ 8, 8 SAY 'Ledger Head Code '
					ENDIF
	

	IF _WINDOWS
      @ 12, 8 SAY 'Enter New Ledger Head Code ' FONT "FoxFont",11
      @ 28, 8 SAY 'Press Ctrl Q To Quit ' FONT "FoxFont",11
	ENDIF



						IF _DOS
						      SET COLOR TO /w
						ENDIF


						IF _DOS
					      @ 8, 30 SAY '  -  -   '
					      @ 8, 30 GET MC1 PICTURE '@z 99'
					      READ
						ENDIF
	
	IF _WINDOWS
      *@ 8, 30 SAY '  -  -   ' FONT "FoxFont",11
      @ 12, 55 GET MC1 SIZE 1,2 PICTURE '@z 99' FONT "FoxFont",11
      READ
	ENDIF

	
						IF _DOS
						      SET COLOR TO 7/1
						ENDIF

      *READ
      
      IF READKEY()=12
         CLEAR
         CLOSE DATA
         SET CONFIRM OFF
         RETURN
      ENDIF
      					IF _DOS
      						@ 24, 50 CLEAR TO 24, 78
      					ENDIF
      
      IF MC1=0
         LOOP
      ENDIF
      
				*SELECT 3
				*USE chart ORDER crt SHARED

      SELECT 3 && chart ORDER crt
      SET FILTER TO MC1>=FR.and.MC1<=TO
      
      GOTO TOP
      
      IF MC1>=FR.and.MC1<=TO
         SEEK STR(FR,2)+STR(TO,2)
         IF FOUND()
            MCH= CHART
         ENDIF
      ENDIF
      
      SET FILTER TO

      SELECT 1 && ldgrcode ORDER ac1
      IF MC1<10

		IF _DOS
         @ 24, 1 SAY 'Code C1 less than 10 not allowed.'
		ENDIF


		IF _WINDOWS
         @ 14, 55 SAY 'Code C1 less than 10 not allowed.' FONT "FoxFont",11
		ENDIF




         WAIT ''
         @ 14, 1
         LOOP
      
      ENDIF

IF _DOS
      SET COLOR TO *+7/1
ENDIF


	IF _DOS
      @ 8, 50 SAY MCH
	ENDIF

	IF _WINDOWS
      @ 12, 95 SAY MCH FONT "FoxFont",11
	ENDIF




IF _DOS
      SET COLOR TO 7/1
ENDIF

      IF MC1>99.and.MC1<100

IF _DOS
         SET COLOR TO *+7/1
ENDIF



		IF _DOS
    	     @ 8, 50 SAY 'Not Defined Head      '
		ENDIF


		IF _WINDOWS
	         @ 18, 50 SAY 'Not Defined Head      ' FONT "FoxFont",11
		ENDIF




IF _DOS
         SET COLOR TO 7/1
ENDIF

      ENDIF
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
      SET COLOR TO /w
ENDIF


	IF _DOS
      @ 8, 30 SAY MPC1
	ENDIF

	IF _WINDOWS
      @ 12, 55 SAY MPC1 + SPACE(2) FONT "FoxFont",11
      @ 12, 58 SAY "-" FONT "FoxFont",11
	ENDIF


	IF _DOS
      @ 8, 33 GET MC2 PICTURE '@z 99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
	ENDIF


	IF _WINDOWS
      @ 12, 60 GET MC2 SIZE 1,2 PICTURE '@z 99' FONT 'FoxFont',11
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ 12, 60 SAY MPC2 + SPACE(2) FONT "FoxFont",11
      @ 12, 63 SAY "-" FONT 'FoxFont',11
	ENDIF

	IF _DOS
      @ 8, 33 SAY MPC2
	ENDIF
	

	IF _DOS
      @ 8, 36 GET MC3 PICTURE '@z 999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
	ENDIF

	IF _WINDOWS
      @ 12, 66 GET MC3 SIZE 1,3 PICTURE '@z 999' FONT 'FoxFont',11
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ 12, 66 SAY MPC3 + SPACE(2) FONT "FoxFont",11
	ENDIF


	IF _DOS
      @ 8, 36 SAY MPC3
	ENDIF


IF _DOS
      SET COLOR TO 7/1
ENDIF

      IF READKEY()=12
         CLOSE DATA
         CLEAR
         RETURN
      ENDIF

      	SELECT 1
		SET ORDER TO ac1
      	STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MLCODE

      	SEEK MLCODE


      IF FOUND()

		IF _DOS
         @ 24, 15 SAY 'This Code Already Exist  -- Please Re-try '
		ENDIF


		IF _WINDOWS
         @ 14, 55 SAY 'This Code Already Exist  -- Please Re-try ' FONT "FoxFont",11
		ENDIF



	IF _DOS
         WAIT ''
         @ 24, 8 CLEAR TO 24, 63
         LOOP
    ENDIF

	IF _WINDOWS
         WAIT ''
         @ 14, 55 SAY SPACE(85)
         LOOP
    ENDIF

      ENDIF

      IF MC3=0

		IF _DOS
         @ 12, 1 CLEAR TO 22, 79
		ENDIF

		IF _DOS
         @ 12, 8 SAY ' Note:-   Three zeros at the end of the ledger code'
         @ 13, 8 SAY '          will make it a consolidation head i.e you'
         @ 14, 8 SAY '          will  not be  able  to Post  Transactions'
         @ 15, 8 SAY '          directly into this head.'
         @ 16, 8 SAY '          Hit any key to continue ..........'
		ENDIF


		IF _WINDOWS
         @ 23, 65 SAY ' Note:-   Three zeros at the end of the ledger code' FONT "FoxFont",11
         @ 24, 65 SAY '          will make it a consolidation head i.e you' FONT "FoxFont",11
         @ 25, 65 SAY '          will  not be  able  to Post  Transactions' FONT "FoxFont",11
         @ 26, 65 SAY '          directly into this head.' FONT "FoxFont",11
         @ 27, 65 SAY '          Hit any key to continue ..........' FONT "FoxFont",11
		ENDIF

		IF _DOS
         WAIT ''
         @ 12, 7 CLEAR TO 16, 63
		ENDIF
		
		IF _WINDOWS
         WAIT ''
         @ 23, 65 SAY SPACE(105)
         @ 24, 65 SAY SPACE(105)
         @ 25, 65 SAY SPACE(105)
         @ 26, 65 SAY SPACE(105)
         @ 27, 65 SAY SPACE(105)
		ENDIF

		
      ENDIF

	IF _DOS
      MTITLE = "."+SPACE(29)
      @ 10, 8 SAY 'Title of Ledger Head ' GET MTITLE
      READ
	ENDIF


	IF _WINDOWS
      	@ 13, 8 SAY 'Title of Ledger Head ' FONT 'FoxFont',11
      	@ 13, 55 GET MTITLE SIZE 1,30 PICTURE '@!' FONT "FoxFont",11
      	READ
	ENDIF

      *READ

      IF MTITLE=SPACE(30)
         LOOP
      ENDIF
      
      IF MC3<>0
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'N'

IF _DOS
  @ 24, 8 SAY 'In Case OF Debtor Or Creditor, do you want to feed address - Y/N ' GET ANS PICTURE '!'
  READ
ENDIF

	IF _WINDOWS
		@ 28, 8 SAY SPACE(40) 
  		@ 24, 8 SAY 'In Case OF Debtor Or Creditor, do you want to feed address - Y/N ' FONT 'FoxFont',11
  		@ 24, 100 GET ANS SIZE 1,1 PICTURE '@!' FONT "FoxFont",11
  		READ
	ENDIF

	*READ


            
ENDDO



	IF _DOS
         @ 24, 5 CLEAR TO 24, 78
         @ 12, 1 CLEAR TO 16, 79
	ENDIF

	IF _WINDOWS
         @ 24, 5 SAY SPACE(150)
	ENDIF

         IF ANS='Y'
            STORE SPACE(35) TO MADD1, MADD2


		IF _DOS
            @ 12, 8 SAY 'Address              ' GET MADD1
            @ 14, 8 SAY '                     ' GET MADD2
		    READ
		ENDIF

		IF _WINDOWS
            @ 14, 8 SAY 'Address              ' FONT 'FoxFont',11
            @ 14,55 GET MADD1 SIZE 1,35 PICTURE '@!' FONT "FoxFont",11
            @ 15, 8 SAY '                     ' FONT 'FoxFont',11 
            @ 15, 55 GET MADD2 SIZE 1,35 PICTURE '@!' FONT "FoxFont",11
            READ
		ENDIF

            *READ
            
            IF CITYZONE=1

	IF _DOS
         @ 16, 8 SAY 'City & Zone          ' GET MCITY_CODE PICTURE '99'
	ENDIF


	IF _WINDOWS
         @ 16, 8 SAY 'City & Zone          ' FONT 'FoxFont',11
         @ 16, 40 GET MCITY_CODE PICTURE '99' FONT "FoxFont",11
	ENDIF

               IF _DOS
               	@ 16, 33 GET MZONE_CODE PICTURE '99'
               	READ
               ENDIF
               
               IF _WINDOWS
               	@ 16, 33 GET MZONE_CODE PICTURE '99' FONT 'FoxFont',11
               	READ
               ENDIF

               
               STORE STR(MCITY_CODE,2)+STR(MZONE_CODE,2) TO MCODE
               SELECT 6
               SEEK MCODE
               IF FOUND()

			IF _DOS
                  SET COLOR TO /w 
			ENDIF

			IF _DOS
                  @ 16, 36 SAY CITY
			ENDIF


			IF _WINDOWS
                  @ 16, 36 SAY CITY FONT "FoxFont",11
			ENDIF


			
			IF _DOS
                  SET COLOR TO 7/1
			ENDIF

               ELSE

		IF _DOS
        @ 24, 15 SAY 'This Code Does Not Exist -- Press any key'
		ENDIF


		IF _WINDOWS
        @ 24, 15 SAY 'This Code Does Not Exist -- Press any key' FONT "FoxFont",11
		ENDIF

                  WAIT ''
                  @ 24, 14 CLEAR TO 24, 79
                  SELECT 1
                  LOOP
               ENDIF
            ENDIF
         ENDIF


		IF _DOS
         @ 18, 8 SAY 'Opening Balance  Rs. ' GET MLDGR_BAL PICTURE '999999999.99'
		ENDIF


		IF _WINDOWS
         @ 18, 8 SAY 'Opening Balance  Rs. ' FONT 'FoxFont',11
         @ 18, 55 GET MLDGR_BAL PICTURE '999,999,999.99' FONT "FoxFont",11
		ENDIF

         READ
         MSTATUS= '  '
         IF MLDGR_BAL>0
            DO WHILE .not. MSTATUS$'CRDR'

			IF _DOS
               @ 20, 8 SAY 'Balance Status CR/DR ' GET MSTATUS FUNCTION '!' VALID MSTATUS='CR'.or.MSTATUS='DR'
			   READ
			ENDIF
			

	IF _WINDOWS
      @ 19, 8 SAY 'Balance Status CR/DR ' FONT 'FoxFont',11
      @ 19, 55 GET MSTATUS FUNCTION '@!' VALID MSTATUS='CR' .or. MSTATUS='DR' FONT "FoxFont",11
      READ
	ENDIF



               *READ
            ENDDO
         ELSE
            	IF _DOS
            		@ 20, 1 CLEAR TO 20, 79
         		ENDIF

            	IF _WINDOWS
            		@ 20, 1 CLEAR TO 20, 79
         		ENDIF

         ENDIF
         
         DO WHILE .T.

IF _DOS
            SET COLOR TO +7/1
ENDIF



		IF _DOS
            SET DATE TO BRIT
            SET CENT ON
            MDATE = {01/07/2015}
            @ 22, 42 SAY 'A/c Head Creation Date:' GET MDATE
            @ 21, 5 SAY 'N.B. Entering Vouchers Dated Earlier '
            @ 22, 5 SAY '     Than This Date May Cause Errors '
			READ
		ENDIF


		IF _WINDOWS
            @ 20, 8 SAY 'A/c Head Creation Date:' FONT 'FoxFont',11
            @ 20, 55 GET MDATE FONT "FoxFont",11
            @ 22, 55 SAY 'N.B. Entering Vouchers Dated Earlier ' FONT "FoxFont",11
            @ 23, 68 SAY 'Than This Date May Cause Errors ' FONT "FoxFont",11
			READ
		ENDIF




            *READ

IF _DOS
            SET COLOR TO 7/1 
ENDIF

            IF _DOS
            @ 21, 1 CLEAR TO 22, 40
            ENDIF
            
            IF _WINDOWS
            	@ 22, 55 SAY SPACE(100)
            	@ 23, 68 SAY SPACE(100)
            ENDIF
            
            IF MDATE=CTOD('  /  /  ')
               LOOP
            ELSE
               EXIT
            ENDIF
         ENDDO
      ENDIF
* Updated on 09-12-2019
* For continuous opening of account heads.
      P= "N"
      DO WHILE .not. P$'YN'

		IF _DOS
         @ 24, 25 SAY 'Any Correction - Y/N ' GET P FUNCTION '!'
		 READ
		ENDIF

		IF _WINDOWS
         @ 25, 55 SAY 'Any Correction - Y/N ' FONT 'FoxFont',11
         @ 25, 85 GET P SIZE 1,1 FUNCTION '@!' FONT "FoxFont",11
         READ
		ENDIF


         *READ
      ENDDO
	      
      IF _DOS
      	@ 24, 24 CLEAR TO 24, 63
      ENDIF
      
      IF _WINDOWS
      	@ 25, 55 SAY SPACE(100)
      ENDIF
		
		*LOOP BACK FOR CORRECTION.
      IF P='Y'
         LOOP
      ENDIF

      SELECT 1

	IF FLOCK()
      APPEND BLANK
      REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
      REPLACE TITLE WITH MTITLE
      FLUSH
    ELSE
    	WAIT "File is in use by another user. Please wait ... " WINDOW NOWAIT
    ENDIF

      IF MC3<>0
    	
    	IF FLOCK()
         REPLACE SR_NO WITH 1, TITLE WITH MTITLE
         REPLACE LDGR_BAL WITH MLDGR_BAL, STATUS WITH MSTATUS
         REPLACE OPEN_BAL WITH MLDGR_BAL, OPEN_STAT WITH MSTATUS
        ELSE
        	WAIT "File is in use by another user. Please wait ... " WINDOW NOWAIT
        ENDIF
        
         IF ANS='Y'
        
        	IF FLOCK()
            REPLACE ADDRESS1 WITH MADD1, ADDRESS2 WITH MADD2
            ELSE
            	WAIT "File is in use by another user. Please wait ... " WINDOW NOWAIT
            ENDIF
            
            IF CITYZONE=1
            	IF FLOCK()
               		REPLACE CITY_CODE WITH MCITY_CODE
               		REPLACE ZONE_CODE WITH MZONE_CODE
               	ELSE
               		WAIT "File is in use by another user. Please wait ... " WINDOW NOWAIT
               	ENDIF
            ENDIF
        
        
         ENDIF
      ENDIF

      SELECT 2
      IF MC3<>0
         IF FLOCK()
         	APPEND BLANK
         	REPLACE DATE WITH MDATE
         	REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
         	REPLACE DESC WITH '** Opening Balance **'
         	REPLACE BL_AMOUNT WITH MLDGR_BAL, STATUS WITH MSTATUS
         	REPLACE SR_NO WITH 1
         	FLUSH
         ELSE
         	WAIT "File is in use by another user. Please wait ... " WINDOW NOWAIT
         ENDIF
      ENDIF
* Updated on 09-12-2019
* For continuous Opening of Account Heads.
      Q= "Y"
      DO WHILE .not. Q$'YN'

		IF _DOS
         @ 24, 25 SAY 'More Data Entry - Y/N' GET Q FUNCTION '!'
		 READ
		ENDIF


		IF _WINDOWS
         @ 25, 55 SAY 'More Data Entry - Y/N' FONT 'FoxFont',11
         @ 25, 85 GET Q SIZE 1,1 FUNCTION '@!' FONT "FoxFont",11
         READ
		ENDIF

         *READ
      ENDDO
      IF Q='N'
         CLEAR
         CLOSE DATA
         SET CONFIRM OFF
         RETURN
      ENDIF
      EXIT
   ENDDO
   MC3= MC3+1
ENDDO
********************************
* End of Main Loop
********************************

RETURN
************************


*******************************************************
* 2- Ledger Heads ... Correction -  		ldgr_cor  *
* Last Update: 16-12-2019
*******************************************************
PROCEDURE ldgr_cor
**********************

CLEAR
CLOSE DATA
SET CONFIRM OFF
SET EXCLUSIVE OFF
SET DELETED ON
IF _DOS
 SET CLOCK ON
 SET CLOCK TO 24,68
ENDIF

IF _DOS
SET COLOR TO 7/1
ENDIF

SELECT 1
USE ldgrcode EXCL
SET ORDER TO ac1
SELECT 2
USE master EXCL 
SET ORDER TO ac2
SELECT 3
USE chart EXCL 
SET ORDER TO crt

CITYZONE= 0

IF CITYZONE=1
   STORE 0 TO MCITY_CODE, MZONE_CODE
   SELECT 6
   USE city
   INDEX ON STR(CITY_CODE,2)+STR(ZONE_CODE,2) TAG city_idx
   SELECT 5
   USE zone
   INDEX ON ZONE_CODE TAG zone_idx
ENDIF


DO WHILE .T.

   CLEAR

   DO top IN tbl_proc.prg
   DO bot IN tbl_proc.prg

IF _DOS
   SET COLOR TO +7/1
ENDIF


IF _DOS
   @ 6, 28 SAY 'Ledger Heads Correction '
ENDIF


IF _WINDOWS
   @ 6, 71 SAY 'Ledger Heads Correction ' FONT "FoxFont",11
ENDIF



IF _DOS
   SET COLOR TO 7/1
ENDIF

IF _DOS
   @ 23, 1 TO 23, 79
ENDIF

   STORE 0 TO MLDGR_BAL
   STORE SPACE(30) TO MTITLE
   STORE 0 TO MC1, MC2, MC3, MLDGR_BAL, MCITY_CODE
   ANS= ' '

   SELECT 1 && ldgrcode.dbf

IF _DOS
   SET COLOR TO 7/1
ENDIF


IF _DOS
   @ 24, 51 SAY 'Press Ctrl Q To Quit '
   @ 8, 8 SAY 'Ledger Head Code '
ENDIF


IF _WINDOWS
   @ 32, 100 SAY 'Press Ctrl Q To Quit ' FONT "FoxFont",11
   @ 8, 8 SAY 'Ledger Head Code: ' FONT "FoxFont",11
ENDIF



IF _DOS
   SET COLOR TO /w
ENDIF


IF _DOS
   @ 8, 30 SAY '  -  -   '
   @ 8, 30 GET MC1 PICTURE '@z 99'
   READ
ENDIF


IF _WINDOWS
   @ 8, 40 GET MC1 PICTURE '@z 99' SIZE 1,2 FONT "FoxFont",11
   READ
ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF


   IF READKEY()=12
      CLEAR
	  CLOSE DATA
      CLOSE ALL
      SET CONFIRM OFF
      RETURN
   ENDIF

IF _DOS
   @ 24, 50 CLEAR TO 24, 78
ENDIF

IF _WINDOWS
   @ 32, 100 SAY SPACE(100)
ENDIF


   IF MC1=0
      LOOP
   ENDIF

   SELECT 3 && chart.dbf
   SET FILTER TO MC1 >= FR .and. MC1 <= TO

   GOTO TOP
   
   IF MC1 >= FR .and. MC1 <= TO
      SEEK STR(FR,2)+STR(TO,2)
      IF FOUND()
         MCH= CHART
      ENDIF
   ENDIF

   SET FILTER TO
   SELECT 1
   
   IF MC1 < 10

		IF _DOS
      		@ 24, 1 SAY 'Code C1 less than 10 not allowed.'
		ENDIF
	
		IF _WINDOWS
      		@ 34, 1 SAY 'Code C1 less than 10 not allowed.' FONT "FoxFont",11
		ENDIF

	
		IF _DOS
      		WAIT ''
      		@ 24, 1
      		LOOP
		ENDIF 

		IF _WINDOWS
      		WAIT ''
      		@ 34, 1 SAY SPACE(110)
      		LOOP
		ENDIF 

   ENDIF

IF _DOS
   SET COLOR TO *+7/1
ENDIF


	IF _DOS
   		@ 8, 50 SAY MCH
	ENDIF
	

	IF _WINDOWS
   		@ 8, 80 SAY MCH FONT "FoxFont",11
	ENDIF


IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF MC1>99.and.MC1<100

IF _DOS
      SET COLOR TO *+7/1
ENDIF


	IF _DOS
      @ 8, 50 SAY 'Not Defined Head      '
	ENDIF


	IF _WINDOWS
      @ 8, 50 SAY 'Not Defined Head      ' FONT "FoxFont",11
	ENDIF




IF _DOS
      SET COLOR TO 7/1
ENDIF

   ENDIF
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
   SET COLOR TO /w
ENDIF


IF _DOS
   @ 8, 30 SAY MPC1
   @ 8, 33 GET MC2 PICTURE '@z 99'
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ 8, 33 SAY MPC2
   @ 8, 36 GET MC3 PICTURE '@z 999'
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 8, 36 SAY MPC3
ENDIF


IF _WINDOWS
   @ 8, 40 SAY MPC1 + SPACE(1) FONT "FoxFont",11
	@ 8, 43 SAY "-" FONT 'FoxFont',11
   @ 8, 45 GET MC2 PICTURE '@z 99' SIZE 1,2 FONT 'FoxFont',11
   READ
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   @ 8, 45 SAY MPC2 + SPACE(1) FONT "FoxFont",11
	@ 8, 48 SAY "-" FONT 'FoxFont',11
   @ 8, 50 GET MC3 PICTURE '@z 999' SIZE 1,3 FONT 'FoxFont',11
   READ
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   @ 8, 50 SAY MPC3 + SPACE(2) FONT "FoxFont",11
ENDIF




IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
	  CLOSE DATA
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF

   SELECT 1
   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MLCODE
   SEEK MLCODE
   IF .not. FOUND()

		IF _DOS
      	@ 24, 15 SAY 'This Code Does Not Exist -- Please Re-try '
		ENDIF

		IF _WINDOWS
      	@ 34, 15 SAY 'This Code Does Not Exist -- Please Re-try ' FONT "FoxFont",11
		ENDIF



IF _DOS
      WAIT ''
      @ 24, 8 CLEAR TO 24, 110
      LOOP
ENDIF

IF _WINDOWS
      WAIT ''
      @ 34, 8 SAY SPACE(110)
      LOOP
ENDIF

   ENDIF

   SELECT 1

	IF _DOS
   	@ 10, 8 SAY 'Title of Ledger Head ' GET TITLE
   	READ

* updated 16-12-2019
		@ 11, 30 GET notes
		READ
	ENDIF

	IF _WINDOWS
   		@ 10, 8 SAY 'Title of Ledger Head' FONT 'FoxFont',11
   		@ 10, 40 GET TITLE FONT "FoxFont",11
   		READ
	ENDIF


   IF SR_NO>1

		IF _DOS
	    @ 14, 17 SAY 'Transactions already  exist for this  '
    	@ 15, 17 SAY 'Ledger Head  therefore  you can only  '
      	@ 16, 17 SAY 'make changes in the Title and cannot  '
      	@ 17, 17 SAY 'make changes in the Openning Balance  '
		ENDIF


		IF _WINDOWS
	    	@ 14, 17 SAY 'Transactions already  exist for this  ' FONT "FoxFont",11
    		@ 15, 17 SAY 'Ledger Head  therefore  you can only  ' FONT "FoxFont",11
      		@ 16, 17 SAY 'make changes in the Title and cannot  ' FONT "FoxFont",11
      		@ 17, 17 SAY 'make changes in the Openning Balance  ' FONT "FoxFont",11
		ENDIF

   IF _DOS
      WAIT ''
      @ 14, 15 CLEAR TO 17, 77
      LOOP
   ENDIF
   
   IF _WINDOWS
      WAIT ''
      @ 14, 15 CLEAR TO 17, 77
      LOOP
   ENDIF

   ENDIF
   
   IF SR_NO=1.and.C3<>0
      IF ADDRESS1<>SPACE(25)

		IF _DOS
         	@ 12, 8 SAY 'Address              ' GET ADDRESS1
         	@ 14, 8 SAY '                     ' GET ADDRESS2
			READ
		ENDIF

		IF _WINDOWS
         	@ 12, 8 SAY 'Address:' FONT 'FoxFont',11
         	@ 12, 40 GET ADDRESS1 FONT 'FoxFont',11
         	@ 14, 8 SAY SPACE(8) 
         	@ 14, 40 GET ADDRESS2 FONT 'FoxFont',11
			READ
		ENDIF


         IF CITYZONE=1

			IF _DOS
            @ 16, 8 SAY 'City & Zone          ' GET CITY_CODE PICTURE '99'
			ENDIF
			
			IF _WINDOWS
            @ 16, 8 SAY 'City & Zone          ' GET CITY_CODE PICTURE '99' FONT "FoxFont",11
			ENDIF
			
            @ 16, 33 GET ZONE_CODE PICTURE '99'
            READ
            STORE STR(CITY_CODE,2)+STR(ZONE_CODE,2) TO MCODE
            SELECT 6
            SEEK MCODE
            IF FOUND()

IF _DOS
               SET COLOR TO /w 
ENDIF


				IF _DOS
               	@ 16, 36 SAY CITY
				ENDIF


				IF _WINDOWS
               	@ 16, 36 SAY CITY FONT "FoxFont",11
				ENDIF

* test 4:25 pm 23-Nov.2005


IF _DOS
               SET COLOR TO 7/1
ENDIF

            ELSE

	IF _DOS
    	@ 24, 15 SAY 'This Code Does Not Exist -- Press any key'
	ENDIF

	IF _WINDOWS
		@ 24, 15 SAY 'This Code Does Not Exist -- Press any key' FONT "FoxFont",11
	ENDIF


			IF _DOS
               WAIT ''
               @ 24, 14 CLEAR TO 24, 79
               SELECT 1 && ldgrcode.dbf
               LOOP
            ENDIF
            
			IF _WINDOWS
               WAIT ''
               @ 24, 14 SAY SPACE(110)
               SELECT 1 && ldgrcode.dbf
               LOOP
            ENDIF

            ENDIF
         ENDIF
      ELSE
         SELECT 1 && ldgrcode.dbf
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'N'

		IF _DOS
            @ 24, 8 SAY 'In Case OF Debtor Or Creditor, do you want to feed address - Y/N ' GET ANS PICTURE '!'
            READ
		ENDIF

		IF _WINDOWS
            @ 24, 8 SAY 'In Case OF Debtor Or Creditor, do you want to feed address - Y/N ' FONT 'FoxFont',11
            @ 24, 90 GET ANS PICTURE '!' FONT "FoxFont",11
            READ
		ENDIF

            
    ENDDO
    
    IF _DOS
         @ 24, 5 CLEAR TO 24, 78
         @ 12, 1 CLEAR TO 16, 79
    ENDIF
    
    IF _WINDOWS
         @ 24, 5 CLEAR TO 24, 78
         @ 12, 1 CLEAR TO 16, 79
    ENDIF

         IF ANS='Y'
            STORE SPACE(25) TO MADD1, MADD2
			
			IF _DOS
            @ 12, 8 SAY 'Address              ' GET MADD1
            @ 14, 8 SAY '                     ' GET MADD2
			ENDIF 

			IF _WINDOWS
            @ 12, 8 SAY 'Address              ' GET MADD1 FONT "FoxFont",11
            @ 14, 8 SAY '                     ' GET MADD2 FONT "FoxFont",11
			ENDIF 

            READ

            IF CITYZONE=1

				IF _DOS
               @ 16, 8 SAY 'City & Zone          ' GET MCITY_CODE PICTURE '99'
               @ 16, 33 GET MZONE_CODE PICTURE '99'
				ENDIF
				
				IF _WINDOWS
               @ 16, 8 SAY 'City & Zone          ' GET MCITY_CODE PICTURE '99' FONT "FoxFont",11
               @ 16, 33 GET MZONE_CODE PICTURE '99' FONT "FoxFont",11
				ENDIF


               READ
               STORE STR(MCITY_CODE,2)+STR(MZONE_CODE,2) TO MCODE
               SELECT 6
               SEEK MCODE
               IF FOUND()

IF _DOS
                  SET COLOR TO /w 
ENDIF

				IF _DOS
                  @ 16, 36 SAY CITY
				ENDIF

				IF _WINDOWS
                  @ 16, 36 SAY CITY FONT "FoxFont",11
				ENDIF


IF _DOS
                  SET COLOR TO 7/1
ENDIF

               ELSE

				IF _DOS
                  @ 24, 15 SAY 'This Code Does Not Exist -- Press any key'
				ENDIF

				IF _WINDOWS
                  @ 24, 15 SAY 'This Code Does Not Exist -- Press any key' FONT "FoxFont",11
				ENDIF

               IF _DOS
                  WAIT ''
                  @ 24, 14 CLEAR TO 24, 79
                  SELECT 1
                  LOOP
               ENDIF
               
               IF _WINDOWS
                  WAIT ''
                  @ 24, 14 SAY SPACE(110)
                  SELECT 1 && ldgrcode.dbf
                  LOOP
               ENDIF

               ENDIF
            ENDIF
         ENDIF
      ENDIF
      SELECT 1
      MLDGR_BAL= LDGR_BAL

	IF _DOS
      @ 18, 8 SAY 'Opening Balance  Rs. ' GET MLDGR_BAL PICTURE '999999999.99'
	  READ
	ENDIF

	IF _WINDOWS
      @ 18, 8 SAY 'Opening Balance ' FONT 'FoxFont',11
      @ 18, 40 SAY 'Rs.' FONT 'FoxFont',11
      @ 18, 45 GET MLDGR_BAL PICTURE '999,999,999.99' FONT 'FoxFont',11
      READ
	ENDIF

      
      MSTATUS= '  '
      
      IF MLDGR_BAL>0
      
         DO WHILE .not. MSTATUS$'CRDR'
            MSTATUS= STATUS

			IF _DOS
            @ 20, 8 SAY 'Balance Status CR/DR ' GET MSTATUS FUNCTION '!' VALID MSTATUS='CR'.or.MSTATUS='DR'
            READ
			ENDIF

			IF _WINDOWS
            @ 20, 8 SAY 'Balance Status CR/DR' FONT 'FoxFont',11 
            @ 20, 40 GET MSTATUS FUNCTION '!' VALID MSTATUS = 'CR' .or. MSTATUS = 'DR' FONT "FoxFont",11
            READ
			ENDIF


            
         ENDDO
      ELSE
         @ 20, 1 CLEAR TO 20, 79
      ENDIF
      SELECT 2 && master.dbf
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MLCODE
      SEEK MLCODE
      IF FOUND()
         MDATE= DATE
      ENDIF
      DO WHILE .T.

IF _DOS
         SET COLOR TO +7/1
ENDIF


		IF _DOS
         @ 22, 42 SAY 'A/c Head Creation Date:' GET MDATE
         @ 21, 5 SAY 'N.B. Entering Vouchers Dated Earlier '
         @ 22, 5 SAY '     Than This Date May Cause Errors '
		 READ
		ENDIF

		IF _WINDOWS
         @ 22, 8 SAY 'A/c Head Creation Date:' FONT 'FoxFont',11
         @ 22, 40 GET MDATE FONT "FoxFont",11
         @ 24, 8 SAY 'N.B. Entering Vouchers Dated Earlier ' FONT "FoxFont",11
         @ 25, 8 SAY '     Than This Date May Cause Errors ' FONT "FoxFont",11
		 READ
		ENDIF

IF _WINDOWS
   	* CLEAR Ctrl - Q to exit
   	@ 32, 100 SAY SPACE(100)
ENDIF

         

IF _DOS
         SET COLOR TO 7/1 
ENDIF

         @ 21, 1 CLEAR TO 22, 40
         IF MDATE=CTOD('  /  /  ')
            LOOP
         ELSE
            EXIT
         ENDIF
      ENDDO
   ENDIF
   SELECT 1
   IF MC3<>0
      REPLACE SR_NO WITH 1
      REPLACE LDGR_BAL WITH MLDGR_BAL, STATUS WITH MSTATUS
      REPLACE OPEN_BAL WITH MLDGR_BAL, OPEN_STAT WITH MSTATUS
      IF ANS='Y'
         REPLACE ADDRESS1 WITH MADD1, ADDRESS2 WITH MADD2
         IF CITYZONE=1
            REPLACE CITY_CODE WITH MCITY_CODE
            REPLACE ZONE_CODE WITH MZONE_CODE
         ENDIF
      ENDIF
   ENDIF
   FLUSH
   SELECT 2
   IF MC3<>0
      REPLACE DATE WITH MDATE
      REPLACE DESC WITH '** Opening Balance **'
      REPLACE BL_AMOUNT WITH MLDGR_BAL, STATUS WITH MSTATUS
      REPLACE SR_NO WITH 1
   ENDIF
   FLUSH
   Q= ' '


   DO WHILE .not. Q$'YN'

	IF _DOS
      @ 24, 25 SAY 'More Data Correction - Y/N' GET Q FUNCTION '!'
	  READ
	ENDIF

	IF _WINDOWS
      @ 34, 25 SAY 'More Data Correction - Y/N' FONT 'FoxFont',11
      @ 34, 65 GET Q FUNCTION '!' FONT "FoxFont",11
      READ
	ENDIF

      
   ENDDO


   IF Q='N'
      CLEAR
      CLOSE ALL
      SET CONFIRM OFF
      RETURN
   ENDIF

ENDDO

RETURN
***************************



*********************************************************
* 3- Ledger Heads ... Deletion - 	 		ldgr_del	*		
*********************************************************
PROCEDURE ldgr_del
*********************

CLEAR
CLOSE DATA

SET CONFIRM OFF
 
			 IF _DOS
			 	SET CLOCK ON
			 	SET CLOCK TO 24,68
			 ENDIF

			IF _DOS
				SET COLOR TO 7/1
			ENDIF


SET DELETED ON


SELECT 1
USE ldgrcode SHARED
SET ORDER TO ac1

SELECT 2
USE master SHARED
SET ORDER TO ac2

SELECT 3
USE chart SHARED
SET ORDER TO crt

DO WHILE .T.
   
   CLEAR
   
   R= 8
   	
   	DO top IN tbl_proc.PRG
	DO bot IN tbl_proc.PRG

			IF _DOS
			   SET COLOR TO +7/1
			ENDIF

			IF _DOS
			   @ 6, 28 SAY 'Ledger Heads Deletion '
			ENDIF

		IF _WINDOWS
		   @ 6, 72 SAY 'Ledger Heads Deletion ' FONT "FoxFont",11
		ENDIF

		IF _WINDOWS
			@ 37,141 SAY "A c c o u n t i n g" FONT "Times New Roman", 14 STYLE "BIT"
		ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF

IF _DOS
   @ 23, 1 TO 23, 79
   STORE 0 TO MC1, MC2, MC3, MLDGR_BAL
ENDIF

IF _WINDOWS
   STORE 0 TO MC1, MC2, MC3, MLDGR_BAL
ENDIF

	IF _DOS
   @ 24, 51 SAY 'Press Ctrl Q To Quit '
   @ R, 8 SAY 'Ledger Head Code '
	ENDIF
	

	IF _WINDOWS
   		@ 34, 100 SAY 'Press Ctrl Q To Quit ' FONT "FoxFont",11
   		@ 8, 8 SAY 'Ledger Head Code' FONT "FoxFont",11
	ENDIF
				

IF _DOS
   SET COLOR TO /w
ENDIF


	IF _DOS
   		@ R, 30 SAY '  -  -   '
   		@ R, 30 GET MC1 PICTURE '@z 99'
		READ
	ENDIF


	IF _WINDOWS
   		@ 8, 45 GET MC1 PICTURE '@z 99' SIZE 1,2 FONT "FoxFont",11
   		READ
	ENDIF



IF _DOS
   SET COLOR TO 7/1
ENDIF

   
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      SET CONFIRM OFF
      RETURN
   ENDIF

IF _DOS
   @ 24, 50 CLEAR TO 24, 78
ENDIF

IF _WINDOWS
   @ 34, 100 SAY SPACE(50)
ENDIF


   IF MC1=0
      LOOP
   ENDIF

   SELECT 3 && chart.dbf

   SET FILTER TO MC1 >= FR .and. MC1 <= TO

   GOTO TOP

   IF MC1 >=FR .and. MC1 <= TO
      SEEK STR(FR,2)+STR(TO,2)
      IF FOUND()
         MCH= CHART
      ENDIF
   ENDIF

   SET FILTER TO
   SELECT 1 && ldgrcode.dbf
   IF MC1<10

IF _DOS
      SET COLOR TO 7/1
ENDIF


	IF _DOS
      @ 23, 1 SAY 'First Level Cannot be Less than 10 '
	ENDIF

	IF _WINDOWS
      @ 23, 1 SAY 'First Level Cannot be Less than 10 ' FONT "FoxFont",11
	ENDIF


IF _DOS
      WAIT ''
      @ 23, 1
      @ 23, 1 CLEAR TO 23, 40
      LOOP
ENDIF

IF _WINDOWS
      WAIT ''
      @ 23, 1
      @ 23, 1 CLEAR TO 23, 40
      LOOP
ENDIF


   ENDIF

IF _DOS
   SET COLOR TO *+7/1
ENDIF


IF _DOS
   @ R, 50 SAY MCH
ENDIF

IF _WINDOWS
   @ 8, 80 SAY MCH FONT "FoxFont",11
ENDIF


IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF MC1>99.and.MC1<100

IF _DOS
      SET COLOR TO *+7/1
ENDIF

	IF _DOS
      @ R, 50 SAY 'Not Defined Head   '
	ENDIF


	IF _WINDOWS
      @ 35, 100 SAY 'Not Defined Head   ' FONT "FoxFont",11
	ENDIF

IF _DOS
      SET COLOR TO 7/1
ENDIF

   ENDIF
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
   SET COLOR TO /w
ENDIF


	IF _DOS
   		@ R, 30 SAY MPC1
   		@ R, 33 GET MC2 PICTURE '@z 99'
   		READ
   		MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   		@ R, 33 SAY MPC2
   		@ R, 36 GET MC3 PICTURE '@z 999'
   		READ
   		MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   		@ R, 36 SAY MPC3
	ENDIF
	
	IF _WINDOWS
   		@ 8, 45 SAY MPC1 + SPACE(1) + "-" FONT "FoxFont",11	
   		@ 8, 52 GET MC2 PICTURE '@z 99' FONT "FoxFont",11	
   		READ
   		MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
   		@ 8, 52 SAY MPC2 + SPACE(1) + "-" FONT "FoxFont",11	
   		@ 8, 59 GET MC3 PICTURE '@z 999' FONT "FoxFont",11	
   		READ
   		MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
   		@ 8, 59 SAY MPC3 + SPACE(1) FONT "FoxFont",11	
	ENDIF





IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF READKEY()=12
      CLOSE ALL
      CLEAR
      SET CONFIRM OFF
      RETURN
   ENDIF

   SELECT 1 && ldgrcode.dbf

   STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MLCODE
   SEEK MLCODE

   IF .not. FOUND()

	IF _DOS
      @ 24, 15 SAY 'This Code Does Not Exist -- Please Re-try '
	ENDIF

	IF _WINDOWS
      @ 24, 15 SAY 'This Code Does Not Exist -- Please Re-try ' FONT "FoxFont",11
	ENDIF


IF _DOS
      WAIT ''
      @ 24, 8 CLEAR TO 24, 63
      LOOP
ENDIF

IF _WINDOWS
      WAIT ''
      @ 24, 15 SAY SPACE(100)
      LOOP
ENDIF

   ENDIF

   SELECT 1 && ldgrcode.dbf

   R= R+2

	IF _DOS
   @ R, 8 SAY 'Title of Ledger Head '
	ENDIF
	
	IF _WINDOWS
	   @ 10, 8 SAY 'Title of Ledger Head ' FONT "FoxFont",11
	ENDIF

IF _DOS
   SET COLOR TO /w
ENDIF

	IF _DOS
	   @ R, 30 SAY TITLE
	ENDIF

	IF _WINDOWS
	   @ 10, 45 SAY TITLE FONT "FoxFont",11
	ENDIF

IF _DOS
   SET COLOR TO 7/1
ENDIF

   IF SR_NO>1

	IF _DOS
      @ 14, 17 SAY 'Transactions exist for this Ledger  '
      @ 15, 17 SAY 'Head, therefore you  cannot delete  '
      @ 16, 17 SAY 'this Legder Head   '
	ENDIF

	IF _WINDOWS
      @ 14, 17 SAY 'Transactions exist for this Ledger  ' FONT "FoxFont",11
      @ 15, 17 SAY 'Head, therefore you  cannot delete  ' FONT "FoxFont",11
      @ 16, 17 SAY 'this Legder Head   ' FONT "FoxFont",11
	ENDIF


      WAIT ''
      LOOP
   ENDIF

   R= R+2

   IF ADDRESS1<>SPACE(25)

	IF _DOS
      @ R, 8 SAY 'Address              '
	ENDIF
	
	IF _WINDOWS
      @ 12, 8 SAY 'Address' FONT "FoxFont",11
	ENDIF


IF _DOS
      SET COLOR TO /w
ENDIF

	IF _DOS
      @ R, 30 SAY ADDRESS1
	ENDIF

	IF _WINDOWS
      @ 12, 45 SAY ADDRESS1 FONT "FoxFont",11
	ENDIF


IF _DOS
      SET COLOR TO 7/1
ENDIF

      R= R+2

	IF _DOS
      @ R, 8 SAY '                     '
	ENDIF
	
	IF _WINDOWS
      @ 14, 8 SAY '                     ' FONT "FoxFont",11		
	ENDIF


IF _DOS
      SET COLOR TO /w
ENDIF


	IF _DOS
      @ R, 30 SAY ADDRESS2
	ENDIF


	IF _WINDOWS
      @ 14, 45 SAY ADDRESS2 FONT "FoxFont",11
	ENDIF

IF _DOS
      SET COLOR TO 7/1
ENDIF

      R= R+2
   ENDIF
   IF SR_NO=1.and.C3<>0

	IF _DOS
      @ R, 8 SAY 'Opening Balance  Rs. '
	ENDIF


	IF _WINDOWS
      @ 16, 8 SAY 'Opening Balance' FONT "FoxFont",11
	ENDIF

		

IF _DOS
      SET COLOR TO /w
ENDIF

	IF _DOS
      @ R, 30 SAY LDGR_BAL
	ENDIF

	IF _WINDOWS
      @ 16, 45 SAY "Rs." FONT "FoxFont",11
      @ 16, 50 SAY LDGR_BAL PICTURE "999,999,999.99" FONT "FoxFont",11
	ENDIF

IF _DOS
      SET COLOR TO 7/1
ENDIF

      R= R+2

	IF _DOS
      @ R, 8 SAY 'Balance Status CR/DR '
	ENDIF

	IF _WINDOWS
      @ 18, 8 SAY 'Balance Status CR/DR ' FONT "FoxFont",11
	ENDIF


IF _DOS
      SET COLOR TO /w
ENDIF

	IF _DOS
      @ R, 30 SAY STATUS
	ENDIF

	IF _WINDOWS
      @ 18, 45 SAY STATUS FONT "FoxFont",11
	ENDIF


IF _DOS
      SET COLOR TO 7/1  
ENDIF

      SELECT 2
      SEEK MLCODE
      IF FOUND()

		IF _DOS
         @ R, 50 SAY 'Date'
		ENDIF


		IF _WINDOWS
         @ 20, 8 SAY 'Date' FONT "FoxFont",11
		ENDIF

IF _DOS
         SET COLOR TO /w 
ENDIF

	IF _DOS
         @ R, 56 SAY DATE
	ENDIF

	IF _WINDOWS
         @ 20, 45 SAY DATE FONT "FoxFont",11
	ENDIF

IF _DOS
         SET COLOR TO 7/1
ENDIF

      ENDIF
   ENDIF
   R= R+2
   P= ' '

   DO WHILE .not. P$'YN'
		IF _DOS
      @ 24, 25 SAY 'Are you Sure - Y/N' GET P FUNCTION '!'
      READ
		ENDIF
		
		IF _WINDOWS
      		@ 26, 8 SAY 'Are you Sure - Y/N' FONT 'FoxFont',11
      		@ 26, 35 GET P FUNCTION '!' FONT "FoxFont",11
      		READ
		ENDIF
		

      
   ENDDO

IF _DOS
   @ 24, 1 CLEAR TO 24, 64
ENDIF

IF _WINDOWS
   @ 26, 8 SAY SPACE(150)
ENDIF

   IF P='Y'
      SELECT 1 && ldgrcode.dbf
      IF SR_NO=1.and.C3<>0
         DELETE FOR C1=MC1.and.C2=MC2.and.C3=MC3
         SELECT 2
         DELETE FOR C1=MC1.and.C2=MC2.and.C3=MC3
      ENDIF

      SELECT 1 && ldgrcode

      IF C3=0
         DELETE FOR C1=MC1.and.C2=MC2.and.C3=MC3
      ENDIF

   ENDIF

   P= ' '

   DO WHILE .not. P$'YN'

	IF _DOS
      @ 24, 24 SAY 'More Data Deletion - Y/N' GET P FUNCTION '!'
	  READ
	ENDIF

	IF _WINDOWS
      @ 26, 8 SAY 'More Data Deletion - Y/N' FONT 'FoxFont',11
      @ 26, 45 GET P FUNCTION '!' FONT "FoxFont",11
	  READ
	ENDIF


      
      
   ENDDO

   IF P='N'
      CLEAR
      CLOSE DATA
      RETURN
   ENDIF
ENDDO

RETURN
*************************


*********************************************************
* 		Printing / Display								*
* 4- All Ledger Heads							ldgr_prt	*
*********************************************************
PROCEDURE ldgr_prt
*********************

CLEAR
CLOSE DATA

IF _DOS
 SET CLOCK ON
 SET CLOCK TO 24,68
ENDIF

IF _DOS
	SET COLOR TO 7/1
ENDIF

SET CONFIRM OFF

CLEAR

USE ldgrcode ORDER ac1

IF RECCOUNT()=0

   CLEAR

	IF _DOS
	   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... '
	ENDIF

	IF _WINDOWS
   		@ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... ' FONT "FoxFont",11
	ENDIF



   WAIT ''
   CLEAR
   CLOSE DATA
   RETURN

ENDIF



NO= ' '


DO WHILE .not. NO$'YN'

IF _DOS
   SET COLOR TO +7/1
ENDIF

IF _WINDOWS
   SET COLOR TO 
ENDIF



	IF _DOS
   		@ 2, 2 SAY 'Ledger Heads Printing/Display :'
   		@ 3, 2 SAY 'Do You Want Printing/Display With Opening Balances - Y/N ' GET NO FUNCTION '!'
	ENDIF


	IF _WINDOWS
   		@ 2, 2 SAY 'Ledger Heads Printing/Display :' FONT "FoxFont",11
   		@ 3, 2 SAY 'Do You Want Printing/Display With Opening Balances - Y/N ' FONT 'FoxFont',11
   		@ 3, 85 GET NO FUNCTION '!' FONT "FoxFont",11
	ENDIF



   READ

IF _DOS
   SET COLOR TO 7/1
ENDIF


IF _WINDOWS
   SET COLOR TO 
ENDIF


ENDDO



STORE 0 TO TOT, TOT1, GTOT, CNT
STORE 0 TO DRTOT, CRTOT, TCR, TDR, LINE, COL, W_DIFF

SUM OPEN_BAL FOR OPEN_STAT='DR'  TO AMT1
SUM OPEN_BAL FOR OPEN_STAT='CR'  TO AMT2


IF _DOS
	@ 7, 10 SAY 'Opening Balances Checking'
	@ 10, 10 SAY 'Debit  Total :'
	@ 12, 10 SAY 'Credit Total :'
	@ 10, 30 SAY AMT1 PICTURE '99,999,999,999.99'
	@ 12, 30 SAY AMT2 PICTURE '99,999,999,999.99'

	W_DIFF= AMT1-AMT2
	LINE= 13
	LINE= LINE+1
	@ LINE, 10 SAY 'Difference   :'
	@ LINE, 30 SAY ABS(W_DIFF) PICTURE '99,999,999,999.99'
ENDIF

IF _WINDOWS
	@ 7, 10 SAY 'Opening Balances Checking' FONT "FoxFont",11
	@ 10, 10 SAY 'Debit  Total :' FONT "FoxFont",11
	@ 12, 10 SAY 'Credit Total :' FONT "FoxFont",11
	@ 10, 35 SAY AMT1 PICTURE '99,999,999,999.99' SIZE 1,18 FONT "FoxFont",11
	@ 12, 35 SAY AMT2 PICTURE '99,999,999,999.99' SIZE 1,18 FONT "FoxFont",11

	W_DIFF= AMT1-AMT2
	
	LINE= 13
	LINE= LINE+1
	
	@ LINE, 10 SAY 'Difference   :' FONT "FoxFont",11
	@ LINE, 35 SAY ABS(W_DIFF) PICTURE '99,999,999,999.99' SIZE 1,18 FONT "FoxFont",11
ENDIF



HEADING = .T.
PAGE = 1
GOTO TOP

P= ' '

DO WHILE .not. P$'PSQ'

	IF _DOS
   		@ 21, 40 SAY 'Ledger Heads Printing'
   		@ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
	ENDIF

	IF _WINDOWS
   		@ 21, 40 SAY 'Chart of Accounts / All Ledger Heads Printing' FONT "FoxFont",11	
   		@ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' FONT 'FoxFont',11
   		@ 22, 90 GET P FUNCTION '!' FONT "FoxFont",11	
	ENDIF



   READ
ENDDO

IF P='Q'
   CLOSE DATA
   CLEAR
   RETURN
ENDIF

CLEAR

IF P='P'
   CLEAR
   PANS= ' '

	IF _DOS
   		DO prnchk IN tbl_proc.prg
	ENDIF

   IF PANS='Q'
      CLOSE DATA
      CLEAR
      RETURN
   ENDIF
   STORE 0 TO SPAGE, EPAGE

	IF _DOS
   		@ 10, 10 SAY 'Start At Page Number (Return For Beginning) ' GET SPAGE PICTURE '99'
   		@ 12, 10 SAY 'Stop After Page Number (Return For End)     ' GET EPAGE PICTURE '99'
	ENDIF

	IF _WINDOWS
   		@ 10, 10 SAY 'Start At Page Number (Return For Beginning) ' GET SPAGE PICTURE '99' FONT "FoxFont",11	
   		@ 12, 10 SAY 'Stop After Page Number (Return For End)     ' GET EPAGE PICTURE '99' FONT "FoxFont",11	
	ENDIF


   READ
   IF SPAGE=0
      SPAGE= 1
   ENDIF
   DO WHILE .not. EOF()
      IF HEADING
         L= 2
         IF P='P'
            L= L+1
         ELSE
            L= L+1
         ENDIF
         L= L+2
         IF NO='Y'
            L= L+1
         ENDIF
         L= L+1
         HEADING= .F.
      ENDIF
      L= L+1
      IF C2=0.and.C3=0.and.RIGHT(STR(C1),1)='0'
         L= L+1
         L= L+1
      ENDIF
      IF C3=0.and.RIGHT(STR(C1),1)<>'0'
         L= L+1
      ENDIF
      
      IF NO='Y'
         IF C3<>0
            IF OPEN_STAT='DR'
               TOT= TOT+OPEN_BAL
            ELSE
               TOT1= TOT1+OPEN_BAL
            ENDIF
         ENDIF
      ENDIF
      
      IF PAGE>=SPAGE
         EXIT
      ENDIF
      
      SKIP
      
      
      IF L>56
         L= 2
         PAGE= PAGE+1
         HEADING= .T.
      ENDIF
   ENDDO
   
   
   CLEAR

	IF _DOS
   		@ 11, 20 SAY 'Please wait ... Printing in progress '
	ENDIF

	IF _WINDOWS
   		@ 11, 20 SAY 'Please wait ... Printing in progress ' FONT "FoxFont",11	
	ENDIF



   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF

HEADING= .T.


DO WHILE .not. EOF()
   IF HEADING
      L= 2
      IF P='P'
		
		IF _DOS
		 do PRNCHK in tbl_proc
         DO top_prt1 IN tbl_proc.prg
		ENDIF

		IF _DOS
         @ 2, 15 SAY CHR(18)
         @ 2, 33 SAY 'CHART OF ACCOUNTS'
         @ 3, 32 SAY '--------------------'
         @ 3, 65 SAY 'Page -'
         @ 3, 71 SAY PAGE PICTURE '99'
		ENDIF

		IF _WINDOWS
         @ 2, 15 SAY CHR(18) FONT "FoxFont",11	
         @ 2, 33 SAY 'CHART OF ACCOUNTS' FONT "FoxFont",11	
         @ 3, 32 SAY '--------------------' FONT "FoxFont",11	
         @ 3, 65 SAY 'Page -' FONT "FoxFont",11	
         @ 3, 71 SAY PAGE PICTURE '99' FONT "FoxFont",11	
		ENDIF



         L= L+1
      ELSE
         DO screen IN tbl_proc.prg

		IF _DOS
         @ 2, 30 SAY 'CHART OF ACCOUNTS'
         L= L+1
         @ L, 30 SAY '-------------------  '
         CNT= 0
		ENDIF
		
		IF _WINDOWS
         @ 2, 30 SAY 'CHART OF ACCOUNTS' FONT "FoxFont",11
         L= L+1
         @ L, 30 SAY '--------------------  ' FONT "FoxFont",11
         CNT= 0
		ENDIF


			

         MREC2= RECNO()
      ENDIF
      L= L+2

	IF _DOS
      @ L, 7 SAY 'Code'
      @ L, 28 SAY 'Title'
	ENDIF


	IF _WINDOWS
      @ L, 7 SAY 'Code' FONT "FoxFont",11
      @ L, 28 SAY 'Title' FONT "FoxFont",11
	ENDIF



	
      IF NO='Y'

		IF _DOS
         @ L, 55 SAY 'Opening Balance'
         L= L+1
         @ L, 55 SAY 'DR.           CR.'
		ENDIF


		IF _WINDOWS
         @ L, 95 SAY 'Opening Balance' FONT "FoxFont",11
         L= L+1
         @ L, 90 SAY 'DR.' FONT "FoxFont",11
         @ L, 115 SAY 'CR.' FONT "FoxFont",11
		ENDIF



      ENDIF
      L= L+1

	IF _DOS
      @ L, 5 SAY REPLICATE('-',72)
	ENDIF

	IF _WINDOWS
      @ L, 5 SAY REPLICATE('-',95) FONT "FoxFont",11
	ENDIF

      HEADING= .F.
   ENDIF
   
   
   L= L+1
   
   
   IF C2=0.and.C3=0.and.RIGHT(STR(C1),1)='0'


		IF _DOS
      		L= L+1
      		MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      		@ L, 5 SAY MPC1
      		@ L, 7 SAY '-'
      		MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      		@ L, 8 SAY MPC2
      		@ L, 10 SAY '-'
      		MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      		@ L, 11 SAY MPC3
      		@ L, 19 SAY TITLE
      		MLEN= LEN(RTRIM(TITLE))
      		L= L+1
      		@ L, 5 SAY REPLICATE('-',15+MLEN)
		ENDIF

		IF _WINDOWS
      		L= L+1
      		MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      		*@ L, 5 SAY MPC1 FONT "FoxFont",11
      		*@ L, 7 SAY '-' FONT "FoxFont",11
      		MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      		*@ L, 8 SAY MPC2 FONT "FoxFont",11
      		*@ L, 10 SAY '-' FONT "FoxFont",11
      		MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      		*@ L, 11 SAY MPC3 FONT "FoxFont",11
      		
      		
      		@ L, 5 SAY MPC1+"-"+MPC2+"-"+MPC3 FONT 'FoxFont',11
      		@ L, 19 SAY TITLE FONT "FoxFont",11
      		
      		MLEN= LEN(RTRIM(TITLE))
      		L= L+1
      		@ L, 5 SAY REPLICATE('-',15+MLEN) FONT "FoxFont",11
		ENDIF


   ENDIF

*will edit later 5:10 pm 23-Nov-2005


   IF C3=0.and.RIGHT(STR(C1),1)<>'0'

		IF _DOS
      		L= L+1
      		MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      		@ L, 5 SAY MPC1
      		@ L, 7 SAY '-'
      		MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      		@ L, 8 SAY MPC2
      		@ L, 10 SAY '-'
      		MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      		@ L, 11 SAY MPC3
      		@ L, 19 SAY TITLE
		ENDIF

		IF _WINDOWS
      		
      		L= L+1
      		MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      		*@ L, 5 SAY MPC1 FONT "FoxFont",11
      		*@ L, 7 SAY '-' FONT "FoxFont",11
      		MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      		*@ L, 8 SAY MPC2 FONT "FoxFont",11
      		*@ L, 10 SAY '-' FONT "FoxFont",11
      		MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      		*@ L, 11 SAY MPC3 FONT "FoxFont",11

			@ L, 5 SAY MPC1+"-"+MPC2+"-"+MPC3 FONT 'FoxFont',11
      		@ L, 20 SAY TITLE FONT "FoxFont",11
		ENDIF

   ELSE


      IF C3<>0

			IF _DOS
         		MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
         		@ L, 8 SAY MPC1
         		@ L, 10 SAY '-'
         		MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
         		@ L, 11 SAY MPC2
         		@ L, 13 SAY '-'
         		MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
         		@ L, 14 SAY MPC3
         		@ L, 21 SAY TITLE
			ENDIF

			IF _WINDOWS
         		MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
         		*@ L, 8 SAY MPC1 SIZE 1,3 FONT "FoxFont",11
         		*@ L, 11 SAY '-' SIZE 1,2 FONT "FoxFont",11
         		MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
         		*@ L, 12 SAY MPC2 SIZE 1,3 FONT "FoxFont",11
         		*@ L, 16 SAY '-' SIZE 1,2 FONT "FoxFont",11
         		MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
         		*@ L, 17 SAY MPC3 SIZE 1,4 FONT "FoxFont",11
         		
         		
         		@ L, 8 SAY MPC1+"-"+MPC2+"-"+MPC3 FONT 'FoxFont',11
         		@ L, 25 SAY TITLE FONT "FoxFont",11
         		
			ENDIF




      ENDIF
   ENDIF
   IF NO='Y'
      IF C3<>0
         IF OPEN_STAT='DR'

			IF _DOS
            @ L, 50 SAY OPEN_BAL PICTURE '999,999,999.99'
			ENDIF

			IF _WINDOWS
            @ L, 75 SAY OPEN_BAL PICTURE '999,999,999.99' SIZE 1,15 FONT "FoxFont",11
			ENDIF


            TOT= TOT+OPEN_BAL
         ELSE

			IF _DOS
            @ L, 63 SAY OPEN_BAL PICTURE '999,999,999.99'
			ENDIF

			IF _WINDOWS
            @ L, 100 SAY OPEN_BAL PICTURE '999,999,999.99' FONT "FoxFont",11
			ENDIF


            TOT1= TOT1+OPEN_BAL
         ENDIF
      ENDIF
   ENDIF
   SKIP
   CNT= CNT+1
   
   
   IF P='S'.and.L>=19

		IF _DOS
      		@ 23, 1 SAY REPLICATE('-',79)
      		@ 24, 5 SAY 'For Next Page Press :'
      		@ 24, 30 SAY 'For Previous Page :'
      		@ 24, 60 SAY 'Quit :'
		ENDIF


		IF _WINDOWS
      		@ 28, 1 SAY REPLICATE('-',95) FONT "FoxFont",11
      		@ 29, 5 SAY 'For Next Page Press :' FONT "FoxFont",11
      		@ 29, 40 SAY 'For Previous Page :' FONT "FoxFont",11
      		@ 29, 90 SAY 'Quit :' FONT "FoxFont",11
		ENDIF


IF _DOS
      SET COLOR TO /w
ENDIF


		IF _DOS
      		@ 24, 27 SAY CHR(25)
      		@ 24, 50 SAY CHR(24)
      		@ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
		ENDIF
		

		IF _WINDOWS
      		@ 29, 35 SAY CHR(25) FONT "FoxFont",11		
      		@ 29, 65 SAY CHR(24) FONT "FoxFont",11		
      		@ 29, 100 SAY CHR(17)+CHR(45)+CHR(45) FONT "FoxFont",11		
		ENDIF



IF _DOS
      SET COLOR TO 7/1
ENDIF

      CHO= 0


IF _DOS
      DO WHILE .T.
         @ 24, 70 GET CHO PICTURE '@z 9'
         READ
         IF READKEY()=0.or.READKEY()=4.or.READKEY()=5
            EXIT
         ENDIF
      ENDDO
ENDIF


IF _WINDOWS
      DO WHILE .T.
         @ 29, 110 GET CHO PICTURE '@z 9' FONT 'FoxFont',11
         READ

         IF READKEY()=0.or.READKEY()=4.or.READKEY()=5
            EXIT
         ENDIF
      ENDDO
ENDIF


      IF READKEY()=0
         CLEAR
         CLOSE DATA

         RETURN
      ENDIF
      IF READKEY()=4
         CLEAR
         GOTO MREC2
         SKIP  -CNT
         HEADING= .T.
      ENDIF
      IF READKEY()=5
         CLEAR
         HEADING= .T.
         PAGE= PAGE+1
      ENDIF
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
      PAGE= PAGE+1
      IF EPAGE<>0
         IF PAGE>EPAGE
            EXIT
         ENDIF
      ENDIF
   ENDIF
ENDDO
IF EOF()
   IF NO='Y'
      L= L+1


	IF _DOS
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 5 SAY 'Total Debits  :'
      @ L, 47 SAY AMT1 PICTURE '99,999,999,999.99'
      L= L+1
      @ L, 5 SAY 'Total Credits :'
      @ L, 62 SAY AMT2 PICTURE '99,999,999,999.99'
      L= L+1
      @ L, 5 SAY 'Diffirence    :'
      @ L, 51 SAY W_DIFF
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
	ENDIF


	IF _WINDOWS
      @ L, 5 SAY REPLICATE('-',72) FONT "FoxFont",11		
      L= L+1
      @ L, 5 SAY 'Total Debits  :' FONT "FoxFont",11		
      @ L, 47 SAY AMT1 PICTURE '99,999,999,999.99' FONT "FoxFont",11		
      L= L+1
      @ L, 5 SAY 'Total Credits :' FONT "FoxFont",11		
      @ L, 62 SAY AMT2 PICTURE '99,999,999,999.99' FONT "FoxFont",11		
      L= L+1
      @ L, 5 SAY 'Diffirence    :' FONT "FoxFont",11		
      @ L, 51 SAY W_DIFF FONT "FoxFont",11		
      L= L+1
      @ L, 5 SAY REPLICATE('-',72) FONT "FoxFont",11		
	ENDIF



   ENDIF
ENDIF
IF P='S'

	IF _DOS
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
	ENDIF 


	IF _WINDOWS
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... ' FONT "FoxFont",11
	ENDIF 


   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE DATA

RETURN
***************



*******************************************************
* 		Printing / Display							  *
* 5- Debtors ... (With Address)				ldgrprt1  *
*******************************************************
PROCEDURE LDGRPRT1
************************


CLEAR
CLOSE DATA
SET CONFIRM OFF
 SET CLOCK ON
 SET CLOCK TO 24,68


IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
USE ldgrcode ORDER ac1
IF RECCOUNT()=0
   CLEAR

	IF _DOS
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... '
	ENDIF


	IF _WINDOWS
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... ' FONT "FoxFont",11
	ENDIF



   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF

MC1= 0


DO WHILE .T.

IF _DOS
   SET COLOR TO +7/1
ENDIF


	IF _DOS
   @ 10, 14 SAY 'Please give "First Level Code" of Debtors'
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.'
   @ 11, 66 GET MC1 PICTURE '99'
   @ 11, 69 SAY '-xx-xxx'
	ENDIF
	

	IF _WINDOWS
   @ 10, 14 SAY 'Please give "First Level Code" of Debtors' FONT "FoxFont",11
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.' FONT "FoxFont",11
   @ 11, 66 GET MC1 PICTURE '99' FONT "FoxFont",11
   @ 11, 69 SAY '-xx-xxx' FONT "FoxFont",11
	ENDIF


			


IF _DOS
   SET COLOR TO 7/1
ENDIF

   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SET FILTER TO C1=MC1
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'

	IF _DOS
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
	ENDIF
	

	IF _WINDOWS
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!' FONT "FoxFont",11
	ENDIF



   READ
ENDDO


IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF


IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   PANS= ' '
   DO prnchk IN tbl_proc.prg
   IF PANS='Q'
      CLOSE DATA
      CLEAR
      RETURN
   ENDIF

	IF _DOS
   @ 11, 20 SAY 'Please wait ... Printing in progress '
	ENDIF


	IF _WINDOWS
   @ 11, 20 SAY 'Please wait ... Printing in progress ' FONT "FoxFont",11
	ENDIF




   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
CNT= 0
PAGE= 1
HEADING= .T.
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      IF P='P'
		 do PRNCHK in tbl_proc
         DO top_prt1 IN tbl_proc.prg

		IF _DOS
         @ 2, 15 SAY CHR(18)
         @ 2, 32 SAY 'LIST OF DEBTORS'
         @ 3, 32 SAY '---------------'
         @ 4, 32 SAY '(With Address)'
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
		ENDIF

		IF _WINDOWS
         @ 2, 15 SAY CHR(18) FONT "FoxFont",11	
         @ 2, 32 SAY 'LIST OF DEBTORS' FONT "FoxFont",11	
         @ 3, 32 SAY '---------------' FONT "FoxFont",11	
         @ 4, 32 SAY '(With Address)' FONT "FoxFont",11	
         @ 4, 65 SAY 'Page -' FONT "FoxFont",11	
         @ 4, 71 SAY PAGE PICTURE '99' FONT "FoxFont",11	
		ENDIF






         L= L+1
      ELSE
         DO screen IN tbl_proc

		IF _DOS
         @ 3, 32 SAY 'LIST OF DEBTORS'
         L= L+1
         @ L, 32 SAY '---------------'
         L= L+1
         @ L, 32 SAY '(With Address)                   Page - '
         @ L, 70 SAY PAGE PICTURE '999'
		ENDIF

		IF _WINDOWS
         @ 3, 32 SAY 'LIST OF DEBTORS' FONT "FoxFont",11	
         L= L+1
         @ L, 32 SAY '---------------' FONT "FoxFont",11
         L= L+1
         @ L, 32 SAY '(With Address)                   Page - ' FONT "FoxFont",11
         @ L, 70 SAY PAGE PICTURE '999' FONT "FoxFont",11
		ENDIF




         CNT= 0
         MREC2= RECNO()
      ENDIF
      L= L+1


	IF _DOS
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 8 SAY 'Code'
      @ L, 25 SAY 'Title/Address'
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
	ENDIF

	IF _WINDOWS
      @ L, 5 SAY REPLICATE('-',72) FONT "FoxFont",11	
      L= L+1
      @ L, 8 SAY 'Code' FONT "FoxFont",11	
      @ L, 25 SAY 'Title/Address' FONT "FoxFont",11	
      L= L+1
      @ L, 5 SAY REPLICATE('-',72) FONT "FoxFont",11	
	ENDIF





      HEADING= .F.
      L= L+1
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
   IF C2=0.and.C3=0.and.RIGHT(STR(C1),1)='0'

		IF _DOS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1
      @ L, 7 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2
      @ L, 10 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3
      @ L, 20 SAY TITLE
      MLEN= LEN(RTRIM(TITLE))
      L= L+1
      @ L, 5 SAY REPLICATE('-',15+MLEN)
		ENDIF

		IF _WINDOWS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1 FONT "FoxFont",11	
      @ L, 7 SAY '-' FONT "FoxFont",11	
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2 FONT "FoxFont",11	
      @ L, 10 SAY '-' FONT "FoxFont",11	
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3 FONT "FoxFont",11	
      @ L, 20 SAY TITLE FONT "FoxFont",11	
      MLEN= LEN(RTRIM(TITLE))
      L= L+1
      @ L, 5 SAY REPLICATE('-',15+MLEN) FONT "FoxFont",11	
		ENDIF





   ENDIF


   IF C3=0.and.RIGHT(STR(C1),1)<>'0'

		IF _DOS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1
      @ L, 7 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2
      @ L, 10 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3
      @ L, 20 SAY TITLE
		ENDIF

		IF _WINDOWS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1 FONT "FoxFont",11	
      @ L, 7 SAY '-' FONT "FoxFont",11	
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2 FONT "FoxFont",11	
      @ L, 10 SAY '-' FONT "FoxFont",11	
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3 FONT "FoxFont",11	
      @ L, 20 SAY TITLE FONT "FoxFont",11	
		ENDIF




   ELSE


      IF C3<>0

			IF _DOS
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
         @ L, 8 SAY MPC1
         @ L, 10 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
         @ L, 11 SAY MPC2
         @ L, 13 SAY '-'
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
         @ L, 14 SAY MPC3
         @ L, 20 SAY TITLE
			ENDIF

			IF _WINDOWS
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
         @ L, 8 SAY MPC1 FONT "FoxFont",11	
         @ L, 10 SAY '-' FONT "FoxFont",11	
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
         @ L, 11 SAY MPC2 FONT "FoxFont",11	
         @ L, 13 SAY '-' FONT "FoxFont",11	
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
         @ L, 14 SAY MPC3 FONT "FoxFont",11	
         @ L, 20 SAY TITLE FONT "FoxFont",11	
			ENDIF





      ENDIF
   ENDIF
   IF C3<>0
      L= L+1

		IF _DOS
      @ L, 20 SAY ADDRESS1
      L= L+1
      @ L, 20 SAY ADDRESS2
		ENDIF

		IF _WINDOWS
      @ L, 20 SAY ADDRESS1 FONT "FoxFont",11	
      L= L+1
      @ L, 20 SAY ADDRESS2 FONT "FoxFont",11	
		ENDIF


   ENDIF
   SKIP
   CNT= CNT+1
   L= L+2
   IF P='S'.and.L>=19



		IF _DOS
      @ 23, 1 SAY REPLICATE('-',79)
      @ 24, 5 SAY 'For Next Page Press :'
      @ 24, 30 SAY 'For Previous Page :'
      @ 24, 60 SAY 'Quit :'
		ENDIF

		IF _WINDOWS
      @ 23, 1 SAY REPLICATE('-',79) FONT "FoxFont",11	
      @ 24, 5 SAY 'For Next Page Press :' FONT "FoxFont",11	
      @ 24, 30 SAY 'For Previous Page :' FONT "FoxFont",11	
      @ 24, 60 SAY 'Quit :' FONT "FoxFont",11	
		ENDIF

IF _DOS
      SET COLOR TO /w
ENDIF



	IF _DOS
      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
	ENDIF


	IF _WINDOWS
      @ 24, 27 SAY CHR(25) FONT "FoxFont",11	
      @ 24, 50 SAY CHR(24) FONT "FoxFont",11	
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45) FONT "FoxFont",11	
	ENDIF




IF _DOS
      SET COLOR TO 7/1
ENDIF

      CHO= 0
      DO WHILE .T.
         @ 24, 70 GET CHO PICTURE '@z 9'
         READ
         IF READKEY()=0.or.READKEY()=4.or.READKEY()=5
            EXIT
         ENDIF
      ENDDO
      IF READKEY()=0
         CLEAR
         CLOSE DATA
    
         RETURN
      ENDIF
      IF READKEY()=4
         CLEAR
         GOTO MREC2
         SKIP  -CNT
         HEADING= .T.
         PAGE= PAGE-1
         IF PAGE<=1
            PAGE= 1
         ENDIF
      ENDIF
      IF READKEY()=5
         CLEAR
         HEADING= .T.
         PAGE= PAGE+1
      ENDIF
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
      PAGE= PAGE+1
   ENDIF
ENDDO
IF P='S'

	IF _DOS
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
	ENDIF


	IF _WINDOWS
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... ' FONT "FoxFont",11	
	ENDIF





   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE DATA

RETURN
************************


*******************************************************
* 		Printing / Display							  *
* 6- Debtors ... (Alphabetically Sorted)	ldgrprt2  *
*******************************************************
PROCEDURE LDGRPRT2
**************************

CLEAR
CLOSE ALL
SET CONFIRM OFF
 SET CLOCK ON
 SET CLOCK TO 24,68


IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
USE ldgrcode ORDER tit
IF RECCOUNT()=0
   CLEAR



	IF _DOS
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... '
	ENDIF


	IF _WINDOWS
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... ' FONT "FoxFont",11	
	ENDIF



   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
MC1= 0
DO WHILE .T.

IF _DOS
   SET COLOR TO +7/1
ENDIF


	IF _DOS
   @ 10, 14 SAY 'Please give "First Level Code" of Debtors'
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.'
	ENDIF

	IF _WINDOWS
   @ 10, 14 SAY 'Please give "First Level Code" of Debtors' FONT "FoxFont",11	
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.' FONT "FoxFont",11	
	ENDIF





IF _DOS
   SET COLOR TO 7/1
ENDIF


	IF _DOS
   @ 11, 66 GET MC1 PICTURE '99'
   @ 11, 69 SAY '-xx-xxx'
	ENDIF


	IF _WINDOWS
   @ 11, 66 GET MC1 PICTURE '99' FONT "FoxFont",11	
   @ 11, 69 SAY '-xx-xxx' FONT "FoxFont",11	
	ENDIF



   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SET FILTER TO C1=MC1
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'

	IF _DOS
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
	ENDIF


	IF _WINDOWS
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!' FONT "FoxFont",11	
	ENDIF


   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   PANS= ' '
   DO prnchk IN tbl_proc.prg
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF


	IF _DOS
   @ 11, 20 SAY 'Please wait ... Printing in progress '
	ENDIF


	IF _WINDOWS
   @ 11, 20 SAY 'Please wait ... Printing in progress ' FONT "FoxFont",11	
	ENDIF



   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
CNT= 0
PAGE= 1
HEADING= .T.
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      IF P='P'
		 do PRNCHK in tbl_proc
         DO top_prt1 IN tbl_proc.prg

			IF _DOS
         @ 2, 15 SAY CHR(18)
         @ 2, 36 SAY 'LIST OF DEBTORS'
         @ 3, 36 SAY '---------------'
         @ 4, 32 SAY '(Alphabetically Sorted)'
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
			ENDIF

			IF _WINDOWS
         @ 2, 15 SAY CHR(18) FONT "FoxFont",11	
         @ 2, 36 SAY 'LIST OF DEBTORS' FONT "FoxFont",11	
         @ 3, 36 SAY '---------------' FONT "FoxFont",11	
         @ 4, 32 SAY '(Alphabetically Sorted)' FONT "FoxFont",11	
         @ 4, 65 SAY 'Page -' FONT "FoxFont",11	
         @ 4, 71 SAY PAGE PICTURE '99' FONT "FoxFont",11	
			ENDIF




         L= L+1
      ELSE
         DO SCREEN IN tbl_proc.prg


		IF _DOS
         @ 3, 36 SAY 'LIST OF DEBTORS'
         L= L+1
         @ L, 36 SAY '---------------'
         L= L+1
         @ L, 32 SAY '(Alphabetically Sorted)          Page - '
         @ L, 70 SAY PAGE PICTURE '999'
         CNT= 0
         MREC2= RECNO()
		ENDIF
		
		IF _WINDOWS
         @ 3, 36 SAY 'LIST OF DEBTORS' FONT "FoxFont",11			
         L= L+1
         @ L, 36 SAY '---------------' FONT "FoxFont",11
         L= L+1
         @ L, 32 SAY '(Alphabetically Sorted)          Page - ' FONT "FoxFont",11
         @ L, 70 SAY PAGE PICTURE '999' FONT "FoxFont",11
         CNT= 0
         MREC2= RECNO()
		ENDIF






      ENDIF
      L= L+1

	IF _DOS
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 8 SAY 'Code'
      @ L, 25 SAY 'Title'
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
	ENDIF

	IF _WINDOWS
      @ L, 5 SAY REPLICATE('-',72) FONT "FoxFont",11
      L= L+1
      @ L, 8 SAY 'Code' FONT "FoxFont",11
      @ L, 25 SAY 'Title' FONT "FoxFont",11
      L= L+1
      @ L, 5 SAY REPLICATE('-',72) FONT "FoxFont",11
	ENDIF


      HEADING= .F.
      L= L+1
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
   IF C2=0.and.C3=0.and.RIGHT(STR(C1),1)='0'


		IF _DOS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1
      @ L, 7 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2
      @ L, 10 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3
      @ L, 20 SAY TITLE
      MLEN= LEN(RTRIM(TITLE))
      L= L+1
      @ L, 5 SAY REPLICATE('-',15+MLEN)
		ENDIF


		IF _WINDOWS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1 FONT "FoxFont",11			
      @ L, 7 SAY '-' FONT "FoxFont",11			
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2 FONT "FoxFont",11			
      @ L, 10 SAY '-' FONT "FoxFont",11			
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3 FONT "FoxFont",11			
      @ L, 20 SAY TITLE FONT "FoxFont",11			
      MLEN= LEN(RTRIM(TITLE))
      L= L+1
      @ L, 5 SAY REPLICATE('-',15+MLEN) FONT "FoxFont",11			
		ENDIF



   ENDIF



   IF C3=0.and.RIGHT(STR(C1),1)<>'0'

		IF _DOS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1
      @ L, 7 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2
      @ L, 10 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3
      @ L, 20 SAY TITLE
		ENDIF

		IF _WINDOWS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1 FONT "FoxFont",11			
      @ L, 7 SAY '-' FONT "FoxFont",11			
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2 FONT "FoxFont",11			
      @ L, 10 SAY '-' FONT "FoxFont",11			
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3 FONT "FoxFont",11			
      @ L, 20 SAY TITLE FONT "FoxFont",11			
		ENDIF




   ELSE
      IF C3<>0

			IF _DOS
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
         @ L, 8 SAY MPC1
         @ L, 10 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
         @ L, 11 SAY MPC2
         @ L, 13 SAY '-'
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
         @ L, 14 SAY MPC3
         @ L, 20 SAY TITLE
			ENDIF


			IF _WINDOWS
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
         @ L, 8 SAY MPC1 FONT "FoxFont",11			
         @ L, 10 SAY '-' FONT "FoxFont",11			
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
         @ L, 11 SAY MPC2 FONT "FoxFont",11			
         @ L, 13 SAY '-' FONT "FoxFont",11			
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
         @ L, 14 SAY MPC3 FONT "FoxFont",11			
         @ L, 20 SAY TITLE FONT "FoxFont",11			
			ENDIF




      ENDIF
   ENDIF
   SKIP
   CNT= CNT+1
   L= L+2
   IF P='S'.and.L>=21


		IF _DOS
      @ 23, 1 SAY REPLICATE('-',79)
      @ 24, 5 SAY 'For Next Page Press :'
      @ 24, 30 SAY 'For Previous Page :'
      @ 24, 60 SAY 'Quit :'
		ENDIF


		IF _WINDOWS
      @ 23, 1 SAY REPLICATE('-',79) FONT "FoxFont",11			
      @ 24, 5 SAY 'For Next Page Press :' FONT "FoxFont",11			
      @ 24, 30 SAY 'For Previous Page :' FONT "FoxFont",11			
      @ 24, 60 SAY 'Quit :' FONT "FoxFont",11			
		ENDIF



IF _DOS
      SET COLOR TO /w
ENDIF


		IF _DOS
      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
		ENDIF

		IF _WINDOWS
      @ 24, 27 SAY CHR(25) FONT "FoxFont",11			
      @ 24, 50 SAY CHR(24) FONT "FoxFont",11			
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45) FONT "FoxFont",11			
		ENDIF




IF _DOS
      SET COLOR TO 7/1
ENDIF

      CHO= 0
      DO WHILE .T.
         @ 24, 70 GET CHO PICTURE '@z 9'
         READ
         IF READKEY()=0.or.READKEY()=4.or.READKEY()=5
            EXIT
         ENDIF
      ENDDO
      IF READKEY()=0
         CLEAR
         CLOSE DATA
         
         RETURN
      ENDIF
      IF READKEY()=4
         CLEAR
         GOTO MREC2
         SKIP  -CNT
         HEADING= .T.
         PAGE= PAGE-1
         IF PAGE<=1
            PAGE= 1
         ENDIF
      ENDIF
      IF READKEY()=5
         CLEAR
         HEADING= .T.
         PAGE= PAGE+1
      ENDIF
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
      PAGE= PAGE+1
   ENDIF
ENDDO
IF P='S'

	IF _DOS
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
	ENDIF

	IF _WINDOWS
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... ' FONT "FoxFont",11
	ENDIF




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
********************



*******************************************************
* 		Printing / Display							  *
* 7- Creditors ... (With Address)			ldgrprt3  *
*******************************************************
PROCEDURE LDGRPRT3
************************

CLEAR
CLOSE ALL
SET CONFIRM OFF
 SET CLOCK ON
 SET CLOCK TO 24,68


IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
USE ldgrcode ORDER ac1
IF RECCOUNT()=0
   CLEAR


	IF _DOS
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... '
	ENDIF

	IF _WINDOWS
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... ' FONT "FoxFont",11			
	ENDIF




   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
MC1= 0
DO WHILE .T.

IF _DOS
   SET COLOR TO +7/1
ENDIF


	IF _DOS
   @ 10, 14 SAY 'Please give "First Level Code" of Creditors'
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.'
   @ 11, 66 GET MC1 PICTURE '99'
   @ 11, 69 SAY '-xx-xxx'
	ENDIF

	IF _WINDOWS
   @ 10, 14 SAY 'Please give "First Level Code" of Creditors' FONT "FoxFont",11			
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.' FONT "FoxFont",11			
   @ 11, 66 GET MC1 PICTURE '99' FONT "FoxFont",11			
   @ 11, 69 SAY '-xx-xxx' FONT "FoxFont",11			
	ENDIF




IF _DOS
   SET COLOR TO 7/1
ENDIF

   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SET FILTER TO C1=MC1
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'


	IF _DOS
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
	ENDIF

	IF _WINDOWS
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!' FONT "FoxFont",11			
	ENDIF




   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   PANS= ' '
   DO prnchk IN tbl_proc.prg
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF

	IF _DOS
   @ 11, 20 SAY 'Please wait ... Printing in progress '
	ENDIF


	IF _WINDOWS
   @ 11, 20 SAY 'Please wait ... Printing in progress ' FONT "FoxFont",11
	ENDIF



   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
CNT= 0
PAGE= 1
HEADING= .T.
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      IF P='P'
		 do PRNCHK in tbl_proc
         DO top_prt1 IN tbl_proc.prg

		IF _DOS
         @ 2, 15 SAY CHR(18)
         @ 2, 32 SAY 'LIST OF CREDITORS'
         @ 3, 32 SAY '-----------------'
         @ 4, 32 SAY '(With Address)'
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
		ENDIF


		IF _WINDOWS
         @ 2, 15 SAY CHR(18) FONT "FoxFont",11
         @ 2, 32 SAY 'LIST OF CREDITORS' FONT "FoxFont",11
         @ 3, 32 SAY '-----------------' FONT "FoxFont",11
         @ 4, 32 SAY '(With Address)' FONT "FoxFont",11
         @ 4, 65 SAY 'Page -' FONT "FoxFont",11
         @ 4, 71 SAY PAGE PICTURE '99' FONT "FoxFont",11
         L= L+1
		ENDIF



      ELSE
         DO screen IN tbl_proc.prg

			IF _DOS
         @ 3, 32 SAY 'LIST OF CREDITORS'
         L= L+1
         @ L, 32 SAY '-----------------'
         L= L+1
         @ L, 32 SAY '(With Address)                   Page - '
         @ L, 70 SAY PAGE PICTURE '999'
			ENDIF


			IF _WINDOWS
         @ 3, 32 SAY 'LIST OF CREDITORS' FONT "FoxFont",11
         L= L+1
         @ L, 32 SAY '-----------------' FONT "FoxFont",11
         L= L+1
         @ L, 32 SAY '(With Address)                   Page - ' FONT "FoxFont",11
         @ L, 70 SAY PAGE PICTURE '999' FONT "FoxFont",11
			ENDIF




         CNT= 0
         MREC2= RECNO()
      ENDIF
      L= L+1



	IF _DOS
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 8 SAY 'Code'
      @ L, 25 SAY 'Title/Address'
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
	ENDIF

	IF _WINDOWS
      @ L, 5 SAY REPLICATE('-',72) FONT "FoxFont",11
      L= L+1
      @ L, 8 SAY 'Code' FONT "FoxFont",11
      @ L, 25 SAY 'Title/Address' FONT "FoxFont",11
      L= L+1
      @ L, 5 SAY REPLICATE('-',72) FONT "FoxFont",11
	ENDIF




      HEADING= .F.
      L= L+1
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
   IF C2=0.and.C3=0.and.RIGHT(STR(C1),1)='0'


		IF _DOS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1
      @ L, 7 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2
      @ L, 10 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3
      @ L, 20 SAY TITLE
      MLEN= LEN(RTRIM(TITLE))
      L= L+1
      @ L, 5 SAY REPLICATE('-',15+MLEN)
		ENDIF
		
		IF _WINDOWS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1 FONT "FoxFont",11		
      @ L, 7 SAY '-' FONT "FoxFont",11		
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2 FONT "FoxFont",11		
      @ L, 10 SAY '-' FONT "FoxFont",11		
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3 FONT "FoxFont",11		
      @ L, 20 SAY TITLE FONT "FoxFont",11		
      MLEN= LEN(RTRIM(TITLE))
      L= L+1
      @ L, 5 SAY REPLICATE('-',15+MLEN) FONT "FoxFont",11		
		ENDIF





   ENDIF


   IF C3=0.and.RIGHT(STR(C1),1)<>'0'

		IF _DOS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1
      @ L, 7 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2
      @ L, 10 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3
      @ L, 20 SAY TITLE
		ENDIF

		IF _WINDOWS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1 FONT "FoxFont",11		
      @ L, 7 SAY '-' FONT "FoxFont",11		
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2 FONT "FoxFont",11		
      @ L, 10 SAY '-' FONT "FoxFont",11		
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3 FONT "FoxFont",11		
      @ L, 20 SAY TITLE FONT "FoxFont",11		
		ENDIF




   ELSE
      IF C3<>0

			IF _DOS
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
         @ L, 8 SAY MPC1
         @ L, 10 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
         @ L, 11 SAY MPC2
         @ L, 13 SAY '-'
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
         @ L, 14 SAY MPC3
         @ L, 20 SAY TITLE
			ENDIF


			IF _WINDOWS
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
         @ L, 8 SAY MPC1 FONT "FoxFont",11		
         @ L, 10 SAY '-' FONT "FoxFont",11		
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
         @ L, 11 SAY MPC2 FONT "FoxFont",11		
         @ L, 13 SAY '-' FONT "FoxFont",11		
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
         @ L, 14 SAY MPC3 FONT "FoxFont",11		
         @ L, 20 SAY TITLE FONT "FoxFont",11		
			ENDIF



      ENDIF
   ENDIF
   IF C3<>0
      L= L+1

		IF _DOS
      @ L, 20 SAY ADDRESS1
      L= L+1
      @ L, 20 SAY ADDRESS2
		ENDIF


		IF _WINDOWS
      @ L, 20 SAY ADDRESS1 FONT "FoxFont",11		
      L= L+1
      @ L, 20 SAY ADDRESS2 FONT "FoxFont",11		
		ENDIF


   ENDIF
   SKIP
   CNT= CNT+1
   L= L+2
   IF P='S'.and.L>=19


	IF _DOS
      @ 23, 1 SAY REPLICATE('-',79)
      @ 24, 5 SAY 'For Next Page Press :'
      @ 24, 30 SAY 'For Previous Page :'
      @ 24, 60 SAY 'Quit :'
	ENDIF


	IF _WINDOWS
      @ 23, 1 SAY REPLICATE('-',79) FONT "FoxFont",11		
      @ 24, 5 SAY 'For Next Page Press :' FONT "FoxFont",11		
      @ 24, 30 SAY 'For Previous Page :' FONT "FoxFont",11		
      @ 24, 60 SAY 'Quit :' FONT "FoxFont",11		
	ENDIF




IF _DOS
      SET COLOR TO /w
ENDIF


		IF _DOS
      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
		ENDIF


		IF _WINDOWS
      @ 24, 27 SAY CHR(25) FONT "FoxFont",11		
      @ 24, 50 SAY CHR(24) FONT "FoxFont",11		
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45) FONT "FoxFont",11		
		ENDIF



IF _DOS
      SET COLOR TO 7/1
ENDIF

      CHO= 0
      DO WHILE .T.
         @ 24, 70 GET CHO PICTURE '@z 9'
         READ
         IF READKEY()=0.or.READKEY()=4.or.READKEY()=5
            EXIT
         ENDIF
      ENDDO
      IF READKEY()=0
         CLEAR
         CLOSE DATA
         
         RETURN
      ENDIF
      IF READKEY()=4
         CLEAR
         GOTO MREC2
         SKIP  -CNT
         HEADING= .T.
         PAGE= PAGE-1
         IF PAGE<=1
            PAGE= 1
         ENDIF
      ENDIF
      IF READKEY()=5
         CLEAR
         HEADING= .T.
         PAGE= PAGE+1
      ENDIF
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
      PAGE= PAGE+1
   ENDIF
ENDDO
IF P='S'

	IF _DOS
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
	ENDIF


	IF _WINDOWS
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... ' FONT "FoxFont",11		
	ENDIF


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
*************************




*******************************************************
* 		Printing / Display							  *
* 8- Creditors ... (Alphabetically Sorted)	ldgrprt4  *
*******************************************************
PROCEDURE LDGRPRT4
***********************


CLEAR
CLOSE ALL
SET CONFIRM OFF
 SET CLOCK ON
 SET CLOCK TO 24,68


IF _DOS
SET COLOR TO 7/1
ENDIF

CLEAR
USE ldgrcode ORDER tit
IF RECCOUNT()=0
   CLEAR

	IF _DOS
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... '
	ENDIF


	IF _WINDOWS
   @ 12, 5 SAY 'No Ledger Heads Exist in Data File  -  Press any key to return to menu .... ' FONT "FoxFont",11		
	ENDIF


   WAIT ''
   CLEAR
   CLOSE ALL
   RETURN
ENDIF
MC1= 0
DO WHILE .T.

IF _DOS
   SET COLOR TO +7/1
ENDIF


	IF _DOS
   @ 10, 14 SAY 'Please give "First Level Code" of Creditors'
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.'
	ENDIF


	IF _WINDOWS
   @ 10, 14 SAY 'Please give "First Level Code" of Creditors' FONT "FoxFont",11		
   @ 11, 14 SAY 'that you have Defined in your Chart of Accounts.' FONT "FoxFont",11		
	ENDIF



IF _DOS
   SET COLOR TO 7/1
ENDIF


	IF _DOS
   @ 11, 66 GET MC1 PICTURE '99'
   @ 11, 69 SAY '-xx-xxx'
	ENDIF


	IF _WINDOWS
   @ 11, 66 GET MC1 PICTURE '99' FONT "FoxFont",11		
   @ 11, 69 SAY '-xx-xxx' FONT "FoxFont",11		
	ENDIF



   READ
   IF MC1=0.or.MC1<10
      LOOP
   ENDIF
   EXIT
ENDDO
SET FILTER TO C1=MC1
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'

	IF _DOS
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
	ENDIF

	IF _WINDOWS
   @ 22, 40 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!' FONT "FoxFont",11		
	ENDIF


   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   RETURN
ENDIF
IF P='S'
   CLEAR
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ELSE
   CLEAR
   PANS= ' '
   DO prnchk IN tbl_proc.prg
   IF PANS='Q'
      CLOSE ALL
      CLEAR
      RETURN
   ENDIF

	IF _DOS
   @ 11, 20 SAY 'Please wait ... Printing in progress '
	ENDIF


	IF _WINDOWS
   @ 11, 20 SAY 'Please wait ... Printing in progress ' FONT "FoxFont",11		
	ENDIF


   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
CNT= 0
PAGE= 1
HEADING= .T.
DO WHILE .not. EOF()
   IF HEADING
      L= 3
      IF P='P'
		 do PRNCHK in tbl_proc
         DO top_prt1 IN tbl_proc.prg

			IF _DOS
         @ 2, 15 SAY CHR(18)
         @ 2, 36 SAY 'LIST OF CREDITORS'
         @ 3, 36 SAY '-----------------'
         @ 4, 32 SAY '(Alphabetically Sorted)'
         @ 4, 65 SAY 'Page -'
         @ 4, 71 SAY PAGE PICTURE '99'
         L= L+1
			ENDIF


			IF _WINDOWS
         @ 2, 15 SAY CHR(18) FONT "FoxFont",11		
         @ 2, 36 SAY 'LIST OF CREDITORS' FONT "FoxFont",11		
         @ 3, 36 SAY '-----------------' FONT "FoxFont",11		
         @ 4, 32 SAY '(Alphabetically Sorted)' FONT "FoxFont",11		
         @ 4, 65 SAY 'Page -' FONT "FoxFont",11		
         @ 4, 71 SAY PAGE PICTURE '99' FONT "FoxFont",11		
         L= L+1
			ENDIF



      ELSE
         DO SCREEN IN tbl_proc.prg


			IF _DOS
         @ 3, 36 SAY 'LIST OF CREDITORS'
         L= L+1
         @ L, 36 SAY '-----------------'
         L= L+1
         @ L, 32 SAY '(Alphabetically Sorted)          Page - '
         @ L, 70 SAY PAGE PICTURE '999'
			ENDIF


			IF _WINDOWS
         @ 3, 36 SAY 'LIST OF CREDITORS' FONT "FoxFont",11		
         L= L+1
         @ L, 36 SAY '-----------------' FONT "FoxFont",11		
         L= L+1
         @ L, 32 SAY '(Alphabetically Sorted)          Page - ' FONT "FoxFont",11		
         @ L, 70 SAY PAGE PICTURE '999' FONT "FoxFont",11		
			ENDIF


         CNT= 0
         MREC2= RECNO()
      ENDIF


	IF _DOS
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
      L= L+1
      @ L, 8 SAY 'Code'
      @ L, 25 SAY 'Title'
      L= L+1
      @ L, 5 SAY REPLICATE('-',72)
	ENDIF

	IF _WINDOWS
      L= L+1
      @ L, 5 SAY REPLICATE('-',72) FONT "FoxFont",11		
      L= L+1
      @ L, 8 SAY 'Code' FONT "FoxFont",11		
      @ L, 25 SAY 'Title' FONT "FoxFont",11		
      L= L+1
      @ L, 5 SAY REPLICATE('-',72) FONT "FoxFont",11		
	ENDIF




      HEADING= .F.
      L= L+1
   ENDIF
   IF C3=0
      SKIP
      LOOP
   ENDIF
   IF C2=0.and.C3=0.and.RIGHT(STR(C1),1)='0'


		IF _DOS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1
      @ L, 7 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2
      @ L, 10 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3
      @ L, 20 SAY TITLE
      MLEN= LEN(RTRIM(TITLE))
      L= L+1
      @ L, 5 SAY REPLICATE('-',15+MLEN)
		ENDIF

		IF _WINDOWS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1 FONT "FoxFont",11		
      @ L, 7 SAY '-' FONT "FoxFont",11		
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2 FONT "FoxFont",11		
      @ L, 10 SAY '-' FONT "FoxFont",11		
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3 FONT "FoxFont",11		
      @ L, 20 SAY TITLE FONT "FoxFont",11		
      MLEN= LEN(RTRIM(TITLE))
      L= L+1
      @ L, 5 SAY REPLICATE('-',15+MLEN) FONT "FoxFont",11		
		ENDIF



   ENDIF




   IF C3=0.and.RIGHT(STR(C1),1)<>'0'

		IF _DOS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1
      @ L, 7 SAY '-'
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2
      @ L, 10 SAY '-'
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3
      @ L, 20 SAY TITLE
		ENDIF

		IF _WINDOWS
      L= L+1
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
      @ L, 5 SAY MPC1 FONT "FoxFont",11		
      @ L, 7 SAY '-' FONT "FoxFont",11		
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
      @ L, 8 SAY MPC2 FONT "FoxFont",11		
      @ L, 10 SAY '-' FONT "FoxFont",11		
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
      @ L, 11 SAY MPC3 FONT "FoxFont",11		
      @ L, 20 SAY TITLE FONT "FoxFont",11		
		ENDIF




   ELSE
      IF C3<>0

			IF _DOS
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
         @ L, 8 SAY MPC1
         @ L, 10 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
         @ L, 11 SAY MPC2
         @ L, 13 SAY '-'
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
         @ L, 14 SAY MPC3
         @ L, 20 SAY TITLE
			ENDIF

			IF _WINDOWS
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
         @ L, 8 SAY MPC1 FONT "FoxFont",11		
         @ L, 10 SAY '-' FONT "FoxFont",11		
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
         @ L, 11 SAY MPC2 FONT "FoxFont",11		
         @ L, 13 SAY '-' FONT "FoxFont",11		
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
         @ L, 14 SAY MPC3 FONT "FoxFont",11		
         @ L, 20 SAY TITLE FONT "FoxFont",11		
			ENDIF




      ENDIF
   ENDIF
   SKIP
   CNT= CNT+1
   L= L+2
   IF P='S'.and.L>=21


		IF _DOS
      @ 23, 1 SAY REPLICATE('-',79)
      @ 24, 5 SAY 'For Next Page Press :'
      @ 24, 30 SAY 'For Previous Page :'
      @ 24, 60 SAY 'Quit :'
		ENDIF

		IF _WINDOWS
      @ 23, 1 SAY REPLICATE('-',79) FONT "FoxFont",11		
      @ 24, 5 SAY 'For Next Page Press :' FONT "FoxFont",11		
      @ 24, 30 SAY 'For Previous Page :' FONT "FoxFont",11		
      @ 24, 60 SAY 'Quit :' FONT "FoxFont",11		
		ENDIF




IF _DOS
      SET COLOR TO /w
ENDIF


		IF _DOS
      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)
		ENDIF


		IF _WINDOWS
      @ 24, 27 SAY CHR(25) FONT "FoxFont",11		
      @ 24, 50 SAY CHR(24) FONT "FoxFont",11		
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45) FONT "FoxFont",11		
		ENDIF



IF _DOS
      SET COLOR TO 7/1
ENDIF


      CHO= 0
      DO WHILE .T.
         @ 24, 70 GET CHO PICTURE '@z 9'
         READ
         IF READKEY()=0.or.READKEY()=4.or.READKEY()=5
            EXIT
         ENDIF
      ENDDO
      IF READKEY()=0
         CLEAR
         CLOSE DATA
         
         RETURN
      ENDIF
      IF READKEY()=4
         CLEAR
         GOTO MREC2
         SKIP  -CNT
         HEADING= .T.
         PAGE= PAGE-1
         IF PAGE<=1
            PAGE= 1
         ENDIF
      ENDIF
      IF READKEY()=5
         CLEAR
         HEADING= .T.
         PAGE= PAGE+1
      ENDIF
   ENDIF
   IF P='P'.and.L>56
      HEADING= .T.
      PAGE= PAGE+1
   ENDIF
ENDDO
IF P='S'

	IF _DOS
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... '
	ENDIF

	IF _WINDOWS
   @ 24, 7 SAY 'End of Screen Printing  -  Press any key to return to menu.... ' FONT "FoxFont",11
	ENDIF




   WAIT ''
ENDIF
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
 SET CLOCK ON
 SET CLOCK TO 24,68

RETURN
*************************

