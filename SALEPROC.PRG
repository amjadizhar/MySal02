* PROGRAM : SALEPROC.PRG
* WRITTEN BY : AMJAD IZHAR
* WRITTEN ON : 01-06-2005
* THIS REVISION: SUNDAY.06-08-2006 3:47 pm
***************************************

**********************
PROCEDURE SALE_ENT
**********************
CLEAR
CLOSE ALL

SET DATE brit
SET CENT ON

SELECT 3
USE item ALIAS item EXCLUSIVE
INDEX ON STR(i1,2)+STR(i2,2)+STR(i3,4) TAG item_idx
INDEX ON STR(i1,2)+STR(i2,2)+STR(i3,4) TAG item

SET ORDER TO item
FLUSH


SELECT 4
USE ldgrcode ALIAS ldgrcode EXCLUSIVE
INDEX ON STR(c1,2)+STR(c2,2)+STR(c3,3) TAG ac1
SET ORDER TO ac1
FLUSH


SELECT 5
USE loc ALIAS loc EXCLUSIVE
INDEX ON loc_code TAG loc_idx
INDEX ON loc_code TAG loc
SET ORDER TO loc
FLUSH


SELECT 6
USE s-order ALIAS s_order EXCLUSIVE
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG sord_idx
SET ORDER TO sord_idx


SELECT 7
USE item-bal ALIAS item_bal EXCLUSIVE
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_bal
INDEX ON STR(loc_code,4)+STR(i1,2)+STR(i2,2)+STR(I3,4) TAG loc
SET ORDER TO item_bal


DO WHILE .T.
   CLEAR
   R= 3
   T= '  '
   MVCH1= '  '
   MVCH2= '     '
   MDOC_TYPE= 6
   MVCH_TYPE= 6

 	IF _DOS
   		@ 1, 20 SAY 'I N V E N T O R Y : S A L E   E N T R Y '
   		@ 2, 1 TO 2, 79 DOUBLE
	ENDIF

 	IF _WINDOWS
   		@ 1, 20 SAY 'I N V E N T O R Y : S A L E   E N T R Y ' FONT "FoxFont",11
   		@ 2, 1 TO 2, 79 DOUBLE
	ENDIF

   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO, MCITY_CODE
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MTOT1, MZONE_CODE


* Amendment made on 23-07-2006

	SELECT 11
      USE
      USE st-key ALIAS st_key EXCLUSIVE
      INDEX ON STR(doc_no,7)+STR(doc_type,1) TAG st_idx
      SET ORDER TO st_idx

	SET FILTER TO DOC_TYPE = 6

	GOTO BOTT
	MDOC_NO = DOC_NO + 1

	USE
	SELECT 7	

   DO WHILE .T.

	IF _DOS
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'Invoice No  ' GET MDOC_NO PICTURE '9999999'
      READ
	ENDIF


	IF _WINDOWS
      @ 23, 50 SAY 'Press Ctrl Q To Quit ' FONT "FoxFont",11
      @ R, 2 SAY 'Invoice No  ' FONT "FoxFont",11
      @ R, 30 GET MDOC_NO PICTURE '9999999' FONT "FoxFont",11
      READ
	ENDIF


      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF

      IF MDOC_NO=0
         LOOP
      ENDIF

      MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

		IF _DOS
      		SET COLOR TO /g
		ELSE
			SET COLOR TO
		ENDIF

      	IF _DOS
      		@ R, 15 SAY MDOC
		ENDIF


      	IF _WINDOWS
      		@ R, 30 SAY MDOC FONT "FoxFont",11
		ENDIF

		IF _DOS
      		SET COLOR TO 
		ELSE
			SET COLOR TO
		ENDIF

      SELECT 2
		DO SEEKSTKEY1 IN SEEKLIB1.PRG      
      
      IF FOUND()
         
         @ 24, 1 SAY SPACE(78)
         
         IF _DOS
         @ 24, 15 SAY 'This INV No Already Exists  -- Press any key'
         ENDIF
         
         IF _WINDOWS
         @ 24, 15 SAY 'This INV No Already Exists  -- Press any key' FONT "FoxFont",11
         ENDIF

         WAIT ''

		IF _DOS
        	 SET COLOR TO 7/1 
		ELSE
		SET COLOR TO
		ENDIF

		IF _DOS
         @ 24, 10 SAY SPACE(75)
		ENDIF
		

		IF _WINDOWS
         @ 24, 10 SAY SPACE(105)
		ENDIF

         LOOP

      ENDIF

      APPEND BLANK

      IF .not. RLOCK()
         rlock()
      ENDIF

      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE

      UNLOCK

      SELECT 2
      USE
      
      ABC= 0

      DO WHILE ABC=0
         IF INT=1
            
            IF _DOS
            @ R, 28 SAY 'Voucher No .... '
            ENDIF
            
            IF _WINDOWS
            @ R, 28 SAY 'Voucher No .... ' FONT "FoxFont",11
            ENDIF
            
            T= 'SV-'

		IF _DOS
            SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

			IF _DOS
            @ R, 45 SAY T
            @ R, 48 SAY '  -     '
			ENDIF

			IF _WINDOWS
            @ R, 45 SAY T FONT "FoxFont",11
            @ R, 48 SAY '  -     ' FONT "FoxFont",11
			ENDIF

		IF _DOS
            SET COLOR TO 7/1
		ELSE
			SET COLOR TO
		ENDIF

            IF _DOS
            @ R, 48 GET MVCH_NO1 PICTURE '@z 99'
            READ
            ENDIF
            
            IF _WINDOWS
            @ R, 48 GET MVCH_NO1 PICTURE '@z 99' FONT "FoxFont",11
            READ
            ENDIF

            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            
            IF MVCH_NO1=0
               LOOP
            ENDIF

            MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))

		IF _DOS
            SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

			IF _DOS
            @ R, 48 SAY MVCH1
			ENDIF
			
			IF _WINDOWS
            @ R, 48 SAY MVCH1 FONT "FoxFont",11
			ENDIF

		IF _DOS
            SET COLOR TO 7/1   
		ELSE
			SET COLOR TO
		ENDIF

            SELECT 2
            USE tran-key ALIAS key EXCLUSIVE
            INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG VchNo
            INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG key_idx
            INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG key

			SET ORDER TO key

            
            SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1

            GOTO BOTTOM

            MVCH_NO2= VCH_NO2+1

			IF _DOS
            @ R, 51 GET MVCH_NO2 PICTURE '@z 99999'
            READ
			ENDIF

			IF _WINDOWS
            @ R, 51 GET MVCH_NO2 PICTURE '@z 99999' FONT "FoxFont",11
            READ
			ENDIF

            IF MVCH_NO2=0
               LOOP
            ENDIF

            MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))

			IF _DOS
            	SET COLOR TO /w
			ELSE
				SET COLOR TO
			ENDIF

			IF _DOS
            @ R, 51 SAY MVCH2
			ENDIF


			IF _WINDOWS
            @ R, 51 SAY MVCH2 FONT "FoxFont",11
			ENDIF

			IF _DOS
            	SET COLOR TO 7/1
			ELSE
				SET COLOR TO
			ENDIF

            SELECT 2 		&& tran-key ORDER key
            DO SEEKTRANK1 IN SEEKLIB1.PRG
            *SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)

            IF FOUND()

			   IF _DOS
               ?? CHR(7)
               @ 24, 1 SAY SPACE(78)
               @ 24, 15 SAY 'This Voucher Already Exists  -- Press any key'
               WAIT ''
               @ 24, 10 SAY SPACE(75)
               LOOP
               ENDIF
            
			   IF _WINDOWS
               @ 24, 1 SAY SPACE(105)
               @ 24, 15 SAY 'This Voucher Already Exists  -- Press any key' FONT "FoxFont",11
               WAIT ''
               @ 24, 10 SAY SPACE(105)
               LOOP
               ENDIF
            
            ELSE
               
               APPEND BLANK
               
               IF .not. RLOCK()
                  rlock()
               ENDIF
               
               REPLACE VCH_TYPE WITH MVCH_TYPE
               REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
               UNLOCK
            ENDIF

            SELECT 2
            USE

         ENDIF

         ABC= 1

      ENDDO



		IF _DOS
      		@ R, 60 SAY 'Date ' GET MDATE
      		READ
      	ENDIF
      
		IF _WINDOWS
      		@ R, 60 SAY 'Date' FONT "FoxFont",11
      		@ R, 66 GET MDATE FONT "FoxFont",11
      		READ
      	ENDIF


      DO chk IN TBL_PROC.PRG

      R= R+1
      ABC= 0

      DO WHILE ABC=0

		IF _DOS
         @ R, 2 SAY 'Debit  / Party Code'
		ENDIF
		
		IF _WINDOWS
         @ R, 2 SAY 'Debit  / Party Code' FONT "FoxFont",11
		ENDIF
		
		IF _DOS
         	SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

		IF _DOS
         @ R, 23 SAY '  -  -   '
		ENDIF
		
		IF _WINDOWS
         @ R, 23 SAY '  -  -   ' FONT "FoxFont",11
		ENDIF

		IF _DOS
         	SET COLOR TO 
		ELSE
			SET COLOR TO
		ENDIF

		IF _DOS
         @ R, 23 GET MC1 PICTURE '99'
         READ
		ENDIF
		
		IF _WINDOWS
         @ R, 23 GET MC1 PICTURE '99' FONT "FoxFont",11
         READ
		ENDIF

		IF _DOS
         	SET COLOR TO n/g
		ELSE
			SET COLOR TO
		ENDIF


         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

		IF _DOS
         @ R, 23 SAY MPC1
         @ R, 25 SAY '-'
         @ R, 26 GET MC2 PICTURE '99'
         READ
		ENDIF


		IF _WINDOWS
         @ R, 23 SAY MPC1 FONT "FoxFont",11
         @ R, 25 SAY '-' FONT "FoxFont",11
         @ R, 26 GET MC2 PICTURE '99' FONT "FoxFont",11
         READ
		ENDIF


         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ R, 26 SAY MPC2
         @ R, 28 SAY '-'
         @ R, 29 GET MC3 PICTURE '999'
         READ
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ R, 29 SAY MPC3

		IF _DOS
         	SET COLOR TO 
		ELSE
			SET COLOR TO
		ENDIF

         SELECT 4
         DO SEEKMCODE1 IN SEEKLIB1.PRG
         
         *STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         *SEEK MCODE
         
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF

		IF _DOS
        	SET COLOR TO /w 
		ELSE
			SET COLOR TO
		ENDIF

         @ R, 33 SAY TITLE
         MTITLE= TITLE

		IF _DOS
         	SET COLOR TO 
		ELSE
			SET COLOR TO
		ENDIF

         ABC= 1
      ENDDO



      R= R+1
      ABC= 0

      DO WHILE ABC=0
         @ R, 2 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
         READ
         IF MLOC_CODE=0
            LOOP
         ENDIF


         SELECT 5
		 DO SEEKLOC1 IN SEEKLIB1.PRG

         *SEEK MLOC_CODE


         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         @ R, 25 SAY LOC
         MLOC= LOC
         ABC= 1
      ENDDO
      ABC= 0
      DO WHILE ABC=0
         @ R, 57 SAY 'City '
         @ R, 62 GET MCITY_CODE PICTURE '999'
         READ
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 23, 45 CLEAR TO 23, 78

		IF _DOS
        	 SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

         MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MCITY_CODE))))+LTRIM(STR(MCITY_CODE))
         @ R, 62 SAY MPC1

		IF _DOS
        	 SET COLOR TO 
		ELSE
			SET COLOR TO
		ENDIF

         IF MCITY_CODE=0
            LOOP
         ENDIF

         SELECT 1
         DO SEEKCITY IN SEEKLIB1.PRG

        * USE city ORDER city_idx
        * SEEK MCITY_CODE

         IF .not. FOUND()
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 14 CLEAR TO 23, 73
            LOOP
         ENDIF
         @ R, 66 SAY CITY
         MZONE_CODE= ZONE_CODE
         ABC= 1
      ENDDO
      SELECT 1
      USE
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 2 SAY 'Item Code'
      @ R, 13 SAY 'Item '
      @ R, 30 SAY 'Order #'
      @ R, 43 SAY 'Qty '
      @ R, 53 SAY 'Rate '
      @ R, 65 SAY 'Amount '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0

			IF _DOS
            	SET COLOR TO /w
			ELSE
				SET COLOR TO
			ENDIF

            @ R, 2 SAY '  -  -    '

			IF _DOS
            	SET COLOR TO 
			ELSE
				SET COLOR TO
			ENDIF

            @ R, 2 GET MI1 PICTURE '99'
            READ

			IF _DOS
            	SET COLOR TO n/g
			ELSE
				SET COLOR TO
			ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 2 SAY MP1
            @ R, 4 SAY '-'
            @ R, 5 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 5 SAY MP2
            @ R, 7 SAY '-'
            @ R, 8 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 8 SAY MP3

			IF _DOS
            	SET COLOR TO 
			ELSE
				SET COLOR TO
			ENDIF

            @ 23, 50 CLEAR TO 23, 78
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            IF MI1=0.and.MI2=0.and.MI3=0
               LOOP
            ENDIF
            
            SELECT 3
            DO SEEKICODE IN SEEKLIB1.PRG

            *STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
            *SEEK MI_CODE
            
            
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF
            @ R, 13 SAY ITEM
            MCC1= CC1
            MCC2= CC2
            MCC3= CC3
            IF INT=1
               IF MCC1=0.and.MCC2=0.and.MCC3=0
                  @ 22, 14 SAY 'Debit Code (For Purchase Voucher) does not exist.'
                  @ 23, 14 SAY 'Please feed Debit Code using Item Code Correction Option '
                  WAIT ''
                  CLEAR
                  CLOSE ALL
                  RETURN
               ENDIF
               
               SELECT 4
               DO SEEKCC1CDE IN SEEKLIB1.PRG
               
               *SEEK STR(MCC1,2)+STR(MCC2,2)+STR(MCC3,3)
               
               
               IF FOUND()
                  MCTITLE= TITLE
               ENDIF
            
            ENDIF



			SELECT 7
			DO SEEKLOCICD IN SEEKLIB1.PRG
			
            *STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            *SEEK XYZ

            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               *LOOP
            ENDIF

            ABC= 1
         ENDDO



         MT= 0

         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ

            IF MORDER_NO=0
               EXIT
            ENDIF

            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

			IF _DOS
            	SET COLOR TO /g
			ELSE
				SET COLOR TO
			ENDIF

            @ R, 30 SAY MORD

			IF _DOS
            	SET COLOR TO 
			ELSE
				SET COLOR TO
			ENDIF

            SELECT 6
            DO SEEKORD IN SEEKLIB1.PRG
            
            *STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            *SEEK ORD


            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               *LOOP
            ENDIF

            MT= 1

         ENDDO



         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO

         @ R, 49 GET MRATE PICTURE '99999999.9999'
         READ
         MTOT= 0
         MTOT= MQTY*MRATE
         @ R, 63 GET MTOT PICTURE '9999999999.99'
         READ
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'Y'
            @ R, 79 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 2
         USE st-work
         MSR= MSR+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE, VCH_NO2 WITH MVCH_NO2
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 6, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE D_D1 WITH MC1, D_D2 WITH MC2, D_D3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MTOT
         REPLACE CITY_CODE WITH MCITY_CODE, ZONE_CODE WITH MZONE_CODE, SR_NO WITH MSR
         REPLACE RATE WITH MRATE, T_AMOUNT WITH MTOT
         UNLOCK
         USE
         MTOT1= MTOT1+MTOT
         IF ANS='Y'
            STORE 0 TO MI1, MI2, MI3, MRATE, MQTY, MORDER_NO
            R= R+1
            IF R>=21
               @ 9, 0 CLEAR TO 24, 79
               R= 9
            ENDIF
            LOOP
         ENDIF
         @ 23, 50 SAY 'Total '
         @ 23, 63 SAY MTOT1 PICTURE '9999999999.99'
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
            READ
         ENDDO
         @ 24, 20 CLEAR TO 24, 78
         IF ANS='Y'
            SELECT 1
            USE st-work
            SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
            DO S-cor
         ENDIF

         IF INT=1

            SELECT 2
            USE tran ALIAS tran EXCLUSIVE
            INDEX ON STR(vch_type,1)+STR(vch_no1,2)+STR(vch_no2,5) TAG tranent
            SET ORDER TO tranent
            
            
            *INDEX tranent

            SELECT 1
            USE st-work

            GOTO TOP

            SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
            SUM FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 T_AMOUNT TO MTOT1

            GOTO TOP

            MC1= D_D1
            MC2= D_D2
            MC3= D_D3

            SELECT 2
            MSR_NO= 1

            APPEND BLANK

            IF .not. RLOCK()
               rlock()
            ENDIF

            REPLACE VCH_TYPE WITH MVCH_TYPE, DATE WITH MDATE
            REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
            REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
            REPLACE DR_CR WITH 'DR', SR_NO WITH MSR_NO
            REPLACE DESCRIP WITH 'Sales ', AMOUNT WITH MTOT1
            FLUSH

            UNLOCK
            SELECT 1

            DO WHILE .not. EOF()
               MI1= I1
               MI2= I2
               MI3= I3
               MTOT= T_AMOUNT
               
               SELECT 3
               DO SEEKICODE IN SEEKLIB1.PRG
               
               *SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)
               
               
               IF FOUND()
                  MDC1= CC1
                  MDC2= CC2
                  MDC3= CC3
               ENDIF
               
               
               SELECT 4
               DO SEEKDC1CDE IN SEEKLIB1.PRG

               *SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)
               *LDGRCODE.DBF
               
               IF FOUND()
                  MDTITLE= TITLE
               ENDIF

               SELECT 2
               MSR_NO= MSR_NO+1
               APPEND BLANK

               IF .not. RLOCK()
                  rlock()
               ENDIF

               REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE
               REPLACE C1 WITH MDC1, C2 WITH MDC2, C3 WITH MDC3, DATE WITH MDATE
               REPLACE DR_CR WITH 'CR', AMOUNT WITH MTOT, SR_NO WITH MSR_NO
               REPLACE DESCRIP WITH MTITLE, VCH_NO2 WITH MVCH_NO2
               UNLOCK
               SELECT 1
               SKIP
            ENDDO

            SELECT 2
            USE

            SELECT 1
            USE

            SELECT 1
            DO SEEKTRANK1 IN SEEKLIB1.PRG

            *USE tran-key ORDER key
            *SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
            
            
            IF FOUND()
               REPLACE FLAG WITH 'Y'
            ENDIF
            
            USE
         
         ENDIF

USE ST-TRAN EXCLUSIVE
INDEX ON DATE TAG STDATE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG STIDX
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG STIDX1
SET ORDER TO STIDX1
USE


         SELECT 2
         USE st-tran 
         SET ORDER TO STIDX
         APPEND FROM st-work FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE

         SELECT 2
         DO SEEKSTKEY1 IN SEEKLIB1.PRG

         *USE st-key ORDER st_idx
         *SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)


         IF FOUND()
            REPLACE FLAG WITH 'Y'
         ENDIF

         USE


         SELECT 1
         USE st-work
         DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE
         EXIT
      ENDDO

      ANS= ' '

      DO WHILE .not. ANS$'YN'
         @ 24, 25 SAY 'More Data Entry - Y/N' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
**************


*********************************
PROCEDURE SALE_COR
*********************************

CLEAR
CLOSE ALL

SET DATE brit
SET DELETED ON

SELECT 2
USE st-tran ORDER stidx

SET FILTER TO DOC_TYPE=6

GOTO TOP

SELECT 3
USE item ORDER item_idx

SELECT 4
USE ldgrcode ORDER ac1

SELECT 5
USE loc ORDER loc_idx

SELECT 6
USE p-order ORDER pord_idx

SELECT 7
USE item-bal ALIAS item_bal EXCLUSIVE
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_bal
SET ORDER TO itm_bal


DO WHILE .T.

		IF _DOS
   			SET COLOR TO 7/1 
		ELSE
			SET COLOR TO
		ENDIF

   CLEAR

   R= 3
   T= '   '

   MVCH1= '  '
   MVCH2= '     '

   MDOC_TYPE= 6
   MVCH_TYPE= 6

		IF _DOS
   			SET COLOR TO +7/1 
		ELSE
			SET COLOR TO
		ENDIF

   @ 1, 20 SAY 'I N V E N T O R Y : S A L E   C O R R E C T I O N '

		IF _DOS
   			SET COLOR TO 7/1 
		ELSE
			SET COLOR TO
		ENDIF

   @ 2, 1 TO 2, 79 DOUBLE

   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT

   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'INV No      ' GET MDOC_NO PICTURE '9999999'
   READ

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

   @ 23, 45 CLEAR TO 23, 78

   IF MDOC_NO=0
      LOOP
   ENDIF

   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

		IF _DOS
   			SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

   @ R, 15 SAY MDOC

		IF _DOS
   			SET COLOR TO 7/1 
		ELSE
			SET COLOR TO
		ENDIF


   SELECT 2
	DO SEEKSTTRAN IN SEEKLIB1.PRG

   *SET ORDER TO STIDX
   *SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)


   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This INV No Does Not Exists ... Press any key'
      WAIT ''

		IF _DOS
      		SET COLOR TO 7/1 
		ELSE
			SET COLOR TO
		ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF

   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Change/Correct it. If you still want to  '
      @ 14, 15 SAY 'Change/Correct it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2


* CORRECT THIS EXPRESSION AMENDMENT ON 26-07-2006
   *SET ORDER TO 2

   T= 'SV-'

		IF _DOS
   			SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

   @ R, 45 SAY T
   @ R, 48 SAY '  -     '

		IF _DOS
   			SET COLOR TO 7/1
		ELSE
			SET COLOR TO
		ENDIF

   @ R, 48 GET MVCH_NO1 PICTURE '@z 99'
   READ

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

   IF MVCH_NO1=0
      LOOP
   ENDIF

   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))

		IF _DOS
   			SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

   @ R, 48 SAY MVCH1

			IF _DOS
   				SET COLOR TO 7/1   
			ELSE
				SET COLOR TO
			ENDIF

   @ R, 51 GET MVCH_NO2 PICTURE '@z 99999'
   READ

   IF MVCH_NO2=0
      LOOP
   ENDIF

   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))

			IF _DOS
   				SET COLOR TO /w
			ELSE
				SET COLOR TO
			ENDIF

   @ R, 51 SAY MVCH2

			IF _DOS
   				SET COLOR TO 7/1
			ELSE
				SET COLOR TO
			ENDIF

   SELECT 2
   DO SEEKSTTRN2 IN SEEKLIB1.PRG
   
   *SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)+STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)

   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 13 SAY 'This INV No & Voucher No Does Not Exists ... Press any key'
      WAIT ''

			IF _DOS
      			SET COLOR TO 7/1 
			ELSE
				SET COLOR TO
			ENDIF

      @ 23, 10 SAY SPACE(80)
      LOOP
   ENDIF
   SELECT 2
   USE
   
   
   SELECT 2
   DO SEEKTRANV1 IN SEEKLIB1.PRG

   *USE tran ALIAS tran EXCLUSIVE
   *INDEX ON STR(vch_type,1)+STR(vch_no1,2)+STR(vch_no2,5) TAG tranent
   *SET ORDER TO tranent
   *FLUSH
   *INDEX tranent
   *SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)


   IF .not. FOUND()
      @ 23, 13 SAY 'This Voucher No Does Not Exists ... Press any key'
      WAIT ''

			IF _DOS
      			SET COLOR TO 7/1 
			ELSE
				SET COLOR TO
			ENDIF

      @ 23, 10 SAY SPACE(75)
      MV_NO= 0
      LOOP
   ENDIF

   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Change/Correct it. If you still want to  '
      @ 14, 15 SAY 'Change/Correct it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

   @ 21, 1 CLEAR TO 24, 79

   SELECT 2
   USE

   SELECT 1

   USE st-cor
   DELETE ALL
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE

   SELECT 1
   MDATE= DATE
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0

   DO WHILE ABC=0
      SELECT 1
      MC1= D_D1
      MC2= D_D2
      MC3= D_D3
      MLOC_CODE= LOC_CODE
      MCITY_CODE= CITY_CODE

			IF _DOS
      			SET COLOR TO 7/1 
			ELSE
				SET COLOR TO
			ENDIF

      @ R, 2 SAY 'Credit / Party Code'

			IF _DOS
      			SET COLOR TO /w
			ELSE
				SET COLOR TO
			ENDIF

      @ R, 23 SAY '  -  -   '

			IF _DOS
      			SET COLOR TO 7/1 
			ELSE
				SET COLOR TO
			ENDIF

      @ R, 23 GET MC1 PICTURE '99'
      READ
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

			IF _DOS
      			SET COLOR TO /w
			ELSE
				SET COLOR TO
			ENDIF

      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      READ

      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3

			IF _DOS
      			SET COLOR TO 7/1 
			ELSE
				SET COLOR TO
			ENDIF

				*Current Amendment on 27-07-2006 Thursday.
				*SELECT 4
				*USE ldgrcode ORDER ac1

      SELECT 4
      DO SEEKMCODE1 IN SEEKLIB1.PRG

      *STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      *SEEK MCODE
      
      
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

			IF _DOS
      			SET COLOR TO /w 
			ELSE
				SET COLOR TO
			ENDIF

      @ R, 33 SAY TITLE
      MTITLE= TITLE

			IF _DOS
      			SET COLOR TO 7/1 
			ELSE
				SET COLOR TO
			ENDIF

      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
      READ
      IF MLOC_CODE=0
         LOOP
      ENDIF
      
    
      SELECT 5
      DO SEEKLOC1 IN SEEKLIB1.PRG
      
      *SELECT 5
	  *USE loc ORDER loc_idx
      *SEEK MLOC_CODE
      
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 25 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   ABC= 0
   DO WHILE ABC=0
      @ R, 57 SAY 'City '
      @ R, 62 GET MCITY_CODE PICTURE '999'
      READ

			IF _DOS
      			SET COLOR TO /w
			ELSE
				SET COLOR TO
			ENDIF

      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MCITY_CODE))))+LTRIM(STR(MCITY_CODE))
      @ R, 62 SAY MPC1

			IF _DOS
      			SET COLOR TO 
			ELSE
				SET COLOR TO
			ENDIF

      IF MCITY_CODE=0
         LOOP
      ENDIF
      
      
      SELECT 2

			*Current amendment on Fri.28th July,2006
			*PROCEDURE SALE_COR


      DO SEEKCITY IN SEEKLIB1.PRG

      *USE city ORDER city_idx
      *SEEK MCITY_CODE
      
      
      IF .not. FOUND()
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 14 CLEAR TO 23, 73
         LOOP
      ENDIF

      @ R, 66 SAY CITY

      MZONE_CODE= ZONE_CODE
      ABC= 1


   ENDDO



   SELECT 2
   USE

   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'

   R= R+1

   @ R, 0 TO R, 79 DOUBLE
   R= R+1

   DO S_cor

   SELECT 1

   GOTO TOP

   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 DATE WITH MDATE
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 D_D1 WITH MC1
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 D_D2 WITH MC2
   REPLACE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 D_D3 WITH MC3

   USE


   IF INT=1
      SELECT 2

      	USE tran ALIAS tran EXCLUSIVE
	   	INDEX ON STR(vch_type,1)+STR(vch_no1,2)+STR(vch_no2,5) TAG tranent
   		SET ORDER TO tranent
   		FLUSH
 
	   *INDEX tranent



      DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SELECT 1

      USE st-cor
      GOTO TOP

      SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SUM FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2 PSR_TPRICE TO MTOT1

      GOTO TOP

      MC1= D_D1
      MC2= D_D2
      MC3= D_D3

      SELECT 2

      MSR_NO= 1
      APPEND BLANK

      IF .not. RLOCK()
         rlock()
      ENDIF

      REPLACE VCH_TYPE WITH MVCH_TYPE, DATE WITH MDATE
      REPLACE VCH_NO1 WITH MVCH_NO1, VCH_NO2 WITH MVCH_NO2
      REPLACE C1 WITH MC1, C2 WITH MC2, C3 WITH MC3
      REPLACE DR_CR WITH 'DR', SR_NO WITH MSR_NO
      REPLACE DESCRIP WITH 'Sales ', AMOUNT WITH MTOT1
      UNLOCK

      SELECT 1

      DO WHILE .not. EOF()

         MI1= I1
         MI2= I2
         MI3= I3
         MTOT= PSR_TPRICE



         SELECT 3
         DO SEEKICODE IN SEEKLIB1.PRG
         
         *SEEK STR(MI1,2)+STR(MI2,2)+STR(MI3,4)


         IF FOUND()
            MDC1= CC1
            MDC2= CC2
            MDC3= CC3
         ENDIF

* Current Amendment on 1st Aug.2006 8:05 PM

         SELECT 4
		 DO SEEKDC1CDE IN SEEKLIB1.PRG

         *SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)

         IF FOUND()
            MDTITLE= TITLE
         ENDIF

         SELECT 2

         MSR_NO= MSR_NO+1
         APPEND BLANK

         IF .not. RLOCK()
            rlock()
         ENDIF

         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE
         REPLACE C1 WITH MDC1, C2 WITH MDC2, C3 WITH MDC3, DATE WITH MDATE
         REPLACE DR_CR WITH 'CR', AMOUNT WITH MTOT, SR_NO WITH MSR_NO
         REPLACE DESCRIP WITH MTITLE, VCH_NO2 WITH MVCH_NO2
         UNLOCK

         SELECT 1
         SKIP

      ENDDO


      SELECT 2
      USE

      SELECT 1

      USE


   ENDIF


   SELECT 2
   USE st-tran ORDER stidx

   DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   APPEND FROM st-cor FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2

   SET FILTER TO DOC_TYPE=6

   GOTO TOP

   SELECT 1
   USE st-cor

   DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   USE

   @ 23, 50 SAY 'Total '
   @ 23, 63 SAY MTOT1 PICTURE '9999999999.99'
   ANS= ' '

   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'More Data Correction - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO

   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

ENDDO



RETURN
****************


************************
PROCEDURE SALE_DEL
*************************

* CLEAR MACRO SHIFT+CTRL+D
RESTORE MACROS FROM CLRMACS.FKY

CLEAR
CLOSE ALL

SET DATE brit
SET CENT ON
SET DELETED ON

SELECT 2
USE st-tran ORDER stidx
USE st-tran ORDER stidx1
USE st-tran ORDER stidx

SET FILTER TO DOC_TYPE = 6

GOTO TOP

SELECT 3
USE item ORDER item_idx

SELECT 4
USE ldgrcode ORDER ac1

SELECT 5
USE loc ORDER loc_idx

SELECT 6
USE p-order ORDER pord_idx


SELECT 7
USE item-bal ALIAS item_bal EXCLUSIVE
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item
INDEX ON STR(LOC_CODE,4)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG itm_bal
SET ORDER TO itm_bal


DO WHILE .T.

		IF _DOS
   			SET COLOR TO 7/1 
		ELSE
			SET COLOR TO
		ENDIF

   CLEAR
   R= 3
   T= '   '
   MVCH1= '  '
   MVCH2= '     '
   MDOC_TYPE= 6
   MVCH_TYPE= 6

		IF _DOS
   			SET COLOR TO +7/1 
		ELSE
			SET COLOR TO
		ENDIF

   @ 1, 20 SAY 'I N V E N T O R Y  : S A L E   D E L E T I O N '

		IF _DOS
   			SET COLOR TO 7/1 
		ELSE
			SET COLOR TO
		ENDIF

   @ 2, 1 TO 2, 79 DOUBLE

   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT

   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'INV No      ' GET MDOC_NO PICTURE '9999999'

   READ

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

   @ 23, 45 CLEAR TO 23, 78

   IF MDOC_NO=0
      LOOP
   ENDIF

   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

		IF _DOS
   			SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

   @ R, 15 SAY MDOC

		IF _DOS
   			SET COLOR TO 7/1 
		ELSE
			SET COLOR TO
		ENDIF

   SELECT 2
   DO SEEKSTTRAN IN SEEKLIB1.PRG
   
   *SET ORDER TO STIDX
   *SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   
   
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This INV No Does Not Exists ... Press any key'
      WAIT ''

		IF _DOS
      		SET COLOR TO 7/1 
		ELSE
			SET COLOR TO
		ENDIF

      @ 23, 10 SAY SPACE(75)

      LOOP

   ENDIF

   IF FLAG='Y'

      CLEAR

      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Delete it. If you still want to Delete   '
      @ 14, 15 SAY 'it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '

      WAIT ''

      CLEAR
      CLOSE ALL

      RETURN


   ENDIF

   MVCH_NO1= VCH_NO1
   MVCH_NO2= VCH_NO2

   SET ORDER TO 2
   T= 'SV-'

		IF _DOS
   			SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

   @ R, 45 SAY T
   @ R, 48 SAY '  -     '

		IF _DOS
   			SET COLOR TO 7/1
		ELSE
			SET COLOR TO
		ENDIF

   @ R, 48 GET MVCH_NO1 PICTURE '@z 99'
   READ

   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF

   IF MVCH_NO1=0
      LOOP
   ENDIF

   MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(MVCH_NO1))))+LTRIM(STR(MVCH_NO1))

		IF _DOS
   			SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

   @ R, 48 SAY MVCH1

		IF _DOS
   			SET COLOR TO 7/1   
		ELSE
			SET COLOR TO
		ENDIF

   @ R, 51 GET MVCH_NO2 PICTURE '@z 99999'
   READ

   IF MVCH_NO2=0
      LOOP
   ENDIF

   MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(MVCH_NO2))))+LTRIM(STR(MVCH_NO2))

		IF _DOS
   			SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

   @ R, 51 SAY MVCH2

		IF _DOS
   			SET COLOR TO 7/1
		ELSE
			SET COLOR TO
		ENDIF

*Current Amendment on Sat. 5th Aug.2006 at 8:10 am

	*PROCEDURE SALE_DEL
	*Sale Deletion
	*USE st-tran ORDER stidx
	
   SELECT 2
   DO SEEKSTTRN2 IN SEEKLIB1.PRG
   
   *SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)+STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)


   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 13 SAY 'This INV No & Voucher No Does Not Exists ... Press any key'
      WAIT ''

			IF _DOS
      			SET COLOR TO 7/1 
			ELSE
				SET COLOR TO
			ENDIF

      @ 23, 10 SAY SPACE(80)
      LOOP
   ENDIF

   SELECT 2
   USE

   SELECT 2

   USE tran ALIAS tran EXCLUSIVE
   INDEX ON STR(vch_type,1)+STR(vch_no1,2)+STR(vch_no2,5) TAG tranent

   SET ORDER TO tranent

   FLUSH

   
   
   *INDEX tranent


	DO SEEKTRANK1 IN SEEKLIB1.PRG
	
   *SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)
   
   
   IF .not. FOUND()
      @ 23, 13 SAY 'This Voucher No Does Not Exists ... Press any key'
      WAIT ''

		IF _DOS
      		SET COLOR TO 7/1 
		ELSE
			SET COLOR TO
		ENDIF

      @ 23, 10 SAY SPACE(75)
      MV_NO= 0
      LOOP
   ENDIF

   IF FLAG='Y'

      CLEAR

      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Delete it. If you still want to Delete   '
      @ 14, 15 SAY 'it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '

      WAIT ''

      CLEAR
      CLOSE ALL

      RETURN

   ENDIF


   @ 21, 1 CLEAR TO 24, 79

   SELECT 2
   USE

   SELECT 1
   USE st-cor

   DELETE ALL

   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE

   SELECT 1

   MDATE= DATE

   @ R, 60 SAY 'Date ' GET MDATE

   R= R+1

   ABC= 0


   DO WHILE ABC=0

      SELECT 1

      MC1= D_D1
      MC2= D_D2
      MC3= D_D3

      MLOC_CODE= LOC_CODE

      MCITY_CODE= CITY_CODE

		IF _DOS
      		SET COLOR TO 7/1 
		ELSE
			SET COLOR TO
		ENDIF

      @ R, 2 SAY 'Credit / Party Code'

		IF _DOS
      		SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

      @ R, 23 SAY '  -  -   '

		IF _DOS
      		SET COLOR TO 7/1 
		ELSE
			SET COLOR TO
		ENDIF

      @ R, 23 GET MC1 PICTURE '99'
      READ

      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

		IF _DOS
      		SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'

      READ

      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'

      READ

      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3

		IF _DOS
      		SET COLOR TO 7/1 
		ELSE
			SET COLOR TO
		ENDIF

      SELECT 4
      DO SEEKMCODE1 IN SEEKLIB1.PRG
      
      *STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      *SEEK MCODE
      
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)

         LOOP


      ENDIF

		IF _DOS
      		SET COLOR TO /w 
		ELSE
			SET COLOR TO
		ENDIF

      @ R, 33 SAY TITLE

      MTITLE= TITLE

		IF _DOS
      		SET COLOR TO 7/1 
		ELSE
			SET COLOR TO
		ENDIF

      ABC= 1


   ENDDO
   
   R= R+1
   ABC= 0
   

   DO WHILE ABC=0
      
      @ R, 2 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
      READ
      
      IF MLOC_CODE=0
         LOOP
      ENDIF
      
      
      SELECT 5
      DO SEEKLOC1 IN SEEKLIB1.PRG
      
      *SEEK MLOC_CODE
      
      
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF


      @ R, 25 SAY LOC

      MLOC= LOC


      ABC= 1



   ENDDO

   ABC= 0


   DO WHILE ABC=0

      @ R, 57 SAY 'City '
      @ R, 62 GET MCITY_CODE PICTURE '999'
      READ

		IF _DOS
      		SET COLOR TO /w
		ELSE
			SET COLOR TO
		ENDIF

      MPC1= REPLICATE('0',3-LEN(LTRIM(STR(MCITY_CODE))))+LTRIM(STR(MCITY_CODE))

      @ R, 62 SAY MPC1

		IF _DOS
      		SET COLOR TO 
		ELSE
			SET COLOR TO
		ENDIF

      IF MCITY_CODE=0
         LOOP
      ENDIF


*Current Amendment on Sun. 06th Aug.2006 5:05 pm
      
      SELECT 2
      DO SEEKCITY IN SEEKLIB1.PRG
      
      *USE city ORDER city_idx
      *SEEK MCITY_CODE
      
      
      IF .not. FOUND()
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 14 CLEAR TO 23, 73
         LOOP
      ENDIF
      @ R, 66 SAY CITY
      MZONE_CODE= ZONE_CODE
      ABC= 1
   ENDDO
   SELECT 2
   USE
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   CLEAR GETS
   SELECT 1
   GOTO TOP
   SET FILTER TO VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   GOTO TOP
   DO WHILE .not. EOF()
      SELECT 1
      MI1= I1
      MI2= I2
      MI3= I3
      MORDER_NO= ORDER_NO
      MQTY= T_QTY
      MRATE= PSR_UPRICE
      MAMOUNT= PSR_TPRICE

IF _DOS
      SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

      @ R, 2 SAY '  -  -    '
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
      @ R, 2 SAY MP1
      @ R, 4 SAY '-'
      @ R, 5 SAY MI2 PICTURE '99'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
      @ R, 5 SAY MP2
      @ R, 7 SAY '-'
      @ R, 8 SAY MI3 PICTURE '9999'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
      @ R, 8 SAY MP3

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      SELECT 3
      STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
      SEEK MI_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 13 SAY ITEM
      SELECT 1

IF _DOS
      SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

      @ R, 30 SAY MORDER_NO PICTURE '99999'
      @ R, 36 SAY MQTY PICTURE '999999999.9999'
      @ R, 51 SAY MRATE PICTURE '99999999.9999'
      @ R, 65 SAY MAMOUNT PICTURE '9999999999.99'

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      R= R+1
      IF R>=23
         WAIT
         @ 9, 0 CLEAR TO 24, 78
         R= 9
      ENDIF
      SKIP
   ENDDO
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='Y'
      IF INT=1
         SELECT 2

    	USE tran ALIAS tran EXCLUSIVE
		INDEX ON STR(vch_type,1)+STR(vch_no1,2)+STR(vch_no2,5) TAG tranent
   		SET ORDER TO tranent
   		FLUSH
         
         *INDEX tranent


         DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE
         SELECT 2
         USE tran-key ORDER key
         DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
         USE
      ENDIF
      SELECT 2
      USE st-key ORDER st_idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         DELETE
      ENDIF
      USE
      SELECT 2
      USE st-tran ORDER stidx, stidx1
      DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
      SET FILTER TO DOC_TYPE=6
      GOTO TOP
   ENDIF
   SELECT 1
   USE st-cor
   DELETE ALL FOR VCH_TYPE=MVCH_TYPE.and.VCH_NO1=MVCH_NO1.and.VCH_NO2=MVCH_NO2
   USE
   @ 24, 20 CLEAR TO 24, 78
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Any More Deletion   - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN

*********************
PROCEDURE S_COR
*********************
SELECT 1
GOTO TOP
MREC2= RECNO()
DO sal_dsp
GOTO TOP
MREC2= RECNO()
R= 8
CK= 0
DO WHILE .T.
   R= R+1
   IF R>=23
      @ 9, 0 CLEAR TO 24, 79
      R= 9
      IF .not. EOF()
         MREC2= RECNO()
         DO sal_dsp
         GOTO MREC2
         R= 9
      ENDIF
   ENDIF
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 1
      DELETE
   ENDIF
   IF MQES='N'
      SELECT 1
      IF .not. EOF()
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 1
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE 0 TO MI1, MI2, MI3, MQTY, MORDER_NO, MRATE, MAMOUNT
         DO WHILE .T.

IF _DOS
            SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

            @ R, 2 SAY '  -  -    '

IF _DOS
            SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

            @ R, 2 GET MI1 PICTURE '99'
            READ

IF _DOS
            SET COLOR TO n/g
ELSE
SET COLOR TO
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 2 SAY MP1
            @ R, 4 SAY '-'
            @ R, 5 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 5 SAY MP2
            @ R, 7 SAY '-'
            @ R, 8 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 8 SAY MP3

IF _DOS
            SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

            @ 23, 50 CLEAR TO 23, 78
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            IF MI1=0.and.MI2=0.and.MI3=0
               LOOP
            ENDIF
            SELECT 3
            STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
            SEEK MI_CODE
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF
            @ R, 13 SAY ITEM
            MCC1= CC1
            MCC2= CC2
            MCC3= CC3
            IF INT=1
               IF MCC1=0.and.MCC2=0.and.MCC3=0
                  @ 22, 14 SAY 'Debit Code (For Purchase Voucher) does not exist.'
                  @ 23, 14 SAY 'Please feed Debit Code using Item Code Correction Option '
                  WAIT ''
                  CLEAR
                  CLOSE ALL
                  RETURN
               ENDIF
               SELECT 4
               SEEK STR(MCC1,2)+STR(MCC2,2)+STR(MCC3,3)
               IF FOUND()
                  MCTITLE= TITLE
               ENDIF
            ENDIF
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            MT= 0
            DO WHILE MT=0
               @ R, 30 GET MORDER_NO PICTURE '99999'
               READ
               IF MORDER_NO=0
                  EXIT
               ENDIF
               MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
               SET COLOR TO /g
ELSE
SET COLOR TO
ENDIF

               @ R, 30 SAY MORD

IF _DOS
               SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

               SELECT 6
               STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
               SEEK ORD
               IF .not. FOUND()
                  ?? CHR(7)
                  @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
                  WAIT ''
                  @ 23, 10 SAY SPACE(58)
                  LOOP
               ENDIF
               MT= 1
            ENDDO
            DO WHILE .T.
               @ R, 36 GET MQTY PICTURE '9999999.9999'
               READ
               IF MQTY=0
                  LOOP
               ENDIF
               EXIT
            ENDDO
            @ R, 49 GET MRATE PICTURE '99999999.9999'
            READ
            MAMOUNT= 0
            MAMOUNT= MQTY*MRATE
            @ R, 63 GET MAMOUNT PICTURE '9999999999.99'
            READ
            EXIT
         ENDDO
         SELECT 1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE, VCH_NO2 WITH MVCH_NO2
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE D_D1 WITH MC1, D_D2 WITH MC2, D_D3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
         REPLACE CITY_CODE WITH MCITY_CODE, ZONE_CODE WITH MZONE_CODE
         REPLACE RATE WITH MRATE, T_AMOUNT WITH MAMOUNT
         UNLOCK
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 1
         MI1= I1
         MI2= I2
         MI3= I3
         MQTY= T_QTY
         MRATE= PSR_UPRICE
         MAMOUNT= PSR_TPRICE

IF _DOS
         SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

         @ R, 2 SAY '  -  -    '

IF _DOS
         SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

         @ R, 2 GET MI1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO n/g
ELSE
SET COLOR TO
ENDIF

         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ R, 2 SAY MP1
         @ R, 4 SAY '-'
         @ R, 5 GET MI2 PICTURE '99'
         READ
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ R, 5 SAY MP2
         @ R, 7 SAY '-'
         @ R, 8 GET MI3 PICTURE '9999'
         READ
         IF MI3=0
            LOOP
         ENDIF
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ R, 8 SAY MP3

IF _DOS
         SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

         @ 23, 50 CLEAR TO 23, 78
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         IF MI1=0.and.MI2=0.and.MI3=0
            LOOP
         ENDIF
         SELECT 3
         STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
         SEEK MI_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         @ R, 13 SAY ITEM
         MCC1= CC1
         MCC2= CC2
         MCC3= CC3
         IF INT=1
            IF MCC1=0.and.MCC2=0.and.MCC3=0
               @ 22, 14 SAY 'Debit Code (For Purchase Voucher) does not exist.'
               @ 23, 14 SAY 'Please feed Debit Code using Item Code Correction Option '
               WAIT ''
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            SELECT 4
            SEEK STR(MCC1,2)+STR(MCC2,2)+STR(MCC3,3)
            IF FOUND()
               MCTITLE= TITLE
            ENDIF
         ENDIF
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 7
         SEEK XYZ
         IF .not. FOUND()
            @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
            WAIT ''
            @ 21, 1 SAY SPACE(79)
            LOOP
         ENDIF
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
            SET COLOR TO /g
ELSE
SET COLOR TO
ENDIF

            @ R, 30 SAY MORD

IF _DOS
            SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

            SELECT 6
            STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 49 GET MRATE PICTURE '99999999.9999'
         READ
         MAMOUNT= 0
         MAMOUNT= MQTY*MRATE
         @ R, 63 GET MAMOUNT PICTURE '9999999999.99'
         READ
         EXIT
      ENDDO
      SELECT 1
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE VCH_NO1 WITH MVCH_NO1, VCH_TYPE WITH MVCH_TYPE, VCH_NO2 WITH MVCH_NO2
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
      REPLACE D_D1 WITH MC1, D_D2 WITH MC2, D_D3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
      REPLACE CITY_CODE WITH MCITY_CODE, ZONE_CODE WITH MZONE_CODE
      REPLACE RATE WITH MRATE, T_AMOUNT WITH MAMOUNT
      UNLOCK
   ENDIF
   SELECT 1
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      @ R, 79 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      IF R<22
         SELECT 1
         IF .not. EOF()
            SKIP
         ENDIF
         SELECT 1
      ELSE
         @ 9, 1 CLEAR TO 23, 79
         IF .not. EOF()
            SKIP
         ENDIF
      ENDIF
   ENDIF
   IF MANS='N'
      SELECT 1
      GOTO TOP
      SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      GOTO TOP
      MSR= 0
      DO WHILE .not. EOF()
         MSR= MSR+1
         REPLACE SR_NO WITH MSR
         SKIP
      ENDDO
      RETURN
   ENDIF
ENDDO
RETURN
******************


****************************
PROCEDURE SAL_DSP
*****************************
R= 9
@ R, 0 CLEAR TO 24, 79
SELECT 1
GOTO MREC2
DO WHILE .not. EOF()
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   MORDER_NO= ORDER_NO
   MQTY= T_QTY
   MRATE= PSR_UPRICE
   MAMOUNT= PSR_TPRICE

IF _DOS
   SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

   @ R, 2 SAY '  -  -    '
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ R, 2 SAY MP1
   @ R, 4 SAY '-'
   @ R, 5 SAY MI2 PICTURE '99'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ R, 5 SAY MP2
   @ R, 7 SAY '-'
   @ R, 8 SAY MI3 PICTURE '9999'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ R, 8 SAY MP3

IF _DOS
   SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

   SELECT 3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SEEK MI_CODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   @ R, 13 SAY ITEM
   SELECT 1

IF _DOS
   SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

   @ R, 30 SAY MORDER_NO PICTURE '99999'
   @ R, 36 SAY MQTY PICTURE '9999999.9999'
   @ R, 49 SAY MRATE PICTURE '99999999.9999'
   @ R, 63 SAY MAMOUNT PICTURE '9999999999.99'

IF _DOS
   SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

   R= R+1
   IF R>=23
      RETURN
   ENDIF
   SKIP
ENDDO
RETURN


************************
PROCEDURE SALE_PRT
************************
CLEAR
CLOSE ALL
SELECT 3
USE item ORDER item_idx
SELECT 4
USE ldgrcode ORDER ac1
SELECT 5
USE loc ORDER loc_idx
SELECT 6
USE p-order ORDER pord_idx

SELECT 7
USE item-bal ALIAS item_bal EXCLUSIVE
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_bal
SET ORDER TO itm_bal

*USE item-bal ORDER itm_bal


MUNIT= '    '

IF _DOS
SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 3, 33 SAY 'Invoice Printing Range '
   @ 4, 33 SAY '---------------------- '
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 6
   MVCH1= '  '
   MVCH2= '     '
   STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO, MDOC_NO1, CNT
   @ 10, 20 SAY 'From INV No : '
   @ 10, 35 GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

   @ 10, 35 SAY MVCH_NO

IF _DOS
   SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

   SELECT 2
   USE st-tran ORDER stidx
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This INV No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   @ 12, 20 SAY 'To   INV No : '
   @ 12, 35 GET MDOC_NO1 PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO1=0
      LOOP
   ENDIF
   MVCH_NO1= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO1))))+LTRIM(STR(MDOC_NO1))

IF _DOS
   SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

   @ 12, 35 SAY MVCH_NO1

IF _DOS
   SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This INV No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   MNO= 1
   @ 14, 20 SAY 'No Of Copies ... ' GET MNO PICTURE '9'
   READ
   SELECT 2
   COPY TO st-rep FOR DOC_NO>=MDOC_NO.and.DOC_NO<=MDOC_NO1.and.DOC_TYPE=6
   SELECT 2
   USE
   SELECT 1
   USE st-rep
   GOTO TOP
   IF RECCOUNT()=0
      CLEAR
      @ 12, 12 SAY 'No GRN Exist in Data File  -  Press any key to return to menu .... '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   ABC= 1
ENDDO
SELECT 1
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
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
MCOPY= 1
DO WHILE MCOPY<=MNO
   HEAD= .T.
   CLEAR
   SELECT 1
   GOTO TOP
   DO WHILE .not. EOF()
      MDOC_NO= DOC_NO
      STORE 0 TO MTOT
      DO WHILE DOC_NO=MDOC_NO
         IF HEAD
            L= 3
            IF P='P'
               DO top_prt IN TBL_PROC.PRG
               @ 3, 35 SAY 'INVOICE '
               L= L+1
               @ L, 35 SAY '------- '
               L= L+1
            ELSE
               DO screen IN TBL_PROC.PRG
               @ 3, 30 SAY 'I N V O I C E '
               L= L+1
               @ L, 30 SAY '------------- '
               L= L+1
               CNT= 0
               MREC2= RECNO()
            ENDIF
            MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
            MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
            MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
            @ L, 1 SAY 'INV No       : '
            @ L, 18 SAY MVCH_NO
            @ L, 30 SAY 'Voucher No : '
            @ L, 43 SAY 'SV-'
            @ L, 46 SAY MVCH1
            @ L, 48 SAY '-'
            @ L, 49 SAY MVCH2
            @ L, 65 SAY 'Date : '+DTOC(DATE)
            L= L+1
            MC1= D_D1
            MC2= D_D2
            MC3= D_D3
            MLOC_CODE= LOC_CODE
            @ L, 1 SAY 'Credit/Party : '
            MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
            @ L, 18 SAY MPC1
            @ L, 20 SAY '-'
            MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
            @ L, 21 SAY MPC2
            @ L, 23 SAY '-'
            MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
            @ L, 24 SAY MPC3
            STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
            SELECT 4
            SEEK MCODE
            IF FOUND()
               @ L, 30 SAY TITLE
            ENDIF
            L= L+1
            SELECT 1
            @ L, 1 SAY 'Location     : '
            MLOC_CODE= REPLICATE('0',4-LEN(LTRIM(STR(LOC_CODE))))+LTRIM(STR(LOC_CODE))
            @ L, 18 SAY MLOC_CODE
            SELECT 5
            SEEK MLOC_CODE
            IF FOUND()
               @ L, 30 SAY LOC
            ENDIF
            L= L+1
            @ L, 1 SAY REPLICATE('-',79)
            L= L+1
            @ L, 1 SAY 'Item Code'
            @ L, 12 SAY 'Item '
            @ L, 31 SAY 'Unit'
            @ L, 37 SAY 'Order #'
            @ L, 51 SAY 'Qty '
            @ L, 59 SAY 'Rate '
            @ L, 74 SAY 'Amount'
            L= L+1
            @ L, 1 SAY REPLICATE('-',79)
            L= L+1
            HEAD= .F.
         ENDIF
         SELECT 1
         IF DOC_NO<>MDOC_NO
            EXIT
         ENDIF
         MP1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
         @ L, 1 SAY MP1
         @ L, 3 SAY '-'
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
         @ L, 4 SAY MP2
         @ L, 6 SAY '-'
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
         @ L, 7 SAY MP3
         STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
         SELECT 3
         SEEK MI_CODE
         IF FOUND()
            MUNIT= UNIT
            @ L, 12 SAY ITEM
         ENDIF
         SELECT 1
         MQTY= T_QTY
         MRATE= RATE
         MAMT= MQTY*MRATE
         @ L, 33 SAY MUNIT
         @ L, 37 SAY ORDER_NO
         @ L, 41 SAY MQTY PICTURE '9999999.9999'
         @ L, 53 SAY MRATE PICTURE '9999999.9999'
         @ L, 66 SAY MAMT PICTURE '999999999.99'
         MTOT= MTOT+MAMT
         L= L+1
         SKIP
         CNT= CNT+1
         IF P='S'.and.L>20
            @ 23, 1 SAY REPLICATE('-',79)
            @ 24, 5 SAY 'For Next Page Press :'
            @ 24, 30 SAY 'For Previous Page :'
            @ 24, 60 SAY 'Quit :'

IF _DOS
            SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

            @ 24, 27 SAY CHR(25)
            @ 24, 50 SAY CHR(24)
            @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)

IF _DOS
            SET COLOR TO 7/1
ELSE
SET COLOR TO
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
               RUN del st-rep.dbf
               CLOSE ALL
               RETURN
            ENDIF
            IF READKEY()=4
               CLEAR
               GOTO MREC2
               SKIP  -CNT
               HEAD= .T.
            ENDIF
            IF READKEY()=5
               CLEAR
               HEAD= .T.
            ENDIF
         ENDIF
         IF P='P'.and.L>56
            HEAD= .T.
         ENDIF
      ENDDO
      @ L, 50 SAY REPLICATE('-',30)
      L= L+1
      @ L, 54 SAY 'Total : '
      @ L, 65 SAY MTOT PICTURE '999999999.99'
      L= L+1
      @ L, 50 SAY REPLICATE('-',30)
      L= L+1
      IF P='S'
         @ 23, 1 SAY REPLICATE('-',79)
         @ 24, 5 SAY 'For Next Page Press :'
         @ 24, 30 SAY 'For Previous Page :'
         @ 24, 60 SAY 'Quit :'

IF _DOS
         SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

         @ 24, 27 SAY CHR(25)
         @ 24, 50 SAY CHR(24)
         @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)

IF _DOS
         SET COLOR TO 7/1
ELSE
SET COLOR TO
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
            RUN del st-rep.dbf
            CLOSE ALL
            RETURN
         ENDIF
         IF READKEY()=4
            CLEAR
            GOTO MREC2
            SKIP  -CNT
            HEAD= .T.
         ENDIF
         IF READKEY()=5
            CLEAR
            HEAD= .T.
         ENDIF
      ENDIF
      IF P='P'
         HEAD= .T.
      ENDIF
      SELECT 1
   ENDDO
   MCOPY= MCOPY+1
ENDDO
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
RUN del st-rep.dbf
CLEAR
CLOSE ALL
RETURN
******************

****************************
PROCEDURE SALE_VCH
*****************************
CLEAR
CLOSE ALL
SELECT 1
USE st-tran ORDER stidx
SELECT 2
USE item ORDER item_idx
SELECT 3
USE ldgrcode ORDER ac1
STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO
STORE DATE() TO MFDAT, MTDAT
ABC= 0
DO WHILE ABC=0
   @ 3, 25 SAY 'VERIFICATION VOUCHER  REPORT'
   @ 4, 25 SAY '----------------------------'
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 1
   @ 10, 30 SAY 'From Date : ' GET MFDAT
   @ 12, 30 SAY 'To   Date : ' GET MTDAT
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MTDAT<MFDAT
      @ 23, 10 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 10 SAY '                                        '
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=MFDAT.and.DATE<=MTDAT.and.DOC_TYPE=6
   IF RECCOUNT()=0
      CLEAR
      @ 23, 10 SAY 'No Transaction Exist in These Two Dates -Press any key to return to menu ....'
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   ABC= 1
ENDDO
SELECT 1
GOTO TOP
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
IF P='P'
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
L= 3
HEADING= .T.
CNT= 0
PG= 0
DO WHILE .not. EOF()
   SELECT 1
   IF HEADING
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(18)
      @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
      L= L+1
      @ L, 27 SAY '---------------------------'
      L= L+1
      PG= PG+1
      @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
      L= L+1
      @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
      @ L, 67 SAY 'Page : '+STR(PG,3)
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 1 SAY 'Date'
      @ L, 10 SAY 'Vch # '
      @ L, 20 SAY 'Inv # '
      @ L, 30 SAY 'Party'
      @ L, 59 SAY 'Item'
      @ L, 88 SAY 'Order #'
      @ L, 103 SAY 'Qty'
      @ L, 113 SAY 'Rate'
      @ L, 126 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(18)
         @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
         L= L+1
         @ L, 27 SAY '---------------------------'
         L= L+1
         PG= PG+1
         @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
         L= L+1
         @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
         @ L, 67 SAY 'Page : '+STR(PG,3)
         L= L+1
         @ L, 1 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 1 SAY 'Date'
         @ L, 10 SAY 'Vch # '
         @ L, 20 SAY 'Inv # '
         @ L, 30 SAY 'Party'
         @ L, 59 SAY 'Item'
         @ L, 88 SAY 'Order #'
         @ L, 103 SAY 'Qty'
         @ L, 113 SAY 'Rate'
         @ L, 126 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      SELECT 1
      MVCH1= '  '
      MVCH2= '     '
      MDOCNO= '       '
      MDOCNO= REPLICATE('0',7-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      MVCH1= REPLICATE('0',2-LEN(LTRIM(STR(VCH_NO1))))+LTRIM(STR(VCH_NO1))
      MVCH2= REPLICATE('0',5-LEN(LTRIM(STR(VCH_NO2))))+LTRIM(STR(VCH_NO2))
      @ L, 1 SAY DATE
      @ L, 10 SAY 'SV-'
      @ L, 13 SAY MVCH1
      @ L, 15 SAY '-'
      @ L, 16 SAY MVCH2
      @ L, 22 SAY MDOCNO
      MD_D1= D_D1
      MD_D2= D_D2
      MD_D3= D_D3
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      STORE STR(MD_D1,2)+STR(MD_D2,2)+STR(MD_D3,3) TO MP_CODE
      SELECT 3
      SEEK MP_CODE
      IF FOUND()
         @ L, 30 SAY TITLE
      ENDIF
      SELECT 2
      SEEK MI_CODE
      IF FOUND()
         @ L, 59 SAY ITEM
      ENDIF
      SELECT 1
      MQTY= T_QTY
      MRATE= PSR_UPRICE
      MAMT= MQTY*MRATE
      @ L, 87 SAY ORDER_NO
      @ L, 93 SAY MQTY PICTURE '9999999.9999'
      @ L, 106 SAY MRATE PICTURE '99999999.9999'
      @ L, 120 SAY MAMT PICTURE '9999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      IF P='P'.and.L>56
         @ 1, 1 SAY CHR(18)
         L= 3
         HEADING= .T.
      ENDIF
   ENDDO
   L= L+1
ENDDO
@ L, 102 SAY REPLICATE('-',30)
L= L+1
@ L, 105 SAY 'Total : '
@ L, 117 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 102 SAY REPLICATE('-',30)
L= L+1
IF P='P'
   EJECT
   @ 1, 1 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN
***************

**************************
PROCEDURE SALR_ENT
**************************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 3
USE item ORDER item_idx
SELECT 4
USE ldgrcode ORDER ac1
SELECT 5
USE loc ORDER loc_idx
SELECT 6
USE s-order ORDER sord-idx

SELECT 7
USE item-bal ALIAS item_bal EXCLUSIVE
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_bal
SET ORDER TO itm_bal

*USE item-bal ORDER itm_bal


DO WHILE .T.
   CLEAR
   R= 3
   MDOC_TYPE= 7
   @ 1, 30 SAY 'SALE RETURN ENTRY '
   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MAMT
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT, MSR_NO
   DO WHILE .T.
      @ 23, 50 SAY 'Press Ctrl Q To Quit '
      @ R, 2 SAY 'S.R No    '
      @ R, 15 GET MDOC_NO PICTURE '9999999'
      READ
      IF READKEY()=12
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      IF MDOC_NO=0
         LOOP
      ENDIF
      MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
      SET COLOR TO /g
ELSE
SET COLOR TO
ENDIF

      @ R, 15 SAY MVCH_NO

IF _DOS
      SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

      SELECT 2
      USE st-key ORDER st_idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         @ 24, 1 SAY SPACE(78)
         @ 24, 15 SAY 'This S.R. No Already Exists  -- Press any key'
         WAIT ''

IF _DOS
         SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

         @ 24, 10 SAY SPACE(75)
         LOOP
      ENDIF
      APPEND BLANK
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE
      UNLOCK
      SELECT 2
      USE
      @ R, 57 SAY 'Date ' GET MDATE
      READ
      DO chk IN TBL_PROC.PRG
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Credit / Party Code'

IF _DOS
         SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

         @ R, 23 SAY '  -  -   '

IF _DOS
         SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

         @ R, 23 GET MC1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO n/g
ELSE
SET COLOR TO
ENDIF

         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ R, 23 SAY MPC1
         @ R, 25 SAY '-'
         @ R, 26 GET MC2 PICTURE '99'
         READ
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ R, 26 SAY MPC2
         @ R, 28 SAY '-'
         @ R, 29 GET MC3 PICTURE '999'
         READ
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ R, 29 SAY MPC3

IF _DOS
         SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

         SELECT 4
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SEEK MCODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF

IF _DOS
         SET COLOR TO /w 
ELSE
SET COLOR TO
ENDIF

         @ R, 33 SAY TITLE
         MTITLE= TITLE

IF _DOS
         SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

         ABC= 1
      ENDDO
      R= R+1
      ABC= 0
      DO WHILE ABC=0
         @ R, 2 SAY 'Location    ' GET MLOC_CODE PICTURE '9999'
         READ
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         @ 23, 45 CLEAR TO 23, 78
         IF MLOC_CODE=0
            LOOP
         ENDIF
         SELECT 5
         SEEK MLOC_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         @ R, 33 SAY LOC
         MLOC= LOC
         ABC= 1
      ENDDO
      R= R+1
      @ R, 1 TO R, 79
      R= R+1
      @ R, 2 SAY 'Item Code'
      @ R, 13 SAY 'Item '
      @ R, 30 SAY 'Order #'
      @ R, 45 SAY 'Qty '
      @ R, 55 SAY 'Rate '
      @ R, 65 SAY 'Amount '
      @ R, 76 SAY 'More'
      R= R+1
      @ R, 0 TO R, 79 DOUBLE
      R= R+1
      DO WHILE .T.
         ABC= 0
         DO WHILE ABC=0

IF _DOS
            SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

            @ R, 2 SAY '  -  -    '

IF _DOS
            SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

            @ R, 2 GET MI1 PICTURE '99'
            READ

IF _DOS
            SET COLOR TO n/g
ELSE
SET COLOR TO
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 2 SAY MP1
            @ R, 4 SAY '-'
            @ R, 5 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 5 SAY MP2
            @ R, 7 SAY '-'
            @ R, 8 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 8 SAY MP3

IF _DOS
            SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

            @ 23, 50 CLEAR TO 23, 78
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            IF MI1=0.and.MI2=0.and.MI3=0
               LOOP
            ENDIF
            SELECT 3
            STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
            SEEK MI_CODE
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF
            @ R, 13 SAY ITEM
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            ABC= 1
         ENDDO
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
            SET COLOR TO /g
ELSE
SET COLOR TO
ENDIF

            @ R, 30 SAY MORD

IF _DOS
            SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

            SELECT 6
            STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 49 GET MRATE PICTURE '99999999.9999'
         READ
         MTOT= 0
         MTOT= MQTY*MRATE
         @ R, 63 GET MTOT PICTURE '9999999999.99'
         READ
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            ANS= 'Y'
            @ R, 79 GET ANS PICTURE '!'
            READ
         ENDDO
         SELECT 1
         USE st-work
         MSR_NO= MSR_NO+1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH 7, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3, PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MTOT
         REPLACE SR_NO WITH MSR_NO
         UNLOCK
         USE
         MAMT= MAMT+MTOT
         IF ANS='Y'
            STORE 0 TO MI1, MI2, MI3, MRATE, MQTY, MORDER_NO
            R= R+1
            IF R>=21
               @ 9, 0 CLEAR TO 24, 79
               R= 9
            ENDIF
            LOOP
         ENDIF
         @ 23, 50 SAY 'Total '
         @ 23, 63 SAY MAMT PICTURE '9999999999.99'
         ANS= ' '
         DO WHILE .not. ANS$'YN'
            @ 24, 25 SAY 'Any Correction - Y/N' GET ANS FUNCTION '!'
            READ
         ENDDO
         @ 24, 20 CLEAR TO 24, 78
         IF ANS='Y'
            SELECT 1
            USE st-work
            SET FILTER TO DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
            DO sr-cor
         ENDIF
         SELECT 1
         USE
         SELECT 2
         USE st-tran ORDER stidx
         APPEND FROM st-work FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         SELECT 2
         USE st-key ORDER st_idx
         SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
         IF FOUND()
            REPLACE FLAG WITH 'Y'
         ENDIF
         USE
         SELECT 1
         USE st-work
         DELETE ALL FOR DOC_TYPE=MDOC_TYPE.and.DOC_NO=MDOC_NO
         USE
         EXIT
      ENDDO
      ANS= ' '
      DO WHILE .not. ANS$'YN'
         @ 24, 25 SAY 'More Data Entry - Y/N' GET ANS FUNCTION '!'
         READ
      ENDDO
      IF ANS='N'
         CLEAR
         CLOSE ALL
         RETURN
      ENDIF
      EXIT
   ENDDO
ENDDO
RETURN
*********************


*******************************
PROCEDURE SALR_COR
*******************************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran ORDER stidx
SET FILTER TO DOC_TYPE=7
GOTO TOP
SELECT 3
USE item ORDER item_idx
SELECT 4
USE ldgrcode ORDER ac1
SELECT 5
USE loc ORDER loc_idx
SELECT 6
USE s-order ORDER sord-idx

SELECT 7
USE item-bal ALIAS item_bal EXCLUSIVE
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_bal
SET ORDER TO itm_bal


*USE item-bal ORDER itm_bal


DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 7
   @ 1, 30 SAY 'SALE RETURN CORRECTION '

IF _DOS
   SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'S.R. No     ' GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

   @ R, 15 SAY MDOC

IF _DOS
   SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This S.R. No Does Not Exists ... Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Change/Correct it. If you still want to  '
      @ 14, 15 SAY 'Change/Correct it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SELECT 1
   MDATE= DATE
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      MLOC_CODE= LOC_CODE

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      @ R, 2 SAY 'Debit / Party Code'

IF _DOS
      SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

      @ R, 23 SAY '  -  -   '

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      @ R, 23 GET MC1 PICTURE '99'
      READ
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
      SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      SELECT 4
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w 
ELSE
SET COLOR TO
ENDIF

      @ R, 33 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    '

IF _DOS
      SET COLOR TO /w 
ELSE
SET COLOR TO
ENDIF

      @ R, 15 GET MLOC_CODE PICTURE '9999'
      READ

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 33 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   DO sr-cor
   SELECT 1
   GOTO TOP
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE DATE WITH MDATE
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE C_C1 WITH MC1
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE C_C2 WITH MC2
   REPLACE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE C_C3 WITH MC3
   USE
   SELECT 2
   USE st-tran ORDER stidx
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   APPEND FROM st-cor FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SET FILTER TO DOC_TYPE=7
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   USE
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'More Data Correction - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
******************



**********************************
PROCEDURE SALR_DEL
**********************************
CLEAR
CLOSE ALL
SET DATE brit
SET DELETED ON
SELECT 2
USE st-tran ORDER stidx
SET FILTER TO DOC_TYPE=7
GOTO TOP
SELECT 3
USE item ORDER item_idx
SELECT 4
USE ldgrcode ORDER ac1
SELECT 5
USE loc ORDER loc_idx
SELECT 6
USE p-order ORDER pord_idx


SELECT 7
USE item-bal ALIAS item_bal EXCLUSIVE
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_bal
SET ORDER TO itm_bal


*USE item-bal ORDER itm_bal


DO WHILE .T.

IF _DOS
   SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

   CLEAR
   R= 3
   MDOC_TYPE= 7
   @ 1, 30 SAY 'SALE RETURN DELETION '

IF _DOS
   SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

   @ 2, 1 TO 2, 79 DOUBLE
   STORE DATE() TO MDATE, MDEADLINE
   STORE 0 TO MVCH_NO1, MVCH_NO2, MSR_NO
   STORE 0 TO MDOC_NO, MC1, MC2, MC3, MLOC_CODE, MORDER_NO, MV_NO
   STORE 0 TO MI1, MI2, MI3, MQTY_ORD, MRATE, MQTY, MRATE, MTOT
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   @ R, 2 SAY 'S.R. No     ' GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MDOC= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

   @ R, 15 SAY MDOC

IF _DOS
   SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This S.R. No Does Not Exists ... Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      @ 23, 10 SAY SPACE(75)
      LOOP
   ENDIF
   IF FLAG='Y'
      CLEAR
      @ 12, 15 SAY 'This Voucher has been posted to Ledger, therefore   '
      @ 13, 15 SAY 'you cannot Change/Correct it. If you still want to  '
      @ 14, 15 SAY 'Change/Correct it, please use UN-POST OPTION first. '
      @ 16, 15 SAY 'Press Any Key to Return ............ '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   SELECT 2
   USE
   SELECT 1
   USE st-cor
   APPEND FROM st-tran FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   SELECT 1
   MDATE= DATE
   @ R, 60 SAY 'Date ' GET MDATE
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      SELECT 1
      MC1= C_C1
      MC2= C_C2
      MC3= C_C3
      MLOC_CODE= LOC_CODE

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      @ R, 2 SAY 'Debit / Party Code'

IF _DOS
      SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

      @ R, 23 SAY '  -  -   '

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      @ R, 23 GET MC1 PICTURE '99'
      READ
      MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))

IF _DOS
      SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

      @ R, 23 SAY MPC1
      @ R, 25 SAY '-'
      @ R, 26 GET MC2 PICTURE '99'
      READ
      MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
      @ R, 26 SAY MPC2
      @ R, 28 SAY '-'
      @ R, 29 GET MC3 PICTURE '999'
      READ
      MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
      @ R, 29 SAY MPC3

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      SELECT 4
      STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
      SEEK MCODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF

IF _DOS
      SET COLOR TO /w 
ELSE
SET COLOR TO
ENDIF

      @ R, 33 SAY TITLE
      MTITLE= TITLE

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      ABC= 1
   ENDDO
   R= R+1
   ABC= 0
   DO WHILE ABC=0
      @ R, 2 SAY 'Location    '

IF _DOS
      SET COLOR TO /w 
ELSE
SET COLOR TO
ENDIF

      @ R, 15 GET MLOC_CODE PICTURE '9999'
      READ

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      SELECT 5
      SEEK MLOC_CODE
      IF .not. FOUND()
         ?? CHR(7)
         @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
         WAIT ''
         @ 23, 10 SAY SPACE(50)
         LOOP
      ENDIF
      @ R, 33 SAY LOC
      MLOC= LOC
      ABC= 1
   ENDDO
   R= R+1
   @ R, 1 TO R, 79
   R= R+1
   @ R, 2 SAY 'Item Code'
   @ R, 13 SAY 'Item '
   @ R, 30 SAY 'Order #'
   @ R, 43 SAY 'Qty '
   @ R, 53 SAY 'Rate '
   @ R, 65 SAY 'Amount '
   @ R, 76 SAY 'More'
   R= R+1
   @ R, 0 TO R, 79 DOUBLE
   R= R+1
   SELECT 1
   SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   GOTO TOP
   MREC2= RECNO()
   DO sr-dsp
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Confirm Deletion  - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   @ 24, 20 CLEAR TO 24, 78
   SELECT 2
   USE st-tran ORDER stidx
   IF ANS='Y'
      DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      SELECT 1
      USE st-key ORDER st_idx
      SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
      IF FOUND()
         DELETE
      ENDIF
      USE
   ENDIF
   SELECT 2
   SET FILTER TO DOC_TYPE=7
   GOTO TOP
   SELECT 1
   USE st-cor
   DELETE ALL FOR DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
   USE
   ANS= ' '
   DO WHILE .not. ANS$'YN'
      @ 24, 25 SAY 'Any More Deletion - Y/N' GET ANS FUNCTION '!'
      READ
   ENDDO
   IF ANS='N'
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
ENDDO
RETURN
*****************


********************************
PROCEDURE SALR_PRT
**********************************
CLEAR
CLOSE ALL
SELECT 3
USE item ORDER item_idx
SELECT 4
USE ldgrcode ORDER ac1
SELECT 5
USE loc ORDER loc_idx
SELECT 6
USE p-order ORDER pord_idx


SELECT 7
USE item-bal ALIAS item_bal EXCLUSIVE
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_bal
SET ORDER TO itm_bal


*USE item-bal ORDER itm_bal


MUNIT= '    '

IF _DOS
SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

ABC= 0
DO WHILE ABC=0
   CLEAR
   @ 3, 33 SAY 'Sale Return Note '
   @ 4, 33 SAY '---------------- '
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 7
   STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO, MDOC_NO1, CNT
   @ 10, 20 SAY 'From S.R.#  : '
   @ 10, 35 GET MDOC_NO PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO=0
      LOOP
   ENDIF
   MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))

IF _DOS
   SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

   @ 10, 35 SAY MVCH_NO

IF _DOS
   SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

   SELECT 2
   USE st-tran ORDER stidx
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This S.R. No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   @ 12, 20 SAY 'To   S.R.#  : '
   @ 12, 35 GET MDOC_NO1 PICTURE '9999999'
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MDOC_NO1=0
      LOOP
   ENDIF
   MVCH_NO1= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO1))))+LTRIM(STR(MDOC_NO1))

IF _DOS
   SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

   @ 12, 35 SAY MVCH_NO1

IF _DOS
   SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

   SELECT 2
   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)
   IF .not. FOUND()
      @ 23, 1 SAY SPACE(78)
      @ 23, 15 SAY 'This S.R. No Does not Exists  -- Press any key'
      WAIT ''

IF _DOS
      SET COLOR TO 7/1 
ELSE
SET COLOR TO
ENDIF

      @ 23, 10 SAY SPACE(78)
      LOOP
   ENDIF
   SELECT 2
   COPY TO st-rep FOR DOC_NO>=MDOC_NO.and.DOC_NO<=MDOC_NO1.and.DOC_TYPE=MDOC_TYPE
   SELECT 2
   USE
   SELECT 1
   USE st-rep
   GOTO TOP
   IF RECCOUNT()=0
      CLEAR
      @ 12, 12 SAY 'No GRN Exist in Data File  -  Press any key to return to menu .... '
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   ABC= 1
ENDDO
SELECT 1
GOTO TOP
P= ' '
DO WHILE .not. P$'PSQ'
   @ 5, 4 SAY 'Printer/Screen/Quit  -  P/S/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
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
HEAD= .T.
CLEAR
DO WHILE .not. EOF()
   MDOC_NO= DOC_NO
   STORE 0 TO MTOT
   DO WHILE DOC_NO=MDOC_NO
      IF HEAD
         L= 3
         IF P='P'
            DO top_prt IN TBL_PROC.PRG
            @ 3, 30 SAY 'Sale Return Note '
            L= L+1
            @ L, 30 SAY '---------------- '
            L= L+1
         ELSE
            DO screen IN TBL_PROC.PRG
            @ 3, 30 SAY 'Sale Return Note '
            L= L+1
            @ L, 30 SAY '---------------- '
            L= L+1
            CNT= 0
            MREC2= RECNO()
         ENDIF
         MVCH_NO= REPLICATE('0',7-LEN(LTRIM(STR(MDOC_NO))))+LTRIM(STR(MDOC_NO))
         @ L, 1 SAY 'S.R. No       : '
         @ L, 18 SAY MVCH_NO
         @ L, 65 SAY 'Date : '+DTOC(DATE)
         L= L+1
         MC1= C_C1
         MC2= C_C2
         MC3= C_C3
         MLOC_CODE= LOC_CODE
         @ L, 1 SAY 'Debit/Party : '
         MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
         @ L, 18 SAY MPC1
         @ L, 20 SAY '-'
         MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
         @ L, 21 SAY MPC2
         @ L, 23 SAY '-'
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ L, 24 SAY MPC3
         STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
         SELECT 4
         SEEK MCODE
         IF FOUND()
            @ L, 30 SAY TITLE
         ENDIF
         L= L+1
         SELECT 1
         @ L, 1 SAY 'Location     : '
         MLOC_CODE= REPLICATE('0',4-LEN(LTRIM(STR(LOC_CODE))))+LTRIM(STR(LOC_CODE))
         @ L, 18 SAY MLOC_CODE
         SELECT 5
         SEEK MLOC_CODE
         IF FOUND()
            @ L, 30 SAY LOC
         ENDIF
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         @ L, 1 SAY 'Item Code'
         @ L, 12 SAY 'Item '
         @ L, 31 SAY 'Unit'
         @ L, 37 SAY 'Order #'
         @ L, 51 SAY 'Qty '
         @ L, 59 SAY 'Rate '
         @ L, 74 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',79)
         L= L+1
         HEAD= .F.
      ENDIF
      SELECT 1
      IF DOC_NO<>MDOC_NO
         EXIT
      ENDIF
      MP1= REPLICATE('0',2-LEN(LTRIM(STR(I1))))+LTRIM(STR(I1))
      @ L, 1 SAY MP1
      @ L, 3 SAY '-'
      MP2= REPLICATE('0',2-LEN(LTRIM(STR(I2))))+LTRIM(STR(I2))
      @ L, 4 SAY MP2
      @ L, 6 SAY '-'
      MP3= REPLICATE('0',4-LEN(LTRIM(STR(I3))))+LTRIM(STR(I3))
      @ L, 7 SAY MP3
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      SELECT 3
      SEEK MI_CODE
      IF FOUND()
         MUNIT= UNIT
         @ L, 12 SAY ITEM
      ENDIF
      SELECT 1
      MQTY= T_QTY
      MRATE= PSR_UPRICE
      MAMT= MQTY*MRATE
      @ L, 33 SAY MUNIT
      @ L, 37 SAY ORDER_NO
      @ L, 41 SAY MQTY PICTURE '9999999.9999'
      @ L, 53 SAY MRATE PICTURE '9999999.9999'
      @ L, 66 SAY MAMT PICTURE '999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      CNT= CNT+1
      IF P='S'.and.L>20
         @ 23, 1 SAY REPLICATE('-',79)
         @ 24, 5 SAY 'For Next Page Press :'
         @ 24, 30 SAY 'For Previous Page :'
         @ 24, 60 SAY 'Quit :'

IF _DOS
         SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

         @ 24, 27 SAY CHR(25)
         @ 24, 50 SAY CHR(24)
         @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)

IF _DOS
         SET COLOR TO 7/1
ELSE
SET COLOR TO
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
            RUN del st-rep.dbf
            CLOSE ALL
            RETURN
         ENDIF
         IF READKEY()=4
            CLEAR
            GOTO MREC2
            SKIP  -CNT
            HEAD= .T.
         ENDIF
         IF READKEY()=5
            CLEAR
            HEAD= .T.
         ENDIF
      ENDIF
      IF P='P'.and.L>56
         HEAD= .T.
      ENDIF
   ENDDO
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   @ L, 54 SAY 'Total : '
   @ L, 65 SAY MTOT PICTURE '999999999.99'
   L= L+1
   @ L, 50 SAY REPLICATE('-',30)
   L= L+1
   IF P='S'
      @ 23, 1 SAY REPLICATE('-',79)
      @ 24, 5 SAY 'For Next Page Press :'
      @ 24, 30 SAY 'For Previous Page :'
      @ 24, 60 SAY 'Quit :'

IF _DOS
      SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

      @ 24, 27 SAY CHR(25)
      @ 24, 50 SAY CHR(24)
      @ 24, 67 SAY CHR(17)+CHR(45)+CHR(45)

IF _DOS
      SET COLOR TO 7/1
ELSE
SET COLOR TO
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
         RUN del st-rep.dbf
         CLOSE ALL
         RETURN
      ENDIF
      IF READKEY()=4
         CLEAR
         GOTO MREC2
         SKIP  -CNT
         HEAD= .T.
      ENDIF
      IF READKEY()=5
         CLEAR
         HEAD= .T.
      ENDIF
   ENDIF
   IF P='P'
      HEAD= .T.
   ENDIF
ENDDO
IF P='P'
   EJECT
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
RUN del st-rep.dbf
CLEAR
CLOSE ALL
RETURN
********************


******************************
PROCEDURE SR_COR
******************************
SELECT 1
GOTO TOP
MREC2= RECNO()
DO sr-dsp
GOTO TOP
MREC2= RECNO()
R= 8
CK= 0
DO WHILE .T.
   R= R+1
   IF R>=23
      @ 9, 0 CLEAR TO 24, 79
      R= 9
      IF .not. EOF()
         MREC2= RECNO()
         DO pur-dsp
         GOTO MREC2
         R= 9
      ENDIF
   ENDIF
   @ 23, 50 SAY 'Press Ctrl Q To Return'
   @ 23, 1 SAY "'D' Delete - 'C' Change -  'N' Next "
   MQES= 'N'
   @ R, 0 GET MQES PICTURE '!' VALID MQES='D'.or.MQES='C'.or.MQES='N'
   READ
   @ 23, 1 CLEAR TO 23, 77
   IF MQES='D'
      SELECT 1
      DELETE
   ENDIF
   IF MQES='N'
      SELECT 1
      IF .not. EOF()
         SKIP
         LOOP
      ENDIF
      IF EOF()
         SELECT 1
         @ 23, 50 SAY 'Press Ctrl Q To Return'
         STORE 0 TO MI1, MI2, MI3, MQTY, MORDER_NO, MRATE, MAMOUNT
         DO WHILE .T.

IF _DOS
            SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

            @ R, 2 SAY '  -  -    '

IF _DOS
            SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

            @ R, 2 GET MI1 PICTURE '99'
            READ

IF _DOS
            SET COLOR TO n/g
ELSE
SET COLOR TO
ENDIF

            MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
            @ R, 2 SAY MP1
            @ R, 4 SAY '-'
            @ R, 5 GET MI2 PICTURE '99'
            READ
            MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
            @ R, 5 SAY MP2
            @ R, 7 SAY '-'
            @ R, 8 GET MI3 PICTURE '9999'
            READ
            IF MI3=0
               LOOP
            ENDIF
            MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
            @ R, 8 SAY MP3

IF _DOS
            SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

            @ 23, 50 CLEAR TO 23, 78
            IF READKEY()=12
               CLEAR
               CLOSE ALL
               RETURN
            ENDIF
            IF MI1=0.and.MI2=0.and.MI3=0
               LOOP
            ENDIF
            SELECT 3
            STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
            SEEK MI_CODE
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(50)
               LOOP
            ENDIF
            @ R, 13 SAY ITEM
            STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
            SELECT 7
            SEEK XYZ
            IF .not. FOUND()
               @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
               WAIT ''
               @ 21, 1 SAY SPACE(79)
               LOOP
            ENDIF
            MT= 0
            DO WHILE MT=0
               @ R, 30 GET MORDER_NO PICTURE '99999'
               READ
               IF MORDER_NO=0
                  EXIT
               ENDIF
               MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
               SET COLOR TO /g
ELSE
SET COLOR TO
ENDIF

               @ R, 30 SAY MORD

IF _DOS
               SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

               SELECT 6
               STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
               SEEK ORD
               IF .not. FOUND()
                  ?? CHR(7)
                  @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
                  WAIT ''
                  @ 23, 10 SAY SPACE(58)
                  LOOP
               ENDIF
               MT= 1
            ENDDO
            DO WHILE .T.
               @ R, 36 GET MQTY PICTURE '9999999.9999'
               READ
               IF MQTY=0
                  LOOP
               ENDIF
               EXIT
            ENDDO
            @ R, 49 GET MRATE PICTURE '99999999.9999'
            READ
            MAMOUNT= 0
            MAMOUNT= MQTY*MRATE
            @ R, 63 GET MAMOUNT PICTURE '9999999999.99'
            READ
            EXIT
         ENDDO
         SELECT 1
         APPEND BLANK
         IF .not. RLOCK()
            rlock()
         ENDIF
         REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
         REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
         REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
         REPLACE PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
         UNLOCK
      ENDIF
   ENDIF
   IF MQES='C'
      DO WHILE .T.
         SELECT 1
         MI1= I1
         MI2= I2
         MI3= I3
         MQTY= T_QTY
         MRATE= PSR_UPRICE
         MAMOUNT= PSR_TPRICE

IF _DOS
         SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

         @ R, 2 SAY '  -  -    '

IF _DOS
         SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

         @ R, 2 GET MI1 PICTURE '99'
         READ

IF _DOS
         SET COLOR TO n/g
ELSE
SET COLOR TO
ENDIF

         MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
         @ R, 2 SAY MP1
         @ R, 4 SAY '-'
         @ R, 5 GET MI2 PICTURE '99'
         READ
         MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
         @ R, 5 SAY MP2
         @ R, 7 SAY '-'
         @ R, 8 GET MI3 PICTURE '9999'
         READ
         IF MI3=0
            LOOP
         ENDIF
         MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
         @ R, 8 SAY MP3

IF _DOS
         SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

         @ 23, 50 CLEAR TO 23, 78
         IF READKEY()=12
            CLEAR
            CLOSE ALL
            RETURN
         ENDIF
         IF MI1=0.and.MI2=0.and.MI3=0
            LOOP
         ENDIF
         SELECT 3
         STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
         SEEK MI_CODE
         IF .not. FOUND()
            ?? CHR(7)
            @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
            WAIT ''
            @ 23, 10 SAY SPACE(50)
            LOOP
         ENDIF
         @ R, 13 SAY ITEM
         STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
         SELECT 7
         SEEK XYZ
         IF .not. FOUND()
            @ 21, 1 SAY 'Opening Balance For This Item At This Location Does Not Exists . Please Re-try'
            WAIT ''
            @ 21, 1 SAY SPACE(79)
            LOOP
         ENDIF
         MT= 0
         DO WHILE MT=0
            @ R, 30 GET MORDER_NO PICTURE '99999'
            READ
            IF MORDER_NO=0
               EXIT
            ENDIF
            MORD= REPLICATE('0',5-LEN(LTRIM(STR(MORDER_NO))))+LTRIM(STR(MORDER_NO))

IF _DOS
            SET COLOR TO /g
ELSE
SET COLOR TO
ENDIF

            @ R, 30 SAY MORD

IF _DOS
            SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

            SELECT 6
            STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
            SEEK ORD
            IF .not. FOUND()
               ?? CHR(7)
               @ 23, 15 SAY 'This Order No Does Not Exist ... Please Re-try'
               WAIT ''
               @ 23, 10 SAY SPACE(58)
               LOOP
            ENDIF
            MT= 1
         ENDDO
         DO WHILE .T.
            @ R, 36 GET MQTY PICTURE '9999999.9999'
            READ
            IF MQTY=0
               LOOP
            ENDIF
            EXIT
         ENDDO
         @ R, 49 GET MRATE PICTURE '99999999.9999'
         READ
         MAMOUNT= 0
         MAMOUNT= MQTY*MRATE
         @ R, 63 GET MAMOUNT PICTURE '9999999999.99'
         READ
         EXIT
      ENDDO
      SELECT 1
      IF .not. RLOCK()
         rlock()
      ENDIF
      REPLACE DOC_NO WITH MDOC_NO, DOC_TYPE WITH MDOC_TYPE, ORDER_NO WITH MORDER_NO, DATE WITH MDATE
      REPLACE C_C1 WITH MC1, C_C2 WITH MC2, C_C3 WITH MC3, T_QTY WITH MQTY, LOC_CODE WITH MLOC_CODE
      REPLACE I1 WITH MI1, I2 WITH MI2, I3 WITH MI3
      REPLACE PSR_UPRICE WITH MRATE, PSR_TPRICE WITH MAMOUNT
      UNLOCK
   ENDIF
   SELECT 1
   MANS= ' '
   DO WHILE .not. MANS$'YN'
      @ R, 79 GET MANS PICTURE '!'
      READ
   ENDDO
   IF MANS='Y'
      IF R<22
         SELECT 1
         IF .not. EOF()
            SKIP
         ENDIF
         SELECT 1
      ELSE
         @ 9, 1 CLEAR TO 23, 79
         IF .not. EOF()
            SKIP
         ENDIF
      ENDIF
   ENDIF
   IF MANS='N'
      SELECT 1
      GOTO TOP
      SET FILTER TO DOC_NO=MDOC_NO.and.DOC_TYPE=MDOC_TYPE
      GOTO TOP
      MSR= 0
      DO WHILE .not. EOF()
         MSR= MSR+1
         REPLACE SR_NO WITH MSR
         SKIP
      ENDDO
      RETURN
   ENDIF
ENDDO
RETURN
*********************


****************************
PROCEDURE SR_DSP
****************************
R= 9
@ R, 0 CLEAR TO 24, 79
SELECT 1
GOTO MREC2
DO WHILE .not. EOF()
   SELECT 1
   MI1= I1
   MI2= I2
   MI3= I3
   MORDER_NO= ORDER_NO
   MQTY= T_QTY
   MRATE= PSR_UPRICE
   MAMOUNT= PSR_TPRICE

IF _DOS
   SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

   @ R, 2 SAY '  -  -    '
   MP1= REPLICATE('0',2-LEN(LTRIM(STR(MI1))))+LTRIM(STR(MI1))
   @ R, 2 SAY MP1
   @ R, 4 SAY '-'
   @ R, 5 SAY MI2 PICTURE '99'
   MP2= REPLICATE('0',2-LEN(LTRIM(STR(MI2))))+LTRIM(STR(MI2))
   @ R, 5 SAY MP2
   @ R, 7 SAY '-'
   @ R, 8 SAY MI3 PICTURE '9999'
   MP3= REPLICATE('0',4-LEN(LTRIM(STR(MI3))))+LTRIM(STR(MI3))
   @ R, 8 SAY MP3

IF _DOS
   SET COLOR TO 
ELSE
SET COLOR TO
ENDIF

   SELECT 3
   STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
   SEEK MI_CODE
   IF .not. FOUND()
      ?? CHR(7)
      @ 23, 15 SAY 'This Code Does Not Exist ... Please Re-try'
      WAIT ''
      @ 23, 10 SAY SPACE(50)
      LOOP
   ENDIF
   @ R, 13 SAY ITEM
   SELECT 1

IF _DOS
   SET COLOR TO /w
ELSE
SET COLOR TO
ENDIF

   @ R, 30 SAY MORDER_NO PICTURE '99999'
   @ R, 36 SAY MQTY PICTURE '9999999.9999'
   @ R, 49 SAY MRATE PICTURE '99999999.9999'
   @ R, 63 SAY MAMOUNT PICTURE '9999999999.99'

IF _DOS
   SET COLOR TO 
ELSE
SET COLOR TO
ENDIF


   R= R+1
   IF R>=23
      RETURN
   ENDIF
   SKIP
ENDDO
RETURN
**************

***************************
PROCEDURE SALR_VCH
***************************
CLEAR
CLOSE ALL
SELECT 1
USE st-tran ORDER stidx
SELECT 2
USE item ORDER item_idx
SELECT 3
USE ldgrcode ORDER ac1
STORE 0 TO MTOT, MAMT, MQTY, MRATE, MC1, MC2, MC3, MORDER_NO, MDOC_NO
STORE DATE() TO MFDAT, MTDAT
ABC= 0
DO WHILE ABC=0
   @ 3, 25 SAY 'VERIFICATION VOUCHER  REPORT'
   @ 4, 25 SAY '----------------------------'
   @ 23, 50 SAY 'Press Ctrl Q To Quit '
   MDOC_TYPE= 1
   @ 10, 30 SAY 'From Date : ' GET MFDAT
   @ 12, 30 SAY 'To   Date : ' GET MTDAT
   READ
   IF READKEY()=12
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   @ 23, 45 CLEAR TO 23, 78
   IF MTDAT<MFDAT
      @ 23, 10 SAY 'To Date should be greater than From Date'
      WAIT ''
      @ 23, 10 SAY '                                        '
      LOOP
   ENDIF
   SELECT 1
   SET FILTER TO DATE>=MFDAT.and.DATE<=MTDAT.and.DOC_TYPE=7
   IF RECCOUNT()=0
      CLEAR
      @ 23, 10 SAY 'No Transaction Exist in These Two Dates -Press any key to return to menu ....'
      WAIT ''
      CLEAR
      CLOSE ALL
      RETURN
   ENDIF
   CLEAR
   ABC= 1
ENDDO
SELECT 1
GOTO TOP
P= ' '
DO WHILE .not. P$'PQ'
   @ 5, 4 SAY 'Printer/Quit  -  P/Q  ' GET P FUNCTION '!'
   READ
ENDDO
IF P='Q'
   CLOSE ALL
   CLEAR
   SET CONFIRM ON
   RETURN
ENDIF
IF P='P'
   CLEAR
   @ 11, 15 SAY 'Please wait ... Printing in progress '
   SET PRINTER ON
   SET DEVICE TO PRINTER
ENDIF
L= 3
HEADING= .T.
CNT= 0
PG= 0
DO WHILE .not. EOF()
   SELECT 1
   IF HEADING
      DO top_prt IN TBL_PROC.PRG
      @ 3, 1 SAY CHR(18)
      @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
      L= L+1
      @ L, 27 SAY '---------------------------'
      L= L+1
      PG= PG+1
      @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
      L= L+1
      @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
      @ L, 67 SAY 'Page : '+STR(PG,3)
      L= L+1
      @ L, 1 SAY CHR(15)
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      @ L, 3 SAY 'Date'
      @ L, 14 SAY 'S.R No'
      @ L, 26 SAY 'Party'
      @ L, 57 SAY 'Item'
      @ L, 88 SAY 'Order #'
      @ L, 103 SAY 'Qty'
      @ L, 113 SAY 'Rate'
      @ L, 126 SAY 'Amount'
      L= L+1
      @ L, 1 SAY REPLICATE('-',132)
      L= L+1
      HEADING= .F.
   ENDIF
   SELECT 1
   MDOC_NO= DOC_NO
   DO WHILE DOC_NO=MDOC_NO
      IF HEADING
         DO top_prt IN TBL_PROC.PRG
         @ 3, 1 SAY CHR(18)
         @ 3, 28 SAY 'VERIFICATION VOUCHER REPORT'
         L= L+1
         @ L, 27 SAY '---------------------------'
         L= L+1
         PG= PG+1
         @ L, 1 SAY 'From Date : '+DTOC(MFDAT)
         L= L+1
         @ L, 1 SAY 'To   Date : '+DTOC(MTDAT)
         @ L, 67 SAY 'Page : '+STR(PG,3)
         L= L+1
         @ L, 1 SAY CHR(15)
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         @ L, 3 SAY 'Date'
         @ L, 14 SAY 'S.R No'
         @ L, 26 SAY 'Party'
         @ L, 57 SAY 'Item'
         @ L, 88 SAY 'Order #'
         @ L, 103 SAY 'Qty'
         @ L, 113 SAY 'Rate'
         @ L, 126 SAY 'Amount'
         L= L+1
         @ L, 1 SAY REPLICATE('-',132)
         L= L+1
         HEADING= .F.
      ENDIF
      SELECT 1
      @ L, 1 SAY DATE
      MDOCNO= REPLICATE('0',5-LEN(LTRIM(STR(DOC_NO))))+LTRIM(STR(DOC_NO))
      @ L, 13 SAY 'SR-'
      @ L, 16 SAY MDOCNO
      MC_C1= C_C1
      MC_C2= C_C2
      MC_C3= C_C3
      STORE STR(I1,2)+STR(I2,2)+STR(I3,4) TO MI_CODE
      STORE STR(MC_C1,2)+STR(MC_C2,2)+STR(MC_C3,3) TO MP_CODE
      SELECT 3
      SEEK MP_CODE
      IF FOUND()
         @ L, 24 SAY TITLE
      ENDIF
      SELECT 2
      SEEK MI_CODE
      IF FOUND()
         @ L, 57 SAY ITEM
      ENDIF
      SELECT 1
      MQTY= T_QTY
      MRATE= PSR_UPRICE
      MAMT= MQTY*MRATE
      @ L, 88 SAY ORDER_NO
      @ L, 91 SAY MQTY PICTURE '9999999.9999'
      @ L, 104 SAY MRATE PICTURE '99999999.9999'
      @ L, 117 SAY MAMT PICTURE '9999999999.99'
      MTOT= MTOT+MAMT
      L= L+1
      SKIP
      IF P='P'.and.L>56
         @ 1, 1 SAY CHR(18)
         L= 3
         HEADING= .T.
      ENDIF
   ENDDO
   L= L+1
ENDDO
@ L, 102 SAY REPLICATE('-',30)
L= L+1
@ L, 105 SAY 'Total : '
@ L, 117 SAY MTOT PICTURE '9999999999.99'
L= L+1
@ L, 102 SAY REPLICATE('-',30)
L= L+1
IF P='P'
   EJECT
   @ 1, 1 SAY CHR(18)
   SET PRINTER OFF
   SET DEVICE TO SCREEN
ENDIF
CLEAR
CLOSE ALL
RETURN
********************
