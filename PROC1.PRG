*********************************************
* PROGRAM PROC1.PRG
* Accounting Program Procedures
* Designed and Developed by Amjad Izhar
* Revised Again on : 18-01-2001
*******************************************

**********************
PROCEDURE BACH_BAL
**********************

DO WHILE .T.
   SELECT 1
   SKIP  -MSNO+1
   L= L-MSNO+1
   PSNO= MSNO
   MSNO= 1
   MCREDIT= 0
   MDEBIT= 0
   TDR= 0
   TCR= 0
   DO WHILE .T.
      DO WHILE .T.
         @ L, 3 SAY MSNO PICTURE '9'
         SET COLOR TO /W
         @ L, 5 SAY '  -  -   '
         @ L, 5 GET C1 PICTURE '@Z 99'
         READ
         MC1= C1
         IF C1=0
            LOOP
         ELSE
            MPC1= REPLICATE('0',2-LEN(LTRIM(STR(MC1))))+LTRIM(STR(MC1))
            @ L, 5 SAY MPC1
            EXIT
         ENDIF
      ENDDO
      DO WHILE .T.
         @ L, 8 GET C2 PICTURE '@Z 99'
         READ
         MC2= C2
         IF C2=0
            LOOP
         ELSE
            MPC2= REPLICATE('0',2-LEN(LTRIM(STR(MC2))))+LTRIM(STR(MC2))
            @ L, 8 SAY MPC2
            EXIT
         ENDIF
      ENDDO
      DO WHILE .T.
         @ L, 11 GET C3 PICTURE '@Z 999'
         READ
         MC3= C3
         IF C3=0
            LOOP
         ENDIF
         MPC3= REPLICATE('0',3-LEN(LTRIM(STR(MC3))))+LTRIM(STR(MC3))
         @ L, 11 SAY MPC3
         EXIT
      ENDDO
      SET COLOR TO 
      SELECT 2
      LOCATE FOR  B->C1= A->C1.and. B->C2= A->C2.and. B->C3= A->C3
      IF .not. FOUND()
         @ 23, 8 SAY 'This Code Does not Exist  --Press any key'
         WAIT ''
         @ 23, 8 CLEAR TO 23, 63
         LOOP
      ENDIF
      SET COLOR TO /w 
      @ L, 16 SAY  B->TITLE
      SET COLOR TO 7/1
      SELECT 1
      @ 9, 40 SAY 'DESCR'
      @ 9, 47 GET DESCRIP
      READ
      @ L, 47 SAY DR_CR
      Y= DR_CR
      DO WHILE .T.
         SET CONFIRM ON
         @ L, 47 GET Y FUNCTION '!'
         READ
         IF Y='CR'.or.Y='DR'
            EXIT
         ELSE
            LOOP
         ENDIF
      ENDDO
      REPLACE DR_CR WITH Y
      IF Y='DR'
         DO WHILE .T.
            SET COLOR TO /W
            @ L, 64 SAY MCREDIT PICTURE '999999999.99'
            SET COLOR TO 
            @ L, 50 GET AMOUNT PICTURE '999999999.99'
            READ
            IF AMOUNT=0
               LOOP
            ENDIF
            TDR= TDR+AMOUNT
            EXIT
         ENDDO
      ENDIF
      IF Y='CR'
         DO WHILE .T.
            SET COLOR TO /W
            @ L, 50 SAY MDEBIT PICTURE '999999999.99'
            SET COLOR TO 
            @ L, 64 GET AMOUNT PICTURE '999999999.99'
            READ
            IF AMOUNT=0
               LOOP
            ENDIF
            TCR= TCR+AMOUNT
            EXIT
         ENDDO
      ENDIF
      X= ' '
      DO WHILE .not. X$'YN'
         @ L, 77 GET X FUNCTION '!'
         READ
      ENDDO
      IF X='N'
         IF Y='DR'
            TDR= TDR-AMOUNT
         ELSE
            TCR= TCR-AMOUNT
         ENDIF
         LOOP
      ENDIF
      IF MSNO=PSNO
         IF TDR<>TCR
            @ 23, 1 SAY ' VOUCHER IS UN-BALANCED PL. CORRECT'
            @ 23, 42 SAY 'TOTALS:'
            @ 23, 50 SAY TDR PICTURE '999999999.99'
            @ 23, 64 SAY TCR PICTURE '999999999.99'
            WAIT ' '
            @ 23, 1
            IND= 1
            EXIT
         ELSE
            IND= 2
            EXIT
         ENDIF
      ENDIF
      SELECT 1
      SKIP
      L= L+1
      MSNO= MSNO+1
      MCREDIT= 0
      MDEBIT= 0
      Y= '  '
      STORE SPACE(30) TO MDESCRIP
      LOOP
   ENDDO
   IF IND=2
      RETURN
   ELSE
      LOOP
   ENDIF
ENDDO

RETURN
********************




******************
PROCEDURE VEW-BACK
******************

DO WHILE .T.
   SELECT 1
   MC1= C1
   MC2= C2
   MC3= C3
   @ 6, 1 SAY REPLICATE('-',78)
   @ 7, 2 SAY 'SR   ACCT-NO.  T I T L E                    DR/CR     DR-AMT.       CR-AMT.  '
   SET COLOR TO /w
   @ L, 3 SAY SR_NO PICTURE '99'
   @ L, 6 SAY '  -  -   '
   MPC1= REPLICATE('0',2-LEN(LTRIM(STR(C1))))+LTRIM(STR(C1))
   MPC2= REPLICATE('0',2-LEN(LTRIM(STR(C2))))+LTRIM(STR(C2))
   MPC3= REPLICATE('0',3-LEN(LTRIM(STR(C3))))+LTRIM(STR(C3))
   @ L, 6 SAY MPC1
   @ L, 9 SAY MPC2
   @ L, 12 SAY MPC3
   SET COLOR TO 7/1
   SELECT 2
   SEEK STR(MC1,2)+STR(MC2,2)+STR(MC3,3)
   IF FOUND()
      @ L, 17 SAY TITLE
   ELSE
      @ L, 17 SAY ' '
   ENDIF
   SELECT 1
   SET COLOR TO /w
   @ L, 47 SAY DR_CR
   IF DR_CR='DR'
      @ L, 50 SAY AMOUNT PICTURE '999,999,999.99'
      SET COLOR TO 
   ELSE
      @ L, 64 SAY AMOUNT PICTURE '999,999,999.99'
      SET COLOR TO 
   ENDIF
   SET COLOR TO 7/1
   @ 22, 1 SAY VCH_NO1
   @ 22, 45 SAY VCH_NO2
   @ 23, 1 SAY MVCH_NO1
   @ 23, 45 SAY MVCH_NO2
   SKIP
   IF VCH_NO1<>MVCH_NO1.and.VCH_NO2<>MVCH_NO2
      @ 23, 1 SAY VCH_NO1
      @ 23, 45 SAY VCH_NO2
      WAIT
      RETURN
   ENDIF
   IF EOF()
      RETURN
   ENDIF
   L= L+1
   IF L>21
      L= 9
      @ 24, 30 SAY 'Press Any Key To Continue '
      WAIT ''
      @ 12, 2 CLEAR TO 23, 77
      @ 24, 30 SAY SPACE(50)
   ENDIF
ENDDO
CLEAR

RETURN
********************



* 
* 
* PROC1.FXP 
* --------- 
*

**************************
PROCEDURE SHWLDGPRO
**************************

PUSH KEY
DEFINE WINDOW SHWLDG FROM 5, 40 TO 20, 75 FOOTER '<Enter To Select / Esc To Exit>' DOUBLE
ACTIVATE WINDOW SHWLDG
SELECT LDGRCOD
ON KEY LABEL F1 
ON KEY LABEL ENTER DO SlcLdgPro
BROWSE FIELDS C1 :H = '  ', C2 :H = '  ', C3 :H = '   ', TITLE :H = 'Leadger Head Title' NOAPPEND NOEDIT NODELETE IN SHWLDG
DEACTIVATE WINDOW SHWLDG
ON KEY LABEL ENTER 
POP KEY
RETURN


***********************
PROCEDURE SLCLDGPRO
***********************
MC1 = C1
MC2 = C2
MC3 = C3
SHOW GETS
CLEAR GETS
DEACTIVATE WINDOW SHWLDG
ON KEY LABEL ENTER 
RETURN
*************

**************************
PROCEDURE SHWITMPRO
*****************************
PUSH KEY
DEFINE WINDOW SHWITM FROM 5, 40 TO 20, 75 FOOTER '<Enter To Select / Esc To Exit>' DOUBLE
ACTIVATE WINDOW SHWITM
SELECT ITEM
ON KEY LABEL F1 
ON KEY LABEL ENTER DO SlcItmPro
BROWSE FIELDS I1 :H = '  ', I2 :H = '  ', I3 :H = '    ', ITEM :H = 'Items Name' NOAPPEND NOEDIT NODELETE IN SHWITM
DEACTIVATE WINDOW SHWITM
ON KEY LABEL ENTER 
POP KEY
RETURN
**********

**********************
PROCEDURE SLCITMPRO
**********************
MI1 = I1
MI2 = I2
MI3 = I3
SHOW GETS
CLEAR GETS
DEACTIVATE WINDOW SHWITM
ON KEY LABEL ENTER 
RETURN
***************


***************************
PROCEDURE SHW1ITMPRO
***************************
PUSH KEY
DEFINE WINDOW SHW1ITM FROM 5, 40 TO 20, 75 FOOTER '<Enter To Select / Esc To Exit>' DOUBLE
ACTIVATE WINDOW SHW1ITM
SELECT ITEM
ON KEY LABEL F1 
ON KEY LABEL ENTER DO Slc1ItmPro
BROWSE FIELDS I1 :H = '  ', I2 :H = '  ', I3 :H = '    ', ITEM :H = 'Items Name' NOAPPEND NOEDIT NODELETE IN SHW1ITM
DEACTIVATE WINDOW SHW1ITM
ON KEY LABEL ENTER 
POP KEY
RETURN
**************

***************************
PROCEDURE SLC1ITMPRO
****************************
MSI3 = I3
SHOW GETS
CLEAR GETS
DEACTIVATE WINDOW SHW1ITM
ON KEY LABEL ENTER 
RETURN
***********

****************************
PROCEDURE SHWDTYPRO
****************************
PUSH KEY
DEFINE WINDOW SHWDTY FROM 5, 40 TO 20, 75 FOOTER '<Enter To Select / Esc To Exit>' DOUBLE
ACTIVATE WINDOW SHWDTY
SELECT DTYPE
ON KEY LABEL F1 
ON KEY LABEL ENTER DO SlcDtyPro
BROWSE FIELDS DOC_TYPE :H = '  ', TYPE1 :H = '  ', TYPE2 :H = 'Data Type' NOAPPEND NOEDIT NODELETE IN SHWDTY
DEACTIVATE WINDOW SHWDTY
ON KEY LABEL ENTER 
POP KEY
RETURN
****************

*****************************
PROCEDURE SLCDTYPRO
*****************************

MDOC_TYPE = DOC_TYPE
SHOW GETS
CLEAR GETS
DEACTIVATE WINDOW SHWDTY
ON KEY LABEL ENTER 
RETURN
***************

**************************
PROCEDURE LEDCODPRO
***************************
MCODLEN = LEN(LTRIM(RTRIM(MC1)))
MC1 = REPLICATE('0',2-MCODLEN)+LTRIM(RTRIM(MC1))
MCODLEN = LEN(LTRIM(RTRIM(MC2)))
MC2 = REPLICATE('0',2-MCODLEN)+LTRIM(RTRIM(MC2))
MCODLEN = LEN(LTRIM(RTRIM(MC3)))
MC3 = REPLICATE('0',3-MCODLEN)+LTRIM(RTRIM(MC3))
RETURN
****************

**************************
PROCEDURE LED1CODPRO
*****************************
MCODLEN = LEN(LTRIM(RTRIM(MMC1)))
MMC1 = REPLICATE('0',2-MCODLEN)+LTRIM(RTRIM(MMC1))
MCODLEN = LEN(LTRIM(RTRIM(MMC2)))
MMC2 = REPLICATE('0',2-MCODLEN)+LTRIM(RTRIM(MMC2))
MCODLEN = LEN(LTRIM(RTRIM(MMC3)))
MMC3 = REPLICATE('0',3-MCODLEN)+LTRIM(RTRIM(MMC3))
RETURN
***************

**************************
PROCEDURE ITMCODPRO
***************************
MCODLEN = LEN(LTRIM(RTRIM(MI1)))
MI1 = REPLICATE('0',2-MCODLEN)+LTRIM(RTRIM(MI1))
MCODLEN = LEN(LTRIM(RTRIM(MI2)))
MI2 = REPLICATE('0',2-MCODLEN)+LTRIM(RTRIM(MI2))
MCODLEN = LEN(LTRIM(RTRIM(MI3)))
MI3 = REPLICATE('0',4-MCODLEN)+LTRIM(RTRIM(MI3))
RETURN
*****************


**************************
PROCEDURE ITM1CODPRO
**************************
MCODLEN = LEN(LTRIM(RTRIM(MMI1)))
MMI1 = REPLICATE('0',2-MCODLEN)+LTRIM(RTRIM(MMI1))
MCODLEN = LEN(LTRIM(RTRIM(MMI2)))
MMI2 = REPLICATE('0',2-MCODLEN)+LTRIM(RTRIM(MMI2))
MCODLEN = LEN(LTRIM(RTRIM(MMI3)))
MMI3 = REPLICATE('0',4-MCODLEN)+LTRIM(RTRIM(MMI3))
RETURN
*****************

****************************
PROCEDURE DSPSCRPRO
****************************
DEFINE WINDOW SCR FROM INT((SROWS()-23)/2), INT((SCOLS()-80)/2) TO INT((SROWS()-23)/2)+23, INT((SCOLS()-80)/2)+79 NOFLOAT NOCLOSE SHADOW NOMINIMIZE PANEL COLOR N/W 
ACTIVATE WINDOW SAME SCR
RETURN
*****************


**************************
PROCEDURE SHW1LDGPRO
**************************
PUSH KEY
DEFINE WINDOW SHW1LDG FROM 5, 40 TO 20, 75 FOOTER '<Enter To Select / Esc To Exit>' DOUBLE
ACTIVATE WINDOW SHW1LDG
SELECT LDGRCOD
ON KEY LABEL F1 
ON KEY LABEL ENTER DO Slc1LdgPro
BROWSE FIELDS C1 :H = '  ', C2 :H = '  ', C3 :H = '   ', TITLE :H = 'Leadger Head Title' NOAPPEND NOEDIT NODELETE IN SHW1LDG
DEACTIVATE WINDOW SHW1LDG
ON KEY LABEL ENTER 
POP KEY
RETURN
****************

***************************
PROCEDURE SLC1LDGPRO
***************************
MSC3 = C3
SHOW GETS
CLEAR GETS
DEACTIVATE WINDOW SHW1LDG
ON KEY LABEL ENTER 
RETURN
**************

************************
PROCEDURE LDGBALPRO
************************
IF MMB_CR_DR='  '
   IF CR_DR='DR'
      MB_AMOUNT = AMOUNT
      MB_CR_DR = 'DR'
   ELSE
      MB_AMOUNT = AMOUNT
      MB_CR_DR = 'CR'
   ENDIF
ENDIF
IF MMB_CR_DR='DR'
   IF CR_DR='DR'
      MB_AMOUNT = MMB_AMOUNT+AMOUNT
      MB_CR_DR = 'DR'
   ELSE
      MB_AMOUNT = MMB_AMOUNT-AMOUNT
      IF MB_AMOUNT>0
         MB_CR_DR = 'DR'
      ELSE
         MB_CR_DR = 'CR'
      ENDIF
   ENDIF
ENDIF
IF MMB_CR_DR='CR'
   IF CR_DR='CR'
      MB_AMOUNT = MMB_AMOUNT+AMOUNT
      MB_CR_DR = 'CR'
   ELSE
      MB_AMOUNT = MMB_AMOUNT-AMOUNT
      IF MB_AMOUNT>0
         MB_CR_DR = 'CR'
      ELSE
         MB_CR_DR = 'DR'
      ENDIF
   ENDIF
ENDIF
*****************

**************************
PROCEDURE SHWCTYPRO
**************************
PUSH KEY
DEFINE WINDOW SHWCTY FROM 5, 40 TO 20, 75 FOOTER '<Enter To Select / Esc To Exit>' DOUBLE
ACTIVATE WINDOW SHWCTY
SELECT CITY
ON KEY LABEL F1 
ON KEY LABEL ENTER DO SlcCtyPro
BROWSE FIELDS C_CODE :H = '     ', NAME :H = 'City Name' NOAPPEND NOEDIT NODELETE IN SHWCTY
DEACTIVATE WINDOW SHWCTY
ON KEY LABEL ENTER 
POP KEY
RETURN
*****************

***************************
PROCEDURE SLCCTYPRO
****************************
MC_CODE = C_CODE
SHOW GETS
CLEAR GETS
DEACTIVATE WINDOW SHWCTY
ON KEY LABEL ENTER 
RETURN
*****************

