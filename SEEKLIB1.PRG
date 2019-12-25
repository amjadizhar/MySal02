* PROGRAM: SEEKLIB1.PRG
* FOR ACCOUNTING & INVENTORY MODULES.
* WRITTEN BY AMJAD IZHAR
* WRITTEN ON: 01-07-2006
* THIS REVISION : Sun. 06-08-2006 4:52 pm
*************************************************



************************
PROCEDURE SEEKSTKEY1
************************
     
USE
USE st-key ALIAS st_key EXCLUSIVE
INDEX ON STR(doc_no,7)+STR(doc_type,1) TAG st_idx
SET ORDER TO st_idx
FLUSH
      
      
SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)

      
RETURN
*********************


************************
PROCEDURE SEEKTRANK1
************************


USE
USE tran-key ALIAS key EXCLUSIVE
INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG VchNo
INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG key_idx
INDEX ON STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG key
SET ORDER TO key

SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)

RETURN
********************

**************************
PROCEDURE SEEKMCODE1
**************************

USE
USE ldgrcode ALIAS ldgrcode EXCLUSIVE
INDEX ON STR(c1,2)+STR(c2,2)+STR(c3,3) TAG ac1
SET ORDER TO ac1
FLUSH

STORE STR(MC1,2)+STR(MC2,2)+STR(MC3,3) TO MCODE
SEEK MCODE



RETURN
************************

************************
PROCEDURE SEEKLOC1
************************


USE
USE loc ALIAS loc EXCLUSIVE
INDEX ON loc_code TAG loc_idx
INDEX ON loc_code TAG loc
SET ORDER TO loc
FLUSH

SEEK MLOC_CODE


RETURN
**************


**********************
PROCEDURE SEEKCITY
**********************

USE
USE city ALIAS city EXCLUSIVE
INDEX ON CITY_CODE TAG city_idx
SET ORDER TO city_idx
SEEK MCITY_CODE

RETURN
*****************

**********************
PROCEDURE SEEKICODE
***********************

USE
USE item ALIAS item EXCLUSIVE
INDEX ON STR(i1,2)+STR(i2,2)+STR(i3,4) TAG item_idx
INDEX ON STR(i1,2)+STR(i2,2)+STR(i3,4) TAG item

SET ORDER TO item

STORE STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO MI_CODE
SEEK MI_CODE

RETURN
******************************

***************************
PROCEDURE SEEKCC1CDE
***************************

USE
USE ldgrcode ALIAS ldgrcode EXCLUSIVE
INDEX ON STR(c1,2)+STR(c2,2)+STR(c3,3) TAG ac1
SET ORDER TO ac1

SEEK STR(MCC1,2)+STR(MCC2,2)+STR(MCC3,3)



RETURN
*****************



************************
PROCEDURE SEEKLOCICD
************************

USE
USE item-bal ALIAS item_bal EXCLUSIVE
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item
INDEX ON STR(I1,2)+STR(I2,2)+STR(I3,4) TAG item_bal
INDEX ON STR(loc_code,4)+STR(i1,2)+STR(i2,2)+STR(I3,4) TAG loc
SET ORDER TO LOC


STORE STR(MLOC_CODE,4)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO XYZ
SEEK XYZ



RETURN
******************



*********************
PROCEDURE SEEKORD
***********************

USE
USE s-order ALIAS s_order EXCLUSIVE
INDEX ON STR(ORDER_NO,5)+STR(C1,2)+STR(C2,2)+STR(C3,3)+STR(I1,2)+STR(I2,2)+STR(I3,4) TAG sord_idx

SET ORDER TO sord_idx

STORE STR(MORDER_NO,5)+STR(MC1,2)+STR(MC2,2)+STR(MC3,3)+STR(MI1,2)+STR(MI2,2)+STR(MI3,4) TO ORD
SEEK ORD



RETURN
**************************



***************************
PROCEDURE SEEKDC1CDE
***************************

USE
USE ldgrcode ALIAS ldgrcode EXCLUSIVE
INDEX ON STR(c1,2)+STR(c2,2)+STR(c3,3) TAG ac1
SET ORDER TO ac1

SEEK STR(MDC1,2)+STR(MDC2,2)+STR(MDC3,3)



RETURN
*****************


***************************
PROCEDURE SEEKSTTRAN
***************************

USE
USE st-tran ALIAS st_tran EXCLUSIVE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1) TAG stidx
SET ORDER TO STIDX


   SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)



RETURN
****************


*****************************
PROCEDURE SEEKSTTRN2
******************************

USE
USE st-tran ALIAS st_tran EXCLUSIVE
INDEX ON STR(DOC_NO,7)+STR(DOC_TYPE,1)+STR(VCH_TYPE,1)+STR(VCH_NO1,2)+STR(VCH_NO2,5) TAG stidx1
SET ORDER TO STIDX1


SEEK STR(MDOC_NO,7)+STR(MDOC_TYPE,1)+STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)



RETURN
**********************


****************************
PROCEDURE SEEKTRANV1
****************************

USE
USE tran ALIAS tran EXCLUSIVE
INDEX ON STR(vch_type,1)+STR(vch_no1,2)+STR(vch_no2,5) TAG tranent
SET ORDER TO tranent
FLUSH
   
 
 SEEK STR(MVCH_TYPE,1)+STR(MVCH_NO1,2)+STR(MVCH_NO2,5)

RETURN
****************************