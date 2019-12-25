
* PROGRAM SEEDIF.PRG 
* To See Is there any Difference in Opening balance In relation to last year



*---------------------------------------------------------------------------


SELECT 1
USE F:\ACCOUNT\AS01\ldgrcode.dbf ALIAS ldgr00  && First Year


IF NOT FILE("F:\ACCOUNT\AS01\ldgrcode.cdx")
INDEX ON str(c1)+str(c2)+str(c3) TAG acc ADDITIVE
ENDIF

SET INDEX TO F:\ACCOUNT\AS01\ldgrcode.cdx
SET ORDER TO acc


SELECT 2
USE F:\OLDACC\AS\AS00\ldgrcode.dbf ALIAS ldgr99 && Previous Year
IF NOT FILE("F:\OLDACC\AS\AS00\ldgrcode.cdx")
INDEX ON str(c1)+str(c2)+str(c3) TAG acc ADDITIVE
ENDIF
SET INDEX TO F:\OLDACC\AS\AS00\ldgrcode.cdx
SET ORDER TO acc



SELECT 3
USE F:\ACCOUNT\AS01\master.dbf ALIAS mast00  && First Year Master file
IF NOT FILE("F:\ACCOUNT\AS01\master.cdx")
INDEX ON str(c1)+str(c2)+str(c3) TAG acc ADDITIVE
ENDIF
SET INDEX TO F:\ACCOUNT\AS01\master.cdx
SET ORDER TO acc



SELECT 1
SET RELATION TO str(c1)+str(c2)+str(c3) INTO ldgr99 ADDITIVE  &&Previous Year
SET RELATION TO str(c1)+str(c2)+str(c3) INTO mast00 ADDITIVE  && First Year Master



BROW FIELDS;
	c1,c2,c3,title,open_bal :H="opb_00",open_stat :H="ops_00",;
	ldgr99.ldgr_bal :H="lb_99",ldgr99.status :H="Ls_99",;
	mast00.bl_amount :h="mbl_00",mast00.status :h="mst_00";
FOR ldgr99.ldgr_bal <> open_bal AND c1 < 60




CLEAR 
PUBLIC Yn
Yn = SPACE(1)



@ 5,10 SAY "Do You Want To Replace Opening Balances with Last Year"
@ 6,10 SAY "Closing Balances ? " GET Yn PICT "@!" VALID INLIST(Yn,'Y','N')
READ



DO CASE
	CASE Yn =  "Y"
		LOCATE FOR sr_no > 1
		IF FOUND()
			CLEAR
			@ 6,5 SAY "Your Data is Posted !!!"
			WAIT WINDOW "Please Unpost All Data First!"
			Yn = "N"
		ENDIF
ENDCASE




DO CASE
	CASE Yn = "Y"
		DO ReplBal
	CASE Yn = "N"
		CLEAR
ENDCASE

SELE 1
Goto Top

PROCEDURE ReplBal

REPLACE ALL open_bal WITH ldgr99.ldgr_bal;
	ADDITIVE open_stat WITH ldgr99.status;
	ADDITIVE ldgr_bal WITH ldgr99.ldgr_bal;
	ADDITIVE status WITH ldgr99.status;
	ADDITIVE mast00.bl_amount WITH ldgr99.ldgr_bal;
	ADDITIVE mast00.status WITH ldgr99.status;
FOR C1 < 60

*FOR ldgr00.ldgr_bal <> open_bal AND c1 < 60

BROW FIELDS;
	c1,c2,c3,open_bal,open_stat,;
	ldgr_bal,status,;
	ldgr99.ldgr_bal,ldgr99.status,;
	mast00.bl_amount,mast00.status;
FOR c1 < 60

RETURN
