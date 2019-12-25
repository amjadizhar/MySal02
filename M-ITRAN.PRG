*-----------------------------------------------*
*Program to Modifiction The Item Transaction	*
*Author		:	Muhammad Usman					*
*Date		:	28-01-2000						*
*Prog		:	M-ITran							*
*Last Modify:	28-01-2000						*
*-----------------------------------------------*
* Use Files

Sele 1
Use ITran Alia ITran Shared
	Tmp=Sys(3)
	Copy Stru To (Tmp+'.Dbf')

Sele 2
Use Item Alia Item Shared
Set Order To I123

Sele 3
Use Tran Alia Tran Shared

Sele 4
Use LdgrCod Alia LdgrCod Shared

Sele 5
Use DType Alia DType Shared

Sele 6
Use (Tmp) Exclusive
Set Relation To Str(I1,2)+Str(I2,2)+Str(I3,4) Into Item Additive

*-----------------------------------
* Window Defination
	DEFINE WINDOW MITran ;
		FROM INT((SROW()-25)/1),INT((SCOL()-80)/1) ;
		TO INT((SROW()-25)/1)+24, INT((SCOL()-80)/1)+79 ;
		TITLE " MODIFICATION DAILY TRANSACTION " ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		COLOR SCHEME 1
		
	ACTIVATE WINDOW MITran SAME
*-----------------------------------
*Veriable Initialization
MDate=Ctod("  /  /    ")			&& Invoice Date
MC1=0								&& Party Code 1
MC2=0								&& Party Code 2
MC3=0								&& Party Code 3
MSAmount=0							&& Sales Tax Amount
MNAmount=0							&& Net Amount
MAmount=0							&& Total Amount
MDoc_No=Spac(6)						&& Document Number
MDoc_Type=0							&& Document Type
MCR_DR=Spac(1)						&& Credit / Debit
MLdgr_Bal=0							&& Leadger Balance
MStatus=Spac(2)						&& Status of Leadger Balance
MI1=0								&& Item Code 1
MI2=0								&& Item Code 2
MI3=0								&& Item Code 3
MQty=0								&& Item Quantity
MTQty=0								&& Total Quantity
MTAmount=0							&& Total Amount
MTNAmount=0							&& Total Net Amount
MCAmount=0							&& Total Current Amount
MSr_No=1							&& Serial Number
MLine=1								&& Line Number
MAns=Spac(1)						&& Answer
MOpt=0								&& Option
*-----------------------------------
Do While .T.
	Do DspHedPro
	@ 22,01 To 22,78 Clear
	@ 22,22 SAY "Press F1 to see Document Type Code List" Color GR+
	ON KEY LABEL F1 DO ShwDtyPro In Proc1
	@ MLine,57 Get MDoc_Type Pict"9"
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Close Databases
		Rele Wind MITran
		Dele File Tmp+'.Dbf'
		Dele File Tmp+'.Cdx'
		Return
		Exit
	Endif
	Sele DType
	Set Order To Doc_Type
	Seek MDoc_Type
	If .Not. Found()
		Wait Wind 'Data Type Not Exist'
		MDoc_Type=0							&& Document Type
		Loop
	Endif				
	@ MLine,60 Get MDoc_No Pict"@!" Valid MDoc_No#Spac(6) Error 'Document Number Can Not Be Blank'
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		MDoc_No=Spac(6)							&& Document Number
		Loop
	Endif
	Sele ITran
	Set Order To Type_No
	Seek Str(MDoc_Type,1)+MDoc_No
	If .Not. Found()
		Wait Wind 'Document Number Not Exist'
		MDoc_Type=0							&& Document Type
		MDoc_No=Spac(6)						&& Document Number
		Loop
	Endif				
	Do While .T.
		@ 22,01 To 22,78 Clear
		@ 22,24 Get MOpt Func '*H \<Cancel;\<Modify;\<Delete' Size 1,10 Color Scheme 19
		Read
	    If Readkey() = 15 .or. Readkey() = 271			&& Enter Key
			Exit
	    Endif
	Enddo
	Do Case
		Case MOpt = 1
		Optflag = 'C'
		Case MOpt = 2
		Optflag = 'M'
		Case MOpt = 3
		Optflag = 'D'
	Endcase
	If Optflag='C'
		Clear
		MDoc_No=Spac(6)						&& Document Number
		MDoc_Type=0							&& Document Type
		MLine=1								&& Line Number
		MOpt=0								&& Option
		Loop
	Endif
	Sele Tran
	Set Order To Type_No
	Seek Str(MDoc_Type,1)+MDoc_No
	MC1=C1
	MC2=C2
	MC3=C3
	MCAmount=Amount
	If Optflag='D'
		Do CurBalPro
		Sele ITran
		Dele All For Doc_Type=MDoc_Type .And. Doc_No=MDoc_No
		Sele Tran
		Dele All For Doc_Type=MDoc_Type .And. Doc_No=MDoc_No
	Endif
	If Optflag='M'
		Sele 6
		Append From ITran For Doc_Type=MDoc_Type .And. Doc_No=MDoc_No
		Index On Sr_No Tag Sr_No
		Set Order To Sr_No
		Goto Top
		MDate=Date
		MC1=C1
		MC2=C2
		MC3=C3
		@ MLine,10 GET MDate Pict("99/99/9999")
		Read
		MLine=MLine+1
		Do While .T.
			Sele LdgrCod
			Set Order To Title
			@ 22,01 To 22,78 Clear
			@ 22,27 SAY "Press F1 to see Party Code List" Color GR+
			ON KEY LABEL F1 DO ShwLdgPro In Proc1
			@ MLine,20 Say "  -  -   "
			@ MLine,20 Get MC1 Pict"@2K 99" Valid MC1#0 Error 'Ledger Head Code Can Not Be Blank'
			@ MLine,23 Get MC2 Pict"@2K 99" Valid MC2#0 Error 'Ledger Head Code Can Not Be Blank'
			@ MLine,26 Get MC3 Pict"@3K 999" Valid MC3#0 Error 'Ledger Head Code Can Not Be Blank'
			Read
			Set Order To C123
			Seek Str(MC1,2)+Str(MC2,2)+Str(MC3,3)
			If .Not. Found()
				Wait Wind 'Party Code Not Exist'
				MC1=0								&& Party Code 1
				MC2=0								&& Party Code 2
				MC3=0								&& Party Code 3
				Loop
			Else
				@ MLine,35 Say Title
				Exit
			Endif
		Enddo
		Sele 6
		Set Order To Sr_No
		Goto Top				
		@ 22,01 To 22,78 Clear
		MLine=MLine+4
		Do While .Not. Eof()
			MI1=I1
			MI2=I2
			MI3=I3
			MQty=Qty
			MAmount=Amount
			MNAmount=NAmount
			MSr_No=Sr_No
			@ MLine,02 Say "  -  -    "
			@ MLine,02 Say MI1 Pict"@2K 99"
			@ MLine,05 Say MI2 Pict"@2K 99"
			@ MLine,08 Say MI3 Pict"@4K 9999"
			@ MLine,15 Say Item.Item
			@ MLine,48 Say MQty Pict"@4K 9999"
			@ MLine,53 Say MNAmount Pict"999999999.99"
			@ MLine,66 Say MAmount Pict"999999999.99"
			MAns="N"
			@ MLine,00 Get MAns Pict"@!" Valid MAns="N" .Or. MAns="C" .Or. MAns="D" Error 'N=Next  C=Change  D=Delete'
			Read
			If MAns="N"
				Sele 6
				Skip
				MLine=MLine+1
				If MLine>19
					@ 6,0 TO 21,78 Clear
					MLine=6
				Endif
				Loop
			Endif		
			If MAns="C"
				Do EdtDatPro
			Endif
			If MAns="D"
				Dele For Sr_No=MSr_No
			Endif
			Do SavDatPro
			Skip
			MLine=MLine+1
			If MLine>19
				@ 6,0 TO 21,78 Clear
				MLine=6
			Endif
		Enddo
		Do While .T.
			MAns=Spac(1)						&& Answer
			@ MLine,00 Get MAns Pict"@!" Valid MAns="A" Error 'A=Addition Or Escape Key For Exit'
			Read
			If Read()=12 .Or. Read()=268				&& Escape Key
				Exit
			Endif
			If MAns="A"
				MSr_No=MSr_No+1
				MI1=0								&& Item Code 1
				MI2=0								&& Item Code 2
				MI3=0								&& Item Code 3
				MQty=0								&& Item Quantity
				Do EdtDatPro
				Do SavDatPro
			Endif
			MLine=MLine+1
			If MLine>19
				@ 6,0 TO 21,78 Clear
				MLine=6
			Endif
		Enddo
		Sele 6
		Sum Qty To MTQty
		Sum NAmount To MTNAmount
		Sum Amount To MTAmount	
		@ 22,01 To 22,62 Clear
		@ 22,30 Say "Total"
		@ 22,48 Say MTQty Pict"9999"
		@ 22,53 Say MTNAmount Pict"999999999.99"
		@ 22,66 Say MTAmount Pict"999999999.99"
		Wait Wind 'Press Enter To Continue'
		Clear 
		Do CurBalPro
		Do Sav1DatPro
	Endif
	Clear
	Sele 6
	Zap
	MDate=Ctod("  /  /    ")			&& Invoice Date
	MC1=0								&& Party Code 1
	MC2=0								&& Party Code 2
	MC3=0								&& Party Code 3
	MSAmount=0							&& Sales Tax Amount
	MNAmount=0							&& Net Amount
	MAmount=0							&& Total Amount
	MDoc_No=Spac(6)						&& Document Number
	MDoc_Type=0							&& Document Type
	MCR_DR=Spac(1)						&& Credit / Debit
	MLdgr_Bal=0							&& Leadger Balance
	MStatus=Spac(2)						&& Status of Leadger Balance
	MI1=0								&& Item Code 1
	MI2=0								&& Item Code 2
	MI3=0								&& Item Code 3
	MQty=0								&& Item Quantity
	MTQty=0								&& Total Quantity
	MTAmount=0							&& Total Amount
	MTNAmount=0							&& Total Net Amount
	MCAmount=0							&& Total Current Amount
	MSr_No=1							&& Serial Number
	MLine=1								&& Line Number
	MAns=Spac(1)						&& Answer
	MOpt=0								&& Option
Enddo
*-----------------------------------
Procedure DspHedPro							&& Display Heading Procdure
*-----------------------------------
@ 1,02 SAY "DATE"
@ 1,50 SAY "DOC #"
@ 2,02 SAY "PARTY CODE"
@ 3,00 SAY Repl("-",78)
@ 4,02 SAY "ITEM CODE"
@ 4,48 SAY "QTY"
@ 4,53 SAY "AMOUNT"
@ 4,66 SAY "AMOUNT"
@ 5,00 SAY Repl("-",78)
*-----------------------------------
Procedure SavDatPro							&& Data Save Procedure
*-----------------------------------
Sele 6
	If MAns="A"
		Append Blank
		Replace Sr_No With MSr_No
	Endif
	Replace Date With MDate
	Replace C1 With MC1
	Replace C2 With MC2
	Replace C3 With MC3
	Replace Amount With MAmount
	Replace Doc_Type With MDoc_Type
	Replace Doc_No With MDoc_No
	Replace I1 With MI1
	Replace I2 With MI2
	Replace I3 With MI3
	Replace Qty With MQty
	Replace Sr_No With MSr_No	
	Replace NAmount With MNAmount
	Flush
*-----------------------------------
Procedure Sav1DatPro						&& Data 1 Save Procedure
*-----------------------------------
Sele 6
Index On Sr_No Tag Sr_No
Set Order To Sr_No
Sele ITran
	Dele All For Doc_Type=MDoc_Type .And. Doc_No=MDoc_No
	Append From (Tmp)
	Flush
Sele Tran
	Replace Date With MDate
	Replace C1 With MC1
	Replace C2 With MC2
	Replace C3 With MC3
	Replace Amount With MTAmount
	Replace Doc_Type With MDoc_Type
	Replace Doc_No With MDoc_No
	Replace Cr_Dr With "DR"
	Replace Disc With "SALE INVOICE"
	Flush
Sele LdgrCod
	Set Order To C123
	Seek Str(MC1,2)+Str(MC2,2)+Str(MC3,3)
	If Status="  "
		MLdgr_Bal=MTAmount
		MStatus="DR"
	Endif
	If Status="DR"
		MLdgr_Bal=Ldgr_Bal+MTAmount
		MStatus="DR"
	Endif
	If Status="CR"
		MLdgr_Bal=Ldgr_Bal-MTAmount
		If MLdgr_Bal>0
			MStatus="CR"
		Else
			MStatus="DR"
		Endif				
	Endif
Replace Ldgr_Bal With ABS(MLdgr_Bal)
Replace Status With MStatus
Flush
*-----------------------------------
Procedure CurBalPro						&& Current Balance Procedure
*-----------------------------------
Sele LdgrCod
	Set Order To C123
	Seek Str(MC1,2)+Str(MC2,2)+Str(MC3,3)
	If Status="DR"
		MLdgr_Bal=Ldgr_Bal-MCAmount
		If MLdgr_Bal>0
			MStatus="DR"
		Else
			MStatus="CR"
		Endif				
	Endif
	If Status="CR"
		MLdgr_Bal=Ldgr_Bal+MCAmount
		MStatus="CR"
	Endif
Replace Ldgr_Bal With ABS(MLdgr_Bal)
Replace Status With MStatus
Flush
*-----------------------------------
Procedure EdtDatPro							&& Edit Data Procdure
*-----------------------------------
Do While .T.
	Sele Item
	Set Order To Item
	@ 22,01 To 22,62 Clear
	@ 22,26 SAY "Press F1 to see Item Code List" Color GR+
	ON KEY LABEL F1 DO ShwItmPro In Proc1
	@ MLine,02 Say "  -  -    "
	@ MLine,02 Get MI1 Pict"@2K 99" Valid MI1#0 Error 'Item Code Can Not Be Blank'
	@ MLine,05 Get MI2 Pict"@2K 99" Valid MI2#0 Error 'Item Code Can Not Be Blank'
	@ MLine,08 Get MI3 Pict"@4K 9999" Valid MI3#0 Error 'Item Code Can Not Be Blank'
	Read
	Set Order To I123
	Seek Str(MI1,2)+Str(MI2,2)+Str(MI3,4)
	If .Not. Found()
		Wait Wind 'Item Code Not Exist'
		MI1=0								&& Item Code 1
		MI2=0								&& Item Code 2
		MI3=0								&& Item Code 3
		Loop
	Else
		@ MLine,15 Say Item
	Endif
	@ MLine,48 Get MQty Pict"@4K 9999"
	Read
	MSAmount=MQty*Item.Rate
	MNAmount=MQty*Item.Rate
	MAmount=MSAmount+Round((MSAmount*Item.STax/100),2)
	@ MLine,53 Say MNAmount Pict"999999999.99"
	@ MLine,53 Get MNAmount Pict"999999999.99"
	@ MLine,66 Say MAmount Pict"999999999.99"
	@ MLine,66 Get MAmount Pict"999999999.99"
	Read
	Exit
Enddo