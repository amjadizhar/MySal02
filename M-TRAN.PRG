*-------------------------------------------*
*Program to Modification The Transaction	*
*Author		:	Muhammad Usman				*
*Date		:	20-01-2000					*
*Prog		:	M-Tran						*
*Last Modify:	20-01-2000					*
*-------------------------------------------*
* Use Files

Sele 1
Use Tran Alia Tran Shared

Sele 2
Use LdgrCod Alia LdgrCod Shared

Sele 3
Use ITran Alia ITran Shared

Sele 4
Use DType Alia DType Shared

*-----------------------------------
* Window Defination
	DEFINE WINDOW MTran ;
		FROM INT((SROW()-25)/1),INT((SCOL()-80)/1) ;
		TO INT((SROW()-25)/1)+24, INT((SCOL()-80)/1)+79 ;
		TITLE " MODIFICATION DAILY TRANSACTION " ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		COLOR SCHEME 1
		
	ACTIVATE WINDOW MTran SAME
*-----------------------------------
*Veriable Initialization
MDate=Ctod("  /  /    ")			&& Invoice Date
MC1=0								&& Party Code 1
MC2=0								&& Party Code 2
MC3=0								&& Party Code 3
MDisc=Spac(30)						&& Discription
MAmount=0							&& Total Amount
MDoc_No=Spac(6)						&& Document Number
MDoc_Type=0							&& Document Type
MCR_DR=Spac(1)						&& Credit / Debit
MLdgr_Bal=0							&& Leadger Balance
MStatus=Spac(2)						&& Status of Leadger Balance
MLine=3								&& Line Number
MAns=Spac(1)						&& Answer
*-----------------------------------
Do DspHedPro
Do While .T.	
	Do While .T.
		@ 22,01 To 22,78 Clear
		@ 22,22 SAY "Press F1 to see Document Type Code List" Color GR+
		ON KEY LABEL F1 DO ShwDtyPro In Proc1
		@ MLine,13 Get MDoc_Type Pict"9"
		Read
		If Read()=12 .Or. Read()=268				&& Escape Key
			Close Databases
			Rele Wind MTran
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
		@ MLine,15 Get MDoc_No Pict"@!" Valid MDoc_No#Spac(6) Error 'Document Number Can Not Be Blank'
		Read
		Sele Tran
		Set Order To Type_No
		Seek Str(MDoc_Type,1)+MDoc_No
		If .Not. Found()
			Wait Wind 'Document Number Not Exist'
			MDoc_Type=0							&& Document Type
			MDoc_No=Spac(6)						&& Document Number
			Loop
		Endif				
		Sele ITran
		Set Order To Type_No
		Seek Str(MDoc_Type,1)+MDoc_No
		If Found()
			Wait Wind 'Document Number Exist In Sale File'
			MDoc_Type=0							&& Document Type
			MDoc_No=Spac(6)						&& Document Number
			Loop
		Endif				
		Exit
	Enddo
	Sele Tran
	@ MLine,02 Say Date
	@ MLine,22 Say "  -  -   "
	@ MLine,22 Say C1
	@ MLine,23 Say C2
	@ MLine,28 Say C3
	@ MLine,32 Say Disc
	@ MLine,63 Say CR_DR
	@ MLine,67 Say Amount Pict"99999999.99"
	@ MLine,00 Get MAns Pict"@!" Valid MAns="D" .Or. MAns="C" .Or. Mans="N" Error 'D=Delete  C=Change  N=Next'
	Read
	If MAns="N"
		MDoc_Type=0							&& Document Type
		MDoc_No=Spac(6)						&& Document Number
		MLine=MLine+2
		Loop
	Endif
	MC1=C1
	MC2=C2
	MC3=C3
	MCr_Dr=Cr_Dr
	MAmount=Amount
	Sele LdgrCod
	Set Order To C123
	Seek Str(MC1,2)+Str(MC2,2)+Str(MC3,3)
	If Found()
		If Status="DR"
			If MCr_Dr="DR"
				MLdgr_Bal=Ldgr_Bal-MAmount
				If MLdgr_Bal>0
					MStatus="DR"
				Else
					MStatus="CR"
				Endif				
			Else
				MLdgr_Bal=Ldgr_Bal+MAmount
				MStatus="DR"
			Endif
		Endif
		If Status="CR"
			If MCr_Dr="CR"
				MLdgr_Bal=Ldgr_Bal-MAmount
				If MLdgr_Bal>0
					MStatus="CR"
				Else
					MStatus="DR"
				Endif				
			Else
				MLdgr_Bal=Ldgr_Bal+MAmount
				MStatus="CR"
			Endif
		Endif
	Endif				
	Replace Ldgr_Bal With ABS(MLdgr_Bal)
	Replace Status With MStatus
	Flush
	If MAns="D"
		Sele Tran
		Dele
		MDoc_No=Spac(6)						&& Document Number
		MLine=MLine+2
		Loop
	Else
		Sele Tran
		MDate=Date
		MC1=C1
		MC2=C2
		MC3=C3
		MDisc=Disc
		MAmount=Amount
		MCR_DR=Cr_Dr
		MLdgr_Bal=0							&& Leadger Balance
		MStatus=Spac(2)						&& Status of Leadger Balance
		Do EdtDatPro
		Do SavDatPro
	Endif
	Sele Tran
	MC1=0								&& Party Code 1
	MC2=0								&& Party Code 2
	MC3=0								&& Party Code 3
	MDisc=Spac(30)						&& Discription
	MAmount=0							&& Total Amount
	MDoc_No=Spac(6)						&& Document Number
	MDoc_Type=0							&& Document Type
	MCR_DR=Spac(1)						&& Credit / Debit
	MAns=Spac(1)						&& Answer
	MLdgr_Bal=0							&& Leadger Balance
	MStatus=Spac(2)						&& Status of Leadger Balance
	MLine=MLine+2
	If MLine>19
		@ 3,0 TO 21,78 Clear
		MLine=3
	Endif
EndDo
*-----------------------------------
Procedure DspHedPro							&& Display Heading Procdure
*-----------------------------------
@ 1,02 SAY "DATE"
@ 1,14 SAY "DOC #"
@ 1,22 SAY "PARTY CODE"
@ 1,33 SAY "DISCRIPTION"
@ 1,61 SAY "C/D"
@ 1,66 SAY "AMOUNT"
*-----------------------------------
Procedure SavDatPro							&& Data Save Procedure
*-----------------------------------
Sele Tran
	Replace Date With MDate
	Replace C1 With MC1
	Replace C2 With MC2
	Replace C3 With MC3
	Replace Doc_No With MDoc_No
	Replace Doc_Type With MDoc_Type
	Replace Cr_Dr With Left(MCr_Dr,1)+"R"
	Replace Amount With MAmount
	Replace Disc With MDisc
	Flush
Sele LdgrCod
If Status="  "
	If MCr_Dr="D"
		MLdgr_Bal=MAmount
		MStatus="DR"
	Else
		MLdgr_Bal=MAmount
		MStatus="CR"
	Endif
Endif
If Status="DR"
	If MCr_Dr="D"
		MLdgr_Bal=Ldgr_Bal+MAmount
		MStatus="DR"
	Else
		MLdgr_Bal=Ldgr_Bal-MAmount
		If MLdgr_Bal>0
			MStatus="DR"
		Else
			MStatus="CR"
		Endif				
	Endif
Endif				
If Status="CR"
	If MCr_Dr="C"
		MLdgr_Bal=Ldgr_Bal+MAmount
		MStatus="CR"
	Else
		MLdgr_Bal=Ldgr_Bal-MAmount
		If MLdgr_Bal>0
			MStatus="CR"
		Else
			MStatus="DR"
		Endif				
	Endif
Endif				
Replace Ldgr_Bal With ABS(MLdgr_Bal)
Replace Status With MStatus
*-----------------------------------
Procedure EdtDatPro							&& Edit Data Procdure
*-----------------------------------
@ MLine,02 GET MDate Pict("99/99/9999")
Read
Do While .T.
	Sele LdgrCod
	Set Order To Title
	@ 22,01 To 22,62 Clear
	@ 22,27 SAY "Press F1 to see Party Code List" Color GR+
	ON KEY LABEL F1 DO ShwLdgPro In Proc1
	@ MLine,22 Say "  -  -   "
	@ MLine,22 Get MC1 Pict"@2K 99" Valid MC1#0 Error 'Ledger Head Code Can Not Be Blank - Press Esc To Exit'
	@ MLine,25 Get MC2 Pict"@2K 99" Valid MC2#0 Error 'Ledger Head Code Can Not Be Blank - Press Esc To Exit'
	@ MLine,28 Get MC3 Pict"@3K 999" Valid MC3#0 Error 'Ledger Head Code Can Not Be Blank - Press Esc To Exit'
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
		Exit
	Endif
Enddo
@ MLine,32 Get MDisc Pict"@!"
@ MLine,63 Get MCR_DR Pict"@!" Valid MCR_DR="C" .Or. MCR_DR="D" Error 'C=Credit Or D=Debit'
@ MLine,67 Get MAmount Pict"99999999.99"
Read
