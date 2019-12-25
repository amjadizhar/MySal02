*-------------------------------------------*
*Program to Report Party Wise Transactions	*
*Author		:	Muhammad Usman				*
*Date		:	25-11-1999					*
*Prog		:	R-Party						*
*Last Modify:	25-11-1999					*
*-------------------------------------------*
* Select Files
Close Databases

Sele 1
Use Stocks Alia Stocks Shared

Sele 2
Use Item Alia Item Shared
Set Order To I123

Sele 3
Use ILdgrcod Alia ILdgrcod Shared
*-----------------------------------
*1.	Purchase
*2.	Purchase Return
*3.	Sale
*4.	Sale Return
*-----------------------------------
*Veriable Initialization
MVch_No=0							&& Voucher Number
MG_Amount=0							&& Net Amount
MT_Qty=0							&& Total Quantity
MGG_Amount=0						&& Net Amount
MGT_Qty=0							&& Total Quantity
MPage=0								&& Page No
MLine=0								&& Line Number
MSDate=Ctod("  /  /    ")			&& Start Transaction Date
MEDate=Ctod("  /  /    ")			&& End Transaction Date
MC1=0								&& First Ledger Code
MC2=0								&& Second Ledger Code
MC3=0								&& Thired Ledger Code
MMC1=0								&& Party Code 1
MMC2=0								&& Party Code 2
MMC3=0								&& Party Code 3
MPS=Spac(1)							&& Printer Or Screen
MCQ=Spac(1)							&& Continue Or Quit
*-----------------------------------
* Window Defination
	DEFINE WINDOW rparty ;
		FROM INT((SROW()-17)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-17)/1)+9,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW rparty SAME
*-----------------------------------
Set Century Off
Clear
Do While .T.
	@ 1,05 Say "Date From:" Color W/N
	@ 3,05 Say "Date To:" Color W/N
	@ 5,05 Say "Party Code:" Color W/N
	@ 1,30 GET MSDate Pict("99/99/9999")
	@ 3,30 GET MEDate Pict("99/99/9999")
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Close Databases
		Rele Wind rparty
		Return
		Exit
	Endif
	Sele Stocks
	Set Order To Date
	Seek MSDate
	If .Not. Found()
		Wait Wind 'Date Not Exist'
		MSDate=Ctod("  /  /    ")			&& Start Transaction Date
		Loop
	Endif		
	@ 7,07 SAY "Press F1 to see Leadger Heads List" Color GR+
	ON KEY LABEL F1 DO ShwLdgPro In Proc1
	@ 5,30 Say "  -  -   "
	@ 5,30 Get MC1 Pict"@2K 99" Valid MC1#0 Error 'Ledger Head Code Can Not Be Blank - Press Esc To Exit'
	@ 5,33 Get MC2 Pict"@2K 99" Valid MC2#0 Error 'Ledger Head Code Can Not Be Blank - Press Esc To Exit'
	@ 5,36 Get MC3 Pict"@3K 999" Valid MC3#0 Error 'Ledger Head Code Can Not Be Blank - Press Esc To Exit'
	Read	
	Sele ILdgrcod
	Set Order To C123
	Seek Str(MC1,2)+Str(MC2,2)+Str(MC3,3)
	If .Not. Found()
		Wait Wind 'Party Code Not Exist'
		MC1=0								&& First Ledger Code
		MC2=0								&& Second Ledger Code
		MC3=0								&& Thired Ledger Code
		Loop
	Else
		Exit
	Endif		
Enddo
Clear
@ 4,05 Say "Report To Printer Or Screen (P/S)" Color W/N
@ 4,40 GET MPS Pict"@!" Valid MPS="P" .Or. MPS="S" Error 'Printer Or Screen (P/S)' Color W/N
Read
Clear
@ 03,13 SAY "Report in Processing"
@ 05,13 SAY "Please Don't Distrub"
Sele Stocks
Set Filter To Date>=MSDate .And. Date<=MEDate .And. Vch_Type=MVch_Type .And. (C1=MC1 .And. C2=MC2 .And. C3=MC3)
Set Order To Vch_No
Goto Top
Set Relation To Str(I1,2)+Str(I2,2)+Str(I3,4) Into Item Additive
If MVch_Type=1
	MTitle="PURCHASE REPORT (PARTY WISE)"
Endif	
If MVch_Type=2
	MTitle="PURCHASE RETURN REPORT (PARTY WISE)"
Endif	
If MVch_Type=3
	MTitle="SALE REPORT (PARTY WISE)"
Endif	
If MVch_Type=4
	MTitle="SALE RETURN REPORT (PARTY WISE)"
Endif	
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind rparty
Set Print Off
Set Device To Screen
Close Databases
Return
*-----------------------------------
Procedure RepPrtPro							&& Report Print Procedure
*-----------------------------------
Set Print On
Set Device To Print
MPage=MPage+1
@ 00,00 Say Chr(18)
@ 00,00 Say Chr(14)
@ 00,01 Say MComp
@ 01,00 Say Chr(18)
@ 01,00 Say Chr(14)
@ 01,01 Say MTitle
@ 02,00 Say Chr(18)
@ 02,01 Say "From Date :   "+Dtoc(MSDate)
@ 03,00 Say "To   Date :   "+Dtoc(MEDate)
MMC1=Str(C1,2)
MMC2=Str(C2,2)
MMC3=Str(C3,3)
Do Led1CodPro In Proc1
@ 04,00 Say "Party Code:  "+MMC1+"-"+MMC2+"-"+MMC3+"   "+ILdgrCod.Title
@ 04,68 Say "Page # "+Str(MPage,4)
@ 05,00 Say Repl('-',80)
@ 06,00 Say Chr(15)
@ 06,0 Say "Date        Inv #     Item Code         Description                  Unit          Qty              Rate               Net"
@ 07,0 Say "                                                                                                                      Amount"
@ 08,00 Say Repl('-',137)
MLine=9								&& Line Number
Do While .Not. Eof()
	MVch_No=Vch_No
	Do While Vch_No=MVch_No
		MI1=Str(I1,2)
		MI2=Str(I2,2)
		MI3=Str(I3,4)
		Do ItmCodPro In Proc1
		@ MLine,00 Say Dtoc(Date)+"   "+Str(Vch_No,6)+"    "+MI1+"-"+MI2+"-"+MI3+"    "+Item.Item+"    "+Item.Unit+"    "+Str(T_Qty,12,4)+"   "+Str(G_Amount/T_Qty,14,4)+"   "+Str(G_Amount,17,4)
		MLine=MLine+1
		MT_Qty=MT_Qty+T_Qty
		MG_Amount=MG_Amount+G_Amount
		MGT_Qty=MGT_Qty+T_Qty
		MGG_Amount=MGG_Amount+G_Amount
		Skip
		If MLine>58
			Eject
			MPage=MPage+1
			MLine=9								&& Line Number
			@ 00,00 Say Chr(18)
			@ 00,00 Say Chr(14)
			@ 00,01 Say MComp
			@ 01,00 Say Chr(18)
			@ 01,00 Say Chr(14)
			@ 01,01 Say MTitle
			@ 02,00 Say Chr(18)
			@ 02,01 Say "From Date :   "+Dtoc(MSDate)
			@ 03,00 Say "To   Date :   "+Dtoc(MEDate)
			MMC1=Str(C1,2)
			MMC2=Str(C2,2)
			MMC3=Str(C3,3)
			Do Led1CodPro In Proc1
			@ 04,00 Say "Party Code:  "+MMC1+"-"+MMC2+"-"+MMC3+"   "+ILdgrCod.Title
			@ 04,68 Say "Page # "+Str(MPage,4)
			@ 05,00 Say Repl('-',80)
			@ 06,00 Say Chr(15)
			@ 06,0 Say "Date        Inv #     Item Code         Description                  Unit          Qty              Rate               Net"
			@ 07,0 Say "                                                                                                                      Amount"
			@ 08,00 Say Repl('-',137)
		Endif
	Enddo
	@ MLine,69 Say Repl('-',65)
	MLine=MLine+1
	@ MLine,70 Say "Total"
	@ MLine,79 Say Str(MT_Qty,12,4)
	@ MLine,111 Say Str(MG_Amount,17,4)
	MLine=MLine+1
	@ MLine,69 Say Repl('-',65)
	MLine=MLine+1
	MG_Amount=0							&& Net Amount
	MT_Qty=0							&& Total Quantity
Enddo
@ MLine,64 Say Repl('-',73)
MLine=MLine+1
@ MLine,65 Say "Grand Total"
@ MLine,79 Say Str(MGT_Qty,12,4)
@ MLine,111 Say Str(MGG_Amount,17,4)
MLine=MLine+1
@ MLine,64 Say Repl('-',73)
Eject
Return
*-----------------------------------
Procedure RepScrPro							&& Report Screen Procedure
*-----------------------------------
Do DspScrPro In Proc1
MLine=9								&& Line Number
@ 00,01 Say MComp
@ 01,01 Say MTitle
@ 02,01 Say "From Date :  "+Dtoc(MSDate)
@ 03,01 Say "To   Date :  "+Dtoc(MEDate)
MMC1=Str(C1,2)
MMC2=Str(C2,2)
MMC3=Str(C3,3)
Do Led1CodPro In Proc1
@ 04,01 Say "Party Code:  "+MMC1+"-"+MMC2+"-"+MMC3+"   "+ILdgrCod.Title
@ 05,0 Say Repl('-',78)
@ 06,0 Say "Date     Inv #  Item Code   Description       Qty           Rate       Net"
@ 07,0 Say "                                                                      Amount"
@ 08,0 Say Repl('-',78)
Do While .Not. Eof()
	MVch_No=Vch_No
	Do While Vch_No=MVch_No
		MI1=Str(I1,2)
		MI2=Str(I2,2)
		MI3=Str(I3,4)
		Do ItmCodPro In Proc1
		@ MLine,00 Say Dtoc(Date)+" "+Str(Vch_No,6)+" "+MI1+"-"+MI2+"-"+MI3+" "+Left(Item.Item,10)+"    "+Str(T_Qty,10,2)+" "+Str(G_Amount/T_Qty,12,2)+" "+Str(G_Amount,13,2)
		MLine=MLine+1
		MT_Qty=MT_Qty+T_Qty
		MG_Amount=MG_Amount+G_Amount
		MGT_Qty=MGT_Qty+T_Qty
		MGG_Amount=MGG_Amount+G_Amount
		Skip
		If MLine>18
			@ 21,28 Say "Continue Or Quit C/Q"
			@ 21,50 GET MCQ Pict"@!" Valid MCQ="C" .Or. MCQ="Q" Error 'Continue Or Quit (C/Q)'
			Read
			If MCQ="Q"
				Rele wind scr
				Return
			Endif	
			@ 9,0 To 21,78 Clear
			MLine=9
		Endif
	Enddo
	@ MLine,34 Say Repl('-',44)
	MLine=MLine+1
	@ MLine,35 Say "Total"
	@ MLine,39 Say Str(MT_Qty,12,2)
	@ MLine,61 Say Str(MG_Amount,17,2)
	MLine=MLine+1
	@ MLine,34 Say Repl('-',44)
	MLine=MLine+1
	MG_Amount=0							&& Net Amount
	MT_Qty=0							&& Total Quantity
Enddo
@ MLine,29 Say Repl('-',49)
MLine=MLine+1
@ MLine,30 Say "Grand Total"
@ MLine,39 Say Str(MGT_Qty,12,2)
@ MLine,61 Say Str(MGG_Amount,17,2)
MLine=MLine+1
@ MLine,29 Say Repl('-',49)
@ 21,28 Say "Quit Only (Q)"
@ 21,50 GET MCQ Pict"@!" Valid MCQ="Q" Error 'Quit Only (Q)'
Read
Rele wind scr
Return
