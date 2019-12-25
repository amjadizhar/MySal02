*-------------------------------------------*
*Program to Report Item Wise Transactions	*
*Author		:	Muhammad Usman				*
*Date		:	26-11-1999					*
*Prog		:	R-Item						*
*Last Modify:	26-11-1999					*
*-------------------------------------------*
* Select Files
Close Databases

Sele 1
Use Stocks Alia Stocks Shared

Sele 2
Use Item Alia Item Shared

Sele 3
Use ILdgrcod Alia ILdgrcod Shared
Set Order To C123
*-----------------------------------
*1.	Purchase
*2.	Purchase Return
*3.	Sale
*4.	Sale Return
*-----------------------------------
*Veriable Initialization
MG_Amount=0							&& Net Amount
MT_Qty=0							&& Total Quantity
MPage=0								&& Page No
MLine=0								&& Line Number
MSDate=Ctod("  /  /    ")			&& Start Transaction Date
MEDate=Ctod("  /  /    ")			&& End Transaction Date
MI1=0								&& First Ledger Code
MI2=0								&& Second Ledger Code
MI3=0								&& Thired Ledger Code
MMI1=0								&& Item Code 1
MMI2=0								&& Item Code 2
MMI3=0								&& Item Code 3
MPS=Spac(1)							&& Printer Or Screen
MCQ=Spac(1)							&& Continue Or Quit
*-----------------------------------
* Window Defination
	DEFINE WINDOW ritem ;
		FROM INT((SROW()-17)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-17)/1)+9,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW ritem SAME
*-----------------------------------
Set Century Off
Clear
Do While .T.
	@ 1,05 Say "Date From:" Color W/N
	@ 3,05 Say "Date To:" Color W/N
	@ 5,05 Say "Item Code:" Color W/N
	@ 1,30 GET MSDate Pict("99/99/9999")
	@ 3,30 GET MEDate Pict("99/99/9999")
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Close Databases
		Rele Wind ritem
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
	@ 07,08 SAY "Press F1 to see Item Code List" Color GR+
	ON KEY LABEL F1 DO ShwItmPro In Proc1
	@ 05,30 Say "  -  -    "
	@ 05,30 Get MI1 Pict"@2K 99" Valid MI1#0 Error 'Item Code Can Not Be Blank - Press Esc To Exit'
	@ 05,33 Get MI2 Pict"@2K 99" Valid MI2#0 Error 'Item Code Can Not Be Blank - Press Esc To Exit'
	@ 05,36 Get MI3 Pict"@4K 9999" Valid MI3#0 Error 'Item Code Can Not Be Blank - Press Esc To Exit'
	Read	
	Sele Item
	Set Order To I123
	Seek Str(MI1,2)+Str(MI2,2)+Str(MI3,4)
	If .Not. Found()
		Wait Wind 'Item Code Not Exist'
		MI1=0								&& Item Code 1
		MI2=0								&& Item Code 2
		MI3=0								&& Item Code 3
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
Set Filter To Date>=MSDate .And. Date<=MEDate .And. Vch_Type=MVch_Type .And. (I1=MI1 .And. I2=MI2 .And. I3=MI3)
Set Order To Vch_No
Goto Top
Set Relation To Str(C1,2)+Str(C2,2)+Str(C3,3) Into ILdgrcod Additive
If MVch_Type=1
	MTitle="PURCHASE REPORT (ITEM WISE)"
Endif	
If MVch_Type=2
	MTitle="PURCHASE RETURN REPORT (ITEM WISE)"
Endif	
If MVch_Type=3
	MTitle="SALE REPORT (ITEM WISE)"
Endif	
If MVch_Type=4
	MTitle="SALE RETURN REPORT (ITEM WISE)"
Endif	
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind ritem
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
MMI1=Str(I1,2)
MMI2=Str(I2,2)
MMI3=Str(I3,4)
Do Itm1CodPro In Proc1
@ 04,00 Say "Item Code:  "+MMI1+"-"+MMI2+"-"+MMI3+"   "+Item.Item
@ 04,68 Say "Page # "+Str(MPage,4)
@ 05,00 Say Repl('-',80)
@ 06,00 Say Chr(15)
@ 06,0 Say "Date        Inv #     Party Code        Description                  Unit          Qty              Rate               Net"
@ 07,0 Say "                                                                                                                      Amount"
@ 08,00 Say Repl('-',137)
MLine=9								&& Line Number
Do While .Not. Eof()
	MC1=Str(C1,2)
	MC2=Str(C2,2)
	MC3=Str(C3,3)
	Do LedCodPro In Proc1
	MVch_No=Str(Vch_No,8)	
	Do VchCodPro In Proc1
	@ MLine,00 Say Dtoc(Date)+"  "+MVch_No+"   "+MC1+"-"+MC2+"-"+MC3+"     "+ILdgrcod.Title+"    "+Item.Unit+"    "+Str(T_Qty,12,4)+"   "+Str(G_Amount/T_Qty,14,4)+"   "+Str(G_Amount,17,4)
	MLine=MLine+1
	MT_Qty=MT_Qty+T_Qty
	MG_Amount=MG_Amount+G_Amount
	Skip
	If MLine>59
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
		MMI1=Str(I1,2)
		MMI2=Str(I2,2)
		MMI3=Str(I3,4)
		Do Itm1CodPro In Proc1
		@ 04,00 Say "Item Code:  "+MMI1+"-"+MMI2+"-"+MMI3+"   "+Item.Item
		@ 04,68 Say "Page # "+Str(MPage,4)
		@ 05,00 Say Repl('-',80)
		@ 06,00 Say Chr(15)
		@ 06,0 Say "Date        Inv #     Party Code        Description                  Unit          Qty              Rate               Net"
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
MMI1=Str(I1,2)
MMI2=Str(I2,2)
MMI3=Str(I3,4)
Do Itm1CodPro In Proc1
@ 04,01 Say "Item Code:  "+MMI1+"-"+MMI2+"-"+MMI3+"   "+Item.Item
@ 05,0 Say Repl('-',78)
@ 06,0 Say "Date     Inv #  Party Code  Description       Qty        Rate         Net"
@ 07,0 Say "                                                                     Amount"
@ 08,0 Say Repl('-',78)
Do While .Not. Eof()
	MC1=Str(C1,2)
	MC2=Str(C2,2)
	MC3=Str(C3,3)
	Do LedCodPro In Proc1
	MVch_No=Str(Vch_No,8)	
	Do VchCodPro In Proc1
	@ MLine,00 Say Dtoc(Date)+" "+Right(MVch_No,6)+" "+MC1+"-"+MC2+"-"+MC3+" "+Left(ILdgrcod.Title,10)+"    "+Str(T_Qty,10,2)+" "+Str(G_Amount/T_Qty,12,2)+" "+Str(G_Amount,13,2)
	MLine=MLine+1
	MT_Qty=MT_Qty+T_Qty
	MG_Amount=MG_Amount+G_Amount
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
@ MLine,32 Say "Total"
@ MLine,38 Say Str(MT_Qty,12,2)
@ MLine,60 Say Str(MG_Amount,17,2)
MLine=MLine+1
@ MLine,34 Say Repl('-',44)
@ 21,28 Say "Quit Only (Q)"
@ 21,50 GET MCQ Pict"@!" Valid MCQ="Q" Error 'Quit Only (Q)'
Read
Rele wind scr
Return
