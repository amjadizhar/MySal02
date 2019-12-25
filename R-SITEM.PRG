*-------------------------------------------*
*Program to Report Summary Item Wise		*
*Author		:	Muhammad Usman				*
*Date		:	29-11-1999					*
*Prog		:	R-SItem						*
*Last Modify:	29-11-1999					*
*-------------------------------------------*
* Select Files
Close Databases

Sele 1
Use Stocks Alia Stocks Shared

Sele 2
Use Item Alia Item Shared
Set Order To I123
*-----------------------------------
*1.	Purchase
*2.	Purchase Return
*3.	Sale
*4.	Sale Return
*-----------------------------------
*Veriable Initialization
MG_Amount=0							&& Net Amount
MT_Qty=0							&& Total Quantity
MGG_Amount=0						&& Net Amount
MGT_Qty=0							&& Total Quantity
MPage=0								&& Page No
MLine=0								&& Line Number
MSDate=Ctod("  /  /    ")			&& Start Transaction Date
MEDate=Ctod("  /  /    ")			&& End Transaction Date
MI1=0								&& First Ledger Code
MI2=0								&& Second Ledger Code
MI3=0								&& Thired Ledger Code
MItem=Spac(30)						&& Item Description
MUnit=Spac(6)						&& Item Unit
MPS=Spac(1)							&& Printer Or Screen
MCQ=Spac(1)							&& Continue Or Quit
*-----------------------------------
* Window Defination
	DEFINE WINDOW rsitem ;
		FROM INT((SROW()-17)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-17)/1)+9,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW rsitem SAME
*-----------------------------------
Set Century Off
Clear
Do While .T.
	@ 2,05 Say "Date From:" Color W/N
	@ 4,05 Say "Date To:" Color W/N
	@ 2,30 GET MSDate Pict("99/99/9999")
	@ 4,30 GET MEDate Pict("99/99/9999")
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Close Databases
		Rele Wind rsparty
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
Set Filter To Date>=MSDate .And. Date<=MEDate .And. Vch_Type=MVch_Type
Set Relation To Str(I1,2)+Str(I2,2)+Str(I3,4) Into Item Additive
Set Order To I123
Goto Top
If MVch_Type=1
	MTitle="PURCHASE REPORT (ITEM WISE SUMMARY)"
Endif	
If MVch_Type=2
	MTitle="PURCHASE RETURN REPORT (ITEM WISE SUMMARY)"
Endif	
If MVch_Type=3
	MTitle="SALE REPORT (ITEM WISE SUMMARY)"
Endif	
If MVch_Type=4
	MTitle="SALE RETURN REPORT (ITEM WISE SUMMARY)"
Endif	
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind rsitem
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
@ 03,68 Say "Page # "+Str(MPage,4)
@ 04,00 Say Repl('-',80)
@ 05,0 Say "Item Code         Description             Unit       Qty             Value"
@ 06,00 Say Repl('-',80)
MLine=7								&& Line Number
Do While .Not. Eof()
	MI1=0								&& First Ledger Code
	MI2=0								&& Second Ledger Code
	MI3=0								&& Thired Ledger Code
	MI1=I1
	MI2=I2
	MI3=I3
	MItem=Item.Item
	MUnit=Item.Unit
	Do While I1=MI1 .And. I2=MI2 .And. I3=MI3
		MT_Qty=MT_Qty+T_Qty
		MG_Amount=MG_Amount+G_Amount
		Skip
	Enddo		
	MI1=Str(MI1,2)
	MI2=Str(MI2,2)
	MI3=Str(MI3,4)
	Do ItmCodPro In Proc1
	@ MLine,00 Say MI1+"-"+MI2+"-"+MI3+"  "+Left(MItem,27)+"  "+MUnit+"  "+Str(MT_Qty,12,4)+"  "+Str(MG_Amount,17,4)
	MLine=MLine+1
	MGT_Qty=MGT_Qty+MT_Qty
	MGG_Amount=MGG_Amount+MG_Amount
	MG_Amount=0							&& Net Amount
	MT_Qty=0							&& Total Quantity
	If MLine>58
		Eject
		MPage=MPage+1
		MLine=7								&& Line Number
		@ 00,00 Say Chr(18)
		@ 00,00 Say Chr(14)
		@ 00,01 Say MComp
		@ 01,00 Say Chr(18)
		@ 01,00 Say Chr(14)
		@ 01,01 Say MTitle
		@ 02,00 Say Chr(18)
		@ 02,01 Say "From Date :   "+Dtoc(MSDate)
		@ 03,00 Say "To   Date :   "+Dtoc(MEDate)
		@ 03,68 Say "Page # "+Str(MPage,4)
		@ 04,00 Say Repl('-',80)
		@ 05,0 Say "Item Code         Description             Unit       Qty             Value"
		@ 06,00 Say Repl('-',80)
	Endif
Enddo
@ MLine,42 Say Repl('-',38)
MLine=MLine+1
@ MLine,43 Say "Total"
@ MLine,50 Say Str(MGT_Qty,12,4)
@ MLine,63 Say Str(MGG_Amount,17,4)
MLine=MLine+1
@ MLine,42 Say Repl('-',38)
Eject
Return
*-----------------------------------
Procedure RepScrPro							&& Report Screen Procedure
*-----------------------------------
Do DspScrPro In Proc1
MLine=7								&& Line Number
@ 00,01 Say MComp
@ 01,01 Say MTitle
@ 02,01 Say "From Date :  "+Dtoc(MSDate)
@ 03,01 Say "To   Date :  "+Dtoc(MEDate)
@ 04,00 Say Repl('-',78)
@ 05,00 Say "Item Code   Description                   Unit      Qty          Value"
@ 06,00 Say Repl('-',78)
Do While .Not. Eof()
	MI1=0								&& First Ledger Code
	MI2=0								&& Second Ledger Code
	MI3=0								&& Thired Ledger Code
	MI1=I1
	MI2=I2
	MI3=I3
	MItem=Item.Item
	MUnit=Item.Unit
	Do While I1=MI1 .And. I2=MI2 .And. I3=MI3
		MT_Qty=MT_Qty+T_Qty
		MG_Amount=MG_Amount+G_Amount
		Skip
	Enddo		
	MI1=Str(MI1,2)
	MI2=Str(MI2,2)
	MI3=Str(MI3,4)
	Do ItmCodPro In Proc1
	@ MLine,00 Say MI1+"-"+MI2+"-"+MI3+" "+MItem+" "+MUnit+" "+Str(MT_Qty,11,3)+" "+Str(MG_Amount,16,3)
	MLine=MLine+1
	MGT_Qty=MGT_Qty+MT_Qty
	MGG_Amount=MGG_Amount+MG_Amount
	MG_Amount=0							&& Net Amount
	MT_Qty=0							&& Total Quantity
	If MLine>18
		@ 21,28 Say "Continue Or Quit C/Q"
		@ 21,50 GET MCQ Pict"@!" Valid MCQ="C" .Or. MCQ="Q" Error 'Continue Or Quit (C/Q)'
		Read
		If MCQ="Q"
			Rele wind scr
			Return
		Endif	
		@ 7,0 To 21,78 Clear
		MLine=7
		Endif
Enddo
@ MLine,43 Say Repl('-',35)
MLine=MLine+1
@ MLine,44 Say "Total"
@ MLine,49 Say Str(MGT_Qty,11,3)
@ MLine,61 Say Str(MGG_Amount,16,3)
MLine=MLine+1
@ MLine,43 Say Repl('-',35)
@ 21,28 Say "Quit Only (Q)"
@ 21,50 GET MCQ Pict"@!" Valid MCQ="Q" Error 'Quit Only (Q)'
Read
Rele wind scr
Return
