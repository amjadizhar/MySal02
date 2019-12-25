*-------------------------------------------*
*Program to Report Stock Ledger				*
*Author		:	Muhammad Usman				*
*Date		:	29-11-1999					*
*Prog		:	R-SLed						*
*Last Modify:	29-11-1999					*
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
Set Order To C123
*-----------------------------------
*1.	Purchase
*2.	Purchase Return
*3.	Sale
*4.	Sale Return
*-----------------------------------
*Veriable Initialization
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
MVch_No=0							&& Voucher Number
MInward=0							&& Inward Total
MOutward=0							&& Outward Total
MQtyBal=0							&& Quantity Balance Total
MValue=0							&& Value Total
MPS=Spac(1)							&& Printer Or Screen
MCQ=Spac(1)							&& Continue Or Quit
*-----------------------------------
* Window Defination
	DEFINE WINDOW rsled ;
		FROM INT((SROW()-17)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-17)/1)+9,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW rsled SAME
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
		Rele Wind rsled
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
Set Filter To Date>=MSDate .And. Date<=MEDate .And. (I1=MI1 .And. I2=MI2 .And. I3=MI3)
Set Relation To Str(C1,2)+Str(C2,2)+Str(C3,3) Into ILdgrcod Additive
Set Order To Sr_No
Goto Top
MTitle="STOCK LEDGER"
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind rsled
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
@ 04,00 Say "Item Code :  "+MMI1+"-"+MMI2+"-"+MMI3+"   "+Item.Item+"   "+Item.Unit
@ 04,69 Say "Page# "+Str(MPage,4)
@ 05,00 Say Repl('-',80)
@ 06,00 Say Chr(15)
@ 06,00 Say " Date       Invoice            Desc                           Inward        Outward       Qty Bal       Rate         Value"
@ 07,00 Say Repl('-',137)
MLine=8								&& Line Number
Do While .Not. Eof()
	MC1=Str(C1,2)
	MC2=Str(C2,2)
	MC3=Str(C3,3)
	Do LedCodPro In Proc1
	MVch_No=Str(Vch_No,8)	
	Do VchCodPro In Proc1
	@ MLine,00 Say Dtoc(Date)+"  "+IIf(Vch_Type=1,'PURCH',IIf(Vch_Type=2,'PUR-R',IIf(Vch_Type=3,'SALE ',IIf(Vch_Type=4,'SAL-R','     '))))+"-"+MVch_No+"  "+ILdgrcod.Title+"  "+IIf(Vch_Type=1 .Or. Vch_Type=4,Str(T_Qty,12,4),'            ')+"  "+IIf(Vch_Type=2 .Or. Vch_Type=3,Str(T_Qty,12,4),'            ')+"  "+Str(Qty_Bal,12,4)+"  "+Str(G_Amount/T_Qty,10,2)+"  "+Str(G_Amount,17,4)
	MLine=MLine+1
	If Vch_Type=1 .Or. Vch_Type=4
		MInward=MInward+T_Qty
	Else
		MOutward=MOutward+T_Qty
	Endif
	MQtyBal=MQtyBal+Qty_Bal
	MValue=MValue+G_Amount
	Skip
	If MLine>59
		Eject
		MPage=MPage+1
		MLine=8								&& Line Number
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
		@ 04,00 Say "Item Code :  "+MMI1+"-"+MMI2+"-"+MMI3+"   "+Item.Item+"   "+Item.Unit
		@ 04,69 Say "Page# "+Str(MPage,4)
		@ 05,00 Say Repl('-',80)
		@ 06,00 Say Chr(15)
		@ 06,00 Say " Date       Invoice            Desc                           Inward        Outward       Qty Bal       Rate         Value"
		@ 07,00 Say Repl('-',137)
	Endif
Enddo
MLine=MLine+1
@ MLine,48 Say Repl('-',89)
MLine=MLine+1
@ MLine,51 Say "Total"+"  "+Str(MInward,12,4)+"  "+Str(MOutward,12,4)+"  "+Str(MQtyBal,12,4)+"              "+Str(MValue,17,4)
MLine=MLine+1
@ MLine,48 Say Repl('-',89)
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
@ 04,01 Say "Item Code :  "+MMI1+"-"+MMI2+"-"+MMI3+"   "+Item.Item+"     "+Item.Unit
@ 05,0 Say Repl('-',78)
@ 06,0 Say "Date      Invoice         Desc      Inward  Outward Qty Bal  Rate   Value"
@ 08,0 Say Repl('-',78)
Do While .Not. Eof()
	MC1=Str(C1,2)
	MC2=Str(C2,2)
	MC3=Str(C3,3)
	Do LedCodPro In Proc1
	MVch_No=Str(Vch_No,8)	
	Do VchCodPro In Proc1
	@ MLine,00 Say Dtoc(Date)+" "+IIf(Vch_Type=1,'PURCH',IIf(Vch_Type=2,'PUR-R',IIf(Vch_Type=3,'SALE ',IIf(Vch_Type=4,'SAL-R','     '))))+"-"+MVch_No+" "+IIf(Sr_No=1,'Open Bal  ',Left(ILdgrcod.Title,10))+" "+IIf(Vch_Type=1 .Or. Vch_Type=4 .Or. Sr_No=1,Str(T_Qty,7,1),'       ')+" "+IIf(Vch_Type=2 .Or. Vch_Type=3,Str(T_Qty,7,1),'       ')+" "+Str(Qty_Bal,7,1)+" "+Str(G_Amount/T_Qty,7,1)+" "+Str(G_Amount,11,1)
	MLine=MLine+1
	If Vch_Type=1 .Or. Vch_Type=4 .Or. Sr_No=1
		MInward=MInward+T_Qty
	Else
		MOutward=MOutward+T_Qty
	Endif
	MQtyBal=MQtyBal+Qty_Bal
	MValue=MValue+G_Amount
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
MLine=MLine+1
@ MLine,26 Say Repl('-',52)
MLine=MLine+1
@ MLine,28 Say "Total"+"  "+Str(MInward,7,1)+" "+Str(MOutward,7,1)+" "+Str(MQtyBal,7,1)+"         "+Str(MValue,11,1)
MLine=MLine+1
@ MLine,26 Say Repl('-',52)
@ 21,28 Say "Quit Only (Q)"
@ 21,50 GET MCQ Pict"@!" Valid MCQ="Q" Error 'Quit Only (Q)'
Read
Rele wind scr
Return
