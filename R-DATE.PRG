*-------------------------------------------*
*Program to Report Date Wise Transactions	*
*Author		:	Muhammad Usman				*
*Date		:	24-11-1999					*
*Prog		:	R-Date						*
*Last Modify:	24-11-1999					*
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
MPage=0								&& Page No
MLine=0								&& Line Number
MSDate=Ctod("  /  /    ")			&& Start Transaction Date
MEDate=Ctod("  /  /    ")			&& End Transaction Date
MPS=Spac(1)							&& Printer Or Screen
MCQ=Spac(1)							&& Continue Or Quit
*-----------------------------------
* Window Defination
	DEFINE WINDOW rdate ;
		FROM INT((SROW()-17)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-17)/1)+9,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW rdate SAME
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
		Rele Wind rdate
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
Set Order To DVch_No
Goto Top
Set Relation To Str(I1,2)+Str(I2,2)+Str(I3,4) Into Item Additive
If MVch_Type=1
	MTitle="PURCHASE REPORT (DATE WISE)"
Endif	
If MVch_Type=2
	MTitle="PURCHASE RETURN REPORT (DATE WISE)"
Endif	
If MVch_Type=3
	MTitle="SALE REPORT (DATE WISE)"
Endif	
If MVch_Type=4
	MTitle="SALE RETURN REPORT (DATE WISE)"
Endif	
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind rdate
Set Print Off
Set Device To Screen
Close Databases
Return
*-----------------------------------
Procedure RepPrtPro							&& Report Print Procedure
*-----------------------------------
Set Print On
Set Device To Print
@ 00,00 Say Chr(18)
@ 00,00 Say Chr(14)
@ 00,01 Say MComp
@ 01,00 Say Chr(18)
@ 01,00 Say Chr(14)
@ 01,01 Say MTitle
@ 02,00 Say Chr(18)
@ 02,01 Say "From Date :   "+Dtoc(MSDate)
@ 03,00 Say "To   Date :   "+Dtoc(MEDate)
@ 04,00 Say Repl('-',80)
@ 05,00 Say Chr(15)
@ 05,0 Say "Date        Inv #     Item Code         Description                  Unit          Qty              Rate               Net"
@ 06,0 Say "                                                                                                                      Amount"
@ 07,00 Say Repl('-',137)
MLine=8								&& Line Number
Do While .Not. Eof()
	MI1=Str(I1,2)
	MI2=Str(I2,2)
	MI3=Str(I3,4)
	Do ItmCodPro In Proc1
	MVch_No=Str(Vch_No,8)	
	Do VchCodPro In Proc1
	@ MLine,00 Say Dtoc(Date)+"  "+MVch_No+"   "+MI1+"-"+MI2+"-"+MI3+"    "+Item.Item+"    "+Item.Unit+"    "+Str(T_Qty,12,4)+"   "+Str(G_Amount/T_Qty,14,4)+"   "+Str(G_Amount,17,4)
	MLine=MLine+1
	MT_Qty=MT_Qty+T_Qty
	MG_Amount=MG_Amount+G_Amount
	Skip
	If MLine>59
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
		@ 04,00 Say Repl('-',80)
		@ 05,00 Say Chr(15)
		@ 05,0 Say "Date     Inv #  Item Code   Description       Qty           Rate       Net"
		@ 06,0 Say "                                                                      Amount"
		@ 07,00 Say Repl('-',137)
	Endif
Enddo
@ MLine,0 Say Repl('-',137)
MLine=MLine+1
@ MLine,0 Say Chr(18)
@ MLine,05 Say "Total"
@ MLine,43 Say Str(MT_Qty,12,4)
@ MLine,61 Say Str(MG_Amount,17,4)
Eject
Return
*-----------------------------------
Procedure RepScrPro							&& Report Screen Procedure
*-----------------------------------
Do DspScrPro In Proc1
MLine=8								&& Line Number
@ 00,01 Say MComp
@ 01,01 Say MTitle
@ 02,01 Say "From Date :"+Dtoc(MSDate)
@ 03,01 Say "To   Date :"+Dtoc(MEDate)
@ 04,0 Say Repl('-',78)
@ 05,0 Say "Date     Inv #  Item Code   Description       Qty           Rate       Net"
@ 06,0 Say "                                                                      Amount"
@ 07,0 Say Repl('-',78)
Do While .Not. Eof()
	MI1=Str(I1,2)
	MI2=Str(I2,2)
	MI3=Str(I3,4)
	Do ItmCodPro In Proc1
	MVch_No=Str(Vch_No,8)	
	Do VchCodPro In Proc1
	@ MLine,00 Say Dtoc(Date)+" "+Right(MVch_No,6)+" "+MI1+"-"+MI2+"-"+MI3+" "+Left(Item.Item,10)+"    "+Str(T_Qty,10,2)+" "+Str(G_Amount/T_Qty,12,2)+" "+Str(G_Amount,13,2)
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
		@ 8,0 To 21,78 Clear
		MLine=8
	Endif
Enddo
@ MLine,0 Say Repl('-',78)
MLine=MLine+1
@ MLine,05 Say "Total"
@ MLine,39 Say Str(MT_Qty,12,2)
@ MLine,61 Say Str(MG_Amount,17,2)
@ 21,28 Say "Quit Only (Q)"
@ 21,50 GET MCQ Pict"@!" Valid MCQ="Q" Error 'Quit Only (Q)'
Read
Rele wind scr
Return
