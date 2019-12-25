*-----------------------------------*
*Program to Report Date Wise Invoice*
*Author		:	Muhammad Usman		*
*Date		:	02-02-2000			*
*Prog		:	RD-Invo				*
*Last Modify:	02-02-2000			*
*-----------------------------------*
* Select Files
Close Databases

Sele 1
Use ITran Alia ITran Shared

Sele 2
Use LdgrCod Alia LdgrCod Shared
Set Order To C123

Sele 3
Use Item Alia Item Shared
Set Order To I123

Sele 4
Use DType Alia DType Shared
Set Order To Doc_Type

*-----------------------------------
*Veriable Initialization
MDoc_Type=0							&& Document Type
MDoc_No=Spac(6)						&& Document Number
MAmount=0							&& Total Amount
MPage=0								&& Page No
MLine=0								&& Line Number
MSDate=Ctod("  /  /    ")			&& Start Transaction Date
MEDate=Ctod("  /  /    ")			&& End Transaction Date
MPS=Spac(1)							&& Printer Or Screen
MCQ=Spac(1)							&& Continue Or Quit
*-----------------------------------
* Window Defination
	DEFINE WINDOW rdinvoic ;
		FROM INT((SROW()-17)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-17)/1)+9,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW rdinvoic SAME
*-----------------------------------
Clear
Do While .T.
	@ 2,05 Say "Date From:" Color W/N
	@ 4,05 Say "Date To:" Color W/N
	@ 2,30 GET MSDate Pict("99/99/9999")
	@ 4,30 GET MEDate Pict("99/99/9999")
	Read	
	If Read()=12 .Or. Read()=268				&& Escape Key
		Close Databases
		Rele Wind rdinvoic
		Return
		Exit
	Endif
	Sele ITran
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
Sele ITran
Set Filter To Date>=MSDate .And. Date<=MEDate
Set Order To Date
Goto Bott
MEDate=Date
Goto Top
MSDate=Date
Set Relation To Str(C1,2)+Str(C2,2)+Str(C3,3) Into LdgrCod Additive
Set Relation To Str(I1,2)+Str(I2,2)+Str(I3,4) Into Item Additive
Set Relation To Doc_Type Into DType Additive
MTitle="SALE INVOICES"
Goto Top
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind rdinvoic
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
@ 00,00 Say Chr(18)
@ 01,01 Say MTitle
@ 02,01 Say "From Date :   "+Dtoc(MSDate)
@ 03,01 Say "To   Date :   "+Dtoc(MEDate)
@ 03,50 Say "Page# "+Str(MPage,4)
@ 04,00 Say Repl('-',80)
@ 05,0 Say "Date       DOC #            Item                        Qty    Rate    Total"
@ 06,0 Say "                                                                     Amount"
@ 07,00 Say Repl('-',80)
MLine=8								&& Line Number
Do While .Not. Eof()
	MDoc_Type=0							&& Document Type
	MDoc_No=Spac(6)						&& Document Number
	MDoc_Type=Doc_Type
	MDoc_No=Doc_No
	@ MLine,05 Say Ldgrcod.Title
	MLine=MLine+1
	Do While MDoc_Type=Doc_Type .And. Doc_No=MDoc_No
		@ MLine,00 Say Dtoc(Date)+"  "+DType.Type1+"-"+Doc_No+"  "+Item.Item+"  "+Str(Qty,4)+"   "+Str(Item.Rate,4)
		@ MLine,65 Say Amount Pict"999,999,999.99"
		MLine=MLine+1
		MAmount=MAmount+Amount
		Skip
		If MLine>59
			Eject
			MPage=MPage+1
			MLine=8								&& Line Number
			@ 00,00 Say Chr(18)
			@ 00,00 Say Chr(14)
			@ 00,01 Say MComp
			@ 00,00 Say Chr(18)
			@ 01,01 Say MTitle
			@ 02,01 Say "From Date :   "+Dtoc(MSDate)
			@ 03,01 Say "To   Date :   "+Dtoc(MEDate)
			@ 03,50 Say "Page# "+Str(MPage,4)
			@ 04,00 Say Repl('-',80)
			@ 05,0 Say "Date       DOC #            Item                        Qty    Rate    Total"
			@ 06,0 Say "                                                                     Amount"
			@ 07,00 Say Repl('-',80)
		Endif
	Enddo
	MLine=MLine+1
Enddo
@ MLine,0 Say Repl('-',80)
MLine=MLine+1
@ MLine,20 Say "Total"
@ MLine,65 Say MAmount Pict"999,999,999.99"
Eject
Return
*-----------------------------------
Procedure RepScrPro							&& Report Screen Procedure
*-----------------------------------
Do DspScrPro In Proc1
MLine=8								&& Line Number
@ 00,01 Say MComp
@ 01,01 Say MTitle
@ 02,01 Say "From Date :"+Dtoc(MEDate)
@ 03,01 Say "To   Date :"+Dtoc(MSDate)
@ 04,0 Say Repl('-',78)
@ 05,0 Say "Date       DOC #            Item                        Qty    Rate    Total"
@ 06,0 Say "                                                                     Amount"
@ 07,0 Say Repl('-',78)
Do While .Not. Eof()
	MDoc_Type=0							&& Document Type
	MDoc_No=Spac(6)						&& Document Number
	MDoc_Type=Doc_Type
	MDoc_No=Doc_No
	@ MLine,05 Say Ldgrcod.Title
	MLine=MLine+1
	Do While MDoc_Type=Doc_Type .And. Doc_No=MDoc_No
		@ MLine,00 Say Dtoc(Date)+"  "+DType.Type1+"-"+Doc_No+"  "+Item.Item+"  "+Str(Qty,4)+"   "+Str(Item.Rate,4)
		@ MLine,64 Say Amount Pict"999,999,999.99"
		MLine=MLine+1
		MAmount=MAmount+Amount
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
	MLine=MLine+1
Enddo
@ MLine,0 Say Repl('-',78)
MLine=MLine+1
@ MLine,20 Say "Total"
@ MLine,64 Say MAmount Pict"999,999,999.99"
@ 21,28 Say "Quit Only (Q)"
@ 21,50 GET MCQ Pict"@!" Valid MCQ="Q" Error 'Quit Only (Q)'
Read
Rele wind scr
Return
