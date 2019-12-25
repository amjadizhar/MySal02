*-----------------------------------------------*
*Program to Report Type & Date Wise Transaction	*
*Author		:	Muhammad Usman					*
*Date		:	03-02-2000						*
*Prog		:	RN-Tran							*
*Last Modify:	03-02-2000						*
*-----------------------------------------------*
* Select Files
Close Databases

Sele 1
Use Tran2 Alia Tran2 Shared

Sele 2
Use LdgrCod Alia LdgrCod Shared
Set Order To C123

Sele 3
Use DType Alia DType Shared
Set Order To Doc_Type

*-----------------------------------
*Veriable Initialization
MDoc_Type=0							&& Document Type
MDAmount=0							&& Total Debit Amount
MCAmount=0							&& Total Credit Amount
MPage=0								&& Page No
MLine=0								&& Line Number
MSDate=Ctod("  /  /    ")			&& Start Transaction Date
MEDate=Ctod("  /  /    ")			&& End Transaction Date
MPS=Spac(1)							&& Printer Or Screen
MCQ=Spac(1)							&& Continue Or Quit
*-----------------------------------
* Window Defination
	DEFINE WINDOW rntran ;
		FROM INT((SROW()-17)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-17)/1)+9,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW rntran SAME
*-----------------------------------
Clear
Do While .T.
	@ 1,05 Say "Document Type:" Color W/N
	@ 3,05 Say "Date From:" Color W/N
	@ 5,05 Say "Date To:" Color W/N
	@ 1,30 Get MDoc_Type Pict"9"
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Close Databases
		Rele Wind rntran
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
	@ 3,30 GET MSDate Pict("99/99/9999")
	@ 5,30 GET MEDate Pict("99/99/9999")
	Read	
	Exit
Enddo
Clear
@ 4,05 Say "Report To Printer Or Screen (P/S)" Color W/N
@ 4,40 GET MPS Pict"@!" Valid MPS="P" .Or. MPS="S" Error 'Printer Or Screen (P/S)' Color W/N
Read
Clear
@ 03,13 SAY "Report in Processing"
@ 05,13 SAY "Please Don't Distrub"
Sele Tran2
Set Filter To Date>=MSDate .And. Date<=MEDate .And. Doc_Type=MDoc_Type
Set Order To Date
Goto Bott
MEDate=Date
Goto Top
MSDate=Date
Set Relation To Str(C1,2)+Str(C2,2)+Str(C3,3) Into LdgrCod Additive
Set Relation To Doc_Type Into DType Additive
MTitle="DAILY VOUCHERS"
Goto Top
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind rntran
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
@ 04,00 Say Chr(15)
@ 04,00 Say Repl('-',137)
@ 05,0 Say " DATE         DOC #     CODE       TITLE                         DESCRIPTION                   DR          CR"
@ 06,0 Say Repl('-',137)
MLine=7								&& Line Number
Do While .Not. Eof()
	MMC1=Str(C1,2)
	MMC2=Str(C2,2)
	MMC3=Str(C3,3)
	Do Led1CodPro In Proc1
	@ MLine,00 Say Dtoc(Date)+" "+DType.Type1+"-"+Doc_No+" "+MMC1+"-"+MMC2+"-"+MMC3+" "+Ldgrcod.Title+" "+Disc
	@ MLine,89 Say IIF(Cr_Dr="DR",(Amount),'') Pict"999,999,999.99"
	@ MLine,104 Say IIF(Cr_Dr="CR",(Amount),'') Pict"999,999,999.99"
	If Cr_Dr="DR"
		MDAmount=MDAmount+Amount
	Else
		MCAmount=MCAmount+Amount
	Endif
	Skip
	MLine=MLine+1
	If MLine>59
		Eject
		MPage=MPage+1
		MLine=7								&& Line Number
		@ 00,00 Say Chr(18)
		@ 00,00 Say Chr(14)
		@ 00,01 Say MComp
		@ 00,00 Say Chr(18)
		@ 01,01 Say MTitle
		@ 02,01 Say "From Date :   "+Dtoc(MSDate)
		@ 03,01 Say "To   Date :   "+Dtoc(MEDate)
		@ 03,50 Say "Page# "+Str(MPage,4)
		@ 04,00 Say Chr(15)
		@ 04,00 Say Repl('-',137)
		@ 05,0 Say " DATE         DOC #     CODE       TITLE                         DESCRIPTION                   DR          CR"
		@ 06,0 Say Repl('-',137)
	Endif
	MLine=MLine+1
Enddo
@ MLine,0 Say Repl('-',137)
MLine=MLine+1
@ MLine,50 Say "Total"
@ MLine,89 Say MDAmount Pict"999,999,999.99"
@ MLine,104 Say MCAmount Pict"999,999,999.99"
Eject
Return
*-----------------------------------
Procedure RepScrPro							&& Report Screen Procedure
*-----------------------------------
Do DspScrPro In Proc1
MLine=7								&& Line Number
@ 00,01 Say MComp
@ 01,01 Say MTitle
@ 02,01 Say "From Date :"+Dtoc(MEDate)
@ 03,01 Say "To   Date :"+Dtoc(MSDate)
@ 04,0 Say Repl('-',78)
@ 05,0 Say "DATE       DOC #        TITLE          DESCRIPTION         DR          CR"
@ 06,0 Say Repl('-',78)
Do While .Not. Eof()
	@ MLine,00 Say Dtoc(Date)+" "+DType.Type1+"-"+Doc_No+" "+Left(Ldgrcod.Title,15)+" "+Left(Disc,15)
	@ MLine,50 Say IIF(Cr_Dr="DR",(Amount),'') Pict"999,999,999.99"
	@ MLine,64 Say IIF(Cr_Dr="CR",(Amount),'') Pict"999,999,999.99"
	If Cr_Dr="DR"
		MDAmount=MDAmount+Amount
	Else
		MCAmount=MCAmount+Amount
	Endif
	Skip
	MLine=MLine+1
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
@ MLine,0 Say Repl('-',78)
MLine=MLine+1
@ MLine,20 Say "Total"
@ MLine,50 Say MDAmount Pict"999,999,999.99"
@ MLine,64 Say MCAmount Pict"999,999,999.99"
@ 21,28 Say "Quit Only (Q)"
@ 21,50 GET MCQ Pict"@!" Valid MCQ="Q" Error 'Quit Only (Q)'
Read
Rele wind scr
Return
