*-------------------------------------------*
*Program to Report Party Ledger				*
*Author		:	Muhammad Usman				*
*Date		:	30-11-1999					*
*Prog		:	R-PLed						*
*Last Modify:	30-11-1999					*
*-------------------------------------------*
* Select Files
Close Databases

Sele 1
Use IMaster Alia IMaster Shared

Sele 2
Use ILdgrcod Alia ILdgrcod Shared
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
MVch_No=0							&& Voucher Number
MC1=0								&& Party Code 1
MC2=0								&& Party Code 2
MC3=0								&& Party Code 3
MMC1=0								&& Party Code 1
MMC2=0								&& Party Code 2
MMC3=0								&& Party Code 3
MDr=0								&& Debit Total
MCr=0								&& Credit Total
MBal=0								&& Balance Total
MPS=Spac(1)							&& Printer Or Screen
MCQ=Spac(1)							&& Continue Or Quit
*-----------------------------------
* Window Defination
	DEFINE WINDOW rpled ;
		FROM INT((SROW()-17)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-17)/1)+9,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW rpled SAME
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
		Rele Wind rpled
		Return
		Exit
	Endif
	Sele IMaster
	Set Order To Date
	Seek MSDate
	If .Not. Found()
		Wait Wind 'Date Not Exist'
		MSDate=Ctod("  /  /    ")			&& Start Transaction Date
		Loop
	Endif		
	@ 07,08 SAY "Press F1 to see Party Code List" Color GR+
	ON KEY LABEL F1 DO ShwLdgPro In Proc1
	@ 05,30 Say "  -  -   "
	@ 05,30 Get MC1 Pict"@2K 99" Valid MC1#0 Error 'Ledger Head Code Can Not Be Blank'
	@ 05,33 Get MC2 Pict"@2K 99" Valid MC2#0 Error 'Ledger Head Code Can Not Be Blank'
	@ 05,36 Get MC3 Pict"@3K 999" Valid MC3#0 Error 'Ledger Head Code Can Not Be Blank'
	Read	
	Sele ILdgrcod
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
Clear
@ 4,05 Say "Report To Printer Or Screen (P/S)" Color W/N
@ 4,40 GET MPS Pict"@!" Valid MPS="P" .Or. MPS="S" Error 'Printer Or Screen (P/S)' Color W/N
Read
Clear
@ 03,13 SAY "Report in Processing"
@ 05,13 SAY "Please Don't Distrub"
Sele IMaster
Set Filter To Date>=MSDate .And. Date<=MEDate .And. (C1=MC1 .And. C2=MC2 .And. C3=MC3)
Set Order To Sr_No
Goto Top
MTitle="PARTY LEDGER"
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind rpled
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
@ 04,00 Say "Party Code :   "+MMC1+"-"+MMC2+"-"+MMC3+"    "+ILdgrcod.Title
@ 04,69 Say "Page# "+Str(MPage,4)
@ 05,00 Say Repl('-',80)
@ 06,00 Say Chr(15)
@ 06,0 Say "Date          Invoice               Desc                      Debit               Credit          Balance"
@ 07,00 Say Repl('-',137)
MLine=8								&& Line Number
Do While .Not. Eof()
	MVch_No=Str(Vch_No,8)	
	Do VchCodPro In Proc1
	@ MLine,00 Say Dtoc(Date)+"    "+IIf(Vch_Type=1,'PURCH',IIf(Vch_Type=2,'PUR-R',IIf(Vch_Type=3,'SALE ',IIf(Vch_Type=4,'SAL-R','     '))))+"-"+MVch_No+"    "+Desc+"    "+IIf(Status="DR",Str(Dr_Amount,12,2),'            ')+"    "+IIf(Status="CR",Str(Cr_Amount,12,2),'            ')+"    "+Str(Bl_Amount,12,2)
	MLine=MLine+1
	If Status="DR"
		MDr=MDr+Dr_Amount		
	Else
		MCr=MCr+Cr_Amount
	Endif
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
		MMC1=Str(C1,2)
		MMC2=Str(C2,2)
		MMC3=Str(C3,3)
		Do Led1CodPro In Proc1
		@ 04,00 Say "Party Code :   "+MMC1+"-"+MMC2+"-"+MMC3+"    "+ILdgrcod.Title
		@ 04,69 Say "Page# "+Str(MPage,4)
		@ 05,00 Say Repl('-',80)
		@ 06,00 Say Chr(15)
		@ 06,0 Say "Date          Invoice               Desc                      Debit               Credit          Balance"
		@ 07,00 Say Repl('-',137)
	Endif
Enddo
MLine=MLine+1
@ MLine,48 Say Repl('-',89)
MLine=MLine+1
@ MLine,57 Say "Total"+"  "+Str(MDr,12,2)+"    "+Str(MCr,12,2)
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
MMC1=Str(C1,2)
MMC2=Str(C2,2)
MMC3=Str(C3,3)
Do Led1CodPro In Proc1
@ 04,01 Say "Party Code :  "+MMC1+"-"+MMC2+"-"+MMC3+"    "+ILdgrcod.Title
@ 05,0 Say Repl('-',78)
@ 06,0 Say "Date      Invoice         Desc            Debit        Credit       Balance"
@ 08,0 Say Repl('-',78)
Do While .Not. Eof()
	MVch_No=Str(Vch_No,8)	
	Do VchCodPro In Proc1
	@ MLine,00 Say Dtoc(Date)+" "+IIf(Vch_Type=1,'PURCH',IIf(Vch_Type=2,'PUR-R',IIf(Vch_Type=3,'SALE ',IIf(Vch_Type=4,'SAL-R','     '))))+"-"+MVch_No+" "+Left(Desc,15)+" "+IIf(Status="DR",Str(Dr_Amount,12,2),'            ')+" "+IIf(Status="CR",Str(Cr_Amount,12,2),'            ')+" "+Str(Bl_Amount,12,2)
	MLine=MLine+1
	If Status="DR"
		MDr=MDr+Dr_Amount		
	Else
		MCr=MCr+Cr_Amount
	Endif
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
@ MLine,30 Say Repl('-',48)
MLine=MLine+1
@ MLine,33 Say "Total"+"  "+Str(MDr,12,2)+" "+Str(MCr,12,2)
MLine=MLine+1
@ MLine,30 Say Repl('-',48)
@ 21,28 Say "Quit Only (Q)"
@ 21,50 GET MCQ Pict"@!" Valid MCQ="Q" Error 'Quit Only (Q)'
Read
Rele wind scr
Return
