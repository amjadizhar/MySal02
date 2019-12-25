*-------------------------------*
*Program to Report Ledger		*
*Author		:	Muhammad Usman	*
*Date		:	31-01-2000		*
*Prog		:	R-Led			*
*Last Modify:	10-02-2000		*
*-------------------------------*
* Select Files
Close Databases

Sele 1
Use Tran2 Alia Tran2 Shared

Sele 2
Use LdgrCod Alia LdgrCod Shared

Tmp=Sys(3)
Create Table (Tmp) (C1 N(2), C2 N(2), C3 N(3), Date D(8), Doc_Type N(1), Doc_No C(6), CR_DR C(2), Amount N(12,2), Disc C(30), B_Amount N(12,2), B_CR_Dr C(2))

Sele 3
Use (Tmp) Alia Tmp Exclusive

Sele 4
Use DType Alia DType Shared
Set Order To Doc_Type

*-----------------------------------
*Veriable Initialization
MPage=0								&& Page No
MLine=0								&& Line Number
MDate=Ctod("  /  /    ")			&& Date
MSDate=Ctod("  /  /    ")			&& Start Transaction Date
MEDate=Ctod("  /  /    ")			&& End Transaction Date
MC1=0								&& Leadger Code 1
MC2=0								&& Leadger Code 2
MC3=0								&& Leadger Code 3
MB_Amount=0							&& Leadger Balance Amount
MB_Cr_Dr=Spac(2)					&& Leadger Balance Credit / Debit
MOpBal=0							&& Leadger Opening Balance
MOCr_Dr=Spac(2)						&& Credit / Debit
MDTot=0								&& Debit Total
MCTot=0								&& Credit Total
MMB_Amount=0						&& Leadger Balance Amount
MMB_Cr_Dr=Spac(2)					&& Leadger Balance Credit / Debit
MPS=Spac(1)							&& Printer Or Screen
MCQ=Spac(1)							&& Continue Or Quit
MOBQ=Spac(1)						&& Opening Balance Yes / No
*-----------------------------------
* Window Defination
	DEFINE WINDOW rled ;
		FROM INT((SROW()-17)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-17)/1)+9,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW rled SAME
*-----------------------------------
Clear
Do While .T.
	@ 1,05 Say "Date From:" Color W/N
	@ 3,05 Say "Date To:" Color W/N
	@ 5,05 Say "Leadger Code:" Color W/N
	@ 1,30 GET MSDate Pict("99/99/9999")
	@ 3,30 GET MEDate Pict("99/99/9999")
	Read
	If Read()=12 .Or. Read()=268				&& Escape Key
		Close Databases
		Rele Wind rled
		Dele File Tmp+'.Dbf'
		Dele File Tmp+'.Cdx'
		Return
		Exit
	Endif
	Sele Tran2
	Set Order To Date
	Seek MSDate
	If .Not. Found()
		Wait Wind 'Date Not Exist'
		MSDate=Ctod("  /  /    ")			&& Start Transaction Date
		Loop
	Endif		
	Sele LdgrCod
	Set Order To Title
	@ 07,08 SAY "Press F1 to see Party Code List" Color GR+
	ON KEY LABEL F1 DO ShwLdgPro In Proc1
	@ 05,30 Say "  -  -   "
	@ 05,30 Get MC1 Pict"@2K 99" Valid MC1#0 Error 'Ledger Head Code Can Not Be Blank - Press Esc To Exit'
	@ 05,33 Get MC2 Pict"@2K 99" Valid MC2#0 Error 'Ledger Head Code Can Not Be Blank - Press Esc To Exit'
	@ 05,36 Get MC3 Pict"@3K 999" Valid MC3#0 Error 'Ledger Head Code Can Not Be Blank - Press Esc To Exit'
	Read
	Set Order To C123
	Seek Str(MC1,2)+Str(MC2,2)+Str(MC3,3)
	If .Not. Found()
		Wait Wind 'Leadger Code Not Exist'
		MC1=0								&& Leadger Code 1
		MC2=0								&& Leadger Code 2
		MC3=0								&& Leadger Code 3
		Loop
	Else
		Exit
	Endif
Enddo
Clear
@ 4,05 Say "Report To Printer Or Screen (P/S)" Color W/N
@ 4,40 GET MPS Pict"@!" Valid MPS="P" .Or. MPS="S" Error 'Printer Or Screen (P/S)' Color W/N
Read
MOBQ="Y"
Clear
@ 03,13 SAY "Report in Processing"
@ 05,13 SAY "Please Don't Distrub"
Sele Tmp
Append From Tran2 For C1=MC1 .And. C2=MC2 .And. C3=MC3
Index On DtoS(Date)+Str(Doc_Type,1)+Doc_No Tag DDD_No
Set Order To DDD_No
Goto Top
Do While .Not. Eof()
	Do LdgBalPro In Proc1
	Replace B_Amount With ABS(MB_Amount)
	Replace B_Cr_Dr With MB_Cr_Dr
	Flush
	MMB_Cr_Dr=B_Cr_Dr
	MMB_Amount=B_Amount
	MB_Amount=0							&& Leadger Balance Amount
	Skip
Enddo
Set Order To DDD_No
Goto Top
If Date>=MSDate
	If Doc_Type=0
		Replace Amount With 0
		Replace Cr_Dr With "  "		
	Endif
	MOBQ="N"
Endif
Set Order To DDD_No
Goto Top
If MSDate>Date
	MDate=MSDate	
	Do While MDate>Date
		If Eof()
			Exit
		Endif
		Skip
	Enddo
	Skip -1
	MOpBal=B_Amount
	MOCr_Dr=B_Cr_Dr
Endif
Goto Top
Set Filter To Date>=MSDate .And. Date<=MEDate
Set Relation To Doc_Type Into DType Additive
Goto Top
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind rled
Set Print Off
Set Device To Screen
Close Databases
Dele File Tmp+'.Dbf'
Dele File Tmp+'.Cdx'
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
@ 01,01 Say "LEDGER FROM: "+Dtoc(MSDate)+"  TO  "+Dtoc(MEDate)
@ 02,00 Say Chr(18)
@ 02,01 Say "Date :"+Dtoc(Date())+"           "+"Page# "+Str(MPage,4)
MMC1=Str(C1,2)
MMC2=Str(C2,2)
MMC3=Str(C3,3)
Do Led1CodPro In Proc1
@ 03,00 Say "ACCOUNT CODE : "+MMC1+"-"+MMC2+"-"+MMC3+"  "+"TITLE OF ACCOUNT : "+LdgrCod.Title
@ 04,00 Say Chr(15)
@ 04,00 Say Repl('-',137)
@ 05,00 Say "DATE     NO            DESCRIPTION                               DEBIT             CREDIT            BALANCE"
@ 06,0 Say Repl('-',137)
MLine=8								&& Line Number
If MOBQ="Y"
	@ MLine,23 Say "Opening Balance"
	@ MLine,95 Say MOpBal Pict"999,999,999.99"
	@ MLine,110 Say MOCr_Dr
	MLine=MLine+1
Endif
Do While .Not. Eof()
	@ MLine,00 Say Dtoc(Date)+"   "+Dtype.Type1+"-"+Doc_No+"   "+Disc
	@ MLine,59 Say IIF(Cr_Dr="DR",(Amount),'') Pict"999,999,999.99"
	@ MLine,77 Say IIF(Cr_Dr="CR",(Amount),'') Pict"999,999,999.99"
	@ MLine,95 Say B_Amount Pict"999,999,999.99"
	@ MLine,110 Say B_Cr_Dr
	MLine=MLine+1
	If Cr_Dr="CR"
		MCTot=MCTot+Amount
	Else
		MDTot=MDTot+Amount
	Endif
	MMB_Amount=B_Amount
	MMB_Cr_Dr=B_Cr_Dr
	Skip
	If MLine>59
		Eject
		MPage=MPage+1
		@ 00,00 Say Chr(18)
		@ 00,00 Say Chr(14)
		@ 00,01 Say MComp
		@ 01,00 Say Chr(18)
		@ 01,01 Say "LEDGER FROM: "+Dtoc(MSDate)+"  TO  "+Dtoc(MEDate)
		@ 02,00 Say Chr(18)
		@ 02,01 Say "Date :"+Dtoc(Date())+"           "+"Page# "+Str(MPage,4)
		MMC1=Str(C1,2)
		MMC2=Str(C2,2)
		MMC3=Str(C3,3)
		Do Led1CodPro In Proc1
		@ 03,00 Say "ACCOUNT CODE : "+MMC1+"-"+MMC2+"-"+MMC3+"  "+"TITLE OF ACCOUNT : "+LdgrCod.Title
		@ 04,00 Say Chr(15)
		@ 04,00 Say Repl('-',137)
		@ 05,00 Say "DATE     NO            DESCRIPTION                               DEBIT             CREDIT            BALANCE"
		@ 06,0 Say Repl('-',137)
		MLine=8								&& Line Number
	Endif
Enddo
MLine=MLine+1
@ MLine,45 Say Repl('-',92)
MLine=MLine+1
@ MLine,48 Say "Total"
@ MLine,59 Say MDTot Pict"999,999,999.99"
@ MLine,77 Say MCTot Pict"999,999,999.99"
@ MLine,95 Say MMB_Amount Pict"999,999,999.99"
@ MLine,110 Say MMB_Cr_Dr
MLine=MLine+1
@ MLine,45 Say Repl('-',92)
Eject
Return
*-----------------------------------
Procedure RepScrPro							&& Report Screen Procedure
*-----------------------------------
Do DspScrPro In Proc1
MLine=8								&& Line Number
MMB_Amount=0						&& Leadger Balance Amount
MMB_Cr_Dr=Spac(2)					&& Leadger Balance Credit / Debit
@ 00,01 Say MComp
@ 01,01 Say "LEDGER FROM: "+Dtoc(MSDate)+"  TO  "+Dtoc(MEDate)
MMC1=Str(C1,2)
MMC2=Str(C2,2)
MMC3=Str(C3,3)
Do Led1CodPro In Proc1
@ 03,01 Say "ACCOUNT CODE : "+MMC1+"-"+MMC2+"-"+MMC3+"  "+"TITLE OF ACCOUNT : "+LdgrCod.Title
@ 04,00 Say Repl('-',78)
@ 05,00 Say "DATE         NO        DESCRIPTION           DEBIT        CREDIT       BALANCE"
@ 06,0 Say Repl('-',78)
If MOBQ="Y"
	@ MLine,19 Say "Opening Balance"
	@ MLine,63 Say MOpBal Pict"9,999,999.99"
	@ MLine,76 Say MOCr_Dr
	MLine=MLine+1
Endif
Do While .Not. Eof()
	@ MLine,00 Say Dtoc(Date)+" "+Dtype.Type1+"-"+Doc_No+" "+Left(Disc,17)
	@ MLine,37 Say IIF(Cr_Dr="DR",(Amount),'') Pict "9,999,999.99"
	@ MLine,51 Say IIF(Cr_Dr="CR",Amount,' ') Pict"9,999,999.99"
	@ MLine,63 Say B_Amount Pict"9,999,999.99"
	@ MLine,76 Say B_Cr_Dr
	MLine=MLine+1
	If Cr_Dr="CR"
		MCTot=MCTot+Amount
	Else
		MDTot=MDTot+Amount
	Endif
	MMB_Amount=B_Amount
	MMB_Cr_Dr=B_Cr_Dr
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
@ MLine,26 Say Repl('-',52)
MLine=MLine+1
@ MLine,28 Say "Total"
@ MLine,37 Say MDTot Pict "9,999,999.99"
@ MLine,51 Say MCTot Pict"9,999,999.99"
@ MLine,63 Say MMB_Amount Pict"9,999,999.99"
@ MLine,76 Say MMB_Cr_Dr
MLine=MLine+1
@ MLine,26 Say Repl('-',52)
@ 21,28 Say "Quit Only (Q)"
@ 21,50 GET MCQ Pict"@!" Valid MCQ="Q" Error 'Quit Only (Q)'
Read
Rele wind scr
Return
