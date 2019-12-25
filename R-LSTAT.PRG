*-------------------------------------------*
*Program to Report Present Leadger Status	*
*Author		:	Muhammad Usman				*
*Date		:	31-01-2000					*
*Prog		:	R-LStat						*
*Last Modify:	10-02-2000					*
*-------------------------------------------*
* Select Files
Close Databases

Sele 1
Use Ldgrcod Alia Ldgrcod Shared

Sele 2
Use Tran2 Alia Tran2 Shared

*-----------------------------------
*Veriable Initialization
MC1=0								&& Party Code 1
MC2=0								&& Party Code 2
MC3=0								&& Party Code 3
MSC3=0								&& Sub Party Code 3
MMC1=0								&& Party Code 1
MMC2=0								&& Party Code 2
MMC3=0								&& Party Code 3
MDate=Ctod("  /  /    ")			&& End Transaction Date
MDTot=0								&& Debit Total
MCTot=0								&& Credit Total
MPage=0								&& Page No
MLine=0								&& Line Number
MPS=Spac(1)							&& Printer Or Screen
MCQ=Spac(1)							&& Continue Or Quit
MOpt=0								&& Option
*-----------------------------------
* Window Defination
	DEFINE WINDOW rlstat ;
		FROM INT((SROW()-17)/1),INT((SCOL()-76)/1) ;
		TO INT((SROW()-17)/1)+5,INT((SCOL()-76)/1)+71 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW rlstat SAME
*-----------------------------------
Sele Ldgrcod
Set Order To C123
Clear
Do While .T.
	@ 01,01 Get MOpt Func '*H \<All;\<First Level;\<Second Level;\<Third Level;\<Cancel' Size 1,12 Color Scheme 19
	Read
    If Readkey() = 15 .or. Readkey() = 271			&& Enter Key
		Exit
    Endif
Enddo
	Do Case
		Case MOpt = 1
			Set Filter To Ldgr_Bal>0
		Case MOpt = 2
			Clear
			@ 03,17 SAY "Press F1 to see Leadger Code List" Color GR+
			ON KEY LABEL F1 DO ShwLdgPro In Proc1
			Do While .T.
				@ 01,15 Say 'Enter First Level Code'
				@ 01,47 Get MC1 Pict"@2K 99" Valid MC1#0 Error 'Ledger Head Code Can Not Be Blank'
				Read
				Seek Str(MC1,2)
				If .Not. Found()
					Wait Wind 'Leadger Code Not Exist'
					MC1=0								&& Leadger Code 1
					Loop
				Else
					Exit
				Endif
			Enddo			
			Set Filter To C1=MC1 .And. Ldgr_Bal>0
		Case MOpt = 3
			Clear
			@ 03,17 SAY "Press F1 to see Leadger Code List" Color GR+
			ON KEY LABEL F1 DO ShwLdgPro In Proc1
			Do While .T.
				@ 01,15 Say 'Enter First & Second Level Code'
				@ 01,47 Get MC1 Pict"@2K 99" Valid MC1#0 Error 'Ledger Head Code Can Not Be Blank'
				@ 01,49 Say '-'
				@ 01,50 Get MC2 Pict"@2K 99" Valid MC2#0 Error 'Ledger Head Code Can Not Be Blank'
				Read
				Seek Str(MC1,2)+Str(MC2,2)
				If .Not. Found()
					Wait Wind 'Leadger Code Not Exist'
					MC1=0								&& Leadger Code 1
					MC2=0								&& Leadger Code 2
					Loop
				Else
					Exit
				Endif
			Enddo			
			Set Filter To C1=MC1 .And. C2=MC2 .And. Ldgr_Bal>0
		Case MOpt = 4
			Clear
			@ 03,17 SAY "Press F1 to see Leadger Code List" Color GR+
			ON KEY LABEL F1 DO ShwLdgPro In Proc1
			Do While .T.
				@ 01,15 Say 'Enter Leadger Code'
				@ 01,47 Say "  -  -   "
				@ 01,47 Get MC1 Pict"@2K 99" Valid MC1#0 Error 'Ledger Head Code Can Not Be Blank'
				@ 01,50 Get MC2 Pict"@2K 99" Valid MC2#0 Error 'Ledger Head Code Can Not Be Blank' 
				@ 01,53 Get MC3 Pict"@3K 999" Valid MC3#0 Error 'Ledger Head Code Can Not Be Blank'
				Read
				Seek Str(MC1,2)+Str(MC2,2)+Str(MC3,3)
				If .Not. Found()
					Wait Wind 'Leadger Code Not Exist'
					MC1=0								&& Party Code 1
					MC2=0								&& Party Code 2
					MC3=0								&& Party Code 3
					Loop
				Endif
				Set Filter To C1=MC1 .And. C2=MC2
				Do While .T.
					@ 03,17 SAY "Press F1 to see Leadger Code List" Color GR+
					ON KEY LABEL F1 DO Shw1LdgPro In Proc1
					@ 02,47 Say "  -  -   "
					@ 02,47 Say MC1 Pict"@2K 99"
					@ 02,50 Say MC2 Pict"@2K 99"
					@ 02,53 Get MSC3 Pict"@3K 999" Valid MC3#0 Error 'Ledger Head Code Can Not Be Blank'
					Read
					Seek Str(MC1,2)+Str(MC2,2)+Str(MSC3,3)
					If .Not. Found()
						Wait Wind 'Leadger Code Not Exist'
						MSC3=0								&& Sub Leadger Code 3
						Loop
					Else
						Exit
					Endif
				Enddo
				Exit
			Enddo			
			Set Filter To C1=MC1 .And. C2=MC2 .And. (C3>=MC3 .And. C3<=MSC3) .And. Ldgr_Bal>0
		Case MOpt = 5
			Close Databases
			Rele Wind rpstat
			Return
	Endcase
Clear
@ 2,12 Say "Report To Printer Or Screen (P/S)" Color W/N
@ 2,48 GET MPS Pict"@!" Valid MPS="P" .Or. MPS="S" Error 'Printer Or Screen (P/S)' Color W/N
Read
Clear
@ 01,25 SAY "Report in Processing"
@ 03,25 SAY "Please Don't Distrub"
Sele Tran2
Set Order To Date
Goto Bott
MDate=Date
Sele Ldgrcod
Set Order To C123
Goto Top
MTitle="PRESENT LEDGER STATUS"
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind rlstat
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
@ 02,01 Say "As On  : "+Dtoc(MDate)
@ 02,68 Say "Page # "+Str(MPage,4)
@ 03,00 Say Repl('-',80)
@ 04,00 Say "Party Code   Description                         Dr               Cr"
@ 05,00 Say Repl('-',80)
MLine=6								&& Line Number
Sum Ldgr_Bal To MCTot For Status="CR"
Sum Ldgr_Bal To MDTot For Status="DR"
@ MLine,00 Say Repl('-',80)
MLine=MLine+1
@ MLine,20 Say 'Total'
@ MLine,44 Say MDTot Pict"999,999,999.99"
@ MLine,59 Say MCTot Pict"999,999,999.99"
MLine=MLine+1
@ MLine,00 Say Repl('-',80)
MLine=MLine+1
Goto Top
Do While .Not. Eof()
	MMC1=Str(C1,2)
	MMC2=Str(C2,2)
	MMC3=Str(C3,3)
	Do Led1CodPro In Proc1
	@ MLine,00 Say MMC1+"-"+MMC2+"-"+MMC3+"  "+Title
	@ MLine,44 Say IIF(Status="DR",(Ldgr_bal),'') Pict"999,999,999.99"
	@ MLine,59 Say IIF(Status="CR",(Ldgr_bal),'') Pict"999,999,999.99"
	@ MLine,00 Say Repl('_',80)
	MLine=MLine+1
	Skip
	If MLine>58
		Eject
		MPage=MPage+1
		MLine=6								&& Line Number
		@ 00,00 Say Chr(18)
		@ 00,00 Say Chr(14)
		@ 00,01 Say MComp
		@ 01,00 Say Chr(18)
		@ 01,00 Say Chr(14)
		@ 01,01 Say MTitle
		@ 02,00 Say Chr(18)
		@ 02,01 Say "As On  : "+Dtoc(MDate)
		@ 02,68 Say "Page # "+Str(MPage,4)
		@ 03,00 Say Repl('-',80)
		@ 04,00 Say "Party Code   Description                         Dr               Cr"
		@ 05,00 Say Repl('-',80)
	Endif
Enddo
Eject
Return
*-----------------------------------
Procedure RepScrPro							&& Report Screen Procedure
*-----------------------------------
Do DspScrPro In Proc1
MLine=6								&& Line Number
@ 00,01 Say MComp
@ 01,01 Say MTitle
@ 02,01 Say "As On :  "+Dtoc(MDate)
@ 03,00 Say Repl('-',78)
@ 04,00 Say "Party Code   Description                         Dr               Cr"
@ 05,00 Say Repl('-',78)
Sum Ldgr_Bal To MCTot For Status="CR"
Sum Ldgr_Bal To MDTot For Status="DR"
@ MLine,00 Say Repl('-',78)
MLine=MLine+1
@ MLine,20 Say 'Total'
@ MLine,44 Say MDTot Pict"999,999,999.99"
@ MLine,59 Say MCTot Pict"999,999,999.99"
MLine=MLine+1
@ MLine,00 Say Repl('-',78)
MLine=MLine+1
Goto Top
Do While .Not. Eof()
	MMC1=Str(C1,2)
	MMC2=Str(C2,2)
	MMC3=Str(C3,3)
	Do Led1CodPro In Proc1
	@ MLine,00 Say MMC1+"-"+MMC2+"-"+MMC3+"  "+Title
	@ MLine,44 Say IIF(Status="DR",(Ldgr_bal),'') Pict"999,999,999.99"
	@ MLine,59 Say IIF(Status="CR",(Ldgr_bal),'') Pict"999,999,999.99"
	MLine=MLine+1
	Skip
	If MLine>18
		@ 21,28 Say "Continue Or Quit C/Q"
		@ 21,50 GET MCQ Pict"@!" Valid MCQ="C" .Or. MCQ="Q" Error 'Continue Or Quit (C/Q)'
		Read
		If MCQ="Q"
			Rele wind scr
			Return
		Endif	
		@ 6,0 To 21,78 Clear
		MLine=6
		Endif
Enddo
MLine=MLine+1
@ MLine,00 Say Repl('-',78)
@ 21,28 Say "Quit Only (Q)"
@ 21,50 GET MCQ Pict"@!" Valid MCQ="Q" Error 'Quit Only (Q)'
Read
Rele wind scr
Return
