*-------------------------------------------*
*Program to Report Present Stock Status		*
*Author		:	Muhammad Usman				*
*Date		:	29-11-1999					*
*Prog		:	R-SStat						*
*Last Modify:	29-11-1999					*
*-------------------------------------------*
* Select Files
Close Databases

Sele 1
Use Item Alia Item Shared

Sele 2
Use Itran Alia ITran Shared

*-----------------------------------
*Veriable Initialization
MI1=0								&& Item Code 1
MI2=0								&& Item Code 2
MI3=0								&& Item Code 3
MSI3=0								&& Sub Item Code 3
MMI1=0								&& Item Code 1
MMI2=0								&& Item Code 2
MMI3=0								&& Item Code 3
MDate=Ctod("  /  /    ")			&& Start Transaction Date
MPage=0								&& Page No
MLine=0								&& Line Number
MPS=Spac(1)							&& Printer Or Screen
MCQ=Spac(1)							&& Continue Or Quit
MOpt=0								&& Option
*-----------------------------------
* Window Defination
	DEFINE WINDOW rsstat ;
		FROM INT((SROW()-17)/1),INT((SCOL()-76)/1) ;
		TO INT((SROW()-17)/1)+5,INT((SCOL()-76)/1)+71 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW rsstat SAME
*-----------------------------------
Set Century Off
Sele Item
Set Order To I123
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
			Set Filter To Qty>0
		Case MOpt = 2
			Clear
			@ 03,17 SAY "Press F1 to see Item Code List" Color GR+
			ON KEY LABEL F1 DO ShwItmPro In Proc1
			Do While .T.
				@ 01,15 Say 'Enter First Level Code'
				@ 01,47 Get MI1 Pict"@2K 99" Valid MI1#0 Error 'Item Code Can Not Be Blank'
				Read
				Seek Str(MI1,2)
				If .Not. Found()
					Wait Wind 'Item Code Not Exist'
					MI1=0								&& Item Code 1
					Loop
				Else
					Exit
				Endif
			Enddo			
			Set Filter To I1=MI1 .And. Qty>0
		Case MOpt = 3
			Clear
			@ 03,17 SAY "Press F1 to see Item Code List" Color GR+
			ON KEY LABEL F1 DO ShwItmPro In Proc1
			Do While .T.
				@ 01,15 Say 'Enter First & Second Level Code'
				@ 01,47 Get MI1 Pict"@2K 99" Valid MI1#0 Error 'Item Code Can Not Be Blank'
				@ 01,49 Say '-'
				@ 01,50 Get MI2 Pict"@2K 99" Valid MI2#0 Error 'Item Code Can Not Be Blank'
				Read
				Seek Str(MI1,2)+Str(MI2,2)
				If .Not. Found()
					Wait Wind 'Item Code Not Exist'
					MI1=0								&& Item Code 1
					MI2=0								&& Item Code 2
					Loop
				Else
					Exit
				Endif
			Enddo			
			Set Filter To I1=MI1 .And. I2=MI2 .And. Qty>0
		Case MOpt = 4
			Clear
			@ 03,17 SAY "Press F1 to see Item Code List" Color GR+
			ON KEY LABEL F1 DO ShwItmPro In Proc1
			Do While .T.
				@ 01,15 Say 'Enter Item Code'
				@ 01,47 Say "  -  -    "
				@ 01,47 Get MI1 Pict"@2K 99" Valid MI1#0 Error 'Item Code Can Not Be Blank'
				@ 01,50 Get MI2 Pict"@2K 99" Valid MI2#0 Error 'Item Code Can Not Be Blank'
				@ 01,53 Get MI3 Pict"@4K 9999" Valid MI3#0 Error 'Item Code Can Not Be Blank'
				Read
				Seek Str(MI1,2)+Str(MI2,2)+Str(MI3,4)
				If .Not. Found()
					Wait Wind 'Item Code Not Exist'
					MI1=0								&& Item Code 1
					MI2=0								&& Item Code 2
					MI3=0								&& Item Code 3
					Loop
				Endif
				Set Filter To I1=MI1 .And. I2=MI2
				Do While .T.
					@ 03,17 SAY "Press F1 to see Item Code List" Color GR+
					ON KEY LABEL F1 DO Shw1ItmPro In Proc1
					@ 02,47 Say "  -  -    "
					@ 02,47 Say MI1 Pict"@2K 99"
					@ 02,50 Say MI2 Pict"@2K 99"
					@ 02,53 Get MSI3 Pict"@3K 9999" Valid MSI3#0 Error 'Item Code Can Not Be Blank'
					Read
					Seek Str(MI1,2)+Str(MI2,2)+Str(MSI3,4)
					If .Not. Found()
						Wait Wind 'Item Code Not Exist'
						MSI3=0								&& Sub Item Code 3
						Loop
					Else
						Exit
					Endif
				Enddo
				Exit
			Enddo			
			Set Filter To I1=MI1 .And. I2=MI2 .And. (I3>=MI3 .And. I3<=MSI3) .And. Qty>0
		Case MOpt = 5
			Close Databases
			Rele Wind rsstat
			Return
	Endcase
Clear
@ 2,12 Say "Report To Printer Or Screen (P/S)" Color W/N
@ 2,48 GET MPS Pict"@!" Valid MPS="P" .Or. MPS="S" Error 'Printer Or Screen (P/S)' Color W/N
Read
Clear
@ 01,25 SAY "Report in Processing"
@ 03,25 SAY "Please Don't Distrub"
Sele Itran
Set Filter To Flag="Y"
Set Order To Date
Goto Bott
MDate=Date
Sele Item
Set Order To I123
Goto Top
MTitle="PRESENT STOCK STATUS"
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind rsstat
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
@ 04,00 Say "Item Code   Description                   Unit   Pur     Sale      Qty"
@ 05,00 Say "                                                 Rate    Rate"
@ 06,00 Say Repl('-',80)
MLine=7								&& Line Number
Do While .Not. Eof()
	MMI1=Str(I1,2)
	MMI2=Str(I2,2)
	MMI3=Str(I3,4)
	Do Itm1CodPro In Proc1
	@ MLine,00 Say MMI1+"-"+MMI2+"-"+MMI3+" "+Item+" "+Unit+" "+Str(Rate,7,2)+" "+Str(S_Rate,7,2)+" "+Str(Qty,12,4)
	Skip
	MLine=MLine+1
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
		@ 02,01 Say "As On  : "+Dtoc(MSDate)
		@ 02,68 Say "Page # "+Str(MPage,4)
		@ 04,00 Say Repl('-',80)
		@ 04,00 Say "Item Code   Description                   Unit   Pur     Sale      Qty"
		@ 05,00 Say "                                                 Rate    Rate"
		@ 06,00 Say Repl('-',80)
	Endif
Enddo
Eject
Return
*-----------------------------------
Procedure RepScrPro							&& Report Screen Procedure
*-----------------------------------
Do DspScrPro In Proc1
MLine=7								&& Line Number
@ 00,01 Say MComp
@ 01,01 Say MTitle
@ 02,01 Say "As On :  "+Dtoc(MDate)
@ 03,00 Say Repl('-',78)
@ 04,00 Say "Item Code   Description                   Unit   Pur     Sale      Qty"
@ 05,00 Say "                                                 Rate    Rate"
@ 06,00 Say Repl('-',78)
Do While .Not. Eof()
	MMI1=Str(I1,2)
	MMI2=Str(I2,2)
	MMI3=Str(I3,4)
	Do Itm1CodPro In Proc1
	@ MLine,00 Say MMI1+"-"+MMI2+"-"+MMI3+" "+Item+" "+Unit+" "+Str(Rate,7,2)+" "+Str(S_Rate,7,2)+" "+Str(Qty,12,4)
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
@ 21,28 Say "Quit Only (Q)"
@ 21,50 GET MCQ Pict"@!" Valid MCQ="Q" Error 'Quit Only (Q)'
Read
Rele wind scr
Return
