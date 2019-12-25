*-----------------------------------*
*Program to Report Item Codes		*
*Author		:	Muhammad Usman		*
*Date		:	29-01-2000			*
*Prog		:	R-ItmCod			*
*Last Modify:	29-01-2000			*
*-----------------------------------*
* Select Files
Close Databases

Sele 1
Use Item Alia Item Shared

*-----------------------------------
*Veriable Initialization
MI1=Spac(2)							&& First Item Code
MI2=Spac(2)							&& Second Item Code
MI3=Spac(4)							&& Third Item Code
MCN=Spac(1)							&& Code / Name
MPS=Spac(1)							&& Printer Or Screen
MPage=1								&& Page No
MLine=0								&& Line Number
MCQ=Spac(1)							&& Continue Or Quit
MSr_No=0							&& Serial Number
*-----------------------------------
* Window Defination
	DEFINE WINDOW ritmcod ;
		FROM INT((SROW()-17)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-17)/1)+9,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW ritmcod SAME
*-----------------------------------
Sele Item
Clear
@ 4,05 Say "Item Code Wise / Item Name Wise" Color W/N
@ 4,40 GET MCN Pict"@!" Valid MCN="C" .Or. MCN="N" Error 'Code / Name (C/N)' Color W/N
Read
Clear
@ 03,13 SAY "Report in Processing"
@ 05,13 SAY "Please Don't Distrub"
If MCN="C"
	Set Order To I123
	MTitle="Code Wise List Of All Items"
Else
	Set Order To Item
	MTitle="Name Wise List Of All Items"
Endif
Goto Top
Clear
@ 4,05 Say "Report To Printer Or Screen (P/S)" Color W/N
@ 4,40 GET MPS Pict"@!" Valid MPS="P" .Or. MPS="S" Error 'Printer Or Screen (P/S)' Color W/N
Read
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind ritmcod
Set Print Off
Set Device To Screen
Close Databases
Return
*-----------------------------------
Procedure RepPrtPro							&& Report Print Procedure
*-----------------------------------
Set Print On
Set Device To Print
MLine=5								&& Line Number
@ 01,01 Say MComp
@ 01,67 Say 'Page: '+Str(MPage,2)
@ 02,01 Say MTitle
@ 02,62 Say "Date: "+Dtoc(Date())
@ 03,00 Say Repl('-',78)
@ 04,00 Say "S.R#    CODE         NAME                             RATE  SALES TAX %"
@ 05,00 Say Repl('-',78)
Do While .Not. Eof()
	MLine=MLine+1	
	MSr_No=MSr_No+1
	If MCN="C"
		If Str(I1,2)#MI1
			MLine=MLine+1
		Endif
	Endif
	MI1=Str(I1,2)
	MI2=Str(I2,2)
	MI3=Str(I3,4)
	Do ItmCodPro In Proc1
	@ MLine,00 Say Str(MSr_No,4)+"  "+MI1+"-"+MI2+"-"+MI3+" "+Item+"     "+Str(Rate,4)+"   "+Str(STax,2)
	Skip
	If MLine>58
		Eject
		MPage=MPage+1
		MLine=5
		@ 01,01 Say MComp
		@ 01,67 Say 'Page: '+Str(MPage,2)
		@ 02,01 Say MTitle
		@ 02,62 Say "Date: "+Dtoc(Date())
		@ 03,00 Say Repl('-',78)
		@ 04,00 Say "S.R#    CODE         NAME                             RATE  SALES TAX %"
		@ 05,00 Say Repl('-',78)
	Endif
Enddo
MLine=MLine+1
@ MLine,00 Say Repl('-',78)
MLine=MLine+1
@ MLine,65 Say "End Of Report"
Eject
Return
*-----------------------------------
Procedure RepScrPro							&& Report Screen Procedure
*-----------------------------------
Do DspScrPro In Proc1
MLine=5								&& Line Number
@ 01,01 Say MComp
@ 02,01 Say MTitle
@ 02,62 Say "Date: "+Dtoc(Date())
@ 03,00 Say Repl('-',78)
@ 04,00 Say "S.R#    CODE         NAME                             RATE  SALES TAX %"
@ 05,00 Say Repl('-',78)
Do While .Not. Eof()
	MLine=MLine+1	
	MSr_No=MSr_No+1
	If MCN="C"
		If Str(I1,2)#MI1
			MLine=MLine+1
		Endif
	Endif
	MI1=Str(I1,2)
	MI2=Str(I2,2)
	MI3=Str(I3,4)
	Do ItmCodPro In Proc1
	@ MLine,00 Say Str(MSr_No,4)+"  "+MI1+"-"+MI2+"-"+MI3+" "+Item+"     "+Str(Rate,6,2)+"   "+Str(STax,2)
	Skip
	If MLine>18
		@ 21,28 Say "Continue Or Quit C/Q"
		@ 21,50 GET MCQ Pict"@!" Valid MCQ="C" .Or. MCQ="Q" Error 'Continue Or Quit (C/Q)'
		Read
		If MCQ="Q"
			Rele wind scr
			Return
		Endif	
		@ 7,0 To 21,78 Clear
		MLine=6
	Endif
Enddo
Wait Wind 'End of Report.....'
Rele wind scr
Return
