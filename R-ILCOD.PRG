*-----------------------------------*
*Program to Report Party Codes		*
*Author		:	Muhammad Usman		*
*Date		:	21-10-1999			*
*Prog		:	R-IlCod				*
*Last Modify:	21-10-1999			*
*-----------------------------------*
* Select Files
Close Databases

Sele 1
Use ILdgrcod Alia ILdgrcod Shared

*-----------------------------------
*Veriable Initialization
MC1=Spac(2)							&& First Party Code
MC2=Spac(2)							&& Second Party Code
MC3=Spac(3)							&& Third Party Code
MTitl=Spac(40)						&& Report Title
MCN=Spac(1)							&& Code / Name
MPS=Spac(1)							&& Printer Or Screen
MPage=1								&& Page No
MLine=0								&& Line Number
MCQ=Spac(1)							&& Continue Or Quit
MSr_No=0							&& Serial Number
*-----------------------------------
* Window Defination
	DEFINE WINDOW rilcod ;
		FROM INT((SROW()-17)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-17)/1)+9,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW rilcod SAME
*-----------------------------------
Sele ILdgrcod
Clear
@ 4,05 Say "Party Code Wise / Party Name Wise" Color W/N
@ 4,40 GET MCN Pict"@!" Valid MCN="C" .Or. MCN="N" Error 'Code / Name (C/N)' Color W/N
Read
Clear
@ 03,13 SAY "Report in Processing"
@ 05,13 SAY "Please Don't Distrub"
If MCN="C"
	Set Order To C123
	MTitle="Code Wise List Of All Parties"
Else
	Set Order To Title
	MTitle="Name Wise List Of All Parties"
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
Rele wind rilcod
Set Print Off
Set Device To Screen
Close Databases
Return
*-----------------------------------
Procedure RepPrtPro							&& Report Print Procedure
*-----------------------------------
Set Print On
Set Device To Print
MLine=6								&& Line Number
@ 01,01 Say MComp
@ 01,67 Say 'Page: '+Str(MPage,2)
@ 02,01 Say MTitle
@ 02,62 Say "Date: "+Dtoc(Date())
@ 03,00 Say Repl('-',80)
@ 04,00 Say "SERIAL    CODE          NAME                            OPENING   STATUS"
@ 05,00 Say "NUMBER                                                  BALANCE"
@ 06,00 Say Repl('-',80)
Do While .Not. Eof()
	MLine=MLine+1	
	MSr_No=MSr_No+1
	If MCN="C"
		If Str(C1,2)#MC1
			MLine=MLine+1
		Endif
	Endif
	MC1=Str(C1,2)
	MC2=Str(C2,2)
	MC3=Str(C3,3)
	Do LedCodPro In Proc1
	@ MLine,00 Say Str(MSr_No,4)+"   "+MC1+"-"+MC2+"-"+MC3+"  "+Title+"  "+Str(Open_Bal,12,2)+"      "+Open_Stat
	Skip
	If MLine>58
		Eject
		MPage=MPage+1
		MLine=6
		@ 01,01 Say MComp
		@ 01,67 Say 'Page: '+Str(MPage,2)
		@ 02,01 Say MTitle
		@ 02,62 Say "Date: "+Dtoc(Date())
		@ 03,00 Say Repl('-',77)
		@ 04,00 Say "SERIAL    CODE          NAME                            OPENING   STATUS"
		@ 05,00 Say "NUMBER                                                  BALANCE"
		@ 06,00 Say Repl('-',77)
	Endif
Enddo
MLine=MLine+1
@ MLine,00 Say Repl('-',77)
MLine=MLine+1
@ MLine,65 Say "End Of Report"
Eject
Return
*-----------------------------------
Procedure RepScrPro							&& Report Screen Procedure
*-----------------------------------
Do DspScrPro In Proc1
MLine=6								&& Line Number
@ 01,01 Say MComp
@ 02,01 Say MTitle
@ 02,62 Say "Date: "+Dtoc(Date())
@ 03,00 Say Repl('-',78)
@ 04,00 Say "SERIAL    CODE          NAME                            OPENING   STATUS"
@ 05,00 Say "NUMBER                                                  BALANCE"
@ 06,00 Say Repl('-',78)
Do While .Not. Eof()
	MLine=MLine+1	
	MSr_No=MSr_No+1
	If MCN="C"
		If Str(C1,2)#MC1
			MLine=MLine+1
		Endif
	Endif
	MC1=Str(C1,2)
	MC2=Str(C2,2)
	MC3=Str(C3,3)
	Do LedCodPro In Proc1
	@ MLine,00 Say Str(MSr_No,4)+"   "+MC1+"-"+MC2+"-"+MC3+"  "+Title+"  "+Str(Open_Bal,12,2)+"      "+Open_Stat
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
