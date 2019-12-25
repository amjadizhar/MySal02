*-----------------------------------*
*Program to Report Zone Wise Sale	*
*Author		:	Muhammad Usman		*
*Date		:	17-04-2000			*
*Prog		:	R-Zone				*
*Last Modify:	17-04-2000			*
*-----------------------------------*
* Select Files
Close Databases

Sele 1
Use ITran Alia ITran Shared

Sele 2
Use LdgrCod Alia LdgrCod Shared
Set Order To C123

Sele 3
Use City Alia City Shared
Set Order To C_Code

Tmp=Sys(3)
Create Table (Tmp) (Zone C(4), TS N(5), TC N(5), RS N(5), RC N(5), BS N(5), BC N(5), PC N(5), PH N(5), Amount N(11,2))
Sele 4
Use (Tmp) Alia Tmp Excl
Index On Zone Tag Zone

*-----------------------------------
*Veriable Initialization
MSDate=Ctod("  /  /    ")			&& Start Transaction Date
MEDate=Ctod("  /  /    ")			&& End Transaction Date
MI3=0								&& Item Last Code
MZone=Spac(4)						&& Zone Specfic
MQty=0								&& Item Quantity
MAmount=0							&& Item Amount
MTS=0								&& Tikki Spicy
MTC=0								&& Tikki Chicken
MRS=0								&& Regular Spicy
MRC=0								&& Regular Chicken
MBS=0								&& Big Bag Spicy
MBC=0								&& Big Bag Chicken
MPC=0								&& Party Puff Chess
MPH=0								&& Party Puff Hot&Spicy
MPage=0								&& Page No
MLine=0								&& Line Number
MPS=Spac(1)							&& Printer Or Screen
MCQ=Spac(1)							&& Continue Or Quit
MSr_No=0							&& Serial Number
*-----------------------------------
* Window Defination
	DEFINE WINDOW rzone ;
		FROM INT((SROW()-17)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-17)/1)+9,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW rzone SAME
*-----------------------------------
Clear
@ 2,05 Say "Date From:" Color W/N
@ 4,05 Say "Date To:" Color W/N
@ 2,30 GET MSDate Pict("99/99/9999")
@ 4,30 GET MEDate Pict("99/99/9999")
Read
If Read()=12 .Or. Read()=268				&& Escape Key
	Close Databases
	Dele File Tmp+'.Dbf'
	Dele File Tmp+'.Cdx'
	Rele Wind rzone
	Return
	Exit
Endif
Clear
@ 4,05 Say "Report To Printer Or Screen (P/S)" Color W/N
@ 4,40 GET MPS Pict"@!" Valid MPS="P" .Or. MPS="S" Error 'Printer Or Screen (P/S)' Color W/N
Read
Clear
@ 03,13 SAY "Report in Processing"
@ 05,13 SAY "Please Don't Distrub"
Sele ITran
Set Relation To Str(C1,2)+Str(C2,2)+Str(C3,3) Into LdgrCod Additive
Set Filter To Date>=MSDate .And. Date<=MEDate
Set Order To C123
Goto Top
Do While .Not. Eof()
	MI3=0								&& Item Last Code
	MQty=0								&& Item Quantity
	MAmount=0							&& Item Amount
	MZone=Spac(4)						&& Zone Specfic
	MI3=I3
	MQty=Qty
	MZone=LdgrCod.C_Code
	MAmount=NAmount
	Sele Tmp
	Set Order To Zone
	Seek MZone
	If .Not. Found()
		Append Blank
		Replace Zone With MZone
	Endif	
	If MI3=401 .Or. MI3=701
		Replace TS With TS+MQty
	Endif
	If MI3=402 .Or. MI3=702
		Replace TC With TC+MQty
	Endif
	If MI3=411 .Or. MI3=711
		Replace RS With RS+MQty
	Endif
	If MI3=412 .Or. MI3=712
		Replace RC With RC+MQty
	Endif
	If MI3=421 .Or. MI3=721
		Replace BS With BS+MQty
	Endif
	If MI3=422 .Or. MI3=722
		Replace BC With BC+MQty
	Endif
	If MI3=431 .Or. MI3=731
		Replace PC With PC+MQty
	Endif
	If MI3=432 .Or. MI3=732
		Replace PH With PH+MQty
	Endif
	Replace Amount With Amount+MAmount
	Sele ITran
	Skip
Enddo
Sele Tmp
Set Relation To Zone Into City Additive
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind rzone
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
@ 01,01 Say "REPORT FROM: "+Dtoc(MSDate)+"  TO  "+Dtoc(MEDate)
@ 02,00 Say Chr(18)
@ 02,01 Say "Date :"+Dtoc(Date())+"           "+"Page# "+Str(MPage,4)
@ 04,00 Say Chr(15)
@ 04,00 Say Repl('-',137)
@ 05,00 Say "CODE             CITY                AMOUNT          TIKKI          REGUALR         BIG BAG        PARTY PUF      TOTAL"
@ 06,00 Say "                                                  SPICY   CHCKN   SPICY   CHCKN   SPICY   CHCKN   CHEES    HOT    CARTNS"
@ 07,00 Say Repl('-',137)
MLine=13							&& Line Number
MAmount=0							&& Item Amount
MTS=0								&& Tikki Spicy
MTC=0								&& Tikki Chicken
MRS=0								&& Regular Spicy
MRC=0								&& Regular Chicken
MBS=0								&& Big Bag Spicy
MBC=0								&& Big Bag Chicken
MPC=0								&& Party Puff Chess
MPH=0								&& Party Puff Hot&Spicy
Sum Amount, TS, TC, RS, RC, BS, BC, PC, PH To MAmount, MTS, MTC, MRS, MRC, MBS, MBC, MPC, MPH
@ 08,00 Say 'TOTAL SUMMARY                       '+Str(MAmount,11,2)+'   '+Str(MTS,5)+'   '+Str(MTC,5)+'   '+Str(MRS,5)+'   '+Str(MRC,5)+'   '+Str(MBS,5)+'   '+Str(MBC,5)+'   '+Str(MPC,5)+'   '+Str(MPH,5)+'   '+Str(MTS+MTC+MRS+MRC+MBS+MBC+MPC+MPH,5)
@ 09,00 Say Repl('-',137)
MAmount=0							&& Item Amount
MTS=0								&& Tikki Spicy
MTC=0								&& Tikki Chicken
MRS=0								&& Regular Spicy
MRC=0								&& Regular Chicken
MBS=0								&& Big Bag Spicy
MBC=0								&& Big Bag Chicken
MPC=0								&& Party Puff Chess
MPH=0								&& Party Puff Hot&Spicy
Sum Amount, TS, TC, RS, RC, BS, BC, PC, PH To MAmount, MTS, MTC, MRS, MRC, MBS, MBC, MPC, MPH For Zone="N"
@ 10,00 Say 'TOTAL NORTH                         '+Str(MAmount,11,2)+'   '+Str(MTS,5)+'   '+Str(MTC,5)+'   '+Str(MRS,5)+'   '+Str(MRC,5)+'   '+Str(MBS,5)+'   '+Str(MBC,5)+'   '+Str(MPC,5)+'   '+Str(MPH,5)+'   '+Str(MTS+MTC+MRS+MRC+MBS+MBC+MPC+MPH,5)
MAmount=0							&& Item Amount
MTS=0								&& Tikki Spicy
MTC=0								&& Tikki Chicken
MRS=0								&& Regular Spicy
MRC=0								&& Regular Chicken
MBS=0								&& Big Bag Spicy
MBC=0								&& Big Bag Chicken
MPC=0								&& Party Puff Chess
MPH=0								&& Party Puff Hot&Spicy
Sum Amount, TS, TC, RS, RC, BS, BC, PC, PH To MAmount, MTS, MTC, MRS, MRC, MBS, MBC, MPC, MPH For Zone="S"
@ 11,00 Say 'TOTAL SOUTH                         '+Str(MAmount,11,2)+'   '+Str(MTS,5)+'   '+Str(MTC,5)+'   '+Str(MRS,5)+'   '+Str(MRC,5)+'   '+Str(MBS,5)+'   '+Str(MBC,5)+'   '+Str(MPC,5)+'   '+Str(MPH,5)+'   '+Str(MTS+MTC+MRS+MRC+MBS+MBC+MPC+MPH,5)
@ 12,00 Say Repl('-',137)
Set Order To Zone
Goto Top
Do While .Not. Eof()
	@ MLine,00 Say Zone+' '+City.Name+' '+Str(Amount,11,2)+'   '+Str(TS,5)+'   '+Str(TC,5)+'   '+Str(RS,5)+'   '+Str(RC,5)+'   '+Str(BS,5)+'   '+Str(BC,5)+'   '+Str(PC,5)+'   '+Str(PH,5)+'   '+Str(TS+TC+RS+RC+BS+BC+PC+PH,5)
	Skip
	MLine = MLine + 1
	If MLine>59
		Eject
		MPage=MPage+1
		@ 00,00 Say Chr(18)
		@ 00,00 Say Chr(14)
		@ 00,01 Say MComp
		@ 01,00 Say Chr(18)
		@ 01,01 Say "REPORT FROM: "+Dtoc(MSDate)+"  TO  "+Dtoc(MEDate)
		@ 02,00 Say Chr(18)
		@ 02,01 Say "Date :"+Dtoc(Date())+"           "+"Page# "+Str(MPage,4)
		@ 04,00 Say Chr(15)
		@ 04,00 Say Repl('-',137)
		@ 05,00 Say "CODE             CITY                AMOUNT          TIKKI          REGUALR         BIG BAG        PARTY PUF      TOTAL"
		@ 06,00 Say "                                                  SPICY   CHCKN   SPICY   CHCKN   SPICY   CHCKN   CHEES    HOT    CARTNS"
		@ 07,00 Say Repl('-',137)
		MLine=8							&& Line Number
	Endif
Enddo
MLine = MLine + 1
@ MLine,00 Say Repl('-',137)
Eject
Return
*-----------------------------------
Procedure RepScrPro							&& Report Screen Procedure
*-----------------------------------
Do DspScrPro In Proc1
MLine=13								&& Line Number
@ 00,01 Say MComp
@ 01,01 Say "REPORT FROM: "+Dtoc(MSDate)+"  TO  "+Dtoc(MEDate)
@ 02,01 Say "DATE  "+DtoC(Date())
@ 04,00 Say Repl('-',78)
@ 05,00 Say "CODE CITY       AMOUNT      TIKKI      REGUALR     BIG BAG    PARTY PUF TOTAL"
@ 06,00 Say "                         SPICY CHCKN SPICY CHCKN SPICY CHCKN CHEES  HOT CARTNS"
@ 07,00 Say Repl('-',78)
MAmount=0							&& Item Amount
MTS=0								&& Tikki Spicy
MTC=0								&& Tikki Chicken
MRS=0								&& Regular Spicy
MRC=0								&& Regular Chicken
MBS=0								&& Big Bag Spicy
MBC=0								&& Big Bag Chicken
MPC=0								&& Party Puff Chess
MPH=0								&& Party Puff Hot&Spicy
Sum Amount, TS, TC, RS, RC, BS, BC, PC, PH To MAmount, MTS, MTC, MRS, MRC, MBS, MBC, MPC, MPH
@ 08,00 Say 'TOTAL SUMMARY'+Str(MAmount,11,2)+' '+Str(MTS,5)+' '+Str(MTC,5)+' '+Str(MRS,5)+' '+Str(MRC,5)+' '+Str(MBS,5)+' '+Str(MBC,5)+' '+Str(MPC,5)+' '+Str(MPH,5)+' '+Str(MTS+MTC+MRS+MRC+MBS+MBC+MPC+MPH,5)
@ 09,00 Say Repl('-',78)
MAmount=0							&& Item Amount
MTS=0								&& Tikki Spicy
MTC=0								&& Tikki Chicken
MRS=0								&& Regular Spicy
MRC=0								&& Regular Chicken
MBS=0								&& Big Bag Spicy
MBC=0								&& Big Bag Chicken
MPC=0								&& Party Puff Chess
MPH=0								&& Party Puff Hot&Spicy
Sum Amount, TS, TC, RS, RC, BS, BC, PC, PH To MAmount, MTS, MTC, MRS, MRC, MBS, MBC, MPC, MPH For Zone="N"
@ 10,00 Say 'TOTAL NORTH  '+Str(MAmount,11,2)+' '+Str(MTS,5)+' '+Str(MTC,5)+' '+Str(MRS,5)+' '+Str(MRC,5)+' '+Str(MBS,5)+' '+Str(MBC,5)+' '+Str(MPC,5)+' '+Str(MPH,5)+' '+Str(MTS+MTC+MRS+MRC+MBS+MBC+MPC+MPH,5)
MAmount=0							&& Item Amount
MTS=0								&& Tikki Spicy
MTC=0								&& Tikki Chicken
MRS=0								&& Regular Spicy
MRC=0								&& Regular Chicken
MBS=0								&& Big Bag Spicy
MBC=0								&& Big Bag Chicken
MPC=0								&& Party Puff Chess
MPH=0								&& Party Puff Hot&Spicy
Sum Amount, TS, TC, RS, RC, BS, BC, PC, PH To MAmount, MTS, MTC, MRS, MRC, MBS, MBC, MPC, MPH For Zone="S"
@ 11,00 Say 'TOTAL SOUTH  '+Str(MAmount,11,2)+' '+Str(MTS,5)+' '+Str(MTC,5)+' '+Str(MRS,5)+' '+Str(MRC,5)+' '+Str(MBS,5)+' '+Str(MBC,5)+' '+Str(MPC,5)+' '+Str(MPH,5)+' '+Str(MTS+MTC+MRS+MRC+MBS+MBC+MPC+MPH,5)
@ 12,00 Say Repl('-',78)
Set Order To Zone
Goto Top
Do While .Not. Eof()
	@ MLine,00 Say Zone+' '+Left(City.Name,7)+' '+Str(Amount,11,2)+' '+Str(TS,5)+' '+Str(TC,5)+' '+Str(RS,5)+' '+Str(RC,5)+' '+Str(BS,5)+' '+Str(BC,5)+' '+Str(PC,5)+' '+Str(PH,5)+' '+Str(TS+TC+RS+RC+BS+BC+PC+PH,5)
	Skip
	MLine = MLine + 1
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
@ 21,28 Say "Quit Only (Q)"
@ 21,50 GET MCQ Pict"@!" Valid MCQ="Q" Error 'Quit Only (Q)'
Read
Rele wind scr
Return
