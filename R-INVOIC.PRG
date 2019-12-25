*-----------------------------------*
*Program to Report Invoice			*
*Author		:	Muhammad Usman		*
*Date		:	22-10-1999			*
*Prog		:	R-Invoic			*
*Last Modify:	22-10-1999			*
*-----------------------------------*
* Select Files
Close Databases

Sele 1
Use ITran Alia ITran Shared

Sele 2
Use ILdgrCod Alia ILdgrCod Shared
Set Order To C123

Sele 3
Use Item Alia Item Shared
Set Order To I123

Sele 4
Use ITran-Ke Alia ITranKe Shared

Sele 5
Use Data Alia Data Shared
Set Order To Sr_No

*-----------------------------------
*1.	Purchase
*2.	Purchase Return
*3.	Sale
*4.	Sale Return
*-----------------------------------
*Veriable Initialization
MVch_No=0							&& Voucher Number
MI1=Spac(2)							&& Item Code 1
MI2=Spac(2)							&& Item Code 2
MI3=Spac(4)							&& Item Code 3
MC1=Spac(2)							&& Party Code 1
MC2=Spac(2)							&& Party Code 2
MC3=Spac(3)							&& Party Code 3
MT_Qty=0							&& Total Quantity
MT_Amount=0							&& Total Amount
MG_Amount=0							&& Net Amount
MPage=0								&& Page No
MLine=0								&& Line Number
MFNo=0								&& Starting Number
MTNo=0								&& Ending Number
MPS=Spac(1)							&& Printer Or Screen
MCQ=Spac(1)							&& Continue Or Quit
MSr_No=0							&& Serial Number
*-----------------------------------
* Window Defination
	DEFINE WINDOW rinvoic ;
		FROM INT((SROW()-17)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-17)/1)+9,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/G
		
	ACTIVATE WINDOW rinvoic SAME
*-----------------------------------
Set Century Off
Clear
Do While .T.
	@ 2,05 Say "Invoice Number From:" Color W/N
	@ 4,05 Say "Invoice Number To:" Color W/N
	@ 2,30 GET MFNo Pict"99999999" Valid MFNo#0 Error 'Voucher No Can Not Be 0'
	@ 4,30 GET MTNo Pict"99999999" Valid MTNo#0 Error 'Voucher No Can Not Be 0'
	Read	
	If Read()=12 .Or. Read()=268				&& Escape Key
		Close Databases
		Rele Wind rinvoic
		Return
		Exit
	Endif
	Sele ITranKe
	Set Order To VchNo
	Seek MFNo
	If .Not. Found()
		Wait Wind 'Voucher No Not Exist'
		MFNo=0								&& Starting Number
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
Set Filter To Vch_No>=MFNo .And. Vch_No<=MTNo .And. Vch_Type=MVch_Type
Goto Top
Set Relation To Str(C1,2)+Str(C2,2)+Str(C3,3) Into ILdgrCod Additive
Set Relation To Str(I1,2)+Str(I2,2)+Str(I3,4) Into Item Additive
If MVch_Type=1
	MTitle="Purchase Invoice # "
Endif	
If MVch_Type=2
	MTitle="Purchase Return Invoice # "
Endif	
If MVch_Type=3
	MTitle="Sale Invoice # "
Endif	
If MVch_Type=4
	MTitle="Sale Return Invoice # "
Endif	
If MPS="P"
	Do RepPrtPro
Else
	Do RepScrPro
Endif
Rele wind rinvoic
Set Print Off
Set Device To Screen
Close Databases
Return
*-----------------------------------
Procedure RepPrtPro							&& Report Print Procedure
*-----------------------------------
Set Print On
Set Device To Print
MLine=8								&& Line Number
Do While .Not. Eof()
	MVch_No=Vch_No
	MC1=Str(C1,2)
	MC2=Str(C2,2)
	MC3=Str(C3,3)
	Do LedCodPro In Proc1
	@ 0,0 Say Chr(18)
	@ 0,0 Say Chr(14)
	@ 0,1 Say MComp
	@ 0,0 Say Chr(18)
	@ 2,0 Say MTitle+Str(Vch_No,8)
	@ 2,63 Say "Date :"+Dtoc(Date)
	@ 3,0 Say "Billed To: "+MC1+"-"+MC2+"-"+MC3+"  "+ILdgrcod.Title
	@ 3,63 Say "Sale Tax %: "+Str(St_Per,2)
	@ 4,0 Say Repl('-',80)
	@ 5,0 Say Chr(15)
	@ 5,0 Say "S.No   Code               Item                           Qty             Rate               Total      Discount          Net"
	@ 6,0 Say "                                                                                            Amount         %            Amount"
	@ 7,0 Say Repl('-',137)
	Do While Vch_No=MVch_No
		MSr_No=MSr_No+1
		MI1=Str(I1,2)
		MI2=Str(I2,2)
		MI3=Str(I3,4)
		Do ItmCodPro In Proc1
		@ MLine,00 Say Str(MSr_No,3)+"   "+MI1+"-"+MI2+"-"+MI3+"   "+Item.Item+"   "+Str(T_Qty,12,4)+"   "+Str(Rate,14,4)+"   "+Str(T_Amount,17,4)+"     "+Str(D_Per,2)+"   "+Str(G_Amount,17,4)
		MLine=MLine+1
		@ MLine,05 Say Item.Item
		MT_Qty=MT_Qty+T_Qty
		MT_Amount=MT_Amount+T_Amount
		MG_Amount=MG_Amount+G_Amount
		Skip
		MLine=MLine+1
		If MLine>59
			MLine=8								&& Line Number
			@ 0,0 Say Chr(18)
			@ 0,0 Say Chr(14)
			@ 0,1 Say MComp
			@ 0,0 Say Chr(18)
			@ 2,01 Say MTitle+Str(Vch_No,8)
			@ 2,63 Say "Date :"+Dtoc(Date)
			@ 3,0 Say "Billed To: "+MC1+"-"+MC2+"-"+MC3+"  "+ILdgrcod.Title
			@ 3,63 Say "Sale Tax %: "+Str(St_Per,2)
			@ 4,0 Say Repl('-',80)
			@ 5,0 Say Chr(15)
			@ 5,0 Say "S.No   Code               Item                           Qty             Rate               Total      Discount          Net"
			@ 6,0 Say "                                                                                            Amount         %            Amount"
			@ 7,0 Say Repl('-',80)
		Endif
	Enddo
	@ MLine,0 Say Repl('-',137)
	MLine=MLine+1
	@ MLine,0 Say Chr(18)
	@ MLine,0 Say "Total Qty :"+Str(MT_Qty,12,4)
	@ MLine,45 Say "Gross Amount :"+Str(MT_Amount,17,4)
	MLine=MLine+1
	@ MLine,45 Say "Discount     :"+Str(MT_Amount-MG_Amount,17,4)
	MLine=MLine+1
	@ MLine,45 Say "NET PAYMENT  :"+Str(MG_Amount,17,4)
	MLine=MLine+1
	@ MLine,0 Say Chr(15)
	MAmt=MG_Amount
	MMAmt=MAmt
	Do Change
	MLine=MLine+1
	@ MLine,0 Say Chr(18)
	@ MLine,0 Say Repl('-',80)
	Sele ITran
	MLine=8								&& Line Number
	MT_Qty=0							&& Total Quantity
	MT_Amount=0							&& Total Amount
	MG_Amount=0							&& Net Amount
	MSr_No=0							&& Serial Number
Eject
Enddo
Return
*-----------------------------------
Procedure RepScrPro							&& Report Screen Procedure
*-----------------------------------
Do DspScrPro In Proc1
MLine=8								&& Line Number
Do While .Not. Eof()
	MVch_No=Vch_No
	MC1=Str(C1,2)
	MC2=Str(C2,2)
	MC3=Str(C3,3)
	Do LedCodPro In Proc1
	@ 00,01 Say MComp
	@ 01,01 Say MTitle+Str(Vch_No,8)
	@ 01,63 Say "Date :"+Dtoc(Date)
	@ 02,0 Say "Billed To: "+MC1+"-"+MC2+"-"+MC3+"  "+ILdgrcod.Title
	@ 02,63 Say "Sale Tax %: "+Str(St_Per,2)
	@ 03,0 Say Repl('-',78)
	@ 04,0 Say "S.No  Code/Item        Qty          Rate           Total  Discount      Net"
	@ 05,0 Say "                                                   Amount    %         Amount"
	@ 06,0 Say Repl('-',78)
	Do While Vch_No=MVch_No
		MSr_No=MSr_No+1
		MI1=Str(I1,2)
		MI2=Str(I2,2)
		MI3=Str(I3,4)
		Do ItmCodPro In Proc1
		@ MLine,00 Say Str(MSr_No,3)+" "+MI1+"-"+MI2+"-"+MI3+" "+Str(T_Qty,12,4)+" "+Str(Rate,14,4)+" "+Str(T_Amount,15,4)+"   "+Str(D_Per,2)+Str(G_Amount,15,4)
		MLine=MLine+1
		@ MLine,05 Say Item.Item
		MT_Qty=MT_Qty+T_Qty
		MT_Amount=MT_Amount+T_Amount
		MG_Amount=MG_Amount+G_Amount
		Skip
		MLine=MLine+1
		If MLine>17
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
	@ MLine,0 Say Repl('-',78)
	MLine=MLine+1
	@ MLine,05 Say "Total"
	@ MLine,15 Say Str(MT_Qty,12,4)
	@ MLine,27 Say Str(MT_Amount,15,4)
	@ MLine,62 Say Str(MG_Amount,15,4)
	MLine=MLine+1
	If MLine>1
		@ 21,28 Say "Continue Or Quit C/Q"
		@ 21,50 GET MCQ Pict"@!" Valid MCQ="C" .Or. MCQ="Q" Error 'Continue Or Quit (C/Q)'
		Read
		If MCQ="Q"
			Rele wind scr
			Return
		Endif	
		Clear
		MLine=8
	Endif
	MT_Qty=0							&& Total Quantity
	MT_Amount=0							&& Total Amount
	MG_Amount=0							&& Net Amount
	MSr_No=0							&& Serial Number
Enddo
Rele wind scr
Return
