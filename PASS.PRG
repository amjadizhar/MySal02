*-----------------------------------*
*Program to Password				*
*Author		:	Muhammad Usman		*
*Date		:	01-05-1999			*
*Prog		:	Pass				*
*Last Modify:	01-05-1999			*
*-----------------------------------*
PUBLIC MPermit,MPass
*-----------------------------------
*Veriable Initialization
MPass=Spac(6)					&& Password
MPermit=Spac(2)					&& Permit
*-----------------------------------
* Window Defination
	DEFINE WINDOW pass ;
		FROM INT((SROW()-15)/1),INT((SCOL()-68)/1) ;
		TO INT((SROW()-15)/1)+6,INT((SCOL()-68)/1)+50 ;
		NOFLOAT ;
		NOCLOSE ;
		SHADOW ;
		NOMINIMIZE ;
		PANEL ;
		Color R/BG
		
	ACTIVATE WINDOW pass SAME
*-----------------------------------
@ 02,08 SAY "Enter Your Password :"
Set Color To W/N,N/N
@ 02,30 Get MPass Function '!'
Read
   NCODE1= SUBSTR(MPass,1,1)
   NCODE2= SUBSTR(MPass,2,1)
   NCODE3= SUBSTR(MPass,3,1)
   NCODE4= SUBSTR(MPass,4,1)
   NCODE5= SUBSTR(MPass,5,1)
   NCODE6= SUBSTR(MPass,6,1)
   Mpass = chr(asc("&ncode1")+26)+chr(asc("&ncode2")+30)+chr(asc("&ncode3")+29)+chr(asc("&ncode4")+21)+chr(asc("&ncode5")+19)+chr(asc("&ncode6")+13)

use Pass in 0
SELE PASS
Set Order To Sr_No
Seek MSrNo
	If MPass=Pass
		MPermit="OK"
	Else
		Wait Wind 'Your Password Is Not Correct'
	Endif	
Rele Wind pass
Return
