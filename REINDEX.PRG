*-----------------------------------*
*Program to Reindex All The Files	*
*Author		:	Muhammad Usman		*
*Date		:	29-01-2000			*
*Prog		:	Reindex				*
*Last Modify:	29-01-2000			*
*-----------------------------------*
* 
* 
* REINDEX.FXP 
* ----------- 
*
DEFINE WINDOW RINDEX FROM INT((SROWS()-15)/1), INT((SCOLS()-68)/1) TO INT((SROWS()-15)/1)+6, INT((SCOLS()-68)/1)+50 NOFLOAT NOCLOSE SHADOW TITLE 'RE-INDEX PROGRAMME' NOMINIMIZE PANEL COLOR SCHEME 1
CLOSE DATABASES
WAIT WINDOW 'Make Sure No Body Using This Program On Any Terminal'
WAIT WINDOW NOWAIT 'Re-Indexing in progress, Please Dont Distrub'
ACTIVATE WINDOW SAME RINDEX
SET EXCLUSIVE ON
DBF1 = ('Ldgrcod.Dbf')
DBF2 = ('Tran2.Dbf')
DBF3 = ('Item.Dbf')
DBF4 = ('ITran.Dbf')
DBF5 = ('DType.Dbf')
DBF6 = ('Pass.Dbf')
DBF7 = ('City.Dbf')
@ 3, 4 SAY '≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤≤'
USE &DBF1
@1,10 SAY FULLPATH('&DBF1')
PACK
REINDEX
READ TIMEOUT 0.10
@ 3, 4 SAY '€€€€€€€€'
@ 1, 1 CLEAR TO 1, 65
USE &DBF2
@1,10 SAY FULLPATH('&DBF2')
PACK
REINDEX
READ TIMEOUT 0.20
@ 3, 12 SAY '€€€€€€€€'
@ 1, 1 CLEAR TO 1, 65
USE &DBF3
@1,10 SAY FULLPATH('&DBF3')
PACK
REINDEX
READ TIMEOUT 0.30
@ 3, 20 SAY '€€€€€€€€'
@ 1, 1 CLEAR TO 1, 65
USE &DBF4
@1,10 SAY FULLPATH('&DBF4')
PACK
REINDEX
READ TIMEOUT 0.40
@ 3, 28 SAY '€€€€€€€€'
@ 1, 1 CLEAR TO 1, 65
USE &DBF5
PACK
REINDEX
USE &DBF6
@1,10 SAY FULLPATH('&DBF6')
PACK
REINDEX
READ TIMEOUT 0.40
@ 3, 36 SAY '€€€€€€€€'
@ 1, 1 CLEAR TO 1, 65
USE &DBF7
@1,10 SAY FULLPATH('&DBF7')
PACK
REINDEX
RELEASE WINDOW RINDEX
WAIT WINDOW NOWAIT 'Reindex Process Completed'
SET EXCLUSIVE OFF
RETURN
