* Screen Functions to place Formatted Data on Screen.

***- Ask a Yes-No question in a dialog Box

*********************************
FUNCTION YesNo
*********************************

PARAMETER mAnswer,mQuestion

DEFINE WINDOW YesNo FROM 8,20 TO 15,60 DOUBLE SHADOW COLOR SCHEME 7
ACTIVATE WINDOW YesNo

PRIVATE _Lmargin,_Rmargin,_Wrap

_Lmargin = 2
_Rmargin = 38
_Wrap = .T.

? mQuestion + "?"

IF mAnswer
	mButton = 1
	mPicture = "@*H \!\<Yes;\?\<No"
ELSE
	mButton = 2
	mPicture = "@*H \<Yes;\!\?\<No"
ENDIF

@ 5,5 GET mButton PICT mPicture SIZE 1,10,8
READ OBJECT mButton

mAnswer = IIF(mButton = 1,.T.,.F.)

RELEASE WINDOW YesNo
RETURN mAnswer
********************************************


**********************************
PROCEDURE Thermo
**********************************

PARAMETERS mPhase,mMessage,mColor1,mColor2

DO CASE
	CASE mPhase = "S"
		PUBLIC mThrmCount,mInterval
		IF EMPTY(mColor1)
			mColor1 = 5
		ENDIF
		IF EMPTY(mColor2)
			mColor1 = 20
		ENDIF
		mTopbox = (SROWS() - 6) /2
		DEFINE WINDOW Thermo FROM mTopbox,8 TO mTopbox+5,71 DOUBLE SHADOW;
			COLOR SCHEME mColor1
		ACTIVATE WINDOW Thermo
		
		@ 0,1 SAY mMessage
		@ 1,1 TO 3,60 COLOR SCHEME mColor2
		@ 2,2 SAY CHR(219)
		
		mThrmCount = 0
		mInterval = INT(RECCOUNT() / 56)
	
	CASE mPhase = "U"
			
		IF mThrmCount % mInterval = 0 AND COL() <= 58
			@ 2,COL() SAY CHR(219)
		ENDIF
		mThrmCount = mThrmCount+1
		
	CASE mPhase = "C"
		DEACTIVATE WINDOW Thermo
		WAIT WINDOW NOWAIT;
			LTRIM(STR(mThrmCount,5)) + " " + mMessage
		
		RELEASE mThrmCount,mInterval
ENDCASE
RETURN
*****************************************