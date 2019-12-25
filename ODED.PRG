**************************
* PROGRAM: ODED.PRG
* A program to test Replace Other Deduction 
* in DEDUCT.DBF for the month 
* Wages Rate Monthly.
***************************

SET VIEW TO NEWMONTH

REPLACE ALL temp.other WITH IIF(salary.wrdaily = .F. .and. temp.days <;
 (month.mdays - month.frds),;
 (salary.dwratem/month.mdays)*((month.mdays-month.frds)-;
 temp.days),0)
 
 
