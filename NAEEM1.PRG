* Routines to get reports in wk1 format for mr.naeem

* Pay List for Salary & Wages employees

* Files to use 
* 1- SQLEMP.DBF
* 2- SQLSLRY.DBF

SELECT dept_name,;
	   basic+hrent+convy+medical+cma+pma+other+special AS "Total_Pay",;
	   basic,;
	   otrated;
FROM sqlemp,sqlslry;
WHERE sqlemp.empcode = sqlslry.empcode AND sqlslry.mpay = .t.;
	  AND sqlemp.working = .t.;
ORDER BY dept_name;
INTO CURSOR empinf1

SELECT dept_name,;
	   dwrate,;
	   dwrate*30 AS "Total_30_d",;
	   otrated;
FROM sqlemp,sqlslry;
WHERE sqlemp.empcode = sqlslry.empcode AND sqlslry.mpay = .f.;
	  AND sqlemp.working = .t.;
ORDER BY dept_name;
INTO CURSOR empinf2
