
*******************************
*Report SheetSal Variables

* Column Gross Pay
* Variable Total
salary.basic + salary.hrent + salary.convy + salary.medical + salary.cma + salary.pma + salary.other + salary.special

* Total Bottom Line
* Variable Gtotal

salary.basic + salary.hrent + salary.convy + salary.medical + salary.cma + salary.pma + salary.other + salary.special

* Total Bottom Line
* Variable Tnet

gtotal - (tmidadv + tadv + tother + tpfund)

* Column Net Pay
* Variable Net

total - (deduct.midadv + deduct.advance + deduct.other + deduct.pfund)