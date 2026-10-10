set rmsg on

cd "`c(pwd)'" // 切换到当前目录

use "../数据/实证数据集.dta",clear

xtset id year

***** 仍然以二次项为主回归
gen Cash_2 = Cash*Cash

reghdfe Tobin_Q Cash Cash_2, absorb(id year) vce(cluster id)
est store aa1
reghdfe Tobin_Q Cash Cash_2 ROE, absorb(id year) vce(cluster id)
est store aa2
reghdfe Tobin_Q Cash Cash_2 ROE Lev, absorb(id year) vce(cluster id)
est store aa3

esttab aa* using "./二次项.csv",replace star(* 0.10 ** 0.05 *** 0.01) compress nogap b(%6.4f) ar2(%9.4f) se(%9.4f) onecell mtitle("Tobin_Q""Tobin_Q""Tobin_Q")
estimates drop aa*

* utest
reghdfe Tobin_Q Cash Cash_2 ROE Lev, absorb(id year) vce(cluster id)
utest Cash Cash_2, fieller