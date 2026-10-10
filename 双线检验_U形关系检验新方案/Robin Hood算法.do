
set rmsg on

cd "`c(pwd)'" // 切换到当前目录

use "../数据/实证数据集.dta",clear

***** Robin Hood 算法寻找断点
*** 估计限制性三次样条
mkspline k=Cash, cubic nknots(7)  // Restricted cubic spline，新变量为k～
reg Tobin_Q k*, vce(r)    // 回归
predict y_hat, xb   // 生成y的拟合值
predict y_se, stdp  // 获得y的标准误

sort Cash  // x排序，画图预处理
line y_hat Cash

*** 确定y_flat区间
* 找出y_hat最大值y_max
qui sum y_hat,detail
gen y_max = r(max)
* 计算与y_max相差不超过1个标准误的y_flat
gen y_flat_up = y_max + y_se
gen y_flat_down = y_max - y_se
gen if_y_flat = 1 if (y_flat_down<=y_hat & y_hat<=y_flat_up)
gen x_flat = Cash if if_y_flat == 1
gen y_flat = y_hat if if_y_flat == 1
* 绘图展示区间
gen x_max = Cash if y_hat == y_max
twoway (line y_hat Cash) (area y_flat x_flat) (dot y_max x_max)

*** 以区间中点为临时断点
* 找出中点
sum x_flat,detail
gen tem = abs(x_flat-r(p50))  
sort tem x_flat
gen mid_x_flat=x_flat[1]  // 成为中位数变量
drop tem
* 构造左侧解释变量
gen xlow1 = Cash-mid_x_flat if Cash<=mid_x_flat
replace xlow1=0 if Cash>mid_x_flat
gen xhigh1 = Cash-mid_x_flat if Cash>mid_x_flat
replace xhigh1=0 if Cash<=mid_x_flat
gen hi1 = Cash>mid_x_flat
* 回归并获取左侧的检验统计量
xtset id year
glm Tobin_Q xlow1 xhigh1 hi1 ROE Lev, family(gaussian) vce(robust)
global z1 = abs(_b[xlow1]/_se[xlow1])

* 构造右侧解释变量
gen xlow2 = Cash-mid_x_flat if Cash<mid_x_flat
replace xlow2=0 if Cash>=mid_x_flat
gen xhigh2 = Cash-mid_x_flat if Cash>=mid_x_flat
replace xhigh2=0 if Cash<mid_x_flat
gen hi2 = Cash>=mid_x_flat
* 回归并获取右侧的检验统计量
xtset id year
glm Tobin_Q xlow2 xhigh2 hi2 ROE Lev, family(gaussian) vce(robust)
global z2 = abs(_b[xhigh2]/_se[xhigh2])
* 计算最终断点所在位置
global q = 100 * $z2 / ( $z1+$z2 )
display $q
drop xlow* xhigh* hi*

*** 最终断点
_pctile x_flat, p($q) altdef
gen x_star = r(r1)

***** 双线检验
*** 回归
gen xlow = Cash-x_star if Cash<=x_star
replace xlow=0 if Cash>x_star
gen xhigh = Cash-x_star if Cash>x_star
replace xhigh=0 if Cash<=x_star
gen hi = Cash>x_star

glm Tobin_Q xlow xhigh hi ROE Lev, family(gaussian) vce(robust)
est store aa1
reghdfe Tobin_Q xlow xhigh hi ROE Lev, absorb(id year) vce(cluster id)
est store aa2

esttab aa* using "../结果/双线检验结果.csv",replace star(* 0.10 ** 0.05 *** 0.01) compress nogap b(%6.4f) ar2(%9.4f) se(%9.4f) onecell mtitle("GLM""OLS")
estimates drop aa*
*** 绘图
sort Cash
gen y_star = y_hat if Cash == x_star
twoway (line y_hat Cash) (area y_flat x_flat) (dot y_star x_star)
graph export "../结果/Robin Hood算法.png",replace
