
set rmsg on

cd "`c(pwd)'" // 切换到当前目录

use "../数据/实证数据集.dta",clear

***** 估计限制性三次样条
mkspline k=ROE, cubic nknots(7)  // Restricted cubic spline，新变量为k～
reg Tobin_Q k*, vce(r)    // 回归
predict y_hat, xb   // 生成y的拟合值
predict y_se, stdp  // 获得y的标准误

sort ROE  // x排序，画图预处理
line y_hat ROE // 绘图

***** 找出y_max和y_min
qui sum y_hat,detail
gen y_max = r(max)
gen y_min = r(min)

gen x_max = ROE if y_hat == y_max
gen x_min = ROE if y_hat == y_min

sum x_max,detail
global xmax = r(mean)
sum x_min,detail
global xmin = r(mean)

***** 围绕第一断点估计
*** 计算与y_min相差不超过1个标准误的y_min_flat
gen y_min_flat_up = y_min + y_se
gen y_min_flat_down = y_min - y_se
gen if_y_min_flat = 1 if (y_min_flat_down<=y_hat & y_hat<=y_min_flat_up)
gen x_min_flat = ROE if if_y_min_flat == 1
gen y_min_flat = y_hat if if_y_min_flat == 1
*** 以区间中点为临时断点
sum x_min_flat,detail
gen tem = abs(x_min_flat-r(p50))  
sort tem x_min_flat
gen mid_x_min_flat=x_min_flat[1]  // 成为中位数变量
drop tem
*** 构造左侧变量
gen xlow1 = ROE-mid_x_min_flat if ROE<=mid_x_min_flat
replace xlow1=0 if ROE>mid_x_min_flat
gen xhigh1 = ROE-mid_x_min_flat if ROE>mid_x_min_flat
replace xhigh1=0 if ROE<=mid_x_min_flat
gen hi1 = ROE>mid_x_min_flat
*** 回归并获取左侧的检验统计量
xtset id year
glm Tobin_Q xlow1 xhigh1 hi1 Cash Lev if ROE < $xmax , family(gaussian) vce(robust)
global z1 = abs(_b[xlow1]/_se[xlow1])
*** 构造右侧变量
gen xlow2 = ROE-mid_x_min_flat if ROE<mid_x_min_flat
replace xlow2=0 if ROE>=mid_x_min_flat
gen xhigh2 = ROE-mid_x_min_flat if ROE>=mid_x_min_flat
replace xhigh2=0 if ROE<mid_x_min_flat
gen hi2 = ROE>=mid_x_min_flat
*** 回归并获取右侧的检验统计量
xtset id year
glm Tobin_Q xlow2 xhigh2 hi2 Cash Lev if ROE < $xmax , family(gaussian) vce(robust)
global z2 = abs(_b[xhigh2]/_se[xhigh2])
*** 计算第一断点
global q1 = 100 * $z2 / ( $z1+$z2 )
display $q1
drop xlow* xhigh* hi*
_pctile x_min_flat, p($q1) altdef
gen x_star_1 = r(r1)

***** 围绕第二断点估计
*** 计算与y_max相差不超过1个标准误的y_max_flat
gen y_max_flat_up = y_max + y_se
gen y_max_flat_down = y_max - y_se
gen if_y_max_flat = 1 if (y_max_flat_down<=y_hat & y_hat<=y_max_flat_up)
gen x_max_flat = ROE if if_y_max_flat == 1
gen y_max_flat = y_hat if if_y_max_flat == 1
*** 以区间中点为临时断点
sum x_max_flat,detail
gen tem = abs(x_max_flat-r(p50))  
sort tem x_max_flat
gen mid_x_max_flat=x_max_flat[1]  // 成为中位数变量
drop tem
*** 构造左侧变量
gen xlow1 = ROE-mid_x_max_flat if ROE<=mid_x_max_flat
replace xlow1=0 if ROE>mid_x_max_flat
gen xhigh1 = ROE-mid_x_max_flat if ROE>mid_x_max_flat
replace xhigh1=0 if ROE<=mid_x_max_flat
gen hi1 = ROE>mid_x_max_flat
*** 回归并获取左侧的检验统计量
xtset id year
glm Tobin_Q xlow1 xhigh1 hi1 Cash Lev if ROE > $xmin , family(gaussian) vce(robust)
global z3 = abs(_b[xlow1]/_se[xlow1])
*** 构造右侧变量
gen xlow2 = ROE-mid_x_max_flat if ROE<mid_x_max_flat
replace xlow2=0 if ROE>=mid_x_max_flat
gen xhigh2 = ROE-mid_x_max_flat if ROE>=mid_x_max_flat
replace xhigh2=0 if ROE<mid_x_max_flat
gen hi2 = ROE>=mid_x_max_flat
*** 回归并获取右侧的检验统计量
xtset id year
glm Tobin_Q xlow2 xhigh2 hi2 Cash Lev if ROE > $xmin , family(gaussian) vce(robust)
global z4 = abs(_b[xhigh2]/_se[xhigh2])
*** 计算第二断点
global q2 = 100 * $z4 / ( $z3+$z4 )
display $q2
drop xlow* xhigh* hi*
_pctile x_max_flat, p($q2) altdef
gen x_star_2 = r(r1)

***** 重叠样本双线检验
*** 第一断点
gen xlow = ROE-x_star_1 if ROE<=x_star_1
replace xlow=0 if ROE>x_star_1
gen xhigh = ROE-x_star_1 if ROE>x_star_1
replace xhigh=0 if ROE<=x_star_1
gen hi = ROE>x_star_1

glm Tobin_Q xlow xhigh hi Cash Lev if ROE < $xmax , family(gaussian) vce(robust)
est store aa1
reghdfe Tobin_Q xlow xhigh hi Cash Lev if ROE < $xmax , absorb(id year) vce(cluster id)
est store aa2
drop xlow* xhigh* hi*

*** 第二断点
gen xlow = ROE-x_star_2 if ROE<=x_star_2
replace xlow=0 if ROE>x_star_2
gen xhigh = ROE-x_star_2 if ROE>x_star_2
replace xhigh=0 if ROE<=x_star_2
gen hi = ROE>x_star_2

glm Tobin_Q xlow xhigh hi Cash Lev if ROE > $xmin , family(gaussian) vce(robust)
est store aa3
reghdfe Tobin_Q xlow xhigh hi Cash Lev if ROE > $xmin , absorb(id year) vce(cluster id)
est store aa4
drop xlow* xhigh* hi*

esttab aa* using "../结果/重叠样本双线检验结果.csv",replace star(* 0.10 ** 0.05 *** 0.01) compress nogap b(%6.4f) ar2(%9.4f) se(%9.4f) onecell mtitle("GLM1""OLS1""GLM2""OLS2")
estimates drop aa*

*** 绘图
sort ROE
gen y_star_1 = y_hat if ROE == x_star_1
gen y_star_2 = y_hat if ROE == x_star_2
twoway (line y_hat ROE) (dot y_star_1 x_star_1) (dot y_star_2 x_star_2)
graph export "../结果/叠样本双线检验.png",replace