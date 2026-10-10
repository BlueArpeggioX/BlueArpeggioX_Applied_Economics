
set rmsg on

cd "`c(pwd)'" // 切换到当前目录
use "../数据/实证数据集.dta",clear

xtset id year

***** 确定断点
global r = 1000 // 搜寻次数

quietly summarize Cash, detail // 获取解释变量取值范围
global Cash_max   = r(max)
global Cash_min = r(min)

*** 生成一系列变量用于保存中间数据
gen n = _n // 序号
gen xstar = . // 候选断点
gen b1 = . // 候选左侧斜率
gen b2 = . // 候选右侧斜率
gen p1 = . // 候选左侧p值
gen p2 = . // 候选右侧p值
gen test = . // 联合p值

*** 对每一个候选断点执行一次分段回归
forval i = 1/$r {
	* 构造左侧和右侧解释变量
	global xc = $Cash_min + `i'/$r * ($Cash_max - $Cash_min)
	qui replace xstar=$xc if n==`i'
	
	qui gen xlow1=Cash-$xc if Cash<=$xc
	qui replace xlow1=0 if Cash>$xc
	qui gen xhigh1=Cash-$xc if Cash>$xc
	qui replace xhigh1=0 if Cash<=$xc
	qui gen hi1 = Cash>$xc
	
	qui gen xlow2=Cash-$xc if Cash<$xc
	qui replace xlow2=0 if Cash>=$xc
	qui gen xhigh2=Cash-$xc if Cash>=$xc
	qui replace xhigh2=0 if Cash<$xc
	qui gen hi2 = Cash>=$xc
	* 回归并保存左侧数据
	qui reghdfe Tobin_Q xlow1 xhigh1 hi1 ROE Lev, absorb(id year) vce(cluster id)
	qui local bxlow1  = _b[xlow1]
	qui replace b1 = `bxlow1'  if n==`i'
	
	qui test xlow1 == 0 // 检验左侧斜率
	qui local pxlow1 = r(p)
	qui replace p1=`pxlow1' if n==`i'
	
	qui test xhigh1 == 0,acc // 检验两侧斜率
	qui local rp = r(p)
	qui replace test=`rp' if n==`i' 
	* 回归并保存右侧数据
	qui reghdfe Tobin_Q xlow2 xhigh2 hi2 ROE Lev, absorb(id year) vce(cluster id)
	qui local bxhigh2 = _b[xhigh2]
	qui replace b2 = `bxhigh2' if n==`i'
	
	qui test xhigh2 == 0 // 检验右侧斜率
	qui local pxhigh2 = r(p)
	qui replace p2=`pxhigh2' if n==`i'

	qui drop xlow* xhigh* hi* 
	* 计数
	if (`i' == 1) {
    nois _dots 0, title(检验:)
    }
    nois _dots `i' 0
}
save "../数据/Chin et al候选断点数据集.dta",replace
use "../数据/Chin et al候选断点数据集.dta",clear

*** 找出统计上最佳的断点并保存
gen double jtest  = ((p1^2 + p2^2)^(0.5))/2
sum jtest if b1>0 & b2<0
sum xstar if jtest==r(min)
global gxstar = r(mean)
display $gxstar

// 候选断点比较
twoway 	(line p1 xstar if b1>0 & b2<0,sort lcol(gs1)lpat(dash)) ///
		(line p2 xstar if b1>0 & b2<0,sort lcol(gs8)lpat(solid)) ///
		(line test xstar if b1>0 & b2<0,sort lcol(red)lpat(solid)) ///
		(line jtest xstar if b1>0 & b2<0,sort lcol(blue)lpat(solid)), ///
		title(候选断点比较) ylab(0(.2)1) ///
		legend(lab(1 "左侧p值")lab(2 "右侧p值") lab(3 "联合检验p值") lab(4 "断点检验p值") ///
		size(small))ytit(p-value)xtit("x*")subtitle("{&beta}{sub:1}>0; {&beta}{sub:2}<0")
graph export "../结果/Chin候选断点比较图.png",replace

***** 双线检验
drop xstar b1 b2 p1 p2 test jtest // 删除中间数据

* 构造左侧和右侧解释变量
display $gxstar
gen xlow=Cash-$gxstar if Cash<=$gxstar
replace xlow=0 if Cash>$gxstar
gen xhigh=Cash-$gxstar if Cash>$gxstar
replace xhigh=0 if Cash<=$gxstar
gen hi = Cash>$gxstar

quietly summarize Cash, detail // 获取解释变量取值范围
global Cash_max   = r(max)
global Cash_min = r(min)

* 回归
xtset id year
reghdfe Tobin_Q xlow xhigh hi ROE Lev, absorb(id year) vce(cluster id)
predict TY ,xb
global b1 = _b[xlow]*( $gxstar-$Cash_min ) // 左侧直线的纵轴范围
global b2 = _b[xhigh]*( $Cash_max-$gxstar ) // 右侧直线的纵轴范围

* 检验左右斜率
reghdfe Tobin_Q xlow xhigh hi ROE Lev, absorb(id year) vce(cluster id)
test xhigh == 0
test xlow == 0,acc
test xlow == 0

* 保存绘图数据
gen x1=. // 左侧直线
gen y1=.
gen x2=. // 右侧直线
gen y2=.

* 计算两侧直线的端点，并平移至全部预测值均值附近
qui sum TY
local m=r(mean) // 全部拟合值的均值

replace x1=$Cash_min if _n==1
replace y1=`m' - ($b1/2) if _n==1
replace x1=$gxstar if _n==2
replace y1=`m' + ($b1/2) if _n==2

replace x2=$gxstar   if _n==1
replace x2=$Cash_max if _n==2
replace y2=`m' - ($b2/2) if _n==1
replace y2=`m' + ($b2/2) if _n==2

* 绘图：预测值散点+双线
twoway 	(scatter TY Cash, mcolor(gs10%40) msize(vsmall) msymbol(circle)) ///
		(line y1 x1,lpat(solid) lcol(blue*1.1)) ///
		(line y2 x2,lpat(solid) lcol(red*1.1)), ///
		xline($gxstar, lpattern(dash) lcolor(black) lwidth(thin)) ///
		xtitle("Cash") ytitle("Tobin_Q预测值") ///
		legend(lab(1 "预测值") lab(2 "左侧直线") label(3 "右侧直线") order(1 2 3) rows(1) ring(1) pos(6))
graph export "../结果/Chin et al双线检验图.png",replace

***** 非参拟合
*** 确实拟合次数
gen time = year - 2000
gen time2 = time^2
xtset id year
local j=1
forval i =1/6 {
 	xi:qui xtsemipar Tobin_Q time time2 ROE Lev,nonpar(Cash) cluster(id) deg(`i') spline gen(fA`j' rA`j')  nograph
	di `i' 
	di e(rmse)
	di e(ll)
	local j=`j'+1
}

twoway 	(lowess fA1 Cash,sort ytit(Tobin_Q)col(red*0.5)) ///
		(lowess fA2 Cash,sort ytit(Tobin_Q)col(red*0.6)) ///
		(lowess fA3 Cash,sort ytit(Tobin_Q)col(red*0.7)) ///
		(lowess fA4 Cash,sort ytit(Tobin_Q)col(red*0.8)) ///
		(lowess fA5 Cash,sort ytit(Tobin_Q)col(red*0.9)lpat(solid)) ///
		(lowess fA6 Cash,sort ytit(Tobin_Q)col(red)lpat(solid)),   ///
		tit("Tobin_Q")legend(lab(1 "degree 1")lab(2 "degree 2") ///
		lab(3 "degree 3")lab(4 "degree 4")lab(5 "degree 5")lab(6 "degree 6")pos(10)ring(0))
drop rA* fA*

*** 正式拟合
global deg = 3

// 获取上下分位数值
quietly summarize Cash, detail
global Cash_低 = r(p10)
global Cash_高 = r(p95)

* 非参拟合
// 无控制变量，全部样本
xtsemipar Tobin_Q time time2 ,nonpar(Cash) cluster(id) gen(fA rA) deg($deg) spline nograph
// 无控制变量，剔除低值
xtsemipar Tobin_Q time time2 if Cash > $Cash_低, nonpar(Cash) cluster(id) gen(fA1 rA1) deg($deg) spline nograph
// 无控制变量，剔除高值
xtsemipar Tobin_Q time time2 if Cash < $Cash_高, nonpar(Cash) cluster(id) gen(fA2 rA2) deg($deg) spline nograph
// 有控制变量，全部样本
xtsemipar Tobin_Q time time2 ROE Lev, nonpar(Cash) cluster(id) gen(fB rB) deg($deg) spline nograph
// 有控制变量，剔除低值
xtsemipar Tobin_Q time time2 ROE Lev if Cash > $Cash_低, nonpar(Cash) cluster(id) gen(fB1 rB1) deg($deg) spline nograph
// 有控制变量，剔除高值
xtsemipar Tobin_Q time time2 ROE Lev if Cash < $Cash_高, nonpar(Cash) cluster(id) gen(fB2 rB2) deg($deg) spline nograph

* 绘图
quietly summarize Tobin_Q
global qmean = r(mean)

// 平移，同时保留原数据
foreach v in fA fA1 fA2 fB fB1 fB2 {
    clonevar plot_`v' = `v'
    quietly summarize plot_`v'
    replace plot_`v' = plot_`v' - r(mean) + $qmean
}

twoway ///
    (histogram Cash, frequency color(gs14) bin(50) yaxis(2)) ///
    (line plot_fA Cash, sort lcolor(blue) lpattern(solid)) ///
    (line plot_fA1 Cash, sort lcolor(cyan) lpattern(dash)) ///
    (line plot_fA2 Cash, sort lcolor(red) lpattern(dash)), ///
    ytitle("Tobin_Q拟合值", axis(1)) ///
    xtitle("Cash") ///
    title("未加入控制变量") ///
    legend( order(2 3 4) ///
        label(2 "全部样本") ///
        label(3 "剔除最低10%") ///
        label(4 "剔除最高5%") ///
        rows(1) ring(1) pos(6))
graph export "../结果/Chin et al半参_无控.png",replace

twoway ///
    (histogram Cash, frequency color(gs14) bin(50) yaxis(2)) ///
    (line plot_fB Cash, sort lcolor(blue) lpattern(solid)) ///
    (line plot_fB1 Cash, sort lcolor(cyan) lpattern(dash)) ///
    (line plot_fB2 Cash, sort lcolor(red) lpattern(dash)), ///
    ytitle("Tobin_Q拟合值", axis(1)) ///
    xtitle("Cash") ///
    title("加入控制变量") ///
    legend( order(2 3 4) ///
        label(2 "全部样本") ///
        label(3 "剔除最低10%") ///
        label(4 "剔除最高5%") ///
        rows(1) ring(1) pos(6))
graph export "../结果/Chin et al半参_有控.png",replace

***** 非参拟合图叠加双线检验图
twoway ///
    (line plot_fB Cash, sort lcolor(gs14*1.5) lpattern(dash)) ///
    (line y1 x1,lpat(solid) lcol(blue*1.1)) ///
    (line y2 x2,lpat(solid) lcol(red*1.1)), ///
	xline($gxstar, lpattern(dash) lcolor(black) lwidth(thin)) ///
    ytitle("Tobin_Q", axis(1)) ///
    xtitle("Cash") ///
    legend( order(1 2 3) ///
        label(1 "非参数拟合") ///
        label(2 "左侧直线") ///
        label(3 "右侧直线") ///
        rows(1) ring(1) pos(6))
graph export "../结果/Chin et al叠加.png",replace