
set rmsg on

cd "`c(pwd)'" // 切换到当前目录

***** 假阳性
clear all
set obs 100000
set seed 114514

gen u = runiform(0,1)
gen x = u^2
gen y = log(x)

reg y x c.x#c.x

predict y_bar, xb

local b1 : display %5.2f _b[x]
local b2 : display %5.2f _b[c.x#c.x]*(-1)
local b3 : display %5.2f _b[_cons]*(-1)

twoway ///
    (line y x if y >= -4, sort lcolor(black) lwidth(medthick)) ///
    (line y_bar x if y_bar >= -4, sort lcolor(red) lpattern(dash) lwidth(medthick)), ///
    xtitle("x") ///
    ytitle("y") ///
	yscale(range(-4 0)) ///
    title("假阳性") ///
    legend(order(1 "真实函数：y = ln(x)" 2 "二次模型拟合：y = `b1'x - `b2'x{sup:2} - `b3'") cols(1) position(6) ring(0) region(lcolor(none))) ///
    graphregion(color(white)) ///
    plotregion(color(white))
	
graph export "../结果/假阳性.png",replace

***** 假阴性
clear all
set obs 100000
set seed 114514

gen u = runiform(0,1)
gen x = u^2
gen y = x - 0.5*x^8

gen x2 = x*x
reg y x x2

predict y_bar, xb

local b1 : display %5.2f _b[x]
local b2 : display %5.2f _b[x2]*(-1)
local b3 : display %5.2f _b[_cons]*(-1)

twoway ///
    (line y x, sort lcolor(black) lwidth(medthick)) ///
    (line y_bar x, sort lcolor(red) lpattern(dash) lwidth(medthick)), ///
    xtitle("x") ///
    ytitle("y") ///
    title("假阴性") ///
    legend(order(1 "真实函数：y = x - 0.5x{sup:8}" 2 "二次模型拟合：y = `b1'x - `b2'x{sup:2} - `b3'") cols(1) position(6) ring(0) region(lcolor(none))) ///
    graphregion(color(white)) ///
    plotregion(color(white))
	
graph export "../结果/假阴性.png",replace