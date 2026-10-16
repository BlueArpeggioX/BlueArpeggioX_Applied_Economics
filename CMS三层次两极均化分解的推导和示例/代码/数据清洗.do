set rmsg on

cd "D:\My Files\文献\ZB 投入产出库\ZB3 创见输出\经济学\世界、区域经济学（空间经济学）\CMS模型"

***** 删除多余数据并合并
local baseyear 2017
forvalues i = `baseyear'(1)2024 {
	import delimited using ".\数据\BACI_HS17_Y`i'_V202601.csv",clear
	rename t year
	rename i exporter
	rename j importer
	rename k product
	rename v value
	rename q quantity
	
	label variable product "产品代码HS6"
	label variable value "成交金额，千美元"
	
	gen hs2 = floor(product / 10000)
	
	keep if inlist(importer,392,410,643,842)
	keep if inlist(hs2, 74, 75, 76, 78, 79, 80)
	
	drop if missing(value)
	drop if missing(quantity)
	
	save ".\数据\HS2017_Y`i'.dta",replace
}

use ".\数据\HS2017_Y2017.dta", clear
local nextyear = 2018
forvalues i = `nextyear'/2024 {
	append using ".\数据\HS2017_Y`i'.dta"
}

order year exporter importer hs2 product value quantity
gsort year importer product exporter

save ".\数据\HS_2017_2024.dta", replace