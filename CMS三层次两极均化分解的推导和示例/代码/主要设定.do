set rmsg on

cd "D:\My Files\文献\ZB 投入产出库\ZB3 创见输出\经济学\世界、区域经济学（空间经济学）\CMS模型"

***** 定义出口国
global C 156

***** 输入数据与输出文件
global cms_rawfile ".\数据\HS_2017_2024.dta"
global cms_datafile ".\数据\S_2017_2024.dta"
global cms_outfile ".\结果\CMS分解汇总.xlsx"
global dofile ".\代码\CMS程序.do"

***** 定义需要依次输出的方案
* 1/15表示依次输出方案1至15；也可写成 "1/3 10/15" 选择部分方案。
* 改变编号的顺序即可改变结果列的顺序。
global cms_cases "1/15"

* all表示输入数据中的全部国家或全部产业。
***** 方案1：2017—2024年
global t0_1 2017
global t_1 2024
global region_j_1 "all"
global industry_hs2_1 "all"
global title_1 "2017—2024年"

***** 方案2：2017—2020年
global t0_2 2017
global t_2 2020
global region_j_2 "all"
global industry_hs2_2 "all"
global title_2 "2017—2020年"

***** 方案3：2020—2024年
global t0_3 2020
global t_3 2024
global region_j_3 "all"
global industry_hs2_3 "all"
global title_3 "2020—2024年"

***** 方案4：2017—2024年，美俄
global t0_4 2017
global t_4 2024
global region_j_4 "842 643"
global industry_hs2_4 "all"
global title_4 "美俄"

***** 方案5：2017—2024年，日韩
global t0_5 2017
global t_5 2024
global region_j_5 "392 410"
global industry_hs2_5 "all"
global title_5 "日韩"

***** 方案6：2017—2024年，美国
global t0_6 2017
global t_6 2024
global region_j_6 "842"
global industry_hs2_6 "all"
global title_6 "美国"

***** 方案7：2017—2024年，俄罗斯
global t0_7 2017
global t_7 2024
global region_j_7 "643"
global industry_hs2_7 "all"
global title_7 "俄罗斯"

***** 方案8：2017—2024年，日本
global t0_8 2017
global t_8 2024
global region_j_8 "392"
global industry_hs2_8 "all"
global title_8 "日本"

***** 方案9：2017—2024年，韩国
global t0_9 2017
global t_9 2024
global region_j_9 "410"
global industry_hs2_9 "all"
global title_9 "韩国"

***** 方案10：2017—2024年，铜产业
global t0_10 2017
global t_10 2024
global region_j_10 "all"
global industry_hs2_10 "74"
global title_10 "铜产业"

***** 方案11：2017—2024年，镍产业
global t0_11 2017
global t_11 2024
global region_j_11 "all"
global industry_hs2_11 "75"
global title_11 "镍产业"

***** 方案12：2017—2024年，铝产业
global t0_12 2017
global t_12 2024
global region_j_12 "all"
global industry_hs2_12 "76"
global title_12 "铝产业"

***** 方案13：2017—2024年，铅产业
global t0_13 2017
global t_13 2024
global region_j_13 "all"
global industry_hs2_13 "78"
global title_13 "铅产业"

***** 方案14：2017—2024年，锌产业
global t0_14 2017
global t_14 2024
global region_j_14 "all"
global industry_hs2_14 "79"
global title_14 "锌产业"

***** 方案15：2017—2024年，锡产业
global t0_15 2017
global t_15 2024
global region_j_15 "all"
global industry_hs2_15 "80"
global title_15 "锡产业"

***** 进口额和进口数量整理
* 原始数据保留所有出口国，用于计算j国i产品的世界进口需求。
use "$cms_rawfile", clear

gsort year importer product exporter

// 出口国对j国i产品的出口额EX_ij，j国i产品的世界进口额D_ij
// 出口国对j国i产品的出口数量Q_Cij，j国i产品的世界进口数量Q_Wij
rename product i
rename importer j

bysort year j i: egen D_ij = total(value)
bysort year j i: egen EX_ij = total(cond(exporter == $C, value, 0))
bysort year j i: egen Q_Wij = total(quantity)
bysort year j i: egen Q_Cij = total(cond(exporter == $C, quantity, 0))

by year j i: keep if _n == 1
keep year j hs2 i D_ij EX_ij Q_Wij Q_Cij

fillin year i j
foreach v in EX_ij D_ij Q_Cij Q_Wij {
    replace `v' = 0 if missing(`v')
}
drop _fillin
replace hs2 = floor(i/10000) if missing(hs2)
gsort year j i

save "$cms_datafile",replace

***** 加载CMS程序
do "$dofile"

***** 建立结果表
putexcel set "$cms_outfile", replace
putexcel A1 = ("波动效应")
local row = 2
foreach v in DEX IME GIM PIM QIM SIM CIM MIM EXE GEX PEX QEX KEX SEX CEX MEX {
    putexcel A`row' = ("`v'")
    local ++row
}

***** 逐个方案读取参数并调用cms
* copy global按宏名复制内容
local col = 2
foreach s of numlist $cms_cases {
    global t0 : copy global t0_`s'
    global t : copy global t_`s'
    global region_j : copy global region_j_`s'
    global industry_hs2 : copy global industry_hs2_`s'
    global cms_title : copy global title_`s'
    global cms_col = `col'

    cms

    * 将当前列号转换为Excel列名
    local colnum = `col'
    local excel_col ""
    while `colnum' > 0 {
        local digit = mod(`colnum'-1,26)
        local excel_col = char(65+`digit')+"`excel_col'"
        local colnum = floor((`colnum'-1)/26)
    }

    * 更新列标题
    putexcel `excel_col'1 = ("$cms_title")

    local ++col
}

* 汇总表中DEX为出口变动额（沿用数据单位：千美元），其余为贡献率（%）。
