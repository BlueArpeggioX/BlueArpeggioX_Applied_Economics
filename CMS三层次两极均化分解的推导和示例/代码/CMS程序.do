
capture program drop cms
program define cms

***** 读取数据
use "$cms_datafile",clear

***** 选择区域
if "$region_j" != "all" {
    gen byte region_sample = 0
    foreach c of numlist $region_j {
        replace region_sample = 1 if j == `c'
    }
    keep if region_sample == 1
    drop region_sample
}

***** 选择产业
if "$industry_hs2" != "all" {
    gen byte industry_sample = 0
    foreach h of numlist $industry_hs2 {
        replace industry_sample = 1 if hs2 == `h'
    }
    keep if industry_sample == 1
    drop industry_sample
}

***** 第一层次DEX=IME+EXE
keep if inlist(year, $t0, $t)
reshape wide EX_ij D_ij Q_Cij Q_Wij, i(i j) j(year)
rename (EX_ij$t0 EX_ij$t D_ij$t0 D_ij$t) (EX_ij_0 EX_ij_t D_ij_0 D_ij_t)
rename (Q_Cij$t0 Q_Cij$t Q_Wij$t0 Q_Wij$t) (Q_Cij_0 Q_Cij_t Q_Wij_0 Q_Wij_t)

* 出口市场份额
gen S_ij_0 = cond(D_ij_0 > 0, EX_ij_0/D_ij_0, 0)
gen S_ij_t = cond(D_ij_t > 0, EX_ij_t/D_ij_t, 0)
gen S_ij_bar = (S_ij_0+S_ij_t)/2
gen Delta_S_ij = S_ij_t-S_ij_0

gen D_ij_bar = (D_ij_0+D_ij_t)/2
gen Delta_D_ij = D_ij_t-D_ij_0

* DEX
foreach s in 0 t {
    quietly summarize EX_ij_`s', meanonly
    scalar EX_`s' = r(sum)
}
gen DEX = scalar(EX_t)-scalar(EX_0)

* IME
gen IME_ij = S_ij_bar*Delta_D_ij
quietly summarize IME_ij, meanonly
gen IME = r(sum)

* EXE
gen EXE_ij = Delta_S_ij*D_ij_bar
quietly summarize EXE_ij, meanonly
gen EXE = r(sum)

***** 第二层次
foreach s in 0 t {
    quietly summarize D_ij_`s', meanonly
    scalar D_`s' = r(sum)
    scalar S_`s' = scalar(EX_`s')/scalar(D_`s')
}
scalar Delta_D = scalar(D_t)-scalar(D_0)
scalar Delta_S = scalar(S_t)-scalar(S_0)
scalar D_bar = (scalar(D_0)+scalar(D_t))/2
scalar S_bar = (scalar(S_0)+scalar(S_t))/2

gsort j i

**** IME分解 IME=GIM+SIM
gen GIM = scalar(S_bar)*scalar(Delta_D)
gen SIM = IME-GIM

**** EXE分解 EXE=GEX+SEX
gen GEX = scalar(Delta_S)*scalar(D_bar)
gen SEX = EXE-GEX

***** 第三层次
**** GIM分解 GIM=PIM+QIM
foreach s in 0 t {
    quietly summarize Q_Wij_`s', meanonly
    scalar Q_im_`s' = r(sum)
    scalar P_im_`s' = scalar(D_`s')/scalar(Q_im_`s')
}
scalar Delta_P_im = scalar(P_im_t)-scalar(P_im_0)
scalar Delta_Q_im = scalar(Q_im_t)-scalar(Q_im_0)
scalar P_im_bar = (scalar(P_im_0)+scalar(P_im_t))/2
scalar Q_im_bar = (scalar(Q_im_0)+scalar(Q_im_t))/2

gen PIM = scalar(S_bar)*scalar(Delta_P_im)*scalar(Q_im_bar)
gen QIM = scalar(S_bar)*scalar(P_im_bar)*scalar(Delta_Q_im)

**** SIM分解 SIM=CIM+MIM
foreach s in 0 t {
	bysort i: egen EX_i_`s' = total(EX_ij_`s')
	bysort i: egen D_i_`s' = total(D_ij_`s')
	gen S_i_`s' = cond(D_i_`s' > 0, EX_i_`s'/D_i_`s', 0)
	bysort j: egen EX_j_`s' = total(EX_ij_`s')
	bysort j: egen D_j_`s' = total(D_ij_`s')
	gen S_j_`s' = cond(D_j_`s' > 0, EX_j_`s'/D_j_`s', 0)
}
gen S_i_bar = (S_i_0+S_i_t)/2
gen S_j_bar = (S_j_0+S_j_t)/2
gen Delta_D_i = D_i_t-D_i_0
gen Delta_D_j = D_j_t-D_j_0

// 汇总值在组内各行重复，让每个产品和进口国只参与求和一次
bysort i (j): gen byte tag_i = (_n == 1)
bysort j (i): gen byte tag_j = (_n == 1)

gen S_i_bar_Delta_D_i = S_i_bar*Delta_D_i
quietly summarize S_i_bar_Delta_D_i if tag_i, meanonly
scalar sum_S_i_bar_Delta_D_i = r(sum)

gen S_j_bar_Delta_D_j = S_j_bar*Delta_D_j
quietly summarize S_j_bar_Delta_D_j if tag_j, meanonly
scalar sum_S_j_bar_Delta_D_j = r(sum)

gen CIM = (SIM-scalar(sum_S_i_bar_Delta_D_i)+scalar(sum_S_j_bar_Delta_D_j))/2
gen MIM = (SIM-scalar(sum_S_j_bar_Delta_D_j)+scalar(sum_S_i_bar_Delta_D_i))/2

**** GEX分解 GEX=PEX+QEX+KEX
// 先求和，再计算三元边际
foreach s in 0 t {
	bysort i: egen Q_CJi_`s' = total(Q_Cij_`s')
	bysort i: egen Q_WJi_`s' = total(Q_Wij_`s')
	gen M_CJi_`s' = EX_i_`s'
	gen M_WJi_`s' = D_i_`s'

	gen byte N_CJ_i_`s' = M_CJi_`s' > 0
	gen M_WJi_N_CJ_`s' = cond(N_CJ_i_`s', M_WJi_`s', 0)
	quietly summarize M_WJi_N_CJ_`s' if tag_i, meanonly
	scalar sum_M_WJi_N_CJ_`s' = r(sum)
	scalar K_`s' = scalar(sum_M_WJi_N_CJ_`s')/scalar(D_`s')

	gen phi_CJi_`s' = M_CJi_`s'/scalar(EX_`s') if N_CJ_i_`s'
	gen phi_WJi_`s' = M_WJi_`s'/scalar(sum_M_WJi_N_CJ_`s') if N_CJ_i_`s'
	gen lnphi_diff_i_`s' = ln(phi_CJi_`s')-ln(phi_WJi_`s') if N_CJ_i_`s'

	gen L_i_`s' = (phi_CJi_`s'+phi_WJi_`s')/2 if N_CJ_i_`s'
	replace L_i_`s' = (phi_CJi_`s'-phi_WJi_`s')/lnphi_diff_i_`s' ///
		if N_CJ_i_`s' & abs(lnphi_diff_i_`s') > 1e-10
	quietly summarize L_i_`s' if tag_i, meanonly
	scalar sum_L_i_`s' = r(sum)
	gen omega_i_`s' = L_i_`s'/scalar(sum_L_i_`s') if N_CJ_i_`s'

	gen P_CJi_`s' = M_CJi_`s'/Q_CJi_`s' if N_CJ_i_`s'
	gen P_WJi_`s' = M_WJi_`s'/Q_WJi_`s' if N_CJ_i_`s'
	gen ln_P_ratio_i_`s' = omega_i_`s'*ln(P_CJi_`s'/P_WJi_`s') if N_CJ_i_`s'
	gen ln_Q_ratio_i_`s' = omega_i_`s'*ln(Q_CJi_`s'/Q_WJi_`s') if N_CJ_i_`s'
	quietly summarize ln_P_ratio_i_`s' if tag_i, meanonly
	scalar P_`s' = exp(r(sum))
	quietly summarize ln_Q_ratio_i_`s' if tag_i, meanonly
	scalar Q_`s' = exp(r(sum))
}

* 三个均值
scalar Delta_P = scalar(P_t)-scalar(P_0)
scalar Delta_Q = scalar(Q_t)-scalar(Q_0)
scalar Delta_K = scalar(K_t)-scalar(K_0)
scalar QK_bar = (2*scalar(Q_t)*scalar(K_t)+scalar(Q_0)*scalar(K_t)+scalar(Q_t)*scalar(K_0)+2*scalar(Q_0)*scalar(K_0))/6
scalar PK_bar = (2*scalar(P_t)*scalar(K_t)+scalar(P_0)*scalar(K_t)+scalar(P_t)*scalar(K_0)+2*scalar(P_0)*scalar(K_0))/6
scalar PQ_bar = (2*scalar(P_t)*scalar(Q_t)+scalar(P_0)*scalar(Q_t)+scalar(P_t)*scalar(Q_0)+2*scalar(P_0)*scalar(Q_0))/6

gen PEX = scalar(D_bar)*scalar(Delta_P)*scalar(QK_bar)
gen QEX = scalar(D_bar)*scalar(Delta_Q)*scalar(PK_bar)
gen KEX = scalar(D_bar)*scalar(Delta_K)*scalar(PQ_bar)

**** SEX分解 SEX=CEX+MEX
gen D_i_bar = (D_i_0+D_i_t)/2
gen D_j_bar = (D_j_0+D_j_t)/2
gen Delta_S_i = S_i_t-S_i_0
gen Delta_S_j = S_j_t-S_j_0

gen Delta_S_i_D_i_bar = Delta_S_i*D_i_bar
quietly summarize Delta_S_i_D_i_bar if tag_i, meanonly
scalar sum_Delta_S_i_D_i_bar = r(sum)

gen Delta_S_j_D_j_bar = Delta_S_j*D_j_bar
quietly summarize Delta_S_j_D_j_bar if tag_j, meanonly
scalar sum_Delta_S_j_D_j_bar = r(sum)

gen CEX = (SEX-scalar(sum_Delta_S_i_D_i_bar)+scalar(sum_Delta_S_j_D_j_bar))/2
gen MEX = (SEX-scalar(sum_Delta_S_j_D_j_bar)+scalar(sum_Delta_S_i_D_i_bar))/2

***** 贡献率与结果输出
foreach s in IME EXE GIM SIM GEX SEX PIM QIM CIM MIM PEX QEX KEX CEX MEX {
	gen g_`s' = `s'/DEX * 100
}
order DEX g_IME g_GIM g_PIM g_QIM g_SIM g_CIM g_MIM g_EXE g_GEX g_PEX g_QEX g_KEX g_SEX g_CEX g_MEX

local colnum = $cms_col
local excel_col ""
while `colnum' > 0 {
    local digit = mod(`colnum'-1,26)
    local excel_col = char(65+`digit')+"`excel_col'"
    local colnum = floor((`colnum'-1)/26)
}

putexcel set "$cms_outfile", modify
putexcel `excel_col'1 = ("$t0"+"-"+"$t"+"年；区域："+"$region_j"+"；产业："+"$industry_hs2")

local row = 2
foreach v in DEX g_IME g_GIM g_PIM g_QIM g_SIM g_CIM g_MIM g_EXE g_GEX g_PEX g_QEX g_KEX g_SEX g_CEX g_MEX {
    putexcel `excel_col'`row' = (`v'[1])
    local ++row
}

end
