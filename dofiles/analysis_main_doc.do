/*******************************************************************************
* Analyses for test time project, main document
* Last edited: 20160113 by hhs@sfi.dk
*******************************************************************************/

* Load preamble
do "D:\Data\workdata\704335\Timeofday\dofiles\preamble.do"
* Set memory
set max_memory 4g, perm

/*******************************************************************************
* Outline:
* 1 Main figure hourly bars	
* 2 The effect of breaks
* 3 figure break bars and quantile regs
*******************************************************************************/

/*******************************************************************************
	1 Main figure hourly bars
*******************************************************************************/
* Load data
use "$tf\analysisdata`sample'.dta",clear
* set fixed effect level
xtset instnr
* singletons in instnr?
bys instnr: gen count=_n
bys instnr: egen instcount=max(count)
* estimate with basic vars
eststo: qui: xtreg testscore_std th2-th6 $controls2 if instcount>1, cluster(instnr) fe
* create dataset to save estimates
clear
set obs 5
gen th=_n+1
* save estimates for testhour==9
gen u= _b[th2]+invttail(e(df_r),0.025)*_se[th2]
gen l= _b[th2]-invttail(e(df_r),0.025)*_se[th2]
gen beta=_b[th2]
* save estimates for testhour>9
forval i=2/5{
local j=`i'+1
replace beta= _b[th`j']-_b[th`i'] if th==`j'
test th`j'-th`i'=0
replace u= _b[th`j']-_b[th`i']+invttail(e(df_r),0.025)*((_b[th`j']-_b[th`i'])/(r(F)^.5)) if th==`j'
replace l= _b[th`j']-_b[th`i']-invttail(e(df_r),0.025)*((_b[th`j']-_b[th`i'])/(r(F)^.5)) if th==`j'
}
* Adjust testhour such that 9 starts at 9 (and is not centered at 9
gen testhour=th+7
replace testhour=9.375 if testhour==9
replace testhour=10.375 if testhour==10
replace testhour=11.375 if testhour==11
replace testhour=12.375 if testhour==12
replace testhour=13.375 if testhour==13
replace testhour=14.375 if testhour==14
* make tw plot
twoway  (bar beta testhour, fcolor(orange_red) lwidth(medthick) lcolor(orange_red) barwidth(.75)) ///
	   (rcap u l testhour, fcolor(black) lcolor(gs3) lwidth(medium)) ///
		,ylabel(#7 , nogrid noticks) graphregion(fcolor(white) lcolor(white)) ///
			xlabel(,noticks) plotregion(fcolor(white) lcolor(black)) ///
			yline(0,lcolor(black) ) title(" ") ///
			ytitle("Change in test score (SD)") ///
			legend(off)  xtitle("Test starting time") ///
			xlabel(8 "8:00" 9 "9:00" 10 "10:00" 11 "11:00" 12 "Noon" 13 "13:00")
graph export "$df\main_graph_testhour.png",replace width(4000)



/*******************************************************************************
	2 The effect of breaks
*******************************************************************************/
use "$tf\analysisdata`sample'.dta",clear
* set fixed effect level
xtset instnr
* regress
set matsize 5000
preserve
* create dataset to save estimates
clear 
set obs 7
gen id=_n
gen breakbeta=.
gen breakl=.
gen breaku=.
gen thbeta=.
gen thl=.
gen thu=.
save "$tf\estimates.dta",replace
restore
eststo clear
* program to run regressions

cap program drop myreg
program myreg
	syntax, id(string) [condition(string)]
	eststo: qui: xtreg testscore_std testhour  break $controls2 `condition', cluster(instnr) fe nonest
	estadd scalar DF=  e(df_m)
	estadd scalar groups=  e(N_g)
	estadd scalar sgroup=   e(g_min)
	estadd scalar lgroup= e(g_max)
	estadd scalar ar2=e(r2_a) 
	preserve
	use  "$tf\estimates.dta",clear
	replace breakbeta=_b[break] if id==`id'
	replace breakl=_b[break]-invttail(e(df_r),0.025)*_se[break] if id==`id'
	replace breaku=_b[break]+invttail(e(df_r),0.025)*_se[break] if id==`id'
	replace thbeta=_b[testhour] if id==`id'
	replace thl=_b[testhour]-invttail(e(df_r),0.025)*_se[testhour] if id==`id'
	replace thu=_b[testhour]+invttail(e(df_r),0.025)*_se[testhour] if id==`id'
	save  "$tf\estimates.dta",replace
end 
* main
* singletons in instnr?
bys instnr: gen count=_n
bys instnr: egen instcount=max(count)
myreg, id(1) condition(if instcount>1)
myreg, id(2) condition(if sub==6 & instcount>1)
myreg, id(3) condition(if sub==1 & instcount>1)
myreg, id(4) condition(if grade<5 & instcount>1)
myreg, id(5) condition(if grade>5 & instcount>1)
myreg, id(6) condition(if breakdata==1 & instcount>1)
xtset id
drop count
bys id: gen count=_n
bys id: egen obs=max(count)
drop if obs==1
myreg, id(7) 

* graph
use  "$tf\estimates.dta",clear
gen expand=2
expand expand
bys id: gen show=_n
replace id=id-0.2 if show==1
replace id=id+0.2 if show==2
tw (bar breakbeta id if show==2, barwidth(0.4) fcolor(blue) lcolor(blue)) (rcap breaku breakl id if show==2, lcolor(black) ) ///
	(bar thbeta id if show==1, barwidth(0.4) fcolor(red) lcolor(red))   (rcap thu thl id  if show==1, lcolor(black)), ///
	xlabel(1 "Main" 2 "Math" 3 "Reading" 4 "Young" 5 "Old" 6 "Breakdata" 7 "Ind. FE", noticks) ///
	yline(0,lcolor(black)) ylabel(-0.02(0.01)0.05, nogrid noticks)  graphregion(fcolor(white) lcolor(white)) ///
	 plotregion(fcolor(white) lcolor(black)) legend(order(3 "Hourly effect" 1 "Effect of a break") region(lcolor(white))) xtitle("") ytitle("Effect size (SD)")
	 graph export "$df\main_breakdata.png",replace width(4000)
esttab using "$df\appendix_reg_hetero_table.csv",  stats(groups sgroup lgroup DF ar2 aic N, fmt(%11.3f)) b(%5.3f) ///
		keep(break testhour) nolines nonotes se nonumbers fragment  ///
		subs("[1em]" " ") label replace

	 
/*******************************************************************************
	3 figure break bars and quantile regs
*******************************************************************************/
* Load data

use "$tf\analysisdata`sample'.dta",clear
* set fixed effect level
xtset instnr
* singletons in instnr
bys instnr: gen count=_n
bys instnr: egen instcount=max(count)
drop if instcount==1
* regress
set matsize 5000
qui: xtreg testscore_std testhour break $controls2, cluster(instnr) fe 
* create dataset to save estimates
preserve
clear
set obs 9
gen id=_n
gen u= .
gen l= .
gen betamean=_b[break] 
gen betameanth=_b[testhour] 
gen beta=.
gen uth=.
gen lth=.
gen betath=.
save "$tf\estimatesq.dta",replace
restore
* quantile reg
qui: xtreg testscore_std  break $controls2, 
predict fe, u
gen y=testscore_std-fe
qui:tab grade,gen(grade)
qui:tab sub,gen(sub)
qui:tab testyear,gen(testyear)
qui:tab dow,gen(dow)

forval i=1/9{
local l=`i'*10
qreg2  y break testhour grade1-grade6 sub1-sub6 testyear1-testyear4 dow1-dow5 $covariates $missings,  quantile(`l') c(instnr)
preserve
use "$tf\estimatesq.dta",clear
replace u= _b[break]+invttail(e(df_r),0.025)*_se[break] if id==`i'
replace l= _b[break]-invttail(e(df_r),0.025)*_se[break] if id==`i'
replace beta=_b[break] if id==`i'
replace uth= _b[testhour]+invttail(e(df_r),0.025)*_se[testhour] if id==`i'
replace lth= _b[testhour]-invttail(e(df_r),0.025)*_se[testhour] if id==`i'
replace betath=_b[testhour] if id==`i'
save "$tf\estimatesq.dta",replace
restore
}

* MAKE GRAPH
use "$tf\estimatesq.dta",clear
replace id=id*10
tw 	(line beta id, lcolor(blue)) ///
	(line betath id, lcolor(red)) ///
   (line u id, lcolor(blue) lpattern(dash) ) ///
   (line l id, lcolor(blue) lpattern(dash) ) ///
    (line uth id, lcolor(red) lpattern(dash) ) ///
   (line lth id, lcolor(red) lpattern(dash) ) ///
   , xtitle(Testscore percentile) ytitle("Effect size (SD)") ///
   legend(order ( 1 "Effect of a break" 2 "Hourly effect") ///
   region(lcolor (white))) yline(0,lcolor(black)) ///
   xlabel(0(10)100) ylabel(-0.03(0.01)0.05,nogrid noticks) graphregion(fcolor(white) lcolor(white)) ///
						xlabel(,noticks) plotregion(fcolor(white) lcolor(black)) 
   
graph export "$df\main_breakdata_quantiles.png",replace width(4000)
