/*******************************************************************************
* Analyses for test time project, appendix
* Last edited: 20160113 by hhs@sfi.dk
*******************************************************************************/

* Load preamble
do "D:\Data\workdata\704335\Timeofday\dofiles\preamble.do"
* Set memory
set max_memory 4g, perm

/*******************************************************************************
* Outline:
* 1 Descriptives
* 2 Main regression table
* 3 Distributions
* 4 Compare precision
* 5 Main regression table with percentiles
* 6 Effect size
*******************************************************************************/




/*******************************************************************************
1 Descriptives
*******************************************************************************/
use "$tf\analysisdata`sample'.dta",clear

* My little program to create covariate means
cap program drop mytab
program mytab
syntax varlist [using/]
preserve
		* close handle if open
			cap file close mytab
		* open handle
			file open mytab using "`using'",write replace
		* temporary variables
			tempvar s1 s2 s3 s4 s5 s6 s7 s8
			forval i=1/8{
				qui: gen `s`i''=.
				}
		* Replace missing obs with missing, if relevant
		cap replace `l'=. if missing_`l'==1

		foreach l of local varlist{

		* Save mean values
		qui: sum `l' 
		qui: replace `s1'=r(mean)
		qui: sum `l'  if testhour==8
		qui: replace `s2'=r(mean)
		qui: sum `l'  if testhour==9
		qui: replace `s3'=r(mean)
		qui: sum `l'  if testhour==10
		qui: replace `s4'=r(mean)
		qui: sum `l'  if testhour==11
		qui: replace `s5'=r(mean)
		qui: sum `l'  if testhour==12
		qui: replace `s6'=r(mean)
		qui: sum `l'  if testhour==13
		qui: replace `s7'=r(mean)
		
		local label: variable lab `l'
		file write mytab  "`label':" _tab _tab _tab %8.3f (`s1') ";" %8.3f (`s2') ";" %8.3f (`s3') ";" ///
						  %8.3f (`s4') ";" %8.3f (`s5') ";"  %8.3f (`s6') ";" %8.3f (`s7') _n
		}
		* Number of Observations
forval i=1/1{
		qui: sum testscore_std 
		qui: replace `s1'=r(N)
		qui: sum testscore_std  if testhour==8
		qui: replace `s2'=r(N)
		qui: sum testscore_std  if testhour==9
		qui: replace `s3'=r(N)
		qui: sum testscore_std  if testhour==10
		qui: replace `s4'=r(N)
		qui: sum testscore_std  if testhour==11
		qui: replace `s5'=r(N)
		qui: sum testscore_std  if testhour==12
		qui: replace `s6'=r(N)
		qui: sum testscore_std  if testhour==13
		qui: replace `s7'=r(N)
}
		file write mytab  "Observations:" _tab _tab _tab %12.0fc (`s1') ";"  %12.0fc (`s2') ";"  %12.0fc (`s3') ";" ///
						  %12.0fc (`s4') ";"  %12.0fc (`s5') ";"  %12.0fc (`s6')   ";"  %12.0fc (`s7') ";"  %12.0fc (`s8')    _n
		file close mytab
	restore
end	
		
		
* Main table of descriptives 
mytab   uncert schoolday $covariates $missings using "$df\appendix_descriptives.txt"
keep if breakdata==1
* Table of descriptives selected schools
mytab   uncert schoolday $covariates  $missings using "$df\appendix_descriptives_surveyed.txt"

/*******************************************************************************
	2 Main regression table
*******************************************************************************/
* Load data
use "$tf\analysisdata`sample'.dta",clear
* set fixed effect level
xtset instnr
* singletons in instnr
bys instnr: gen count=_n
bys instnr: egen instcount=max(count)
* Now create table with six eduumns
eststo clear
* Test hour effect, no controls
eststo: qui: reg testscore_std testhour, cluster(instnr) 
estadd scalar Fval=r(F)
estadd scalar Pval=r(p)
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
* Hourly effect, no controls
eststo: qui: reg testscore_std th2-th6, cluster(instnr) 
qui: test th2=th3=th4=th5=th6=0
estadd scalar Fval=r(F)
estadd scalar Pval=r(p)
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
* Hourly effect, basic controls
eststo: qui: xtreg testscore_std th2-th6 $controls1 if instcount>1, cluster(instnr) fe
qui: test th2=th3=th4=th5=th6=0
estadd scalar Fval=r(F)
estadd scalar Pval=r(p)
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
* Hourly effect, extended controls
eststo: qui: xtreg testscore_std th2-th6 $controls2 if instcount>1, cluster(instnr) fe
qui: test th2=th3=th4=th5=th6=0
estadd scalar Fval=r(F)
estadd scalar Pval=r(p)
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
* Break effect, no controls
eststo: qui: reg testscore_std testhour break, cluster(instnr) 
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
* Break effect, basic controls
eststo: qui: xtreg testscore_std testhour break $controls1 if instcount>1, cluster(instnr) fe
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
* Break effect, extended controls
eststo: qui: xtreg testscore_std testhour break $controls2 if instcount>1, cluster(instnr) fe
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
esttab using "$df\appendix_reg_table.csv",  stats( groups sgroup lgroup Fval Pval DF ar2 aic N, fmt(%11.3f)) b(%5.3f) ///
		keep(break testhour th2 th3 th4 th5 th6) nolines nonotes se nonumbers fragment  ///
		subs("[1em]" " ") label replace


		
/*******************************************************************************
3 Distribution
*******************************************************************************/
* Load data
use "$tf\analysisdata`sample'.dta",clear
* make density plots
 twoway (kdensity testscore_std if testhour==8,lcolor(gs11)  bwidth(0.25) kernel(triangle)) ///
		(kdensity testscore_std if testhour==9,lcolor(gs9)   bwidth(0.25) kernel(triangle)) ///
		(kdensity testscore_std if testhour==10,lcolor(gs7)  bwidth(0.25) kernel(triangle)) ///
		(kdensity testscore_std if testhour==11,lcolor(gs5)  bwidth(0.25) kernel(triangle)) ///
		(kdensity testscore_std if testhour==12,lcolor(gs3)  bwidth(0.25) kernel(triangle)) ///
		(kdensity testscore_std if testhour==13,lcolor(gs1)  bwidth(0.25) kernel(triangle)) ///
		,legend(order(1 "8AM" 2 "9AM" 3 "10AM" 4 "11AM" 5 "Noon" 6 "1PM") rows(2) )  ///
		graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(black)) ///
		ylabel(,noticks) xlabel(,noticks) ytitle("Density") xtitle("Standardized test score")
			graph export "$df\appendix_test_distribution.png",replace width(4000)
			
use "$tf\analysisdata`sample'.dta",clear
* set fixed effect level
xtset instnr
* singletons in instnr
bys instnr: gen count=_n
bys instnr: egen instcount=max(count)
qui: xtreg testscore_std testhour break $controls2 if instcount>1, cluster(instnr) fe		
predict res, u
twoway (kdensity res if testhour==8,lcolor(gs11)  bwidth(0.25) kernel(triangle)) ///
		,legend(off )  ///
		graphregion(fcolor(white) lcolor(white)) plotregion(fcolor(white) lcolor(black)) ///
		ylabel(,noticks) xlabel(,noticks) ytitle("Density") xtitle("Residuals")
			graph export "$df\appendix_residual_distribution.png",replace width(4000)	
/*******************************************************************************
4 Compare precision across samples
*******************************************************************************/
* Create empty dataset to save estimates
clear
set obs 4
gen id=_n
gen depvar="edu"
replace depvar="birthweight" if id==2
replace depvar="inc" if id==3
replace depvar="testscore_std" if id==4
gen expand=5
expand expand
drop expand
bys id: gen th=_n+1
gen expand=10
expand expand
drop expand
bys th depvar: gen sample=_n
gen samplesize=.
gen tstat=.
save "$tf\estimates.dta",replace
* Load data
use "$tf\analysisdata`sample'.dta",clear
xtset instnr
* singletons in instnr
bys instnr: gen count=_n
bys instnr: egen instcount=max(count)
drop if instcount==1
* run regressions
gen missing_testscore_std=testscore_std==.
foreach l in  edu inc birthweight testscore_std {
forval a=1(1)10{
	local i=`a'/10
	di "`i'"
	preserve 
	keep if runiform()<`i'
	qui: sum testscore_std
	local N=r(N)
	qui: xtreg  `l' th2-th6 $controls1 if missing_`l'!=1, cluster(instnr) fe 
	restore
	preserve
	use "$tf\estimates.dta",clear
	replace samplesize=`N' if depvar=="`l'" & sample==`a'
	forval j=2/6{
	replace tstat=_b[th`j']/_se[th`j'] if depvar=="`l'" & sample==`a' & th==`j'
	}
	save "$tf\estimates.dta",replace
	restore
}
}
	
use "$tf\estimates.dta",clear
* make graph
replace tstat=abs(tstat)


tw  (scatter tstat samplesize if th==2 & depvar=="testscore_std" ,msymbol(o) mcolor(green)) ///
	(scatter tstat samplesize if th==3 &  depvar=="testscore_std",msymbol(d)  mcolor(green)) ///
	(scatter tstat samplesize if th==4 &  depvar=="testscore_std", msymbol(s)  mcolor(green)) ///
	(scatter tstat samplesize if th==5 &  depvar=="testscore_std",msymbol(x)  mcolor(green)) ///
	(scatter tstat samplesize if th==6 &  depvar=="testscore_std",msymbol(t)  mcolor(green)) ///
	(scatter tstat samplesize if th==2 & depvar=="edu",msymbol(o)  mcolor(gs1)) ///
	(scatter tstat samplesize if th==3 &  depvar=="edu",msymbol(d) mcolor(gs1)) ///
	(scatter tstat samplesize if th==4 &  depvar=="edu", msymbol(s) mcolor(gs1)) ///
	(scatter tstat samplesize if th==5 &  depvar=="edu",msymbol(x) mcolor(gs1)) ///
	(scatter tstat samplesize if th==6 &  depvar=="edu",msymbol(t) mcolor(gs1)) ///
	(scatter tstat samplesize if th==2 & depvar=="inc",msymbol(o) mcolor(gs7)) ///
	(scatter tstat samplesize if th==3 &  depvar=="inc",msymbol(d) mcolor(gs7)) ///
	(scatter tstat samplesize if th==4 &  depvar=="inc", msymbol(s) mcolor(gs7)) ///
	(scatter tstat samplesize if th==5 &  depvar=="inc",msymbol(x) mcolor(gs7)) ///
	(scatter tstat samplesize if th==6 &  depvar=="inc",msymbol(t) mcolor(gs7)) ///
	(scatter tstat samplesize if th==2 & depvar=="b",msymbol(o) mcolor(gs12)) ///
	(scatter tstat samplesize if th==3 &  depvar=="b",msymbol(d) mcolor(gs12)) ///
	(scatter tstat samplesize if th==4 &  depvar=="b", msymbol(s) mcolor(gs12)) ///
	(scatter tstat samplesize if th==5 &  depvar=="b",msymbol(x) mcolor(gs12)) ///
	(scatter tstat samplesize if th==6 &  depvar=="b",msymbol(t) mcolor(gs12)) ///
	(scatter tstat samplesize if th==22 &  depvar=="b",msymbol(t) mcolor(white)) ///
	,legend(order(2 "Testscore" 7 "Parental Education" 12 "Parental income" 17 "Birth weight" 21 " " 6 "9AM" 7 "10AM" 8 "11AM" 9 "Noon" 10 "1PM") rows(2) region(lcolor(white))) ///
	ylabel(#7 ,nogrid noticks) graphregion(fcolor(white) lcolor(white)) ///
						xlabel(,noticks) plotregion(fcolor(white) lcolor(black)) ///
						ytitle("T-value (numerical)") xtitle(Sample size)
	
	

	graph export "$df\appendix_graph_testhour_tstats_sample.png",replace width(4000)

/*******************************************************************************
5 Main regression table, with percentiles
*******************************************************************************/

* Load data
use "$tf\analysisdata`sample'.dta",clear
* set fixed effect level
xtset instnr
* singletons in instnr
bys instnr: gen count=_n
bys instnr: egen instcount=max(count)
eststo clear
* Test hour effect, no controls
eststo: qui: reg percentile testhour, cluster(instnr) 
estadd scalar Fval=r(F)
estadd scalar Pval=r(p)
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
* Hourly effect, no controls
eststo: qui: reg percentile th2-th6, cluster(instnr) 
qui: test th2=th3=th4=th5=th6=0
estadd scalar Fval=r(F)
estadd scalar Pval=r(p)
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
* Hourly effect, basic controls
eststo: qui: xtreg percentile th2-th6 $controls1 if instcount>1, cluster(instnr) fe
qui: test th2=th3=th4=th5=th6=0
estadd scalar Fval=r(F)
estadd scalar Pval=r(p)
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
* Hourly effect, extended controls
eststo: qui: xtreg percentile th2-th6 $controls2 if instcount>1, cluster(instnr) fe
qui: test th2=th3=th4=th5=th6=0
estadd scalar Fval=r(F)
estadd scalar Pval=r(p)
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
* Break effect, no controls
eststo: qui: reg percentile testhour break, cluster(instnr) 
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
* Break effect, basic controls
eststo: qui: xtreg percentile testhour break $controls1 if instcount>1, cluster(instnr) fe
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
* Break effect, extended controls
eststo: qui: xtreg percentile testhour break $controls2 if instcount>1, cluster(instnr) fe
estadd scalar DF=  e(df_m) 
estadd scalar groups=  e(N_g)
estadd scalar sgroup=   e(g_min)
estadd scalar lgroup= e(g_max)
estadd scalar ar2=e(r2_a) 
esttab using "$df\appendix_reg_table_percentiles.csv",  stats(  groups sgroup lgroup Fval Pval DF ar2 aic N, fmt(%11.3f)) b(%5.3f) ///
		keep(break  testhour th2 th3 th4 th5 th6) nolines nonotes se nonumbers fragment  ///
		subs("[1em]" " ") label replace


/*******************************************************************************
6 Unconditional correlations for covariates and testscore
*******************************************************************************/
* Load data
use "$tf\analysisdata`sample'.dta",clear

* regress
sum inc,d
replace inc=. if inc>r(p99)
replace inc=. if inc<r(p1)
sum birthweight,d
replace birthweight=. if birthweight>r(p99)
replace birthweight=. if birthweight<r(p1)
eststo clear
set matsize 5000
 eststo: qui: reg testscore_std  inc  if missing_incrank!=1, cluster(instnr) 
 estadd scalar ar2=e(r2_a) 
 eststo: qui: reg testscore_std  birthweight  if missing_bir!=1, cluster(instnr)  
 estadd scalar ar2=e(r2_a) 
 eststo: qui: reg testscore_std  edu  if missing_edu!=1, cluster(instnr) 
 estadd scalar ar2=e(r2_a) 
 eststo: qui: reg testscore_std  schoold  , cluster(instnr) 
 estadd scalar ar2=e(r2_a) 
 esttab using "$df\appendix_main_effectsizes.csv",  stats( ar2 aic N, fmt(%11.3f)) b(%6.4f) ///
		keep( inc   birthweight edu schoolday) nolines nonotes se nonumbers fragment  ///
		subs("[1em]" " ") label replace

