
/*******************************************************************************
* Create data for time of day project
* Last edited: 20160113 by hhs@sfi.dk
*******************************************************************************/

* Load preamble
do "D:\Data\workdata\704335\Timeofday\dofiles\preamble.do"
* Set memory
set max_memory 12g, perm

/*******************************************************************************
* Outline:
* 1: Create covariate data		
* 2: Create test data
* 3: Merge 1 and 2
*******************************************************************************/




/*******************************************************************************
* 1 Create covariate data
*******************************************************************************/
* Load "Grund" data for 1995 to 2012 and make sure everything is in the right format.
forval i=1995/2012{
	use "$rf\GRUND`i'.dta", clear 
	* foed_dag changes name after 2006. Correct this
	if `i'>2006{
		rename FOED_DAG foed_dag
	}
	* Keep what we need
	keep pnr familie_id koen kom IE_TYPE OPR_LAND hfaudd SAMLINK_NY foed_dag
	* Education level within the family
	rename hfaudd audd
	tostring audd, replace
	merge m:1 audd using "$tf\audd.dta"
	drop if _merge==2
	drop _merge
	bys familie_id: egen educ=max(pria)
	gen edu=educ/12
	
	* Household income
	* Adjust to 2010 level
	gen gross_inc=SAMLINK_NY
	replace gross_inc=gross_inc*(122.4/89.2) if `i'==1995
	replace gross_inc=gross_inc*(122.4/91.1) if `i'==1996
	replace gross_inc=gross_inc*(122.4/93.1) if `i'==1997
	replace gross_inc=gross_inc*(122.4/94.8) if `i'==1998
	replace gross_inc=gross_inc*(122.4/97.2) if `i'==1999
	replace gross_inc=gross_inc*(122.4/100.0) if `i'==2000
	replace gross_inc=gross_inc*(122.4/102.4) if `i'==2001
	replace gross_inc=gross_inc*(122.4/104.8) if `i'==2002
	replace gross_inc=gross_inc*(122.4/107.0) if `i'==2003
	replace gross_inc=gross_inc*(122.4/108.3) if `i'==2004
	replace gross_inc=gross_inc*(122.4/110.2) if `i'==2005
	replace gross_inc=gross_inc*(122.4/112.3) if `i'==2006
	replace gross_inc=gross_inc*(122.4/114.2) if `i'==2007
	replace gross_inc=gross_inc*(122.4/118.1) if `i'==2008
	replace gross_inc=gross_inc*(122.4/119.7) if `i'==2009
	replace gross_inc=gross_inc*(122.4/122.4) if `i'==2010
	replace gross_inc=gross_inc*(122.4/125.8) if `i'==2011
	replace gross_inc=gross_inc*(122.4/128.8) if `i'==2012
	* Total income on household level
	bys familie_id: egen inc=sum(gross_inc)
	* Family members
	bys familie_id: gen count=_n
	bys familie_id: egen members=max(count)
	* Adjusted household income
	replace inc=inc/(members^0.5)
	replace inc=inc/1000
	* Household income rank
	gen inc1=inc if count==1
	egen income=xtile(inc1),nq(100)
	bys familie_id: egen incrank=min(income)
	
	* Country formats
	rename OPR_LAND land
	merge m:1 land using "$tf\ieland.dta"
	drop if _merge==2
	gen western=VEST_EJ==1|VEST_EJ==2
	gen immigr_nonwestern=(IE_TYPE==2|IE_TYPE==3)&western==0
	* Gender
	gen female=koen-1
	* Spring child
	gen spring=month(foed_dag)<7
	* Variable labels
	label var spring "Spring child"
	label var inc "Household income, 1,000 DKK"
	label var immigr_nonwestern "Nonwestern immigrant/desc."
	label var female "Female"
	label var incrank "Household inc. percentile"
	label var  edu "Parents' years of schooling"
	* Keep what we need	
	keep kom pnr western immigr_nonwestern female incrank edu  inc spring
	
	gen y=`i'
	compress
	save "$tf\GRUND`i'.dta",  replace
}

*Append to one dataset
clear 
use "$tf\GRUND1995.dta", clear
forval i=1996/2012{
	append using  "$tf\GRUND`i'.dta", 
	}

save "$tf\covariates.dta",replace


/*******************************************************************************
* 2 Create test data
*******************************************************************************/
* Load
use "$rf\DNT2010_2014_HHS_SFI.dta", clear
* grade
rename klassetrin grade
* subject
gen subject=substr(fag,1,5)
* test time
gen testyear=substr(testtid,1,4)
gen testmonth=substr(testtid,6,2)
gen testday=substr(testtid,9,2)
gen testhour=substr(testtid,12,2)
gen testmin=substr(testtid,15,2)
destring testyear testmonth testday testhour testmin ,replace
drop testtid
gen date=mdy(testmonth,testday,testyear)
* create one uncertainty measure (simple average of the the three)
gen uncert=(sem_p1+sem_p2+sem_p3)/3
* create one testscore per test 
gen testscore=(theta_p1+theta_p2+theta_p3)/3
bys testyear grade subject: egen sdscoreraw=sd(testscore)
bys testyear grade subject: egen mscoreraw=mean(testscore)
gen  testscore_std=(testscore-mscoreraw)/sdscoreraw
* Percentile scores
bys testyear grade sub:  egen percentile=xtile(testscore), nq(100)
label var percentile "Percentile score"

* create variables and labels
* Test hour indicators
tab testhour, gen(th)
label var th1 "8AM"
label var th2 "9AM"
label var th3 "10AM" 
label var th4 "11AM"
label var th5 "12Noon"
label var th6 "1PM"
label var th7 "2PM"
label var testhour "Hour of the day"
* Break variables
gen break=th1==1|th3==1|th5==1|th7==1
gen nobreak=th2==1|th4==1|th6==1
label var break "After a break"
label var nobreak "Not after a break"
* Subject value labels and indicators
gen sub=1 if subject=="Dansk"
replace sub=2 if subject=="Biolo"
replace sub=3 if subject=="Engel"
replace sub=4 if subject=="Fysik"
replace sub=5 if subject=="Geogr"
replace sub=6 if subject=="Matem"
label define subl 1 "Danish" 2 "Biology" 3 "English" 4 "Physics" 5 "Geography" 6 "Math"
label values sub subl
* Day of the week
gen dow=dow(date)
* Various labels
label var uncert "Uncertainty"
label var testscore "Testscore (1-100)"
label var testscore_std "Standardized testscore"
label var grade "Grade"
label var dow "Day of the week"
label var sub "Subject"
* keep, compress and save
keep pnr break nobreak testmin uncert instnr percentile  date testyear grade sub testyear testscore testscore_std grade th1-th7 testhour dow 
compress
save "$tf\testscoredata.dta",replace



/*******************************************************************************
* 3 Merge test score data and covariate data
*******************************************************************************/
use "$tf\testscoredata.dta",clear
* merge to birthweight data
merge m:1 pnr using "$rf\NYLFOED2010.dta"
rename V_VAGT V_VAGT1
drop if _merge==2
drop _merge
merge m:1 pnr using "$rf\MFR2010.dta"
drop if _merge==2
drop _merge
gen birthweight=V_VAGT
replace birthweight=V_VAGT1 if V_VAGT==.
drop V_GA_DAGE V_APGAR fodtdato   V_VAGT1 V_VAGT 
* merge to covariate panel using the year before the test
gen y=year(date)-1
merge m:1 pnr y using "$tf\covariates.dta",
drop if _merge==2
drop _merge
* Indicator for break break data
gen breakdata=0
gen deviates=.
destring instnr,replace
foreach l in xxxxxx xxxxxx xxxxxx xxxxxx xxxxxx xxxxxx xxxxxx xxxxxx .... xxxxxx {
qui: replace breakdata=1 if instnr==`l'
qui: replace deviates=0 if instnr==`l'
}
foreach l in xxxxxx xxxxxx.... xxxxxx {
qui: replace deviates=1 if instnr==`l'
}
* Missing variable indicators
foreach l in $covariates {
gen missing_`l'=0
replace missing_`l'=1 if `l'==.
replace `l'=0 if missing_`l'==1
}
label var missing_birthweight "Missing birthweight data"
label var missing_edu "Missing education data"
label var missing_immig "Missing origin data"
label var missing_female "Missing gender data"
label var missing_spring "Missing date of birth"
label var missing_inc "Missing income data"
label var missing_incrank "Missing income data"
* School day (approximated)
gen schoolday=date-mdy(8,1,year(date)-1)
* adjust for weekends
replace schoolday=schoolday-floor(schoolday/7)*2
label var schoolday "School day"
 
* PNR as numeric
egen id=group(pnr)

compress

* Sample selection
preserve
gen day=testhour+testmin/60
keep if testyear==2014
drop if day>14
hist day, xtitle(Time of the Day) title(Distribution of test times in 2014)
graph export "$df\fig2014_testtimedist.pdf",replace
tw (lpolyci testscore_std day,bwidth(0.2)), xline(9.5) xline(11.5)
graph export "$df\fig2014_testtimeperf.pdf",replace
restore
drop if testyear==2014
qui: sum testscore
local nu=r(N)
drop if grade==.
* tests in nonscheduled grades
drop if grade==0 
drop if grade==1
drop if grade==5
drop if grade>8
drop if grade<8 & sub==2
drop if grade==2 & sub==6
drop if grade==4 & sub==6
drop if grade==7 & sub==6
drop if grade==8 & sub==6
drop if grade==3 & sub==1
drop if grade==7 & sub==1
drop if grade!=7 & sub==3
drop if grade!=8 & sub==4
drop if grade!=8 & sub==5
drop if testhour==14
sum testscore,d
local r1=`nu'-r(N)
di "Sampleselection: Deleted `r1' out of `nu' observations"
* Keep what we need
keep id percentile instnr uncert schoolday break nobreak date testhour testscore testscore_std th1-th7 testyear sub breakdata deviates grade dow $covariates $missings
* final labels
label var breakdata "School included in break survey"
label var deviates "School deviates from normal schedule when testing"
label var birthweight "Child birth weight"
label var date "Test date"

save "$tf\analysisdata.dta",replace
keep if runiform()<.01
save "$tf\analysisdata1pct.dta",replace
