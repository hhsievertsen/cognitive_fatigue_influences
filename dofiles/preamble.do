* time of day and test performance
* Last edited: 20160113 by hhs@sfi.dk
********************************************************************************
clear all
set max_memory 2g, perm

* Set directory
cd "D:\Data\workdata\704335\Timeofday"

global tf "D:\Data\workdata\704335\Timeofday\tempfiles"
global df "D:\Data\workdata\704335\Timeofday\download"
global rf "D:\Data\workdata\704335\stataraw_new"

adopath + "D:\Data\workdata\704335\Timeofday\adofiles"

* Controls
global covariates "birthweight edu inc incrank immigr_nonwestern female spring "
global missings "missing_birthweight missing_edu missing_inc missing_incrank missing_immigr_nonwestern missing_female missing_spring"
global controls1 "i.sub i.grade i.dow i.testyear"
global controls2 "$covariates $missings $controls1"
