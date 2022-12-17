capture log close
capture clear all
set more off

global project_directory "D:\projects\skill_gaps_lead" 

log using "${project_directory}\programs\_LOGS\09_create_v01_phdcn_merged.log", replace

//Merging PHDCN  
use "${project_directory}\data\phdcn\v01_phdcn_demo.dta", clear
merge 1:1 nSUBID using "${project_directory}\data\phdcn\v01_phdcn_header.dta", nogen
merge 1:1 nSUBID using "${project_directory}\data\phdcn\v01_phdcn_master.dta", nogen
merge 1:1 nSUBID using "${project_directory}\data\phdcn\v01_phdcn_cbcl.dta", nogen
merge 1:1 nSUBID using "${project_directory}\data\phdcn\v01_phdcn_ppvt.dta", nogen
save "${project_directory}\data\_TEMP\phdcn_full.dta", replace

//Merging BLDB

//wave 1
use "${project_directory}\data\cdph_bldb\v01_bldb_nc.dta", clear
keep nyear nlinknc navgbllsm nprbllgt5sm nobs
keep if nyear >= 1994 & nyear <=1997
reshape wide navgbllsm nprbllgt5sm nobs, i(nlinknc) j(nyear)
rename nlinknc nlinknc_1
merge m:m nlinknc_1 using "${project_directory}\data\_TEMP\phdcn_full.dta", nogen keepusing(*_1 nSUBID nCohort nstrata)

drop if nSUBID == .

gen nBL5_1 = .
replace nBL5_1 = nprbllgt5sm1995 if nPCYearInterview_1 == 1995 | nPCYearInterview_1 == 1994
replace nBL5_1 = nprbllgt5sm1996 if nPCYearInterview_1 == 1996 | nPCYearInterview_1 == .
replace nBL5_1 = nprbllgt5sm1997 if nPCYearInterview_1 == 1997

gen nBLAVG_1 = .
replace nBLAVG_1 = navgbllsm1995 if nPCYearInterview_1 == 1995 | nPCYearInterview_1 == 1994
replace nBLAVG_1 = navgbllsm1996 if nPCYearInterview_1 == 1996 | nPCYearInterview_1 == .
replace nBLAVG_1 = navgbllsm1997 if nPCYearInterview_1 == 1997

gen nBLAVGlog_1 = ln(nBLAVG_1)

drop navgbllsm* nprbllgt5sm* 

save "${project_directory}\data\_TEMP\phdcn_bldb_w1.dta", replace

//wave 2
use "${project_directory}\data\cdph_bldb\v01_bldb_nc.dta", clear
keep nyear nlinknc navgbllsm nprbllgt5sm nobs
keep if nyear >= 1997 & nyear <=2000
reshape wide navgbllsm nprbllgt5sm nobs, i(nlinknc) j(nyear)
rename nlinknc nlinknc_2
merge m:m nlinknc_2 using "${project_directory}\data\_TEMP\phdcn_full.dta", nogen keepusing(*_2 nSUBID nCohort nstrata)

drop if nSUBID == .

gen nBL5_2 = .
replace nBL5_2 = nprbllgt5sm1997 if nPCYearInterview_2 == 1997
replace nBL5_2 = nprbllgt5sm1998 if nPCYearInterview_2 == 1998 | nPCYearInterview_2 == .
replace nBL5_2 = nprbllgt5sm1999 if nPCYearInterview_2 == 1999
replace nBL5_2 = nprbllgt5sm2000 if nPCYearInterview_2 == 2000

gen nBLAVG_2 = .
replace nBLAVG_2 = navgbllsm1997 if nPCYearInterview_2 == 1997
replace nBLAVG_2 = navgbllsm1998 if nPCYearInterview_2 == 1998 | nPCYearInterview_2 == .
replace nBLAVG_2 = navgbllsm1999 if nPCYearInterview_2 == 1999
replace nBLAVG_2 = navgbllsm2000 if nPCYearInterview_2 == 2000

gen nBLAVGlog_2 = ln(nBLAVG_2)

drop navgbllsm* nprbllgt5sm* 

save "${project_directory}\data\_TEMP\phdcn_bldb_w2.dta", replace

//wave 3
use "${project_directory}\data\cdph_bldb\v01_bldb_nc.dta", clear
keep nyear nlinknc navgbllsm nprbllgt5sm nobs
keep if nyear >= 2000 & nyear <=2002
reshape wide navgbllsm nprbllgt5sm nobs, i(nlinknc) j(nyear)
rename nlinknc nlinknc_3
merge m:m nlinknc_3 using "${project_directory}\data\_TEMP\phdcn_full.dta", nogen keepusing(*_3 nSUBID nCohort nstrata)

drop if nSUBID == .

gen nBL5_3 = .
replace nBL5_3 = nprbllgt5sm2000 if nPCYearInterview_3 == 2000
replace nBL5_3 = nprbllgt5sm2001 if nPCYearInterview_3 == 2001 | nPCYearInterview_3 == .
replace nBL5_3 = nprbllgt5sm2002 if nPCYearInterview_3 == 2002

gen nBLAVG_3 = .
replace nBLAVG_3 = navgbllsm2000 if nPCYearInterview_3 == 2000
replace nBLAVG_3 = navgbllsm2001 if nPCYearInterview_3 == 2001 | nPCYearInterview_3 == .
replace nBLAVG_3 = navgbllsm2002 if nPCYearInterview_3 == 2002

gen nBLAVGlog_3 = ln(nBLAVG_3)

drop navgbllsm* nprbllgt5sm*

save "${project_directory}\data\_TEMP\phdcn_bldb_w3.dta", replace

use "${project_directory}\data\_TEMP\phdcn_bldb_w1.dta", clear
merge 1:1 nSUBID using "${project_directory}\data\_TEMP\phdcn_bldb_w2.dta", nogen
merge 1:1 nSUBID using "${project_directory}\data\_TEMP\phdcn_bldb_w3.dta", nogen
save "${project_directory}\data\_TEMP\phdcn_bldb.dta", replace

//Merging NCDB

//wave 1
use "${project_directory}\data\ncdb2000\v01_ncdb_nc.dta", clear
keep nlinknc nyear ncondadvg npovertyp 
keep if nyear >= 1994 & nyear <=1997
reshape wide ncondadvg npovertyp, i(nlinknc) j(nyear)
rename nlinknc nlinknc_1

merge m:m nlinknc_1 using "${project_directory}\data\_TEMP\phdcn_bldb.dta", nogen keepusing(*_1 nSUBID nCohort nstrata)

drop if nSUBID == .

gen ncondadvg_1 = .
replace ncondadvg_1 = ncondadvg1994 if nPCYearInterview_1 == 1994
replace ncondadvg_1 = ncondadvg1995 if nPCYearInterview_1 == 1995
replace ncondadvg_1 = ncondadvg1996 if nPCYearInterview_1 == 1996 | nPCYearInterview_1 == .
replace ncondadvg_1 = ncondadvg1997 if nPCYearInterview_1 == 1997

gen npovertyp_1 = .
replace npovertyp_1 = npovertyp1994 if nPCYearInterview_1 == 1994
replace npovertyp_1 = npovertyp1995 if nPCYearInterview_1 == 1995
replace npovertyp_1 = npovertyp1996 if nPCYearInterview_1 == 1996 | nPCYearInterview_1 == .
replace npovertyp_1 = npovertyp1997 if nPCYearInterview_1 == 1997

drop ncondadvg1* npovertyp1* 

save "${project_directory}\data\_TEMP\full_w1.dta", replace

//wave 2
use "${project_directory}\data\ncdb2000\v01_ncdb_nc.dta", clear
keep nlinknc nyear ncondadvg npovertyp
keep if nyear >= 1997 & nyear <=2000
reshape wide ncondadvg npovertyp, i(nlinknc) j(nyear)
rename nlinknc nlinknc_2

merge m:m nlinknc_2 using "${project_directory}\data\_TEMP\phdcn_bldb.dta", nogen keepusing(*_2 nSUBID nCohort nstrata)

drop if nSUBID == .

gen ncondadvg_2 = .
replace ncondadvg_2 = ncondadvg1997 if nPCYearInterview_2 == 1997
replace ncondadvg_2 = ncondadvg1998 if nPCYearInterview_2 == 1998 | nPCYearInterview_2 == .
replace ncondadvg_2 = ncondadvg1999 if nPCYearInterview_2 == 1999
replace ncondadvg_2 = ncondadvg2000 if nPCYearInterview_2 == 2000

gen npovertyp_2 = .
replace npovertyp_2 = npovertyp1997 if nPCYearInterview_2 == 1997
replace npovertyp_2 = npovertyp1998 if nPCYearInterview_2 == 1998 | nPCYearInterview_2 == .
replace npovertyp_2 = npovertyp1999 if nPCYearInterview_2 == 1999
replace npovertyp_2 = npovertyp2000 if nPCYearInterview_2 == 2000

drop ncondadvg1* npovertyp1* ncondadvg2* npovertyp2* 

save "${project_directory}\data\_TEMP\full_w2.dta", replace

//wave 3
use "${project_directory}\data\ncdb2000\v01_ncdb_nc.dta", clear
keep nlinknc nyear ncondadvg npovertyp
keep if nyear == 2000
reshape wide ncondadvg npovertyp, i(nlinknc) j(nyear)
rename nlinknc nlinknc_3

merge m:m nlinknc_3 using "${project_directory}\data\_TEMP\phdcn_bldb.dta", nogen keepusing(*_3 nSUBID nCohort nstrata)

drop if nSUBID == .

gen ncondadvg_3 = .
replace ncondadvg_3 = ncondadvg2000 if nPCYearInterview_3 == 2000
replace ncondadvg_3 = ncondadvg2000 if nPCYearInterview_3 == 2001 | nPCYearInterview_3 == .
replace ncondadvg_3 = ncondadvg2000 if nPCYearInterview_3 == 2002

gen npovertyp_3 = .
replace npovertyp_3 = npovertyp2000 if nPCYearInterview_3 == 2000
replace npovertyp_3 = npovertyp2000 if nPCYearInterview_3 == 2001 | nPCYearInterview_3 == .
replace npovertyp_3 = npovertyp2000 if nPCYearInterview_3 == 2002

drop ncondadvg2* npovertyp2*

save "${project_directory}\data\_TEMP\full_w3.dta", replace

use "${project_directory}\data\_TEMP\full_w1.dta", clear 
merge 1:1 nSUBID using "${project_directory}\data\_TEMP\full_w2.dta", nogen
merge 1:1 nSUBID using "${project_directory}\data\_TEMP\full_w3.dta", nogen

gen nlnSalary_1 = ln(nSalary_1+1)
gen nlnSalary_2 = ln(nSalary_2+1)
gen nlnSalary_3 = ln(nSalary_3+1)

recode nPCMarstat_1 (1=1 married) (2 3 = 0 unmarried), gen(nPCMarried_1)
recode nPCMarstat_2 (1=1 married) (2 3 = 0 unmarried), gen(nPCMarried_2)
recode nPCMarstat_3 (1=1 married) (2 3 = 0 unmarried), gen(nPCMarried_3)

recode nPCLanguage_1 (1=1 english) (2 3 = 0 other), gen(nPCEnglish_1)
recode nPCLanguage_2 (1=1 english) (2 3 = 0 other), gen(nPCEnglish_2)
recode nPCLanguage_3 (1=1 english) (2 3 = 0 other), gen(nPCEnglish_3)

save "${project_directory}\data\v01_phdcn_merged.dta", replace

tab1 nSUBAge_1 nSUBAge_2 nSUBAge_3 if nCohort==0
sum nSUBAge_1 nSUBAge_2 nSUBAge_3 if nCohort==0
gen t_agediff2=nSUBAge_2-nSUBAge_1
gen t_agediff3=nSUBAge_3-nSUBAge_2
tab1 t_agediff2 t_agediff3 if nCohort==0
sum t_agediff2 t_agediff3 if nCohort==0
drop t_*

erase "${project_directory}\data\_TEMP\phdcn_bldb_w1.dta"
erase "${project_directory}\data\_TEMP\phdcn_bldb_w2.dta"
erase "${project_directory}\data\_TEMP\phdcn_bldb_w3.dta"
erase "${project_directory}\data\_TEMP\phdcn_bldb.dta"
erase "${project_directory}\data\_TEMP\full_w1.dta"
erase "${project_directory}\data\_TEMP\full_w2.dta"
erase "${project_directory}\data\_TEMP\full_w3.dta"
erase "${project_directory}\data\_TEMP\phdcn_full.dta"

log close
