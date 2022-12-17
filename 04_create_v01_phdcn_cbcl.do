capture log close
capture clear all
set more off

global project_directory "D:\projects\skill_gaps_lead" 

log using "${project_directory}\programs\_LOGS\04_create_v01_phdcn_cbcl.log", replace

//CBCL WAVE 3
use "${project_directory}\data\phdcn\14802059\ICPSR_13679\DS0001\13679-0001-Data-REST.dta", clear
	
keep SUBID COHORT ATTNTC3 CE2 CE4 CE7 CE10 CE25 CE28 CE37 CE48

rename SUBID nSUBID
rename COHORT nCohort
rename ATTNTC3 nAttention_3

foreach x of varlist CE2 CE4 CE7 CE10 CE25 CE28 CE37 CE48 {
    replace `x'=. if inrange(`x',-99,-1)
	tab `x', missing
	}

alpha CE2 CE4 CE7 CE10 CE25 CE28 CE37 CE48

keep n*

keep if nCohort==0

save "${project_directory}\data\phdcn\v01_phdcn_cbcl.dta", replace 

log close
