capture log close
capture clear all
set more off

global project_directory "D:\projects\skill_gaps_lead" 

log using "${project_directory}\programs\_LOGS\02_create_v01_phdcn_demo.log", replace

//Demographic File Wave 1 - Cohort 0
use "${project_directory}\data\phdcn\14802059\ICPSR_13581\DS0001\13581-0001-Data-REST.dta", clear

keep SUBID COHORT DB13 DB1

rename SUBID nSUBID
rename COHORT nCohort
rename DB13 nPCUSCitizen_1

gen nPCHomeowner_1 = 0
replace nPCHomeowner_1 = 1 if DB1 == 2
replace nPCHomeowner_1 = . if DB1 == . | DB1 == -99

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave1Cohort0.dta", replace

//Demographic File Wave 2 - Cohort 0
use "${project_directory}\data\phdcn\14802059\ICPSR_13609\DS0001\13609-0001-Data-REST.dta", clear
 
keep SUBID COHORT DD19 DD44

rename SUBID nSUBID
rename COHORT nCohort

rename DD19 nPCPublicAssist_2

rename DD44 nPCOwnCar_2
replace nPCOwnCar_2 = . if nPCOwnCar_2 == -99

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave2Cohort0.dta", replace 

//Demographic File Wave 3 - Cohort 0
use "${project_directory}\data\phdcn\14802059\ICPSR_13669\DS0001\13669-0001-Data-REST.dta", clear

keep SUBID COHORT DF47 DF3 DF85 DF6A DF57A DF76 DF50

rename SUBID nSUBID
rename COHORT nCohort
rename DF47 nPCOwnCar_3

recode DF3 (1 = 1 LessThanHS) (2 3 = 2 HS) (4 = 3 MoreThanHS) (5 6 7 = 4 BA+) (8 . = .), gen(nPCEdu_3) 

recode DF6A (2 3 4 5 6 7 = 0 Unemployed) (1 = 1 Employed) (.=.), gen(nPCEmployed_3)
recode DF57A (2 3 4 5 6 7 = 0 Unemployed) (1 = 1 Employed) (. -96 = .), gen(nPCPartnerEmployed_3)

rename DF76 nPCPublicAssist_3
replace nPCPublicAssist_3 = . if nPCPublicAssist_3 == -99

recode DF50 (5 = 1 Married) (6 = 2 Cohab) (1 2 3 4 = 3 Single) (-99 . = .), gen(nPCMarstat_3)

gen nSalary_3 = 0  
replace nSalary_3 = 2500 if DF85 == 1
replace nSalary_3 = 7500 if DF85 == 2
replace nSalary_3 = 15000 if DF85 == 3
replace nSalary_3 = 25000 if DF85 == 4
replace nSalary_3 = 35000 if DF85 == 5
replace nSalary_3 = 45000 if DF85 == 6
replace nSalary_3 = 55000 if DF85 == 7
replace nSalary_3 = 65000 if DF85 == 8
replace nSalary_3 = 75000 if DF85 == 9
replace nSalary_3 = 85000 if DF85 == 10
replace nSalary_3 = 90000 * 1.4 if DF85 == 11  
replace nSalary_3 = . if DF85 == -99 | DF85 == -98 | DF85 == -96 | DF85 == .
merge 1:1 nSUBID using "${project_directory}\data\phdcn\v01_phdcn_master.dta", keepusing(nPCYearInterview_3) 
keep if _merge==3
replace nSalary_3 = nSalary_3 * (220.0/252.9) if nPCYearInterview_3 == 2000 
replace nSalary_3 = nSalary_3 * (220.0/260.1) if nPCYearInterview_3 == 2001
replace nSalary_3 = nSalary_3 * (220.0/264.2) if nPCYearInterview_3 == 2002
drop _merge nPCYearInterview_3

keep n*

save "${project_directory}\data\_TEMP\DemoFileWave3Cohort0.dta", replace 

//MERGE
use "${project_directory}\data\_TEMP\DemoFileWave1Cohort0.dta", clear
merge 1:1 nSUBID using "${project_directory}\data\_TEMP\DemoFileWave2Cohort0.dta"
drop _merge
save "${project_directory}\data\_TEMP\DemoFileWave1And2.dta", replace

use "${project_directory}\data\_TEMP\DemoFileWave1And2.dta", clear
merge 1:1 nSUBID using "${project_directory}\data\_TEMP\DemoFileWave3Cohort0.dta"
drop _merge

save "${project_directory}\data\phdcn\v01_phdcn_demo.dta", replace 

erase "${project_directory}\data\_TEMP\DemoFileWave1Cohort0.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave2Cohort0.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave3Cohort0.dta"
erase "${project_directory}\data\_TEMP\DemoFileWave1And2.dta"

log close
