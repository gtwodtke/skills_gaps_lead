capture log close
capture clear all
set more off

global project_directory "D:\projects\skill_gaps_lead" 

log using "${project_directory}\programs\_LOGS\03_create_v01_phdcn_header.log", replace

//Header File Wave 3 - Cohort 0
use "${project_directory}\data\phdcn\14802059\ICPSR_13712\DS0001\13712-0001-Data-REST.dta", clear

rename SUBID nSUBID
rename COHORT nCohort
rename LANGPC3 nPCLanguage_3

keep n*

save "${project_directory}\data\phdcn\v01_phdcn_header.dta", replace

log close
