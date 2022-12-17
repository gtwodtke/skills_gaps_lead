capture log close
capture clear all
set more off

global project_directory "D:\projects\skill_gaps_lead" 

log using "${project_directory}\programs\_LOGS\06_create_v01_phdcn_tractlink.log", replace

import spss using "${project_directory}\data\phdcn\tract_linknc\tract_linknc.sav", clear

save "${project_directory}\data\phdcn\v01_phdcn_tractlink.dta", replace

log close
