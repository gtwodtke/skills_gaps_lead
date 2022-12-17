capture log close
capture clear all
set more off

global project_directory "D:\projects\skill_gaps_lead" 

log using "${project_directory}\programs\_LOGS\13_create_table_3.log", replace

/******************************************
DO-FILE NAME: 13_create_table_3.do
PURPOSE: create outcome distn by race/class
*******************************************/

///Table 3: Developmental Outcomes by Race and Class
use "${project_directory}\data\v02_phdcn_merged_mi.dta", clear

//Macros
global outcomes nppvt_3 natten_3 

//Sample sizes
tab ncohort if _mj==0
tab nrace_b [iw=miwt] if _mj!=0
tab npceduc_b [iw=miwt] if _mj!=0

///Observed gaps
mi import ice, auto
foreach x of varlist $outcomes { 
	mi estimate, post: reg `x' nblack_b nhispan_b nothrace_b, cluster(nlinknc_1)
	mi estimate, post: reg `x' npcnodeg_b, cluster(nlinknc_1)
	}

log close
