capture log close
capture clear all
set more off

global project_directory "D:\projects\skill_gaps_lead" 

log using "${project_directory}\programs\_LOGS\12_create_table_2.log", replace

/******************************************
DO-FILE NAME: 12_create_table_2.do
PURPOSE: create sample descriptives by class
*******************************************/

///Table 2: Covariates by Class
use "${project_directory}\data\v02_phdcn_merged_mi.dta", clear

//Macros
global covars ///
	nwhite_b nblack_b nhispan_b nothrace_b ///
	nfemale_b nownhome_b nfamsize_b npcage_b ///
	nlninc_1 nlninc_2 nlninc_3 ///
	npcemply_1 npcemply_2 npcemply_3 ///
	npcmarr_1 npcmarr_2 npcmarr_3 ///
	npcwelf_1 npcwelf_2 npcwelf_3 ///
	npcengl_1 npcengl_2 npcengl_3 ///
	ncondadvg_1 ncondadvg_2 ncondadvg_3
	
//Sample sizes
tab ncohort if _mj==0
tab npceduc_b [iw=miwt] if _mj!=0

//Less Than College Degree
sum $covars	[iw=miwt] if _mj!=0 & npccolgrd_b==0

//College Graduate
sum $covars	[iw=miwt] if _mj!=0 & npccolgrd_b==1

//Hypothesis tests
mi import ice, auto

foreach x in $covars { 
    di "Test of racial differences in `x'"
	quietly mi estimate, post: reg `x' npccolgrd_b, cluster(nlinknc_1)
	mi test npccolgrd_b
	} 

log close
