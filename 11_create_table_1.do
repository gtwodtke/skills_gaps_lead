capture log close
capture clear all
set more off

global project_directory "D:\projects\skill_gaps_lead" 

log using "${project_directory}\programs\_LOGS\11_create_table_1.log", replace

/******************************************
DO-FILE NAME: 11_create_table_1.do
PURPOSE: create sample descriptives by race
*******************************************/

///Table 1: Covariates by Race
use "${project_directory}\data\v02_phdcn_merged_mi.dta", clear

//Macros
global covars ///
	npclesshs_b npchsgrad_b npcsomcol_b npccolgrd_b ///
	nfemale_b nownhome_b nfamsize_b npcage_b ///
	nlninc_1 nlninc_2 nlninc_3 ///
	npcemply_1 npcemply_2 npcemply_3 ///
	npcmarr_1 npcmarr_2 npcmarr_3 ///
	npcwelf_1 npcwelf_2 npcwelf_3 ///
	npcengl_1 npcengl_2 npcengl_3 ///
	ncondadvg_1 ncondadvg_2 ncondadvg_3
	
//Sample sizes
tab ncohort if _mj==0
tab nrace_b [iw=miwt] if _mj!=0

//Whites
sum $covars	[iw=miwt] if _mj!=0 & nwhite_b==1

//Blacks
sum $covars	[iw=miwt] if _mj!=0 & nblack_b==1

//Hispanics
sum $covars [iw=miwt] if _mj!=0 & nhispan_b==1

//Hypothesis tests
mi import ice, auto

foreach x in $covars { 
    di "Test of racial differences in `x'"
	quietly mi estimate, post: reg `x' nblack_b nhispan_b nothrace_b, cluster(nlinknc_1)
	mi test nblack_b nhispan_b
	} 

log close 
