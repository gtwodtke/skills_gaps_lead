capture log close
capture clear all
set more off

global project_directory "D:\projects\skill_gaps_lead" 

log using "${project_directory}\programs\_LOGS\14_create_figure_1.log", replace

/****************************************
DO-FILE NAME: 14_create_figure_1.do
PURPOSE: create maps of nh comp/lead
*****************************************/

///Figure 1: Race, Class, and Lead Exposure in Chicago, 1997
capture erase "${project_directory}\data\_TEMP\ILcoord.dta"
capture erase "${project_directory}\data\_TEMP\ILdb.dta"

shp2dta using "${project_directory}\data\shpfiles\tr17_d00.shp", ///
	database("${project_directory}\data\_TEMP\ILdb.dta") ///
	coordinates("${project_directory}\data\_TEMP\ILcoord.dta") genid(id)
	
import excel using "${project_directory}\data\shpfiles\chicago2000.xlsx", firstrow
gen ntract = 0
replace ntract = real(substr((census_tra),1,4))
keep ntract

save "${project_directory}\data\_TEMP\chicago2000tracts.dta", replace

use "${project_directory}\data\phdcn\v01_phdcn_tractlink.dta", clear
rename tract ntract
rename link_nc nlinknc
save "${project_directory}\data\_TEMP\tractlinknc.dta", replace 

use "${project_directory}\data\_TEMP\chicago2000tracts.dta", clear
merge m:1 ntract using "${project_directory}\data\_TEMP\tractlinknc.dta"
keep if _merge==3
drop _merge

save "${project_directory}\data\_TEMP\chicago2000tracts.dta", replace

use "${project_directory}\data\ncdb2000\v01_ncdb_nc", clear
keep if nyear==1997
keep neducollegep nraceblackp nracehispanicp nlinknc

replace neducollegep=round(neducollegep, 0.01)
replace nraceblackp=round(nraceblackp, 0.01)
replace nracehispanicp=round(nracehispanicp, 0.01)

save "${project_directory}\data\_TEMP\ncdb_temp.dta", replace

use "${project_directory}\data\_TEMP\chicago2000tracts.dta", clear
merge m:1 nlinknc using "${project_directory}\data\_TEMP\ncdb_temp.dta"
keep if _merge==3
drop _merge

save "${project_directory}\data\_TEMP\ncdb_temp.dta", replace

use "${project_directory}\data\_TEMP\ILdb.dta", clear

foreach x in STATE COUNTY TRACT NAME LSAD_TRANS {
	destring `x', replace 
	}
	
keep if CO == 31 //cook county 
sort TRACT

gen ntract = round(NAME, 1)	

merge m:1 ntract using "${project_directory}\data\_TEMP\ncdb_temp.dta"
keep if _merge == 3
keep id ntract nlinknc neducollegep nraceblackp nracehispanicp

mergepoly id using "${project_directory}\data\_TEMP\ILcoord.dta", by(nlinknc) ///
	coor("${project_directory}\data\_TEMP\nhoodClust_coord.dta") replace

save "${project_directory}\data\_TEMP\ncdb_temp.dta", replace

//Panel A: Racial Composition (% Black)
spmap nraceblackp using "${project_directory}\data\_TEMP\nhoodClust_coord.dta", ///
	id(id) fcolor(Greys2) clm(q) cln(10) ndf(magenta) ///
	title("Proportion Black", size(medsmall)) ///
	legtitle("Deciles")

graph save "${project_directory}\figures\_TEMP\fig1a.gph", replace

//Panel B: Racial Composition (% Hispanic)
spmap nracehispanicp using "${project_directory}\data\_TEMP\nhoodClust_coord.dta", ///
	id(id) fcolor(Greys2) clm(q) cln(10) ndf(magenta) ///
	title("Proportion Hispanic", size(medsmall)) ///
	legtitle("Deciles")

graph save "${project_directory}\figures\_TEMP\fig1b.gph", replace

//Panel C: Education
spmap neducollegep using "${project_directory}\data\_TEMP\nhoodClust_coord.dta", ///
	id(id) fcolor(Greys2) clm(q) cln(10) ndf(magenta) ///
	title("Proportion with a College Degree", size(medsmall)) ///
	legtitle("Deciles")

graph save "${project_directory}\figures\_TEMP\fig1c.gph", replace

//Panel D: Lead Contamination
use "${project_directory}\data\cdph_bldb\v01_bldb_nc.dta", clear
keep if nyear==1997
keep nlinknc navgbllsm
replace navgbllsm=round(navgbllsm, 0.1)

save "${project_directory}\data\_TEMP\bldb_temp.dta", replace

use "${project_directory}\data\_TEMP\chicago2000tracts.dta", clear
merge m:1 nlinknc using "${project_directory}\data\_TEMP\bldb_temp.dta"
keep if _merge==3
drop _merge

save "${project_directory}\data\_TEMP\bldb_temp.dta", replace

use "${project_directory}\data\_TEMP\ILdb.dta", clear

foreach x in STATE COUNTY TRACT NAME LSAD_TRANS {
	destring `x', replace 
	}
	
keep if CO == 31 //cook county 
sort TRACT

gen ntract = round(NAME, 1)	

merge m:1 ntract using "${project_directory}\data\_TEMP\bldb_temp.dta"
keep if _merge==3
keep id ntract nlinknc navgbllsm

mergepoly id using "${project_directory}\data\_TEMP\ILcoord.dta", by(nlinknc) ///
	coor("${project_directory}\data\_TEMP\nhoodClust_coord.dta") replace
	
save "${project_directory}\data\_TEMP\bldb_temp.dta", replace

spmap navgbllsm using "${project_directory}\data\_TEMP\nhoodClust_coord.dta", ///
	id(id) fcolor(Greys2) clm(q) cln(10) ndf(magenta) /// 
	title("Mean Blood Lead Level (ug/dL)", size(medsmall)) ///
	legtitle("Deciles")
	
graph save "${project_directory}\figures\_TEMP\fig1d.gph", replace

//Combine Panels A, B, C, and D
graph combine ///
	"${project_directory}\figures\_TEMP\fig1a.gph" ///
	"${project_directory}\figures\_TEMP\fig1b.gph" ///
	"${project_directory}\figures\_TEMP\fig1c.gph" ///
	"${project_directory}\figures\_TEMP\fig1d.gph", ///
	col(2) row(2) ysize(6.25) xsize(9) scheme(s2mono) imargin(medsmall)

graph export "${project_directory}\figures\figure_1.eps", as(eps) replace

erase "${project_directory}\data\_TEMP\ILcoord.dta"
erase "${project_directory}\data\_TEMP\ILdb.dta"
erase "${project_directory}\data\_TEMP\chicago2000tracts.dta"
erase "${project_directory}\data\_TEMP\tractlinknc.dta"
erase "${project_directory}\data\_TEMP\nhoodClust_coord.dta"
erase "${project_directory}\data\_TEMP\ncdb_temp.dta"
erase "${project_directory}\data\_TEMP\bldb_temp.dta"
erase "${project_directory}\figures\_TEMP\fig1a.gph"
erase "${project_directory}\figures\_TEMP\fig1b.gph"
erase "${project_directory}\figures\_TEMP\fig1c.gph"
erase "${project_directory}\figures\_TEMP\fig1d.gph"

log close
