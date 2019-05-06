clear all
* this version calculates LFP gender instrument.

* this version calculates industry composition by gender
* this version adds IV that uses changes in industry size as treatment and 
* wage and industry composition as fixed at initial exposure rate
* this version saves industry wages and gender ratios (excluding own occupation) by year
* that way I can construct bartik-style IV fixing the 1950 industry composition

global whichocc occfinal
global whichind ind15 // ind1990
/*
* get crosswalk from occfinal to occ1990
cd "$ACSdata"
use usa00011new, clear
keep $whichocc occ1990
byso $whichocc occ1990: keep if _n == 1
drop if $whichocc == . | occ1990 == .
save crosswalk_${whichocc}_occ1990, replace
* get crosswalk from occfinal to occ6
cd "$ACSdata"
use usa00011new, clear
keep $whichocc occ6
byso $whichocc occ6: keep if _n == 1
drop if $whichocc == . | occ6 == .
save crosswalk_${whichocc}_occ6, replace
*/

/*
* make smaller datasets for use
foreach year in 1950 1960 1970 1980 1990 2000 2012 {
cd "$ACSdata"
use usa00011new, clear
* SAMPLE SELECTION
keep if year == `year'
keep if inrange(age,26,65)==1 // range of 4 generations
drop if missing(${whichocc},age,sex,empstat)==1
*drop if occ22 == 15 // extractive industries

*drop if ind15 == 14 // active duty military starts in 1990

*keep if inlist(empstat,1)==1 // use raw size of industry not as percent of workers since shocks act on the extensive margin
* make cells for occ*ed
g ed2 = .
replace ed2 = 1 if inrange(ed4,1,2)
replace ed2 = 2 if inrange(ed4,3,4)
*egen XX = group(ed2 $whichocc), label
*egen YY = group($whichocc ed2), label
save testdata`year', replace
}
*/

***** Education Requirements
foreach year in 1950 1960 1970 1980 1990 2000 2012 {
cd "$ACSdata"
use testdata`year', clear
recode ed2 (1=0) (2=1)
collapse (mean) ed2, by(${whichocc})
drop if $whichocc == .
save edreq`year', replace
} 

***** gender ratio by occupation (to check bartikgender IV)
foreach year in 1950 1960 1970 1980 1990 2000 2012 {
cd "$ACSdata"
use testdata`year', clear
*keep if inrange(age,25,34)==1 // range of youngest generation
keep if inrange(age,36,65)==1 // range of oldest 3 generations
recode sex (1=0) (2=1)
collapse (mean) gender = sex [fw = perwt], by(${whichocc})
drop if $whichocc == .
sort $whichocc
egen Y = group(${whichocc})
save gender`year', replace
}

***** wage by occupation (to check bartikwage IV)
foreach year in 1960 1970 1980 1990 2000 2012 {
cd "$SIPP2004"
use lifeinc_occfinal_`year', clear 
collapse (mean) lifeinc [aw = weight], by(matlabocc sex)
drop if matlabocc == .
save wage`year', replace
}


/*
*****   predicted occupation demand = 
*****   sum_industry: national industry growth rates*number in occupation*industry in t-1
* 1) get percent change in national industry size from t-1 to t
* 2) get number of jobs in occupation*industry cell in t-1
* 3) multiply 1)*2) to predict number of jobs in occupation*industry in t
* 4) sum across industry to predict size of occupation in t due to industry shock
* 5) make a variable that is yhat_t - y_t-1 / y_t-1, so percent growth in occupation

* 1) growth rate in industry at national level from t-1 to t
cd "$ACSdata"
global year = 1980
global nextyear = $year + 10
* industry size nationally
foreach year in $year $nextyear {
preserve
	use testdata`year', clear
	contract ind15 year [fw=perwt], zero nomiss freq(find`year') // percent(pind`year')
	*replace pind`year' = pind`year'/100
	save ind`year', replace
restore
}
use ind$year, clear
merge 1:1 ind15 using ind$nextyear, nogen
g growthind = (find$nextyear / find$year)
save growthind, replace


* 2) industry composition of occupation in year t
use testdata${year}, clear
	contract ind15 $whichocc [fw=perwt], zero nomiss freq(occind${year})
	byso $whichocc: egen total = sum(occind${year})
	g occshare${year} = occind${year}/total // so the sum of these within $whichocc is 1
save occshare${year}, replace

use occshare$year, clear
merge m:1  ind15 using growthind, nogen

* growth rate of occ predicted by national industry growth rate, holding industry composition of occ constant
g occindhat$nextyear = growthind*occshare$year 

* put at occupation level (sum over industry)
byso $whichocc : egen occhat$nextyear = sum(occindhat$nextyear)

* keep one observation per occupation
collapse (mean) occhat$nextyear,  by($whichocc)

preserve
* check against actual occ growth rate, get occ frequencies in each year
use testdata${year}, clear
collapse (count) occfreq${year}= year [fw=perwt], by(${whichocc})
save occ${year}, replace
use testdata${nextyear}, clear
collapse (count) occfreq${nextyear}= year [fw=perwt], by(${whichocc})
save occ${nextyear}, replace
restore

merge 1:1 ${whichocc} using occ${year}, nogen
merge 1:1 ${whichocc} using occ${nextyear}, nogen

g gocc$nextyear = occfreq${nextyear}/occfreq${year}

* we have F=6.34 for actual growth rate on predicted growth rate
* guess that is not bad for only 33 observations??
reg gocc$nextyear  occhat$nextyear 

* note there is very little variation in occhat!!

ren occhat$nextyear bartik$nextyear
keep $whichocc bartik$nextyear
save bartik_${whichocc}, replace
*/

********************************************************************************
* now do it by gender
********************************************************************************
* let the gender ratio of the occupation = 
* industry share of occ * gender ratio of industries (excluding own occupation)

global whichind ind15 // ind1990
* industry share of occ

foreach year in 1950 1960 1970 1980 1990 2000 2012 { // 1950 1960 1970 1980 1990 2000 2012
cd "$ACSdata"
use testdata`year', clear
keep if inrange(age,36,65)==1 // range of oldest 3 generations
contract $whichind $whichocc year sex [fw=perwt], zero nomiss freq(occind)
byso $whichocc sex: egen totocc = sum(occind)
g pind_`year' = occind/totocc // fraction of occupation in each industry
save pind${whichocc}_`year'_sex , replace

* gender ratio of industries (excluding own occupation)
cd "$ACSdata"
use testdata`year', clear
keep if inrange(age,36,65)==1 // range of oldest 3 generations
contract $whichind $whichocc sex year [fw=perwt], zero nomiss freq(goccind)

* loop over occupations to exclude them from the industry gender ratio calculation:
levelsof $whichocc, local(occlist)
foreach occ in `occlist' {
preserve
byso $whichind : egen totind = sum(goccind) if $whichocc != `occ'
byso $whichind : egen totindfemale = sum(goccind) if sex == 2 & $whichocc != `occ'
g genderratioind = totindfemale/totind // fraction female by industry

byso $whichind : egen mgenderratioind = max(genderratioind)
drop genderratioind
ren mgenderratioind genderratioind_`occ'
collapse (mean) genderratioind_`occ', by($whichind)
tempfile genderind_${whichocc}_`occ'
save "`genderind_${whichocc}_`occ''" , replace
restore
}

* merge together all the occupation excluding industry gender ratios
use "`genderind_${whichocc}_10'", clear // 10 is the first occupation code
local occlist = subinstr("`occlist'","10 ","",.)
disp("`occlist'")
foreach occ in `occlist' {
merge 1:1 $whichind using "`genderind_${whichocc}_`occ''", nogen
}
reshape long genderratioind_ , i(${whichind}) j(${whichocc})
save genderind_${whichocc}_`year' , replace

* multiply industry composition of occupation by industry gender ratios (excluding occupation)
use pind${whichocc}_`year'_sex, clear
merge 1:1 $whichocc $whichind sex using pind${whichocc}_1950_sex, nogen
merge m:1 $whichocc $whichind using  genderind_${whichocc}_`year', nogen
g bartikgender 		= pind_`year'*genderratioind_
g bartikgender_1950 = pind_1950*genderratioind_
byso $whichocc sex: egen sumbartikgender = sum(bartikgender)
byso $whichocc sex: egen sumbartikgender_1950 = sum(bartikgender_1950)
drop bartikgender bartikgender_1950
ren sumbartikgender bartikgender
ren sumbartikgender_1950 bartikgender_1950
collapse (mean) bartikgender bartikgender_1950, by(${whichocc} sex) 
merge m:1 $whichocc using gender`year' , nogen
reg gender bartikgender
reg gender bartikgender_1950

disp("`year'")

*ren bartikgender bartikgender`year'
reshape wide bartikgender bartikgender_1950, i($whichocc ) j(sex)

cap drop Y
sort $whichocc
egen Y = group($whichocc), label
drop if Y == .


cd "$ACSdata"
save bartik_${whichocc}_gender`year', replace

} // end year loop

pause
********************************************************************************
* INDUSTRY WAGE IV
********************************************************************************
* let the gender ratio of the occupation = 
* industry share of occ * gender ratio of industries (excluding own occupation)

global whichind ind15 // ind1990
* industry share of occ

foreach year in 1960 1970 1980 1990 2000 2012 { // 1950 1960 1970 1980 1990 2000 2012
qui {
/* this is same as in gender above (industry compositions)
cd "$ACSdata"
use testdata`year', clear
keep if inrange(age,36,65)==1 // range of oldest 3 generations
drop if $whichind == .
contract $whichind $whichocc year sex [fw=perwt], zero nomiss freq(occind)
byso $whichocc sex: egen totocc = sum(occind)
g pind_`year' = occind/totocc // fraction of occupation in each industry
save pind${whichocc}_`year'_sex , replace
*/

* mean wage of industries (excluding own occupation)
* want to use lifeinc not incwage
cd "$SIPP2004"
use lifeinc_occfinal_`year', clear 
ren matlabocc occfinal
drop if $whichind == .
levelsof $whichocc, local(occlist)
foreach occ in `occlist' {
preserve
collapse (mean) indwage_`occ' = lifeinc [fw=weight] if $whichocc != `occ', by($whichind sex)
tempfile indwage_${whichocc}_`occ'
save "`indwage_${whichocc}_`occ''" , replace
restore
}

cd "$ACSdata"
* merge together all the occupation excluding industry gender ratios
use "`indwage_${whichocc}_10'", clear // 10 is the first occupation code
local occlist = subinstr("`occlist'","10 ","",.)
disp("`occlist'")
foreach occ in `occlist' {
merge 1:1 $whichind sex using "`indwage_${whichocc}_`occ''", nogen
}
reshape long indwage_ , i(${whichind} sex) j(${whichocc})
save indwage_${whichocc}_`year' , replace

* multiply industry composition of occupation by industry gender ratios (excluding occupation)
use pind${whichocc}_`year'_sex, clear
merge 1:1 $whichocc $whichind sex using pind${whichocc}_1950_sex, nogen
merge 1:1 $whichocc $whichind sex using  indwage_${whichocc}_`year', nogen
g bartikwage 		= pind_`year'*indwage_
g bartikwage_1950 	= pind_1950*indwage_
byso $whichocc sex: egen sumbartikwage = sum(bartikwage)
byso $whichocc sex: egen sumbartikwage_1950 = sum(bartikwage_1950)
drop bartikwage bartikwage_1950
ren sumbartikwage bartikwage
ren sumbartikwage_1950 bartikwage_1950
collapse (mean) bartikwage bartikwage_1950, by(${whichocc} sex) 
} // end qui
pause
cd "$SIPP2004"
g matlabocc = $whichocc
merge 1:1 matlabocc sex using wage`year' , nogen
pause
reg lifeinc bartikwage
reg lifeinc bartikwage_1950

disp("`year'")
pause
drop lifeinc
reshape wide bartikwage bartikwage_1950, i($whichocc ) j(sex)

*ren bartikwage bartikwage`year'

/*
sort $whichocc
egen Y = group($whichocc), label
drop if Y == .
*/

cd "$ACSdata"
save bartik_${whichocc}_wage`year', replace

} // end year loop

**************************************************************************
* Change over time bartik IV
**************************************************************************
* WAGE LEVEL = treatment INDUSTRY COMPOSITION = continuous exposure - fix at 1950
* this is the same as the static IV, created above as bartikgender_1950 and bartikwage_19501/2

* other way around:
* INDUSTRY GROWTH = treatment INDUSTRY COMPOSITION AND WAGE = continuous exposure - fix at 1950 and 1960 respectively
* existing files pind_1950 and indwage_occfinal_1960
* need to calculate industry growth by year excluding own occupation
* multiply all 3 together and sum to occupation level. 


* 1) growth rate in industry at national level from 1950 to t (excluding own occupation)

cd "$ACSdata"
* industry size nationally
foreach year in 1950 1960 1970 1980 1990 2000 2012  {
	use testdata`year', clear
		levelsof $whichocc, local(occlist)
		foreach occ in `occlist' {
		preserve
			contract $whichind year [fw=perwt] if $whichocc != `occ', zero nomiss freq(find`year'_`occ') // percent(pind`year')
			*replace pind`year' = pind`year'/100
			tempfile ind_${whichocc}_`occ'
			save "`ind_${whichocc}_`occ''" , replace
			*save ind`year', replace
			
		restore
} // end occ loop

cd "$ACSdata"
* merge together all the occupation excluding industry gender ratios
use "`ind_${whichocc}_10'", clear // 10 is the first occupation code
local occlist = subinstr("`occlist'","10 ","",.)
disp("`occlist'")
foreach occ in `occlist' {
merge 1:1 $whichind using "`ind_${whichocc}_`occ''", nogen
}
reshape long find`year'_ , i(${whichind}) j(${whichocc})
drop if ind15 == 14 // active duty military starts in 1990, does not exist in 1950 reference year
save ind_${whichocc}_`year' , replace
} // end year loop



* make growth rates, merge in industry wages and composition, multiply to get bartikgrowth IV
foreach year in 1960 1970 1980 1990 2000 2012   { // 1960 1970 1980 1990 2000 2012 
/*
if "`year'" != "2000" {
local nextyear = `year' + 10
}
if "`year'" == "2000" {
local nextyear 2012
}
*/
cd "$ACSdata"
* measure growth rate from INITIAL YEAR = 1950
use ind_${whichocc}_1950, clear
merge 1:1 $whichind $whichocc using ind_${whichocc}_`year', nogen
* g growthind = (find`nextyear' / find`year')
*g growthind = (find`year'/ find1950)
g growthind = (find`year'_/ find1950_)
save growthind_1950_`year', replace

* merge in initial industry composition and wage levels
merge 1:m $whichind $whichocc using pind${whichocc}_1950_sex, nogen
cd "$ACSdata"
merge 1:1 $whichind $whichocc sex using indwage_occfinal_1960, nogen  // NOTE WAGE VARIES BY SEX
merge m:1 $whichind $whichocc using genderind_${whichocc}_1950, nogen

* industry growth instrument for wages
g bartikgrowth`year' = growthind*pind_1950*indwage_
byso $whichocc sex : egen sumbartikgrowth`year' = sum(bartikgrowth`year')
drop bartikgrowth`year'
ren sumbartikgrowth`year' bartikgrowth

* industry growth instrument for gender ratio
g bartikgrowth`year'g = growthind*pind_1950*genderratioind_
byso $whichocc sex : egen sumbartikgrowth`year'g = sum(bartikgrowth`year'g)
drop bartikgrowth`year'g
ren sumbartikgrowth`year'g bartikgrowthg


collapse (mean) bartikgrowth bartikgrowthg, by(${whichocc} sex) 
cd "$SIPP2004"
g matlabocc = $whichocc
merge 1:1 matlabocc sex using wage`year' , nogen
reg lifeinc bartikgrowth if sex == 1
reg lifeinc bartikgrowth if sex == 2
disp("`year'")
pause
drop lifeinc
reshape wide bartikgrowth bartikgrowthg, i($whichocc ) j(sex)

pause
cd "$ACSdata"
save bartik_${whichocc}_growth`year', replace

}
********************************************************************************
* change over time in female LFP IV
********************************************************************************
* 1) gender ratio by occupation in 1950
* 2) ratio of male to female in the labor force in year t relative to 1950
* (so value for 1950 would be 1)
* 3) multiply these together to get the predicted gender ratio by occupation 
* resulting from initial sorting and overall LFP growth

foreach year in 1950 1960 1970 1980 1990 2000 2012  {
	use testdata`year', clear
		levelsof $whichocc, local(occlist)
		foreach occ in `occlist' {
		preserve
			collapse (count) find`year'_`occ' = serial [fw=perwt] if $whichocc != `occ', by(sex)
			tempfile ind_${whichocc}_`occ'
			save "`ind_${whichocc}_`occ''" , replace
			*save ind`year', replace
			
		restore
} // end occ loop

cd "$ACSdata"
* merge together all the occupation excluding industry gender ratios
use "`ind_${whichocc}_10'", clear // 10 is the first occupation code
local occlist = subinstr("`occlist'","10 ","",.)
disp("`occlist'")
foreach occ in `occlist' {
merge 1:1 sex using "`ind_${whichocc}_`occ''", nogen
}
reshape long find`year'_ , i(sex) j(${whichocc})
*drop if ind15 == 14 // active duty military starts in 1990, does not exist in 1950 reference year
save LFPsex_${whichocc}_`year' , replace
} // end year loop

* get ratio of LFP gender ratios relative to 1950:
foreach year in 1960 1970 1980 1990 2000 2012   {
use LFPsex_${whichocc}_1950, clear
	g find1950_M = find1950_ if sex == 1
	byso ${whichocc}: egen mfind1950_M = max(find1950_M)
	g genderratio1950 = find1950_/mfind1950_M if sex == 2
	byso ${whichocc}: egen mgenderratio1950 = max(genderratio1950)
	
merge 1:1 ${whichocc} sex using LFPsex_${whichocc}_`year'
g find`year'_M = find`year'_ if sex == 1
byso ${whichocc}: egen mfind`year'_M = max(find`year'_M)
g genderratio`year' = find`year'_/mfind`year'_M if sex == 2
byso ${whichocc}: egen mgenderratio`year' = max(genderratio`year')

g LFP_`year'_1950 = mgenderratio`year'/mgenderratio1950
pause
collapse (mean) LFP_`year'_1950, by(${whichocc})
save LFPratio`year', replace
}

* multiply ratio of LFP gender ratios by occupation gender ratios in 1950
foreach year in 1950 1960 1970 1980 1990 2000 2012   {
cd "$ACSdata"
use gender1950, clear // gender ratio by occupation in 1950
merge 1:1 ${whichocc} using LFPratio`year', nogen
g LFPIV = gender*LFP_`year'_1950
save LFPIV`year', replace
}


********************************************************************************
* attribute regressions
********************************************************************************

*cd "$DOTdata"
*use onet_occ1990_clean, clear // old data lacks competition but has change over time
cd "$ACSdata"
use onet_clean13_norm, clear // all recent ONET data
cd "$DOTdata"
merge 1:1 occ1990 using onet_occ1990_clean, nogen // change over time data
cd "$ACSdata"
merge 1:1 occ1990 using crosswalk_${whichocc}_occ1990, nogen keep(match) // note we are missing ONET data for many occ1990!!
* the missing onet occupations are due to occupations that only exist in older census years
* and these are not matched to onet but their neighbors are so when we aggregate to occfinal we do catch everyone!!
ds ${whichocc}, not 
disp("`r(varlist)'")
collapse (mean) `r(varlist)', by(${whichocc})
* IMLevel_of_Competitionn, IMContact_With_Others, IMTime_Pressuren
drop if year ==. | occ1990 ==. | occ6 == . | occfinal == .
ren Level_of_Competition competition
ren Contact_With_Others contact
ren Time_Pressure time
foreach var in language math people qreason strengthnum things data {
ren `var'9 `var'
}

keep $whichocc competition time contact language math people qreason strengthnum things data

* studentize the attribute data!!
ren * c*
ren c$whichocc $whichocc 
foreach var of varlist c* {
tempvar meanval
egen `meanval' = mean(`var')
tempvar sdval
egen `sdval' = sd(`var')
*g `var'n = (`var' - `meanval')/`sdval'
replace `var' = (`var' - `meanval')/`sdval'
}

ren c* *

cd "$ACSdata"
save attributes_${whichocc}, replace

cd "$CPSdata"
use flexcps_${whichocc}, clear
cd "$ACSdata"
merge m:1 $whichocc using attributes_${whichocc}, nogen 
// get flex variables: nonstandard and flexhrs ALSO occ80, vary at ed2*occ level!!

la var competition "Level of Competition"
la var contact "Contact with Others"
la var time "Time Pressure"
la var flexhrs "Flexible Working Hours"
la var nonstandard "Nonstandard Working Hours"
la var data "Relationship to Data"
la var people "Relationship to People"
la var things "Relationship to Things"




foreach year in 1960 1970 1980 1990 2000 2012 { 

preserve

cd "$ACSdata"
*merge 1:m $whichocc using bartik_${whichocc}, nogen
merge 1:1 $whichocc using bartik_${whichocc}_gender`year', nogen
merge 1:1 $whichocc using bartik_${whichocc}_wage`year', nogen
merge 1:1 $whichocc using bartik_${whichocc}_growth`year', nogen
cap drop Y



merge m:1 $whichocc using edreq`year', nogen

***** BLP instruments: sum of attributes of other occupations
* better would be competitor occupations which I can define by education level?
* median occ has .4 with college degree so that seems like a good cutoff
g edflag = ed2 > .4
foreach var in competition contact time flexhrs nonstandard data people things {
g BLP`var' = .
levelsof $whichocc, local(occlist)
foreach occ in `occlist' {

g temp0 = sum(`var') if $whichocc != `occ' & edflag == 0
g temp1 = sum(`var') if $whichocc != `occ' & edflag == 1
egen BLP`var'_occ`occ'ed0 = max(temp0) 
egen BLP`var'_occ`occ'ed1 = max(temp1) 

replace BLP`var' = BLP`var'_occ`occ'ed0 if $whichocc == `occ' & edflag == 0
replace BLP`var' = BLP`var'_occ`occ'ed1 if $whichocc == `occ' & edflag == 1
drop temp* BLP`var'_occ`occ'*
} // end occ loop
} // end var loop
pause
cap drop __*

* Drop "Farming Forestry and Fishing" because no observations....
* drop if occ6 == 4
sort $whichocc
egen Y = group($whichocc), label
drop if Y == .

merge 1:m $whichocc using gender`year', nogen

sort $whichocc
cd "$ACSdata"
save bartik_${whichocc}_att_`year', replace

*cd "$home/matlab/data"
*export delimited using bartik_${whichocc}_att_`year' , nolabel replace 

restore

} // end year loop

*************************
* POOL YEARS
*************************
cd "$ACSdata"
use bartik_${whichocc}_att_1960, clear
g year = 1960
foreach year in 1970 1980 1990 2000 2012 {
append using bartik_${whichocc}_att_`year'
replace year = `year' if year == .
}
save bartik_${whichocc}_att_pooled, replace

*cd "$home/matlab/data"
*export delimited using bartik_${whichocc}_att_pooled , nolabel replace 




**************************************************************************
* Import Whatm from Matlab
**************************************************************************
global finaldata "$data/FINAL POOLED"
foreach year in 1960 1970 1980 1990 2000 2012 {
cd "$home/matlab/data"
import delimited using W_estimate_sigmas_`year'.csv, clear // W_estimate_`year'.csv means it is that men and women have one sigma
ren v1 Y
ren v2 sex
ren v3 What
ren v4 sharej
ren v5 share0
ren v6 pi
ren v7 year
cd "$finaldata"
save W_estimate_`year', replace
}

cd "$finaldata"
use W_estimate_1960, clear
foreach year in 1970 1980 1990 2000 2012 {
append using W_estimate_`year'
}

cd "$ACSdata"
merge m:1 Y year using bartik_${whichocc}_att_pooled, nogen

g delta = log(sharej) - log(share0)
g Wgender = What*gender
g Pgender1 = pi*bartikgender1


local attributes nonstandard flexhrs contact competition time
forval x = 1/2 {
reg delta gender What if sex == `x', robust
reg delta gender What `attributes' if sex == `x', robust
reg delta gender What i.occfinal i.year if sex == `x', robust
*predict yhat`x' if sex == `x'
}

/*
ds sex year $whichocc , not
reshape wide `r(varlist)', i(${whichocc} year) j(sex)
xtset ${whichocc} year

reg delta1 gender1 What1 i.occfinal i.year, robust cluster(occfinal)
predict yhat1
reg delta2 gender2 What2 i.occfinal i.year, robust cluster(occfinal)
predict yhat2

xtreg delta1 gender1 What1 i.year, fe vce(robust)
predict yhatxt1, xbu
xtreg delta2 gender2 What2 i.year, fe vce(robust)
predict yhatxt2, xbu
br yhat*
*/

la var bartikwage1 "Male: Bartik Wage"
la var bartikwage2 "Female: Bartik Wage"
la var bartikgender1 "Male: Bartik Gender"
la var bartikgender2 "Female: Bartik Gender"
la var bartikwage_19501 "Male: Bartik 1950 Wage"
la var bartikwage_19502 "Female: Bartik 1950 Wage"
la var bartikgender_19501 "Male: Bartik 1950 Gender"
la var bartikgender_19502 "Female: Bartik 1950 Gender"
la var bartikgrowth1 "Male: Bartik Industry Growth Wage"
la var bartikgrowth2 "Female: Bartik Industry Growth Wage"
la var bartikgrowthg1 "Male: Bartik Industry Growth Gender"
la var bartikgrowthg2 "Female: Bartik Industry Growth Gender"


forval sex = 1/2 {
*********** cross section covariates ************************
local attributes nonstandard flexhrs contact competition time
local ivvars1 bartikgender`sex' bartikwage`sex' 
local ivvars2 bartikgender`sex' bartikwage`sex'  pi

if "`sex'" == "1" {
local title "Male"
local G "M"
}
if "`sex'" == "2" {
local title "Female"
local G "F"
}
la var gender "Fraction Female ($\frac{ \gamma^`G'}{\sigma^`G'_{\eta}}$)"
la var What "Latent Wage Offer ($\frac{1}{\sigma^`G'_{\eta}}$)"
la var Wgender "Interaction of Wage Offer and Fraction Female"

* normal regressions
	eststo clear
	foreach year in 1960 1970 1980 1990 2000 2012 {
		eststo: reg delta gender What `attributes' if sex == `sex' & year == `year', robust
	} // end year loop
	cd "$tables"
	esttab using Stata_cv_sex`sex'.tex, se label replace /*
	*/ title(Decomposition of Utility for `title' Workers: Cross Sectional Evidence)  /*
	*/ mtitles(1960 1970 1980 1990 2000 2012 ) nonum
/*
* first stage
eststo clear
foreach year in 1960 1970 1980 1990 2000 2012 {	
	reg What  `ivvars' `attributes'  if sex == `sex' & year == `year'
	eststo: testparm  `ivvars'
	noi disp("W for sex = `sex' is `r(F)'")
	reg gender `ivvars' `attributes' if sex == `sex'  & year == `year'
	eststo: testparm  `ivvars'
	noi disp("gender ratio for sex = `sex' is `r(F)'")
} // end year loop
	cd "$tables"
	esttab using Stata_cvIVF_sex`sex'.tex, se label replace /*
	*/ title(Test of Strength of First Stage)  /*
	*/ mtitles(1960 1970 1980 1990 2000 2012 ) nonum
*/

forval num= 1/2 { // loop over sets of instruments
* IV regressions	
	eststo clear
	foreach year in 1960 1970 1980 1990 2000 2012 {
			eststo iv`year': ivregress gmm  delta `attributes'  (gender What = `ivvars`num'') if sex == `sex' & year == `year' ,first robust
* add first stage F stats
eststo firstW`year': reg What  `ivvars`num'' `attributes'  if sex == `sex' & year == `year', robust
testparm  `ivvars`num''
estadd scalar F_IV_W = r(F) : iv`year'
eststo firstG`year':  reg gender  `ivvars`num'' `attributes'  if sex == `sex' & year == `year', robust
testparm  `ivvars`num''
estadd scalar F_IV_G = r(F) : iv`year'

	} // end year loop
	* output IV
	cd "$tables"
	esttab iv* using Stata_cvIV`num'_sex`sex'.tex, se label replace /*
	*/ title(Decomposition of Utility for `title' Workers: Instrumented Cross Sectional Evidence)  /*
	*/ mtitles(1960 1970 1980 1990 2000 2012 ) nonum scalars("F_IV_W First-stage wage F=" "F_IV_G First-stage gender F=")
	cd "$tables"
	* output first stage
	esttab firstW* using Stata_cvfirstW`num'_sex`sex'.tex, se label replace /*
	*/ title(First Stage IV Estimates for Wage for `title')  /*
	*/ mtitles(1960 1970 1980 1990 2000 2012 ) nonum 
	esttab firstG* using Stata_cvfirstG`num'_sex`sex'.tex, se label replace /*
	*/ title(First Stage IV Estimates for Gender for `title')  /*
	*/ mtitles(1960 1970 1980 1990 2000 2012 ) nonum 

	} // end loop over sets of instruments
} // end sex loop
*indicate(Major dummies = *majorBA_agg1*)


*********** Pooled Years ************************
forval sex = 1/2 {
local Wgender Wgender
local WgenderIV c.bartikwage_1950`sex'#c.bartikgender_1950`sex'
local Wgender
local WgenderIV

local ivvars1 " bartikgender_1950`sex' 	bartikwage_1950`sex' `WgenderIV' " // second most exogenous
local ivvars2 " bartikgrowthg`sex' 		bartikgrowth`sex'  `WgenderIV' " // most exogenous

*local ivvars bartikwage1 bartikwage2 bartikgender1 bartikgender2 // least exogenous

* combination
local ivvars3 "bartikgrowth`sex' bartikgrowthg`sex' bartikwage_1950`sex' bartikgender_1950`sex'  `WgenderIV' "
*local ivvars bartikwage_19501 bartikwage_19502 bartikgender_1950
*local ivvars bartikgrowth1 bartikgrowth2 bartikgrowthg1



if "`sex'" == "1" {
local title "Male"
local G "M"
}
if "`sex'" == "2" {
local title "Female"
local G "F"
}

la var gender "Fraction Female ($\frac{ \gamma^`G'}{\sigma^`G'_{\eta}}$)"
la var What "Latent Wage Offer ($\frac{1}{\sigma^`G'_{\eta}}$)"
la var Wgender "Interaction of Wage Offer and Fraction Female"

* normal regressions
	eststo clear
	eststo OLS: reg delta gender What `Wgender' i.occfinal i.year if sex == `sex', robust
	cd "$tables"
	esttab using Stata_pv_sex`sex'.tex, se label replace /*
	*/ title(Decomposition of Utility for `title' Workers: Panel Evidence)  /*
	*/ mtitles(OLS ) nonum indicate("Occupation dummies = *occfinal*" "Year dummies = *year*")

forval num= 1/3 { // loop over sets of instruments
* IV regressions	
	*eststo clear
	eststo piv`num': ivregress gmm  delta i.occfinal i.year  (gender What `Wgender' = `ivvars`num'') if sex == `sex' , robust
	estat firststage
	eststo piv`num': ivreg2  delta i.occfinal i.year  (gender What `Wgender' = `ivvars`num'') if sex == `sex' , gmm2s robust

pause

	* add first stage F stats
eststo pfirstW`num': reg What  `ivvars`num'' i.occfinal i.year if sex == `sex', robust
testparm  `ivvars`num''
estadd scalar F_IV_W = r(F) : piv`num'
eststo pfirstG`num':  reg gender  `ivvars`num'' i.occfinal i.year  if sex == `sex', robust
testparm  `ivvars`num''
estadd scalar F_IV_G = r(F) : piv`num'
	} // end loop over sets of instruments
	
	* output IV
	cd "$tables"
	esttab OLS piv* using Stata_pvIV_sex`sex'_`Wgender'.tex, se label replace /*
	*/ title(Decomposition of Utility for `title' Workers: Instrumented Panel Evidence)  /*
	*/ mtitles(OLS IV1 IV2 IV3 ) nonum /*
	*/ scalars("F_IV_W First-stage wage F=" "F_IV_G First-stage gender F=") /*
	*/ indicate("Occupation dummies = *occfinal*" "Year dummies = *year*")
	cd "$tables"
	* output first stage
	esttab pfirstW* using Stata_pvfirstW_sex`sex'_`Wgender'.tex, se label replace /*
	*/ title(Pooled First Stage IV Estimates for Wage for `title')  /*
	*/ mtitles(IV1 IV2 IV3 ) nonum indicate("Occupation dummies = *occfinal*" "Year dummies = *year*")
	esttab pfirstG* using Stata_pvfirstG_sex`sex'_`Wgender'.tex, se label replace /*
	*/ title(Pooled First Stage IV Estimates for Gender for `title')  /*
	*/ mtitles(IV1 IV2 IV3  ) nonum indicate("Occupation dummies = *occfinal*" "Year dummies = *year*")


} // end sex loop


* playing around to look at F stats of first stage:
forval sex = 1/2 {
ivreg2  delta i.occfinal i.year  (gender What = bartikgrowth`sex' bartikgrowthg`sex' bartikwage_1950`sex' bartikgender_1950`sex') if sex == `sex' , gmm2s
*ivreg2  delta i.occfinal i.year  (gender What = bartikgrowth`sex' bartikgrowthg`sex' bartikwage`sex' bartikgender`sex') if sex == `sex' , gmm2s
*ivreg2  delta i.occfinal i.year  (gender What = bartikgrowth1 bartikgrowth2 bartikgrowthg1 bartikgrowthg2 bartikwage`sex' bartikgender`sex') if sex == `sex' , gmm2s
*ivreg2  delta i.occfinal i.year  (gender What = bartikgrowth1 bartikgrowth2 bartikgrowthg1 bartikgrowthg2 bartikwage`sex' bartikgender`sex') if sex == `sex' , gmm2s

***********
* interpreting the results
***********
tabstat gender, stats(p25 p50 p75) // moving from .2 to .4 to .76
margins , at((p25) gender)
mat p25 = r(b)
g deltap25 = p25[1,1]
margins, at((p50) gender) 
mat p50 = r(b)
margins, at((p75) gender) 
mat p75 = r(b)

* go from delta to a new share estimate- delta is uhat
* exp(delta)/sum(exp(delta))


*
g sharehat = exp(delta)*share0
*

g numerator = exp(delta)
egen denominator1 = sum(numerator) if sex == 1
egen denominator2 = sum(numerator) if sex == 2
replace denominator1 = denominator1 + 1
replace denominator2 = denominator2 + 1
g sharehat1 = numerator/denominator1 if sex == 1
g sharehat2 = numerator/denominator2 if sex == 2

}


****************** options for IV ******************
* bartikgrowth1 bartikgrowth2 bartikgrowthg1 
* BLP* pi 
* bartikwage_19501 bartikgender_1950
* bartikwage1 bartikwage2 bartikgender


local ivvars bartikgrowth1 bartikgrowth2 bartikgrowthg1 // most exogenous
local ivvars bartikwage_19501 bartikwage_19502 bartikgender_1950 // second most exogenous
local ivvars bartikwage1 bartikwage2 bartikgender1 bartikgender2 // least exogenous

* combination
local ivvars bartikgrowth1 bartikgrowth2 bartikgrowthg1 bartikwage_19501 bartikwage_19502 bartikgender_1950 BLP*
local ivvars bartikwage_19501 bartikwage_19502 bartikgender_1950 BLP*
local ivvars bartikgrowth1 bartikgrowth2 bartikgrowthg1  BLP* pi


qui {
* test strength of first stage
forval sex = 1/2 {
reg What  `ivvars' i.occfinal i.year  if sex == `sex'
testparm  `ivvars'
noi disp("W for sex = `sex' is `r(F)'")
}

reg gender `ivvars' i.occfinal i.year if sex == 1 // this is the same by gender, need to do it by gender to get correct sample size
testparm  `ivvars'
noi disp("gender ratio for sex = `sex' is `r(F)'")
} // end qui

ivregress gmm delta i.occfinal i.year (What gender = `ivvars') if sex == 1
disp _b[What]
disp _b[gender]
ivregress gmm delta i.occfinal i.year (What gender  = `ivvars') if sex == 2
disp _b[What]
disp _b[gender]

* old runs
ivregress gmm delta i.occfinal i.year (What gender = bartikgender bartikwage1 pi) if sex == 1
ivregress gmm delta i.occfinal i.year (What gender = bartikgrowthg1 bartikgender_1950 bartikwage_19501 pi BLP*) if sex == 1
*ivregress 2sls delta i.occfinal i.year (What gender Wgender = bartikgender bartikwage1 pi Pgender) if sex == 1

reg delta gender What if sex == 2
reg delta gender What i.occfinal i.year if sex == 2
ivregress gmm delta i.occfinal i.year (What gender = bartikgender bartikwage2 pi) if sex == 2
ivregress gmm delta i.occfinal i.year (What gender = bartikgrowthg1 bartikgender_1950 bartikwage_19501 pi BLP*) if sex == 2, first

/*
drop gender // this is contemporaneous gender from matlab
cd "$ACSdata"
merge m:1 Y using gender2000, nogen // merge in older generation gender
*/ 
g delta = log(sharej) - log(share0)

cd "$ACSdata"
merge m:1 Y using bartik_${whichocc}_att_2000, nogen
merge m:1 Y using bartik_${whichocc}_gender2000, nogen
merge m:1 $whichocc using bartik_${whichocc}_wage2000, nogen

*drop if occfinal == 48
*drop if occfinal == 55

* test first stages
reg gender bartikgender if sex == 1
reg gender bartikgender if sex == 2
reg What pi if sex == 1
reg What pi if sex == 2

reg delta gender What if sex == 1
ivreg delta What (gender = bartikgender) if sex == 1
ivreg delta (What gender = bartikgender pi) if sex == 1
ivreg delta (What gender = bartikgender pi BLP* bartikwage1) if sex == 1
*reg3 (delta What gender) (gender bartikgender pi) (What pi) if sex == 1, 2sls

reg delta gender What if sex == 2
ivreg delta What (gender = bartikgender) if sex == 2
ivreg delta (What gender = bartikgender pi) if sex == 2
ivreg delta  (What gender = bartikgender pi) if sex == 2

reg delta nonstandard flexhrs contact competition time data gender What if sex == 1
ivreg delta nonstandard flexhrs contact competition time data gender (What = pi) if sex == 1
ivreg delta nonstandard flexhrs contact competition time data ed2 (gender What = bartikgender2000 pi) if sex == 1
ivreg delta nonstandard flexhrs contact competition time data ed2 (gender What = bartikgender2000 BLP* pi) if sex == 1

reg delta nonstandard flexhrs contact competition time data gender What if sex == 2
ivreg delta nonstandard flexhrs contact competition time data gender (What = pi) if sex == 2
ivreg delta nonstandard flexhrs contact competition time data ed2 (gender What = bartikgender pi) if sex == 2
ivreg delta nonstandard flexhrs contact competition time data ed2 (gender What = bartikgender BLP* pi) if sex == 2

reg delta flexhrs contact competition if sex == 2
ivreg delta flexhrs contact competition (gender=bartikgender) if sex == 2
reg delta flexhrs contact competition if sex == 1
ivreg delta flexhrs contact competition (gender=bartikgender) if sex == 1

* nonstandard flexhrs contact competition time data language math people qreason strengthnum things

* National occupation shock method
* 1) get percent change in national occupation size from t-1 to t
* 2) get number of jobs in occupation*region cell in t-1
* 3) multiply 1)*2) to predict number of jobs in occupation*region in t
* 4) make a variable that is yhat_t - y_t-1 / y_t-1, so percent growth in occupation by region

