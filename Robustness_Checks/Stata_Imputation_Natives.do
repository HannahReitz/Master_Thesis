/*----------------------------------------------------------------------
Hannah Luisa Reitz
University of Mannheim

Master Thesis "Classroom Ethnic Composition, Educational Achievement, and School Satisfaction"
*************************************************************************/
*net install nepstools, from(http://nocrypt.neps-data.de/stata)

clear
version 16
set more off
capture log close


global datverz Z:/...  // Global for data 


/************************************************************************
Prepare Master Data from Cohort Data
*************************************************************************/

use $datverz/SC4_CohortProfile_R_11-0-0.dta, clear

keep ID_t wave ID_cc ID_i tx80501 t723080_g1 tx80107 tx80106 tx8050y 

// Recode missings
nepsmiss _all

bysort wave: count

// Reduce to first wave
drop if wave > 1
bysort wave: count

sort ID_t

save master.dta, replace 


/************************************************************************
Prepare Target
*************************************************************************/
use $datverz/SC4_pTarget_R_11-0-0.dta, clear

keep ID_t wave t514006 t400500_g1v1 t31035a t412010 t412020 t412030   ///
     t700031 t70004y t70004m t731422_g14 t731472_g14 t731320 t731370 t400500_g3v1R
	 
// Recode missings
nepsmiss _all

list ID_t wave in 1/100, sepby(ID_t)


// Find and delete duplicates
duplicates report ID_t wave
quietly : duplicates tag ID_t wave , generate(dups)
tabulate dups
duplicates drop ID_t wave, force
drop dups

save target.dta, replace 


/************************************************************************
Merge Target Data with Master Data
*************************************************************************/
use master.dta, clear

merge 1:1 ID_t wave using target.dta, ///
   keep(master matched)
drop _merge

save master.dta, replace  



/************************************************************************
Prepare pParent Data
*************************************************************************/
use $datverz/SC4_pParent_R_11-0-0.dta, clear

keep ID_t wave p731904_g14 p731954_g14 p731802_g2 p731852_g2

// Recode missings
nepsmiss _all

list ID_t wave in 1/100, sepby(ID_t)

save parent.dta, replace 


/************************************************************************
Merge Parent Data with Master Data
*************************************************************************/
use master.dta, clear

merge 1:1 ID_t wave using parent.dta, ///
   keep(master matched)
drop _merge

save master.dta, replace  



/************************************************************************
Prepare Competency Data
*************************************************************************/

use $datverz/SC4_xTargetCompetencies_R_11-0-0.dta, clear

keep ID_t mag9_sc1 reg9_sc1 dgg9_sc3b

// Recode missings
nepsmiss _all

save competence.dta, replace 


/************************************************************************
Merge Competency Data with Master Data
*************************************************************************/
use master.dta, clear

merge m:1 ID_t using competence.dta,  ///
   keep(master matched)
drop _merge

save master.dta, replace  

 




/************************************************************************
Prepare Dependent Variables
*************************************************************************/

/*------------------------------------------------------------------------
School Satisfaction
------------------------------------------------------------------------*/

sum t514006, detail
tab t514006, m
gen sat = .
replace sat = t514006
tab sat



/*------------------------------------------------------------------------
Math competence 
------------------------------------------------------------------------*/

sum mag9_sc1, detail
mdesc mag9_sc1
ren mag9_sc1 math
mdesc math




/************************************************************************
Prepare Independent Variables
*************************************************************************/

/*------------------------------------------------------------------------
General Cognitive Ability
------------------------------------------------------------------------*/

fre dgg9_sc3b
ren dgg9_sc3b cog
fre cog


/*------------------------------------------------------------------------
SES parents
------------------------------------------------------------------------*/

/* Information from parents
------------------------------*/

sum p731904_g14, d
sum p731954_g14, d

// SES parent (respondent)
gen sesbef = p731904_g14

// SES parent (partner)
gen sespart = p731954_g14


// Take the higher value of both parents
egen ses = rowmax(sesbef sespart)

sum ses, d
mdesc ses



/* Information from students
------------------------------*/

sum t731422_g14, d 
sum t731472_g14, d

// SES parent (mother)
gen sesm = t731422_g14

// SES parent (father)
gen sesf = t731472_g14


// Take the higher value of both parents
egen ses2 = rowmax(sesm sesf)

sum ses2, d
mdesc ses2


// Take value from parents; if missing, replace with value from respondent
gen ses3 = .
replace ses3 = ses 
replace ses3 = ses2 if ses == . | ses == .l | ses == .j | ses == .e | ses == .b

drop ses
drop ses2
ren ses3 ses
mdesc ses


/*------------------------------------------------------------------------
Migration Background
------------------------------------------------------------------------*/

fre t400500_g1v1

gen mig = .
replace mig = 0 if t400500_g1v1 == 0 // No migration background
replace mig = 1 if t400500_g1v1 == 1 | t400500_g1v1 == 2 | t400500_g1v1 == 3 | t400500_g1v1 == 4 | t400500_g1v1 == 5 | t400500_g1v1 == 6 | t400500_g1v1 == 7 | t400500_g1v1 == 8 | t400500_g1v1 == 9 // Migration background
tab mig, m


/*------------------------------------------------------------------------
Generation
------------------------------------------------------------------------*/

fre t400500_g1v1

gen generation = .
replace generation = 0 if t400500_g1v1 == 1 | t400500_g1v1 == 2 // First generation
replace generation = 1 if t400500_g1v1 == 3 | t400500_g1v1 == 4 | t400500_g1v1== 5 | t400500_g1v1 == 6 // Second generation
replace generation = 2 if t400500_g1v1 == 7 | t400500_g1v1 == 8 | t400500_g1v1 == 9 // Third generation
fre generation


/*------------------------------------------------------------------------
Gender
------------------------------------------------------------------------*/

fre tx80501

gen female = .
replace female = 0 if tx80501 == 1 // Male
replace female = 1 if tx80501 == 2 // Female
fre female


/*------------------------------------------------------------------------
Age
------------------------------------------------------------------------*/

*fre t70004y
fre tx8050y // Birth year

gen age = .
replace age = 2010 - tx8050y
fre age


/*------------------------------------------------------------------------
Education Parents
------------------------------------------------------------------------*/

/* Information from parents
------------------------------*/
fre p731802_g2
fre p731852_g2

// Education parent (respondent)
gen edubef = p731802_g2

// Education parent (partner)
gen edupart = p731852_g2


// Take the higher value of both parents
egen edu1 = rowmax(edubef edupart)

mdesc edu1
fre edu1

// Recode to low, middle, high
gen edu2 = .
replace edu2 = 0 if edu1 == 0 | edu1 == 1 | edu1 == 2  // Low level
replace edu2 = 1 if edu1 == 3 | edu1 == 4 // Middle level
replace edu2 = 2 if edu1 == 5 | edu1 == 6 | edu1 == 7 | edu1 == 8 // High level
fre edu2



/* Information from students
------------------------------*/

fre t731320
fre t731370

// Education parent (mother)
gen edum = t731320

// Education parent (father)
gen eduf = t731370

// Take the higher value of both parents
egen edu3 = rowmax(edum eduf)

gen edu4 = .
replace edu4 = 0 if edu3 == 0 | edu3 == 1
replace edu4 = 1 if edu3 == 2 
replace edu4 = 2 if edu3 == 3 | edu3 == 4 | edu3 == 5 | edu3 == 6

// Take value from parents; if missing, replace with value from respondent
gen edu5 = .
replace edu5 = edu2
replace edu5 = edu4 if edu2 == . | edu2 == .l | edu2 == .j | edu2 == .e
fre edu5

drop edu1
drop edu2
drop edu3
drop edu4
ren edu5 edu
mdesc edu


/*-----------------------------------------------------------------------
Aspiration Education
------------------------------------------------------------------------*/

fre t31035a

gen attedu = .
replace attedu = 0 if t31035a == 1 | t31035a == 2 | t31035a == 3 // Low aspiration
replace attedu = 1 if t31035a == 4 // High aspiration
tab attedu, m


/*------------------------------------------------------------------------
Sprachgebrauch Familie
------------------------------------------------------------------------*/

// Language with mother

fre t412010

gen langm = .
replace langm = 0 if t412010 == 1 | t412010 == .j
replace langm = 1 if t412010 != 1 & t412010 != . & t412010 != .e & t412010 != .g & t412010 != .j
tab langm, m

// Language with father
fre t412020

gen langf = .
replace langf = 0 if t412020 == 1 | t412020 == .j
replace langf = 1 if t412020 != 1 & t412020 != . & t412020 != .e & t412020 != .g & t412020 != .j
tab langf, m

// Language with both parents
gen lang = .
replace lang = 0 if langm == 0 & langf == 0 // Only German
replace lang = 1 if langm == 1 | langf == 1 // At least sometimes other language
tab lang, m


/*------------------------------------------------------------------------
Schulform
------------------------------------------------------------------------*/

*fre t723080_g1
fre tx80106


// Dropping students on special needs schools
drop if tx80106 == 7 

gen school = .
replace school = 0 if tx80106 == 2 // Vocational track
replace school = 1 if tx80106 == 4 // Intermediate track
replace school = 2 if tx80106 == 6 // Academic track
replace school = 3 if tx80106 == 5 | tx80106 == 3 // Comprehensive tracks

fre school


/************************************************************************
Dropping Classes with less than 10 Students
*************************************************************************/

sort ID_cc
distinct ID_cc


bysort ID_cc: gen ID_cc_freq = _N
drop if ID_cc_freq < 10


/************************************************************************
Generating Averages on Class Level for ses, cog and mig
*************************************************************************/

sort ID_cc

// Mean SES in classroom
bysort ID_cc: egen meanses = mean(ses)

// Mean cognitive ability in classroom
bysort ID_cc: egen meancog = mean(cog)

// Percentage of minority students in classroom
bysort ID_cc: egen meanmig = mean(mig)
bysort ID_cc: gen meanmig2 = meanmig * 100

// Dividing by 10
gen percmig = meanmig2 / 10


save master.dta, replace  



/************************************************************************
Imputation
*************************************************************************/

use /master.dta, clear

keep ID_cc ID_t sat math cog ses edu mig female age attedu lang generation school meanses meancog percmig

mdesc sat math cog ses edu mig female age attedu lang generation school meanses meancog percmig


// Setting all missings to .
mvencode _all, mv(.a = -999 \ .k = -999 \ .l = -999 \ .y = -999 \ .c = -999 \ .j = -999 \ .e = -999)

mvdecode _all, mv(-999 = .)

// Dropping observations with missing on migration background
drop if mig == .

// Dropping observations of ethnic minority
drop if mig == 1

// Setting mi
mi set mlong

// Analyzing missings
mdesc sat math cog ses edu female age attedu school meanses meancog percmig

mi misstable summarize sat math cog ses edu mig female age attedu lang generation school meanses meancog percmig

mi misstable patterns sat math cog ses edu female age attedu school meanses meancog percmig


// Imputation
mi register imputed sat math cog ses edu female attedu meancog age

mi register regular percmig meanses school

mi impute chained (pmm,knn(5)) ses math sat cog meancog age (ologit) edu attedu (logit) female = i.school percmig meanses, burn(10) add(20) rseed(9478)


// Checking imputation
midiagplots ses, m(1/5) combine
midiagplots cog, m(1/5) combine
midiagplots sat, m(1/5) combine
midiagplots math, m(1/5) combine
midiagplots meancog, m(1/5) combine



save imp_nat.dta, replace





