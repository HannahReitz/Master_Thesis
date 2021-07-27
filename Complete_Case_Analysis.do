/************************************************************************
Hannah Luisa Reitz
University of Mannheim

Master Thesis "Classroom Ethnic Composition, Educational Achievement, and School Satisfaction"

File: Complete case analysis
*************************************************************************/

*net install nepstools, from(http://nocrypt.neps-data.de/stata)

clear
version 16
set more off
capture log close 


global datverz C:\Users...  // Global for data 
global arbverz C:\Users... // Global for working directory

/************************************************************************
Prepare Master Data from Cohort Data
*************************************************************************/

use $datverz/SC4_CohortProfile_D_11-0-0.dta, clear

keep ID_t wave ID_cc ID_i tx80501 t723080_g1 tx80107 tx80106 tx8050y 

// Recode missings
nepsmiss _all

bysort wave: count

// Reduce to first wave
drop if wave > 1
count

sort ID_t

save $arbverz/master.dta, replace 


/************************************************************************
Prepare Target
*************************************************************************/
use $datverz/SC4_pTarget_D_11-0-0.dta, clear

keep ID_t wave t514006 t400500_g1v1 t31035a t412010 t412020 t412030   ///
     t700031 t70004y t70004m t731422_g14 t731472_g14 t731320 t731370
	 
// Recode missings
nepsmiss _all

list ID_t wave in 1/100, sepby(ID_t)


// Find and delete duplicates
duplicates report ID_t wave
quietly : duplicates tag ID_t wave , generate(dups)
tabulate dups
duplicates drop ID_t wave, force
drop dups

sort ID_t 

save $arbverz/target.dta, replace 


/************************************************************************
Merge Target Data with Master Data
*************************************************************************/
use $arbverz/master.dta, clear

merge 1:1 ID_t wave using $arbverz/target.dta, ///
   keep(master matched)
drop _merge

sort ID_t 

save $arbverz/master.dta, replace  



/************************************************************************
Prepare pParent Data
*************************************************************************/
use $datverz/SC4_pParent_D_11-0-0.dta, clear

keep ID_t wave p731904_g14 p731954_g14 p731802_g2 p731852_g2

// Recode missings
nepsmiss _all

list ID_t wave in 1/100, sepby(ID_t)

sort ID_t

save $arbverz/parent.dta, replace 



/************************************************************************
Merge Parent Data with Master Data
*************************************************************************/
use $arbverz/master.dta, clear

merge 1:1 ID_t wave using $arbverz/parent.dta, ///
   keep(master matched)
drop _merge

sort ID_t

save $arbverz/master.dta, replace  



/************************************************************************
Prepare Competencies Data
*************************************************************************/

use $datverz/SC4_xTargetCompetencies_D_11-0-0.dta, clear

keep ID_t mag9_sc1 dgg9_sc3b

// Recode missings
nepsmiss _all

sort ID_t

save $arbverz/competence.dta, replace 



/************************************************************************
Merge Competency Data with Master Data
*************************************************************************/
use $arbverz/master.dta, clear

merge m:1 ID_t using $arbverz/competence.dta,  ///
   keep(master matched)
drop _merge

sort ID_t

save $arbverz/master.dta, replace  

count


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
Migration Generation
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
fre tx8050y // birth year

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


// Recode to low, middle, high
gen edu2 = .
replace edu2 = 0 if edu1 == 0 | edu1 == 1 | edu1 == 2 // Low level
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


// Recode to low, middle, high
gen edu4 = .
replace edu4 = 0 if edu3 == 0 | edu3 == 1 // Low level
replace edu4 = 1 if edu3 == 2 // Middle level
replace edu4 = 2 if edu3 == 3 | edu3 == 4 | edu3 == 5 | edu3 == 6 // High level


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


/*------------------------------------------------------------------------
Aspiration Education
------------------------------------------------------------------------*/

fre t31035a

gen attedu = .
replace attedu = 0 if t31035a == 1 | t31035a == 2 | t31035a == 3 // Low aspiration
replace attedu = 1 if t31035a == 4 // High aspiration
tab attedu, m


/*------------------------------------------------------------------------
Language with Parents
------------------------------------------------------------------------*/

fre t412010

// Language with mother
gen langm = .
replace langm = 0 if t412010 == 1 | t412010 == .j
replace langm = 1 if t412010 == 2 | t412010 == 3 | t412010 == 4 | t412010 == 5 
tab langm, m


// Language with father
fre t412020

gen langf = .
replace langf = 0 if t412020 == 1 | t412020 == .j
replace langf = 1 if t412020 == 2 | t412020 == 3 | t412020 == 4 | t412020 == 5
tab langf, m

// Language with both parents
gen lang = .
replace lang = 0 if langm == 0 & langf == 0
replace lang = 1 if langm == 1 | langf == 1
tab lang, m


/*------------------------------------------------------------------------
School Track
------------------------------------------------------------------------*/
*fre t723080_g1
fre tx80106

// Dropping students on special needs schools
drop if tx80106 == 7 

// Recoding to 4 different tracks
gen school = .
replace school = 0 if tx80106 == 2 // Vocational track
replace school = 1 if tx80106 == 4 // Intermediate track 
replace school = 2 if tx80106 == 6 // Academic track
replace school = 3 if tx80106 == 5 | tx80106 == 3 // Comprehensive tracks

fre school


save $arbverz/master_final.dta, replace  

/************************************************************************
Creating Sample
*************************************************************************/
sort ID_cc

// Keeping relevant variables
keep ID_t ID_cc ID_i sat math cog ses mig generation female age edu attedu lang school

// Set all missings to .
mvencode _all, mv(-99)	
mvdecode _all, mv(-99 = .)

// Generating sample by list wise deletion
generate sample = 1
replace sample =  0 if sat == .
replace sample = -1 if math == .
replace sample = -2 if cog == .
replace sample = -3 if ses == .
replace sample = -4 if mig == . 
replace sample = -5 if female == .
replace sample = -6 if ID_cc == . 
replace sample = -7 if edu == .
replace sample = -8 if attedu == .
replace sample = -9 if lang == .
replace sample = -10 if school == .
replace sample = -11 if age == .


tabulate sample if sample != 1
keep if sample == 1

tabulate sample // N (sample==1) : 10,604

missings report


/************************************************************************
Dropping Classes with less than 10 Students
*************************************************************************/

sort ID_cc
distinct ID_cc

bysort ID_cc: gen ID_cc_freq = _N
drop if ID_cc_freq < 10

tabulate sample // N (sample==1) : 7,978


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

// Divided by 10
gen percmig = meanmig2 / 10



/************************************************************************
Descriptive Statistics
*************************************************************************/

// Total sample
tab edu
tab attedu
tab female
tab lang
tab school
tabstat sat math ses cog age meanses percmig meancog, statistics(mean sd) columns(statistics) 

// Separate for majority and minority students
bys mig: tab edu
bys mig: tab attedu
bys mig: tab female
bys mig: tab lang
bys mig: tab school
tab generation

bys mig: tabstat sat math ses cog age meanses percmig meancog, statistics(mean sd) columns(statistics) 

// Number of classrooms
distinct ID_cc
bys mig: distinct ID_cc


/************************************************************************
Centering Variables
*************************************************************************/
/*------------------------------------------------------------------------
Grand Mean Centering Level 2 Variables
------------------------------------------------------------------------*/

// Mean SES
egen meanses_gm = mean(meanses)
fre meanses_gm
generate meanses_c = meanses - meanses_gm
tabstat meanses_gm meanses_c, statistics( mean sd ) 
drop meanses_gm

// Percentage minority students
egen percmig_gm = mean(percmig)
generate percmig_c = percmig - percmig_gm
tabstat percmig_gm percmig_c, statistics( mean sd ) 
drop percmig_gm

// Mean cognitive ability
egen meancog_gm = mean(meancog)
fre meancog_gm
generate meancog_c = meancog - meancog_gm
tabstat meancog_gm meancog_c, statistics( mean sd ) 
drop meancog_gm


/*------------------------------------------------------------------------
Group Mean Centering Level 1 Variables
------------------------------------------------------------------------*/

// SES
egen ses_gm = mean(ses)
generate ses_c = ses - ses_gm
drop ses_gm

// Age
egen age_gm = mean(age)
generate age_c = age - age_gm
tabstat age_gm age_c, statistics( mean sd ) 
drop age_gm

// Cognitive ability
egen cog_gm = mean(cog)
generate cog_c = cog - cog_gm
*bys ID_cc: tabstat cog_cm cog_c, statistics( mean sd ) 
drop cog_gm

// School satisfaction
egen sat_gm = mean(sat)
generate sat_c = sat - sat_gm
drop sat_gm


// Quadratic term of percentag minority students
// Excluded in final models as there were no non-linear relationships

*gen percmig2_c = percmig_c*percmig_c


/************************************************************************
Multilevel Regressions: School Satisfaction
*************************************************************************/
/*------------------------------------------------------------------------
Majority Students
------------------------------------------------------------------------*/

mixed sat if mig == 0 || ID_cc:, mle
estat icc // ICC = 0.03
estat ic
estimates store M1

mixed sat ses_c i.edu i.attedu cog_c i.female age_c if mig == 0 || ID_cc:,
estat ic
estimates store M2

mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c meanses_c if mig == 0 || ID_cc:,
estat ic
estimates store M3

mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c meanses_c meancog_c if mig == 0 || ID_cc:,
estat ic
estimates store M4


// Exporting regression table
estimates table M1 M2 M3 M4, star 

esttab M1 M2 M3 M4, b(3) se(3) se aic obslast, using sat_nat.tex, replace



/*------------------------------------------------------------------------
Minority Students
------------------------------------------------------------------------*/

mixed sat if mig == 1 || ID_cc:, mle
estat icc // ICC = 0.04
estat ic
estimates store M5

mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang if mig == 1 || ID_cc:,
estat ic
estimates store M6

mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang i.school percmig_c meanses_c if mig == 1 || ID_cc:,
estat ic
estimates store M7

mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang i.school percmig_c meanses_c meancog_c if mig == 1 || ID_cc:,
estat ic
estimates store M8


// Exporting regression table
estimates table M5 M6 M7 M8, star 

esttab M5 M6 M7 M8, b(3) se(3) se aic obslast, using sat_mig.tex, replace




/************************************************************************
Multilevel Regressions: Math
*************************************************************************/
/*------------------------------------------------------------------------
Majority Students
------------------------------------------------------------------------*/

mixed math if mig == 0|| ID_cc:, mle
estat icc // ICC = 0.40
estat ic
estimates store M9

mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c if mig == 0 || ID_cc:, mle
estat ic
estimates store M10

mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.school meanses_c percmig_c if mig == 0 || ID_cc:, mle
estat ic
estimates store M11

mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.school meanses_c percmig_c meancog_c if mig == 0 || ID_cc:, mle
estat ic
estimates store M12

mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.school meanses_c percmig_c meancog_c if mig == 0 || ID_cc: ses_c, covariance(independent) mle
estat ic

mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.school meanses_c percmig_c meancog_c c.percmig_c#c.ses_c if mig == 0 || ID_cc: ses_c, covariance(independent) mle
estat ic
estimates store M13


// Exporting regression table
estimates table M9 M10 M11 M12 M13, star 

esttab M9 M10 M11 M12 M13, b(3) se(3) se aic obslast, using math_nat.tex, replace



/*------------------------------------------------------------------------
Minority Students
------------------------------------------------------------------------*/

mixed math if mig == 1|| ID_cc:, mle
estat icc // ICC = 0.39
estat ic
estimates store M14

mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang if mig == 1 || ID_cc:, mle
estat ic
estimates store M15

mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang i.school percmig_c meanses_c if mig == 1 || ID_cc:, mle
estat ic
estimates store M16

mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang i.school percmig_c meanses_c meancog_c if mig == 1 || ID_cc:, mle
estat ic
estimates store M17

mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang i.school percmig_c meanses_c meancog_c if mig == 1 || ID_cc: ses_c, covariance(independent) mle
estat ic

mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang i.school percmig_c meanses_c meancog_c c.percmig_c#c.ses_c if mig == 1 || ID_cc: ses_c, covariance(independent) mle
estat ic
estimates store M18


// Exporting regression table
estimates table M14 M15 M16 M17 M18, star 

esttab M14 M15 M16 M17 M18, b(3) se(3) se aic obslast, using math_mig.tex, replace





/************************************************************************
Regression Diagnostics of Final Models: School Satisfaction
*************************************************************************/

/*------------------------------------------------------------------------
Majority Students
------------------------------------------------------------------------*/

mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c meanses_c meancog_c if mig == 0 || ID_cc:,


predict double Pred_m1, xb
predict double Pred_c1, fitted
predict double Resid_cu1, residuals
predict Resid_cs1, rstandard
predict Resid_csr1, relevel(ID_cc) rstandard
gen double Resid_mu1 = sat - Pred_m1

// Normality
histogram Resid_cs1, width(.5) normal kdensity
qnorm Resid_cs1
pnorm Resid_cs1
graph box Resid_cs1

// Heteroscedasticity
graph twoway (scatter Resid_cs1 Pred_c1) (lfit Resid_cs1 Pred_c1), yline(0)

// Outliers
scatter Resid_cs1 Pred_c1, mlabel(ID_t)

iqr Resid_cs1



/*------------------------------------------------------------------------
Minority Students
------------------------------------------------------------------------*/

mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang i.school percmig_c meanses_c meancog_c if mig == 1 || ID_cc:,

predict double Pred_m2, xb
predict double Pred_c2, fitted
predict double Resid_cu2, residuals
predict Resid_cs2, rstandard
predict Resid_csr2, relevel(ID_cc) rstandard
gen double Resid_mu2 = sat - Pred_m2

// Normality
histogram Resid_cs2, width(.5) normal kdensity
qnorm Resid_cs2
pnorm Resid_cs2
graph box Resid_cs2

// Heteroscedasticity
graph twoway (scatter Resid_cs2 Pred_c2) (lfit Resid_cs2 Pred_c2), yline(0)

// Outliers
scatter Resid_cs2 Pred_c2, mlabel(ID_t)

iqr Resid_cs2




/************************************************************************
Regression Diagnostics of Final Model: Math Competence
*************************************************************************/

/*------------------------------------------------------------------------
Majority Students
------------------------------------------------------------------------*/

mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.school meanses_c percmig_c meancog_c c.percmig_c#c.ses_c if mig == 0 || ID_cc: ses_c, covariance(independent) mle

predict double Pred_m3, xb
predict double Pred_c3, fitted
predict double Resid_cu3, residuals
predict Resid_cs3, rstandard
predict Resid_csr3, relevel(ID_cc) rstandard
gen double Resid_mu3 = math - Pred_m3

// Normality
histogram Resid_cs3, width(.5) normal kdensity
qnorm Resid_cs3
pnorm Resid_cs3
graph box Resid_cs3

// Heteroscedasticity
graph twoway (scatter Resid_cs3 Pred_c3) (lfit Resid_cs3 Pred_c3), yline(0)

// Outliers
scatter Resid_cs3 Pred_c3, mlabel(ID_t)

iqr Resid_cs3


/*------------------------------------------------------------------------
Minority Students
------------------------------------------------------------------------*/

mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang i.school percmig_c meanses_c meancog_c c.percmig_c#c.ses_c if mig == 1 || ID_cc: ses_c, covariance(independent) mle


predict double Pred_m4, xb
predict double Pred_c4, fitted
predict double Resid_cu4, residuals
predict Resid_cs4, rstandard
predict Resid_csr4, relevel(ID_cc) rstandard
gen double Resid_mu4 = sat - Pred_m4

// Normality
histogram Resid_cs4, width(.5) normal kdensity
qnorm Resid_cs4
pnorm Resid_cs4
graph box Resid_cs4

// Heteroscedasticity
graph twoway (scatter Resid_cs4 Pred_c4) (lfit Resid_cs4 Pred_c4), yline(0)

// Outliers
scatter Resid_cs4 Pred_c4, mlabel(ID_t)

iqr Resid_cs4