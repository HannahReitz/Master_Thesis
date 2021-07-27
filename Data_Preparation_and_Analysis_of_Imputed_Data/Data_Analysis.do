/************************************************************************
Hannah Luisa Reitz
University of Mannheim

Master Thesis "Classroom Ethnic Composition, Educational Achievement, and School Satisfaction"

File: Analysis of imputed data
*************************************************************************/

clear
version 16
set more off
capture log close

global arbverz C:\Users... // Global for working directory



/************************************************************************
School Satisfaction
*************************************************************************
*************************************************************************
*************************************************************************/


/************************************************************************
Importing csv Imputation Files
*************************************************************************/

// Data for majority students
import delimited imp ID_t ID_cc sat cog ses edu female age attedu school percmig meanses meancog using $arbverz/imps_nat_s.csv, clear 

save $arbverz/imps_nat_s.dta, replace 

// Data for minority students
import delimited imp ID_t ID_cc sat cog ses edu female age attedu miggen lang school percmig meanses meancog using $arbverz/imps_mig_s.csv, clear 

save $arbverz/imps_mig_s.dta, replace


/************************************************************************
Appending Minority Data to Majority Data
*************************************************************************/

use $arbverz/imps_nat_s.dta, clear 

append using $arbverz/imps_mig_s.dta

save $arbverz/imps_s.dta, replace 
use $arbverz/imps_s.dta, clear


/************************************************************************
Recoding all Missings to .
*************************************************************************/

mvdecode _all, mv(999999 = .)	
sort imp ID_t

save $arbverz/imps_s.dta, replace 
use $arbverz/imps_s.dta, clear

sort imp ID_cc


/************************************************************************
Generating new Class Means with all Observations in Imputations
*************************************************************************/

// Mean SES
drop meanses
bys imp ID_cc: egen meanses = mean(ses)
mdesc meanses

// Mean cognitive ability
drop meancog
bys imp ID_cc: egen meancog = mean(cog)
mdesc meancog

// Percmig had no missings, no need to calculate it again based on values in imputation


/************************************************************************
Centering Predictors
*************************************************************************/
/*------------------------------------------------------------------------
Grand Mean Centering Level 2 Variables
------------------------------------------------------------------------*/

// Mean SES
egen meanses_gm = mean(meanses), by(imp)
gen meanses_c = meanses - meanses_gm
drop meanses_gm

// Mean cognitive ability
egen meancog_gm = mean(meancog), by(imp)
gen meancog_c = meancog - meancog_gm
drop meancog_gm

// Percentage of minority students
egen percmig_gm = mean(percmig), by(imp)
gen percmig_c = percmig - percmig_gm
drop percmig_gm


/*------------------------------------------------------------------------
Grand Mean Centering Level 1 Variables
------------------------------------------------------------------------*/

// SES
egen ses_gm = mean(ses), by(imp)
gen ses_c = ses - ses_gm
drop ses_gm
sort imp ID_cc

// Cognitive ability
egen cog_gm = mean(cog), by(imp)
gen cog_c = cog - cog_gm
sort imp ID_cc
drop cog_gm

// Age
egen age_gm = mean(age), by(imp)
gen age_c = age - age_gm
drop age_gm




/************************************************************************
Importing Data Set as Imputed
*************************************************************************/

mi import flong, m(imp) id(ID_t) imputed(sat - age_c) clear

sort imp ID_cc

mi describe
mi varying


/************************************************************************
Generating new Migration Background Variable for Separate Regressions 
*************************************************************************/

gen mig = .
replace mig = 0 if miggen == .
replace mig = 1 if miggen != .

mi varying


/************************************************************************
Diagnostics of Imputations
*************************************************************************/
*midiagplots ses, m(1/5) combine
*midiagplots math, m(1/5) combine
*midiagplots sat, m(1/5) combine
*midiagplots cog, m(1/5) combine
*midiagplots meancog, m(1/5) combine


/************************************************************************
Generating percmig_c squared to detect non-linear relationships
*************************************************************************/

// Excluded in final models as there were no non-linear relationships

*gen percmig2_c = percmig_c*percmig_c



/************************************************************************
Multilevel Regressions: School Satisfaction
*************************************************************************/
/*------------------------------------------------------------------------
Majority Students
------------------------------------------------------------------------*/

mi estimate, post dots: mixed sat if mig == 0 || ID_cc:, mle
estimates store M1
// Calculating ICC by hand
di  (.4088147^2)/(.4088147^2 + 2.219905^2) // ICC = 0.03280195

mi estimate, post dots: mixed sat ses_c i.edu i.attedu cog_c i.female age_c if mig == 0 || ID_cc:,
estimates store M2

mi estimate, post dots: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c if mig == 0 || ID_cc:,
estimates store M3

mi estimate, post dots: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c meanses_c if mig == 0 || ID_cc:,
estimates store M4

mi estimate, post dots: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.school  percmig_c meanses_c meancog_c if mig == 0 || ID_cc:,
estimates store M5


// Exporting regression table
estimates table M1 M2 M3 M4 M5, star 

esttab M2 M3 M4 M5, b(3) se(3) se aic obslast, using sat_nat.tex, replace



/*------------------------------------------------------------------------
Minority Students
------------------------------------------------------------------------*/

mi estimate, post dots: mixed sat if mig == 1 || ID_cc:, mle
estimates store M6
// Calculating ICC by hand
di  (.4062329^2)/(.4062329^2 + 2.352316^2) // ICC = .02895983

mi estimate, post dots: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang if mig == 1 || ID_cc:,
estimates store M7

mi estimate, post dots: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c  if mig == 1 || ID_cc:,
estimates store M8

mi estimate, post dots: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c meanses_c if mig == 1 || ID_cc:,
estimates store M9


mi estimate, post dots: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c meanses_c meancog_c if mig == 1 || ID_cc:,
estimates store M10


// Exporting regression table
estimates table M6 M7 M8 M9 M10, star 

esttab M7 M8 M9 M10, b(3) se(3) se aic obslast, using sat_mig.tex, replace





/************************************************************************
Obtaining -2LL for Models 
*************************************************************************/
/*------------------------------------------------------------------------
Majority Students
------------------------------------------------------------------------*/

// M2
scalar ll = 0
mi xeq 1/20: mixed sat ses_c i.edu i.attedu cog_c i.female age_c if mig == 0 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M3
scalar ll = 0
mi xeq 1/20: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c if mig == 0 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M4
scalar ll = 0
mi xeq 1/20: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c meanses_c if mig == 0 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M5
scalar ll = 0
mi xeq 1/20: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c meanses_c meancog_c if mig == 0 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll



/*------------------------------------------------------------------------
Minority Students
------------------------------------------------------------------------*/

// M7
scalar ll = 0
mi xeq 1/20: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M8
scalar ll = 0
mi xeq 1/20: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M9
scalar ll = 0
mi xeq 1/20: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c meanses_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M10
scalar ll = 0
mi xeq 1/20: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c meanses_c meancog_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll






/************************************************************************
Math Achievement
*************************************************************************/
*************************************************************************
*************************************************************************


/************************************************************************
Importing csv Imputation Files
*************************************************************************/
clear 

// Data for majority students
import delimited imp ID_t ID_cc math cog sat ses edu female age attedu school percmig meanses meancog using $arbverz/imps_nat_m.csv, clear 

save $arbverz/imps_nat_m.dta, replace 

// Data for minority students
import delimited imp ID_t ID_cc math cog sat ses edu female age attedu miggen lang school percmig meanses meancog using $arbverz/imps_mig_m.csv, clear 

save $arbverz/imps_mig_m.dta, replace


/************************************************************************
Appending Migrant Data to Native Data
*************************************************************************/

use $arbverz/imps_nat_m.dta, clear 

append using $arbverz/imps_mig_m.dta

save $arbverz/imps_m.dta, replace 
use $arbverz/imps_m.dta, clear


/************************************************************************
Recoding all Missings to .
*************************************************************************/

mvdecode _all, mv(999999 = .)	
sort imp ID_t

save $arbverz/imps_m.dta, replace 
use $arbverz/imps_m.dta, clear

sort imp ID_cc


/************************************************************************
Generating new Class Means with all Observations in Imputations
*************************************************************************/

// Mean SES
drop meanses
bys imp ID_cc: egen meanses = mean(ses)
mdesc meanses

// Mean cognitive ability
drop meancog
bys imp ID_cc: egen meancog = mean(cog)
mdesc meancog

// percmig had no missings, no need to calculate it again based on values in imputation


/************************************************************************
Centering Predictors
*************************************************************************/

/*------------------------------------------------------------------------
Grand Mean Centering Level 2 Variables
------------------------------------------------------------------------*/

// Mean SES
egen meanses_gm = mean(meanses), by(imp)
gen meanses_c = meanses - meanses_gm
drop meanses_gm

// Mean cognitive ability
egen meancog_gm = mean(meancog), by(imp)
gen meancog_c = meancog - meancog_gm
drop meancog_gm

// Percentag minority students
egen percmig_gm = mean(percmig), by(imp)
gen percmig_c = percmig - percmig_gm
drop percmig_gm


/*------------------------------------------------------------------------
Grand Mean Centering Level 1 Variables
------------------------------------------------------------------------*/

// SES
egen ses_gm = mean(ses), by(imp)
gen ses_c = ses - ses_gm
drop ses_gm
sort imp ID_cc

// Cognitive ability
egen cog_gm = mean(cog), by(imp)
gen cog_c = cog - cog_gm
sort imp ID_cc
drop cog_gm

// Age
egen age_gm = mean(age), by(imp)
gen age_c = age - age_gm
drop age_gm

// School satisfaction
egen sat_gm = mean(sat), by(imp)
gen sat_c = sat - sat_gm
drop sat_gm 


/************************************************************************
Importing Data Set as Imputed
*************************************************************************/

mi import flong, m(imp) id(ID_t) imputed(math - sat_c) clear

sort imp ID_cc

mi describe
mi varying


/************************************************************************
Generating new Migration Background Variable for Separate Regressions 
*************************************************************************/

gen mig = .
replace mig = 0 if miggen == .
replace mig = 1 if miggen != .

mi varying


/************************************************************************
Diagnostics of Imputations
*************************************************************************/

*midiagplots ses, m(1/5) combine
*midiagplots math, m(1/5) combine
*midiagplots sat, m(1/5) combine
*midiagplots cog, m(1/5) combine
*midiagplots meancog, m(1/5) combine


/************************************************************************
Generating percmig_c squared to detect non-linear relationships
*************************************************************************/

//excluded in subsequent analysis as there were no non-linear relationships

*gen percmig2_c = percmig_c*percmig_c



/************************************************************************
Multilevel Regressions: Math
*************************************************************************/
/*------------------------------------------------------------------------
Majority Students
------------------------------------------------------------------------*/

mi estimate, post dots: mixed math if mig == 0 || ID_cc:, 
estimates store M10
// Calculating ICC by hand
di  (.813255^2)/(.813255^2 + 0.9227248^2) // ICC = 0.43719039

mi estimate, post dots: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c if mig == 0 || ID_cc:, mle
estimates store M11

mi estimate, post dots: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c if mig == 0 || ID_cc:, mle
estimates store M12

mi estimate, post dots: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c meanses_c  if mig == 0 || ID_cc:, mle
estimates store M13

mi estimate, post dots: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c meanses_c meancog_c if mig == 0 || ID_cc:, mle
estimates store M14

mi estimate, post dots: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.school meanses_c percmig_c meancog_c c.percmig_c#c.ses_c if mig == 0 || ID_cc: ses_c, difficult
estimates store M15


// Exporting regression table
estimates table M10 M11 M12 M13 M14 M15, star 

esttab M11 M12 M13 M14 M15, b(3) se(3) se aic obslast, using math_nat.tex, replace



/*------------------------------------------------------------------------
Minority Students
------------------------------------------------------------------------*/

mi estimate, post dots: mixed math if mig == 1 || ID_cc:, mle
estimates store M16
// Calculating ICC by hand 
di  (.7673159^2)/(.7673159^2 + .8461302^2) // ICC = .45126786

mi estimate, post dots: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang if mig == 1 || ID_cc:, mle
estimates store M17

mi estimate, post dots: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c if mig == 1 || ID_cc:, mle
estimates store M18

mi estimate, post dots: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c meanses_c if mig == 1 || ID_cc:, mle
estimates store M19

mi estimate, post dots: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c meanses_c meancog_c if mig == 1 || ID_cc:, mle
estimates store M20

mi estimate, post dots: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c meanses_c meancog_c c.percmig_c#c.ses_c if mig == 1 || ID_cc: ses_c,
estimates store M21


// Exporting regression table
estimates table M16 M17 M18 M19 M20 M21, star 

esttab M17 M18 M19 M20 M21, b(3) se(3) se aic obslast, using math_mig.tex, replace




/************************************************************************
Obtaining -2LL for Models 
*************************************************************************/
/*------------------------------------------------------------------------
Majority Students
------------------------------------------------------------------------*/

// M11
scalar ll = 0
mi xeq 1/20: mixed math ses_c i.edu i.attedu cog_c i.female age_c if mig == 0 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M12
scalar ll = 0
mi xeq 1/20: mixed math ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c if mig == 0 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M13
scalar ll = 0
mi xeq 1/20: mixed math ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c meanses_c if mig == 0 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M14
scalar ll = 0
mi xeq 1/20: mixed math ses_c i.edu i.attedu cog_c i.female age_c i.school percmig_c meanses_c meancog_c if mig == 0 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M15
scalar ll = 0
mi xeq 1/20: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.school meanses_c percmig_c meancog_c c.percmig_c#c.ses_c if mig == 0 || ID_cc: ses_c, difficult; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll


/*------------------------------------------------------------------------
Minority Students
------------------------------------------------------------------------*/

// M17
scalar ll = 0
mi xeq 1/20: mixed math ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M18
scalar ll = 0
mi xeq 1/20: mixed math ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M19
scalar ll = 0
mi xeq 1/20: mixed math ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c meanses_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M20
scalar ll = 0
mi xeq 1/20: mixed math ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c meanses_c meancog_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

// M21
scalar ll = 0
mi xeq 1/20: mixed math ses_c i.edu i.attedu cog_c i.female age_c i.miggen i.lang i.school percmig_c meanses_c meancog_c c.percmig_c#c.ses_c if mig == 1 || ID_cc: ses_c, difficult; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll











