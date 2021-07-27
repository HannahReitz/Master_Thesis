/************************************************************************
Hannah Luisa Reitz
University of Mannheim

Master Thesis "Classroom Ethnic Composition, Educational Achievement, and School Satisfaction"

File: Preparing data for imputation in Blimp and exporting csv files
*************************************************************************/

clear
version 16
set more off 
capture log close 

global arbverz C:\Users... // Global for working directory


use $arbverz/blimp.dta, clear 



/************************************************************************
Majority Students: DV Math Achievement
*************************************************************************
*************************************************************************
*************************************************************************/

count

keep ID_cc ID_t sat math cog ses edu mig female age attedu lang generation school meanses meancog percmig


/************************************************************************
Dropping Observations with no ID_cc (Cluster Variable) or Missings on mig
*************************************************************************/

mdesc ID_cc
drop if ID_cc == .l // No missings

mdesc mig
drop if mig == .

count


/************************************************************************
Dropping Minority Students
*************************************************************************/

keep if mig == 0

mdesc math

	
/************************************************************************
Define Missings for Blimp
*************************************************************************/

mvencode _all, mv(999999)	
misstable sum _all



keep ID_cc ID_t sat math cog ses edu female age attedu school  percmig meanses meancog
order ID_t ID_cc math cog sat ses edu female age attedu school percmig meanses meancog

save $arbverz/blimp_nat_m.dta, replace 

// Export csv for Blimp
export delimited using $arbverz/blimp_nat_m.csv, novarnames nolabel replace





/************************************************************************
Minority Students: DV Math Achievement
*************************************************************************
*************************************************************************
*************************************************************************/

use $arbverz/blimp.dta, clear 

keep ID_cc ID_t sat math cog ses edu mig female age attedu lang generation school meanses meancog percmig


/************************************************************************
Dropping Observations with no ID_cc (Cluster Variable) or Missings on mig
*************************************************************************/

mdesc ID_cc
drop if ID_cc == .l // No missings

mdesc mig
drop if mig == .


/************************************************************************
Define Missings for Blimp
*************************************************************************/

mvencode _all, mv(999999)	
misstable sum _all


/************************************************************************
Dropping Majority Students
*************************************************************************/

keep if mig == 1



// Rename and shorten some variables, 8 letters allowed in Blimp
ren generation miggen

keep ID_cc ID_t sat math cog ses edu female age attedu lang miggen school meanses meancog percmig
order ID_t ID_cc math cog sat ses edu female age attedu miggen lang school percmig meanses meancog


save $arbverz/blimp_mig_m.dta, replace 

// Export csv for Blimp
export delimited using $arbverz/blimp_mig_m.csv, novarnames nolabel replace







/************************************************************************
Majority Students: DV School Satisfaction
*************************************************************************
*************************************************************************
*************************************************************************/

use $arbverz/blimp.dta, clear 

keep ID_cc ID_t sat cog ses edu mig female age attedu lang generation school meanses meancog percmig


/************************************************************************
Dropping Observations with Missings on mig
*************************************************************************/

mdesc mig
drop if mig == .


/************************************************************************
Dropping Minority Students
*************************************************************************/

keep if mig == 0

	
/************************************************************************
Define Missings for Blimp
*************************************************************************/

mvencode _all, mv(999999)	
misstable sum _all



keep ID_cc ID_t sat cog ses edu female age attedu school meanses meancog percmig
order ID_t ID_cc sat cog ses edu female age attedu school percmig meanses meancog

save $arbverz/blimp_nat_s.dta, replace 

// Export csv for Blimp
export delimited using $arbverz/blimp_nat_s.csv, novarnames nolabel replace







/************************************************************************
Minority Students: DV School Satisfaction
*************************************************************************
*************************************************************************
*************************************************************************/

use $arbverz/blimp.dta, clear 


/************************************************************************
Recoding
*************************************************************************/

count

keep  ID_cc ID_t sat cog ses edu mig female age attedu lang generation school meanses meancog percmig


/************************************************************************
Dropping Observations with no ID_cc (Cluster Variable) or Missings on mig
*************************************************************************/

mdesc mig
drop if mig == .

mdesc sat


/************************************************************************
Missings Definition
*************************************************************************/

mvencode _all, mv(999999)	
misstable sum _all


/************************************************************************
Dropping Majority Students
*************************************************************************/

keep if mig == 1



// Rename and shorten some variables, 8 letters allowed in Blimp
ren generation miggen

keep ID_cc ID_t sat cog ses edu female age attedu lang miggen school meanses meancog percmig
order ID_t ID_cc sat cog ses edu female age attedu miggen lang school percmig meanses meancog 

save $arbverz/blimp_mig_s.dta, replace 

// Export csv for Blimp
export delimited using $arbverz/blimp_mig_s.csv, novarnames nolabel replace













