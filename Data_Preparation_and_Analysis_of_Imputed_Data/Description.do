/************************************************************************
Hannah Luisa Reitz
University of Mannheim


Master Thesis "Classroom Ethnic Composition, Educational Achievement, and School Satisfaction"


File: Description of variables
*************************************************************************/
*net install nepstools, from(http://nocrypt.neps-data.de/stata)

clear
version 16
set more off
capture log close 


global datverz C:\Users...  // Global for data 
global arbverz C:\Users... // Global for working directory

/************************************************************************
Load master.dta
*************************************************************************/

use $arbverz/master_final.dta, clear


/************************************************************************
Dropping Classes with less than 10 Students and Students with Missing on Migration Background
*************************************************************************/

// Recoding all missings
mvencode _all, mv(-99)	
mvdecode _all, mv(-99 = .)



// Dropping classes with less than 10 students
count // 15,239

sort ID_cc
distinct ID_cc

bysort ID_cc: gen ID_cc_freq = _N
drop if ID_cc_freq < 10

count // 13,753

// Dropping students with missing on migration background
drop if mig == .

count // 13,101
distinct ID_cc
distinct ID_i

// Keeping relevant variables
keep ID_t ID_cc ID_i sat math read cog ses mig generation female age edu attedu lang school


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



/************************************************************************
Descriptive Statistics
*************************************************************************/

// Total sample
tab edu
tab attedu
tab female
tab lang
tab school
tabstat sat math ses cog age meanses percmig meancog, statistics(mean sd min max) columns(statistics) 



// Separate for majority and minority students
bys mig: tab edu
bys mig: tab attedu
bys mig: tab female
bys mig: tab lang
bys mig: tab school
tab generation


bys mig: tabstat sat math ses cog age meanses percmig meancog, statistics(mean sd min max) columns(statistics) 

count
bys mig: count

distinct ID_cc
bys mig: distinct ID_cc



