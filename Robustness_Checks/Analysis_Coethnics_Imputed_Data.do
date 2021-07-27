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


/************************************************************************
 Appending Imputation Datasets from Ethnic Majority and Minority
*************************************************************************/
use imp_nat.dta, clear

mi append using imp_mig.dta



/************************************************************************
Dropping and Newly Generating Class Means
*************************************************************************/

drop meanses
drop meancog

mi update

mi convert flong, clear

// Mean SES
mi xeq: bys ID_cc: egen meanses = mean(ses)

// Mean cog
mi xeq: bys ID_cc: egen meancog = mean(cog)

mi varying


/************************************************************************
Centering Variables
*************************************************************************/

/*------------------------------------------------------------------------
Grand Mean Centering Level 2 Variables
------------------------------------------------------------------------*/

sort ID_cc

// meanses
mi xeq: egen meanses_gm = mean(meanses)
mi xeq: generate meanses_c = meanses - meanses_gm
drop meanses_gm
sum meanses_c, d

// meancog
mi xeq: egen meancog_gm = mean(meancog)
mi xeq: generate meancog_c = meancog - meancog_gm
*tabstat avgmath5_gm avgmath5_c, statistics( mean sd ) 
drop meancog_gm

// percmig
mi xeq: egen percmig_gm = mean(percmig)
mi xeq: generate percmig_c = percmig - percmig_gm
drop percmig_gm



/*------------------------------------------------------------------------
Grand Mean Centering Level 1 Variables
------------------------------------------------------------------------*/

// ses
mi xeq: egen ses_gm = mean(ses)
mi xeq: generate ses_c = ses - ses_gm
*bys ID_cc: tabstat ses_cm ses_c, statistics( mean sd ) 
drop ses_gm

mi varying

// sat
mi xeq: egen sat_gm = mean(sat)
mi xeq: generate sat_c = sat - sat_gm
drop sat_gm


// age
mi xeq: egen age_gm = mean(age)
mi xeq: generate age_c = age - age_gm
drop age_gm

// cog
mi xeq: egen cog_gm = mean(cog)
mi xeq: generate cog_c = cog - cog_gm
*bys ID_cc: tabstat math5_cm math5_c, statistics( mean sd ) 
drop cog_gm


// coeth
mi xeq: egen perccoeth_gm = mean(perccoeth)
mi xeq: generate perccoeth_c = perccoeth - perccoeth_gm
drop perccoeth_gm


mi varying
mi describe

sort ID_cc
fre mig


/************************************************************************
Multilevel Regressions: Satisfaction
*************************************************************************/

/*------------------------------------------------------------------------
Minority Students
------------------------------------------------------------------------*/

mi estimate, post: mixed sat if mig == 1 || ID_cc:, mle
estimates store M1
di (.4060252^2) / (.4060252^2 + 2.352494^2)
// ICC =  0.2892

mi estimate, post: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c if mig == 1 || ID_cc:,
estimates store M2

mi estimate, post: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c i.school percmig_c if mig == 1 || ID_cc:,
estimates store M3

mi estimate, post: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c i.school percmig_c meanses_c if mig == 1 || ID_cc:,
estimates store M4

mi estimate, post: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c i.school percmig_c meanses_c meancog_c if mig == 1 || ID_cc:,
estimates store M5


estimates table M2 M3 M4 M5, star 


/*------------------------------------------------------------------------
Obtaining -2LL for Models: School Satisfaction
------------------------------------------------------------------------*/

scalar ll = 0
mi xeq 1/20: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

scalar ll = 0
mi xeq 1/20: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c i.school percmig_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll


scalar ll = 0
mi xeq 1/20: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c i.school percmig_c meanses_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll


scalar ll = 0
mi xeq 1/20: mixed sat ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c i.school percmig_c meanses_c meancog_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll





/************************************************************************
Multilevel Regressions: Math
*************************************************************************/

/*------------------------------------------------------------------------
Minority Students
------------------------------------------------------------------------*/

*Null model for IV math: Obtaining maximum likelihood (ML) estimates
mi estimate, post: mixed math if mig == 1|| ID_cc:, mle
estimates store M6
di (.7673309^2) / (.7673309^2 + .8460899^2)
// ICC =  0.4513

mi estimate, post: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c if mig == 1 || ID_cc:, mle
estimates store M7


mi estimate, post: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c i.school percmig_c if mig == 1 || ID_cc:, mle
estimates store M8


mi estimate, post: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c i.school percmig_c meanses_c if mig == 1 || ID_cc:, mle
estimates store M9


mi estimate, post: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c i.school percmig_c meanses_c meancog_c if mig == 1 || ID_cc:, mle
estimates store M10




estimates table M7 M8 M9 M10, star 



/*------------------------------------------------------------------------
Obtaining -2LL for Models: Math Competence
------------------------------------------------------------------------*/

scalar ll = 0
mi xeq 1/20: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll


scalar ll = 0
mi xeq 1/20: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c i.school percmig_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll


scalar ll = 0
mi xeq 1/20: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c i.school percmig_c meanses_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll


scalar ll = 0
mi xeq 1/20: mixed math sat_c ses_c i.edu i.attedu cog_c i.female age_c i.generation i.lang perccoeth_c i.school percmig_c meanses_c meancog_c if mig == 1 || ID_cc:,; scalar ll = ll + (-2*e(ll))
scalar ll = ll/20
di as txt "-2LL over imputed data = " as res ll

















