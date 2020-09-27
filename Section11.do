********************************************************
********************************************************
** Gov 1000/2000e Section 11
** 11-14-13
** Soledad Artiz Prillaman
********************************************************
********************************************************

set more off

* Clear our workspace and set our working directory
clear
clear matrix
cd "/Users/Sole/Dropbox/Gov 2000/2013/Sections/Section 11/Code/"


***********************************
** ESTIMATION: CASAUAL INFERENCE **
***********************************

** To explore estimation within the framework of causal inference
** we are going to use a toy example regarding unemployment insurance.
** In our experiment, we want to know what the effect of providing
** information about UI has on the likelihood of applying for benefits.

** TREATMENT: 1 if received information, 0 otherwise
** OUTCOME (benefits): On a scale of 1-5, how likely a person is to apply for UI
**          5 - very likely

* Load our data
use "ui.dta", clear
br


***************************************
** Estimation with Random Assignment **
***************************************

** We want to estimate the ATE on the likelihood of applying for benefits

** We can calculate this directly
** Remembering that ATE = E[Y|T=1] - E[Y|T=0]

* Store our mean of benefits for treated group
sum benefits if treatment==1
scalar mean_treated = r(mean)
* Store our mean of benefits for control group
sum benefits if treatment==0
scalar mean_control = r(mean)
* Estimate ATE
scalar ate = mean_treated - mean_control
display ate


** Alternatively, we can get the ATE using simple regression
** All we have to do is regress benefits on treatment
** We don't need any covariates - why?
** Exchangeability! - as a result of random treatment assignment
reg benefits treatment
* our coefficient on treatment is the same as we found above!
* And, now we have an estimate of our standard error


* What happens if we add a covariate?
* Say we believe that type (whatever that is) might be important
reg benefits treatment type
* We still get the same answer
* Why? - because of exchangeability!




***************************************************
** Estimation with Conditional Random Assignment **
***************************************************

** Instead of executing the unemployment insurance with strict
** random assignment, the experimenters decided to condition treatment
** assignment on employment status
** employment = 1 if employed, 0 if unemployed


** Stratum-specific ATEs

** We can calculate thiese directly
** Remembering that stratum-specific ATE = E[Y|T=1, X=x] - E[Y|T=0, X=x]

*EMPLOYED
* Let's start with those who are employed
* Store our mean of benefits for treated group
sum benefits if treatment==1 & employment==1
scalar mean_emp_treated = r(mean)
* Store our mean of benefits for control group
sum benefits if treatment==0 & employment==1
scalar mean_emp_control = r(mean)
* Estimate ATE
scalar ate_emp = mean_emp_treated - mean_emp_control
display ate_emp

*UNEMPLOYED
* Now let's calculate this for the unemployed
* Store our mean of benefits for treated group
sum benefits if treatment==1 & employment==0
scalar mean_unemp_treated = r(mean)
* Store our mean of benefits for control group
sum benefits if treatment==0 & employment==0
scalar mean_unemp_control = r(mean)
* Estimate ATE
scalar ate_unemp = mean_unemp_treated - mean_unemp_control
display ate_unemp



** Alternatively, we can get the stratum specific ATEs using interactive regression
** All we have to do is regress benefits on treatment, employment, and the interaction
** We don't need any covariates - why?
** Conditional exchangeability! - as a result of conditional
** random treatment assignment

* Create our interaction term
gen treat_emp = treatment*employment
* Run interactive regression
reg benefits treatment employment treat_emp
* our coefficient on treatment is the ATE for unemployed
* To get an estimate of the ATE for the employed
* We must add beta1 and beta3
display _b[treatment] + _b[treat_emp]
* And, now we can calculate standard errors for these estimates!
* Go back to the weeks on interactive models to understand how we do this



*********************
** Standardization **
*********************

** But what if we want an estimate for the ATE, not the stratum-specific
** ATEs, when we have a conditional random experiment?

** We can use standardization
** This weights the stratum-specific ATEs by the probability
** of being in that strata


** We can calculate this directly
* First we find the probability of being employed
sum employment
scalar prob_emp = r(mean)
display prob_emp
* and then we find the probability of being unemployed
scalar prob_unemp = 1 - prob_emp
display prob_unemp

* Using this and the stratum-specific ates we found before
* we can get an estimate of our ATE
scalar ate_stand = ate_emp*prob_emp + ate_unemp*prob_unemp
display ate_stand


** Alternatively, we can get the stanardization ATE
** using interactive regression
** Remember the standardized ATE = beta1 + beta3* mean(X)
** where beta1 and beta3 come from the interaction model we
** calculated above

*Store the mean of employment
sum employment
scalar mean_emp = r(mean)
* Plug into marginal effect equation
scalar lm_ate_stand_1 = _b[treatment] + _b[treat_emp]*mean_emp
display lm_ate_stand_1

* Or we can demean employment and then the coefficient on beta1
* will be our standardized ATE
* Create a new demeaned variable for employment
gen employ_demean = employment - mean_emp
* Create a new interaction term with treatment
gen treat_emp2 = treatment*employ_demean
* Then we run the interaction model using this new variable
reg benefits treatment employ_demean treat_emp2
** ta-da!

