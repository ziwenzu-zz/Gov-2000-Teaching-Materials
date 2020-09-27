********************************************************
********************************************************
** Gov 1000/2000e Section 7
** 10-17-13
** Soledad Artiz Prillaman
********************************************************
********************************************************

set more off

* Clear our workspace and set our working directory
cd "/Users/Sole/Dropbox/Gov 2000/2013/Sections/Section 7/Code"

* Read in data, since this is a .csv file, we use insheet, comma
* Data is on log weekely earnings (luwe), education, etc.
insheet using "nls_stata.csv", comma clear


* What does our data look like?
br
* Each column is a variable and each row is an observation
* so for each person, we have data on every variable
* this is why we subscript things by i in our formulas


*******************************
*** Multivariate Regression ***
*******************************

* First let's look at a single variable regression
* we will regress log earnings on IQ
* Y = b0 + b1x1 + e
reg luwe iq

* What happens if we add another variable, say education?
* Y = b0 + b1x1 + b2x2 + e
reg luwe iq educ

* Notice that our coefficient on educ changed
* Why?
cor iq educ
* because education and IQ are positively correlated 

* So what does this tell us about multivariate regression?
* Just like with simple regression, we are minimizing the squared residuals


*******************************
** How can we visualize this in two dimensions?

* generate regression lines 
reg luwe iq educ

* educ only takes on values from 9 to 18
* so what is the line when education is 9?
gen line_9 = (_b[_cons] + _b[educ]*9) + _b[iq]*iq
* 12?
gen line_12 = (_b[_cons] + _b[educ]*12) + _b[iq]*iq
*18?
gen line_18 = (_b[_cons] + _b[educ]*18) + _b[iq]*iq

* plot regression lines 3

twoway (scatter luwe iq, msize(vsmall)) /*
*/ (line line_9 iq, lcolor(orange_red)) /*
*/ (line line_12 iq, lcolor(blue)) /*
*/ (line line_18 iq, lcolor(lavender)), /*
*/ ytitle(Log Weekly Earnings) xtitle(IQ) /*
*/ legend(order(1 "" 3 "educ = 12" 2 "educ = 9" 4 "educ = 18"))




******** QUIZ *********
* So haven't we done all of this before?
* Earlier we assumed we had the POPULATION, not a sample

* What does this change when we do regression or simply use the reg function?























* When we had the population data, the betas we calculated were the true population beta values
* Now that we are sampling, our betas are estimators of the true value!

* What is our estimand, estimator and estimate?





* For example, let's assume that the entire data set is the population
* In reality, the data we have is just a sample from the true, unkown population

* Let's then look at our population model again
reg luwe iq educ

* Now let's take a sample of 250 from the population
set seed 12345
clear matrix
set matsize 11000
	
preserve 
*take a sample of 250
quietly sample 250, count
	
*compute our regression model for this sample
reg luwe iq educ
* They are close, but not exactly the same as when we used the entire dataset
	
*Save our coefficients
mat betas = (nullmat(betas) \ e(b))
restore


* Our betas will now change depending upon which sample we draw
* i.e. they will have sampling distributions
* which means that they have uncertainty around them
* which means that they are not constants
* which means that they have a variance! (or standard error)
* remember our true population values are constants and therefore have var = 0


***********************
*** Standard Errors ***
***********************

* This idea of sampling and uncertainty around our estimators is important

* How much uncertainty do we have?
* Remember that Var(beta) = sigma^2/ sum(x-xbar)
* But we don't know sigma!
* So we don't know how much uncertainty we have!

* How then do we estimate our uncertainty?
* 1. Use the estimated standard error and assume that if n is large enough this is ok
      * We can get this by pulling out the variance-covariance matrix 
	  *   for our coefficients using the e(V) command
	  * e(V) must be used after reg - it pulls the estimates from the most recent regression
	  reg luwe iq educ
	  matrix list e(V)
      * the diagonal elements are the variances of our coefficients
      * the off-diagonal elements are the covariances
	  * note some spaces are empty because the matrix is symmetric

      * So what are our estimated standard errors?
	  * First we store the variance-covariance matrix
	  matrix vcov = e(V)
      * Pull out variances and take square root to get standard deviation
      scalar se_b1 = sqrt(vcov[1,1])
	  scalar se_b2 = sqrt(vcov[2,2])
	  scalar se_b0 = sqrt(vcov[3,3])
	  display se_b1 se_b2 se_b0
      * These match what we get from our model output!
	  reg luwe iq educ

* 2. Bootstrap to create a "fake" sampling distribution and get the variance of this
      * see Section 5 and 6 notes

	  
	  
*************************************************
*** Multivariate Regression with Interactions ***
*************************************************

* Sometimes we want to include an interaction term
* What if we think that iq and education interact to affect wages?
* we will regress log earnings on iq and education and their interaction
* Y = b0 + b1x1 + b2x2 + b3x1*x2 + e

* Create our interaction term
gen iq_educ = iq*educ
reg luwe educ iq iq_educ

* But this doesn't give us the effect we really care about
* What is the marginal effect of education on wages CONDITIONAl on IQ?
* b1 + b3*iq!

* What does this look like in two dimensions?

* iq only takes on values from 50 to 145
* so what is the line when IQ is 50?
gen line_50 = (_b[_cons] + _b[iq]*50) + (_b[educ]*educ + _b[iq_educ]*50*educ)
* 100?
gen line_100 = (_b[_cons] + _b[iq]*100) + (_b[educ]*educ + _b[iq_educ]*100*educ)
*145?
gen line_145 = (_b[_cons] + _b[iq]*145) + (_b[educ]*educ + _b[iq_educ]*145*educ)

* plot regression lines 3

twoway (scatter luwe educ, msize(vsmall)) /*
*/ (line line_50 educ, lcolor(orange_red)) /*
*/ (line line_100 educ, lcolor(blue)) /*
*/ (line line_145 educ, lcolor(lavender)), /*
*/ ytitle(Log Weekly Earnings) xtitle(Years of Education) /*
*/ legend(order(1 "" 3 "iq = 100" 2 "iq = 50" 4 "iq = 145"))

* The slopes of the lines look very similar
* and we know that interaction terms are supposed to change the slp[e]
* Maybe the interaction doesn't add anything, i.e. it is not significant
* But this plot doesn't tell us anything about our uncertainty around
*    our marginal effects (the slope of these lines)!


* How do we get our uncertainty for our marginal effect?
* Is it the standard error on the interaction term?
reg luwe educ iq iq_educ
* NO!

* Our marginal effect is beta1 + beta3*z 
* So the uncertainty of this is Var(beta1+beta3*z)



******** QUIZ ********
** How do we get this variance if we only have the variances of our coefficients?
** i.e. we only know var(beta1) and the var(beta3)









































* What can we do with what we have?
* Var(beta1 + beta3z)
* = var(beta1) + var(beta3*z) + 2*cov(beta1,beta3*z)
* = var(beta1) + z^2*var(beta3) + 2*z*cov(beta1,beta3)

* We can calculate this!


******************************************
*** Marginal Effects with Interactions ***
******************************************

* So what is our estimated marginal effect if iq is 100?
reg luwe educ iq iq_educ
scalar me_100 = _b[educ] + _b[iq_educ]*100
display me_100

* What is our estimated standard error for this effect?
* remember we can get our variances from the vcov matrix
matrix list e(V)
matrix vcov = e(V)
      * Pull out variances and take square root to get standard deviation
      scalar V_b1 = vcov[1,1]
	  scalar V_b3 = vcov[3,3]
	  scalar cov_b1_b3 = vcov[3,1]
	  display V_b1 V_b3 cov_b1_b3
	  
* so our estimated standard error is:
scalar se_me_100 = sqrt(V_b1 + 100^2*V_b3 + 2*100*cov_b1_b3)
display se_me_100

* Now we can do hypothesis testing! We have everything we need.
* H0: marginal effect | (iq = 100) = 0
* HA: marginal effect | (iq = 100) =/= 0
scalar test_statistic = (me_100 - 0) /se_me_100
display test_statistic

scalar p_value = 2*(1 - normal(test_statistic))
display p_value
* we can reject the null hypothesis of no effect when iq is 100


************************************
* MARGINAL EFFECT PLOT

* What if we didn't want to have to do this by hand for each value of iq?
* We could plot the marginal effect (linear function) and it's 95% confidence intervals
reg luwe educ iq iq_educ

* let's create a ruler variable with possible all values of IQ
* this assigns to our new variable rule, the value of the observation (_n)
generate ruler = (_n) if _n>49 & _n<146
br
sum ruler

* We have already stored the variances and covariances we need
* Now let's store the coefficients
matrix betas = e(b)
matrix list betas
scalar b1 = betas[1,1]
scalar b3 = betas[1,3]
display b1 b3

* Let's create the function for our marginal effect line
* Remember our marginal effect is beta1 + beta3*z
gen me = b1+b3*ruler

* Create our function for the standard error of the marginal effect
* Remember our standard error is var(b1) + z^2*var(b3) + 2*z*cov(b1,b3)
gen se = sqrt(V_b1 + (ruler^2)*V_b3 + 2*ruler*cov_b1_b3)

* Create the upper and lower bound of our confidence interval for the marginal effect
* this is a function of the marginal effect and it's standard error
* upper bound = marginal effect + 1.96*se of marginal effect
gen upper = me + 1.96*se
gen lower = me - 1.96*se

graph twoway (line me ruler, clwidth(thick) clcolor(red)) /*
*/ (line upper ruler, clpattern(shortdash) clwidth(medium) clcolor(gs8)) /*
*/ (line lower ruler, clpattern(shortdash) clwidth(medium) clcolor(gs8)), /*
*/ xtitle("IQ", size(4.5) height(4) color(black)) /*
*/ ytitle("Marginal Effect of Education on Earnings", size(4.5) height(4) color(black)) /*
** Add a line at 0
*/ yline(0, lcolor(gs10))  /*
*/ legend(off)

** What does this tell us? 
* Each y-value is a marginal effect (slope) conditional on the x-value (value of IQ)
* The confidence bounds do not contain 0 at any point
* So our marginal effects are statistically significant at any point, even though they are small


** Note: you can get estimates for the standard error also by 
* 1. bootstrapping a sampling distribution for the marginal effect
* 2. Recenter regression as in Professor Glynn's slides




***************
*** F-Tests ***
***************


***************
** F distribution 

** Let's start off with a quick demonstration of what the F
** distribution looks like for some parameters:

** df1 is the number of restrictions or betas you are testing (k)
**       i.e. the number of betas - 1
** df2 = n-(k+1) like with the t-distribution
**       i.e the number of observations - number betas
** if we have 4 restrictions and 100 degrees of freedom
** and an F-test statistic of 4.56, then our probability is:
di Ftail(4,100,4.56)

** The F distribution is NOT two sided, so no need to multiply by 2


***********************
** F test for all coefficients

** A special case of the F-test that is often used is the "omnibus" test
** of all of the variables in the regression. Stata (and most other
** software packages) gives you this in the regression output:

* let's return to our original mutivariate regression
reg luwe iq educ
* you can see that R gives you an F-statistic = 77.5

** note that df1 = 2 (two coefficients)
** and df2 = n-(k+1) -- > 929 - 2 - 1

** What is the probability that we would see a test statistic this large?
di Ftail(2,926,77.5)
** Very small!



** We can also calculate the F-statistic "manually."
** We know from lecture that 

* F test stat = (R^2/k)/((1-R^2)/(n-k-1))

* Pull out R-squared from regression summary
scalar r2 = e(r2)
display r2

* k is the number of betas - 1 (our df1)
* n is the number of observations

scalar f_test_statistic = (r2/2)/((1-r2)/(929-2-1))
display f_test_statistic

** Ta da!














