********************************************************
********************************************************
** Gov 1000/2000e Section 9
** 10-31-13
** Soledad Artiz Prillaman
********************************************************
********************************************************

set more off

* Clear our workspace and set our working directory
clear
clear matrix
cd "/Users/Sole/Dropbox/Gov 2000/2013/Sections/Section 9/Code/"


******************
** Independence **
******************

* why do we care?  In mild forms, dependence among units leads
* to bad estimates of standard errors.  In more severe forms,
* dependence can lead to very badly biased estimates of model
* parameters.  

* Fixed effects models for clustered dependence

* These are fairly straightforward: set a dummy variable for each
* cluster in our data. This is equivalent to setting a separate
* intercept for each cluster.  Note that this approach is fine for
* making inferences about sampled clusters, but it will not be 
* possible to make predictions/inferences about non-sampled 
* clusters.  For this, we might need a random effects model.  

* Let's look at an example using the Robey data
* this data is in the car package
use "Robey.dta"

* what do we know about the data?
br 
* this tells us that we have observations on developing countries
* and we have data on fertility rates (tfr) abd contraceptors 
sum region tfr contraceptors

* Say we wanted to regress fertility on contraceptors
* but we are concerned about dependence across regions

* First, let's see if there's a big difference in the residuals
* the four regions in our study
* we can do this by plotting our residuals from the regression on the region

* Run the regression
reg tfr contraceptor
* Extract the residuals
predict res, residuals
* Plot the residuals by region
* We can do this with a box plot
help graph box
graph box res, over(region)
* There are slight differences across the three regions.  In
* Asia, especially, it looks like there may be some regional confounder that we
* could use if we had more data.  

* we can still use a regional dummy variable to control for this
* and then let's re-look at our residuals

* Re-run regression with region variable
reg tfr contraceptors region
* Extract new residuals
predict res2, residuals
* Replot residuals
graph box res2, over(region)
* these look pretty similar across regions


******************
** Missing Data **
******************

* Now let's think a little bit about nonresponse and missing data

* Load in credit card data with missing values
insheet using "ccarddata_missing.csv", comma clear

* Let's look at the data
br
sum age income ccexpend homeowner

* The NA's in ccexpend represent missing values
* But stata reads these as characters, not numerics
* so we have to convert these to numerics by subbing in . for NA
* and then destring the variable (make it a numeric)
destring ccexpend, replace ignore(NA)
br


*******************

*** Characterize Missingness - which of the assumptions do we believe?

* create a new indicator variable that tells us which observations have 
* a missing value for ccexpend
* Start by creating a variable missing and filling in 0 for ALL observations
gen missing = 0
* Now replace missing with a 1 for observations where there is an NA for ccexpend
replace missing = 1 if ccexpend==.
* this tells us which observations have missing values

* So we have several observations that are missing data for the outcome variable
* and many that are not
* we can look at these data as two different "groups" or even as two different data sets
* Data summary for non-missing observations
sum age income homeowner if missing==0
* Data summary for missing observations
sum age income homeowner if missing==1

* What does this tell us?

* Are these two data sets (one for missing and one for not)
* different in their covariate values?
* to test this we can use a ttest for each covariate (remember there is no missingness in our covariates)
* this is the same as the difference in means
* test that we used earlier in the semester when we had two groups

*******************
** QUIZ BREAK: What was the test for a test of difference in means?
** i.e. H0: mu_missing - mu_nonmissing = 0 















* For each covariate we use the command ttest 
* and specify our covariate and the groups we want to compare (missing)
ttest age, by(missing) unequal
* Below the table, on the right, it gives us our test statistic
* since it assumes mu_nonmissing-mu_missing, our test statistic is actually -t
* Let's store the value
scalar age_ttest = -r(t)
display age_ttest

* Now let's do this for the remaining two covariates
ttest income, by(missing) unequal
scalar income_ttest = -r(t)
ttest homeowner, by(missing) unequal
scalar homeowner_ttest = -r(t)
display age_ttest income_ttest homeowner_ttest

* We can prove that this test statistic is the difference in means/ se(difference in means)
sum age if missing==0, detail
scalar mean_age0 = r(mean)
scalar var_age0 = r(Var)
scalar n_age0 = r(N)
sum age if missing==1, detail
scalar mean_age1 = r(mean)
scalar var_age1 = r(Var)
scalar n_age1 = r(N)
display (mean_age1 - mean_age0)/sqrt(var_age1/n_age1 + var_age0/n_age0)
display age_ttest
* viola, they match! 


* We want to create a balance plot to represent these test statistics
* First lets just create a ruler, which will help us in our plot
* since we have three test statistics, we just need three values
generate ruler = (_n) if _n<4
* Now let's create a vector (variable) of the test statistics
* and match them to their point on the ruler
generate ttests = .
replace ttests = age_ttest if ruler==1
replace ttests = income_ttest if ruler==2
replace ttests = homeowner_ttest if ruler==3

* create balance plot to compare covariate distributions across the two groups
graph twoway (scatter ruler ttests), /*
	*/ xtitle("Standardized Diff. in Means", size(4.5) height(4) color(black)) /*
	*/ ytitle("") /*
	*/ ylabel(1 "age" 2 "income" 3 "homeowner", angle(0)) /*
	** Add a line at 0
	*/ xline(0, lcolor(red))  /*
	*/ legend(off)	

* What does this tell us?
* this tells us that income and age are larger for the observations with missing values
* and homeowner is lower
* but only age is statistically different from 0 (i.e. t.stat > critical value)


* Let's relook at the difference in means for age
display mean_age1 - mean_age0
* This tells us that those who didn't report ccexpend are on average 4.5 years older
* than those that did!


*******************
** QUIZ BREAK: What does this tell us about our missingness mechanism?
* Could our data be MCAR? MAR? NMAR?






















* an alternative is to make a box plot for the two groups (missing and non-missing outcome) for the covariates
graph box age, over(missing)
graph box income, over(missing)
graph box homeowner, over(missing)


********************

* So how can we deal with this missingness?

*** 1. Complete Case Analysis
* delete all observations that have missing values
* this is what Stata automatically does
reg ccexpend income homeowner age

* Let's show that this is the same as if we deleted all of the rows that had an na
preserve
drop if ccexpend==.
reg ccexpend income homeowner age
restore
* Ta-da!


*** 2. Mean Imputation
* fill in missing ccexpend values with mean of ccexpend

* start by saving mean of ccexpend
sum ccexpend
scalar ccexpend_mean = r(mean)
display ccexpend_mean

* Create a new variable that will replace missing values of ccexpend with 
* mean imputed values
gen ccexpendmeanimp = ccexpend
* fill in missing values with mean
replace ccexpendmeanimp = ccexpend_mean if ccexpend==.
sum ccexpendmeanimp


*** 3. Regression Imputation
* fill in missing ccexpend values with predicted value from regression line
* from complete-case analysis

* Start by running complete-case analysis regression
reg ccexpend income homeowner age
* and predict fitted values
predict yhat

* Create a new variable that will replace missing values of ccexpend with 
* regression imputed values
gen ccexpendregimp = ccexpend
* fill in missing values with predicted values
replace ccexpendregimp = yhat if ccexpend==.
sum ccexpendregimp


** Now, let's compare our data from each of these processes
* First plot a histogram of ccexpend for non-missing values (complete case analysis)
hist ccexpend, xtitle("ccexpend") title("Complete Case Analysis") name(his1, replace)
* Second plot a histogram of ccexpend with mean imputed values
hist ccexpendmeanimp, xtitle("ccexpend") title("Mean Imputation") name(his2, replace)
* Third plot a histogram of ccexpend with regression imputed values
hist ccexpendregimp, xtitle("ccexpend") title("Regression Imputation") name(his3, replace)
* Combine into one graph
graph combine his1 his2 his3, rows(1)






************************************
** Sample Replication-ish Problem **
************************************

*Load some data
* this is a lot of data on the 50 US states from 1990
use "statedata.dta", clear
br

* Let's try to estimate the regression model (conditional expectation function)
* of murder given popdensity, police, blackpop and unemp

* murder = murders per 100,000 population
* popdensity = population per square mile
* police = number of police officers
* blackpop = black populations in 1000's
* unemp = unemployment rate as percentage of state's labor force


* Let's fit a basic OLS model
reg murder popdensity police blackpop unemp

************************

* Do we have missingdata on the outcome? Yes!

* Let's create an indicator of missingness
gen missing = 0
replace missing = 1 if murder==.

* Now let's store t-tests for each covariate
ttest popdensity, by(missing) unequal
scalar popdensity_ttest = -r(t)
ttest police, by(missing) unequal
scalar police_ttest = -r(t)
ttest blackpop, by(missing) unequal
scalar blackpop_ttest = -r(t)
ttest unemp, by(missing) unequal
scalar unemp_ttest = -r(t)

* create balance plot to compare covariate distributions across the two groups
generate ruler = (_n) if _n<5
generate ttests = .
replace ttests = popdensity_ttest if ruler==1
replace ttests = police_ttest if ruler==2
replace ttests = blackpop_ttest if ruler==3
replace ttests = unemp_ttest if ruler==4

* create balance plot to compare covariate distributions across the two groups
graph twoway (scatter ruler ttests), /*
	*/ xtitle("Standardized Diff. in Means", size(4.5) height(4) color(black)) /*
	*/ ytitle("") /*
	*/ ylabel(1 "popdensity" 2 "police" 3 "blackpop" 4 "unemp", angle(0)) /*
	*/ xlabel(-2 -1 0 1 2) /*
	** Add a line at 0
	*/ xline(0, lcolor(red))  /*
	*/ legend(off)	

* We have pretty good balance on everything but popdensity - maybe it is not MCAR
* We should try to fill in this missingness

* Let's use regression imputation
reg murder popdensity police blackpop unemp
predict yhat
* create imputed variable
gen murderimp = murder
replace murderimp = yhat if murder==.


** Let's rerun our model
reg murderimp popdensity police blackpop unemp



************************

* Do our assumptions hold?

* 1. Linearity:

cprplot popdensity, lowess msymbol(circle_hollow) mcolor("yellow") name(cr1, replace)
cprplot police, lowess msymbol(circle_hollow) mcolor("yellow") name(cr2, replace)
cprplot blackpop, lowess msymbol(circle_hollow) mcolor("yellow") name(cr3, replace)
cprplot unemp, lowess msymbol(circle_hollow) mcolor("yellow") name(cr4, replace)
graph combine cr1 cr2 cr3 cr4, rows(2)


* As the scatter plots suggested, popdensity, police and blackpop
* may have concern regarding non-linearities
* Let's try logging these popdensity, police and blackpop to deal with this
gen logpopdensity = log(popdensity + 1)
* note that we add 1 because the log(0) is undefined
gen logpolice = log(police)
gen logblackpop = log(blackpop)

* Now let's rerun our model with these new terms
reg murderimp logpopdensity logpolice logblackpop unemp

cprplot logpopdensity, lowess msymbol(circle_hollow) mcolor("yellow") name(cr1, replace)
cprplot logpolice, lowess msymbol(circle_hollow) mcolor("yellow") name(cr2, replace)
cprplot logblackpop, lowess msymbol(circle_hollow) mcolor("yellow") name(cr3, replace)
cprplot unemp, lowess msymbol(circle_hollow) mcolor("yellow") name(cr4, replace)
graph combine cr1 cr2 cr3 cr4, rows(2)

* Gah! Now linearity looks worse.  Let's keep tinkering.  
* Try including polynomials 

* Square the unemployment variable and the logpolice variable:
gen unempsq = unemp^2
gen policesq = police^2

reg murderimp logpopdensity logpolice logblackpop unemp unempsq policesq

cprplot logpopdensity, lowess msymbol(circle_hollow) mcolor("yellow") name(cr1, replace)
cprplot logpolice, lowess msymbol(circle_hollow) mcolor("yellow") name(cr2, replace)
cprplot logblackpop, lowess msymbol(circle_hollow) mcolor("yellow") name(cr3, replace)
cprplot unemp, lowess msymbol(circle_hollow) mcolor("yellow") name(cr4, replace)
cprplot unempsq, lowess msymbol(circle_hollow) mcolor("yellow") name(cr5, replace)
cprplot policesq, lowess msymbol(circle_hollow) mcolor("yellow") name(cr6, replace)
graph combine cr1 cr2 cr3 cr4 cr5 cr6, rows(3)

* much better!


************************

* 2. Heteroskedasticity:

* Spread Location plot
predict res, residuals
predict yhat2, xb
gen res2 = sqrt(abs(res))
twoway (scatter res2 yhat2, msymbol(circle_hollow) mcolor("yellow")) (lowess res2 yhat2)

* not so bad

************************

* 3. Normality:

* QQ plot
qnorm res

* not too bad






