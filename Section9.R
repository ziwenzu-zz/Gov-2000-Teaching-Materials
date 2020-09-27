##################################################################
##################################################################
# Gov 2000/E-2000 Section 9
# 10-31-13
# Soledad Artiz Prillaman
##################################################################
##################################################################

setwd("/Users/Sole/Dropbox/Gov 2000/2013/Sections/Section 9/Code/")


##################
## Independence ##
##################

# why do we care?  In mild forms, dependence among units leads
# to bad estimates of standard errors.  In more severe forms,
# dependence can lead to very badly biased estimates of model
# parameters.  

# Fixed effects models for clustered dependence

# These are fairly straightforward: set a dummy variable for each
# cluster in our data. This is equivalent to setting a separate
# intercept for each cluster.  Note that this approach is fine for
# making inferences about sampled clusters, but it will not be 
# possible to make predictions/inferences about non-sampled 
# clusters.  For this, we might need a random effects model.  

# Let's look at an example using the Robey data.
# this data is in the car package
library(car)
data(Robey)

# what variables do we have?
names(Robey)

# what do we know about the data?
?Robey 
# this tells us that we have observations on developing countries
# and we have data on fertility rates (tfr) abd contraceptors 
head(Robey)
summary(Robey)

# Say we wanted to regress fertility on contraceptors
# but we are concerned about dependence across regions

# First, let's see if there's a big difference in the residuals
# the four regions in our study
# we can do this by plotting our residuals from the regression on the region
# R automatically makes this a box plot!
lm.rob <- lm(tfr ~ contraceptors, data=Robey)
plot(residuals(lm.rob) ~ Robey$region, ylim=c(-1.5,1.5))
# There are slight differences across the three regions.  In
# Asia, especially, it looks like there may be some regional confounder that we
# could use if we had more data.  

# we can still use a regional dummy variable to control for this
# and then let's re-look at our residuals
lm.rob.fix<-lm(tfr ~ contraceptors + region, data=Robey)
plot(residuals(lm.rob.fix) ~ Robey$region, , ylim=c(-1.5, 1.5))
# these look pretty similar across regions


##################
## Missing Data ##
##################

# Now let's think a little bit about nonresponse and missing data

# Load in credit card data with missing values
cc.missing <- read.csv("ccarddata_missing.csv")
names(cc.missing)
head(cc.missing)
# The NA's in ccexpend represent missing values

##################

### Characterize Missingness - which of the assumptions do we believe?

# create a new indicator variable that tells us which observations have 
# a missing value for ccexpend
# Start by creating a variable missing and filling in 0 for ALL observations
cc.missing$missing <- 0
# Now replace missing with a 1 for observations where there is an NA for ccexpend
cc.missing[is.na(cc.missing$ccexpend),]$missing <-1

# So we have several observations that are missing data for the outcome variable
# and many that are not
# we can look at these data as two different "groups" or even as two different data sets
# Data summary for non-missing observations
summary(cc.missing[cc.missing$missing==0,])
# Data summary for missing observations
summary(cc.missing[cc.missing$missing==1,])

# What does this tell us?

# Are these two data sets (one for missing and one for not)
# different in their covariate values?
# to test this we can use a ttest for each covariate (remember there is no missingness in our covariates)
# this is the same as the difference in means
# test that we used earlier in the semester when we had two groups

##################
## QUIZ BREAK: What was the test for a test of difference in means?
## i.e. H0: mu_missing - mu_nonmissing = 0 















# For each covariate we use the command t.test 
# and specify our variable for group 1 (missing observations)
# and group 2 (non-missing observations)
age.ttest <- t.test(cc.missing[cc.missing$missing==1,"age"],cc.missing[cc.missing$missing==0,"age"])
income.ttest <- t.test(cc.missing[cc.missing$missing==1,"income"],cc.missing[cc.missing$missing==0,"income"])
homeowner.ttest <- t.test(cc.missing[cc.missing$missing==1,"homeowner"],cc.missing[cc.missing$missing==0,"homeowner"])

# Let's extract the test statistics from these difference in means test

# First remember that the test statistic is the difference in means/ se(difference in means)
(mean(cc.missing[cc.missing$missing==1,"age"]) - mean(cc.missing[cc.missing$missing==0,"age"]))/
  sqrt(var(cc.missing[cc.missing$missing==1,"age"])/length(cc.missing[cc.missing$missing==1,"age"])
    + var(cc.missing[cc.missing$missing==0,"age"])/length(cc.missing[cc.missing$missing==0,"age"]))
age.ttest$statistic
# viola!

# Now let's create a vector of these test statistic
t.stats <- c(age.ttest$statistic, income.ttest$statistic, homeowner.ttest$statistic)
t.stats

# create balance plot to compare covariate distributions across the two groups
dotchart(t.stats, labels=c("age","income","homeowner"), xlim=c(-3,3), xlab="Standardized Diff. in Means",pch=19)
abline(v=0, col="red", lty=2)

# What does this tell us?
# this tells us that income and age are larger for the observations with missing values
# and homeowner is lower
# but only age is statistically different from 0 (i.e. t.stat > critical value)

# Let's relook at the difference in means for age
mean(cc.missing[cc.missing$missing==1,"age"]) - mean(cc.missing[cc.missing$missing==0,"age"])
# This tells us that those who didn't report ccexpend are on average 4.5 years older
# than those that did!

##################
## QUIZ BREAK: What does this tell us about our missingness mechanism? 
# Could our data be MCAR? MAR? NMAR?















### an alternative is to make a box plot for the two groups (missing and non-missing outcome) for the covariates
boxplot(age~missing,data=cc.missing)
boxplot(income~missing,data=cc.missing)


##################

# So how can we deal with this missingness?

### 1. Complete Case Analysis
# delete all observations that have missing values
# this is what R automatically does
lm.cc.complete.case <- lm(ccexpend ~ income + homeowner + age, data=cc.missing)
summary(lm.cc.complete.case)

# Let's show that this is the same as if we deleted all of the rows that had an na
cc.nomissing <- na.omit(cc.missing)
lm.cc.complete.case2 <- lm(ccexpend ~ income + homeowner + age, data=cc.nomissing)
summary(lm.cc.complete.case2)
# Ta-da!


### 2. Mean Imputation
# fill in missing ccexpend values with mean of ccexpend

# start by creating a new data frame for the data with imputed values
cc.meanimpute <- cc.missing
# for the rows with missing data, fill in ccexpend with the mean of ccexpend
cc.meanimpute[cc.meanimpute$missing==1,]$ccexpend <- mean(cc.missing$ccexpend, na.rm=TRUE)


### 3. Regression Imputation
# fill in missing ccexpend values with predicted value from regression line
# from complete-case analysis

#  start by creating a new data frame for just the observations with missing values
# is.na specifies the observations that have an NA
missing.data.frame<- cc.missing[is.na(cc.missing$ccexpend),c("income","homeowner","age")]

# create another data frame for the data with imputed values
cc.regimpute <- cc.missing
# for the rows with missing data, fill in ccexpend with the predicted values 
#  from the complete case analysis
# to do this we use the predict function, which takes as its inputs a regression model
# and the data that you want the fitted values for
cc.regimpute[cc.regimpute$missing==1,]$ccexpend <- predict(lm.cc.complete.case,missing.data.frame)


## Now, let's compare our data from each of these processes
par(mfrow=c(1,3))
# First plot a histogram of ccexpend for non-missing values (complete case analysis)
hist(cc.missing[cc.missing$missing==0, "ccexpend"], xlab="ccexpend", main="Complete Case Analysis")
# Second plot a histogram of ccexpend with mean imputed values
hist(cc.meanimpute[, "ccexpend"], xlab="ccexpend", main="Mean Imputation")
# Third plot a histogram of ccexpend with regression imputed values
hist(cc.regimpute[, "ccexpend"], xlab="ccexpend", main="Regression Imputation")





####################################
## Sample Replication-ish Problem ##
####################################

#Load some data
# this is a lot of data on the 50 US states from 1990
library(foreign)
statedata <- read.dta("statedata.dta")
head(statedata)
dim(statedata)


# Let's try to estimate the regression model (conditional expectation function)
# of murder given popdensity, police, blackpop and unemp

# murder = murders per 100,000 population
# popdensity = population per square mile
# police = number of police officers
# blackpop = black populations in 1000's
# unemp = unemployment rate as percentage of state's labor force


## Let's fit a basic OLS model
lm.base <- lm(murder ~ popdensity + police + blackpop + unemp, data = statedata)
summary(lm.base)


#####################

# Do we have missingdata on the outcome? Yes!

# Let's create an indicator of missingness
statedata$missing <- 0
statedata[is.na(statedata$murder),]$missing <-1

# Now let's create a vector of the t test statistics for each covariate
t.stats <- c(t.test(statedata[statedata$missing==1,"popdensity"],statedata[statedata$missing==0,"popdensity"])$statistic,
             t.test(statedata[statedata$missing==1,"police"],statedata[statedata$missing==0,"police"])$statistic,
             t.test(statedata[statedata$missing==1,"blackpop"],statedata[statedata$missing==0,"blackpop"])$statistic,
             t.test(statedata[statedata$missing==1,"unemp"],statedata[statedata$missing==0,"unemp"])$statistic)
t.stats

# create balance plot to compare covariate distributions across the two groups
dotchart(t.stats, labels=c("popdensity","police","blackpop", "unemp"), xlim=c(-2,2), xlab="Standardized Diff. in Means",pch=19)
abline(v=0, col="red", lty=2)

# We have pretty good balance on everything but popdensity - maybe it is not MCAR
# We should try to fill in this missingness

# Let's use regression imputation
missing.data <- statedata[is.na(statedata$murder),c("popdensity","police","blackpop", "unemp")]
data.regimpute <- statedata
data.regimpute[data.regimpute$missing==1,]$murder <- predict(lm.base,missing.data)
summary(data.regimpute)


## Let's rerun our model
lm.miss <- lm(murder ~ popdensity + police + blackpop + unemp, data = data.regimpute)
summary(lm.miss)


#####################

# Do our assumptions hold?

# 1. Linearity:

crPlots(lm.miss)
# As the scatter plots suggested, popdensity, police and blackpop
# may have concern regarding non-linearities
# Let's try logging these popdensity, police and blackpop to deal with this
data.regimpute$logpopdensity <- log(data.regimpute$popdensity + 1) 
# note that we add 1 because the log(0) is undefined
data.regimpute$logpolice<- log(data.regimpute$police)
data.regimpute$logblackpop <- log(data.regimpute$blackpop)

# Now let's rerun our model with these new terms
lm.log <- lm(murder ~ logpopdensity + logpolice + logblackpop + unemp, data = data.regimpute)
summary(lm.log)
crPlots(lm.log)

# Gah! Now linearity looks worse.  Let's keep tinkering.  
# Try including polynomials of the variables included in lm.log
# and interactions and examine the linearity assumption

# Square the unemployment variable and the logpolice variable:
data.regimpute$unempsq <- data.regimpute$unemp^2
data.regimpute$policesq <- data.regimpute$logpolice^2

lm.logsq <- lm(murder ~ logpopdensity + logpolice + logblackpop + unemp
               + unempsq + policesq,  data = data.regimpute)
summary(lm.logsq)
crPlots(lm.logsq)
# much better!


#####################

# 2. Heteroskedasticity:

# Scale Location plot
scatter.smooth(fitted(lm.logsq), sqrt(abs(rstudent(lm.logsq))), col="red")

# not so bad

#####################

# 3. Normality:

# QQ plot
qqPlot(lm.logsq)

# not too bad

