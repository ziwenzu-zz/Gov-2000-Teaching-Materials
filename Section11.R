##################################################################
##################################################################
# Gov 2000/E-2000 Section 11
# 11-14-13
# Soledad Artiz Prillaman
##################################################################
##################################################################

setwd("/Users/Sole/Dropbox/Gov 2000/2013/Sections/Section 11/Code/")
setwd("C:/Users/sartiz/Dropbox/Gov 2000/2013/Sections/Section 11/Code/")


###################################
## ESTIMATION: CASAUAL INFERENCE ##
###################################

## To explore estimation within the framework of causal inference
## we are going to use a toy example regarding unemployment insurance.
## In our experiment, we want to know what the effect of providing
## information about UI has on the likelihood of applying for benefits.

## TREATMENT: 1 if received information, 0 otherwise
## OUTCOME (benefits): On a scale of 1-5, how likely a person is to apply for UI
##          5 - very likely

# Load our data
ui <- read.table("ui.txt")
ui


#######################################
## Estimation with Random Assignment ##
#######################################

## We want to estimate the ATE on the likelihood of applying for benefits

## We can calculate this directly
## Remembering that ATE = E[Y|T=1] - E[Y|T=0]

# Store our mean of benefits for treated group
mean.treated <- mean(ui[ui$treatment==1,]$benefits)
# Store our mean of benefits for control group
mean.control <- mean(ui[ui$treatment==0,]$benefits)
# Estimate ATE
ate <- mean.treated - mean.control
ate



## Alternatively, we can get the ATE using simple regression
## All we have to do is regress benefits on treatment
## We don't need any covariates - why?
## Exchangeability! - as a result of random treatment assignment
lm.ate <- lm(benefits ~ treatment, data=ui)
summary(lm.ate)
# our coefficient on treatment is the same as we found above!
# And, now we have an estimate of our standard error



# What happens if we add a covariate?
# Say we believe that type (whatever that is) might be important
lm.ate2 <- lm(benefits ~ treatment + type, data=ui)
summary(lm.ate2)
# We still get the same answer
# Why? - because of exchangeability!




###################################################
## Estimation with Conditional Random Assignment ##
###################################################

## Instead of executing the unemployment insurance with strict
## random assignment, the experimenters decided to condition treatment
## assignment on employment status
## employment = 1 if employed, 0 if unemployed


## Stratum-specific ATEs

## We can calculate thiese directly
## Remembering that stratum-specific ATE = E[Y|T=1, X=x] - E[Y|T=0, X=x]

#EMPLOYED
# Let's start with those who are employed
# Store our mean of benefits for treated group
mean.emp.treated <- mean(ui[ui$treatment==1 & ui$employment==1,]$benefits)
# Store our mean of benefits for control group
mean.emp.control <- mean(ui[ui$treatment==0 & ui$employment==1,]$benefits)
# Estimate ATE
ate.emp <- mean.emp.treated - mean.emp.control
ate.emp

#UNEMPLOYED
# Now let's calculate this for the unemployed
# Store our mean of benefits for treated group
mean.unemp.treated <- mean(ui[ui$treatment==1 & ui$employment==0,]$benefits)
# Store our mean of benefits for control group
mean.unemp.control <- mean(ui[ui$treatment==0 & ui$employment==0,]$benefits)
# Estimate ATE
ate.unemp <- mean.unemp.treated - mean.unemp.control
ate.unemp



## Alternatively, we can get the stratum specific ATEs using interactive regression
## All we have to do is regress benefits on treatment, employment, and the interaction
## We don't need any covariates - why?
## Conditional exchangeability! - as a result of conditional
## random treatment assignment
lm.strata.ate <- lm(benefits ~ treatment*employment, data=ui)
summary(lm.strata.ate)
# our coefficient on treatment is the ATE for unemployed
# To get an estimate of the ATE for the employed
# We must add beta1 and beta3
coef(lm.strata.ate)[2] + coef(lm.strata.ate)[4]
# And, now we can calculate standard errors for these estimates!
# Go back to the weeks on interactive models to understand how we do this



#####################
## Standardization ##
#####################

## But what if we want an estimate for the ATE, not the stratum-specific
## ATEs, when we have a conditional random experiment?

## We can use standardization
## This weights the stratum-specific ATEs by the probability
## of being in that strata


## We can calculate this directly
# First we find the probability of being employed
prob.emp <- mean(ui$employment)
# and then we find the probability of being unemployed
prob.unemp <- 1 - prob.emp

# Using this and the stratum-specific ates we found before
# we can get an estimate of our ATE
ate.stand <- ate.emp*prob.emp + ate.unemp*prob.unemp
ate.stand


## Alternatively, we can get the stanardization ATE
## using interactive regression
## Remember the standardized ATE = beta1 + beta3* mean(X)
## where beta1 and beta3 come from the interaction model we
## calculated above
lm.ate.stand.1 <- coef(lm.strata.ate)[2] + coef(lm.strata.ate)[4]*mean(ui$employment)
lm.ate.stand.1

# Or we can demean employment and then the coefficient on beta1
# will be our standardized ATE
employment.demean <- ui$employment - mean(ui$employment)
# Then we run the interaction model using this new variable
lm.ate.stand.2 <- lm(benefits ~ treatment*employment.demean, data=ui)
summary(lm.ate.stand.2)
## ta-da!

