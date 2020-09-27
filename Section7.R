##################################################################
##################################################################
# Gov 2000/E-2000 Section 7
# 10-17-13
# Soledad Artiz Prillaman
##################################################################
##################################################################


######### QUIZ ###########
## Write a function that takes in a vector x and a vector y 
#  and reports the r^2 from a regression of x on y
#  do not use the lm() command at all! 




















r2.function <- function(x,y){
  # Find our estimate of b1 using the first normal equation
  b1 <- sum((x-mean(x))*y)/sum((x-mean(x))^2)
  # Find our estimate of b0 using the second normal equation
  b0 <- mean(y) - b1*mean(x)
  # using our estimates of b1 and b0 find our yhat
  yhat <- b0 + b1*x
  # use these in the formula for r2
  r2 <- sum((yhat-mean(y))^2)/sum((y-mean(y))^2)
  return(r2)
}






# Clear our workspace and set our working directory
rm(list=ls())
setwd("/Users/Sole/Dropbox/Gov 2000/2013/Sections/Section 7/")

# Read in data, since this is a .txt file, we use read.table()
# Data is on log weekely earnings (luwe), education, etc.
data <- read.table("nls_2012.txt")

# Name the variables because text file has no var names
# each "" denotes a name for a column (variable)
colnames(data) <- c("luwe", "educ", "exper", "age", "fed", "med", "kww", "iq")

# What does our data look like?
head(data)
# Each column is a variable and each row is an observation
# so for each person, we have data on every variable
# this is why we subscript things by i in our formulas

# Attach data so that we can access varnames without $ operator
# This is not always a good idea if you are working with multiple data sets
# but since we only have one data set, it's ok
attach(data)


###############################
### Multivariate Regression ###
###############################

# First let's look at a single variable regression
# we will regress log earnings on IQ
# Y = b0 + b1x1 + e
simple.reg <- lm(luwe ~ iq)
summary(simple.reg)

## ASIDE: Does our r-squared function work?
r2.function(x=iq,y=luwe)
# Yes!


# What happens if we add another variable, say education?
# Y = b0 + b1x1 + b2x2 + e
multi.reg <- lm(luwe ~ iq + educ)
summary(multi.reg)


# Notice that our coefficient on educ changed
# Why?
cor(iq, educ)
# because education and IQ are positively correlated 

# So what does this tell us about multivariate regression?
# Just like with simple regression, we are minimizing the squared residuals

##############################

## Minimizing residuals in simple regression

# Let's think about this for simple regression
# We can plot our data
plot(y=luwe, x=iq, xlab="IQ", ylab="Log Weekly Earnings")
# And then overlay our estimated regression line
abline(simple.reg, col="red", lwd=3)

# How did we get this line?
# We minimized the sum of the vertical distances from the points to the line
segments(x0=iq[32],y0=simple.reg$fitted.values[32],x1=iq[32],y1=luwe[32], col="blue", lwd=3)
segments(x0=iq[193],y0=simple.reg$fitted.values[193],x1=iq[193],y1=luwe[193], col="blue", lwd=3)
segments(x0=iq, y0=simple.reg$fitted.values, x1=iq, y1=luwe, col="blue")



##############################

## Minimizing residuals in multivariate regression

# What about when we have more than one explanatory variable?
# We need to think in higher dimensions
# Let's return to our regression of earnings on education and IQ
# we have two explanatory variables and our dependent variable
# 3 variables means 3 dimensions!

#Load 3d graphing package
install.packages(scatterplot3d)
library(scatterplot3d) 

# Create our 3d scatterplot with our 3 variables
# parallel to plot() command
s3d <- scatterplot3d(iq,educ,luwe, pch=16, xlab="IQ", ylab="Years of Education", zlab="Log Weekly Earnings")
# And then overlay our estimated regression plane
# it is a plane because it has to fit the 3d data!
s3d$plane3d(multi.reg, col="red", lwd=3)

# How did we get this plane?
# We minimized the sum of the vertical distances from the points to the plane
# Convert our 3d coordinates to 2d for the x0 and y0 position (the point itself)
orig <- s3d$xyz.convert(iq, educ, luwe)
# Convert our 3d coordinates to 2d for the x1 and y1 position (the plane for each observation)
plane <- s3d$xyz.convert(iq, educ, multi.reg$fitted.values)
# And now we can see what the residuals are
segments(orig$x[32], orig$y[32], plane$x[32], plane$y[32], col="blue", lwd=3)
segments(orig$x[193], orig$y[193], plane$x[193], plane$y[193], col="blue", lwd=3)
segments(orig$x, orig$y, plane$x, plane$y, col="blue")

# So the beta1 and beta2 that we get are the values which give us this plane!


# What does this look like in two dimensions?
plot(y=luwe, x=iq, xlab="IQ", ylab="Log Weekly Earnings")
# educ only takes on values from 9 to 18
# so it doesn't make sense given our data to plot lines controlling for other values
# So what is our regression line when educ is 9?
abline(a=multi.reg$coef["(Intercept)"]+multi.reg$coef["educ"]*9, b=multi.reg$coef["iq"], col="red")
# What about for any value of education?
for(i in 9:18){
  abline(a=multi.reg$coef["(Intercept)"]+multi.reg$coef["educ"]*i, b=multi.reg$coef["iq"], col="red")
}




######## QUIZ #########
# So haven't we done all of this before?
# Earlier we assumed we had the POPULATION, not a sample

# What does this change when we do regression or simply use the lm() function?

















# When we had the population data, the betas we calculated were the true population beta values
# Now that we are sampling, our betas are estimators of the true value!

# What is our estimand, estimator, and estimate?




# For example, let's assume that the entire data set is the population
# In reality, the data we have is just a sample from the true, unkown population

# Let's then look at our population model again
summary(multi.reg)

# Now let's take a sample of 250 from the population
set.seed(12345)
rand.rows <- sample(1:nrow(data),250, replace=F)
samp.1 <- data[rand.rows,]
dim(samp.1)

# And we can perform our model on this new sample
multi.reg.samp1 <- lm(luwe ~ iq + educ, data=samp.1)
summary(multi.reg.samp1)

# Our betas will now change depending upon which sample we draw
# i.e. they will have sampling distributions
# which means that they have uncertainty around them
# which means that they are not constants
# which means that they have a variance! (or standard error)
# remember our true population values are constants and therefore have var = 0


#######################
### Standard Errors ###
#######################

# This idea of sampling and uncertainty around our estimators is important

# How much uncertainty do we have?
# Remember that Var(beta) = sigma^2/ sum(x-xbar)
# But we don't know sigma!
# So we don't know how much uncertainty we have!

# How then do we estimate our uncertainty?
# 1. Use the estimated standard error and assume that if n is large enough this is ok
      # We can get this using the vcov() function
      vcov(multi.reg)
      # this gives us the variance-covariance matrix for our coefficients
      # the diagonal elements are the variances of our coefficients
      # the off-diagonal elements are the covariances - note the symmetry!

      # So what are our estimated standard errors?
      # Pull out variances
      var.betas <- diag(vcov(multi.reg))
      # Take square root to get standard deviation
      sd.betas <- sqrt(var.betas)
      sd.betas
      # These match what we get from our model output!

# 2. Bootstrap to create a "fake" sampling distribution and get the variance of this
      # see Section 5 and 6 notes


#################################################
### Multivariate Regression with Interactions ###
#################################################

# Sometimes we want to include an interaction term
# What if we think that iq and education interact to affect wages?
# we will regress log earnings on iq and education and their interaction
# Y = b0 + b1x1 + b2x2 + b3x1*x2 + e
interact.reg <- lm(luwe ~ educ + iq + iq*educ)
summary(interact.reg)

# But this doesn't give us the effect we really care about
# What is the marginal effect of education on wages CONDITIONAl on IQ?
# b1 + b3*iq!

# What does this look like in two dimensions?
plot(y=luwe, x=educ, xlab="Years of Education", ylab="Log Weekly Earnings")
# So what is our regression line for an IQ of 100?
abline(a=interact.reg$coef["(Intercept)"]+interact.reg$coef["iq"]*100,
       b=interact.reg$coef["educ"]+interact.reg$coef["educ:iq"]*100, col="red")
# What about for any value of education?
# IQ takes on values from 50 to 145
for(i in 50:145){
  abline(a=interact.reg$coef["(Intercept)"]+interact.reg$coef["iq"]*i,
         b=interact.reg$coef["educ"]+interact.reg$coef["educ:iq"]*i, col="red")
}

# The slopes of the lines look very similar
# and we know that interaction terms are supposed to change the slp[e]
# Maybe the interaction doesn't add anything, i.e. it is not significant
# But this plot doesn't tell us anything about our uncertainty around our marginal effects (the slope of these lines)!


# How do we get our uncertainty for our marginal effect?
# Is it the standard error on the interaction term?
summary(interact.reg)
# NO!

# Our marginal effect is beta1 + beta3*z 
# So the uncertainty of this is Var(beta1+beta3*z)


######## QUIZ ########
## How do we get this variance if we only have the variances of our coefficients?
## i.e. we only know var(beta1) and the var(beta3)





















# But what can we do with this?
# Var(beta1 + beta3z)
# = var(beta1) + var(beta3*z) + 2*cov(beta1,beta3*z)
# = var(beta1) + z^2*var(beta3) + 2*z*cov(beta1,beta3)

# We can calculate this!


##########################################
### Marginal Effects with Interactions ###
##########################################

# So what is our estimated marginal effect if iq is 100?
marginal.effect.100 <- interact.reg$coef["educ"]+interact.reg$coef["educ:iq"]*100

# What is our estimated standard error for this effect?
# remember we can get our variances from the vcov matrix
vcov(interact.reg)
var.beta1 <- vcov(interact.reg)["educ", "educ"]
var.beta3 <- vcov(interact.reg)["educ:iq", "educ:iq"]
cov.beta1.beta3 <- vcov(interact.reg)["educ","educ:iq"]

# so our estimated standard error is 
se.effect.100 <- sqrt(var.beta1 + 100^2*var.beta3 + 2*100*cov.beta1.beta3)


# Now we can do hypothesis testing! We have everything we need.
# H0: marginal effect | (iq = 100) = 0
# HA: marginal effect | (iq = 100) =/= 0
test.statistic <- (marginal.effect.100 - 0) / se.effect.100
test.statistic

p.value <- 2*(1-pnorm(test.statistic))
p.value
# we can reject the null hypothesis of no effect when iq is 100



# MARGINAL EFFECT PLOT
# What if we didn't want to have to do this by hand for each value of iq?
# We could plot the marginal effect (linear function) and it's 95% confidence intervals
# let's create a ruler vector with possible values of IQ
ruler <- seq(45,150,0.01)
# Create an empty plot
plot(iq, luwe, col="white", xlab="IQ", ylab="Marginal Effect of Education on Earnings", ylim=c(-.1,.1))
# Add our marginal effect line
lines(ruler, interact.reg$coef["educ"]+interact.reg$coef["educ:iq"]*ruler, lwd=2)
# Add our 95% confidence intervals
# upper bound = marginal effect + 1.96*se of marginal effect
curve((interact.reg$coef["educ"]+interact.reg$coef["educ:iq"]*x) + 1.96 * sqrt(var.beta1 + x^2*var.beta3 + 2*x*cov.beta1.beta3), add=T, col="red", lty=2)
# lower bound = marginal effect - 1.96*se of marginal effect
curve((interact.reg$coef["educ"]+interact.reg$coef["educ:iq"]*x) - 1.96 * sqrt(var.beta1 + x^2*var.beta3 + 2*x*cov.beta1.beta3), add=T, col="red", lty=2)

##Add a line at 0
abline(h=0, col="blue")

## What does this tell us? 
# Each y-value is a marginal effect (slope) conditional on the x-value (value of IQ)
# The confidence bounds do not contain 0 at any point
# So our marginal effects are statistically significant at any point, even though they are small


## Note: you can get estimates for the standard error also by 
# 1. bootstrapping a sampling distribution for the marginal effect
# 2. Recenter regression as in Professor Glynn's slides




###############
### F-Tests ###
###############


#######################
##  F distribution 

## Let's start off with a quick demonstration of what the F
## distribution looks like for some parameters:

curve(df(x=x,df1=5,df2=200), from=0, to=15, col="chocolate", lwd=2)

## We can use the same pXXXX functions from the normal and t
## with the F:

pf(q=2, df1=5, df2=200)
pf(q=2, df1=5, df2=200, lower.tail=FALSE)
## if lower.tail = T, probabilities are P[X <= x], otherwise, P[X > x]
## df1 is the number of restrictions or betas you are testing (k)
## df2 = n-(k+1) like with the t-distribution


## We can also use the qXXXX functions to find the point where
## x% of the distribution is higher or lower:

qf(p=0.95, df1=5, df2=200)
## or
qf(p=0.05,  df1=5, df2=200, lower.tail=FALSE)

curve(df(x=x,df1=5,df2=200), from=0, to=15, col="chocolate", lwd=2)
abline(v=qf(p=0.95, df1=5, df2=200), lwd=2)


#######################
## F test for all coefficients

## A special case of the F-test that is often used is the "omnibus" test
## of all of the variables in the regression. R (and most other
## software packages) gives you this in the regression output:

# let's return to our original mutivariate regression
summary(multi.reg)
# you can see that R gives you an F-statistic

## We can plot this test-statistic against its null distribution
## and we can see that it is very unlikely.
curve(df(x, 2, 926), from=0, to=80, col="chocolate")
abline(v=summary(multi.reg)$fstatistic[1])
## extracting the omnibus f stat from the lm output

## note that df1 = 2 (two restrictions)
## and df2 = n-(k+1) -- > 929 - 2 - 1


## We can also calculate the F-statistic "manually."
## We know from lecture that 

# F test stat = (R^2/k)/((1-R^2)/(n-k-1))

# Pull out R-squared from regression summary
r.sq <- summary(multi.reg)$r.squared
# k is the number of betas - 1
k <- length(multi.reg$coef) - 1
# n is the number of observations
n <- length(multi.reg$fitted.values)

f.test.statistic <- (r.sq/k)/((1-r.sq)/(n-k-1))
f.test.statistic
summary(multi.reg)$fstatistic[1]

## Ta da!





