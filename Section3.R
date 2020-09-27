##################################################################
##################################################################
# Gov 2000/E-2000 Section 3
# 9-19-13
# Soledad Artiz Prillaman
##################################################################
##################################################################

## As always, let's start by setting our working directory
setwd("/Users/Sole/Dropbox/Gov 2000/2013/Sections/Section 3")


########################################################
## Extracting Items from Regression
########################################################

## load some data
load(file="Leinhardt.RData")

## run a model like last week
my.lm <- lm(linfant ~ lincome, data=Leinhardt)

## our lm object has many components we may want to extract
names(my.lm)

## For example, we may want to look at the estimates of our coefficients
## We can save these coefficients as a new object (this can be very useful)
## Two ways to get the coefficients:
my.coefs <- my.lm$coefficients
my.coefs <- coefficients(my.lm)
my.coefs

## Or we may want to get the residuals
## Remember that the residuals are the vertical distance 
##    from each point to the line we estimates
##    ei = yi - yhat
## Two ways to get the residuals:
my.resids <- my.lm$residuals
my.resids <- residuals(my.lm)

## Or we may want to extract our fitted values
## yhat = beta0 + beta1 xi
## One way to get the fitted values:
my.fvs <- my.lm$fitted.values


########################################################
## Outputting Regression coefficients
########################################################

## A nice way to output regression coefficients to LaTeX
##    is with the apsrtable command
## This command formats your lm output to the table style of APSR
install.packages("apsrtable")
library(apsrtable)
apsrtable(my.lm)

## Don't worry about this if you are not using LaTeX



########################################################
## Creating Probability Distributions
########################################################

## PMF

## Example: roll of a die
## Let's start by creating an empty plot
plot(0,0,type="n",xlab="X",ylab="f(x)",
     xlim=c(1,6), ylim=c(0,1),
     main="PMF of Dice Roll")

## A PMF represents the probability of each event occurring individually
## i.e. the probability that you will get any outcome

## To start, we specify the probability of our first outcome
## and draw a line from 0 to that probability
## to do so, we will use the segments command 
##    which takes as its input two points:
##    the starting point (x0,y0) and the end point(x1,y1)
## for PMFs, the starting point will always be (x,0)
## and the end point will be (x,P(X=x))
segments(x0=1,x1=1,y0=0,y1=1/6)

## Next we add in the point that is the probability for our first outcome
## to do so, we will use the points command
##    which takes as its input just the point (x1,y1)
## this is the same (x1,y1) from our segment 
## i.e. (x,P(X=x))
points(x=1,y=1/6,pch=19)

## Now we just repeat this for all of our values of x
segments(x0=2,x1=2,y0=0,y1=1/6)
points(x=2,y=1/6,pch=19)
segments(x0=3,x1=3,y0=0,y1=1/6)
points(x=3,y=1/6,pch=19)
segments(x0=4,x1=4,y0=0,y1=1/6)
points(x=4,y=1/6,pch=19)
segments(x0=5,x1=5,y0=0,y1=1/6)
points(x=5,y=1/6,pch=19)
segments(x0=6,x1=6,y0=0,y1=1/6)
points(x=6,y=1/6,pch=19)

## Voila! We have a beautiful PMF



## CDF

## Create an empty plot
plot(0,0,type="n",xlab="X",ylab="f(x)",
     xlim=c(1,6), ylim=c(0,1),
     main="PMF of Dice Roll")

## A CDF represents the probability of getting an outcome less than 
##    or equal to x

## To start, we specify the probability of our first outcome
## and draw a line from 0 to that probability
## again using the segments command 
## for CDFs, the starting point will always be (x-1,P(X<x-1))
## and the end point will be (x,P(X<x-1))
## First, add the horizontal line for each value of x
segments(x0=0,x1=1,y0=0,y1=0)

## Next we add in the point that is the probability for x-1 (our preceding outcome)
## to do so, we will again use the points command
##    which takes as its input (x,P(X<x-1))
## this is an open point since it is not the actual value of F(x)
points(x=1,y=0,pch=21,bg="white")

## Next we add in the point that is the probability for x (the outcome)
## to do so, we will again use the points command
##    which takes as its input (x,P(X<x))
## this is a closed point since it is the actual value of F(x)
points(x=1,y=1/6,pch=19)

## Now we just repeat this for all of our values of x
segments(x0=1,x1=2,y0=1/6,y1=1/6)
points(x=2,y=1/6,pch=21,bg="white")
points(x=2,y=2/6,pch=19)
segments(x0=2,x1=3,y0=2/6,y1=2/6)
points(x=3,y=2/6,pch=21,bg="white")
points(x=3,y=3/6,pch=19)
segments(x0=3,x1=4,y0=3/6,y1=3/6)
points(x=4,y=3/6,pch=21,bg="white")
points(x=4,y=4/6,pch=19)
segments(x0=4,x1=5,y0=4/6,y1=4/6)
points(x=5,y=4/6,pch=21,bg="white")
points(x=5,y=5/6,pch=19)
segments(x0=5,x1=6,y0=5/6,y1=5/6)
points(x=6,y=5/6,pch=21,bg="white")
points(x=6,y=1,pch=19)

## And Voila! A CDF




########################################################
## Drawing Discrete Distributions
########################################################

## If we do not know what a distribution looks like,
## we can sample from the set of possible outcomes (the sample space)
## and then look at the distribution of the sample

## Again, think about rolling a die
## Define the sample space
S <- 1:6

## Set the sample size 
## the larger it is the more accurate will be your distribution
##    Prove it to yourself!
n <- 1000000

## Sample from the population
## i.e. Roll the "die" repeatedly
dice.rolls <- sample(S, size=n, replace=T)

## Plot the distribution 
hist(dice.rolls, freq=F)



########################################################
## Drawing Continuous Distributions
########################################################

## Like with discrete distributions, we can sample from the population
## to see what our distribution looks like

## R has a bunch of canned functions which make this easy

## Say we want to look at the distribution of salaries
## and we assume that salaries are normally distributed 
## with mean 40000 and standard deviation 10000

## It may be hard to conceptualize this distribution,
## So we can sample from it using the rnorm() command
##    n is the sample size
salaries <- rnorm(n=1000000 , mean=40000 , sd=10000)

## Plot the distribution
plot(density(salaries))

## Now, what if we wanted to know the density of the distribution
## i.e. the height of the distribution
## when x = 20000
## dnorm provides the PDF of the normal distribution
dnorm(20000, mean=40000, sd=10000)

## Or maybe we want to know the probability that a salary is 
## is less than 20000
## pnorm provides the CDF of the normal distribution
pnorm(20000, mean=40000, sd=10000)

## Or maybe we want to know what salary marks the 95% percentile
## qnorm provides the inverse CDF of the normal distribution
qnorm(0.95, mean=40000, sd=10000)
## this tells us that under our assumed model, 
## 95% of people earn a salary of less than roughly $56000


## There are many other distributions which you can sample from in a similar way!



