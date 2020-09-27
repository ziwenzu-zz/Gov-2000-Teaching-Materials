##################################################################
##################################################################
# Gov 2000/E-2000 Section 5
# 10-3-13
# Soledad Artiz Prillaman
##################################################################
##################################################################


###############################
### The Normal distribution ###
###############################

## the Normal distribution is invaluable to statistics. as
## we will see later in the course, we will often use the 
## Normal distribution as an approximation of some unknown 
## distribution. This means that we think that the probability 
## statements about the Normal are very close to the  probability
## statements about the unknown distribution.

plot(dnorm, mean = 1, sd = 4, from = -4, to = 4, col = "blue", main = "PDF of the Standard Normal")

## - area under curve always equals 1, regardless of mu and sigma
## - y-axis values do not have straight-forward interpretation, as the
## integral of the pdf evaluated at any specific point is 0 (since
## p(x) is continuous.  Were we to integrate within a certain area, we
## would get a probability value, though.)
## - Examples of such integrals:
##    - \int[-1,1] \approx .68
##    - \int[-2,2] \approx .95
##    - \int[-3,3] \approx .997
## - the y-axis values can be thought of as "normalizing" the density
## - unit normal: mu = 0, s^2 = 1 : will be a building block for us


## often we want to know the probability of being in some interval
## for a normal random variable. 
## Remember: if you use pnorm(q,...)
## lower.tail=TRUE:  probability of (-infinity, q)
## lower.tail=FALSE: probability of (q, infinity)
## YOU SHOULD DRAW PICTURES!!!

pnorm(-2, mean = 0, sd = 1, lower.tail = TRUE)
pnorm(-2, mean = 0, sd = 1, lower.tail = FALSE)

## what about 0?
pnorm(0, mean = 0, sd = 1, lower.tail = TRUE)
pnorm(0, mean = 0, sd = 1, lower.tail = FALSE)

## how can we get the probability of being between 2 numbers?

pnorm(0) - pnorm(-2)

## The Normal Distribution and Interval Estimation
## For large samples, we generally need to find our critical 
## value which gives us the 100(1-alpha)% confidence interval
## This means, we know the probability we want covered under the curve
## but we do not know the values of x or z which give us this
## probability

## To get these values, we can use the qnorm() function
qnorm(.95, mean=0, sd=1)
## this tells us that 95% of the area under the N(0,1) pdf
## is from -infty to 1.65
## alternatively, you can think of this as telling you that
## 5% of the area is from 1.65 to infty

## 95% confidence interval
## alpha = .05
## so alpha/2 = .025
qnorm(1-.05/2, mean = 0, sd=1)
## Here is where the 1.96 comes from!

## Let's look at it
plot(dnorm, from = -4, to = 4, col = "black", main = "PDF of the Standard Normal", 
     ylab = "Density", xlab="z")
abline(v=qnorm(1-.05/2, mean = 0, sd=1))
abline(v=-qnorm(1-.05/2, mean = 0, sd=1))
## Create a vector of simulating the values between -1.96 and 1.96
x.cord<-c(-1.96,seq(-1.96,1.96,0.01),1.96)
## Create a vector of simulating the densities between z= -1.96 and 1.96
y.cord<-c(-1,dnorm(seq(-1.96, 1.96, 0.01)),-1)
## Fill in 
polygon(x.cord, y.cord, col="cornflowerblue")
## Overlay some text
text(x=0, y=.2, "95%")



###############################
##### The t- distribution #####
###############################

## The t- Distribution and Interval Estimation

## When we have small samples, but a normal population distriubtion
## we can use the t-distribution to get our critical values
## for interval estimation
## again, we generally need to find our critical 
## value which gives us the 100(1-alpha)% confidence interval
## This means, we know the probability we want covered under the curve
## but we do not know the values of x or z which give us this
## probability

## To get these values, we can use the qt() function
## we specify the probability we want and the degrees of freedom (n-1)
qt(.95, df=9)
## this tells us that 95% of the area under the t-distribution with 9 df
## is from -infty to 1.83
## alternatively, you can think of this as telling you that
## 5% of the area is from 1.83 to infty

## 95% confidence interval
## alpha = .05
## so alpha/2 = .025
qt(1-.05/2, df=9)

## Let's look at it
x<-seq(-4,4,.01)
plot(x,dt(x, df=9), col = "black", main = "PDF of the t-Distribution \n (9 degrees of freedom)", 
     ylab = "Density", xlab="t", type="l")
abline(v=qt(1-.05/2, df=9))
abline(v=-qt(1-.05/2, df=9))
## Create a vector of simulating the values between -2.26 and 2.26
x.cord<-c(-2.26,seq(-2.26,2.26,0.01),2.26)
## Create a vector of simulating the densities between z= -1.96 and 1.96
y.cord<-c(-1,dt(seq(-2.26, 2.26, 0.01),df=9),-1)
## Fill in 
polygon(x.cord, y.cord, col="cornflowerblue")
## Overlay some text
text(x=0, y=.2, "95%")



###############################
#### Confidence Intervals #####
###############################

# let's simulate some data and find the 95% confidence interval
# for the mean

# set our seed since we will be randomly sampling
set.seed(02138)

# First, let's draw a population
# set mu to be 12 and sigma to be 4
# assume our population is normally distributed
pop.mean <- 12
pop.sd <- 4
pop <- rnorm(n=1e6, mean=pop.mean, sd=pop.sd)

# Now let's draw a single sample of size 100
# We can set the parameter values as objects
# That way when you want to change them you just have one spot to change
sample.size <- 100

# because we want a 95% confidence interval
# we know that the critical value (z) is 1.96
critical.value <- 1.96

# draw our sample
my.sample <- sample(pop, size=sample.size, replace=T)

# we can look at the mean of our sample
# remember our sample is just one of the rows in the table
# so this mean is just one element of the table
# i.e. one realization of our estimator
sample.mean <- mean(my.sample)
sample.mean

# let's also assume that we don't know sigma
# (as is usually the case in the real world)
# as mentioned in lecture, we can estimate sigma
# with the sample standard deviation from our sample
sample.sd <- sd(my.sample)
sample.sd


# Construct analytic 95% confidence interval
# Remember formula from lecture xbar - 1.96*(s/sqrt(n))
lower.bound <- sample.mean - critical.value*(sample.sd/sqrt(sample.size))
upper.bound <- sample.mean + critical.value*(sample.sd/sqrt(sample.size))
lower.bound
upper.bound

# so our estimated confidence interval is [11.3,12.7]
# Note: we started with the population, but we did not use this information 
# again after we took our sample
# When we have actual data, we can just imagin skipping the first step
# and starting after you have the sample (that's what our data is)


#### Quiz: What happens if I…
####  		(a) Increase the sample size?
####            (b) Increase the variance of the population?
####  Will the interval get wider or narrower?


###############################
## Bootstrapping the sampling distribution
###############################

# Construct a bootstrap confidence interval

# set the number of samples we will bootstrap
s <- 100000
# create a holder vector that we will later fille
means <- rep(NA, s)

# sample from the sample with replacement (bootstrapping) s times
# for each sample, store the sample mean to our means vector
for(i in 1:s) {
  boot.sample <- sample(my.sample, replace=T, size=sample.size)
  means[i] <- mean(boot.sample)
}

# plot the resulting sampling distribution of the means from our bootstrapped vector
plot(density(means))

# calculate our confidence interval by finding the values which leave 95% of the 
# sampling distribution
lower.boot <- quantile(means, .025)
upper.boot <- quantile(means, .975)
lower.boot
upper.boot

# our bootstrapped confidence interval is [11.3,12.7]
# which is exactly the same as what we got from our single sample!

################################

# Now let's make sure this whole process "works"
# Does a 95% CI contain the truth 95% of the time?

# We'll take many samples from the population, generate the CI's
# And keep track of when they do/do not contain the true mean.
# Remember we set the true mean to be 12
# we will use the same number of samples from our bootstrap (s)
# and the same sample size as before (sample.size)

# we create an object that stores the number of successes 
# (how many CIs contained the true population mean)
# this starts at 0
contained <- 0
for(i in 1:s) {
  repeat.sample <- sample(pop, size=sample.size, replace=T)
  repeat.sample.mean <- mean(repeat.sample)
  lower.bound.sample <- repeat.sample.mean - critical.value*(sd(repeat.sample)/sqrt(sample.size))
  upper.bound.sample <- repeat.sample.mean + critical.value*(sd(repeat.sample)/sqrt(sample.size))
  # this just says if thet population mean is within the calculated 95% confidence interval
  # add one to our vector named contained (i.e. the cumulative sum of successes)
  # this will tell us how of the confidence intervals from our samples contained the true mean
  if (pop.mean > lower.bound.sample & pop.mean < upper.bound.sample) {
    contained <- contained + 1
  }
}

# What percentage contained the true mean? should be 0.95
contained / s
# So cool!!

###############################
# QUIZ:
# prove that this holds true even when the population distribution 
# is NOT normal 



###############################
#### Calculating p-values #####
###############################

## say our set-up is
## H0: mu=0
## HA: mu!=0
## T=xbar-mu.o/s/sqrtn

## Let's go back to our original sample
# we know that the population mean is 12, so we should be able to reject 
# the null that it is 0

# what is our test statistic?
test.statistic <- sample.mean /(sample.sd/sqrt(sample.size))
test.statistic

# we know our test statistic is distributed approximately N(0,1) since n is large
# let's look at this
plot(dnorm, from = -4, to = 35, col = "black", main = "PDF of the Standard Normal", 
     ylab = "Density", xlab="z")
abline(v=test.statistic, col="red", lty=2)

# so what is the probability that we would see a value at least this extreme?
# we multiply by 2 since our alternative hypothesis means extreme could be pos or neg
p.value<-2*(1 - pnorm(test.statistic))
p.value # 0, as expected!


# What if we had the same set-up, but a different population
# Now our population has mean 0 and sd 1
# assume our population is normally distributed
pop2.mean <- 0
pop2.sd <- .1
pop2 <- rnorm(n=1e6, mean=pop2.mean, sd=pop2.sd)

# Now let's draw a single sample of size 10
sample2.size <- 10

# now our test statistic is distributed t 
# so we need a new critical value = 2.26
critical.value2 <- qt(1-.05/2,df=9)
critical.value2

# draw our sample
my.sample2 <- sample(pop2, size=sample2.size, replace=T)

# we can look at the mean of our sample
sample2.mean <- mean(my.sample2)
sample2.mean

# let's also assume that we don't know sigma
sample2.sd <- sd(my.sample2)
sample2.sd

# what is our new test statistic?
test.statistic2 <- sample2.mean /(sample2.sd/sqrt(sample2.size))
test.statistic2

# so what is the probability that we would see a value at least this extreme?
# we multiply by 2 since our alternative hypothesis means extreme could be pos or neg
p.value2 <- 2*(pt(test.statistic2, df=9))
p.value2 

# let's look at what this means from our null distribution
x<-seq(-4,4,.01)
plot(x,dt(x, df=9), col = "black", main = "PDF of the t-Distribution \n (9 degrees of freedom)", 
     ylab = "Density", xlab="t", type="l")
abline(v=test.statistic2, col="red", lty=2)
abline(v=-test.statistic2, col="black", lty=2)
# remember, since we defined our alternative hypothesis to be two-sided
# extreme means on either end!

## So where do we get the p-value from
## Create a vector of simulating the values between -infty and test.statistic
x.cord1 <- c(-4,seq(-4,test.statistic2,0.01),test.statistic2)
# and -test.statistic and 
x.cord2 <- c(-test.statistic2,seq(-test.statistic2,4,0.01),4)
## Create vectors simulating the densities 
y.cord1 <- c(-1,dt(seq(-4,test.statistic2,0.01),df=9),-1)
y.cord2 <- c(-1,dt(seq(-test.statistic2,4,0.01),df=9),-1)
## Fill in 
polygon(x.cord1, y.cord1, col="cornflowerblue")
polygon(x.cord2, y.cord2, col="cornflowerblue")


