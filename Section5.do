********************************************************
********************************************************
** Gov 1000/2000e Section 5
** 10-3-13
** Soledad Artiz Prillaman
********************************************************
********************************************************

set more off

********************************************************
** The Normal Distribution
********************************************************

** the Normal distribution is invaluable to statistics. as
** we will see later in the course, we will often use the 
** Normal distribution as an approximation of some unknown 
** distribution. This means that we think that the probability 
** statements about the Normal are very close to the  probability
** statements about the unknown distribution.

** So let's look at a large sample from a normal distribution
** which should approximate the truth since the sample is large
set seed 02138

*** Let's imagine a population of variable x
*** In the population x is normally distributed 
*** With mean 12 and sd 4
*** and now let's take a sample of size 10000
clear
set obs 100000
drawnorm x, mean(12) sd(4)
kdensity x
sum x

** - area under curve always equals 1, regardless of mu and sigma
** - y-axis values do not have straight-forward interpretation, as the
** integral of the pdf evaluated at any specific point is 0 (since
** p(x) is continuous.  Were we to integrate within a certain area, we
** would get a probability value, though.)
** - Examples of such integrals:
**    - \int[-1,1] \approx .68
**    - \int[-2,2] \approx .95
**    - \int[-3,3] \approx .997
** - the y-axis values can be thought of as "normalizing" the density
** - unit normal: mu = 0, s^2 = 1 : will be a building block for us


** often we want to know the probability of being in some interval
** for a normal random variable. 
** normal() provides us with this probability, given that we have a 
** standard normal distribution (N(0,1))
** remember any normal variable can be transformed to be standard normal!

** What is the probability of a value between -infty and 1
display normal(1)

** What is the probability of a value between -2 and 2
display normal(2) - normal(-2)


*************************

** The Normal Distribution and Interval Estimation
** For large samples, we generally need to find our critical 
** value which gives us the 100(1-alpha)% confidence interval
** This means, we know the probability we want covered under the curve
** but we do not know the values of x or z which give us this
** probability

** To get these values, we can use the invnormal() function
** which also assumes a standard normal distribution (N(0,1))
display invnormal(.95)
** this tells us that 95% of the area under the N(0,1) pdf
** is from -infty to 1.65
** alternatively, you can think of this as telling you that
** 5% of the area is from 1.65 to infty

** 95% confidence interval
** alpha = .05
** so alpha/2 = .025
display invnormal(1-.05/2)
** Here is where the 1.96 comes from!


********************************************************
** The t- Distribution
********************************************************

** The t- Distribution and Interval Estimation

** When we have small samples, but a normal population distriubtion
** we can use the t-distribution to get our critical values
** for interval estimation
** again, we generally need to find our critical 
** value which gives us the 100(1-alpha)% confidence interval
** This means, we know the probability we want covered under the curve
** but we do not know the values of x or z which give us this
** probability

** To get these values, we can use the invttail() function
** the degrees of freedome (n-1) and the probability we want
display invttail(9,.95)
** this tells us that 95% of the area under the t-distribution with 9 df
** is from -1.83 to infty
** alternatively, you can think of this as telling you that
** 5% of the area is from -infty to -1.83

** 95% confidence interval
** alpha = .05
** so alpha/2 = .025
display invttail(9,1-.05/2)


********************************************************
** Confidence Intervals
********************************************************

** Let's return to the idea that our population data
** is normally distributed with mean 12 and sd 4

** Now we are just going to take a sample from this distribution
** of size 100
set seed 02138
clear
set obs 100
drawnorm x, mean(12) sd(4)

** we can look at our estimate of the sample meand standard deviation
sum x

** Assuming we don't know sigma
** Let's construct a 95% confidence interval 
** Remember formula from lecture xbar - 1.96*(s/sqrt(n))

** First, we store the estimate of the sample mean
scalar xmean = r(mean)

** Then we store the estimate of the sample standard deviation
scalar xsd = r(sd)

** Calculate lower and upper bounds
scalar lbound = xmean - 1.96*(xsd/sqrt(100))
scalar ubound = xmean + 1.96*(xsd/sqrt(100))

** We can look at our bounds
display lbound 
display ubound
* so our estimated confidence interval is [11.6,13.3]


** We could also get this using the confidence interval command
** we specify our variable and the confidence level we want
ci x, level(95)



********************************************************
** p-Values
********************************************************

** say our set-up is
** H0: mu=0
** HA: mu!=0
** T=xbar-mu.o/s/sqrtn

** Let's go back to our sample of 100
** we know that the population mean is 12, so we should be able to reject 
** the null that it is 0

** what is our test statistic?
scalar test_statistic = xmean /(xsd/sqrt(100))
display test_statistic

** so what is the probability that we would see a value at least 
** this extreme?
* we multiply by 2 since our alternative hypothesis means extreme 
** could be pos or neg
scalar p_value = 2*(1 - normal(test_statistic))
display p_value
* 0 as expected!








