********************************************************
********************************************************
** Gov 1000/2000e Section 3
** 9-19-13
** Soledad Artiz Prillaman
********************************************************
********************************************************

set more off

** As always, let's start by setting our working directory
cd "/Users/Sole/Dropbox/Gov 2000/2013/Sections/Section 3"
clear


********************************************************
** Added Variable Plots
********************************************************

** Let's load the data from last week
use "Leinhardt.dta", clear

** Just to revisit, we can use the avplot command to create 
** an added variable plot

** First we must run a regression
** generate numeric (not factor) variable for oil 
gen oilnum = 1
replace oilnum = 0 if oil==1 

** Our multiple regression model
reg linfant lincome oilnum

** We can use the avplot command to create added variable plots
** for each of our independent variables
avplot lincome
avplot oilnum 


********************************************************
** Drawing Discrete Distributions
********************************************************

** Let's clear our workspace and load in some data
** all this is is one variable with many possible outcomes 
** from a roll of a die
use dice_population , clear

** Let's check it out
sum dice
tab dice
** tab is a good command if you have a discrete variable

** We can preserve the data so that we can guarantee that 
** the data will be resored after we finish our session
preserve


** If we do not know what a distribution looks like,
** we can sample from the set of possible outcomes (the sample space)
** and then look at the distribution of the sample

** Again, think about rolling a die
** in this case, our variable dice provides us with the sample space

** Sample from the population
** i.e. Roll the "die" repeatedly
sample 10000 , count
** 1000000 is the sample size
** the rest of the data will be deleted (this is why we preserved it)
** the larger it is the more accurate will be your distribution
**    Prove it to yourself!
** count says that we are specifying the size of the sample as a count

** Plot the distribution of our variable (roll of a die)
hist dice , discrete

** restores the data to the point when we preserved it
restore


********************************************************
** Drawing Continuous Distributions
********************************************************

** Let's clear our work space
clear

** Like with discrete distributions, we can sample from the population
** to see what our distribution looks like

** Stata has a bunch of canned functions which make this easy

** Say we want to look at the distribution of salaries
** and we assume that salaries are normally distributed 
** with mean 40000 and standard deviation 10000

** It may be hard to conceptualize this distribution,
** So we can sample from it using the drawnorm command
** First, we set our sample size or the number of observations we want
set obs 1000000
drawnorm salary , mean (40000) sd (10000)

** Plot the distribution
kdensity salary






