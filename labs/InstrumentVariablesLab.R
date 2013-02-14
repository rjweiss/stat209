#####################################
#Lab 3 Instrumental variables Stat 209  2/13/13 (see also Rogosa session)
#####################################

#*********************************************
# Task 0:  Get the data
#*********************************************
mroz87 =  read.table( "http://www-stat.stanford.edu/~rag/stat209/Mroz87.dat", header = T)
names( mroz87)
# lfp Dummy variable for labor-force participation.
# hours Wife's hours of work in 1975.
# kids5 Number of children 5 years old or younger.
# kids618 Number of children 6 to 18 years old.
# age Wife's age.
# educ Wife's educational attainment, in years.
# wage Wife's average hourly earnings, in 1975 dollars.
# repwage Wife's wage reported at the time of the 1976 interview.
# hushrs Husband's hours worked in 1975.
# husage Husband's age.
# huseduc Husband's educational attainment, in years.
# huswage Husband's wage, in 1975 dollars.
# faminc Family income, in 1975 dollars.
# mtr Marginal tax rate facing the wife.
# motheduc Wife's mother's educational attainment, in years.
# fatheduc Wife's father's educational attainment, in years.
# unem Unemployment rate in county of residence, in percentage points.
# city Dummy variable = 1 if live in large city, else 0.
# exper Actual years of wife's previous labor market experience.
# nwifeinc Non-wife income.
# wifecoll Dummy variable for wife's college attendance.
# huscoll Dummy variable for husband's college attendance.


#*********************************************
# Task1: Single instrument example
#*********************************************

###
# 1.  Load the sem library  (or alternatively use AER)
###
library(sem)
###
# 2.  Read help files for tsls function
###
help(tsls)
###
# 3.  Model log wage as a linear function of
# education
###

# Take a subset of the data with wage > 0
posWage =  subset( mroz87, wage > 0 )
posWage$logWage =  log( posWage$wage )
summary( posWage$logWage )
# Fit a linear model (using OLS)
lm.posWage =  lm( logWage ~ educ, data = posWage )
summary( lm.posWage )

###
# 4.  Use fatheduc as an instrumental variable
###
# Suppose we suspect that an unobserved variable such as ability should be included in the model
# If we also believe that education and ability are correlated, then we can use
# the method of instrumental variables to get a consistent estimate of the educ coefficient.

# fatheduc as an instrumental variable
# First check that educ and fatheduc are correlated.  
lm.educ =  lm( educ ~ fatheduc, data = posWage )
summary( lm.educ )
# The t-statistic shows that educ and fatheduc are correlated.

###
# 5.  Fit the instrumental variable model
###
instr.posWage =  tsls( logWage ~ educ, instruments =~ fatheduc, data = posWage )
# regression formula:  logWage ~ educ
# specify instruments:  instruments=~ fatheduc
# specify the data frame:  data = posWage
summary( instr.posWage )


###
# 6.  Notice the difference in coefficient estimates and
# also the difference in standard errors
###
summary( lm.posWage )
summary( instr.posWage )

###
# 7.  Check that the instrumental variable coefficient estimates
# can be calculated as follows
###

# slope
slope =  cov(posWage$logWage,posWage$fatheduc)/cov(posWage$educ,posWage$fatheduc)
slope
# intercept
ivint = mean(posWage$logWage) - slope*mean(posWage$educ)
ivint


###
# 8.  Two-stage least squares estimates can be computed
# using two calls to lm.  
###

lm.1 =  lm( educ ~ fatheduc, data = posWage )
lm.2 =  lm( logWage ~ lm.1$fitted, data = posWage )
summary( lm.2 )

#*********************************************
# Task2:  Multiple instruments
#*********************************************

###
# 1.  Model logWage as a linear function of educ, exper and exper^2
###
# Create a variable for exper^2 and add it to the posWage data frame
posWage$exper2 =  posWage$exper^2

lm.posWage.2 =  lm( logWage ~ educ + exper + exper2, data = posWage)
summary( lm.posWage.2 )

###
# 2.  Use motheduc, fatheduc as instruments, assuming exper and exper^2 are exogenous variables
###

# First check that educ is correlated with motheduc and fatheduc
lm.educ.2 =  lm( educ ~ fatheduc + motheduc, data = posWage )
summary( lm.educ.2 )
lm.educ.3 =  update( lm.educ.2, .~. -fatheduc - motheduc, data = posWage )
anova( lm.educ.3, lm.educ.2 )
# The F test shows that fatheduc and motheduc are correlated with educ.

# Now fit the multiple instruments model
instr.posWage2 =  tsls( logWage ~ educ + exper + exper2, instruments =~ exper + exper2 + fatheduc + motheduc, data = posWage )
summary( instr.posWage2 )

#*********************************************
# Task3:  Simultaneous equations
#*********************************************

# Labor supply of married working women.  
# hours = alpha1*logWage + beta10 + beta11*educ + beta12*age + beta13*kids5 + beta14*nwifeinc + u1
# logWage = alpha2*hours + beta20 + beta21*educ + beta22*exper + beta23*exper^2 + u2

###
# 1. Assume all variables except hours and logWage are exogenous.
# Use educ, age, kids5, nwifeinc, exper and exper^2 as instruments, check indentifiability 
###
# scan of Wooldridge p.562 at http://www-stat.stanford.edu/~rag/stat209/woolp562.pdf
# Instruments for each equation must have distinct members
# The first equation can be estimated provided beta22 and/or beta23 are non-zero.
lm.hours =  lm( hours ~ educ + age + kids5 + nwifeinc + exper + exper2, data = posWage )
lm.hours2 =  update( lm.hours, .~. -exper - exper2 )
anova( lm.hours2, lm.hours )
# The F-test is significant

# The second equation can be estimated provided beta12 and/or beta13 and/or beta14 are non-zero.
lm.logWage =  lm( logWage ~ educ + exper + exper2 + educ + age + kids5 + nwifeinc, data = posWage )
lm.logWage2 =  update( lm.logWage, .~. -educ - age - kids5 - nwifeinc )
anova( lm.logWage2, lm.logWage)
# The F-test is significant

###
# 2.  Now fit the simultaneous modeling equations
###
sem.hours =  tsls( hours ~ logWage + educ + age + kids5 + nwifeinc, instruments =~ educ + age + kids5 + nwifeinc + exper + exper2, data = posWage )
summary( sem.hours )

sem.logWage =  tsls( logWage ~ hours + educ + exper + exper2, instruments =~ educ + exper + exper2 + age + kids5 + nwifeinc, data = posWage )
summary( sem.logWage )



End Lab 3 2013





#*********************************************
# For reference R does LISREL:  RAM formulation of LISREL models
#*********************************************

# p.472-479
http://socserv.mcmaster.ca/jfox/Misc/sem/SEM-paper.pdf
#Stability of Alienation example, used by LISREL (Joreskog)
#and Proc CALIS (SAS) etc
# also Structural Equation Models Appendix to 
# An R and S-PLUS Companion to Applied Regression
John Fox January 2002 
http://cran.r-project.org/doc/contrib/Fox-Companion/appendix-sems.pdf
Additional references and front-end tools linked in Week 6 resources,
a number of recent projects

