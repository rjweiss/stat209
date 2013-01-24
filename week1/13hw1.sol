Stat209 D Rogosa   1/19/13

               Solutions Assignment 1. 

-----------------------
Lab1 posted, provides more practice on the basics (and Week 1 Week 2)
----------------------

Assignment 1.
Review regression, properties of regression coefficients


1. Yule's Data via Freedman (deep review regression)
The purpose here is just to get started reading in data and
doing standard regression fits. Also to remind us that Yule(1989) 
was doing much the same sort of analysis as is done more than 100 years 
later.


File yuledoc.dat contains the
data in Table 3, p.10 of Freedman (1871 to 1881 comp)
or described sec. 4 (p.6) of linked "Association to Causation" and elsewhere)
http://www-stat.stanford.edu/~rag/stat209/yuledoc.dat
Note that I commmented out the two leading lines of text in that data file
with leading "#" so that the file would read without modification.
But it is always good to look at the data file before attempting
statistical analysis.


a. replicate Yule's regression equation for
the metropolitan unions, 1871--81.  See chapter 1.
(Subtract 100 from each entry to get the percent changes.)
I scanned and posted p10-11 of Freedman's text for reference
on the variables and fit
http://www-stat.stanford.edu/~rag/stat209/DAFp10.pdf
----------------------------------------
Arithmetic addendum
Example: a variable has value 50 in 1870 and 60 in 1880
that's an increase of 10units or 20% (the metric used in
the regression equation)
The data in yuledoc.dat reside in a somewhat cryptic form:
in this case the entry would be (60/50)*100 = 120. So
we obtain the (desired) 20% entry by subtracting 100
from the value in yuledoc.dat.
Example 2. value 70 in 1870 and 56 in 1880
yuledoc.dat entry would be (56/70)*100 = 80.
Subtract 100 to get -20 (20% decline)
---------------------------------------
The results, shown below, are similar to those reported
p.11 but don't match perfectly (rounding, matrix inversion etc) 


> yule = read.table("http://www-stat.stanford.edu/~rag/stat209/yuledoc.dat", header = T)
> yule
   paup outrelief old pop
1    27         5 104 136
2    47        12 115 111
3    31        21  85 174
4    64        21  81 124
5    46        18 113  96
6    52        27 105  91
7    81        36 100  97
8    61        39 103 141
9    61        35 101 107
10   59        35 101 132
11   33        22  91 150
12   76        30 103  85
13   64        27  97  81
14   79        33  95  93
15   79        64 113  68
16   52        21 108 100
17   46        19 102 106
18   35         6  93  93
19   37         6  98  98
20   34        10  87 101
21   43        15 102 113
22   37        20 102 135
23   52        22 100 111
24   57        32 102 110
25   57        38  99 122
26   23        18  91 168
27   30        14  83 168
28   55        37  94 131
29   41        24 100 142
30   76        20 119 110
31   38        29 101 142
32   38        49  86 203
> cor(yule) # a quick overview of association, outrelief is the policy variable
                paup   outrelief        old         pop
paup       1.0000000  0.59403244  0.3952942 -0.59343318
outrelief  0.5940324  1.00000000  0.1088055 -0.01223797
old        0.3952942  0.10880553  1.0000000 -0.52813161
pop       -0.5934332 -0.01223797 -0.5281316  1.00000000
> pairs(yule) # gives you the scatterplot array
> attach(yule) #so that you can use var names directly
> # to go to percentage change over time
> # transform variables
> paupPercent =  paup - 100
> outreliefPercent =  outrelief - 100
> oldPercent =  old - 100
> popPercent =  pop - 100
> lm.yule =  lm( paupPercent ~ outreliefPercent + oldPercent + popPercent )
> summary(lm.yule)

Call:
lm(formula = paupPercent ~ outreliefPercent + oldPercent + popPercent)

Residuals:
    Min      1Q  Median      3Q     Max 
-17.475  -5.311  -1.829   3.132  25.335 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)      12.88356   10.36722   1.243    0.224    
outreliefPercent  0.75209    0.13499   5.572 5.83e-06 ***
oldPercent        0.05560    0.22336   0.249    0.805    
popPercent       -0.31074    0.06685  -4.648 7.25e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 9.547 on 28 degrees of freedom
Multiple R-Squared: 0.6972,     Adjusted R-squared: 0.6647 
F-statistic: 21.49 on 3 and 28 DF,  p-value: 2.001e-07 

> fitted(lm.yule) # the 'y-hats' from the multiple regression
         1          2          3          4          5          6          7          8 
-69.529594 -55.884852 -70.360576 -55.046068 -46.822415 -38.944688 -34.318278 -45.567674 
         9         10         11         12         13         14         15         16 
-38.122154 -45.890612 -61.817150 -34.935179 -36.282121 -35.609618  -3.525394 -46.087094 
        17         18         19         20         21         22         23         24 
-49.789325 -56.027374 -57.303055 -55.838514 -54.972871 -58.048642 -49.197937 -41.255050 
        25         26         27         28         29         30         31         32 
-40.638149 -70.418818 -73.872012 -44.464898 -57.326636 -49.334950 -53.510562 -58.257739 
> #so row 1 (kensington has fitted value for deltPaup of -69.53 which in text is -70
> # and actual value is 27-100=-73, a good decrease
> # coeff of outrelief is large significant and positive; 
    does out-relief (welfare) cause poverty? (Yule conclusion)


b. composite hypothesis (orig verFreedman p.63, problem 4)
Testing the null hypothesis, c = d = 0 (coefficients for Old and Pop)
against the alternative, c != 0 or d != 0.  These two models are nested.  
Therefore, we can conduct an F-test.

This is a comparison of full and reduced models (often done
equivalently by increment to R^2 tests in the social sciences)
Reduced model has c=d=0

> #reduced model
> lm.yule2 =  lm( paupPercent ~ outreliefPercent)
> summary(lm.yule2)

Call:
lm(formula = paupPercent ~ outreliefPercent)

Residuals:
     Min       1Q   Median       3Q      Max 
-30.5935  -8.0520   0.3066   4.2634  29.6028 

Coefficients:
                 Estimate Std. Error t value Pr(>|t|)    
(Intercept)        7.6283    14.3623   0.531 0.599237    
outreliefPercent   0.7654     0.1892   4.045 0.000338 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 13.48 on 30 degrees of freedom
Multiple R-Squared: 0.3529,     Adjusted R-squared: 0.3313 
F-statistic: 16.36 on 1 and 30 DF,  p-value: 0.0003377 

> # compare the nested models lm.yule done above
> anova( lm.yule2, lm.yule ) # this is a nice feature of R
Analysis of Variance Table

Model 1: paupPercent ~ outreliefPercent
Model 2: paupPercent ~ outreliefPercent + oldPercent + popPercent
  Res.Df    RSS Df Sum of Sq      F    Pr(>F)    
1     30 5453.5                                  
2     28 2551.9  2    2901.6 15.918 2.414e-05 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 
> 

clearly reject composite null hypothesis for regression parameters c=d=0
test statistic 15.9 df 2,28

old-fashioned increment-to-R-squared arithmetic
0.6972	- 0.3529	= 0.3443	
0.3443	/2	= 0.17215	
0.17215	/0.3028	= 0.568527080581241664	
0.568527080581241792	* 28	= 15.918  
#matches the nested model comparison from anova command (as it should)
#anova works with more complex models, glm etc


--------------------------------------------------------
----------------------------------------------------------

2.  More on the Coleman example.
The class sample of only 20
schools (with 5 predictors) is a toy example and just
two predictors have statistically significant coefficients
(for a variety of reasons including sample size).
So I created a larger artificial data set, with 320 rows,
with the same mean and covariances as the n=20 sample.
http://www-stat.stanford.edu/~rag/stat209/coleman320.dat
Repeat the multiple regression and adjusted variables
demonstration for momed. Also plot outcome (or adjusted
outcome) vs the adjusted predictor (residuals from momed
on the other predictors), the scatterplot for the
multiple regression weight (as was done in the class materials).



Computation note, data generation
to create the artificial data set with 320 rows
I used the mvrnorm function in R, which requires the
package MASS--  standard with new distributions
so you don't need to do the install.pacakages just the library
statement, you have it if the command
help(mvrnorm)
gives you a helpfile



> library(MASS)
then to obtain a sample of 320 drawn from a mvnormal population
with mean and cov the same as the n=20 sample (ed here)
> eddat320 = mvrnorm(n = 320, mean(ed), cov(ed), tol = 1e-6, empirical = FALSE)
# empirical = TRUE would exactly match up the sample moments

-------------------
We repeat the multiple regression and adjusted variable demonstration for
momed.  We plot outcome versus the residuals. The larger sample size (320 vs 20)
makes the negative coefficient for momed significant for these artificial data

R Code:  # we broke up input and output in an attempt to give a clear look at R-commands
#read in data
coleman320dat =  read.table("http://www-stat.stanford.edu/~rag/stat209/coleman320.dat", header = T )
attach(coleman320dat)
#run the full multiple regression
lm.coleman320 =  lm( vach ~ ssal + whcol + ses + tverb + momed )
#create momed adjusted variable and use as a predictor
lm.momed =  lm( momed ~ ssal + whcol + ses + tverb )
lm.adj.momed =  lm( vach ~ residuals( lm.momed ) )
summary( lm.coleman320 )
summary( lm.adj.momed )
#plot for adjusted variable
plot( vach, residuals( lm.momed ) )

Output:
> summary( lm.coleman320 )

Call:
lm(formula = vach ~ ssal + whcol + ses + tverb + momed)

Residuals:
     Min       1Q   Median       3Q      Max
-5.11513 -1.14456 -0.07761  1.18590  4.38739

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept) 18.52972    2.65133   6.989 1.67e-11 ***
ssal        -1.56895    0.25062  -6.260 1.26e-09 ***
whcol        0.05115    0.01011   5.059 7.20e-07 ***
ses          0.54895    0.01782  30.813  < 2e-16 ***
tverb        1.17711    0.08900  13.227  < 2e-16 ***
momed       -1.97219    0.38614  -5.107 5.68e-07 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 1.732 on 314 degrees of freedom
Multiple R-Squared: 0.9189,     Adjusted R-squared: 0.9176
F-statistic: 711.1 on 5 and 314 DF,  p-value: < 2.2e-16

> summary( lm.adj.momed )

Call:
lm(formula = vach ~ residuals(lm.momed))

Residuals:
     Min       1Q   Median       3Q      Max
-14.7169  -4.2035  -0.4736   3.7459  21.1857

Coefficients:
                    Estimate Std. Error t value Pr(>|t|)
(Intercept)          35.0105     0.3366 103.997   <2e-16 ***
residuals(lm.momed)  -1.9722     1.3425  -1.469    0.143
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

Residual standard error: 6.022 on 318 degrees of freedom
Multiple R-Squared: 0.006741,   Adjusted R-squared: 0.003618
F-statistic: 2.158 on 1 and 318 DF,  p-value: 0.1428

#multiple regression coeff and adjusted variable coeff -1.9722 match

To obtain the adjusted variable plot (following the class example) 
for the momed coeff

plot(residuals(lm.momed), vach, pch = 20)
  abline(lm.adj.momed)

-----------------------------
A little more on this to follow-up on a promise/assertion in-class 
> #also adjust vach for 'other' predictors (often done in texts)
> lm.vach =  lm( vach ~ ssal + whcol + ses + tverb )
> lm.adj.momed2 =  lm( residuals(lm.vach) ~ residuals( lm.momed ) )
> summary(lm.adj.momed2)

Call:
lm(formula = residuals(lm.vach) ~ residuals(lm.momed))

Residuals:
    Min      1Q  Median      3Q     Max 
-5.1151 -1.1446 -0.0776  1.1859  4.3874 

Coefficients:
                      Estimate Std. Error t value Pr(>|t|)    
(Intercept)          1.241e-17  9.622e-02    0.00        1    
residuals(lm.momed) -1.972e+00  3.837e-01   -5.14 4.81e-07 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 

Residual standard error: 1.721 on 318 degrees of freedom
Multiple R-squared: 0.0767,     Adjusted R-squared: 0.0738 
F-statistic: 26.42 on 1 and 318 DF,  p-value: 4.81e-07 

> this comes alot closer to the standard error for the momed coeff in the coleman320
> # multiple regression. (the very small discrepancy is just a d.f. issue)

-------------------------------------------------------------

3. The "Regression Recursion" slide (useful trick) from class generated questions so is
worth revisiting.
Take the second version (using vars labelled 1 2 3), and use from the Coleman data
vach as var1, momed as var2, and ses as var3. Demonstrate that this relation holds in the
sample for the parameter estimates from the corresponding regressions.

fit the regressions and do the arithmetic
> p3dat = read.table("http://www-stat.stanford.edu/~rag/stat209/coleman.dat", header = T )
> attach(p3dat)
> cor(p3dat)
           ssal      whcol       ses      tverb     momed      vach
ssal  1.0000000 0.18113980 0.2296278 0.50266385 0.1967731 0.1922916
whcol 0.1811398 1.00000000 0.8271829 0.05105812 0.9271008 0.7534008
ses   0.2296278 0.82718291 1.0000000 0.18332924 0.8190633 0.9271611
tverb 0.5026638 0.05105812 0.1833292 1.00000000 0.1238087 0.3336495
momed 0.1967731 0.92710081 0.8190633 0.12380866 1.0000000 0.7329859
vach  0.1922916 0.75340081 0.9271611 0.33364951 0.7329859 1.0000000

> lm1 = lm(vach ~ momed)
> lm2 = lm(vach ~ momed +ses)
> lm3 = lm(ses ~ momed)
> coef(lm1)
(Intercept)       momed 
  -5.677808    6.516436 
> coef(lm2)
(Intercept)       momed         ses 
 37.6613964  -0.7135687   0.6000560 
> coef(lm3)
(Intercept)       momed 
  -72.22527    12.04888 

> coef(lm1)[2]
   momed 
6.516436 

> -.7136 + 12.05*.6000 # recursion substitution
[1] 6.5164  # it matches in the sample



--------------------------------------------------------
4. Measurement error, linear regression

Construct a simple artificial data illustration of effects of measurement error
in the predictor variable on a regression slope. Result is shown in week 1 class handout.
Set the reliability coefficient for the predictor variable to be .8. Set the slope for
the perfectly measured predictor to be 1.5. Compare slope for perfectly measured predictor 
with the slope using the fallible predictor measurement.

A couple of ways of doing this exercise
a. generate true predictor values, predictor error, and outcome variable and do the two regressions
b. use mvnorm and generate (all at once) outcome, true predictor, fallible predictor, and do the two regressions
c. use the DAAG function errorsINx from 1/12 handout (linked in week 1 materials)

-----------------------------------
From the problem statement, the pop value of the slope for the fallible predictor 
(which I will call Z) is 1.2 = 1.5*.8
(attenuated version of the slope for the perfectly measured predictor, which I call X)
To get reliability .8 for the observed predictor let perfectly measured predictor have variance 16
(my arbitrary choice for convenience), then variance observed predictor needs to be 20 with 
      Z = X + error
and the measurement error has mean zero, variance 4 and is uncorrelated with X etc
The mean of Z is same as mean of X and we'll set that to 10 (my choice)
Also left to choice is the exact form of the perfectly measured regression;
for observed outcome Y problem sets slope  to 1.5, but we have to specify E(Y|X = 0), 
the intercept, and set that equal to 5. Thus mean of Y is 20.
If we set correlation of Y and X at .9, then then Y has variance 44.44 and the
residual variance in the straight-line regression E(Y|X) is 8.44.

That is, E(Y|X) = 5 + 1.5*X   and Var(Y|X) = 8.44

that's all the things we need to set (and a good review of Stat60, straight-line regression)

Let's go to R and do this the simplest (most tedious) way, choice (a) above


R version 2.14.1 (2011-12-22)

> 1.5*.8  #slope we expect for Y on Z
[1] 1.2

> ?rnorm
starting httpd help server ... done

> X = rnorm(10000, 10, sqrt(16)) #even n=10000 doesn't eliminate sampling wobble
> var(X)
[1] 15.83535
> summary(X)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -6.491   7.347   9.940   9.989  12.740  24.760 

> disturb = rnorm(10000,0, sqrt(8.4444444)) #for the YonX regression
> var(disturb)
[1] 8.597265


> Y = 5 + 1.5*X + disturb     #create the outcome
> var(Y)
[1] 43.80098
> cor(X,Y)
[1] 0.8965204

> errZ = rnorm(10000,0, sqrt(4))
> Z = X + errZ    #form the fallible predictor
> var(Z)
[1] 19.71366

> var(X)/var(Z)   #Reliability coeff for Z, target .8
[1] 0.8032681


> YonX = lm(Y ~ X)  # do the regressions
> YonZ = lm(Y ~ Z)

> summary(YonX)
Call:
lm(formula = Y ~ X)
Residuals:
    Min      1Q  Median      3Q     Max 
-11.748  -1.980   0.002   1.991  12.443 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 5.028349   0.079229   63.47   <2e-16 ***
X           1.491036   0.007368  202.35   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
Residual standard error: 2.932 on 9998 degrees of freedom
Multiple R-squared: 0.8037,     Adjusted R-squared: 0.8037 
F-statistic: 4.095e+04 on 1 and 9998 DF,  p-value: < 2.2e-16 

> coef(YonX)[2]
       X 
1.491036 

> summary(YonZ)
Call:
lm(formula = Y ~ Z)
Residuals:
    Min      1Q  Median      3Q     Max 
-16.617  -2.673   0.023   2.665  16.139 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  7.97226    0.09700   82.19   <2e-16 ***
Z            1.19728    0.00888  134.83   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
Residual standard error: 3.943 on 9998 degrees of freedom
Multiple R-squared: 0.6452,     Adjusted R-squared: 0.6451 
F-statistic: 1.818e+04 on 1 and 9998 DF,  p-value: < 2.2e-16 

> coef(YonZ)[2]/coef(YonX)[2]  #ratio of regression coeffs should be the Reliability Coeff
        Z                     # see class handout 1/12
0.8029865 
> 
--------------------------------------------------------
----------------------------------------------------------

(b) Option b is to repeat this demonstration with data generation
by mvrnorm which allows matching sample generated to specified population moments 


> library(MASS)
> ?mvrnorm
> p3mean = c(20,10,10)  # mean vector  (Y,X,Z)

> p3cov = matrix(c(44.444, 1.5*16, 1.5*16, 1.5*16, 16,16, 1.5*16, 16, 20 ),3,3) #covariance matrix
> p3cov
       [,1] [,2] [,3]
[1,] 44.444   24   24
[2,] 24.000   16   16
[3,] 24.000   16   20

> p3dat = mvrnorm(n=10000, p3mean, p3cov, empirical =TRUE)

> head(p3dat)
          [,1]      [,2]      [,3]
[1,] 15.857164  6.184050  7.172891
[2,] 19.713746 11.262252 12.589756
[3,] 22.027241 13.470027 15.757616
[4,]  6.646021  2.610674  4.111276
[5,] 15.212193  8.231687  8.082325
[6,] 14.744156  8.195988 11.157956

> summary(p3dat)
       V1              V2               V3        
 Min.   :-3.89   Min.   :-3.914   Min.   :-6.441  
 1st Qu.:15.46   1st Qu.: 7.284   1st Qu.: 6.935  
 Median :20.10   Median :10.064   Median :10.025  
 Mean   :20.00   Mean   :10.000   Mean   :10.000  
 3rd Qu.:24.55   3rd Qu.:12.747   3rd Qu.:13.047  
 Max.   :45.03   Max.   :24.616   Max.   :26.977


> cov(p3dat)           #matches exactly
       [,1] [,2] [,3]
[1,] 44.444   24   24
[2,] 24.000   16   16
[3,] 24.000   16   20

> var(p3dat[,2])/var(p3dat[,3])
[1] 0.8

> YonXb = lm(p3dat[,1] ~ p3dat[,2])
> summary(YonXb)
Call:
lm(formula = p3dat[, 1] ~ p3dat[, 2])
Residuals:
     Min       1Q   Median       3Q      Max 
-10.7734  -1.9712   0.0132   1.9748  11.9449 
Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 5.000000   0.078250    63.9   <2e-16 ***
p3dat[, 2]  1.500000   0.007265   206.5   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
Residual standard error: 2.906 on 9998 degrees of freedom
Multiple R-squared:  0.81,      Adjusted R-squared:  0.81 
F-statistic: 4.263e+04 on 1 and 9998 DF,  p-value: < 2.2e-16 

> YonZb = lm(p3dat[,1] ~ p3dat[,3])
> summary(YonZb)
Call:
lm(formula = p3dat[, 1] ~ p3dat[, 3])

Residuals:
     Min       1Q   Median       3Q      Max 
-16.0482  -2.7005  -0.0027   2.6834  15.2990 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) 8.000000   0.096892   82.57   <2e-16 ***
p3dat[, 3]  1.200000   0.008845  135.67   <2e-16 ***
---
Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1 
Residual standard error: 3.955 on 9998 degrees of freedom
Multiple R-squared: 0.648,      Adjusted R-squared: 0.648 
F-statistic: 1.841e+04 on 1 and 9998 DF,  p-value: < 2.2e-16 

> 1.2/1.5  ratio of slopes matches result exactly
[1] 0.8
> 
----------------
c. I'll do this via DAAG function when I get a chance
=====================================
end HW1 solutions




















































