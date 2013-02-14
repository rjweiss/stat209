Stat209/Ed260 D Rogosa   1/31/13

        Solutions Assignment 4.  Multilevel data, ecological fallacy

----------------------------------------------------------------------
NOTE: R multilevel (random effects) modelling (lme) exercises in Lab 2
----------------------------------------------------------------------

Problem 1

So this is another data generation exercise, with the purpose of illustrating
the multiple regression multi-level analyses in week 4 Tues lecture
A bit tedious, but at least be familiar with the analysis options
and what those give you.

> #  grouping problem
> library(MASS)   # MASS is part of  R-distribution, 

> x = c(1:200)  # integers 1 to 200
> ?sample  # to see how sample works
> var(x)
[1] 3350

> u = rnorm(200,0,70)
> y = 10 + x +u #this gives me theoretical x,y correlation .637 
> cor(x,y)
[1] 0.6389937         #pretty close

> totreg = lm(y ~ x)  #individual level regression, theoretical slope 1.0
> summary(totreg)

Call:
lm(formula = y ~ x)

Residuals:
     Min       1Q   Median       3Q      Max 
-231.923  -50.030   -2.716   50.311  210.550 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   6.6415     9.9208   0.669    0.504    
x             1.0006     0.0856  11.689   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 69.89 on 198 degrees of freedom
Multiple R-Squared: 0.4083,     Adjusted R-squared: 0.4053 
F-statistic: 136.6 on 1 and 198 DF,  p-value: < 2.2e-16 


> #group by x into 10 groups of 20 on the basis of x

#here's a crude do loop for clumping the x's and getting group means,
there's likely a fancier way
trial-and-error revealed a surprising (to me) syntax for this in the indices
easier take elements mean(x[1:20]) etc

> xBar = c()
> for(i in 1:10) {
+ xBar[i] = mean(x[(i - 1)*20 + 1: 20]) }
> xBar
 [1]  10.5  30.5  50.5  70.5  90.5 110.5 130.5 150.5 170.5 190.5


> var(xBar)              #var uses n-1 in denom, try ?var
[1] 3666.667
> var(x)
[1] 3350
> var(xBar)*.9/var(x) # approximately eta-sq in the sample
[1] 0.9850746            # so eta-sq essentially 1 because we grouped on x

> 
> yBar = c()
> for(i in 1:10) {
+ yBar[i] = mean(y[(i - 1)*20 + 1: 20]) }
> yBar
 [1]  -6.75261  55.07633  58.72489  91.47110  78.39501 137.13548 134.15138 141.41726 198.67811
[10] 183.66834
> mean(y[1:20])
[1] -6.75261
> mean(y[21:40])
[1] 55.07633
> mean(y[41:60])
[1] 58.72489

#between group regression (10 observations)
> betwreg = lm(yBar ~ xBar)
> summary(betwreg)

Call:
lm(formula = yBar ~ xBar)

Residuals:
     Min       1Q   Median       3Q      Max 
-23.4243 -15.5664  -0.7001  17.3284  21.0734 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)   6.1105    11.8190   0.517    0.619    
xBar          1.0058     0.1021   9.851 9.49e-06 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 18.55 on 8 degrees of freedom
Multiple R-Squared: 0.9238,     Adjusted R-squared: 0.9143 
F-statistic: 97.05 on 1 and 8 DF,  p-value: 9.49e-06 


# for relative standing easiest to make a vector of length 200 where
each individual's attribute is the group mean.
# I did this in the crudest possible way, laughably crude

> xBar200 = c(rep(xBar[1],20), rep(xBar[2],20),rep(xBar[3],20),rep(xBar[4],20),rep(xBar[5],20),rep(xBar[6],20),rep(xBar[7],20),rep(xBar[8],20),rep(xBar[9],20),rep(xBar[10],20) )
> xBar200
  [1]  10.5  10.5  10.5  10.5  10.5  10.5  10.5  10.5  10.5  10.5  10.5  10.5  10.5  10.5  10.5  10.5  10.5  10.5  10.5  10.5  30.5  30.5  30.5  30.5  30.5  30.5  30.5  30.5  30.5  30.5
 [31]  30.5  30.5  30.5  30.5  30.5  30.5  30.5  30.5  30.5  30.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5  50.5
 [61]  70.5  70.5  70.5  70.5  70.5  70.5  70.5  70.5  70.5  70.5  70.5  70.5  70.5  70.5  70.5  70.5  70.5  70.5  70.5  70.5  90.5  90.5  90.5  90.5  90.5  90.5  90.5  90.5  90.5  90.5
 [91]  90.5  90.5  90.5  90.5  90.5  90.5  90.5  90.5  90.5  90.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5 110.5
[121] 130.5 130.5 130.5 130.5 130.5 130.5 130.5 130.5 130.5 130.5 130.5 130.5 130.5 130.5 130.5 130.5 130.5 130.5 130.5 130.5 150.5 150.5 150.5 150.5 150.5 150.5 150.5 150.5 150.5 150.5
[151] 150.5 150.5 150.5 150.5 150.5 150.5 150.5 150.5 150.5 150.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5 170.5
[181] 190.5 190.5 190.5 190.5 190.5 190.5 190.5 190.5 190.5 190.5 190.5 190.5 190.5 190.5 190.5 190.5 190.5 190.5 190.5 190.5

> yBar200 = c(rep(yBar[1],20), rep(yBar[2],20),rep(yBar[3],20),rep(yBar[4],20),rep(yBar[5],20),rep(yBar[6],20),rep(yBar[7],20),rep(yBar[8],20),rep(yBar[9],20),rep(yBar[10],20) )
> relx = x - xBar200
> rely = y - yBar200
# some description
> cor(relx, rely)
[1] 0.04065814
> summary(rely)
      Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
-2.274e+02 -4.637e+01 -5.717e+00  2.118e-15  5.049e+01  1.841e+02 
> summary(y)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
-115.90   42.77  110.60  107.20  169.40  363.60 
> summary(yBar)
   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
 -6.753  63.640 112.800 107.200 140.300 198.700 

#relative standing regression
> relreg = lm(rely ~ relx)
> summary(relreg)

Call:
lm(formula = rely ~ relx)

Residuals:
     Min       1Q   Median       3Q      Max 
-228.074  -46.075   -5.102   49.937  188.174 

Coefficients:
             Estimate Std. Error  t value Pr(>|t|)
(Intercept) 2.283e-15  4.794e+00 4.76e-16    1.000
relx        4.761e-01  8.314e-01    0.573    0.568

Residual standard error: 67.8 on 198 degrees of freedom
Multiple R-Squared: 0.001653,   Adjusted R-squared: -0.003389 
F-statistic: 0.3279 on 1 and 198 DF,  p-value: 0.5676 

> #multiple regression versions

> regcontext = lm(y ~ x + xBar200)
> summary(regcontext)

Call:
lm(formula = y ~ x + xBar200)

Residuals:
     Min       1Q   Median       3Q      Max 
-231.294  -52.950   -2.927   53.340  206.462 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)
(Intercept)   6.1105     9.9741   0.613    0.541
x             0.4761     0.8584   0.555    0.580
xBar200       0.5298     0.8627   0.614    0.540

Residual standard error: 70 on 197 degrees of freedom
Multiple R-Squared: 0.4094,     Adjusted R-squared: 0.4034 
F-statistic: 68.29 on 2 and 197 DF,  p-value: < 2.2e-16 

# .4761 coeff of x matches the relative standing slope (within-pooled)
# .5298 coeff of xBar matches between slope minus relative standing slope (context effect)


> regcronbach = lm(y ~ relx + xBar200)
> summary(regcronbach)

Call:
lm(formula = y ~ relx + xBar200)

Residuals:
     Min       1Q   Median       3Q      Max 
-231.294  -52.950   -2.927   53.340  206.462 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept)  6.11046    9.97406   0.613    0.541    
relx         0.47607    0.85837   0.555    0.580    
xBar200      1.00583    0.08616  11.674   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 70 on 197 degrees of freedom
Multiple R-Squared: 0.4094,     Adjusted R-squared: 0.4034 
F-statistic: 68.29 on 2 and 197 DF,  p-value: < 2.2e-16 

# .4761 coeff of x matches the relative standing slope (win-pooled)
# 1.0058 coeff of xBar matches between slope

Approximate Duncan-Cuzzort-Duncan relation
> .985*1.0058 + (1 - .985)*.476
[1] 0.997853
close to the total slope 1.00
-----------------------------------------------------------------

> # now try random grouping (part II) use "Rnd" suffix to indicate groups formed 
                                                random grouping (independent of x)
> #get indices
> k = 1:200
> rndind = sample(k , size = 200, p = rep(1:1,200)/200)
> rndind
  [1]  48  52 197  31  79  62  69 129 172  84  20 177  43 195 113  24  75  74 173 144  65 137  41 168   3 185 103 141 107 170 184 198 101   8 126 180 153  32 196  80  95 183 124  56  18
 [46]   4 169  67 167  28 138 109 146  37  50 122 148 174  96 178  46  72 115 121  40  19  83  81 108  42 133 128 190  70 164 145 132  92  36 176  13  16 158  10   9  49  90 159 152 160
 [91]   1  78  11 110 182   7  58 188 147 179  45 162  91 130 155 106  60 191  39  66  47 117  15  61 166 135 116  57 123 112 140  54  86  44   2 114 163  51  21  14  82  26 154 189  29
[136]  55 118 139  27 200 161  22 102 199 150   6  35 143 157 104  64  53 165 131 105  98 151  89 149  85  73  59  12  30  25 136  88   5 134 156 119  93 171 186  68  87 194  97 192  94
[181] 142  17 100  33 187  71 193 181  77 125  99 120  34  38  63  23 111 127  76 175

#what I did here is draw a random ordering of the integers 1:200 that I'll
use to create groups for part II (Rnd suffix)

now create the groups
> xBarRnd = c()
> for(i in 1:10) {
+ xBarRnd[i] = mean(x[rndind[(i -1)*20 + 1:20]])  }

#result shows xBar rather similar across the 10 groups
> xBarRnd
 [1]  98.05 118.90 105.45  99.65  88.85  99.70  85.40 108.45 100.95  99.60
> var(xBarRnd)
[1] 88.70833
> var(x)
[1] 3350
> var(xBarRnd)/var(x)
[1] 0.0264801
very small value of the eta-sq
> #grouping very small (groups not formed on x)

> yBarRnd = c()
> for(i in 1:10) {
+ yBarRnd[i] = mean(y[rndind[(i -1)*20 + 1:20]])  }

> yBarRnd
 [1] 126.79351 135.30195  91.02650  76.22435  88.59928  96.39977  71.77486 147.88802 107.30942 130.64764

> cor(xBarRnd, yBarRnd)
[1] 0.6581125
> cor(x,y)
[1] 0.6389937
> cor(xBar, yBar)
[1] 0.9611697
# grouping on X gave a much higher between-group correlation

> betwregRnd = lm(yBarRnd ~ xBarRnd)
> summary(betwregRnd)

Call:
lm(formula = yBarRnd ~ xBarRnd)

Residuals:
    Min      1Q  Median      3Q     Max 
-29.399  -8.854  -3.339  18.842  25.974 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)  
(Intercept) -78.8572    75.5530  -1.044   0.3271  
xBarRnd       1.8513     0.7488   2.472   0.0386 *
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 21.16 on 8 degrees of freedom
Multiple R-Squared: 0.4331,     Adjusted R-squared: 0.3623 
F-statistic: 6.112 on 1 and 8 DF,  p-value: 0.03857 

#between slope is larger from random grouping than under grouping on X
(because var(Xbar) is smaller)

> plot(xBarRnd, yBarRnd)

#now create the elongated group mean vector
> xBarRnd200 = c(rep(xBarRnd[1],20), rep(xBarRnd[2],20),rep(xBarRnd[3],20),rep(xBarRnd[4],20),rep(xBarRnd[5],20),rep(xBarRnd[6],20),rep(xBarRnd[7],20),rep(xBarRnd[8],20),rep(xBarRnd[9],20),rep(xBarRnd[10],20) )
> yBarRnd200 = c(rep(yBarRnd[1],20), rep(yBarRnd[2],20),rep(yBarRnd[3],20),rep(yBarRnd[4],20),rep(yBarRnd[5],20),rep(yBarRnd[6],20),rep(yBarRnd[7],20),rep(yBarRnd[8],20),rep(yBarRnd[9],20),rep(yBarRnd[10],20) )

#relative standing measures
> relxRnd = x - xBarRnd200
> relyRnd = y - yBarRnd200
> relregRnd = lm(relyRnd ~ relxRnd)

> summary(relregRnd)

Call:
lm(formula = relyRnd ~ relxRnd)

Residuals:
      Min        1Q    Median        3Q       Max 
-208.6740  -50.4961    0.5453   50.0308  196.2732 

Coefficients:
             Estimate Std. Error  t value Pr(>|t|)    
(Intercept) 1.655e-15  5.305e+00 3.12e-16        1    
relxRnd     9.376e-01  8.757e-02    10.71   <2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 75.03 on 198 degrees of freedom
Multiple R-Squared: 0.3667,     Adjusted R-squared: 0.3635 
F-statistic: 114.6 on 1 and 198 DF,  p-value: < 2.2e-16 


#coeffs for random groups are 1.85 between and .938 within for
this ex, your artificial data will differ slightly
=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=
---------------------------------------------------------------------
Enrichment problem (better to spend time on Lab2 etc)
 Ecological fallacy: Is Radon good for you?

------------------------------------

Following p.750 Greenland and Robins American Journal of 
Epidemiology Vol. 139, No. 8: 747-760
Reading through their exposition probably is helpful

here's my attempt (it can be done more elegantly)


#make the region indices

> r = c(0:40)
> r
 [1]  0  1  2  3  4  5  6  7  8  9 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24
[26] 25 26 27 28 29 30 31 32 33 34 35 36 37 38 39 40

#now radon concentrations
> regradon = .1 + .3*sqrt(r)
> regradon
 [1] 0.1000000 0.4000000 0.5242641 0.6196152 0.7000000 0.7708204 0.8348469
 [8] 0.8937254 0.9485281 1.0000000 1.0486833 1.0949874 1.1392305 1.1816654
[15] 1.2224972 1.2618950 1.3000000 1.3369317 1.3727922 1.4076697 1.4416408
[22] 1.4747727 1.5071247 1.5387495 1.5696938 1.6000000 1.6297059 1.6588457
[29] 1.6874508 1.7155494 1.7431677 1.7703293 1.7970563 1.8233688 1.8492856
[36] 1.8748239 1.9000000 1.9248288 1.9493242 1.9734994 1.9973666

> # set up smoking variable (counterbalanced with radon)
> u = rnorm(41)
> var(u)
[1] 0.9883518
> mean(u)
[1] 0.1323434
> #that's a good enough u


> s = .2*(34 + .4*r -u) + .4*(13 - .2*r -.5*u)
> s
 [1] 11.65626 12.02922 11.45928 11.86984 11.96606 12.05849 11.54374 11.99844
 [9] 11.42576 12.42389 11.88704 12.42439 11.93378 12.39780 11.55852 11.78481
[17] 11.69073 11.74597 12.69344 12.01483 11.84285 10.74535 11.70950 12.54065
[25] 12.25184 12.75854 12.22308 12.12930 11.85289 12.27538 11.76754 11.37832
[33] 12.27040 12.22711 11.75117 11.99551 12.49188 12.12246 11.60682 11.72744
[41] 11.59924

#smoking proportions
> p0 = 53 - .2*r + 1.5*u
> p1 = 34 + .4*r  - u
> p2 = 13 - .2*r - .5*u

#eq2 p.751
> lungc = .40*(1 + .2*regradon)*exp(s/10)*(p0  + 2.7183*2.7183*p1 + exp(4)*p2)

#resulting data
> cor(lungc, s)
[1] 0.8518681
> cor(lungc, regradon)
[1] -0.2588493
> cor(s, regradon)
[1] 0.1254401
> plot( regradon, lungc)
#my plot is at 
http://www-stat.stanford.edu/~rag/stat209/hw4p1.pdf

#run the regression
> luncreg = lm(lungc ~ regradon + s)
> summary(luncreg)

Call:
lm(formula = lungc ~ regradon + s)

Residuals:
     Min       1Q   Median       3Q      Max 
-165.490  -28.550    9.463   38.072   77.763 

Coefficients:
            Estimate Std. Error t value Pr(>|t|)    
(Intercept) -2012.52     239.73  -8.395 3.48e-10 ***
regradon     -102.60      16.80  -6.107 4.06e-07 ***
s             298.39      20.21  14.768  < 2e-16 ***
---
Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1 

Residual standard error: 50.42 on 38 degrees of freedom
Multiple R-Squared: 0.8616,     Adjusted R-squared: 0.8543 
F-statistic: 118.2 on 2 and 38 DF,  p-value: < 2.2e-16 

even with confounder (smoking) "controlled for" [laughter supressed]
radon concentration negatively associated with lung cancer in this
regional-level regression. 
Causal conclusion--start pumping radioactive gas into everyone's basements?
(or be careful about ecological fallacy, and do analysis at individual
level)
=============================================================================

end HW4 solutions