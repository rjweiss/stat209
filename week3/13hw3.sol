Stat209/Ed260 D Rogosa   1/29/13

               Solutions Assignment 3. 


Problem 1, 

first part
refer to Freedman text  or Freedman "Statistical models for causation" linked
for week 3 readings
> # do Blau-Duncan path analysis from correlation matrices (cf Lab 1 and week 3 handouts)
# predictor correlation matrices
> RxxBD1 =  matrix( nrow = 2, ncol = 2, data = c(1, 0.516, .516, 1), byrow = T )
> RxxBD2 =  matrix( nrow = 2, ncol = 2, data = c(1, 0.438, .438, 1), byrow = T )
> RxxBD3 =  matrix( nrow = 3, ncol = 3, data = c(1, 0.438, .538, .438, 1, .417, .538,.417,1), byrow = T )
> RxxBD3
      [,1]  [,2]  [,3]
[1,] 1.000 0.438 0.538
[2,] 0.438 1.000 0.417
[3,] 0.538 0.417 1.000

# correlation matrices between outcome var and predictors
> RxyBD1 =  matrix( nrow = 2, ncol = 1, data = c(0.453,.438) )
> RxyBD2 =  matrix( nrow = 2, ncol = 1, data = c(0.538,.417) )
> RxyBD3 =  matrix( nrow = 3, ncol = 1, data = c(0.596,.405, .541) )
> #path coeffs for the 3 eqs pp.76-77 DAF text

# obtain the standardized regression coefficients
> pathcBD1 = t(RxyBD1)%*%solve(RxxBD1)
> pathcBD2 = t(RxyBD2)%*%solve(RxxBD2)
> pathcBD3 = t(RxyBD3)%*%solve(RxxBD3)
> pathcBD1
          [,1]      [,2]
[1,] 0.3093613 0.2783696
> pathcBD2
          [,1]      [,2]
[1,] 0.4397097 0.2244072

> pathcBD3
          [,1]      [,2]      [,3]
[1,] 0.3945428 0.1151266 0.2807282


> #coeffs match well p.76
> #get Rsq and coeff for distrurbance terms
> RsqBD1 = pathcBD1%*%RxyBD1
> sqrt(1-RsqBD1)
          [,1]
[1,] 0.8590305
> RsqBD2 = pathcBD2%*%RxyBD2
> sqrt(1-RsqBD2)
          [,1]
[1,] 0.8184488
> RsqBD3 = pathcBD3%*%RxyBD3
> sqrt(1-RsqBD3)
          [,1]
[1,] 0.7525638
> #coeff for disturbance terms matched (DAF SD's p.79)
> #in his solutions Freedman does these eq's more generally all at once, 
which is fine and that can be written in his noatation as
A1.  Y = [ U X W ]' [ e f g ] + eta = M' [e f g] + eta
The coefficient estimates are given by
[eHat fHat gHat] = (M'M)^(-1) M'y.
The matrix M'M is given by the correlation matrix of the predictors.
The value M'y is given by the correlations of the predictors with the
response.
The standard deviations can be calculated exactly as shown in the book on
page 79 (1st ed).

second part text problems
scan of pp.80-81 Freedman text at
  http://www-stat.stanford.edu/~rag/stat209/DAFtextp8081.pdf

A5.  We should measure variation by the standard deviation, not the
variance.  The reason is that variation is E[( X - \mu)^2], which is on the
scale of X^2, and has units of X^2.  The standard deviation is on the scale
of X, and has units of X.

A6.  Since there is an arrow indirectly from V to W that passes through U,
the model says that the father's education affects the son's first job by
affecting the son's education.  An arrow from V to W or V to Y would imply
that V has a direct affect on W or Y, respectively.  In principle, there is
nothing wrong with such a model.  However, the predictors would be highly
correlated, resulting in unstable estimates of the coefficients.
In principle, there could be an arrow from Y to V, which would be
interpreted as the son's occupation affects his father's education.  For example,
the son's occupation may require him to further his education in order to
work for his son's company. But then this model would be recursive, much
more complex to estimate.

prob 8
A8.  Let Es denote the education of the son, Ef the education of the father,
and Of the occupation of the father.  Equation 1 says, Es = a* Ef + b * Of.
If education is a 0-1 variable, then we would interpret the fitted values,
EsHat, as the probability that Es is equal to 1.  Since probabilities are
bounded by 0 and 1, we might question the validity of this equation because
the coefficients a and b can be chosen so that the response is unbounded.
In this situation a generalized linear model with binomial random component
(i.e. logistic regression) may be a better model.

freedman p.97 prob 4(a,b) [I forgot to provide a scan of this page...]
E4.  (a)  False.  The effect is indirect.  The father's education influences
the son's education which in turn influences the son's job.
(b)  True.

============================================================
Problem 2

I have used this path analysis in past exam problems 

Causal Models of Publishing Productivity

Homework 3 problem 2 considered one of the path
analysis models from "Causal Models of Publishing
Productivity in Psychology", Rogers & Maranto,
J. Applied Psychology, 1989, 74(4), 636-649.
direct link to paper
http://content.apa.org/journals/apl/74/4/636.pdf

The path analysis conducted by the authors
from a sample of 86 men and 76 women is shown
in p.101 of Freedman's text and on page 647
of the publication; that page also exists at
http://www-stat.stanford.edu/~rag/stat209/pathpage647.pdf

The diagram provides estimates of supposed causal effects ("causal
model of publishing" is the article); it displays regression coeffs
, with coefficient estimates shown on the edges.  
Consider a "productive
researcher" to be defined in terms of the number of publications and the
number of cites.  The good news is that ability "affects" pubs and cites with
a positive coefficient in each case.  Therefore, higher ability leads to a
more "productive researcher", according to the causal path gospel.
Some bad news is that sex is a predictor of pubs with a large coefficient
value.  However, it is likely that there are confounding variables between
sex and pubs.
One argument to discount the findings of the study is that only 86/241 = 36%
of  men and 76/244 = 31% of women responded to the survey.  This could
result in sample bias.

===================================================
Problem 3.

Method-of-moments for two-variable, two-indicator model

For the Structural Equation Models handout from Joreskog
book, obtain parameter estimates for the no-correlated error 
version (9 parameters, top covariance matrix) in terms of the 
sample variance and covariances among the four indicators (y_ij).
Brute force substitution will get you a non-optimal estimate,
suffices for instructional purposes.

solution (nicely formatted) at
http://www-stat.stanford.edu/~rag/stat209/hw3p5.pdf

----------------------
end HW3 solutions

































