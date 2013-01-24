library(GGally) #for ggpairs

data = read.table('q2_data', head=T)
attach(data)

ggpairs(data[,2:4])

cor(data[,2:4]) #though the ggpair plot shows the correlations in the upper triangle

YX <- lm(Y ~ X)

cor.test(X, Y)
cor.test(X, Z)
cor.test(Y, Z)

XZ <- lm(X ~ Z)
YZ <- lm(Y ~ Z)

#partial correlation = fit the linear model and look at the cor between the residuals

cor(resid(XZ), resid(YZ))
cor.test(resid(XZ), resid(YZ))

YX.Z <- lm(Y ~ X + Z)

