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

YX.Z <- lm(Y ~ X + Z)

