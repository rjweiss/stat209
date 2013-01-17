library(memisc) #for mtable

data = read.table('http://www-stat.stanford.edu/~rag/stat209/yuledoc.dat', header=T)

#part A, replicating Yule

#subtract 100 from everything
deltaData = as.data.frame(sapply(data, function(x) x-100))
modelFull = lm(paup ~ outrelief + old + pop, data=deltaData)
modelFull
summary(modelFull)

#part B, testing coefficients
modelOld = lm(paup ~ old, data=deltaData)
modelPop = lm(paup ~ pop, data=deltaData)
modelOutrelief = lm(paup ~ outrelief, data=deltaData)
mtable(modelFull, modelOutrelief, modelOld, modelPop)
#old is not significant but pop is

anova(modelOutrelief, modelFull) #reject the null



