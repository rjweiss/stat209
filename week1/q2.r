data = read.table('http://www-stat.stanford.edu/~rag/stat209/coleman320.dat', header=T)

#correlation plot
cor(data)

#models
modelVach = lm(vach ~ momed + ssal + whcol + ses + tverb, data = data)
modelMomed = lm(momed ~ ssal + whcol + ses + tverb, data = data)
momedResids = modelMomedReg$residuals
modelAdjMomed = lm(vach ~ momedResids, data = data)

#summaries
summary(modelVach)
summary(modelAdjMomed)

#plots
plot(momedResids, data$vach)
abline(modelAdjMomed)

