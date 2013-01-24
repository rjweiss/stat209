library(multilevel)
library(mediation)

data = read.table("http://www-stat.stanford.edu/~rag/stat209/coleman.dat", header=T)

# Let us take another look at the momed, this time asking the more 
# modern question of a mediating variable in the presumed effects of 
# momed on child verbal achievement. Investigate tverb as a mediating 
# variable; obtain an estimate of the amount of mediation due to tverb. 
# Also obtain the asymptotic standard error for the amount of mediation.

attach(data)

#using 'mediation'

yx = lm(vach ~ momed + tverb)
mx = lm(tverb ~ momed)
analysis = mediate(mx, yx, treat="momed", mediator="tverb") #mediate is slow!
summary(analysis) #cool, matches sobel test output
plot(analysis)

sobel(momed, tverb, vach) #large and significant
