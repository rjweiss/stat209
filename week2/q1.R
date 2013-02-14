illness = c(-0.8,-0.16,-0.29,0.34)

exercise = c(1, -.03, .39, -.05)
hardiness = c(-.03, 1, .07, -.23)
fitness = c(.39, .07, 1, -.13)
stress = c(-.05, -.23,-.13, 1)

pred_mat = cbind(exercise, hardiness, fitness, stress)

coefs = solve(pred_mat, illness_y) #same as solve(pred_mat) %*% illness
coefs_st = c(0.03, -0.07, -.26, .29)

#estimating the mediation effect
yx = #direct effect of fitness on illness
mx = #direct effect of fitness on stress
ym = #direct effect of stress on illness
y.xm = #direct effect of stress and fitness on illness
  
#indirect effect = coef of mediator in all models
  
exercise = c(1, -.03, .39)
hardiness = c(-.03, 1, .07)
fitness = c(.39, .07, 1)
stress = c(-.05, -.23,-.13)

pred_mat = cbind(exercise, hardiness, fitness)
coefs = solve(pred_mat, stress)

library(mediation)
data(framing)

attach(framing)

fit1 = lm(emo ~ treat)
fit2 = lm(cong_mesg ~ emo)
fit3 = lm(cong_mesg ~ treat)

#is there a significant effect of treatment on emo
summary(fit1)

#is there a significant effect of treatment on cong_mesg
summary(fit3)

#is there a significant effect of 