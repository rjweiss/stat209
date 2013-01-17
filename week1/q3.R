data = read.table('http://www-stat.stanford.edu/~rag/stat209/coleman320.dat', header=T)

attach(data)
var1 = vach
var2 = momed
var3 = ses

B12 = lm(var1 ~ var2)
B13 = lm(var1 ~ var3)
B32 = lm(var3 ~ var2)
B12.3 = lm(var1 ~ var2 + var3)
B13.2 = lm(var1 ~ var3 + var2)

#test that the relationship holds

B12$coefficients[2]
sum(B12.3$coefficients[2], B32$coefficients[2] * B13.2$coefficients[2])

B12.3$coefficients[2]
1 / (1 - cor(var2, var3)^2)  * (B12$coefficients[2] - B32$coefficients[2] * B13$coefficients[2])