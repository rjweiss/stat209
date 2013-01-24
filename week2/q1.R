t_tc.u = rnorm(100, mean=2, sd=1)
y.uc = rnorm(100, mean=10, sd=1)

g =  round(rnorm(100, 0.5, 0.2), digits=0)
gneq = round(rnorm(100, 0.5, 0.2), digits=0) # this is wrong

data = data.frame(t_tc.u, y.uc)